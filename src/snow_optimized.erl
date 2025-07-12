-module(snow_optimized).

-export([init/3,
         next_id/0,
         decode_id/1,
         info/0]).

-define(PERSISTENT_KEY, snow_optimized_state).

%% Compile-time configurable bit allocation (same as original)
-ifndef(timestamp_bits).
-define(timestamp_bits, 41).
-endif.

-ifndef(region_bits).
-define(region_bits, 4).
-endif.

-ifndef(worker_bits).
-define(worker_bits, 6).
-endif.

%% Sequence bits is automatically calculated
-define(sequence_bits, (64 - 1 - ?timestamp_bits - ?region_bits - ?worker_bits)).

%% Pre-computed bit shift amounts
-define(TIMESTAMP_SHIFT, (?region_bits + ?worker_bits + ?sequence_bits)).
-define(REGION_SHIFT, (?worker_bits + ?sequence_bits)).
-define(WORKER_SHIFT, ?sequence_bits).

%% Maximum values
-define(MAX_REGION, ((1 bsl ?region_bits) - 1)).
-define(MAX_WORKER, ((1 bsl ?worker_bits) - 1)).
-define(MAX_SEQUENCE, ((1 bsl ?sequence_bits) - 1)).

%% Lazy initialization macro (borrowed from petrkozorezov)
-define(pt_get(Key, Expr),
  case persistent_term:get(Key, undefined) of
    undefined ->
      __InitValue = Expr,
      ok = persistent_term:put(Key, __InitValue),
      __InitValue;
    __Value ->
      __Value
  end
).

-record(snow_config, {
    epoch :: non_neg_integer(),
    region :: non_neg_integer(),
    worker :: non_neg_integer(),
    base_id :: non_neg_integer()  % Pre-computed base ID
}).

%% Initialize with epoch, region, and worker
-spec init(Epoch :: non_neg_integer(), Region :: non_neg_integer(), Worker :: non_neg_integer()) -> ok.
init(Epoch, Region, Worker)
  when is_integer(Epoch), Epoch >= 0,
       is_integer(Region), Region >= 0, Region =< ?MAX_REGION,
       is_integer(Worker), Worker >= 0, Worker =< ?MAX_WORKER ->
    
    %% Pre-compute base ID: region and worker parts
    BaseId = (Region bsl ?REGION_SHIFT) bor (Worker bsl ?WORKER_SHIFT),
    Config = #snow_config{
        epoch = Epoch,
        region = Region,
        worker = Worker,
        base_id = BaseId
    },
    persistent_term:put(?PERSISTENT_KEY, Config),
    ok.

%% Generate single ID using petrkozorezov's optimized approach
-spec next_id() -> non_neg_integer().
next_id() ->
    #snow_config{
        epoch = Epoch,
        base_id = BaseId
    } = persistent_term:get(?PERSISTENT_KEY),
    
    %% Lazy initialization of atomics (borrowed from petrkozorezov)
    AtomicsRef = ?pt_get(snow_optimized_counter, atomics:new(1, [{signed, false}])),
    
    generate_id_iter(Epoch, BaseId, AtomicsRef, atomics:get(AtomicsRef, 1)).

%% Core generation logic inspired by petrkozorezov's approach
generate_id_iter(Epoch, BaseId, AtomicsRef, AtomicValue) ->
    Timestamp = erlang:system_time(millisecond) - Epoch,
    %% Key optimization: Store timestamp<<sequence_bits directly
    MSInitial = Timestamp bsl ?sequence_bits,
    Diff = AtomicValue - MSInitial,
    
    %% Determine new atomic value using petrkozorezov's logic
    NewAtomicValue = case Diff of
        D when D == ?MAX_SEQUENCE ->
            %% Sequence exhausted - wait for next millisecond
            timer:sleep(1),
            NewTimestamp = erlang:system_time(millisecond) - Epoch,
            NewTimestamp bsl ?sequence_bits;
        D when D > ?MAX_SEQUENCE ->
            %% Clock went backwards
            error({clock_backwards, AtomicValue bsr ?sequence_bits, Timestamp});
        D when D >= 0 ->
            %% Same millisecond - increment sequence
            AtomicValue + 1;
        _ ->
            %% New millisecond - reset sequence to 0
            MSInitial
    end,
    
    %% Compare and swap (no backoff strategy like petrkozorezov)
    case atomics:compare_exchange(AtomicsRef, 1, AtomicValue, NewAtomicValue) of
        ok ->
            %% Success - construct final ID
            Sequence = NewAtomicValue band ?MAX_SEQUENCE,
            Id = (Timestamp bsl ?TIMESTAMP_SHIFT) bor BaseId bor Sequence,
            Id;
        UpdatedCounter ->
            %% CAS failed - retry immediately (no backoff)
            generate_id_iter(Epoch, BaseId, AtomicsRef, UpdatedCounter)
    end.

%% Decode ID into components
-spec decode_id(Id :: non_neg_integer()) -> #{
    timestamp := non_neg_integer(),
    region := non_neg_integer(),
    worker := non_neg_integer(),
    sequence := non_neg_integer()
}.
decode_id(Id) when is_integer(Id), Id >= 0 ->
    Sequence = Id band ?MAX_SEQUENCE,
    Worker = (Id bsr ?WORKER_SHIFT) band ((1 bsl ?worker_bits) - 1),
    Region = (Id bsr ?REGION_SHIFT) band ((1 bsl ?region_bits) - 1),
    Timestamp = Id bsr ?TIMESTAMP_SHIFT,
    
    #{
        timestamp => Timestamp,
        region => Region,
        worker => Worker,
        sequence => Sequence
    }.

%% Get current configuration info
-spec info() -> #{
    epoch := non_neg_integer(),
    region := non_neg_integer(),
    worker := non_neg_integer(),
    bits := #{
        timestamp := pos_integer(),
        region := pos_integer(),
        worker := pos_integer(),
        sequence := pos_integer()
    }
}.
info() ->
    #snow_config{epoch = Epoch, region = Region, worker = Worker} = 
        persistent_term:get(?PERSISTENT_KEY),
    #{
        epoch => Epoch,
        region => Region,
        worker => Worker,
        bits => #{
            timestamp => ?timestamp_bits,
            region => ?region_bits,
            worker => ?worker_bits,
            sequence => ?sequence_bits
        }
    }.