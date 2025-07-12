-module(snow).

-export([init/3,
         init/4,
         next_id/0,
         next_ids/1,
         decode_id/1,
         info/0]).

-define(PERSISTENT_KEY, snow_state).

%% Compile-time configurable bit allocation
%% These can be overridden in rebar.config or via command line
-ifndef(timestamp_bits).
-define(timestamp_bits, 41).
-endif.

-ifndef(region_bits).
-define(region_bits, 4).
-endif.

-ifndef(worker_bits).
-define(worker_bits, 6).
-endif.

%% Sequence bits is automatically calculated to ensure total = 64 bits
-define(sequence_bits, (64 - 1 - ?timestamp_bits - ?region_bits - ?worker_bits)).

%% Pre-computed bit shift amounts for fast ID construction
-define(TIMESTAMP_SHIFT, (?region_bits + ?worker_bits + ?sequence_bits)).
-define(REGION_SHIFT, (?worker_bits + ?sequence_bits)).
-define(WORKER_SHIFT, ?sequence_bits).

%% Maximum values for each field
-define(MAX_REGION, ((1 bsl ?region_bits) - 1)).
-define(MAX_WORKER, ((1 bsl ?worker_bits) - 1)).
-define(MAX_SEQUENCE, ((1 bsl ?sequence_bits) - 1)).

%% Compile-time validation: ensure sequence bits is positive
-define(ASSERT_VALID_BITS, 
    case ?sequence_bits of
        N when N > 0 -> ok;
        _ -> error({invalid_bit_allocation, 
                   "sequence_bits would be " ++ integer_to_list(?sequence_bits) ++ 
                   ", but must be positive. Reduce other bit allocations."})
    end).

%% Generation modes
-type generation_mode() :: conservative | aggressive.

-record(snow_config, {
    epoch :: non_neg_integer(),
    region :: non_neg_integer(),
    worker :: non_neg_integer(),
    atomic_ref :: atomics:atomics_ref(),
    %% Pre-computed base ID for fast generation: (Region << region_shift) | (Worker << worker_shift)
    base_id :: non_neg_integer(),
    %% Generation mode: conservative (with backoff) or aggressive (immediate retry)
    mode :: generation_mode()
}).

%% Initialize with epoch, region, and worker (default aggressive mode)
-spec init(Epoch :: non_neg_integer(), Region :: non_neg_integer(), Worker :: non_neg_integer()) -> ok.
init(Epoch, Region, Worker) ->
    init(Epoch, Region, Worker, aggressive).

%% Initialize with epoch, region, worker, and generation mode
-spec init(Epoch :: non_neg_integer(), Region :: non_neg_integer(), Worker :: non_neg_integer(), Mode :: generation_mode()) -> ok.
init(Epoch, Region, Worker, Mode)
  when is_integer(Epoch), Epoch >= 0,
       is_integer(Region), Region >= 0, Region =< ?MAX_REGION,
       is_integer(Worker), Worker >= 0, Worker =< ?MAX_WORKER,
       (Mode =:= conservative orelse Mode =:= aggressive) ->
    %% Validate bit allocation at runtime
    ?ASSERT_VALID_BITS,
    
    AtomicRef = atomics:new(1, [{signed, false}]),
    %% Initialize with current timestamp and sequence 0
    InitTimestamp = erlang:system_time(millisecond) - Epoch,
    InitVal = (InitTimestamp bsl ?sequence_bits) bor 0,
    atomics:put(AtomicRef, 1, InitVal),
    %% Pre-compute base ID: region and worker parts
    BaseId = (Region bsl ?REGION_SHIFT) bor (Worker bsl ?WORKER_SHIFT),
    Config = #snow_config{
        epoch = Epoch,
        region = Region,
        worker = Worker,
        atomic_ref = AtomicRef,
        base_id = BaseId,
        mode = Mode
    },
    persistent_term:put(?PERSISTENT_KEY, Config),
    ok.

%% Generate single ID
-spec next_id() -> non_neg_integer().
next_id() ->
    #snow_config{
        epoch = Epoch,
        atomic_ref = AtomicRef,
        base_id = BaseId,
        mode = Mode
    } = persistent_term:get(?PERSISTENT_KEY),
    
    case Mode of
        conservative ->
            generate_id_loop(Epoch, BaseId, AtomicRef);
        aggressive ->
            generate_id_aggressive(Epoch, BaseId, AtomicRef)
    end.

%% Generate multiple IDs efficiently by reserving a sequence range
-spec next_ids(Count :: pos_integer()) -> [non_neg_integer()].
next_ids(Count) when Count > 0 ->
    #snow_config{
        epoch = Epoch,
        atomic_ref = AtomicRef,
        base_id = BaseId,
        mode = Mode
    } = persistent_term:get(?PERSISTENT_KEY),
    
    case Mode of
        conservative ->
            generate_ids_batch_loop(Count, Epoch, BaseId, AtomicRef);
        aggressive ->
            generate_ids_batch_aggressive(Count, Epoch, BaseId, AtomicRef)
    end.

%% Decode ID into components using bit operations
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
    mode := generation_mode(),
    bits := #{
        timestamp := pos_integer(),
        region := pos_integer(),
        worker := pos_integer(),
        sequence := pos_integer()
    }
}.
info() ->
    #snow_config{epoch = Epoch, region = Region, worker = Worker, mode = Mode} = 
        persistent_term:get(?PERSISTENT_KEY),
    #{
        epoch => Epoch,
        region => Region,
        worker => Worker,
        mode => Mode,
        bits => #{
            timestamp => ?timestamp_bits,
            region => ?region_bits,
            worker => ?worker_bits,
            sequence => ?sequence_bits
        }
    }.

%% Internal functions
generate_id_loop(Epoch, BaseId, AtomicRef) ->
    generate_id_loop(Epoch, BaseId, AtomicRef, 0).

generate_id_loop(Epoch, BaseId, AtomicRef, Retries) ->
    OldVal = atomics:get(AtomicRef, 1),
    %% Parse current state: timestamp in high bits, sequence in low bits
    OldTimestamp = OldVal bsr ?sequence_bits,
    OldSequence = OldVal band ?MAX_SEQUENCE,
    
    %% Get current timestamp
    Now = erlang:system_time(millisecond) - Epoch,
    
    %% Determine new timestamp and sequence
    {NewTimestamp, NewSequence} = case Now of
        Now when Now =:= OldTimestamp ->
            %% Same millisecond - increment sequence
            case OldSequence of
                ?MAX_SEQUENCE ->
                    %% Sequence exhausted - wait for next millisecond
                    timer:sleep(1),
                    NewNow = erlang:system_time(millisecond) - Epoch,
                    {NewNow, 0};
                _ ->
                    %% Normal increment
                    {Now, OldSequence + 1}
            end;
        Now when Now > OldTimestamp ->
            %% New millisecond - reset sequence to 0
            {Now, 0};
        _ ->
            %% Clock went backwards
            error({clock_backwards, OldTimestamp, Now})
    end,
    
    %% Create new atomic value
    NewVal = (NewTimestamp bsl ?sequence_bits) bor NewSequence,
    
    %% Compare and swap
    case atomics:compare_exchange(AtomicRef, 1, OldVal, NewVal) of
        ok ->
            %% Success - construct final ID using pre-computed base
            Id = (NewTimestamp bsl ?TIMESTAMP_SHIFT) bor BaseId bor NewSequence,
            Id;
        _ ->
            %% CAS failed - retry
            adaptive_backoff(Retries),
            generate_id_loop(Epoch, BaseId, AtomicRef, Retries + 1)
    end.

%% Generate multiple IDs efficiently by reserving a sequence range
generate_ids_batch_loop(Count, Epoch, BaseId, AtomicRef) ->
    generate_ids_batch_loop(Count, Epoch, BaseId, AtomicRef, 0).

generate_ids_batch_loop(Count, Epoch, BaseId, AtomicRef, Retries) ->
    OldVal = atomics:get(AtomicRef, 1),
    OldTimestamp = OldVal bsr ?sequence_bits,
    OldSequence = OldVal band ?MAX_SEQUENCE,
    
    %% Get current timestamp
    Now = erlang:system_time(millisecond) - Epoch,
    
    %% Calculate how many IDs we can generate in current millisecond
    case Now of
        Now when Now =:= OldTimestamp ->
            %% Same millisecond - check available sequence space
            AvailableSeqs = ?MAX_SEQUENCE - OldSequence,
            case AvailableSeqs of
                0 ->
                    %% No space left, wait for next millisecond
                    timer:sleep(1),
                    generate_ids_batch_loop(Count, Epoch, BaseId, AtomicRef, Retries);
                _ ->
                    %% Reserve as many as possible (up to Count)
                    ReserveCount = erlang:min(Count, AvailableSeqs),
                    NewSequence = OldSequence + ReserveCount,
                    NewVal = (OldTimestamp bsl ?sequence_bits) bor NewSequence,
                    
                    %% Try to reserve the range
                    case atomics:compare_exchange(AtomicRef, 1, OldVal, NewVal) of
                        ok ->
                            %% Success - generate IDs for reserved range
                            TimestampBaseId = (OldTimestamp bsl ?TIMESTAMP_SHIFT) bor BaseId,
                            Ids = [TimestampBaseId bor (OldSequence + I) || I <- lists:seq(1, ReserveCount)],
                            
                            %% If we need more IDs, recursively get them
                            case ReserveCount < Count of
                                true ->
                                    Ids ++ generate_ids_batch_loop(Count - ReserveCount, Epoch, BaseId, AtomicRef, 0);
                                false ->
                                    Ids
                            end;
                        _ ->
                            %% CAS failed - retry
                            erlang:yield(),
                            generate_ids_batch_loop(Count, Epoch, BaseId, AtomicRef, Retries + 1)
                    end
            end;
        Now when Now > OldTimestamp ->
            %% New millisecond - can reserve up to Count or MAX_SEQUENCE
            ReserveCount = erlang:min(Count, ?MAX_SEQUENCE + 1),
            NewSequence = ReserveCount - 1,
            NewVal = (Now bsl ?sequence_bits) bor NewSequence,
            
            %% Try to reserve the range starting from 0
            case atomics:compare_exchange(AtomicRef, 1, OldVal, NewVal) of
                ok ->
                    %% Success - generate IDs
                    TimestampBaseId = (Now bsl ?TIMESTAMP_SHIFT) bor BaseId,
                    Ids = [TimestampBaseId bor I || I <- lists:seq(0, NewSequence)],
                    
                    %% If we need more IDs, recursively get them
                    case ReserveCount < Count of
                        true ->
                            Ids ++ generate_ids_batch_loop(Count - ReserveCount, Epoch, BaseId, AtomicRef, 0);
                        false ->
                            Ids
                    end;
                _ ->
                    %% CAS failed - retry  
                    erlang:yield(),
                    generate_ids_batch_loop(Count, Epoch, BaseId, AtomicRef, Retries + 1)
            end;
        _ ->
            %% Clock went backwards
            error({clock_backwards, OldTimestamp, Now})
    end.

%% Aggressive mode: use CAS return value directly, no backoff
generate_id_aggressive(Epoch, BaseId, AtomicRef) ->
    %% Get initial atomic value
    AtomicValue = atomics:get(AtomicRef, 1),
    generate_id_aggressive_loop(Epoch, BaseId, AtomicRef, AtomicValue).

generate_id_aggressive_loop(Epoch, BaseId, AtomicRef, OldVal) ->
    %% Parse current state: timestamp in high bits, sequence in low bits
    OldTimestamp = OldVal bsr ?sequence_bits,
    OldSequence = OldVal band ?MAX_SEQUENCE,
    
    %% Get current timestamp
    Now = erlang:system_time(millisecond) - Epoch,
    
    %% Determine new timestamp and sequence
    {NewTimestamp, NewSequence} = case Now of
        Now when Now =:= OldTimestamp ->
            %% Same millisecond - increment sequence
            case OldSequence of
                ?MAX_SEQUENCE ->
                    %% Sequence exhausted - wait for next millisecond
                    timer:sleep(1),
                    NewNow = erlang:system_time(millisecond) - Epoch,
                    {NewNow, 0};
                _ ->
                    %% Normal increment
                    {Now, OldSequence + 1}
            end;
        Now when Now > OldTimestamp ->
            %% New millisecond - reset sequence to 0
            {Now, 0};
        _ ->
            %% Clock went backwards
            error({clock_backwards, OldTimestamp, Now})
    end,
    
    %% Create new atomic value
    NewVal = (NewTimestamp bsl ?sequence_bits) bor NewSequence,
    
    %% Compare and swap
    case atomics:compare_exchange(AtomicRef, 1, OldVal, NewVal) of
        ok ->
            %% Success - construct final ID using pre-computed base
            Id = (NewTimestamp bsl ?TIMESTAMP_SHIFT) bor BaseId bor NewSequence,
            Id;
        CurrentVal ->
            %% CAS failed - use returned current value to retry immediately
            generate_id_aggressive_loop(Epoch, BaseId, AtomicRef, CurrentVal)
    end.

%% Aggressive mode batch generation: use CAS return value directly, no backoff
generate_ids_batch_aggressive(Count, Epoch, BaseId, AtomicRef) ->
    %% Get initial atomic value
    AtomicValue = atomics:get(AtomicRef, 1),
    generate_ids_batch_aggressive_loop(Count, Epoch, BaseId, AtomicRef, AtomicValue).

generate_ids_batch_aggressive_loop(Count, Epoch, BaseId, AtomicRef, OldVal) ->
    OldTimestamp = OldVal bsr ?sequence_bits,
    OldSequence = OldVal band ?MAX_SEQUENCE,
    
    %% Get current timestamp
    Now = erlang:system_time(millisecond) - Epoch,
    
    %% Calculate how many IDs we can generate in current millisecond
    case Now of
        Now when Now =:= OldTimestamp ->
            %% Same millisecond - check available sequence space
            AvailableSeqs = ?MAX_SEQUENCE - OldSequence,
            case AvailableSeqs of
                0 ->
                    %% No space left, wait for next millisecond
                    timer:sleep(1),
                    generate_ids_batch_aggressive(Count, Epoch, BaseId, AtomicRef);
                _ ->
                    %% Reserve as many as possible (up to Count)
                    ReserveCount = erlang:min(Count, AvailableSeqs),
                    NewSequence = OldSequence + ReserveCount,
                    NewVal = (OldTimestamp bsl ?sequence_bits) bor NewSequence,
                    
                    %% Try to reserve the range
                    case atomics:compare_exchange(AtomicRef, 1, OldVal, NewVal) of
                        ok ->
                            %% Success - generate IDs for reserved range
                            TimestampBaseId = (OldTimestamp bsl ?TIMESTAMP_SHIFT) bor BaseId,
                            Ids = [TimestampBaseId bor (OldSequence + I) || I <- lists:seq(1, ReserveCount)],
                            
                            %% If we need more IDs, recursively get them
                            case ReserveCount < Count of
                                true ->
                                    Ids ++ generate_ids_batch_aggressive(Count - ReserveCount, Epoch, BaseId, AtomicRef);
                                false ->
                                    Ids
                            end;
                        CurrentVal ->
                            %% CAS failed - use returned current value to retry immediately
                            generate_ids_batch_aggressive_loop(Count, Epoch, BaseId, AtomicRef, CurrentVal)
                    end
            end;
        Now when Now > OldTimestamp ->
            %% New millisecond - can reserve up to Count or MAX_SEQUENCE
            ReserveCount = erlang:min(Count, ?MAX_SEQUENCE + 1),
            NewSequence = ReserveCount - 1,
            NewVal = (Now bsl ?sequence_bits) bor NewSequence,
            
            %% Try to reserve the range starting from 0
            case atomics:compare_exchange(AtomicRef, 1, OldVal, NewVal) of
                ok ->
                    %% Success - generate IDs
                    TimestampBaseId = (Now bsl ?TIMESTAMP_SHIFT) bor BaseId,
                    Ids = [TimestampBaseId bor I || I <- lists:seq(0, NewSequence)],
                    
                    %% If we need more IDs, recursively get them
                    case ReserveCount < Count of
                        true ->
                            Ids ++ generate_ids_batch_aggressive(Count - ReserveCount, Epoch, BaseId, AtomicRef);
                        false ->
                            Ids
                    end;
                CurrentVal ->
                    %% CAS failed - use returned current value to retry immediately
                    generate_ids_batch_aggressive_loop(Count, Epoch, BaseId, AtomicRef, CurrentVal)
            end;
        _ ->
            %% Clock went backwards
            error({clock_backwards, OldTimestamp, Now})
    end.

%% Adaptive backoff strategy for CAS contention
%% Based on retry count, apply progressively stronger backoff
-spec adaptive_backoff(Retries :: non_neg_integer()) -> ok.
adaptive_backoff(Retries) when Retries < 3 ->
    %% Light contention: just yield CPU to other processes
    erlang:yield();
adaptive_backoff(Retries) when Retries < 6 ->
    %% Medium contention: brief sleep to reduce immediate retry pressure
    timer:sleep(1);
adaptive_backoff(Retries) ->
    %% Heavy contention: progressive sleep with cap at 10ms
    SleepTime = min(10, Retries - 5),
    timer:sleep(SleepTime).
