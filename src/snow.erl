-module(snow).

%% Include shared definitions
-include("snow.hrl").

-export([init/3,
         next_id/0,
         next_id/1,
         next_ids/1,
         next_ids/2,
         decode_id/1,
         info/0,
         start_worker/3,
         worker_info/1]).

%% Type exports - re-export from snow_core
-type worker_handle() :: snow_core:worker_handle().

%% Initialize with epoch, region, and worker
-spec init(Epoch :: non_neg_integer(), Region :: non_neg_integer(), Worker :: non_neg_integer()) -> ok.
init(Epoch, Region, Worker)
  when is_integer(Epoch), Epoch >= 0,
       is_integer(Region), Region >= 0, Region =< ?MAX_REGION,
       is_integer(Worker), Worker >= 0, Worker =< ?MAX_WORKER ->
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
        base_id = BaseId
    },
    persistent_term:put(?PERSISTENT_KEY, Config),
    ok.

%% Generate single ID
-spec next_id() -> non_neg_integer().
next_id() ->
    #snow_config{
        epoch = Epoch,
        atomic_ref = AtomicRef,
        base_id = BaseId
    } = persistent_term:get(?PERSISTENT_KEY),
    
    snow_core:generate(Epoch, BaseId, AtomicRef).

%% Generate multiple IDs efficiently by reserving a sequence range
-spec next_ids(Count :: pos_integer()) -> [non_neg_integer()].
next_ids(Count) when Count > 0 ->
    #snow_config{
        epoch = Epoch,
        atomic_ref = AtomicRef,
        base_id = BaseId
    } = persistent_term:get(?PERSISTENT_KEY),
    
    snow_core:generate_batch(Count, Epoch, BaseId, AtomicRef).

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

%% ============================================================================
%% Multi-Worker API - Delegated to snow_core
%% ============================================================================

%% Start a new worker instance
-spec start_worker(Epoch :: non_neg_integer(), Region :: non_neg_integer(), Worker :: non_neg_integer()) -> worker_handle().
start_worker(Epoch, Region, Worker) ->
    snow_core:start(Epoch, Region, Worker).

%% Generate single ID using worker handle
-spec next_id(worker_handle()) -> non_neg_integer().
next_id(WorkerHandle) ->
    snow_core:next_id(WorkerHandle).

%% Generate multiple IDs using worker handle
-spec next_ids(worker_handle(), Count :: pos_integer()) -> [non_neg_integer()].
next_ids(WorkerHandle, Count) when Count > 0 ->
    snow_core:next_ids(WorkerHandle, Count).

%% Get worker configuration info
-spec worker_info(worker_handle()) -> #{
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
worker_info(WorkerHandle) ->
    snow_core:worker_info(WorkerHandle).

%% All internal functions have been moved to snow_core.erl
