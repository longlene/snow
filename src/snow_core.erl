-module(snow_core).

%% Include shared definitions
-include("snow.hrl").

%% Worker API exports
-export([start/3,
         next_id/1,
         next_ids/2,
         worker_info/1,
         %% Shared functions for global worker
         generate/3,
         generate_batch/4]).

%% Type exports
-export_type([worker_handle/0]).

%% Opaque type for worker handles
-opaque worker_handle() :: #worker_handle{}.

%% ============================================================================
%% Multi-Worker API
%% ============================================================================

%% Start a new worker instance
-spec start(Epoch :: non_neg_integer(), Region :: non_neg_integer(), Worker :: non_neg_integer()) -> worker_handle().
start(Epoch, Region, Worker)
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
    #worker_handle{
        epoch = Epoch,
        region = Region,
        worker = Worker,
        atomic_ref = AtomicRef,
        base_id = BaseId
    }.

%% Generate single ID using worker handle
-spec next_id(worker_handle()) -> non_neg_integer().
next_id(#worker_handle{
    epoch = Epoch,
    atomic_ref = AtomicRef,
    base_id = BaseId
}) ->
    generate_id_loop(Epoch, BaseId, AtomicRef).

%% Generate multiple IDs using worker handle
-spec next_ids(worker_handle(), Count :: pos_integer()) -> [non_neg_integer()].
next_ids(#worker_handle{
    epoch = Epoch,
    atomic_ref = AtomicRef,
    base_id = BaseId
}, Count) when Count > 0 ->
    generate_ids_batch_loop(Count, Epoch, BaseId, AtomicRef).

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
worker_info(#worker_handle{epoch = Epoch, region = Region, worker = Worker}) ->
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
%% Shared functions for global worker (called from snow.erl)
%% ============================================================================

%% Shared function for global worker (called from snow.erl)
-spec generate(non_neg_integer(), non_neg_integer(), atomics:atomics_ref()) -> non_neg_integer().
generate(Epoch, BaseId, AtomicRef) ->
    generate_id_loop(Epoch, BaseId, AtomicRef).

%% Shared function for global worker batch generation
-spec generate_batch(pos_integer(), non_neg_integer(), non_neg_integer(), atomics:atomics_ref()) -> [non_neg_integer()].
generate_batch(Count, Epoch, BaseId, AtomicRef) when Count > 0 ->
    generate_ids_batch_loop(Count, Epoch, BaseId, AtomicRef).

%% ============================================================================
%% Internal functions - Core ID generation algorithms
%% ============================================================================

generate_id_loop(Epoch, BaseId, AtomicRef) ->
    %% Get initial atomic value
    AtomicValue = atomics:get(AtomicRef, 1),
    generate_id_loop(Epoch, BaseId, AtomicRef, AtomicValue).

generate_id_loop(Epoch, BaseId, AtomicRef, OldVal) ->
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
            generate_id_loop(Epoch, BaseId, AtomicRef, CurrentVal)
    end.

%% Generate multiple IDs efficiently by reserving a sequence range
generate_ids_batch_loop(Count, Epoch, BaseId, AtomicRef) ->
    %% Get initial atomic value
    AtomicValue = atomics:get(AtomicRef, 1),
    generate_ids_batch_loop(Count, Epoch, BaseId, AtomicRef, AtomicValue).

generate_ids_batch_loop(Count, Epoch, BaseId, AtomicRef, OldVal) ->
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
                    generate_ids_batch_loop(Count, Epoch, BaseId, AtomicRef);
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
                                    Ids ++ generate_ids_batch_loop(Count - ReserveCount, Epoch, BaseId, AtomicRef);
                                false ->
                                    Ids
                            end;
                        CurrentVal ->
                            %% CAS failed - use returned current value to retry immediately
                            generate_ids_batch_loop(Count, Epoch, BaseId, AtomicRef, CurrentVal)
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
                            Ids ++ generate_ids_batch_loop(Count - ReserveCount, Epoch, BaseId, AtomicRef);
                        false ->
                            Ids
                    end;
                CurrentVal ->
                    %% CAS failed - use returned current value to retry immediately
                    generate_ids_batch_loop(Count, Epoch, BaseId, AtomicRef, CurrentVal)
            end;
        _ ->
            %% Clock went backwards
            error({clock_backwards, OldTimestamp, Now})
    end.