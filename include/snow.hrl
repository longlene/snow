%% Snow Snowflake ID Generator - Shared Definitions
%% This header file contains all shared macros, records, and constants
%% used across snow modules.

%% ============================================================================
%% Compile-time configurable bit allocation
%% These can be overridden in rebar.config or via command line
%% ============================================================================

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

%% ============================================================================
%% Pre-computed bit shift amounts for fast ID construction
%% ============================================================================

-define(TIMESTAMP_SHIFT, (?region_bits + ?worker_bits + ?sequence_bits)).
-define(REGION_SHIFT, (?worker_bits + ?sequence_bits)).
-define(WORKER_SHIFT, ?sequence_bits).

%% ============================================================================
%% Maximum values for each field
%% ============================================================================

-define(MAX_REGION, ((1 bsl ?region_bits) - 1)).
-define(MAX_WORKER, ((1 bsl ?worker_bits) - 1)).
-define(MAX_SEQUENCE, ((1 bsl ?sequence_bits) - 1)).

%% ============================================================================
%% Compile-time validation: ensure sequence bits is positive
%% ============================================================================

-define(ASSERT_VALID_BITS, 
    case ?sequence_bits of
        N when N > 0 -> ok;
        _ -> error({invalid_bit_allocation, 
                   "sequence_bits would be " ++ integer_to_list(?sequence_bits) ++ 
                   ", but must be positive. Reduce other bit allocations."})
    end).

%% ============================================================================
%% Record definitions
%% ============================================================================

%% Global worker configuration record
-record(snow_config, {
    epoch :: non_neg_integer(),
    region :: non_neg_integer(),
    worker :: non_neg_integer(),
    atomic_ref :: atomics:atomics_ref(),
    %% Pre-computed base ID for fast generation: (Region << region_shift) | (Worker << worker_shift)
    base_id :: non_neg_integer()
}).

%% Worker handle record for multi-worker support
-record(worker_handle, {
    epoch :: non_neg_integer(),
    region :: non_neg_integer(),
    worker :: non_neg_integer(),
    atomic_ref :: atomics:atomics_ref(),
    base_id :: non_neg_integer()
}).

%% ============================================================================
%% Constants and keys
%% ============================================================================

-define(PERSISTENT_KEY, snow_state).