# Snow - High-Performance Lock-Free Erlang Snowflake ID Generator

A high-performance, lock-free Snowflake ID generation library for Erlang, built with `persistent_term` and `atomics` for truly concurrent operation.

## Features

- 🚀 **Lock-Free Design**: Uses Erlang atomics and CAS operations for true lock-free concurrency
- ⚡ **High Performance**: 
  - Single-thread: 2M+ IDs/sec
  - Concurrent (20 processes): 3.7M+ IDs/sec with 1.8x speedup
- 🌍 **Distributed-Friendly**: Supports region and worker ID configuration for multi-node deployments
- ⏰ **Clock Protection**: Automatic detection and handling of clock drift/backwards
- 🔧 **Configurable**: Custom epoch, region, worker ID, and compile-time bit allocation
- 🎯 **Aggressive CAS**: Direct retry with CAS return value optimization for maximum throughput
- 📦 **Zero Dependencies**: Pure Erlang implementation

## ID Structure (64-bit)

```
| 1 bit | 41 bits | 4 bits | 6 bits | 12 bits |
|-------|---------|--------|--------|---------|
|   0   |timestamp| region | worker |sequence |
```

- **Reserved** (1 bit): Always 0
- **Timestamp** (41 bits): Millisecond timestamp, ~69 years from epoch
- **Region** (4 bits): Region ID (0-15, configurable)
- **Worker** (6 bits): Worker node ID (0-63, configurable)  
- **Sequence** (12 bits): Per-millisecond sequence (0-4095, auto-calculated)

## Quick Start

### Add Dependency

```erlang
{deps, [
    {snow, {git, "https://github.com/yourusername/snow.git", {branch, "main"}}}
]}.
```

### Configuration

Configure in `sys.config`:

```erlang
[
  {snow, [
    {epoch, 1640995200000}, % 2022-01-01 00:00:00 UTC
    {region, 0},            % 0-15
    {worker, 0}             % 0-63
  ]}
].
```

### Usage Examples

```erlang
%% Start application
application:ensure_all_started(snow).

%% Generate single ID
Id = snow:next_id().
% 7128834371969425408

%% Batch generate IDs (optimized for bulk operations)
Ids = snow:next_ids(1000).

%% Decode ID
#{timestamp := Ts, region := R, worker := W, sequence := Seq} = snow:decode_id(Id).

%% Custom initialization
snow:init(1640995200000, 5, 10).

%% Get configuration info
snow:info().
```

## API Reference

### snow:init/3
Initialize the generator with custom configuration.

**Parameters:**
- `Epoch :: non_neg_integer()` - Start timestamp in milliseconds
- `Region :: 0..15` - Region ID
- `Worker :: 0..63` - Worker node ID

### snow:next_id/0
Generate a single Snowflake ID.

**Returns:** `non_neg_integer()`

### snow:next_ids/1
Efficiently generate multiple IDs in batch.

**Parameters:**
- `Count :: pos_integer()` - Number of IDs to generate

**Returns:** `[non_neg_integer()]`

### snow:decode_id/1
Decode a Snowflake ID into its components.

**Parameters:**
- `Id :: non_neg_integer()` - Snowflake ID to decode

**Returns:**
```erlang
#{
    timestamp := non_neg_integer(),
    region := non_neg_integer(),
    worker := non_neg_integer(),
    sequence := non_neg_integer()
}
```

### snow:info/0
Get current generator configuration and bit allocation.

**Returns:**
```erlang
#{
    epoch := non_neg_integer(),
    region := non_neg_integer(),
    worker := non_neg_integer(),
    bits := #{
        timestamp := pos_integer(),
        region := pos_integer(),
        worker := pos_integer(),
        sequence := pos_integer()
    }
}
```

## Performance

Benchmarks on modern hardware (Erlang/OTP 26):

| Scenario | Performance | Notes |
|----------|------------|--------|
| Single-thread | 2.07M IDs/sec | Maximum single-threaded performance |
| 10 processes | 2.26M IDs/sec | Good concurrent performance |
| 20 processes | 3.24M IDs/sec | Excellent concurrent scalability |
| 50 processes | 2.75M IDs/sec | Strong performance under high load |

### Performance Optimizations

1. **Pre-computed Base IDs**: Region and worker bits calculated once at initialization
2. **Compile-time Constants**: Bit shifts resolved at compile time
3. **CAS Return Value Optimization**: Uses actual values from failed CAS operations for immediate retry
4. **Batch Sequence Reservation**: Single CAS operation reserves multiple sequence numbers
5. **Lock-Free Design**: Pure atomic operations without any locking mechanisms

## Compile-time Configuration

Customize bit allocation by modifying `rebar.config`:

```erlang
{erl_opts, [
    debug_info,
    {d, timestamp_bits, 41},  % Timestamp bits (supports ~69 years)
    {d, region_bits, 4},      % Region bits (16 regions)
    {d, worker_bits, 6}       % Worker bits (64 workers)
    % sequence_bits automatically calculated: 64 - 1 - 41 - 4 - 6 = 12
]}.
```

**Example configurations:**
- High regions: `{d, region_bits, 5}` + `{d, worker_bits, 5}` = 32 regions, 32 workers, 2048/ms
- High workers: `{d, region_bits, 3}` + `{d, worker_bits, 7}` = 8 regions, 128 workers, 2048/ms

## Architecture

### Lock-Free Design
- Uses `atomics:compare_exchange/4` for atomic sequence updates
- No GenServer or process-based state management
- Configuration stored in `persistent_term` for fast access

### CAS Optimization Strategy
Maximizes throughput through immediate CAS retry:

```erlang
%% Uses return value from failed CAS operations
case atomics:compare_exchange(AtomicRef, 1, OldVal, NewVal) of
    ok ->
        %% Success - return generated ID
        construct_final_id(Timestamp, BaseId, Sequence);
    CurrentVal ->
        %% Failed - retry immediately with current value
        generate_id_loop(Epoch, BaseId, AtomicRef, CurrentVal)
end
```

### Distributed Deployment
Each Erlang node should use a unique `worker` ID:

```erlang
%% Node 1
snow:init(1640995200000, 0, 1).

%% Node 2  
snow:init(1640995200000, 0, 2).

%% Node 3
snow:init(1640995200000, 0, 3).
```

## Testing

```bash
# Run unit tests
rebar3 ct

# Run performance benchmarks
make bench

# Run with custom compiler optimizations
rebar3 as bench compile
```

## Error Handling

- **Clock backwards**: Throws `{clock_backwards, OldTime, NewTime}` 
- **Invalid configuration**: Validates region/worker bounds at initialization
- **Sequence exhaustion**: Automatically waits for next millisecond

## License

MIT License - see LICENSE file for details.

## Contributing

Contributions welcome! Please ensure:
- All tests pass (`rebar3 ct`)
- Performance benchmarks show no regression
- Code follows existing style conventions