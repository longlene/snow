-module(snow_bench).
-export([run/0, run/1]).

%% Configuration for different test scenarios
-define(DEFAULT_N, 1000000).
-define(WARMUP_COUNT, 1000).
-define(BATCH_SIZES, [1000, 10000]).
-define(CONCURRENT_SCENARIOS, [
    {system, "System schedulers"},
    {10, "10 processes"},
    {20, "20 processes"}, 
    {50, "50 processes"}
]).

run() ->
    run(?DEFAULT_N).

run(N) ->
    application:ensure_all_started(snow),
    
    io:format("Snow Performance Benchmark - ~p IDs~n", [N]),
    io:format("=========================================~n"),
    
    %% Warm up
    warmup(),
    
    %% Single thread benchmark
    SingleRate = benchmark_single_thread(N),
    
    %% Batch generation benchmarks
    BatchRates = benchmark_batches(N),
    
    %% Concurrent benchmarks
    benchmark_concurrent(N, SingleRate),
    
    %% Summary
    print_summary(SingleRate, BatchRates),
    
    ok.

%% Warm up the system
warmup() ->
    io:format("Warming up...~n"),
    _ = [snow:next_id() || _ <- lists:seq(1, ?WARMUP_COUNT)],
    ok.

%% Single thread benchmark
benchmark_single_thread(N) ->
    io:format("~nSingle Thread Performance:~n"),
    io:format("--------------------------~n"),
    
    {TimeUs, _} = timer:tc(fun() ->
        [snow:next_id() || _ <- lists:seq(1, N)]
    end),
    
    Rate = calculate_rate(N, TimeUs),
    print_benchmark_result("Single thread", TimeUs, Rate, N),
    Rate.

%% Batch generation benchmarks
benchmark_batches(N) ->
    io:format("~nBatch Generation Performance:~n"),
    io:format("-----------------------------~n"),
    
    lists:map(fun(BatchSize) ->
        benchmark_batch(N, BatchSize)
    end, ?BATCH_SIZES).

benchmark_batch(N, BatchSize) ->
    NumBatches = N div BatchSize,
    ActualN = NumBatches * BatchSize,
    
    {TimeUs, _} = timer:tc(fun() ->
        [snow:next_ids(BatchSize) || _ <- lists:seq(1, NumBatches)]
    end),
    
    Rate = calculate_rate(ActualN, TimeUs),
    print_benchmark_result(io_lib:format("Batch (~p per batch)", [BatchSize]), 
                          TimeUs, Rate, ActualN),
    {BatchSize, Rate}.

%% Concurrent benchmarks
benchmark_concurrent(N, SingleRate) ->
    io:format("~nConcurrent Performance:~n"),
    io:format("-----------------------~n"),
    
    lists:foreach(fun({ProcSpec, Description}) ->
        benchmark_concurrent_scenario(N, ProcSpec, Description, SingleRate)
    end, ?CONCURRENT_SCENARIOS).

benchmark_concurrent_scenario(N, system, Description, SingleRate) ->
    NumProcs = erlang:system_info(schedulers),
    benchmark_concurrent_scenario(N, NumProcs, Description, SingleRate);

benchmark_concurrent_scenario(N, NumProcs, Description, SingleRate) ->
    PerProc = N div NumProcs,
    ActualN = PerProc * NumProcs,
    
    {TimeUs, _} = timer:tc(fun() ->
        run_concurrent_workers(NumProcs, PerProc)
    end),
    
    Rate = calculate_rate(ActualN, TimeUs),
    Speedup = Rate / SingleRate,
    
    io:format("~s (~p procs): ~s, ~.2fx speedup~n", 
              [Description, NumProcs, format_rate(Rate), Speedup]).

%% Run concurrent workers
run_concurrent_workers(NumProcs, PerProc) ->
    Parent = self(),
    Pids = [spawn_link(fun() ->
                _ = [snow:next_id() || _ <- lists:seq(1, PerProc)],
                Parent ! done
            end) || _ <- lists:seq(1, NumProcs)],
    
    [receive done -> ok end || _ <- Pids],
    ok.

%% Utility functions
calculate_rate(N, TimeUs) ->
    N * 1000000 / TimeUs.

format_rate(Rate) ->
    if
        Rate >= 1000000 ->
            io_lib:format("~.2f M IDs/sec", [Rate / 1000000]);
        Rate >= 1000 ->
            io_lib:format("~.2f K IDs/sec", [Rate / 1000]);
        true ->
            io_lib:format("~.2f IDs/sec", [Rate])
    end.

print_benchmark_result(Name, TimeUs, Rate, N) ->
    TimeMs = TimeUs / 1000,
    LatencyUs = TimeUs / N,
    
    io:format("~s:~n", [Name]),
    io:format("  Time: ~.3f ms~n", [TimeMs]),
    io:format("  Rate: ~s~n", [format_rate(Rate)]),
    io:format("  Latency: ~.3f μs/ID~n", [LatencyUs]).

print_summary(SingleRate, BatchRates) ->
    io:format("~nPerformance Summary:~n"),
    io:format("===================~n"),
    io:format("Single thread baseline: ~s~n", [format_rate(SingleRate)]),
    
    lists:foreach(fun({BatchSize, BatchRate}) ->
        Speedup = BatchRate / SingleRate,
        io:format("Batch-~p speedup: ~.2fx~n", [BatchSize, Speedup])
    end, BatchRates),
    
    %% Performance classification
    classify_performance(SingleRate).

classify_performance(Rate) ->
    io:format("~nPerformance Classification:~n"),
    if
        Rate >= 2000000 ->
            io:format("🚀 Excellent (>2M IDs/sec)~n");
        Rate >= 1000000 ->
            io:format("✅ Very Good (>1M IDs/sec)~n");
        Rate >= 500000 ->
            io:format("👍 Good (>500K IDs/sec)~n");
        Rate >= 100000 ->
            io:format("⚠️  Fair (>100K IDs/sec)~n");
        true ->
            io:format("❌ Poor (<100K IDs/sec)~n")
    end.