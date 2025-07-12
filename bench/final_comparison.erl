#!/usr/bin/env escript

-mode(compile).

main(_) ->
    io:format("Final Performance Comparison~n"),
    io:format("============================~n~n"),
    
    %% Add path for compiled modules
    code:add_path("_build/default/lib/snow/ebin"),
    code:add_path("3rd/esnowflake/_build/default/lib/esnowflake/ebin"),
    code:add_path("3rd/petrkozorezov_erl_snowflake/_build/default/lib/erl_snowflake/ebin"),
    
    %% Test with different scenarios
    io:format("Single Process (100K IDs):~n"),
    test_single_process(),
    
    io:format("~nModerate Concurrency (10 processes x 10K IDs):~n"),
    test_moderate_concurrency(),
    
    io:format("~nHigh Concurrency (20 processes x 5K IDs):~n"),
    test_high_concurrency(),
    
    io:format("~nExtreme Concurrency (50 processes x 2K IDs):~n"),
    test_extreme_concurrency(),
    
    io:format("~nBenchmark completed.~n").

test_single_process() ->
    %% Snow
    snow:init(1640995200000, 0, 0),
    {SnowTime, _} = timer:tc(fun() ->
        [snow:next_id() || _ <- lists:seq(1, 100000)]
    end),
    
    %% esnowflake
    application:ensure_all_started(esnowflake),
    {EsnowTime, _} = timer:tc(fun() ->
        [esnowflake:generate_id() || _ <- lists:seq(1, 100000)]
    end),
    
    %% petrkozorezov
    {PetrTime, _} = timer:tc(fun() ->
        [erl_snowflake:generate() || _ <- lists:seq(1, 100000)]
    end),
    
    io:format("  Snow:         ~.2f M IDs/sec~n", [100 / (SnowTime / 1000000)]),
    io:format("  esnowflake:   ~.2f M IDs/sec~n", [100 / (EsnowTime / 1000000)]),
    io:format("  petrkozorezov: ~.2f M IDs/sec~n", [100 / (PetrTime / 1000000)]).

test_moderate_concurrency() ->
    test_concurrent(10, 10000).

test_high_concurrency() ->
    test_concurrent(20, 5000).

test_extreme_concurrency() ->
    test_concurrent(50, 2000).

test_concurrent(Procs, IdsPerProc) ->
    TotalIds = Procs * IdsPerProc,
    
    %% Snow
    {SnowTime, _} = timer:tc(fun() ->
        concurrent_test(fun snow:next_id/0, Procs, IdsPerProc)
    end),
    
    %% esnowflake
    {EsnowTime, _} = timer:tc(fun() ->
        concurrent_test(fun esnowflake:generate_id/0, Procs, IdsPerProc)
    end),
    
    %% petrkozorezov - with error handling
    {PetrTime, ValidCount} = timer:tc(fun() ->
        Self = self(),
        Pids = [spawn_link(fun() ->
            Ids = lists:foldl(fun(_, Acc) ->
                try
                    [erl_snowflake:generate() | Acc]
                catch
                    _:_ -> Acc  % Skip errors
                end
            end, [], lists:seq(1, IdsPerProc)),
            Self ! {done, length(Ids)}
        end) || _ <- lists:seq(1, Procs)],
        
        lists:sum([receive {done, Count} -> Count end || _ <- Pids])
    end),
    
    io:format("  Snow:         ~.2f M IDs/sec~n", [TotalIds / (SnowTime / 1000000) / 1000]),
    io:format("  esnowflake:   ~.2f M IDs/sec~n", [TotalIds / (EsnowTime / 1000000) / 1000]),
    io:format("  petrkozorezov: ~.2f M IDs/sec (~p% success)~n", 
              [ValidCount / (PetrTime / 1000000) / 1000, round(ValidCount * 100 / TotalIds)]).

concurrent_test(GenFun, Procs, IdsPerProc) ->
    Self = self(),
    Pids = [spawn_link(fun() ->
        Ids = [GenFun() || _ <- lists:seq(1, IdsPerProc)],
        Self ! {done, length(Ids)}
    end) || _ <- lists:seq(1, Procs)],
    
    lists:sum([receive {done, Count} -> Count end || _ <- Pids]).