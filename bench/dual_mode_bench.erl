#!/usr/bin/env escript

-mode(compile).

main(_) ->
    io:format("Snow Dual Mode Performance Benchmark~n"),
    io:format("====================================~n~n"),
    
    %% Add path for compiled modules
    code:add_path("_build/default/lib/snow/ebin"),
    
    %% Test conservative mode (default)
    test_mode(conservative, 1000000),
    
    io:format("~n"),
    
    %% Test aggressive mode
    test_mode(aggressive, 1000000),
    
    io:format("~nBenchmark completed.~n").

test_mode(Mode, N) ->
    io:format("Testing ~p mode:~n", [Mode]),
    io:format("~s~n", [lists:duplicate(20, $-)]),
    
    %% Initialize with the specified mode
    snow:init(1640995200000, 0, 0, Mode),
    
    %% Verify the mode is set correctly
    #{mode := ActualMode} = snow:info(),
    io:format("Configured mode: ~p~n", [ActualMode]),
    
    %% Warm up
    _ = [snow:next_id() || _ <- lists:seq(1, 1000)],
    
    %% Single thread test
    {Time1, _} = timer:tc(fun() ->
        [snow:next_id() || _ <- lists:seq(1, N)]
    end),
    Rate1 = calculate_rate(N, Time1),
    io:format("Single thread: ~s~n", [format_rate(Rate1)]),
    
    %% Concurrent tests with different process counts
    lists:foreach(fun(Procs) ->
        test_concurrent(Mode, Procs, N div 10)
    end, [10, 20, 50]).

test_concurrent(Mode, Procs, N) ->
    PerProc = N div Procs,
    ActualN = PerProc * Procs,
    
    {Time, _} = timer:tc(fun() ->
        run_concurrent_workers(Procs, PerProc)
    end),
    
    Rate = calculate_rate(ActualN, Time),
    io:format("~p processes: ~s~n", [Procs, format_rate(Rate)]).

run_concurrent_workers(NumProcs, PerProc) ->
    Parent = self(),
    Pids = [spawn_link(fun() ->
                _ = [snow:next_id() || _ <- lists:seq(1, PerProc)],
                Parent ! done
            end) || _ <- lists:seq(1, NumProcs)],
    
    [receive done -> ok end || _ <- Pids],
    ok.

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