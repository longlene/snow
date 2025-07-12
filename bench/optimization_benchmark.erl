#!/usr/bin/env escript

-mode(compile).

main(_) ->
    io:format("Snow Optimization Benchmark~n"),
    io:format("==========================~n~n"),
    
    %% Add path for compiled modules
    code:add_path("_build/default/lib/snow/ebin"),
    
    %% Test original implementation
    test_original(),
    
    %% Test optimized implementation
    test_optimized(),
    
    %% Test petrkozorezov for comparison
    test_petrkozorezov(),
    
    io:format("~nBenchmark completed.~n").

test_original() ->
    io:format("Testing Original Snow Implementation...~n"),
    try
        %% Initialize
        snow:init(1640995200000, 0, 0),
        
        %% Single-thread test
        {Time1, _} = timer:tc(fun() ->
            [snow:next_id() || _ <- lists:seq(1, 100000)]
        end),
        Rate1 = round(100000 / (Time1 / 1000000)),
        io:format("  Single-thread: ~p IDs/sec~n", [Rate1]),
        
        %% Concurrent test  
        {Time2, _} = timer:tc(fun() ->
            Self = self(),
            Pids = [spawn_link(fun() ->
                Ids = [snow:next_id() || _ <- lists:seq(1, 5000)],
                Self ! {done, Ids}
            end) || _ <- lists:seq(1, 20)],
            
            AllIds = lists:flatten([
                receive {done, Ids} -> Ids end || _ <- Pids
            ]),
            length(AllIds)
        end),
        Rate2 = round(100000 / (Time2 / 1000000)),
        io:format("  Concurrent (20 procs): ~p IDs/sec~n", [Rate2])
        
    catch
        Error:Reason ->
            io:format("  Error: ~p:~p~n", [Error, Reason])
    end.

test_optimized() ->
    io:format("~nTesting Optimized Snow Implementation...~n"),
    try
        %% Initialize
        snow_optimized:init(1640995200000, 0, 0),
        
        %% Single-thread test
        {Time1, _} = timer:tc(fun() ->
            [snow_optimized:next_id() || _ <- lists:seq(1, 100000)]
        end),
        Rate1 = round(100000 / (Time1 / 1000000)),
        io:format("  Single-thread: ~p IDs/sec~n", [Rate1]),
        
        %% Concurrent test  
        {Time2, _} = timer:tc(fun() ->
            Self = self(),
            Pids = [spawn_link(fun() ->
                Ids = [snow_optimized:next_id() || _ <- lists:seq(1, 5000)],
                Self ! {done, Ids}
            end) || _ <- lists:seq(1, 20)],
            
            AllIds = lists:flatten([
                receive {done, Ids} -> Ids end || _ <- Pids
            ]),
            length(AllIds)
        end),
        Rate2 = round(100000 / (Time2 / 1000000)),
        io:format("  Concurrent (20 procs): ~p IDs/sec~n", [Rate2])
        
    catch
        Error:Reason ->
            io:format("  Error: ~p:~p~n", [Error, Reason])
    end.

test_petrkozorezov() ->
    io:format("~nTesting petrkozorezov erl_snowflake (reference)...~n"),
    try
        %% Add path for erl_snowflake
        code:add_path("3rd/petrkozorezov_erl_snowflake/_build/default/lib/erl_snowflake/ebin"),
        
        %% Single-thread test
        {Time1, _} = timer:tc(fun() ->
            [erl_snowflake:generate() || _ <- lists:seq(1, 100000)]
        end),
        Rate1 = round(100000 / (Time1 / 1000000)),
        io:format("  Single-thread: ~p IDs/sec~n", [Rate1]),
        
        %% Concurrent test
        {Time2, _} = timer:tc(fun() ->
            Self = self(),
            Pids = [spawn_link(fun() ->
                Ids = [erl_snowflake:generate() || _ <- lists:seq(1, 5000)],
                Self ! {done, Ids}
            end) || _ <- lists:seq(1, 20)],
            
            AllIds = lists:flatten([
                receive {done, Ids} -> Ids end || _ <- Pids
            ]),
            length(AllIds)
        end),
        Rate2 = round(100000 / (Time2 / 1000000)),
        io:format("  Concurrent (20 procs): ~p IDs/sec~n", [Rate2])
        
    catch
        Error:Reason ->
            io:format("  Error: ~p:~p~n", [Error, Reason])
    end.