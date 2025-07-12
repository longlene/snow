#!/usr/bin/env escript

-mode(compile).

main(_) ->
    io:format("Comprehensive Snowflake ID Benchmark~n"),
    io:format("=====================================~n~n"),
    
    %% Test our snow implementation
    test_snow_impl(),
    
    %% Test esnowflake
    test_esnowflake(),
    
    %% Test petrkozorezov erl_snowflake
    test_petrkozorezov(),
    
    io:format("~nBenchmark completed.~n").

test_snow_impl() ->
    io:format("Testing Snow (our implementation)...~n"),
    try
        %% Add path for our compiled beam files
        code:add_path("_build/default/lib/snow/ebin"),
        
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

test_esnowflake() ->
    io:format("~nTesting esnowflake...~n"),
    try
        %% Add path for esnowflake
        code:add_path("3rd/esnowflake/_build/default/lib/esnowflake/ebin"),
        
        %% Start esnowflake
        application:ensure_all_started(esnowflake),
        
        %% Single-thread test
        {Time1, _} = timer:tc(fun() ->
            [esnowflake:generate_id() || _ <- lists:seq(1, 100000)]
        end),
        Rate1 = round(100000 / (Time1 / 1000000)),
        io:format("  Single-thread: ~p IDs/sec~n", [Rate1]),
        
        %% Concurrent test
        {Time2, _} = timer:tc(fun() ->
            Self = self(),
            Pids = [spawn_link(fun() ->
                Ids = [esnowflake:generate_id() || _ <- lists:seq(1, 5000)],
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
    io:format("~nTesting petrkozorezov erl_snowflake...~n"),
    try
        %% Add path for erl_snowflake
        code:add_path("3rd/petrkozorezov_erl_snowflake/_build/default/lib/erl_snowflake/ebin"),
        
        %% Initialize - this implementation doesn't have an init function,
        %% it uses compile-time configuration
        
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