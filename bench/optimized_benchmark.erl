#!/usr/bin/env escript

-mode(compile).

main(_) ->
    io:format("Snow CAS Optimization Benchmark~n"),
    io:format("===============================~n~n"),
    
    %% Add path for compiled modules
    code:add_path("_build/default/lib/snow/ebin"),
    code:add_path("3rd/petrkozorezov_erl_snowflake/_build/default/lib/erl_snowflake/ebin"),
    
    %% Test with different concurrency levels
    test_all(1),
    test_all(10),
    test_all(20),
    test_all(50),
    
    io:format("~nBenchmark completed.~n").

test_all(Procs) ->
    io:format("~nTesting with ~p concurrent processes:~n", [Procs]),
    io:format("------------------------------------~n"),
    
    %% Test our optimized implementation
    test_snow(Procs),
    
    %% Test petrkozorezov for comparison
    test_petrkozorezov(Procs).

test_snow(Procs) ->
    io:format("Snow (optimized): "),
    try
        %% Initialize
        snow:init(1640995200000, 0, 0),
        
        %% Concurrent test
        IdsPerProc = 100000 div Procs,
        {Time, _} = timer:tc(fun() ->
            Self = self(),
            Pids = [spawn_link(fun() ->
                Ids = [snow:next_id() || _ <- lists:seq(1, IdsPerProc)],
                Self ! {done, length(Ids)}
            end) || _ <- lists:seq(1, Procs)],
            
            Total = lists:sum([
                receive {done, Count} -> Count end || _ <- Pids
            ]),
            Total
        end),
        Rate = round(100000 / (Time / 1000000)),
        io:format("~p IDs/sec~n", [Rate])
        
    catch
        Error:Reason ->
            io:format("Error: ~p:~p~n", [Error, Reason])
    end.

test_petrkozorezov(Procs) ->
    io:format("petrkozorezov:    "),
    try
        %% Concurrent test
        IdsPerProc = 100000 div Procs,
        {Time, _} = timer:tc(fun() ->
            Self = self(),
            Pids = [spawn_link(fun() ->
                Ids = [try erl_snowflake:generate() catch _:_ -> skip end || _ <- lists:seq(1, IdsPerProc)],
                ValidIds = [Id || Id <- Ids, Id =/= skip],
                Self ! {done, length(ValidIds)}
            end) || _ <- lists:seq(1, Procs)],
            
            Total = lists:sum([
                receive {done, Count} -> Count end || _ <- Pids
            ]),
            Total
        end),
        Rate = round(100000 / (Time / 1000000)),
        io:format("~p IDs/sec~n", [Rate])
        
    catch
        Error:Reason ->
            io:format("Error: ~p:~p~n", [Error, Reason])
    end.