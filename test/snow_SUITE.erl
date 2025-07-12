-module(snow_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         all/0
        ]).

-export([
         test_init/1,
         test_generate_id/1,
         test_generate_multiple_ids/1,
         test_decode_id/1,
         test_uniqueness/1,
         test_sequential/1,
         test_concurrent_generation/1,
         test_concurrent_generation_batch/1,
         test_concurrent_generation_huge/1,
         test_multi_worker_basic/1,
         test_multi_worker_uniqueness/1,
         test_multi_worker_concurrent/1,
         test_worker_info/1
        ]).

suite() -> [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    application:start(snow),
    Config.

end_per_suite(_Config) ->
    application:stop(snow),
    ok.

all() ->
    [test_init,
     test_generate_id,
     test_generate_multiple_ids,
     test_decode_id,
     test_uniqueness,
     test_sequential,
     test_concurrent_generation,
     test_concurrent_generation_batch,
     test_concurrent_generation_huge,
     test_multi_worker_basic,
     test_multi_worker_uniqueness,
     test_multi_worker_concurrent,
     test_worker_info].

test_init(_Config) ->
    %% Test initialization with custom config
    ok = snow:init(1600000000000, 5, 10),
    Info = snow:info(),
    1600000000000 = maps:get(epoch, Info),
    5 = maps:get(region, Info),
    10 = maps:get(worker, Info),
    ok.

test_generate_id(_Config) ->
    Id = snow:next_id(),
    true = is_integer(Id),
    true = Id > 0,
    ok.

test_generate_multiple_ids(_Config) ->
    Ids = snow:next_ids(100),
    100 = length(Ids),
    %% All should be unique
    100 = length(lists:usort(Ids)),
    ok.

test_decode_id(_Config) ->
    ok = snow:init(1640995200000, 7, 31),
    Id = snow:next_id(),
    Decoded = snow:decode_id(Id),

    7 = maps:get(region, Decoded),
    31 = maps:get(worker, Decoded),
    true = is_integer(maps:get(timestamp, Decoded)),
    true = is_integer(maps:get(sequence, Decoded)),
    ok.

test_uniqueness(_Config) ->
    %% Generate many IDs and check uniqueness
    Ids = snow:next_ids(10000),
    UniqueCount = length(lists:usort(Ids)),
    10000 = UniqueCount,
    ok.

test_sequential(_Config) ->
    %% IDs should be sequential
    Id1 = snow:next_id(),
    Id2 = snow:next_id(),
    Id3 = snow:next_id(),

    true = Id2 > Id1,
    true = Id3 > Id2,
    ok.

test_concurrent_generation(_Config) ->
    %% Test concurrent ID generation
    NumProcesses = 100,
    IdsPerProcess = 10000,

    Parent = self(),
    Pids = [spawn_link(fun() ->
                               Ids = [snow:next_id() ||_ <- lists:seq(1, IdsPerProcess)],
                               Parent ! {self(), Ids}
                       end) || _ <- lists:seq(1, NumProcesses)],

    AllIds = lists:foldl(fun(Pid, Acc) ->
                                 receive
                                     {Pid, Ids} -> Ids ++ Acc
                                 after 5000 ->
                                         ct:fail("Timeout waiting for process ~p", [Pid])
                                 end
                         end, [], Pids),

    TotalIds = NumProcesses * IdsPerProcess,
    TotalIds = length(AllIds),

    %% All IDs should be unique
    TotalIds = length(lists:usort(AllIds)),
    ok.

test_concurrent_generation_batch(_Config) ->
    NumProcesses = 100,
    IdsPerProcess = 10000,

    Parent = self(),
    Pids = [spawn_link(fun() ->
                               Ids = snow:next_ids(IdsPerProcess),
                               Parent ! {self(), Ids}
                       end) || _ <- lists:seq(1, NumProcesses)],

    AllIds = lists:foldl(fun(Pid, Acc) ->
                                 receive
                                     {Pid, Ids} -> Ids ++ Acc
                                 after 5000 ->
                                         ct:fail("Timeout waiting for process ~p", [Pid])
                                 end
                         end, [], Pids),

    TotalIds = NumProcesses * IdsPerProcess,
    TotalIds = length(AllIds),

    %% All IDs should be unique
    TotalIds = length(lists:usort(AllIds)),
    ok.


test_concurrent_generation_huge(_Config) ->
    NumProcesses = 10,
    IdsPerProcess = 80000,

    Parent = self(),
    Pids = [spawn_link(fun() ->
                               Ids = snow:next_ids(IdsPerProcess),
                               Parent ! {self(), Ids}
                       end) || _ <- lists:seq(1, NumProcesses)],

    AllIds = lists:foldl(fun(Pid, Acc) ->
                                 receive
                                     {Pid, Ids} -> Ids ++ Acc
                                 after 10000 ->
                                         ct:fail("Timeout waiting for process ~p", [Pid])
                                 end
                         end, [], Pids),

    TotalIds = NumProcesses * IdsPerProcess,
    TotalIds = length(AllIds),

    %% All IDs should be unique
    TotalIds = length(lists:usort(AllIds)),
    ok.

%% ============================================================================
%% Multi-Worker Tests  
%% ============================================================================

test_multi_worker_basic(_Config) ->
    %% Test basic multi-worker functionality
    Worker1 = snow:start_worker(1640995200000, 1, 2),
    Worker2 = snow:start_worker(1640995200000, 1, 3),
    Worker3 = snow:start_worker(1640995200000, 2, 1),
    
    %% Generate IDs from different workers
    Id1 = snow:next_id(Worker1),
    Id2 = snow:next_id(Worker2),
    Id3 = snow:next_id(Worker3),
    
    %% All IDs should be different
    true = (Id1 =/= Id2) and (Id2 =/= Id3) and (Id1 =/= Id3),
    
    %% Decode and verify worker information
    #{region := 1, worker := 2} = snow:decode_id(Id1),
    #{region := 1, worker := 3} = snow:decode_id(Id2),
    #{region := 2, worker := 1} = snow:decode_id(Id3),
    
    %% Test batch generation
    Ids = snow:next_ids(Worker1, 5),
    5 = length(Ids),
    5 = length(lists:usort(Ids)), % All should be unique
    ok.

test_multi_worker_uniqueness(_Config) ->
    %% Test uniqueness across multiple workers generating many IDs
    Workers = [snow:start_worker(1640995200000, R, W) 
               || R <- lists:seq(0, 3), W <- lists:seq(0, 3)],
    
    %% Generate 100 IDs from each worker
    AllIds = lists:foldl(fun(Worker, Acc) ->
                                 Ids = snow:next_ids(Worker, 100),
                                 Ids ++ Acc
                         end, [], Workers),
    
    TotalIds = length(AllIds),
    UniqueIds = length(lists:usort(AllIds)),
    TotalIds = UniqueIds, % All IDs should be unique
    ok.

test_multi_worker_concurrent(_Config) ->
    %% Test concurrent generation from multiple workers
    NumWorkers = 10,
    IdsPerWorker = 1000,
    
    %% Create workers with different region/worker combinations
    Workers = [snow:start_worker(1640995200000, R rem 4, W rem 16) 
               || {R, W} <- [{I div 16, I rem 16} || I <- lists:seq(0, NumWorkers-1)]],
    
    Parent = self(),
    Pids = [spawn_link(fun() ->
                               Ids = snow:next_ids(Worker, IdsPerWorker),
                               Parent ! {self(), Ids}
                       end) || Worker <- Workers],
    
    AllIds = lists:foldl(fun(Pid, Acc) ->
                                 receive
                                     {Pid, Ids} -> Ids ++ Acc
                                 after 10000 ->
                                         ct:fail("Timeout waiting for process ~p", [Pid])
                                 end
                         end, [], Pids),
    
    TotalIds = NumWorkers * IdsPerWorker,
    TotalIds = length(AllIds),
    
    %% All IDs should be unique
    TotalIds = length(lists:usort(AllIds)),
    ok.

test_worker_info(_Config) ->
    %% Test worker_info functionality
    Epoch = 1640995200000,
    Region = 5,
    Worker = 10,
    
    WorkerHandle = snow:start_worker(Epoch, Region, Worker),
    Info = snow:worker_info(WorkerHandle),
    
    %% Verify info contents
    Epoch = maps:get(epoch, Info),
    Region = maps:get(region, Info),
    Worker = maps:get(worker, Info),
    
    Bits = maps:get(bits, Info),
    41 = maps:get(timestamp, Bits),
    4 = maps:get(region, Bits),
    6 = maps:get(worker, Bits),
    12 = maps:get(sequence, Bits),
    ok.
