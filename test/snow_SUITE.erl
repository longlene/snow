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
         test_concurrent_generation_huge/1
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
     test_concurrent_generation_huge].

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
