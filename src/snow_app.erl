-module(snow_app).
-behaviour(application).

-export([start/2, stop/1]).

-ifndef(epoch).
-define(epoch, 1640995200000).
-endif.

start(_StartType, _StartArgs) ->
    %% Initialize snow with application env config
    Epoch = application:get_env(snow, epoch, ?epoch),
    Region = application:get_env(snow, region, 0),
    Worker = application:get_env(snow, worker, 0),
    ok = snow:init(Epoch, Region, Worker),
    snow_sup:start_link().

stop(_State) ->
    ok.

