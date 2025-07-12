-module(snow_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    %% Empty supervisor - no child processes needed
    %% All state is managed through atomics and persistent_term
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    {ok, {SupFlags, []}}.