-module(rc).
-include("rc.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         start/1,
         stop/1,
         ping/1
        ]).

%% Public API

%% @doc Starts a named process
start(Name) ->
    rc_coordinator:do(?PROCESS_VMASTER, {Name, start}, ?N, ?W).

%% @doc Stops a named process, asynchronously
stop(Name) ->
    rc_coordinator:do(?PROCESS_VMASTER, {Name, stop}, ?N, 0).

%% @doc Pings a process to make sure communication is functional
ping(Name) ->
    rc_coordinator:do(?PROCESS_VMASTER, {Name, ping}, ?N, ?R).
