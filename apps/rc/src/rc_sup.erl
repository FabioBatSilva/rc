-module(rc_sup).
-behaviour(supervisor).

-include("rc.hrl").

%% API
-export([start_link/0, start_link/1, start_process/1, start_coordinator/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(Sup) ->
    supervisor:start_link({local, Sup}, ?MODULE, [Sup]).

start_process(Name) ->
    supervisor:start_child(rc_process_sup, [Name]).

start_coordinator(VnodeMaster) ->
    supervisor:start_child(rc_coordinator_sup, [VnodeMaster]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    ProcessSup = {rc_process_sup,
                  {?MODULE, start_link, [rc_process_sup]},
                  permanent, 5000, supervisor, [?MODULE]},
    CoordinatorSup = {rc_coordinator_sup,
                      {?MODULE, start_link, [rc_coordinator_sup]},
                      permanent, 5000, supervisor, [?MODULE]},
    ProcessVMaster = {?PROCESS_VMASTER,
                      {riak_core_vnode_master, start_link, [rc_process_vnode]},
                      permanent, 5000, worker, [riak_core_vnode_master]},
    {ok, {{one_for_one, 5, 10}, [ProcessSup,
                                 CoordinatorSup,
                                 ProcessVMaster]}};

init([rc_process_sup]) ->
    Mod = rc_process,
    {ok, {{simple_one_for_one, 0, 1}, [{Mod, {Mod, start_link, []},
                                        temporary, 5000, worker, [Mod]}
                                      ]}};

init([rc_coordinator_sup]) ->
    Mod = rc_coordinator,
    {ok, {{simple_one_for_one, 0, 1}, [{Mod, {Mod, start_link, []},
                                        temporary, 5000, worker, [Mod]}
                                      ]}}.
