-module(rc_process_vnode).
-behaviour(riak_core_vnode).

-include("rc.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-ignore_xref([
             start_vnode/1
             ]).

-record(state, {partition :: binary(),
                pids      :: orddict:orddict(), % pid to name
                names     :: orddict:orddict()  % name to pid
               }).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state{partition=Partition,
                pids=[],
                names=[]}}.

handle_command({Name, start}, _Sender, State) ->
    case orddict:is_key(Name, State#state.names) of
        true ->
            Res = {already_exists, Name},
            ?PRINT(Res),
            {reply, Res, State};
        false ->
            {ok, Pid} = rc_sup:start_process(Name),
            Pids = orddict:store(Pid, Name, State#state.pids),
            Names = orddict:store(Name, Pid, State#state.names),
            link(Pid), % see handle_exit/3
            Res = ok,
            ?PRINT(Res),
            {reply, Res, State#state{pids=Pids, names=Names}}
    end;

handle_command({Name, stop}, _Sender, State) ->
    Res = case orddict:find(Name, State#state.names) of
              error -> {error, unknown_process};
              {ok, Pid} -> rc_process:stop(Pid)
          end,
    ?PRINT(Res),
    {reply, Res, State};

handle_command({Name, ping}, _Sender, State) ->
    Res = case orddict:find(Name, State#state.names) of
              error -> {error, unknown_process};
              {ok, Pid} -> rc_process:ping(Pid)
          end,
    ?PRINT(Res),
    {reply, Res, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(Pid, Reason, State) ->
    ?PRINT({process_died, Reason}),
    Name = orddict:fetch(Pid, State#state.pids),
    Names = orddict:erase(Name, State#state.names),
    Pids = orddict:erase(Pid, State#state.pids),
    {noreply, State#state{pids=Pids, names=Names}}.

terminate(_Reason, _State) ->
    ok.
