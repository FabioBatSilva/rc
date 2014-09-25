-module(rc_process).
-behaviour(gen_server).

-include("rc.hrl").

%% API
-export([start_link/1, stop/1, ping/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {name}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

ping(Pid) ->
    gen_server:call(Pid, ping).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Name]) ->
    ?PRINT({start, Name}),
    {ok, #state{name=Name}}.

handle_call(ping, _From, State) ->
    Name = State#state.name,
    ?PRINT({ping, Name}),
    {reply, {pong, Name}, State}.

handle_cast(stop, State) ->
    Name = State#state.name,
    ?PRINT({stop, Name}),
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
