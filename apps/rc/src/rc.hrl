-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-define(PROCESS_VMASTER, rc_process_vnode_master).

-define(N, 3).
-define(R, 1).
-define(W, 1).
