-module(bkfw_mcu).
-author('jean.parpaillon@free.fr').

-export([start_link/1]).

-export([loop/1]).

start_link(Idx) ->
    Pid = spawn_link(?MODULE, loop, [Idx]),
    {ok, Pid}.

loop(Idx) ->
    receive
	_ ->
	    loop(Idx)
    end.
