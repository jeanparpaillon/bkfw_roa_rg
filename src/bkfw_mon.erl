-module(bkfw_mon).
-author('jean.parpaillon@free.fr').

-include("bkfw.hrl").

%%%
%%% API
%%%
-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(LOOP_TIME, 60000).

%%%
%%% API
%%%
start_link() ->
    spawn_link(?MODULE, init, ?LOOP_TIME).

%%%
%%% Private
%%%
init(Time) ->
    loop(Time).

loop(Time) ->
    receive
	_ ->
	    loop(Time)
    end.
