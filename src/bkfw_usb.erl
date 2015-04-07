-module(bkfw_usb).
-author('jean.parpaillon@lizenn.com').

-include("bkfw.hrl").

% API
-export([start_link/0]).

% Internal
-export([init/0]).

-define(FSM, ?MODULE).
-define(TIMEOUT, 1000).

%%%
%%% API
%%%
start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    register(?FSM, Pid),
    {ok, Pid}.

init() ->
    ?info("Starting USB monitor", []),
    case bkfw_com:start_link(application:get_env(bkfw, usbtty, undefined)) of
	{ok, Com} ->
	    loop(Com);
	ignore ->
	    ?error("Error starting USB-serial port: ignore", []),
	    ok;
	{error, Err} -> 
	    ?error("Error starting USB-serial port: ~p", [Err]),
	    ok
    end.

loop(Com) ->
    receive
	stop -> stop(Com);
	_ -> loop(Com)
    end.

stop(Com) ->
    bkfw_com:stop(Com).
