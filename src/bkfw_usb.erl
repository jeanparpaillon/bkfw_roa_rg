-module(bkfw_usb).
-author('jean.parpaillon@lizenn.com').

-include("bkfw.hrl").

% API
-export([start_link/0,
	 get_status/0,
	 set_status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-define(FSM, ?MODULE).
-define(TIMEOUT, 1000).

-record(state, {
	  com,
	  enable = false   :: boolean()
	 }).

%%%
%%% API
%%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_status() ->
    try gen_server:call(?MODULE, get_status) of
	Enable -> Enable
    catch _:_ -> false
    end.

set_status(Enable) ->
    try gen_server:call(?MODULE, {set_status, Enable}) of
	_ -> ok
    catch _:_ -> ok
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(_) ->
    ?info("Starting USB monitor", []),
    case bkfw_com:start_link(application:get_env(bkfw, usbtty, undefined)) of
	{ok, Com} ->
	    {ok, #state{com=Com}};
	{error, Err} -> 
	    ?error("Error starting USB-serial port: ~p", [Err]),
	    {stop, Err}
    end.

handle_call(get_status, _From, S) ->
    {reply, S#state.enable, S};
handle_call({set_status, Enable}, _From, S) ->
    ?debug("Set USB Mode to : ~p", [Enable]),
    {reply, ok, S#state{enable=Enable}};
handle_call(_Call, _From, S) ->
    {reply, ok, S}.

handle_cast(_Cast, S) ->
    {noreply, S}.

handle_info(_Info, S) ->
    {noreply, S}.

terminate(_Reason, #state{com=Com}) ->
    bkfw_com:stop(Com),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
