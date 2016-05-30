-module(bkfw_usb).
-author('jean.parpaillon@lizenn.com').

-include("bkfw.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(FSM, ?MODULE).
-define(TIMEOUT, 1000).

-record(state, {
		  usb_fd,
		  usb
		 }).

%%%
%%% API
%%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(_) ->
    ?info("Starting USB monitor", []),
	case application:get_env(bkfw, usbtty, undefined) of
		undefined ->
			{ok, #state{}};
		UsbDev ->
			case open_com_port(UsbDev) of
				{ok, UsbFd, UsbPort} ->
					{ok, #state{usb_fd=UsbFd, usb=UsbPort}};
				{error, enoent} ->
					?info("No such USB port, ignoring: ~p", [UsbDev]),
					{ok, #state{}};
				{error, UsbErr} ->
					?error("Error opening port ~p: ~p", [UsbDev, UsbErr]),
					{stop, UsbErr}
			end
	end.

handle_call(_Call, _From, S) ->
    {reply, ok, S}.

handle_cast(_Cast, S) ->
    {noreply, S}.


handle_info({Usb, {data, Bin}}, #state{usb=Usb}=S) ->
	?debug("Receive data from USB: ~p", [Bin]),
	bkfw_srv:raw(Bin),
	{noreply, S};

handle_info(_Info, S) ->
    {noreply, S}.


terminate(_Reason, #state{usb_fd=UsbFd, usb=UsbPort}) ->
	UsbPort ! {self(), close},
	receive	_ -> cereal:close_tty(UsbFd)
	after 500 -> ok
	end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

open_com_port(Dev) ->
	?debug("Opening COM dev ~p", [Dev]),
	case cereal:open_tty(Dev) of
		{ok, Fd} ->
			Port = open_port({fd, Fd, Fd}, [binary, stream, {line, 80}]),
			{ok, Fd, Port};
		{error, Err} ->
			{error, Err}
	end.
