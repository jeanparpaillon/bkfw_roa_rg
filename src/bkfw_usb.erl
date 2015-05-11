-module(bkfw_usb).
-author('jean.parpaillon@lizenn.com').

-include("bkfw.hrl").

% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(FSM, ?MODULE).
-define(TIMEOUT, 1000).

-record(state, {
		  com_fd,
		  usb_fd,
		  com,
		  usb,
		  enable = false   :: boolean()
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
	UsbDev = application:get_env(bkfw, usbtty, undefined),
	case open_com_port(UsbDev) of
		{ok, UsbFd, UsbPort} ->
			ComDev = application:get_env(bkfw, com, undefined),
			case open_com_port(ComDev) of
				{ok, ComFd, ComPort} ->
					{ok, #state{com_fd=ComFd, com=ComPort, usb_fd=UsbFd, usb=UsbPort}};
				{error, ComErr} ->
					?error("Error opening port ~p: ~p", [ComDev, ComErr]),
					{stop, ComErr}
			end;
		{error, UsbErr} ->
			?error("Error opening port ~p: ~p", [UsbDev, UsbErr]),
			{stop, UsbErr}
	end.

handle_call(_Call, _From, S) ->
    {reply, ok, S}.

handle_cast(_Cast, S) ->
    {noreply, S}.


handle_info({From, {data, Bin}}, #state{com=Com, usb=Usb}=S) ->
	To = case From of 
			 Com -> S#state.usb;
			 Usb -> S#state.com
		 end,
	Ans = case Bin of
			  {noeol, Data} -> Data;
			  {eol, Data} -> [Data, $\r, $\n]
		  end,
	To ! {self(), {command, Ans}},
	{noreply, S};

handle_info(_Info, S) ->
	?debug("got ~p", [_Info]),
    {noreply, S}.


terminate(_Reason, #state{com_fd=ComFd, com=ComPort, usb_fd=UsbFd, usb=UsbPort}) ->
	ComPort ! {self(), close},
	receive _ -> cereal:close_tty(ComFd)
	after 500 -> ok
	end,
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
