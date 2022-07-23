-module(bkfw_usb).
-author('jean.parpaillon@lizenn.com').

-include("bkfw.hrl").

%% API
-export([start_link/0, enable/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-record(state, {
		  enable,
		  dev,
		  data,
		  usb_fd,
		  usb
		 }).

%%%
%%% API
%%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enable(Mode) ->
	gen_server:cast(?MODULE, {enable, Mode}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(_) ->
	case application:get_env(bkfw, usbtty, undefined) of
		undefined ->
			{ok, #state{}};
		UsbDev ->
			{ok, #state{dev=UsbDev}}
	end.

handle_call(_Call, _From, S) ->
    {reply, ok, S}.


handle_cast({enable, true}, #state{dev=Dev} = S) ->
    ?info("Starting USB monitor", []),
	case open_com_port(Dev) of
		{ok, UsbFd, UsbPort} ->
			?info("Opened USB serial port: ~s", [Dev]),
			bkfw_com:subscribe(),
			{noreply, S#state{usb_fd=UsbFd, usb=UsbPort, data= <<>>}};
		{error, enoent} ->
			?info("No such USB port, ignoring: ~p", [Dev]),
			{noreply, #state{}};
		{error, UsbErr} ->
			?error("Error opening port ~p: ~p", [Dev, UsbErr]),
			{noreply, #state{}}
	end;

handle_cast({enable, false}, #state{usb=undefined} = S) ->
	bkfw_com:unsubscribe(),
	{noreply, S#state{usb_fd=undefined, usb=undefined}};

handle_cast({enable, false}, #state{usb_fd=UsbFd, usb=UsbPort} = S) ->
	bkfw_com:unsubscribe(),
	UsbPort ! {self(), close},
	receive	_ -> cereal:close_tty(UsbFd)
	after 500 -> ok
	end,
	{noreply, S#state{usb_fd=undefined, usb=undefined}};

handle_cast(_Cast, S) ->
    {noreply, S}.


% COM -> USB
handle_info({data, Data}, #state{ usb=Port }=S) ->
	Port ! {self(), {command, Data}},
	{noreply, S};

handle_info({Usb, {data, {noeol, Bin}}}, #state{usb=Usb, data=Acc}=S) ->
	% Accumulate until eol
	{noreply, S#state{ data= << Acc/binary, Bin/binary >>}};

% USB -> COM
handle_info({Usb, {data, {eol, Bin}}}, #state{usb=Usb, data=Acc}=S) ->
	bkfw_com:send(<< Acc/binary, Bin/binary, $\r, $\n >>),
	{noreply, S#state{ data= <<>> }}.


terminate(_Reason, #state{usb=undefined}) ->
	ok;

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
