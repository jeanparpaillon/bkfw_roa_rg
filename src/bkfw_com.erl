-module(bkfw_com).
-author('jean.parpaillon@lizenn.com').

-behaviour(gen_server).

-include("bkfw.hrl").

-export([start_link/1,
	 stop/1,
	 send/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-define(TRACE, "/tmp/bkfw").
-record(state, {owner                 :: pid(),
		com                   :: term(),
		port    = undefined   :: port(),
		data    = <<>>        :: binary(),
		msg     = undefined   :: msg(),
		crlf,
		trace}).

%%%
%%% API
%%%
start_link(Dev) ->
    gen_server:start_link(?MODULE, Dev, []).

stop(Com) ->
    ?debug("Stopping COM~n", []),
    gen_server:call(Com, stop).

-spec send(Com :: pid(), To :: integer(), Msg :: iolist()) -> {ok, Reply :: term()} | {error, Err :: term()}.
send(Com, To, Msg) when is_integer(To) ->
    gen_server:call(Com, {To, Msg}).

%%%
%%% gen_server callbacks
%%%

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init(Com :: string() | undefined) -> {ok, term()} | {error, term()} | ignore.
init(undefined) ->
    ?error("COM port undefined"),
    {stop, {undefined_com}};
init([]) ->
    ?error("COM port undefined"),
    {stop, {undefined_com}};
init(Com) when is_list(Com) ->
    ?info("Opening com port: ~p", [Com]),
    Trace = case application:get_env(bkfw, debug, true) of
		true -> 
		    Tracepath = get_trace_path(0),
		    {ok, Dev} = file:open(Tracepath, [append]),
		    file:write(Dev, ["[COM] Reopening COM port\n"]),
		    Dev;
		false -> undefined
	    end,
    case cereal:open_tty(Com) of
	{ok, Fd} ->
	    Port = open_port({fd, Fd, Fd}, [binary, stream, {line, 80}]),
	    {ok, #state{com=Fd, port=Port, trace=Trace, crlf=$\n}};
	{error, Err} ->
	    ?error("Error opening com port: ~p", [Err]),
	    {stop, {error, Err}}
    end;
init(_Args) ->
    ?error("Invalid args: ~p", [_Args]),
    {stop, {error, badarg}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({To, Msg}, {Pid, _Tag}, #state{port=Port, trace=Trace}=S) ->
    debug_com(Trace, "[COM] Send command\n"),
    Bin = ["0x", io_lib:format("~2.16.0b", [To]), " ", Msg, $\r, $\n],
    debug_com(Trace, ["[RPI -> CPU] ", Bin]),
    Port ! {self(), {command, iolist_to_binary(Bin)}},
    {reply, ok, S#state{owner=Pid}};

handle_call(stop, _From, S) ->
    {stop, stop, S};

handle_call(_, _, S) ->
    {reply, {error, invalid_call}, S}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({Port, {data, {_, Bin}}}, #state{port=Port, owner=undefined}=S) ->
    ?debug("Garbage data: ~p~n", [Bin]),
    {noreply, S#state{data= <<>>, msg=undefined}};
handle_info({Port, {data, {noeol, Bin}}}, #state{port=Port, data=Acc, trace=Trace}=S) ->
    debug_com(Trace, ["[RPI <- CPU] ", Bin]),
    {noreply, S#state{data= << Acc/binary, Bin/binary >>}};

handle_info({Port, {data, {eol, Bin}}}, #state{port=Port, crlf=$\n, trace=Trace}=S) ->
    debug_com(Trace, ["[RPI <- CPU] ", Bin, "\r"]),
    {noreply, S#state{data=Bin, crlf=$\r}};

handle_info({Port, {data, {eol, Bin}}}, #state{msg=Msg, owner=Owner, crlf=$\r,
					       port=Port, data=Data, trace=Trace}=S) ->
    debug_com(Trace, ["[RPI <- CPU] ", Bin, "\n"]),
    case bkfw_parser:parse(<< Data/binary, Bin/binary, $\r, $\n >>, Msg) of
	{ok, Msg2, Rest} ->
	    debug_com(Trace, io_lib:format("[COM] Answer received: ~p ! ~p\n", [Owner, Msg2])),
	    Owner ! {msg, Msg2},
	    {noreply, S#state{owner=undefined, msg=undefined, data=Rest, crlf=$\n}};
	{more, Msg2, Rest} ->
	    {noreply, S#state{msg=Msg2, data=Rest, crlf=$\n}};
	{error, Err, Rest} ->
	    Owner ! {error, Err},
	    {noreply, S#state{owner=undefined, msg=undefined, data=Rest, crlf=$\n}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{com=Fd, port=Port}=_S) ->
    Port ! {self(), close},
    receive
	_ -> 
	    cereal:close_tty(Fd)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% Priv
%%%
debug_com(undefined, _) -> ok;
debug_com(Dev, Bytes) -> 
    Now = erlang:now(),
    {{_Y,_M,_D},{H,M,S}} = calendar:now_to_universal_time(Now),
    {_,_,MS} = Now,
    file:write(Dev, [io_lib:format("[~p:~p:~p.~p]", [H,M,S,MS]), Bytes]).

get_trace_path(I) ->
    Filename = io_lib:format("~s-~b.trace", [?TRACE, I]),
    case filelib:is_file(Filename) of
	true -> get_trace_path(I+1);
	false -> Filename
    end.
