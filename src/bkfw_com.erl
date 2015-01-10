-module(bkfw_com).
-author('jean.parpaillon@lizenn.com').

-behaviour(gen_server).

-include("bkfw.hrl").

-export([start_link/0,
	 stop/1,
	 send/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {owner                 :: pid(),
		com                   :: term(),
		port    = undefined   :: port(),
		data    = <<>>        :: binary(),
		msg     = undefined   :: msg()}).

%%%
%%% API
%%%
start_link() ->
    gen_server:start_link(?MODULE, self(), []).

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
-spec init(pid()) -> {ok, term()} | {error, term()} | ignore.
init(Owner) ->
    case application:get_env(bkfw, com) of
	undefined ->
	    {stop, {missing_parameter, com}};
	{ok, Com} ->
	    ?info("Opening com port: ~p~n", [Com]),
	    case cereal:open_tty(Com) of
		{ok, Fd} ->
		    Port = open_port({fd, Fd, Fd}, [binary, stream, {line, 80}]),
		    {ok, #state{owner=Owner, com=Fd, port=Port}};
		{error, Err} ->
		    ?error("Error opening com port: ~p~n", [Err]),
		    {stop, {error, Err}}
	    end
    end.

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
handle_call({To, Msg}, {Pid, _Tag}, #state{owner=Pid, port=Port}=S) ->
    bkfw_mutex:wait(),
    Bin = ["0x", io_lib:format("~2.16.0b", [To]), " ", Msg, "\r\n"],
    Port ! {self(), {command, iolist_to_binary(Bin)}},
    {reply, ok, S};

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
handle_info({Port, {data, {noeol, Bin}}}, #state{port=Port, data=Acc}=S) ->
    {noreply, S#state{data= << Acc/binary, Bin/binary >>}};

handle_info({Port, {data, {eol, Bin}}}, #state{msg=Msg, owner=Owner, port=Port, data=Acc}=S) ->
    case bkfw_parser:parse(<< Acc/binary, Bin/binary >>, Msg) of
	{ok, Msg2} ->
	    Owner ! {msg, Msg2},
	    bkfw_mutex:signal(),
	    {noreply, S#state{msg=undefined, data= <<>>}};
	{more, Msg2} ->
	    {noreply, S#state{msg=Msg2, data= <<>>}};
	{error, Err} ->
	    Owner ! {error, Err},
	    bkfw_mutex:signal(),
	    {noreply, S#state{data= <<>>}}
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
