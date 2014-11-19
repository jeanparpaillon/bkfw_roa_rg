-module(bkfw_srv).
-author('jean.parpaillon@lizenn.com').

-behaviour(gen_fsm).

-include("bkfw.hrl").

% API
-export([start_link/0,
	 command/3]).

% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

% states
-export([wait_cmd/3,
	 wait_answer/3,
	 max_queue/3]).

-define(FSM, ?MODULE).
-define(TIMEOUT, 1000).
-define(MAX_QUEUE, 100).

-record(state, {
	  com        :: pid(),
	  req        :: queue:queue(),  % pending request
	  ans        :: ets:tid(),      % pending answers, per MCU
	  max_queue  :: integer()
	 }).

%%%
%%% API
%%%
start_link() ->
    gen_fsm:start_link({local, ?FSM}, ?MODULE, [], []).

-spec command(Idx :: integer(), Cmd :: atom(), Args :: list()) -> {ok, term()} | {error, term()}.
command(Idx, Cmd, Args) when is_integer(Idx), is_atom(Cmd) ->
    CmdName = string:to_upper(atom_to_list(Cmd)),
    Timeout = application:get_env(bkfw, timeout, ?TIMEOUT),
    ArgsStr = case Args of
		  [] -> "";
		  _ -> [" ", Args]
	      end,
    try gen_fsm:sync_send_event(?FSM, {cmd, Idx, [CmdName, ArgsStr]}) of
	{ok, Ref} ->
	    receive
		{error, Err} -> {error, Err};
		{Ref, Ret} -> {ok, Ret};
		Ret -> {error, {unexpected, Ret}}
	    after 
		Timeout ->
		    gen_fsm:send_all_state_event(?FSM, {flush, Idx}),
		    {error, timeout}
	    end;
	{error, Err} ->
	    {error, Err}
    catch _:Err ->
	    {error, Err}
    end.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ?info("Starting command server~n", []),
    case bkfw_com:start_link() of
	{ok, Com} ->
	    {ok, wait_cmd, #state{com=Com, 
				  req=queue:new(), 
				  ans=ets:new(ans, []),
				  max_queue=application:get_env(bkfw, max_queue, ?MAX_QUEUE)}};
	ignore ->
	    ignore;
	{error, Err} ->
	    {error, Err}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event({flush, Idx}, _, #state{ans=Tid}=State) ->
    true = ets:insert(Tid, {Idx, queue:new()}),
    {next_state, wait_cmd, State};
handle_event(stop, _, State) ->
    {stop, normal, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(stop, _, _, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info({msg, Msg}, wait_cmd, State) ->
    ?debug("Unexpected message: ~p~n", [Msg]),
    {next_state, wait_cmd, State};

handle_info({msg, {Idx, _, _}=Msg}, StateName, #state{ans=Ans}=S) ->
    case ets:lookup(Ans, Idx) of
	[] ->
	    ?debug("Unexpected answer: ~p~n", [Msg]),
	    {next_state, StateName, S};
	[{_, Q}] ->
	    case queue:out(Q) of
		{{value, {Pid, Tag}}, Q2} ->
		    Pid ! {Tag, Msg},
		    ets:insert(Ans, {Idx, Q2}),
		    {NextState, S2} = send_pending(S),
		    {next_state, NextState, S2};
		{empty, _} ->
		    ?debug("Nobody to send answer to...~n", []),
		    {stop, unexpected, S}
	    end
    end;

handle_info({error, Err}, StateName, State) ->
    ?debug("Bad message: ~s~n", [Err]),
    {next_state, StateName, State};

handle_info(Info, StateName, State) ->
    ?debug("Invalid message: ~p~n", [Info]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, #state{com=Com}=_State) ->
    bkfw_com:stop(Com),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

wait_cmd({cmd, Idx, Msg}, {_, Tag}=From, S) ->
    send_cmd(From, Idx, Msg, S),
    {reply, {ok, Tag}, wait_answer, S}.

wait_answer({cmd, Idx, Msg}, {_, Tag}=From, #state{req=Q, max_queue=Max}=S) ->
    Q2 = queue:in({From, Idx, Msg}, Q),
    case queue:len(Q) of 
	L when L >= Max ->
	    {reply, {ok, Tag}, max_queue, S#state{req=Q2}};
	_ ->
	    {reply, {ok, Tag}, wait_answer, S#state{req=Q2}}
    end.

max_queue(_, _From, S) ->
    {reply, {error, max_queue}, max_queue, S}.


%%%
%%% internals
%%%
send_cmd(From, Idx, Msg, #state{com=Com, ans=Ans}) ->
    Q = case ets:lookup(Ans, Idx) of
	    [] -> 
		queue:from_list([From]);
	    [{_, Q1}] -> 
		queue:in(From, Q1)
	end,
    ets:insert(Ans, {Idx, Q}),
    bkfw_com:send(Com, Idx, Msg).    

send_pending(#state{req=Req}=S) ->
    case queue:out(Req) of
	{empty, _} ->
	    {wait_cmd, S};
	{{value, {From, Idx, Msg}}, Req2} ->
	    send_cmd(From, Idx, Msg, S),
	    {wait_answer, S#state{req=Req2}}
    end.
