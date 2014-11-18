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
-export([wait_cmd/2,
	 wait_answer/2]).

-define(FSM, ?MODULE).
-define(TIMEOUT, 1000).
-record(state, {
	  com        :: pid(),
	  pending    :: term()    % queue()
	 }).

%%%
%%% API
%%%
start_link() ->
    gen_fsm:start_link({local, ?FSM}, ?MODULE, [], []).

-spec command(Idx :: integer(), Cmd :: atom(), Args :: iolist()) -> {ok, term()} | {error, term()}.
command(Idx, Cmd, Args) when is_integer(Idx), is_atom(Cmd) ->
    Ref = make_ref(),
    CmdName = string:to_upper(atom_to_list(Cmd)),
    Timeout = application:get_env(bkfw, timeout, ?TIMEOUT),
    try gen_fsm:send_event(?FSM, {cmd, {Ref, self()}, Idx, [CmdName, Args]}) of
	ok ->
	    receive
		{error, Err} -> {error, Err};
		{Ref, Ret} -> {ok, Ret};
		Ret -> {error, {unexpected, Ret}}
	    after 
		Timeout ->
		    gen_fsm:send_all_state_event(?FSM, flush),
		    {error, timeout}
	    end
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
	    {ok, wait_cmd, #state{com=Com, pending=queue:new()}};
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
handle_event(flush, _, State) ->
    {next_state, wait_cmd, State#state{pending=queue:new()}};
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

handle_info({msg, Msg}, wait_answer, #state{com=Com, pending=Q}=S) ->
    ?debug("Got answer: ~p~n", [Msg]),
    case queue:out(Q) of
	{{value, {{Ref, Pid}, _, _}}, Q2} ->
	    Pid ! {Ref, Msg},
	    case queue:peek(Q2) of
		empty ->
		    {next_state, wait_cmd, S#state{pending=Q2}};
		{value, {_From, Idx, Msg}} ->
		    bkfw_com:send(Com, Idx, Msg),
		    {next_state, wait_answer, S#state{pending=Q2}}
	    end;
	{empty, _Q} ->
	    ?debug("Nobody to send answer to..."),
	    {stop, unexpected, S}
    end;

handle_info({error, Err}, StateName, State) ->
    ?debug("Bad message: ~p~n", [Err]),
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

wait_cmd({cmd, From, Idx, Msg}, #state{com=Com, pending=Q}=S) ->
    Q2 = queue:in({From, Idx, Msg}, Q),
    bkfw_com:send(Com, Idx, Msg),
    {next_state, wait_answer, S#state{pending=Q2}}.

wait_answer({cmd, From, Idx, Msg}, #state{pending=Q}=S) ->
    Q2 = queue:in({From, Idx, Msg}, Q),
    {next_state, wait_answer, S#state{pending=Q2}}.
