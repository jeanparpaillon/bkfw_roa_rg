-module(bkfw_srv).
-author('jean.parpaillon@lizenn.com').

-include("bkfw.hrl").

% API
-export([start_link/0,
	 command/3]).

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

-spec command(Idx :: integer(), Cmd :: atom(), Args :: list()) -> {ok, term()} | {error, term()}.
command(Idx, Cmd, Args) when is_integer(Idx), is_atom(Cmd) ->
    CmdName = string:to_upper(atom_to_list(Cmd)),
    Timeout = application:get_env(bkfw, timeout, ?TIMEOUT),
    ArgsStr = case Args of
		  [] -> "";
		  _ -> [" ", Args]
	      end,
    Ref = make_ref(),
    ?FSM ! {wait, self(), Ref},
    receive
	{com, Com} ->
	    bkfw_com:send(Com, Idx, [CmdName, ArgsStr]),
	    wait_answer(Idx, cmd_to_ans(Cmd), Ref, Timeout)
    after Timeout ->
	    ?FSM ! {signal, Ref},
	    {error, timeout}
    end.

wait_answer(Idx, '_', Ref, T) ->
    receive
	{error, Err} ->
	    ?FSM ! {signal, Ref},
	    {error, Err};
	{msg, {Idx, _, _}=Msg} -> 
	    ?FSM ! {signal, Ref},
	    {ok, Msg}
    after T -> 
	    ?FSM ! {signal, Ref},
	    {error, timeout}
    end;    
wait_answer(Idx, Ans, Ref, T) ->
    receive
	{error, Err} ->
	    ?FSM ! {signal, Ref},
	    {error, Err};
	{msg, {Idx, Ans, _}=Msg} -> 
	    ?FSM ! {signal, Ref},
	    {ok, Msg}
    after T -> 
	    ?FSM ! {signal, Ref},
	    {error, timeout}
    end.

init() ->
    ?info("Starting command server", []),
    Dev = application:get_env(bkfw, com, undefined),
    case bkfw_com:start_link(Dev) of
	{ok, Com} ->
	    ?info("Command server started: ~p", [Com]),
	    free(Com);
	ignore ->
	    ?error("Error starting COM port: ignore", []),
	    ok;
	{error, Err} -> 
	    ?error("Error starting COM port: ~p", [Err]),
	    ok
    end.

free(Com) ->
    receive {wait, By, Ref} ->
	    By ! {com, Com},
	    busy(Com, Ref)
    end.

busy(Com, Ref) ->
    Timeout = application:get_env(bkfw, timeout, ?TIMEOUT),
    receive 
	{signal, Ref} ->
	    free(Com)
    after Timeout ->
	    free(Com)
    end.

cmd_to_ans(rcc) -> cc;
cmd_to_ans(scc) -> scc;
cmd_to_ans(rgc) -> gc;
cmd_to_ans(sgc) -> sgc;
cmd_to_ans(rpc) -> pc;
cmd_to_ans(spc) -> spc;
cmd_to_ans(rmode) -> mode;
cmd_to_ans(smode) -> smode;
cmd_to_ans(ra) -> alarms;
cmd_to_ans(rlt) -> lt;
cmd_to_ans(rlc) -> lc;
cmd_to_ans(rit) -> it;
cmd_to_ans(ri) -> i;
cmd_to_ans(rpm) -> pd;
cmd_to_ans(rv) -> v;
cmd_to_ans(rli) -> li;
cmd_to_ans(sli) -> sli;
cmd_to_ans(rlo) -> lo;
cmd_to_ans(slo) -> slo;
cmd_to_ans(rn) -> n;
cmd_to_ans(_) -> '_'.
