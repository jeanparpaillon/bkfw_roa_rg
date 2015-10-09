-module(bkfw_srv).
-author('jean.parpaillon@lizenn.com').

-include("bkfw.hrl").

% API
-export([start_link/0,
		 flush/0,
		 wait/0,
		 release/1,
		 command/3,
		 command/5,
		 command/6]).

% Internal
-export([init/0]).

-define(FSM, ?MODULE).
-define(TIMEOUT, 1000).

-type comref() :: {reference(), atom()}.

%%%
%%% API
%%%
start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    register(?FSM, Pid),
    {ok, Pid}.


flush() ->
	?info("Flushing command server", []),
	case wait() of
		{ok, {_, Com}} ->
			bkfw_com:stop(Com);
		{error, _} ->
			ignore
	end.


-spec command(ComRef :: comref(), 
			  Idx :: iolist(), 
			  Cmd :: atom(), Args :: list(), 
			  Timeout :: integer()) -> {ok, term()} | {error, term()}.
command(ComRef, Idx, Cmd, Args, Timeout) ->
	Parser = parse_cmd_ans(Cmd),
	command(ComRef, Idx, Cmd, Args, Timeout, Parser).


-spec command(ComRef :: comref(), 
			  Idx :: iolist(), 
			  Cmd :: atom(), Args :: list(), 
			  Timeout :: integer(),
			  Parser :: fun()) -> {ok, term()} | {error, term()}.
command({_, Com}, Idx, Cmd, Args, Timeout, Parser) ->
    CmdName = string:to_upper(atom_to_list(Cmd)),
    ArgsStr = case Args of
				  [] -> "";
				  _ -> [" ", Args]
			  end,
    Bin = [Idx, " ", [CmdName, ArgsStr], $\r, $\n],
	bkfw_com:raw(Com, Bin),
	wait_answer(Idx, Parser, Com, Timeout).


-spec command(Idx :: integer() | iolist(), Cmd :: atom(), Args :: list()) -> {ok, term()} | {error, term()}.
command(Idx, Cmd, Args) when is_integer(Idx), is_atom(Cmd) ->
	command(["0x", io_lib:format("~2.16.0b", [Idx])], Cmd, Args);
command(Idx, Cmd, Args) ->
	case wait() of
		{ok, ComRef} ->
			Timeout = application:get_env(bkfw, timeout, ?TIMEOUT),
			Ret = command(ComRef, Idx, Cmd, Args, Timeout),
			release(ComRef),
			Ret;
		{error, _} = Err ->
			Err
	end.

-spec wait() -> {ok, comref()} | {error, term()}.
wait() ->
    Ref = make_ref(),
    ?FSM ! {wait, self(), Ref},
	receive
		{com, Com} -> {ok, {Ref, Com}}
	after 1000*300 ->
			?FSM ! {signal, Ref},
			{error, timeout}
	end.

-spec release(term()) -> ok.
release({Ref, _}) ->
	?FSM ! {signal, Ref},
	ok.

%%%
%%% Priv
%%%
wait_answer(Idx, Parser, Com, T) ->
	wait_answer(Idx, Parser, Com, T, undefined).


wait_answer(Idx, Parser, Com, T, SoFar) ->
	receive
		{error, _} = Err ->
			?info("Error waiting answer<1>: ~p", [Err]),
			Err;
		{msg, Data} ->
			case Parser(Data, SoFar) of
				{ok, _} = Ret  ->
					bkfw_com:release(Com),
					Ret;
				{more, Msg, Rest} ->
					bkfw_com:more(Com, Rest),
					wait_answer(Idx, Parser, Com, T, Msg);
				{error, _} = Err ->
					?info("Error waiting answer<2>: ~p", [Err]),
					Err
			end
	after T ->
			?info("Timeout waiting answer", []),
			{error, timeout}
	end.

parse_cmd_ans(Cmd) ->
	fun(Data, SoFar) ->
			case bkfw_parser:parse(Data, SoFar) of
				{ok, Msg, _} -> match_ans(cmd_to_ans(Cmd), Msg);
				{more, Msg, Rest} -> {more, Msg, Rest};
				{error, Err, _} -> {error, Err}
			end
	end.

match_ans('_', Msg) -> {ok, Msg};
match_ans(Ans, {_, Ans, _}=Msg) -> {ok, Msg};
match_ans(E, R) -> 
	?error("<Parse error> expected: ~p received ~p", [E, R]),
	{error, {unexpected, answer}}.

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
