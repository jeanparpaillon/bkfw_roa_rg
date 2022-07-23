-module(bkfw_cmd).
-author('jean@parpaillon.info').

-include("bkfw.hrl").

-behaviour(gen_server).

% API
-export([call/3, call/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(TIMEOUT, 5000).

-record(state, {
          from,
          cmd,
		  com       :: pid(),
		  sofar     :: undefined,
          partial
		 }).

%%%
%%% API
%%%
call(Idx, Cmd, Args) ->
    call(Idx, Cmd, Args, ?TIMEOUT).

call(Idx, Cmd, Args, Timeout) ->
    Bin = format_cmd(Idx, Cmd, Args),
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    gen_server:call(Pid, {call, Cmd, Bin}, Timeout).

%%%
%%% gen_server callbacks
%%%
init([]) ->
    bkfw_com:subscribe(),
    {ok, #state{partial= <<>>}}.

handle_call({call, Cmd, Bin}, From, S) ->
    bkfw_com:send(Bin),
    {noreply, S#state{from = From, cmd = Cmd}}.

handle_cast(_Cast, S) ->
	{noreply, S}.

handle_info({data, Data}, #state{ sofar=SoFar, partial=Partial }=S) ->
    case bkfw_parser:parse(<<Partial/binary, Data/binary>>, SoFar) of
        {ok, Msg, _} ->
            case match_ans(cmd_to_ans(S#state.cmd), Msg) of
                true ->
                    gen_server:reply(S#state.from, {ok, Msg}),
                    {stop, normal, S};

                false ->
                    gen_server:reply(S#state.from, {error, {unexpected, answer}}),
                    {stop, normal, S}
            end;
        {more, Msg, Rest} ->
            {noreply, S#state{ sofar=Msg, partial=Rest } };
        {error, _Reason, _} ->
            {noreply, S#state{ sofar=undefined }}
    end;

handle_info(_Info, S) ->
	{noreply, S}.


terminate(_Reason, _S) ->
	ok.


code_change(_OldVsn, S, _Extra) ->
	{ok, S}.


%%%
%%% Priv
%%%
format_cmd(Idx, Cmd, Args) ->
    CmdName = string:to_upper(atom_to_list(Cmd)),
    ArgStr = case Args of
                [] -> "";
                _ -> [" ", Args]
            end,
    [io_lib:format("0x~2.16.0b ", [Idx]), [CmdName, ArgStr], $\r, $\n].

match_ans('_', _Msg) -> true;
match_ans(Ans, {_, Ans, _}) -> true;
match_ans(E, R) -> 
	?error("<Parse error> expected: ~p received ~p", [E, R]),
	false.


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
