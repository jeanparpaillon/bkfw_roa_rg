-module(bkfw_srv).
-author('jean.parpaillon@lizenn.com').

-include("bkfw.hrl").

-behaviour(gen_server).

% API
-export([start_link/0,
		 call/2,
		 call/3,
		 command/3,
		 command/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).


-define(TIMEOUT, 5000).

-record(state, {
		  com       :: pid(),
		  current   :: {pid(), reference()}
		 }).

%%%
%%% API
%%%
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


call(Handler, State0) ->
	call(Handler, State0, ?TIMEOUT).


-spec call(Handler :: fun(), State0 :: term(), Timeout :: integer()) -> term().
call(Handler, State0, Timeout) when is_function(Handler)  ->
	Mutex = bkfw_mutex:wait(),
	case gen_server:call(?MODULE, {call, Handler, State0}, Timeout) of
		{ok, Ref} ->
			receive
				{Ref, {error, _}=Err} ->
					?info("Error waiting answer<1>: ~p", [Err]),
					bkfw_mutex:signal(Mutex),
					Err;
				{Ref, {ok, _}=Ok} ->
					bkfw_mutex:signal(Mutex),
					Ok;
				{Ref, ok} ->
					bkfw_mutex:signal(Mutex),
					ok
			after Timeout ->
					bkfw_mutex:signal(Mutex),
					{error, timeout}
			end;
		{error, _}=Err ->
			bkfw_mutex:signal(Mutex),
			Err
	end.


command(Idx, Cmd, Args) ->
	command(Idx, Cmd, Args, ?TIMEOUT).


-spec command(integer(), binary(), list()) -> {ok, term()} | {error, term()}.
command(Idx, Cmd, Args, Timeout) ->
	Fun = fun(init, Com, _) ->
				  CmdName = string:to_upper(atom_to_list(Cmd)),
				  ArgStr = case Args of
							   [] -> "";
							   _ -> [" ", Args]
						   end,
				  Bin = [io_lib:format("0x~2.16.0b ", [Idx]), [CmdName, ArgStr], $\r, $\n],
				  ok = bkfw_com:raw(Com, Bin),
				  {ok, undefined};

			 ({msg, Data}, Com, SoFar) ->
				  case bkfw_parser:parse(Data, SoFar) of
					  {ok, Msg, _} -> 
						  match_ans(cmd_to_ans(Cmd), Msg);
					  {more, Msg, Rest} -> 
						  bkfw_com:more(Com, Rest),
						  {more, Msg};
					  {error, _}=Err ->
						  Err
				  end
		  end,
	call(Fun, undefined, Timeout).


%%%
%%% gen_server callbacks
%%%
init([]) ->
    ?info("Starting command server", []),
    Dev = application:get_env(bkfw, com, undefined),
    case bkfw_com:start_link(Dev) of
		{ok, Com} ->
			?info("Command server started: ~p", [Com]),
			{ok, #state{ com=Com, current=undefined }};
		ignore ->
			?error("Error starting COM port: ignore", []),
			{stop, ignore};
		{error, Err} -> 
			?error("Error starting COM port: ~p", [Err]),
			{stop, Err}
    end.


handle_call({call, Handler, State0}, {_Pid, Tag}=From, #state{ com=Com, current=undefined }=S) ->
	case Handler(init, Com, State0) of
		{ok, State1} ->
			{reply, {ok, {self(), Tag}}, S#state{ current={From, Handler, State1} } };
		{error, _}=Err ->
			{reply, Err, S}
	end;

handle_call(_Call, _From, S) ->
	{reply, ok, S}.


handle_cast(_Cast, S) ->
	{noreply, S}.


handle_info({msg, Msg}, #state{ current=undefined }=S) ->
	?info("Ignoring message (unknown recipient): ~p", [Msg]),
	{noreply, S};

handle_info({msg, Msg}, #state{ com=Com, current={{Pid, Tag}=From, Handler, State0} }=S) ->
	case Handler({msg, Msg}, Com, State0) of
		{ok, State1} ->
			Pid ! { {self(), Tag}, {ok, State1} },
			{noreply, S#state{ current=undefined }};
		ok ->
			Pid ! { {self(), Tag}, ok },
			{noreply, S#state{ current=undefined }};
		{more, State1} ->
			{noreply, S#state{ current={From, Handler, State1} } };
		{error, _}=Err ->
			Pid ! { {self(), Tag}, Err },
			{noreply, S#state{ current=undefined }}
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
match_ans('_', Msg) -> {ok, Msg};
match_ans(Ans, {_, Ans, _}=Msg) -> {ok, Msg};
match_ans(E, R) -> 
	?error("<Parse error> expected: ~p received ~p", [E, R]),
	{error, {unexpected, answer}}.


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
