-module(bkfw_mcu).
-author('jean.parpaillon@free.fr').

-include("bkfw.hrl").

-export([start_link/1]).

-export([init/1,
	 read_cc/1,
	 read_gc/1,
	 read_pc/1,
	 read_mode/1,
	 read_a/1,
	 read_lt/1,
	 read_lc/1,
	 read_it/1,
	 read_i/1,
	 read_pm/1,
	 read_v/1,
	 read_li/1,
	 read_lo/1]).

-define(PERIOD, 1000).
-define(FUNS, [fun read_cc/1,
	       fun read_gc/1,
	       fun read_pc/1,
	       fun read_mode/1,
	       fun read_a/1,
	       fun read_lt/1,
	       fun read_lc/1,
	       fun read_it/1,
	       fun read_pm/1,
	       fun read_i/1,
	       fun read_v/1,
	       fun read_li/1,
	       fun read_lo/1]).

-record(state, {idx              :: integer(),
		period           :: integer(),
		positions   = 1  :: integer()}).

start_link(Idx) ->
    ?info("Start MCU monitor (slot: ~p)~n", [Idx+1]),
    Pid = spawn_link(?MODULE, init, [Idx]),
    {ok, Pid}.

%%%
%%% Internals
%%%
init(Idx) ->
    Period = application:get_env(bkfw, mcu_period, ?PERIOD),
    loop(#state{idx=Idx+1, period=Period}).

loop(#state{period=Period}=S) ->
    S2 = lists:foldl(fun (F, Acc) -> F(Acc) end, S, ?FUNS),
    timer:sleep(Period),
    loop(S2).

read_cc(#state{idx=Idx, positions=P}=S) ->
    F = fun(X) ->
		case bkfw_srv:command(Idx, rcc, [integer_to_binary(X)]) of
		    {ok, {Idx, cc, [X, BinA, <<"mA">>]}} ->
			?debug("[~p] current setting(~p): ~p mA~n", [Idx, X, BinA]);
		    {ok, _Ret} ->
			?error("[~p] RCC invalid answer: ~p~n", [Idx, _Ret]);
		    {error, Err} ->
			?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err])
		end
	end,
    lists:foreach(F, lists:seq(1,P)),
    S.

read_gc(#state{idx=Idx}=S) ->
    case bkfw_srv:command(Idx, rgc, []) of
	{ok, {Idx, gc, [BinY, <<"dB">>]}} ->
	    ?debug("[~p] gain consign: ~p dB~n", [Idx, BinY]);
	{ok, _Ret} ->
	    ?error("[~p] RGC invalid answer: ~p~n", [Idx, _Ret]);
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err])
    end,
    S.

read_pc(#state{idx=Idx}=S) ->
    case bkfw_srv:command(Idx, rpc, []) of
	{ok, {Idx, pc, [BinY, <<"dBm">>]}} ->
	    ?debug("[~p] output power setting: ~p dBm~n", [Idx, BinY]);
	{ok, _Ret} ->
	    ?error("[~p] RPC invalid answer: ~p~n", [Idx, _Ret]);
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err])
    end,
    S.

read_mode(#state{idx=Idx}=S) ->
    case bkfw_srv:command(Idx, rmode, []) of
	{ok, {Idx, mode, [Mode]}} ->
	    ?debug("[~p] : operating mode: ~p~n", [Idx, Mode]);
	{ok, _Ret} ->
	    ?error("[~p] RMODE invalid answer: ~p~n", [Idx, _Ret]);
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err])
    end,
    S.

read_a(#state{idx=Idx}=S) ->
    case bkfw_srv:command(Idx, ra, []) of
	{ok, {Idx, alarms, Alarms}} ->
	    ?debug("[~p] Alarms: ~p~n", [Idx, Alarms]);
	{ok, _Ret} ->
	    ?error("[~p] RA invalid answer: ~p~n", [Idx, _Ret]);
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err])
    end,
    S.

read_lt(#state{idx=Idx, positions=P}=S) ->
    F = fun(X) ->
		case bkfw_srv:command(Idx, rlt, [integer_to_binary(X)]) of
		    {ok, {Idx, lt, [BinX, BinT, <<"C">>]}} ->
			?debug("[~p] current temperature(~p): ~p C~n", [Idx, BinX, BinT]);
		    {ok, _Ret} ->
			?error("[~p] RLT invalid answer: ~p~n", [Idx, _Ret]);
		    {error, Err} ->
			?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err])
		end
	end,
    lists:foreach(F, lists:seq(1,P)),
    S.

read_lc(#state{idx=Idx, positions=P}=S) ->
    F = fun(X) ->
		case bkfw_srv:command(Idx, rlc, [integer_to_binary(X)]) of
		    {ok, {Idx, lc, [BinX, BinA, <<"mA">>]}} ->
			?debug("[~p] current current(~p): ~p C~n", [Idx, BinX, BinA]);
		    {ok, _Ret} ->
			?error("[~p] RLC invalid answer: ~p~n", [Idx, _Ret]);
		    {error, Err} ->
			?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err])
		end
	end,
    lists:foreach(F, lists:seq(1,P)),
    S.

read_it(#state{idx=Idx}=S) ->
    case bkfw_srv:command(Idx, rit, []) of
	{ok, {Idx, it, [BinT, <<"C">>]}} ->
	    ?debug("[~p] internal temperature: ~p C~n", [Idx, BinT]);
	{ok, _Ret} ->
	    ?error("[~p] RGC invalid answer: ~p~n", [Idx, _Ret]);
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err])
    end,
    S.

read_i(#state{idx=Idx}=S) ->
    case bkfw_srv:command(Idx, ri, []) of
	{ok, {Idx, i, Infos}} ->
	    ?debug("[~p] informations: ~p~n", [Idx, Infos]);
	{ok, _Ret} ->
	    ?error("[~p] RI invalid answer: ~p~n", [Idx, _Ret]);
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err])
    end,
    S.

read_pm(#state{idx=Idx}=S) ->
    case bkfw_srv:command(Idx, rpm, []) of
	{ok, {Idx, pd, Lines}} ->
	    ?debug("[~p] pd: ~p~n", [Idx, Lines]);
	{ok, _Ret} ->
	    ?error("[~p] RPM invalid answer: ~p~n", [Idx, _Ret]);
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err])
    end,
    S.

read_v(#state{idx=Idx}=S) ->
    case bkfw_srv:command(Idx, rv, []) of
	{ok, {Idx, v, [BinV, v]}} ->
	    ?debug("[~p] power supply voltage: ~p V~n", [Idx, BinV]);
	{ok, _Ret} ->
	    ?error("[~p] RV invalid answer: ~p~n", [Idx, _Ret]);
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err])
    end,
    S.

read_li(#state{idx=Idx}=S) ->
    case bkfw_srv:command(Idx, rli, []) of
	{ok, {Idx, li, [BinY, <<"dBm">>]}} ->
	    ?debug("[~p] loss of input power treshold: ~p dBm~n", [Idx, BinY]);
	{ok, _Ret} ->
	    ?error("[~p] RLI invalid answer: ~p~n", [Idx, _Ret]);
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err])
    end,
    S.

read_lo(#state{idx=Idx}=S) ->
    case bkfw_srv:command(Idx, rlo, []) of
	{ok, {Idx, lo, [BinY, <<"dBm">>]}} ->
	    ?debug("[~p] loss of output power treshold: ~p dBm~n", [Idx, BinY]);
	{ok, _Ret} ->
	    ?error("[~p] RLO invalid answer: ~p~n", [Idx, _Ret]);
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err])
    end,
    S.
