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
	       fun read_lt/1,
	       fun read_lc/1,
	       fun read_it/1,
	       fun read_pm/1,
	       fun read_i/1,
	       fun read_v/1,
	       fun read_li/1,
	       fun read_lo/1,
	       fun read_a/1
	      ]).

-record(state, {idx                        :: integer(),
		period                     :: integer(),
		positions   = 1            :: integer(),
		entry       = #edfaTable{} :: edfaTable()
	       }).

start_link(Idx) ->
    ?info("Start MCU monitor (slot: ~p)~n", [Idx]),
    Pid = spawn_link(?MODULE, init, [Idx]),
    {ok, Pid}.

%%%
%%% Internals
%%%
init(Idx) ->
    Period = application:get_env(bkfw, mcu_period, ?PERIOD),
    loop(#state{idx=Idx+1, period=Period, entry=#edfaTable{index=Idx}}).

loop(#state{period=Period}=S) ->
    S2 = lists:foldl(fun (F, Acc) -> F(Acc) end, S, ?FUNS),
    ok = mnesia:dirty_write(S2#state.entry),
    timer:sleep(Period),
    loop(S2).

read_cc(#state{idx=Idx, positions=P}=S) ->
    F = fun(X, #state{entry=E}=Acc) ->
		case bkfw_srv:command(Idx, rcc, [integer_to_binary(X)]) of
		    {ok, {Idx, cc, [X, A, <<"mA">>]}} when is_float(A); is_integer(A) ->
			Acc#state{entry=E#edfaTable{ampConsign=round(A)}};
		    {ok, _Ret} ->
			?error("[~p] RCC invalid answer: ~p~n", [Idx, _Ret]),
			Acc;
		    {error, Err} ->
			?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err]),
			Acc
		end
	end,
    lists:foldl(F, S, lists:seq(1,P)).

read_gc(#state{idx=Idx, entry=E}=S) ->
    case bkfw_srv:command(Idx, rgc, []) of
	{ok, {Idx, gc, [Y, <<"dB">>]}} when is_float(Y); is_integer(Y) ->
	    S#state{entry=E#edfaTable{gainConsign=round(Y)}};
	{ok, _Ret} ->
	    ?error("[~p] RGC invalid answer: ~p~n", [Idx, _Ret]),
	    S;
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err]),
	    S
    end.

read_pc(#state{idx=Idx, entry=E}=S) ->
    case bkfw_srv:command(Idx, rpc, []) of
	{ok, {Idx, pc, [Y, <<"dBm">>]}} when is_float(Y); is_integer(Y) ->
	    S#state{entry=E#edfaTable{outputPowerConsign=round(Y)}};
	{ok, _Ret} ->
	    ?error("[~p] RPC invalid answer: ~p~n", [Idx, _Ret]),
	    S;
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err]),
	    S
    end.

read_mode(#state{idx=Idx, entry=E}=S) ->
    case bkfw_srv:command(Idx, rmode, []) of
	{ok, {Idx, mode, [Mode]}} ->
	    M = parse_mode(Mode, E#edfaTable.operatingMode),
	    S#state{entry=E#edfaTable{operatingMode=M}};
	{ok, _Ret} ->
	    ?error("[~p] RMODE invalid answer: ~p~n", [Idx, _Ret]),
	    S;
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err]),
	    S
    end.

read_a(#state{idx=Idx}=S) ->
    case bkfw_srv:command(Idx, ra, []) of
	{ok, {Idx, alarms, Alarms}} ->
	    handle_alarms(Alarms, S);
	{ok, _Ret} ->
	    ?error("[~p] RA invalid answer: ~p~n", [Idx, _Ret]),
	    S;
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err]),
	    S
    end.

read_lt(#state{idx=Idx, positions=P}=S) ->
    F = fun(X, #state{entry=E}=Acc) ->
		case bkfw_srv:command(Idx, rlt, [integer_to_binary(X)]) of
		    {ok, {Idx, lt, [X, T, <<"C">>]}} when is_float(T); is_integer(T) ->
			S#state{entry=E#edfaTable{curLaserTemp=round(T)}};
		    {ok, _Ret} ->
			?error("[~p] RLT invalid answer: ~p~n", [Idx, _Ret]),
			Acc;
		    {error, Err} ->
			?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err]),
			Acc
		end
	end,
    lists:foldl(F, S, lists:seq(1,P)).

read_lc(#state{idx=Idx, positions=P}=S) ->
    F = fun(X, #state{entry=E}=Acc) ->
		case bkfw_srv:command(Idx, rlc, [integer_to_binary(X)]) of
		    {ok, {Idx, lc, [X, A, <<"mA">>]}} when is_float(A); is_integer(A) ->
			S#state{entry=E#edfaTable{curAmp=round(A)}};
		    {ok, _Ret} ->
			?error("[~p] RLC invalid answer: ~p~n", [Idx, _Ret]),
			Acc;
		    {error, Err} ->
			?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err]),
			Acc
		end
	end,
    lists:foldl(F, S, lists:seq(1,P)).

read_it(#state{idx=Idx, entry=E}=S) ->
    case bkfw_srv:command(Idx, rit, []) of
	{ok, {Idx, it, [T, <<"C">>]}} when is_float(T); is_integer(T) ->
	    S#state{entry=E#edfaTable{curInternalTemp=round(T)}};
	{ok, _Ret} ->
	    ?error("[~p] RGC invalid answer: ~p~n", [Idx, _Ret]),
	    S;
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err]),
	    S
    end.

read_i(#state{idx=Idx, entry=E}=S) ->
    case bkfw_srv:command(Idx, ri, []) of
	{ok, {Idx, i, Infos}} ->
	    S#state{entry=E#edfaTable{
			    vendor=get_info(vendor, Infos, E#edfaTable.vendor),
			    moduleType=get_info(moduleType, Infos, E#edfaTable.moduleType),
			    hwVer=get_info(hwVer, Infos, E#edfaTable.hwVer),
			    hwRev=get_info(hwRev, Infos, E#edfaTable.hwRev),
			    swVer=get_info(swVer, Infos, E#edfaTable.swVer),
			    fwVer=get_info(fwVer, Infos, E#edfaTable.fwVer),
			    partNum=get_info(partNum, Infos, E#edfaTable.partNum),
			    serialNum=get_info(serialNum, Infos, E#edfaTable.serialNum),
			    productDate=get_info(productDate, Infos, E#edfaTable.productDate)
			   }};
	{ok, _Ret} ->
	    ?error("[~p] RI invalid answer: ~p~n", [Idx, _Ret]),
	    S;
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err]),
	    S
    end.

read_pm(#state{idx=Idx, entry=E}=S) ->
    case bkfw_srv:command(Idx, rpm, []) of
	{ok, {Idx, pd, Lines}} ->
	    Defaults = { E#edfaTable.powerPd1,
			 E#edfaTable.powerPd2,
			 E#edfaTable.powerPd3 },
	    {Pd1, Pd2, Pd3} = parse_pd(Lines, Defaults),
	    S#state{entry=E#edfaTable{powerPd1=Pd1,
				      powerPd2=Pd2,
				      powerPd3=Pd3}};
	{ok, _Ret} ->
	    ?error("[~p] RPM invalid answer: ~p~n", [Idx, _Ret]),
	    S;
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err]),
	    S
    end.

read_v(#state{idx=Idx, entry=E}=S) ->
    case bkfw_srv:command(Idx, rv, []) of
	{ok, {Idx, v, [V, v]}} when is_float(V); is_integer(V) ->
	    S#state{entry=E#edfaTable{powerSupply=round(V)}};
	{ok, _Ret} ->
	    ?error("[~p] RV invalid answer: ~p~n", [Idx, _Ret]),
	    S;
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err]),
	    S
    end.

read_li(#state{idx=Idx, entry=E}=S) ->
    case bkfw_srv:command(Idx, rli, []) of
	{ok, {Idx, li, [Y, <<"dBm">>]}} ->
	    S#state{entry=E#edfaTable{inputLossThreshold=round(Y)}};
	{ok, _Ret} ->
	    ?error("[~p] RLI invalid answer: ~p~n", [Idx, _Ret]),
	    S;
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err]),
	    S
    end.

read_lo(#state{idx=Idx, entry=E}=S) ->
    case bkfw_srv:command(Idx, rlo, []) of
	{ok, {Idx, lo, [Y, <<"dBm">>]}} ->
	    S#state{entry=E#edfaTable{outputLossThreshold=round(Y)}};
	{ok, _Ret} ->
	    ?error("[~p] RLO invalid answer: ~p~n", [Idx, _Ret]),
	    S;
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [Idx, Err]),
	    S
    end.

%%%
%%% Convenience functions
%%%
parse_mode(<<"PC">>, _) -> ?edfaOperatingMode_pc;
parse_mode(<<"GC">>, _) -> ?edfaOperatingMode_gc;
parse_mode(<<"CC">>, _) -> ?edfaOperatingMode_cc;
parse_mode(<<"OFF">>, _) -> ?edfaOperatingMode_off;
parse_mode(_, Dft) -> Dft.

parse_pd([], Acc) -> Acc;
parse_pd([ [1, P, <<"dBm">>] | Tail], {_, Pd2, Pd3}) -> parse_pd(Tail, {round(P), Pd2, Pd3});
parse_pd([ [2, P, <<"dBm">>] | Tail], {Pd1, _, Pd3}) -> parse_pd(Tail, {Pd1, round(P), Pd3});
parse_pd([ [3, P, <<"dBm">>] | Tail], {Pd1, Pd2, _}) -> parse_pd(Tail, {Pd1, Pd2, round(P)});
parse_pd([ _ | Tail], Acc) -> parse_pd(Tail, Acc).

get_info(Key, Infos, Default) ->
    try binary_to_list(proplists:get_value(Key, Infos, Default)) of
	Str -> Str
    catch error:badarg ->
	    Default
    end.


handle_alarms([], S) -> S;

handle_alarms([pin  | Tail], #state{entry=E}=S) -> 
    Varbinds = [{edfaInputLossTh, [E#edfaTable.index], E#edfaTable.inputLossThreshold}],
    snmpa:send_notification2(snmp_master_agent, edfaInputPowerTrap, [{varbinds, Varbinds}]),
    handle_alarms(Tail, S);

handle_alarms([pout  | Tail], #state{entry=E}=S) -> 
    Varbinds = [{edfaOutputLossTh, [E#edfaTable.index], E#edfaTable.outputLossThreshold}],
    snmpa:send_notification2(snmp_master_agent, edfaOutputPowerTrap, [{varbinds, Varbinds}]),
    handle_alarms(Tail, S);

handle_alarms(['pump_temp'  | Tail], #state{entry=E}=S) -> 
    Varbinds = [{edfaCurLaserTemp, [E#edfaTable.index], E#edfaTable.curLaserTemp}],
    snmpa:send_notification2(snmp_master_agent, edfaPumpTempTrap, [{varbinds, Varbinds}]),
    handle_alarms(Tail, S);

handle_alarms(['pump_bias'  | Tail], #state{entry=E}=S) -> 
    Varbinds = [{edfaCurAmp, [E#edfaTable.index], E#edfaTable.curAmp}],
    snmpa:send_notification2(snmp_master_agent, edfaPumpBiasTrap, [{varbinds, Varbinds}]),
    handle_alarms(Tail, S);

handle_alarms(['edfa_temp'  | Tail], #state{entry=E}=S) -> 
    Varbinds = [{edfaCurInternalTemp, [E#edfaTable.index], E#edfaTable.curInternalTemp}],
    snmpa:send_notification2(snmp_master_agent, edfaInternalTempTrap, [{varbinds, Varbinds}]),
    handle_alarms(Tail, S);

handle_alarms(['edfa_psu'  | Tail], #state{entry=E}=S) -> 
    Varbinds = [{edfaPowerSupply, [E#edfaTable.index], E#edfaTable.powerSupply}],
    snmpa:send_notification2(snmp_master_agent, edfaPowerSupplyTrap, [{varbinds, Varbinds}]),
    handle_alarms(Tail, S);

handle_alarms([bref  | Tail], #state{entry=E}=S) -> 
    Varbinds = [{edfaIndex, E#edfaTable.index}],
    snmpa:send_notification2(snmp_master_agent, edfaBrefTrap, [{varbinds, Varbinds}]),
    handle_alarms(Tail, S);

handle_alarms([adi  | Tail], #state{entry=E}=S) -> 
    Varbinds = [{edfaIndex, E#edfaTable.index}],
    snmpa:send_notification2(snmp_master_agent, edfaAdiTrap, [{varbinds, Varbinds}]),
    handle_alarms(Tail, S);

handle_alarms([mute  | Tail], #state{entry=E}=S) -> 
    Varbinds = [{edfaIndex, E#edfaTable.index}],
    snmpa:send_notification2(snmp_master_agent, edfaMuteTrap, [{varbinds, Varbinds}]),
    handle_alarms(Tail, S).
