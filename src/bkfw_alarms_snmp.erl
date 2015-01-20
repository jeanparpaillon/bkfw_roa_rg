-module(bkfw_alarms_snmp).
-author('jean.parpaillon@free.fr').

-behaviour(gen_event).

-include("bkfw.hrl").

-export([init/1,
	 handle_event/2,
	 handle_call/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

init(_Args) ->
    {ok, []}.

handle_event(#smmAlarm{name=Name, obj=Obj}, S) -> 
    Trap = alarm_to_trap(Name, Obj),
    Varbinds = alarm_to_vars(Name, Obj),
    send_trap(Trap, Varbinds),
    {ok, S}.

handle_call(_Call, State) ->
    {ok, State}.


handle_info({snmp_targets, {notify, _Trap}, _Vars}, State) ->
    {ok, State};
handle_info(_Info, State) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% Priv
%%%
send_trap(Trap, Varbinds) ->
    Recv = no_receiver,
    case snmpa:send_notification(snmp_master_agent, Trap, Recv, "", "", Varbinds) of
	{error, _Err} ->
	    ok;
	ok ->
	    ok
    end.

alarm_to_trap(pin, _) -> ampInputPowerTrap;
alarm_to_trap(pout, _) -> ampOutputPowerTrap;
alarm_to_trap(pump_temp, _) -> ampPumpTempTrap;
alarm_to_trap(pump_bias, _) -> ampPumpBiasTrap;
alarm_to_trap(edfa_temp, #ampTable{}) -> ampInternalTempTrap;
alarm_to_trap(edfa_temp, {_,_}) -> smmInternalTempTrap;
alarm_to_trap(edfa_psu, #ampTable{}) -> ampPowerSupplyTrap;
alarm_to_trap(edfa_psu, {_,_}) -> smmPowerSupplyTrap;
alarm_to_trap(bref, _) -> ampBrefTrap;
alarm_to_trap(adi, _) -> ampAdiTrap;
alarm_to_trap(mute, _) -> ampMuteTrap;
alarm_to_trap(_, _) -> undefined.

alarm_to_vars(pin, #ampTable{index=Idx}=E) ->                        [{ampPowerPd1,     [Idx], round(E#ampTable.powerPd1)}];
alarm_to_vars(pout, #ampTable{index=Idx}=E) ->                       [{ampPowerPd2,     [Idx], round(E#ampTable.powerPd2)}];
alarm_to_vars(pump_temp, #ampTable{index=Idx}=E) ->                  [{ampCurLaserTemp, [Idx], round(E#ampTable.curLaserTemp)}];
alarm_to_vars(pump_bias, #ampTable{index=Idx}=E) ->                  [{ampCurAmp,       [Idx], round(E#ampTable.curAmp)}];
alarm_to_vars(edfa_temp, #ampTable{index=Idx, curInternalTemp=T}) -> [{ampCurInternalTemp, [Idx], round(T)}];
alarm_to_vars(edfa_psu, #ampTable{index=Idx, powerSupply=P}) ->      [{ampPowerSupply, [Idx], round(P)}];
alarm_to_vars(edfa_temp, {IT, _}) ->                                 [{smmCurInternalTemp, round(IT)}];
alarm_to_vars(edfa_psu, {_, PS}) ->                                  [{smmPowerSupply, round(PS)}];
alarm_to_vars(bref, #ampTable{index=Idx}=E) ->                       [{ampPowerPd2, [Idx], round(E#ampTable.powerPd2)},
								      {ampPowerPd3, [Idx], round(E#ampTable.powerPd3)}];
alarm_to_vars(adi, #ampTable{index=Idx}) ->                          [{ampIndex, Idx}];
alarm_to_vars(mute, #ampTable{index=Idx}) ->                         [{ampIndex, Idx}].
