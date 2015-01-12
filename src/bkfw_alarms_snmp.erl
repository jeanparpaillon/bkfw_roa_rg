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

handle_event(#edfaAlarm{name=Name, obj=Obj}, S) -> 
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
    %Recv = {{notify, Trap}, self()},
    Recv = no_receiver,
    case snmpa:send_notification(snmp_master_agent, Trap, Recv, "", "", Varbinds) of
	{error, _Err} ->
	    ok;
	ok ->
	    ok
    end.

alarm_to_trap(pin, _) -> edfaInputPowerTrap;
alarm_to_trap(pout, _) -> edfaOutputPowerTrap;
alarm_to_trap(pump_temp, _) -> edfaPumpTempTrap;
alarm_to_trap(pump_bias, _) -> edfaPumpBiasTrap;
alarm_to_trap(edfa_temp, #edfaMcuTable{}) -> edfaMcuInternalTempTrap;
alarm_to_trap(edfa_temp, {_,_}) -> edfaInternalTempTrap;
alarm_to_trap(edfa_psu, #edfaMcuTable{}) -> edfaMcuPowerSupplyTrap;
alarm_to_trap(edfa_psu, {_,_}) -> edfaPowerSupplyTrap;
alarm_to_trap(bref, _) -> edfaBrefTrap;
alarm_to_trap(adi, _) -> edfaAdiTrap;
alarm_to_trap(mute, _) -> edfaMuteTrap;
alarm_to_trap(_, _) -> undefined.

alarm_to_vars(pin, E) ->       [{edfaMcuPowerPd1, [E#edfaMcuTable.index], 
				 round(E#edfaMcuTable.powerPd1)}];
alarm_to_vars(pout, E) ->      [{edfaMcuPowerPd2, [E#edfaMcuTable.index], 
				 round(E#edfaMcuTable.powerPd2)}];
alarm_to_vars(pump_temp, E) -> [{edfaMcuCurLaserTemp, [E#edfaMcuTable.index], 
				 round(E#edfaMcuTable.curLaserTemp)}];
alarm_to_vars(pump_bias, E) -> [{edfaMcuCurAmp, [E#edfaMcuTable.index], 
				 round(E#edfaMcuTable.curAmp)}];
alarm_to_vars(edfa_temp, #edfaMcuTable{index=Idx, curInternalTemp=T}) -> [{edfaMcuCurInternalTemp, [Idx],
									   round(T)}];
alarm_to_vars(edfa_psu, #edfaMcuTable{index=Idx, powerSupply=P}) ->  [{edfaMcuPowerSupply, [Idx],
								       round(P)}];
alarm_to_vars(edfa_temp, {IT, _}) -> [{edfaCurInternalTemp,  round(IT)}];
alarm_to_vars(edfa_psu, {_, PS}) ->  [{edfaPowerSupply, round(PS)}];
alarm_to_vars(bref, _E) ->     [];
alarm_to_vars(adi, _E) ->      [];
alarm_to_vars(mute, _E) ->     [].
