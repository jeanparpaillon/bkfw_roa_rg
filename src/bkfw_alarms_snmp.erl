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

handle_event(#edfaAlarm{name=Name, obj=#edfaMcuTable{}=Mcu}, S) -> 
    snmpa:send_notification(snmp_master_agent, alarm_to_trap(Name), no_receiver, "", "", 
			    [{varbinds, alarm_to_vars(Name, Mcu)}]),
    {ok, S};
handle_event(#edfaAlarm{name=Name, obj=Obj}, S) -> 
    snmpa:send_notification(snmp_master_agent, alarm_to_trap(Name), no_receiver, "", "",
			    [{varbinds, alarm_to_vars(Name, Obj)}]),
    {ok, S}.

handle_call(_Call, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% Priv
%%%
alarm_to_trap(pin) -> edfaInputPowerTrap;
alarm_to_trap(pout) -> edfaOutputPowerTrap;
alarm_to_trap(pump_temp) -> edfaPumpTempTrap;
alarm_to_trap(pump_bias) -> edfaPumpBiasTrap;
alarm_to_trap(edfa_temp) -> edfaInternalTempTrap;
alarm_to_trap(edfa_psu) -> edfaPowerSupplyTrap;
alarm_to_trap(bref) -> edfaBrefTrap;
alarm_to_trap(adi) -> edfaAdiTrap;
alarm_to_trap(mute) -> edfaMuteTrap;
alarm_to_trap(_) -> undefined.

alarm_to_vars(pin, E) ->       [{edfaMcuPowerPd1, E#edfaMcuTable.index, 
				 round(E#edfaMcuTable.powerPd1)}];
alarm_to_vars(pout, E) ->      [{edfaMcuPowerPd2, E#edfaMcuTable.index, 
				 round(E#edfaMcuTable.powerPd2)}];
alarm_to_vars(pump_temp, E) -> [{edfaMcuCurLaserTemp, E#edfaMcuTable.index, 
				 round(E#edfaMcuTable.curLaserTemp)}];
alarm_to_vars(pump_bias, E) -> [{edfaMcuCurAmp, E#edfaMcuTable.index, 
				 round(E#edfaMcuTable.curAmp)}];
alarm_to_vars(edfa_temp, {IT, _}) -> [{edfaCurInternalTemp,  round(IT)}];
alarm_to_vars(edfa_psu, {_, PS}) ->  [{edfaPowerSupply, round(PS)}];
alarm_to_vars(bref, _E) ->     [];
alarm_to_vars(adi, _E) ->      [];
alarm_to_vars(mute, _E) ->     [].
