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

handle_event(#edfaAlarm{index=Idx, name=Name, vars=Vars}, S) -> 
    ?debug("Dispatch alarm to SNMP~n", []),
    Varbinds = lists:map(fun ({N, Value}) ->
				 {N, [Idx], Value}
			 end, Vars),
    snmpa:send_notification2(snmp_master_agent, alarm_to_trap(Name), [{varbinds, Varbinds}]),
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
