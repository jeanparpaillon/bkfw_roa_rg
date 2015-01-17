-module(bkfw_alarms_http).
-author('jean.parpaillon@free.fr').

-behaviour(gen_event).

-include("bkfw.hrl").

-export([init/1,
	 handle_event/2,
	 handle_call/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {handler}).

init([Handler]) ->
    {ok, #state{handler=Handler}}.

handle_event(#smmAlarm{index=Idx, name=Name}, #state{handler=To}=S) ->
    To ! {alarm, [{index, Idx},
		  {name, Name},
		  {var, alarm_to_var(Name)}]},
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
alarm_to_var(pin) -> powerInput;
alarm_to_var(pout) -> powerOutput;
alarm_to_var(pump_temp) -> curLaserTemp;
alarm_to_var(pump_bias) -> curAmp;
alarm_to_var(edfa_temp) -> curInternalTemp;
alarm_to_var(edfa_psu) -> powerSupply;
alarm_to_var(bref) -> powerInput;
alarm_to_var(adi) -> undefined;
alarm_to_var(mute) -> undefined;
alarm_to_var(_) -> undefined.
