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

init(Handler) ->
    {ok, #state{handler=Handler}}.

handle_event(#edfaAlarm{index=Idx, name=Name, vars=Vars}, #state{handler=To}=S) ->
    ?debug("Dispatch alarm to WS~n", []),
    {ok, S}.

handle_call(_Call, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
