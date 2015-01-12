%%%
%%% Monitor EDFA
%%%
-module(bkfw_edfa).
-author('jean.parpaillon@free.fr').

-include("bkfw.hrl").

%%%
%%% API
%%%
-export([start_link/0,
	 get_kv/0]).

%% SNMP instrumentation
-export([variable_func/2]).

%% Internal
-export([init/1,
	 read_it/1,
	 read_v/1,
	 read_n/1,
	 read_a/1]).

-define(PERIOD, 1000).
-define(SLOTS, 32).
-define(TID, ?MODULE).

-define(FUNS, [fun read_it/1,
	       fun read_v/1,
	       fun read_n/1,
	       fun read_a/1]).

-record(state, {
	  slots                    :: tuple(),
	  curInternalTemp = 0.0,
	  powerSupply = 0.0,
	  n=0
	 }).

%%%
%%% API
%%%
start_link() ->
    Period = application:get_env(bkfw, edfa_period, ?PERIOD),
    Pid = spawn_link(?MODULE, init, [Period]),
    {ok, Pid}.

get_kv() ->
    [
     {curInternalTemp, get_ets_value(edfaCurInternalTemp, 0.0)},
     {powerSupply,     get_ets_value(edfaPowerSupply, 0.0)},
     {vendor,          get_ets_value(edfaVendor, <<>>)},
     {moduleType,      get_ets_value(moduleType, <<>>)},
     {hwVer,           get_ets_value(edfaHWVer, <<>>)},
     {hwRev,           get_ets_value(edfaHWRev, <<>>)},
     {swVer,           get_ets_value(edfaSWVer, <<>>)},
     {fwVer,           get_ets_value(edfaFWVer, <<>>)},
     {partNum,         get_ets_value(edfaPartNum, <<>>)},
     {serialNum,       get_ets_value(edfaSerialNum, <<>>)},
     {productDate,     get_ets_value(edfaProductDate, <<>>)}
    ].

%%% SNMP functions
variable_func(new, _) ->
    {value, ok};

variable_func(delete, _) ->
    {value, ok};

variable_func(get, Key) ->
    case ets:lookup(?TID, Key) of
	[{Key, V}] when is_float(V) -> {value, round(V)};
	[{Key, V}] when is_binary(V) -> {value, binary_to_list(V)};
	[{Key, V}] -> {value, V};
	[] -> {value, noSuchName}
    end.

%%%
%%% Private
%%%
init(Time) ->
    ?info("Starting EDFA monitoring~n", []),
    timer:sleep(1000),
    _ = ets:new(?TID, [named_table]),
    ets:insert(?TID, {edfaNumber, 0}),
    case init_state() of
	{ok, #state{}=S} ->
	    loop(Time, S);
	{error, Err} ->
	    {error, Err}
    end.

loop(Time, S) -> 
    S2 = lists:foldl(fun (F, Acc) -> F(Acc) end, S, ?FUNS),
    timer:sleep(Time),
    loop(Time, S2).


init_state() ->
    init_infos(#state{slots=list_to_tuple(lists:duplicate(?SLOTS, false))}).

init_infos(S) ->
    case bkfw_srv:command(0, ri, []) of
	{ok, {0, i, Infos}} ->
	    ets:insert(?TID, 
		       [
			{edfaVendor, get_info(vendor, Infos)},
			{edfaModuleType, get_info(moduleType, Infos)},
			{edfaHWVer, get_info(hwVer, Infos)},
			{edfaHWRev, get_info(hwRev, Infos)},
			{edfaSWVer, get_info(swVer, Infos)},
			{edfaFWVer, get_info(fwVer, Infos)},
			{edfaPartNum, get_info(partNum, Infos)},
			{edfaSerialNum, get_info(serialNum, Infos)},
			{edfaProductDate, get_info(productDate, Infos)}
		       ]
		      ),
	    {ok, S};
	{ok, _Ret} ->
	    ?error("[0] RI invalid answer: ~p~n", [_Ret]),
	    {error, invalid_infos};
	{error, {timeout, _}} ->
	    ?debug("EDFA RI timeout: retry in 2 seconds...\n"),
	    timer:sleep(2000),
	    init_infos(S);
	{error, Err} ->
	    ?error("[0] Error monitoring EDFA: ~p~n", [Err]),
	    {error, Err}
    end.

get_info(Key, Infos) ->
    proplists:get_value(Key, Infos, <<>>).

read_v(S) ->
    case bkfw_srv:command(0, rv, []) of
	{ok, {0, v, [V, v]}} when is_float(V); is_integer(V) ->
	    ets:insert(?TID, {edfaPowerSupply, V}),
	    S#state{powerSupply=V};
	{ok, _Ret} ->
	    ?error("[0] RV invalid answer: ~p~n", [_Ret]),
	    S;
	{error, Err} ->
	    ?error("[0] Error monitoring EDFA: ~p~n", [Err]),
	    S
    end.

read_it(S) ->
    case bkfw_srv:command(0, rit, []) of
	{ok, {0, it, [T, <<"C">>]}} when is_float(T); is_integer(T) ->
	    ets:insert(?TID, {edfaCurInternalTemp, T}),
	    S#state{curInternalTemp=T};
	{ok, _Ret} ->
	    ?error("[0] RIT invalid answer: ~p~n", [_Ret]),
	    S;
	{error, Err} ->
	    ?error("[0] Error monitoring EDFA: ~p~n", [Err]),
	    S
    end.

read_n(#state{n=0}=S) ->
    ets:insert(?TID, {edfaNumber, 0}),
    case bkfw_srv:command(0, rn, []) of
	{ok, {0, n, [Mask]}} when is_integer(Mask) ->
	    handle_slots(Mask, 0, S#state{n=1});
	{ok, _Ret} ->
	    ?error("Unrecognized answer: ~p~n", [_Ret]),
	    S#state{n=0};
	{error, Err} ->
	    ?error("Error monitoring EDFA: ~p~n", [Err]),
	    S#state{n=0}
    end;
read_n(#state{n=N}=S) when N<5 ->
    S#state{n=N+1};
read_n(#state{}=S) ->
    S#state{n=0}.


% loop over all bits of mask and compare with old slots,
% start or kill bkfw_mon if necessary
handle_slots(_Mask, ?SLOTS, S) ->
    S;

handle_slots(Mask, Idx, #state{slots=Slots}=S) when 
      Mask band (1 bsl Idx) == 0, element(Idx+1, Slots) == false ->
    % slot is not occupied, slot was not occupied
    handle_slots(Mask, Idx+1, S);

handle_slots(Mask, Idx, #state{slots=Slots}=S) when 
      Mask band (1 bsl Idx) == 0, is_pid(element(Idx+1, Slots)) ->
    % slot is not occupied, slot was occupied
    ?info("Stop MCU monitor (slot: ~p)~n", [Idx]),
    ok = mnesia:dirty_delete(edfaMcuTable, Idx+1),
    bkfw_mcus_sup:terminate_mcu(element(Idx+1, Slots)),
    handle_slots(Mask, Idx+1, S#state{slots=setelement(Idx+1, Slots, false)});

handle_slots(Mask, Idx, #state{slots=Slots}=S) when
      Mask band (1 bsl Idx) /= 0, element(Idx+1, Slots) == false ->
    % slot is occupied, slot was not occupied
    {ok, Pid} = bkfw_mcus_sup:start_mcu(Idx+1),
    ets:insert(?TID, {edfaNumber, ets:lookup_element(?TID, edfaNumber, 2)+1}),
    handle_slots(Mask, Idx+1, S#state{slots=setelement(Idx+1, Slots, Pid)});

handle_slots(Mask, Idx, #state{slots=Slots}=S) when
      Mask band (1 bsl Idx) /= 0, is_pid(element(Idx+1, Slots)) ->
    % slot is occupied, slot was occupied
    ets:insert(?TID, {edfaNumber, ets:lookup_element(?TID, edfaNumber, 2)+1}),
    handle_slots(Mask, Idx+1, S).


read_a(S) ->
    case bkfw_srv:command(0, ra, []) of
	{ok, {0, alarms, Alarms}} ->
	    handle_alarms(Alarms, S);
	{ok, _Ret} ->
	    ?error("[~p] RA invalid answer: ~p~n", [0, _Ret]),
	    S;
	{error, Err} ->
	    ?error("[~p] Error monitoring MCU: ~p~n", [0, Err]),
	    S
    end.

handle_alarms([], S) -> S;
handle_alarms([Name  | Tail], #state{curInternalTemp=IT, powerSupply=PS}=S) -> 
    gen_event:notify(bkfw_alarms, #edfaAlarm{index=0, name=Name, obj={IT, PS}}),
    handle_alarms(Tail, S).


get_ets_value(Key, Default) ->
    try ets:lookup(?TID, Key) of
	[{Key, V}] -> V;
	[] -> Default
    catch _:_ -> Default
    end.
