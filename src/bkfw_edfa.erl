%%%
%%% Monitor EDFA
%%%
-module(bkfw_edfa).
-author('jean.parpaillon@free.fr').

-include("bkfw.hrl").

%%%
%%% API
%%%
-export([start_link/0]).

%% SNMP instrumentation
-export([variable_func/2]).

%% Internal
-export([init/1]).

-define(PERIOD, 1000).
-define(SLOTS, 32).
-define(TID, ?MODULE).

-record(state, {
	  slots                    :: tuple()
	 }).

%%%
%%% API
%%%
start_link() ->
    Period = application:get_env(bkfw, slots_period, ?PERIOD),
    Pid = spawn_link(?MODULE, init, [Period]),
    {ok, Pid}.

%%% SNMP functions
variable_func(new, _) ->
    {value, ok};

variable_func(delete, _) ->
    {value, ok};

variable_func(get, Key) ->
    case ets:lookup(?TID, Key) of
	[{Key, V}] when is_float(V) -> {value, round(V)};
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
    loop(Time, init_state()).

loop(Time, S) -> 
    ets:insert(?TID, {edfaNumber, 0}),
    S2 = case bkfw_srv:command(1, rn, []) of
		 {ok, {1, n, [Mask]}} when is_integer(Mask) ->
		     handle_slots(Mask, 0, S);
		 {ok, _Ret} ->
		     ?error("Unrecognized answer: ~p~n", [_Ret]),
		     S;
		 {error, Err} ->
		     ?error("Error monitoring MCUs: ~p~n", [Err]),
		     S
	     end,
    timer:sleep(Time),
    loop(Time, S2).

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

init_state() ->
    S = init_infos(#state{slots=list_to_tuple(lists:duplicate(?SLOTS, false))}),
    S1 = init_v(S),
    init_it(S1).

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
	    S;
	{ok, _Ret} ->
	    ?error("[0] RI invalid answer: ~p~n", [_Ret]),
	    S;
	{error, Err} ->
	    ?error("[0] Error monitoring EDFA: ~p~n", [Err]),
	    S
    end.

get_info(Key, Infos) ->
    binary_to_list(proplists:get_value(Key, Infos, <<>>)).

init_v(S) ->
    case bkfw_srv:command(0, rv, []) of
	{ok, {0, v, [V, v]}} when is_float(V); is_integer(V) ->
	    ets:insert(?TID, {edfaPowerSupply, V}),
	    S;
	{ok, _Ret} ->
	    ?error("[0] RV invalid answer: ~p~n", [_Ret]),
	    S;
	{error, Err} ->
	    ?error("[0] Error monitoring MCU: ~p~n", [Err]),
	    S
    end.

init_it(S) ->
    case bkfw_srv:command(0, rit, []) of
	{ok, {0, it, [T, <<"C">>]}} when is_float(T); is_integer(T) ->
	    ets:insert(?TID, {edfaCurInternalTemp, T}),
	    S;
	{ok, _Ret} ->
	    ?error("[0] RIT invalid answer: ~p~n", [_Ret]),
	    S;
	{error, Err} ->
	    ?error("[0] Error monitoring MCU: ~p~n", [Err]),
	    S
    end.
