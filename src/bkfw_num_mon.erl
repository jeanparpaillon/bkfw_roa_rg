%%%
%%% Monitor MCU number and start one monitor for each
%%%
-module(bkfw_num_mon).
-author('jean.parpaillon@free.fr').

-include("bkfw.hrl").

%%%
%%% API
%%%
-export([start_link/0]).

-export([init/1]).

-define(PERIOD, 1000).
-define(SLOTS, 32).

%%%
%%% API
%%%
start_link() ->
    Period = application:get_env(bkfw, slots_period, ?PERIOD),
    Pid = spawn_link(?MODULE, init, [Period]),
    {ok, Pid}.

%%%
%%% Private
%%%
init(Time) ->
    ?info("Starting MCU slots monitoring~n", []),
    Slots = list_to_tuple(lists:duplicate(?SLOTS, false)),
    timer:sleep(1000),
    loop(Time, Slots).

loop(Time, Slots) -> 
    Slots2 = case bkfw_srv:command(1, rn, []) of
		 {ok, {1, n, [Mask]}} when is_integer(Mask) ->
		     handle_slots(Slots, Mask, 0, 0);
		 {ok, _Ret} ->
		     ?error("Unrecognized answer: ~p~n", [_Ret]),
		     Slots;
		 {error, Err} ->
		     ?error("Error monitoring MCUs: ~p~n", [Err]),
		     Slots
	     end,
    timer:sleep(Time),
    loop(Time, Slots2).

% loop over all bits of mask and compare with old slots,
% start or kill bkfw_mon if necessary
handle_slots(Slots, _Mask, ?SLOTS, Count) ->
    true = snmp_generic:variable_set(edfaNumber, Count),
    Slots;

handle_slots(Slots, Mask, Idx, Count) when 
      Mask band (1 bsl Idx) == 0, element(Idx+1, Slots) == false ->
    % slot is not occupied, slot was not occupied
    handle_slots(Slots, Mask, Idx+1, Count);

handle_slots(Slots, Mask, Idx, Count) when 
      Mask band (1 bsl Idx) == 0, is_pid(element(Idx+1, Slots)) ->
    % slot is not occupied, slot was occupied
    ?info("Stop MCU monitor (slot: ~p)~n", [Idx+1]),
    ok = mnesia:dirty_delete(edfaTable, Idx+1),
    bkfw_mcus_sup:terminate_mcu(element(Idx+1, Slots)),
    Slots2 = setelement(Idx+1, Slots, false),
    handle_slots(Slots2, Mask, Idx+1, Count);

handle_slots(Slots, Mask, Idx, Count) when
      Mask band (1 bsl Idx) /= 0, element(Idx+1, Slots) == false ->
    % slot is occupied, slot was not occupied
    {ok, Pid} = bkfw_mcus_sup:start_mcu(Idx+1),
    Slots2 = setelement(Idx+1, Slots, Pid),
    handle_slots(Slots2, Mask, Idx+1, Count+1);

handle_slots(Slots, Mask, Idx, Count) when
      Mask band (1 bsl Idx) /= 0, is_pid(element(Idx+1, Slots)) ->
    % slot is occupied, slot was occupied
    handle_slots(Slots, Mask, Idx+1, Count+1).
