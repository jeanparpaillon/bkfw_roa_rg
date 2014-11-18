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
    Period = application:get_env(bkfw, num_mon_period, ?PERIOD),
    Pid = spawn_link(?MODULE, init, [Period]),
    {ok, Pid}.

%%%
%%% Private
%%%
init(Time) ->
    ?info("Starting MCU slots monitoring~n", []),
    Slots = list_to_tuple(lists:duplicate(?SLOTS, false)),
    loop(Time, Slots).

loop(Time, Slots) -> 
    timer:sleep(Time),
    Slots2 = case bkfw_srv:command(1, rn, []) of
		 {ok, {1, n, [M]}} ->
		     try binary_to_integer(M, 16) of
			 Mask -> 
			     handle_slots(Slots, Mask, 0)
		     catch 
			 error:badarg ->
			     ?error("Invalid mask: ~p~n", [M]),
			     Slots
		     end;
		 {ok, _Ret} ->
		     ?error("Unrecognized answer: ~p~n", [_Ret]),
		     Slots;
		 {error, Err} ->
		     ?error("Error monitoring MCUs: ~p~n", [Err]),
		     Slots
	     end,
    loop(Time, Slots2).

% loop over all bits of mask and compare with old slots,
% start or kill bkfw_mon if necessary
handle_slots(Slots, _Mask, ?SLOTS) ->
    Slots;

handle_slots(Slots, Mask, Idx) when 
      Mask band (1 bsl Idx) == 0, element(Idx+1, Slots) == false ->
    % slot is not occupied, slot was not occupied
    handle_slots(Slots, Mask, Idx+1);

handle_slots(Slots, Mask, Idx) when 
      Mask band (1 bsl Idx) == 0, is_pid(element(Idx+1, Slots)) ->
    % slot is not occupied, slot was occupied
    ?info("Stop MCU monitor (slot: ~p)~n", [Idx+1]),
    bkfw_mcus_sup:terminate_mcu(element(Idx+1, Slots)),
    Slots2 = setelement(Idx+1, Slots, false),
    handle_slots(Slots2, Mask, Idx+1);

handle_slots(Slots, Mask, Idx) when
      Mask band (1 bsl Idx) /= 0, element(Idx+1, Slots) == false ->
    % slot is occupied, slot was not occupied
    ?info("Start MCU monitor (slot: ~p)~n", [Idx+1]),
    {ok, Pid} = bkfw_mcus_sup:start_mcu(Idx+1),
    Slots2 = setelement(Idx+1, Slots, Pid),
    handle_slots(Slots2, Mask, Idx+1);

handle_slots(Slots, Mask, Idx) when
      Mask band (1 bsl Idx) /= 0, is_pid(element(Idx+1, Slots)) ->
    % slot is occupied, slot was occupied
    handle_slots(Slots, Mask, Idx+1).
