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
		 get_kv/1]).

%% SNMP instrumentation
-export([variable_func/2]).

-define(PERIOD, 100).
-define(SLOTS, 128).
%%-define(TID, ?MODULE).

-define(FUNS, [fun read_n/1, fun read_it/1, fun read_v/1, fun read_a/1]).

-record(state, {
		  slots                    :: tuple(),
		  curInternalTemp = 0.0,
		  powerSupply = 0.0
		 }).

%%%
%%% API
%%%
start_link() ->
	Pid = spawn_link(fun init/0),
	{ok, Pid}.


init() ->
    ?info("Starting SMM monitoring", []),
	gen_event:add_handler(bkfw_alarms, bkfw_alarms_snmp, []),
	_Tid = ets:new(?MODULE, [public, named_table]),
	true = ets:insert(?MODULE, {smmNumber, 0}),
	loop([ fun read_infos/1 | ?FUNS ], #state{ slots=list_to_tuple(lists:duplicate(?SLOTS, false))}).


get_kv(1) ->
    [
     {curInternalTemp, get_ets_value(smmCurInternalTemp, 0.0)},
     {powerSupply,     get_ets_value(smmPowerSupply, 0.0)},
     {vendor,          get_ets_value(smmVendor, <<>>)},
     {moduleType,      get_ets_value(smmModuleType, <<>>)},
     {hwVer,           get_ets_value(smmHWVer, <<>>)},
     {hwRev,           get_ets_value(smmHWRev, <<>>)},
     {swVer,           get_ets_value(smmSWVer, <<>>)},
     {fwVer,           get_ets_value(smmFWVer, <<>>)},
     {partNum,         get_ets_value(smmPartNum, <<>>)},
     {serialNum,       get_ets_value(smmSerialNum, <<>>)},
     {productDate,     get_ets_value(smmProductDate, <<>>)}
    ];

get_kv(2) ->
	[
	 {serialnum,       get_ets_value(smmSerialNum, <<>>)},
	 {partnum,         get_ets_value(smmPartNum, <<>>)},
	 {date,            get_ets_value(smmProductDate, <<>>)},
	 {vendor,          case get_ets_value(smmVendor, <<>>) of
						   <<"BKTel Photonics">> -> <<" Bktel\n      Photonics ">>;
						   <<"Laser 2000">> -> <<"   Laser 2000   ">>;
						   <<"Alnair">> -> <<"     Alnair     ">>;
						   <<"Infractive">> -> <<"   Infractive   ">>;
						   _ -> <<"vendor not\nconfigured">>
					   end},
	 {hard,            get_ets_value(smmHWVer, <<>>)},
	 {soft,            get_ets_value(smmSWVer, <<>>)}
	].


%%% SNMP functions
variable_func(new, _) ->
    {value, ok};

variable_func(delete, _) ->
    {value, ok};

variable_func(get, Key) ->
	case ets:lookup(?MODULE, Key) of
		[{Key, V}] when is_float(V) -> {value, round(V)};
		[{Key, V}] when is_binary(V) -> {value, binary_to_list(V)};
		[{Key, V}] -> {value, V};
		[] -> {value, noSuchName}
	end.


%%%
%%% Priv
%%% 
get_ets_value(Key, Default) ->
	try ets:lookup(?MODULE, Key) of
		[{Key, V}] -> V;
		[] -> Default
	catch _:_ -> Default
	end.


read_infos(S) ->
	Infos = application:get_env(bkfw, i, []),
	ets:insert(?MODULE,
			   [
				{smmVendor, get_info(vendor, Infos)},
				{smmModuleType, get_info(moduleType, Infos)},
				{smmHWVer, get_info(hwVer, Infos)},
				{smmHWRev, get_info(hwRev, Infos)},
				{smmSWVer, get_info(swVer, Infos)},
				{smmFWVer, get_info(fwVer, Infos)},
				{smmPartNum, get_info(partNum, Infos)},
				{smmSerialNum, get_info(serialNum, Infos)},
				{smmProductDate, get_info(productDate, Infos)}
			   ]
			  ),
	{ok, S}.


read_v(S) ->
	case bkfw_srv:command(0, rv, []) of
		{ok, {0, v, [V, v]}} when is_float(V); is_integer(V) ->
			ets:insert(?MODULE, {smmPowerSupply, V}),
			{ok, S#state{powerSupply=V}};
		{ok, Ret} ->
			{error, {string, io_lib:format("RV invalid answer: ~p~n", [Ret])}, S};
		{error, Err} ->
			{error, Err, S}
    end.

read_it(S) ->
	case bkfw_srv:command(0, rit, []) of
		{ok, {0, it, [T, <<"C">>]}} when is_float(T); is_integer(T) ->
			ets:insert(?MODULE, {smmCurInternalTemp, T}),
			{ok, S#state{curInternalTemp=T}};
		{ok, Ret} ->
			{error, {string, io_lib:format("RIT invalid answer: ~p~n", [Ret])}, S};
		ok ->
			{error, {string, io_lib:format("RIT invalid answer: ok~n", [])}, S};
		{error, Err} ->
			{error, Err, S}
    end.


read_n(S) ->
    ets:insert(?MODULE, {smmNumber, 0}),
    case bkfw_srv:command(0, rn, []) of
		{ok, {0, n, [Mask]}} when is_integer(Mask) ->
			handle_slots(Mask, 0, S);
		{ok, Ret} ->
			{error, {string, io_lib:format("RN invalid answer: ~p~n", [Ret])}, S};
		ok ->
			{error, {string, io_lib:format("RN invalid answer: ok~n", [])}, S};
		{error, Err} ->
			{error, Err, S}
    end.


read_a(S) ->
    case bkfw_srv:command(0, ra, []) of
		{ok, {0, alarms, Alarms}} ->
			handle_alarms(Alarms, S);
		{ok, Ret} ->
			{error, {string, io_lib:format("RA invalid answer: ~p~n", [Ret])}, S};
		ok ->
			{error, {string, io_lib:format("RA invalid answer: ok~n", [])}, S};
		{error, Err} ->
			{error, Err, S}
    end.


get_info(Key, Infos) ->
    case proplists:get_value(Key, Infos, <<>>) of
		<<>> ->
			<<>>;
		S when is_list(S) ->
			list_to_binary(S)
	end.


%% loop over all bits of mask and compare with old slots,
%% start or kill bkfw_mon if necessary
handle_slots(_Mask, ?SLOTS, S) ->
    {ok, S};

handle_slots(Mask, Idx, S) when Mask band (1 bsl Idx) == 0 ->
    mnesia:transaction(fun () -> mnesia:delete({ampTable, Idx+1}) end),
    handle_slots(Mask, Idx+1, S);

handle_slots(Mask, Idx, S) when Mask band (1 bsl Idx) /= 0 ->
    case mnesia:dirty_match_object(#ampTable{index=Idx+1, _='_'}) of
		[] ->
			%% Amp was not there
			Amp = #ampTable{index=Idx+1},
			mnesia:transaction(fun() -> mnesia:write(Amp) end);
		_ ->
			%% Amp is already there
			ok
    end,
    ets:insert(?MODULE, {smmNumber, ets:lookup_element(?MODULE, smmNumber, 2)+1}),
    handle_slots(Mask, Idx+1, S).


handle_alarms([], S) -> 
    {ok, S};

handle_alarms([Name  | Tail], #state{curInternalTemp=IT, powerSupply=PS}=S) -> 
    gen_event:notify(bkfw_alarms, #smmAlarm{index=0, name=Name, obj={IT, PS}}),
    handle_alarms(Tail, S).


loop([], S) ->
    case mnesia:transaction(fun () -> mnesia:first(ampTable) end) of
		{atomic, Key} ->
			S1 = loop_mcu(Key, S),
			Period = application:get_env(bkfw, edfa_period, ?PERIOD),
			timer:sleep(Period),
			loop(?FUNS, S1);
		{aborted, Err} ->
			?error("Error reading AMP table: ~p~n", [Err])
    end;

loop([Fun | Tail], S) ->
    case Fun(S) of
		{ok, S1} ->
			loop(Tail, S1);
		{error, timeout, S1} ->
			?debug("SMM ~p timeout. Retrying in ~p ms~n", [Fun, 1000]),
			timer:sleep(1000),
			loop([ Fun | Tail ], S1);
		{error, {string, Err}, S1} ->
			?error("SMM error: ~s~n", [Err]),
			loop([ Fun | Tail ], S1);
		{error, Err, S1} ->
			?error("SMM error: ~p~n", [Err]),
			loop([ Fun | Tail ], S1)
    end.


loop_mcu('$end_of_table', S) ->
    S;

loop_mcu(Key, S) ->
    case mnesia:transaction(fun () -> mnesia:read(ampTable, Key) end) of
		{atomic, []} ->
			{ok, S};
		{atomic, [Amp]} -> 
			case bkfw_mcu:loop(Amp) of
				{ok, Amp2} ->
					{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Amp2) end),
					case mnesia:transaction(fun() -> mnesia:next(ampTable, Key) end) of
						{atomic, Key2} -> loop_mcu(Key2, S);
						{aborted, Err} -> ?error("Error reading AMP table: ~p~n", [Err])
					end;
				{error, timeout} ->
					?error("AMP ~p not responding~n", [Key]),
					loop(?FUNS, S);
				{error, Err, Amp2} ->
					case Err of
						{string, E} -> ?error("Error monitoring AMP ~p: ~s~n", [Key, E]);
						E when is_list(E) -> ?error("Error monitoring AMP ~p: ~s~n", [Key, E]);
						E -> ?error("Error monitoring AMP ~p: ~p~n", [Key, E])
					end,
					mnesia:transaction(fun() -> mnesia:write(Amp2) end),
					case mnesia:transaction(fun() -> mnesia:next(ampTable, Key) end) of
						{atomic, Key2} -> loop_mcu(Key2, S);
						{aborted, Err} -> ?error("Error reading AMP table: ~p~n", [Err])
					end
			end
    end.
