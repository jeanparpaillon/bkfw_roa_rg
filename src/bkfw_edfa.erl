%%%
%%% Monitor EDFA
%%%
-module(bkfw_edfa).
-author('jean.parpaillon@free.fr').

-behaviour(gen_server).

-include("bkfw.hrl").

%%%
%%% API
%%%
-export([
    start_link/0,
	enable/1,
	enabled/0,
    get_kv/1
]).

%% SNMP instrumentation
-export([variable_func/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PERIOD, 100).
-define(SLOTS, 128).
%%-define(TID, ?MODULE).

-define(FUNS, [
    fun read_n/1,
    fun read_it/1,
    fun read_v/1,
    fun read_a/1
]).

-record(state, {
	enable :: boolean(),
    slots :: tuple(),
    curInternalTemp = 0.0,
    powerSupply = 0.0,
	amp
}).

%%%
%%% API
%%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Start/pause protocol
enable(Enable) ->
	gen_server:call(?MODULE, {enable, Enable}).

%% Returned true if enabled, false otherwise
enabled() ->
	gen_server:call(?MODULE, enabled).

get_kv(1) ->
    [
        {curInternalTemp, get_ets_value(smmCurInternalTemp, 0.0)},
        {powerSupply, get_ets_value(smmPowerSupply, 0.0)},
        {vendor, get_ets_value(smmVendor, <<>>)},
        {moduleType, get_ets_value(smmModuleType, <<>>)},
        {hwVer, get_ets_value(smmHWVer, <<>>)},
        {hwRev, get_ets_value(smmHWRev, <<>>)},
        {swVer, get_ets_value(smmSWVer, <<>>)},
        {fwVer, get_ets_value(smmFWVer, <<>>)},
        {partNum, get_ets_value(smmPartNum, <<>>)},
        {serialNum, get_ets_value(smmSerialNum, <<>>)},
        {productDate, get_ets_value(smmProductDate, <<>>)}
    ];
get_kv(2) ->
    [
        {serialnum, get_ets_value(smmSerialNum, <<>>)},
        {partnum, get_ets_value(smmPartNum, <<>>)},
        {date, get_ets_value(smmProductDate, <<>>)},
        {vendor,
            case get_ets_value(smmVendor, <<>>) of
                <<"BKTel Photonics">> -> <<" Bktel\n      Photonics ">>;
                <<"Laser 2000">> -> <<"   Laser 2000   ">>;
                <<"Alnair">> -> <<"     Alnair     ">>;
                <<"Infractive">> -> <<"   Infractive   ">>;
                _ -> <<"vendor not\nconfigured">>
            end},
        {hard, get_ets_value(smmHWVer, <<>>)},
        {soft, get_ets_value(smmSWVer, <<>>)},
        {alarms, bkfw_alarms_srv:get(0)}
    ].

%%% SNMP functions
variable_func(new, _) ->
    {value, ok};
variable_func(delete, _) ->
    {value, ok};
variable_func(get, Key) ->
    case ets:lookup(?MODULE, Key) of
        [{Key, V}] -> snmp_value(Key, V);
        [] -> {value, noSuchName}
    end.

%%%
%%% Callbacks
%%%
init(_) ->
    ?info("Starting SMM monitoring", []),
    gen_event:add_handler(bkfw_alarms, bkfw_alarms_snmp, []),
    _Tid = ets:new(?MODULE, [public, named_table]),
    true = ets:insert(?MODULE, {smmNumber, 0}),
    true = ets:insert(?MODULE, {smmPowerSupply, 0.0}),
    true = ets:insert(?MODULE, {smmCurInternalTemp, 0.0}),
    S0 = #state{enable = true, slots = list_to_tuple(lists:duplicate(?SLOTS, false))},
    self() ! {edfa, [fun read_infos/1 | ?FUNS]},
    {ok, S0}.

handle_call(enabled, _From, S) ->
	{reply, S#state.enable, S};

handle_call({enable, true}, _From, S) ->
	case S#state.enable of
		true ->
			{reply, true, S};

		false ->
			self() ! {edfa, ?FUNS},
			{reply, false, S#state{enable = true}}
	end;

handle_call({enable, false}, _From, S) ->
    {reply, false, S#state{enable = false}}.

handle_cast(_Cast, S) ->
	{noreply, S}.

handle_info(_, #state{enable = false} = S) ->
	%% If protocol is disabled, ignore messages
	{noreply, S};

handle_info({edfa, []}, S) ->
    case mnesia:transaction(fun() -> mnesia:first(ampTable) end) of
        {atomic, Key} ->
			self() ! {init_mcu, Key},
            {noreply, S};
        {aborted, Err} ->
            ?error("Error reading AMP table: ~p~n", [Err]),
			timer:send_after(1000, self(), {edfa, ?FUNS}),
            {noreply, S}
    end;

handle_info({edfa, [Fun | Tail]}, S) ->
    case Fun(S) of
        {ok, S1} ->
			self() ! {edfa, Tail},
			{noreply, S1};
        {error, timeout, S1} ->
            ?debug("SMM ~p timeout. Retrying in ~p ms~n", [Fun, 1000]),
			timer:send_after(1000, self(), {edfa, [Fun | Tail]}),
			{noreply, S1};
        {error, {string, Err}, S1} ->
            ?error("SMM error: ~s~n", [Err]),
			self() ! {edfa, [Fun | Tail]},
			{noreply, S1};
        {error, Err, S1} ->
            ?error("SMM error: ~p~n", [Err]),
			self() ! {edfa, [Fun | Tail]},
			{noreply, S1}
    end;

handle_info({init_mcu, '$end_of_table'}, S) ->
	Period = application:get_env(bkfw, edfa_period, ?PERIOD),
	timer:send_after(Period, self(), {edfa, ?FUNS}),
	{noreply, S};

handle_info({init_mcu, Key}, S) ->
	case mnesia:transaction(fun() -> mnesia:read(ampTable, Key) end) of
        {atomic, []} ->
			Period = application:get_env(bkfw, edfa_period, ?PERIOD),
			timer:send_after(Period, self(), {edfa, ?FUNS}),
			{noreply, S};

        {atomic, [Amp]} ->
			self() ! {mcu, Key, bkfw_mcu:proto()},
			{noreply, S#state{amp = Amp}}
	end;

handle_info({mcu, Key, []}, S) ->
	{atomic, ok} = mnesia:transaction(fun() -> 
		mnesia:write(S#state.amp) 
	end),
	case mnesia:transaction(fun() -> mnesia:next(ampTable, Key) end) of
		{atomic, Key2} -> 
			self() ! {init_mcu, Key2},
			{noreply, S};
		{aborted, Err} -> 
			?error("Error reading AMP table: ~p~n", [Err]),
			self() ! {edfa, ?FUNS},
			{noreply, S}
	end;

handle_info({mcu, Key, [Fun | Tail]}, S) ->
	case Fun(S#state.amp) of
		{ok, Amp2} ->
			self() ! {mcu, Key, Tail},
			{noreply, S#state{amp = Amp2}};

		{error, timeout} ->
			?error("AMP ~p not responding~n", [Key]),
			self() ! {mcu, Key, []},
			{noreply, S};

		{error, Err} ->
			case Err of
				{string, E} -> ?error("Error monitoring AMP ~p: ~s~n", [Key, E]);
				E when is_list(E) -> ?error("Error monitoring AMP ~p: ~s~n", [Key, E]);
				E -> ?error("Error monitoring AMP ~p: ~p~n", [Key, E])
			end,
			self() ! {mcu, Key, []}
	end.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%
%%% Priv
%%%
snmp_value(smmPowerSupply, V) -> {value, round(V * 10)};
snmp_value(smmCurInternalTemp, V) -> {value, round(V * 10)};
snmp_value(ampGainConsign, V) -> {value, round(V * 10)};
snmp_value(ampOutputPowerConsign, V) -> {value, round(V * 10)};
snmp_value(ampCurLaserTemp, V) -> {value, round(V * 10)};
snmp_value(ampPowerPd1, V) -> {value, round(V * 10)};
snmp_value(ampPowerPd2, V) -> {value, round(V * 10)};
snmp_value(ampPowerPd3, V) -> {value, round(V * 10)};
snmp_value(ampPowerSupply, V) -> {value, round(V * 10)};
snmp_value(ampInputLossTh, V) -> {value, round(V * 10)};
snmp_value(ampOutputLossTh, V) -> {value, round(V * 10)};
snmp_value(ampPCMin, V) -> {value, round(V * 10)};
snmp_value(ampPCMax, V) -> {value, round(V * 10)};
snmp_value(ampGCMin, V) -> {value, round(V * 10)};
snmp_value(ampGCMax, V) -> {value, round(V * 10)};
snmp_value(_, V) when is_binary(V) -> {value, binary_to_list(V)};
snmp_value(_, V) -> {value, V}.

get_ets_value(Key, Default) ->
    try ets:lookup(?MODULE, Key) of
        [{Key, V}] -> V;
        [] -> Default
    catch
        _:_ -> Default
    end.

read_infos(S) ->
    Infos = application:get_env(bkfw, i, []),
    ets:insert(
        ?MODULE,
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
            {ok, S#state{powerSupply = V}};
        {ok, Ret} ->
            {error, {string, io_lib:format("RV invalid answer: ~p~n", [Ret])}, S};
        {error, Err} ->
            {error, Err, S}
    end.

read_it(S) ->
    case bkfw_srv:command(0, rit, []) of
        {ok, {0, it, [T, <<"C">>]}} when is_float(T); is_integer(T) ->
            ets:insert(?MODULE, {smmCurInternalTemp, T}),
            {ok, S#state{curInternalTemp = T}};
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
    mnesia:transaction(fun() -> mnesia:delete({ampTable, Idx + 1}) end),
    handle_slots(Mask, Idx + 1, S);
handle_slots(Mask, Idx, S) when Mask band (1 bsl Idx) /= 0 ->
    case mnesia:dirty_match_object(#ampTable{index = Idx + 1, _ = '_'}) of
        [] ->
            %% Amp was not there
            Amp = bkfw_mcu:new(Idx + 1),
            mnesia:transaction(fun() -> mnesia:write(Amp) end);
        _ ->
            %% Amp is already there
            ok
    end,
    ets:insert(?MODULE, {smmNumber, ets:lookup_element(?MODULE, smmNumber, 2) + 1}),
    handle_slots(Mask, Idx + 1, S).

handle_alarms([], S) ->
    {ok, S};
handle_alarms([Name | Tail], #state{curInternalTemp = IT, powerSupply = PS} = S) ->
    gen_event:notify(bkfw_alarms, #smmAlarm{index = 0, name = Name, obj = {IT, PS}}),
    bkfw_alarms_srv:set(0, Name),
    handle_alarms(Tail, S).
