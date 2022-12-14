-module(bkfw_mcu).
-author('jean.parpaillon@free.fr').

-include("bkfw.hrl").
-include("SMM-MIB.hrl").

-export([new/1,
		 proto/0,
		 get_kv/2,
		 set_kv/3]).

%%% SNMP functions
-export([table_func/2,
		 table_func/4]).

%% internals
-export([read_cc/1, 
		 read_gc/1, 
		 read_pc/1,
		 read_mode/1, 
		 read_lt/1, 
		 read_lc/1,
		 read_it/1,
		 read_pm/1,
		 read_i/1, 
		 read_v/1, 
		 read_li/1, 
		 read_lo/1, 
		 read_a/1,
		 read_rg1/1,
		 read_rg2/1,
		 read_rg3/1,
		 read_limits/1]).

-define(PERIOD, 100).
-define(FUNS, [fun set_pw/1,
			   fun read_pm/1, 
			   fun read_cc/1,
			   fun read_gc/1,
			   fun read_pc/1,
			   fun read_mode/1,
			   fun read_lt/1,
			   fun read_lc/1,
			   fun read_it/1,
			   fun read_i/1,
			   fun read_v/1,
			   fun read_li/1,
			   fun read_lo/1,
			   fun read_a/1,
			   fun read_rg1/1,
			   fun read_rg2/1,
			   fun read_rg3/1,
			   fun read_limits/1]).

new(Idx) ->
	Config = proplists:get_value(Idx, application:get_env(bkfw, amps, []), []),
	Params = #{ 
	  'has_PC_mode'      => proplists:get_value('has_PC_mode', Config, true),
	  'has_GC_mode'      => proplists:get_value('has_GC_mode', Config, true),
	  'has_input_PD'     => proplists:get_value('has_input_PD', Config, true),
	  'has_output_PD'    => proplists:get_value('has_output_PD', Config, true),
	  'has_settable_LD1' => proplists:get_value('has_settable_LD1', Config, true),
	  'number_of_laser'  => proplists:get_value('number_of_laser', Config, 1),
	  password           => proplists:get_value(password, Config, application:get_env(bkfw, amp_password, 0000)),
	  auth               => false
	 },
	#ampTable{ index=Idx, params=Params }.

proto() ->
	?FUNS.

get_kv(#ampTable{ params=Params }=T, 1) ->
    [
	 {index,               T#ampTable.index},
	 {ampConsign,          T#ampTable.ampConsign},
	 {ampConsign2,         T#ampTable.ampConsign2},
	 {gainConsign,         T#ampTable.gainConsign},
	 {outputPowerConsign,  T#ampTable.outputPowerConsign},
	 {operatingMode,       T#ampTable.operatingMode},
	 {curLaserTemp,        T#ampTable.curLaserTemp},
	 {curAmp,              T#ampTable.curAmp},
	 {curAmp2,             T#ampTable.curAmp2},
	 {curInternalAmp,      T#ampTable.curInternalTemp},
	 {powerInput,          T#ampTable.powerPd1},
	 {powerOutput,         T#ampTable.powerPd2},
	 {powerSupply,         T#ampTable.powerSupply},
	 {inputLossThreshold,  T#ampTable.inputLossThreshold},
	 {outputLossThreshold, T#ampTable.outputLossThreshold},
	 {ccMax1,              T#ampTable.ccMax1},
	 {ccMax2,              T#ampTable.ccMax2},
	 {pcMin,               T#ampTable.pcMin},
	 {pcMax,               T#ampTable.pcMax},
	 {gcMin,               T#ampTable.gcMin},
	 {gcMax,               T#ampTable.gcMax},
	 {'has_PC_mode',       maps:get('has_PC_mode', Params, true)},
	 {'has_GC_mode',       maps:get('has_GC_mode', Params, true)},
	 {'has_input_PD',      maps:get('has_input_PD', Params, true)},
	 {'has_output_PD',     maps:get('has_output_PD', Params, true)},
	 {'has_settable_LD1',  maps:get('has_settable_LD1', Params, true)},
	 {'number_of_laser',   maps:get('number_of_laser', Params, 1)},
	 {overallGain,         T#ampTable.overallGain},
	 {gainBeforeVoa,       T#ampTable.gainBeforeVoa},
	 {voaAttenuation,      T#ampTable.voaAttenuation}
	];


get_kv(#ampTable{ params=Params }=T, 2) ->
    [
     {index,               T#ampTable.index},
	 {mode,                T#ampTable.operatingMode},
	 {max_current_LD1,     T#ampTable.ccMax1},
	 {max_current_LD2,     T#ampTable.ccMax2},
	 {min_pc,              T#ampTable.pcMin},
	 {max_pc,              T#ampTable.pcMax},
	 {min_gc,              T#ampTable.gcMin},
	 {max_gc,              T#ampTable.gcMax},
	 {number_of_laser,     maps:get('number_of_laser', T#ampTable.params, 1)},
	 {has_settable_LD1,    maps:get('has_settable_LD1', T#ampTable.params, true)},
	 {alarms,              bkfw_alarms_srv:get(T#ampTable.index)},
	 {'LD1_current',       T#ampTable.curAmp},
	 {'LD2_current',       T#ampTable.curAmp2},
	 {input_power,         T#ampTable.powerPd1},
	 {output_power,        T#ampTable.powerPd2},
	 {internal_temp,       T#ampTable.curInternalTemp},
	 {'CC1_setpoint',      T#ampTable.ampConsign},
	 {'CC2_setpoint',      T#ampTable.ampConsign2},
	 {'PC_setpoint',       T#ampTable.outputPowerConsign},
	 {'GC_setpoint',       T#ampTable.gainConsign},
	 {'has_PC_mode',       maps:get('has_PC_mode', Params, true)},
	 {'has_GC_mode',       maps:get('has_GC_mode', Params, true)},
	 {'has_input_PD',      maps:get('has_input_PD', Params, true)},
	 {'has_output_PD',     maps:get('has_output_PD', Params, true)},
	 {overallGain,         T#ampTable.overallGain},
	 {gainBeforeVoa,       T#ampTable.gainBeforeVoa},
	 {voaAttenuation,      T#ampTable.voaAttenuation}
    ].


set_kv(Idx, Kv, 1) ->
	[Amp] = mnesia:dirty_read(ampTable, Idx),
    case get_kv_integer(operatingMode, Kv) of
		undefined ->
			set_thresholds(Idx, Kv);
		?ampOperatingMode_off ->
			set_operating_mode(Idx, ?ampOperatingMode_off, Amp);
		?ampOperatingMode_cc ->
			case set_operating_mode(Idx, {?ampOperatingMode_cc, 1, get_consign(ampConsign, Kv)}, Amp) of
				ok ->
					set_operating_mode(Idx, {?ampOperatingMode_cc, 2, get_consign(ampConsign2, Kv)}, Amp);
				{error, _}=Err ->
					Err
			end;
		?ampOperatingMode_gc ->
			set_operating_mode(Idx, {?ampOperatingMode_gc, get_consign(gainConsign, Kv)}, Amp);
		?ampOperatingMode_pc ->
			set_operating_mode(Idx, {?ampOperatingMode_pc, get_consign(outputPowerConsign, Kv)}, Amp)
    end;

set_kv(Idx, Kv, 2) ->
	%%?debug("Set MCU #~p: ~p", [Idx, Kv]),
	F = fun (_, {error, _}=Err) ->
				Err;
			({mode, Mode}, _Acc) ->
				set_operating_mode2(Idx, Mode);
			({'CC1_setpoint', V}, _Acc) ->
				bkfw_cmd:call(Idx, scc, [io_lib:format("~b ~.2f", [1, to_float(V)])]);
			({'CC2_setpoint', V}, _Acc) ->
				bkfw_cmd:call(Idx, scc, [io_lib:format("~b ~.2f", [2, to_float(V)])]);
			({'GC_setpoint', V}, _Acc) ->
				bkfw_cmd:call(Idx, sgc, [io_lib:format("~.2f", [to_float(V)])]);
			({'PC_setpoint', V}, _Acc) ->
				bkfw_cmd:call(Idx, spc, [io_lib:format("~.2f", [to_float(V)])]);
			(_, _Acc) ->
				{error, invalid_key}
		end,
	case lists:foldl(F, ok, Kv) of
		ok -> ok;
		{ok, _} -> ok;
		{error, _}=Err -> Err
	end.


%%% SNMP functions
table_func(new, NameDb) ->
    snmp_generic:table_func(new, NameDb);
table_func(delete, NameDb) ->
    snmp_generic:table_func(delete, NameDb).


table_func(is_set_ok, RowIndex, Cols, NameDb) ->
    snmp_generic:table_func(is_set_ok, RowIndex, Cols, NameDb);

table_func(set, [RowIndex], Cols, _NameDb) ->
    case set_from_snmp(RowIndex, Cols) of
		ok ->
			{noError, 0};
		{error, Col} ->
			{error, Col}
    end;

table_func(get, RowIndex, Cols, NameDb) ->
    Vars = snmp_generic:table_func(get, RowIndex, Cols, NameDb),
    lists:map(fun ({value, V}) -> snmp_value(Cols, V) end, Vars);

table_func(get_next, RowIndex, Cols, NameDb) ->
	case snmp_generic:table_func(get_next, RowIndex, Cols, NameDb) of
		[] -> [];
		Next when is_list(Next) ->
			lists:map(fun (endOfTable) ->
							endOfTable;

						({[Col, _] = NextOID, Value}) ->
							{value, CastedValue} = snmp_value([Col], Value),
							{NextOID, CastedValue}
					  end, Next);
		{genErr, Cols} -> 
			{genErr, Cols}
    end;

table_func(Op, RowIndex, Cols, NameDb) ->
    snmp_generic:table_func(Op, RowIndex, Cols, NameDb).

%%%
%%% Internals
%%%
snmp_value([?ampGainConsign], V) -> {value, round(V * 10)};
snmp_value([?ampOutputPowerConsign], V) -> {value, round(V * 10)};
snmp_value([?ampCurLaserTemp], V) -> {value, round(V * 10)};
snmp_value([?ampPowerPd1], V) -> {value, round(V * 10)};
snmp_value([?ampPowerPd2], V) -> {value, round(V * 10)};
snmp_value([?ampPowerPd3], V) -> {value, round(V * 10)};
snmp_value([?ampPowerSupply], V) -> {value, round(V * 10)};
snmp_value([?ampInputLossTh], V) -> {value, round(V * 10)};
snmp_value([?ampOutputLossTh], V) -> {value, round(V * 10)};
snmp_value([?ampPCMin], V) -> {value, round(V * 10)};
snmp_value([?ampPCMax], V) -> {value, round(V * 10)};
snmp_value([?ampGCMin], V) -> {value, round(V * 10)};
snmp_value([?ampGCMax], V) -> {value, round(V * 10)};
snmp_value(_, V) when is_float(V) -> {value, round(V)};
snmp_value(_, V) when is_binary(V) -> {value, binary_to_list(V)};
snmp_value(_, V) -> {value, V}.

set_pw(#ampTable{index=Idx, params=Params}=E) ->
	Passwd = maps:get(password, Params, 0000),
    case bkfw_cmd:call(Idx, spw, [integer_to_binary(Passwd)]) of
		{ok, {Idx, pwd, [ok]}} ->
			{ok, E#ampTable{ params=Params#{ auth := true }}};
		{ok, {Idx, pwd, [nok]}} ->
			?error("Authentication failed on amp #~b", [Idx]),
			{ok, E};
		{ok, _Ret} ->
			{error, {string, io_lib:format("SPW invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.
	

read_cc(#ampTable{index=Idx, params=Params}=E) ->
    F = fun(X, {ok, Acc}) ->
				case bkfw_cmd:call(Idx, rcc, [integer_to_binary(X)]) of
					{ok, {Idx, cc, [1, A, <<"mA">>]}} when is_float(A); is_integer(A) ->
						{ok, Acc#ampTable{ ampConsign=A }};
					{ok, {Idx, cc, [2, A, <<"mA">>]}} when is_float(A); is_integer(A) ->
						{ok, Acc#ampTable{ ampConsign2=A }};
					{ok, {Idx, cc, [X, A, <<"mA">>]}} when is_float(A); is_integer(A) ->
						?debug("Laser #~p current consign: ~p (ignore)", [X, A]),
						{ok, Acc};
					{ok, _Ret} ->
						{error, {string, io_lib:format("RCC invalid answer: ~p~n", [_Ret])}};
					{error, Err} ->
						{error, Err}
				end;
		   (_, {error, Err}) ->
				{error, Err}
		end,
    lists:foldl(F, {ok, E}, lists:seq(1, maps:get('number_of_laser', Params, 1))).

read_gc(#ampTable{index=Idx}=E) ->
    case bkfw_cmd:call(Idx, rgc, []) of
		{ok, {Idx, gc, [Y, <<"dB">>]}} when is_float(Y); is_integer(Y) ->
			{ok, E#ampTable{gainConsign=Y}};
		{ok, _Ret} ->
			{error, {string, io_lib:format("RGC invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.

read_pc(#ampTable{index=Idx}=E) ->
    case bkfw_cmd:call(Idx, rpc, []) of
		{ok, {Idx, pc, [Y, <<"dBm">>]}} when is_float(Y); is_integer(Y) ->
			{ok, E#ampTable{outputPowerConsign=Y}};
		{ok, _Ret} ->
			{error, {string, io_lib:format("RPC invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.

read_mode(#ampTable{index=Idx}=E) ->
    case bkfw_cmd:call(Idx, rmode, []) of
		{ok, {Idx, mode, [Mode]}} ->
			M = parse_mode(Mode, E#ampTable.operatingMode),
			{ok, E#ampTable{operatingMode=M}};
		{ok, _Ret} ->
			{error, {string, io_lib:format("RMODE invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.

read_a(#ampTable{index=Idx}=E) ->
    case bkfw_cmd:call(Idx, ra, []) of
		{ok, {Idx, alarms, Alarms}} ->
			handle_alarms(Alarms, E);
		{ok, _Ret} ->
			{error, {string, io_lib:format("RA invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.

read_rg1(#ampTable{index=Idx}=E) ->
    case bkfw_cmd:call(Idx, rg, [<<"1">>]) of
		{ok, {Idx, gain, [1, G, _]}} ->
			{ok, E#ampTable{overallGain=G}};
		{ok, _Ret} ->
			{error, {string, io_lib:format("RG 1 invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.

read_rg2(#ampTable{index=Idx}=E) ->
    case bkfw_cmd:call(Idx, rg, [<<"2">>]) of
		{ok, {Idx, gain, [2, G, _]}} ->
			{ok, E#ampTable{gainBeforeVoa=G}};
		{ok, _Ret} ->
			{error, {string, io_lib:format("RG 2 invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.

read_rg3(#ampTable{index=Idx}=E) ->
    case bkfw_cmd:call(Idx, rg, [<<"3">>]) of
		{ok, {Idx, gain, [3, G, _]}} ->
			{ok, E#ampTable{voaAttenuation=G}};
		{ok, _Ret} ->
			{error, {string, io_lib:format("RG 1 invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.

read_lt(#ampTable{ index=Idx, params=Params }=E) ->
    F = fun(X, {ok, Acc}) ->
				case bkfw_cmd:call(Idx, rlt, [integer_to_binary(X)]) of
					{ok, {Idx, lt, [1, T, <<"C">>]}} when is_float(T); is_integer(T) ->
						{ok, Acc#ampTable{ curLaserTemp=T }};
					{ok, {Idx, lt, [X, T, <<"C">>]}} when is_float(T); is_integer(T) ->
						?debug("laser #~p temp: ~p (ignore)", [X, T]),
						{ok, Acc};
					{ok, _Ret} ->
						{error, {string, io_lib:format("RLT invalid answer: ~p~n", [_Ret])}};
					{error, Err} ->
						{error, Err}
				end;
		   (_, {error, Err}) ->
				{error, Err}
		end,
    lists:foldl(F, {ok, E}, lists:seq(1, maps:get('number_of_laser', Params, 1))).

read_lc(#ampTable{ index=Idx, params=Params }=E) ->
    F = fun(X, {ok, Acc}) ->
				case bkfw_cmd:call(Idx, rlc, [integer_to_binary(X)]) of
					{ok, {Idx, lc, [1, A, <<"mA">>]}} when is_float(A); is_integer(A) ->
						{ok, Acc#ampTable{ curAmp=A }};
					{ok, {Idx, lc, [2, A, <<"mA">>]}} when is_float(A); is_integer(A) ->
						{ok, Acc#ampTable{ curAmp2=A }};
					{ok, {Idx, lc, [X, A, <<"mA">>]}} when is_float(A); is_integer(A) ->
						?debug("laser #~p current: ~p (ignore)", [X, A]),
						{ok,Acc};
					{ok, _Ret} ->
						{error, {string, io_lib:format("RLC invalid answer: ~p~n", [_Ret])}};
					{error, Err} ->
						{error, Err}
				end
		end,
    lists:foldl(F, {ok, E}, lists:seq(1, maps:get('number_of_laser', Params, 1))).

read_it(#ampTable{index=Idx}=E) ->
    case bkfw_cmd:call(Idx, rit, []) of
		{ok, {Idx, it, [T, <<"C">>]}} when is_float(T); is_integer(T) ->
			{ok, E#ampTable{curInternalTemp=T}};
		{ok, _Ret} ->
			{error, {string, io_lib:format("RIT invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.

read_i(#ampTable{}=E) ->
	{ok, E}.

read_pm(#ampTable{index=Idx}=E) ->
    case bkfw_cmd:call(Idx, rpm, []) of
		{ok, {Idx, pd, Lines}} ->
			E1 = parse_pd(Lines, E),
			{ok, E1};
		{ok, _Ret} ->
			{error, {string, io_lib:format("RPM invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.

read_v(#ampTable{index=Idx}=E) ->
    case bkfw_cmd:call(Idx, rv, []) of
		{ok, {Idx, v, [V, v]}} when is_float(V); is_integer(V) ->
			{ok, E#ampTable{powerSupply=V}};
		{ok, _Ret} ->
			{error, {string, io_lib:format("RV invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.

read_li(#ampTable{index=Idx}=E) ->
    case bkfw_cmd:call(Idx, rli, []) of
		{ok, {Idx, li, [Y, <<"dBm">>]}} ->
			{ok, E#ampTable{inputLossThreshold=Y}};
		{ok, _Ret} ->
			{error, {string, io_lib:format("RLI invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.

read_lo(#ampTable{index=Idx}=E) ->
    case bkfw_cmd:call(Idx, rlo, []) of
		{ok, {Idx, lo, [Y, <<"dBm">>]}} ->
			{ok, E#ampTable{outputLossThreshold=Y}};
		{ok, _Ret} ->
			{error, io_lib:format("RLO invalid answer: ~p~n", [_Ret])};
		{error, Err} ->
			{error, Err}
    end.


read_limits(#ampTable{ params=#{ auth := false }}=E) ->
	{ok, E};

read_limits(#ampTable{ index=Idx, params=Params }=E) ->
    F = fun(X, {ok, Acc}) ->
				case bkfw_cmd:call(Idx, rlcc, [integer_to_binary(X)]) of
					{ok, {Idx, max, [_, _, A, <<"mA">>]}} when is_float(A); is_integer(A) ->
						case X of
							1 ->
								{ok, Acc#ampTable{ ccMax1=A }};
							2 ->
								{ok, Acc#ampTable{ ccMax2=A }};
							_ ->
								?debug("laser #~p max current: ~p", [X, A]),
								{ok, Acc}
						end;
					{ok, _Ret} ->
						{error, {string, io_lib:format("RLCC ~b invalid answer: ~p~n", [_Ret, X])}};
					{error, Err} ->
						{error, Err}
				end
		end,
    case lists:foldl(F, {ok, E}, lists:seq(1, maps:get('number_of_laser', Params, 1))) of
		{ok, E1} ->
			read_limits_pc(E1);
		{error, Err} ->
			{error, Err}
	end.

read_limits_pc(#ampTable{ index=Idx }=E) ->
    case bkfw_cmd:call(Idx, rlpc, []) of
		{ok, {Idx, pc, [ [min, Min, _], [max, Max, _] ]}} ->
			E1 = E#ampTable{ pcMin=Min, pcMax=Max },
			read_limits_gc(E1);
		{ok, _Ret} ->
			{error, iolist_to_binary(io_lib:format("RLPC invalid answer: ~p~n", [_Ret]))};
		{error, Err} ->
			{error, Err}
    end.


read_limits_gc(#ampTable{ index=Idx }=E) ->
    case bkfw_cmd:call(Idx, rlgc, []) of
		{ok, {Idx, gc, [ [min, Min, _], [max, Max, _] ]}} ->
			E1 = E#ampTable{ gcMin=Min, gcMax=Max },
			{ok, E1};
		{ok, _Ret} ->
			{error, iolist_to_binary(io_lib:format("RLGC invalid answer: ~p~n", [_Ret]))};
		{error, Err} ->
			{error, Err}
    end.

%%%
%%% Convenience functions
%%%
parse_mode(pc, _) -> ?ampOperatingMode_pc;
parse_mode(gc, _) -> ?ampOperatingMode_gc;
parse_mode(cc, _) -> ?ampOperatingMode_cc;
parse_mode(off, _) -> ?ampOperatingMode_off.


parse_pd([], Acc) -> 
	Acc;

parse_pd([ [1, P, <<"dBm">>] | Tail], #ampTable{}=E) -> 
	parse_pd(Tail, E#ampTable{ powerPd1=P });

parse_pd([ [2, P, <<"dBm">>] | Tail], #ampTable{}=E) -> 
	parse_pd(Tail, E#ampTable{ powerPd2=P });

parse_pd([ [3, P, <<"dBm">>] | Tail], #ampTable{}=E) -> 
	parse_pd(Tail, E#ampTable{ powerPd3=P });

parse_pd([ [_, _, <<"dBm">>] | Tail], E) -> 
	parse_pd(Tail, E).


handle_alarms([], E) -> {ok, E};
handle_alarms([Name  | Tail], E) -> 
    gen_event:notify(bkfw_alarms, #smmAlarm{index=E#ampTable.index,
											name=Name,
											obj=E}),
	bkfw_alarms_srv:set(E#ampTable.index, Name),
    handle_alarms(Tail, E).

get_consign(Name, Kv) ->
    case proplists:get_value(Name, Kv) of
		undefined -> undefined;
		F when is_float(F) -> 
			F;
		I when is_integer(I) ->
			I + 0.0;
		Bin ->
			try binary_to_integer(Bin) of
				I -> I + 0.0
			catch
				error:badarg ->
					try binary_to_float(Bin) of
						F -> F
					catch
						error:badarg ->
							undefined
					end
			end
    end.

set_from_snmp(_, []) ->
    ok;
set_from_snmp(Idx, [{?ampAmpConsign, Val} | Tail]) when is_integer(Val) ->
    bkfw_cmd:call(Idx, scc, [<<"1 ">>, io_lib:format("~b.0", [Val])]),
    set_from_snmp(Idx, Tail);
set_from_snmp(Idx, [{?ampGainConsign, Val} | Tail]) when is_integer(Val) ->
    bkfw_cmd:call(Idx, sgc, [io_lib:format("~.1f", [Val / 10])]),
    set_from_snmp(Idx, Tail);
set_from_snmp(Idx, [{?ampOutputPowerConsign, Val} | Tail]) when is_integer(Val) ->
    bkfw_cmd:call(Idx, spc, [io_lib:format("~.1f", [Val / 10])]),
    set_from_snmp(Idx, Tail);
set_from_snmp(Idx, [{?ampInputLossTh, Val} | Tail]) when is_integer(Val) ->
    bkfw_cmd:call(Idx, sli, [io_lib:format("~.1f", [Val / 10])]),
    set_from_snmp(Idx, Tail);
set_from_snmp(Idx, [{?ampOutputLossTh, Val} | Tail]) when is_integer(Val) ->
    bkfw_cmd:call(Idx, slo, [io_lib:format("~.1f", [Val / 10])]),
    set_from_snmp(Idx, Tail);

set_from_snmp(Idx, [{?ampOperatingMode, Val} | Tail]) ->
    case Val of
		?ampOperatingMode_off -> 
			bkfw_cmd:call(Idx, smode, [<<"OFF">>]),
			set_from_snmp(Idx, Tail);
		?ampOperatingMode_cc -> 
			bkfw_cmd:call(Idx, smode, [<<"CC">>]),
			set_from_snmp(Idx, Tail);
		?ampOperatingMode_gc -> 
			bkfw_cmd:call(Idx, smode, [<<"GC">>]),
			set_from_snmp(Idx, Tail);
		?ampOperatingMode_pc -> 
			bkfw_cmd:call(Idx, smode, [<<"PC">>]),
			set_from_snmp(Idx, Tail);
		_ -> 
			{error, ?ampOperatingMode}
    end;
set_from_snmp(_, [{Col, _} | _]) ->
    {error, Col}.


set_operating_mode(Idx, ?ampOperatingMode_off, _Amp) ->
    bkfw_cmd:call(Idx, smode, [<<"OFF">>]),
    ok;

set_operating_mode(_Idx, {?ampOperatingMode_cc, 1, _V}, #ampTable{ params=#{ 'has_settable_LD1' := false } }) ->
	{error, unsettableLD1};

set_operating_mode(_Idx, {?ampOperatingMode_cc, 1, V}, #ampTable{ ccMax1=Max }) 
  when V > Max ->
	{error, ofr};

set_operating_mode(_Idx, {?ampOperatingMode_cc, 2, V}, #ampTable{ ccMax2=Max }) 
  when V > Max ->
	{error, ofr};

set_operating_mode(Idx, {?ampOperatingMode_cc, Laser, V}, _Amp) ->
	case bkfw_cmd:call(Idx, scc, [io_lib:format("~b ~.2f", [Laser, V])]) of
		{ok, {Idx, scc, [Laser, ofr]}} ->
			{error, ofr};
		_ ->
			bkfw_cmd:call(Idx, smode, [<<"CC">>]),
			ok
	end;

set_operating_mode(_Idx, {?ampOperatingMode_gc, V}, #ampTable{ gcMin=Min, gcMax=Max }) 
  when V > Max orelse V < Min ->
	{error, ofr};

set_operating_mode(Idx, {?ampOperatingMode_gc, V}, _Amp) ->
	case bkfw_cmd:call(Idx, sgc, [io_lib:format("~.2f", [V])]) of
		{ok, {Idx, sgc, [ofr]}} ->
			{error, ofr};
		_ ->
			bkfw_cmd:call(Idx, smode, [<<"GC">>]),
			ok
	end;

set_operating_mode(_Idx, {?ampOperatingMode_pc, V}, #ampTable{ pcMin=Min, pcMax=Max }) 
  when V > Max orelse V < Min ->
	{error, ofr};

set_operating_mode(Idx, {?ampOperatingMode_pc, V}, _Amp) ->
	case bkfw_cmd:call(Idx, spc, [io_lib:format("~.2f", [V])]) of
		{ok, {Idx, spc, [ofr]}} ->
			{error, ofr};
		_ ->
			bkfw_cmd:call(Idx, smode, [<<"PC">>]),
			ok
	end;

set_operating_mode(_, {_, undefined}, _Amp) ->
	{error, missing_consign};

set_operating_mode(_, _, _Amp) ->
    {error, internal}.


set_operating_mode2(Idx, Mode) when is_binary(Mode) ->
	set_operating_mode2(Idx, binary_to_integer(Mode));

set_operating_mode2(Idx, ?ampOperatingMode_off) ->
	bkfw_cmd:call(Idx, smode, [<<"OFF">>]);

set_operating_mode2(Idx, ?ampOperatingMode_cc) ->
	bkfw_cmd:call(Idx, smode, [<<"CC">>]);

set_operating_mode2(Idx, ?ampOperatingMode_gc) ->
	bkfw_cmd:call(Idx, smode, [<<"GC">>]);

set_operating_mode2(Idx, ?ampOperatingMode_pc) ->
	bkfw_cmd:call(Idx, smode, [<<"PC">>]).


to_float(I) when is_float(I) ->
	I;

to_float(I) when is_integer(I) ->
	float(I);

to_float(I) when is_binary(I) ->
	binary_to_float(I).


set_thresholds(Idx, Kv) ->
    case get_kv_float(inputLossThreshold, Kv) of
		undefined ->
			{error, invalid_thresholds};
		IT ->
			case bkfw_cmd:call(Idx, sli, [io_lib:format("~.2f", [IT])]) of
				{ok, {Idx, sli, _}} ->
					case get_kv_float(outputLossThreshold, Kv) of
						undefined ->
							{error, invalid_thresholds};
						OT ->
							case bkfw_cmd:call(Idx, slo, [io_lib:format("~.2f", [OT])]) of
								{ok, {Idx, slo, _}} ->
									ok;
								_ ->
									{error, internal}
							end
					end;
				_ ->
					{error, internal}
			end
    end.

get_kv_integer(Key, Props) ->
    case proplists:get_value(Key, Props) of
		undefined -> undefined;
		I when is_integer(I) -> I;
		F when is_float(F) -> undefined;
		Else ->
			try binary_to_integer(Else) of
				I -> I
			catch error:badarg ->
					undefined
			end
    end.

get_kv_float(Key, Props) ->
    case proplists:get_value(Key, Props) of
		undefined -> undefined;
		I when is_integer(I) -> I + 0.0;
		F when is_float(F) -> F;
		Else ->
			try binary_to_integer(Else) of
				I -> I + 0.0
			catch error:badarg ->
					try binary_to_float(Else) of
						F -> F
					catch error:badarg ->
							undefined
					end
			end
    end.
