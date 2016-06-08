-module(bkfw_mcu).
-author('jean.parpaillon@free.fr').

-include("bkfw.hrl").

-export([new/1,
		 loop/1,
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
		 read_limits/1]).

-define(PERIOD, 100).
-define(FUNS, [fun read_pm/1, 
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
			   fun read_limits/1]).

new(Idx) ->
	Config = proplists:get_value(Idx, application:get_env(bkfw, amps, []), []),
	Params = #{ 
	  'has_PC_mode'      => proplists:get_value('has_PC_mode', Config, true),
	  'has_GC_mode'      => proplists:get_value('has_GC_mode', Config, true),
	  'has_input_PD'     => proplists:get_value('has_input_PD', Config, true),
	  'has_output_PD'    => proplists:get_value('has_output_PD', Config, true),
	  'has_settable_LD1' => proplists:get_value('has_settable_LD1', Config, true),
	  lasers             => []
	 },
	#ampTable{ index=Idx, params=Params }.


loop(#ampTable{}=Amp) ->
    loop(Amp, ?FUNS).

loop(Amp, []) ->
    {ok, Amp};

loop(Amp, [ Fun | Tail ]) ->
    case Fun(Amp) of
		{ok, Amp2} ->
			loop(Amp2, Tail);
		{error, timeout} -> {error, timeout};
		{error, Err} -> {error, Err, Amp}
    end.

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
	 {'lasers',            maps:get(lasers, Params, [])}
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
	 {number_of_laser,     length(maps:get(lasers, T#ampTable.params, [1]))},
	 {has_settable_LD1,    maps:get('has_settable_LD1', T#ampTable.params, true)},
	 {alarms,              []},
	 {'LD1_current',       T#ampTable.curAmp},
	 {'LD2_current',       T#ampTable.curAmp2},
	 {input_power,         0.0},
	 {output_power,        T#ampTable.outputPowerConsign},
	 {internal_temp,       T#ampTable.curInternalTemp},
	 {'CC1_setpoint',      T#ampTable.ampConsign},
	 {'CC2_setpoint',      T#ampTable.ampConsign2},
	 {'PC_setpoint',       T#ampTable.outputPowerConsign},
	 {'GC_setpoint',       T#ampTable.gainConsign},
	 {'has_PC_mode',       maps:get('has_PC_mode', Params, true)},
	 {'has_GC_mode',       maps:get('has_GC_mode', Params, true)},
	 {'has_input_PD',      maps:get('has_input_PD', Params, true)},
	 {'has_output_PD',     maps:get('has_output_PD', Params, true)}
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
	lists:foldl(fun (_, {error, _}=Err) ->
						Err;
					({mode, ?ampOperatingMode_off}, _Acc) ->
						bkfw_srv:command(Idx, smode, [<<"OFF">>]);
					({mode, ?ampOperatingMode_cc}, _Acc) ->
						bkfw_srv:command(Idx, smode, [<<"CC">>]);
					({mode, ?ampOperatingMode_gc}, _Acc) ->
						bkfw_srv:command(Idx, smode, [<<"GC">>]);
					({mode, ?ampOperatingMode_pc}, _Acc) ->
						bkfw_srv:command(Idx, smode, [<<"PC">>]);
					({'CC1_setpoint', V}, _Acc) ->
						bkfw_srv:command(Idx, scc, [io_lib:format("~b ~.2f", [1, V])]);
					({'CC2_setpoint', V}, _Acc) ->
						bkfw_srv:command(Idx, scc, [io_lib:format("~b ~.2f", [2, V])]);
					({'GC_setpoint', V}, _Acc) ->
						bkfw_srv:command(Idx, sgc, [io_lib:format("~.2f", [V])]);
					({'PC_setpoint', V}, _Acc) ->
						bkfw_srv:command(Idx, spc, [io_lib:format("~.2f", [V])]);
					(_, _Acc) ->
						{error, invalid_key}
				end, ok, Kv).


%%% SNMP functions
table_func(new, NameDb) ->
    snmp_generic:table_func(new, NameDb);
table_func(delete, NameDb) ->
    snmp_generic:table_func(delete, NameDb).


table_func(is_set_ok, RowIndex, Cols, NameDb) ->
    snmp_generic:table_func(is_set_ok, RowIndex, Cols, NameDb);

table_func(set, [RowIndex], Cols, NameDb) ->
    case set_from_snmp(RowIndex, Cols) of
		ok ->
			snmp_generic:table_func(set, [RowIndex], Cols, NameDb);
		{error, Col} ->
			{error, Col}
    end;

table_func(get, RowIndex, Cols, NameDb) ->
    Vars = snmp_generic:table_func(get, RowIndex, Cols, NameDb),
    lists:map(fun ({value, V}) when is_float(V) ->
					  {value, round(V)};
				  ({value, V}) when is_binary(V) ->
					  {value, binary_to_list(V)};
				  ({value, V}) ->
					  {value, V}
			  end, Vars);

table_func(get_next, RowIndex, Cols, NameDb) ->
    case snmp_generic:table_func(get_next, RowIndex, Cols, NameDb) of
		[] -> [];
		Next when is_list(Next) ->
			lists:map(fun ({NextOID, NextValue}) when is_float(NextValue) ->
							  {NextOID, round(NextValue)};
						  ({NextOID, NextValue}) when is_binary(NextValue) ->
							  {NextOID, binary_to_list(NextValue)};
						  (Else) ->
							  Else
					  end, Next);
		{genErr, Cols} -> 
			{genErr, Cols}
    end;

table_func(Op, RowIndex, Cols, NameDb) ->
    snmp_generic:table_func(Op, RowIndex, Cols, NameDb).

%%%
%%% Internals
%%%
read_cc(#ampTable{index=Idx, params=Params}=E) ->
    F = fun(X, {ok, Acc}) ->
				case bkfw_srv:command(Idx, rcc, [integer_to_binary(X)]) of
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
    lists:foldl(F, {ok, E}, maps:get(lasers, Params, [])).

read_gc(#ampTable{index=Idx}=E) ->
    case bkfw_srv:command(Idx, rgc, []) of
		{ok, {Idx, gc, [Y, <<"dB">>]}} when is_float(Y); is_integer(Y) ->
			{ok, E#ampTable{gainConsign=Y}};
		{ok, _Ret} ->
			{error, {string, io_lib:format("RGC invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.

read_pc(#ampTable{index=Idx}=E) ->
    case bkfw_srv:command(Idx, rpc, []) of
		{ok, {Idx, pc, [Y, <<"dBm">>]}} when is_float(Y); is_integer(Y) ->
			{ok, E#ampTable{outputPowerConsign=Y}};
		{ok, _Ret} ->
			{error, {string, io_lib:format("RPC invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.

read_mode(#ampTable{index=Idx}=E) ->
    case bkfw_srv:command(Idx, rmode, []) of
		{ok, {Idx, mode, [Mode]}} ->
			M = parse_mode(Mode, E#ampTable.operatingMode),
			{ok, E#ampTable{operatingMode=M}};
		{ok, _Ret} ->
			{error, {string, io_lib:format("RMODE invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.

read_a(#ampTable{index=Idx}=E) ->
    case bkfw_srv:command(Idx, ra, []) of
		{ok, {Idx, alarms, Alarms}} ->
			handle_alarms(Alarms, E);
		{ok, _Ret} ->
			{error, {string, io_lib:format("RA invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.

read_lt(#ampTable{ index=Idx, params=Params }=E) ->
    F = fun(X, {ok, Acc}) ->
				case bkfw_srv:command(Idx, rlt, [integer_to_binary(X)]) of
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
    lists:foldl(F, {ok, E}, maps:get(lasers, Params, [])).

read_lc(#ampTable{ index=Idx, params=Params }=E) ->
    F = fun(X, {ok, Acc}) ->
				case bkfw_srv:command(Idx, rlc, [integer_to_binary(X)]) of
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
    lists:foldl(F, {ok, E}, maps:get(lasers, Params, [])).

read_it(#ampTable{index=Idx}=E) ->
    case bkfw_srv:command(Idx, rit, []) of
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
    case bkfw_srv:command(Idx, rpm, []) of
		{ok, {Idx, pd, Lines}} ->
			E1 = parse_pd(Lines, E),
			{ok, E1};
		{ok, _Ret} ->
			{error, {string, io_lib:format("RPM invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.

read_v(#ampTable{index=Idx}=E) ->
    case bkfw_srv:command(Idx, rv, []) of
		{ok, {Idx, v, [V, v]}} when is_float(V); is_integer(V) ->
			{ok, E#ampTable{powerSupply=V}};
		{ok, _Ret} ->
			{error, {string, io_lib:format("RV invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.

read_li(#ampTable{index=Idx}=E) ->
    case bkfw_srv:command(Idx, rli, []) of
		{ok, {Idx, li, [Y, <<"dBm">>]}} ->
			{ok, E#ampTable{inputLossThreshold=Y}};
		{ok, _Ret} ->
			{error, {string, io_lib:format("RLI invalid answer: ~p~n", [_Ret])}};
		{error, Err} ->
			{error, Err}
    end.

read_lo(#ampTable{index=Idx}=E) ->
    case bkfw_srv:command(Idx, rlo, []) of
		{ok, {Idx, lo, [Y, <<"dBm">>]}} ->
			{ok, E#ampTable{outputLossThreshold=Y}};
		{ok, _Ret} ->
			{error, io_lib:format("RLO invalid answer: ~p~n", [_Ret])};
		{error, Err} ->
			{error, Err}
    end.

read_limits(#ampTable{ index=Idx, params=Params }=E) ->
    F = fun(X, {ok, Acc}) ->
				case bkfw_srv:command(Idx, rlcc, [integer_to_binary(X)]) of
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
    case lists:foldl(F, {ok, E}, maps:get(lasers, Params, [])) of
		{ok, E1} ->
			read_limits_pc(E1);
		{error, Err} ->
			{error, Err}
	end.

read_limits_pc(#ampTable{ index=Idx }=E) ->
    case bkfw_srv:command(Idx, rlpc, []) of
		{ok, {Idx, pc, [ [min, Min, _], [max, Max, _] ]}} ->
			E1 = E#ampTable{ pcMin=Min, pcMax=Max },
			read_limits_gc(E1);
		{ok, _Ret} ->
			{error, iolist_to_binary(io_lib:format("RLPC invalid answer: ~p~n", [_Ret]))};
		{error, Err} ->
			{error, Err}
    end.


read_limits_gc(#ampTable{ index=Idx }=E) ->
    case bkfw_srv:command(Idx, rlgc, []) of
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

parse_pd([ [Idx, P, <<"dBm">>] | Tail], #ampTable{ params=Params }=E) -> 
	Lasers0 = maps:get(lasers, Params, []),
	Lasers = case lists:member(Idx, Lasers0) of
				 true ->
					 Lasers0;
				 false ->
					 lists:merge([Idx], Lasers0)
			 end,
	parse_pd2(Idx, P, Tail, E#ampTable{ params=Params#{ lasers := Lasers } }).


parse_pd2(1, P, Tail, E) ->
	parse_pd(Tail, E#ampTable{ powerPd1=P });

parse_pd2(2, P, Tail, E) ->
	parse_pd(Tail, E#ampTable{ powerPd2=P });

parse_pd2(3, P, Tail, E) ->
	parse_pd(Tail, E#ampTable{ powerPd3=P });

parse_pd2(_, _, Tail, E) ->
	parse_pd(Tail, E).


handle_alarms([], E) -> {ok, E};
handle_alarms([Name  | Tail], E) -> 
    gen_event:notify(bkfw_alarms, #smmAlarm{index=E#ampTable.index,
											name=Name,
											obj=E}),
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
    bkfw_srv:command(Idx, scc, [<<"1 ">>, io_lib:format("~b.0", [Val])]),
    set_from_snmp(Idx, Tail);
set_from_snmp(Idx, [{?ampGainConsign, Val} | Tail]) when is_integer(Val) ->
    bkfw_srv:command(Idx, sgc, [io_lib:format("~b.0", [Val])]),
    set_from_snmp(Idx, Tail);
set_from_snmp(Idx, [{?ampOutputPowerConsign, Val} | Tail]) when is_integer(Val) ->
    bkfw_srv:command(Idx, spc, [io_lib:format("~b.0", [Val])]),
    set_from_snmp(Idx, Tail);
set_from_snmp(Idx, [{?ampOperatingMode, Val} | Tail]) ->
    case Val of
		?ampOperatingMode_off -> 
			bkfw_srv:command(Idx, smode, [<<"OFF">>]),
			set_from_snmp(Idx, Tail);
		?ampOperatingMode_cc -> 
			bkfw_srv:command(Idx, smode, [<<"CC">>]),
			set_from_snmp(Idx, Tail);
		?ampOperatingMode_gc -> 
			bkfw_srv:command(Idx, smode, [<<"GC">>]),
			set_from_snmp(Idx, Tail);
		?ampOperatingMode_pc -> 
			bkfw_srv:command(Idx, smode, [<<"PC">>]),
			set_from_snmp(Idx, Tail);
		_ -> 
			{error, ?ampOperatingMode}
    end;
set_from_snmp(_, [{Col, _} | _]) ->
    {error, Col}.


set_operating_mode(Idx, ?ampOperatingMode_off, _Amp) ->
    bkfw_srv:command(Idx, smode, [<<"OFF">>]),
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
	case bkfw_srv:command(Idx, scc, [io_lib:format("~b ~.2f", [Laser, V])]) of
		{ok, {Idx, scc, [Laser, ofr]}} ->
			{error, ofr};
		_ ->
			bkfw_srv:command(Idx, smode, [<<"CC">>]),
			ok
	end;

set_operating_mode(_Idx, {?ampOperatingMode_gc, V}, #ampTable{ gcMin=Min, gcMax=Max }) 
  when V > Max orelse V < Min ->
	{error, ofr};

set_operating_mode(Idx, {?ampOperatingMode_gc, V}, _Amp) ->
	case bkfw_srv:command(Idx, sgc, [io_lib:format("~.2f", [V])]) of
		{ok, {Idx, sgc, [ofr]}} ->
			{error, ofr};
		_ ->
			bkfw_srv:command(Idx, smode, [<<"GC">>]),
			ok
	end;

set_operating_mode(_Idx, {?ampOperatingMode_pc, V}, #ampTable{ pcMin=Min, pcMax=Max }) 
  when V > Max orelse V < Min ->
	{error, ofr};

set_operating_mode(Idx, {?ampOperatingMode_pc, V}, _Amp) ->
	case bkfw_srv:command(Idx, spc, [io_lib:format("~.2f", [V])]) of
		{ok, {Idx, spc, [ofr]}} ->
			{error, ofr};
		_ ->
			bkfw_srv:command(Idx, smode, [<<"PC">>]),
			ok
	end;

set_operating_mode(_, {_, undefined}, _Amp) ->
	{error, missing_consign};

set_operating_mode(_, _, _Amp) ->
    {error, internal}.


set_thresholds(Idx, Kv) ->
    case get_kv_float(inputLossThreshold, Kv) of
		undefined ->
			{error, invalid_thresholds};
		IT ->
			case bkfw_srv:command(Idx, sli, [io_lib:format("~.2f", [IT])]) of
				{ok, {Idx, sli, _}} ->
					case get_kv_float(outputLossThreshold, Kv) of
						undefined ->
							{error, invalid_thresholds};
						OT ->
							case bkfw_srv:command(Idx, slo, [io_lib:format("~.2f", [OT])]) of
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
