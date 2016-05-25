-module(bkfw_mcu).
-author('jean.parpaillon@free.fr').

-include("bkfw.hrl").

-export([loop/1,
		 get_kv/2,
		 set_kv/2]).

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
-define(FUNS, [read_pm, read_cc, read_gc, read_pc, read_mode, read_lt, read_lc, read_it,
			   read_i, read_v, read_li, read_lo, read_a, read_limits]).

-record(laser, {index,
				amp           = 0.0,
				amp_consign   = 0.0, 
				power         = 0.0,
				cc_limit      = 0.0,
				temp          = 0.0,
				settable      = false}).
-define(default_laser(I), #laser{index=I}).

loop(#ampTable{}=Amp) ->
    loop(Amp, ?FUNS).

loop(Amp, []) ->
    {ok, Amp};
loop(Amp, [ Fun | Tail ]) ->
    case apply(?MODULE, Fun, [Amp]) of
		{ok, Amp2} ->
			loop(Amp2, Tail);
		{error, timeout} -> {error, timeout};
		{error, Err} -> {error, Err, Amp}
    end.

get_kv(#ampTable{}=T, 1) ->
    [
     {index,               T#ampTable.index},
     {ampConsign,          T#ampTable.ampConsign},
     {gainConsign,         T#ampTable.gainConsign},
     {outputPowerConsign,  T#ampTable.outputPowerConsign},
     {operatingMode,       T#ampTable.operatingMode},
     {curLaserTemp,        T#ampTable.curLaserTemp},
     {curAmp,              T#ampTable.curAmp},
     {curInternalAmp,      T#ampTable.curInternalTemp},
     {powerInput,          T#ampTable.powerPd1},
     {powerOutput,         T#ampTable.powerPd2},
     {powerSupply,         T#ampTable.powerSupply},
     {inputLossThreshold,  T#ampTable.inputLossThreshold},
     {outputLossThreshold, T#ampTable.outputLossThreshold},
     {vendor,              T#ampTable.vendor},
     {moduleType,          T#ampTable.moduleType},
     {hwVer,               T#ampTable.hwVer},
     {hwRev,               T#ampTable.hwRev},
     {swVer,               T#ampTable.swVer},
     {fwVer,               T#ampTable.fwVer},
     {partNum,             T#ampTable.partNum},
     {serialNum,           T#ampTable.serialNum},
     {productDate,         T#ampTable.productDate}
    ];

get_kv(#ampTable{ lasers=Lasers, pc_limit={MinPC, MaxPC}, gc_limit={MinGC, MaxGC} }=T, 2) ->
	Laser1 = case lists:keyfind(1, 2, Lasers) of
				 false -> #laser{index=1};
				 L1 -> L1
			 end,
	Laser2 = case lists:keyfind(2, 2, Lasers) of
				 false -> #laser{index=2};
				 L2 -> L2
			 end,						  
    [
     {index,               T#ampTable.index},
	 {mode,                T#ampTable.operatingMode},
	 {max_current_LD1,     Laser1#laser.cc_limit},
	 {max_current_LD2,     Laser2#laser.cc_limit},
	 {min_pc,              MinPC},
	 {max_pc,              MaxPC},
	 {min_gc,              MinGC},
	 {max_gc,              MaxGC},
	 {number_of_laser,     length(Lasers)},
	 {has_settable_LD1,    Laser1#laser.settable},
	 {alarms,              []},
	 {'LD1_current',       Laser1#laser.amp},
	 {'LD2_current',       Laser1#laser.amp},
	 {input_power,         0.0},
	 {output_power,        T#ampTable.outputPowerConsign},
	 {internal_temp,       T#ampTable.curInternalTemp},
	 {'CC1_setpoint',      Laser1#laser.amp_consign},
	 {'CC2_setpoint',      Laser2#laser.amp_consign},
	 {'PC_setpoint',       T#ampTable.outputPowerConsign},
	 {'GC_setpoint',       T#ampTable.gainConsign}
    ].


set_kv(Idx, Kv) ->
    case get_kv_integer(operatingMode, Kv) of
		undefined ->
			set_thresholds(Idx, Kv);
		Mode ->
			set_operating_mode(Idx, Mode, Kv)
    end.

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
read_cc(#ampTable{index=Idx, lasers=Lasers}=E) ->
    F = fun(#laser{index=X}=L, {ok, Acc}) ->
				case bkfw_srv:command(Idx, rcc, [integer_to_binary(X)]) of
					{ok, {1, cc, [X, A, <<"mA">>]}} when is_float(A); is_integer(A) ->
						{ok, Acc#ampTable{ ampConsign=A, lasers=lists:keystore(1, 2, Lasers, L#laser{ amp_consign=A }) }};
					{ok, {Idx, cc, [X, A, <<"mA">>]}} when is_float(A); is_integer(A) ->
						{ok, Acc#ampTable{ lasers=lists:keystore(Idx, 2, Lasers, L#laser{ amp_consign=A }) }};
					{ok, _Ret} ->
						{error, {string, io_lib:format("RCC invalid answer: ~p~n", [_Ret])}};
					{error, Err} ->
						{error, Err}
				end;
		   (_, {error, Err}) ->
				{error, Err}
		end,
    lists:foldl(F, {ok, E}, Lasers).

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

read_lt(#ampTable{ index=Idx, lasers=Lasers }=E) ->
    F = fun(#laser{ index=X }=L, {ok, Acc}) ->
				case bkfw_srv:command(Idx, rlt, [integer_to_binary(X)]) of
					{ok, {1, lt, [X, T, <<"C">>]}} when is_float(T); is_integer(T) ->
						{ok, Acc#ampTable{ curLaserTemp=T, lasers=lists:keystore(1, 2, Lasers, L#laser{ temp=T }) } };
					{ok, {Idx, lt, [X, T, <<"C">>]}} when is_float(T); is_integer(T) ->
						{ok, Acc#ampTable{ curLaserTemp=T, lasers=lists:keystore(Idx, 2, Lasers, L#laser{ temp=T }) } };
					{ok, _Ret} ->
						{error, {string, io_lib:format("RLT invalid answer: ~p~n", [_Ret])}};
					{error, Err} ->
						{error, Err}
				end;
		   (_, {error, Err}) ->
				{error, Err}
		end,
    lists:foldl(F, {ok, E}, Lasers).

read_lc(#ampTable{ index=Idx, lasers=Lasers }=E) ->
    F = fun(#laser{ index=X }=L, {ok, Acc}) ->
				case bkfw_srv:command(Idx, rlc, [integer_to_binary(X)]) of
					{ok, {1, lc, [X, A, <<"mA">>]}} when is_float(A); is_integer(A) ->
						{ok, Acc#ampTable{ curAmp=A, lasers=lists:keystore(1, 2, Lasers, L#laser{ amp=A }) } };
					{ok, {Idx, lc, [X, A, <<"mA">>]}} when is_float(A); is_integer(A) ->
						{ok, Acc#ampTable{ curAmp=A, lasers=lists:keystore(Idx, 2, Lasers, L#laser{ amp=A }) } };
					{ok, _Ret} ->
						{error, {string, io_lib:format("RLC invalid answer: ~p~n", [_Ret])}};
					{error, Err} ->
						{error, Err}
				end
		end,
    lists:foldl(F, {ok, E}, Lasers).

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

read_limits(#ampTable{index=Idx, lasers=Lasers}=E) ->
    F = fun(#laser{ index=X }=L, {ok, Acc}) ->
				case bkfw_srv:command(Idx, rlcc, [integer_to_binary(X)]) of
					{ok, {Idx, max, [_, _, A, <<"mA">>]}} when is_float(A); is_integer(A) ->
						{ok, Acc#ampTable{ lasers=lists:keystore(Idx, 2, Lasers, L#laser{ cc_limit=A }) } };
					{ok, _Ret} ->
						{error, {string, io_lib:format("RLCC ~b invalid answer: ~p~n", [_Ret, X])}};
					{error, Err} ->
						{error, Err}
				end
		end,
    case lists:foldl(F, {ok, E}, Lasers) of
		{ok, E1} ->
			read_limits_pc(E1);
		{error, Err} ->
			{error, Err}
	end.

read_limits_pc(#ampTable{ index=Idx }=E) ->
    case bkfw_srv:command(Idx, rlpc, []) of
		{ok, {Idx, pc, [ [min, Min, _], [max, Max, _] ]}} ->
			E1 = E#ampTable{ pc_limit={Min, Max} },
			read_limits_gc(E1);
		{ok, _Ret} ->
			{error, iolist_to_binary(io_lib:format("RLPC invalid answer: ~p~n", [_Ret]))};
		{error, Err} ->
			{error, Err}
    end.


read_limits_gc(#ampTable{ index=Idx }=E) ->
    case bkfw_srv:command(Idx, rlgc, []) of
		{ok, {Idx, gc, [ [min, Min, _], [max, Max, _] ]}} ->
			E1 = E#ampTable{ gc_limit={Min, Max} },
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

parse_pd([ [Idx, P, <<"dBm">>] | Tail], #ampTable{ lasers=Lasers }=E) -> 
	Laser = case lists:keyfind(Idx, 2, Lasers) of
				false -> #laser{ index=Idx, power=P };
				L -> L#laser{ power=P }
			end,
	parse_pd2(Idx, P, Tail, E#ampTable{ lasers=lists:keystore(Idx, 2, Lasers, Laser) }).


parse_pd2(1, P, Tail, E) ->
	parse_pd(Tail, E#ampTable{ powerPd1=P });

parse_pd2(2, P, Tail, E) ->
	parse_pd(Tail, E#ampTable{ powerPd2=P });

parse_pd2(3, P, Tail, E) ->
	parse_pd(Tail, E#ampTable{ powerPd3=P });

parse_pd2(_, _, Tail, E) ->
	parse_pd(Tail, E).


%% get_info(Key, Infos, Default) ->
%%     try proplists:get_value(Key, Infos, Default) of
%% 		Str -> Str
%%     catch error:badarg ->
%% 			Default
%%     end.


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


set_operating_mode(Idx, ?ampOperatingMode_off, _) ->
    bkfw_srv:command(Idx, smode, [<<"OFF">>]),
    ok;
set_operating_mode(Idx, ?ampOperatingMode_cc, Kv) ->
    case get_consign(ampConsign, Kv) of
		undefined -> {error, missing_consign};
		V -> 
			case bkfw_srv:command(Idx, scc, [<<"1 ">>, io_lib:format("~.2f", [V])]) of
				{ok, {Idx, scc, [1, ofr]}} ->
					{error, ofr};
				_ ->
					bkfw_srv:command(Idx, smode, [<<"CC">>]),
					ok
			end
    end;
set_operating_mode(Idx, ?ampOperatingMode_gc, Kv) ->
    case get_consign(gainConsign, Kv) of
		undefined -> {error, missing_consign};
		V -> 
			case bkfw_srv:command(Idx, sgc, [io_lib:format("~.2f", [V])]) of
				{ok, {Idx, sgc, [ofr]}} ->
					{error, ofr};
				_ ->
					bkfw_srv:command(Idx, smode, [<<"GC">>]),
					ok
			end
    end;
set_operating_mode(Idx, ?ampOperatingMode_pc, Kv) ->
    case get_consign(outputPowerConsign, Kv) of
		undefined -> {error, mising_consign};
		V ->
			case bkfw_srv:command(Idx, spc, [io_lib:format("~.2f", [V])]) of
				{ok, {Idx, spc, [ofr]}} ->
					{error, ofr};
				_ ->
					bkfw_srv:command(Idx, smode, [<<"PC">>]),
					ok
			end
    end;
set_operating_mode(_, _, _) ->
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
