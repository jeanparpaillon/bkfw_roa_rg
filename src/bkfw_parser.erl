-module(bkfw_parser).
-author('jean.parpaillon@lizenn.com').

-include("bkfw.hrl").

-export([is_raw/2,
		 parse/1,
		 parse/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

parse(Bin) ->
    parse(Bin, undefined).


is_raw(<< "0x", Rest/binary >>, undefined) ->
	case bkfw_scanner:token(<< "0x", Rest/binary >>) of
		{ok, N, _} when is_integer(N) ->
			false;
		_ ->
			true
	end;

is_raw(_Bin, undefined) ->
	true;

is_raw(_Bin, {_, _, _}) ->
	false.
	
		
-spec parse(Data :: binary(), undefined | msg()) -> {ok, msg(), binary()} 
														| {more, msg(), binary()}
														| {error, term(), binary()}.
parse(Bin, Acc) ->
    parse_msg(bkfw_scanner:token(Bin), Acc).

%%%
%%% Priv
%%%

%% multi-line commands ends with an empty line
parse_msg({eof, Rest}, undefined) ->
    {more, undefined, Rest};

parse_msg({eof, Rest}, {A, i, Lines}) ->
    {ok, {A, i, lists:reverse(Lines)}, Rest};

parse_msg({eof, Rest}, {A, pd, Lines}) ->
    {ok, {A, pd, lists:reverse(Lines)}, Rest};

parse_msg({eof, Rest}, {A, pc, Lines}) ->
    {ok, {A, pc, lists:reverse(Lines)}, Rest};

parse_msg({eof, Rest}, {A, gc, Lines}) ->
    {ok, {A, gc, lists:reverse(Lines)}, Rest};

parse_msg({eof, Rest}, Msg) when is_list(Msg) ->
	{ok, lists:reverse(Msg), Rest};

parse_msg({ok, _, _}=Tok, {_, i, _}=Msg) ->
    parse_kv(Tok, Msg);

parse_msg({ok, _, _}=Tok, {_, pd, _}=Msg) ->
    parse_cmd(Tok, Msg);

parse_msg({ok, _, _}=Tok, {_, pc, _}=Msg) ->
    parse_cmd(Tok, Msg);

parse_msg({ok, _, _}=Tok, {_, gc, _}=Msg) ->
    parse_cmd(Tok, Msg);

parse_msg({ok, N, Rest}, undefined) when is_integer(N) ->
    parse_cmd(bkfw_scanner:token(Rest), {N, undefined, []});

parse_msg({ok, _, _}=Tok, {_, undefined, _}=Msg) ->
    parse_cmd(Tok, Msg);

parse_msg({ok, V, Rest}, undefined) ->
	parse_msg(bkfw_scanner:token(Rest), [V]);

parse_msg({ok, L, Rest}, [_Adr, write, flash]=Msg) when is_integer(L) ->
	parse_msg(bkfw_scanner:buf(L, Rest), [ L | Msg]);

parse_msg({ok, V, Rest}, Msg) when is_list(Msg) ->
	parse_msg(bkfw_scanner:token(Rest), [V | Msg ]);

parse_msg({more, Rest}, Msg) ->
    {more, Msg, Rest};

parse_msg({error, Err, Bin}, _) ->
    {error, Err, Bin}.


parse_cmd({error, Err, Rest}, _) ->
    {error, Err, Rest};

parse_cmd({eof, Rest}, {Addr, pd, Lines}) ->
    {ok, {Addr, pd, Lines}, Rest};

parse_cmd({eof, Rest}, {Addr, i, Lines}) ->
    {ok, {Addr, i, Lines}, Rest};

parse_cmd({eof, Rest}, _) ->
    {error, eof, Rest};

parse_cmd({ok, <<  "PD", BinI/bits >>, Rest}, {Addr, _, Lines}) ->
    try binary_to_integer(BinI) of
		I ->
			Tok = bkfw_scanner:token(Rest),
			parse_args(Tok, {Addr, pd, Lines}, [I])
    catch error:badarg ->
			{error, io_lib:format("Expect integer, got: ~p", [BinI]), Rest}
    end;

parse_cmd({ok, Cmd, Rest}, {A, _, Lines}) when is_atom(Cmd) ->
    parse_args(bkfw_scanner:token(Rest), {A, Cmd, Lines}, []);

parse_cmd({ok, Tok, Rest}, _) ->
    {error, io_lib:format("Expect atom, got: ~p", [Tok]), Rest};

parse_cmd({more, Rest}, Msg) ->
    {more, Msg, Rest}.


parse_args({error, Err, Rest}, {_, _, _}, _) ->
    {error, Err, Rest};

parse_args({eof, Rest}, {A, C, Lines}, []) ->
    is_multiline({A, C, Lines}, Rest);

parse_args({eof, Rest}, {A, C, Lines}, Args) ->
    is_multiline({A, C, [ lists:reverse(Args) | Lines ]}, Rest);

parse_args({ok, Arg, Rest}, {A, C, L}, Args) ->
    parse_args(bkfw_scanner:token(Rest), {A, C, L}, [ Arg | Args ]);

parse_args({more, Rest}, {A, C, Lines}, []) ->
    {more, {A, C, Lines}, Rest};

parse_args({more, Rest}, {A, C, Lines}, Args) ->
    {more, {A, C, [ lists:reverse(Args) | Lines ]}, Rest}.


parse_kv({error, Err, Rest}, _) ->
    {error, Err, Rest};

parse_kv({eof, Rest}, Msg) ->
    {ok, Msg, Rest};

parse_kv({ok, Key, Rest}, Msg) ->
	Tok = bkfw_scanner:token(Rest),
    parse_value(Tok, Msg, Key);

parse_kv({more, Rest}, Msg) ->
    {more, Msg, Rest}.


parse_value({error, Err, Rest}, _, _) ->
    {error, Err, Rest};

parse_value({eof, Rest}, {A, C, Lines}, K) ->
    {more, {A, C, [{K, undefined} | Lines]}, Rest};

parse_value({ok, Value, Rest}, {A, C, [ {_, more} | Lines ]}, Key) ->
    {more, {A, C, [{Key, Value} | Lines]}, Rest};

parse_value({ok, Value, Rest}, {A, C, Lines}, Key) ->
    {more, {A, C, [{Key, Value} | Lines]}, Rest};

parse_value({more, Rest}, {A, C, [ {_, more} | Lines ]}, K) ->
    {more, {A, C, [ {K, more} | Lines]}, Rest};

parse_value({more, Rest}, {A, C, Lines}, K) ->
    {more, {A, C, [ {K, more} | Lines]}, Rest}.


is_multiline({_, i, _}=Msg, Rest) -> {more, Msg, Rest};

is_multiline({_, pd, _}=Msg, Rest) -> {more, Msg, Rest};

is_multiline({_, pc, [ [A | _ ] | _ ]}=Msg, Rest) when min =:= A orelse max =:= A -> {more, Msg, Rest};

is_multiline({_, gc, [ [A | _ ] | _ ]}=Msg, Rest) when min =:= A orelse max =:= A -> {more, Msg, Rest};

is_multiline({A, C, []}, Rest) -> {ok, {A, C, []}, Rest};

is_multiline({A, C, [Line]}, Rest) -> {ok, {A, C, Line}, Rest}.


-ifdef(TEST).
parse_cc_test() ->
	?assertMatch({ok, {0, cc, [2, 34.56, <<"mA">>]}, <<>>}, parse(<<"0x00 CC 2 34.56 mA\r\n">>)).

parse_scc_test() ->
	?assertMatch({ok, {0, scc, [2, ofr]}, <<>>}, parse(<<"0x00 SCC 2 OFR\r\n">>)),
	?assertMatch({ok, {0, scc, [2, 32.87]}, <<>>}, parse(<<"0x00 SCC 2 32.87\r\n">>)).
	
parse_gc_test() ->
	?assertMatch({ok, {0, gc, [2.0, <<"dB">>]}, <<>>}, parse(<<"0x00 GC 2.0 dB\r\n">>)).
	
parse_lcc_test() ->
	?assertMatch({ok, {0, max, [<<"Current">>, <<"LD1">>, 345.21, <<"mA">>]}, <<>>}, 
				 parse(<<"0x00 MAX Current LD1 345.21 mA\r\n">>)).

parse_pc_test() ->
	?assertMatch({ok, {4, rpc, []}, <<>>}, parse(<<"0x04 RPC\r\n">>)),
	?assertMatch({ok, {4, pc, [4.5, <<"dBm">>]}, <<>>}, parse(<<"0x04 PC 4.5 dBm\r\n">>)).


parse_lpc_test() ->
	?assertMatch({more, {0, pc, [ [min, 21.0, <<"dBm">>] ]}, <<"PC MAX 45.3 dBm\r\n\r\n">>}, 
				 parse(<<"0x00 PC MIN 21.0 dBm\r\nPC MAX 45.3 dBm\r\n\r\n">>)),
	?assertMatch({more, {0, pc, [ [max, 45.3, <<"dBm">>], [min, 21.0, <<"dBm">>] ]}, <<"\r\n">>}, 
				 parse(<<"PC MAX 45.3 dBm\r\n\r\n">>, {0, pc, [ [min, 21.0, <<"dBm">>] ]})),
	?assertMatch({ok, {0, pc, [ [min, 21.0, <<"dBm">>], [max, 45.3, <<"dBm">>] ]}, <<>>}, 
				 parse(<<"\r\n">>, {0, pc, [ [max, 45.3, <<"dBm">>], [min, 21.0, <<"dBm">>] ]})).


parse_lgc_test() ->
	?assertMatch({more, {0, gc, [ [min, 21.0, <<"dBm">>] ]}, <<"GC MAX 45.3 dBm\r\n\r\n">>}, 
				 parse(<<"0x00 GC MIN 21.0 dBm\r\nGC MAX 45.3 dBm\r\n\r\n">>)),
	?assertMatch({more, {0, gc, [ [max, 45.3, <<"dBm">>], [min, 21.0, <<"dBm">>] ]}, <<"\r\n">>}, 
				 parse(<<"GC MAX 45.3 dBm\r\n\r\n">>, {0, gc, [ [min, 21.0, <<"dBm">>] ]})),
	?assertMatch({ok, {0, gc, [ [min, 21.0, <<"dBm">>], [max, 45.3, <<"dBm">>] ]}, <<>>}, 
				 parse(<<"\r\n">>, {0, gc, [ [max, 45.3, <<"dBm">>], [min, 21.0, <<"dBm">>] ]})).


parse_n_test() ->
	?assertMatch({ok, {3, rn, []}, <<>>}, parse(<<"0x03 RN\r\n">>)),
	?assertMatch({ok, {3, n, [3]}, <<>>}, parse(<<"0x03 N 0x3\r\n">>)).


parse_rpm_test() ->
	?assertMatch({ok, {1, rpm, []}, <<>>}, parse(<<"0x01 RPM\r\n">>)),

	?assertMatch({more, {1, pd, [ [1, 1.85, <<"dBm">>] ]}, <<"PD2 4.83 dBm\r\n\r\n">>}, 
				 parse(<<"0x01 PD1 1.85 dBm\r\nPD2 4.83 dBm\r\n\r\n">>)),

	?assertMatch({more, {1, pd, [ [2, 4.83, <<"dBm">>], [1, 1.85, <<"dBm">>] ]}, <<"\r\n">>}, 
				 parse(<<"PD2 4.83 dBm\r\n\r\n">>, {1, pd, [ [1, 1.85, <<"dBm">>] ]})),

	?assertMatch({ok, {1, pd, [ [1, 1.85, <<"dBm">>], [2, 4.83, <<"dBm">>] ]}, <<>>}, 
				 parse(<<"\r\n">>, {1, pd, [ [2, 4.83, <<"dBm">>], [1, 1.85, <<"dBm">>] ]})).


parse_i_test() ->
	?assertMatch({more, {0, i, []}, <<"Vendor= toto\r\n"
									  "Module= toto\r\n"
									  "HW Ver= toto\r\n"
									  "HW Rev= toto\r\n"
									  "SW Ver= toto\r\n"
									  "FW Ver= toto\r\n"
									  "Part Num= toto\r\n"
									  "Ser. Num= toto\r\n"
									  "Prod. Date= toto\r\n\r\n">>}, 
				 parse(<<"0x00 I\r\n"
						 "Vendor= toto\r\n"
						 "Module= toto\r\n"
						 "HW Ver= toto\r\n"
						 "HW Rev= toto\r\n"
						 "SW Ver= toto\r\n"
						 "FW Ver= toto\r\n"
						 "Part Num= toto\r\n"
						 "Ser. Num= toto\r\n"
						 "Prod. Date= toto\r\n\r\n">>)),
	?assertMatch({more, {0, i, [ {vendor, <<"toto">>} ]}, <<"Module= toto\r\n"
															"HW Ver= toto\r\n"
															"HW Rev= toto\r\n"
															"SW Ver= toto\r\n"
															"FW Ver= toto\r\n"
															"Part Num= toto\r\n"
															"Ser. Num= toto\r\n"
															"Prod. Date= toto\r\n\r\n">>}, 
				 parse(<<"Vendor= toto\r\n"
						 "Module= toto\r\n"
						 "HW Ver= toto\r\n"
						 "HW Rev= toto\r\n"
						 "SW Ver= toto\r\n"
						 "FW Ver= toto\r\n"
						 "Part Num= toto\r\n"
						 "Ser. Num= toto\r\n"
						 "Prod. Date= toto\r\n\r\n">>, {0, i, []})),
	?assertMatch({more, {0, i, [ {moduleType, <<"toto">>}, 
								 {vendor, <<"toto">>} ]}, 
				  <<"HW Ver= toto\r\n"
					"HW Rev= toto\r\n"
					"SW Ver= toto\r\n"
					"FW Ver= toto\r\n"
					"Part Num= toto\r\n"
					"Ser. Num= toto\r\n"
					"Prod. Date= toto\r\n\r\n">>}, 
				 parse(<<"Module= toto\r\n"
						 "HW Ver= toto\r\n"
						 "HW Rev= toto\r\n"
						 "SW Ver= toto\r\n"
						 "FW Ver= toto\r\n"
						 "Part Num= toto\r\n"
						 "Ser. Num= toto\r\n"
						 "Prod. Date= toto\r\n\r\n">>, {0, i, [ {vendor, <<"toto">>} ]})),
	?assertMatch({more, {0, i, [ {productDate, <<"toto">>},
								 {serialNum, <<"toto">>},
								 {partNum, <<"toto">>},
								 {fwVer, <<"toto">>},
								 {swVer, <<"toto">>},
								 {hwRev, <<"toto">>},
								 {hwVer, <<"toto">>},
								 {module, <<"toto">>},
								 {vendor, <<"toto">>}							   
							   ]}, <<"\r\n">>}, 
				 parse(<<"Prod. Date= toto\r\n\r\n">>, 
					   {0, i, [ {serialNum, <<"toto">>},
								{partNum, <<"toto">>},
								{fwVer, <<"toto">>},
								{swVer, <<"toto">>},
								{hwRev, <<"toto">>},
								{hwVer, <<"toto">>},
								{module, <<"toto">>},
								{vendor, <<"toto">>}
							  ]})),
	?assertMatch({ok, {0, i, [ {vendor, <<"toto">>},
							   {module, <<"toto">>},
							   {hwVer, <<"toto">>},
							   {hwRev, <<"toto">>},
							   {swVer, <<"toto">>},
							   {fwVer, <<"toto">>},
							   {partNum, <<"toto">>},
							   {serialNum, <<"toto">>},
							   {productDate, <<"toto">>}
							 ]}, <<>>}, 
				 parse(<<"\r\n">>, 
					   {0, i, [ {productDate, <<"toto">>},
								{serialNum, <<"toto">>},
								{partNum, <<"toto">>},
								{fwVer, <<"toto">>},
								{swVer, <<"toto">>},
								{hwRev, <<"toto">>},
								{hwVer, <<"toto">>},
								{module, <<"toto">>},
								{vendor, <<"toto">>}
							  ]})).


parse_raw_test() ->
	?assertMatch(true, is_raw(<<"A BINARY\r\n">>, undefined)),
	?assertMatch(true, is_raw(<<"0x A BINARY\r\n">>, undefined)),

	?assertMatch(false, is_raw(<<"0x00 A BINARY\r\n">>, undefined)),

	?assertMatch(false, is_raw(<<"A BINARY\r\n">>, {1, un, []})),
	?assertMatch(false, is_raw(<<"0x A BINARY\r\n">>, {1, un, []})),
	?assertMatch(false, is_raw(<<"0x00 A BINARY\r\n">>, {1, un, []})).

-endif.
