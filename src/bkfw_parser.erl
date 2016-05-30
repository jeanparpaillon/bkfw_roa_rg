-module(bkfw_parser).
-author('jean.parpaillon@lizenn.com').

-include("bkfw.hrl").

-export([parse/1,
		 parse/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

parse(Bin) ->
    parse(Bin, undefined).

-spec parse(Data :: binary(), undefined | msg()) -> {raw, binary()} 
														| {ok, msg()} 
														| {more, msg(), binary()}
														| {error, term()}.
parse(<< "0x", Rest/binary >>, undefined) ->
	parse_msg(bkfw_scanner:token(<< "0x", Rest/binary >>), undefined);

parse(Bin, undefined) ->
	{raw, Bin};
	
parse(Bin, Acc) ->
    parse_msg(bkfw_scanner:token(Bin), Acc).

%%%
%%% Priv
%%%
parse_msg({eof, Rest}, undefined) ->
    {more, undefined, Rest};

parse_msg({eof, Rest}, {A, i, Lines}) ->
    {ok, {A, i, lists:reverse(Lines)}, Rest};

parse_msg({eof, Rest}, {_, pd, _}=Msg) ->
    {ok, Msg, Rest};

parse_msg({eof, Rest}, {_, pc, _}=Msg) ->
    {ok, Msg, Rest};

parse_msg({eof, Rest}, {_, gc, _}=Msg) ->
    {ok, Msg, Rest};

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
			parse_args(bkfw_scanner:token(Rest), {Addr, pd, Lines}, [I])
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
    is_multiline({A, C, Lines ++ [lists:reverse(Args)]}, Rest);

parse_args({ok, Arg, Rest}, {A, C, L}, Args) ->
    parse_args(bkfw_scanner:token(Rest), {A, C, L}, [ Arg | Args ]);

parse_args({more, Rest}, {A, C, Lines}, []) ->
    {more, {A, C, Lines}, Rest};

parse_args({more, Rest}, {A, C, Lines}, Args) ->
    {more, {A, C, Lines ++ [lists:reverse(Args)]}, Rest}.


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

parse_value({ok, Value, Rest}, {A, C, [ {_, more} | Lines ]}, productDate) ->
    {ok, {A, C, lists:reverse([{productDate, Value} | Lines])}, Rest};

parse_value({ok, Value, Rest}, {A, C, [ {_, more} | Lines ]}, Key) ->
    {more, {A, C, [{Key, Value} | Lines]}, Rest};

parse_value({ok, Value, Rest}, {A, C, Lines}, productDate) ->
    {ok, {A, C, lists:reverse([{productDate, Value} | Lines])}, Rest};

parse_value({ok, Value, Rest}, {A, C, Lines}, Key) ->
    {more, {A, C, [{Key, Value} | Lines]}, Rest};

parse_value({more, Rest}, {A, C, [ {_, more} | Lines ]}, K) ->
    {more, {A, C, [ {K, more} | Lines]}, Rest};

parse_value({more, Rest}, {A, C, Lines}, K) ->
    {more, {A, C, [ {K, more} | Lines]}, Rest}.


is_multiline({_, i, _}=Msg, Rest) -> {more, Msg, Rest};

is_multiline({_, pd, _}=Msg, Rest) -> {more, Msg, Rest};

is_multiline({_, pc, [ [min | _ ] ]}=Msg, Rest) -> {more, Msg, Rest};

is_multiline({_, pc, [ [min | _ ], [max | _ ] ]}=Msg, Rest) -> {ok, Msg, Rest};

is_multiline({_, gc, [ [min | _ ] ]}=Msg, Rest) -> {more, Msg, Rest};

is_multiline({_, gc, [ [min | _ ], [max | _ ] ]}=Msg, Rest) -> {ok, Msg, Rest};

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

parse_lpc_test() ->
	?assertMatch({more, {0, pc, [ [min, 21.0, <<"dBm">>] ]}, <<"PC MAX 45.3 dBm\r\n">>}, 
				 parse(<<"0x00 PC MIN 21.0 dBm\r\nPC MAX 45.3 dBm\r\n">>)),
	?assertMatch({ok, {0, pc, [ [min, 21.0, <<"dBm">>], [max, 45.3, <<"dBm">>] ]}, <<>>}, 
				 parse(<<"PC MAX 45.3 dBm\r\n">>, {0, pc, [ [min, 21.0, <<"dBm">>] ]})).

parse_lgc_test() ->
	?assertMatch({more, {0, gc, [ [min, 21.0, <<"dBm">>] ]}, <<"GC MAX 45.3 dBm\r\n">>}, 
				 parse(<<"0x00 GC MIN 21.0 dBm\r\nGC MAX 45.3 dBm\r\n">>)),
	?assertMatch({ok, {0, gc, [ [min, 21.0, <<"dBm">>], [max, 45.3, <<"dBm">>] ]}, <<>>}, 
				 parse(<<"GC MAX 45.3 dBm\r\n">>, {0, gc, [ [min, 21.0, <<"dBm">>] ]})).

parse_rn_test() ->
	?assertMatch({ok, {3, rn, []}, <<>>}, parse(<<"0x03 RN\r\n">>)).

parse_n_test() ->
	?assertMatch({ok, {3, n, [3]}, <<>>}, parse(<<"0x03 N 0x3\r\n">>)).

parse_i_test() ->
	?assertMatch({more, {0, i, []}, <<"Vendor= toto\r\n"
									  "Module= toto\r\n"
									  "HW Ver= toto\r\n"
									  "HW Rev= toto\r\n"
									  "SW Ver= toto\r\n"
									  "FW Ver= toto\r\n"
									  "Part Num= toto\r\n"
									  "Ser. Num= toto\r\n"
									  "Prod. Date= toto\r\n">>}, 
				 parse(<<"0x00 I\r\n"
						 "Vendor= toto\r\n"
						 "Module= toto\r\n"
						 "HW Ver= toto\r\n"
						 "HW Rev= toto\r\n"
						 "SW Ver= toto\r\n"
						 "FW Ver= toto\r\n"
						 "Part Num= toto\r\n"
						 "Ser. Num= toto\r\n"
						 "Prod. Date= toto\r\n">>)),
	?assertMatch({more, {0, i, [ {vendor, <<"toto">>} ]}, <<"Module= toto\r\n"
															"HW Ver= toto\r\n"
															"HW Rev= toto\r\n"
															"SW Ver= toto\r\n"
															"FW Ver= toto\r\n"
															"Part Num= toto\r\n"
															"Ser. Num= toto\r\n"
															"Prod. Date= toto\r\n">>}, 
				 parse(<<"Vendor= toto\r\n"
						 "Module= toto\r\n"
						 "HW Ver= toto\r\n"
						 "HW Rev= toto\r\n"
						 "SW Ver= toto\r\n"
						 "FW Ver= toto\r\n"
						 "Part Num= toto\r\n"
						 "Ser. Num= toto\r\n"
						 "Prod. Date= toto\r\n">>, {0, i, []})),
	?assertMatch({more, {0, i, [ {moduleType, <<"toto">>}, 
								 {vendor, <<"toto">>} ]}, 
				  <<"HW Ver= toto\r\n"
					"HW Rev= toto\r\n"
					"SW Ver= toto\r\n"
					"FW Ver= toto\r\n"
					"Part Num= toto\r\n"
					"Ser. Num= toto\r\n"
					"Prod. Date= toto\r\n">>}, 
				 parse(<<"Module= toto\r\n"
						 "HW Ver= toto\r\n"
						 "HW Rev= toto\r\n"
						 "SW Ver= toto\r\n"
						 "FW Ver= toto\r\n"
						 "Part Num= toto\r\n"
						 "Ser. Num= toto\r\n"
						 "Prod. Date= toto\r\n">>, {0, i, [ {vendor, <<"toto">>} ]})),
	?assertMatch({ok, {0, i, [ {vendor, <<"toto">>},
							   {module, <<"toto">>},
							   {hwVer, <<"toto">>},
							   {hwRev, <<"toto">>},
							   {swVer, <<"toto">>},
							   {fwVer, <<"toto">>},
							   {partNum, <<"toto">>},
							   {serialNum, <<"toto">>},
							   {productDate, <<"toto">>}]}, <<>>}, 
				 parse(<<"Prod. Date= toto\r\n">>, 
					   {0, i, [ {serialNum, <<"toto">>},
								{partNum, <<"toto">>},
								{fwVer, <<"toto">>},
								{swVer, <<"toto">>},
								{hwRev, <<"toto">>},
								{hwVer, <<"toto">>},
								{module, <<"toto">>},
								{vendor, <<"toto">>}
							  ]})).


parse_raw_test() ->
	?assertMatch({raw, <<"A BINARY\r\n">>}, parse(<<"A BINARY\r\n">>)).

-endif.
