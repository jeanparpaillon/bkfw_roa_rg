-module(bkfw_scanner).
-author('jean.parpaillon@lizenn.com').

-include("bkfw.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([token/1]).

-type token_ret() :: {ok, token(), binary()} | {eof, binary()} | {more, binary()} | {error, term(), binary()}.

-spec token(binary()) -> token_ret().
token(Bin) -> token(Bin, <<>>).

token(<<>>, SoFar) -> {more, SoFar};
token(<< $\r, $\n >>, _) -> {eof, <<>>};
token(<< $\r, $\n, R/bits>>, _) -> {eof, R};
token(<< $\s, R/bits >>, SoFar) -> token(R, << SoFar/bits, $\s >>);
token(<< $\t, R/bits >>, SoFar) -> token(R, << SoFar/bits, $\t >>);
token(<< $=, R/bits >>, SoFar) -> s_value(R, <<>>, << SoFar/bits, $= >>);
token(<< "0x", R/bits >>, SoFar)                    -> s_hex_i(R, << SoFar/bits, "0x" >>);
token(<< Alpha, R/bits >>, SoFar) when Alpha >= 65, Alpha =< 90 -> 
    s_string(R, << Alpha >>, << SoFar/bits, Alpha >>);   % Upper-case alpha
token(<< Alpha, R/bits >>, SoFar) when Alpha >= 97, Alpha =< 122 -> 
    s_string(R, << Alpha >>, << SoFar/bits, Alpha >>);  % Lower-case alpha
token(<< $_, R/bits >>, SoFar) -> 
    s_string(R, << $_ >>, << SoFar/bits, $_ >>);
token(<< $0, R/bits >>, SoFar) -> s_num_i(R, << SoFar/bits, $0 >>);
token(<< $1, R/bits >>, SoFar) -> s_num(R, 1, << SoFar/bits, $1 >>);
token(<< $2, R/bits >>, SoFar) -> s_num(R, 2, << SoFar/bits, $2 >>);
token(<< $3, R/bits >>, SoFar) -> s_num(R, 3, << SoFar/bits, $3 >>);
token(<< $4, R/bits >>, SoFar) -> s_num(R, 4, << SoFar/bits, $4 >>);
token(<< $5, R/bits >>, SoFar) -> s_num(R, 5, << SoFar/bits, $5 >>);
token(<< $6, R/bits >>, SoFar) -> s_num(R, 6, << SoFar/bits, $6 >>);
token(<< $7, R/bits >>, SoFar) -> s_num(R, 7, << SoFar/bits, $7 >>);
token(<< $8, R/bits >>, SoFar) -> s_num(R, 8, << SoFar/bits, $8 >>);
token(<< $9, R/bits >>, SoFar) -> s_num(R, 9, << SoFar/bits, $9 >>);
token(<< C, R/bits >>, _) -> {error, io_lib:format("Invalid char: ~p", [C]), R}.

%%%
%%% priv
%%%
s_hex_i(<<>>, SoFar) -> {more, SoFar};
s_hex_i(<< $\r, $\n>>, _) -> {error, eof, <<>>};
s_hex_i(<< $\r, $\n, R/bits>>, _) -> {error, eof, <<$\r, $\n, R/bits>>};
s_hex_i(<< $0, R/bits >>, SoFar) -> s_hex(R, 0, << SoFar/bits, $0>>);
s_hex_i(<< $1, R/bits >>, SoFar) -> s_hex(R, 1, << SoFar/bits, $1>>);
s_hex_i(<< $2, R/bits >>, SoFar) -> s_hex(R, 2, << SoFar/bits, $2>>);
s_hex_i(<< $3, R/bits >>, SoFar) -> s_hex(R, 3, << SoFar/bits, $3>>);
s_hex_i(<< $4, R/bits >>, SoFar) -> s_hex(R, 4, << SoFar/bits, $4>>);
s_hex_i(<< $5, R/bits >>, SoFar) -> s_hex(R, 5, << SoFar/bits, $5>>);
s_hex_i(<< $6, R/bits >>, SoFar) -> s_hex(R, 6, << SoFar/bits, $6>>);
s_hex_i(<< $7, R/bits >>, SoFar) -> s_hex(R, 7, << SoFar/bits, $7>>);
s_hex_i(<< $8, R/bits >>, SoFar) -> s_hex(R, 8, << SoFar/bits, $8>>);
s_hex_i(<< $9, R/bits >>, SoFar) -> s_hex(R, 9, << SoFar/bits, $9>>);
s_hex_i(<< $a, R/bits >>, SoFar) -> s_hex(R, 10, << SoFar/bits, $a>>);
s_hex_i(<< $b, R/bits >>, SoFar) -> s_hex(R, 11, << SoFar/bits, $b>>);
s_hex_i(<< $c, R/bits >>, SoFar) -> s_hex(R, 12, << SoFar/bits, $c>>);
s_hex_i(<< $d, R/bits >>, SoFar) -> s_hex(R, 13, << SoFar/bits, $d>>);
s_hex_i(<< $e, R/bits >>, SoFar) -> s_hex(R, 14, << SoFar/bits, $e>>);
s_hex_i(<< $f, R/bits >>, SoFar) -> s_hex(R, 15, << SoFar/bits, $f>>);
s_hex_i(<< C, R/bits >>, _) -> {error, io_lib:format("Invalid hex: ~p", [C]), R}.

s_hex(<<>>, _, SoFar) -> {more, SoFar};
s_hex(<< $\r, $\n>>, Acc, _) -> {ok, Acc, <<>>};
s_hex(<< $\r, $\n, R/bits>>, Acc, _) -> {ok, Acc, <<$\r, $\n, R/bits>>};
s_hex(<< $\s, R/bits >>, Acc, _) -> {ok, Acc, R};
s_hex(<< $\t, R/bits >>, Acc, _) -> {ok, Acc, R};
s_hex(<< $0, R/bits >>, Acc, SoFar) -> s_hex(R, Acc * 16, << SoFar/bits, $0 >>);
s_hex(<< $1, R/bits >>, Acc, SoFar) -> s_hex(R, Acc * 16 + 1, << SoFar/bits, $1 >>);
s_hex(<< $2, R/bits >>, Acc, SoFar) -> s_hex(R, Acc * 16 + 2, << SoFar/bits, $2 >>);
s_hex(<< $3, R/bits >>, Acc, SoFar) -> s_hex(R, Acc * 16 + 3, << SoFar/bits, $3 >>);
s_hex(<< $4, R/bits >>, Acc, SoFar) -> s_hex(R, Acc * 16 + 4, << SoFar/bits, $4 >>);
s_hex(<< $5, R/bits >>, Acc, SoFar) -> s_hex(R, Acc * 16 + 5, << SoFar/bits, $5 >>);
s_hex(<< $6, R/bits >>, Acc, SoFar) -> s_hex(R, Acc * 16 + 6, << SoFar/bits, $6 >>);
s_hex(<< $7, R/bits >>, Acc, SoFar) -> s_hex(R, Acc * 16 + 7, << SoFar/bits, $7 >>);
s_hex(<< $8, R/bits >>, Acc, SoFar) -> s_hex(R, Acc * 16 + 8, << SoFar/bits, $8 >>);
s_hex(<< $9, R/bits >>, Acc, SoFar) -> s_hex(R, Acc * 16 + 9, << SoFar/bits, $9 >>);
s_hex(<< $a, R/bits >>, Acc, SoFar) -> s_hex(R, Acc * 16 + 10, << SoFar/bits, $a >>);
s_hex(<< $b, R/bits >>, Acc, SoFar) -> s_hex(R, Acc * 16 + 11, << SoFar/bits, $b >>);
s_hex(<< $c, R/bits >>, Acc, SoFar) -> s_hex(R, Acc * 16 + 12, << SoFar/bits, $c >>);
s_hex(<< $d, R/bits >>, Acc, SoFar) -> s_hex(R, Acc * 16 + 13, << SoFar/bits, $d >>);
s_hex(<< $e, R/bits >>, Acc, SoFar) -> s_hex(R, Acc * 16 + 14, << SoFar/bits, $e >>);
s_hex(<< $f, R/bits >>, Acc, SoFar) -> s_hex(R, Acc * 16 + 15, << SoFar/bits, $f >>);
s_hex(<< C, R/bits >>, _Acc, _SoFar) -> {error, io_lib:format("Invalid hex: ~p", [C]), R}.

s_value(<<>>, _, SoFar) -> {more, SoFar};
s_value(<< $\r, $\n>>, Acc, _) -> {ok, Acc, <<>>};
s_value(<< $\r, $\n, R/bits>>, Acc, _) -> {ok, Acc, <<$\r, $\n, R/bits>>};
s_value(<< C, R/bits >>, Acc, SoFar) ->  s_value(R, << Acc/binary, C >>, << SoFar/bits, C >>).

s_string(<<>>, _, SoFar) -> {more, SoFar};
s_string(<< $\r, $\n>>, Acc, _) -> s_str_or_atom(Acc, <<>>);
s_string(<< $\r, $\n, R/bits>>, Acc, _) -> s_str_or_atom(Acc, <<$\r, $\n, R/bits>>);
s_string(<< $=, $\s, R/bits >>, Acc, _) when 
      Acc =:= <<"FW Ver">>;
      Acc =:= <<"HW Rev">>;
      Acc =:= <<"HW Ver">>;
      Acc =:= <<"Module">>;
      Acc =:= <<"Part Num">>;
      Acc =:= <<"Prod. Date">>;
      Acc =:= <<"Ser. Num">>;
      Acc =:= <<"SW Ver">>;
      Acc =:= <<"Vendor">>
      -> s_str_or_atom(Acc, << $=, R/bits >>);
s_string(<< $\s, R/bits >>, Acc, SoFar) when
      Acc =:= <<"FW">>;
      Acc =:= <<"HW">>;
      Acc =:= <<"Part">>;
      Acc =:= <<"Prod.">>;
      Acc =:= <<"Ser.">>;
      Acc =:= <<"SW">>
      -> s_string(R, << Acc/bits, $\s >>, << SoFar/bits, $\s >>);
s_string(<< $\s, R/bits >>, Acc, _) -> s_str_or_atom(Acc, R);
s_string(<< $\t, R/bits >>, Acc, _) -> s_str_or_atom(Acc, R);
s_string(<< C, R/bits >>, Acc, SoFar) when C >= 65, C =< 90 ->        % upper-case letters
    s_string(R, << Acc/binary, C >>, << SoFar/bits, C >>);
s_string(<< C, R/bits >>, Acc, SoFar) when C >= 97, C =< 122 ->       % lower-case letters
    s_string(R, << Acc/binary, C >>, << SoFar/bits, C >>);
s_string(<< C, R/bits >>, Acc, SoFar) when C >= 48, C =< 57 ->        % digit
    s_string(R, << Acc/binary, C >>, << SoFar/bits, C >>);
s_string(<< $., R/bits >>, Acc, SoFar) -> s_string(R, << Acc/binary, $. >>, << SoFar/bits, $. >>);
s_string(<< $_, R/bits >>, Acc, SoFar) -> s_string(R, << Acc/binary, $_ >>, << SoFar/bits, $_ >>);
s_string(<< $:, R/bits >>, Acc, SoFar) -> s_string(R, << Acc/binary, $: >>, << SoFar/bits, $: >>);
s_string(<< C, R/bits >>, _Acc, _) -> 
    {error, io_lib:format("Invalid char in string: ~p", [C]), R}.

s_str_or_atom(Str, Rest) ->
    case kw_to_atom(Str) of
	A when is_atom(A) -> {ok, A, Rest};
	B -> {ok, B, Rest}
    end.

s_num_i(<<>>, SoFar) -> {more, SoFar};
s_num_i(<< $\r, $\n>>, _) -> {ok, 0, <<>>};
s_num_i(<< $\r, $\n, R/bits>>, _) -> {ok, 0, << $\r, $\n, R/bits>>};
s_num_i(<< $\s, R/bits >>, _) -> {ok, 0, R};
s_num_i(<< $\t, R/bits >>, _) -> {ok, 0, R};
s_num_i(<< $0, R/bits >>, SoFar) -> s_num_i(R, << SoFar/bits, $0 >>);
s_num_i(<< $1, R/bits >>, SoFar) -> s_num(R, 1, << SoFar/bits, $1 >>);
s_num_i(<< $2, R/bits >>, SoFar) -> s_num(R, 2, << SoFar/bits, $2 >>);
s_num_i(<< $3, R/bits >>, SoFar) -> s_num(R, 3, << SoFar/bits, $3 >>);
s_num_i(<< $4, R/bits >>, SoFar) -> s_num(R, 4, << SoFar/bits, $4 >>);
s_num_i(<< $5, R/bits >>, SoFar) -> s_num(R, 5, << SoFar/bits, $5 >>);
s_num_i(<< $6, R/bits >>, SoFar) -> s_num(R, 6, << SoFar/bits, $6 >>);
s_num_i(<< $7, R/bits >>, SoFar) -> s_num(R, 7, << SoFar/bits, $7 >>);
s_num_i(<< $8, R/bits >>, SoFar) -> s_num(R, 8, << SoFar/bits, $8 >>);
s_num_i(<< $9, R/bits >>, SoFar) -> s_num(R, 9, << SoFar/bits, $9 >>);
s_num_i(<< $., R/bits >>, SoFar) -> s_frac_i(R, 0, << SoFar/bits, $. >>);
s_num_i(<< C, R/bits >>, _) -> {error, io_lib:format("Invalid num: ~p", [C]), R}.

s_num(<<>>, _, SoFar) -> {more, SoFar};
s_num(<<$\r, $\n>>, Acc, _) -> {ok, Acc, <<>>};
s_num(<<$\r, $\n, R/bits>>, Acc, _) -> {ok, Acc, <<$\r, $\n, R/bits>>};
s_num(<< $\s, R/bits >>, Acc, _) -> {ok, Acc, R};
s_num(<< $\t, R/bits >>, Acc, _) -> {ok, Acc, R};
s_num(<< $0, R/bits >>, Acc, SoFar) -> s_num(R, Acc * 10, << SoFar/bits, $0 >>);
s_num(<< $1, R/bits >>, Acc, SoFar) -> s_num(R, Acc * 10 + 1, << SoFar/bits, $1 >>);
s_num(<< $2, R/bits >>, Acc, SoFar) -> s_num(R, Acc * 10 + 2, << SoFar/bits, $2 >>);
s_num(<< $3, R/bits >>, Acc, SoFar) -> s_num(R, Acc * 10 + 3, << SoFar/bits, $3 >>);
s_num(<< $4, R/bits >>, Acc, SoFar) -> s_num(R, Acc * 10 + 4, << SoFar/bits, $4 >>);
s_num(<< $5, R/bits >>, Acc, SoFar) -> s_num(R, Acc * 10 + 5, << SoFar/bits, $5 >>);
s_num(<< $6, R/bits >>, Acc, SoFar) -> s_num(R, Acc * 10 + 6, << SoFar/bits, $6 >>);
s_num(<< $7, R/bits >>, Acc, SoFar) -> s_num(R, Acc * 10 + 7, << SoFar/bits, $7 >>);
s_num(<< $8, R/bits >>, Acc, SoFar) -> s_num(R, Acc * 10 + 8, << SoFar/bits, $8 >>);
s_num(<< $9, R/bits >>, Acc, SoFar) -> s_num(R, Acc * 10 + 9, << SoFar/bits, $9 >>);
s_num(<< $., R/bits >>, Acc, SoFar) -> s_frac_i(R, Acc, << SoFar/bits, $. >>);
s_num(<< C, R/bits >>, _Acc, _) ->  {error, io_lib:format("Invalid num: ~p", [C]), R}.

s_frac_i(<<>>, _, SoFar) -> {more, SoFar};
s_frac_i(<<$\r, $\n>>, _, _) -> {error, "Invalid number: missing fraction", <<>>};
s_frac_i(<<$\r, $\n, R/bits>>, _, _) -> {error, "Invalid number: missing fraction", R};
s_frac_i(<< $0, R/bits >>, Int, SoFar) -> s_frac(R, Int, 0, 1, << SoFar/bits, $0 >>);
s_frac_i(<< $1, R/bits >>, Int, SoFar) -> s_frac(R, Int, 1, 1, << SoFar/bits, $1 >>);
s_frac_i(<< $2, R/bits >>, Int, SoFar) -> s_frac(R, Int, 2, 1, << SoFar/bits, $2 >>);
s_frac_i(<< $3, R/bits >>, Int, SoFar) -> s_frac(R, Int, 3, 1, << SoFar/bits, $3 >>);
s_frac_i(<< $4, R/bits >>, Int, SoFar) -> s_frac(R, Int, 4, 1, << SoFar/bits, $4 >>);
s_frac_i(<< $5, R/bits >>, Int, SoFar) -> s_frac(R, Int, 5, 1, << SoFar/bits, $5 >>);
s_frac_i(<< $6, R/bits >>, Int, SoFar) -> s_frac(R, Int, 6, 1, << SoFar/bits, $6 >>);
s_frac_i(<< $7, R/bits >>, Int, SoFar) -> s_frac(R, Int, 7, 1, << SoFar/bits, $7 >>);
s_frac_i(<< $8, R/bits >>, Int, SoFar) -> s_frac(R, Int, 8, 1, << SoFar/bits, $8 >>);
s_frac_i(<< $9, R/bits >>, Int, SoFar) -> s_frac(R, Int, 9, 1, << SoFar/bits, $9 >>);
s_frac_i(<< C, R/bits >>, _, _) -> {error, io_lib:format("Invalid num: ~p", [C]), R}.

s_frac(<<>>, _, _, _, SoFar) -> {more, SoFar};
s_frac(<<$\r, $\n>>, Int, Frac, E, _) -> {ok, Int + Frac / math:pow(10, E), <<>>};
s_frac(<<$\r, $\n, R/bits>>, Int, Frac, E, _) -> {ok, Int + Frac / math:pow(10, E), <<$\r, $\n, R/bits>>};
s_frac(<< $\s, R/bits >>, Int, Frac, E, _) -> {ok, Int + Frac / math:pow(10, E), R};
s_frac(<< $\t, R/bits >>, Int, Frac, E, _) -> {ok, Int + Frac / math:pow(10, E), R};
s_frac(<< $0, R/bits >>, Int, Frac, E, SoFar) -> s_frac(R, Int, Frac * 10, E + 1, << SoFar/bits, $0 >>);
s_frac(<< $1, R/bits >>, Int, Frac, E, SoFar) -> s_frac(R, Int, Frac * 10 + 1, E + 1, << SoFar/bits, $1 >>);
s_frac(<< $2, R/bits >>, Int, Frac, E, SoFar) -> s_frac(R, Int, Frac * 10 + 2, E + 1, << SoFar/bits, $2 >>);
s_frac(<< $3, R/bits >>, Int, Frac, E, SoFar) -> s_frac(R, Int, Frac * 10 + 3, E + 1, << SoFar/bits, $3 >>);
s_frac(<< $4, R/bits >>, Int, Frac, E, SoFar) -> s_frac(R, Int, Frac * 10 + 4, E + 1, << SoFar/bits, $4 >>);
s_frac(<< $5, R/bits >>, Int, Frac, E, SoFar) -> s_frac(R, Int, Frac * 10 + 5, E + 1, << SoFar/bits, $5 >>);
s_frac(<< $6, R/bits >>, Int, Frac, E, SoFar) -> s_frac(R, Int, Frac * 10 + 6, E + 1, << SoFar/bits, $6 >>);
s_frac(<< $7, R/bits >>, Int, Frac, E, SoFar) -> s_frac(R, Int, Frac * 10 + 7, E + 1, << SoFar/bits, $7 >>);
s_frac(<< $8, R/bits >>, Int, Frac, E, SoFar) -> s_frac(R, Int, Frac * 10 + 8, E + 1, << SoFar/bits, $8 >>);
s_frac(<< $9, R/bits >>, Int, Frac, E, SoFar) -> s_frac(R, Int, Frac * 10 + 9, E + 1, << SoFar/bits, $9 >>);
s_frac(<< C, R/bits >>, _, _, _, _) -> {error, io_lib:format("Invalid num: ~p", [C]), R}.

kw_to_atom(<<"FW Ver">>)    -> fwVer;
kw_to_atom(<<"HW Rev">>)    -> hwRev;
kw_to_atom(<<"HW Ver">>)    -> hwVer;
kw_to_atom(<<"Module">>)    -> moduleType;
kw_to_atom(<<"Part Num">>)  -> partNum;
kw_to_atom(<<"Prod. Date">>)-> productDate;
kw_to_atom(<<"Ser. Num">>)  -> serialNum;
kw_to_atom(<<"SW Ver">>)    -> swVer;
kw_to_atom(<<"Vendor">>)    -> vendor;
kw_to_atom(<<"ADI">>)       -> adi;
kw_to_atom(<<"ALARMS:">>)   -> alarms;
kw_to_atom(<<"BREF">>)      -> bref;
kw_to_atom(<<"CC">>)        -> cc;
kw_to_atom(<<"EDFA_TEMP">>) -> edfa_temp;
kw_to_atom(<<"EDFA_PSU">>)  -> edfa_psu;
kw_to_atom(<<"GC">>)        -> gc;
kw_to_atom(<<"IT">>)        -> it;
kw_to_atom(<<"I">>)         -> i;
kw_to_atom(<<"LC">>)        -> lc;
kw_to_atom(<<"LI">>)        -> li;
kw_to_atom(<<"LO">>)        -> lo;
kw_to_atom(<<"LT">>)        -> lt;
kw_to_atom(<<"MODE">>)      -> mode;
kw_to_atom(<<"MODULE=">>)   -> module;
kw_to_atom(<<"MUTE">>)      -> mute;
kw_to_atom(<<"N">>)         -> n;
kw_to_atom(<<"OFF">>)       -> off;
kw_to_atom(<<"PC">>)        -> pc;
kw_to_atom(<<"PIN">>)       -> pin;
kw_to_atom(<<"POUT">>)      -> pout;
kw_to_atom(<<"PUMP_BIAS">>) -> pump_bias;
kw_to_atom(<<"PUMP_TEMP">>) -> pump_temp;
kw_to_atom(<<"RA">>)        -> ra;
kw_to_atom(<<"RCC">>)       -> rcc;
kw_to_atom(<<"RGC">>)       -> rgc;
kw_to_atom(<<"RI">>)        -> ri;
kw_to_atom(<<"RIT">>)       -> rit;
kw_to_atom(<<"RLC">>)       -> rlc;
kw_to_atom(<<"RLI">>)       -> rli;
kw_to_atom(<<"RLO">>)       -> rlo;
kw_to_atom(<<"RLT">>)       -> rlt;
kw_to_atom(<<"RMODE">>)     -> rmode;
kw_to_atom(<<"RN">>)        -> rn;
kw_to_atom(<<"RPC">>)       -> rpc;
kw_to_atom(<<"RPM">>)       -> rpm;
kw_to_atom(<<"RV">>)        -> rv;
kw_to_atom(<<"SCC">>)       -> scc;
kw_to_atom(<<"SGC">>)       -> sgc;
kw_to_atom(<<"SLI">>)       -> sli;
kw_to_atom(<<"SLO">>)       -> slo;
kw_to_atom(<<"SMODE">>)     -> smode;
kw_to_atom(<<"SPC">>)       -> spc;
kw_to_atom(<<"VENDOR=">>)   -> vendor;
kw_to_atom(<<"V">>)         -> v;
kw_to_atom(S)               -> S.

-ifdef(TEST).

float_test() ->
    ?assertEqual({more, <<"0.001">>}, token(<<"0.001">>)),
    ?assertEqual({more, <<"0.1234">>}, token(<<"0.1234">>)),
    ?assertEqual({more, <<"3.0">>}, token(<<"3.0">>)),
    ?assertEqual({ok, 0.001, <<>>}, token(<<"0.001 ">>)),
    ?assertEqual({ok, 0.1234, <<>>}, token(<<"0.1234 ">>)),
    ?assertEqual({ok, 3.0, <<>>}, token(<<"3.0 ">>)).    

-endif.
