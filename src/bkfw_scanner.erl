-module(bkfw_scanner).
-author('jean.parpaillon@lizenn.com').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([token/1]).

-type token() :: {integer, integer()} | {float, float()} | {string, string()}.
-type token_ret() :: {ok, token(), binary()} | eof | {error, term()}.

-spec token(binary()) -> token_ret().
token(<<>>) -> eof;
token(<< $\s, R/bits >>) -> token(R);
token(<< $\t, R/bits >>) -> token(R);
token(<< $0, $x, R/bits >>) -> s_hex_i(R);
token(<< Alpha, R/bits >>) when Alpha >= 65, Alpha =< 90 -> s_string(R, << Alpha >>);   % Upper-case alpha
token(<< Alpha, R/bits >>) when Alpha >= 97, Alpha =< 122 -> s_string(R, << Alpha >>);  % Lower-case alpha
token(<< $0, R/bits >>) -> s_num_i(R);
token(<< $1, R/bits >>) -> s_num(R, 1);
token(<< $2, R/bits >>) -> s_num(R, 2);
token(<< $3, R/bits >>) -> s_num(R, 3);
token(<< $4, R/bits >>) -> s_num(R, 4);
token(<< $5, R/bits >>) -> s_num(R, 5);
token(<< $6, R/bits >>) -> s_num(R, 6);
token(<< $7, R/bits >>) -> s_num(R, 7);
token(<< $8, R/bits >>) -> s_num(R, 8);
token(<< $9, R/bits >>) -> s_num(R, 9);
token(<< C, _R/bits >>) -> {error, io_lib:format("Invalid char: ~p", [C])}.

%%%
%%% priv
%%%
s_hex_i(<<>>) -> {error, eof};
s_hex_i(<< $0, R/bits >>) -> s_hex(R, 0);
s_hex_i(<< $1, R/bits >>) -> s_hex(R, 1);
s_hex_i(<< $2, R/bits >>) -> s_hex(R, 2);
s_hex_i(<< $3, R/bits >>) -> s_hex(R, 3);
s_hex_i(<< $4, R/bits >>) -> s_hex(R, 4);
s_hex_i(<< $5, R/bits >>) -> s_hex(R, 5);
s_hex_i(<< $6, R/bits >>) -> s_hex(R, 6);
s_hex_i(<< $7, R/bits >>) -> s_hex(R, 7);
s_hex_i(<< $8, R/bits >>) -> s_hex(R, 8);
s_hex_i(<< $9, R/bits >>) -> s_hex(R, 9);
s_hex_i(<< $a, R/bits >>) -> s_hex(R, 10);
s_hex_i(<< $b, R/bits >>) -> s_hex(R, 11);
s_hex_i(<< $c, R/bits >>) -> s_hex(R, 12);
s_hex_i(<< $d, R/bits >>) -> s_hex(R, 13);
s_hex_i(<< $e, R/bits >>) -> s_hex(R, 14);
s_hex_i(<< $f, R/bits >>) -> s_hex(R, 15);
s_hex_i(<< C, _R/bits >>) -> {error, io_lib:format("Invalid hex: ~p", [C])}.

s_hex(<<>>, Acc) -> {ok, {integer, Acc}, <<>>};
s_hex(<< $\s, R/bits >>, Acc) -> {ok, {integer, Acc}, R};
s_hex(<< $\t, R/bits >>, Acc) -> {ok, {integer, Acc}, R};
s_hex(<< $0, R/bits >>, Acc) -> s_hex(R, Acc * 16);
s_hex(<< $1, R/bits >>, Acc) -> s_hex(R, Acc * 16 + 1);
s_hex(<< $2, R/bits >>, Acc) -> s_hex(R, Acc * 16 + 2);
s_hex(<< $3, R/bits >>, Acc) -> s_hex(R, Acc * 16 + 3);
s_hex(<< $4, R/bits >>, Acc) -> s_hex(R, Acc * 16 + 4);
s_hex(<< $5, R/bits >>, Acc) -> s_hex(R, Acc * 16 + 5);
s_hex(<< $6, R/bits >>, Acc) -> s_hex(R, Acc * 16 + 6);
s_hex(<< $7, R/bits >>, Acc) -> s_hex(R, Acc * 16 + 7);
s_hex(<< $8, R/bits >>, Acc) -> s_hex(R, Acc * 16 + 8);
s_hex(<< $9, R/bits >>, Acc) -> s_hex(R, Acc * 16 + 9);
s_hex(<< $a, R/bits >>, Acc) -> s_hex(R, Acc * 16 + 10);
s_hex(<< $b, R/bits >>, Acc) -> s_hex(R, Acc * 16 + 11);
s_hex(<< $c, R/bits >>, Acc) -> s_hex(R, Acc * 16 + 12);
s_hex(<< $d, R/bits >>, Acc) -> s_hex(R, Acc * 16 + 13);
s_hex(<< $e, R/bits >>, Acc) -> s_hex(R, Acc * 16 + 14);
s_hex(<< $f, R/bits >>, Acc) -> s_hex(R, Acc * 16 + 15);
s_hex(<< C, _R/bits >>, _Acc) -> {error, io_lib:format("Invalid hex: ~p", [C])}.

s_string(<<>>, Acc) -> {ok, {string, Acc}, <<>>};
s_string(<< $\s, R/bits >>, Acc) -> {ok, {string, Acc}, R};
s_string(<< $\t, R/bits >>, Acc) -> {ok, {string, Acc}, R};
s_string(<< C, R/bits >>, Acc) when C >= 65, C =< 90 ->
    s_string(R, << Acc/binary, C >>);
s_string(<< C, R/bits >>, Acc) when C >= 97, C =< 122 ->
    s_string(R, << Acc/binary, C >>);
s_string(<< C, _R/bits >>, _Acc) -> 
    {error, io_lib:format("Invalid char in string: ~p", [C])}.

s_num_i(<<>>) -> {ok, {integer, 0}, <<>>};
s_num_i(<< $\s, R/bits >>) -> {ok, {integer, 0}, R};
s_num_i(<< $\t, R/bits >>) -> {ok, {integer, 0}, R};
s_num_i(<< $0, R/bits >>) -> s_num_i(R);
s_num_i(<< $1, R/bits >>) -> s_num(R, 1);
s_num_i(<< $2, R/bits >>) -> s_num(R, 2);
s_num_i(<< $3, R/bits >>) -> s_num(R, 3);
s_num_i(<< $4, R/bits >>) -> s_num(R, 4);
s_num_i(<< $5, R/bits >>) -> s_num(R, 5);
s_num_i(<< $6, R/bits >>) -> s_num(R, 6);
s_num_i(<< $7, R/bits >>) -> s_num(R, 7);
s_num_i(<< $8, R/bits >>) -> s_num(R, 8);
s_num_i(<< $9, R/bits >>) -> s_num(R, 9);
s_num_i(<< $., R/bits >>) -> s_frac_i(R, 0);
s_num_i(<< C, _R/bits >>) -> {error, io_lib:format("Invalid num: ~p", [C])}.

s_num(<<>>, Acc) -> {ok, {integer, Acc}, <<>>};
s_num(<< $\s, R/bits >>, Acc) -> {ok, {integer, Acc}, R};
s_num(<< $\t, R/bits >>, Acc) -> {ok, {integer, Acc}, R};
s_num(<< $0, R/bits >>, Acc) -> s_num(R, Acc * 10);
s_num(<< $1, R/bits >>, Acc) -> s_num(R, Acc * 10 + 1);
s_num(<< $2, R/bits >>, Acc) -> s_num(R, Acc * 10 + 2);
s_num(<< $3, R/bits >>, Acc) -> s_num(R, Acc * 10 + 3);
s_num(<< $4, R/bits >>, Acc) -> s_num(R, Acc * 10 + 4);
s_num(<< $5, R/bits >>, Acc) -> s_num(R, Acc * 10 + 5);
s_num(<< $6, R/bits >>, Acc) -> s_num(R, Acc * 10 + 6);
s_num(<< $7, R/bits >>, Acc) -> s_num(R, Acc * 10 + 7);
s_num(<< $8, R/bits >>, Acc) -> s_num(R, Acc * 10 + 8);
s_num(<< $9, R/bits >>, Acc) -> s_num(R, Acc * 10 + 9);
s_num(<< $., R/bits >>, Acc) -> s_frac_i(R, Acc);
s_num(<< C, _R/bits >>, _Acc) ->  {error, io_lib:format("Invalid num: ~p", [C])}.

s_frac_i(<<>>, _) -> {error, "Invalid number: missing fraction"};
s_frac_i(<< $0, R/bits >>, Int) -> s_frac(R, Int, 0, 1);
s_frac_i(<< $1, R/bits >>, Int) -> s_frac(R, Int, 1, 1);
s_frac_i(<< $2, R/bits >>, Int) -> s_frac(R, Int, 2, 1);
s_frac_i(<< $3, R/bits >>, Int) -> s_frac(R, Int, 3, 1);
s_frac_i(<< $4, R/bits >>, Int) -> s_frac(R, Int, 4, 1);
s_frac_i(<< $5, R/bits >>, Int) -> s_frac(R, Int, 5, 1);
s_frac_i(<< $6, R/bits >>, Int) -> s_frac(R, Int, 6, 1);
s_frac_i(<< $7, R/bits >>, Int) -> s_frac(R, Int, 7, 1);
s_frac_i(<< $8, R/bits >>, Int) -> s_frac(R, Int, 8, 1);
s_frac_i(<< $9, R/bits >>, Int) -> s_frac(R, Int, 9, 1);
s_frac_i(<< C, _R/bits >>, _) -> {error, io_lib:format("Invalid num: ~p", [C])}.

s_frac(<<>>, Int, Frac, E) -> {ok, {float, Int + Frac / math:pow(10, E)}, <<>>};
s_frac(<< $\s, R/bits >>, Int, Frac, E) -> {ok, {float, Int + Frac / math:pow(10, E)}, R};
s_frac(<< $\t, R/bits >>, Int, Frac, E) -> {ok, {float, Int + Frac / math:pow(10, E)}, R};
s_frac(<< $0, R/bits >>, Int, Frac, E) -> s_frac(R, Int, Frac * 10, E + 1);
s_frac(<< $1, R/bits >>, Int, Frac, E) -> s_frac(R, Int, Frac * 10 + 1, E + 1);
s_frac(<< $2, R/bits >>, Int, Frac, E) -> s_frac(R, Int, Frac * 10 + 2, E + 1);
s_frac(<< $3, R/bits >>, Int, Frac, E) -> s_frac(R, Int, Frac * 10 + 3, E + 1);
s_frac(<< $4, R/bits >>, Int, Frac, E) -> s_frac(R, Int, Frac * 10 + 4, E + 1);
s_frac(<< $5, R/bits >>, Int, Frac, E) -> s_frac(R, Int, Frac * 10 + 5, E + 1);
s_frac(<< $6, R/bits >>, Int, Frac, E) -> s_frac(R, Int, Frac * 10 + 6, E + 1);
s_frac(<< $7, R/bits >>, Int, Frac, E) -> s_frac(R, Int, Frac * 10 + 7, E + 1);
s_frac(<< $8, R/bits >>, Int, Frac, E) -> s_frac(R, Int, Frac * 10 + 8, E + 1);
s_frac(<< $9, R/bits >>, Int, Frac, E) -> s_frac(R, Int, Frac * 10 + 9, E + 1);
s_frac(<< C, _R/bits >>, _, _, _) -> {error, io_lib:format("Invalid num: ~p", [C])}.

-ifdef(TEST).

float_test() ->
    ?assertEqual({ok, {float, 0.001}, <<>>}, token(<<"0.001">>)),
    ?assertEqual({ok, {float, 0.1234}, <<>>}, token(<<"0.1234">>)),
    ?assertEqual({ok, {float, 3.0}, <<>>}, token(<<"3.0">>)).    

-endif.
