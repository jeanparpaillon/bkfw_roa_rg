-module(bkfw_parser).
-author('jean.parpaillon@lizenn.com').

-include("bkfw.hrl").

-export([parse/1,
	 parse/2]).

parse(Bin) ->
    parse(Bin, undefined).

-spec parse(Data :: binary(), undefined | msg()) -> {ok, msg()} | {more, msg(), binary()} | {error, term()}.
parse(<<>>, Msg) ->
    {ok, Msg, <<>>};
parse(Bin, undefined) ->
    parse_address(bkfw_scanner:token(Bin));
parse(Bin, {_, i, _}=Msg) ->
    parse_kv(bkfw_scanner:token(Bin), Msg);
parse(Bin, {_, pd, _}=Msg) ->
    parse_cmd(bkfw_scanner:token(Bin), Msg).


%%%
%%% Priv
%%%
parse_address({error, Err, Bin}) ->
    {error, Err, Bin};
parse_address({eof, Rest}) ->
    {error, eof, Rest};
parse_address({ok, N, Rest}) when is_integer(N) ->
    parse_cmd(bkfw_scanner:token(Rest), {N, undefined, []});
parse_address({ok, Tok, Rest}) ->
    {error, io_lib:format("Expect integer, got: ~p", [Tok]), Rest}.


parse_cmd({error, Err, Rest}, _) ->
    {error, Err, Rest};
parse_cmd({eof, Rest}, {Addr, pd, Lines}) ->
    {ok, {Addr, pd, Lines}, Rest};
parse_cmd({eof, Rest}, {Addr, ri, Lines}) ->
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
parse_cmd({ok, Cmd, <<>>}, {A, _, Lines}) when is_atom(Cmd) ->
    is_multiline({A, Cmd, Lines}, <<>>);
parse_cmd({ok, Cmd, Rest}, {A, _, Lines}) when is_atom(Cmd) ->
    parse_args(bkfw_scanner:token(Rest), {A, Cmd, Lines}, []);
parse_cmd({ok, Tok, Rest}, _) ->
    {error, io_lib:format("Expect atom, got: ~p", [Tok]), Rest}.


parse_args({error, Err, Rest}, {_, _, _}, _) ->
    {error, Err, Rest};
parse_args({eof, Rest}, {A, C, Lines}, Args) ->
    is_multiline({A, C, Lines ++ [lists:reverse(Args)]}, Rest);
parse_args({ok, Arg, Rest}, {A, C, L}, Args) ->
    parse_args(bkfw_scanner:token(Rest), {A, C, L}, [ Arg | Args ]).

parse_kv({error, Err, Rest}, _) ->
    {error, Err, Rest};
parse_kv({eof, Rest}, Msg) ->
    {ok, Msg, Rest};
parse_kv({ok, Key, Rest}, Msg) ->
    parse_value(bkfw_scanner:token(Rest), Msg, Key).


parse_value({error, Err, Rest}, _, _) ->
    {error, Err, Rest};
parse_value({eof, Rest}, {A, C, Lines}, K) ->
    {more, {A, C, Lines ++ [{K, undefined}]}, Rest};
parse_value({ok, Value, Rest}, {A, C, Lines}, Key) ->
    {more, {A, C, Lines ++ [{Key, Value}]}, Rest}.


is_multiline({_, i, _}=Msg, Rest) -> {more, Msg, Rest};
is_multiline({_, pd, _}=Msg, Rest) -> {more, Msg, Rest};
is_multiline({A, C, []}, Rest) -> {ok, {A, C, []}, Rest};
is_multiline({A, C, [Line]}, Rest) -> {ok, {A, C, Line}, Rest}.
