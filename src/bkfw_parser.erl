-module(bkfw_parser).
-author('jean.parpaillon@lizenn.com').

-include("bkfw.hrl").

-export([parse/1,
	 parse/2]).

parse(Bin) ->
    parse(Bin, undefined).

-spec parse(Data :: binary(), undefined | msg()) -> {ok, msg()} | {more, msg()} | {error, term()}.
parse(<<>>, Msg) ->
    {ok, Msg};
parse(Bin, undefined) ->
    parse_address(bkfw_scanner:token(Bin));
parse(Bin, {_, i, _}=Msg) ->
    parse_kv(bkfw_scanner:token(Bin), Msg);
parse(Bin, {_, pd, _}=Msg) ->
    parse_cmd(bkfw_scanner:token(Bin), Msg).


%%%
%%% Priv
%%%
parse_address({error, Err}) ->
    {error, Err};
parse_address(eof) ->
    {error, eof};
parse_address({ok, N, Rest}) when is_integer(N) ->
    parse_cmd(bkfw_scanner:token(Rest), {N, undefined, []});
parse_address({ok, Tok, _}) ->
    {error, io_lib:format("Expect integer, got: ~p", [Tok])}.


parse_cmd({error, Err}, _) ->
    {error, Err};
parse_cmd(eof, _) ->
    {error, eof};
parse_cmd({ok, vendor, Rest}, {Addr, _, _}) ->
    parse_value(bkfw_scanner:token(Rest), {Addr, i, []}, vendor);
parse_cmd({ok, <<  "PD", BinI/bits >>, Rest}, {Addr, _, Lines}) ->
    try binary_to_integer(BinI) of
	I ->
	    parse_args(bkfw_scanner:token(Rest), {Addr, pd, Lines}, [I])
    catch error:badarg ->
	    {error, io_lib:format("Expect integer, got: ~p", [BinI])}
    end;
parse_cmd({ok, Cmd, <<>>}, {A, _, Lines}) when is_atom(Cmd) ->
    is_multiline({A, Cmd, Lines});
parse_cmd({ok, Cmd, Rest}, {A, _, Lines}) when is_atom(Cmd) ->
    parse_args(bkfw_scanner:token(Rest), {A, Cmd, Lines}, []);
parse_cmd({ok, Tok, _Rest}, _) ->
    {error, io_lib:format("Expect atom, got: ~p", [Tok])}.


parse_args({error, Err}, {_, _, _}, _) ->
    {error, Err};
parse_args(eof, {A, C, Lines}, Args) ->
    is_multiline({A, C, Lines ++ [lists:reverse(Args)]});
parse_args({ok, Arg, Rest}, {A, C, L}, Args) ->
    parse_args(bkfw_scanner:token(Rest), {A, C, L}, [ Arg | Args ]).

parse_kv({error, Err}, _) ->
    {error, Err};
parse_kv(eof, Msg) ->
    {more, Msg};
parse_kv({ok, Key, Rest}, Msg) ->
    parse_value(bkfw_scanner:token(Rest), Msg, Key).


parse_value({error, Err}, _, _) ->
    {error, Err};
parse_value(eof, {A, C, Lines}, K) ->
    {more, {A, C, Lines ++ [{K, undefined}]}};
parse_value({ok, Value, _Rest}, {A, C, Lines}, Key) ->
    {more, {A, C, Lines ++ [{Key, Value}]}}.


is_multiline({_, i, _}=Msg) -> {more, Msg};
is_multiline({_, pd, _}=Msg) -> {more, Msg};
is_multiline({A, C, []}) -> {ok, {A, C, []}};
is_multiline({A, C, [Line]}) -> {ok, {A, C, Line}}.
