-module(bkfw_parser).
-author('jean.parpaillon@lizenn.com').

-export([parse_query/1]).

parse_query(<<>>) ->
    eof;
parse_query(Bin) ->
    parse_address(bkfw_scanner:token(Bin)).

%%%
%%% Priv
%%%
parse_address({error, Err}) ->
    {error, Err};
parse_address(eof) ->
    {error, eof};
parse_address({ok, {integer, N}, Rest}) ->
    parse_cmd(bkfw_scanner:token(Rest), N);
parse_address({ok, {Tok, _}, _}) ->
    {error, io_lib:format("Expect integer, got: ~p", [Tok])}.


parse_cmd({error, Err}, _) ->
    {error, Err};
parse_cmd(eof, _) ->
    {error, eof};
parse_cmd({ok, {string, Cmd}, <<>>}, Addr) ->
    {ok, Addr, Cmd, []};
parse_cmd({ok, {string, Cmd}, Rest}, Addr) ->
    parse_args(bkfw_scanner:token(Rest), Addr, Cmd, []);
parse_cmd({ok, {Tok, _}, _Rest}, _A) ->
    {error, io_lib:format("Expect string, got: ~p", [Tok])}.


parse_args({error, Err}, _, _, _) ->
    {error, Err};
parse_args(eof, A, C, Args) ->
    {ok, A, C, lists:reverse(Args)};
parse_args({ok, {_Type, Arg}, Rest}, A, C, Args) ->
    parse_args(bkfw_scanner:token(Rest), A, C, [ Arg | Args ]).
