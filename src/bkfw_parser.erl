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
    case kw_to_atom(Cmd) of
	{error, Err} -> 
	    {error, Err};
	AtomCmd ->
	    {ok, Addr, AtomCmd, []}
    end;
parse_cmd({ok, {string, Cmd}, Rest}, Addr) ->
    case kw_to_atom(Cmd) of
	{error, Err} -> 
	    {error, Err};
	AtomCmd ->
	    parse_args(bkfw_scanner:token(Rest), Addr, AtomCmd, [])
    end;
parse_cmd({ok, {Tok, _}, _Rest}, _A) ->
    {error, io_lib:format("Expect string, got: ~p", [Tok])}.


parse_args({error, Err}, _, _, _) ->
    {error, Err};
parse_args(eof, A, C, Args) ->
    {ok, A, C, lists:reverse(Args)};
parse_args({ok, {_Type, Arg}, Rest}, A, C, Args) ->
    parse_args(bkfw_scanner:token(Rest), A, C, [ Arg | Args ]).

kw_to_atom(<<"RCC">>)       -> rcc;
kw_to_atom(<<"CC">>)        -> cc;
kw_to_atom(<<"SCC">>)       -> scc;
kw_to_atom(<<"RGC">>)       -> rgc;
kw_to_atom(<<"GC">>)        -> gc;
kw_to_atom(<<"SGC">>)       -> sgc;
kw_to_atom(<<"RPC">>)       -> rpc;
kw_to_atom(<<"PC">>)        -> pc;
kw_to_atom(<<"SPC">>)       -> spc;
kw_to_atom(<<"RMODE">>)     -> rmode;
kw_to_atom(<<"MODE">>)      -> mode;
kw_to_atom(<<"OFF">>)       -> off;
kw_to_atom(<<"SMODE">>)     -> smode;
kw_to_atom(<<"RA">>)        -> ra;
kw_to_atom(<<"ALARMS">>)    -> alarms;
kw_to_atom(<<"PIN">>)       -> pin;
kw_to_atom(<<"POUT">>)      -> pout;
kw_to_atom(<<"PUMP_TEMP">>) -> pump_temp;
kw_to_atom(<<"PUMP_BIAS">>) -> pump_bias;
kw_to_atom(<<"EDFA_TEMP">>) -> edfa_temp;
kw_to_atom(<<"EDFA_PSU">>)  -> edfa_psu;
kw_to_atom(<<"BREF">>)      -> bref;
kw_to_atom(<<"ADI">>)       -> adi;
kw_to_atom(<<"MUTE">>)      -> mute;
kw_to_atom(<<"RLT">>)       -> rlt;
kw_to_atom(<<"LT">>)        -> lt;
kw_to_atom(<<"RLC">>)       -> rlc;
kw_to_atom(<<"LC">>)        -> lc;
kw_to_atom(<<"RIT">>)       -> rit;
kw_to_atom(<<"IT">>)        -> it;
kw_to_atom(<<"RI">>)        -> ri;
kw_to_atom(<<"RPM">>)       -> rpm;
kw_to_atom(<<"RV">>)        -> rv;
kw_to_atom(<<"RLI">>)       -> rli;
kw_to_atom(<<"SLI">>)       -> sli;
kw_to_atom(<<"RLO">>)       -> rlo;
kw_to_atom(<<"SLO">>)       -> slo;
kw_to_atom(_S) -> {error, io_lib:format("Invalid keyword: ~p", [_S])}.
