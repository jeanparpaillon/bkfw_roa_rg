%% @doc Serve index.html file
%%
-module(bkfw_index).
-behaviour(cowboy_middleware).

-export([execute/2]).

%% @private
execute(Req, Env) ->
    case cowboy_req:method(Req) of
	{<<"GET">>, Req1} -> check_url(Req1, Env);
	{<<"HEAD">>, Req1} -> check_url(Req1, Env);
	{<<"OPTIONS">>, Req1} -> check_url(Req1, Env);
	{_M, Req1} -> {ok, Req1, Env}
    end.

check_url(Req, Env) ->
    {Path, Req1} = cowboy_req:path(Req),
    case binary:at(Path, byte_size(Path)-1) of
	$/ ->
	    Idx = << Path/binary, "index.html" >>,
	    {ok, cowboy_req:set([{path, Idx}], Req1), Env};
	_ ->
	    {ok, Req1, Env}
    end.
