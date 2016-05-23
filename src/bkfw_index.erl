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
    case cowboy_req:path(Req) of
		{<<"/">>, Req2} ->
			{ok, cowboy_req:set([{path, <<"/index.html">>}], Req2), Env};
		{_, Req2} ->
			{ok, Req2, Env}
    end.
