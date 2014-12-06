-module(bkfw_http).

-include("bkfw.hrl").

%%% API
-export([get_config/0]).

%%% Cowboy REST callbacks
-export([
	 init/3,
	 rest_init/2,
	 allowed_methods/2,
	 content_types_accepted/2,
	 content_types_provided/2,
	 resource_exists/2,
	 allow_missing_post/2,
	 to_json/2,
	 from_json/2
	]).

-define(PORT, 8080).
-define(JSX_OPTS, [{space, 1}, {indent, 2}]).

-record(state, {
	  section   = undefined :: mcu | edfa | sys,
	  index     = undefined :: integer() | undefined | badarg,
	  mcu       = undefined,
	  sys       = undefined :: login | net | password | community | protocol | firmware
	 }).

%%%
%%% API
%%%
get_config() ->
    Opts = application:get_env(bkfw, http, []),
    Dir = filename:join(code:priv_dir(bkfw), "www"),
    Handlers = [
		{"/api/mcu/[:index]", bkfw_http, mcu},
		{"/api/edfa",       bkfw_http, edfa},
		{"/api/sys/:name",  bkfw_http, sys},
		{"/[...]", cowboy_static, {dir, Dir, [{mimetypes, cow_mimetypes, all}]}}
	       ],
    Args = [http, 1, 
	    [{port, proplists:get_value(port, Opts, ?PORT)}],
	    [{env, [{dispatch, cowboy_router:compile([{'_', Handlers}])}]},
	     {middlewares, [bkfw_index, cowboy_router, cowboy_handler]}]
	   ],
    {http, {cowboy, start_http, Args}, permanent, 5000, worker, [cowboy]}.

%%%
%%% Cowboy callbacks
%%%
init(_, _, _) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, mcu) ->
    case cowboy_req:binding(index, Req) of
	{undefined, Req2} ->
	    {ok, Req2, #state{section=mcu, index=undefined}};
	{BinI, Req2} ->
	    try binary_to_integer(BinI) of
		I ->
		    {ok, Req2, #state{section=mcu, index=I}}
	    catch error:badarg ->
		    {ok, Req2, #state{section=mcu, index=badarg}}
	    end
    end;
rest_init(Req, edfa) ->
    {ok, Req, #state{section=edfa}};
rest_init(Req, sys) ->
    {ok, Req, #state{section=sys}};
rest_init(Req, _Sec) ->
    {ok, Req, #state{section=undefined}}.


allowed_methods(Req, #state{section=edfa}=S) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, S};
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"POST">>, <<"OPTIONS">>], Req, State}.


content_types_accepted(Req, State) ->
    case cowboy_req:has_body(Req) of
	true ->
	    {[
	      {{<<"application">>, <<"json">>, []}, from_json}
	     ], Req, State};
	false ->
	    {[], Req, State}
    end.


content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, to_json}
     ], Req, State}.


resource_exists(Req, #state{section=mcu, index=badarg}=S) ->
    {false, Req, S};
resource_exists(Req, #state{section=mcu, index=undefined}=S) ->
    {true, Req, S};
resource_exists(Req, #state{section=mcu, index=I}=S) ->
    case mnesia:dirty_match_object(#edfaMcuTable{index=I, _='_'}) of
	[] ->
	    {false, Req, S};
	[Mcu] ->
	    {true, Req, S#state{mcu=Mcu}}
    end;
resource_exists(Req, #state{section=sys}=S) ->
    case cowboy_req:binding(name, Req) of
	{<<"login">>, Req2} -> {true, Req2, S#state{sys=login}};
	{<<"net">>, Req2} -> {true, Req2, S#state{sys=net}};
	{<<"password">>, Req2} -> {true, Req2, S#state{sys=password}};
	{<<"community">>, Req2} -> {true, Req2, S#state{sys=community}};
	{<<"protocol">>, Req2} -> {true, Req2, S#state{sys=protocol}};
	{<<"firmware">>, Req2} -> {true, Req2, S#state{sys=firmware}}
    end;
resource_exists(Req, #state{section=undefined}=S) ->
    {false, Req, S};
resource_exists(Req, S) ->
    {true, Req, S}.


allow_missing_post(Req, State) ->
    {false, Req, State}.



to_json(Req, #state{section=mcu, index=undefined}=S) ->
    Mcus = mnesia:dirty_match_object(#edfaMcuTable{_='_'}),
    Ejson = lists:map(fun bkfw_mcu:get_kv/1, Mcus),
    {jsx:encode(Ejson, ?JSX_OPTS), Req, S};

to_json(Req, #state{section=mcu, mcu=Mcu}=S) ->
    {jsx:encode(bkfw_mcu:get_kv(Mcu), ?JSX_OPTS), Req, S};

to_json(Req, #state{section=edfa}=S) ->
    {jsx:encode(bkfw_edfa:get(), ?JSX_OPTS), Req, S};

to_json(Req, #state{section=sys, sys=login}=S) ->
    {jsx:encode(bkfw_config:get_kv(login)), Req, S};

to_json(Req, #state{section=sys, sys=net}=S) ->
    {jsx:encode(bkfw_config:get_kv(net), ?JSX_OPTS), Req, S};

to_json(Req, #state{section=sys, sys=password}=S) ->
    {<<>>, Req, S};

to_json(Req, #state{section=sys, sys=community}=S) ->
    {jsx:encode(bkfw_config:get_kv(community), ?JSX_OPTS), Req, S};

to_json(Req, #state{section=sys, sys=protocol}=S) ->
    {jsx:encode(bkfw_config:get_kv(protocol), ?JSX_OPTS), Req, S};

to_json(Req, #state{section=sys, sys=firmware}=S) ->
    {jsx:encode(bkfw_config:get_kv(firmware), ?JSX_OPTS), Req, S}.


from_json(Req, #state{section=sys, sys=Cat}=S) ->
    case parse_body(Req) of
	{error, invalid_body, Req2} ->
	    {false, Req2, S};
	{error, Err, Req2} ->
	    ?error("Internal error: ~p~n", [Err]),
	    {halt, Req2, S};
	{ok, Json, Req2} ->
	    case bkfw_config:set_kv(Cat, Json) of
		ok ->
		    {true, Req2, S};
		{error, Err} ->
		    ?error("Request error: ~p~n", [Err]),
		    {false, Req2, S}
	    end
    end.

parse_body(Req) ->
    case cowboy_req:body(Req) of
	{ok, <<>>, Req2} ->
	    {error, invalid_body, Req2};
	{ok, Body, Req2} ->
	    try jsx:decode(Body, [{labels, attempt_atom}]) of
		Props when is_list(Props) ->
		    {ok, Props, Req2}
	    catch error:badarg ->
		    {error, invalid_body, Req2}
	    end;
	{error, Err} ->
	    {error, Err}
    end.
