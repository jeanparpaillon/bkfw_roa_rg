-module(bkfw_http_edfa).

-include("bkfw.hrl").

-export([
	 init/3,
	 rest_init/2,
	 allowed_methods/2,
	 content_types_accepted/2,
	 content_types_provided/2,
	 resource_exists/2,
	 allow_missing_post/2,
	 to_json/2
	]).

-record(state, {
	  edfa = undefined
	 }).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, #state{}}.

allowed_methods(Req, State) ->
    {[<<"GET">>,
      <<"HEAD">>,
      <<"POST">>,
      <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, from_json}
     ], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, to_json}
     ], Req, State}.

resource_exists(Req, State) ->
    case cowboy_req:binding(index, Req) of
	{undefined, Req2} ->
	    {true, Req2, State};
	{BinI, Req2} ->
	    ?debug("binding index: ~p~n", [BinI]),
	    try binary_to_integer(BinI) of
		I ->
		    case mnesia:dirty_match_object(#edfaMcuTable{index=I, _='_'}) of
			[] ->
			    {false, Req2, State};
			[Edfa] ->
			    {true, Req2, State#state{edfa=Edfa}}
		    end
	    catch error:badarg ->
		    {false, Req2, State}
	    end
    end.

allow_missing_post(Req, State) ->
    {false, Req, State}.

to_json(Req, #state{edfa=undefined}=S) ->
    Edfas = mnesia:dirty_match_object(#edfaMcuTable{_='_'}),
    ?debug("EDFAS: ~p~n", [Edfas]),
    {<<"{}">>, Req, S};

to_json(Req, #state{edfa=Edfa}=S) ->
    ?debug("EDFA: ~p~n", [Edfa]),
    {<<"">>, Req, S}.

