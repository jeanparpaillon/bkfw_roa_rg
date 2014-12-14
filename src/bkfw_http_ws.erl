-module(bkfw_http_ws).
-author('jean.parpaillon@free.fr').

-include("bkfw.hrl").

-record(state, {ref}).

-export([init/4, stream/3, info/3, terminate/2]).

init(_Transport, Req, _Opts, _Active) ->
    Ref = make_ref(),
    gen_event:add_handler(bkfw_alarms, {bkfw_alarms_http, Ref}, [self()]),
    {ok, Req, #state{ref=Ref}}.

stream(Data, Req, State) ->
    {reply, Data, Req, State}.

info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Req, #state{ref=Ref}) ->
    gen_event:delete_handler(bkfw_alarms, {bkfw_alarms_http, Ref}, []),
    ok.
