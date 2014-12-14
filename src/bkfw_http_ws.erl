-module(bkfw_http_ws).
-author('jean.parpaillon@free.fr').

-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-include("bkfw.hrl").

-record(state, {ref}).


-export([init/3, handle/2, terminate/3]).  
-export([websocket_init/3,
	 websocket_handle/3,
	 websocket_info/3, 
	 websocket_terminate/3]).  
  
init({tcp, http}, _Req, _Opts) ->  
  {upgrade, protocol, cowboy_websocket}.  


handle(Req, State) ->  
    {ok, Req2} = cowboy_req:reply(404, [{'Content-Type', <<"text/html">>}], Req),
    {ok, Req2, State}.  


websocket_init(_TransportName, Req, _Opts) ->  
    Ref = make_ref(),
    gen_event:add_handler(bkfw_alarms, {bkfw_alarms_http, Ref}, [self()]),
    {ok, Req, #state{ref=Ref}}.


websocket_handle(_Any, Req, State) ->  
    {reply, {text, <<"huh">>}, Req, State, hibernate}.  


websocket_info({alarm, Alarm}, Req, State) -> 
    {reply, {text, jsx:encode(Alarm)}, Req, State};  

websocket_info(_Info, Req, State) ->  
    {ok, Req, State, hibernate}.  


websocket_terminate(_Reason, _Req, _State) ->  
    ok.  


terminate(_Reason, _Req, #state{ref=Ref}) ->
    gen_event:delete_handler(bkfw_alarms, {bkfw_alarms_http, Ref}, []),
    ok.
