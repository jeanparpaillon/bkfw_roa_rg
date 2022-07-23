%%%
%%% Accepts and handles a client
%%% 
-module(bkfw_telnet_server).
-behaviour(gen_server).
-author('jean@parpaillon.info').

-include("bkfw.hrl").

%% Constants
-define(PROMPT, "edfa> ").
-define(QUIT, "quit").
-define(CRLF, "\r\n").
-define(CR, "\r").
-define(LF, "\n").
-define(TAB, "\t").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {lsock, socket, mode}).

%%%
%%% API
%%% 
start_link(Sock) ->
    gen_server:start_link(?MODULE, Sock, []).

%%%
%%% internals
%%% 
init(LSock) ->
    gen_server:cast(self(), accept),
    {ok, #state{lsock=LSock}}.

handle_call(_Call, _From, S) ->
    {reply, ok, S}.

handle_cast(accept, S = #state{lsock=LSock}) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, {ClientAddr, _}} = inet:peername(Sock),
    ?info("Telnet client from ~p", [inet:ntoa(ClientAddr)]),
    Mode = bkfw_sup:get_usbmode(),
    bkfw_sup:set_usbmode(true),
    send(Sock, ?PROMPT, []),
    {noreply, S#state{socket=Sock, mode=Mode}}.

handle_info({tcp, Sock, ?QUIT ++ _}, S) ->
    quit(Sock, S),
    {stop, normal, S};

handle_info({tcp, Sock, Str}, S) 
  when Str =:= ?TAB ->
    % Ignore
    refresh_socket(Sock),
    {noreply, S};

handle_info({tcp, Sock, Str}, S) 
  when Str =:= ?CRLF ; Str =:= ?CR ; Str =:= ?LF ->
    send(Sock, ?PROMPT, []),
    refresh_socket(Sock),
    {noreply, S};

handle_info({tcp, Sock, _Str}, S) ->
    process(Str, S),
    send(Sock, ?PROMPT, []),
    refresh_socket(Sock),
    {noreply, S};

handle_info({tcp_closed, _Sock}, S) ->
    {stop, normal, S};

handle_info({tcp_error, _Sock, _}, S) ->
    {stop, normal, S};

handle_info(E, S) ->
    ?debug("Unexpected: ~p", [E]),
    {noreply, S}.

terminate(_Reason, S) ->
    gen_tcp:close(S#state.socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% Internals
%%% 
quit(_Sock, S) ->
    ?info("Telnet client left", []),
    bkfw_sup:set_usbmode(S#state.mode),
    gen_tcp:close(S#state.socket).

send(Sock, Str, Args) ->
    ok = gen_tcp:send(Sock, io_lib:format(Str, Args)),
    refresh_socket(Sock),
    ok.

refresh_socket(Sock) ->
    ok = inet:setopts(Sock, [{active, once}]).

process(Str, S) ->
    bkfw_srv:call(fun (init, Com, _) ->
        bkfw_com:raw(Com, << Str/binary, $\r, $\n >>),
        ok
    end, undefined).