%%%
%%% Handles listener socket and launch (1) server
%%%
-module(bkfw_telnet).
-behaviour(supervisor).
-author('jean@parpaillon.info').

-include("bkfw.hrl").

%% API
-export([start_link/0, stop/0]).

%% supervisor callback
-export([init/1]).

-define(SERVER, bkfw_telnet_server).

%%%
%%% API
%%%
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    {ok, #{start := {_, _, [ListenSock]}}} = supervisor:get_childspec(?MODULE, ?SERVER),
    gen_tcp:close(ListenSock).

%%%
%%% gen_server callbacks
%%%
init([]) ->
    Port = application:get_env(bkfw, telnet_port, 23),
    ?info("Starting Telnet server on port ~p", [Port]),
    {ok, ListenSock} = gen_tcp:listen(Port, [{active, once}, {packet, line}]),
    Flags = #{},
    Spec = #{id => ?SERVER, start => {?SERVER, start_link, [ListenSock]}},
    {ok, {Flags, [Spec]}}.
