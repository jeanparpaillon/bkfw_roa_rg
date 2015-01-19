%%%
%%% Monitor MCU number and start an instance of bkfw_mon per MCU
%%%
-module(bkfw_mcus_sup).
-author('jean.parpaillon@free.fr').

-include("bkfw.hrl").

-behaviour(supervisor).

%%%
%%% API
%%%
-export([start_link/0,
	 start_mcu/1,
	 terminate_mcu/2]).

%%% supervisor callback
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

%%%
%%% API
%%%
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% Idx start at 1
start_mcu(Idx) ->
    bkfw_srv:flush(),
    supervisor:start_child(?MODULE, [Idx]).

terminate_mcu(Idx, Pid) ->
    ?debug("Stop AMP monitor: ~p\n", [Idx]),
    bkfw_srv:flush(),
    ok = supervisor:terminate_child(?MODULE, Pid).

%%%
%%% Private
%%%
init([]) ->
    gen_event:add_handler(bkfw_alarms, bkfw_alarms_snmp, []),
    {ok, { {simple_one_for_one, 5, 10}, [?CHILD(bkfw_mcu, worker)]}}.
