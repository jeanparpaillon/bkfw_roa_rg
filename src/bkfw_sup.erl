-module(bkfw_sup).

-behaviour(supervisor).

-include("bkfw.hrl").

%% API
-export([start_link/0,
	 restart/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SRV, ?MODULE).
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SRV}, ?MODULE, []).

restart() ->
    lists:foreach(fun ({_Id, restarting, _, _}) ->
			  %?debug("Child ~p: restarting~n", [Id]);
			  true;
		      ({_Id, undefined, _, _}) ->
			  %?debug("Child ~p: undefined~n", [Id]);
			  true;
		      ({_Id, Child, _, _}) ->
			  %?debug("Child ~p: about to restart ~p~n", [Id, Child]),
			  try supervisor:restart_child(Child) catch _:_ -> exit(Child, kill) end
		  end, supervisor:which_children(?SRV)).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Config = ?CHILD(bkfw_config, worker),
    Mutex = ?CHILD(bkfw_mutex, worker),
    Srv = ?CHILD(bkfw_srv, worker),
    EdfaMon = ?CHILD(bkfw_edfa, worker),
    %EdfaLoop = {bkfw_edfa_loop, {bkfw_edfa, start_loop, []}, permanent, 5000, worker, []},
    McuSup = ?CHILD(bkfw_mcus_sup, supervisor),
    Http = bkfw_http:get_config(),
    Alarms = {bkfw_alarms, {gen_event, start_link, [{local, bkfw_alarms}]}, permanent, 5000, worker, [gen_event]},
    {ok, { {one_for_one, 5, 10}, [Config, Mutex, Alarms, Srv, EdfaMon, McuSup, Http]} }.
