-module(bkfw_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Srv = ?CHILD(bkfw_srv, worker),
    McuSup = ?CHILD(bkfw_mcus_sup, supervisor),
    McuNumMon = ?CHILD(bkfw_num_mon, worker),
    {ok, { {one_for_one, 5, 10}, [Srv, McuSup, McuNumMon]} }.
