-module(bkfw_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(PORT, 8080).

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
    Http = http_config(),
    {ok, { {one_for_one, 5, 10}, [Srv, McuSup, McuNumMon, Http]} }.

http_config() ->
    Opts = application:get_env(bkfw, http, []),
    Dir = filename:join(code:priv_dir(bkfw), "www"),
    Handlers = [
		{"/[...]", cowboy_static, {dir, Dir, [{mimetypes, cow_mimetypes, all}]}}
	       ],
    Args = [http, 1, 
	    [{port, proplists:get_value(port, Opts, ?PORT)}],
	    [{env, [{dispatch, cowboy_router:compile([{'_', Handlers}])}]},
	     {middlewares, [bkfw_index, cowboy_router, cowboy_handler]}]
	   ],
    {http, {cowboy, start_http, Args}, permanent, 5000, worker, [cowboy]}.
