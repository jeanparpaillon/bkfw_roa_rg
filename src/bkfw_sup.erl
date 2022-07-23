-module(bkfw_sup).

-behaviour(supervisor).

-include("bkfw.hrl").

%% API
-export([start_link/0,
		 post_http/0,
		 get_usbmode/0,
		 set_usbmode/1]).

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


-spec get_usbmode() -> boolean().
get_usbmode() ->
	bkfw_edfa:enabled().

-spec set_usbmode(boolean()) -> ok.
set_usbmode(Mode) ->
	bkfw_edfa:enable(not Mode).

post_http() ->
	case bkfw_lcd:enabled() of
		true ->
			Spec = #{ id => bkfw_lcd,
					  start => {bkfw_lcd, start_link, []},
					  type => worker },
			start_children([Spec]);
		false ->
			?info("LCD control disabled", []),
			ok
	end.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
	Children = [
				#{ id => bkfw_config,
				   start => {bkfw_config, start_link, []},
				   type => worker
				 },
				#{ id => bkfw_com,
				   start => {bkfw_com, start_link, []},
				   type => worker },
				   				{bkfw_alarms, {gen_event, start_link, [{local, bkfw_alarms}]}, permanent, 5000, worker, [gen_event]},
				?CHILD(bkfw_edfa, worker),
				?CHILD(bkfw_usb, worker),
				?CHILD(bkfw_telnet, supervisor),
				?CHILD(bkfw_alarms_srv, worker),
				bkfw_http:get_config()
			   ],
	{ok, { {one_for_one, 5, 10}, Children} }.


start_children([]) ->
	ok;

start_children([ Spec | Children ]) ->
	case supervisor:start_child(?SRV, Spec) of
		{ok, _} ->
			start_children(Children);
		{error, _}=Err ->
			Err
	end.
	

%% start_or_restart(Spec = {Id, _, _, _, _, _}) ->
%%     case supervisor:restart_child(?SRV, Id) of
%% 		ok ->
%% 			ok;
%% 		{error, not_found} -> 
%% 			case supervisor:start_child(?SRV, Spec) of
%% 				{ok, _Pid} ->
%% 					ok;
%% 				{error, Err2} ->
%% 					{error, Err2}
%% 			end;
%% 		{error, Err} -> 
%% 			{error, Err}
%%     end.
