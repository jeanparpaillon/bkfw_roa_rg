-module(bkfw_sup).

-behaviour(supervisor).

-include("bkfw.hrl").

%% API
-export([start_link/0,
		 post_http/0,
		 set_upgrade/1,
		 get_usbmode/0,
		 set_usbmode/1,
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
						  true;
					  ({_Id, undefined, _, _}) ->
						  true;
					  ({_Id, Child, _, _}) ->
						  try supervisor:restart_child(Child) catch _:_ -> exit(Child, kill) end
				  end, supervisor:which_children(?SRV)).

-spec set_upgrade(boolean()) -> ok | {error, term()}.
set_upgrade(false) ->
	?info("(re)starting monitoring loop", []),
	case supervisor:restart_child(?SRV, bkfw_edfa) of
		{ok, _} -> ok;
		{ok, _, _} -> ok;
		{error, _} = Err -> Err
	end;
set_upgrade(true) ->
	?info("Stopping monitoring loop", []),
	supervisor:terminate_child(?SRV, bkfw_edfa).		


-spec get_usbmode() -> boolean().
get_usbmode() ->
    F = fun F0([]) -> false;
			F0([{bkfw_usb, undefined, _, _} | _]) -> false;
			F0([{bkfw_usb, Pid, _, _ } | _]) when is_pid(Pid) -> true;
			F0([_ | Tail]) -> F0(Tail)
		end,
    F(supervisor:which_children(?SRV)).

-define(NON_USB_CHILDREN, [bkfw_mutex, bkfw_alarms, bkfw_srv, bkfw_edfa]).

-spec set_usbmode(boolean()) -> ok.
set_usbmode(false) ->
    case supervisor:terminate_child(?SRV, bkfw_usb) of
		ok -> 
			lists:foreach(fun (Id) -> 
								  supervisor:restart_child(?SRV, Id)  end, 
						  ?NON_USB_CHILDREN);
		{error, not_found} -> ok;
		{error, Err} -> throw(Err)
    end;

set_usbmode(true) ->
    lists:foreach(fun (Id) -> 
						  spawn(fun () -> supervisor:terminate_child(?SRV, Id) end) 
				  end,
				  ?NON_USB_CHILDREN),
	start_or_restart(?CHILD(bkfw_usb, worker)).

post_http() ->
	Spec = #{ id => bkfw_lcd,
			  start => {bkfw_lcd, start_link, []},
			  type => worker },
	case supervisor:start_child(?SRV, Spec) of
		{ok, _} ->
			ok;
		{error, _}=Err ->
			Err
	end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Children = [
				?CHILD(bkfw_config, worker),
				?CHILD(bkfw_mutex, worker),
				{bkfw_alarms, {gen_event, start_link, [{local, bkfw_alarms}]}, permanent, 5000, worker, [gen_event]},
				?CHILD(bkfw_srv, worker),
				?CHILD(bkfw_edfa, worker),
				bkfw_http:get_config()
			   ],
    {ok, { {one_for_one, 5, 10}, Children} }.

start_or_restart(Spec = {Id, _, _, _, _, _}) ->
    case supervisor:restart_child(?SRV, Id) of
		ok ->
			ok;
		{error, not_found} -> 
			case supervisor:start_child(?SRV, Spec) of
				{ok, _Pid} ->
					ok;
				{error, Err2} ->
					{error, Err2}
			end;
		{error, Err} -> 
			{error, Err}
    end.
