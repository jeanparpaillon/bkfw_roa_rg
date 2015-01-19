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
	 terminate_mcu/1]).

%%% supervisor callback
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).
-define(TID, ?MODULE).

%%%
%%% API
%%%
start_link() ->
    _ = ets:new(?TID, [named_table, public]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% Idx start at 1
start_mcu(Idx) ->
    case ets:lookup(?TID, Idx) of
	[] ->
	    case supervisor:start_child(?MODULE, [Idx]) of
		{ok, Child} ->
		    ets:insert(?TID, {Idx, Child}),
		    ok;
		{ok, Child, _Info} ->
		    ets:insert(?TID, {Idx, Child}),
		    ok;
		{error, Err} ->
		    {error, Err}
	    end;
	[{_Idx, _Pid}] -> ok
    end.

terminate_mcu(Idx) ->
    bkfw_srv:flush(Idx),
    case ets:lookup(?TID, Idx) of
	[] ->
	    ok;
	[{Idx, Pid}] ->
	    ?debug("Stop AMP monitor: ~p\n", [Idx]),
	    mnesia:dirty_delete(ampTable, Idx),
	    case supervisor:terminate_child(?MODULE, Pid) of
		ok ->
		    ets:delete(?TID, Idx),
		    ok;
		{error, _Err} -> 
		    ok
	    end
    end.


%%%
%%% Private
%%%
init([]) ->
    gen_event:add_handler(bkfw_alarms, bkfw_alarms_snmp, []),
    {ok, { {simple_one_for_one, 5, 10}, [?CHILD(bkfw_mcu, worker)]}}.
