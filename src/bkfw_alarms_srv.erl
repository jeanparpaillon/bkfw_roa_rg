%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 29 Jun 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(bkfw_alarms_srv).

-include("bkfw.hrl").

-behaviour(gen_server).


-export([start_link/0,
		 get/1,
		 set/2]).

%% priv
-export([expires/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(EXPIRE, 5000).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get(Idx) ->
	gen_server:call(?MODULE, {get, Idx}).


set(Idx, Name) ->
	gen_server:call(?MODULE, {set, Idx, Name}).


expires(Idx, Name) ->
	gen_server:call(?MODULE, {expires, Idx, Name}).


%%%
%%% Callbacks
%%%
init([]) ->
	?debug("Start alarm server", []),
	{ok, #{}}.


handle_call({expires, Idx, Name}, _From, S) ->
	?debug("Expires alarm ~s (#~p)", [Name, Idx]),
	Alarms = lists:filter(fun ({AlarmName, _}) -> 
								  case AlarmName of
									  Name -> false;
									  _ -> true
								  end
						 end, maps:get(Idx, S, [])),
	{reply, ok, S#{ Idx => Alarms }};

handle_call({get, Idx}, _From, S) ->
	Alarms = lists:map(fun ({Name, _}) -> Name end, maps:get(Idx, S, [])),
	{reply, Alarms, S};

handle_call({set, Idx, Name}, _From, S) ->
	?debug("Add alarm ~s (#~p)", [Name, Idx]),
	Alarms0 = maps:get(Idx, S, []),
	case lists:keyfind(Name, 1, Alarms0) of
		false -> ok;
		{_, T0} -> timer:cancel(T0)
	end,
	case timer:apply_after(?EXPIRE, ?MODULE, expires, [Idx, Name]) of
		{ok, TRef} ->
			Alarms = lists:keystore(Name, 1, Alarms0, {Name, TRef}),
			{reply, ok, S#{ Idx => Alarms }};
		{error, Err} ->
			?error("Error setting alarms timeout: ~p", [Err]),
			{reply, ok, S}
	end.


handle_cast(_Cast, State) ->
	{noreply, State}.


handle_info(_Info, State) ->
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
