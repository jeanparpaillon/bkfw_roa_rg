%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 23 May 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(bkfw_lcd).

-behaviour(gen_server).

-include("bkfw.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
	?info("Start LCD controller", []),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%%
%%% gen_server callbacks
%%%
init([]) ->
	process_flag(trap_exit, true),
	RoaPath = os:find_executable("roa.py"),
	HttpOpts = application:get_env(bkfw, http, [{port, 80}]),
	Baseurl = iolist_to_binary(io_lib:format("http://localhost:~b", [proplists:get_value(port, HttpOpts, 80)])),
	Port = open_port({spawn_executable, RoaPath}, [{args, [Baseurl]}, exit_status]),
	{ok, Port}.


handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.


handle_cast(_Msg, State) ->
	{noreply, State}.


handle_info({Port, {exit_status, _}}, Port) ->
	{stop, exit, undefined};

handle_info(_Info, State) ->
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%
%%% Priv
%%%
