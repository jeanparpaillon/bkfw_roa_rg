%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, BKtel Photonics
%%% @doc
%%%
%%% @end
%%% Created : 25 Nov 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(bkfw_config).

-include("bkfw.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,
	 get_kv/1,
	 set_kv/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-type category() :: net | community | protocol | firmware.
-type net_opt() :: {type, static | dhcp} |
		   {ifname, string()} |
		   {ip, binary()} |
		   {netmask, binary()} |
		   {gateway, binary()}.
-type auth_opt() :: {password, binary()} |
		    {comm_public, binary()} |
		    {comm_restricted, binary()}.
-type protocol_opt() :: {snmpv1, boolean()} |
			{snmpv2, boolean()} |
			{snmpv3, boolean()}.
-type firmware_opt() :: {version, binary()}.
-type session_opt() :: #session{}.
-record(state, {
	  net           :: [net_opt()],
	  auth          :: [auth_opt()],
	  protocol      :: [protocol_opt()],
	  firmware      :: [firmware_opt()],
	  session       :: session_opt()
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get_kv(category()) -> list().
get_kv(Cat) ->
    gen_server:call(?SERVER, {get_kv, Cat}).

-spec set_kv(category(), term()) -> ok | {error, term()}.
set_kv(Cat, Props) ->
    gen_server:call(?SERVER, {set_kv, Cat, Props}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{net=application:get_env(bkfw, net, []),
		firmware=load_resources(),
		session=undefined}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_kv, net}, _From, #state{net=Net}=State) ->
    IfName = proplists:get_value(ifname, Net),
    Props = case proplists:get_value(type, Net) of
		static ->
		    [
		     {type, static},
		     {ip, proplists:get_value(ip, Net)},
		     {netmask, proplists:get_value(netmask, Net)},
		     {gateway, proplists:get_value(gateway, Net)}
		    ];
		dhcp ->
		    P = [{type, dhcp}],
		    case get_if(IfName) of
			undefined -> P;		    
			NetIf -> get_if_infos(NetIf) ++ P
		    end
	    end,
    {reply, Props, State};

handle_call({get_kv, community}, _From, State) ->
    {reply, [
	     {public, <<"public">>},
	     {restricted, <<"private">>}
	    ], State};

handle_call({get_kv, protocol}, _From, State) ->
    {reply, [
	     {snmpv1, true},
	     {snmpv2, false},
	     {snmpv3, false}
	    ], State};

handle_call({get_kv, firmware}, _From, #state{firmware=FW}=S) ->
    {reply, [
	     {id, list_to_binary(proplists:get_value(description, FW, ""))},
	     {version, list_to_binary(proplists:get_value(vsn, FW, ""))}
	    ], S};

handle_call({get_kv, login}, _From, #state{session=#session{user=User}}=S) ->
    {reply, [{user, User}], S};

handle_call({get_kv, login}, _From, #state{session=undefined}=S) ->
    {reply, [], S};

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_if(Name) ->
    case inet:getifaddrs() of
	{ok, List} ->
	    filter_if(Name, List);
	{error, Err} ->
	    ?error("Error getting network interfaces: ~p~n", [Err]),
	    undefined
    end.

filter_if(_, []) ->
    undefined;
filter_if(Name, [{Name, Props} | _]) ->
    Props;
filter_if(Name, [{_, _} | Tail]) ->
    filter_if(Name, Tail).

get_if_infos(NetIf) ->
    Addr = case proplists:get_value(addr, NetIf) of
	       undefined -> <<>>;
	       T1 -> list_to_binary(inet:ntoa(T1))
	   end,
    Mask = case proplists:get_value(netmask, NetIf) of
	       undefined -> <<>>;
	       T2 -> list_to_binary(inet:ntoa(T2))
	   end,
    [{ip, Addr}, {netmask, Mask}].

load_resources() ->
    case file:consult(filename:join(code:lib_dir(bkfw, ebin), "bkfw.app")) of
	{ok, [{application, bkfw, Props}]} -> Props;
	_ -> []
    end.
