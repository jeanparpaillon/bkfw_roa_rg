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
	 encode_password/1,
	 get_kv/1,
	 set_kv/2]).

-export([upgrade/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-type category() :: net | community | protocol | firmware.
-type net_opt() :: {type, static | dhcp} |
		   {ip, string()} |
		   {mask, string()} |
		   {gateway, string()}.
-type auth_opt() :: {password, binary()} |
		    {comm_public, binary()} |
		    {comm_restricted, binary()}.
-type protocol_opt() :: {snmpv1, boolean()} |
			{snmpv2, boolean()} |
			{snmpv3, boolean()}.
-type firmware_opt() :: {version, binary()}.
-record(state, {
	  net           :: [net_opt()],
	  auth          :: [auth_opt()],
	  protocol      :: [protocol_opt()],
	  firmware      :: [firmware_opt()]
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

-spec upgrade(string()) -> ok | {error, term()}.
upgrade(Filename) ->
    case script("check_pkg.sh", Filename) of
	ok ->
	    script("upgrade.sh", Filename);
	{error, Err} -> {error, Err}
    end.

-spec encode_password(iolist()) -> string().
encode_password(Passwd) when is_list(Passwd); is_binary(Passwd) ->
    base64:encode(hexstring(crypto:hash(md5, Passwd))).
    

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
    Iface = application:get_env(bkfw, netif, ""),
    NetConfig = case get_network_config(Iface) of
		    {ok, Config}  -> Config;
		    {error, _} ->
			set_network_dhcp(Iface),
			[{type, dhcp}]
		end,
    {ok, #state{net=NetConfig, firmware=load_resources()}}.

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
    Props = case proplists:get_value(type, Net) of
		static -> Net;
		dhcp ->
		    [ {type, dhcp} | get_if_infos(application:get_env(bkfw, net, "")) ]
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

handle_call({set_kv, net, Props}, _From, State) ->
    case proplists:get_value(type, Props) of
	undefined ->
	    {reply, {error, missing_net_type}, State};
	<<"dhcp">> ->
	    case set_network_dhcp(application:get_env_(bkfw, net, "")) of
		ok ->
		    {reply, ok, State};
		{error, Err} ->
		    {reply, {error, Err}, State}
	    end;
	<<"static">> ->
	    case set_network_static(application:get_env(bkfw, net, ""), Props) of
		ok ->
		    {reply, ok, State};
		{error, Err} ->
		    {reply, {error, Err}, State}
	    end
    end;
handle_call({set_kv, password, Props}, _From, State) ->
    ?debug("Setting password options: ~p~n", [Props]),
    {reply, ok, State};

handle_call({set_kv, community, Props}, _From, State) ->
    ?debug("Setting community options: ~p~n", [Props]),
    {reply, ok, State};

handle_call({set_kv, protocol, Props}, _From, State) ->
    ?debug("Setting protocol options: ~p~n", [Props]),
    {reply, ok, State};

handle_call({set_kv, reset, Props}, _From, State) ->
    ?debug("Reset user options: ~p~n", [Props]),
    {reply, ok, State};

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
get_if_infos(NetIf) ->
    Addr = case proplists:get_value(addr, NetIf) of
	       undefined -> <<>>;
	       T1 -> inet:ntoa(T1)
	   end,
    Mask = case proplists:get_value(netmask, NetIf) of
	       undefined -> <<>>;
	       T2 -> inet:ntoa(T2)
	   end,
    [{ip, Addr}, {mask, Mask}].

load_resources() ->
    case file:consult(filename:join(code:lib_dir(bkfw, ebin), "bkfw.app")) of
	{ok, [{application, bkfw, Props}]} -> Props;
	_ -> []
    end.

get_script(Name) ->
    filename:join([code:priv_dir(bkfw), "scripts", Name]).

script(Cmd, Args) ->
    cmd(get_script(Cmd) ++ " " ++ Args).

cmd(Cmd) ->
    case application:get_env(bkfw, system_cmd) of
	{ok, true} -> 
	    case os:cmd(binary_to_list(iolist_to_binary(Cmd))) of
		"ok\n" ->
		    ok;
		"err_" ++ Err ->
		    {error, Err};
		Else ->
		    {error, {unexpected, Else}}
	    end;
	{ok, false} -> 
	    ?info("(fake) running: ~s~n", [iolist_to_binary(Cmd)]),
	    ok;
	undefined -> 
	    ?info("(fake) running: ~s~n", [iolist_to_binary(Cmd)]),
	    ok
    end.

hexstring(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X])).

%% Network related functions
-spec get_network_config(string()) -> {ok, [net_opt()]} | {error, term()}.
get_network_config(Iface) when is_list(Iface) ->
    Cmd = get_script("readInterfaces.awk ") 
	++ application:get_env(bkfw, net, "")
	++ " device=" ++ Iface
	++ Iface,
    case os:cmd(Cmd) of
	"dhcp\n" -> {ok, [{type, dhcp}]};
	Str ->
	    case string:tokens(Str, " ") of
		[Ip, Mask] ->
		    case valid_ip(Ip, Mask) of
			{ok, _, _}  ->
			    {ok, [{mode, static},
				  {ip, Ip},
				  {mask, Mask}]};
			{error, Err} ->
			    {error, Err}
		    end;
		_ -> {error, invalid_net_config}
	    end
    end.

-spec valid_ip(list(), list()) -> {ok, inet:ip_address(), inet:ip_address()}.
valid_ip(Addr, Mask) ->
    case inet:parse_address(Addr) of
	{ok, IpAddr} ->
	    case inet:parse_address(Mask) of
		{ok, IpMask} ->
		    {ok, IpAddr, IpMask};
		{error, einval} ->
		    {error, invalid_net_mask}
	    end;
	{error, einval} ->
	    {error, invalid_net_address}
    end.


set_network_dhcp(Iface)  when is_list(Iface) ->
    ok.

set_network_static(Iface, Props) when is_list(Iface), is_list(Props) ->
    ok.
