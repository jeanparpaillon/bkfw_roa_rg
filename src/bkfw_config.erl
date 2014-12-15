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
	 set_app_conf/4,
	 get_kv/1,
	 set_kv/2]).

-export([upgrade/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(USER_CONF, "/var/lib/bkfw/user.config").

-define(SERVER, ?MODULE).
-type category() :: net | community | protocol | firmware.
-type net_opt() :: {type, static | dhcp} |
		   {ip, string()} |
		   {netmask, string()} |
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
	    script("upgrade.sh", Filename),
	    bkfw_app:reboot(),
	    ok;
	{error, Err} -> {error, Err}
    end.

-spec encode_password(iolist()) -> string().
encode_password(Passwd) when is_list(Passwd); is_binary(Passwd) ->
    binary_to_list(base64:encode(hexstring(crypto:hash(md5, Passwd)))).
    

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
			set_network_dhcp(Iface)
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
    case snmpa_conf:read_community_config(get_snmp_configdir()) of
	{ok, Com} ->
	    Ret = [{public, list_to_binary(element(2, lists:keyfind("public", 1, Com)))},
		   {restricted, list_to_binary(element(2, lists:keyfind("private", 1, Com)))}],
	    {reply, Ret, State};
	{error, Err} ->
	    {reply, {error, Err}, State}
    end;

handle_call({get_kv, protocol}, _From, State) ->
    Protocols = proplists:get_value(versions, application:get_env(snmp, agent, []), []),
    Kv = [{snmpv1, proplists:get_bool(v1, Protocols)},
	  {snmpv2, proplists:get_bool(v2, Protocols)},
	  {snmpv3, proplists:get_bool(v3, Protocols)}],	    
    {reply, Kv, State};

handle_call({get_kv, targets}, _From, State) ->
    case snmpa_conf:read_target_addr_config(get_snmp_configdir()) of
	{ok, Addrs} ->
	    Ret = [{target1, get_target_addr("target1_v1", Addrs)},
		   {target2, get_target_addr("target2_v1", Addrs)},
		   {target3, get_target_addr("target3_v1", Addrs)}],
	    {reply, Ret, State};
	{error, Err} ->
	    {reply, {error, Err}, State}
    end;

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
	    case set_network_dhcp(application:get_env(bkfw, netif, "")) of
		{ok, Conf} ->
		    {reply, ok, State#state{net=Conf}};
		{error, Err} ->
		    {reply, {error, Err}, State}
	    end;
	<<"static">> ->
	    case set_network_static(application:get_env(bkfw, netif, ""), Props) of
		{ok, Conf} ->
		    {reply, ok, State#state{net=Conf}};
		{error, Err} ->
		    {reply, {error, Err}, State}
	    end
    end;
handle_call({set_kv, password, Props}, _From, State) ->
    case proplists:get_value(password, Props) of
	undefined ->
	    {reply, {error, empty_password}, State};
	P ->
	    Ret = save_user_config(bkfw, password, {md5, encode_password(P)}),
	    {reply, Ret, State}
    end;
handle_call({set_kv, community, Props}, _From, State) ->
    Dir = get_snmp_configdir(),
    case snmpa_conf:read_community_config(Dir) of
	{ok, Com} ->
	    case get_communities(Props) of
		{ok, Pub, Priv} ->
		    Com2 = set_community("public", Pub, Com),
		    Com3 = set_community("private", Priv, Com2),
		    ok = snmpa_conf:write_community_config(Dir, Com3),
		    snmp_community_mib:reconfigure(Dir),
		    {reply, ok, State};
		{error, Err} ->
		    {reply, {error, Err}, State}
	    end;
	{error, Err} ->
	    {reply, {error, Err}, State}
    end;

handle_call({set_kv, protocol, Props}, _From, State) ->
    AgentEnv = application:get_env(snmp, agent, []),
    Versions = lists:foldl(fun ({snmpv1, true}, Acc) ->
				   [v1 | Acc];
			       ({snmpv2, true}, Acc) ->
				   [v2 | Acc];
			       ({snmpv3, true}, Acc) ->
				   [v3 | Acc];
			       (_, Acc) ->
				   Acc
			   end, [], Props),
    Ret = save_user_config(snmp, agent, lists:keystore(versions, 1, AgentEnv, {versions, Versions})),
    bkfw_app:restart(),
    {reply, Ret, State};

handle_call({set_kv, targets, Props}, _From, State) ->
    Dir = get_snmp_configdir(),
    case get_targets_conf(Props, []) of
	{ok, Conf} ->
	    ok = snmpa_conf:write_target_addr_config(Dir, Conf),
	    snmp_target_mib:reconfigure(Dir),
	    {reply, ok, State};
	{error, Err} ->
	    {reply, {error, Err}, State}
    end;

handle_call({set_kv, reset, Props}, _From, State) ->
    case proplists:get_value(reset, Props, false) of
	true ->
	    case file:write_file(?USER_CONF, <<"[].">>) of
		ok ->
		    set_network_dhcp(application:get_env(bkfw, netif, "eth0")),
		    bkfw_app:restart(),
		    {reply, ok, State};
		{error, eacces} ->
		    % For debugging purpose...
		    bkfw_app:restart(),
		    {reply, ok, State};
		{error, Err} ->
		    {reply, {error, Err}, State}
	    end;
	false ->
	    {reply, ok, State}
    end;
handle_call({set_kv, reboot, Props}, _From, State) ->
    case proplists:get_value(reboot, Props, false) of
	true ->
	    bkfw_app:reboot(),
	    {reply, ok, State};
	false ->
	    {reply, ok, State}
    end;
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
		    {error, clean(Err, $\n)};
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

save_user_config(App, Name, Value) ->
    case file:consult(?USER_CONF) of
	{ok, [Conf]} ->
	    Conf2 = set_app_conf(App, Name, Value, Conf),
	    file:write_file(?USER_CONF, [io_lib:print(Conf2), ".\n"]);
	{error, Err} ->
	    {error, Err}
    end.

set_app_conf(App, Name, Value, Conf) ->
    application:set_env(App, Name, Value),
    AppConf = proplists:get_value(App, Conf, []),
    lists:keystore(App, 1, Conf, 
		   {App, lists:keystore(Name, 1, AppConf, {Name, Value})}).


%% Network related functions
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

-spec get_network_config(string()) -> {ok, [net_opt()]} | {error, term()}.
get_network_config(Iface) when is_list(Iface) ->
    Cmd = get_script("readInterfaces.awk ") 
	++ application:get_env(bkfw, net, "")
	++ " device=" ++ Iface,
    case os:cmd(Cmd) of
	"dhcp\n" -> {ok, [{type, dhcp}]};
	Str ->
	    case string:tokens(clean(Str, $\n), " ") of
		[Addr, Mask] ->
		    case valid_ip(Addr, Mask) of
			{ok,  IpAddr, IpMask}  ->
			    {ok, [{type, static},
				  {ip, list_to_binary(inet:ntoa(IpAddr))},
				  {netmask, list_to_binary(inet:ntoa(IpMask))}]};
			{error, Err} ->
			    {error, Err}
		    end;
		_ -> {error, invalid_net_config}
	    end
    end.

-spec valid_ip(binary() | list(), binary() | list()) -> {ok, inet:ip_address(), inet:ip_address()}.
valid_ip(Addr, Mask) when is_binary(Addr) ->
    valid_ip(binary_to_list(Addr), Mask);
valid_ip(Addr, Mask) when is_binary(Mask) ->
    valid_ip(Addr, binary_to_list(Mask));
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
    File = application:get_env(bkfw, net, ""),
    Cmd = get_script("changeInterface.awk")
	++ "  " ++ File
	++ " device=" ++ Iface
	++ " mode=dhcp",
    NewConfig = os:cmd(Cmd), 
    case file:write_file(File, NewConfig) of
	ok ->
	    case apply_network() of
		ok ->
		    {ok, [{type, dhcp}]};
		{error, Err} ->
		    {error, Err}
	    end;
	{error, Err} ->
	    {error, Err}
    end.

set_network_static(Iface, Props) when is_list(Iface), is_list(Props) ->
    case valid_ip(proplists:get_value(ip, Props, ""),
		  proplists:get_value(netmask, Props, "")) of
	{ok, Ip, Mask} ->
	    File = application:get_env(bkfw, net, ""),
	    Cmd = get_script("changeInterface.awk")
		++ " " ++  File
		++ " device=" ++ Iface
		++ " mode=static"
		++ " address=" ++ inet:ntoa(Ip)
		++ " netmask=" ++ inet:ntoa(Mask),
	    NewConfig = os:cmd(Cmd),
	    case file:write_file(File, NewConfig) of
		ok ->
		    case apply_network() of
			ok ->
			    {ok, [{type, static},
				  {ip, list_to_binary(inet:ntoa(Ip))},
				  {netmask, list_to_binary(inet:ntoa(Mask))}]};
			{error, Err} ->
			    {error, Err}
		    end;
		{error, Err} ->
		    {error, Err}
	    end;
	{error, Err} ->
	    {error, Err}
    end.

apply_network() ->
    script("commit_network.sh", []).

clean(Text, Char) ->
    string:strip(string:strip(Text, right, Char), left, Char).

get_snmp_configdir() ->
    Props = application:get_env(snmp, agent, []),
    [{dir, Dir}] = proplists:get_value(config, Props, [{dir, ""}]),
    Dir.

get_communities(Props) ->
    case proplists:get_value(public, Props) of
	undefined ->
	    {error, invalid_community};
	Pub ->
	    case proplists:get_value(restricted, Props) of
		undefined ->
		    {error, invalid_community};
		Priv ->
		    {ok, binary_to_list(Pub), binary_to_list(Priv)}
	    end
    end.

set_community(Index, Name, Conf) ->
    Com = lists:keyfind(Index, 1, Conf),
    lists:keystore(Index, 1, Conf, setelement(2, Com, Name)).


get_target_addr(_Name, []) ->
    <<>>;
get_target_addr(Name, [{Name, _, Addr, Port, _, _, _, _, _, _, _} | _]) ->
    iolist_to_binary([inet:ntoa(list_to_tuple(Addr)), $:, io_lib:format("~b", [Port])]);
get_target_addr(Name, [ _ | Tail ]) ->
    get_target_addr(Name, Tail).


get_targets_conf([], Acc) ->
    {ok, Acc};
get_targets_conf([{Prefix, Addr} | Tail], Acc) ->
    case parse_target(Addr) of
	{ok, Ip, Port} ->
	    get_targets_conf(Tail, add_targets(atom_to_list(Prefix), Ip, Port, Acc));
	undefined ->
	    get_targets_conf(Tail, Acc);
	{error, Err} ->
	    {error, Err}
    end.

add_targets(Prefix, Ip, Port, Acc) ->
    [ 
      {Prefix ++ "_v1", Ip, Port, 1500, 3, "bkfw", "target_v1", "bkfw_master_agent", [], 2048},
      {Prefix ++ "_v2", Ip, Port, 1500, 3, "bkfw", "target_v2", "bkfw_master_agent", [], 2048},
      {Prefix ++ "_v3", Ip, Port, 1500, 3, "bkfw", "target_v3", "bkfw_master_agent", [], 2048}
      | Acc].

parse_target(<<>>) ->
    undefined;
parse_target(Addr) ->
    case binary:split(Addr, [<<":">>]) of
	[BinIP, BinPort] ->
	    parse_ip_port(BinIP, BinPort);
	[BinIP] ->
	    parse_ip_port(BinIP, <<"162">>)
    end.

parse_ip_port(BinIP, BinPort) ->
    case inet:parse_address(binary_to_list(BinIP)) of
	{ok, IP} ->
	    try binary_to_integer(BinPort) of
		Port ->
		    {ok, tuple_to_list(IP), Port}
	    catch 
		error:badarg ->
		    {error, invalid_target}
	    end;
	{error, einval} ->
	    {error, invalid_target}
    end.
