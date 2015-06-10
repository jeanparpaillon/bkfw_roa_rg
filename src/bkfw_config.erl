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

-export([upgrade/2,
		 cmd/1,
		 script/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(USER_CONF, "/var/lib/bkfw/user.config").
-define(DEFAULT_NETCONF, [{type, static},
			  {ip, "10.0.0.3"},
			  {netmask, "255.0.0.0"},
			  {gateway, "10.0.0.1"}]).
-define(DEFAULT_COMMUNITY, [{public, <<"public">>},
			    {restricted, <<"private">>}]).

-define(SERVER, ?MODULE).
-type category() :: net | community | usm | protocol | firmware.
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
    gen_server:call(?SERVER, {set_kv, Cat, Props}, 10000).

-spec upgrade(string(), string()) -> ok | {error, term()}.
upgrade("fw", Filename) ->
	bkfw_fw:upgrade_fw(Filename);
upgrade("cpu", Filename) ->
    bkfw_fw:upgrade_cpu(Filename);
upgrade("amp", Filename) ->
    bkfw_fw:upgrade_amp(Filename).

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
		    {error, Err} ->
			?debug("Invalid net config: ~p~n", [Err]),
			case set_network_static(Iface, ?DEFAULT_NETCONF) of
			    {ok, Config} -> Config;
			    {error, Err} ->
				throw(Err)
			end
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
    Dir = get_snmp_configdir(),
    case get_snmp_com(Dir) of
	{error, Err} ->
	    {reply, {error, Err}, State};
	{ok, Com} ->
	    {reply, Com, State}
    end;

handle_call({get_kv, usm}, _From, State) ->    
    Dir = get_snmp_configdir(),
    case get_snmp_usm(Dir) of
	{error, Err} ->
	    {reply, {error, Err}, State};
	{ok, Usm} ->
	    {reply, Usm, State}
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

handle_call({get_kv, usb}, _From, State) ->
    Kv = [{enable, bkfw_sup:get_usbmode()}],
    {reply, Kv, State};

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
    case set_snmp_com(Dir, Props) of
	ok ->
	    {reply, ok, State};
	{error, Err} -> {reply, {error, Err}, State}
    end;

handle_call({set_kv, usm, Props}, _From, State) ->
    Dir = get_snmp_configdir(),
    case set_snmp_usm(Dir, Props) of
	ok -> {reply, ok, State};
	{error, Err} -> {reply, {error, Err}, State}
    end;

handle_call({set_kv, protocol, Props}, _From, State) ->
    AgentEnv = application:get_env(snmp, agent, []),
    Versions = lists:foldl(fun ({snmpv1, true}, Acc) -> [v1 | Acc];
			       ({snmpv2, true}, Acc) -> [v2 | Acc];
			       ({snmpv3, true}, Acc) -> [v3 | Acc];
			       (_, Acc) -> Acc
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
		    set_network_static(application:get_env(bkfw, netif, "eth0"),
				       ?DEFAULT_NETCONF),
		    set_snmp_com(get_snmp_configdir(), ?DEFAULT_COMMUNITY),
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
handle_call({set_kv, usb, Props}, _From, State) ->
    case proplists:get_value(enable, Props, undefined) of
		undefined -> {reply, ok, State};
		V when is_boolean(V) -> {reply, bkfw_sup:set_usbmode(V), State};
		_ -> {reply, {error, invalid_value}, State}
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
    filename:join([application:get_env(bkfw, scripts_dir, "priv/scripts"), Name]).

script(Cmd, Args) ->
    cmd(get_script(Cmd) ++ " " ++ Args).

cmd(Cmd) ->
    FullCmd = binary_to_list(iolist_to_binary(Cmd)),
    ?debug("execute: ~p~n", [FullCmd]),
    case os:cmd(FullCmd) of
	"ok\n" ->
	    ok;
	"err_" ++ Err ->
	    {error, clean(Err, $\n)};
	Else ->
	    {error, {unexpected, Else}}
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
	    [Addr, Mask, Gw] = case string:tokens(clean(Str, $\n), " ") of
				   [ReadAddr, ReadMask] -> 
				       [ReadAddr, ReadMask, proplists:get_value(gateway, ?DEFAULT_NETCONF)];
				   [ReadAddr, ReadMask, ReadGw] -> 
				       [ReadAddr, ReadMask, ReadGw];
				   _ -> 
				       [proplists:get_value(ip, ?DEFAULT_NETCONF),
					proplists:get_value(netmask, ?DEFAULT_NETCONF),
					proplists:get_value(gateway, ?DEFAULT_NETCONF)]
			       end,
	    case valid_ip(Addr, Mask, Gw) of
		{ok,  IpAddr, IpMask, IpGw}  ->
		    {ok, [{type, static},
			  {ip, list_to_binary(inet:ntoa(IpAddr))},
			  {netmask, list_to_binary(inet:ntoa(IpMask))},
			  {gateway, list_to_binary(inet:ntoa(IpGw))}]};
		{error, Err} ->
		    {error, Err}
	    end
    end.

-spec valid_ip(binary() | list(), binary() | list(), binary() | list()) -> 
		      {ok, inet:ip_address(), inet:ip_address(), inet:ip_address()}.
valid_ip(Addr, Mask, Gw) when is_binary(Addr) ->
    valid_ip(binary_to_list(Addr), Mask, Gw);
valid_ip(Addr, Mask, Gw) when is_binary(Mask) ->
    valid_ip(Addr, binary_to_list(Mask), Gw);
valid_ip(Addr, Mask, Gw) when is_binary(Gw) ->
    valid_ip(Addr, Mask, binary_to_list(Gw));
valid_ip(Addr, Mask, Gw) ->
    case inet:parse_address(Addr) of
	{ok, IpAddr} ->
	    case inet:parse_address(Mask) of
		{ok, IpMask} ->
		    case inet:parse_address(Gw) of
			{ok, IpGw} ->
			    {ok, IpAddr, IpMask, IpGw};
			{error, einval}  ->
			    {error, invalid_net_gw}
		    end;
		{error, einval} ->
		    {error, invalid_net_mask}
	    end;
	{error, einval} ->
	    {error, invalid_net_address}
    end.


set_network_dhcp(Iface)  when is_list(Iface) ->
    File = application:get_env(bkfw, net, ""),
    NewConfig = io_lib:format("auto lo~n"
			      ++ "iface lo inet loopback~n"
			      ++ "~n"
			      ++ "auto ~s~n"
			      ++ "iface ~s inet dhcp~n", 
			      [Iface, Iface]),
    case file:write_file(File, NewConfig) of
	ok ->
	    case apply_network(Iface) of
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
		  proplists:get_value(netmask, Props, ""),
		  proplists:get_value(gateway, Props, "")) of
	{ok, Ip, Mask, Gw} ->
	    File = application:get_env(bkfw, net, ""),
	    NewConfig = io_lib:format("auto lo~n"
				      ++ "iface lo inet loopback~n"
				      ++ "~n"
				      ++ "auto ~s~n"
				      ++ "iface ~s inet static~n"
				      ++ "\taddress ~s~n"
				      ++ "\tnetmask ~s~n"
				      ++ "\tgateway ~s~n",
				     [Iface, Iface, inet:ntoa(Ip), inet:ntoa(Mask), inet:ntoa(Gw)]),
	    case file:write_file(File, NewConfig) of
		ok ->
		    case apply_network(Iface) of
			ok ->
			    {ok, [{type, static},
				  {ip, list_to_binary(inet:ntoa(Ip))},
				  {netmask, list_to_binary(inet:ntoa(Mask))},
				  {gateway, list_to_binary(inet:ntoa(Gw))}]};
			{error, Err} ->
			    {error, Err}
		    end;
		{error, Err} ->
		    {error, Err}
	    end;
	{error, Err} ->
	    {error, Err}
    end.

apply_network(Iface) ->
    script("commit_network.sh", [Iface]).

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

get_snmp_com(Dir) ->
    case snmpa_conf:read_community_config(Dir) of
	{ok, Com} ->
	    {ok, [{public, list_to_binary(element(2, lists:keyfind("public", 1, Com)))},
		  {restricted, list_to_binary(element(2, lists:keyfind("private", 1, Com)))}
		 ]};
	{error, Err} -> {error, Err}
    end.

get_snmp_usm(Dir) ->
    case read_usm_config(Dir) of
	{ok, Conf} ->
	    Ret = [
		   {username, list_to_binary(element(2, Conf))},
		   {engine, get_snmp_engine_id(Dir)},
		   {level, get_usm_level(Conf)},
		   {auth, element(5, Conf)},
		   {authKey, <<>>},
		   {priv, element(8, Conf)},
		   {privKey, <<>>}
		  ],
	    {ok, Ret};
	{error, Err} -> {error, Err}
    end.

% USM entry:
% {EngineID, UserName, SecName, Clone, AuthP, AuthKeyC, OwnAuthKeyC, PrivP, PrivKeyC, OwnPrivKeyC, Public, AuthKey, PrivKey}
-define(SEC_NAME, "privateSec").
-define(SEC_PUBLIC, {"agent", "", "publicSec", zeroDotZero, 
		     usmNoAuthProtocol, "", "", 
		     usmNoPrivProtocol, "", "", "", "", ""}).

read_usm_config(Dir) ->
    case snmpa_conf:read_usm_config(Dir) of
	{ok, Conf} ->
	    case lists:keyfind(?SEC_NAME, 3, Conf) of
		false ->
		    {ok, {"agent", "admin", "privateSec", zeroDotZero, 
			  usmHMACMD5AuthProtocol, "", "",
			  usmNoPrivProtocol, "", "", "", "", ""}};
		PrivateConf -> {ok, PrivateConf}
	    end;
	{error, Err} ->
	    {error, Err}
    end.

set_snmp_com(Dir, Props) ->
    case snmpa_conf:read_community_config(Dir) of
	{ok, Com} ->
	    case get_communities(Props) of
		{ok, Pub, Priv} ->
		    Com2 = set_community("public", Pub, Com),
		    Com3 = set_community("private", Priv, Com2),
		    ok = snmpa_conf:write_community_config(Dir, Com3),
		    snmp_community_mib:reconfigure(Dir),
		    ok;
		{error, Err} -> {error, Err}
	    end;
	{error, Err} -> {error, Err}
    end.

set_snmp_usm(Dir, Props) ->
    EngineID = binary_to_list(proplists:get_value(engine, Props)),
    AuthP = binary_to_existing_atom(proplists:get_value(auth, Props), latin1),
    case get_usm_authkey(EngineID, AuthP, Props) of
	{ok, AuthKey} ->
	    PrivP = binary_to_existing_atom(proplists:get_value(priv, Props), latin1),
	    case get_usm_privkey(EngineID, Props) of
		{ok, PrivKey} ->
		    Conf = { EngineID,                                                      % EngineID
			     binary_to_list(proplists:get_value(username, Props)),          % UserName
			     ?SEC_NAME,                                                     % SecName
			     zeroDotZero,                                                   % Clone
			     AuthP,                                                         % AuthP
			     "",                                                            % AuthKeyC
			     "",                                                            % OwnAuthKeyC
			     PrivP,                                                         % PrivP
			     "",                                                            % PrivKeyC
			     "",                                                            % OwnPrivKeyC
			     "",                                                            % Public
			     AuthKey,                                                       % AuthKey
			     PrivKey                                                        % PrivKey
			   },
		    ok = snmpa_conf:write_usm_config(Dir, [?SEC_PUBLIC, Conf]),
		    snmp_user_based_sm_mib:reconfigure(Dir);
		{error, Err} -> {error, Err}
	    end;
	{error, Err} -> {error, Err}
    end.


get_usm_authkey(EngineID, usmHMACMD5AuthProtocol, Props) ->
    case binary_to_list(proplists:get_value(authKey, Props)) of
	[] -> {ok, []};
	Key when length(Key) >= 8 -> 
	    {ok, snmp:passwd2localized_key(md5, Key, EngineID)};
	_ ->
	    {error, invalid_snmp_authkey}
    end;
get_usm_authkey(EngineID, usmHMACSHAAuthProtocol, Props) ->
    case binary_to_list(proplists:get_value(authKey, Props)) of
	[] -> {ok, []};
	Key when length(Key) >= 8 ->
	    {ok, snmp:passwd2localized_key(sha, Key, EngineID)};
	_ ->
	    {error, invalid_snmp_authkey}
    end.


get_usm_privkey(EngineID, Props) ->
    case binary_to_list(proplists:get_value(privKey, Props)) of
	[] -> {ok, []};
	Key when length(Key) >= 8 ->
	    {ok, snmp:passwd2localized_key(md5, Key, EngineID)};
	_ ->
	    {error, invalid_snmp_privkey}
    end.

get_usm_level(Conf) ->
    case {element(5, Conf), element(8, Conf)} of
	{usmNoAuthProtocol, usmNoPrivProtocol} -> noauthnopriv;
	{_, usmNoPrivProtocol} -> authnopriv;
	{_, _} -> authpriv
    end.

get_snmp_engine_id(Dir) ->
    case snmpa_conf:read_agent_config(Dir) of
	{ok, Conf} -> list_to_binary(proplists:get_value(snmpEngineID, Conf, "agent"));
	{error, _} -> <<"agent">>
    end.
