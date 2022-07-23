-module(bkfw).

-behaviour(application).

-include("bkfw.hrl").

-export([start/0]).

%% Application callbacks
-export([start/2, 
		 start_phase/3,
		 stop/1,
		 restart/0,
		 reboot/0]).

start() ->
	_ = application:ensure_all_started(bkfw).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	application:load(bkfw),
    init_db(),
    load_mibs(snmp, ["SNMP-NOTIFICATION-MIB", "SNMP-TARGET-MIB"]),
    load_mibs(bkfw, ["BKTEL-PHOTONICS-SMI", "SMM-MIB"]),
    bkfw_sup:start_link().


stop(_State) ->
	% Stop telnet listening socket
	bkfw_telnet:stop(),
    ok.


start_phase(http, normal, _Args) ->
	%% Wait for HTTP API to be ready
	timer:sleep(1000),
	bkfw_sup:post_http();

start_phase(_, _, _) ->
	{error, bas_phase}.

%%%
%%% Priv
%%%
init_db() ->
	{atomic, ok} = mnesia:create_table(ampTable, 
									   [{snmp, [{key, integer}]},
										{attributes, record_info(fields, ampTable)}]).


load_mibs(App, Mibs) ->
    Paths = lists:map(fun (Path) ->
							  ?info("Loading MIB: ~p", [Path]),
							  Dir = priv_dir(App) ++ "/mibs/",
							  Dir ++ Path
					  end, Mibs),
    ok = snmpa:load_mibs(snmp_master_agent, Paths).


restart() ->
    spawn(fun() ->
				  timer:sleep(1000),
				  init:restart()
		  end).


reboot() ->
    spawn(fun() ->
				  timer:sleep(1000),
				  bkfw_config:script("restart.sh", [])
		  end).

priv_dir(bkfw) ->
	case code:lib_dir(bkfw, priv) of
		{error, bad_name} ->
			filename:absname("priv");

		Path ->
			Path
	end;

priv_dir(App) ->
	code:lib_dir(App, priv).