-module(bkfw_app).

-behaviour(application).

-include("bkfw.hrl").

%% Application callbacks
-export([start/2, 
		 stop/1,
		 restart/0,
		 reboot/0]).

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
    ok.


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
			      Dir = code:priv_dir(App) ++ "/mibs/",
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
