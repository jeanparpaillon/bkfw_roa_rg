-module(bkfw_app).

-behaviour(application).

-include("bkfw.hrl").
-include("EDFA-MIB.hrl").
%-include_lib("snmp/include/snmp_types.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    init_db(),
    application:start(ranch),
    application:start(crypto),
    application:start(cowlib),
    application:start(cowboy),
    application:start(bullet),
    application:start(snmp),
    load_mibs(snmp, ["SNMP-NOTIFICATION-MIB", "SNMP-TARGET-MIB"]),
    load_mibs(bkfw, ["BKTEL-PHOTONICS-SMI", "EDFA-MIB"]),
    bkfw_sup:start_link().

stop(_State) ->
    ok.


%%%
%%% Priv
%%%
init_db() ->
     application:start(mnesia),
     {atomic, ok} = mnesia:create_table(edfaMcuTable, 
					[{snmp, [{key, integer}]},
					 {attributes, record_info(fields, edfaMcuTable)}]).

load_mibs(App, Mibs) ->
    Paths = lists:map(fun (Path) ->
			      ?info("Loading MIB: ~p~n", [Path]),
			      Dir = code:priv_dir(App) ++ "/mibs/",
			      Dir ++ Path
		      end, Mibs),
    ok = snmpa:load_mibs(snmp_master_agent, Paths).
