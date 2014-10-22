-module(bkfw_app).

-behaviour(application).

-include("bkfw.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(snmp),
    load_mibs(["BKTEL-SMI", "EDFA-MIB"]),
    bkfw_sup:start_link().

stop(_State) ->
    ok.


%%%
%%% Priv
%%%
load_mibs(Mibs) ->
    Paths = lists:map(fun (Path) ->
			      ?info("Loading MIB: ~p~n", [Path]),
			      Dir = code:priv_dir(bkfw) ++ "/mibs/",
			      Dir ++ Path
		      end, Mibs),
    snmpa:load_mibs(snmp_master_agent, Paths).
