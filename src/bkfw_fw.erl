%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, BKtel Photonics
%%% @doc
%%%
%%% @end
%%% Created : 10 Jun 2015 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(bkfw_fw).

-include("bkfw.hrl").

-export([upgrade_fw/1,
		 upgrade_cpu/1,
		 upgrade_amp/1]).

upgrade_fw(Filename) ->
    case bkfw_config:script("check_pkg.sh", Filename) of
		ok ->
			bkfw_config:script("upgrade.sh", Filename),
			bkfw_app:reboot(),
			ok;
		{error, Err} -> {error, Err}
    end.

upgrade_cpu(Filename) ->
    ?debug("Upgrading CPU firmware from ~s [fake]", [Filename]),
	ok.

upgrade_amp(Filename) ->
	?debug("Upgrading AMP firmware from ~s [fake]", [Filename]),
	ok.
