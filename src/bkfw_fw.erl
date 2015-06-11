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
		 upgrade_amps/1]).

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
	case file:read_file(Filename) of
		{ok, Data} ->
			upgrade_micro(0, Data);
		{error, enomem} ->
			{error, invalid_fw};
		{error, Err} ->
			?error("Internal error reading firmware: ~p~n", [Err]),
			{error, internal}
	end.

upgrade_amps(Filename) ->
	case file:read_file(Filename) of
		{ok, Data} ->
			Amps = mnesia:dirty_match_object(#ampTable{_='_'}),
			upgrade_amp(Amps, Data);
		{error, enomem} ->
			{error, invalid_fw};
		{error, Err} ->
			?error("Internal error reading firmware: ~p~n", [Err]),
			{error, internal}
	end.

%%%
%%% Priv
%%%
upgrade_amp([], _) ->
	ok;
upgrade_amp([ #ampTable{index=Idx} | Tail ], Data) when Idx rem 2 == 0 ->
	upgrade_amp(Tail, Data);
upgrade_amp([ #ampTable{index=Idx} | Tail ], Data) ->
	case upgrade_micro(Idx, Data) of
		ok ->
			upgrade_amp(Tail, Data);
		{error, Err} ->
			{error, Err}
	end.

upgrade_micro(Idx, _Fw) ->
	?debug("Upgrading firmware on unit: ~p~n", [Idx]),
	case bkfw_srv:raw("0000") of
		{ok, Data} ->
			ok;
		{error, _} = Err ->
			Err
	end.
