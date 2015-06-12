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

-define(TIMEOUT, 5000).
-define(FW_START, 16#08004000).
-define(FW_LENGTH, 64).

upgrade_fw(Filename) ->
    case bkfw_config:script("check_pkg.sh", Filename) of
		ok ->
			bkfw_config:script("upgrade.sh", Filename),
			bkfw_app:reboot(),
			ok;
		{error, Err} -> {error, Err}
    end.

upgrade_cpu(Filename) ->
    ?debug("Upgrading CPU firmware from ~s", [Filename]),
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

upgrade_micro(Idx, Fw) ->
	?debug("Upgrading firmware on unit: ~p~n", [Idx]),
	case bkfw_srv:wait() of
		{ok, ComRef} ->
			IdxStr = ["0x", io_lib:format("~4.16.0b", [Idx])],
			Ret = upgrade_micro(ComRef, IdxStr, Fw),
			bkfw_srv:release(ComRef),
			Ret;
		{error, _} = Err ->
			Err
	end.

upgrade_micro(ComRef, Idx, Fw) ->
	case bkfw_srv:command(ComRef, Idx, upg, [], ?TIMEOUT) of
		{ok, [ok]} ->
			upg_flash_open(ComRef, Idx, Fw);
		{ok, [nok, error, Code]} ->
			{error, code_to_err(Code)};
		{ok, _} ->
			{error, unexpected};
		{error, _} = Err ->
			Err
	end.

upg_flash_open(ComRef, Idx, Fw) ->
	case bkfw_srv:command(ComRef, Idx, flash, ["OPEN"], ?TIMEOUT) of
		{ok, [ok | _]} ->
			upg_flash_clear(ComRef, Idx, Fw);
		{ok, [nok, error, Code]} ->
			{error, code_to_err(Code)};
		{ok, _} ->
			{error, unexpected};
		{error, _} = Err ->
			Err
	end.

upg_flash_clear(ComRef, Idx, Fw) ->
	case bkfw_srv:command(ComRef, Idx, flash, ["CLEAR"], ?TIMEOUT) of
		{ok, [ok | _]} ->
			upg_flash_write(ComRef, Idx, Fw, ?FW_START);
		{ok, [nok, error, Code]} ->
			{error, code_to_err(Code)};
		{ok, _} ->
			{error, unexpected};
		{error, _} = Err ->
			Err
	end.


% Firmware written
upg_flash_write(ComRef, Idx, <<>>, _) ->
	case bkfw_src:command(ComRef, Idx, flash, ["START ", ?FW_START]) of
		{ok, [ok]} -> ok;
		{ok, _} -> {error, unexpected};
		{error, _} = Err -> Err
	end;

% Last chunk, in case it is smaller than ?FW_LENGTH
upg_flash_write(ComRef, Idx, Fw, Adr) when byte_size(Fw) < ?FW_LENGTH ->
	Length = byte_size(Fw),
	case bkfw_srv:command(ComRef, Idx, flash, ["WRITE ", Adr, Length, Fw], ?TIMEOUT) of
		{ok, [ok]} ->
			case bkfw_srv:command(ComRef, Idx, flash, ["READ", Adr, Length]) of
				{ok, [ok, Fw]} ->
					upg_flash_write(ComRef, Idx, <<>>, Adr + Length);
				{ok, [ok, _]} -> {error, invalid_payload};
				{ok, _} -> {error, unexpected};
				{error, _} = Err ->	Err
			end;
		{ok, [nok, error, Code]} -> {error, code_to_err(Code)};
		{ok, _} ->{error, unexpected};
		{error, _} = Err ->	Err
	end;

% Regular chunk
upg_flash_write(ComRef, Idx, Fw, Adr) ->
	<< Payload:(?FW_LENGTH*8), Rest/binary >> = Fw,
	case bkfw_srv:command(ComRef, Idx, flash, ["WRITE ", Adr, ?FW_LENGTH, Payload], ?TIMEOUT) of
		{ok, [ok]} ->
			case bkfw_srv:command(ComRef, Idx, flash, ["READ", Adr, ?FW_LENGTH]) of
				{ok, [ok, Payload]} ->
					upg_flash_write(ComRef, Idx, Rest, Adr + ?FW_LENGTH);
				{ok, [ok, _]} -> {error, invalid_payload};
				{ok, _} -> {error, unexpected};
				{error, _} = Err ->	Err
			end;
		{ok, [nok, error, Code]} -> {error, code_to_err(Code)};
		{ok, _} ->{error, unexpected};
		{error, _} = Err ->	Err
	end.


code_to_err(1) -> not_ready;
code_to_err(2) -> not_opened;
code_to_err(3) -> unexpected;
code_to_err(4) -> timeout.
