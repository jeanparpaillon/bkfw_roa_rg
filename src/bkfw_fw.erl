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
-define(FW_START, 16#8004000).
-define(FW_LENGTH, 48).

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
	ok = bkfw_sup:set_upgrade(true),
	case bkfw_srv:wait() of
		{ok, ComRef} ->
			IdxStr = ["0x", io_lib:format("~4.16.0b", [Idx])],
			Ret = upgrade_micro(ComRef, IdxStr, Fw),
			bkfw_srv:release(ComRef),
			ok = bkfw_sup:set_upgrade(false),
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
		{ok, _Else} ->
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


upg_flash_write(ComRef, Idx, <<>>, _) ->
	?debug("firmware written, starting", []),
	EncAdr = io_lib:format("0x~8.16.0b", [?FW_START]),
	case bkfw_srv:command(ComRef, Idx, flash, ["START ", EncAdr], ?TIMEOUT) of
		{ok, [ok]} -> ok;
		{ok, _} -> {error, unexpected};
		{error, _} = Err -> Err
	end;

upg_flash_write(ComRef, Idx, Fw, Adr) ->
	{Payload, Rest, Length} = if 
								  byte_size(Fw) < ?FW_LENGTH -> 
									  {Fw, <<>>, byte_size(Fw)};
								  true ->
									  << P:?FW_LENGTH/bytes, R/binary >> = Fw,
									  {P, R, ?FW_LENGTH}
							  end,
	EncAdr = io_lib:format("0x~8.16.0b", [Adr]),
	EncLength = io_lib:format("~b", [Length]),
	EncPayload = encode(Payload),
	%?debug("write: ~p", [EncPayload]),
	case bkfw_srv:command(ComRef, Idx, flash, ["WRITE ", EncAdr, " ", EncLength, " ", EncPayload], ?TIMEOUT) of
		{ok, [ok]} ->
			case bkfw_srv:command(ComRef, Idx, flash, ["READ ", EncAdr, " ", EncLength], ?TIMEOUT) of
				{ok, [ok, EncPayload]} ->
					upg_flash_write(ComRef, Idx, Rest, Adr + Length);
				{ok, [ok, _Else]} -> 
					{error, invalid_payload};
				{ok, _Else} -> 
					{error, unexpected};
				{error, Err} ->	
					?error("Error checking firmware: ~s", [Err]),
					{error, Err}
			end;
		{ok, [nok, error, Code]} -> 
			{error, code_to_err(Code)};
		{ok, _} ->
			{error, unexpected};
		{error, Err}  ->
			?error("Error writing firmware: ~p", [Err]),
			{error, Err}
	end.

encode(Bin) -> base64:encode(Bin).

code_to_err(1) -> not_ready;
code_to_err(2) -> not_opened;
code_to_err(3) -> unexpected;
code_to_err(4) -> timeout.
