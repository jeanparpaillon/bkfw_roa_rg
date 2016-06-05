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
			bkfw:reboot(),
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
			%Amps = [ #ampTable{index=71}, #ampTable{index=72} ],
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
	Fun = fun (init, Com, S) ->
				  IdxStr = ["0x", io_lib:format("~2.16.0b", [Idx])],
				  Bin = [IdxStr, " UCAN", $\r, $\n],
				  ok = bkfw_com:raw(Com, Bin),
				  {more, S#{ idx => IdxStr, fw => Fw, state => open, adr => ?FW_START }};
			  ({msg, {_Idx, Cmd, Args}}, Com, S) ->
				  upgrade(Com, Cmd, Args, S)
		  end,
	bkfw_srv:call(Fun, #{}, 1000*300).


upgrade(Com, ok, [], #{ idx := Idx, state := open }=S) ->
	ok = bkfw_com:raw(Com, [Idx, " FLASH OPEN", $\r, $\n]),
	{more, S#{ state := clear }};

upgrade(Com, ok, [], #{ idx := Idx, state := clear }=S) ->
	ok = bkfw_com:raw(Com, [Idx, " FLASH CLEAR", $\r, $\n]),
	{more, S#{ state := init_write }};

upgrade(Com, ok, [], #{ state := init_write }=S) ->
	S1 = write(Com, S),
	{more, S1};

upgrade(Com, ok, [PrevPayload], #{ fw := <<>>, payload := PrevPayload, idx := Idx, state := write }=S) ->
	ok = bkfw_com:raw(Com, [Idx, " FLASH START ", io_lib:format("0x~8.16.0b", [?FW_START]) ]),
	{more, #{ state := start }=S};

upgrade(Com, ok, [PrevPayload], #{ payload := PrevPayload, state := write }=S) ->
	S1 = write(Com, S),
	{more, S1};

upgrade(_Com, ok, [_OtherPayload], #{ payload := _PrevPayload, state := write }) ->
	{error, invalid_payload};

upgrade(Com, ok, [], #{ idx := Idx, state := read, to_read := ToRead }=S) ->
	ok = bkfw_com:raw(Com, [Idx, " FLASH READ ", ToRead, $\r, $\n]),
	{more, S#{ state := write }};

upgrade(_Com, ok, [], #{ state := start }) ->
	ok;

upgrade(_Com, nok, [error, Code], _S) ->
	{error, code_to_err(Code)};

upgrade(_Com, _, _, _) ->
	{error, unexpected}.


write(Com, #{ idx := Idx, fw := Fw, adr := Adr }=S) ->	
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
	ok = bkfw_com:raw(Com, [Idx, " FLASH WRITE ", EncAdr, " ", EncLength, " ", EncPayload, $\r, $\n]),
	{more, S#{ state := read, to_read := [EncAdr, " ", EncLength], 
			   payload := EncPayload, 
			   adr := Adr + Length, fw := Rest }}.


encode(Bin) -> base64:encode(Bin).


code_to_err(1) -> not_ready;
code_to_err(2) -> not_opened;
code_to_err(3) -> unexpected;
code_to_err(4) -> timeout_can_bus.
