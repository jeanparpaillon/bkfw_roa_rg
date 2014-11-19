-define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(Msg), io:format("DEBUG: " ++ Msg)).
-define(debug(Msg, Data), io:format("DEBUG: " ++ Msg, Data)).
-else.
-define(debug(_Msg), true).
-define(debug(_Msg, _Data), true).
-endif.

-define(info(Msg), io:format("INFO: " ++ Msg)).
-define(info(Msg, Data), io:format("INFO: " ++ Msg, Data)).

-define(error(Msg), io:format("ERROR: " ++ Msg)).
-define(error(Msg, Data), io:format("ERROR: " ++ Msg, Data)).

% scan & parse structs
-type terminal() :: adi | alarms | bref | cc | 'edfa_temp' | 'edfa_psu' | fw_ver | gc | hw_rev | 
		    hw_ver | it | i | lc | li | lo | lt | mode | module | mute | n | off | pc | pd | 
		    pin | pout | prod_date | 'pump_bias' | 'pump_temp' | ra | 
		    rcc | rgc | ri | rit | rlc | rli | rlo | rlt | rmode | rn | rpc | rpm |
		    rv | scc | sgc | sli | slo | smode | spc | sw_ver | vendor | v.
-type token() :: integer() | float() | terminal() | binary().

-type msg_line() :: [Args :: [token()]].
-type msg() :: {Addr :: integer(), Cmd :: atom(), Lines :: [msg_line()]}.

% SNMP table record
-record(edfaTable, {ampConsign,
		    gainConsign,
		    outputPowerConsign,
		    operatingMode,
		    curLaserTemp,
		    curAmp,
		    curInternalTemp,
		    vendor,
		    moduleType,
		    hwVer,
		    hwRev,
		    swVer,
		    fwVer,
		    partNum,
		    serialNum,
		    productDate}).
