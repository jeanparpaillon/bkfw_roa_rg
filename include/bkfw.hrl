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
-type terminal() :: adi | alarms | bref | cc | 'edfa_temp' | 'edfa_psu' | fwVer | gc | hwRev | 
		    hwVer | it | i | lc | li | lo | lt | mode | moduleType | mute | n | off | partNum | pc | pd | 
		    pin | pout | productDate | 'pump_bias' | 'pump_temp' | ra | 
		    rcc | rgc | ri | rit | rlc | rli | rlo | rlt | rmode | rn | rpc | rpm |
		    rv | scc | sgc | sli | slo | smode | spc | swVer | vendor | v.
-type token() :: integer() | float() | terminal() | binary().

-type msg_line() :: [Args :: [token()]].
-type msg() :: {Addr :: integer(), Cmd :: atom(), Lines :: [msg_line()]}.

% SNMP table record
-include("EDFA-MIB.hrl").

-record(edfaTable, {index                                         :: integer(),
		    ampConsign         = 0                        :: integer(),
		    gainConsign        = 0                        :: integer(),
		    outputPowerConsign = 0                        :: integer(),
		    operatingMode      = ?edfaOperatingMode_off   :: integer(),
		    curLaserTemp       = 0                        :: integer(),
		    curAmp             = 0                        :: integer(),
		    curInternalTemp    = 0                        :: integer(),
		    powerPd1           = 0                        :: integer(),
		    powerPd2           = 0                        :: integer(),
		    powerPd3           = 0                        :: integer(),
		    powerSupply        = 0                        :: integer(),
		    inputLossThreshold = 0                        :: integer(),
		    outputLossThreshold= 0                        :: integer(),
		    vendor             = ""                       :: string(),
		    moduleType         = ""                       :: string(),
		    hwVer              = ""                       :: string(),
		    hwRev              = ""                       :: string(),
		    swVer              = ""                       :: string(),
		    fwVer              = ""                       :: string(),
		    partNum            = ""                       :: string(),
		    serialNum          = ""                       :: string(),
		    productDate        = ""                       :: string()
		   }).
-type edfaTable() :: #edfaTable{}.
