-define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(Msg), case application:get_env(bkfw, debug, true) of true -> io:format("DEBUG: " ++ Msg); false -> true end).
-define(debug(Msg, Data), case application:get_env(bkfw, debug, true) of true -> io:format("DEBUG: " ++ Msg, Data); false -> true end).
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

-record(edfaMcuTable, {index                                         :: integer(),
		       ampConsign         = 0                        :: float(),
		       gainConsign        = 0                        :: float(),
		       outputPowerConsign = 0                        :: float(),
		       operatingMode      = ?edfaMcuOperatingMode_off:: integer(),
		       curLaserTemp       = 0                        :: float(),
		       curAmp             = 0                        :: float(),
		       curInternalTemp    = 0                        :: float(),
		       powerPd1           = 0                        :: float(),
		       powerPd2           = 0                        :: float(),
		       powerPd3           = 0                        :: float(),
		       powerSupply        = 0                        :: float(),
		       inputLossThreshold = 0                        :: float(),
		       outputLossThreshold= 0                        :: float(),
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
-type edfaMcuTable() :: #edfaMcuTable{}.

-type edfaAlarmName() :: pin | pout | pump_temp | pump_bias | edfa_temp | edfa_psu | bref | adi | mute.
-record(edfaAlarm, {index                       :: integer(),
		    name                        :: edfaAlarmName(),
		    obj                         :: term()}).
-type edfaAlarm() :: #edfaAlarm{}.
