-define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(Msg), case application:get_env(bkfw, debug, true) of 
						true -> 
							error_logger:info_msg("DEBUG: " ++ Msg); 
						false -> true 
					end).
-define(debug(Msg, Data), case application:get_env(bkfw, debug, true) of 
							  true -> 
								  error_logger:info_msg("DEBUG: " ++ Msg, Data); 
							  false -> true 
						  end).
-else.
-define(debug(_Msg), true).
-define(debug(_Msg, _Data), true).
-endif.

-define(info(Msg), error_logger:info_msg("INFO: " ++ Msg)).
-define(info(Msg, Data), error_logger:info_msg("INFO: " ++ Msg, Data)).

-define(error(Msg), error_logger:error_msg("ERROR: " ++ Msg)).
-define(error(Msg, Data), error_logger:error_msg("ERROR: " ++ Msg, Data)).

%% scan & parse structs
-type terminal() :: adi | alarms | bref | cc | 'edfa_temp' | 'edfa_psu' | fwVer | gc | hwRev | fan1 | fan2 | fan3 | fan4 |
					hwVer | it | i | lc | li | lo | lt | mode | moduleType | mute | n | off | partNum | pc | pd | 
					pin | pout | productDate | psu | psu1 | psu2 | 'pump_bias' | 'pump_temp' | ra | 
					rcc | rgc | ri | rit | rlcc | rlc | rlpc | rlgc | rli | rlo | rlt | rmode | rn | rpc | rpm |
					rv | scc | sgc | sli | slo | smode | spc | swVer | vendor | v.
-type token() :: integer() | float() | terminal() | binary().

-type msg_line() :: [Args :: [token()]].
-type msg() :: {Addr :: integer(), Cmd :: atom(), Lines :: [msg_line()]}.

%% SNMP table record
-include("SMM-MIB.hrl").

-record(ampTable, {index                                         :: integer(),
				   ampConsign         = 0.0                      :: float(),
				   ampConsign2        = 0.0                      :: float(),
				   gainConsign        = 0.0                      :: float(),
				   outputPowerConsign = 0.0                      :: float(),
				   operatingMode      = ?ampOperatingMode_off    :: integer(),
				   curLaserTemp       = 0.0                      :: float(),
				   curAmp             = 0.0                      :: float(),
				   curAmp2            = 0.0                      :: float(),
				   curInternalTemp    = 0.0                      :: float(),
				   powerPd1           = 0.0                      :: float(),
				   powerPd2           = 0.0                      :: float(),
				   powerPd3           = 0.0                      :: float(),
				   powerSupply        = 0.0                      :: float(),
				   inputLossThreshold = 0.0                      :: float(),
				   outputLossThreshold= 0.0                      :: float(),
				   ccMax1             = 0.0                      :: float(),
				   ccMax2             = 0.0                      :: float(),
				   pcMin              = 0.0                      :: float(),
				   pcMax              = 0.0                      :: float(),
				   gcMin              = 0.0                      :: float(),
				   gcMax              = 0.0                      :: float(),
				   overallGain		  = 0.0                      :: float(),
				   gainBeforeVoa      = 0.0                      :: float(),
				   voaAttenuation     = 0.0                      :: float(),
				   params             = []                       :: []
				  }).
-type ampTable() :: #ampTable{}.

-type smmAlarmName() :: pin | pout | pump_temp | pump_bias | edfa_temp | edfa_psu | bref | adi | mute.
-record(smmAlarm, {index                       :: integer(),
				   name                        :: smmAlarmName(),
				   obj                         :: term()}).
-type smmAlarm() :: #smmAlarm{}.
