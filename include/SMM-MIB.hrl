%%% This file was automatically generated by snmpc_mib_to_hrl version 4.25.1
%%% Date: 17-Jan-2015::11:10:04
-ifndef('SMM-MIB').
-define('SMM-MIB', true).

%% Notifications
-define(smmGenericTrap, [1,3,6,1,4,1,44890,1,1,2,12]).
-define(ampMuteTrap, [1,3,6,1,4,1,44890,1,1,2,11]).
-define(ampAdiTrap, [1,3,6,1,4,1,44890,1,1,2,10]).
-define(ampBrefTrap, [1,3,6,1,4,1,44890,1,1,2,9]).
-define(smmPowerSupplyTrap, [1,3,6,1,4,1,44890,1,1,2,8]).
-define(smmInternalTempTrap, [1,3,6,1,4,1,44890,1,1,2,7]).
-define(ampPowerSupplyTrap, [1,3,6,1,4,1,44890,1,1,2,6]).
-define(ampInternalTempTrap, [1,3,6,1,4,1,44890,1,1,2,5]).
-define(ampPumpBiasTrap, [1,3,6,1,4,1,44890,1,1,2,4]).
-define(ampPumpTempTrap, [1,3,6,1,4,1,44890,1,1,2,3]).
-define(ampOutputPowerTrap, [1,3,6,1,4,1,44890,1,1,2,2]).
-define(ampInputPowerTrap, [1,3,6,1,4,1,44890,1,1,2,1]).

%% Oids

-define(smmModule, [1,3,6,1,4,1,44890,1]).

-define(smmMIB, [1,3,6,1,4,1,44890,1,1]).

-define(smmMIBObjects, [1,3,6,1,4,1,44890,1,1,1]).

-define(smm, [1,3,6,1,4,1,44890,1,1,1,1]).
-define(smmVendor, [1,3,6,1,4,1,44890,1,1,1,1,1]).
-define(smmVendor_instance, [1,3,6,1,4,1,44890,1,1,1,1,1,0]).
-define(smmModuleType, [1,3,6,1,4,1,44890,1,1,1,1,2]).
-define(smmModuleType_instance, [1,3,6,1,4,1,44890,1,1,1,1,2,0]).
-define(smmHWVer, [1,3,6,1,4,1,44890,1,1,1,1,3]).
-define(smmHWVer_instance, [1,3,6,1,4,1,44890,1,1,1,1,3,0]).
-define(smmHWRev, [1,3,6,1,4,1,44890,1,1,1,1,4]).
-define(smmHWRev_instance, [1,3,6,1,4,1,44890,1,1,1,1,4,0]).
-define(smmSWVer, [1,3,6,1,4,1,44890,1,1,1,1,5]).
-define(smmSWVer_instance, [1,3,6,1,4,1,44890,1,1,1,1,5,0]).
-define(smmFWVer, [1,3,6,1,4,1,44890,1,1,1,1,6]).
-define(smmFWVer_instance, [1,3,6,1,4,1,44890,1,1,1,1,6,0]).
-define(smmPartNum, [1,3,6,1,4,1,44890,1,1,1,1,7]).
-define(smmPartNum_instance, [1,3,6,1,4,1,44890,1,1,1,1,7,0]).
-define(smmSerialNum, [1,3,6,1,4,1,44890,1,1,1,1,8]).
-define(smmSerialNum_instance, [1,3,6,1,4,1,44890,1,1,1,1,8,0]).
-define(smmProductDate, [1,3,6,1,4,1,44890,1,1,1,1,9]).
-define(smmProductDate_instance, [1,3,6,1,4,1,44890,1,1,1,1,9,0]).
-define(smmPowerSupply, [1,3,6,1,4,1,44890,1,1,1,1,10]).
-define(smmPowerSupply_instance, [1,3,6,1,4,1,44890,1,1,1,1,10,0]).
-define(smmCurInternalTemp, [1,3,6,1,4,1,44890,1,1,1,1,11]).
-define(smmCurInternalTemp_instance, [1,3,6,1,4,1,44890,1,1,1,1,11,0]).
-define(smmNumber, [1,3,6,1,4,1,44890,1,1,1,1,12]).
-define(smmNumber_instance, [1,3,6,1,4,1,44890,1,1,1,1,12,0]).

-define(ampTable, [1,3,6,1,4,1,44890,1,1,1,1,13]).

-define(ampEntry, [1,3,6,1,4,1,44890,1,1,1,1,13,1]).
-define(ampIndex, 1).
-define(ampAmpConsign, 2).
-define(ampGainConsign, 3).
-define(ampOutputPowerConsign, 4).
-define(ampOperatingMode, 5).
-define(ampCurLaserTemp, 6).
-define(ampCurAmp, 7).
-define(ampCurInternalTemp, 8).
-define(ampPowerPd1, 9).
-define(ampPowerPd2, 10).
-define(ampPowerPd3, 11).
-define(ampPowerSupply, 12).
-define(ampInputLossTh, 13).
-define(ampOutputLossTh, 14).
-define(ampVendor, 15).
-define(ampModuleType, 16).
-define(ampHWVer, 17).
-define(ampHWRev, 18).
-define(ampSWVer, 19).
-define(ampFWVer, 20).
-define(ampPartNum, 21).
-define(ampSerialNum, 22).
-define(ampProductDate, 23).

-define(smmMIBTraps, [1,3,6,1,4,1,44890,1,1,2]).

-define(smmMIBConformance, [1,3,6,1,4,1,44890,1,1,3]).

-define(smmMIBTrapGroups, [1,3,6,1,4,1,44890,1,1,3,1]).

-define(smmMIBTrapGroup, [1,3,6,1,4,1,44890,1,1,3,1,1]).


%% Range values
-define(low_smmVendor, 0).
-define(high_smmVendor, 255).
-define(low_smmModuleType, 0).
-define(high_smmModuleType, 255).
-define(low_smmHWVer, 0).
-define(high_smmHWVer, 255).
-define(low_smmHWRev, 0).
-define(high_smmHWRev, 255).
-define(low_smmSWVer, 0).
-define(high_smmSWVer, 255).
-define(low_smmFWVer, 0).
-define(high_smmFWVer, 255).
-define(low_smmPartNum, 0).
-define(high_smmPartNum, 255).
-define(low_smmSerialNum, 0).
-define(high_smmSerialNum, 255).
-define(low_smmProductDate, 0).
-define(high_smmProductDate, 255).
-define(low_smmPowerSupply, -2147483648).
-define(high_smmPowerSupply, 2147483647).
-define(low_smmCurInternalTemp, -2147483648).
-define(high_smmCurInternalTemp, 2147483647).
-define(low_smmNumber, -2147483648).
-define(high_smmNumber, 2147483647).
-define(low_ampAmpConsign, 0).
-define(high_ampAmpConsign, 4294967295).
-define(low_ampGainConsign, -2147483648).
-define(high_ampGainConsign, 2147483647).
-define(low_ampOutputPowerConsign, 0).
-define(high_ampOutputPowerConsign, 4294967295).
-define(low_ampCurLaserTemp, -2147483648).
-define(high_ampCurLaserTemp, 2147483647).
-define(low_ampCurAmp, 0).
-define(high_ampCurAmp, 4294967295).
-define(low_ampCurInternalTemp, -2147483648).
-define(high_ampCurInternalTemp, 2147483647).
-define(low_ampPowerPd1, -2147483648).
-define(high_ampPowerPd1, 2147483647).
-define(low_ampPowerPd2, -2147483648).
-define(high_ampPowerPd2, 2147483647).
-define(low_ampPowerPd3, -2147483648).
-define(high_ampPowerPd3, 2147483647).
-define(low_ampPowerSupply, -2147483648).
-define(high_ampPowerSupply, 2147483647).
-define(low_ampInputLossTh, -2147483648).
-define(high_ampInputLossTh, 2147483647).
-define(low_ampOutputLossTh, -2147483648).
-define(high_ampOutputLossTh, 2147483647).
-define(low_ampVendor, 0).
-define(high_ampVendor, 255).
-define(low_ampModuleType, 0).
-define(high_ampModuleType, 255).
-define(low_ampHWVer, 0).
-define(high_ampHWVer, 255).
-define(low_ampHWRev, 0).
-define(high_ampHWRev, 255).
-define(low_ampSWVer, 0).
-define(high_ampSWVer, 255).
-define(low_ampFWVer, 0).
-define(high_ampFWVer, 255).
-define(low_ampPartNum, 0).
-define(high_ampPartNum, 255).
-define(low_ampSerialNum, 0).
-define(high_ampSerialNum, 255).
-define(low_ampProductDate, 0).
-define(high_ampProductDate, 255).


%% Enum definitions from ampOperatingMode
-define(ampOperatingMode_off, 4).
-define(ampOperatingMode_cc, 3).
-define(ampOperatingMode_gc, 2).
-define(ampOperatingMode_pc, 1).

%% Default values
-define(default_smmVendor, []).
-define(default_smmModuleType, []).
-define(default_smmHWVer, []).
-define(default_smmHWRev, []).
-define(default_smmSWVer, []).
-define(default_smmFWVer, []).
-define(default_smmPartNum, []).
-define(default_smmSerialNum, []).
-define(default_smmProductDate, []).
-define(default_smmPowerSupply, -2147483648).
-define(default_smmCurInternalTemp, -2147483648).
-define(default_smmNumber, -2147483648).

-endif.
