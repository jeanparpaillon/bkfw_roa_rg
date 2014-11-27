BKtel EDFA
==========

Requirements:

* erlang >= 16
* socat

# Parameters

* `com` (string, mandatory): path to com device
* `slots_period` (integer, default: 1000): refreshing time (ms) for occupied slots
* `mcu_period` (integer, default: 1000): refreshing time (ms) for MCU status
* `timeout` (integer, default: 1000): ms before timeout when communicating to COM port
* `max_queue` (integer, default: 100): maximum size of queue for pending message (avoid out-of-memory)

# Test with MCU emulation

1. Run `priv/start_pty.sh`
2. Run `priv/mcu_emul -s hex_mask /path/to/pty1`
  where hex\_mask representing occupied slots
3. Check in priv/default.config the second pty

# REST API

## /api/mcu/[index]

* describe MCU at position [index]

```
{
	index: (integer),
	ampConsign: (float),
	gainConsign: (float),
	outputPowerConsign: (float),
	operatingMode: [1 (PC)|2 (GC)|3 (CC)|4 (OFF)],
	curLaserTemp: (float),
	curAmp: (float),
	curInternalAmp: (float),
	powerInput: (float),
	powerOutput: (float),
	powerSupply: (float),
	inputLossThreshold: (float),
	outputLossThreshold: (float),
	vendor: (string),
	moduleType: (string),
	hwVer: (string),
	hwRev: (string),
	swVer: (string),
	fwVer: (string),
	partNum: (string),
	serialNum: (string),
	productDate: (string)
}
```

## /api/mcu/

List of MCU descriptions

## /api/edfa

* describe EDFA (main chassis):

```
{
	curInternalAmp: (float),
	powerSupply: (float),
	vendor: (string),
	moduleType: (string),
	hwVer: (string),
	hwRev: (string),
	swVer: (string),
	fwVer: (string),
	partNum: (string),
	serialNum: (string),
	productDate: (string)
}
```

## /api/sys/login

## /api/sys/net

```
{
	type: dhcp | static,
	ip: (string),
	netmask: (string),
	gateway: (string)
}
```

## /api/sys/password

## /api/sys/community

```
{
	public: (string),
	restricted: (string)
}
```

## /api/sys/protocol

```
{
	snmpv1: (boolean),
	snmpv2: (boolean),
	snmpv3: (boolean)
}
```

## /api/sys/firmware

```
{
	id: (string),
	version: (string)
}
```
