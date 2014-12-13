BKtel EDFA
==========

Requirements:

* erlang >= 16
* socat

# Factory settings

Factory settings are placed in the following file:
`/var/lib/bkfw/factory.config`

Syntax is (don't forget brackets and final dot !):
```
[
 {bkfw,
   [
     {param1, "string parameter"},
     {param2, 12},
     {param3, value}
   ]
 }
].
```

# Parameters

* `com` (string, default: "/dev/tty/AMA0"): path to com device
* `slots_period` (integer, default: 10000): refreshing time (ms) for occupied slots
* `mcu_period` (integer, default: 2000): refreshing time (ms) for MCU status
* `timeout` (integer, default: 2000): ms before timeout when communicating to COM port
* `max_queue` (integer, default: 1000): maximum size of queue for pending message (avoid out-of-memory)
* `logo` (string, default: bktel logo): full path to logo, eg: /root/mylogo.png. width: 250px
* `http` (list of tuples): cowboy HTTP server parameters, see http://ninenines.eu/docs/en/cowboy/HEAD/guide/getting_started/
* `upload_dir` (string, default: "/var/lib/bkfw/upload"): path for uploaded firmware
* `system_cmd` (true | false): if false, does not really execute system commands (reboot, etc.): for debugging purpose
* `password` ({md5, hash_value}, default pasword: 'admin'): default admin password. hash_value is base64 encoded value of the hash, using md5 algorithm. eg: {md5, "0DPiKuNIrrVmD8IUCuw1hQxNqZc="}

# HTTP Authentication

HTTP authentication uses Basic mechanism with a special name, to avoid internal browser mechanism.
On authentication needed, following headers is sent:
```www-authenticate: x-basic realm="bkfw"```

For succesful authentication, client must return:
```authorization: x-basic ...```

# Test with MCU emulation

1. Run `priv/start_pty.sh`
2. Run `priv/mcu_emul -s hex_mask /path/to/pty1`
  where hex\_mask representing occupied slots
3. Start with: `./start.sh /path/to/pty`

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

# Alarms

* pin: "Input power loss" (powerInput)
* pout: "Output power loss" (powerOutput)
* pump_temp: "Pump temperature alarm" (curLaserTemp)
* pump_bias: "Laser current is over 95% of EOL" (curAmp)
* edfa_temp: "Internal temperature alarm" (curInternalTemp)
* edfa_psu: "Power Supply alarm" (powerSupply)
* bref: "Back reflection alarm"
* adi: "Shutdown input is active"
* mute: "Mute input is active"
