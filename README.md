BKtel EDFA firmware
===================

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

# SNMP Standard settings

The included SNMP agent implements SNMP STANDARD MIB. Variables are set in the file
`/opt/bkfw/snmp/agent/conf/standard.conf`. See mibs/SNMPv2-MIB for variables meaning.

Example:
```
{sysName, "BKtel EDFA"}.
{sysDescr, "BKtel EDFA"}.
{sysContact, "support@bktel-photonics.com"}.
{sysLocation, ""}.
{sysObjectID, [3,6,1,4,1,44890,1]}.
{sysServices, 1}.
{snmpEnableAuthenTraps, enabled}.
```

# Parameters

## Basic parameters

* `com` (string, default: "/dev/tty/AMA0"): path to com device
* `logo` (string, default: bktel logo): full path to logo, eg: /root/mylogo.png. width: 250px
* `netif` (string, default: "eth0"): network interface name

## Performance parameters

* `edfa_period` (integer, default: 2000): refreshing time (ms) for EDFA
* `mcu_period` (integer, default: 2000): refreshing time (ms) for MCU status
* `timeout` (integer, default: 2000): ms before timeout when communicating to COM port
* `max_queue` (integer, default: 1000): maximum size of queue for pending message (avoid out-of-memory)

## Advanced parameters

* `http` (list of tuples): cowboy HTTP server parameters, see http://ninenines.eu/docs/en/cowboy/HEAD/guide/getting_started/
* `upload_dir` (string, default: "/var/lib/bkfw/upload"): path for uploaded firmware
* `system_cmd` (true | false): if false, does not really execute system commands (reboot, etc.): for debugging purpose
* `password` ({md5, hash_value}, default pasword: 'admin'): default admin password. hash_value is base64 encoded value of the hash, using md5 algorithm. eg: {md5, "0DPiKuNIrrVmD8IUCuw1hQxNqZc="}

# Default values

* SNMP communities (v1 + v2): public (read-only), private (read-write)
* SNMP v3:
  * .1.3.6.1.2.1 accessible noAUthNoPriv
  * .1.3.6.1.4.1 accessible with authPriv or authNoPriv
* HTTP login/password: admin / admin
* SNMP v3: 

