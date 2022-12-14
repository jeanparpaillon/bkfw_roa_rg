# 0.1

* Initial release

# 0.2.7

* HTTP authentication
* Reduce restart time
* SNMP traps
* Alarms show in HTTP UI

# 0.2.8 (broken release)

* Improve concurrent access to COM port:
  block until an answer has been received
* Add mutex for COM access

# 0.2.9

* Fix 0.2.8 packaging

# 0.2.10

* Adapt to Raspberry COM
* Update graphicals (colours)
* Fix typos in UI
* Temporary disable SNMPv3 in UI

# 0.2.11

* Update protocol: handle out-of-range values for setpoints

# 0.3

* Add SNMPv3 support
* Update MIB
* Enable multiple amps setting
* Improve query time when amps are added/removed

# 0.3.1

* Fix bug after changing MIB

# 0.3.2

* Poll slots on every loop

# 0.3.3

* Improve slots detection
* Improve dialogs and messages in system page

# 0.3.4

* Fix slots detection
* Improve latency times

# 0.3.5

* Fix network settings
* Fix SNMP traps

# 0.3.6

* Set default network to 10.0.0.3

# 0.3.7

* Fix setting defaults (network and SNMP communities)

# 0.3.8

* Fix network config

# 0.4

* Fix restarting

# 0.4.1

* Add network gateway in system config page

# 0.4.2

* Fix scripts loading

# 0.5

* Add new traps: smmPsu1Trap and smmPsu2Trap

# 0.6

* Increase maximum slots allowed: 128

# 1.0rc1

* Add USB mode: when enable, user can communicate with CPU from front
  USB plug
* New parameter usbtty: points to the USB-to-serial device (default:
  "/dev/ttyUSB0")

# 1.0rc2

* Ignore when USB port is not available

# 1.1rc1

* USB mode ok
* Firmware upload quite ok: binary encoding yet to decide

# 1.1

* Use base64 encoding for firmware transfer

# 1.2

* Include amps fiwnware upgrade