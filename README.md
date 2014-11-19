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
