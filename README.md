BKtel EDFA
==========

Requirements:
- erlang >= 16
- socat

# Test with MCU emulation

1. Run priv/start_pty.sh
2. Run priv/mcu_emul /path/to/pty1
3. echo XXX > /path/to/pty2
