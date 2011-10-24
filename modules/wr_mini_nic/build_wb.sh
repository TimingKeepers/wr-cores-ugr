#!/bin/bash

mkdir -p doc
wbgen2 -D ./doc/minic.html -V minic_wb_slave.vhd -p minic_wbgen2_pkg.vhd --cstyle defines --lang vhdl  -H record -K ../../sim/minic_regs.vh mini_nic.wb