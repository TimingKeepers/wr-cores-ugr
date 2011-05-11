#!/bin/bash

mkdir -p doc
wbgen2 -D ./doc/minic.html -V minic_wb_slave.vhd -C ../../../software/include/hw/minic_regs.h --cstyle defines --lang vhdl -K ../../sim/minic_regs.v mini_nic.wb