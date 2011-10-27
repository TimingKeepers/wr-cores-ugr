#!/bin/bash

mkdir -p doc
wbgen2 -D ./doc/pps_gen.html -V pps_gen_wb.vhd -C ../../../software/include/hw/pps_gen_regs.h --cstyle defines --lang vhdl -K ../../sim/pps_gen_regs.v wrsw_pps_gen.wb