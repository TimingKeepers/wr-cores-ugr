#!/bin/bash

mkdir -p doc 
wbgen2 -D ./doc/wrsw_pps_gen.html -V pps_gen_wb.vhd --cstyle struct -C pps_gen_regs.h --lang vhdl -K ../../sim/pps_gen_regs.v wrsw_pps_gen.wb
