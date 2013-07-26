#!/bin/bash

mkdir -p doc
wbgen2 -D ./doc/pps_gen.html -V pps_gen_wb.vhd -C pps_gen_regs.h --cstyle struct --lang vhdl -K ../../sim/pps_gen_regs.v pps_gen_wb.wb
