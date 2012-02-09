#!/bin/bash

mkdir -p doc
wbgen2 -D ./doc/wrc_syscon.html -p wrc_syscon_pkg.vhd -H record -V wrc_syscon_wb.vhd -C wrc_syscon_regs.h --cstyle defines --lang vhdl -K ../../sim/wrc_syscon_regs.vh wrc_syscon_wb.wb
