#!/bin/bash

mkdir -p doc
wbgen2 -D ./doc/obp_regs.html -V obp_wb_slave.vhd --cstyle defines --lang vhdl -p obp_wbgen2_pkg.vhd --hstyle record obp_wb_slave.wb 
