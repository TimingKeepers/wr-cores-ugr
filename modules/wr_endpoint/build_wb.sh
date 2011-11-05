#!/bin/bash

mkdir -p doc
wbgen2 -D ./doc/wrsw_endpoint.html -C endpoint_regs.h -p ep_registers_pkg.vhd -H record -V ep_wishbone_controller.vhd  --cstyle struct --lang vhdl -K ../../sim/endpoint_regs.v ep_wishbone_controller.wb
wbgen2 -D ./doc/wrsw_endpoint_mdio.html -V ep_pcs_tbi_mdio_wb.vhd --cstyle defines --lang vhdl -K ../../sim/endpoint_mdio.v pcs_regs.wb
