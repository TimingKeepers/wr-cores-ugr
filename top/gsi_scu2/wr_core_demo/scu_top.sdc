derive_pll_clocks -create_base_clocks
create_clock -period 33Mhz -name LPC_FPGA_CLK [get_ports {LPC_FPGA_CLK}]
create_clock -period 100Mhz -name pcie_refclk_i [get_ports {pcie_refclk_i}]
derive_clock_uncertainty

# cut the clock domains from each other
set_clock_groups -asynchronous \
 -group { altera_reserved_tck                      } \
 -group { clk_20m_vcxo_i       dmtd_clk_pll_inst|* } \
 -group { clk_125m_pllref_p    sys_pll_inst|*      } \
 -group { L_CLKp               ddr3_stub|*         } \
 -group { pcie_refclk_i        PCIe|*              } \
 -group {                      wr_gxb_phy*         } \
 -group { LPC_FPGA_CLK                             }

# these paths are supposedly made safe by Tom's sync_ffs. i have my doubts, but ...
set_false_path -from {xwr_core:U_WR_CORE|wr_core:WRPC|xwr_pps_gen:PPS_GEN|wr_pps_gen:WRAPPED_PPSGEN|adj_utc*} \
               -to   {xwr_core:U_WR_CORE|wr_core:WRPC|xwr_pps_gen:PPS_GEN|wr_pps_gen:WRAPPED_PPSGEN|cntr_utc*}
