derive_pll_clocks -create_base_clocks
create_clock -period 33Mhz -name LPC_FPGA_CLK [get_ports {LPC_FPGA_CLK}]
create_clock -period 100Mhz -name pcie_refclk_i [get_ports {pcie_refclk_i}]
create_clock -period 125Mhz -name L_CLKp [get_ports {L_CLKp}]
derive_clock_uncertainty

# cut the clock domains from each other
set_clock_groups -asynchronous  \
 -group { altera_reserved_tck } \
 -group { dmtd_pll_inst|*     } \
 -group { sys_pll_inst|*      } \
 -group { L_CLKp              } \
 -group { PCIe|*              } \
 -group { wr_gxb_phy*         } \
 -group { LPC_FPGA_CLK        }
