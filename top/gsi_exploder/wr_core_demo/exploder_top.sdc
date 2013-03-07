derive_pll_clocks -create_base_clocks
derive_clock_uncertainty

# cut the clock domains from each other
set_clock_groups -asynchronous                \
 -group { altera_reserved_tck               } \
 -group { clk_20m_vcxo_i    dmtd_inst|*     } \
 -group { clk_125m_pllref_i ref_inst|*      } \
 -group { clk_125m_local_i  sys_inst|*      } \
 -group { wr_gxb_phy*                       }
