derive_pll_clocks -create_base_clocks

derive_clock_uncertainty

# connected to SOPC
#create_clock -period 125Mhz -name F_PLL_6p [get_ports {F_PLL_6p}]

create_clock -period 125Mhz -name clk_125m_pllref_p [get_ports {clk_125m_pllref_p}]
create_clock -period 125Mhz -name L_CLKp [get_ports {L_CLKp}]
create_clock -period 125Mhz -name F_PLL_6p [get_ports {F_PLL_6p}]
create_clock -period 20Mhz -name clk_20m_vcxo_i [get_ports {clk_20m_vcxo_i}]