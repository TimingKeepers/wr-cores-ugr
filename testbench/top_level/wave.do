onerror {resume}
quietly WaveActivateNextPane {} 0
#add wave -noupdate /main/DUT/U_VIC/g_interface_mode
#add wave -noupdate /main/DUT/U_VIC/g_address_granularity
#add wave -noupdate /main/DUT/U_VIC/g_num_interrupts
#add wave -noupdate /main/DUT/U_VIC/clk_sys_i
#add wave -noupdate /main/DUT/U_VIC/rst_n_i
#add wave -noupdate /main/DUT/U_VIC/slave_i
#add wave -noupdate /main/DUT/U_VIC/slave_o
#add wave -noupdate /main/DUT/U_VIC/irqs_i
#add wave -noupdate /main/DUT/U_VIC/irq_master_o

add wave -noupdate /main/clk_125m_pllref
add wave -noupdate /main/clk_20m_vcxo
add wave -noupdate /main/DUT/L_RST_N

add wave -noupdate /main/I_Gennum/ready
add wave -noupdate /main/I_Gennum/p2l_data
add wave -noupdate /main/I_Gennum/p2l_valid

add wave -noupdate /main/DUT/cmp_gn4124_core/p2l_data_i
add wave -noupdate /main/DUT/cmp_gn4124_core/p2l_valid_i
add wave -noupdate /main/DUT/cmp_gn4124_core/csr_adr_o
add wave -noupdate /main/DUT/cmp_gn4124_core/csr_dat_o
add wave -noupdate /main/DUT/cmp_gn4124_core/dma_adr_o
add wave -noupdate /main/DUT/cmp_gn4124_core/dma_dat_o

add wave -noupdate /main/DUT/dac_hpll_data
add wave -noupdate /main/DUT/dac_hpll_load_p1
add wave -noupdate /main/DUT/dac_cs1_n_o
add wave -noupdate /main/DUT/dac_cs2_n_o
add wave -noupdate /main/DUT/dac_clr_n_o
add wave -noupdate /main/DUT/dac_sclk_o
add wave -noupdate /main/DUT/dac_din_o

add wave -noupdate /main/DUT/wrc_slave_i.cyc
add wave -noupdate /main/DUT/wrc_slave_i.stb
add wave -noupdate /main/DUT/wrc_slave_i.adr
add wave -noupdate /main/DUT/wrc_slave_i.dat


TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {2367015 ps} 0}
configure wave -namecolwidth 150
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 1
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ns
update
WaveRestoreZoom {0 ps} {26250 ns}
