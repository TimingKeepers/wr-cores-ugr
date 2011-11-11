onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /main/DUT/g_class_mask
add wave -noupdate /main/DUT/g_our_ethertype
add wave -noupdate /main/DUT/clk_sys_i
add wave -noupdate /main/DUT/rst_n_i
add wave -noupdate /main/DUT/snk_cyc_i
add wave -noupdate /main/DUT/snk_stb_i
add wave -noupdate /main/DUT/snk_sel_i
add wave -noupdate /main/DUT/snk_adr_i
add wave -noupdate /main/DUT/snk_dat_i
add wave -noupdate /main/DUT/snk_we_i
add wave -noupdate /main/DUT/snk_stall_o
add wave -noupdate /main/DUT/snk_ack_o
add wave -noupdate /main/DUT/snk_err_o
add wave -noupdate /main/DUT/src_cyc_o
add wave -noupdate /main/DUT/src_stb_o
add wave -noupdate /main/DUT/src_dat_o
add wave -noupdate /main/DUT/src_adr_o
add wave -noupdate /main/DUT/src_we_o
add wave -noupdate /main/DUT/src_ack_i
add wave -noupdate /main/DUT/src_err_i
add wave -noupdate /main/DUT/src_sel_o
add wave -noupdate /main/DUT/src_stall_i
add wave -noupdate /main/DUT/master_cyc_o
add wave -noupdate /main/DUT/master_we_o
add wave -noupdate /main/DUT/master_stb_o
add wave -noupdate /main/DUT/master_sel_o
add wave -noupdate /main/DUT/master_adr_o
add wave -noupdate /main/DUT/master_dat_o
add wave -noupdate /main/DUT/master_dat_i
add wave -noupdate /main/DUT/master_ack_i
add wave -noupdate /main/DUT/src_out
add wave -noupdate /main/DUT/src_in
add wave -noupdate /main/DUT/snk_out
add wave -noupdate /main/DUT/snk_in
add wave -noupdate /main/DUT/master_out
add wave -noupdate /main/DUT/master_in
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {67408050000 fs} 0}
configure wave -namecolwidth 183
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
WaveRestoreZoom {125312500 ps} {256562500 ps}
