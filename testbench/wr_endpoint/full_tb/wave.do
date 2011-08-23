onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/clk_sys_i
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/rst_n_i
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/snk_fab_i
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/snk_dreq_o
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/src_fab_o
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/src_dreq_i
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/done_o
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/pclass_o
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/drop_o
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/regs_b
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/pc
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/ir
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/insn
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/done_int
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/regs
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/result_cmp
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/mask
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/ra
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/rb
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/rc
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/result1
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/result2
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/rd
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/pmem_addr
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/pmem_rdata
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/mm_addr
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/mm_write
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/mm_rdata
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/mm_wdata
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/stage1
add wave -noupdate /main/DUT/U_Rx_Deframer/gen_with_packet_filter/U_packet_filter/stage2
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {14267727040 fs} 0}
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
WaveRestoreZoom {0 fs} {84 us}
