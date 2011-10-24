onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /main/clk_sys
add wave -noupdate /main/clk_ref
add wave -noupdate /main/clk_tdc
add wave -noupdate /main/rst_n
add wave -noupdate /main/trig_a
add wave -noupdate /main/trig_cal
add wave -noupdate /main/acam_wr_n
add wave -noupdate /main/acam_cs_n
add wave -noupdate /main/acam_rd_n
add wave -noupdate /main/acam_oe_n
add wave -noupdate /main/acam_adr
add wave -noupdate /main/acam_data
add wave -noupdate /main/tdc_d_o
add wave -noupdate /main/tdc_d_oe
add wave -noupdate /main/acam_start_dis
add wave -noupdate /main/acam_stop_dis
add wave -noupdate /main/acam_alutrigger
add wave -noupdate /main/trig_a_n_delayed
add wave -noupdate /main/tdc_start_delayed
add wave -noupdate /main/wr_utc
add wave -noupdate /main/wr_coarse
add wave -noupdate /main/wr_time_valid
add wave -noupdate /main/tdc_start_div
add wave -noupdate /main/tdc_start
add wave -noupdate /main/wr_time_valid_d0
add wave -noupdate /main/acam_ef1
add wave -noupdate /main/delay_len
add wave -noupdate /main/delay_pulse
add wave -noupdate /main/delay_val
add wave -noupdate /main/d_out
add wave -noupdate /main/spi_loop
add wave -noupdate /main/c_coarse_range
add wave -noupdate /main/fd_drv
add wave -noupdate /main/wb
add wave -noupdate /main/prev
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {20442364860 fs} 0}
configure wave -namecolwidth 413
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
WaveRestoreZoom {0 fs} {6754054050 fs}
