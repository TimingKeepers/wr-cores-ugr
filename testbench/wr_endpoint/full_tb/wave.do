onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /main/U_Wrapped_EP/DUT/U_Rx_Path/dreq_pipe
add wave -noupdate /main/U_Wrapped_EP/DUT/U_Rx_Path/fab_pipe
add wave -noupdate /main/U_Wrapped_EP/DUT/U_Rx_Path/src_wb_o
add wave -noupdate /main/U_Wrapped_EP/DUT/U_Rx_Path/src_wb_i
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {199349192100 fs} 0}
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
WaveRestoreZoom {0 fs} {262500 ns}
