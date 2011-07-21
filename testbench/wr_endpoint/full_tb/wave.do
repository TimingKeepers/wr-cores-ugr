onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -format Logic /main/gtp_tx_k
add wave -noupdate -format Logic /main/gtp_tx_enc_error
add wave -noupdate -format Logic /main/gtp_tx_disparity2
add wave -noupdate -format Logic /main/gtp_tx_disparity
add wave -noupdate -format Literal /main/gtp_tx_data
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {77041921380 fs} 0}
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
