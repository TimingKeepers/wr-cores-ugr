vlib work
vlog -nologo -sv main.sv +incdir+"." +incdir+../../sim
vcom -nologo  ../../ip_cores/general-cores/modules/wishbone/wishbone_pkg.vhd
vcom -nologo  ../../modules/wr_endpoint/wr_fabric_pkg.vhd
vcom -nologo  ../../modules/mini_bone/xmini_bone.vhd
vcom -nologo  ../../modules/mini_bone/mini_bone.vhd


vsim -L unisim -t 10fs work.main -voptargs="+acc"
set StdArithNoWarnings 1
set NumericStdNoWarnings 1
do wave.do
radix -hexadecimal
run 10ms
wave zoomfull
radix -hexadecimal
