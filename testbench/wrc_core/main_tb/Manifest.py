action = "simulation"
target = "xilinx"
fetchto = "../../../ip_cores"
vlog_opt = "+incdir+../../../sim"

files = [ "main.sv" ]

modules = { "local" : [ "../../..",
			"../../../modules/fabric",
			"../../../ip_cores/general-cores",
			"../../../ip_cores/etherbone-core",
			"../../../ip_cores/gn4124-core"]}



