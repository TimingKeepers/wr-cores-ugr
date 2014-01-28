target = "xilinx"
action = "synthesis"

fetchto = "../../../ip_cores"

syn_device = "xc7vx690t"
syn_grade = "-2"
syn_package = "ffg1761"
syn_top = "spec_top"
syn_project = "spec_top_wrc.xise"

modules = { "local" : 
						[ "../../../top/spec_1_1/wr_core_demo", 
							"../../../platform",
							"../../../ip_cores/general-cores",
							"../../../ip_cores/etherbone-core",
							"../../../ip_cores/gn4124-core"] 
					}
