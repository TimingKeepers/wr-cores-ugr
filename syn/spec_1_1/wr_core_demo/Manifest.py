target = "xilinx"
action = "synthesis"

fetchto = "../../../ip_cores"

syn_device = "xc6slx45t"
syn_grade = "-3"
syn_package = "fgg484"
syn_top = "spec_top"
syn_project = "spec_top_wrc.xise"

modules = { "local" : 
						[ "../../../top/spec_1_1/wr_core_demo", 
							"../../../platform",
							"../../../ip_cores/general-cores",
							"../../../ip_cores/etherbone-core",
							"../../../ip_cores/gn4124-core"] 
					}
