action = "simulation"
files = "main.sv"
fetchto = "../../../ip_cores"

vlog_opt="+incdir+../../../sim +incdir+../../../sim/fabric_emu"

modules ={"git" : [    "git@ohwr.org:hdl-core-lib/general-cores.git" ],
					"local" : ["../../../modules/wr_endpoint", 
					           "../../../modules/timing",
					           "../old_ep",
					           "../../../platform/xilinx/wr_gtp_phy" ] };