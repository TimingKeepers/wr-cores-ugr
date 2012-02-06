action = "simulation"
files = "main.sv"
fetchto = "../../../ip_cores"

vlog_opt="+incdir+../../sim"

modules ={"local" : ["../../modules/mini_bone", 
					           "../../" ] };

