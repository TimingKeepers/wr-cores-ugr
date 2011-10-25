action = "simulation"
files = "main.sv"
#fetchto = "../../ip_cores"

vlog_opt="+incdir+../../sim"

modules ={"local" : ["../../ip_cores/general-cores",
                     "../../modules/wr_endpoint", 
                     "../../modules/wr_mini_nic" ] };
