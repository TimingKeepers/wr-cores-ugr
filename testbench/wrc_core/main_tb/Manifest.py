action = "simulation"
fetchto = "../../../ip_cores"
vlog_opt = "+incdir+../../../sim"

files = [ "main.sv" ]

modules = { "local" : [ 
        "../../../ip_cores/general-cores",
        "../../../modules/wr_endpoint",
        "../../../modules/wr_mini_nic",
        "../../../modules/wrc_core",
        "../../../modules/wrc_lm32",
        "../../../modules/wr_softpll",
        "../../../modules/wrsw_pps_gen",
        "../../../modules/timing"
        
        ]
            };



