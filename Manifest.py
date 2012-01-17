fetchto = "ip_cores"

modules =  {"local" : 
											[ "platform/xilinx/wr_gtp_phy",
												"modules/fabric",
												"modules/wr_tbi_phy",
												"modules/timing",
												"modules/wr_mini_nic",
												"modules/wr_softpll",
												"modules/wr_endpoint",
												"modules/wr_pps_gen",
												"modules/wrc_core" ],
						"git" : "git://ohwr.org/hdl-core-lib/general-cores.git::wishbone_with_adapter"
						}
												
										
												