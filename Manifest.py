fetchto = "ip_cores"

modules =  {"local" : 
											[ "modules/fabric",
												"modules/wr_tbi_phy",
												"modules/timing",
												"modules/wr_mini_nic",
												"modules/wr_softpll_ng",
												"modules/wr_endpoint",
												"modules/wr_pps_gen",
												"modules/wr_dacs",
												"modules/wr_eca",
												"modules/wr_tlu",
												"modules/wrc_core",
												"platform" ],
                      "git" : "git://ohwr.org/hdl-core-lib/general-cores.git"
						}
