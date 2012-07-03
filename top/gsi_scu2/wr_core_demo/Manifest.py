fetchto = "../../../ip_cores"

modules = {
    "local" : "../../../modules/mini_bone",
    "git" : "git://ohwr.org/hdl-core-lib/etherbone-core.git"
};

files = ["scu_top.vhd", "pow_reset.vhd", "spec_serial_dac_arb.vhd",
	"spec_serial_dac.vhd"]
