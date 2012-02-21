fetchto = "../../../ip_cores"

modules = {
    "local" : "../../../modules/mini_bone",
    "svn" : "http://svn.ohwr.org/etherbone-core/hdl/EB_SPEC_Test"
};

files = ["scu_top.vhd", "pow_reset.vhd", "spec_serial_dac_arb.vhd",
	"spec_serial_dac.vhd"]
