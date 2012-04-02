fetchto = "../../../ip_cores"

modules = {
    "local" : "../../../modules/mini_bone",
    "git" : "git://ohwr.org/hdl-core-lib/etherbone-core.git"
};

files = ["exploder_top.sdc", "exploder_top.vhd", "pow_reset.vhd", "spec_serial_dac.vhd", "flash_loader.vhd"]
