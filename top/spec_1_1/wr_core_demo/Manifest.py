files = ["spec_top.vhd", "spec_top.ucf", "spec_reset_gen.vhd"]

fetchto = "../../../ip_cores"

modules = {
    "local" : ["../../../"],
    "git" : "git://ohwr.org/hdl-core-lib/etherbone-core.git",
    "svn" : [ "http://svn.ohwr.org/gn4124-core/trunk/hdl/gn4124core/rtl" ]
    }
