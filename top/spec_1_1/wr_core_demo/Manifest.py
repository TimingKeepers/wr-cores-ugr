files = ["spec_top.vhd", "spec_top.ucf", "spec_serial_dac.vhd", "spec_serial_dac_arb.vhd", "wb_gpio_port_notristates.vhd"]

fetchto = "../../../ip_cores"

modules = {
    "local" : ["../../../",
               "../../../modules/mini_bone"],
    "svn" : [ "http://svn.ohwr.org/gn4124-core/branches/hdlmake-compliant/rtl" ]
    }
