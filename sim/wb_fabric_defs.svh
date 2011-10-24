`ifndef __WB_FABRIC_DEFS_SVH
 `define __WB_FABRIC_DEFS_SVH

const bit [2:0] WRF_STATUS = 3'b100;
const bit [2:0] WRF_DATA = 3'b000;
const bit [2:0] WRF_OOB = 3'b010;
const bit [2:0] WRF_USER = 3'b110;

const bit [3:0] WRF_OOB_TX_FID = 4'b0001;
const bit [3:0] WRF_OOB_RX_TIMESTAMP = 4'b0000;

`endif //  `ifndef __WB_FABRIC_DEFS_SVH

