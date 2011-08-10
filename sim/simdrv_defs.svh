`ifndef SIMDRV_DEFS_SV
 `define SIMDRV_DEFS_SV 1

typedef longint unsigned uint64_t;


virtual class CBusAccessor;
   pure virtual task write32(int addr, bit[31:0] data); 
   pure virtual task read32(int addr, output bit[31:0] rdata);
endclass // CBusAccessor

`endif