`ifndef IF_WISHBONE_ACCESSOR_SV
`define IF_WISHBONE_ACCESSOR_SV

`include "if_wishbone_types.svh"

virtual class CWishboneAccessor extends CBusAccessor;

   static int _null  = 0;

   
   // [slave only] checks if there are any transactions in the queue 
   virtual function automatic int poll();
      return 0;
   endfunction // poll

   // [slave only] adds a simulation event (e.g. a forced STALL, RETRY, ERROR)
   // evt = event type (STALL, ERROR, RETRY)
   // behv = event behavior: DELAYED - event occurs after a predefined delay (dly_start)
   //                        RANDOM - event occurs randomly with probability (prob)
   // These two can be combined (random events occuring after a certain initial delay)
   // DELAYED events can be repeated (rep_rate parameter)
   virtual task add_event(wba_sim_event_t evt, wba_sim_behavior_t behv, int dly_start, real prob, int rep_rate);

   endtask // add_event


   // [slave only] gets a cycle from the queue
   virtual task get(ref wb_cycle_t xfer);
      
   endtask // get

   // [master only] executes a cycle and returns its result
   virtual task put(ref wb_cycle_t xfer);

   endtask // put
   
   virtual function int idle();
      return 1;
   endfunction // idle
   
   // [master only] generic write(s), blocking
   virtual task writem(uint64_t addr[], uint64_t data[], int size = 32, ref int result = _null);
      wb_cycle_t cycle;
      int i;
//      xfer.ctype  = PIPELINED;
      cycle.rw  = 0;
      
      for(i=0;i < addr.size(); i++)
        begin
           wb_xfer_t xfer;
           xfer.a     = addr[i];
           xfer.d     = data[i];
           xfer.size  = size;
           cycle.data.push_back(xfer);
        end

      put(cycle);
      get(cycle);
      result  = cycle.result;
      
   endtask // write

   // [master only] generic read(s), blocking
   virtual task readm(uint64_t addr[], ref uint64_t data[],input int size = 32, ref int result = _null);

   endtask // read

/* -----\/----- EXCLUDED -----\/-----
   virtual task read(uint32_t addr, ref uint32_t data, int size = 32, ref int result = _null);
      uint64_t aa[1], da[1];
      aa[0]  = addr;
      read(aa, da, size, result);
      data  = da[0];
   endtask


   virtual task write(uint32_t addr, ref uint32_t data, int size = 32, ref int result = _null);
      uint64_t aa[1], da[1];
      aa[0]  = addr;
      da[1]  = data;
      write(aa, da, size, result);
   endtask
 -----/\----- EXCLUDED -----/\----- */
   
endclass // CWishboneAccessor

static int seed = 0;

function automatic int probability_hit(real prob);
   real rand_val;
   rand_val 	= real'($dist_uniform(seed, 0, 1000)) / 1000.0;
      
   if(rand_val < prob)
     return 1;
   else
     return 0;
    
endfunction // probability_hit


`endif //  `ifndef IF_WISHBONE_ACCESSOR_SV

