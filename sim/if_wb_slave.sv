`timescale 1ns/1ps

`include "if_wishbone_defs.sv"



interface IWishboneSlave
  (
   input clk_i,
   input rst_n_i
   );

   parameter g_addr_width  = 32;
   parameter g_data_width  = 32;
   
   
   wire [g_addr_width - 1: 0] adr;
   wire [g_data_width - 1: 0] dat_i;
   wire [(g_data_width/8)-1 : 0] sel; 
   logic [g_data_width - 1 : 0] dat_o;
   logic ack;
   logic stall;
   logic err;
   logic rty;
   wire	cyc;
   wire stb;
   wire we;



   time last_access_t  = 0;

   modport slave
     (
      input adr,
      input dat_o,
      input sel,
      input cyc,
      input stb,
      input we,
      output ack,
      output dat_i,
      output stall,
      output err,
      output rty
      );

   wb_cycle_t c_queue[$];
   wb_cycle_t current_cycle;

   reg cyc_prev;
   int trans_index;
   int first_transaction;

   struct {
      wb_cycle_type_t mode;
      int gen_random_stalls;
      real stall_prob;

   } settings;


   function automatic int _poll(); return poll(); endfunction
   task automatic _get(output wb_cycle_t xfer); get(xfer); endtask

class CIWBSlaveAccessor extends CWishboneAccessor;

   function automatic int poll();
      return _poll();
   endfunction
	
   task get(output wb_cycle_t xfer);
      _get(xfer);
   endtask
      
   task clear();
   endtask // clear
      
   endclass // CIWBSlaveAccessor
   

   function CIWBSlaveAccessor get_accessor();
      CIWBSlaveAccessor tmp;
      tmp  = new;
      return tmp;
   endfunction // get_accessor
      
   
   function automatic int poll();
      return c_queue.size() != 0;
   endfunction // poll
      
   task automatic get(output wb_cycle_t xfer);
      while(c_queue.size() <= 0)
	@(posedge clk_i);
	
      xfer 			    = c_queue.pop_front();
   endtask // pop_cycle


   always@(posedge clk_i) cyc_prev <= cyc;
   wire cyc_start 		    = !cyc_prev && cyc;
   wire cyc_end 		    = cyc_prev && !cyc;


   task gen_random_stalls();
      if(settings.gen_random_stalls && probability_hit(settings.stall_prob))
	stall <= 1;
      else
	stall <= 0;
      
	
   endtask // gen_random_stalls
   
   task pipelined_fsm();

      if(cyc) begin
	 if(settings.gen_random_stalls)
	   gen_random_stalls();
	 end else
	   stall 	    <= 0;
      
      if(cyc_start) begin
	 current_cycle.data  = {};
	 trans_index 	    <= 0;
	 first_transaction   = 1;
      end

      if(cyc_end) begin
	 c_queue.push_back(current_cycle);
      end

      if(stb && we) begin
	 wb_xfer_t d;

	 d.a 	 = adr;
	 d.d 	 = dat_i;
	 d.sel [g_data_width/8-1:0] = sel;
	 
	 d.size  = g_data_width; /* fixme */

	 current_cycle.data.push_back(d);

	// $display("ifWb: write a %x d %x sel %x", adr, dat_i, sel);
	 ack <= 1;
	 
      end else if(stb && !we) begin
	 $error("Sorry, no pipelined read for slave yet implemented");
	ack 			<= 0;
      end else
	ack 			<= 0;
      
   endtask // pipelined_fsm
      
   always@(posedge clk_i)
     begin
	if(!rst_n_i)
	  begin
	     c_queue 		 = {};
	     current_cycle.data  = {};
	     trans_index 	 = 0;
	     ack 		<= 0;
	     rty 		<= 0;
	     err 		<= 0;
	     dat_o 		<= 0;
	     stall 		<= 0;
	     
	  end else begin
	     if(settings.mode == PIPELINED)
		  pipelined_fsm();
	     end
     end
   
   initial begin
      settings.mode 		  = PIPELINED;
      settings.gen_random_stalls  = 1;
      settings.stall_prob 	  = 0.1;
   end
   
   
   
endinterface // IWishboneSlave
