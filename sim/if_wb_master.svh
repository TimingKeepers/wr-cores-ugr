//
// Title          : Software Wishbone master unit for testbenches
//
// File           : if_wishbone.sv
// Author         : Tomasz Wlostowski <tomasz.wlostowski@cern.ch>
// Created        : Tue Mar 23 12:19:36 2010
// Standard       : SystemVerilog
//

// Default values of certain WB parameters.

`include "simdrv_defs.sv"

`include "if_wishbone_defs.sv"

interface IWishboneMaster
  (
   input clk_i,
   input rst_n_i
   );

   parameter g_data_width 	   = 32;
   parameter g_addr_width 	   = 32;
   
   logic [g_addr_width - 1 : 0] adr;
   logic [g_data_width - 1 : 0] dat_o;
   logic [(g_data_width/8)-1 : 0] sel; 
   wire [g_data_width - 1 : 0] dat_i;
   wire ack;
   wire stall;
   wire err;
   wire rty;
   logic	cyc;
   logic 	stb;
   logic 	we;

   wire clk;
   wire rst_n;
 
   time last_access_t 	  = 0;

   struct {
      wb_cycle_type_t mode;
      int gen_random_throttling;
      real throttle_prob;
   } settings;
   

   modport master 
     (
      output adr,
      output dat_o,
      output sel,
      output cyc,
      output stb,
      output we,
      input ack,
      input dat_i,
      input stall,
      input err,
      input rty
      );

   function automatic logic[g_addr_width-1:0] gen_addr(uint64_t addr, int xfer_size);
      case(xfer_size)
	1: return addr;
	2: return addr << 1;
	4: return addr << 2;
	8: return addr << 3;
	default: $error("IWishbone: invalid WB transfer size [%d bytes]\n", xfer_size);
      endcase // case (xfer_size)
   endfunction

   // FIXME: little-endian
   function automatic logic[(g_data_width/8)-1:0] gen_sel(uint64_t addr, int xfer_size);
      logic [(g_data_width/8)-1:0] sel;

      sel  = (1<<xfer_size)-1;
      
      return sel << (addr % xfer_size);
      endfunction

   function automatic logic[(g_data_width/8)-1:0] gen_data(uint64_t addr, int xfer_size);
      logic [(g_data_width/8)-1:0] sel;

      sel  = (1<<xfer_size)-1;
      
      return sel << (addr % xfer_size);
   endfunction // gen_data

   function automatic uint64_t decode_data(uint64_t addr, int xfer_size, logic[g_data_width-1:0] data);
      int rem;

      rem  = addr % xfer_size;
      return (data[rem] >> (8*rem)) & (1<<(xfer_size*8-1));
   endfunction // decode_data
   

   task automatic classic_cycle 
     (
      wb_xfer_t xfer[],
      bit rw,
      int n_xfers,
      output wb_cycle_result_t result
      );
      
      int i;
      
      if($time != last_access_t) 
	    @(posedge clk_i); /* resynchronize, just in case */
      
      for(i=0;i<n_xfers;i++)
	begin
	      
	   stb 	 <= 1'b1;
	   cyc 	 <= 1'b1;
	   adr  <= /*gen_addr(*/xfer[i].a;/*, xfer[i].size);*/
	   we 	 <= rw;
	   sel 	 <= xfer[i].sel[g_data_width/8-1:0];
//gen_sel(xfer[i].a, xfer[i].size);
	   dat_o <= gen_data(xfer[i].a, xfer[i].d);
	   
	   @(posedge clk_i);
	 	 
	   if(ack == 0) begin
	      while(ack == 0) begin @(posedge clk_i); end
	   end else if(err == 1'b1 || rty == 1'b1)
	     begin
		cyc    <= 0;
		we     <= 0;
		stb    <= 0;
		result 	= (err ==1'b1 ? R_ERROR: R_RETRY);
		break;
	     end

	   xfer[i].d 	 = decode_data(xfer[i].a, xfer[i].d, xfer[i].size);
	      
 	   cyc 		 <= 0;
	   we 		 <= 0;
	   stb 		 <= 0;
	   
	end // if (ack == 0)
      
      @(posedge clk_i);
      
      result 	     = R_OK;
      last_access_t  = $time;
   endtask // automatic

   reg xf_idle 	     = 1;
   

   task automatic pipelined_write_cycle 
     (
      wb_xfer_t xfer[],
      int n_xfers,
      output wb_cycle_result_t result
      );
      
      int i;
      int ack_count ;
      int failure ;

      ack_count  = 0;
      failure 	 = 0;

      xf_idle 	 = 0;
      
      
      if($time != last_access_t) 
	@(posedge clk_i); /* resynchronize, just in case */

      while(stall)
	@(posedge clk_i);
      
      cyc <= 1'b1;
      i    =0;
      
      while(i<n_xfers)
	begin

	   if(stb && !ack) 
	     ack_count++;
	   else if(!stb && ack) 
	     ack_count--;

	   if(err) begin
	      result   = R_ERROR;
	      failure  = 1;
	      break;
	   end
	
	   if(rty) begin
	      result   = R_RETRY;
	      failure  = 1;
	      break;
	      end
	   
	   if (stall || (settings.gen_random_throttling && probability_hit(settings.throttle_prob))) begin
	      stb <= 1'b0;
	      
	      @(posedge clk_i);
	      end else begin
	      adr   <= xfer[i].a;
//gen_addr(xfer[i].a, xfer[i].size);
	      stb   <= 1'b1;
	      we    <= 1'b1;
	      sel   <= xfer[i].sel[g_data_width/8-1:0];
//gen_sel(xfer[i].a, xfer[i].size);
	      dat_o <= xfer[i].d;
//		 $display("wbWrite: a %x d %x\n", xfer[i].a, xfer[i].d);
		 i++;
		 
	      @(posedge clk_i);
	   end

	end // for (i=0;i<n_xfers;i++)

      while((ack_count > 0) && !failure)
	begin

	   if(err) begin
	      result   = R_ERROR;
	      failure  = 1;
	      break;
	   end
	
	   if(rty) begin
	      result   = R_RETRY;
	      failure  = 1;
	      break;
	   end

	   if(stb && !ack) 
	     ack_count++;
	   else if(!stb && ack) 
	     ack_count--;
	   @(posedge clk_i);
	   end

      cyc 	    <= 1'b0;
      @(posedge clk_i);
      
      result 	     = R_OK;
      xf_idle 	     = 1;
      last_access_t  = $time;
   endtask // automatic

	
   wb_cycle_t request_queue[$];
   wb_cycle_t result_queue[$];
   

class CIWBMasterAccessor extends CWishboneAccessor;

   function automatic int poll();
      return 0;
   endfunction
	
   task get(output wb_cycle_t xfer);
      while(!result_queue.size())
	@(posedge clk_i);
      xfer  = result_queue.pop_front();
   endtask
      
   task clear();
   endtask // clear

   task put(input wb_cycle_t xfer);
    //  $display("wbMasteR: put");
      
      request_queue.push_back(xfer);
   endtask // put

   function int idle();
      return (request_queue.size() == 0) && xf_idle;
   endfunction // idle
endclass // CIWBMasterAccessor
   
   

   function CIWBMasterAccessor get_accessor();
      CIWBMasterAccessor tmp;
      tmp  = new;
      return tmp;
      endfunction // get_accessoror

   always@(posedge clk_i)
     if(!rst_n_i)
       begin
	  request_queue 	      = {};
	  result_queue 		      = {};
	  xf_idle 		      = 1;
	  cyc 			     <= 0;
	  dat_o 		     <= 0;
	  stb 			     <= 0;
	  sel 			     <= 0;
	  adr 			     <= 0;
	  we 			     <= 0;
       end

   initial begin
      settings.mode 		      = PIPELINED;
      settings.gen_random_throttling  =1;
      settings.throttle_prob 	      = 0.01;
      

   end

    
   
   initial forever
     begin
	@(posedge clk_i);
	
	
	if(request_queue.size() > 0)
	  begin
	     wb_cycle_t c;

//	     $display("wbMaster: got cycle [%d]", c.data.size());

	     c 	= request_queue.pop_front();

	     
	     
	     if(settings.mode == PIPELINED)
	       begin
		  wb_cycle_result_t res;
		  pipelined_write_cycle(c.data, c.data.size(), res);
		  c.result  =res;
		  c.data    = {};
		  
		  result_queue.push_back(c);
	       end
	  end
     end
   
   
endinterface // IWishbone
