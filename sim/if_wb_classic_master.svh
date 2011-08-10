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

interface IWishboneClassicMaster
  (
   input clk_i,
   input rst_n_i
   );

   parameter g_data_width 	   = 32;
   parameter g_addr_width 	   = 32;
   

   /* Interface signals */
   logic [g_addr_width - 1 : 0] adr;
   logic [g_data_width - 1 : 0] dat_o;
   logic [3 : 0]       sel; // FIXME: 32-bit only
   wire [g_data_width - 1 : 0]       dat_i;
   wire  ack;
   logic cyc;
   logic stb;
   logic we;
   wire stall;

   initial begin
      adr    = 0;
      dat_o  = 0;
      sel     = 0;
      cyc     = 0;
      stb     = 0;
      we      = 0;
   end

 
   time last_access_t  = 0;
   reg [g_data_width-1:0] dummy;
   
   
// enables/disables displaying information about each read/write operation.

   int tb_verbose  = 0;
   
   
   task verbose(int onoff);
      tb_verbose = onoff;
   endtask // wb_verbose


   task classic_single_rw_generic;
      input [g_addr_width - 1 : 0] trans_addr;
      input [g_data_width - 1 : 0] trans_wdata;
      output [g_data_width - 1 : 0] trans_rdata;
      input rw;
      input [3:0] size;
      begin : rw_generic_main
	 if(tb_verbose && rw) 
	   $display("WB write %s: addr %x, data %x",
		    (size==1?"byte":((size==2)?"short":"int")), 
		    trans_addr, trans_wdata);

	 if($time != last_access_t) begin
	    @(posedge clk_i);
	 end
	 	 
	 stb<=1;
	 cyc<=1;
	 adr <= {2'b00, trans_addr[31:2]};
	 we <= rw;

	 if(rw) begin
	    case(size)
	      4: begin dat_o<=trans_wdata; sel <= 4'b1111; end
		 
	      2: begin
		 
		 if(adr[1]) begin
		    dat_o[31:16] <= trans_wdata[15:0];
		    sel <= 4'b1100;
		 end else begin
		    dat_o[15:0] <= trans_wdata[15:0];
		    sel <= 4'b0011;
		 end
	      end
	      1: begin
		 case(adr[1:0])
		   0: begin dat_o[31:24] <= trans_wdata[7:0]; sel <= 4'b1000; end
		   1: begin dat_o[23:16] <= trans_wdata[7:0]; sel <= 4'b0100; end
		   2: begin dat_o[15:8] <= trans_wdata[7:0]; sel <= 4'b0010; end
		   3: begin dat_o[7:0] <= trans_wdata[7:0]; sel <= 4'b0001; end
		 endcase // case(addr[1:0])
	      end
	     
	    endcase // case(size)
	 end // if (rw)
	 

	 @(posedge clk_i);
	 	 
	 if(ack == 0) begin
	    while(ack == 0) begin @(posedge clk_i); end
	 end

	 trans_rdata = dat_i;
 	 cyc <= 0;
	 we<=0;
	 stb<=0;

	 if(tb_verbose && !rw) 
	   $display("WB read %s: addr %x, data %x",
		    (size==1?"byte":((size==2)?"short":"int")), 
		    trans_addr, trans_rdata);


	 last_access_t = $time;
      end
   endtask // rw_generic

   task write32;
      input [g_addr_width - 1 : 0] addr;
      input [31 : 0] data_i;
      begin
	 classic_single_rw_generic(addr, data_i, dummy, 1, 4);
      end
   endtask // write32

   task read32;
      input [g_addr_width - 1 : 0] addr;
      output [31 : 0] data_o;
      begin : read32_body
	 reg [g_data_width - 1 : 0] rval;
	 classic_single_rw_generic(addr, 0, rval, 0, 4);
	 data_o = rval[31:0];
      end
   endtask // write32


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
      input stall);

endinterface // IWishbone
