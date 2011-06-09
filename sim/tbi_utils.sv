
`timescale 1ps/1ps
// Clock/reset generator module for the TBI interface.
module tbi_clock_rst_gen
  (
   output clk_ref_o,
   output clk_sys_o,
   output phy_rbclk_o,
   output rst_n_o);

   parameter g_rbclk_period   = 8010;
   parameter g_refclk_period  = 8000;
   parameter g_sysclk_period  = 15900;
   
   reg refclk 		      = 0, refclk2 = 0, rbclk = 0, rst_n = 0;

   always #(g_rbclk_period/2) rbclk <= ~rbclk;
   always #(g_refclk_period/2) refclk <= ~refclk;
   always #(g_sysclk_period/2) refclk2 <= ~refclk2;
//   always@(posedge refclk) refclk2 <= ~refclk2;
   initial begin repeat(10) @(posedge refclk2); rst_n = 1; end

   assign clk_ref_o   = refclk;
   assign clk_sys_o  = refclk2;
   assign phy_rbclk_o    = rbclk;
   assign rst_n_o    = rst_n;
endmodule // tbi_clock_gen


`timescale 1ns/1ps
// Clock alignment FIFO for looping back the endpoint TX/RX path
module tbi_loopback_fifo
  (
   input tx_clk_i,
   input rx_clk_i,
   input [9:0] tx_data_i,
   output reg [9:0] rx_data_o
   );

   parameter g_buf_size  = 20000;
   parameter g_error_prob  = 0;
   
   function automatic int probability_hit(int prob, int max_prob);
      
     int rand_val;

      rand_val 	= $random % (max_prob+1);
      if(rand_val < 0) rand_val = -rand_val;

      if(rand_val < prob) 
	return 1;
      else
	return 0;
   endfunction // probability_hit
   
   
   reg[9:0] buffer[100000];
   int write_ptr, read_ptr, count;

   initial begin
      write_ptr 	 = 0;
      read_ptr 		 = 0;
      count 		 = 0;
   end
		   
   always@(posedge tx_clk_i) begin
      buffer[write_ptr]  <= tx_data_i;
      count++;
      write_ptr++;
      end

   always@(posedge rx_clk_i) begin
      if(count == 0) begin
	 $display("loopback FIFO underrun!");
	 rx_data_o   <= 0;
	 
      end else begin
	 if(probability_hit(g_error_prob, 1000))
	   rx_data_o <= 'hfff;
	 else
	   rx_data_o <= buffer[read_ptr];
	 
	 read_ptr++;
	 count--;
      end 
   end
   
endmodule 
