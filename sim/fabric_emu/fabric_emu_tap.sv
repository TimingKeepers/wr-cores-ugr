/* Linux TAP driver interface to WR fabric */

`timescale 1ns/1ps

`include "fabric_emu_defs.sv"

// module uses Linux TAP interface as a packet source/sink for the fabric simulator.
// link with VPI tap.sl

/* TODO:
 
   - add CTRL code generation
 
 */

class CPacketFIFO;


   int m_size, m_wrptr, m_rdptr, m_count, m_pktcnt;
   bit[8:0] m_buffer[];
   
   function new(int size);
      m_size 	= size;
      m_buffer 	= new [size];
      m_rdptr 	= 0;
      m_wrptr 	= 0;
      m_count 	= 0;
      m_pktcnt 	= 0;
   endfunction // new

   task _push(bit[8:0] val);
      if(m_count >= m_size)
	$error("FIFO overflow");

      m_buffer[m_wrptr++]  = val;
      m_count++;
      
   endtask // _push

   function bit[8:0] get();
      return m_buffer[m_rdptr];
   endfunction // _get

   function bit[8:0] pop();
      bit [8:0] rval;

      rval  = m_buffer[m_rdptr++];
      m_count--;
      return rval;
   endfunction

   function int empty();
      return (m_count == 0);
   endfunction // _empty
   
   

   task write(bit [7:0] val, bit eop);
//      $display("fwrite: %x %x", val, eop);
      
      _push({eop,val});
   endtask // write


   
   function int got_packet();
     return (m_pktcnt != 0 );
   endfunction // got_packets

   function int end_of_packet();
      
     bit[8:0] rval;
     
      
      rval =  get();
      return rval[8];
   endfunction // end_of_packet


   
endclass // packet_fifo



module fabric_emu_tap
  (
   input clk_sys_i,
   input rst_n_i,

  `WRF_FULL_PORTS_SINK(tx),
  `WRF_FULL_PORTS_SOURCE(rx)
   );

   reg [7:0] tap_rx;   
   reg tap_dvalid_rx;   
   reg [7:0] tap_tx = 0;
   reg tap_dvalid_tx = 0;

   CPacketFIFO rx_fifo;
   CPacketFIFO tx_fifo;

   const int c_FIFO_SIZE       = 32768;
   const int c_INTERFRAME_GAP  = 32768;

   assign tx_drop_o 	       = 0;
   
   
   initial begin
      rx_fifo 		       = new(c_FIFO_SIZE);
      tx_fifo 		       = new (c_FIFO_SIZE);
   end

   reg rx_sof_p_int 	       = 0;
   reg rx_eof_p_int 	       = 0;
   reg tx_dreq_int 	       = 0;
   reg [15:0] rx_data_int = 0;
   reg [3:0] rx_ctrl_int = 0;
   reg rx_error_p_int 	  = 0;
   reg rx_bytesel_int 	  = 0; 
   reg rx_valid_int 	  = 0;
   
   assign rx_sof_p1_o 	  = rx_sof_p_int;
   assign rx_eof_p1_o 	  = rx_eof_p_int;
   assign tx_dreq_o 	  = tx_dreq_int;
   assign rx_data_o 	  = rx_data_int;
   assign rx_ctrl_o 	  = rx_ctrl_int;
   assign rx_rerror_p1_o  = rx_error_p_int;
   assign rx_bytesel_o 	  = rx_bytesel_int;
   assign rx_valid_o 	  = rx_valid_int;
   
   assign rx_idle_o 	  = 0;
   assign rx_tabort_p1_o  = 0;

   assign tx_terror_p1_o  = 0;
   assign tx_rabort_p1_o  = 0;
   

   reg tap_dvalid_rx_d0   = 0;
   reg [7:0] tap_data_d0;
   
   
   always@(posedge clk_sys_i or negedge clk_sys_i)
     begin
	// TAP interface PLI call
	$tap_io(tap_tx, tap_dvalid_tx, tap_rx, tap_dvalid_rx);

	tap_dvalid_rx_d0 <= tap_dvalid_rx;

	// TAP reception
	if(tap_dvalid_rx_d0 && !tap_dvalid_rx)
	  rx_fifo.write(0, 1);
	else if (tap_dvalid_rx)
	  rx_fifo.write(tap_rx, 0);

	// TAP transmission
	if(!tx_fifo.empty())
	  begin
	     bit[7:0] data;
	     bit eof;
	     {eof, data}    = tx_fifo.pop();


//	     $display("TX: %x eof %b", data, eof);

	     tap_data_d0 	   <= data;
	     tap_tx 	   <= tap_data_d0;
	     tap_dvalid_tx  = !eof;
	  end
	

	
     end


   task automatic wait_clks(int howmuch);
      while(howmuch--) @(posedge clk_sys_i);
      endtask // automatic
   

   const int FRX_NOFRAME  = 1;
   const int FRX_TX       = 2;
   const int FRX_EOF      = 3;
   
   int fab_rx_state;
   int fab_rx_offset;

   function bit[3:0] gen_ctrl(int offset);
      if(offset >=0 && offset <= 2)
	return `c_wrsw_ctrl_dst_mac;
      else if(offset >=3 && offset <= 5)
	return `c_wrsw_ctrl_src_mac;
      else if(offset == 6)
	return `c_wrsw_ctrl_ethertype;
      else
	return `c_wrsw_ctrl_payload;
   endfunction // gen_ctrl
   
   
   task automatic fabric_do_rx();
    //  $display("FabDoRX");


      case (fab_rx_state)
	FRX_NOFRAME:begin
	   rx_eof_p_int     <= 0;
	   
	   if(!rx_fifo.empty()) begin
	      rx_sof_p_int  <= 1;
	      fab_rx_state   = FRX_TX;
	      fab_rx_offset  = 0;
	      
	      return;
	      
	   end
	end
	
	FRX_TX:begin
	   bit [8:0] lsb, msb;

	   rx_sof_p_int 	   <= 0;
	   
	   if(rx_fifo.end_of_packet()) begin
	  
	      rx_fifo.pop();
	      fab_rx_state <= FRX_EOF;
	      rx_valid_int   <= 0;
	      return;
	   end
	   
	   msb 		    = rx_fifo.pop();

	   if(rx_fifo.end_of_packet()) begin
	      rx_fifo.pop();
	      fab_rx_state <= FRX_EOF;
	      rx_valid_int   <= 1;
	      rx_data_int [15:8] <= msb[7:0];
	      rx_ctrl_int    <= gen_ctrl(fab_rx_offset++);
	      
	      rx_bytesel_int <= 1;
	   end else begin
	      lsb 	      = rx_fifo.pop();
	      rx_valid_int   <= 1;
	      rx_bytesel_int <= 0;
	      rx_data_int     = {msb[7:0], lsb[7:0] };
	      rx_ctrl_int    <= gen_ctrl(fab_rx_offset++);

	   end
	end

	FRX_EOF: begin
	   rx_eof_p_int 	<= 1;
	   wait_clks(1);
	   rx_eof_p_int 	<= 0;
	   fab_rx_state <= FRX_NOFRAME;
	   wait_clks(c_INTERFRAME_GAP);
	   end
	
      endcase // case (fab_rx_state)
   endtask // automatic
   

   task fabric_do_tx();
      bit[7:0] buffer[2048];
      int i, len ;

      i  = 0;
      
      
      while(1) begin
	 if(tx_valid_i) begin
// ignore OOB
	    if(tx_ctrl_i == `c_wrsw_ctrl_tx_oob || tx_ctrl_i == `c_wrsw_ctrl_rx_oob)
	      continue;

	    buffer[i++]  = tx_data_i[15:8];
	    if(!tx_bytesel_i)
	      buffer[i++]  = tx_data_i[7:0];
	 end

	 if(tx_eof_p1_i)
	   break;

	 wait_clks(1);
      end

  //    $display("FabTX: %d bytes", i);
      
      len 		 = i;

      for(i=0;i<len;i++)
	tx_fifo.write(buffer[i], 0);

      tx_fifo.write(0, 1);
   endtask // fabric_do_tx
   
   
   initial fab_rx_state  = FRX_NOFRAME;
   
   initial forever begin
     wait_clks(1);
      
      if(rx_dreq_i) 
	 fabric_do_rx();
      else
	rx_valid_int <= 0;
   end

   initial forever begin
      tx_dreq_int      <= 1;
      
      wait_clks(1);

      if(tx_sof_p1_i)
	fabric_do_tx();
   end

   
endmodule // tap_if

	      
	      