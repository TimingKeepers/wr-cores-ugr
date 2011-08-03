`timescale 1ns/1ps

`include "../../../sim/endpoint_regs.v"
`include "../../../sim/endpoint_mdio.v"
`include "../../../sim/tbi_utils.sv"
`include "../../../sim/fabric_emu/fabric_emu.sv"

`timescale 1ps/1ps

`define EP_QMODE_ACCESS 0
`define EP_QMODE_TRUNK 1
`define EP_QMODE_UNQ 3

const int c_RBCLK_PERIOD   = 8010;
const int c_REFCLK_PERIOD  = 8000;


interface old_endpoint_test_wrapper
  (
   input clk_sys_i,
   input clk_ref_i,
   input rst_n_i
   );

   `WRF_FULL_WIRES(toEP)
   `WRF_FULL_WIRES(fromEP)
   
   wire txtsu_valid, txtsu_ack;
   wire [4:0] txtsu_pid;
   wire [15:0] txtsu_fid;
   wire [31:0] txtsu_timestamp;

   
   
   wire[7:0] gtx_data;
   wire gtx_k;
   wire gtx_disparity;
   wire gtx_enc_error;
   wire [7:0] grx_data;
   wire grx_clk;
   wire grx_k;
   wire grx_enc_error;
   wire [3:0] grx_bitslide;

   modport gtp
     (
      output gtx_data,
      output gtx_k,
      output gtx_disparity,
      output gtx_enc_error,

      input grx_data,
      input grx_clk,
      input grx_k,
      input grx_enc_error,
      input [3:0] grx_bitslide
      );


   WB_TEST_MASTER  WB
     (
      .clk_i(clk_sys_i),
      .rst_n_i(rst_n_i)
      );
   
   
   fabric_emu EMU
      (
       .clk_i(clk_sys_i),
       .rst_n_i(rst_n_i),
    
       `WRF_FULL_CONNECT_SOURCE(rx, toEP),
       `WRF_FULL_CONNECT_SINK(tx, fromEP),

       .txtsu_port_id_i(txtsu_pid),
       .txtsu_fid_i (txtsu_fid),
       .txtsu_tsval_i  (txtsu_timestamp),
       .txtsu_valid_i (txtsu_valid),
       .txtsu_ack_o (txtsu_ack)
       );

   
   task mdio_write(int addr, int data);
      reg[31:0] rval;

      $display("MDIO_write %x %x", addr, data);
      
      WB.write32(`ADDR_EP_MDIO_CR, ((addr >> 2) << 16) | `EP_MDIO_CR_RW | data);
      while(1)begin
	 WB.read32(`ADDR_EP_MDIO_SR, rval);
	 if(rval & `EP_MDIO_SR_READY) break;
      end
   endtask // mdio_write

   task mdio_read(int addr, output int data);
      reg[31:0] rval;
      
      WB.write32(`ADDR_EP_MDIO_CR, (addr >> 2));
      while(1)begin
	 WB.read32(`ADDR_EP_MDIO_SR, rval);
	 if(rval & `EP_MDIO_SR_READY) begin
	    data  = rval[15:0];
	    break;
	 end
      end
   endtask // mdio_read
  
   task initialize_EP_regs();
      WB.verbose(0);
      WB.monitor_bus(0);

      $display("Initializing EP registers...");
      
      WB.write32(`ADDR_EP_ECR, `EP_ECR_RX_EN_FRA | `EP_ECR_TX_EN_FRA);
      WB.write32(`ADDR_EP_RFCR, 3 << `EP_RFCR_QMODE_OFFSET); // QMODE = UNQUALIFIED
      WB.write32(`ADDR_EP_MACH, 'haabb);  // assign a dummy MAC address
      WB.write32(`ADDR_EP_MACL, 'hccddeeff);
      WB.write32(`ADDR_EP_TSCR, `EP_TSCR_EN_RXTS);

      mdio_write(`ADDR_MDIO_MCR, `MDIO_MCR_RESET);
      
   endtask // initialize_EP_regs

   old_wrsw_endpoint
     #(
       .g_interface_mode("SERDES"),
       .g_simulation(1)
       )
     DUT (
	  .clk_ref_i(clk_ref_i),
	  .clk_sys_i(clk_sys_i),
	  .clk_dmtd_i(1'b0),
	  .rst_n_i (rst_n_i),
	  .pps_csync_p1_i(1'b0),
	  
	  `_WRF_CONNECT_MANDATORY_SOURCE(rx, fromEP),
	  `_WRF_CONNECT_MANDATORY_SINK(tx, toEP),

	  .rtu_full_i(1'b0),
	  .rtu_almost_full_i(1'b0),
	  
	  .wb_cyc_i  (WB.wb_cyc),
	  .wb_stb_i  (WB.wb_stb),
	  .wb_we_i   (WB.wb_we),
	  .wb_sel_i  (WB.wb_bwsel),
	  .wb_addr_i (WB.wb_addr [5:0]),
	  .wb_data_i (WB.wb_data_o),
	  .wb_data_o (WB.wb_data_i),
	  .wb_ack_o  (WB.wb_ack),
	  
	  .txtsu_port_id_o(txtsu_pid),
	  .txtsu_frame_id_o (txtsu_fid),
	  .txtsu_tsval_o  (txtsu_timestamp),
	  .txtsu_valid_o (txtsu_valid),
	  .txtsu_ack_i (txtsu_ack),

// GTP I/F
          .gtp_rst_o(),
	  .gtp_tx_clk_i(clk_ref_i),
	  .gtp_tx_data_o (gtx_data),
	  .gtp_tx_k_o    (gtx_k),
	  .gtp_tx_disparity_i (gtx_disparity),
	  .gtp_tx_enc_err_i  (gtx_enc_error),
	
	  .gtp_rx_data_i     (grx_data),
	  .gtp_rx_clk_i    (grx_clk),
	  .gtp_rx_k_i        (grx_k),
	  .gtp_rx_enc_err_i  (grx_enc_error),
	  .gtp_rx_bitslide_i (grx_bitslide)
    );

   
endinterface // old_endpoint_test_wrapper


module main;

   wire clk_ref_old;
   wire clk_ref_new;
   wire clk_sys;
   wire rst_n;

  // WRF links
   
   wire [9:0] phy_td, phy_rd;
   wire phy_rbclk;

   tbi_clock_rst_gen
     #(
       .g_rbclk_period(8091))
     clkgen
       (
	.clk_ref_o(clk_ref_new),
	.clk_sys_o(clk_sys),
	.phy_rbclk_o(clk_ref_old),
	.rst_n_o(rst_n)
	);
   

   // sets the Q-mode of the endpoint
   task ep_set_qmode(int qmode);
      reg[31:0] rfcr;
      string s;
      
      case (qmode)
	`EP_QMODE_ACCESS: s="ACCESS"; 
	`EP_QMODE_TRUNK: s="TRUNK";
	`EP_QMODE_UNQ: s="UNQUALIFIED";
      endcase // case (qmode)
      
      
      $display("Setting qmode to: %s", s);
      
      WB.read32(`ADDR_EP_RFCR, rfcr);
      rfcr  = rfcr & (~`EP_RFCR_QMODE);
      rfcr  = rfcr | ( qmode << `EP_RFCR_QMODE_OFFSET);
      WB.write32(`ADDR_EP_RFCR, rfcr);

     endtask // ep_set_qmode
   

   // sets the VLAN ID/Priority for ACCESS port
   task ep_set_vlan(input [11:0] vid, input [2:0] prio);
      reg[31:0] rfcr;
      WB.read32(`ADDR_EP_RFCR, rfcr);
      rfcr  = rfcr & ~(`EP_RFCR_VID_VAL | `EP_RFCR_PRIO_VAL);
      rfcr  = rfcr | ( vid << `EP_RFCR_VID_VAL_OFFSET ) | ( prio << `EP_RFCR_PRIO_VAL_OFFSET);
      WB.write32(`ADDR_EP_RFCR, rfcr);
   endtask // ep_set_vlan
   
   task ep_size_check(int runts, int giants);
      reg[31:0] rfcr;

      WB.read32(`ADDR_EP_RFCR, rfcr);
      rfcr 	       = rfcr & ~(`EP_RFCR_A_RUNT | `EP_RFCR_A_GIANT);
      if(!runts) rfcr  = rfcr | `EP_RFCR_A_RUNT;
      if(!giants) rfcr  = rfcr | `EP_RFCR_A_GIANT;
      WB.write32(`ADDR_EP_RFCR, rfcr);
   endtask // ep_frame_check
   
   
   ether_header_t hdr;



   initial $dumpfile("dump.vcd");
   initial $dumpvars(0, main);
   
   
`define NUM_TEST_VECTORS 13
   
   test_vector_t test_vectors[]  = 
      '{
       // tx_error rx_error ep_assigned_mac is_vlan vid    prio  tx_oob rx_oob

// regular frames of min/max size
       '{0,          0,       0,              0,      0,     0,    0,     0},
       '{0,          0,       0,              0,      0,     0,    0,     0},	       	        
// runt and giant (check enabled)
       '{0,          1,       0,              0,      0,     0,    0,     0},	       	               '{0,          1,       0,              0,      0,     0,    0,     0},	    
// runt and giant (check disabled)
       '{0,          0,       0,              0,      0,     0,    0,     0},	       	               '{0,          0,       0,              0,      0,     0,    0,     0},	    
// normal frames (min and max size, check enabled)
       '{0,          0,       0,              0,      0,     0,    0,     0},	       	               '{0,          0,       0,              0,      0,     0,    0,     0},
       // runt and giant (check disabled)
       '{0,          0,       0,              0,      0,     0,    0,     0},	       	               '{0,          0,       0,              0,      0,     0,    0,     0},	    
       // simulated TX underrun
       '{1,          1,       0,              0,      0,     0,    0,     0},	 
       // simulated TX abort (quick and late)
       '{0,          1,       0,              0,      0,     0,    0,     0},	    
       '{0,          1,       0,              0,      0,     0,    0,     0}	    
       
       
       };
   
   task verify_rxtx();
      int i;
      int valid 	   = 1;
      int step 		   = 0;
      
      
      for(i=0;i<`NUM_TEST_VECTORS;i++) begin
	 test_vector_t tv;
	 ether_frame_t txf, rxf;

	 EMU.tx_queue.pop(txf);
	 EMU.rx_queue.pop(rxf);

	 tv 				    = test_vectors[i];

	 step 				    = 0;
	 valid 				    = 1;
	 
	 
	 if(txf.error ^ tv.tx_error) valid  = 0; 
	 if(rxf.error ^ tv.rx_error) valid  = 0;

	 if(rxf.hdr.is_802_1q && (rxf.hdr.vid != tv.vid || rxf.hdr.prio != tv.prio)) valid = 0;
	 
	 
	 if(!valid) break;
	 
      end

      if(valid)
	$display("VERIFICATION PASSED");
      else
	$display("VERIFICATION FAILED: test vector %d step %d", i, step);
      




   endtask // verify_rxtx

   
   
   
   
   initial begin
      automatic  int k;
      automatic int data[1600];
      automatic int i;
      int seed;

      reg [31:0] reg_lacr;
      

  const bit[7:0] payload[] = '{
'h00, 'h01, 'h08, 'h00, 'h06, 'h04, 'h00, 'h01, 'h00, 'h50, 
'hfc, 'h96, 'h9b, 'h0e, 'hc0, 'ha8, 'h01, 'h01, 'h00, 'h00, 'h00, 'h00, 'h00, 'h00, 'hc0, 'ha8,
'h01, 'h02, 
'h00, 'h00, 
'h00, 'h00, 
'h00, 'h00, 
'h00, 'h00, 
'h00, 'h00, 
'h00, 'h00, 
'h00, 'h00, 
'h00, 'h00, 
'h00, 'h00,
 'h01};
      #100;

      // configure some EP registers
      initialize_EP_regs();

      
      hdr.dst        = 'hffffffffffff;
      hdr.src        = 'h0050fc969b0e;
      
      hdr.is_802_1q  = 0;
      hdr.ethertype  = 'h0806;
      hdr.oob_type   = 0;//`OOB_TYPE_TXTS;
      hdr.oob_fid    = 0;
      
      for(i=0;i<47;i++) data[i] = payload[i];
 
      

      hdr.dst        = 'hda0203040506;
      hdr.src        = 'h5a0203040506;
      
      hdr.is_802_1q  = 0;
      hdr.ethertype  = 'h0003;
      hdr.oob_type   = `OOB_TYPE_TXTS;
      hdr.oob_fid    = 0;
      
      #100ns;

      #(70us);
      
      
      

      $display("StartTX");
      
      
      for(k=60;k<=80;k++)
        begin
           int l;
           
           for(l=0;l<1518;l++) data[l] = $dist_normal(seed, 0, 255);
           

	   $display("i=%d\n", k);
	EMU.send(hdr, data, k);
	end
      #100000000;

      $stop;
      
      
      EMU.send(hdr, data, 1200);

      ep_size_check(0, 0);
      
// send a runt and a giant frame		  
      EMU.send(hdr, data, 45);
      EMU.send(hdr, data, 1522-18+1);

      ep_size_check(1, 1);
  
// send a runt and a giant, this time they should NOT be accepted
      EMU.send(hdr, data, 45);
      EMU.send(hdr, data, 1522-18+1);
	

      EMU.simulate_tx_underrun(1,10);

      EMU.send(hdr, data, 46);
      EMU.simulate_tx_underrun(0, 0);

      #100000;

      $stop;

           
      EMU.simulate_tx_abort(1, 2);
      EMU.send(hdr, data, 46);

      EMU.simulate_tx_abort(1, 20);
      EMU.send(hdr, data, 46);
      EMU.simulate_tx_abort(0, 0);
      
      ep_set_qmode(`EP_QMODE_ACCESS);
      ep_set_vlan('habc, 5);

      hdr.is_802_1q  = 1;
      hdr.vid 	     = 'hdee;
      hdr.prio 	     = 0;

      
// send a 802.1q-tagged frame to ACCESS port - it should be retagged to VLAN assigned to the port      
      EMU.send(hdr, data, 46);      
      hdr.is_802_1q  = 0;

// send a non-802.1q frame to ACCESS port      
      EMU.send(hdr, data, 46);      
      
      #10000;

      verify_rxtx();
   end // initial begin
   

   task automatic stupid_integrity_check(ref ether_frame_t fra);
      int i;
      

	for(i=1;i<fra.size;i++)
	  if((fra.payload[i] & 'hff) != ((fra.payload[i-1]+1) & 'hff))
	    begin
	       $display("Error i=%d %x!=%x", i, fra.payload[i] & 'hff, (fra.payload[i-1]+1)&'hff);
	       break;
	    end

     endtask // stupid_intergrity_check
   
     
   
   initial forever
     begin
	@(posedge clk_sys);
	
     if(EMU.rx_queue != null && EMU.rx_queue.get_count())
     begin
	automatic int i;
	automatic ether_frame_t fra;

	 EMU.rx_queue.pop(fra);
	stupid_integrity_check(fra);
	
	
	
     end
   end
   
endmodule // main
