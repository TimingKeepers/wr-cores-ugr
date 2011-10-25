`timescale 1ps/1ps

`include "old_endpoint_regs.v"
`include "wishbone_test_master.v"
`include "fabric_emu/fabric_emu.sv"


`define EP_QMODE_ACCESS 0
`define EP_QMODE_TRUNK 1
`define EP_QMODE_UNQ 3
`define EP_QMODE_VLAN_DISABLED 2


module old_endpoint_test_wrapper
  (
   input clk_sys_i,
   input clk_ref_i,
   input clk_rx_i,
   input rst_n_i,

   output [9:0] td_o,
   input [9:0] rd_i,

   output txn_o,
   output txp_o,

   input rxn_i,
   input rxp_i
   );

   parameter g_phy_type  = "GTP";   
   
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
   wire gtp_rst;
   wire rx_clock;
   

   CWishboneAccessor WB;
     
   IWishboneMaster 
     #(
       .g_data_width(32),
       .g_addr_width(7))
   U_WB
     (
      .clk_i(clk_sys_i),
      .rst_n_i(rst_n_i)
      );

   generate

      if(g_phy_type == "TBI") begin
         assign rx_clock  = clk_rx_i;

         wr_tbi_phy U_Phy 
                     (
                      .serdes_rst_i (gtp_rst),
                      .serdes_loopen_i(1'b0),
                      .serdes_prbsen_i(1'b0),
                      .serdes_enable_i(1'b1), 
                      .serdes_syncen_i(1'b1),

                      .serdes_tx_data_i  (gtx_data),
                      .serdes_tx_k_i     (gtx_k),
                      .serdes_tx_disparity_o (gtx_disparity),
                      .serdes_tx_enc_err_o   (gtx_enc_error),

                      .serdes_rx_data_o      (grx_data),
                      .serdes_rx_k_o        (grx_k),
                      .serdes_rx_enc_err_o  (grx_enc_error),
                      .serdes_rx_bitslide_o (grx_bitslide),


                      .tbi_refclk_i (clk_ref_i),
                      .tbi_rbclk_i  (rx_clock),

                      .tbi_td_o     (td_o),
                      .tbi_rd_i     (rd_i),
                      .tbi_syncen_o (),
                      .tbi_loopen_o (),
                      .tbi_prbsen_o (),
                      .tbi_enable_o ()
                      );


      end else begin // if (g_phy_type == "TBI")
                 wr_gtp_phy_spartan6 U_Phy 
                     (
                      .ch0_rst_i (gtp_rst),
                      .ch0_ref_clk_i(clk_ref_i),
                      .ch0_loopen_i (1'b0),

                      .ch0_tx_data_i  (gtx_data),
                      .ch0_tx_k_i     (gtx_k),
                      .ch0_tx_disparity_o (gtx_disparity),
                      .ch0_tx_enc_err_o   (gtx_enc_error),

                      .ch0_rx_data_o      (grx_data),
                      .ch0_rx_k_o        (grx_k),
                      .ch0_rx_enc_err_o  (grx_enc_error),
                      .ch0_rx_bitslide_o (grx_bitslide),
                      .ch0_rx_rbclk_o(rx_clock),

                      .pad_txp0_o(txp_o),
                      .pad_txn0_o(txn_o),
                      .pad_rxp0_i(rxp_i),
                      .pad_rxn0_i(rxn_i));
         
      end // else: !if(g_phy_type == "TBI")

   endgenerate
   
   
   
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
        uint64_t rval;
        

      $display("MDIO_write %x %x", addr, data);
      
      WB.write(`OLD_ADDR_EP_MDIO_CR, ((addr >> 2) << 16) | `OLD_EP_MDIO_CR_RW | data);
      while(1)begin
	 WB.read(`OLD_ADDR_EP_MDIO_SR, rval);
	 if(rval & `OLD_EP_MDIO_SR_READY) break;
      end
   endtask // mdio_write

   task mdio_read(int addr, output int data);
        uint64_t rval;
      
      WB.write(`OLD_ADDR_EP_MDIO_CR, (addr >> 2));
      while(1)begin
	 WB.read(`OLD_ADDR_EP_MDIO_SR, rval);
	 if(rval & `OLD_EP_MDIO_SR_READY) begin
	    data  = rval[15:0];
	    break;
	 end
      end
   endtask // mdio_read
  
   task initialize_EP_regs();
    
      $display("Initializing EP registers...");
    
      WB  = U_WB.get_accessor();
      WB.set_mode(CLASSIC);
  
      WB.write(`OLD_ADDR_EP_ECR, `OLD_EP_ECR_RX_EN_FRA | `OLD_EP_ECR_TX_EN_FRA);
      WB.write(`OLD_ADDR_EP_RFCR, 3 << `OLD_EP_RFCR_QMODE_OFFSET); // QMODE = UNQUALIFIED
      WB.write(`OLD_ADDR_EP_MACH, 'haabb);  // assign a dummy MAC address
      WB.write(`OLD_ADDR_EP_MACL, 'hccddeeff);
      WB.write(`OLD_ADDR_EP_TSCR, `OLD_EP_TSCR_EN_RXTS);

  //    mdio_write(`ADDR_MDIO_MCR, `MDIO_MCR_RESET);
      
   endtask // initialize_EP_regs


   old_wrsw_endpoint
     #(
       .g_phy_mode("GTP"),
       .g_simulation(1)
       )
     DUT (
	  .clk_ref_i(clk_ref_i),
	  .clk_sys_i(clk_sys_i),
	  .clk_dmtd_i(clk_ref_i),
	  .rst_n_i (rst_n_i),
	  .pps_csync_p1_i(1'b0),
	  
	  `_WRF_CONNECT_MANDATORY_SOURCE(rx, fromEP),
	  `_WRF_CONNECT_MANDATORY_SINK(tx, toEP),

	  .rtu_full_i(1'b0),
	  .rtu_almost_full_i(1'b0),
	  
	  .wb_cyc_i  (U_WB.master.cyc),
	  .wb_stb_i  (U_WB.master.stb),
	  .wb_we_i   (U_WB.master.we),
	  .wb_sel_i  (U_WB.master.sel),
	  .wb_addr_i (U_WB.master.adr [5:0]),
	  .wb_data_i (U_WB.master.dat_o),
	  .wb_data_o (U_WB.master.dat_i),
	  .wb_ack_o  (U_WB.master.ack),
	  
	  .txtsu_port_id_o(txtsu_pid),
	  .txtsu_frame_id_o (txtsu_fid),
	  .txtsu_tsval_o  (txtsu_timestamp),
	  .txtsu_valid_o (txtsu_valid),
	  .txtsu_ack_i (txtsu_ack),

// GTP I/F
          .gtp_rst_o(gtp_rst),
	  .gtp_tx_clk_i(clk_ref_i),
	  .gtp_tx_data_o (gtx_data),
	  .gtp_tx_k_o    (gtx_k),
	  .gtp_tx_disparity_i (gtx_disparity),
	  .gtp_tx_enc_err_i  (gtx_enc_error),
	
	  .gtp_rx_data_i     (grx_data),
	  .gtp_rx_clk_i    (rx_clock),
	  .gtp_rx_k_i        (grx_k),
	  .gtp_rx_enc_err_i  (grx_enc_error),
	  .gtp_rx_bitslide_i (grx_bitslide)
    );

   
   

   // sets the Q-mode of the endpoint
   task ep_set_qmode(int qmode);
      uint64_t rfcr;

      string s;
      
      case (qmode)
	`EP_QMODE_ACCESS: s="ACCESS"; 
	`EP_QMODE_TRUNK: s="TRUNK";
	`EP_QMODE_UNQ: s="UNQUALIFIED";
        
      endcase // case (qmode)
      
      
      $display("Setting qmode to: %s", s);
      
      WB.read(`OLD_ADDR_EP_RFCR, rfcr);
      rfcr  = rfcr & (~`OLD_EP_RFCR_QMODE);
      rfcr  = rfcr | ( qmode << `OLD_EP_RFCR_QMODE_OFFSET);
      WB.write(`OLD_ADDR_EP_RFCR, rfcr);

     endtask // ep_set_qmode
   

   
   initial $dumpfile("dump.vcd");
   initial $dumpvars(0, main);

   reg ready  = 0;

   always@(rst_n_i) if(!rst_n_i)
     ready    = 0;
   
   
   initial begin

      @(posedge rst_n_i);
      @(posedge clk_sys_i);

      // configure some EP registers
      initialize_EP_regs();

      ready  = 1;
   end
   

   EthPacket rx_queue[$];

class WRFPacketSink extends EthPacketSink;
   
   function int poll();
      return rx_queue.size();
   endfunction // poll
   
   task recv(ref EthPacket pkt, ref int result = _null);
      while(!poll()) @(posedge clk_sys_i);
      pkt     = rx_queue.pop_front();
      result  = 1;
   endtask // recv
   
endclass // WRFPacketSink
   
   WRFPacketSink sink;

class WRFPacketSource extends EthPacketSource;
   task send(ref EthPacket pkt, ref int result = _null);
      byte tmp[];

      pkt.serialize(tmp);

      EMU.send_raw(tmp);
      
   endtask // send
endclass // WRFPacketSource
   

   WRFPacketSource src;
   
   
   
   initial 
     begin
        sink  = new;
        src   = new;
        
        
        forever begin
	@(posedge clk_sys_i);
	
        if(EMU.rx_queue != null && EMU.rx_queue.get_count())
          begin
	     automatic int i;
	     automatic ether_frame_t fra;
             automatic EthPacket pkt;
             
             pkt= new;
             
	     EMU.rx_queue.pop(fra);
             pkt.deserialize(fra.raw_data);
             rx_queue.push_back(pkt);
             end
        end
   end
   
endmodule

