
`include "endpoint_regs.v"
`include "endpoint_mdio.v"

interface ITXTSU_Bus;


   logic valid;
   logic ack;
   logic [4:0] port_id;
   logic [15:0] frame_id;
   logic [31:0] ts;

   modport endpoint
     (
      input ack,
      output valid,
      output port_id,
      output frame_id,
      output ts
      );

   modport nic
     (
      output ack,
      input valid,
      input port_id,
      input frame_id,
      input ts
      );
endinterface // txtsu_if

`define EP_QMODE_ACCESS 0
`define EP_QMODE_TRUNK 1
`define EP_QMODE_UNQ 3
`define EP_QMODE_VLAN_DISABLED 2

module ep2ep_wrapper
  (
   input clk_sys_i,
   input clk_ref_i,
   input rst_n_i,

   IWishboneSlave.slave src_a,
   IWishboneMaster.master snk_a,

   IWishboneLink.slave src_b,
   IWishboneLink.master snk_b,
   
   ITXTSU_Bus.endpoint txts_a,
   ITXTSU_Bus.endpoint txts_b
  );


   wire[15:0] gtx_data;
   wire [1:0]gtx_k;
   wire gtx_disparity;
   wire gtx_enc_error;
   wire [15:0] grx_data;
   wire grx_clk;
   wire [1:0]grx_k;
   wire grx_enc_error;
   wire [3:0] grx_bitslide;
   wire gtp_rst;
   wire tx_clock;

   IWishboneMaster 
     #(
       .g_data_width(32),
       .g_addr_width(32))
   sys_a
     (
      .clk_i(clk_sys_i),
      .rst_n_i(rst_n_i)
      );

   IWishboneMaster 
     #(
       .g_data_width(32),
       .g_addr_width(32))
   sys_b
     (
      .clk_i(clk_sys_i),
      .rst_n_i(rst_n_i)
      );

   
   wr_endpoint
     #(
       .g_simulation          (1),
       .g_pcs_16bit(0),
       .g_rx_buffer_size (1024),
       .g_with_rx_buffer(1),
       .g_with_timestamper    (1),
       .g_with_dpi_classifier (0),
       .g_with_vlans          (0),
       .g_with_rtu            (0)
      ) EP_A (
              .clk_ref_i (clk_ref_i),
              .clk_sys_i (clk_sys_i),
              .rst_n_i  (rst_n_i),
              .pps_csync_p1_i (1'b0),

              .phy_rst_o   (),
              .phy_loopen_o (),
              .phy_enable_o (),
              .phy_syncen_o (),

              .phy_ref_clk_i      (clk_ref_i),
              .phy_tx_data_o      (gtx_data),
              .phy_tx_k_o         (gtx_k),
              .phy_tx_disparity_i (1'b1),
              .phy_tx_enc_err_i   (1'b0),

              .phy_rx_data_i     (grx_data),
              .phy_rx_clk_i      (clk_ref_i),
              .phy_rx_k_i        (grx_k),
              .phy_rx_enc_err_i  (1'b0),
              .phy_rx_bitslide_i (5'b0),

              .src_dat_o   (src_a.dat_i),
              .src_adr_o   (src_a.adr),
              .src_sel_o   (src_a.sel),
              .src_cyc_o   (src_a.cyc),
              .src_stb_o   (src_a.stb),
              .src_we_o    (src_a.we),
              .src_stall_i (src_a.stall),
              .src_ack_i   (src_a.ack),

              .snk_dat_i   (snk_a.dat_o[15:0]),
              .snk_adr_i   (snk_a.adr[1:0]),
              .snk_sel_i   (snk_a.sel[1:0]),
              .snk_cyc_i   (snk_a.cyc),
              .snk_stb_i   (snk_a.stb),
              .snk_we_i    (snk_a.we),
              .snk_stall_o (snk_a.stall),
              .snk_ack_o   (snk_a.ack),
              .snk_err_o   (snk_a.err),
              .snk_rty_o   (snk_a.rty),

              .txtsu_port_id_o (txts_a.port_id),
              .txtsu_frame_id_o (txts_a.frame_id),
              .txtsu_tsval_o (txts_a.ts),
              .txtsu_valid_o (txts_a.valid),
              .txtsu_ack_i (txts_a.ack),

              .rtu_full_i (1'b0),
              .rtu_almost_full_i (1'b0),

              .wb_cyc_i(sys_a.cyc),
              .wb_stb_i (sys_a.stb),
              .wb_we_i (sys_a.we),
              .wb_sel_i(sys_a.sel),
              .wb_adr_i(sys_a.adr[7:0]),
              .wb_dat_i(sys_a.dat_o),
              .wb_dat_o(sys_a.dat_i),
              .wb_ack_o (sys_a.ack)
    );


      wr_endpoint
     #(
       .g_simulation          (1),
       .g_pcs_16bit(0),
       .g_rx_buffer_size (1024),
       .g_with_rx_buffer(1),
       .g_with_timestamper    (1),
       .g_with_dpi_classifier (0),
       .g_with_vlans          (0),
       .g_with_rtu            (0)
      ) EP_B (
              .clk_ref_i (clk_ref_i),
              .clk_sys_i (clk_sys_i),
              .rst_n_i  (rst_n_i),
              .pps_csync_p1_i (1'b0),

              .phy_rst_o   (),
              .phy_loopen_o (),
              .phy_enable_o (),
              .phy_syncen_o (),

              .phy_ref_clk_i      (clk_ref_i),
              .phy_tx_data_o      (grx_data),
              .phy_tx_k_o         (grx_k),
              .phy_tx_disparity_i (1'b1),
              .phy_tx_enc_err_i   (1'b0),

              .phy_rx_data_i     (gtx_data),
              .phy_rx_clk_i      (clk_ref_i),
              .phy_rx_k_i        (gtx_k),
              .phy_rx_enc_err_i  (1'b0),
              .phy_rx_bitslide_i (5'b0),

              .src_dat_o   (src_b.dat_o),
              .src_adr_o   (src_b.adr),
              .src_sel_o   (src_b.sel),
              .src_cyc_o   (src_b.cyc),
              .src_stb_o   (src_b.stb),
              .src_we_o    (src_b.we),
              .src_stall_i (src_b.stall),
              .src_ack_i   (src_b.ack),
              .src_err_i   (src_b.err),
              
              .snk_dat_i   (snk_b.dat_i[15:0]),
              .snk_adr_i   (snk_b.adr[1:0]),
              .snk_sel_i   (snk_b.sel[1:0]),
              .snk_cyc_i   (snk_b.cyc),
              .snk_stb_i   (snk_b.stb),
              .snk_we_i    (snk_b.we),
              .snk_stall_o (snk_b.stall),
              .snk_ack_o   (snk_b.ack),
              .snk_err_o   (snk_b.err),
              .snk_rty_o   (snk_b.rty),

              .txtsu_port_id_o (txts_b.port_id),
              .txtsu_frame_id_o (txts_b.frame_id),
              .txtsu_tsval_o (txts_b.ts),
              .txtsu_valid_o (txts_b.valid),
              .txtsu_ack_i (txts_b.ack),

              .rtu_full_i (1'b0),
              .rtu_almost_full_i (1'b0),

              .wb_cyc_i(sys_b.cyc),
              .wb_stb_i (sys_b.stb),
              .wb_we_i (sys_b.we),
              .wb_sel_i(sys_b.sel),
              .wb_adr_i(sys_b.adr[7:0]),
              .wb_dat_i(sys_b.dat_o),
              .wb_dat_o(sys_b.dat_i),
              .wb_ack_o (sys_b.ack)
    );



   
   task ep_init(CWishboneAccessor acc);
      acc.set_mode(CLASSIC);
      acc.write(`ADDR_EP_ECR, `EP_ECR_TX_EN | `EP_ECR_RX_EN);
      acc.write(`ADDR_EP_RFCR, 1518 << `EP_RFCR_MRU_OFFSET);
      acc.write(`ADDR_EP_VCR0, `EP_QMODE_VLAN_DISABLED << `EP_VCR0_QMODE_OFFSET);
      acc.write(`ADDR_EP_TSCR, `EP_TSCR_EN_RXTS);
   endtask // ep_init

   initial begin
      while(!rst_n_i)
        @(posedge clk_sys_i);
      #1us;
      
      $display("InitEndpoints");
      
      
      ep_init(sys_a.get_accessor());
      ep_init(sys_b.get_accessor());

      
   end
   

endmodule // endpoint_phy_wrapper