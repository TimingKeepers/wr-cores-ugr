`timescale 1ns/1ps

   
`include "tbi_utils.sv"

`include "simdrv_defs.svh"
`include "if_wb_master.svh"
`include "wrc_syscon_regs.vh"
`include "if_wb_slave.svh"
`include "wb_packet_source.svh"
`include "wb_packet_sink.svh"
`include "if_wb_link.svh"

`define BASE_SYSCON 'h20400


module main;

  wire clk_ref;
  wire clk_sys;
  wire rst_n;

  IWishboneMaster WB (
    .clk_i(clk_sys),
    .rst_n_i(rst_n));

  IWishboneMaster #(2,16) U_ep_src (
    .clk_i(clk_sys),
    .rst_n_i(rst_n));

  IWishboneSlave  #(2,16) U_ep_snk (
    .clk_i(clk_sys),
    .rst_n_i(rst_n));

  WBPacketSource ep_src;
  WBPacketSink ep_snk;
	 
  tbi_clock_rst_gen
  #(
    .g_rbclk_period(8000))
  clkgen(
    .clk_ref_o(clk_ref),
    .clk_sys_o(clk_sys),
    .phy_rbclk_o(phy_rbclk),
    .rst_n_o(rst_n)
  );

  wire   [7:0]phy_tx_data      ;
  wire   phy_tx_k         ;
  wire   phy_tx_disparity ;
  wire   phy_tx_enc_err   ;
  wire   [7:0]phy_rx_data      ;
  wire   phy_rx_rbclk     ;
  wire   phy_rx_k         ;
  wire   phy_rx_enc_err   ;
  wire   [3:0]phy_rx_bitslide  ;
  wire   phy_rst          ;
  wire   phy_loopen;

  wr_core #(
    .g_simulation             (1),
    .g_interface_mode(PIPELINED),
    .g_address_granularity(BYTE),
    .g_dpram_initf            ("sw/wrc.ram"),
    .g_dpram_size             (90112/4))
  DUT (
    .clk_sys_i      (clk_sys),
    .clk_dmtd_i     (clk_ref),
    .clk_ref_i      (clk_ref),
    .clk_aux_i      (clk_ref),
    .rst_n_i        (rst_n),

    .pps_p_o        (),
    .dac_hpll_load_p1_o (),
    .dac_hpll_data_o (),

    .dac_dpll_load_p1_o (),
    .dac_dpll_data_o (),

    .uart_rxd_i       (1'b0),
    .uart_txd_o       (),

    .scl_i(scl_loop),
    .scl_o(scl_loop),
    .sda_i(sda_loop),
    .sda_o(sda_loop),

    .btn1_i(1'b0),
    .btn2_i(1'b0),

    .ext_snk_adr_i	(U_ep_src.adr[1:0]),
    .ext_snk_dat_i	(U_ep_src.dat_o),
    .ext_snk_sel_i	(U_ep_src.sel),
    .ext_snk_cyc_i	(U_ep_src.cyc),
    .ext_snk_we_i 	(1'b1),
    .ext_snk_stb_i	(U_ep_src.stb),
    .ext_snk_ack_o	(U_ep_src.ack),
    .ext_snk_err_o	(U_ep_src.err),
    .ext_snk_stall_o(U_ep_src.stall),
    .ext_src_ack_i (1'b1),
    .ext_src_err_i (1'b0),
    .ext_src_stall_i(1'b0),

    .wb_adr_i      (WB.master.adr[31:0]),
    .wb_dat_i      (WB.master.dat_o),
    .wb_dat_o      (WB.master.dat_i),
    .wb_sel_i       (4'b1111),
    .wb_we_i        (WB.master.we),
    .wb_cyc_i       (WB.master.cyc),
    .wb_stb_i       (WB.master.stb),
    .wb_ack_o       (WB.master.ack),
    .wb_stall_o     (WB.master.stall),

    .phy_ref_clk_i(clk_ref),
    .phy_tx_data_o(phy_tx_data),
    .phy_tx_k_o(phy_tx_k),
    .phy_tx_disparity_i(phy_tx_disparity),
    .phy_tx_enc_err_i(phy_tx_enc_err),
    .phy_rx_data_i(phy_rx_data),
    .phy_rx_rbclk_i(clk_ref),
    .phy_rx_k_i(phy_rx_k),
    .phy_rx_enc_err_i(phy_rx_enc_err),
    .phy_rx_bitslide_i(phy_rx_bitslide),
    .phy_rst_o(phy_rst),
    .phy_loopen_o(phy_lo)
  );

  assign phy_rx_data       = phy_tx_data;
  assign phy_rx_k          = phy_tx_k;
  assign phy_tx_disparity  = 0;
  assign phy_tx_enc_err    = 0;
  assign phy_rx_enc_err    = 0;
   
  //////////////////////////////////////
  task send_frames(WBPacketSource src, int n_packets);
    int i, seed = 0,n1=0,n2=0;
    EthPacket pkt, tmpl;
    EthPacket to_ext[$], to_minic[$];
    EthPacketGenerator gen  = new;
    
    tmpl                = new;
    tmpl.src                = '{1,2,3,4,5,6};
    tmpl.dst                = '{'hff,'hff,'hff,'hff,'hff,'hff};
    //tmpl.dst                = '{'h01,'h1b,'h19,'h00,'h00,'h00}; // PTP dst MAC
    tmpl.has_smac           = 1;
    tmpl.is_q               = 0;
    tmpl.ethertype	=	{'h0800};
    //tmpl.ethertype	=	{'h88f7};
    
    //gen.set_randomization(EthPacketGenerator::SEQ_PAYLOAD | EthPacketGenerator::ETHERTYPE /*| EthPacketGenerator::RX_OOB*/) ;
    gen.set_randomization(EthPacketGenerator::SEQ_PAYLOAD ) ;
    gen.set_template(tmpl);
    gen.set_size(46, 1000);
    
    for(i=0;i<n_packets;i++) begin
      pkt         = gen.gen();
      //pkt.payload[22] = 'heb; //pretend frame is etherbone
      //pkt.payload[23] = 'hd0;
      src.send(pkt);
    end
  endtask
  //////////////////////////////////////


  initial begin
    CWishboneAccessor acc;

    @(posedge rst_n);
    repeat(3) @(posedge clk_sys);

    #1us;

    acc  = WB.get_accessor();
    acc.set_mode(PIPELINED);

    ep_src = new(U_ep_src.get_accessor());
    ep_snk = new(U_ep_snk.get_accessor());
    U_ep_src.settings.cyc_on_stall = 1;

    #1us
    acc.write(`BASE_SYSCON + `ADDR_SYSC_RSTR, 'hdeadbee | `SYSC_RSTR_RST);
    #1us;
    acc.write(`BASE_SYSCON + `ADDR_SYSC_RSTR, 'hdeadbee );

    #250us;
    //NOW LET'S SEND A FRAME
    send_frames(ep_src, 1);

  end

endmodule // main
