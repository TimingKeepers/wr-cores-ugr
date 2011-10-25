`timescale 1ns/1ps



`include "if_wishbone.sv"
`include "endpoint_regs.v"
`include "endpoint_mdio.v"
`include "tbi_utils.sv"

`timescale 1ps/1ps

`define EP_QMODE_ACCESS 0
`define EP_QMODE_TRUNK 1
`define EP_QMODE_UNQ 3

// Clock periods (in picoseconds)
const int c_RBCLK_PERIOD   = 8001;
const int c_REFCLK_PERIOD  = 8000;

`define ADDR_RST_GEN 'h62000

module main;

   wire clk_ref;
   wire clk_sys;
   wire rst_n;

   IWishbone WB 
     (
      .clk_i(clk_sys),
      .rst_n_i(rst_n)
      );
   
   tbi_clock_rst_gen
     #(
       .g_rbclk_period(8002))
     clkgen(
	    .clk_ref_o(clk_ref),
	    .clk_sys_o(clk_sys),
	    .phy_rbclk_o(phy_rbclk),
	    .rst_n_o(rst_n)
	    );

   wire clk_sys_dly;

   assign  #10 clk_sys_dly  = clk_sys;
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
    .g_virtual_uart(1),
    .g_ep_rxbuf_size_log2     (12),
    .g_dpram_initf            ("/home/slayer/wrpc-sw/hello.ram"),
    .g_dpram_size             (16384),
    .g_num_gpio               (8)
    )
   DUT (
	.clk_sys_i      (clk_sys),
	.clk_dmtd_i     (clk_ref),
	.clk_ref_i      (clk_ref),
	.rst_n_i         (rst_n),

	.pps_p_o        (),

	.dac_hpll_load_p1_o (),
	.dac_hpll_data_o (),

	.dac_dpll_load_p1_o (),
	.dac_dpll_data_o (),

	.gpio_o          (),
    
	.uart_rxd_i       (1'b0),
	.uart_txd_o       (),

	.wb_addr_i      (WB.adr[17:0]),
	.wb_data_i      (WB.dat_o),
	.wb_data_o      (WB.dat_i),
	.wb_sel_i       (4'b1111),
	.wb_we_i        (WB.we),
	.wb_cyc_i       (WB.cyc),
	.wb_stb_i       (WB.stb),
	.wb_ack_o       (WB.ack),

        .phy_ref_clk_i(clk_ref),
        .phy_tx_data_o(phy_tx_data),
        .phy_tx_k_o(phy_tx_k),
        .phy_tx_disparity_i(phy_tx_disparity),
        .phy_tx_enc_err_i(phy_tx_enc_err),
        .phy_rx_data_i(phy_rx_data),
        .phy_rx_rbclk_i(phy_rx_rbclk),
        .phy_rx_k_i(phy_rx_k),
        .phy_rx_enc_err_i(phy_rx_enc_err),
        .phy_rx_bitslide_i(phy_rx_bitslide),
        .phy_rst_o(phy_rst),
        .phy_loopen_o(phy_lo),

	.genrest_n        ()
	);

 
   

   wr_gtp_phy_spartan6
     #(
       .g_simulation(1),
       .g_ch0_use_refclk_out (0),
       .g_ch1_use_refclk_out (0)
       ) PHY 
       (
        .ch0_ref_clk_i(clk_ref),
        .ch0_ref_clk_o(),
        .ch0_tx_data_i(8'h00),
        .ch0_tx_k_i(1'b0),
        .ch0_tx_disparity_o(),
        .ch0_tx_enc_err_o(),
        .ch0_rx_rbclk_o(),
        .ch0_rx_data_o(),
        .ch0_rx_k_o(),
        .ch0_rx_enc_err_o(),
        .ch0_rx_bitslide_o(),
        .ch0_rst_i(1'b0),
        .ch0_loopen_i(1'b0),

        .ch1_ref_clk_i(clk_ref),
        .ch1_ref_clk_o(),
        .ch1_tx_data_i(phy_tx_data),
        .ch1_tx_k_i(phy_tx_k),
        .ch1_tx_disparity_o(phy_tx_disparity),
        .ch1_tx_enc_err_o(phy_tx_enc_err),
        .ch1_rx_data_o(phy_rx_data),
        .ch1_rx_rbclk_o(phy_rx_rbclk),
        .ch1_rx_k_o(phy_rx_k),
        .ch1_rx_enc_err_o(phy_rx_enc_err),
        .ch1_rx_bitslide_o(phy_rx_bitslide),
        .ch1_rst_i(phy_rst),
        .ch1_loopen_i(phy_lo),
        .pad_txn0_o(),
        .pad_txp0_o(),
        .pad_rxn0_i(1'b0),
        .pad_rxp0_i(1'b0),
        .pad_txn1_o(sfp_txn_o),
        .pad_txp1_o(sfp_txp_o),
        .pad_rxn1_i(sfp_rxn_i),
        .pad_rxp1_i(sfp_rxp_i));

   assign sfp_rxp_i    = sfp_txp_o;
   
     assign sfp_rxn_i  = sfp_txn_o;
   
     
   

   initial begin
        
      @(posedge rst_n);
      repeat(3) @(posedge clk_sys);

      WB.write32('h40000, 1);
      WB.write32('h40010, 'hdead);

      forever begin
	 reg[31:0] rval;
	 
	 repeat(100) @(posedge clk_sys);

	 WB.read32('h40000, rval);

	 if(rval[3]) begin
	     WB.read32('h40004, rval);
	    $display("Got TAG: %d", rval);
	 end
	 
	 
      end
      
       
	
      
      
      
   end
   
   
endmodule // main

