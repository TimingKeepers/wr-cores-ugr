-------------------------------------------------------------------------------
-- Title      : WhiteRabbit PTP Core
-- Project    : WhiteRabbit
-------------------------------------------------------------------------------
-- File       : wr_core.vhd
-- Author     : Grzegorz Daniluk
-- Company    : Elproma
-- Created    : 2011-02-02
-- Last update: 2011-05-11
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description:
-- WR PTP Core is a HDL module implementing a complete gigabit Ethernet 
-- interface (MAC + PCS + PHY) with integrated PTP slave ordinary clock 
-- compatible with White Rabbit protocol. It performs subnanosecond clock 
-- synchronization via WR protocol and also acts as an Ethernet "gateway", 
-- providing access to TX/RX interfaces of the built-in WR MAC.
-------------------------------------------------------------------------------
-- Copyright (c) 2011 Grzegorz Daniluk
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2011-02-02  1.0      greg.d          Created
-------------------------------------------------------------------------------
-- TODO:
-- Testing
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library work;
use work.wrcore_pkg.all;
use work.wbconmax_pkg.all;

entity wr_core is
  generic(
    --if set to 1, then blocks in PCS use smaller calibration counter to speed 
    --up simulation
    g_gtp_channel            : integer := 1;
    g_simulation             : integer := 0;
    g_virtual_uart           : natural := 0;
    g_ep_phy_mode            : string  := "GTP";
    g_ep_rxbuf_size_log2     : integer := 12;
    g_mnic_memsize_log2      : integer := 14;     --log2(g_dpram_size)
    g_mnic_buf_little_endian : boolean := true;
    g_dpram_initf            : string  := "sw/hello.ram";
    g_dpram_size             : integer := 16384;  --in 32-bit words
    g_dpram_byte_ena         : boolean := false;  --enable byte access to dpram
    g_num_gpio               : integer := 8;
    g_wbicon_rf_addr         : integer := 15
    );
  port(
    clk_sys_i : in std_logic;

    -- DDMTD offset lcock (125.x MHz)
    clk_dmtd_i : in std_logic;

    -- Timing reference (125 MHz)
    clk_ref_i : in std_logic;

    rst_n_i : in std_logic;

    -----------------------------------------
    --PPS gen
    -----------------------------------------
    pps_p_o : out std_logic;

    -----------------------------------------
    --Timing system
    -----------------------------------------

    dac_hpll_load_p1_o : out std_logic;
    dac_hpll_data_o    : out std_logic_vector(15 downto 0);

    dac_dpll_load_p1_o : out std_logic;
    dac_dpll_data_o    : out std_logic_vector(15 downto 0);

    -----------------------------------------
    --Endpoint
    -----------------------------------------
    --TBI
    -- data output, 8b10b-encoded
    tbi_td_o        : out std_logic_vector(9 downto 0);
    -- PHY enable, active HI
    tbi_enable_o    : out std_logic;
    -- PHY comma sync enable, active HI
    tbi_syncen_o    : out std_logic;
    -- PHY loopback mode enable, active HI
    tbi_loopen_o    : out std_logic;
    -- PHY PRBS pattern test enable, active HI  
    tbi_prbsen_o    : out std_logic;
    -- RX clock (125.x MHz)
    tbi_rbclk_i     : in  std_logic;
    -- data input, 8b10b-encoded
    tbi_rd_i        : in  std_logic_vector(9 downto 0);
    -- PHY sync detect pulse (active HI when PHY detects valid comma pattern)
    tbi_sync_pass_i : in  std_logic;

    -----------------------------------------
    --GTP
    -----------------------------------------
    pad_txn0_o : out std_logic;
    pad_txp0_o : out std_logic;
    pad_rxn0_i : in  std_logic;
    pad_rxp0_i : in  std_logic;

    -----------------------------------------
    --GPIO
    -----------------------------------------
    gpio_o : out std_logic_vector(g_num_gpio-1 downto 0);
    gpio_i : in std_logic_vector(g_num_gpio-1 downto 0);
    gpio_dir_o : out std_logic_vector(g_num_gpio-1 downto 0);

    -----------------------------------------
    --UART
    -----------------------------------------
    uart_rxd_i : in  std_logic;
    uart_txd_o : out std_logic;

    -----------------------------------------
    --External WB interface
    -----------------------------------------
    wb_addr_i : in  std_logic_vector(c_aw-1 downto 0);
    wb_data_i : in  std_logic_vector(c_dw-1 downto 0);
    wb_data_o : out std_logic_vector(c_dw-1 downto 0);
    wb_sel_i  : in  std_logic_vector(c_sw-1 downto 0);
    wb_we_i   : in  std_logic;
    wb_cyc_i  : in  std_logic;
    wb_stb_i  : in  std_logic;
    wb_ack_o  : out std_logic;
    --DEBUG
    genrest_n : out std_logic;

    dio_o : out std_logic_vector(3 downto 0)
    );
end wr_core;

architecture struct of wr_core is


  
  signal s_rst   : std_logic;
  signal s_rst_n : std_logic;

  -----------------------------------------------------------------------------
  --PPS generator
  -----------------------------------------------------------------------------
  signal s_pps_csync : std_logic;
  signal ppsg_wb_i   : t_wb_i;
  signal ppsg_wb_o   : t_wb_o;

  -----------------------------------------------------------------------------
  --Timing system
  -----------------------------------------------------------------------------
  signal hpll_wb_i : t_wb_i;
  signal hpll_wb_o : t_wb_o;
  signal dpll_wb_i : t_wb_i;
  signal dpll_wb_o : t_wb_o;

  -----------------------------------------------------------------------------
  --Endpoint
  -----------------------------------------------------------------------------
  signal s_ep_rx_data_o      : std_logic_vector(15 downto 0);
  signal s_ep_rx_ctrl_o      : std_logic_vector(4 - 1 downto 0);
  signal s_ep_rx_bytesel_o   : std_logic;
  signal s_ep_rx_sof_p1_o    : std_logic;
  signal s_ep_rx_eof_p1_o    : std_logic;
  signal s_ep_rx_dreq_i      : std_logic;
  signal s_ep_rx_valid_o     : std_logic;
  signal s_ep_rx_rerror_p1_o : std_logic;

  signal s_ep_tx_data_i      : std_logic_vector(15 downto 0);
  signal s_ep_tx_ctrl_i      : std_logic_vector(4 - 1 downto 0);
  signal s_ep_tx_bytesel_i   : std_logic;
  signal s_ep_tx_sof_p1_i    : std_logic;
  signal s_ep_tx_eof_p1_i    : std_logic;
  signal s_ep_tx_dreq_o      : std_logic;
  signal s_ep_tx_valid_i     : std_logic;
  signal s_ep_tx_terror_p1_o : std_logic;

  signal txtsu_port_id_o  : std_logic_vector(4 downto 0);
  signal txtsu_frame_id_o : std_logic_vector(16 -1 downto 0);
  signal txtsu_tsval_o : std_logic_vector(28 + 4 - 1 downto 0); 
  signal txtsu_valid_o : std_logic;
  signal txtsu_ack_i   : std_logic;

  signal ep_wb_i : t_wb_i;
  signal ep_wb_o : t_wb_o;

  -----------------------------------------------------------------------------
  --GTP
  -----------------------------------------------------------------------------
  signal s_gtp_tx_data_i      : std_logic_vector(7 downto 0);
  signal s_gtp_tx_k_i         : std_logic;
  signal s_gtp_tx_disparity_o : std_logic;
  signal s_gtp_tx_enc_err_o   : std_logic;
  signal s_gtp_rx_data_o      : std_logic_vector(7 downto 0);
  signal s_gtp_rx_rbclk_o     : std_logic;
  signal s_gtp_rx_k_o         : std_logic;
  signal s_gtp_rx_enc_err_o   : std_logic;
  signal s_gtp_rx_bitslide_o  : std_logic_vector(3 downto 0);
  signal s_gtp_rst_i          : std_logic;
  signal s_gtp_loopen_i       : std_logic;

  -----------------------------------------------------------------------------
  --Mini-NIC
  -----------------------------------------------------------------------------
  signal s_mnic_mem_data_o : std_logic_vector(31 downto 0);
  signal s_mnic_mem_addr_o : std_logic_vector(g_mnic_memsize_log2-1 downto 0);
  signal s_mnic_mem_data_i : std_logic_vector(31 downto 0);
  signal s_mnic_mem_wr_o   : std_logic;

  signal mnic_wb_i : t_wb_i;
  signal mnic_wb_o : t_wb_o;

  signal mnic_wb_irq_o : std_logic;

  -----------------------------------------------------------------------------
  --CPU
  -----------------------------------------------------------------------------
  signal zpu_wb_i : t_wb_o;
  signal zpu_wb_o : t_wb_i;

  signal lm32_iwb_i : t_wb_o;
  signal lm32_iwb_o : t_wb_i;
  signal lm32_dwb_i : t_wb_o;
  signal lm32_dwb_o : t_wb_i;

  -----------------------------------------------------------------------------
  --Dual-port RAM
  -----------------------------------------------------------------------------
  signal dpram_wb_i : t_wb_i;
  signal dpram_wb_o : t_wb_o;

  -----------------------------------------------------------------------------
  --WB GPIO
  -----------------------------------------------------------------------------
  --signal gpio_wb_i : t_wb_i;
  --signal gpio_wb_o : t_wb_o;

  -----------------------------------------------------------------------------
  --WB UART
  -----------------------------------------------------------------------------

  --signal uart_wb_i : t_wb_i;
  --signal uart_wb_o : t_wb_o;
  --signal uart_rxd  : std_logic;
  --signal uart_txd  : std_logic;

  -----------------------------------------------------------------------------
  --WB Peripherials
  -----------------------------------------------------------------------------
  signal periph_wb_i : t_wb_i;
  signal periph_wb_o : t_wb_o;


  -----------------------------------------------------------------------------
  --WB intercon
  -----------------------------------------------------------------------------
  signal wbm_unused_i : t_wb_i;
  signal wbs_unused_i : t_wb_o;
  signal cnx_master_i : t_conmax_masters_i;
  signal cnx_master_o : t_conmax_masters_o;
  signal cnx_slave_i  : t_conmax_slaves_i;
  signal cnx_slave_o  : t_conmax_slaves_o;

  -----------------------------------------------------------------------------
  --External WB interface
  -----------------------------------------------------------------------------
  signal ext_wb_i : t_wb_o;
  signal ext_wb_o : t_wb_i;

  --===========================--
  --         For SPEC          --
  --===========================--
  signal s_wrcila_ctrl : std_logic_vector(35 downto 0);
  signal s_ilatrig3    : std_logic_vector(20 downto 0);
--  signal gpio_b        : std_logic_vector(g_num_gpio-1 downto 0);
  signal licznik       : integer range 0 to 16383;
  type t_State is (IDLE, SETA, SET1, WACK, INC);
  signal State         : t_State;

  signal rst_wb_addr_o : std_logic_vector(17 downto 0);
  signal rst_wb_data_i : std_logic_vector(31 downto 0);
  signal rst_wb_data_o : std_logic_vector(31 downto 0);
  signal rst_wb_sel_o  : std_logic_vector(3 downto 0);
  signal rst_wb_we_o   : std_logic;
  signal rst_wb_cyc_o  : std_logic;
  signal rst_wb_stb_o  : std_logic;
  signal rst_wb_ack_i  : std_logic;
  signal genrst_n      : std_logic;
  signal rst_wb_i      : t_wb_i;
  signal rst_wb_o      : t_wb_o;

  signal hpll_auxout  : std_logic_vector(2 downto 0);
  signal dmpll_auxout : std_logic_vector(2 downto 0);

  signal clk_ref_slv : std_logic_vector(0 downto 0);
  signal clk_rx_slv  : std_logic_vector(0 downto 0);

  signal s_dummy_addr : std_logic_vector(31 downto 0);
  signal rst_n_inv    : std_logic;

  signal softpll_irq    : std_logic;
  signal softpll_rx_clk : std_logic;

  signal lm32_irq_slv : std_logic_vector(0 downto 0);


  constant c_USE_LM32 : boolean := true;
  
  
begin

  s_rst_n <= genrst_n and rst_n_i;
  s_rst   <= not s_rst_n;
  -----------------------------------------------------------------------------
  --PPS generator
  -----------------------------------------------------------------------------

  PPS_GEN : wrsw_pps_gen
    port map(
      clk_ref_i => clk_ref_i,
      clk_sys_i => clk_sys_i,

      rst_n_i => s_rst_n,

      wb_addr_i => ppsg_wb_i.addr(3 downto 0),
      wb_data_i => ppsg_wb_i.data,
      wb_data_o => ppsg_wb_o.data,
      wb_cyc_i  => ppsg_wb_i.cyc,
      wb_sel_i  => ppsg_wb_i.sel,
      wb_stb_i  => ppsg_wb_i.stb,
      wb_we_i   => ppsg_wb_i.we,
      wb_ack_o  => ppsg_wb_o.ack,

      -- Single-pulse PPS output for synchronizing endpoint to
      pps_in_i => '0',
      pps_csync_o => s_pps_csync,
      pps_out_o   => pps_p_o
      );


  U_SOFTPLL : wr_softpll
    generic map (
      g_deglitcher_threshold => 3000,
      g_tag_bits             => 17)
    port map (
      clk_sys_i  => clk_sys_i,
      rst_n_i    => s_rst_n,
      clk_ref_i  => clk_ref_i,
      clk_dmtd_i => clk_dmtd_i,
      clk_rx_i   => softpll_rx_clk,

      dac_hpll_data_o  => dac_hpll_data_o,
      dac_hpll_load_o  => dac_hpll_load_p1_o,
      dac_dmpll_data_o => dac_dpll_data_o,
      dac_dmpll_load_o => dac_dpll_load_p1_o,

      wb_addr_i => dpll_wb_i.addr(3 downto 0),
      wb_data_i => dpll_wb_i.data,
      wb_data_o => dpll_wb_o.data,
      wb_cyc_i  => dpll_wb_i.cyc,
      wb_sel_i  => dpll_wb_i.sel,
      wb_stb_i  => dpll_wb_i.stb,
      wb_we_i   => dpll_wb_i.we,
      wb_ack_o  => dpll_wb_o.ack,
      wb_irq_o  => softpll_irq,
      debug_o   => dio_o);




  -----------------------------------------------------------------------------
  -- Endpoint
  -----------------------------------------------------------------------------
  WR_ENDPOINT : wrsw_endpoint
    generic map(
      g_simulation          => g_simulation,
      g_phy_mode            => g_ep_phy_mode,
      g_rx_buffer_size_log2 => g_ep_rxbuf_size_log2
      )
    port map(
      -- Endpoint transmit reference clock. Must be 125 MHz +- 100 ppm
      clk_ref_i  => clk_ref_i,
      -- reference clock / 2 (62.5 MHz, in-phase with refclk)
      clk_sys_i  => clk_sys_i,
      -- DMTD sampling clock (125.x MHz) (from Timing system)
      clk_dmtd_i => clk_dmtd_i,

      -- sync reset (clk_sys_i domain), active LO
      rst_n_i => rst_n_i,

      -- PPS input (1 clk_ref_i cycle HI) for synchronizing timestamp counter
      pps_csync_p1_i => s_pps_csync,

      -------------------------------------------------------------------------
      -- Ten-Bit PHY interface (TLK1221)
      -------------------------------------------------------------------------
      tbi_td_o        => tbi_td_o,
      tbi_enable_o    => tbi_enable_o,
      tbi_syncen_o    => tbi_syncen_o,
      tbi_loopen_o    => tbi_loopen_o,
      tbi_prbsen_o    => tbi_prbsen_o,
      tbi_rbclk_i     => tbi_rbclk_i,
      tbi_rd_i        => tbi_rd_i,
      tbi_sync_pass_i => tbi_sync_pass_i,

      -------------------------------------------------------------------------
      -- Xilinx GTP PHY Interace
      -------------------------------------------------------------------------    
      gtp_tx_clk_i       => clk_ref_i,
      gtp_tx_data_o      => s_gtp_tx_data_i,
      gtp_tx_k_o         => s_gtp_tx_k_i,
      gtp_tx_disparity_i => s_gtp_tx_disparity_o,
      gtp_tx_enc_err_i   => s_gtp_tx_enc_err_o,
      gtp_rx_data_i      => s_gtp_rx_data_o,
      gtp_rx_clk_i       => s_gtp_rx_rbclk_o,
      gtp_rx_k_i         => s_gtp_rx_k_o,
      gtp_rx_enc_err_i   => s_gtp_rx_enc_err_o,
      gtp_rx_bitslide_i  => s_gtp_rx_bitslide_o,
      gtp_rst_o          => s_gtp_rst_i,
      gtp_loopen_o       => s_gtp_loopen_i,

      -------------------------------------------------------------------------
      -- WRF source (output of RXed packets)
      -------------------------------------------------------------------------
      rx_data_o      => s_ep_rx_data_o,
      rx_ctrl_o      => s_ep_rx_ctrl_o,
      rx_bytesel_o   => s_ep_rx_bytesel_o,
      rx_sof_p1_o    => s_ep_rx_sof_p1_o,
      rx_eof_p1_o    => s_ep_rx_eof_p1_o,
      rx_dreq_i      => s_ep_rx_dreq_i,
      rx_valid_o     => s_ep_rx_valid_o,
      rx_rabort_p1_i => '0',
      rx_idle_o      => open,
      rx_rerror_p1_o => s_ep_rx_rerror_p1_o,

      -------------------------------------------------------------------------
      -- WRF Sink (input for the packets to be TXed)
      -------------------------------------------------------------------------
      tx_data_i      => s_ep_tx_data_i,
      tx_ctrl_i      => s_ep_tx_ctrl_i,
      tx_bytesel_i   => s_ep_tx_bytesel_i,
      tx_sof_p1_i    => s_ep_tx_sof_p1_i,
      tx_eof_p1_i    => s_ep_tx_eof_p1_i,
      tx_dreq_o      => s_ep_tx_dreq_o,
      tx_valid_i     => s_ep_tx_valid_i,
      tx_rerror_p1_i => '0',
      tx_tabort_p1_i => '0',
      tx_terror_p1_o => s_ep_tx_terror_p1_o,

      -------------------------------------------------------------------------
      -- TX timestamping unit interface
      -------------------------------------------------------------------------  
      txtsu_port_id_o  => txtsu_port_id_o,
      txtsu_frame_id_o => txtsu_frame_id_o,
      txtsu_tsval_o    => txtsu_tsval_o,
      txtsu_valid_o    => txtsu_valid_o,
      txtsu_ack_i      => txtsu_ack_i,

      -------------------------------------------------------------------------
      -- RTU interface
      -------------------------------------------------------------------------
      rtu_full_i         => '0',
      rtu_almost_full_i  => '0',
      rtu_rq_strobe_p1_o => open,
      rtu_rq_smac_o      => open,
      rtu_rq_dmac_o      => open,
      rtu_rq_vid_o       => open,
      rtu_rq_has_vid_o   => open,
      rtu_rq_prio_o      => open,
      rtu_rq_has_prio_o  => open,

      -------------------------------------------------------------------------   
      -- Wishbone bus
      -------------------------------------------------------------------------
      wb_cyc_i  => ep_wb_i.cyc,
      wb_stb_i  => ep_wb_i.stb,
      wb_we_i   => ep_wb_i.we,
      wb_sel_i  => ep_wb_i.sel,
      wb_addr_i => ep_wb_i.addr(5 downto 0),
      wb_data_i => ep_wb_i.data,
      wb_data_o => ep_wb_o.data,
      wb_ack_o  => ep_wb_o.ack
      );


  gen_gtp : if(g_ep_phy_mode = "GTP") generate
    -----------------------------------------------------------------------------
    -- Xilinx GTP PHY
    -----------------------------------------------------------------------------

    softpll_rx_clk <= s_gtp_rx_rbclk_o;

gen_use_channel0: if(g_gtp_channel = 0) generate
    
    GTP_PHY : wr_gtp_phy_spartan6
      generic map(
        g_simulation => g_simulation
--        g_loopback0_mode => 1
        )
      port map(
        clk_ref_i          => clk_ref_i,
        ch0_tx_data_i      => s_gtp_tx_data_i,
        ch0_tx_k_i         => s_gtp_tx_k_i,
        ch0_tx_disparity_o => s_gtp_tx_disparity_o,
        ch0_tx_enc_err_o   => s_gtp_tx_enc_err_o,
        ch0_rx_data_o      => s_gtp_rx_data_o,
        ch0_rx_rbclk_o     => s_gtp_rx_rbclk_o,
        ch0_rx_k_o         => s_gtp_rx_k_o,
        ch0_rx_enc_err_o   => s_gtp_rx_enc_err_o,
        ch0_rx_bitslide_o  => s_gtp_rx_bitslide_o,
        ch0_rst_i          => s_gtp_rst_i,
        ch0_loopen_i       => s_gtp_loopen_i,

        ch1_tx_data_i      => (others => '0'),
        ch1_tx_k_i         => '0',
        ch1_tx_disparity_o => open,
        ch1_tx_enc_err_o   => open,
        ch1_rx_data_o      => open,
        ch1_rx_rbclk_o     => open,
        ch1_rx_k_o         => open,
        ch1_rx_enc_err_o   => open,
        ch1_rx_bitslide_o  => open,
        ch1_rst_i          => '0',
        ch1_loopen_i       => '0',

                                        --   Serial I/O
        pad_txn0_o => pad_txn0_o,
        pad_txp0_o => pad_txp0_o,
        pad_rxn0_i => pad_rxn0_i,
        pad_rxp0_i => pad_rxp0_i,
        pad_txn1_o => open,
        pad_txp1_o => open,
        pad_rxn1_i => '0',
        pad_rxp1_i => '0'
);

end generate gen_use_channel0;

    gen_use_channel1: if(g_gtp_channel = 1) generate
    
      GTP_PHY : wr_gtp_phy_spartan6
      generic map(
        g_simulation => g_simulation
        )
      port map(
        clk_ref_i          => clk_ref_i,
        ch1_tx_data_i      => s_gtp_tx_data_i,
        ch1_tx_k_i         => s_gtp_tx_k_i,
        ch1_tx_disparity_o => s_gtp_tx_disparity_o,
        ch1_tx_enc_err_o   => s_gtp_tx_enc_err_o,
        ch1_rx_data_o      => s_gtp_rx_data_o,
        ch1_rx_rbclk_o     => s_gtp_rx_rbclk_o,
        ch1_rx_k_o         => s_gtp_rx_k_o,
        ch1_rx_enc_err_o   => s_gtp_rx_enc_err_o,
        ch1_rx_bitslide_o  => s_gtp_rx_bitslide_o,
        ch1_rst_i          => s_gtp_rst_i,
        ch1_loopen_i       => s_gtp_loopen_i,

        ch0_tx_data_i      => (others => '0'),
        ch0_tx_k_i         => '0',
        ch0_tx_disparity_o => open,
        ch0_tx_enc_err_o   => open,
        ch0_rx_data_o      => open,
        ch0_rx_rbclk_o     => open,
        ch0_rx_k_o         => open,
        ch0_rx_enc_err_o   => open,
        ch0_rx_bitslide_o  => open,
        ch0_rst_i          => '0',
        ch0_loopen_i       => '0',

                                        --   Serial I/O
        pad_txn1_o => pad_txn0_o,
        pad_txp1_o => pad_txp0_o,
        pad_rxn1_i => pad_rxn0_i,
        pad_rxp1_i => pad_rxp0_i,
        pad_txn0_o => open,
        pad_txp0_o => open,
        pad_rxn0_i => '0',
        pad_rxp0_i => '0'
);

end generate gen_use_channel1;

    
end generate gen_gtp;

  gen_TBI : if(g_ep_phy_mode = "TBI") generate
    softpll_rx_clk <= tbi_rbclk_i;
    
  end generate gen_TBI;

  -----------------------------------------------------------------------------
  -- Mini-NIC
  -----------------------------------------------------------------------------
  MINIC : wr_mini_nic
    generic map(
      g_memsize_log2         => g_mnic_memsize_log2,
      g_buffer_little_endian => g_mnic_buf_little_endian
      )
    port map(
      clk_sys_i        => clk_sys_i,
      rst_n_i          => s_rst_n,
      -------------------------------------------------------------------------
      -- System memory i/f
      -------------------------------------------------------------------------
      mem_data_o       => s_mnic_mem_data_o,
      mem_addr_o       => s_mnic_mem_addr_o,
      mem_data_i       => s_mnic_mem_data_i,
      mem_wr_o         => s_mnic_mem_wr_o,
      -------------------------------------------------------------------------
      -- WRF source/sink
      -------------------------------------------------------------------------
      --mNIC Source -> EP Sink
      src_data_o       => s_ep_tx_data_i,
      src_ctrl_o       => s_ep_tx_ctrl_i,
      src_bytesel_o    => s_ep_tx_bytesel_i,
      src_sof_p1_o     => s_ep_tx_sof_p1_i,
      src_eof_p1_o     => s_ep_tx_eof_p1_i,
      src_dreq_i       => s_ep_tx_dreq_o,
      src_valid_o      => s_ep_tx_valid_i,
      src_error_p1_o   => open,
      src_error_p1_i   => s_ep_tx_terror_p1_o,
      --mNIC Sink <- EP Source
      snk_data_i       => s_ep_rx_data_o,
      snk_ctrl_i       => s_ep_rx_ctrl_o,
      snk_bytesel_i    => s_ep_rx_bytesel_o,
      snk_sof_p1_i     => s_ep_rx_sof_p1_o,
      snk_eof_p1_i     => s_ep_rx_eof_p1_o,
      snk_dreq_o       => s_ep_rx_dreq_i,
      snk_valid_i      => s_ep_rx_valid_o,
      snk_error_p1_o   => open,
      snk_error_p1_i   => s_ep_rx_rerror_p1_o,
      -------------------------------------------------------------------------
      -- TXTSU i/f
      -------------------------------------------------------------------------
      txtsu_port_id_i  => txtsu_port_id_o,
      txtsu_frame_id_i => txtsu_frame_id_o,
      txtsu_tsval_i    => txtsu_tsval_o,
      txtsu_valid_i    => txtsu_valid_o,
      txtsu_ack_o      => txtsu_ack_i,
      -------------------------------------------------------------------------
      -- Wishbone slave
      -------------------------------------------------------------------------    
      wb_cyc_i         => mnic_wb_i.cyc,
      wb_stb_i         => mnic_wb_i.stb,
      wb_we_i          => mnic_wb_i.we,
      wb_sel_i         => mnic_wb_i.sel,
      wb_addr_i        => mnic_wb_i.addr(3 downto 0),
      wb_data_i        => mnic_wb_i.data,
      wb_data_o        => mnic_wb_o.data,
      wb_ack_o         => mnic_wb_o.ack,
      wb_irq_o         => open
      );

  mnic_wb_irq_o <= '0';

  -----------------------------------------------------------------------------
  -- ZPU
  -----------------------------------------------------------------------------

  gen_with_zpu : if(c_USE_LM32 = false) generate
    
    ZPU_CORE : wrc_zpu

      generic map (
        g_spStart       => x"0fff8",
        g_DontCareValue => '0')
      port map(
        clk_i => clk_sys_i,
        rst_i => s_rst,

        --Wishbone Master
        wb_addr_o => zpu_wb_o.addr(17 downto 0),
        wb_data_o => zpu_wb_o.data,
        wb_data_i => zpu_wb_i.data,
        wb_sel_o  => zpu_wb_o.sel,
        wb_we_o   => zpu_wb_o.we,
        wb_cyc_o  => zpu_wb_o.cyc,
        wb_stb_o  => zpu_wb_o.stb,
        wb_int_i  => mnic_wb_irq_o,
        wb_ack_i  => zpu_wb_i.ack
        );

  end generate gen_with_zpu;

  gen_with_LM32 : if(c_USE_LM32 = true) generate

    lm32_irq_slv(0) <= softpll_irq;

    LM32_CORE : wrc_lm32
      generic map (
        g_addr_width => c_aw,
        g_num_irqs   => 1)
      port map (
        clk_i     => clk_sys_i,
        rst_n_i   => s_rst_n,
        irq_i     => lm32_irq_slv,
        iwb_adr_o => lm32_iwb_o.addr,
        iwb_dat_o => open,
        iwb_dat_i => lm32_iwb_i.data,
        iwb_cyc_o => lm32_iwb_o.cyc,
        iwb_stb_o => lm32_iwb_o.stb,
        iwb_ack_i => lm32_iwb_i.ack,

        dwb_adr_o => lm32_dwb_o.addr,
        dwb_dat_o => lm32_dwb_o.data,
        dwb_dat_i => lm32_dwb_i.data,
        dwb_cyc_o => lm32_dwb_o.cyc,
        dwb_stb_o => lm32_dwb_o.stb,
        dwb_sel_o => lm32_dwb_o.sel,
        dwb_we_o  => lm32_dwb_o.we,
        dwb_ack_i => lm32_dwb_i.ack);

    lm32_iwb_o.data <= (others => '0');
    lm32_iwb_o.sel  <= (others => '1');
    lm32_iwb_o.we   <= '0';
    
  end generate gen_with_LM32;

  -----------------------------------------------------------------------------
  -- Dual-port RAM
  -----------------------------------------------------------------------------  
  DPRAM : wrc_dpram
    generic map(
      g_data_width       => 32,
      g_size             => g_dpram_size,
      g_with_byte_enable => true,
      g_init_file        => g_dpram_initf,
      g_dual_clock       => false
      )
    port map(
      clk_i   => clk_sys_i,
      rst_n_i => rst_n_i,

      --PORT A (Wishbone)
      wb_addr_i  => dpram_wb_i.addr(13 downto 0),
      wb_data_i  => dpram_wb_i.data,
      wb_data_o  => dpram_wb_o.data,
      wb_sel_i   => dpram_wb_i.sel,
      wb_cyc_i   => dpram_wb_i.cyc,
      wb_stb_i   => dpram_wb_i.stb,
      wb_we_i    => dpram_wb_i.we,
      wb_ack_o   => dpram_wb_o.ack,
      --PORT B (miniNIC)
      mem_addr_i => s_mnic_mem_addr_o,
      mem_data_i => s_mnic_mem_data_o,
      mem_data_o => s_mnic_mem_data_i,
      mem_wr_i   => s_mnic_mem_wr_o
      );


  -----------------------------------------------------------------------------
  -- WB Peripherials
  -----------------------------------------------------------------------------
  PERIPH : wrc_periph
    generic map(
      g_gpio_pins    => g_num_gpio,
      g_virtual_uart => g_virtual_uart,
      g_tics_period  => 62500
      )
    port map(
      clk_sys_i => clk_sys_i,
      clk_ref_i => clk_ref_i,
      rst_n_i   => rst_n_i,

      gpio_o     => gpio_o,
      gpio_i     => gpio_i,
      gpio_dir_o     => gpio_dir_o,

      uart_rxd_i => uart_rxd_i,
      uart_txd_o => uart_txd_o,
      genrst_n_o => genrst_n,

      wb_addr_i => periph_wb_i.addr(11 downto 0),
      wb_data_i => periph_wb_i.data,
      wb_data_o => periph_wb_o.data,
      wb_sel_i  => periph_wb_i.sel,
      wb_stb_i  => periph_wb_i.stb,
      wb_cyc_i  => periph_wb_i.cyc,
      wb_we_i   => periph_wb_i.we,
      wb_ack_o  => periph_wb_o.ack
      );

--  gpio_o <= gpio_b;

  -----------------------------------------------------------------------------
  -- External WB interface
  -----------------------------------------------------------------------------
  ext_wb_o.addr <= wb_addr_i;
  ext_wb_o.data <= wb_data_i;
  ext_wb_o.sel  <= wb_sel_i;
  ext_wb_o.we   <= wb_we_i;
  ext_wb_o.cyc  <= wb_cyc_i;
  ext_wb_o.stb  <= wb_stb_i;
  wb_data_o     <= ext_wb_i.data;
  wb_ack_o      <= ext_wb_i.ack;

  -----------------------------------------------------------------------------
  -- WB intercon
  -----------------------------------------------------------------------------
  wbm_unused_i.data <= (others => '0');
  wbm_unused_i.addr <= (others => '0');
  wbm_unused_i.sel  <= (others => '0');
  wbm_unused_i.we   <= '0';
  wbm_unused_i.cyc  <= '0';
  wbm_unused_i.stb  <= '0';

  wbs_unused_i.data <= (others => '0');
  wbs_unused_i.ack  <= '0';
  wbs_unused_i.err  <= '0';
  wbs_unused_i.rty  <= '0';

  rst_n_inv <= not rst_n_i;

  WB_CON : wb_conmax_top
    generic map(
      g_rf_addr => g_wbicon_rf_addr
      )
    port map(
      clk_i => clk_sys_i,
      rst_i => rst_n_inv,

      wb_masters_i => cnx_master_i,
      wb_masters_o => cnx_master_o,
      wb_slaves_i  => cnx_slave_i,
      wb_slaves_o  => cnx_slave_o
      );

  gen_with_lm32_2 : if(c_USE_LM32 = true) generate
    cnx_master_i(0) <= lm32_iwb_o;
    cnx_master_i(2) <= lm32_dwb_o;
    lm32_iwb_i      <= cnx_master_o(0);
    lm32_dwb_i      <= cnx_master_o(2);
  end generate gen_with_lm32_2;

  gen_with_zpu_2 : if(c_USE_LM32 = false) generate
    cnx_master_i(0) <= zpu_wb_o;
    cnx_master_i(2) <= wbm_unused_i;
    zpu_wb_i        <= cnx_master_o(0);
  end generate gen_with_zpu_2;


--  cnx_master_i(0) <= zpu_wb_o;
  cnx_master_i(1) <= ext_wb_o;
--  cnx_master_i(2) <= wbm_unused_i;
  cnx_master_i(3) <= wbm_unused_i;
  cnx_master_i(4) <= wbm_unused_i;
  cnx_master_i(5) <= wbm_unused_i;
  cnx_master_i(6) <= wbm_unused_i;
  cnx_master_i(7) <= wbm_unused_i;

--  zpu_wb_i <= cnx_master_o(0);
  ext_wb_i <= cnx_master_o(1);

  cnx_slave_i(0)  <= dpram_wb_o;
  cnx_slave_i(1)  <= mnic_wb_o;
  cnx_slave_i(2)  <= ep_wb_o;
  cnx_slave_i(3)  <= hpll_wb_o;
  cnx_slave_i(4)  <= dpll_wb_o;
  cnx_slave_i(5)  <= ppsg_wb_o;
  cnx_slave_i(6)  <= periph_wb_o;       --gpio_wb_o;
  cnx_slave_i(7)  <= wbs_unused_i;
  cnx_slave_i(8)  <= wbs_unused_i;
  cnx_slave_i(9)  <= wbs_unused_i;
  cnx_slave_i(10) <= wbs_unused_i;
  cnx_slave_i(11) <= wbs_unused_i;
  cnx_slave_i(12) <= wbs_unused_i;
  cnx_slave_i(13) <= wbs_unused_i;
  cnx_slave_i(14) <= wbs_unused_i;
  cnx_slave_i(15) <= wbs_unused_i;

  dpram_wb_i  <= cnx_slave_o(0);
  mnic_wb_i   <= cnx_slave_o(1);
  ep_wb_i     <= cnx_slave_o(2);
  hpll_wb_i   <= cnx_slave_o(3);
  dpll_wb_i   <= cnx_slave_o(4);
  ppsg_wb_i   <= cnx_slave_o(5);
  periph_wb_i <= cnx_slave_o(6);


end struct;
