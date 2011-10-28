-------------------------------------------------------------------------------
-- Title      : WhiteRabbit PTP Core
-- Project    : WhiteRabbit
-------------------------------------------------------------------------------
-- File       : wr_core.vhd
-- Author     : Grzegorz Daniluk
-- Company    : Elproma
-- Created    : 2011-02-02
-- Last update: 2011-10-28
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
use work.genram_pkg.all;

use work.wr_fabric_pkg.all;
use work.endpoint_pkg.all;
use work.wishbone_pkg.all;

entity wr_core is
  generic(
    --if set to 1, then blocks in PCS use smaller calibration counter to speed 
    --up simulation
    g_simulation         : integer := 0;
    g_virtual_uart       : natural := 0;
    g_ep_rxbuf_size_log2 : integer := 12;
    g_dpram_initf        : string  := "";
    g_dpram_size         : integer := 16384;  --in 32-bit words
    g_num_gpio           : integer := 8
    );
  port(
    clk_sys_i : in std_logic;

    -- DDMTD offset lcock (125.x MHz)
    clk_dmtd_i : in std_logic;

    -- Timing reference (125 MHz)
    clk_ref_i : in std_logic;

    -- Aux clock (i.e. the FMC clock)
    clk_aux_i : in std_logic;


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

    -- PHY I/f
    phy_ref_clk_i : in std_logic;

    phy_tx_data_o      : out std_logic_vector(7 downto 0);
    phy_tx_k_o         : out std_logic;
    phy_tx_disparity_i : in  std_logic;
    phy_tx_enc_err_i   : in  std_logic;

    phy_rx_data_i     : in std_logic_vector(7 downto 0);
    phy_rx_rbclk_i    : in std_logic;
    phy_rx_k_i        : in std_logic;
    phy_rx_enc_err_i  : in std_logic;
    phy_rx_bitslide_i : in std_logic_vector(3 downto 0);

    phy_rst_o    : out std_logic;
    phy_loopen_o : out std_logic;

    -----------------------------------------
    --GPIO
    -----------------------------------------
    gpio_o     : out std_logic_vector(g_num_gpio-1 downto 0);
    gpio_i     : in  std_logic_vector(g_num_gpio-1 downto 0);
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

-------------------------------------------------------------------------------
-- External Fabric I/F
-------------------------------------------------------------------------------    
    ext_snk_adr_i   : in  std_logic_vector(1 downto 0)  := "00";
    ext_snk_dat_i   : in  std_logic_vector(15 downto 0) := x"0000";
    ext_snk_sel_i   : in  std_logic_vector(1 downto 0)  := "00";
    ext_snk_cyc_i   : in  std_logic                     := '0';
    ext_snk_we_i    : in  std_logic                     := '0';
    ext_snk_stb_i   : in  std_logic                     := '0';
    ext_snk_ack_o   : out std_logic;
    ext_snk_err_o   : out std_logic;
    ext_snk_stall_o : out std_logic;


    ext_src_adr_o   : out std_logic_vector(1 downto 0);
    ext_src_dat_o   : out std_logic_vector(15 downto 0);
    ext_src_sel_o   : out std_logic_vector(1 downto 0);
    ext_src_cyc_o   : out std_logic;
    ext_src_stb_o   : out std_logic;
    ext_src_we_o    : out std_logic;
    ext_src_ack_i   : in  std_logic := '1';
    ext_src_err_i   : in  std_logic := '0';
    ext_src_stall_i : in  std_logic := '0';

-------------------------------------------------------------------------------
-- Timecode/Servo Control
-------------------------------------------------------------------------------

    
    -- DAC Control
    tm_dac_value_o: out std_logic_vector(23 downto 0);
    tm_dac_wr_o: out std_logic;

    -- Aux clock lock enable
    tm_clk_aux_lock_en_i: in std_logic;

    -- Aux clock locked flag
    tm_clk_aux_locked_o: out std_logic;

    -- Timecode output
    tm_time_valid_o: out std_logic;
    tm_utc_o: out std_logic_vector(31 downto 0);
    tm_cycles_o: out std_logic_vector(27 downto 0);
    
    
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

  signal txtsu_port_id_o  : std_logic_vector(4 downto 0);
  signal txtsu_frame_id_o : std_logic_vector(16 -1 downto 0);
  signal txtsu_tsval_o    : std_logic_vector(28 + 4 - 1 downto 0);
  signal txtsu_valid_o    : std_logic;
  signal txtsu_ack_i      : std_logic;

  signal ep_wb_i : t_wb_i;
  signal ep_wb_o : t_wb_o;


  constant c_mnic_memsize_log2 : integer := f_log2_size(g_dpram_size);

  -----------------------------------------------------------------------------
  --Mini-NIC
  -----------------------------------------------------------------------------
  signal s_mnic_mem_data_o : std_logic_vector(31 downto 0);
  signal s_mnic_mem_addr_o : std_logic_vector(c_mnic_memsize_log2-1 downto 0);
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
  signal lm32_jwb_i : t_wb_i;
  signal lm32_jwb_o : t_wb_o;
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

  signal genrst_n : std_logic;
  signal rst_wb_i : t_wb_i;
  signal rst_wb_o : t_wb_o;

  signal hpll_auxout  : std_logic_vector(2 downto 0);
  signal dmpll_auxout : std_logic_vector(2 downto 0);

  signal clk_ref_slv : std_logic_vector(0 downto 0);
  signal clk_rx_slv  : std_logic_vector(0 downto 0);

  signal s_dummy_addr : std_logic_vector(31 downto 0);
  signal rst_n_inv    : std_logic;

  signal softpll_irq : std_logic;

  signal lm32_irq_slv : std_logic_vector(0 downto 0);
  signal trace_pc     : std_logic_vector(31 downto 0);
  signal trace_eret   : std_logic;
  signal trace_valid  : std_logic;

  signal ep_wb_in  : t_wishbone_slave_in;
  signal ep_wb_out : t_wishbone_slave_out;

  signal minic_wb_in  : t_wishbone_slave_in;
  signal minic_wb_out : t_wishbone_slave_out;

  signal ep_src_out : t_wrf_source_out;
  signal ep_src_in  : t_wrf_source_in;
  signal ep_snk_out : t_wrf_sink_out;
  signal ep_snk_in  : t_wrf_sink_in;

  signal minic_src_out : t_wrf_source_out;
  signal minic_src_in  : t_wrf_source_in;
  signal minic_snk_out : t_wrf_sink_out;
  signal minic_snk_in  : t_wrf_sink_in;


  signal ext_src_out : t_wrf_source_out;
  signal ext_src_in  : t_wrf_source_in;
  signal ext_snk_out : t_wrf_sink_out;
  signal ext_snk_in  : t_wrf_sink_in;

  signal dummy : std_logic_vector(31 downto 0);

  component chipscope_ila
    port (
      CONTROL : inout std_logic_vector(35 downto 0);
      CLK     : in    std_logic;
      TRIG0   : in    std_logic_vector(31 downto 0);
      TRIG1   : in    std_logic_vector(31 downto 0);
      TRIG2   : in    std_logic_vector(31 downto 0);
      TRIG3   : in    std_logic_vector(31 downto 0));
  end component;

  component chipscope_icon
    port (
      CONTROL0 : inout std_logic_vector (35 downto 0));
  end component;

  component xwbp_mux
    port (
      clk_sys_i    : in  std_logic;
      rst_n_i      : in  std_logic;
      ep_src_o     : out t_wrf_source_out;
      ep_src_i     : in  t_wrf_source_in;
      ep_snk_o     : out t_wrf_sink_out;
      ep_snk_i     : in  t_wrf_sink_in;
      ptp_src_o    : out t_wrf_source_out;
      ptp_src_i    : in  t_wrf_source_in;
      ptp_snk_o    : out t_wrf_sink_out;
      ptp_snk_i    : in  t_wrf_sink_in;
      ext_src_o    : out t_wrf_source_out;
      ext_src_i    : in  t_wrf_source_in;
      ext_snk_o    : out t_wrf_sink_out;
      ext_snk_i    : in  t_wrf_sink_in;
      class_core_i : in  std_logic_vector(7 downto 0));
  end component;

  signal CONTROL : std_logic_vector(35 downto 0);
  signal CLK     : std_logic;
  signal TRIG0   : std_logic_vector(31 downto 0);
  signal TRIG1   : std_logic_vector(31 downto 0);
  signal TRIG2   : std_logic_vector(31 downto 0);
  signal TRIG3   : std_logic_vector(31 downto 0);
begin

  genrest_n <= genrst_n;

  s_rst_n <= genrst_n and rst_n_i;
  s_rst   <= not s_rst_n;
  -----------------------------------------------------------------------------
  --PPS generator
  -----------------------------------------------------------------------------

  PPS_GEN : wr_pps_gen
    port map(
      clk_ref_i => clk_ref_i,
      clk_sys_i => clk_sys_i,

      rst_n_i => s_rst_n,

      wb_adr_i => ppsg_wb_i.addr(4 downto 0),
      wb_dat_i => ppsg_wb_i.data,
      wb_dat_o => ppsg_wb_o.data,
      wb_cyc_i  => ppsg_wb_i.cyc,
      wb_sel_i  => ppsg_wb_i.sel,
      wb_stb_i  => ppsg_wb_i.stb,
      wb_we_i   => ppsg_wb_i.we,
      wb_ack_o  => ppsg_wb_o.ack,

      -- Single-pulse PPS output for synchronizing endpoint to
      pps_in_i    => '0',
      pps_csync_o => s_pps_csync,
      pps_out_o   => pps_p_o
      );


  U_SOFTPLL : wr_softpll
    generic map (
      g_deglitcher_threshold => 3000,
      g_tag_bits             => 20)
    port map (
      clk_sys_i  => clk_sys_i,
      rst_n_i    => s_rst_n,
      clk_ref_i  => clk_ref_i,
      clk_dmtd_i => clk_dmtd_i,
      clk_rx_i   => phy_rx_rbclk_i,
      clk_aux_i  => clk_aux_i,

      dac_hpll_data_o  => dac_hpll_data_o,
      dac_hpll_load_o  => dac_hpll_load_p1_o,

      dac_dmpll_data_o => dac_dpll_data_o,
      dac_dmpll_load_o => dac_dpll_load_p1_o,

      dac_aux_data_o => tm_dac_value_o,
      dac_aux_load_o => tm_dac_wr_o,

      clk_aux_lock_en_i => tm_clk_aux_lock_en_i,
      clk_aux_locked_o => tm_clk_aux_locked_o,
      
      wb_adr_i => dpll_wb_i.addr(6 downto 0),
      wb_dat_i => dpll_wb_i.data,
      wb_dat_o => dpll_wb_o.data,
      wb_cyc_i  => dpll_wb_i.cyc,
      wb_sel_i  => dpll_wb_i.sel,
      wb_stb_i  => dpll_wb_i.stb,
      wb_we_i   => dpll_wb_i.we,
      wb_ack_o  => dpll_wb_o.ack,
      wb_irq_o  => softpll_irq,
      debug_o   => dio_o);


  U_Endpoint : xwr_endpoint
    generic map (
      g_interface_mode      => CLASSIC,
      g_address_granularity => WORD,
      g_simulation          => false,
      g_pcs_16bit           => false,
      g_rx_buffer_size      => 1024,
      g_with_rx_buffer      => true,
      g_with_flow_control   => false,
      g_with_timestamper    => true,
      g_with_dpi_classifier => true,
      g_with_vlans          => false,
      g_with_rtu            => false,
      g_with_leds           => false)
    port map (
      clk_ref_i      => clk_ref_i,
      clk_sys_i      => clk_sys_i,
      rst_n_i        => s_rst_n,
      pps_csync_p1_i => s_pps_csync,

      phy_rst_o                     => phy_rst_o,
      phy_loopen_o                  => phy_loopen_o,
--      phy_enable_o       => phy_enable_o,
--      phy_syncen_o       => phy_syncen_o,
      phy_ref_clk_i                 => phy_ref_clk_i,
      phy_tx_data_o(7 downto 0)     => phy_tx_data_o,
      phy_tx_data_o(15 downto 8)    => dummy(7 downto 0),
      phy_tx_k_o(0)                 => phy_tx_k_o,
      phy_tx_k_o(1)                 => dummy(8),
      phy_tx_disparity_i            => phy_tx_disparity_i,
      phy_tx_enc_err_i              => phy_tx_enc_err_i,
      phy_rx_data_i(7 downto 0)     => phy_rx_data_i,
      phy_rx_data_i(15 downto 8)    => x"00",
      phy_rx_clk_i                  => phy_rx_rbclk_i,
      phy_rx_k_i(0)                 => phy_rx_k_i,
      phy_rx_k_i(1)                 => '0',
      phy_rx_enc_err_i              => phy_rx_enc_err_i,
      phy_rx_bitslide_i(3 downto 0) => phy_rx_bitslide_i,
      phy_rx_bitslide_i(4)          => '0',

      src_o => ep_src_out,
      src_i => ep_src_in,
      snk_o => ep_snk_out,
      snk_i => ep_snk_in,

      txtsu_port_id_o  => txtsu_port_id_o,
      txtsu_frame_id_o => txtsu_frame_id_o,
      txtsu_tsval_o    => txtsu_tsval_o,
      txtsu_valid_o    => txtsu_valid_o,
      txtsu_ack_i      => txtsu_ack_i,
      wb_i             => ep_wb_in,
      wb_o             => ep_wb_out);

  ep_wb_in.cyc              <= ep_wb_i.cyc;
  ep_wb_in.stb              <= ep_wb_i.stb;
  ep_wb_in.we               <= ep_wb_i.we;
  ep_wb_in.sel              <= ep_wb_i.sel;
  ep_wb_in.adr(10 downto 0) <= ep_wb_i.addr(10 downto 0);
  ep_wb_in.dat              <= ep_wb_i.data;
  ep_wb_o.ack               <= ep_wb_out.ack;
  ep_wb_o.data              <= ep_wb_out.dat;

  xwr_mini_nic_1 : xwr_mini_nic
    generic map (
      g_interface_mode       => CLASSIC,
      g_address_granularity  => WORD,
      g_memsize_log2         => 14,
      g_buffer_little_endian => false)
    port map (
      clk_sys_i => clk_sys_i,
      rst_n_i   => s_rst_n,

      mem_data_o => s_mnic_mem_data_o,
      mem_addr_o => s_mnic_mem_addr_o,
      mem_data_i => s_mnic_mem_data_i,
      mem_wr_o   => s_mnic_mem_wr_o,

      src_o => minic_src_out,
      src_i => minic_src_in,
      snk_o => minic_snk_out,
      snk_i => minic_snk_in,

      txtsu_port_id_i  => txtsu_port_id_o,
      txtsu_frame_id_i => txtsu_frame_id_o,
      txtsu_tsval_i    => txtsu_tsval_o,
      txtsu_valid_i    => txtsu_valid_o,
      txtsu_ack_o      => txtsu_ack_i,

      wb_i => minic_wb_in,
      wb_o => minic_wb_out);

  minic_wb_in.cyc              <= mnic_wb_i.cyc;
  minic_wb_in.stb              <= mnic_wb_i.stb;
  minic_wb_in.we               <= mnic_wb_i.we;
  minic_wb_in.sel              <= mnic_wb_i.sel;
  minic_wb_in.adr(10 downto 0) <= mnic_wb_i.addr(10 downto 0);
  minic_wb_in.dat              <= mnic_wb_i.data;
  mnic_wb_o.ack                <= minic_wb_out.ack;
  mnic_wb_o.data               <= minic_wb_out.dat;


  mnic_wb_irq_o   <= '0';
  lm32_irq_slv(0) <= softpll_irq;  -- according to the doc, it's active low.



  LM32_CORE : wrc_lm32
    generic map (
      g_addr_width => c_aw,
      g_num_irqs   => 1)
    port map (
      clk_i   => clk_sys_i,
      rst_n_i => s_rst_n,
      irq_i   => lm32_irq_slv,

      iwb_adr_o => lm32_iwb_o.addr,
      iwb_dat_o => lm32_iwb_o.data,
      iwb_dat_i => lm32_iwb_i.data,
      iwb_cyc_o => lm32_iwb_o.cyc,
      iwb_stb_o => lm32_iwb_o.stb,
      iwb_sel_o => lm32_iwb_o.sel,
      iwb_we_o  => lm32_iwb_o.we,
      iwb_ack_i => lm32_iwb_i.ack,

      dwb_adr_o => lm32_dwb_o.addr,
      dwb_dat_o => lm32_dwb_o.data,
      dwb_dat_i => lm32_dwb_i.data,
      dwb_cyc_o => lm32_dwb_o.cyc,
      dwb_stb_o => lm32_dwb_o.stb,
      dwb_sel_o => lm32_dwb_o.sel,
      dwb_we_o  => lm32_dwb_o.we,
      dwb_ack_i => lm32_dwb_i.ack,

      jwb_adr_i => lm32_jwb_i.addr,
      jwb_dat_i => lm32_jwb_i.data,
      jwb_dat_o => lm32_jwb_o.data,
      jwb_cyc_i => lm32_jwb_i.cyc,
      jwb_stb_i => lm32_jwb_i.stb,
      jwb_sel_i => lm32_jwb_i.sel,
      jwb_we_i  => lm32_jwb_i.we,
      jwb_ack_o => lm32_jwb_o.ack,

      trace_pc_o       => trace_pc,
      trace_pc_valid_o => trace_valid,
      trace_eret_o     => trace_eret);

  -----------------------------------------------------------------------------
  -- Dual-port RAM
  -----------------------------------------------------------------------------  
  DPRAM : wrc_dpram
    generic map(
      g_size      => g_dpram_size,
      g_init_file => g_dpram_initf
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
      gpio_dir_o => gpio_dir_o,

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
      g_rf_addr => 15
      )
    port map(
      clk_i => clk_sys_i,
      rst_i => rst_n_inv,

      wb_masters_i => cnx_master_i,
      wb_masters_o => cnx_master_o,
      wb_slaves_i  => cnx_slave_i,
      wb_slaves_o  => cnx_slave_o
      );

  cnx_master_i(0) <= lm32_iwb_o;
  cnx_master_i(2) <= lm32_dwb_o;
  lm32_iwb_i      <= cnx_master_o(0);
  lm32_dwb_i      <= cnx_master_o(2);

  cnx_master_i(1) <= ext_wb_o;
  cnx_master_i(3) <= wbm_unused_i;
  cnx_master_i(4) <= wbm_unused_i;
  cnx_master_i(5) <= wbm_unused_i;
  cnx_master_i(6) <= wbm_unused_i;
  cnx_master_i(7) <= wbm_unused_i;

  ext_wb_i <= cnx_master_o(1);

  cnx_slave_i(0)  <= dpram_wb_o;
  cnx_slave_i(1)  <= mnic_wb_o;
  cnx_slave_i(2)  <= ep_wb_o;
  cnx_slave_i(3)  <= hpll_wb_o;
  cnx_slave_i(4)  <= dpll_wb_o;
  cnx_slave_i(5)  <= ppsg_wb_o;
  cnx_slave_i(6)  <= periph_wb_o;       --gpio_wb_o;
  cnx_slave_i(7)  <= lm32_jwb_o;
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
  lm32_jwb_i  <= cnx_slave_o(7);


  --chipscope_ila_1 : chipscope_ila
  --  port map (
  --    CONTROL => CONTROL,
  --    CLK     => clk_sys_i,
  --    TRIG0   => TRIG0,
  --    TRIG1   => TRIG1,
  --    TRIG2   => TRIG2,
  --    TRIG3   => TRIG3);

  --chipscope_icon_1 : chipscope_icon
  --  port map (
  --    CONTROL0 => CONTROL);

  --TRIG0(15 downto 0)                            <= ext_src_out.dat;
  --trig0(17 downto 16) <= ext_src_out.adr;
  --trig0(19 downto 18) <= ext_src_out.sel;
  --trig0(20) <= ext_src_out.cyc;
  --trig0(21) <= ext_src_out.stb;
  --trig0(22) <= ext_src_out.we;
  --trig0(23) <= ext_src_in.ack;
  --trig0(24) <= ext_src_in.stall;
  --trig0(26) <= ext_src_in.err;

 

  --TRIG2(15 downto 0)                            <= ext_snk_in.dat;
  --trig2(17 downto 16) <= ext_snk_in.adr;
  --trig2(19 downto 18) <= ext_snk_in.sel;
  --trig2(20) <= ext_snk_in.cyc;
  --trig2(21) <= ext_snk_in.stb;
  --trig2(22) <= ext_snk_in.we;
  --trig2(23) <= ext_snk_out.ack;
  --trig2(24) <= ext_snk_out.stall;
  --trig2(26) <= ext_snk_out.err;

    

  U_WBP_Mux : xwbp_mux
    port map (
      clk_sys_i    => clk_sys_i,
      rst_n_i      => rst_n_i,
      ep_src_o     => ep_snk_in,
      ep_src_i     => ep_snk_out,
      ep_snk_o     => ep_src_in,
      ep_snk_i     => ep_src_out,
      ptp_src_o    => minic_snk_in,
      ptp_src_i    => minic_snk_out,
      ptp_snk_o    => minic_src_in,
      ptp_snk_i    => minic_src_out,
      ext_src_o    => ext_src_out,
      ext_src_i    => ext_src_in,
      ext_snk_o    => ext_snk_out,
      ext_snk_i    => ext_snk_in,
      class_core_i => "00001111");

  ext_src_adr_o <= ext_src_out.adr;
  ext_src_dat_o <= ext_src_out.dat;
  ext_src_stb_o <= ext_src_out.stb;
  ext_src_cyc_o <= ext_src_out.cyc;
  ext_src_sel_o <= ext_src_out.sel;
  ext_src_we_o  <= '1';

  ext_src_in.ack   <= ext_src_ack_i;
  ext_src_in.stall <= ext_src_stall_i;
  ext_src_in.err   <= ext_src_err_i;

  ext_snk_in.adr <= ext_snk_adr_i;
  ext_snk_in.dat <= ext_snk_dat_i;
  ext_snk_in.stb <= ext_snk_stb_i;
  ext_snk_in.cyc <= ext_snk_cyc_i;
  ext_snk_in.sel <= ext_snk_sel_i;
  ext_snk_in.we  <= ext_snk_we_i;

  ext_snk_ack_o   <= ext_snk_out.ack;
  ext_snk_err_o   <= ext_snk_out.err;
  ext_snk_stall_o <= ext_snk_out.stall;

end struct;
