library ieee;
use ieee.std_logic_1164.all;

library work;
use work.genram_pkg.all;
use work.wbconmax_pkg.all;

package wrcore_pkg is

  component wb_conmax_top
    generic (
      g_rf_addr : integer range 0 to 15);
    port (
      clk_i        :     std_logic;
      rst_i        :     std_logic;
      wb_masters_i : in  t_conmax_masters_i;
      wb_masters_o : out t_conmax_masters_o;
      wb_slaves_i  : in  t_conmax_slaves_i;
      wb_slaves_o  : out t_conmax_slaves_o);
  end component;
  -----------------------------------------------------------------------------
  --PPS generator
  -----------------------------------------------------------------------------
  component wrsw_pps_gen
    port (
      clk_ref_i   : in  std_logic;
      clk_sys_i   : in  std_logic;
      rst_n_i     : in  std_logic;
      wb_addr_i   : in  std_logic_vector(3 downto 0);
      wb_data_i   : in  std_logic_vector(31 downto 0);
      wb_data_o   : out std_logic_vector(31 downto 0);
      wb_cyc_i    : in  std_logic;
      wb_sel_i    : in  std_logic_vector(3 downto 0);
      wb_stb_i    : in  std_logic;
      wb_we_i     : in  std_logic;
      wb_ack_o    : out std_logic;
      pps_in_i    : in  std_logic;
      pps_csync_o : out std_logic;
      pps_out_o   : out std_logic);
  end component;

-----------------------------------------------------------------------------
  --WR Endpoint
  -----------------------------------------------------------------------------
  component wrsw_endpoint is
  generic (
    g_simulation          : integer;
    g_phy_mode            : string  := "TBI";
    g_rx_buffer_size_log2 : integer := 12);
  port (
    clk_ref_i : in std_logic;
    clk_sys_i : in std_logic;
    clk_dmtd_i : in std_logic;
    rst_n_i : in std_logic;
    pps_csync_p1_i : in std_logic;
    --TBI
    tbi_td_o     : out std_logic_vector(9 downto 0);
    tbi_enable_o : out std_logic;
    tbi_syncen_o : out std_logic;
    tbi_loopen_o : out std_logic;
    tbi_prbsen_o : out std_logic;
    tbi_rbclk_i : in std_logic;
    tbi_rd_i : in std_logic_vector(9 downto 0);
    tbi_sync_pass_i : in std_logic;
    --GTP
    gtp_tx_clk_i       : in  std_logic;
    gtp_tx_data_o      : out std_logic_vector(7 downto 0);
    gtp_tx_k_o         : out std_logic;
    gtp_tx_disparity_i : in  std_logic;
    gtp_tx_enc_err_i   : in  std_logic;
    gtp_rx_data_i     : in std_logic_vector(7 downto 0);
    gtp_rx_clk_i      : in std_logic;
    gtp_rx_k_i        : in std_logic;
    gtp_rx_enc_err_i  : in std_logic;
    gtp_rx_bitslide_i : in std_logic_vector(3 downto 0);
    gtp_rst_o    : out std_logic;
    gtp_loopen_o : out std_logic;
    --WRF Source
    rx_data_o : out std_logic_vector(15 downto 0);
    rx_ctrl_o : out std_logic_vector(4 - 1 downto 0);
    rx_bytesel_o : out std_logic;
    rx_sof_p1_o : out std_logic;
    rx_eof_p1_o : out std_logic;
    rx_dreq_i : in std_logic;
    rx_valid_o : out std_logic;
    rx_rabort_p1_i : in std_logic;
    rx_idle_o : out std_logic;
    rx_rerror_p1_o : out std_logic;
    --WRF Sink
    tx_data_i : in std_logic_vector(15 downto 0);
    tx_ctrl_i : in std_logic_vector(4 -1 downto 0);
    tx_bytesel_i : in std_logic;
    tx_sof_p1_i : in std_logic;
    tx_eof_p1_i : in std_logic;
    tx_dreq_o : out std_logic;
    tx_valid_i : in std_logic;
    tx_rerror_p1_i : in std_logic;
    tx_tabort_p1_i : in std_logic;
    tx_terror_p1_o : out std_logic;
    --TXTSU
    txtsu_port_id_o : out std_logic_vector(4 downto 0);
    txtsu_frame_id_o : out std_logic_vector(16 -1 downto 0);
    txtsu_tsval_o : out std_logic_vector(28 + 4 - 1 downto 0);
    txtsu_valid_o : out std_logic;
    txtsu_ack_i : in std_logic;
    --RTU
    rtu_full_i : in std_logic;
    rtu_almost_full_i : in std_logic;
    rtu_rq_strobe_p1_o : out std_logic;
    rtu_rq_smac_o : out std_logic_vector(48 - 1 downto 0);
    rtu_rq_dmac_o : out std_logic_vector(48 - 1 downto 0);
    rtu_rq_vid_o : out std_logic_vector(12 - 1 downto 0);
    rtu_rq_has_vid_o : out std_logic;
    rtu_rq_prio_o : out std_logic_vector(3-1 downto 0);
    rtu_rq_has_prio_o : out std_logic;
    --WB
    wb_cyc_i  : in  std_logic;
    wb_stb_i  : in  std_logic;
    wb_we_i   : in  std_logic;
    wb_sel_i  : in  std_logic_vector(3 downto 0);
    wb_addr_i : in  std_logic_vector(5 downto 0);
    wb_data_i : in  std_logic_vector(31 downto 0);
    wb_data_o : out std_logic_vector(31 downto 0);
    wb_ack_o  : out std_logic
  );
  end component;
  
  -----------------------------------------------------------------------------
  --GTP PHY
  -----------------------------------------------------------------------------
  component wr_gtp_phy_spartan6 is
  generic (
    g_simulation : integer := 1
  );
  port (
    clk_ref_i : in std_logic;
    ch0_tx_data_i      : in  std_logic_vector(7 downto 0);
    ch0_tx_k_i         : in  std_logic;
    ch0_tx_disparity_o : out std_logic;
    ch0_tx_enc_err_o   : out std_logic;
    ch0_rx_data_o      : out std_logic_vector(7 downto 0);
    ch0_rx_rbclk_o     : out std_logic;
    ch0_rx_k_o         : out std_logic;
    ch0_rx_enc_err_o   : out std_logic;
    ch0_rx_bitslide_o  : out std_logic_vector(3 downto 0);
    ch0_rst_i          : in std_logic;
    ch0_loopen_i       : in std_logic;

    ch1_tx_data_i      : in  std_logic_vector(7 downto 0);
    ch1_tx_k_i         : in  std_logic;
    ch1_tx_disparity_o : out std_logic;
    ch1_tx_enc_err_o   : out std_logic;
    ch1_rx_data_o      : out std_logic_vector(7 downto 0);
    ch1_rx_rbclk_o     : out std_logic;
    ch1_rx_k_o         : out std_logic;
    ch1_rx_enc_err_o   : out std_logic;
    ch1_rx_bitslide_o  : out std_logic_vector(3 downto 0);
    ch1_rst_i          : in std_logic;
    ch1_loopen_i       : in std_logic;
    
    -- Serial I/O
    pad_txn0_o : out std_logic;
    pad_txp0_o : out std_logic;
    pad_rxn0_i : in std_logic;
    pad_rxp0_i : in std_logic;
    pad_txn1_o : out std_logic;
    pad_txp1_o : out std_logic;
    pad_rxn1_i : in std_logic;
    pad_rxp1_i : in std_logic
  );
  end component;
  
  -----------------------------------------------------------------------------
  --Mini NIC
  -----------------------------------------------------------------------------
  component wr_mini_nic is
  generic (
    g_memsize_log2         : integer := 14;
    g_buffer_little_endian : boolean := true
  );
  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;
      -- System memory i/f
    mem_data_o : out std_logic_vector(31 downto 0);
    mem_addr_o : out std_logic_vector(g_memsize_log2-1 downto 0);
    mem_data_i : in  std_logic_vector(31 downto 0);
    mem_wr_o   : out std_logic;
      -- WRF source/sink
    src_data_o     : out std_logic_vector(15 downto 0);
    src_ctrl_o     : out std_logic_vector(4 - 1 downto 0);
    src_bytesel_o  : out std_logic;
    src_sof_p1_o   : out std_logic;
    src_eof_p1_o   : out std_logic;
    src_dreq_i     : in  std_logic;
    src_valid_o    : out std_logic;
    src_error_p1_o : out std_logic;
    src_error_p1_i : in  std_logic;

    snk_data_i     : in  std_logic_vector(15 downto 0);
    snk_ctrl_i     : in  std_logic_vector(4 -1 downto 0);
    snk_bytesel_i  : in  std_logic;
    snk_sof_p1_i   : in  std_logic;
    snk_eof_p1_i   : in  std_logic;
    snk_dreq_o     : out std_logic;
    snk_valid_i    : in  std_logic;
    snk_error_p1_o : out std_logic;
    snk_error_p1_i : in  std_logic;
      -- TXTSU i/f
    txtsu_port_id_i  : in  std_logic_vector(4 downto 0);
    txtsu_frame_id_i : in  std_logic_vector(16 -1 downto 0);
    txtsu_tsval_i    : in  std_logic_vector(28 + 4 - 1 downto 0);
    txtsu_valid_i    : in  std_logic;
    txtsu_ack_o      : out std_logic;
      --WB
    wb_cyc_i  : in  std_logic;
    wb_stb_i  : in  std_logic;
    wb_we_i   : in  std_logic;
    wb_sel_i  : in  std_logic_vector(3 downto 0);
    wb_addr_i : in  std_logic_vector(3 downto 0);
    wb_data_i : in  std_logic_vector(31 downto 0);
    wb_data_o : out std_logic_vector(31 downto 0);
    wb_ack_o  : out std_logic;
    wb_irq_o  : out std_logic
  );
  end component;

  constant maxAddrBitIncIO : integer := 10;

  component wrc_zpu
    generic (
      g_gen_trace     : boolean := false;
      g_DontCareValue : std_logic := 'X';
      g_spStart       : std_logic_vector(maxAddrBitIncIO downto 0));
    port (
      clk_i     : in  std_logic;
      rst_i     : in  std_logic;
      wb_addr_o : out std_logic_vector(c_aw-1 downto 0);
      wb_data_o : out std_logic_vector(c_dw-1 downto 0);
      wb_data_i : in  std_logic_vector(c_dw-1 downto 0);
      wb_sel_o  : out std_logic_vector(c_sw-1 downto 0);
      wb_we_o   : out std_logic;
      wb_cyc_o  : out std_logic;
      wb_stb_o  : out std_logic;
      wb_int_i  : in  std_logic;
      wb_ack_i  : in  std_logic);
  end component;
  
  -----------------------------------------------------------------------------
  --Dual-port RAM
  -----------------------------------------------------------------------------
  component wrc_dpram is
  generic(
    g_data_width  : natural := 32;
    g_size        : natural := 16;  -- 16 * 32bit = 64kB
    g_with_byte_enable  : boolean := true;
    g_addr_conflict_resolution : string := "read_first";
    g_init_file   : string  := "";
    g_dual_clock  : boolean := false
  );
  port(
    clk_i      : in std_logic;
    rst_n_i    : in std_logic;

    --PORT A (Wishbone)
    wb_addr_i  : in  std_logic_vector(f_log2_size(g_size)-1 downto 0); 
    wb_data_i  : in  std_logic_vector(g_data_width-1 downto 0); 
    wb_data_o  : out std_logic_vector(g_data_width-1 downto 0); 
    wb_sel_i   : in  std_logic_vector(g_data_width/8-1 downto 0); 
    wb_cyc_i   : in  std_logic;
    wb_stb_i   : in  std_logic;
    wb_we_i    : in  std_logic;
    wb_ack_o   : out std_logic;
    --PORT B (miniNIC)
    mem_addr_i : in  std_logic_vector(f_log2_size(g_size)-1 downto 0);
    mem_data_i : in  std_logic_vector(31 downto 0);
    mem_data_o : out std_logic_vector(31 downto 0);
    mem_wr_i   : in  std_logic
  );
  end component;

  component wrc_periph
    generic (
      g_gpio_pins    : natural;
      g_virtual_uart : natural;
      g_tics_period  : integer);
    port (
      clk_sys_i  : in  std_logic;
      clk_ref_i  : in  std_logic;
      rst_n_i    : in  std_logic;
      gpio_o     : out std_logic_vector(g_gpio_pins-1 downto 0);
      gpio_i     : in  std_logic_vector(g_gpio_pins-1 downto 0);
      gpio_dir_o : out std_logic_vector(g_gpio_pins-1 downto 0);
      uart_rxd_i : in  std_logic;
      uart_txd_o : out std_logic;
      genrst_n_o : out std_logic;
      wb_addr_i  : in  std_logic_vector(11 downto 0);
      wb_data_i  : in  std_logic_vector(31 downto 0);
      wb_data_o  : out std_logic_vector(31 downto 0);
      wb_sel_i   : in  std_logic_vector(3 downto 0);
      wb_stb_i   : in  std_logic;
      wb_cyc_i   : in  std_logic;
      wb_we_i    : in  std_logic;
      wb_ack_o   : out std_logic);
  end component;

  component wb_reset
    port (
      clk_i      : in  std_logic;
      rst_n_i    : in  std_logic;
      genrst_n_o : out std_logic;
      wb_addr_i  : in  std_logic_vector(1 downto 0);
      wb_data_i  : in  std_logic_vector(31 downto 0);
      wb_data_o  : out std_logic_vector(31 downto 0);
      wb_sel_i   : in  std_logic_vector(3 downto 0);
      wb_stb_i   : in  std_logic;
      wb_cyc_i   : in  std_logic;
      wb_we_i    : in  std_logic;
      wb_ack_o   : out std_logic);
  end component;        

  component wrc_lm32
    generic (
      g_addr_width : integer;
      g_num_irqs   : integer);
    port (
      clk_i     : in  std_logic;
      rst_n_i   : in  std_logic;
      irq_i     : in  std_logic_vector(g_num_irqs-1 downto 0);
      iwb_adr_o : out std_logic_vector(g_addr_width-1 downto 0);
      iwb_dat_o : out std_logic_vector(31 downto 0);
      iwb_dat_i : in  std_logic_vector(31 downto 0);
      iwb_cyc_o : out std_logic;
      iwb_stb_o : out std_logic;
      iwb_ack_i : in  std_logic;
      dwb_adr_o : out std_logic_vector(g_addr_width-1 downto 0);
      dwb_dat_o : out std_logic_vector(31 downto 0);
      dwb_dat_i : in  std_logic_vector(31 downto 0);
      dwb_cyc_o : out std_logic;
      dwb_stb_o : out std_logic;
      dwb_sel_o : out std_logic_vector(3 downto 0);
      dwb_we_o  : out std_logic;
      dwb_ack_i : in  std_logic);
  end component;

  component wr_softpll
    generic (
      g_deglitcher_threshold : integer;
      g_tag_bits             : integer);
    port (
      clk_sys_i        : in  std_logic;
      rst_n_i          : in  std_logic;
      clk_ref_i        : in  std_logic;
      clk_dmtd_i       : in  std_logic;
      clk_rx_i         : in  std_logic;
      dac_hpll_data_o  : out std_logic_vector(15 downto 0);
      dac_hpll_load_o  : out std_logic;
      dac_dmpll_data_o : out std_logic_vector(15 downto 0);
      dac_dmpll_load_o : out std_logic;
      wb_addr_i        : in  std_logic_vector(3 downto 0);
      wb_data_i        : in  std_logic_vector(31 downto 0);
      wb_data_o        : out std_logic_vector(31 downto 0);
      wb_cyc_i         : in  std_logic;
      wb_sel_i         : in  std_logic_vector(3 downto 0);
      wb_stb_i         : in  std_logic;
      wb_we_i          : in  std_logic;
      wb_ack_o         : out std_logic;
      wb_irq_o         : out std_logic;
      debug_o: out std_logic_vector(3 downto 0));
  end component;
  
end wrcore_pkg;
