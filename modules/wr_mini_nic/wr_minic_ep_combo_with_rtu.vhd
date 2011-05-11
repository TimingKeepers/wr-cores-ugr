library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.global_defs.all;
use work.endpoint_pkg.all;
use work.gigaspy_pkg.all;

entity wrsw_minic_ep_combo_with_rtu is
  
  generic (
    g_memsize_log2 : integer := 14;
    g_with_gigaspy : boolean := false);

  port (
-- Ethernet reference clock (125 MHz)
    clk_ref_i : in std_logic;

-- System clock (>=62.5 MHz)
    clk_sys_i : in std_logic;

-- DMTD sampling clock (125.x MHz)
    clk_dmtd_i : in std_logic;

-- sync reset (refclk2 domain), active LO
    rst_n_i : in std_logic;

-- PPS input pulse, refclk-synchronous
    pps_csync_p1_i : in std_logic;

-------------------------------------------------------------------------------
-- PHY interface (TLK1221)
-------------------------------------------------------------------------------

-- PHY TX path - synchronous to refclk:

-- data output, 8b10b-encoded
    phy_td_o     : out std_logic_vector(9 downto 0);
-- PHY enable, active HI
    phy_enable_o : out std_logic;
-- PHY comma sync enable, active HI
    phy_syncen_o : out std_logic;
-- PHY loopback mode enable, active HI
    phy_loopen_o : out std_logic;
-- PHY PRBS pattern test enable, active HI  
    phy_prbsen_o : out std_logic;

-- PHY RX path - synchronous to phy_rbclk_i:

-- RX clock (125.x MHz)
    phy_rbclk_i : in std_logic;

-- data input, 8b10b-encoded
    phy_rd_i : in std_logic_vector(9 downto 0);

-- PHY sync detect pulse (active HI when PHY detects valid comma pattern)
    phy_sync_pass_i : in std_logic;

    wb_cyc_i  : in  std_logic;
    wb_stb_i  : in  std_logic;
    wb_we_i   : in  std_logic;
    wb_sel_i  : in  std_logic_vector(3 downto 0);
    wb_addr_i : in  std_logic_vector(g_memsize_log2+1 downto 0);
    wb_data_i : in  std_logic_vector(31 downto 0);
    wb_data_o : out std_logic_vector(31 downto 0);
    wb_ack_o  : out std_logic;
    wb_irq_o  : out std_logic;


-------------------------------------------------------------------------------
-- RTU interface
-------------------------------------------------------------------------------

-- 1 indicates that coresponding RTU port is full.
    rtu_full_i         : in  std_logic;
    rtu_almost_full_i  : in  std_logic;
    rtu_rq_strobe_p1_o : out std_logic;
    rtu_rq_smac_o      : out std_logic_vector(c_wrsw_mac_addr_width - 1 downto 0);
    rtu_rq_dmac_o      : out std_logic_vector(c_wrsw_mac_addr_width - 1 downto 0);
    rtu_rq_vid_o       : out std_logic_vector(c_wrsw_vid_width - 1 downto 0);
    rtu_rq_has_vid_o   : out std_logic;
    rtu_rq_prio_o      : out std_logic_vector(c_wrsw_prio_width-1 downto 0);
    rtu_rq_has_prio_o  : out std_logic
    );

end wrsw_minic_ep_combo_with_rtu;

architecture syn of wrsw_minic_ep_combo_with_rtu is

  constant c_BASE_MINIC    : std_logic_vector(1 downto 0) := "00";
  constant c_BASE_ENDPOINT : std_logic_vector(1 downto 0) := "01";
  constant c_BASE_PMEM     : std_logic_vector(1 downto 0) := "10";
  constant c_BASE_GIGASPY  : std_logic_vector(1 downto 0) := "11";

  alias wb_addr_selbits is wb_addr_i(g_memsize_log2+1 downto g_memsize_log2);


  component wrsw_mini_nic
    generic (
      g_memsize_log2         : integer;
      g_buffer_little_endian : boolean);
    port (
      clk_sys_i        : in  std_logic;
      rst_n_i          : in  std_logic;
      mem_data_o       : out std_logic_vector(31 downto 0);
      mem_addr_o       : out std_logic_vector(g_memsize_log2-1 downto 0);
      mem_data_i       : in  std_logic_vector(31 downto 0);
      mem_wr_o         : out std_logic;
      rx_data_o        : out std_logic_vector(15 downto 0);
      rx_ctrl_o        : out std_logic_vector(c_wrsw_ctrl_size - 1 downto 0);
      rx_bytesel_o     : out std_logic;
      rx_sof_p1_o      : out std_logic;
      rx_eof_p1_o      : out std_logic;
      rx_dreq_i        : in  std_logic;
      rx_valid_o       : out std_logic;
      rx_rerror_p1_o   : out std_logic;
      rx_terror_p1_i   : in  std_logic;
      rx_tabort_p1_o   : out std_logic;
      rx_rabort_p1_i   : in  std_logic;
      rx_idle_o        : out std_logic;
      tx_data_i        : in  std_logic_vector(15 downto 0);
      tx_ctrl_i        : in  std_logic_vector(c_wrsw_ctrl_size -1 downto 0);
      tx_bytesel_i     : in  std_logic;
      tx_sof_p1_i      : in  std_logic;
      tx_eof_p1_i      : in  std_logic;
      tx_dreq_o        : out std_logic;
      tx_valid_i       : in  std_logic;
      tx_rerror_p1_i   : in  std_logic;
      tx_terror_p1_o   : out std_logic;
      tx_tabort_p1_i   : in  std_logic;
      tx_rabort_p1_o   : out std_logic;
      txtsu_port_id_i  : in  std_logic_vector(4 downto 0);
      txtsu_frame_id_i : in  std_logic_vector(c_wrsw_oob_frame_id_size -1 downto 0);
      txtsu_tsval_i    : in  std_logic_vector(c_wrsw_timestamp_size_r + c_wrsw_timestamp_size_f - 1 downto 0);
      txtsu_valid_i    : in  std_logic;
      txtsu_ack_o      : out std_logic;
      wb_cyc_i         : in  std_logic;
      wb_stb_i         : in  std_logic;
      wb_we_i          : in  std_logic;
      wb_sel_i         : in  std_logic_vector(3 downto 0);
      wb_addr_i        : in  std_logic_vector(3 downto 0);
      wb_data_i        : in  std_logic_vector(31 downto 0);
      wb_data_o        : out std_logic_vector(31 downto 0);
      wb_ack_o         : out std_logic;
      wb_irq_o         : out std_logic);
  end component;

  component minic_packet_buffer
    generic (
      g_memsize_log2 : integer);
    port (
      clk_sys_i    : in  std_logic;
      rst_n_i      : in  std_logic;
      minic_addr_i : in  std_logic_vector(g_memsize_log2-1 downto 0);
      minic_data_i : in  std_logic_vector(31 downto 0);
      minic_wr_i   : in  std_logic;
      minic_data_o : out std_logic_vector(31 downto 0);
      wb_data_i    : in  std_logic_vector(31 downto 0);
      wb_data_o    : out std_logic_vector(31 downto 0);
      wb_addr_i    : in  std_logic_vector(g_memsize_log2-1 downto 0);
      wb_cyc_i     : in  std_logic;
      wb_stb_i     : in  std_logic;
      wb_we_i      : in  std_logic;
      wb_ack_o     : out std_logic);
  end component;



  signal wb_ack_minic    : std_logic;
  signal wb_ack_pmem     : std_logic;
  signal wb_ack_endpoint : std_logic;
  signal wb_ack_gigaspy  : std_logic;

  signal wb_cyc_minic    : std_logic;
  signal wb_cyc_pmem     : std_logic;
  signal wb_cyc_endpoint : std_logic;
  signal wb_cyc_gigaspy  : std_logic;

  signal wb_rdata_minic    : std_logic_vector(31 downto 0);
  signal wb_rdata_pmem     : std_logic_vector(31 downto 0);
  signal wb_rdata_endpoint : std_logic_vector(31 downto 0);
  signal wb_rdata_gigaspy  : std_logic_vector(31 downto 0);

  signal pmem_data_r : std_logic_vector(31 downto 0);
  signal pmem_data_w : std_logic_vector(31 downto 0);
  signal pmem_addr   : std_logic_vector(g_memsize_log2-1 downto 0);
  signal pmem_wr     : std_logic;

  -- two WRF links
  signal m2e_sof_p1    : std_logic;
  signal m2e_eof_p1    : std_logic;
  signal m2e_dreq      : std_logic;
  signal m2e_ctrl      : std_logic_vector(c_wrsw_ctrl_size - 1 downto 0);
  signal m2e_data      : std_logic_vector(15 downto 0);
  signal m2e_valid     : std_logic;
  signal m2e_bytesel   : std_logic;
  signal m2e_idle      : std_logic;
  signal m2e_rabort_p1 : std_logic;
  signal m2e_tabort_p1 : std_logic;
  signal m2e_rerror_p1 : std_logic;
  signal m2e_terror_p1 : std_logic;

  signal e2m_sof_p1    : std_logic;
  signal e2m_eof_p1    : std_logic;
  signal e2m_data      : std_logic_vector(15 downto 0);
  signal e2m_ctrl      : std_logic_vector(c_wrsw_ctrl_size -1 downto 0);
  signal e2m_valid     : std_logic;
  signal e2m_bytesel   : std_logic;
  signal e2m_dreq      : std_logic;
  signal e2m_rabort_p1 : std_logic;
  signal e2m_tabort_p1 : std_logic;
  signal e2m_rerror_p1 : std_logic;
  signal e2m_terror_p1 : std_logic;


  signal txtsu_port_id  : std_logic_vector(4 downto 0);
  signal txtsu_frame_id : std_logic_vector(c_wrsw_oob_frame_id_size -1 downto 0);
  signal txtsu_tsval    : std_logic_vector(c_wrsw_timestamp_size_r + c_wrsw_timestamp_size_f - 1 downto 0);
  signal txtsu_valid    : std_logic;
  signal txtsu_ack      : std_logic;

  signal phy_rd_int, phy_td_int : std_logic_vector(9 downto 0);

  
begin  -- syn

  MINIC : wrsw_mini_nic
    generic map (
      g_memsize_log2         => g_memsize_log2,
      g_buffer_little_endian => true)
    port map (
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_i,
      mem_data_o => pmem_data_w,
      mem_addr_o => pmem_addr,
      mem_data_i => pmem_data_r,
      mem_wr_o   => pmem_wr,

      rx_data_o      => m2e_data,
      rx_ctrl_o      => m2e_ctrl,
      rx_bytesel_o   => m2e_bytesel,
      rx_sof_p1_o    => m2e_sof_p1,
      rx_eof_p1_o    => m2e_eof_p1,
      rx_dreq_i      => m2e_dreq,
      rx_valid_o     => m2e_valid,
      rx_idle_o      => open,
      rx_rerror_p1_o => m2e_rerror_p1,
      rx_terror_p1_i => m2e_terror_p1,
      rx_tabort_p1_o => m2e_tabort_p1,
      rx_rabort_p1_i => m2e_rabort_p1,

      tx_data_i      => e2m_data,
      tx_ctrl_i      => e2m_ctrl,
      tx_bytesel_i   => e2m_bytesel,
      tx_sof_p1_i    => e2m_sof_p1,
      tx_eof_p1_i    => e2m_eof_p1,
      tx_dreq_o      => e2m_dreq,
      tx_valid_i     => e2m_valid,
      tx_rerror_p1_i => e2m_rerror_p1,
      tx_terror_p1_o => e2m_terror_p1,
      tx_tabort_p1_i => e2m_tabort_p1,
      tx_rabort_p1_o => e2m_rabort_p1,

      txtsu_port_id_i  => txtsu_port_id,
      txtsu_frame_id_i => txtsu_frame_id,
      txtsu_tsval_i    => txtsu_tsval,
      txtsu_valid_i    => txtsu_valid,
      txtsu_ack_o      => txtsu_ack,

      wb_cyc_i  => wb_cyc_minic,
      wb_stb_i  => wb_stb_i,
      wb_we_i   => wb_we_i,
      wb_sel_i  => wb_sel_i,
      wb_addr_i => wb_addr_i(3 downto 0),
      wb_data_i => wb_data_i,
      wb_data_o => wb_rdata_minic,
      wb_ack_o  => wb_ack_minic,
      wb_irq_o  => wb_irq_o);


  e2m_tabort_p1 <= '0';
  m2e_rabort_p1 <= '0';

  PMEM : minic_packet_buffer
    generic map (
      g_memsize_log2 => g_memsize_log2)
    port map (
      clk_sys_i    => clk_sys_i,
      rst_n_i      => rst_n_i,
      minic_addr_i => pmem_addr,
      minic_data_i => pmem_data_w,
      minic_wr_i   => pmem_wr,
      minic_data_o => pmem_data_r,

      wb_data_i => wb_data_i,
      wb_data_o => wb_rdata_pmem,
      wb_addr_i => wb_addr_i(g_memsize_log2-1 downto 0),
      wb_cyc_i  => wb_cyc_pmem,
      wb_stb_i  => wb_stb_i,
      wb_we_i   => wb_we_i,
      wb_ack_o  => wb_ack_pmem);

  
  ENDPOINT : wrsw_endpoint
    generic map (
      g_rx_buffer_size_log2 => 12,
      g_simulation          => 0,
      g_phy_mode            => "TBI")
    port map (
      clk_ref_i      => clk_ref_i,
      clk_sys_i      => clk_sys_i,
      clk_dmtd_i     => clk_dmtd_i,
      rst_n_i        => rst_n_i,
      pps_csync_p1_i => pps_csync_p1_i,

      tbi_td_o        => phy_td_int,
      tbi_enable_o    => phy_enable_o,
      tbi_syncen_o    => phy_syncen_o,
      tbi_loopen_o    => phy_loopen_o,
      tbi_prbsen_o    => phy_prbsen_o,
      tbi_rbclk_i     => phy_rbclk_i,
      tbi_rd_i        => phy_rd_int,
      tbi_sync_pass_i => phy_sync_pass_i,


      gtp_tx_clk_i       => '0',
      gtp_tx_data_o      => open,
      gtp_tx_k_o         => open,
      gtp_tx_disparity_i => '0',
      gtp_tx_enc_err_i   => '0',

      gtp_rx_data_i     => "00000000",
      gtp_rx_clk_i      => '0',
      gtp_rx_k_i        => '0',
      gtp_rx_enc_err_i  => '0',1
      gtp_rx_bitslide_i => "0000",

      gtp_rst_o    => open,
      gtp_loopen_o => open,

      rx_data_o      => e2m_data,
      rx_ctrl_o      => e2m_ctrl,
      rx_bytesel_o   => e2m_bytesel,
      rx_sof_p1_o    => e2m_sof_p1,
      rx_eof_p1_o    => e2m_eof_p1,
      rx_dreq_i      => e2m_dreq,
      rx_valid_o     => e2m_valid,
      rx_rabort_p1_i => e2m_rabort_p1,
      rx_idle_o      => open,
      rx_rerror_p1_o => e2m_rerror_p1,

      tx_data_i    => m2e_data,
      tx_ctrl_i    => m2e_ctrl,
      tx_bytesel_i => m2e_bytesel,
      tx_sof_p1_i  => m2e_sof_p1,
      tx_eof_p1_i  => m2e_eof_p1,
      tx_dreq_o    => m2e_dreq,
      tx_valid_i   => m2e_valid,

      tx_rerror_p1_i => m2e_rerror_p1,
      tx_tabort_p1_i => m2e_tabort_p1,
      tx_terror_p1_o => m2e_terror_p1,

      txtsu_port_id_o  => txtsu_port_id,
      txtsu_frame_id_o => txtsu_frame_id,
      txtsu_tsval_o    => txtsu_tsval,
      txtsu_valid_o    => txtsu_valid,
      txtsu_ack_i      => txtsu_ack,

      rtu_full_i         => rtu_full_i,
      rtu_almost_full_i  => rtu_almost_full_i,
      rtu_rq_strobe_p1_o => rtu_rq_strobe_p1_o,
      rtu_rq_smac_o      => rtu_rq_smac_o,
      rtu_rq_dmac_o      => rtu_rq_dmac_o,
      rtu_rq_vid_o       => rtu_rq_vid_o,
      rtu_rq_has_vid_o   => rtu_rq_has_vid_o,
      rtu_rq_prio_o      => rtu_rq_prio_o,
      rtu_rq_has_prio_o  => rtu_rq_has_prio_o,

      wb_cyc_i  => wb_cyc_endpoint,
      wb_stb_i  => wb_stb_i,
      wb_we_i   => wb_we_i,
      wb_sel_i  => wb_sel_i,
      wb_addr_i => wb_addr_i(5 downto 0),
      wb_data_i => wb_data_i,
      wb_data_o => wb_rdata_endpoint,
      wb_ack_o  => wb_ack_endpoint);

  phy_td_o   <= phy_td_int;
  phy_rd_int <= phy_rd_i;

  wb_cyc_minic    <= wb_cyc_i when (wb_addr_selbits = c_BASE_MINIC)    else '0';
  wb_cyc_endpoint <= wb_cyc_i when (wb_addr_selbits = c_BASE_ENDPOINT) else '0';
  wb_cyc_pmem     <= wb_cyc_i when (wb_addr_selbits = c_BASE_PMEM)     else '0';

  gen_gigaspy : if(g_with_gigaspy = true) generate

    G_SPY : wrsw_gigaspy_dual
      generic map (
        g_memsize_log2 => 10
        )
      port map (
        tbi_rx_data_i => phy_rd_i,
        tbi_rx_data_o => open,

        tbi_tx_data_o => open,
        tbi_tx_data_i => "0000000000",

        tbi_tx_clk_i   => clk_ref_i,
        tbi_rx_clk_i   => phy_rbclk_i,
        phyio_prbsen_o => open,
        phyio_syncen_o => open,
        phyio_loopen_o => open,
        phyio_enable_o => open,
        refclk2_i      => clk_sys_i,
        sys_rst_n_i    => rst_n_i,
        wb_addr_i      => wb_addr_i(11 downto 0),
        wb_data_i      => wb_data_i,
        wb_data_o      => wb_rdata_gigaspy,
        wb_ack_o       => wb_ack_gigaspy,
        wb_cyc_i       => wb_cyc_gigaspy,
        wb_sel_i       => '1',
        wb_stb_i       => wb_stb_i,
        wb_we_i        => wb_we_i,
        pps_i          => '0');

    wb_cyc_gigaspy <= wb_cyc_i when (wb_addr_selbits = c_BASE_GIGASPY) else '0';
  end generate gen_gigaspy;

  gen_bypass_gigaspy : if(g_with_gigaspy = false) generate
    wb_cyc_gigaspy   <= '0';
    wb_ack_gigaspy   <= '0';
    wb_rdata_gigaspy <= (others => 'X');
  end generate gen_bypass_gigaspy;




  wb_ack_o <= wb_ack_minic or wb_ack_endpoint or wb_ack_pmem or wb_ack_gigaspy;

  wb_data_o <= wb_rdata_minic when (wb_addr_selbits = c_BASE_MINIC)
               else wb_rdata_endpoint when (wb_addr_selbits = c_BASE_ENDPOINT)
               else wb_rdata_pmem     when (wb_addr_selbits = c_BASE_PMEM)
               else wb_rdata_gigaspy  when (wb_addr_selbits = c_BASE_GIGASPY)
               else (others => 'X');
  

end syn;
