library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.global_defs.all;
use work.endpoint_pkg.all;
--use work.gigaspy_pkg.all;

entity wr_minic_ep_combo is
  
  generic (
    g_memsize_log2 : integer := 14;
    g_with_gigaspy : boolean := false;
    g_phy_mode     : string  := "TBI";
    g_simulation   : integer := 1);

  port (
-- system reference clock (125 MHz)
    clk_ref_i : in std_logic;

-- reference clock / 2 (62.5 MHz, in-phase with refclk)
    clk_sys_i : in std_logic;

-- DMTD sampling clock (125.x MHz)
    clk_dmtd_i : in std_logic;

-- sync reset (clk_sys_i domain), active LO
    rst_n_i : in std_logic;

-- PPS input pulse, clk_ref_i-synchronous
    pps_csync_p1_i : in std_logic;

-------------------------------------------------------------------------------
-- PHY interface (TLK1221)
-------------------------------------------------------------------------------

-- PHY TX path - synchronous to refclk:

-- data output, 8b10b-encoded
    tbi_td_o     : out std_logic_vector(9 downto 0);
-- PHY enable, active HI
    tbi_enable_o : out std_logic;
-- PHY comma sync enable, active HI
    tbi_syncen_o : out std_logic;
-- PHY loopback mode enable, active HI
    tbi_loopen_o : out std_logic;
-- PHY PRBS pattern test enable, active HI  
    tbi_prbsen_o : out std_logic;

-- PHY RX path - synchronous to phy_rbclk_i:

-- RX clock (125.x MHz)
    tbi_rbclk_i : in std_logic;

-- data input, 8b10b-encoded
    tbi_rd_i : in std_logic_vector(9 downto 0);

-- PHY sync detect pulse (active HI when PHY detects valid comma pattern)
    tbi_sync_pass_i : in std_logic;

-------------------------------------------------------------------------------
-- PHY interface (Spartan6 GTP)
-------------------------------------------------------------------------------    

    gtp_txp_o : out std_logic;
    gtp_txn_o : out std_logic;

    gtp_rxp_i : in std_logic;
    gtp_rxn_i : in std_logic;


    wb_cyc_i  : in  std_logic;
    wb_stb_i  : in  std_logic;
    wb_we_i   : in  std_logic;
    wb_sel_i  : in  std_logic_vector(3 downto 0);
    wb_addr_i : in  std_logic_vector(g_memsize_log2+1 downto 0);
    wb_data_i : in  std_logic_vector(31 downto 0);
    wb_data_o : out std_logic_vector(31 downto 0);
    wb_ack_o  : out std_logic;
    wb_irq_o  : out std_logic

    );

end wr_minic_ep_combo;

architecture syn of wr_minic_ep_combo is

  constant c_BASE_MINIC    : std_logic_vector(1 downto 0) := "00";
  constant c_BASE_ENDPOINT : std_logic_vector(1 downto 0) := "01";
  constant c_BASE_PMEM     : std_logic_vector(1 downto 0) := "10";
  constant c_BASE_GIGASPY  : std_logic_vector(1 downto 0) := "11";

  alias wb_addr_selbits is wb_addr_i(g_memsize_log2+1 downto g_memsize_log2);

  component wr_mini_nic
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
      src_data_o       : out std_logic_vector(15 downto 0);
      src_ctrl_o       : out std_logic_vector(c_wrsw_ctrl_size - 1 downto 0);
      src_bytesel_o    : out std_logic;
      src_sof_p1_o     : out std_logic;
      src_eof_p1_o     : out std_logic;
      src_dreq_i       : in  std_logic;
      src_valid_o      : out std_logic;
      src_error_p1_o   : out std_logic;
      src_error_p1_i   : in  std_logic;
      snk_data_i       : in  std_logic_vector(15 downto 0);
      snk_ctrl_i       : in  std_logic_vector(c_wrsw_ctrl_size -1 downto 0);
      snk_bytesel_i    : in  std_logic;
      snk_sof_p1_i     : in  std_logic;
      snk_eof_p1_i     : in  std_logic;
      snk_dreq_o       : out std_logic;
      snk_valid_i      : in  std_logic;
      snk_error_p1_o   : out std_logic;
      snk_error_p1_i   : in  std_logic;
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

  component wrsw_gigaspy_dual
    generic (
      g_memsize_log2 : integer);
    port (
      tbi_rx_data_i  : in  std_logic_vector(9 downto 0);
      tbi_rx_data_o  : out std_logic_vector(9 downto 0);
      tbi_tx_data_o  : out std_logic_vector(9 downto 0);
      tbi_tx_data_i  : in  std_logic_vector(9 downto 0);
      tbi_tx_clk_i   : in  std_logic;
      tbi_rx_clk_i   : in  std_logic;
      phyio_prbsen_o : out std_logic;
      phyio_syncen_o : out std_logic;
      phyio_loopen_o : out std_logic;
      phyio_enable_o : out std_logic;
      refclk2_i      : in  std_logic;
      sys_rst_n_i    : in  std_logic;
      wb_addr_i      : in  std_logic_vector(g_memsize_log2+1 downto 0);
      wb_data_i      : in  std_logic_vector(31 downto 0);
      wb_data_o      : out std_logic_vector(31 downto 0);
      wb_ack_o       : out std_logic;
      wb_cyc_i       : in  std_logic;
      wb_sel_i       : in  std_logic;
      wb_stb_i       : in  std_logic;
      wb_we_i        : in  std_logic;
      pps_i          : in  std_logic);
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


  component wr_gtp_phy_spartan6
    generic (
      g_simulation : integer);
    port (
      clk_ref_i          : in  std_logic;
      ch0_tx_data_i      : in  std_logic_vector(7 downto 0);
      ch0_tx_k_i         : in  std_logic;
      ch0_tx_disparity_o : out std_logic;
      ch0_tx_enc_err_o   : out std_logic;
      ch0_rx_rbclk_o     : out std_logic;
      ch0_rx_data_o      : out std_logic_vector(7 downto 0);
      ch0_rx_k_o         : out std_logic;
      ch0_rx_enc_err_o   : out std_logic;
      ch0_rx_bitslide_o  : out std_logic_vector(3 downto 0);
      ch0_rst_i          : in  std_logic;
      ch0_loopen_i       : in  std_logic;
      ch1_tx_data_i      : in  std_logic_vector(7 downto 0) := "00000000";
      ch1_tx_k_i         : in  std_logic                    := '0';
      ch1_tx_disparity_o : out std_logic;
      ch1_tx_enc_err_o   : out std_logic;
      ch1_rx_data_o      : out std_logic_vector(7 downto 0);
      ch1_rx_rbclk_o     : out std_logic;
      ch1_rx_k_o         : out std_logic;
      ch1_rx_enc_err_o   : out std_logic;
      ch1_rx_bitslide_o  : out std_logic_vector(3 downto 0);
      ch1_rst_i          : in  std_logic                    := '0';
      ch1_loopen_i       : in  std_logic                    := '0';
      pad_txn0_o         : out std_logic;
      pad_txp0_o         : out std_logic;
      pad_rxn0_i         : in  std_logic                    := '0';
      pad_rxp0_i         : in  std_logic                    := '0';
      pad_txn1_o         : out std_logic;
      pad_txp1_o         : out std_logic;
      pad_rxn1_i         : in  std_logic                    := '0';
      pad_rxp1_i         : in  std_logic                    := '0');
  end component;


  signal gtp_tx_data      : std_logic_vector(7 downto 0);
  signal gtp_tx_k         : std_logic;
  signal gtp_tx_disparity : std_logic;
  signal gtp_tx_enc_err   : std_logic;
  signal gtp_rx_data      : std_logic_vector(7 downto 0);
  signal gtp_rx_clk       : std_logic;
  signal gtp_rx_k         : std_logic;
  signal gtp_rx_enc_err   : std_logic;
  signal gtp_rx_bitslide  : std_logic_vector(3 downto 0);
  signal gtp_rst          : std_logic;
  signal gtp_loopen       : std_logic;

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

  signal m2e_sof_p1     : std_logic;
  signal m2e_eof_p1     : std_logic;
  signal m2e_dreq       : std_logic;
  signal m2e_ctrl       : std_logic_vector(c_wrsw_ctrl_size - 1 downto 0);
  signal m2e_data       : std_logic_vector(15 downto 0);
  signal m2e_valid      : std_logic;
  signal m2e_bytesel    : std_logic;
  signal m2e_error_p1_o : std_logic;
  signal m2e_error_p1_i : std_logic;

  signal e2m_sof_p1     : std_logic;
  signal e2m_eof_p1     : std_logic;
  signal e2m_data       : std_logic_vector(15 downto 0);
  signal e2m_ctrl       : std_logic_vector(c_wrsw_ctrl_size -1 downto 0);
  signal e2m_valid      : std_logic;
  signal e2m_bytesel    : std_logic;
  signal e2m_dreq       : std_logic;
  signal e2m_error_p1_o : std_logic;
  signal e2m_error_p1_i : std_logic;

  signal txtsu_port_id  : std_logic_vector(4 downto 0);
  signal txtsu_frame_id : std_logic_vector(c_wrsw_oob_frame_id_size -1 downto 0);
  signal txtsu_tsval    : std_logic_vector(c_wrsw_timestamp_size_r + c_wrsw_timestamp_size_f - 1 downto 0);
  signal txtsu_valid    : std_logic;
  signal txtsu_ack      : std_logic;

  signal phy_rd_int, phy_td_int : std_logic_vector(9 downto 0);

  signal rd_vec : std_logic_vector(9 downto 0);
  signal tx_vec : std_logic_vector(9 downto 0);
  
  
  
begin  -- syn

  MINIC : wr_mini_nic
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

      src_sof_p1_o   => m2e_sof_p1,
      src_eof_p1_o   => m2e_eof_p1,
      src_dreq_i     => m2e_dreq,
      src_ctrl_o     => m2e_ctrl,
      src_data_o     => m2e_data,
      src_valid_o    => m2e_valid,
      src_bytesel_o  => m2e_bytesel,
      src_error_p1_i => m2e_error_p1_i,
      src_error_p1_o => m2e_error_p1_o,

      snk_sof_p1_i   => e2m_sof_p1,
      snk_eof_p1_i   => e2m_eof_p1,
      snk_data_i     => e2m_data,
      snk_ctrl_i     => e2m_ctrl,
      snk_valid_i    => e2m_valid,
      snk_bytesel_i  => e2m_bytesel,
      snk_dreq_o     => e2m_dreq,
      snk_error_p1_i => e2m_error_p1_i,
      snk_error_p1_o => e2m_error_p1_o,

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
      g_phy_mode            => g_phy_mode,
      g_simulation          => g_simulation)
    port map (
      clk_ref_i      => clk_ref_i,
      clk_sys_i      => clk_sys_i,
      clk_dmtd_i     => clk_dmtd_i,
      rst_n_i        => rst_n_i,
      pps_csync_p1_i => pps_csync_p1_i,

      tbi_td_o        => phy_td_int,
      tbi_enable_o    => tbi_enable_o,
      tbi_syncen_o    => tbi_syncen_o,
      tbi_loopen_o    => tbi_loopen_o,
      tbi_prbsen_o    => tbi_prbsen_o,
      tbi_rbclk_i     => tbi_rbclk_i,
      tbi_rd_i        => phy_rd_int,
      tbi_sync_pass_i => tbi_sync_pass_i,

      rx_sof_p1_o    => e2m_sof_p1,
      rx_eof_p1_o    => e2m_eof_p1,
      rx_dreq_i      => e2m_dreq,
      rx_ctrl_o      => e2m_ctrl,
      rx_data_o      => e2m_data,
      rx_bytesel_o   => e2m_bytesel,
      rx_valid_o     => e2m_valid,
      rx_idle_o      => open,
      rx_rerror_p1_o => e2m_error_p1_i,
      rx_rabort_p1_i => '0',

      tx_data_i      => m2e_data,
      tx_ctrl_i      => m2e_ctrl,
      tx_valid_i     => m2e_valid,
      tx_bytesel_i   => m2e_bytesel,
      tx_dreq_o      => m2e_dreq,
      tx_sof_p1_i    => m2e_sof_p1,
      tx_eof_p1_i    => m2e_eof_p1,
      tx_tabort_p1_i => '0',
      tx_terror_p1_o => m2e_error_p1_i,
      tx_rerror_p1_i => '0',

      txtsu_port_id_o  => txtsu_port_id,
      txtsu_frame_id_o => txtsu_frame_id,
      txtsu_tsval_o    => txtsu_tsval,
      txtsu_valid_o    => txtsu_valid,
      txtsu_ack_i      => txtsu_ack,

      gtp_tx_clk_i       => clk_ref_i,
      gtp_tx_data_o      => gtp_tx_data,
      gtp_tx_k_o         => gtp_tx_k,
      gtp_tx_disparity_i => gtp_tx_disparity,
      gtp_tx_enc_err_i   => gtp_tx_enc_err,

      gtp_rx_data_i     => gtp_rx_data,
      gtp_rx_clk_i      => gtp_rx_clk,
      gtp_rx_k_i        => gtp_rx_k,
      gtp_rx_enc_err_i  => gtp_rx_enc_err,
      gtp_rx_bitslide_i => gtp_rx_bitslide,

      gtp_rst_o    => gtp_rst,
      gtp_loopen_o => gtp_loopen,

      wb_cyc_i  => wb_cyc_endpoint,
      wb_stb_i  => wb_stb_i,
      wb_we_i   => wb_we_i,
      wb_sel_i  => wb_sel_i,
      wb_addr_i => wb_addr_i(5 downto 0),
      wb_data_i => wb_data_i,
      wb_data_o => wb_rdata_endpoint,
      wb_ack_o  => wb_ack_endpoint);



  wb_cyc_minic    <= wb_cyc_i when (wb_addr_selbits = c_BASE_MINIC)    else '0';
  wb_cyc_endpoint <= wb_cyc_i when (wb_addr_selbits = c_BASE_ENDPOINT) else '0';
  wb_cyc_pmem     <= wb_cyc_i when (wb_addr_selbits = c_BASE_PMEM)     else '0';


  gen_gigaspy : if(g_with_gigaspy = true) generate

    rd_vec <= "0" & gtp_rx_k & gtp_rx_data;
    tx_vec <= "0" & gtp_tx_k & gtp_tx_data;
    
    G_SPY : wrsw_gigaspy_dual
      generic map (
        g_memsize_log2 => 10
        )
      port map (
        tbi_rx_data_i => tx_vec,
        tbi_rx_data_o => open,

        tbi_tx_data_o => open,
        tbi_tx_data_i => rd_vec,

        tbi_tx_clk_i   => gtp_rx_clk, --clk_ref_i,
        tbi_rx_clk_i   => clk_ref_i,
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


  gen_tbi : if(g_phy_mode = "TBI") generate
    tbi_td_o   <= phy_td_int;
    phy_rd_int <= tbi_rd_i;
  end generate gen_tbi;


  gen_gtp : if(g_phy_mode = "GTP") generate

    U_GTP_PHY : wr_gtp_phy_spartan6
      generic map (
        g_simulation => g_simulation)
      port map (
        clk_ref_i => clk_ref_i,

        ch0_tx_data_i      => gtp_tx_data,
        ch0_tx_k_i         => gtp_tx_k,
        ch0_tx_disparity_o => gtp_tx_disparity,
        ch0_tx_enc_err_o   => gtp_tx_enc_err,
        ch0_rx_data_o      => gtp_rx_data,
        ch0_rx_rbclk_o     => gtp_rx_clk,
        ch0_rx_k_o         => gtp_rx_k,
        ch0_rx_enc_err_o   => gtp_rx_enc_err,
        ch0_rx_bitslide_o  => gtp_rx_bitslide,
        ch0_rst_i          => gtp_rst,
        ch0_loopen_i       => gtp_loopen,

        pad_txn0_o => gtp_txn_o,
        pad_txp0_o => gtp_txp_o,
        pad_rxn0_i => gtp_rxn_i,
        pad_rxp0_i => gtp_rxp_i);
  end generate gen_gtp;


  wb_ack_o <= wb_ack_minic or wb_ack_endpoint or wb_ack_pmem or wb_ack_gigaspy;

  wb_data_o <= wb_rdata_minic when (wb_addr_selbits = c_BASE_MINIC)
               else wb_rdata_endpoint when (wb_addr_selbits = c_BASE_ENDPOINT)
               else wb_rdata_pmem     when (wb_addr_selbits = c_BASE_PMEM)
               else wb_rdata_gigaspy  when (wb_addr_selbits = c_BASE_GIGASPY)
               else (others => 'X');
  

end syn;
