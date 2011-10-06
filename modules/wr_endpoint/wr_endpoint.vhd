-------------------------------------------------------------------------------
-- Title      : 1000base-X MAC/Endpoint
-- Project    : WhiteRabbit Switch
-------------------------------------------------------------------------------
-- File       : wr_endpoint.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-Co-HT
-- Created    : 2010-04-26
-- Last update: 2011-10-05
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description: Module implements a gigabit-only optical PCS + MAC + some-of-l2
-- layer stuff for the purpose of WhiteRabbit switch. Features:
-- - frame reception & transmission
-- - flow control (pause frames)
-- - VLANs: inserting/removing tags (for ACCESS/TRUNK port support)
-- - RX/TX precise timestaping
-- - full PCS for optical Gigabit Ethernet 
-- - clock phase measurement (DMTD)
-- - decodes MAC addresses, VIDs and priorities and passes them to the RTU.
-------------------------------------------------------------------------------
-- Copyright (c) 2010 Tomasz Wlostowski
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2010-04-26  1.0      twlostow        Created
-------------------------------------------------------------------------------



library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;

use work.gencores_pkg.all;
use work.endpoint_private_pkg.all;
use work.ep_wbgen2_pkg.all;
use work.wr_fabric_pkg.all;


entity wr_endpoint is
  
  generic (
    g_simulation          : boolean := true;
    g_interface_mode      : string  := "SERDES";
    g_rx_buffer_size_log2 : integer := 12;
    g_with_rx_buffer      : boolean := true;
    g_with_flow_control   : boolean := true;
    g_with_timestamper    : boolean := true;
    g_with_dmtd           : boolean := true;
    g_with_dpi_classifier : boolean := true;
    g_with_vlans          : boolean := true;
    g_with_rtu            : boolean := true
    );
  port (

-------------------------------------------------------------------------------
-- Clocks
-------------------------------------------------------------------------------

-- Endpoint transmit reference clock. Must be 125 MHz +- 100 ppm
    clk_ref_i : in std_logic;

-- reference clock / 2 (62.5 MHz, in-phase with refclk)
    clk_sys_i : in std_logic;

-- DMTD sampling clock (125.x MHz)
    clk_dmtd_i : in std_logic;

-- sync reset (clk_sys_i domain), active LO
    rst_n_i : in std_logic;

-- PPS input (1 clk_ref_i cycle HI) for synchronizing timestamp counter
    pps_csync_p1_i : in std_logic;

-------------------------------------------------------------------------------
-- Xilinx GTP PHY Interace
-------------------------------------------------------------------------------    

    phy_rst_o    : out std_logic;
    phy_loopen_o : out std_logic;
    phy_prbsen_o : out std_logic;
    phy_enable_o : out std_logic;
    phy_syncen_o : out std_logic;

    phy_ref_clk_i      : in  std_logic;
    phy_tx_data_o      : out std_logic_vector(7 downto 0);
    phy_tx_k_o         : out std_logic;
    phy_tx_disparity_i : in  std_logic;
    phy_tx_enc_err_i   : in  std_logic;

    phy_rx_data_i     : in std_logic_vector(7 downto 0);
    phy_rx_clk_i      : in std_logic;
    phy_rx_k_i        : in std_logic;
    phy_rx_enc_err_i  : in std_logic;
    phy_rx_bitslide_i : in std_logic_vector(3 downto 0);

    src_dat_o   : out std_logic_vector(15 downto 0);
    src_adr_o   : out std_logic_vector(1 downto 0);
    src_sel_o   : out std_logic_vector(1 downto 0);
    src_cyc_o   : out std_logic;
    src_stb_o   : out std_logic;
    src_we_o    : out std_logic;
    src_stall_i : in  std_logic;
    src_ack_i   : in  std_logic;

    snk_dat_i   : in  std_logic_vector(15 downto 0);
    snk_adr_i   : in  std_logic_vector(1 downto 0);
    snk_sel_i   : in  std_logic_vector(1 downto 0);
    snk_cyc_i   : in  std_logic;
    snk_stb_i   : in  std_logic;
    snk_we_i    : in  std_logic;
    snk_stall_o : out std_logic;
    snk_ack_o   : out std_logic;
    snk_err_o   : out std_logic;
    snk_rty_o   : out std_logic;

-------------------------------------------------------------------------------
-- TX timestamping unit interface
-------------------------------------------------------------------------------  

-- port ID value
    txtsu_port_id_o : out std_logic_vector(4 downto 0);

-- frame ID value
    txtsu_frame_id_o : out std_logic_vector(16 - 1 downto 0);

-- timestamp values: gathered on rising clock edge (the main timestamp)
    txtsu_tsval_o : out std_logic_vector(28 + 4 - 1 downto 0);

-- HI indicates a valid timestamp/frame ID pair for the TXTSU
    txtsu_valid_o : out std_logic;

-- HI acknowledges that the TXTSU have recorded the timestamp
    txtsu_ack_i : in std_logic;

-------------------------------------------------------------------------------
-- RTU interface
-------------------------------------------------------------------------------

-- 1 indicates that coresponding RTU port is full.
    rtu_full_i : in std_logic;

-- 1 indicates that coresponding RTU port is almost full.
    rtu_almost_full_i : in std_logic;

-- request strobe, single HI pulse begins evaluation of the request. 
    rtu_rq_strobe_p1_o : out std_logic;

-- source and destination MAC addresses extracted from the packet header
    rtu_rq_smac_o : out std_logic_vector(48 - 1 downto 0);
    rtu_rq_dmac_o : out std_logic_vector(48 - 1 downto 0);

-- VLAN id (extracted from the header for TRUNK ports and assigned by the port
-- for ACCESS ports)
    rtu_rq_vid_o : out std_logic_vector(12 - 1 downto 0);

-- HI means that packet has valid assigned a valid VID (low - packet is untagged)
    rtu_rq_has_vid_o : out std_logic;

-- packet priority (either extracted from the header or assigned per port).
    rtu_rq_prio_o : out std_logic_vector(3 - 1 downto 0);

-- HI indicates that packet has assigned priority.
    rtu_rq_has_prio_o : out std_logic;

-------------------------------------------------------------------------------   
-- Wishbone bus
-------------------------------------------------------------------------------

    wb_cyc_i : in  std_logic;
    wb_stb_i : in  std_logic;
    wb_we_i  : in  std_logic;
    wb_sel_i : in  std_logic_vector(3 downto 0);
    wb_adr_i : in  std_logic_vector(5 downto 0);
    wb_dat_i : in  std_logic_vector(31 downto 0);
    wb_dat_o : out std_logic_vector(31 downto 0);
    wb_ack_o : out std_logic

    );

end wr_endpoint;

architecture syn of wr_endpoint is

  constant c_zeros : std_logic_vector(63 downto 0) := (others => '0');
  constant c_ones  : std_logic_vector(63 downto 0) := (others => '0');

  component ep_tx_framer
    generic (
      g_with_vlans       : boolean;
      g_with_timestamper : boolean);
    port (
      clk_sys_i        : in  std_logic;
      rst_n_i          : in  std_logic;
      pcs_fab_o        : out t_ep_internal_fabric;
      pcs_error_i      : in  std_logic;
      pcs_busy_i       : in  std_logic;
      pcs_dreq_i       : in  std_logic;
      snk_i            : in  t_wrf_sink_in;
      snk_o            : out t_wrf_sink_out;
      fc_pause_p_i     : in  std_logic;
      fc_pause_delay_i : in  std_logic_vector(15 downto 0);
      fc_pause_ack_o   : out std_logic;
      fc_flow_enable_i : in  std_logic;
      oob_fid_value_o  : out std_logic_vector(15 downto 0);
      oob_fid_stb_o    : out std_logic;
      regs_i           : in  t_ep_out_registers);
  end component;

  component ep_rx_path
    generic (
      g_with_vlans          : boolean;
      g_with_dpi_classifier : boolean;
      g_with_rtu            : boolean);
    port (
      clk_sys_i              : in    std_logic;
      clk_rx_i               : in    std_logic;
      rst_n_sys_i            : in    std_logic;
      rst_n_rx_i             : in    std_logic;
      pcs_fab_i              : in    t_ep_internal_fabric;
      pcs_fifo_almostfull_o  : out   std_logic;
      pcs_busy_i             : in    std_logic;
      oob_data_i             : in    std_logic_vector(47 downto 0);
      oob_valid_i            : in    std_logic;
      oob_ack_o              : out   std_logic;
      src_wb_o               : out   t_wrf_source_out;
      src_wb_i               : in    t_wrf_source_in;
      fc_pause_p_o           : out   std_logic;
      fc_pause_delay_o       : out   std_logic_vector(15 downto 0);
      fc_buffer_occupation_o : out   std_logic_vector(7 downto 0);
      rmon_o                 : inout t_rmon_triggers;
      regs_i                 : in    t_ep_out_registers;
      rtu_rq_o               : out   t_ep_internal_rtu_request;
      rtu_full_i             : in    std_logic;
      rtu_rq_valid_o         : out   std_logic);
  end component;


  component ep_1000basex_pcs_8bit
    generic (
      g_simulation : boolean); 
    port (
      rst_n_i                 : in    std_logic;
      clk_sys_i               : in    std_logic;
      rxpcs_fab_o             : out   t_ep_internal_fabric;
      rxpcs_fifo_almostfull_i : in    std_logic;
      rxpcs_busy_o            : out   std_logic;
      rxpcs_timestamp_stb_p_o : out   std_logic;
      rxpcs_timestamp_valid_i : in    std_logic;
      rxpcs_timestamp_i       : in    std_logic_vector(31 downto 0);
      txpcs_fab_i             : in    t_ep_internal_fabric;
      txpcs_error_o           : out   std_logic;
      txpcs_busy_o            : out   std_logic;
      txpcs_dreq_o            : out   std_logic;
      txpcs_timestamp_stb_p_o : out   std_logic;
      link_ok_o               : out   std_logic;
      serdes_rst_o            : out   std_logic;
      serdes_syncen_o         : out   std_logic;
      serdes_loopen_o         : out   std_logic;
      serdes_prbsen_o         : out   std_logic;
      serdes_enable_o         : out   std_logic;
      serdes_tx_clk_i         : in    std_logic;
      serdes_tx_data_o        : out   std_logic_vector(7 downto 0);
      serdes_tx_k_o           : out   std_logic;
      serdes_tx_disparity_i   : in    std_logic;
      serdes_tx_enc_err_i     : in    std_logic;
      serdes_rx_data_i        : in    std_logic_vector(7 downto 0);
      serdes_rx_clk_i         : in    std_logic;
      serdes_rx_k_i           : in    std_logic;
      serdes_rx_enc_err_i     : in    std_logic;
      serdes_rx_bitslide_i    : in    std_logic_vector(3 downto 0);
      rmon_o                  : inout t_rmon_triggers;
      mdio_addr_i             : in    std_logic_vector(15 downto 0);
      mdio_data_i             : in    std_logic_vector(15 downto 0);
      mdio_data_o             : out   std_logic_vector(15 downto 0);
      mdio_stb_i              : in    std_logic;
      mdio_rw_i               : in    std_logic;
      mdio_ready_o            : out   std_logic); 
  end component;

  component ep_timestamping_unit
    generic (
      g_timestamp_bits_r : natural;
      g_timestamp_bits_f : natural);
    port (
      clk_ref_i              : in  std_logic;
      clk_sys_i              : in  std_logic;
      clk_rx_i               : in  std_logic;
      rst_n_rx_i             : in  std_logic;
      rst_n_ref_i            : in  std_logic;
      rst_n_sys_i            : in  std_logic;
      pps_csync_p1_i         : in  std_logic;
      tx_timestamp_stb_p_i   : in  std_logic;
      rx_timestamp_stb_p_i   : in  std_logic;
      rxts_timestamp_o       : out std_logic_vector(31 downto 0);
      rxts_timestamp_valid_o : out std_logic;
      txoob_fid_i            : in  std_logic_vector(16 - 1 downto 0);
      txoob_stb_p_i          : in  std_logic;
      txtsu_port_id_o        : out std_logic_vector(4 downto 0);
      txtsu_fid_o            : out std_logic_vector(16 -1 downto 0);
      txtsu_tsval_o          : out std_logic_vector(28 + 4 - 1 downto 0);
      txtsu_valid_o          : out std_logic;
      txtsu_ack_i            : in  std_logic;
      regs_i                 : in  t_ep_out_registers;
      regs_o                 : out t_ep_in_registers);
  end component;

-------------------------------------------------------------------------------
-- TX FRAMER -> TX PCS signals
-------------------------------------------------------------------------------

  signal txpcs_fab   : t_ep_internal_fabric;
  signal txpcs_dreq  : std_logic;
  signal txpcs_error : std_logic;
  signal txpcs_busy  : std_logic;

-------------------------------------------------------------------------------
-- Timestamping/OOB signals
-------------------------------------------------------------------------------

  signal txoob_fid_value : std_logic_vector(15 downto 0);
  signal txoob_fid_stb   : std_logic;

  signal rxoob_data  : std_logic_vector(47 downto 0);
  signal rxoob_valid : std_logic;
  signal rxoob_ack   : std_logic;

  signal txpcs_timestamp_stb_p : std_logic;
  signal rxpcs_timestamp_stb_p : std_logic;
  signal rxpcs_timestamp_valid : std_logic;
  signal rxpcs_timestamp_value : std_logic_vector(31 downto 0);

  --signal txts_timestamp_value : std_logic_vector(28 + 4 - 1 downto 0);
  --signal rxts_timestamp_value : std_logic_vector(28 + 4 - 1 downto 0);
  --signal rxts_done_p          : std_logic;
  --signal txts_done_p          : std_logic;

-------------------------------------------------------------------------------
-- RX PCS -> RX DEFRAMER signals
-------------------------------------------------------------------------------

  signal rxpcs_fab             : t_ep_internal_fabric;
  signal rxpcs_busy            : std_logic;
  signal rxpcs_fifo_almostfull : std_logic;

-------------------------------------------------------------------------------
-- RX deframer -> RX buffer signals
-------------------------------------------------------------------------------

  --signal rbuf_data    : std_logic_vector(15 downto 0);
  --signal rbuf_ctrl    : std_logic_vector(4-1 downto 0);
  --signal rbuf_sof_p   : std_logic;
  --signal rbuf_eof_p   : std_logic;
  --signal rbuf_error_p : std_logic;
  --signal rbuf_valid   : std_logic;
  --signal rbuf_drop    : std_logic;
  --signal rbuf_bytesel : std_logic;

  --signal rx_buffer_used : std_logic_vector(7 downto 0);


-------------------------------------------------------------------------------
-- WB slave signals
-------------------------------------------------------------------------------

  signal rmon          : t_rmon_triggers;
  signal regs_fromwb   : t_ep_out_registers;
  signal regs_towb     : t_ep_in_registers;
  signal regs_towb_ep  : t_ep_in_registers;
  signal regs_towb_tsu : t_ep_in_registers;


-------------------------------------------------------------------------------
-- flow control signals
-------------------------------------------------------------------------------

  signal txfra_flow_enable : std_logic;
  signal rxfra_pause_p     : std_logic;
  signal rxfra_pause_delay : std_logic_vector(15 downto 0);
  --signal rxbuf_threshold_hit : std_logic;

  signal txfra_pause_p     : std_logic;
  signal txfra_pause_ack   : std_logic;
  signal txfra_pause_delay : std_logic_vector(15 downto 0);


-------------------------------------------------------------------------------
-- RMON signals
-------------------------------------------------------------------------------

  signal ep_rmon_ram_addr   : std_logic_vector(4 downto 0);
  signal ep_rmon_ram_data_o : std_logic_vector(31 downto 0);
  signal ep_rmon_ram_rd     : std_logic;
  signal ep_rmon_ram_data_i : std_logic_vector(31 downto 0);
  signal ep_rmon_ram_wr     : std_logic;

  signal rmon_counters : std_logic_vector(31 downto 0);

  --signal rofifo_write, rofifo_full, oob_valid_d0 : std_logic;

  --signal phase_meas    : std_logic_vector(31 downto 0);
  --signal phase_meas_p  : std_logic;
  --signal validity_cntr : unsigned(1 downto 0);

  signal link_ok : std_logic;

  signal txfra_enable, rxfra_enable : std_logic;
  signal mdio_addr                  : std_logic_vector(15 downto 0);

  signal sink_in  : t_wrf_sink_in;
  signal sink_out : t_wrf_sink_out;

  signal src_in  : t_wrf_source_in;
  signal src_out : t_wrf_source_out;

  signal rst_n_rx, rst_n_sys, rst_n_ref : std_logic;
  
begin

  -----------------------------------------------------------------------------
  -- Reset signal synchronization
  -----------------------------------------------------------------------------
  
  U_Sync_Rst_RX : gc_sync_ffs
    port map (
      clk_i    => phy_rx_clk_i,
      rst_n_i  => '1',
      data_i   => rst_n_i,
      synced_o => rst_n_rx);

  U_Sync_Rst_REF : gc_sync_ffs
    port map (
      clk_i    => clk_ref_i,
      rst_n_i  => '1',
      data_i   => rst_n_i,
      synced_o => rst_n_ref);

  rst_n_sys <= rst_n_i;


-------------------------------------------------------------------------------
-- 1000Base-X PCS
-------------------------------------------------------------------------------

  mdio_addr <= regs_fromwb.mdio_asr_phyad_o & regs_fromwb.mdio_cr_addr_o;

  U_PCS_1000BASEX : ep_1000basex_pcs_8bit
    generic map (
      g_simulation => g_simulation)
    port map (
      rst_n_i   => rst_n_i,
      clk_sys_i => clk_sys_i,

      rxpcs_fab_o             => rxpcs_fab,
      rxpcs_busy_o            => rxpcs_busy,
      rxpcs_fifo_almostfull_i => rxpcs_fifo_almostfull,
      rxpcs_timestamp_stb_p_o => rxpcs_timestamp_stb_p,
      rxpcs_timestamp_i       => rxpcs_timestamp_value,
      rxpcs_timestamp_valid_i => rxpcs_timestamp_valid,

      txpcs_fab_i             => txpcs_fab,
      txpcs_busy_o            => txpcs_busy,
      txpcs_dreq_o            => txpcs_dreq,
      txpcs_error_o           => txpcs_error,
      txpcs_timestamp_stb_p_o => txpcs_timestamp_stb_p,

      link_ok_o => link_ok,

      serdes_rst_o    => phy_rst_o,
      serdes_loopen_o => phy_loopen_o,
      serdes_prbsen_o => phy_prbsen_o,
      serdes_enable_o => phy_enable_o,
      serdes_syncen_o => phy_syncen_o,

      serdes_tx_clk_i       => phy_ref_clk_i,
      serdes_tx_data_o      => phy_tx_data_o,
      serdes_tx_k_o         => phy_tx_k_o,
      serdes_tx_disparity_i => phy_tx_disparity_i,
      serdes_tx_enc_err_i   => phy_tx_enc_err_i,
      serdes_rx_data_i      => phy_rx_data_i,
      serdes_rx_clk_i       => phy_rx_clk_i,
      serdes_rx_k_i         => phy_rx_k_i,
      serdes_rx_enc_err_i   => phy_rx_enc_err_i,
      serdes_rx_bitslide_i  => phy_rx_bitslide_i,

      rmon_o => rmon,

      mdio_addr_i  => mdio_addr,
      mdio_data_i  => regs_fromwb.mdio_cr_data_o,
      mdio_data_o  => regs_towb_ep.mdio_asr_rdata_i,
      mdio_stb_i   => regs_fromwb.mdio_cr_data_wr_o,
      mdio_rw_i    => regs_fromwb.mdio_cr_rw_o,
      mdio_ready_o => regs_towb_ep.mdio_asr_ready_i);


-------------------------------------------------------------------------------
-- TX FRAMER
-------------------------------------------------------------------------------

  txfra_enable <= link_ok and regs_fromwb.ecr_tx_en_o;

  U_Tx_Framer : ep_tx_framer
    generic map (
      g_with_vlans       => g_with_vlans,
      g_with_timestamper => g_with_timestamper)
    port map (
      clk_sys_i        => clk_sys_i,
      rst_n_i          => rst_n_i,
      pcs_error_i      => txpcs_error,
      pcs_busy_i       => txpcs_busy,
      pcs_fab_o        => txpcs_fab,
      pcs_dreq_i       => txpcs_dreq,
      snk_i            => sink_in,
      snk_o            => sink_out,
      fc_pause_p_i     => txfra_pause_p,
      fc_pause_delay_i => txfra_pause_delay,
      fc_pause_ack_o   => txfra_pause_ack,
      fc_flow_enable_i => txfra_flow_enable,
      oob_fid_value_o  => txoob_fid_value,
      oob_fid_stb_o    => txoob_fid_stb,
      regs_i           => regs_fromwb);


  txfra_flow_enable <= '1';
  txfra_pause_p     <= '0';

  sink_in.dat <= snk_dat_i;
  sink_in.adr <= snk_adr_i;
  sink_in.sel <= snk_sel_i;
  sink_in.cyc <= snk_cyc_i;
  sink_in.stb <= snk_stb_i;
  sink_in.we  <= snk_we_i;
  snk_stall_o <= sink_out.stall;
  snk_ack_o   <= sink_out.ack;
  snk_err_o   <= sink_out.err;
  snk_rty_o   <= sink_out.rty;


-------------------------------------------------------------------------------
-- RX deframer
-------------------------------------------------------------------------------
  rxfra_enable <= link_ok and regs_fromwb.ecr_rx_en_o;

  U_Rx_Path : ep_rx_path
    generic map (
      g_with_vlans          => g_with_vlans,
      g_with_dpi_classifier => g_with_dpi_classifier,
      g_with_rtu            => g_with_rtu)
    port map (
      clk_sys_i => clk_sys_i,
      clk_rx_i  => phy_rx_clk_i,

      rst_n_sys_i => rst_n_sys,
      rst_n_rx_i  => rst_n_rx,

      pcs_fab_i             => rxpcs_fab,
      pcs_fifo_almostfull_o => rxpcs_fifo_almostfull,
      pcs_busy_i            => rxpcs_busy,

      oob_data_i  => rxoob_data,
      oob_valid_i => rxoob_valid,
      oob_ack_o   => rxoob_ack,

      fc_pause_p_o     => rxfra_pause_p,
      fc_pause_delay_o => rxfra_pause_delay,

      rmon_o => rmon,
      regs_i => regs_fromwb,

      rtu_full_i => rtu_full_i,
      src_wb_o   => src_out,
      src_wb_i   => src_in
--      rtu_rq_smac_o      => rtu_rq_smac_o,
--      rtu_rq_dmac_o      => rtu_rq_dmac_o,
--      rtu_rq_vid_o       => rtu_rq_vid_o,
--      rtu_rq_has_vid_o   => rtu_rq_has_vid_o,
--      rtu_rq_prio_o      => rtu_rq_prio_o,
--      rtu_rq_has_prio_o  => rtu_rq_has_prio_o,
--      rtu_rq_strobe_p1_o => rtu_rq_strobe_p1_o);
      );

  src_dat_o    <= src_out.dat;
  src_adr_o    <= src_out.adr;
  src_sel_o    <= src_out.sel;
  src_cyc_o    <= src_out.cyc;
  src_stb_o    <= src_out.stb;
  src_we_o     <= src_out.we;
  src_in.stall <= src_stall_i;
  src_in.ack   <= src_ack_i;

---------------------------------------------------------------------------------
---- RX buffer
---------------------------------------------------------------------------------

--  U_RX_BUF : ep_rx_buffer
--    generic map (
--      g_size_log2 => g_rx_buffer_size_log2)
--    port map (
--      clk_sys_i => clk_sys_i,
--      rst_n_i   => rst_n_i,

--      fra_data_i    => rbuf_data,
--      fra_ctrl_i    => rbuf_ctrl,
--      fra_sof_p_i   => rbuf_sof_p,
--      fra_eof_p_i   => rbuf_eof_p,
--      fra_error_p_i => rbuf_error_p,
--      fra_valid_i   => rbuf_valid,
--      fra_bytesel_i => rbuf_bytesel,
--      fra_drop_o    => rbuf_drop,

--      fab_data_o    => rx_data_o,
--      fab_ctrl_o    => rx_ctrl_o,
--      fab_sof_p_o   => rx_sof_p1_o,
--      fab_eof_p_o   => rx_eof_p1_o,
--      fab_error_p_o => rx_rerror_p1_o,
--      fab_bytesel_o => rx_bytesel_o,
--      fab_valid_o   => rx_valid_o,
--      fab_dreq_i    => rx_dreq_i,

--      ep_ecr_rx_en_fra_i => regs.ecr_rx_en_fra_o,

--      buffer_used_o => rx_buffer_used);

-------------------------------------------------------------------------------
-- Flow control unit
-------------------------------------------------------------------------------

  --U_FLOW_CTL : ep_flow_control
  --  port map (
  --    clk_sys_i => clk_sys_i,
  --    rst_n_i   => rst_n_i,

  --    rx_pause_p1_i    => rxfra_pause_p,
  --    rx_pause_delay_i => rxfra_pause_delay,

  --    tx_pause_o       => txfra_pause,
  --    tx_pause_delay_o => txfra_pause_delay,
  --    tx_pause_ack_i   => txfra_pause_ack,

  --    tx_flow_enable_o => txfra_flow_enable,

  --    rx_buffer_used_i => rx_buffer_used,

  --    ep_fcr_txpause_i   => regs.fcr_txpause_o,
  --    ep_fcr_rxpause_i   => regs.fcr_rxpause_o,
  --    ep_fcr_tx_thr_i    => regs.fcr_tx_thr_o,
  --    ep_fcr_tx_quanta_i => regs.fcr_tx_quanta_o,
  --    rmon_rcvd_pause_o  => rmon.rx_pause,
  --    rmon_sent_pause_o  => rmon.tx_pause
  --    );

-------------------------------------------------------------------------------
-- RMON counters
-------------------------------------------------------------------------------

  --U_RMON_CNT : ep_rmon_counters
  --  generic map (
  --    g_num_counters   => 12,
  --    g_ram_addr_width => 5)
  --  port map (
  --    clk_sys_i       => clk_sys_i,
  --    rst_n_i         => rst_n_i,
  --    cntr_rst_i      => ep_ecr_rst_cnt,
  --    cntr_pulse_i    => rmon_counters(11 downto 0),
  --    ram_addr_o      => ep_rmon_ram_addr,
  --    ram_data_i      => ep_rmon_ram_data_o,
  --    ram_data_o      => ep_rmon_ram_data_i,
  --    ram_wr_o        => ep_rmon_ram_wr,
  --    cntr_overflow_o => open);

  --ep_rmon_ram_rd <= '1';

-------------------------------------------------------------------------------
-- Timestamping unit
-------------------------------------------------------------------------------

  U_EP_TSU : ep_timestamping_unit
    generic map (
      g_timestamp_bits_r => 28,
      g_timestamp_bits_f => 4)
    port map (
      clk_ref_i      => clk_ref_i,
      clk_rx_i       => phy_rx_clk_i,
      clk_sys_i      => clk_sys_i,
      rst_n_rx_i     => rst_n_rx,
      rst_n_sys_i    => rst_n_sys,
      rst_n_ref_i    => rst_n_ref,
      pps_csync_p1_i => pps_csync_p1_i,

      tx_timestamp_stb_p_i => txpcs_timestamp_stb_p,
      rx_timestamp_stb_p_i => rxpcs_timestamp_stb_p,

      rxts_timestamp_o       => rxpcs_timestamp_value,
      rxts_timestamp_valid_o => rxpcs_timestamp_valid,

      txoob_fid_i   => txoob_fid_value,
      txoob_stb_p_i => txoob_fid_stb,


      txtsu_port_id_o => txtsu_port_id_o,
      txtsu_fid_o     => txtsu_frame_id_o,
      txtsu_tsval_o   => txtsu_tsval_o,
      txtsu_valid_o   => txtsu_valid_o,
      txtsu_ack_i     => txtsu_ack_i,

      regs_i => regs_fromwb,
      regs_o => regs_towb_tsu);

------------------------------------------------------------------------------
-- DMTD phase meter
------------------------------------------------------------------------------  

  --U_DMTD : dmtd_phase_meas
  --  generic map (
  --    g_counter_bits         => 14,
  --    g_deglitcher_threshold => 1000)
  --  port map (
  --    clk_sys_i => clk_sys_i,
  --    clk_a_i    => clk_ref_i,
  --    clk_b_i    => phy_rx_clk_i,
  --    clk_dmtd_i => clk_dmtd_i,
  --    rst_n_i    => rst_n_i,

  --    en_i           => regs.dmcr_en_o,
  --    navg_i         => regs.dmcr_n_avg_o,
  --    phase_meas_o   => phase_meas,
  --    phase_meas_p_o => phase_meas_p);

  --p_dmtd_update : process(clk_sys_i, rst_n_i)
  --begin
  --  if rising_edge(clk_sys_i) then
  --    if rst_n_i = '0' then
  --      validity_cntr      <= (others => '0');
  --      regs.dmsr_ps_rdy_i <= '0';
  --    else

  --      if(regs.dmcr_en = '0') then
  --        validity_cntr      <= (others => '0');
  --        regs.dmsr_ps_rdy_i <= '0';
  --      elsif(regs.dmsr_ps_rdy_o = '1' and regs.dmsr_ps_rdy_load_o = '1') then
  --        regs.dmsr_ps_rdy_i <= '0';
  --      elsif(phase_meas_p = '1') then

  --        if(validity_cntr = "11") then
  --          regs.dmsr_ps_rdy_i <= '1';
  --          regs.dmsr_ps_val_i <= phase_meas(23 downto 0);  -- discard few
  --                                                          -- samples right
  --                                                          -- after input change
  --        else
  --          regs.dmsr_ps_rdy_i <= '0';
  --          validity_cntr      <= validity_cntr + 1;
  --        end if;
  --      end if;
  --    end if;
  --  end if;
  --end process;

-------------------------------------------------------------------------------
-- Wishbone controller & IO registers
-------------------------------------------------------------------------------

  U_WB_SLAVE : ep_wishbone_controller
    port map (
      rst_n_i   => rst_n_i,
      wb_clk_i  => clk_sys_i,
      wb_addr_i => wb_adr_i(5 downto 0),
      wb_data_i => wb_dat_i,
      wb_data_o => wb_dat_o,
      wb_cyc_i  => wb_cyc_i,
      wb_sel_i  => wb_sel_i,
      wb_stb_i  => wb_stb_i,
      wb_we_i   => wb_we_i,
      wb_ack_o  => wb_ack_o,

      tx_clk_i => clk_ref_i,

      ep_rmon_ram_wr_i   => ep_rmon_ram_wr,
      ep_rmon_ram_rd_i   => ep_rmon_ram_rd,
      ep_rmon_ram_data_i => ep_rmon_ram_data_i,
      ep_rmon_ram_data_o => ep_rmon_ram_data_o,
      ep_rmon_ram_addr_i => ep_rmon_ram_addr,

      regs_o => regs_fromwb,
      regs_i => regs_towb
      );     


  regs_towb <= regs_towb_ep or regs_towb_tsu;


  p_link_activity : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then

      if(rst_n_i = '0') then
        regs_towb_ep.dsr_lact_i <= '0';
      else
        if(regs_fromwb.dsr_lact_o = '1' and regs_fromwb.dsr_lact_load_o = '1') then
          regs_towb_ep.dsr_lact_i <= '0';  -- clear-on-write
        elsif(txpcs_fab.dvalid = '1' or rxpcs_fab.dvalid = '1') then
          regs_towb_ep.dsr_lact_i <= '1';
        end if;
      end if;
    end if;
  end process;


end syn;


