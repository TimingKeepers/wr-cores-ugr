-------------------------------------------------------------------------------
-- Title      : 1000base-X MAC/Endpoint
-- Project    : WhiteRabbit Switch
-------------------------------------------------------------------------------
-- File       : wrsw_endpoint.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-Co-HT
-- Created    : 2010-04-26
-- Last update: 2011-05-07
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

use work.global_defs.all;
use work.common_components.all;
use work.endpoint_pkg.all;
use work.platform_specific.all;


entity wrsw_endpoint is
  
  generic (
    g_simulation          : integer := 0;
    g_phy_mode            : string  := "GTP";
    g_rx_buffer_size_log2 : integer := 12);
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
-- Ten-Bit PHY interface (TLK1221)
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
-- Xilinx GTP PHY Interace
-------------------------------------------------------------------------------    

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

-------------------------------------------------------------------------------
-- WRF source (output of RXed packets)
-------------------------------------------------------------------------------

-- RX data: framedata word
    rx_data_o : out std_logic_vector(15 downto 0);

-- RX control bus: indicates type of word currently present on rx_data_o:
-- SRC_MAC, DST_MAC, VID_PRIO, PAYLOAD, CRC, OOB
    rx_ctrl_o : out std_logic_vector(c_wrsw_ctrl_size - 1 downto 0);

-- RX byte enable signal: when active, only MSB of rx_data_o contains valid
-- data byte (the last byte of the frame). Used for handling odd-sized frames.
    rx_bytesel_o : out std_logic;


-- start-of-frame pulse: active each time new frame is received. Frame data is
-- available one cycle after on ctrl/data lines
    rx_sof_p1_o : out std_logic;

-- end-of-frame pulse: indicates end of the current frame on fabric i/f. When rx_valid_o
-- is active, rx_ctrl_o and rx_data_o contain the last data word of the current
-- frame.
    rx_eof_p1_o : out std_logic;

-- RX data request line: when HI, subsequent frame words are outputted to
-- rx_ctrl_o, rx_data_o, etc.
    rx_dreq_i : in std_logic;

-- Active HI when rx_ctrl_o, rx_data_o, rx_bytesel_o are valid.
    rx_valid_o : out std_logic;

-- When active, endpoint drops all the incoming frames. When asserted in the
-- middle of reception of the frame, reception is immediately terminated.
    rx_rabort_p1_i : in std_logic;

-- Active HI: indicates that RX path is idle (no data is being currently processed)
    rx_idle_o : out std_logic;

-- RX error strobe: HI pulse indicates that an RX error occured. Error code is
-- present on rx_error_code_o.
    rx_rerror_p1_o : out std_logic;

-------------------------------------------------------------------------------
-- WRF Sink (input for the packets to be TXed)
-------------------------------------------------------------------------------

-- TX data input
    tx_data_i : in std_logic_vector(15 downto 0);

-- RX control bus: indicates type of word currently present on rx_data_o:
-- SRC_MAC, DST_MAC, VID_PRIO, PAYLOAD, CRC, OOB, END_OF_FRAME
    tx_ctrl_i : in std_logic_vector(c_wrsw_ctrl_size -1 downto 0);

-- active HI: indicates the last byte of odd-sized frame. Byte is transferred
-- on MSB of tx_data_i.
    tx_bytesel_i : in std_logic;

-- start of frame signal. HI pulse indicates the beginning of new frame. Upon
-- assertion of tx_sof_p_i, tx_ready_o shall become active, allowing the frame
-- data to be sent.
    tx_sof_p1_i : in std_logic;

-- end-of-frame pulse: indicates end of the current frame on fabric i/f. When rx_valid_o
-- is active, rx_ctrl_o and rx_data_o contain the last data word of the current
-- frame.
    tx_eof_p1_i : in std_logic;


-- active HI: TX fabric is ready to accept data.
    tx_dreq_o : out std_logic;

-- active HI: indicates that tx_data_i, tx_ctrl_i, tx_bytesel_i are valid
    tx_valid_i : in std_logic;

-- Source error: kept only for the comptibility with WRF spec. Ignored by the endpoint.
    tx_rerror_p1_i : in std_logic;

-- TX abort: HI pulse immediately aborts transmission of current frame. 
    tx_tabort_p1_i : in std_logic;

-- TX error strobe: HI pulse indicates that an TX error occured. Error code is
-- present on tx_error_code_o.
    tx_terror_p1_o : out std_logic;

-------------------------------------------------------------------------------
-- TX timestamping unit interface
-------------------------------------------------------------------------------  

-- port ID value
    txtsu_port_id_o : out std_logic_vector(4 downto 0);

-- frame ID value
    txtsu_frame_id_o : out std_logic_vector(c_wrsw_oob_frame_id_size -1 downto 0);

-- timestamp values: gathered on rising clock edge (the main timestamp)
    txtsu_tsval_o : out std_logic_vector(c_wrsw_timestamp_size_r + c_wrsw_timestamp_size_f - 1 downto 0);

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
    rtu_rq_smac_o : out std_logic_vector(c_wrsw_mac_addr_width - 1 downto 0);
    rtu_rq_dmac_o : out std_logic_vector(c_wrsw_mac_addr_width - 1 downto 0);

-- VLAN id (extracted from the header for TRUNK ports and assigned by the port
-- for ACCESS ports)
    rtu_rq_vid_o : out std_logic_vector(c_wrsw_vid_width - 1 downto 0);

-- HI means that packet has valid assigned a valid VID (low - packet is untagged)
    rtu_rq_has_vid_o : out std_logic;

-- packet priority (either extracted from the header or assigned per port).
    rtu_rq_prio_o : out std_logic_vector(c_wrsw_prio_width-1 downto 0);

-- HI indicates that packet has assigned priority.
    rtu_rq_has_prio_o : out std_logic;

-------------------------------------------------------------------------------   
-- Wishbone bus
-------------------------------------------------------------------------------

    wb_cyc_i  : in  std_logic;
    wb_stb_i  : in  std_logic;
    wb_we_i   : in  std_logic;
    wb_sel_i  : in  std_logic_vector(3 downto 0);
    wb_addr_i : in  std_logic_vector(5 downto 0);
    wb_data_i : in  std_logic_vector(31 downto 0);
    wb_data_o : out std_logic_vector(31 downto 0);
    wb_ack_o  : out std_logic

    );

end wrsw_endpoint;

architecture syn of wrsw_endpoint is

  component dmtd_phase_meas
    generic (
      g_deglitch_thr_lo     : integer;
      g_deglitch_thr_hi     : integer;
      g_deglitch_thr_glitch : integer;
      g_counter_bits        : integer);
    port (
      clk_sys_i      : in  std_logic;
      clk_a_i        : in  std_logic;
      clk_b_i        : in  std_logic;
      clk_dmtd_i     : in  std_logic;
      rst_n_i        : in  std_logic;
      en_i           : in  std_logic;
      navg_i         : in  std_logic_vector(11 downto 0);
      phase_meas_o   : out std_logic_vector(31 downto 0);
      phase_meas_p_o : out std_logic);
  end component;

  signal sv_zero : std_logic_vector(63 downto 0);
  signal sv_one  : std_logic_vector(63 downto 0);

  signal tx_clk : std_logic;
  signal rx_clk : std_logic;

-------------------------------------------------------------------------------
-- TX FRAMER -> TX PCS signals
-------------------------------------------------------------------------------

  signal txpcs_data            : std_logic_vector(15 downto 0);
  signal txpcs_bytesel         : std_logic;
  signal txpcs_sof             : std_logic;
  signal txpcs_eof             : std_logic;
  signal txpcs_abort           : std_logic;
  signal txpcs_error_p         : std_logic;
  signal txpcs_busy            : std_logic;
  signal txpcs_valid           : std_logic;
  signal txpcs_fifo_almostfull : std_logic;

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

  signal txts_timestamp_value : std_logic_vector(c_wrsw_timestamp_size_f + c_wrsw_timestamp_size_r - 1 downto 0);
  signal rxts_timestamp_value : std_logic_vector(c_wrsw_timestamp_size_f + c_wrsw_timestamp_size_r - 1 downto 0);
  signal rxts_done_p          : std_logic;
  signal txts_done_p          : std_logic;


-------------------------------------------------------------------------------
-- MDIO signals
-------------------------------------------------------------------------------

  signal ep_mdio_strobe : std_logic;

  signal ep_mdio_cr_data  : std_logic_vector(15 downto 0);
  signal ep_mdio_cr_addr  : std_logic_vector(7 downto 0);
  signal ep_mdio_cr_rw    : std_logic;
  signal ep_mdio_sr_rdata : std_logic_vector(15 downto 0);
  signal ep_mdio_sr_ready : std_logic;

-------------------------------------------------------------------------------
-- RX PCS -> RX DEFRAMER signals
-------------------------------------------------------------------------------

  signal rxpcs_busy    : std_logic;
  signal rxpcs_data    : std_logic_vector(15 downto 0);
  signal rxpcs_bytesel : std_logic;
  signal rxpcs_sof     : std_logic;
  signal rxpcs_eof     : std_logic;
  signal rxpcs_error   : std_logic;
  signal rxpcs_dreq    : std_logic;
  signal rxpcs_valid   : std_logic;

-------------------------------------------------------------------------------
-- RX deframer -> RX buffer signals
-------------------------------------------------------------------------------

  signal rbuf_data    : std_logic_vector(15 downto 0);
  signal rbuf_ctrl    : std_logic_vector(c_wrsw_ctrl_size -1 downto 0);
  signal rbuf_sof_p   : std_logic;
  signal rbuf_eof_p   : std_logic;
  signal rbuf_error_p : std_logic;
  signal rbuf_valid   : std_logic;
  signal rbuf_drop    : std_logic;
  signal rbuf_bytesel : std_logic;

  signal rx_buffer_used : std_logic_vector(7 downto 0);


-------------------------------------------------------------------------------
-- WB slave signals
-------------------------------------------------------------------------------

  signal ep_ecr_portid  : std_logic_vector(4 downto 0);
  signal ep_ecr_pcs_lbk : std_logic;
  signal ep_ecr_fra_lbk : std_logic;




  signal ep_tscr_en_txts  : std_logic;
  signal ep_tscr_en_rxts  : std_logic;
  signal ep_tscr_cs_start : std_logic;
  signal ep_tscr_cs_done  : std_logic;

  signal ep_rfcr_a_runt   : std_logic;
  signal ep_rfcr_a_giant  : std_logic;
  signal ep_rfcr_a_hp     : std_logic;
  signal ep_rfcr_a_frag   : std_logic;
  signal ep_rfcr_qmode    : std_logic_vector(1 downto 0);
  signal ep_rfcr_fix_prio : std_logic;
  signal ep_rfcr_prio_val : std_logic_vector(2 downto 0);
  signal ep_rfcr_vid_val  : std_logic_vector(11 downto 0);
  signal ep_mach          : std_logic_vector(15 downto 0);
  signal ep_macl          : std_logic_vector(31 downto 0);

  signal ep_fcr_rxpause   : std_logic;
  signal ep_fcr_txpause   : std_logic;
  signal ep_fcr_tx_thr    : std_logic_vector(7 downto 0);
  signal ep_fcr_tx_quanta : std_logic_vector(15 downto 0);

  signal ep_dmcr_en          : std_logic;
  signal ep_dmcr_n_avg       : std_logic_vector(11 downto 0);
  signal ep_dmsr_ps_val      : std_logic_vector(23 downto 0);
  signal ep_dmsr_ps_rdy_out  : std_logic;
  signal ep_dmsr_ps_rdy_in   : std_logic;
  signal ep_dmsr_ps_rdy_load : std_logic;

  signal ep_dsr_lact_out  : std_logic;
  signal ep_dsr_lact_in   : std_logic;
  signal ep_dsr_lact_load : std_logic;

-------------------------------------------------------------------------------
-- TBI signals
-------------------------------------------------------------------------------  

  signal tbi_tx_data : std_logic_vector(9 downto 0);
  signal tbi_rx_data : std_logic_vector(9 downto 0);

-------------------------------------------------------------------------------
-- flow control signals
-------------------------------------------------------------------------------

  signal txfra_flow_enable : std_logic;

  signal rxfra_pause_p       : std_logic;
  signal rxfra_pause_delay   : std_logic_vector(15 downto 0);
  signal rxbuf_threshold_hit : std_logic;

  signal txfra_pause       : std_logic;
  signal txfra_pause_ack   : std_logic;
  signal txfra_pause_delay : std_logic_vector(15 downto 0);


-------------------------------------------------------------------------------
-- RMON signals
-------------------------------------------------------------------------------

  signal ep_ecr_rst_cnt   : std_logic;
  signal ep_ecr_rx_en_fra : std_logic;
  signal ep_ecr_tx_en_fra : std_logic;


  signal ep_rmon_ram_addr   : std_logic_vector(4 downto 0);
  signal ep_rmon_ram_data_o : std_logic_vector(31 downto 0);
  signal ep_rmon_ram_rd     : std_logic;
  signal ep_rmon_ram_data_i : std_logic_vector(31 downto 0);
  signal ep_rmon_ram_wr     : std_logic;

  signal rmon_counters : std_logic_vector(31 downto 0);

  signal rofifo_write, rofifo_full, oob_valid_d0 : std_logic;

  signal phase_meas    : std_logic_vector(31 downto 0);
  signal phase_meas_p  : std_logic;
  signal validity_cntr : unsigned(1 downto 0);

  signal link_ok : std_logic;

  signal txfra_enable, rxfra_enable : std_logic;

begin

  sv_zero <= (others => '0');
  sv_one  <= (others => '1');

-------------------------------------------------------------------------------
-- clock/reset stuff
-------------------------------------------------------------------------------

  gen_error_invalid_mode : if(g_phy_mode /= "TBI" and g_phy_mode /= "GTP") generate
    assert false report "Unsupported PHY mode (g_phy_mode generic). You can use either the TBI or GTP mode." severity failure;
  end generate gen_error_invalid_mode;


  -- TBI interface mode (TLK1221 PHY)
  gen_tbi : if(g_phy_mode = "TBI") generate
    tx_clk <= clk_ref_i;
    rx_clk <= tbi_rbclk_i;

-- PHY data inout/output registers

    p_tbi_data_output_reg : process (tx_clk)
    begin
      if rising_edge(tx_clk) then
        tbi_td_o <= tbi_tx_data;
      end if;
    end process;

    p_tbi_data_input_reg : process (rx_clk)
    begin
      if rising_edge(rx_clk) then
        tbi_rx_data <= tbi_rd_i;
      end if;
    end process;
  end generate gen_tbi;

-- Xilinx GTP interface mode
  gen_gtp : if(g_phy_mode = "GTP") generate
    tx_clk <= gtp_tx_clk_i;
    rx_clk <= gtp_rx_clk_i;
  end generate gen_gtp;

-------------------------------------------------------------------------------
-- 1000Base-X PCS
-------------------------------------------------------------------------------

  U_PCS_1000BASEX : ep_1000basex_pcs
    generic map (
      g_simulation => g_simulation,
      g_phy_mode   => g_phy_mode)
    port map (
      rst_n_i   => rst_n_i,
      clk_sys_i => clk_sys_i,

      rxpcs_busy_o            => rxpcs_busy,
      rxpcs_data_o            => rxpcs_data,
      rxpcs_bytesel_o         => rxpcs_bytesel,
      rxpcs_sof_o             => rxpcs_sof,
      rxpcs_eof_o             => rxpcs_eof,
      rxpcs_error_o           => rxpcs_error,
      rxpcs_dreq_i            => rxpcs_dreq,
      rxpcs_valid_o           => rxpcs_valid,
      rxpcs_timestamp_stb_p_o => rxpcs_timestamp_stb_p,

      txpcs_data_i            => txpcs_data,
      txpcs_bytesel_i         => txpcs_bytesel,
      txpcs_sof_i             => txpcs_sof,
      txpcs_eof_i             => txpcs_eof,
      txpcs_abort_i           => txpcs_abort,
      txpcs_error_p_o         => txpcs_error_p,
      txpcs_busy_o            => txpcs_busy,
      txpcs_valid_i           => txpcs_valid,
      txpcs_fifo_almostfull_o => txpcs_fifo_almostfull,
      txpcs_timestamp_stb_p_o => txpcs_timestamp_stb_p,

      link_ok_o => link_ok,

      tbi_rbclk_i  => rx_clk,
      tbi_rxdata_i => tbi_rx_data,
      tbi_txclk_i  => tx_clk,
      tbi_txdata_o => tbi_tx_data,

      tbi_syncen_o => tbi_syncen_o,
      tbi_loopen_o => tbi_loopen_o,
      tbi_prbsen_o => tbi_prbsen_o,
      tbi_enable_o => tbi_enable_o,

      gtp_tx_clk_i       => gtp_tx_clk_i,
      gtp_tx_data_o      => gtp_tx_data_o,
      gtp_tx_k_o         => gtp_tx_k_o,
      gtp_tx_disparity_i => gtp_tx_disparity_i,
      gtp_tx_enc_err_i   => gtp_tx_enc_err_i,
      gtp_rx_data_i      => gtp_rx_data_i,
      gtp_rx_clk_i       => gtp_rx_clk_i,
      gtp_rx_k_i         => gtp_rx_k_i,
      gtp_rx_enc_err_i   => gtp_rx_enc_err_i,
      gtp_rx_bitslide_i  => gtp_rx_bitslide_i,
      gtp_rst_o          => gtp_rst_o,
      gtp_loopen_o       => gtp_loopen_o,

      rmon_syncloss_p_o     => rmon_counters(0),
      rmon_invalid_code_p_o => rmon_counters(1),
      rmon_rx_overrun_p_o   => rmon_counters(2),
      rmon_tx_underrun_o    => rmon_counters(3),

      mdio_addr_i  => ep_mdio_cr_addr,
      mdio_data_i  => ep_mdio_cr_data,
      mdio_data_o  => ep_mdio_sr_rdata,
      mdio_stb_i   => ep_mdio_strobe,
      mdio_rw_i    => ep_mdio_cr_rw,
      mdio_ready_o => ep_mdio_sr_ready);



-------------------------------------------------------------------------------
-- TX FRAMER
-------------------------------------------------------------------------------

  txfra_enable <= link_ok and ep_ecr_tx_en_fra;

  U_TX_FRA : ep_tx_framer
    port map (
      clk_sys_i => clk_sys_i,
      rst_n_i   => rst_n_i,

      pcs_data_o            => txpcs_data,
      pcs_bytesel_o         => txpcs_bytesel,
      pcs_sof_o             => txpcs_sof,
      pcs_eof_o             => txpcs_eof,
      pcs_abort_o           => txpcs_abort,
      pcs_error_i           => txpcs_error_p,
      pcs_busy_i            => txpcs_busy,
      pcs_fifo_write_o      => txpcs_valid,
      pcs_fifo_almostfull_i => txpcs_fifo_almostfull,


      tx_data_i    => tx_data_i,
      tx_ctrl_i    => tx_ctrl_i,
      tx_bytesel_i => tx_bytesel_i,

      tx_sof_p1_i => tx_sof_p1_i,
      tx_eof_p1_i => tx_eof_p1_i,

      tx_dreq_o  => tx_dreq_o,
      tx_valid_i => tx_valid_i,

      tx_rerror_p1_i => tx_rerror_p1_i,
      tx_tabort_p1_i => tx_tabort_p1_i,
      tx_terror_p1_o => tx_terror_p1_o,

      oob_fid_value_o => txoob_fid_value,
      oob_fid_stb_o   => txoob_fid_stb,

      tx_pause_i       => txfra_pause,
      tx_pause_ack_o   => txfra_pause_ack,
      tx_pause_delay_i => txfra_pause_delay,

      tx_flow_enable_i => txfra_flow_enable,

      ep_tcr_en_fra_i => txfra_enable,
      ep_rfcr_qmode_i => ep_rfcr_qmode,
      ep_macl_i       => ep_macl,
      ep_mach_i       => ep_mach);



-------------------------------------------------------------------------------
-- RX deframer
-------------------------------------------------------------------------------
  rxfra_enable <= link_ok and ep_ecr_rx_en_fra;

  U_RX_DFRA : ep_rx_deframer
    port map (
      clk_sys_i => clk_sys_i,
      rst_n_i   => rst_n_i,

      pcs_data_i    => rxpcs_data,
      pcs_bytesel_i => rxpcs_bytesel,
      pcs_sof_i     => rxpcs_sof,
      pcs_eof_i     => rxpcs_eof,
      pcs_dreq_o    => rxpcs_dreq,
      pcs_valid_i   => rxpcs_valid,
      pcs_error_i   => rxpcs_error,

      oob_data_i  => rxoob_data,
      oob_valid_i => rxoob_valid,
      oob_ack_o   => rxoob_ack,

      rbuf_sof_p1_o   => rbuf_sof_p,
      rbuf_eof_p1_o   => rbuf_eof_p,
      rbuf_ctrl_o    => rbuf_ctrl,
      rbuf_data_o    => rbuf_data,
      rbuf_valid_o   => rbuf_valid,
      rbuf_drop_i    => rbuf_drop,
      rbuf_bytesel_o => rbuf_bytesel,
      rbuf_rerror_p1_o => rbuf_error_p,

      fc_pause_p_o     => rxfra_pause_p,
      fc_pause_delay_o => rxfra_pause_delay,

      rmon_rx_crc_err_p_o  => rmon_counters(4),
      rmon_rx_ok_p_o       => rmon_counters(5),
      rmon_rx_runt_p_o     => rmon_counters(6),
      rmon_rx_giant_p_o    => rmon_counters(7),
      rmon_rx_pcs_err_p_o  => rmon_counters(8),
      rmon_rx_buf_drop_p_o => rmon_counters(9),

      ep_rcr_en_fra_i    => rxfra_enable,
      ep_tscr_en_rxts_i  => ep_tscr_en_rxts,
      ep_rfcr_qmode_i    => ep_rfcr_qmode,
      ep_rfcr_a_runt_i   => ep_rfcr_a_runt,
      ep_rfcr_a_giant_i  => ep_rfcr_a_giant,
      ep_rfcr_fix_prio_i => ep_rfcr_fix_prio,
      ep_rfcr_prio_val_i => ep_rfcr_prio_val,
      ep_rfcr_vid_val_i  => ep_rfcr_vid_val,

      rtu_full_i => rtu_full_i,

      rtu_rq_smac_o      => rtu_rq_smac_o,
      rtu_rq_dmac_o      => rtu_rq_dmac_o,
      rtu_rq_vid_o       => rtu_rq_vid_o,
      rtu_rq_has_vid_o   => rtu_rq_has_vid_o,
      rtu_rq_prio_o      => rtu_rq_prio_o,
      rtu_rq_has_prio_o  => rtu_rq_has_prio_o,
      rtu_rq_strobe_p1_o => rtu_rq_strobe_p1_o);

-------------------------------------------------------------------------------
-- RX buffer
-------------------------------------------------------------------------------

  U_RX_BUF : ep_rx_buffer
    generic map (
      g_size_log2 => g_rx_buffer_size_log2)
    port map (
      clk_sys_i => clk_sys_i,
      rst_n_i   => rst_n_i,

      fra_data_i    => rbuf_data,
      fra_ctrl_i    => rbuf_ctrl,
      fra_sof_p_i   => rbuf_sof_p,
      fra_eof_p_i   => rbuf_eof_p,
      fra_error_p_i => rbuf_error_p,
      fra_valid_i   => rbuf_valid,
      fra_bytesel_i => rbuf_bytesel,
      fra_drop_o    => rbuf_drop,

      fab_data_o    => rx_data_o,
      fab_ctrl_o    => rx_ctrl_o,
      fab_sof_p_o   => rx_sof_p1_o,
      fab_eof_p_o   => rx_eof_p1_o,
      fab_error_p_o => rx_rerror_p1_o,
      fab_bytesel_o => rx_bytesel_o,
      fab_valid_o   => rx_valid_o,
      fab_dreq_i    => rx_dreq_i,

      ep_ecr_rx_en_fra_i => ep_ecr_rx_en_fra,

      buffer_used_o => rx_buffer_used);

-------------------------------------------------------------------------------
-- Flow control unit
-------------------------------------------------------------------------------

  U_FLOW_CTL : ep_flow_control
    port map (
      clk_sys_i => clk_sys_i,
      rst_n_i   => rst_n_i,

      rx_pause_p1_i    => rxfra_pause_p,
      rx_pause_delay_i => rxfra_pause_delay,

      tx_pause_o       => txfra_pause,
      tx_pause_delay_o => txfra_pause_delay,
      tx_pause_ack_i   => txfra_pause_ack,

      tx_flow_enable_o => txfra_flow_enable,

      rx_buffer_used_i => rx_buffer_used,

      ep_fcr_txpause_i   => ep_fcr_txpause,
      ep_fcr_rxpause_i   => ep_fcr_rxpause,
      ep_fcr_tx_thr_i    => ep_fcr_tx_thr,
      ep_fcr_tx_quanta_i => ep_fcr_tx_quanta,
      rmon_rcvd_pause_o  => rmon_counters(10),
      rmon_sent_pause_o  => rmon_counters(11)

      );

-------------------------------------------------------------------------------
-- RMON counters
-------------------------------------------------------------------------------

  U_RMON_CNT : ep_rmon_counters
    generic map (
      g_num_counters   => 12,
      g_ram_addr_width => 5)
    port map (
      clk_sys_i       => clk_sys_i,
      rst_n_i         => rst_n_i,
      cntr_rst_i      => ep_ecr_rst_cnt,
      cntr_pulse_i    => rmon_counters(11 downto 0),
      ram_addr_o      => ep_rmon_ram_addr,
      ram_data_i      => ep_rmon_ram_data_o,
      ram_data_o      => ep_rmon_ram_data_i,
      ram_wr_o        => ep_rmon_ram_wr,
      cntr_overflow_o => open);

  ep_rmon_ram_rd <= '1';

-------------------------------------------------------------------------------
-- Timestamping unit
-------------------------------------------------------------------------------

  U_EP_TSU : ep_timestamping_unit
    generic map (
      g_timestamp_bits_r => c_wrsw_timestamp_size_r,
      g_timestamp_bits_f => c_wrsw_timestamp_size_f)
    port map (
      clk_ref_i      => clk_ref_i,
      clk_sys_i      => clk_sys_i,
      rst_n_i        => rst_n_i,
      pps_csync_p1_i => pps_csync_p1_i,

      tx_timestamp_stb_p_i => txpcs_timestamp_stb_p,
      rx_timestamp_stb_p_i => rxpcs_timestamp_stb_p,

      txoob_fid_i   => txoob_fid_value,
      txoob_stb_p_i => txoob_fid_stb,

      rxoob_data_o  => rxoob_data,
      rxoob_valid_o => rxoob_valid,
      rxoob_ack_i   => rxoob_ack,

      txtsu_port_id_o => txtsu_port_id_o,
      txtsu_fid_o     => txtsu_frame_id_o,
      txtsu_tsval_o   => txtsu_tsval_o,
      txtsu_valid_o   => txtsu_valid_o,
      txtsu_ack_i     => txtsu_ack_i,

      ep_tscr_en_txts_i  => ep_tscr_en_txts,
      ep_tscr_en_rxts_i  => ep_tscr_en_rxts,
      ep_tscr_cs_done_o  => ep_tscr_cs_done,
      ep_tscr_cs_start_i => ep_tscr_cs_start,
      ep_ecr_portid_i    => ep_ecr_portid);

-------------------------------------------------------------------------------
-- DMTD phase meter
------------------------------------------------------------------------------  

  U_DMTD : dmtd_phase_meas
    generic map (
      g_deglitch_thr_lo     => 150,
      g_deglitch_thr_hi     => 150,
      g_deglitch_thr_glitch => 60,
      g_counter_bits        => 14)
    port map (
      clk_sys_i  => clk_sys_i,
      clk_a_i    => tx_clk,
      clk_b_i    => rx_clk,
      clk_dmtd_i => clk_dmtd_i,
      rst_n_i    => rst_n_i,

      en_i           => ep_dmcr_en,
      navg_i         => ep_dmcr_n_avg,
      phase_meas_o   => phase_meas,
      phase_meas_p_o => phase_meas_p);

  p_dmtd_update : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        validity_cntr      <= (others => '0');
        ep_dmsr_ps_rdy_out <= '0';
      else

        if(ep_dmcr_en = '0') then
          validity_cntr      <= (others => '0');
          ep_dmsr_ps_rdy_out <= '0';
        elsif(ep_dmsr_ps_rdy_in = '1' and ep_dmsr_ps_rdy_load = '1') then
          ep_dmsr_ps_rdy_out <= '0';
        elsif(phase_meas_p = '1') then

          if(validity_cntr = "11") then
            ep_dmsr_ps_rdy_out <= '1';
            ep_dmsr_ps_val     <= phase_meas(23 downto 0);  -- discard few
                                                            -- samples right
                                                            -- after input change
          else
            ep_dmsr_ps_rdy_out <= '0';
            validity_cntr      <= validity_cntr + 1;
          end if;
        end if;
      end if;
    end if;
  end process;

-------------------------------------------------------------------------------
-- Wishbone controller & IO registers
-------------------------------------------------------------------------------

  U_WB_SLAVE : ep_wishbone_controller
    port map (
      rst_n_i   => rst_n_i,
      wb_clk_i  => clk_sys_i,
      wb_addr_i => wb_addr_i(5 downto 0),
      wb_data_i => wb_data_i,
      wb_data_o => wb_data_o,
      wb_cyc_i  => wb_cyc_i,
      wb_sel_i  => wb_sel_i,
      wb_stb_i  => wb_stb_i,
      wb_we_i   => wb_we_i,
      wb_ack_o  => wb_ack_o,

      tx_clk_i => tx_clk,

      ep_ecr_portid_o    => ep_ecr_portid,
      ep_ecr_rst_cnt_o   => ep_ecr_rst_cnt,
      ep_ecr_tx_en_fra_o => ep_ecr_tx_en_fra,
      ep_ecr_rx_en_fra_o => ep_ecr_rx_en_fra,

      ep_tscr_en_txts_o  => ep_tscr_en_txts,
      ep_tscr_en_rxts_o  => ep_tscr_en_rxts,
      ep_tscr_cs_start_o => ep_tscr_cs_start,
      ep_tscr_cs_done_i  => ep_tscr_cs_done,

      ep_rfcr_a_runt_o   => ep_rfcr_a_runt,
      ep_rfcr_a_giant_o  => ep_rfcr_a_giant,
      ep_rfcr_a_hp_o     => ep_rfcr_a_hp,
      ep_rfcr_a_frag_o   => ep_rfcr_a_frag,
      ep_rfcr_qmode_o    => ep_rfcr_qmode,
      ep_rfcr_fix_prio_o => ep_rfcr_fix_prio,
      ep_rfcr_prio_val_o => ep_rfcr_prio_val,
      ep_rfcr_vid_val_o  => ep_rfcr_vid_val,

      ep_fcr_tx_thr_o    => ep_fcr_tx_thr,
      ep_fcr_tx_quanta_o => ep_fcr_tx_quanta,
      ep_fcr_txpause_o   => ep_fcr_txpause,
      ep_fcr_rxpause_o   => ep_fcr_rxpause,

      ep_macl_o => ep_macl,
      ep_mach_o => ep_mach,

      ep_rmon_ram_wr_i   => ep_rmon_ram_wr,
      ep_rmon_ram_rd_i   => ep_rmon_ram_rd,
      ep_rmon_ram_data_i => ep_rmon_ram_data_i,
      ep_rmon_ram_data_o => ep_rmon_ram_data_o,
      ep_rmon_ram_addr_i => ep_rmon_ram_addr,

      ep_dmcr_en_o          => ep_dmcr_en,
      ep_dmcr_n_avg_o       => ep_dmcr_n_avg,
      ep_dmsr_ps_val_i      => ep_dmsr_ps_val,
      ep_dmsr_ps_rdy_o      => ep_dmsr_ps_rdy_in,
      ep_dmsr_ps_rdy_i      => ep_dmsr_ps_rdy_out,
      ep_dmsr_ps_rdy_load_o => ep_dmsr_ps_rdy_load,

      ep_mdio_cr_data_o    => ep_mdio_cr_data,
      ep_mdio_cr_data_wr_o => ep_mdio_strobe,
      ep_mdio_cr_addr_o    => ep_mdio_cr_addr,
      ep_mdio_cr_rw_o      => ep_mdio_cr_rw,
      ep_mdio_sr_rdata_i   => ep_mdio_sr_rdata,
      ep_mdio_sr_ready_i   => ep_mdio_sr_ready,

      ep_dsr_lstatus_i   => link_ok,
      ep_dsr_lact_o      => ep_dsr_lact_out,
      ep_dsr_lact_i      => ep_dsr_lact_in,
      ep_dsr_lact_load_o => ep_dsr_lact_load
      );     


  p_link_activity : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then

      if(rst_n_i = '0') then
        ep_dsr_lact_in <= '0';
      else
        if(ep_dsr_lact_out = '1' and ep_dsr_lact_load = '1') then
          ep_dsr_lact_out <= '0';       -- clear-on-write
        elsif(txpcs_valid = '1' or rxpcs_valid = '1') then
          ep_dsr_lact_out <= '1';
        end if;
      end if;
    end if;
  end process;


end syn;


