-------------------------------------------------------------------------------
-- Title      : 1000base-X MAC/Endpoint
-- Project    : WhiteRabbit Switch
-------------------------------------------------------------------------------
-- File       : wr_endpoint.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-Co-HT
-- Created    : 2010-04-26
-- Last update: 2013-03-15
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
-- - decodes MAC addresses, VIDs and priorities and passes them to the RTU.
-------------------------------------------------------------------------------
-- Copyright (c) 2010, 2011 Tomasz Wlostowski
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
use work.wishbone_pkg.all;

entity wr_endpoint is
  
  generic (
    g_interface_mode      : t_wishbone_interface_mode      := CLASSIC;
    g_address_granularity : t_wishbone_address_granularity := WORD;
    g_tx_force_gap_length : integer                        := 0;
    g_simulation          : boolean                        := false;
    g_pcs_16bit           : boolean                        := true;
    g_rx_buffer_size      : integer                        := 1024;
    g_with_rx_buffer      : boolean                        := true;
    g_with_flow_control   : boolean                        := true;
    g_with_timestamper    : boolean                        := true;
    g_with_dpi_classifier : boolean                        := false;
    g_with_vlans          : boolean                        := true;
    g_with_rtu            : boolean                        := true;
    g_with_leds           : boolean                        := true;
    g_with_dmtd           : boolean                        := false
    );
  port (

-------------------------------------------------------------------------------
-- Clocks
-------------------------------------------------------------------------------

-- Endpoint transmit reference clock. Must be 125 MHz +- 100 ppm
    clk_ref_i : in std_logic;

-- reference clock / 2 (62.5 MHz, in-phase with refclk)
    clk_sys_i : in std_logic;

-- DMTD offset clock for phase tracking - used only if g_with_dmtd == true
    clk_dmtd_i : in std_logic;

-- sync reset (clk_sys_i domain), active LO
    rst_n_i : in std_logic;

-- PPS input (1 clk_ref_i cycle HI) for synchronizing timestamp counter
    pps_csync_p1_i : in std_logic;

-- PPS valid input (clk_ref_i domain), when 1, the external PPS generator/servo
-- is not adjusting the time scale, so we can safely timestamp.
    pps_valid_i : in std_logic := '1';

-------------------------------------------------------------------------------
-- PHY Interace (8/16 bit PCS)
-------------------------------------------------------------------------------    

    phy_rst_o    : out std_logic;
    phy_loopen_o : out std_logic;
    phy_enable_o : out std_logic;
    phy_syncen_o : out std_logic;

    phy_ref_clk_i      : in  std_logic;
    phy_tx_data_o      : out std_logic_vector(15 downto 0);
    phy_tx_k_o         : out std_logic_vector(1 downto 0);
    phy_tx_disparity_i : in  std_logic;
    phy_tx_enc_err_i   : in  std_logic;

    phy_rx_data_i     : in std_logic_vector(15 downto 0);
    phy_rx_clk_i      : in std_logic;
    phy_rx_k_i        : in std_logic_vector(1 downto 0);
    phy_rx_enc_err_i  : in std_logic;
    phy_rx_bitslide_i : in std_logic_vector(4 downto 0);

-------------------------------------------------------------------------------
-- GMII Interface (8-bit)
-------------------------------------------------------------------------------

    gmii_tx_clk_i : in  std_logic;
    gmii_txd_o    : out std_logic_vector(7 downto 0) := x"00";
    gmii_tx_en_o  : out std_logic                    := '0';
    gmii_tx_er_o  : out std_logic                    := '0';

    gmii_rx_clk_i : in std_logic;
    gmii_rxd_i    : in std_logic_vector(7 downto 0);
    gmii_rx_er_i  : in std_logic;
    gmii_rx_dv_i  : in std_logic;

    ---------------------------------------------------------------------------
    -- Wishbone I/O
    ---------------------------------------------------------------------------

    src_dat_o   : out std_logic_vector(15 downto 0);
    src_adr_o   : out std_logic_vector(1 downto 0);
    src_sel_o   : out std_logic_vector(1 downto 0);
    src_cyc_o   : out std_logic;
    src_stb_o   : out std_logic;
    src_we_o    : out std_logic;
    src_stall_i : in  std_logic;
    src_ack_i   : in  std_logic;
    src_err_i   : in  std_logic;

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

-- Port ID value
    txtsu_port_id_o  : out std_logic_vector(4 downto 0);
-- Frame ID value
    txtsu_frame_id_o : out std_logic_vector(16 -1 downto 0);

-- TX Timestamp and correctness info
    txtsu_ts_value_o     : out std_logic_vector(28 + 4 - 1 downto 0);
    txtsu_ts_incorrect_o : out std_logic;

-- TX timestamp strobe: HI tells the TX timestamping unit that a timestamp is
-- available on txtsu_ts_value_o, txtsu_fid_o andd txtsu_port_id_o. The correctness
-- of the timestamping is indiacted on txtsu_ts_incorrect_o. Line remains HI
-- until assertion of txtsu_ack_i.
    txtsu_stb_o : out std_logic;

-- TX timestamp acknowledge: HI indicates that TXTSU has successfully received
-- the timestamp
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

    wb_cyc_i   : in  std_logic;
    wb_stb_i   : in  std_logic;
    wb_we_i    : in  std_logic;
    wb_sel_i   : in  std_logic_vector(3 downto 0);
    wb_adr_i   : in  std_logic_vector(7 downto 0);
    wb_dat_i   : in  std_logic_vector(31 downto 0);
    wb_dat_o   : out std_logic_vector(31 downto 0);
    wb_ack_o   : out std_logic;
    wb_stall_o : out std_logic;

-------------------------------------------------------------------------------
-- Misc stuff
-------------------------------------------------------------------------------

    led_link_o : out std_logic;
    led_act_o  : out std_logic

    );

end wr_endpoint;

architecture syn of wr_endpoint is

  constant c_zeros : std_logic_vector(63 downto 0) := (others => '0');
  constant c_ones  : std_logic_vector(63 downto 0) := (others => '0');

  function f_pick_rate (pcs_16bit : boolean) return integer is
  begin
    if(pcs_16bit) then
      return 62500000;
    else
      return 125000000;
    end if;
  end f_pick_rate;




-------------------------------------------------------------------------------
  component dmtd_phase_meas
    generic (
      g_deglitcher_threshold : integer;
      g_counter_bits         : integer);
    port (
      rst_n_i        : in  std_logic;
      clk_sys_i      : in  std_logic;
      clk_a_i        : in  std_logic;
      clk_b_i        : in  std_logic;
      clk_dmtd_i     : in  std_logic;
      en_i           : in  std_logic;
      navg_i         : in  std_logic_vector(11 downto 0);
      phase_meas_o   : out std_logic_vector(31 downto 0);
      phase_meas_p_o : out std_logic);
  end component;

  component ep_tx_framer
    generic (
      g_with_vlans       : boolean;
      g_with_timestamper : boolean;
      g_force_gap_length : integer);
    port (
      clk_sys_i              : in  std_logic;
      rst_n_i                : in  std_logic;
      pcs_fab_o              : out t_ep_internal_fabric;
      pcs_error_i            : in  std_logic;
      pcs_busy_i             : in  std_logic;
      pcs_dreq_i             : in  std_logic;
      snk_i                  : in  t_wrf_sink_in;
      snk_o                  : out t_wrf_sink_out;
      fc_pause_p_i           : in  std_logic;
      fc_pause_delay_i       : in  std_logic_vector(15 downto 0);
      fc_pause_ack_o         : out std_logic;
      fc_flow_enable_i       : in  std_logic;
      txtsu_port_id_o        : out std_logic_vector(4 downto 0);
      txtsu_fid_o            : out std_logic_vector(16 -1 downto 0);
      txtsu_ts_value_o       : out std_logic_vector(28 + 4 - 1 downto 0);
      txtsu_ts_incorrect_o   : out std_logic;
      txtsu_stb_o            : out std_logic;
      txtsu_ack_i            : in  std_logic;
      txts_timestamp_i       : in  std_logic_vector(31 downto 0);
      txts_timestamp_valid_i : in  std_logic;
      regs_i                 : in  t_ep_out_registers);
  end component;

  component ep_rx_path
    generic (
      g_with_vlans          : boolean;
      g_with_dpi_classifier : boolean;
      g_with_rtu            : boolean;
      g_with_rx_buffer      : boolean;
      g_rx_buffer_size      : integer);
    port (
      clk_sys_i              : in    std_logic;
      clk_rx_i               : in    std_logic;
      rst_n_sys_i            : in    std_logic;
      rst_n_rx_i             : in    std_logic;
      pcs_fab_i              : in    t_ep_internal_fabric;
      pcs_fifo_almostfull_o  : out   std_logic;
      pcs_busy_i             : in    std_logic;
      src_wb_o               : out   t_wrf_source_out;
      src_wb_i               : in    t_wrf_source_in;
      fc_pause_p_o           : out   std_logic;
      fc_pause_delay_o       : out   std_logic_vector(15 downto 0);
      fc_buffer_occupation_o : out   std_logic_vector(7 downto 0);
      rmon_o                 : inout t_rmon_triggers;
      regs_i                 : in    t_ep_out_registers;
      regs_o                 : out   t_ep_in_registers;
      rtu_rq_o               : out   t_ep_internal_rtu_request;
      rtu_full_i             : in    std_logic;
      rtu_rq_valid_o         : out   std_logic);
  end component;

  component ep_1000basex_pcs
    generic (
      g_simulation : boolean;
      g_16bit      : boolean);
    port (
      rst_n_i                       : in    std_logic;
      clk_sys_i                     : in    std_logic;
      rxpcs_fab_o                   : out   t_ep_internal_fabric;
      rxpcs_fifo_almostfull_i       : in    std_logic;
      rxpcs_busy_o                  : out   std_logic;
      rxpcs_timestamp_trigger_p_a_o : out   std_logic;
      rxpcs_timestamp_i             : in    std_logic_vector(31 downto 0);
      rxpcs_timestamp_stb_i         : in    std_logic;
      rxpcs_timestamp_valid_i       : in    std_logic;
      txpcs_fab_i                   : in    t_ep_internal_fabric;
      txpcs_error_o                 : out   std_logic;
      txpcs_busy_o                  : out   std_logic;
      txpcs_dreq_o                  : out   std_logic;
      txpcs_timestamp_trigger_p_a_o : out   std_logic;
      link_ok_o                     : out   std_logic;
      serdes_rst_o                  : out   std_logic;
      serdes_syncen_o               : out   std_logic;
      serdes_loopen_o               : out   std_logic;
      serdes_enable_o               : out   std_logic;
      serdes_tx_clk_i               : in    std_logic;
      serdes_tx_data_o              : out   std_logic_vector(15 downto 0);
      serdes_tx_k_o                 : out   std_logic_vector(1 downto 0);
      serdes_tx_disparity_i         : in    std_logic;
      serdes_tx_enc_err_i           : in    std_logic;
      serdes_rx_clk_i               : in    std_logic;
      serdes_rx_data_i              : in    std_logic_vector(15 downto 0);
      serdes_rx_k_i                 : in    std_logic_vector(1 downto 0);
      serdes_rx_enc_err_i           : in    std_logic;
      serdes_rx_bitslide_i          : in    std_logic_vector(4 downto 0);
      rmon_o                        : inout t_rmon_triggers;
      mdio_addr_i                   : in    std_logic_vector(15 downto 0);
      mdio_data_i                   : in    std_logic_vector(15 downto 0);
      mdio_data_o                   : out   std_logic_vector(15 downto 0);
      mdio_stb_i                    : in    std_logic;
      mdio_rw_i                     : in    std_logic;
      mdio_ready_o                  : out   std_logic);
  end component;

  component ep_timestamping_unit
    generic (
      g_timestamp_bits_r : natural;
      g_timestamp_bits_f : natural;
      g_ref_clock_rate   : integer);
    port (
      clk_ref_i                  : in  std_logic;
      clk_sys_i                  : in  std_logic;
      clk_rx_i                   : in  std_logic;
      rst_n_rx_i                 : in  std_logic;
      rst_n_ref_i                : in  std_logic;
      rst_n_sys_i                : in  std_logic;
      pps_csync_p1_i             : in  std_logic;
      pps_valid_i                : in  std_logic;
      tx_timestamp_trigger_p_a_i : in  std_logic;
      rx_timestamp_trigger_p_a_i : in  std_logic;
      rxts_timestamp_o           : out std_logic_vector(31 downto 0);
      rxts_timestamp_stb_o       : out std_logic;
      rxts_timestamp_valid_o     : out std_logic;
      txts_timestamp_o           : out std_logic_vector(31 downto 0);
      txts_timestamp_stb_o       : out std_logic;
      txts_timestamp_valid_o     : out std_logic;
      regs_i                     : in  t_ep_out_registers;
      regs_o                     : out t_ep_in_registers);
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

  signal txpcs_timestamp_trigger_p_a : std_logic;

  signal txts_timestamp_stb   : std_logic;
  signal txts_timestamp_valid : std_logic;
  signal txts_timestamp_value : std_logic_vector(31 downto 0);


  signal rxpcs_timestamp_stb         : std_logic;
  signal rxpcs_timestamp_trigger_p_a : std_logic;
  signal rxpcs_timestamp_valid       : std_logic;
  signal rxpcs_timestamp_value       : std_logic_vector(31 downto 0);


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
  signal regs_towb_rpath: t_ep_in_registers;


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

  signal wb_in  : t_wishbone_slave_in;
  signal wb_out : t_wishbone_slave_out;

  signal extended_ADDR : std_logic_vector(c_wishbone_address_width-1 downto 0);

  signal phase_meas    : std_logic_vector(31 downto 0);
  signal phase_meas_p  : std_logic;
  signal validity_cntr : unsigned(1 downto 0);
  signal r_dmcr_en     : std_logic;
  signal r_dmcr_n_avg  : std_logic_vector(11 downto 0);


  signal rtu_rq               : t_ep_internal_rtu_request;
  signal dvalid_tx, dvalid_rx : std_logic;

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

  U_PCS_1000BASEX : ep_1000basex_pcs
    generic map (
      g_simulation => g_simulation,
      g_16bit      => g_pcs_16bit)
    port map (
      rst_n_i   => rst_n_i,
      clk_sys_i => clk_sys_i,

      rxpcs_fab_o             => rxpcs_fab,
      rxpcs_busy_o            => rxpcs_busy,
      rxpcs_fifo_almostfull_i => rxpcs_fifo_almostfull,

      rxpcs_timestamp_trigger_p_a_o => rxpcs_timestamp_trigger_p_a,
      rxpcs_timestamp_i             => rxpcs_timestamp_value,
      rxpcs_timestamp_stb_i         => rxpcs_timestamp_stb,
      rxpcs_timestamp_valid_i       => rxpcs_timestamp_valid,

      txpcs_fab_i   => txpcs_fab,
      txpcs_busy_o  => txpcs_busy,
      txpcs_dreq_o  => txpcs_dreq,
      txpcs_error_o => txpcs_error,

      txpcs_timestamp_trigger_p_a_o => txpcs_timestamp_trigger_p_a,

      link_ok_o => link_ok,

      serdes_rst_o    => phy_rst_o,
      serdes_loopen_o => phy_loopen_o,
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
      serdes_rx_bitslide_i  => phy_rx_bitslide_i(4 downto 0),

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

--  txfra_enable <= link_ok and regs_fromwb.ecr_tx_en_o;

  U_Tx_Framer : ep_tx_framer
    generic map (
      g_with_vlans       => g_with_vlans,
      g_with_timestamper => g_with_timestamper,
      g_force_gap_length => g_tx_force_gap_length)
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
      regs_i           => regs_fromwb,

      txts_timestamp_i       => txts_timestamp_value,
      txts_timestamp_valid_i => txts_timestamp_valid,

      txtsu_port_id_o      => txtsu_port_id_o,
      txtsu_fid_o          => txtsu_frame_id_o,
      txtsu_ts_value_o     => txtsu_ts_value_o,
      txtsu_ts_incorrect_o => txtsu_ts_incorrect_o,
      txtsu_stb_o          => txtsu_stb_o,
      txtsu_ack_i          => txtsu_ack_i
      );


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
      g_with_rtu            => g_with_rtu,
      g_with_rx_buffer      => g_with_rx_buffer,
      g_rx_buffer_size      => g_rx_buffer_size)
    port map (
      clk_sys_i => clk_sys_i,
      clk_rx_i  => phy_rx_clk_i,

      rst_n_sys_i => rst_n_sys,
      rst_n_rx_i  => rst_n_rx,

      pcs_fab_i             => rxpcs_fab,
      pcs_fifo_almostfull_o => rxpcs_fifo_almostfull,
      pcs_busy_i            => rxpcs_busy,

      fc_pause_p_o     => rxfra_pause_p,
      fc_pause_delay_o => rxfra_pause_delay,

      rmon_o => rmon,
      regs_i => regs_fromwb,
      regs_o =>regs_towb_rpath,

      rtu_full_i     => rtu_full_i,
      rtu_rq_o       => rtu_rq,
      rtu_rq_valid_o => rtu_rq_strobe_p1_o,
      src_wb_o       => src_out,
      src_wb_i       => src_in
      );


  rtu_rq_smac_o     <= rtu_rq.smac;
  rtu_rq_dmac_o     <= rtu_rq.dmac;
  rtu_rq_vid_o      <= rtu_rq.vid;
  rtu_rq_prio_o     <= rtu_rq.prio;
  rtu_rq_has_vid_o  <= rtu_rq.has_vid;
  rtu_rq_has_prio_o <= rtu_rq.has_prio;

  src_dat_o    <= src_out.dat;
  src_adr_o    <= src_out.adr;
  src_sel_o    <= src_out.sel;
  src_cyc_o    <= src_out.cyc;
  src_stb_o    <= src_out.stb;
  src_we_o     <= src_out.we;
  src_in.stall <= src_stall_i;
  src_in.ack   <= src_ack_i;
  src_in.err   <= src_err_i;

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
      g_timestamp_bits_f => 4,
      g_ref_clock_rate   => f_pick_rate(g_pcs_16bit))
    port map (
      clk_ref_i      => clk_ref_i,
      clk_rx_i       => phy_rx_clk_i,
      clk_sys_i      => clk_sys_i,
      rst_n_rx_i     => rst_n_rx,
      rst_n_sys_i    => rst_n_sys,
      rst_n_ref_i    => rst_n_ref,
      pps_csync_p1_i => pps_csync_p1_i,
      pps_valid_i    => pps_valid_i,

      tx_timestamp_trigger_p_a_i => txpcs_timestamp_trigger_p_a,
      rx_timestamp_trigger_p_a_i => rxpcs_timestamp_trigger_p_a,

      rxts_timestamp_o       => rxpcs_timestamp_value,
      rxts_timestamp_valid_o => rxpcs_timestamp_valid,
      rxts_timestamp_stb_o   => rxpcs_timestamp_stb,

      txts_timestamp_o       => txts_timestamp_value,
      txts_timestamp_valid_o => txts_timestamp_valid,

      regs_i => regs_fromwb,
      regs_o => regs_towb_tsu);


-------------------------------------------------------------------------------
-- Wishbone controller & IO registers
-------------------------------------------------------------------------------

  extended_ADDR <= std_logic_vector(resize(unsigned(wb_adr_i), c_wishbone_address_width));

  U_Slave_adapter : wb_slave_adapter
    generic map (
      g_master_use_struct  => true,
      g_master_mode        => CLASSIC,
      g_master_granularity => WORD,
      g_slave_use_struct   => false,
      g_slave_mode         => g_interface_mode,
      g_slave_granularity  => g_address_granularity)
    port map (
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_sys,
      sl_adr_i   => extended_ADDR,
      sl_dat_i   => wb_dat_i,
      sl_sel_i   => wb_sel_i,
      sl_cyc_i   => wb_cyc_i,
      sl_stb_i   => wb_stb_i,
      sl_we_i    => wb_we_i,
      sl_dat_o   => wb_dat_o,
      sl_ack_o   => wb_ack_o,
      sl_stall_o => wb_stall_o,
      master_i   => wb_out,
      master_o   => wb_in);

  U_WB_SLAVE : ep_wishbone_controller
    port map (
      rst_n_i    => rst_n_sys,
      clk_sys_i  => clk_sys_i,
      wb_adr_i   => wb_in.adr(5 downto 0),
      wb_dat_i   => wb_in.dat,
      wb_dat_o   => wb_out.dat,
      wb_cyc_i   => wb_in.cyc,
      wb_sel_i   => wb_in.sel,
      wb_stb_i   => wb_in.stb,
      wb_we_i    => wb_in.we,
      wb_ack_o   => wb_out.ack,
      wb_stall_o => open,

      tx_clk_i => clk_ref_i,
      rx_clk_i => phy_rx_clk_i,

      ep_rmon_ram_wr_i   => ep_rmon_ram_wr,
      ep_rmon_ram_rd_i   => ep_rmon_ram_rd,
      ep_rmon_ram_data_i => ep_rmon_ram_data_i,
      ep_rmon_ram_data_o => ep_rmon_ram_data_o,
      ep_rmon_ram_addr_i => ep_rmon_ram_addr,

      regs_o => regs_fromwb,
      regs_i => regs_towb
      );     

  wb_out.stall <= '0';
  wb_out.rty   <= '0';
  wb_out.err   <= '0';
  wb_out.int   <= '0';

  regs_towb <= regs_towb_ep or regs_towb_tsu or regs_towb_rpath;


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


-------------------------------------------------------------------------------
-- DMTD phase meter
------------------------------------------------------------------------------  

  gen_with_dmtd : if(g_with_dmtd) generate
    U_DMTD : dmtd_phase_meas
      generic map (
        g_counter_bits         => 14,
        g_deglitcher_threshold => 1000)
      port map (
        clk_sys_i => clk_sys_i,

        clk_a_i    => phy_ref_clk_i,
        clk_b_i    => phy_rx_clk_i,
        clk_dmtd_i => clk_dmtd_i,
        rst_n_i    => rst_n_i,

        en_i           => r_dmcr_en,
        navg_i         => r_dmcr_n_avg,
        phase_meas_o   => phase_meas,
        phase_meas_p_o => phase_meas_p);



    regs_towb.dmcr_en_i    <= r_dmcr_en;
    regs_towb.dmcr_n_avg_i <= r_dmcr_n_avg;

    p_dmtd_update : process(clk_sys_i)
    begin
      if rising_edge(clk_sys_i) then
        if rst_n_i = '0' then
          validity_cntr              <= (others => '0');
          regs_towb_ep.dmsr_ps_rdy_i <= '0';
        else

          if(regs_fromwb.dmcr_en_load_o = '1') then
            r_dmcr_en    <= regs_fromwb.dmcr_en_o;
            r_dmcr_n_avg <= regs_fromwb.dmcr_n_avg_o;
          end if;

          if(r_dmcr_en = '0') then
            validity_cntr              <= (others => '0');
            regs_towb_ep.dmsr_ps_rdy_i <= '0';
          elsif(regs_fromwb.dmsr_ps_rdy_o = '1' and regs_fromwb.dmsr_ps_rdy_load_o = '1') then
            regs_towb_ep.dmsr_ps_rdy_i <= '0';
          elsif(phase_meas_p = '1') then

            if(validity_cntr = "11") then
              regs_towb_ep.dmsr_ps_rdy_i <= '1';
              regs_towb_ep.dmsr_ps_val_i <= phase_meas(23 downto 0);  -- discard few
            else
              regs_towb_ep.dmsr_ps_rdy_i <= '0';
              validity_cntr              <= validity_cntr + 1;
            end if;
          end if;
        end if;
      end if;
    end process;

  end generate gen_with_dmtd;

  dvalid_tx <= snk_cyc_i and snk_stb_i and link_ok;
  dvalid_rx <= src_out.cyc and src_out.stb and link_ok;

  gen_leds : if g_with_leds generate
    U_Led_Ctrl : ep_leds_controller
      generic map (
        g_blink_period_log2 => 20)
      port map (
        clk_sys_i   => clk_sys_i,
        rst_n_i     => rst_n_i,
        dvalid_tx_i => dvalid_tx,
        dvalid_rx_i => dvalid_rx,
        link_ok_i   => link_ok,
        led_link_o  => led_link_o,
        led_act_o   => led_act_o);
  end generate gen_leds;

end syn;


