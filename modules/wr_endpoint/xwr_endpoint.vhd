-------------------------------------------------------------------------------
-- Title      : 1000base-X MAC/Endpoint
-- Project    : White Rabbit
-------------------------------------------------------------------------------
-- File       : xwr_endpoint.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-Co-HT
-- Created    : 2010-04-26
-- Last update: 2011-10-18
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description: Wrapper for wr_endpoint using strutures in ports.
-------------------------------------------------------------------------------
-- Copyright (c) 2011 Tomasz Wlostowski
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;

use work.endpoint_pkg.all;
use work.wr_fabric_pkg.all;
use work.wishbone_pkg.all;

entity xwr_endpoint is
  
  generic (
    g_interface_mode      : t_wishbone_interface_mode      := CLASSIC;
    g_address_granularity : t_wishbone_address_granularity := WORD;
    g_simulation          : boolean := false;
    g_pcs_16bit           : boolean := false;
    g_rx_buffer_size      : integer := 1024;
    g_with_rx_buffer      : boolean := true;
    g_with_flow_control   : boolean := true;
    g_with_timestamper    : boolean := true;
    g_with_dpi_classifier : boolean := true;
    g_with_vlans          : boolean := true;
    g_with_rtu            : boolean := true;
    g_with_leds           : boolean := true
    );
  port (

-------------------------------------------------------------------------------
-- Clocks
-------------------------------------------------------------------------------

-- Endpoint transmit reference clock. Must be 125 MHz +- 100 ppm
    clk_ref_i : in std_logic;

-- reference clock / 2 (62.5 MHz, in-phase with refclk)
    clk_sys_i : in std_logic;

-- sync reset (clk_sys_i domain), active LO
    rst_n_i : in std_logic;

-- PPS input (1 clk_ref_i cycle HI) for synchronizing timestamp counter
    pps_csync_p1_i : in std_logic := '0';

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

    gmii_tx_clk_i : in  std_logic := '0';
    gmii_txd_o    : out std_logic_vector(7 downto 0);
    gmii_tx_en_o  : out std_logic;
    gmii_tx_er_o  : out std_logic;

    gmii_rx_clk_i : in std_logic := '0';
    gmii_rxd_i    : in std_logic_vector(7 downto 0) := x"00";
    gmii_rx_er_i  : in std_logic := '0';
    gmii_rx_dv_i  : in std_logic := '0';

    ---------------------------------------------------------------------------
    -- Wishbone I/O
    ---------------------------------------------------------------------------

    src_o: out t_wrf_source_out;
    src_i: in t_wrf_source_in;

    snk_o: out t_wrf_sink_out;
    snk_i: in t_wrf_sink_in;
    
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
    txtsu_ack_i : in std_logic := '1';

-------------------------------------------------------------------------------
-- RTU interface
-------------------------------------------------------------------------------

-- 1 indicates that coresponding RTU port is full.
    rtu_full_i : in std_logic:='0';

-- 1 indicates that coresponding RTU port is almost full.
    rtu_almost_full_i : in std_logic:='0';

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

  wb_i: in t_wishbone_slave_in;
  wb_o: out t_wishbone_slave_out;
  
-------------------------------------------------------------------------------
-- Misc stuff
-------------------------------------------------------------------------------

  led_link_o: out std_logic;
  led_act_o: out std_logic

    );

end xwr_endpoint;

architecture syn of xwr_endpoint is
  
begin

  U_Wrapped_Endpoint: wr_endpoint
    generic map (
      g_interface_mode => g_interface_mode,
      g_address_granularity => g_address_granularity,
      
      g_simulation          => g_simulation,
      g_pcs_16bit           => g_pcs_16bit,
      g_rx_buffer_size      => g_rx_buffer_size,
      g_with_rx_buffer      => g_with_rx_buffer,
      g_with_flow_control   => g_with_flow_control,
      g_with_timestamper    => g_with_timestamper,
      g_with_dpi_classifier => g_with_dpi_classifier,
      g_with_vlans          => g_with_vlans,
      g_with_rtu            => g_with_rtu,
      g_with_leds           => g_with_leds)
    port map (
      clk_ref_i          => clk_ref_i,
      clk_sys_i          => clk_sys_i,
      rst_n_i            => rst_n_i,
      pps_csync_p1_i     => pps_csync_p1_i,
      phy_rst_o          => phy_rst_o,
      phy_loopen_o       => phy_loopen_o,
      phy_enable_o       => phy_enable_o,
      phy_syncen_o       => phy_syncen_o,
      phy_ref_clk_i      => phy_ref_clk_i,
      phy_tx_data_o      => phy_tx_data_o,
      phy_tx_k_o         => phy_tx_k_o,
      phy_tx_disparity_i => phy_tx_disparity_i,
      phy_tx_enc_err_i   => phy_tx_enc_err_i,
      phy_rx_data_i      => phy_rx_data_i,
      phy_rx_clk_i       => phy_rx_clk_i,
      phy_rx_k_i         => phy_rx_k_i,
      phy_rx_enc_err_i   => phy_rx_enc_err_i,
      phy_rx_bitslide_i  => phy_rx_bitslide_i,
      gmii_tx_clk_i      => gmii_tx_clk_i,
      gmii_txd_o         => gmii_txd_o,
      gmii_tx_en_o       => gmii_tx_en_o,
      gmii_tx_er_o       => gmii_tx_er_o,
      gmii_rx_clk_i      => gmii_rx_clk_i,
      gmii_rxd_i         => gmii_rxd_i,
      gmii_rx_er_i       => gmii_rx_er_i,
      gmii_rx_dv_i       => gmii_rx_dv_i,
      src_dat_o          => src_o.dat,
      src_adr_o          => src_o.adr,
      src_sel_o          => src_o.sel,
      src_cyc_o          => src_o.cyc,
      src_stb_o          => src_o.stb,
      src_we_o           => src_o.we,
      src_stall_i        => src_i.stall,
      src_ack_i          => src_i.ack,
      snk_dat_i          => snk_i.dat,
      snk_adr_i          => snk_i.adr,
      snk_sel_i          => snk_i.sel,
      snk_cyc_i          => snk_i.cyc,
      snk_stb_i          => snk_i.stb,
      snk_we_i           => snk_i.we,
      snk_stall_o        => snk_o.stall,
      snk_ack_o          => snk_o.ack,
      snk_err_o          => snk_o.err,
      snk_rty_o          => snk_o.rty,
      txtsu_port_id_o    => txtsu_port_id_o,
      txtsu_frame_id_o   => txtsu_frame_id_o,
      txtsu_tsval_o      => txtsu_tsval_o,
      txtsu_valid_o      => txtsu_valid_o,
      txtsu_ack_i        => txtsu_ack_i,
      rtu_full_i         => rtu_full_i,
      rtu_almost_full_i  => rtu_almost_full_i,
      rtu_rq_strobe_p1_o => rtu_rq_strobe_p1_o,
      rtu_rq_smac_o      => rtu_rq_smac_o,
      rtu_rq_dmac_o      => rtu_rq_dmac_o,
      rtu_rq_vid_o       => rtu_rq_vid_o,
      rtu_rq_has_vid_o   => rtu_rq_has_vid_o,
      rtu_rq_prio_o      => rtu_rq_prio_o,
      rtu_rq_has_prio_o  => rtu_rq_has_prio_o,
      wb_cyc_i           => wb_i.cyc,
      wb_stb_i           => wb_i.stb,
      wb_we_i            => wb_i.we,
      wb_sel_i           => wb_i.sel,
      wb_adr_i           => wb_i.adr(7 downto 0),
      wb_dat_i           => wb_i.dat,
      wb_dat_o           => wb_o.dat,
      wb_ack_o           => wb_o.ack,
      led_link_o         => led_link_o,
      led_act_o          => led_act_o);
  
end syn;


