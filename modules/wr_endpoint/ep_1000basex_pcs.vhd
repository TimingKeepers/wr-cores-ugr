-------------------------------------------------------------------------------
-- Title      : 1000Base-X Physical Coding Sublayer (PCS)
-- Project    : White Rabbit MAC/Endpoint
-------------------------------------------------------------------------------
-- File       : ep_1000basex_pcs.vhd
-- Author     : Tomasz Włostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2010-11-18
-- Last update: 2012-03-16
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Module implements the top level of a 1000Base-X compliant PCS
-- (Physical Coding Sublayer) with precise RX/TX timestamping. The PCS module
-- incorporates:
-- - configurable 8/16-bit RX/TX data paths,
-- - TX clock alignment FIFO,
-- - 802.3 autonegotiation.
-- - White Rabbit serdes-specific features (calibration patterns & bitslide)
-------------------------------------------------------------------------------
--
-- Copyright (c) 2009-2011 CERN / BE-CO-HT
--
-- This source file is free software; you can redistribute it   
-- and/or modify it under the terms of the GNU Lesser General   
-- Public License as published by the Free Software Foundation; 
-- either version 2.1 of the License, or (at your option) any   
-- later version.                                               
--
-- This source is distributed in the hope that it will be       
-- useful, but WITHOUT ANY WARRANTY; without even the implied   
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR      
-- PURPOSE.  See the GNU Lesser General Public License for more 
-- details.                                                     
--
-- You should have received a copy of the GNU Lesser General    
-- Public License along with this source; if not, download it   
-- from http://www.gnu.org/licenses/lgpl-2.1.html
--
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author    Description
-- 2010-11-18  0.4      twlostow  Created (separeted from wrsw_endpoint)
-- 2011-02-07  0.5      twlostow  Tested on Spartan6 GTP
-- 2011-10-18  0.6      twlostow  Virtex-6 GTX port
-- 2012-01-24  0.7      twlostow  Redone TX timestamping
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.endpoint_private_pkg.all;

entity ep_1000basex_pcs is

  generic (
    -- simulation mode: when true, all internal timeouts are reduced by few orders of
    -- magnitude to speed up simulations.
    g_simulation : boolean;
    -- PCS datapath width selection: true = 16-bit (Virtex-6), false = 8-bit
    -- (Spartan-6 or TBI).
    g_16bit      : boolean);

  port (

    ---------------------------------------------------------------------------
    -- System clock & reset
    ---------------------------------------------------------------------------

    rst_n_i   : in std_logic;
    clk_sys_i : in std_logic;

    ---------------------------------------------------------------------------
    -- PCS <-> MAC Interface
    ---------------------------------------------------------------------------

    -- Internal data output (incoming data).
    rxpcs_fab_o : out t_ep_internal_fabric;

    -- 1: RX FIFO is almost full (drop the packet).
    rxpcs_fifo_almostfull_i : in std_logic;

    -- 1: RX PCS is busy receiving a packet or waiting for its' timestamp.
    rxpcs_busy_o : out std_logic;

    -- 1-pulse: RX timestamp trigger (to timestamping unit).
    rxpcs_timestamp_trigger_p_a_o : out std_logic;

    -- RX timestamp value (4 falling : 28 rising edge bits).
    rxpcs_timestamp_i : in std_logic_vector(31 downto 0);

    rxpcs_timestamp_stb_i : in std_logic;

    -- 1: timestamp on rxpcs_timestamp_i is valid).
    rxpcs_timestamp_valid_i : in std_logic;

    -- Internal data input (data to be TXed).
    txpcs_fab_i : in t_ep_internal_fabric;

    -- 1: TX error occured.
    txpcs_error_o : out std_logic;

    -- 1: TX PCS is busy transmitting a packet.
    txpcs_busy_o : out std_logic;

    -- 1: TX PCS requests another transfer on txpcs_fab_i.
    txpcs_dreq_o : out std_logic;

    -- 1-pulse: TX timestamp trigger (to timestamping unit).
    txpcs_timestamp_trigger_p_a_o : out std_logic;

    link_ok_o : out std_logic;

    -----------------------------------------------------------------------------
    -- GTP/GTX/TBI Serdes interface
    ---------------------------------------------------------------------------

    -- 1: serdes is reset, 0: serdes is operating normally.
    serdes_rst_o : out std_logic;

    -- 1: serdes comma alignent is enabled.
    serdes_syncen_o : out std_logic;

    -- 1: serdes near-end PMA loopback is enabled.
    serdes_loopen_o : out std_logic;

    -- 1: serdes TX/RX is enabled.
    serdes_enable_o : out std_logic;


    ---------------------------------------------------------------------------
    -- Serdes TX path (all synchronous to serdes_tx_clk_i)
    ---------------------------------------------------------------------------

    -- Transmit path clock:
    -- 62.5 MHz in 16-bit mode, 125 MHz in 8-bit mode.
    serdes_tx_clk_i : in std_logic;

    -- TX Code group. In 16-bit mode, the MSB is TXed first (tx_data_o[15:8],
    -- then tx_data_o[7:0]). In 8-bit mode only bits [7:0] are used.
    serdes_tx_data_o : out std_logic_vector(15 downto 0);

    -- TX Control Code: When 1, a K-character is transmitted. In 16-bit mode,
    -- bit 1 goes first, in 8-bit mode only bit 0 is used.
    serdes_tx_k_o : out std_logic_vector(1 downto 0);

    -- TX Disparity input: 1 = last transmitted code group ended with negative
    -- running disparity, 0 = positive RD.
    serdes_tx_disparity_i : in std_logic;

    -- TX Encoding Error: 1 = PHY encountered a transmission error, drop the current
    -- packet.
    serdes_tx_enc_err_i : in std_logic;

    -------------------------------------------------------------------------------
    -- Serdes RX path (all synchronous to serdes_rx_clk_i)
    -------------------------------------------------------------------------------

    -- RX recovered clock. MUST be synchronous to incoming serial data stream
    -- for proper PTP/SyncE operation. 62.5 MHz in 16-bit mode, 125 MHz in 8-bit mode.
    serdes_rx_clk_i      : in std_logic;
    serdes_rx_data_i     : in std_logic_vector(15 downto 0);
    serdes_rx_k_i        : in std_logic_vector(1 downto 0);
    serdes_rx_enc_err_i  : in std_logic;
    serdes_rx_bitslide_i : in std_logic_vector(4 downto 0);

    -- RMON statistic counters
    rmon_o : inout t_rmon_triggers;

    --  MDIO interface

    mdio_addr_i  : in  std_logic_vector(15 downto 0);
    mdio_data_i  : in  std_logic_vector(15 downto 0);
    mdio_data_o  : out std_logic_vector(15 downto 0);
    mdio_stb_i   : in  std_logic;
    mdio_rw_i    : in  std_logic;
    mdio_ready_o : out std_logic
    );

end ep_1000basex_pcs;

architecture rtl of ep_1000basex_pcs is

  component ep_tx_pcs_8bit
    port (
      rst_n_i                 : in    std_logic;
      clk_sys_i               : in    std_logic;
      pcs_fab_i               : in    t_ep_internal_fabric;
      pcs_error_o             : out   std_logic;
      pcs_busy_o              : out   std_logic;
      pcs_dreq_o              : out   std_logic;
      mdio_mcr_pdown_i        : in    std_logic;
      mdio_wr_spec_tx_cal_i   : in    std_logic;
      an_tx_en_i              : in    std_logic;
      an_tx_val_i             : in    std_logic_vector(15 downto 0);
      timestamp_trigger_p_a_o : out   std_logic;
      rmon_o                  : inout t_rmon_triggers;
      phy_tx_clk_i            : in    std_logic;
      phy_tx_data_o           : out   std_logic_vector(7 downto 0);
      phy_tx_k_o              : out   std_logic;
      phy_tx_disparity_i      : in    std_logic;
      phy_tx_enc_err_i        : in    std_logic);
  end component;

  component ep_tx_pcs_16bit
    port (
      rst_n_i                 : in    std_logic;
      clk_sys_i               : in    std_logic;
      pcs_fab_i               : in    t_ep_internal_fabric;
      pcs_error_o             : out   std_logic;
      pcs_busy_o              : out   std_logic;
      pcs_dreq_o              : out   std_logic;
      mdio_mcr_pdown_i        : in    std_logic;
      mdio_wr_spec_tx_cal_i   : in    std_logic;
      an_tx_en_i              : in    std_logic;
      an_tx_val_i             : in    std_logic_vector(15 downto 0);
      timestamp_trigger_p_a_o : out   std_logic;
      rmon_o                  : inout t_rmon_triggers;
      phy_tx_clk_i            : in    std_logic;
      phy_tx_data_o           : out   std_logic_vector(15 downto 0);
      phy_tx_k_o              : out   std_logic_vector(1 downto 0);
      phy_tx_disparity_i      : in    std_logic;
      phy_tx_enc_err_i        : in    std_logic);
  end component;

  component ep_rx_pcs_8bit
    generic (
      g_simulation : boolean);
    port (
      clk_sys_i                  : in    std_logic;
      rst_n_i                    : in    std_logic;
      pcs_fifo_almostfull_i      : in    std_logic;
      pcs_busy_o                 : out   std_logic;
      pcs_fab_o                  : out   t_ep_internal_fabric;
      timestamp_trigger_p_a_o    : out   std_logic;  -- strobe for RX timestamping
      timestamp_i                : in    std_logic_vector(31 downto 0);
      timestamp_stb_i            : in    std_logic;
      timestamp_valid_i          : in    std_logic;
      phy_rx_clk_i               : in    std_logic;
      phy_rx_data_i              : in    std_logic_vector(7 downto 0);
      phy_rx_k_i                 : in    std_logic;
      phy_rx_enc_err_i           : in    std_logic;
      mdio_mcr_pdown_i           : in    std_logic;
      mdio_wr_spec_cal_crst_i    : in    std_logic;
      mdio_wr_spec_rx_cal_stat_o : out   std_logic;
      synced_o                   : out   std_logic;
      sync_lost_o                : out   std_logic;
      an_rx_en_i                 : in    std_logic;
      an_rx_val_o                : out   std_logic_vector(15 downto 0);
      an_rx_valid_o              : out   std_logic;
      an_idle_match_o            : out   std_logic;
      rmon_o                     : inout t_rmon_triggers);
  end component;

  component ep_rx_pcs_16bit
    generic (
      g_simulation : boolean);
    port (
      clk_sys_i                  : in    std_logic;
      rst_n_i                    : in    std_logic;
      pcs_fifo_almostfull_i      : in    std_logic;
      pcs_busy_o                 : out   std_logic;
      pcs_fab_o                  : out   t_ep_internal_fabric;
      timestamp_trigger_p_a_o    : out   std_logic;  -- strobe for RX timestamping
      timestamp_i                : in    std_logic_vector(31 downto 0);
      timestamp_stb_i            : in    std_logic;
      timestamp_valid_i          : in    std_logic;
      phy_rx_clk_i               : in    std_logic;
      phy_rx_data_i              : in    std_logic_vector(15 downto 0);
      phy_rx_k_i                 : in    std_logic_vector(1 downto 0);
      phy_rx_enc_err_i           : in    std_logic;
      mdio_mcr_pdown_i           : in    std_logic;
      mdio_wr_spec_cal_crst_i    : in    std_logic;
      mdio_wr_spec_rx_cal_stat_o : out   std_logic;
      synced_o                   : out   std_logic;
      sync_lost_o                : out   std_logic;
      an_rx_en_i                 : in    std_logic;
      an_rx_val_o                : out   std_logic_vector(15 downto 0);
      an_rx_valid_o              : out   std_logic;
      an_idle_match_o            : out   std_logic;
      rmon_o                     : inout t_rmon_triggers);
  end component;

  component ep_pcs_tbi_mdio_wb
    port (
      rst_n_i                    : in  std_logic;
      clk_sys_i                  : in  std_logic;
      wb_adr_i                   : in  std_logic_vector(4 downto 0);
      wb_dat_i                   : in  std_logic_vector(31 downto 0);
      wb_dat_o                   : out std_logic_vector(31 downto 0);
      wb_cyc_i                   : in  std_logic;
      wb_sel_i                   : in  std_logic_vector(3 downto 0);
      wb_stb_i                   : in  std_logic;
      wb_we_i                    : in  std_logic;
      wb_ack_o                   : out std_logic;
      wb_stall_o                 : out std_logic;
      tx_clk_i                   : in  std_logic;
      rx_clk_i                   : in  std_logic;
      mdio_mcr_uni_en_o          : out std_logic;
      mdio_mcr_anrestart_o       : out std_logic;
      mdio_mcr_pdown_o           : out std_logic;
      mdio_mcr_anenable_o        : out std_logic;
      mdio_mcr_loopback_o        : out std_logic;
      mdio_mcr_reset_o           : out std_logic;
      mdio_msr_lstatus_i         : in  std_logic;
      mdio_msr_rfault_i          : in  std_logic;
      mdio_msr_anegcomplete_i    : in  std_logic;
      mdio_advertise_pause_o     : out std_logic_vector(1 downto 0);
      mdio_advertise_rfault_o    : out std_logic_vector(1 downto 0);
      mdio_lpa_full_i            : in  std_logic;
      mdio_lpa_half_i            : in  std_logic;
      mdio_lpa_pause_i           : in  std_logic_vector(1 downto 0);
      mdio_lpa_rfault_i          : in  std_logic_vector(1 downto 0);
      mdio_lpa_lpack_i           : in  std_logic;
      mdio_lpa_npage_i           : in  std_logic;
      mdio_wr_spec_tx_cal_o      : out std_logic;
      mdio_wr_spec_rx_cal_stat_i : in  std_logic;
      mdio_wr_spec_cal_crst_o    : out std_logic;
      mdio_wr_spec_bslide_i      : in  std_logic_vector(4 downto 0);
      lstat_read_notify_o        : out std_logic);
  end component;



  component ep_autonegotiation
    generic (
      g_simulation : boolean);
    port (
      clk_sys_i               : in  std_logic;
      rst_n_i                 : in  std_logic;
      pcs_synced_i            : in  std_logic;
      pcs_los_i               : in  std_logic;
      pcs_link_ok_o           : out std_logic;
      an_idle_match_i         : in  std_logic;
      an_rx_en_o              : out std_logic;
      an_rx_val_i             : in  std_logic_vector(15 downto 0);
      an_rx_valid_i           : in  std_logic;
      an_tx_en_o              : out std_logic;
      an_tx_val_o             : out std_logic_vector(15 downto 0);
      mdio_mcr_anrestart_i    : in  std_logic;
      mdio_mcr_anenable_i     : in  std_logic;
      mdio_msr_anegcomplete_o : out std_logic;
      mdio_advertise_pause_i  : in  std_logic_vector(1 downto 0);
      mdio_advertise_rfault_i : in  std_logic_vector(1 downto 0);
      mdio_lpa_full_o         : out std_logic;
      mdio_lpa_half_o         : out std_logic;
      mdio_lpa_pause_o        : out std_logic_vector(1 downto 0);
      mdio_lpa_rfault_o       : out std_logic_vector(1 downto 0);
      mdio_lpa_lpack_o        : out std_logic;
      mdio_lpa_npage_o        : out std_logic);
  end component;

  signal mdio_mcr_uni_en          : std_logic;
  signal mdio_mcr_anrestart       : std_logic;
  signal mdio_mcr_pdown           : std_logic;
  signal mdio_mcr_anenable        : std_logic;
  signal mdio_mcr_loopback        : std_logic;
  signal mdio_mcr_reset           : std_logic;
  signal mdio_msr_lstatus         : std_logic;
  signal mdio_msr_rfault          : std_logic;
  signal mdio_msr_anegcomplete    : std_logic;
  signal mdio_advertise_pause     : std_logic_vector(1 downto 0);
  signal mdio_advertise_rfault    : std_logic_vector(1 downto 0);
  signal mdio_lpa_full            : std_logic;
  signal mdio_lpa_half            : std_logic;
  signal mdio_lpa_pause           : std_logic_vector(1 downto 0);
  signal mdio_lpa_rfault          : std_logic_vector(1 downto 0);
  signal mdio_lpa_lpack           : std_logic;
  signal mdio_lpa_npage           : std_logic;
  signal mdio_wr_spec_tx_cal      : std_logic;
  signal mdio_wr_spec_rx_cal_stat : std_logic;
  signal mdio_wr_spec_cal_crst    : std_logic;
  signal mdio_wr_spec_bslide      : std_logic_vector(4 downto 0);

  signal lstat_read_notify : std_logic;

-------------------------------------------------------------------------------
-- Autonegotiation signals
-------------------------------------------------------------------------------

  signal an_tx_en      : std_logic;
  signal an_rx_en      : std_logic;
  signal an_tx_val     : std_logic_vector(15 downto 0);
  signal an_rx_val     : std_logic_vector(15 downto 0);
  signal an_rx_valid   : std_logic;
  signal an_idle_match : std_logic;

  signal pcs_enable        : std_logic;
  signal synced, sync_lost : std_logic;

  signal txpcs_busy_int : std_logic;
  signal link_ok        : std_logic;

  signal pcs_reset_n : std_logic;

  signal wb_stb, wb_ack : std_logic;

  signal dummy : std_logic_vector(31 downto 0);

  signal tx_clk, rx_clk : std_logic;
  
begin  -- rtl

  pcs_reset_n <= '0' when (mdio_mcr_reset = '1' or rst_n_i = '0') else '1';

  gen_16bit : if(g_16bit) generate
    U_TX_PCS : ep_tx_pcs_16bit
      port map (
        rst_n_i   => pcs_reset_n,
        clk_sys_i => clk_sys_i,

        pcs_fab_i   => txpcs_fab_i,
        pcs_error_o => txpcs_error_o,
        pcs_busy_o  => txpcs_busy_int,
        pcs_dreq_o  => txpcs_dreq_o,

        mdio_mcr_pdown_i      => mdio_mcr_pdown,
        mdio_wr_spec_tx_cal_i => mdio_wr_spec_tx_cal,

        an_tx_en_i              => an_tx_en,
        an_tx_val_i             => an_tx_val,
        timestamp_trigger_p_a_o => txpcs_timestamp_trigger_p_a_o,
        rmon_o                  => rmon_o,

        phy_tx_clk_i       => serdes_tx_clk_i,
        phy_tx_data_o      => serdes_tx_data_o,
        phy_tx_k_o         => serdes_tx_k_o,
        phy_tx_disparity_i => serdes_tx_disparity_i,
        phy_tx_enc_err_i   => serdes_tx_enc_err_i
        );

    U_RX_PCS : ep_rx_pcs_16bit
      generic map (
        g_simulation => g_simulation)
      port map (
        clk_sys_i => clk_sys_i,
        rst_n_i   => pcs_reset_n,

        pcs_busy_o            => rxpcs_busy_o,
        pcs_fab_o             => rxpcs_fab_o,
        pcs_fifo_almostfull_i => rxpcs_fifo_almostfull_i,

        timestamp_trigger_p_a_o => rxpcs_timestamp_trigger_p_a_o,
        timestamp_i             => rxpcs_timestamp_i,
        timestamp_valid_i       => rxpcs_timestamp_valid_i,
        timestamp_stb_i         => rxpcs_timestamp_stb_i,

        mdio_mcr_pdown_i           => mdio_mcr_pdown,
        mdio_wr_spec_cal_crst_i    => mdio_wr_spec_cal_crst,
        mdio_wr_spec_rx_cal_stat_o => mdio_wr_spec_rx_cal_stat,

        synced_o        => synced,
        sync_lost_o     => sync_lost,
        an_rx_en_i      => an_rx_en,
        an_rx_val_o     => an_rx_val,
        an_rx_valid_o   => an_rx_valid,
        an_idle_match_o => an_idle_match,

        rmon_o => rmon_o,

        phy_rx_clk_i     => serdes_rx_clk_i,
        phy_rx_data_i    => serdes_rx_data_i,
        phy_rx_k_i       => serdes_rx_k_i,
        phy_rx_enc_err_i => serdes_rx_enc_err_i
        );

    
  end generate gen_16bit;

  gen_8bit : if(not g_16bit) generate
    U_TX_PCS : ep_tx_pcs_8bit
      port map (
        rst_n_i   => pcs_reset_n,
        clk_sys_i => clk_sys_i,

        pcs_fab_i   => txpcs_fab_i,
        pcs_error_o => txpcs_error_o,
        pcs_busy_o  => txpcs_busy_int,
        pcs_dreq_o  => txpcs_dreq_o,

        mdio_mcr_pdown_i      => mdio_mcr_pdown,
        mdio_wr_spec_tx_cal_i => mdio_wr_spec_tx_cal,

        an_tx_en_i              => an_tx_en,
        an_tx_val_i             => an_tx_val,
        timestamp_trigger_p_a_o => txpcs_timestamp_trigger_p_a_o,
        rmon_o                  => rmon_o,

        phy_tx_clk_i       => serdes_tx_clk_i,
        phy_tx_data_o      => serdes_tx_data_o(7 downto 0),
        phy_tx_k_o         => serdes_tx_k_o(0),
        phy_tx_disparity_i => serdes_tx_disparity_i,
        phy_tx_enc_err_i   => serdes_tx_enc_err_i
        );

    

    U_RX_PCS : ep_rx_pcs_8bit
      generic map (
        g_simulation => g_simulation)
      port map (
        clk_sys_i => clk_sys_i,
        rst_n_i   => pcs_reset_n,

        pcs_busy_o            => rxpcs_busy_o,
        pcs_fab_o             => rxpcs_fab_o,
        pcs_fifo_almostfull_i => rxpcs_fifo_almostfull_i,

        timestamp_trigger_p_a_o => rxpcs_timestamp_trigger_p_a_o,
        timestamp_i             => rxpcs_timestamp_i,
        timestamp_valid_i       => rxpcs_timestamp_valid_i,
        timestamp_stb_i         => rxpcs_timestamp_stb_i,

        mdio_mcr_pdown_i           => mdio_mcr_pdown,
        mdio_wr_spec_cal_crst_i    => mdio_wr_spec_cal_crst,
        mdio_wr_spec_rx_cal_stat_o => mdio_wr_spec_rx_cal_stat,

        synced_o        => synced,
        sync_lost_o     => sync_lost,
        an_rx_en_i      => an_rx_en,
        an_rx_val_o     => an_rx_val,
        an_rx_valid_o   => an_rx_valid,
        an_idle_match_o => an_idle_match,

        rmon_o => rmon_o,

        phy_rx_clk_i     => serdes_rx_clk_i,
        phy_rx_data_i    => serdes_rx_data_i(7 downto 0),
        phy_rx_k_i       => serdes_rx_k_i(0),
        phy_rx_enc_err_i => serdes_rx_enc_err_i
        );

  end generate gen_8bit;

  txpcs_busy_o <= txpcs_busy_int;

  serdes_rst_o        <= (not pcs_reset_n) or mdio_mcr_pdown;
  mdio_wr_spec_bslide <= serdes_rx_bitslide_i(4 downto 0);

  U_MDIO_WB : ep_pcs_tbi_mdio_wb
    port map (
      rst_n_i                 => rst_n_i,
      clk_sys_i               => clk_sys_i,
      wb_adr_i                => mdio_addr_i(4 downto 0),
      wb_dat_i(15 downto 0)   => mdio_data_i,
      wb_dat_i(31 downto 16)  => x"0000",
      wb_dat_o(15 downto 0)   => mdio_data_o,
      wb_dat_o(31 downto 16)  => dummy(31 downto 16),

      wb_cyc_i   => wb_stb,
      wb_sel_i   => "1111",
      wb_stb_i   => wb_stb,
      wb_we_i    => mdio_rw_i,
      wb_ack_o   => wb_ack,
      wb_stall_o => open,
      tx_clk_i   => serdes_tx_clk_i,
      rx_clk_i   => serdes_rx_clk_i,

      mdio_mcr_uni_en_o          => mdio_mcr_uni_en,
      mdio_mcr_anrestart_o       => mdio_mcr_anrestart,
      mdio_mcr_pdown_o           => mdio_mcr_pdown,
      mdio_mcr_anenable_o        => mdio_mcr_anenable,
      mdio_mcr_loopback_o        => mdio_mcr_loopback,
      mdio_mcr_reset_o           => mdio_mcr_reset,
      mdio_msr_lstatus_i         => mdio_msr_lstatus,
      mdio_msr_rfault_i          => mdio_msr_rfault,
      mdio_msr_anegcomplete_i    => mdio_msr_anegcomplete,
      mdio_advertise_pause_o     => mdio_advertise_pause,
      mdio_advertise_rfault_o    => mdio_advertise_rfault,
      mdio_lpa_full_i            => mdio_lpa_full,
      mdio_lpa_half_i            => mdio_lpa_half,
      mdio_lpa_pause_i           => mdio_lpa_pause,
      mdio_lpa_rfault_i          => mdio_lpa_rfault,
      mdio_lpa_lpack_i           => mdio_lpa_lpack,
      mdio_lpa_npage_i           => mdio_lpa_npage,
      mdio_wr_spec_tx_cal_o      => mdio_wr_spec_tx_cal,
      mdio_wr_spec_rx_cal_stat_i => mdio_wr_spec_rx_cal_stat,
      mdio_wr_spec_cal_crst_o    => mdio_wr_spec_cal_crst,
      mdio_wr_spec_bslide_i      => mdio_wr_spec_bslide,

      lstat_read_notify_o => lstat_read_notify
      );


  mdio_msr_rfault <= '0';

  U_AUTONEGOTIATION : ep_autonegotiation
    generic map (
      g_simulation => g_simulation)

    port map (
      clk_sys_i => clk_sys_i,
      rst_n_i   => pcs_reset_n,

      pcs_synced_i  => synced,
      pcs_los_i     => sync_lost,
      pcs_link_ok_o => link_ok,

      an_idle_match_i         => an_idle_match,
      an_rx_en_o              => an_rx_en,
      an_rx_val_i             => an_rx_val,
      an_rx_valid_i           => an_rx_valid,
      an_tx_en_o              => an_tx_en,
      an_tx_val_o             => an_tx_val,
      mdio_mcr_anrestart_i    => mdio_mcr_anrestart,
      mdio_mcr_anenable_i     => mdio_mcr_anenable,
      mdio_msr_anegcomplete_o => mdio_msr_anegcomplete,
      mdio_advertise_pause_i  => mdio_advertise_pause,
      mdio_advertise_rfault_i => mdio_advertise_rfault,
      mdio_lpa_full_o         => mdio_lpa_full,
      mdio_lpa_half_o         => mdio_lpa_half,
      mdio_lpa_pause_o        => mdio_lpa_pause,
      mdio_lpa_rfault_o       => mdio_lpa_rfault,
      mdio_lpa_lpack_o        => mdio_lpa_lpack,
      mdio_lpa_npage_o        => mdio_lpa_npage
      );

  -- process: translates the MDIO reads/writes into Wishbone read/writes
  -- inputs: mdio_stb_i, wb_ack
  -- ouputs: mdio_ready_o, wb_stb
  p_translate_mdio_wb : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if (rst_n_i = '0') then
        wb_stb       <= '0';
        mdio_ready_o <= '1';
      else
        if(mdio_stb_i = '1' and wb_stb = '0') then
          wb_stb       <= '1';
          mdio_ready_o <= '0';
        elsif(wb_stb = '1' and wb_ack = '1') then
          mdio_ready_o <= '1';
          wb_stb       <= '0';
        end if;
      end if;
    end if;
  end process;

  -- process: handles the LSTATUS bit in MSR register
  -- inputs: sync_lost, synced, lstat_read_notify
  -- outputs: mdio_msr_lstatus
  p_gen_link_status : process(clk_sys_i, pcs_reset_n)
  begin
    if rising_edge(clk_sys_i) then
      if(pcs_reset_n = '0') then
        mdio_msr_lstatus <= '0';
      else
        if(sync_lost = '1') then
          mdio_msr_lstatus <= '0';
        elsif(lstat_read_notify = '1') then
          mdio_msr_lstatus <= synced and link_ok;
        end if;
      end if;
    end if;
  end process;

  link_ok_o <= link_ok and synced;
end rtl;
