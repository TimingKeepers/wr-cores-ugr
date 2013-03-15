-------------------------------------------------------------------------------
-- Title      : Private constants/types/functions package
-- Project    : White Rabbit MAC/Endpoint
-------------------------------------------------------------------------------
-- File       : endpoint_private_pkg.vhd
-- Author     : Tomasz Włostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2010-11-18
-- Last update: 2013-03-15
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Endpoint private definitions:
-- - 8B10B codes
-- - VLAN control registers
-- - Data types: internal fabric, RMON, RTU
-- - 18-bit FIFO fabric packing/unpacking functions
-- - Endpoint subcomponents declarations
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.ep_wbgen2_pkg.all;
use work.wr_fabric_pkg.all;

package endpoint_private_pkg is

  -- special/control characters
  constant c_k28_5 : std_logic_vector(7 downto 0) := "10111100";  -- bc
  constant c_k23_7 : std_logic_vector(7 downto 0) := "11110111";  -- f7
  constant c_k27_7 : std_logic_vector(7 downto 0) := "11111011";  -- fb
  constant c_k29_7 : std_logic_vector(7 downto 0) := "11111101";  -- fd
  constant c_k30_7 : std_logic_vector(7 downto 0) := "11111110";  -- fe
  constant c_k28_7 : std_logic_vector(7 downto 0) := "11111100";  -- fc
  constant c_d21_5 : std_logic_vector(7 downto 0) := "10110101";  -- b5

  constant c_d2_2          : std_logic_vector(7 downto 0) := "01000010";  -- 42
  constant c_d5_6          : std_logic_vector(7 downto 0) := "11000101";  -- c5
  constant c_d16_2         : std_logic_vector(7 downto 0) := "01010000";  -- 50
  constant c_preamble_char : std_logic_vector(7 downto 0) := "01010101";
  constant c_preamble_sfd  : std_logic_vector(7 downto 0) := "11010101";

  constant c_QMODE_PORT_ACCESS        : std_logic_vector(1 downto 0) := "00";
  constant c_QMODE_PORT_TRUNK         : std_logic_vector(1 downto 0) := "01";
  constant c_QMODE_PORT_UNQUALIFIED   : std_logic_vector(1 downto 0) := "11";
  constant c_QMODE_PORT_VLAN_DISABLED : std_logic_vector(1 downto 0) := "10";

  -- fixme: remove these along with the non-WB version of the endpoint
  constant c_wrsw_ctrl_none      : std_logic_vector(4 - 1 downto 0) := x"0";
  constant c_wrsw_ctrl_dst_mac   : std_logic_vector(4 - 1 downto 0) := x"1";
  constant c_wrsw_ctrl_src_mac   : std_logic_vector(4 - 1 downto 0) := x"2";
  constant c_wrsw_ctrl_ethertype : std_logic_vector(4 - 1 downto 0) := x"3";
  constant c_wrsw_ctrl_vid_prio  : std_logic_vector(4 - 1 downto 0) := x"4";
  constant c_wrsw_ctrl_tx_oob    : std_logic_vector(4 - 1 downto 0) := x"5";
  constant c_wrsw_ctrl_rx_oob    : std_logic_vector(4 - 1 downto 0) := x"6";
  constant c_wrsw_ctrl_payload   : std_logic_vector(4 - 1 downto 0) := x"7";
  constant c_wrsw_ctrl_fcs       : std_logic_vector(4 - 1 downto 0) := x"8";

  type t_ep_internal_rtu_request is record
    smac     : std_logic_vector(47 downto 0);
    dmac     : std_logic_vector(47 downto 0);
    vid      : std_logic_vector(11 downto 0);
    prio     : std_logic_vector(2 downto 0);
    has_vid  : std_logic;
    has_prio : std_logic;
    hash     : std_logic_vector(15 downto 0);
  end record;

  type t_rmon_triggers is record
    rx_sync_lost           : std_logic;
    rx_invalid_code        : std_logic;
    rx_overrun             : std_logic;
    rx_crc_err             : std_logic;
    rx_ok                  : std_logic;
    rx_pfilter_drop        : std_logic;
    rx_runt                : std_logic;
    rx_giant               : std_logic;
    rx_pause               : std_logic;
    rx_pcs_err             : std_logic;
    rx_buffer_overrun      : std_logic;
    rx_rtu_overrun         : std_logic;
    rx_path_timing_failure : std_logic;

    tx_pause    : std_logic;
    tx_underrun : std_logic;
  end record;

  -- Endpoint's internal fabric used to connect the submodules with each other.
  -- Easier to handle than pipelined Wishbone.
  type t_ep_internal_fabric is record
    sof                : std_logic;
    eof                : std_logic;
    error              : std_logic;
    dvalid             : std_logic;
    bytesel            : std_logic;
    has_rx_timestamp   : std_logic;
    rx_timestamp_valid : std_logic;
    data               : std_logic_vector(15 downto 0);
    addr               : std_logic_vector(1 downto 0);
  end record;

  component ep_1000basex_pcs
    generic (
      g_simulation : boolean);
    port (
      rst_n_i                 : in    std_logic;
      clk_sys_i               : in    std_logic;
      rxpcs_fab_o             : out   t_ep_internal_fabric;
      rxpcs_busy_o            : out   std_logic;
      rxpcs_dreq_i            : in    std_logic;
      rxpcs_timestamp_stb_p_o : out   std_logic;
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

  component ep_rmon_counters
    generic (
      g_num_counters   : integer;
      g_ram_addr_width : integer);
    port (
      clk_sys_i       : in  std_logic;
      rst_n_i         : in  std_logic;
      cntr_rst_i      : in  std_logic;
      cntr_pulse_i    : in  std_logic_vector(g_num_counters-1 downto 0);
      ram_addr_o      : out std_logic_vector(g_ram_addr_width-1 downto 0);
      ram_data_i      : in  std_logic_vector(31 downto 0);
      ram_data_o      : out std_logic_vector(31 downto 0);
      ram_wr_o        : out std_logic;
      cntr_overflow_o : out std_logic);
  end component;

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
      regs_i           : in  t_ep_out_registers;
      regs_o           : out t_ep_in_registers);
  end component;

  component ep_timestamping_unit
    generic (
      g_timestamp_bits_r : natural;
      g_timestamp_bits_f : natural;
      g_ref_clock_rate   : integer);
    port (
      clk_ref_i            : in  std_logic;
      clk_sys_i            : in  std_logic;
      rst_n_i              : in  std_logic;
      pps_csync_p1_i       : in  std_logic;
      tx_timestamp_stb_p_i : in  std_logic;
      rx_timestamp_stb_p_i : in  std_logic;
      txoob_fid_i          : in  std_logic_vector(16 - 1 downto 0);
      txoob_stb_p_i        : in  std_logic;
      rxoob_data_o         : out std_logic_vector(47 downto 0);
      rxoob_valid_o        : out std_logic;
      rxoob_ack_i          : in  std_logic;
      txtsu_port_id_o      : out std_logic_vector(4 downto 0);
      txtsu_fid_o          : out std_logic_vector(16 -1 downto 0);
      txtsu_tsval_o        : out std_logic_vector(28 + 4 - 1 downto 0);
      txtsu_valid_o        : out std_logic;
      txtsu_ack_i          : in  std_logic;
      ep_tscr_en_txts_i    : in  std_logic;
      ep_tscr_en_rxts_i    : in  std_logic;
      ep_tscr_cs_start_i   : in  std_logic;
      ep_tscr_cs_done_o    : out std_logic;
      ep_ecr_portid_i      : in  std_logic_vector(4 downto 0));
  end component;

  component ep_flow_control
    port (
      clk_sys_i          : in  std_logic;
      rst_n_i            : in  std_logic;
      rx_pause_p1_i      : in  std_logic;
      rx_pause_delay_i   : in  std_logic_vector(15 downto 0);
      tx_pause_o         : out std_logic;
      tx_pause_delay_o   : out std_logic_vector(15 downto 0);
      tx_pause_ack_i     : in  std_logic;
      tx_flow_enable_o   : out std_logic;
      rx_buffer_used_i   : in  std_logic_vector(7 downto 0);
      ep_fcr_txpause_i   : in  std_logic;
      ep_fcr_rxpause_i   : in  std_logic;
      ep_fcr_tx_thr_i    : in  std_logic_vector(7 downto 0);
      ep_fcr_tx_quanta_i : in  std_logic_vector(15 downto 0);
      rmon_rcvd_pause_o  : out std_logic;
      rmon_sent_pause_o  : out std_logic);
  end component;

  component ep_wishbone_controller
    port (
      rst_n_i            : in  std_logic;
      clk_sys_i          : in  std_logic;
      wb_adr_i           : in  std_logic_vector(5 downto 0);
      wb_dat_i           : in  std_logic_vector(31 downto 0);
      wb_dat_o           : out std_logic_vector(31 downto 0);
      wb_cyc_i           : in  std_logic;
      wb_sel_i           : in  std_logic_vector(3 downto 0);
      wb_stb_i           : in  std_logic;
      wb_we_i            : in  std_logic;
      wb_ack_o           : out std_logic;
      wb_stall_o         : out std_logic;
      tx_clk_i           : in  std_logic;
      rx_clk_i           : in  std_logic;
      ep_rmon_ram_addr_i : in  std_logic_vector(4 downto 0);
      ep_rmon_ram_data_o : out std_logic_vector(31 downto 0);
      ep_rmon_ram_rd_i   : in  std_logic;
      ep_rmon_ram_data_i : in  std_logic_vector(31 downto 0);
      ep_rmon_ram_wr_i   : in  std_logic;
      regs_o             : out t_ep_out_registers;
      regs_i             : in  t_ep_in_registers);
  end component;

  component ep_rx_bypass_queue
    generic (
      g_size  : integer;
      g_width : integer);
    port (
      rst_n_i : in  std_logic;
      clk_i   : in  std_logic;
      d_i     : in  std_logic_vector(g_width-1 downto 0);
      valid_i : in  std_logic;
      dreq_o  : out std_logic;
      q_o     : out std_logic_vector(g_width-1 downto 0);
      valid_o : out std_logic;
      dreq_i  : in  std_logic;
      flush_i : in  std_logic;
      purge_i : in  std_logic);
  end component;

  component ep_leds_controller
    generic (
      g_blink_period_log2 : integer);
    port (
      clk_sys_i   : in  std_logic;
      rst_n_i     : in  std_logic;
      dvalid_tx_i : in  std_logic;
      dvalid_rx_i : in  std_logic;
      link_ok_i   : in  std_logic;
      led_link_o  : out std_logic;
      led_act_o   : out std_logic);
  end component;

  procedure f_pack_fifo_contents (
    signal fab        : in  t_ep_internal_fabric;
    signal dout       : out std_logic_vector;
    signal dout_valid : out std_logic;
    early_eof         :     boolean := false);


  procedure f_unpack_fifo_contents (
    signal din       : in  std_logic_vector;
    signal din_valid : in  std_logic;
    signal fab       : out t_ep_internal_fabric;
    early_eof        :     boolean := false);

end endpoint_private_pkg;

-------------------------------------------------------------------------------

package body endpoint_private_pkg is

  procedure f_pack_fifo_contents
    (
      signal fab        : in  t_ep_internal_fabric;
      signal dout       : out std_logic_vector;
      signal dout_valid : out std_logic;
      early_eof         :     boolean := false) is
  begin
    -- the encodings are slightly different:
    -- - if early_eof == 1, the target needs the EOF information along with the last data word.
    --   This is the case for ep_tx_pcs.
    -- - if early_eof == 0, EOF is an independent transfer
    if(early_eof) then
      if(fab.sof = '1' or fab.error = '1') then
        -- tag = 01
        dout(17 downto 16) <= "01";
        dout(15)           <= fab.sof;
        dout(14)           <= 'X';
        dout(13)           <= fab.error;
        dout(12 downto 0)  <= (others => 'X');
        dout_valid         <= '1';
      elsif(fab.eof = '1') then
        -- tag = 1x
        dout(17)          <= '1';
        dout(16)          <= fab.bytesel;
        dout(15 downto 0) <= fab.data;
        dout_valid        <= '1';
      elsif(fab.dvalid = '1') then
        -- tag = 00
        dout(17)          <= '0';
        dout(16)          <= '0';
        dout(15 downto 0) <= fab.data;
        dout_valid        <= '1';
      else
        dout(17 downto 0) <= (others => 'X');
        dout_valid        <= '0';
      end if;
    else
      if(fab.sof = '1' or fab.error = '1' or fab.eof = '1' or fab.has_rx_timestamp = '1') then
        -- tag = 01
        dout(17)          <= 'X';
        dout(16)          <= '1';
        dout(15)          <= fab.sof;
        dout(14)          <= fab.eof;
        dout(13)          <= fab.error;
        dout(12)          <= fab.has_rx_timestamp;
        dout(11)          <= fab.rx_timestamp_valid;
        dout(10 downto 0) <= (others => 'X');
        dout_valid        <= '1';
      elsif(fab.dvalid = '1') then
        dout(17)          <= fab.bytesel;
        dout(16)          <= '0';
        dout(15 downto 0) <= fab.data;
        dout_valid        <= '1';
      else
        dout(17 downto 0) <= (others => 'X');
        dout_valid        <= '0';
      end if;

    end if;
  end f_pack_fifo_contents;

  procedure f_unpack_fifo_contents
    (
      signal din       : in  std_logic_vector;
      signal din_valid : in  std_logic;
      signal fab       : out t_ep_internal_fabric;
      early_eof        :     boolean := false) is
  begin

    fab.data <= din(15 downto 0);
    if(din_valid = '1') then
      if(early_eof) then
        fab.dvalid             <= not (not din(17) and din(16));
        fab.sof                <= not din(17) and din(16) and din(15);
        fab.eof                <= din(17);
        fab.error              <= not din(17) and din(16) and din(13);
        fab.has_rx_timestamp   <= '0';
        fab.rx_timestamp_valid <= '0';
        fab.bytesel            <= din(17) and din(16);

      else
        fab.dvalid             <= not din(16);
        fab.sof                <= din(16) and din(15);
        fab.eof                <= din(16) and din(14);
        fab.error              <= din(16) and din(13);
        fab.has_rx_timestamp   <= din(16) and din(12);
        fab.rx_timestamp_valid <= din(16) and din(11);
        fab.bytesel            <= (not din(16)) and din(17);
      end if;
    else
      fab.bytesel            <= 'X';
      fab.dvalid             <= '0';
      fab.sof                <= '0';
      fab.eof                <= '0';
      fab.error              <= '0';
      fab.has_rx_timestamp   <= '0';
      fab.rx_timestamp_valid <= '0';
      fab.data               <= (others => 'X');
    end if;
  end f_unpack_fifo_contents;




end endpoint_private_pkg;

-------------------------------------------------------------------------------
