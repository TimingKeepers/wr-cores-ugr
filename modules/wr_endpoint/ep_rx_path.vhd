-------------------------------------------------------------------------------
-- Title      : 1000base-X MAC/Endpoint 
-- Project    : White Rabbit Switch 
-------------------------------------------------------------------------------
-- File       : ep_rx_deframer.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2009-06-22
-- Last update: 2011-10-06
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: RX deframing module:
-- - checks frame CRC
-- - inserts 802.1q headers when necessary
-- - parses packet headers and generates RTU requests
-- - embeds RX OOB block with timestamp information
-------------------------------------------------------------------------------
-- Copyright (c) 2009 Tomasz Wlostowski
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2009-06-22  0.1      twlostow  Created
------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.genram_pkg.all;
use work.endpoint_private_pkg.all;
use work.ep_wbgen2_pkg.all;
use work.wr_fabric_pkg.all;

entity ep_rx_path is
  generic (
    g_with_vlans          : boolean := true;
    g_with_dpi_classifier : boolean := true;
    g_with_rtu            : boolean := true);
  port (
    clk_sys_i   : in std_logic;
    clk_rx_i    : in std_logic;
    rst_n_sys_i : in std_logic;
    rst_n_rx_i  : in std_logic;

-- physical coding sublayer (PCS) interface
    pcs_fab_i             : in  t_ep_internal_fabric;
    pcs_fifo_almostfull_o : out std_logic;
    pcs_busy_i            : in  std_logic;

-- OOB frame tag value and strobing signal
    oob_data_i  : in  std_logic_vector(47 downto 0);
    oob_valid_i : in  std_logic;
    oob_ack_o   : out std_logic;

    src_wb_o : out t_wrf_source_out;
    src_wb_i : in  t_wrf_source_in;

-- flow control signals
    fc_pause_p_o           : out std_logic;
    fc_pause_delay_o       : out std_logic_vector(15 downto 0);
    fc_buffer_occupation_o : out std_logic_vector(7 downto 0);

-- RMON/statistic counters signals
    rmon_o : inout t_rmon_triggers;
    regs_i : in    t_ep_out_registers;
--    regs_o : out   t_ep_in_registers;


-------------------------------------------------------------------------------
-- RTU interface
-------------------------------------------------------------------------------

    rtu_rq_o       : out t_ep_internal_rtu_request;
    rtu_full_i     : in  std_logic;
    rtu_rq_valid_o : out std_logic
    );

end ep_rx_path;

architecture behavioral of ep_rx_path is

  component ep_rx_early_address_match
    generic (
      g_with_rtu : boolean);
    port (
      clk_sys_i            : in  std_logic;
      clk_rx_i             : in  std_logic;
      rst_n_sys_i          : in  std_logic;
      rst_n_rx_i           : in  std_logic;
      snk_fab_i            : in  t_ep_internal_fabric;
      src_fab_o            : out t_ep_internal_fabric;
      match_done_o         : out std_logic;
      match_is_hp_o        : out std_logic;
      match_is_pause_o     : out std_logic;
      match_pause_quanta_o : out std_logic_vector(15 downto 0);
      rtu_rq_o             : out t_ep_internal_rtu_request;
      regs_i               : in  t_ep_out_registers);
  end component;
  
  component ep_clock_alignment_fifo
    generic (
      g_size                 : integer;
      g_almostfull_threshold : integer;
      g_early_eof            : boolean);
    port (
      rst_n_rd_i       : in  std_logic;
      clk_wr_i         : in  std_logic;
      clk_rd_i         : in  std_logic;
      dreq_i           : in  std_logic;
      fab_i            : in  t_ep_internal_fabric;
      fab_o            : out t_ep_internal_fabric;
      full_o           : out std_logic;
      empty_o          : out std_logic;
      almostfull_o     : out std_logic;
      pass_threshold_i : in  std_logic_vector(f_log2_size(g_size)-1 downto 0));
  end component;

  component ep_packet_filter
    port (
      clk_rx_i    : in  std_logic;
      clk_sys_i   : in  std_logic;
      rst_n_rx_i  : in  std_logic;
      rst_n_sys_i : in  std_logic;
      snk_fab_i   : in  t_ep_internal_fabric;
      src_fab_o   : out t_ep_internal_fabric;
      done_o      : out std_logic;
      pclass_o    : out std_logic_vector(7 downto 0);
      drop_o      : out std_logic;
      regs_i      : in  t_ep_out_registers);
  end component;

  component ep_rx_vlan_unit
    port (
      clk_sys_i  : in    std_logic;
      rst_n_i    : in    std_logic;
      snk_fab_i  : in    t_ep_internal_fabric;
      snk_dreq_o : out   std_logic;
      src_fab_o  : out   t_ep_internal_fabric;
      src_dreq_i : in    std_logic;
      tclass_o   : out   std_logic_vector(2 downto 0);
      vid_o      : out   std_logic_vector(11 downto 0);
      tag_done_o : out   std_logic;
      rmon_o     : inout t_rmon_triggers;
      regs_i     : in    t_ep_out_registers);
  end component;

  component ep_rx_oob_insert
    port (
      clk_sys_i   : in  std_logic;
      rst_n_i     : in  std_logic;
      snk_fab_i   : in  t_ep_internal_fabric;
      snk_dreq_o  : out std_logic;
      src_fab_o   : out t_ep_internal_fabric;
      src_dreq_i  : in  std_logic;
      oob_data_i  : in  std_logic_vector(31 downto 0);
      oob_valid_i : in  std_logic;
      oob_ack_o   : out std_logic;
      regs_i      : in  t_ep_out_registers);
  end component;

  component ep_rx_crc_size_check
    port (
      clk_sys_i  : in    std_logic;
      rst_n_i    : in    std_logic;
      snk_fab_i  : in    t_ep_internal_fabric;
      snk_dreq_o : out   std_logic;
      src_fab_o  : out   t_ep_internal_fabric;
      src_dreq_i : in    std_logic;
      rmon_o     : inout t_rmon_triggers;
      regs_i     : in    t_ep_out_registers);
  end component;

  component ep_rx_wb_master
    port (
      clk_sys_i  : in  std_logic;
      rst_n_i    : in  std_logic;
      snk_fab_i  : in  t_ep_internal_fabric;
      snk_dreq_o : out std_logic;
      src_wb_i   : in  t_wrf_source_in;
      src_wb_o   : out t_wrf_source_out);
  end component;

  type t_rx_deframer_state is (RXF_IDLE, RXF_DATA, RXF_FLUSH_STALL, RXF_FINISH_CYCLE, RXF_THROW_ERROR);

  signal state : t_rx_deframer_state;

  signal gap_cntr : unsigned(3 downto 0);

  -- new sigs
  signal counter : unsigned(7 downto 0);

  signal rxdata_saved : std_logic_vector(15 downto 0);
  signal next_hdr     : std_logic;
  signal is_pause     : std_logic;

  signal data_firstword : std_logic;


  signal flush_stall : std_logic;
  signal stb_int     : std_logic;

  signal fab_int  : t_ep_internal_fabric;
  signal dreq_int : std_logic;

  signal ack_count   : unsigned(7 downto 0);
  signal src_out_int : t_wrf_source_out;

  signal tmp_sel : std_logic;
  signal tmp_dat : std_logic_vector(15 downto 0);


  type t_fab_pipe is array(integer range <>) of t_ep_internal_fabric;

  signal fab_pipe  : t_fab_pipe(0 to 5);
  signal dreq_pipe : std_logic_vector(5 downto 0);

  signal ematch_done         : std_logic;
  signal ematch_is_hp        : std_logic;
  signal ematch_is_pause     : std_logic;
  signal ematch_pause_quanta : std_logic_vector(15 downto 0);

  signal pfilter_pclass : std_logic_vector(7 downto 0);
  signal pfilter_drop   : std_logic;
  signal pfilter_done   : std_logic;

  signal vlan_tclass   : std_logic_vector(2 downto 0);
  signal vlan_vid      : std_logic_vector(11 downto 0);
  signal vlan_tag_done : std_logic;

  
begin  -- behavioral

  fab_pipe(0) <= pcs_fab_i;

  U_early_addr_match : ep_rx_early_address_match
    generic map (
      g_with_rtu => g_with_rtu)

    port map (
      clk_sys_i            => clk_sys_i,
      clk_rx_i             => clk_rx_i,
      rst_n_sys_i          => rst_n_sys_i,
      rst_n_rx_i           => rst_n_rx_i,
      snk_fab_i            => fab_pipe(0),
      src_fab_o            => fab_pipe(1),
      match_done_o         => ematch_done,
      match_is_hp_o        => ematch_is_hp,
      match_is_pause_o     => ematch_is_pause,
      match_pause_quanta_o => ematch_pause_quanta,
      regs_i               => regs_i);


  gen_with_packet_filter : if(g_with_dpi_classifier) generate
    U_packet_filter : ep_packet_filter
      port map (
        clk_sys_i   => clk_sys_i,
        clk_rx_i    => clk_rx_i,
        rst_n_sys_i => rst_n_sys_i,
        rst_n_rx_i  => rst_n_rx_i,

        snk_fab_i => fab_pipe(1),
        src_fab_o => fab_pipe(2),
        done_o    => pfilter_done,
        pclass_o  => pfilter_pclass,
        drop_o    => pfilter_drop,
        regs_i    => regs_i);
  end generate gen_with_packet_filter;

  gen_without_packet_filter : if(not g_with_dpi_classifier) generate
    fab_pipe(2)  <= fab_pipe(1);
  end generate gen_without_packet_filter;

  
  U_Rx_Clock_Align_FIFO : ep_clock_alignment_fifo
    generic map (
      g_size                 => 64,
      g_almostfull_threshold => 52,
      g_early_eof            => false)
    port map (
      rst_n_rd_i       => rst_n_sys_i,
      clk_wr_i         => clk_rx_i,
      clk_rd_i         => clk_sys_i,
      dreq_i           => dreq_pipe(3),
      fab_i            => fab_pipe(2),
      fab_o            => fab_pipe(3),
      full_o           => open,
      empty_o          => open,
      almostfull_o     => pcs_fifo_almostfull_o,
      pass_threshold_i => std_logic_vector(to_unsigned(16, 6)));

  

  U_crc_size_checker : ep_rx_crc_size_check
    port map (
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_sys_i,
      snk_fab_i  => fab_pipe(3),
      snk_dreq_o => dreq_pipe(3),
      src_dreq_i => dreq_pipe(4),
      src_fab_o  => fab_pipe(4),
      rmon_o     => rmon_o,
      regs_i     => regs_i);


  gen_with_vlan_unit : if(g_with_vlans) generate
    U_vlan_unit : ep_rx_vlan_unit
      port map (
        clk_sys_i  => clk_sys_i,
        rst_n_i    => rst_n_sys_i,
        snk_fab_i  => fab_pipe(4),
        snk_dreq_o => dreq_pipe(4),
        src_fab_o  => fab_pipe(5),
        src_dreq_i => dreq_pipe(5),
        tclass_o   => vlan_tclass,
        vid_o      => vlan_vid,
        tag_done_o => vlan_tag_done,
        rmon_o     => rmon_o,
        regs_i     => regs_i);
  end generate gen_with_vlan_unit;


  U_RX_Wishbone_Master : ep_rx_wb_master
    port map (
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_sys_i,
      snk_fab_i  => fab_pipe(5),
      snk_dreq_o => dreq_pipe(5),
      src_wb_i   => src_wb_i,
      src_wb_o   => src_wb_o);

end behavioral;

