-------------------------------------------------------------------------------
-- Title      : White Rabbit Softcore PLL (new generation) - SoftPLL-ng
-- Project    : White Rabbit
-------------------------------------------------------------------------------
-- File       : wr_softpll_ng.vhd
-- Author     : Tomasz Włostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2011-01-29
-- Last update: 2013-07-25
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: 
--
-- The hardware part of the revised softcore PLL. Incorporates a user-defined
-- number of DDMTD taggers, a FIFO allowing for sequential readout of
-- the phase tags and ports for driving oscillator tuning DACs.
-- The rest of the magic is done in the software.
-------------------------------------------------------------------------------
--
-- Copyright (c) 2012-2013 CERN
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

use work.gencores_pkg.all;
use work.wishbone_pkg.all;
use work.softpll_pkg.all;
use work.spll_wbgen2_pkg.all;

entity wr_softpll_ng is
  generic(
-- Number of bits in phase tags produced by DDMTDs.
-- Must be large enough to cover at least a hundred of DDMTD periods to ensure
-- correct operation of the SoftPLL software servo algorithm - that
-- means, for a typical DMTD frequency offset N=16384, there number of tag bits
-- should be log2(N) + 7 == 21. Note: the value must match the TAG_BITS constant
-- in spll_defs.h file!
    g_tag_bits : integer;

-- These two are obvious:
    g_num_ref_inputs : integer := 1;
    g_num_outputs    : integer := 1;

-- When true, an additional FIFO is instantiated, providing a realtime record
-- of user-selectable SoftPLL parameters (e.g. tag values, phase error, DAC drive).
-- These values can be read by "spll_dbg_proxy" daemon for further analysis.
    g_with_debug_fifo : boolean := false;

-- When true, an additional accumulating bang-bang phase detector is instantiated
-- for wideband locking of the local oscillator to an external stable reference
-- (e.g. GPSDO/Cesium 10 MHz)
    g_with_ext_clock_input : boolean := false;

-- When true, DDMTD inputs are reversed (so that the DDMTD offset clocks is
-- being sampled by the measured clock). This is functionally equivalent to
-- "direct" operation, but may improve FPGA timing/routability.
    g_reverse_dmtds : boolean := true;

-- Divides the DDMTD clock inputs by 2, removing the "CLOCK_DEDICATED_ROUTE"
-- errors under ISE tools, at the cost of bandwidth reduction. Advanced option
-- use with care.
    g_divide_input_by_2 : boolean := false;

-- Configuration of all output channels (phase detector type & dividers). See
-- softpll_pkg.vhd for details.
    g_channels_config : t_softpll_channel_config_array := c_softpll_default_channel_config;

    g_interface_mode      : t_wishbone_interface_mode      := PIPELINED;
    g_address_granularity : t_wishbone_address_granularity := WORD
    );

  port(
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

-- Reference inputs (i.e. the RX clocks recovered by the PHYs)
    clk_ref_i : in std_logic_vector(g_num_ref_inputs-1 downto 0);

-- Feedback clocks (i.e. the outputs of the main or auxillary oscillator)
-- Note: clk_fb_i(0) must be always connected to the primary board's oscillator
-- (i.e. the one driving the PTP and Ethernet PHY) to ensure correct operation
-- of the PTP core.
    clk_fb_i : in std_logic_vector(g_num_outputs-1 downto 0);

-- DMTD Offset clock
    clk_dmtd_i : in std_logic;

-- External reference clock (e.g. 10 MHz from Cesium/GPSDO). Used only if
-- g_with_ext_clock_input == true
    clk_ext_i : in std_logic;

-- External clock sync/alignment singnal. SoftPLL will align clk_ext_i/clk_fb_i(0)
-- to match the edges immediately following the rising edge in sync_p_i.
    sync_p_i : in std_logic;

-- DMTD oscillator drive
    dac_dmtd_data_o : out std_logic_vector(15 downto 0);
-- When HI, load the data from dac_dmtd_data_o to the DAC.
    dac_dmtd_load_o : out std_logic;

-- Output channel DAC value
    dac_out_data_o : out std_logic_vector(15 downto 0);
-- Output channel select (0 = Output channel 0, 1 == OC 1, etc...)
    dac_out_sel_o  : out std_logic_vector(3 downto 0);
    dac_out_load_o : out std_logic;

-- Output enable input: when HI, enables locking the output(s)
-- to the reference clock(s)
    out_enable_i : in  std_logic_vector(g_num_outputs-1 downto 0);
-- When HI, the respective clock output is locked.
    out_locked_o : out std_logic_vector(g_num_outputs-1 downto 0);

    wb_adr_i   : in  std_logic_vector(6 downto 0);
    wb_dat_i   : in  std_logic_vector(31 downto 0);
    wb_dat_o   : out std_logic_vector(31 downto 0);
    wb_cyc_i   : in  std_logic;
    wb_sel_i   : in  std_logic_vector(3 downto 0);
    wb_stb_i   : in  std_logic;
    wb_we_i    : in  std_logic;
    wb_ack_o   : out std_logic;
    wb_stall_o : out std_logic;
    wb_irq_o   : out std_logic;
    debug_o    : out std_logic_vector(3 downto 0);

-- Debug FIFO readout interrupt
    dbg_fifo_irq_o : out std_logic
    );

end wr_softpll_ng;

architecture rtl of wr_softpll_ng is

  constant c_log2_replication : integer := 2;
  constant c_use_multi_dmtd   : boolean := false;

  constant c_DBG_FIFO_THRESHOLD : integer := 8180;
  constant c_DBG_FIFO_COALESCE  : integer := 100;
  constant c_BB_ERROR_BITS      : integer := 16;

  component spll_bangbang_pd
    generic (
      g_error_bits : integer);
    port (
      clk_ref_i      : in  std_logic;
      clk_fb_i       : in  std_logic;
      clk_sys_i      : in  std_logic;
      rst_n_refclk_i : in  std_logic;
      rst_n_fbck_i   : in  std_logic;
      rst_n_sysclk_i : in  std_logic;
      cfg_div_ref_i  : in  std_logic_vector(5 downto 0);
      cfg_div_fb_i   : in  std_logic_vector(5 downto 0);
      cfg_gating_i   : in  std_logic_vector(3 downto 0);
      sync_p_i       : in  std_logic;
      sync_en_i      : in  std_logic;
      sync_done_o    : out std_logic;
      err_wrap_o     : out std_logic;
      err_o          : out std_logic_vector(g_error_bits-1 downto 0);
      err_stb_o      : out std_logic;
      ref_present_o  : out std_logic);
  end component;

  component dmtd_with_deglitcher
    generic (
      g_counter_bits      : natural;
      g_divide_input_by_2 : boolean);
    port (
      rst_n_dmtdclk_i      : in  std_logic;
      rst_n_sysclk_i       : in  std_logic;
      clk_in_i             : in  std_logic;
      clk_dmtd_i           : in  std_logic;
      clk_sys_i            : in  std_logic;
      resync_p_a_i         : in  std_logic := '0';
      resync_p_o           : out std_logic;
      resync_start_p_i     : in  std_logic;
      resync_done_o        : out std_logic;
      shift_en_i           : in  std_logic;
      shift_dir_i          : in  std_logic;
      clk_dmtd_en_i        : in  std_logic := '1';
      deglitch_threshold_i : in  std_logic_vector(15 downto 0);
      dbg_dmtdout_o        : out std_logic;
      tag_o                : out std_logic_vector(g_counter_bits-1 downto 0);
      tag_stb_p1_o         : out std_logic);
  end component;

  component spll_wb_slave
    generic (
      g_with_debug_fifo : integer);
    port (
      rst_n_i    : in  std_logic;
      clk_sys_i  : in  std_logic;
      wb_adr_i   : in  std_logic_vector(4 downto 0);
      wb_dat_i   : in  std_logic_vector(31 downto 0);
      wb_dat_o   : out std_logic_vector(31 downto 0);
      wb_cyc_i   : in  std_logic;
      wb_sel_i   : in  std_logic_vector(3 downto 0);
      wb_stb_i   : in  std_logic;
      wb_we_i    : in  std_logic;
      wb_ack_o   : out std_logic;
      wb_stall_o : out std_logic;
      wb_int_o   : out std_logic;
      irq_tag_i  : in  std_logic;
      regs_i     : in  t_spll_in_registers;
      regs_o     : out t_spll_out_registers);
  end component;

  procedure f_rr_arbitrate (
    signal req       : in  std_logic_vector;
    signal pre_grant : in  std_logic_vector;
    signal grant     : out std_logic_vector)is

    variable reqs  : std_logic_vector(req'length - 1 downto 0);
    variable gnts  : std_logic_vector(req'length - 1 downto 0);
    variable gnt   : std_logic_vector(req'length - 1 downto 0);
    variable gntM  : std_logic_vector(req'length - 1 downto 0);
    variable zeros : std_logic_vector(req'length - 1 downto 0);
    
  begin
    zeros := (others => '0');
    reqs  := req;
    -- bit twiddling magic :
    gnt   := reqs and std_logic_vector(unsigned(not reqs) + 1);
    reqs  := reqs and not (std_logic_vector(unsigned(pre_grant) - 1) or pre_grant);
    gnts  := reqs and std_logic_vector(unsigned(not reqs) + 1);

    if(reqs = zeros) then
      gntM := gnt;
    else
      gntM := gnts;
    end if;

    if((req and pre_grant) = zeros) then
      grant <= gntM;
    end if;
    
  end f_rr_arbitrate;

  function f_onehot_decode(x : std_logic_vector) return std_logic_vector is
  begin
    for j in 0 to x'left loop
      if x(j) /= '0' then
        return std_logic_vector(to_unsigned(j, 6));
      end if;
    end loop;  -- i
    return std_logic_vector(to_unsigned(0, 6));
  end f_onehot_decode;


  function f_num_total_channels
    return integer is
  begin
    if(g_with_ext_clock_input) then
      return g_num_ref_inputs + g_num_outputs + 1;
    else
      return g_num_ref_inputs + g_num_outputs;
    end if;
  end f_num_total_channels;

  function f_pick (
    cond     : boolean;
    if_true  : std_logic;
    if_false : std_logic
    ) return std_logic is
  begin
    if(cond) then
      return if_true;
    else
      return if_false;
    end if;
  end f_pick;

  function f_pick (
    cond     : boolean;
    if_true  : integer;
    if_false : integer
    ) return integer is
  begin
    if(cond) then
      return if_true;
    else
      return if_false;
    end if;
  end f_pick;

  function resize(x : std_logic_vector; new_length : integer) return std_logic_vector is
    variable tmp : std_logic_vector(new_length-1 downto 0);
  begin
    tmp                       := (others => '0');
    tmp (x'length-1 downto 0) := x;
    return tmp;
  end resize;

  type t_out_channel_bb_config is record
    div_ref : std_logic_vector(5 downto 0);
    div_fb  : std_logic_vector(5 downto 0);
    gating  : std_logic_vector(3 downto 0);
  end record;

  type t_tag_array is array (0 to f_num_total_channels-1) of std_logic_vector(g_tag_bits-1 downto 0);

  type t_phase_error_array is array(0 to g_num_outputs-1) of std_logic_vector(c_BB_ERROR_BITS-1 downto 0);

  type t_out_channel_bb_config_array is array (0 to g_num_outputs-1) of t_out_channel_bb_config;

  signal tags, tags_masked                          : t_tag_array;
  signal tags_grant_p, tags_p, tags_req, tags_grant : std_logic_vector(f_num_total_channels-1 downto 0);
  signal tag_muxed                                  : std_logic_vector(g_tag_bits-1 downto 0);
  signal tag_src, tag_src_pre                       : std_logic_vector (5 downto 0);
  signal tag_valid, tag_valid_pre                   : std_logic;

  signal rst_n_refclk  : std_logic;
  signal rst_n_extclk  : std_logic;
  signal rst_n_dmtdclk : std_logic;
  signal rst_n_rxclk   : std_logic_vector(g_num_ref_inputs-1 downto 0);
  signal rst_n_fb      : std_logic;

  signal deglitch_thr_slv : std_logic_vector(15 downto 0);

  signal irq_tag : std_logic;

  signal dmtd_freq_err       : std_logic_vector(11 downto 0);
  signal dmtd_freq_err_stb_p : std_logic;

  signal bb_phase_err                          : std_logic_vector(15 downto 0);
  signal bb_phase_err_stb_p, bb_phase_err_wrap : std_logic;

  signal rcer_int : std_logic_vector(g_num_ref_inputs-1 downto 0);
  signal ocer_int : std_logic_vector(g_num_outputs-1 downto 0);

  signal clk_ref_buf : std_logic;
  signal clk_rx_buf  : std_logic;

  signal wb_irq_out : std_logic;

  signal resized_addr : std_logic_vector(c_wishbone_address_width-1 downto 0);
  signal wb_out       : t_wishbone_slave_out;
  signal wb_in        : t_wishbone_slave_in;
  signal regs_in      : t_SPLL_out_registers;
  signal regs_out     : t_SPLL_in_registers;

  -- Debug FIFO signals
  signal dbg_fifo_almostfull   : std_logic;
  signal dbg_seq_id            : unsigned(15 downto 0);
  signal dbg_fifo_permit_write : std_logic;

  -- Temporary vectors for DDMTD clock selection (straight/reversed)
  signal dmtd_ref_clk_in, dmtd_ref_clk_dmtd : std_logic_vector(g_num_ref_inputs-1 downto 0);
  signal dmtd_fb_clk_in, dmtd_fb_clk_dmtd   : std_logic_vector(g_num_outputs-1 downto 0);

  signal bb_sync_en, bb_sync_done : std_logic;
  signal ext_ref_present          : std_logic;
  signal fb_resync_out            : std_logic_vector(g_num_outputs-1 downto 0);

  signal ref_resync_start_p : std_logic_vector(31 downto 0);
  signal fb_resync_start_p  : std_logic_vector(15 downto 0);

  signal rst_n_bb_ref : std_logic_vector(g_num_outputs-1 downto 0);
  signal rst_n_bb_fb  : std_logic_vector(g_num_outputs-1 downto 0);


  signal bb_chx_phase_err                              : t_phase_error_array;
  signal bb_chx_phase_err_wrap, bb_chx_phase_err_stb_p : std_logic_vector(g_num_outputs-1 downto 0);

  signal bb_config    : t_out_channel_bb_config_array;
  signal bb_det_reset : std_logic_vector(g_num_outputs-1 downto 0);
  
begin  -- rtl

  resized_addr(6 downto 0)                          <= wb_adr_i;
  resized_addr(c_wishbone_address_width-1 downto 7) <= (others => '0');

  U_Adapter : wb_slave_adapter
    generic map(
      g_master_use_struct  => true,
      g_master_mode        => CLASSIC,
      g_master_granularity => WORD,
      g_slave_use_struct   => false,
      g_slave_mode         => g_interface_mode,
      g_slave_granularity  => g_address_granularity)
    port map (
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_i,
      master_i   => wb_out,
      master_o   => wb_in,
      sl_adr_i   => resized_addr,
      sl_dat_i   => wb_dat_i,
      sl_sel_i   => wb_sel_i,
      sl_cyc_i   => wb_cyc_i,
      sl_stb_i   => wb_stb_i,
      sl_we_i    => wb_we_i,
      sl_dat_o   => wb_dat_o,
      sl_ack_o   => wb_ack_o,
      sl_stall_o => wb_stall_o);

  sync_ffs_rst2 : gc_sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => clk_dmtd_i,
      rst_n_i  => '1',
      data_i   => rst_n_i,
      synced_o => rst_n_dmtdclk,
      npulse_o => open,
      ppulse_o => open);



  gen_ref_dmtds : for i in 0 to g_num_ref_inputs-1 generate

    dmtd_ref_clk_in(i)   <= f_pick(g_reverse_dmtds, clk_dmtd_i, clk_ref_i(i));
    dmtd_ref_clk_dmtd(i) <= f_pick(g_reverse_dmtds, clk_ref_i(i), clk_dmtd_i);

    DMTD_REF : dmtd_with_deglitcher
      generic map (
        g_counter_bits      => g_tag_bits,
        g_divide_input_by_2 => g_divide_input_by_2)
      port map (
        rst_n_dmtdclk_i => rst_n_dmtdclk,
        rst_n_sysclk_i  => rst_n_i,

        clk_dmtd_i    => dmtd_ref_clk_dmtd(i),
        clk_dmtd_en_i => '1',

        clk_sys_i => clk_sys_i,
        clk_in_i  => dmtd_ref_clk_in(i),

        resync_done_o    => regs_out.crr_in_i(i),
        resync_start_p_i => ref_resync_start_p(i),
        resync_p_a_i     => fb_resync_out(0),
        resync_p_o       => open,

        tag_o                => tags(i),
        tag_stb_p1_o         => tags_p(i),
        shift_en_i           => '0',
        shift_dir_i          => '0',
        deglitch_threshold_i => deglitch_thr_slv,
        dbg_dmtdout_o        => open);


  end generate gen_ref_dmtds;

  gen_feedback_dmtds : for i in 0 to g_num_outputs-1 generate

    gen_output_pd_ddmtd : if(g_channels_config(i) = CH_DDMTD or i = 0) generate
      
      dmtd_fb_clk_in(i)   <= f_pick(g_reverse_dmtds, clk_dmtd_i, clk_fb_i(i));
      dmtd_fb_clk_dmtd(i) <= f_pick(g_reverse_dmtds, clk_fb_i(i), clk_dmtd_i);

      DMTD_FB : dmtd_with_deglitcher
        generic map (
          g_counter_bits      => g_tag_bits,
          g_divide_input_by_2 => g_divide_input_by_2)
        port map (
          rst_n_dmtdclk_i => rst_n_dmtdclk,
          rst_n_sysclk_i  => rst_n_i,
          clk_dmtd_i      => dmtd_fb_clk_dmtd(i),
          clk_dmtd_en_i   => '1',

          clk_sys_i => clk_sys_i,
          clk_in_i  => dmtd_fb_clk_in(i),

          resync_done_o    => regs_out.crr_out_i(i),
          resync_start_p_i => fb_resync_start_p(i),
          resync_p_a_i     => fb_resync_out(0),
          resync_p_o       => fb_resync_out(i),

          tag_o        => tags(i+g_num_ref_inputs),
          tag_stb_p1_o => tags_p(i+g_num_ref_inputs),
          shift_en_i   => '0',
          shift_dir_i  => '0',

          deglitch_threshold_i => deglitch_thr_slv,
          dbg_dmtdout_o        => open);

      regs_out.occr_out_det_type_i(i) <= '0';

    end generate gen_output_pd_ddmtd;


    gen_output_pd_bb : if (g_channels_config(i) = CH_BANGBANG and i /= 0) generate

      p_dividers_config : process(clk_sys_i)
      begin
        if rising_edge(clk_sys_i) then
          if(regs_in.aux_cr_aux_sel_o = std_logic_vector(to_unsigned(i, 3)) and regs_in.aux_cr_aux_sel_wr_o = '1') then
            bb_config(i).div_ref <= regs_in.aux_cr_div_ref_o;
            bb_config(i).div_fb  <= regs_in.aux_cr_div_fb_o;
            bb_config(i).gating  <= regs_in.aux_cr_gate_o;
          end if;
        end if;
      end process;

      bb_det_reset(i) <= rst_n_i and ocer_int(i);

      U_sync_rst_ref : gc_sync_ffs
        generic map (
          g_sync_edge => "positive")
        port map (
          clk_i    => clk_fb_i(0),  --ref_i(g_channels_config(i).ref_input),
          rst_n_i  => '1',
          data_i   => bb_det_reset(i),
          synced_o => rst_n_bb_ref(i));

      U_sync_rst_fb : gc_sync_ffs
        generic map (
          g_sync_edge => "positive")
        port map (
          clk_i    => clk_fb_i(i),
          rst_n_i  => '1',
          data_i   => bb_det_reset(i),
          synced_o => rst_n_bb_fb(i));

      U_BB_Detect : spll_bangbang_pd
        generic map (
          g_error_bits => c_BB_ERROR_BITS)
        port map (
-- note: bb detectors can be referenced only to the local 125 MHz oscillator
          clk_ref_i      => clk_fb_i(0),
          clk_fb_i       => clk_fb_i(i),
          clk_sys_i      => clk_sys_i,
          rst_n_refclk_i => rst_n_bb_ref(i),
          rst_n_fbck_i   => rst_n_bb_fb(i),
          rst_n_sysclk_i => rst_n_i,

          cfg_div_ref_i => bb_config(i).div_ref,
          cfg_div_fb_i  => bb_config(i).div_fb,
          cfg_gating_i  => bb_config(i).gating,

          sync_p_i      => sync_p_i,
          sync_en_i     => '1',
          sync_done_o   => open,
          err_o         => bb_chx_phase_err(i),
          err_wrap_o    => bb_chx_phase_err_wrap(i),
          err_stb_o     => bb_chx_phase_err_stb_p(i),
          ref_present_o => open);

      tags(i+g_num_ref_inputs)(c_BB_ERROR_BITS downto 0) <= bb_chx_phase_err_wrap(i) & bb_chx_phase_err(i);
      tags_p(i+g_num_ref_inputs)                         <= bb_chx_phase_err_stb_p(i);

      regs_out.occr_out_det_type_i(i) <= '1';
      
    end generate gen_output_pd_bb;
  end generate gen_feedback_dmtds;

  gen_with_ext_clock_input : if(g_with_ext_clock_input) generate
    
    U_sync_rst_ext : gc_sync_ffs
      generic map (
        g_sync_edge => "positive")
      port map (
        clk_i    => clk_ext_i,
        rst_n_i  => '1',
        data_i   => rst_n_i,
        synced_o => rst_n_extclk);

    U_sync_rst_fb0 : gc_sync_ffs
      generic map (
        g_sync_edge => "positive")
      port map (
        clk_i    => clk_fb_i(0),
        rst_n_i  => '1',
        data_i   => rst_n_i,
        synced_o => rst_n_fb);

    U_sync_ffs_sync_en : gc_sync_ffs
      generic map (
        g_sync_edge => "positive")
      port map (
        clk_i    => clk_ext_i,
        rst_n_i  => rst_n_i,
        data_i   => regs_in.eccr_align_en_o,
        synced_o => bb_sync_en);

    U_sync_ffs_sync_done : gc_sync_ffs
      generic map (
        g_sync_edge => "positive")
      port map (
        clk_i    => clk_sys_i,
        rst_n_i  => rst_n_i,
        data_i   => bb_sync_done,
        synced_o => regs_out.eccr_align_done_i);

    
    U_Ext_BB_Detect : spll_bangbang_pd
      generic map (
        g_error_bits => c_BB_ERROR_BITS)
      port map (
        clk_ref_i      => clk_ext_i,
        clk_fb_i       => clk_fb_i(0),
        clk_sys_i      => clk_sys_i,
        rst_n_refclk_i => rst_n_i,
        rst_n_fbck_i   => rst_n_fb,
        rst_n_sysclk_i => rst_n_i,
        cfg_div_ref_i  => std_logic_vector(to_unsigned(c_softpll_ext_div_ref, 6)),
        cfg_div_fb_i   => std_logic_vector(to_unsigned(c_softpll_ext_div_fb, 6)),
        cfg_gating_i   => std_logic_vector(to_unsigned(c_softpll_ext_log2_gating, 4)),
        sync_p_i       => sync_p_i,
        sync_en_i      => bb_sync_en,
        sync_done_o    => bb_sync_done,
        err_o          => bb_phase_err,
        err_wrap_o     => bb_phase_err_wrap,
        err_stb_o      => bb_phase_err_stb_p,
        ref_present_o  => ext_ref_present);

    tags(g_num_ref_inputs + g_num_outputs)(c_BB_ERROR_BITS-1 downto 0) <= bb_phase_err(c_BB_ERROR_BITS-1 downto 0);
    tags(g_num_ref_inputs + g_num_outputs)(c_BB_ERROR_BITS)            <= bb_phase_err_wrap;

    regs_out.eccr_ext_supported_i   <= '1';
    regs_out.eccr_ext_ref_present_i <= ext_ref_present;
  end generate gen_with_ext_clock_input;



  gen_without_ext_clock_input : if(not g_with_ext_clock_input) generate
    regs_out.eccr_ext_supported_i <= '0';
    bb_phase_err_stb_p            <= '0';
  end generate gen_without_ext_clock_input;


  U_WB_SLAVE : spll_wb_slave
    generic map (
      g_with_debug_fifo => f_pick(g_with_debug_fifo, 1, 0))
    port map (
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_i,
      wb_adr_i   => wb_in.adr(4 downto 0),
      wb_dat_i   => wb_in.dat,
      wb_dat_o   => wb_out.dat,
      wb_cyc_i   => wb_in.cyc,
      wb_sel_i   => wb_in.sel,
      wb_stb_i   => wb_in.stb,
      wb_we_i    => wb_in.we,
      wb_ack_o   => wb_out.ack,
      wb_int_o   => wb_irq_out,
      wb_stall_o => open,

      regs_o => regs_in,
      regs_i => regs_out,

      irq_tag_i => irq_tag);

  -- Counter resync logic
  process(regs_in)
  begin
    for i in 0 to g_num_outputs-1 loop
      fb_resync_start_p(i) <= regs_in.crr_out_load_o and regs_in.crr_out_o(i);
    end loop;
    for i in 0 to g_num_ref_inputs-1 loop
      ref_resync_start_p(i) <= regs_in.crr_in_load_o and regs_in.crr_in_o(i);
    end loop;  -- i
  end process;

  p_ocer_rcer_regs : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        ocer_int <= (others => '0');
        rcer_int <= (others => '0');
      else
        if(regs_in.ocer_load_o = '1') then
          ocer_int <= regs_in.ocer_o(g_num_outputs -1 downto 0);
        end if;

        if(regs_in.rcer_load_o = '1') then
          rcer_int <= regs_in.rcer_o(g_num_ref_inputs -1 downto 0);
        end if;
      end if;
    end if;
  end process;

  -- Drive back the respective registers
  regs_out.ocer_i(g_num_outputs-1 downto 0)    <= ocer_int;
  regs_out.rcer_i(g_num_ref_inputs-1 downto 0) <= rcer_int;

  p_latch_tags : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0') then
        tags_req   <= (others => '0');
        tags_grant <= (others => '0');
      else
        f_rr_arbitrate(tags_req, tags_grant, tags_grant);

        for i in 0 to g_num_ref_inputs-1 loop
          if(tags_p(i) = '1') then
            tags_req(i) <= rcer_int(i);
          elsif(tags_grant(i) = '1') then
            tags_req(i) <= '0';
          end if;
        end loop;  -- i

        for i in 0 to g_num_outputs-1 loop
          if(tags_p(i + g_num_ref_inputs) = '1') then
            tags_req(i + g_num_ref_inputs) <= ocer_int(i);
          elsif(tags_grant(i + g_num_ref_inputs) = '1') then
            tags_req(i + g_num_ref_inputs) <= '0';
          end if;
        end loop;  -- i

        if(bb_phase_err_stb_p = '1') then
          tags_req(f_num_total_channels-1) <= regs_in.eccr_ext_en_o;
        elsif(tags_grant(f_num_total_channels-1) = '1') then
          tags_req(f_num_total_channels-1) <= '0';
        end if;
        
      end if;
    end if;
  end process;

  tags_grant_p <= tags_req and tags_grant;

  p_mux_tags : process(clk_sys_i)
    variable muxed  : std_logic_vector(g_tag_bits-1 downto 0);
    variable src_id : std_logic_vector(5 downto 0);
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        tag_muxed     <= (others => '0');
        tag_src_pre   <= (others => '0');
        tag_src       <= (others => '0');
        tag_valid_pre <= '0';
        tag_valid     <= '0';
      else
        
        for i in 0 to f_num_total_channels-1 loop
          if(tags_grant_p(i) = '1') then
            tags_masked(i) <= tags(i);
          else
            tags_masked(i) <= (others => '0');
          end if;
        end loop;  -- i

        if(unsigned(tags_grant_p) /= 0) then
          tag_valid_pre <= '1';
        else
          tag_valid_pre <= '0';
        end if;

        tag_valid <= tag_valid_pre;

        tag_src_pre <= f_onehot_decode(tags_grant_p);
        tag_src     <= tag_src_pre;

        muxed := (others => '0');

        for i in 0 to f_num_total_channels-1 loop
          muxed := muxed or tags_masked(i);
        end loop;

        tag_muxed <= muxed;
        
      end if;
    end if;
  end process;

  regs_out.trr_wr_req_i                       <= tag_valid and not regs_in.trr_wr_full_o;
  regs_out.trr_value_i(g_tag_bits-1 downto 0) <= tag_muxed;
  regs_out.trr_chan_id_i                      <= '0'&tag_src;

  regs_out.occr_out_en_i(g_num_outputs-1 downto 0) <= out_enable_i;
  regs_out.occr_out_en_i(7 downto g_num_outputs)   <= (others => '0');

  out_locked_o <= regs_in.occr_out_lock_o(g_num_outputs-1 downto 0);

  irq_tag <= not regs_in.trr_wr_empty_o;

  deglitch_thr_slv <= regs_in.deglitch_thr_o;



  -----------------------------------------------------------------------------
  -- Debugging FIFO
  -----------------------------------------------------------------------------

  gen_with_debug_fifo : if(g_with_debug_fifo = true) generate
    
    dbg_fifo_almostfull <= '1' when unsigned(regs_in.dfr_host_wr_usedw_o) > 8180 else '0';

    p_request_counter : process(clk_sys_i)
    begin
      if rising_edge(clk_sys_i) then
        if rst_n_i = '0' then
          dbg_seq_id <= (others => '0');
        else
          if(regs_in.dfr_spll_eos_o = '1' and regs_in.dfr_spll_eos_wr_o = '1') then
            dbg_seq_id <= dbg_seq_id + 1;
          end if;
        end if;
      end if;
    end process;

    p_fifo_permit_write : process(clk_sys_i)
    begin
      if rising_edge(clk_sys_i) then
        if rst_n_i = '0' then
          dbg_fifo_permit_write <= '1';
        else
          if(dbg_fifo_almostfull = '0') then
            dbg_fifo_permit_write <= '1';
          elsif(regs_in.dfr_spll_eos_o = '1' and regs_in.dfr_spll_eos_wr_o = '1') then
            dbg_fifo_permit_write <= '0';
          end if;
        end if;
      end if;
    end process;

    p_coalesce_fifo_irq : process(clk_sys_i)
    begin
      if rising_edge(clk_sys_i) then
        if rst_n_i = '0' then
          dbg_fifo_irq_o <= '0';
        else
          if(unsigned(regs_in.dfr_host_wr_usedw_o) = 0) then
            dbg_fifo_irq_o <= '0';
          elsif(unsigned(regs_in.dfr_host_wr_usedw_o) = c_DBG_FIFO_COALESCE) then
            dbg_fifo_irq_o <= '1';
          end if;
        end if;
      end if;
    end process;

    regs_out.dfr_host_wr_req_i <= regs_in.dfr_spll_value_wr_o and dbg_fifo_permit_write;
    regs_out.dfr_host_value_i  <= regs_in.dfr_spll_eos_o & regs_in.dfr_spll_value_o;
    regs_out.dfr_host_seq_id_i <= std_logic_vector(dbg_seq_id);

  end generate gen_with_debug_fifo;

  gen_without_debug_fifo : if(g_with_debug_fifo = false) generate
    regs_out.dfr_host_wr_req_i <= '0';
  end generate gen_without_debug_fifo;

  -----------------------------------------------------------------------------
  -- CSR N_OUT/N_REF fields
  -----------------------------------------------------------------------------

  regs_out.csr_n_ref_i <= std_logic_vector(to_unsigned(g_num_ref_inputs, regs_out.csr_n_ref_i'length));
  regs_out.csr_n_out_i <= std_logic_vector(to_unsigned(g_num_outputs, regs_out.csr_n_out_i'length));

  dac_dmtd_load_o <= regs_in.dac_hpll_wr_o;
  dac_dmtd_data_o <= regs_in.dac_hpll_o;

  dac_out_data_o <= regs_in.dac_main_value_o;
  dac_out_sel_o  <= regs_in.dac_main_dac_sel_o;
  dac_out_load_o <= regs_in.dac_main_value_wr_o;

  wb_irq_o <= wb_irq_out;

  
end rtl;
