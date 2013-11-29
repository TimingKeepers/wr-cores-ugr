--! @file eca.vhd
--! @brief Event-Condition-Action Unit
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This entity ties all the ECA components together under Wishbone control.
--! The register layout is as follows:
--!
--! Wishbone registers (all 4-byte values):
--!
--! 0x00 RW: ECA params
--!  0   R : log(table size)
--!  1   R : log(queue depth)
--!  2   R : number of channels
--!  3   RW: index of ECA
--! 0x04 RW: ECA Control
--!  0   R : Feature bits; 1=inspect_table, 2=inspect_queue
--!  1   R : ASCII ECA Name
--!  2    W: Clear control bits
--!  3   RW: Set   control bits; 1=disable, 2=interrupt enable, 4=flip(toggle only)
--! 0x08 R : Time1
--! 0x0C R : Time0
--! 0x10 RW: Search index
--! 0x14 RW:  First
--! 0x18 RW:  Event1
--! 0x1C RW:  Event0
--! 0x20 RW: Walk index
--! 0x24 RW:  Next
--! 0x28 RW:  Offset1
--! 0x2C RW:  Offset0
--! 0x30 RW:  Tag
--! 0x34 RW:  Channel
--! 0x38 R : Frequency numerator
--! 0x3C R : Frequency coefficients
--!  0   R : powers of 5
--!  1   R : powers of 2
--!  2-3 R : Frequency divisor
--!
--! 0x40 RW:  Channel+Record Select
--!  0-1 R :  Channel #
--!  2-3 R :  Record Index
--! 0x44 RW:  Channel Control
--!  0   R :  Record status; 1=valid, 2=late
--!  1   R :  ASCII Channel Name
--!  2    W:  Clear control bits
--!  3   RW:  Set   control bits; 1=draining, 2=frozen, 4=interrupt mask
--! 0x48 RW:  Interrupt address
--! 0x4C -- reserved --
--!
--! 0x50 RW:  Fill
--!   0x0-1:  Current Channel fill
--!   0x2-3:  Max fill (can be cleared to 0)
--! 0x54 RW:  Valid    actions counter (includes conflict+late)
--! 0x58 RW:  Conflict actions counter
--! 0x5C RW:  Late     actions counter
--! 
--! 0x60 R :  Event1 ... do NOT synchronize; hold index long enough
--! 0x64 R :  Event0
--! 0x68 R :  Param1
--! 0x6C R :  Param0
--! 0x70 R :  Tag
--! 0x74 R :  Tef
--! 0x78 R :  Time1
--! 0x7C R :  Time0
--!
--------------------------------------------------------------------------------
--! This library is free software; you can redistribute it and/or
--! modify it under the terms of the GNU Lesser General Public
--! License as published by the Free Software Foundation; either
--! version 3 of the License, or (at your option) any later version.
--!
--! This library is distributed in the hope that it will be useful,
--! but WITHOUT ANY WARRANTY; without even the implied warranty of
--! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--! Lesser General Public License for more details.
--!  
--! You should have received a copy of the GNU Lesser General Public
--! License along with this library. If not, see <http://www.gnu.org/licenses/>.
---------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wishbone_pkg.all;
use work.eca_pkg.all;
use work.gencores_pkg.all;
use work.wb_irq_pkg.all;

entity eca is
  generic(
    g_eca_name       : t_name;
    g_channel_names  : t_name_array;
    g_log_table_size : natural := 7;  -- 128 entries -- condition table
    g_log_queue_len  : natural := 8;  -- 256 entries -- action queue size
    g_num_channels   : natural := 4;  -- max 256
    g_log_clock_mult : natural := 4; -- a_clk_i and c_clk_i must be within 16*
    g_inspect_queue  : boolean := true;
    g_inspect_table  : boolean := true;
    g_frequency_mul  : natural := 1; -- 125MHz = 1*5^9*2^6/1
    g_frequency_5s   : natural := 9;
    g_frequency_2s   : natural := 6;
    g_frequency_div  : natural := 1);
  port(
    -- Push events to the ECA unit (a_clk_i domain)
    e_stb_i     : in  std_logic;
    e_stall_o   : out std_logic;
    e_event_i   : in  t_event;
    e_param_i   : in  t_param;
    e_tef_i     : in  t_tef;
    e_time_i    : in  t_time;
    e_index_o   : out std_logic_vector(7 downto 0);
    -- ECA control registers
    c_clk_i     : in  std_logic;
    c_rst_n_i   : in  std_logic;
    c_slave_i   : in  t_wishbone_slave_in; -- 1KB space
    c_slave_o   : out t_wishbone_slave_out;
    -- Actions output according to time
    a_clk_i     : in  std_logic;
    a_rst_n_i   : in  std_logic;
    a_time_i    : in  t_time;
    a_channel_o : out t_channel_array(g_num_channels-1 downto 0);
    -- Interrupts that report failure conditions
    i_clk_i     : in  std_logic;
    i_rst_n_i   : in  std_logic;
    i_master_i  : in  t_wishbone_master_in;
    i_master_o  : out t_wishbone_master_out);
end eca;

architecture rtl of eca is
  -- Out of principle, tell quartus to leave my design alone.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";
  
  constant c_channel_bits  : natural := f_ceil_log2(g_num_channels);
  constant c_address_bits  : natural := f_ceil_log2(g_num_channels+2) + 5;
  constant c_all_name_bits : natural := (g_num_channels+1)*7;
  constant c_control_zeros : std_logic_vector(c_address_bits-1 downto 6) := (others => '0');
  constant c_counter_bits  : natural := 32;
  
  subtype t_search_index  is std_logic_vector(g_log_table_size   downto 0);
  subtype t_event_index   is std_logic_vector(g_log_table_size-1 downto 0);
  subtype t_queue_index   is std_logic_vector(g_log_queue_len    downto 0);
  subtype t_qtable_index  is std_logic_vector(g_log_queue_len-1  downto 0);
  subtype t_channel_index is std_logic_vector(c_channel_bits-1   downto 0);
  subtype t_all_name      is std_logic_vector(c_all_name_bits-1  downto 0);
  subtype t_counter       is std_logic_vector(c_counter_bits-1   downto 0);
  subtype t_counter_cross is std_logic_vector(g_log_clock_mult   downto 0);
  
  type t_queue_index_array   is array(natural range <>) of t_queue_index;
  type t_counter_array       is array(natural range <>) of t_counter;
  type t_counter_cross_array is array(natural range <>) of t_counter_cross;
  type t_ascii_array         is array(natural range <>) of t_ascii;
  type t_all_name_array      is array(63 downto 0) of t_all_name;
  
  -- Registers:
  signal rc_cs_page     : std_logic       := '0';
  signal rc_cf_enabled  : std_logic       := '0';
  signal rc_cs_wen      : std_logic       := '0';
  signal rc_cs_active   : std_logic       := '0';
  signal rc_cs_addr     : t_search_index  := (others => '0');
  signal rc_cs_valid    : std_logic       := '0';
  signal rc_cs_first    : t_event_index   := (others => '0');
  signal rc_cs_event    : t_event         := (others => '0');
  signal rc_cw_wen      : std_logic       := '0';
  signal rc_cw_active   : std_logic       := '0';
  signal rc_cw_addr     : t_event_index   := (others => '0');
  signal rc_cw_valid    : std_logic       := '0';
  signal rc_cw_next     : t_event_index   := (others => '0');
  signal rc_cw_time     : t_time          := (others => '0');
  signal rc_cw_tag      : t_tag           := (others => '0');
  signal rc_cw_channel  : t_channel_index := (others => '0');
  signal rc_cq_channel  : t_channel_index := (others => '0');
  signal rc_cq_index    : t_qtable_index  := (others => '0');
  signal rc_max_fill    : t_queue_index_array (g_num_channels-1 downto 0) := (others => (others => '0'));
  signal rc_cq_drain    : std_logic_vector(g_num_channels-1 downto 0) := (others => '1');
  signal rc_cq_freeze   : std_logic_vector(g_num_channels-1 downto 0) := (others => '1');
  signal rc_ce_idx      : std_logic_vector(7 downto 0)                := (others => '0');
  signal rc_cn_index    : std_logic_vector(5 downto 0)                := (others => '1');
  signal rc_stall       : std_logic_vector(9 downto 0)                := (others => '1');
  signal rc_ci_enable   : std_logic := '0';
  signal rc_ci_mask     : std_logic_vector(g_num_channels-1 downto 0) := (others => '0');
  signal rc_ci_dest     : t_wishbone_address_array(g_num_channels-1 downto 0) := (others => (others => '0'));
  
  -- Registers fed from c_clk_i => a_clk_i
  signal ra1_cs_page    : std_logic;
  signal ra0_cs_page    : std_logic;
  signal ra1_cf_enabled : std_logic;
  signal ra0_cf_enabled : std_logic;
  signal ra1_cq_drain   : std_logic_vector(g_num_channels-1 downto 0);
  signal ra0_cq_drain   : std_logic_vector(g_num_channels-1 downto 0);
  signal ra1_cq_freeze  : std_logic_vector(g_num_channels-1 downto 0);
  signal ra0_cq_freeze  : std_logic_vector(g_num_channels-1 downto 0);
  
  -- Registers fed from c_clk_i => i_clk_i
  signal rc_ci_ready : std_logic_vector(g_num_channels-1 downto 0);
  signal sc_ci_clear : std_logic_vector(g_num_channels-1 downto 0);
  signal sc_ci_send  : std_logic_vector(g_num_channels-1 downto 0);
  
  -- Signals between c_clk_i => i_clk_i
  signal si_interrupt : std_logic_vector(g_num_channels-1 downto 0);
  
  -- Signals between name and control
  signal sc_nc_record   : t_all_name;
  signal sc_nc_eca      : t_ascii;
  signal sc_nc_channel  : t_ascii_array(g_num_channels-1 downto 0); 
  
  -- Signals between Control and Search (c_clk_i)
  signal sc_cs_program_page : std_logic;
  signal sc_sc_valid        : std_logic;
  signal sc_sc_first        : t_event_index;
  signal sc_sc_event        : t_event;
  
  -- Signals between Control and Walker (c_clk_i)
  signal sc_cw_program_page : std_logic;
  signal sc_wc_valid        : std_logic;
  signal sc_wc_next         : t_event_index;
  signal sc_wc_time         : t_time;
  signal sc_wc_tag          : t_tag;
  signal sc_wc_channel      : t_channel_index;
  
  -- Signals for search->walker
  signal sa_sw_stb   : std_logic;
  signal sa_ws_stall : std_logic;
  signal sa_sw_page  : std_logic;
  signal sa_sw_event : t_event;
  signal sa_sw_param : t_param;
  signal sa_sw_tef   : t_tef;
  signal sa_sw_time  : t_time;
  signal sa_sw_first : t_event_index;
  
  -- Registers for the action queues
  signal ra_qc_channel  : t_channel_array    (g_num_channels-1 downto 0);
  signal ra_aq_time_Q   : t_time;
  
  -- Signals for the action queues
  signal sa_qc_fill     : t_queue_index_array(g_num_channels-1 downto 0);
  signal sa_qw_full     : std_logic_vector   (g_num_channels-1 downto 0);
  signal sa_wq_channel  : t_channel_array    (g_num_channels-1 downto 0);
  signal sa_qc_channel  : t_channel_array    (g_num_channels-1 downto 0);
  signal sa_qc_inspect  : t_channel_array    (g_num_channels-1 downto 0);
  signal sa_aq_time_off : t_time;
  
  -- Registers fed from e_clk_i to a_clk_i
  signal ra_qc_fill_gray  : t_queue_index_array(g_num_channels-1 downto 0);
  signal rc1_qc_fill_gray : t_queue_index_array(g_num_channels-1 downto 0);
  signal rc0_qc_fill_gray : t_queue_index_array(g_num_channels-1 downto 0);
  signal rc_qc_fill       : t_queue_index_array(g_num_channels-1 downto 0);
  signal ra_time_gray     : t_time;
  signal rc1_time_gray    : t_time;
  signal rc0_time_gray    : t_time;
  
  -- Wide counters, crossed using smaller counters
  signal ra_qc_valid                : std_logic_vector(g_num_channels-1 downto 0);
  signal ra_qc_valid_cross          : t_counter_cross_array(g_num_channels-1 downto 0);
  signal ra_qc_valid_cross_gray     : t_counter_cross_array(g_num_channels-1 downto 0);
  signal rc1_qc_valid_cross_gray    : t_counter_cross_array(g_num_channels-1 downto 0);
  signal rc0_qc_valid_cross_gray    : t_counter_cross_array(g_num_channels-1 downto 0);
  signal rc_qc_valid_cross          : t_counter_cross_array(g_num_channels-1 downto 0);
  signal rc_qc_valid_done           : t_counter_cross_array(g_num_channels-1 downto 0);
  signal rc_valid_count             : t_counter_array(g_num_channels-1 downto 0);
  signal ra_qc_late                 : std_logic_vector(g_num_channels-1 downto 0);
  signal ra_qc_late_cross           : t_counter_cross_array(g_num_channels-1 downto 0);
  signal ra_qc_late_cross_gray      : t_counter_cross_array(g_num_channels-1 downto 0);
  signal rc1_qc_late_cross_gray     : t_counter_cross_array(g_num_channels-1 downto 0);
  signal rc0_qc_late_cross_gray     : t_counter_cross_array(g_num_channels-1 downto 0);
  signal rc_qc_late_cross           : t_counter_cross_array(g_num_channels-1 downto 0);
  signal rc_qc_late_done            : t_counter_cross_array(g_num_channels-1 downto 0);
  signal rc_late_count              : t_counter_array(g_num_channels-1 downto 0);
  signal ra_qc_conflict             : std_logic_vector(g_num_channels-1 downto 0);
  signal ra_qc_conflict_cross       : t_counter_cross_array(g_num_channels-1 downto 0);
  signal ra_qc_conflict_cross_gray  : t_counter_cross_array(g_num_channels-1 downto 0);
  signal rc1_qc_conflict_cross_gray : t_counter_cross_array(g_num_channels-1 downto 0);
  signal rc0_qc_conflict_cross_gray : t_counter_cross_array(g_num_channels-1 downto 0);
  signal rc_qc_conflict_cross       : t_counter_cross_array(g_num_channels-1 downto 0);
  signal rc_qc_conflict_done        : t_counter_cross_array(g_num_channels-1 downto 0);
  signal rc_conflict_count          : t_counter_array(g_num_channels-1 downto 0);
  
  impure function update(x : std_logic_vector) return std_logic_vector is
    variable v_sel : std_logic_vector(x'range);
    variable v_dat : std_logic_vector(x'range);
  begin
    for i in x'range loop
      v_sel(i) := c_slave_i.sel((i-x'low) / 8);
      v_dat(i) := c_slave_i.dat(i-x'low);
    end loop;
    return (x and not v_sel) or (v_dat and v_sel);
  end update;
  
  impure function toggle(x : std_logic; i : natural) return std_logic is
    variable v_set : std_logic := c_slave_i.dat(i+0) and c_slave_i.sel(0);
    variable v_clr : std_logic := c_slave_i.dat(i+8) and c_slave_i.sel(1);
  begin
    return ((not v_set) and (not v_clr) and (    x)) or -- unmodified
           ((    v_set) and (    v_clr) and (not x)) or -- toggled
           ((    v_set) and (not v_clr));               -- set
  end toggle;
  
  function f_all_names return t_all_name_array is
    variable result : t_all_name_array;
  begin
    for i in 0 to 63 loop
      for c in 0 to g_num_channels-1 loop
        result(i)((c+1)*7-1 downto c*7) := g_channel_names(c)(i);
      end loop;
      result(i)((g_num_channels+1)*7-1 downto g_num_channels*7) := g_eca_name(i);
    end loop;
    return result;
  end f_all_names;
  
  constant c_names : t_all_name_array := f_all_names;
begin

  -- Name index
  sc_nc_record <= c_names(to_integer(unsigned(rc_cn_index)));
  sc_nc_eca <= sc_nc_record((g_num_channels+1)*7-1 downto g_num_channels*7);
  names : for channel_idx in 0 to g_num_channels-1 generate
    sc_nc_channel(channel_idx) <= sc_nc_record((channel_idx+1)*7-1 downto channel_idx*7);
  end generate;
  
  c_slave_o.STALL <= rc_stall(0);
  c_slave_o.ERR <= '0';
  c_slave_o.RTY <= '0';
  c_slave_o.INT <= '0'; -- Why is this here?? :-/
  
  e_index_o <= rc_ce_idx;

  sc_cs_program_page <= (not rc_cs_page) xor rc_cs_active;
  sc_cw_program_page <= (not rc_cs_page) xor rc_cw_active;
  
  wb : process(c_clk_i) is
    variable channel : integer;
  begin
    if rising_edge(c_clk_i) then
      channel := to_integer(unsigned(rc_cq_channel));
      
      if c_rst_n_i = '0' then
        rc_cs_page    <= '0';
        rc_cf_enabled <= '0';
        rc_cs_wen     <= '0';
        rc_cs_active  <= '0';
        rc_cs_addr    <= (others => '0');
        rc_cs_valid   <= '0';
        rc_cs_first   <= (others => '0');
        rc_cs_event   <= (others => '0');
        rc_cw_wen     <= '0';
        rc_cw_active  <= '0';
        rc_cw_addr    <= (others => '0');
        rc_cw_valid   <= '0';
        rc_cw_next    <= (others => '0');
        rc_cw_time    <= (others => '0');
        rc_cw_tag     <= (others => '0');
        rc_cw_channel <= (others => '0');
        rc_cq_channel <= (others => '0');
        rc_cq_index   <= (others => '0');
        rc_max_fill   <= (others => (others => '0'));
        rc_cq_drain   <= (others => '1');
        rc_cq_freeze  <= (others => '1');
        rc_ce_idx     <= (others => '0');
        rc_cn_index   <= (others => '1');
        rc_stall      <= (others => '1');
        rc_ci_enable  <= '0';
        rc_ci_mask    <= (others => '0');
        rc_ci_dest    <= (others => (others => '0'));
        
        rc_valid_count    <= (others => (others => '0'));
        rc_late_count     <= (others => (others => '0'));
        rc_conflict_count <= (others => (others => '0'));
        
        c_slave_o.DAT <= (others => '0');
        c_slave_o.ACK <= '0';
      else
        c_slave_o.DAT <= (others => '0');
        c_slave_o.ACK <= c_slave_i.CYC and c_slave_i.STB and not rc_stall(0);
        rc_stall <= '0' & rc_stall(rc_stall'length-1 downto 1);
        
        if c_slave_i.CYC = '1' and c_slave_i.STB = '1' then
          rc_cn_index <= f_eca_add(rc_cn_index, -1);
        end if;
        
        case to_integer(unsigned(c_slave_i.ADR(6 downto 2))) is
          when  0 => c_slave_o.DAT(31 downto 24) <= std_logic_vector(to_unsigned(g_log_table_size, 8));
                     c_slave_o.DAT(23 downto 16) <= std_logic_vector(to_unsigned(g_log_queue_len,  8));
                     c_slave_o.DAT(15 downto  8) <= std_logic_vector(to_unsigned(g_num_channels,   8));
                     c_slave_o.DAT( 7 downto  0) <= rc_ce_idx;
          when  1 => c_slave_o.DAT(24) <= f_eca_active_high(g_inspect_table);
                     c_slave_o.DAT(25) <= f_eca_active_high(g_inspect_queue);
                     c_slave_o.DAT(22 downto 16) <= sc_nc_eca;
                     c_slave_o.DAT(1) <= rc_ci_enable;
                     c_slave_o.DAT(0) <= not rc_cf_enabled;
          when  2 => c_slave_o.DAT <= f_eca_gray_decode(rc0_time_gray(63 downto 32), 1);
          when  3 => c_slave_o.DAT <= f_eca_gray_decode(rc0_time_gray(31 downto  0), 1);
          when  4 => c_slave_o.DAT(31) <= rc_cs_active;
                     c_slave_o.DAT(rc_cs_addr'range) <= rc_cs_addr;
          when  5 => c_slave_o.DAT(31) <= rc_cs_valid;
                     c_slave_o.DAT(rc_cs_first'range) <= rc_cs_first;
          when  6 => c_slave_o.DAT <= rc_cs_event(63 downto 32);
          when  7 => c_slave_o.DAT <= rc_cs_event(31 downto  0);
          when  8 => c_slave_o.DAT(31) <= rc_cw_active;
                     c_slave_o.DAT(rc_cw_addr'range) <= rc_cw_addr;
          when  9 => c_slave_o.DAT(31) <= rc_cw_valid;
                     c_slave_o.DAT(rc_cw_next'range) <= rc_cw_next;
          when 10 => c_slave_o.DAT <= rc_cw_time(63 downto 32);
          when 11 => c_slave_o.DAT <= rc_cw_time(31 downto  0);
          when 12 => c_slave_o.DAT(rc_cw_tag'range) <= rc_cw_tag;
          when 13 => c_slave_o.DAT(rc_cw_channel'range) <= rc_cw_channel;
          when 14 => c_slave_o.DAT(31 downto  0) <= std_logic_vector(to_unsigned(g_frequency_mul, 32));
          when 15 => c_slave_o.DAT(31 downto 24) <= std_logic_vector(to_unsigned(g_frequency_5s, 8));
                     c_slave_o.DAT(23 downto 16) <= std_logic_vector(to_unsigned(g_frequency_2s, 8));
                     c_slave_o.DAT(15 downto  0) <= std_logic_vector(to_unsigned(g_frequency_div, 16));
          
          when 16 => c_slave_o.DAT(rc_cq_channel'left+16 downto rc_cq_channel'right+16) <= rc_cq_channel;
                     c_slave_o.DAT(rc_cq_index'range) <= rc_cq_index;
          when 17 => c_slave_o.DAT(24) <= ra_qc_channel(channel).valid;
                     c_slave_o.DAT(25) <= ra_qc_channel(channel).late;
                     -- conflict is always '0' because this can only be determined on execution
                     -- inspecting the channel happens before the action is sorted
                     -- c_slave_o.DAT(26) <= ra_qc_channel(channel).conflict;
                     c_slave_o.DAT(22 downto 16) <= sc_nc_channel(channel);
                     c_slave_o.DAT(2) <= rc_ci_mask(channel);
                     c_slave_o.DAT(1) <= rc_cq_freeze(channel);
                     c_slave_o.DAT(0) <= rc_cq_drain(channel);
          when 18 => c_slave_o.DAT(t_wishbone_address'range) <= rc_ci_dest(channel);
          when 19 => null; -- reserved
          when 20 => c_slave_o.DAT(t_queue_index'length+15 downto 16) <= rc_qc_fill(channel);
                     c_slave_o.DAT(t_queue_index'range) <= rc_max_fill(channel);
          when 21 => c_slave_o.DAT(t_counter'range) <= rc_valid_count(channel);
          when 22 => c_slave_o.DAT(t_counter'range) <= rc_conflict_count(channel);
          when 23 => c_slave_o.DAT(t_counter'range) <= rc_late_count(channel);
          
          -- These all cross clock domain.
          -- However, they are held unchanging for several cycles due to freeze+stall
          when 24 => c_slave_o.DAT <= ra_qc_channel(channel).event(63 downto 32);
          when 25 => c_slave_o.DAT <= ra_qc_channel(channel).event(31 downto  0);
          when 26 => c_slave_o.DAT <= ra_qc_channel(channel).param(63 downto 32);
          when 27 => c_slave_o.DAT <= ra_qc_channel(channel).param(31 downto  0);
          when 28 => c_slave_o.DAT <= ra_qc_channel(channel).tag;
          when 29 => c_slave_o.DAT <= ra_qc_channel(channel).tef;
          when 30 => c_slave_o.DAT <= ra_qc_channel(channel).time(63 downto 32);
          when 31 => c_slave_o.DAT <= ra_qc_channel(channel).time(31 downto  0);
          
          when others => null; -- No other cases
        end case;
        
        rc_cs_wen <= '0';
        rc_cw_wen <= '0';
        
        if g_inspect_table then
          rc_cs_valid <= sc_sc_valid;
          rc_cs_first <= sc_sc_first;
          rc_cs_event <= sc_sc_event;
          
          rc_cw_valid   <= sc_wc_valid;
          rc_cw_next    <= sc_wc_next;
          rc_cw_time    <= sc_wc_time;
          rc_cw_tag     <= sc_wc_tag;
          rc_cw_channel <= sc_wc_channel;
        end if;
        
        -- Only allow the next pointer to be valid if it is topologically sorted
        -- This ensures that there is never a loop in the walker chain.
        if rc_cw_wen = '1' and rc_cw_valid = '1' and 
           unsigned(rc_cw_next) >= unsigned(rc_cw_addr) then
          rc_cw_valid <= '0';
          rc_cw_wen   <= '1';
        end if;
        
        -- If the channel is out-of-range, zero it.
        if to_integer(unsigned(rc_cw_channel)) >= g_num_channels then
          rc_cw_channel <= (others => '0');
          rc_cw_wen <= '1';
        end if;
        
        -- Update counter crossings
        for channel_idx in 0 to g_num_channels-1 loop
          if unsigned(rc_max_fill(channel_idx)) < unsigned(rc_qc_fill(channel_idx)) then
            rc_max_fill(channel_idx) <= rc_qc_fill(channel_idx);
          end if;
          rc_valid_count(channel_idx) <= 
            f_eca_delta(rc_valid_count(channel_idx), 
                        rc_qc_valid_done(channel_idx), 
                        rc_qc_valid_cross(channel_idx));
          rc_late_count(channel_idx) <= 
            f_eca_delta(rc_late_count(channel_idx), 
                        rc_qc_late_done(channel_idx), 
                        rc_qc_late_cross(channel_idx));
          rc_conflict_count(channel_idx) <= 
            f_eca_delta(rc_conflict_count(channel_idx), 
                        rc_qc_conflict_done(channel_idx), 
                        rc_qc_conflict_cross(channel_idx));
        end loop;
        
        if c_slave_i.CYC = '1' and c_slave_i.STB = '1' and c_slave_i.WE = '1' and rc_stall(0) = '0' then
          case to_integer(unsigned(c_slave_i.ADR(6 downto 2))) is
            when  0 => rc_ce_idx <= update(rc_ce_idx);
            when  1 => 
              rc_cs_page <= rc_cs_page xor (c_slave_i.DAT(2) and c_slave_i.SEL(0));
              rc_cf_enabled <= not toggle(not rc_cf_enabled,  0);
              rc_ci_enable <= toggle(rc_ci_enable, 1);
            when  2 => null; -- Cannot write to Time1
            when  3 => null; -- Cannot write to Time0
            when  4 => if c_slave_i.SEL(3) = '1' then rc_cs_active <= c_slave_i.DAT(31); end if;
                       rc_cs_addr <= update(rc_cs_addr);
                       rc_stall(2 downto 0) <= (others => '1'); -- wait for rc_cs_* to fill
            when  5 => if c_slave_i.SEL(3) = '1' then rc_cs_valid  <= c_slave_i.DAT(31); end if;
                       rc_cs_first <= update(rc_cs_first); 
                       rc_cs_wen <= not rc_cs_active;
                       rc_stall(2 downto 0) <= (others => '1'); -- prevent reading old data
            when  6 => rc_cs_event(63 downto 32) <= update(rc_cs_event(63 downto 32));
                       rc_cs_wen <= not rc_cs_active;
                       rc_stall(2 downto 0) <= (others => '1');
            when  7 => rc_cs_event(31 downto 0) <= update(rc_cs_event(31 downto 0));
                       rc_cs_wen <= not rc_cs_active;
                       rc_stall(2 downto 0) <= (others => '1');
            when  8 => if c_slave_i.SEL(3) = '1' then rc_cw_active <= c_slave_i.DAT(31); end if; 
                       rc_cw_addr <= update(rc_cw_addr);
                       rc_stall(2 downto 0) <= (others => '1'); -- wait for rc_cw_* to fill
            when  9 => if c_slave_i.SEL(3) = '1' then rc_cw_valid <= c_slave_i.DAT(31); end if;
                       rc_cw_next <= update(rc_cw_next);
                       rc_cw_wen <= not rc_cw_active;
                       rc_stall(3 downto 0) <= (others => '1'); -- extra cycle for validity check
            when 10 => rc_cw_time(63 downto 32) <= update(rc_cw_time(63 downto 32));
                       rc_cw_wen <= not rc_cw_active;
                       rc_stall(2 downto 0) <= (others => '1');
            when 11 => rc_cw_time(31 downto 0) <= update(rc_cw_time(31 downto  0));
                       rc_cw_wen <= not rc_cw_active;
                       rc_stall(2 downto 0) <= (others => '1');
            when 12 => rc_cw_tag <= update(rc_cw_tag);
                       rc_cw_wen <= not rc_cw_active;
                       rc_stall(2 downto 0) <= (others => '1');
            when 13 => rc_cw_channel <= update(rc_cw_channel);
                       rc_cw_wen <= not rc_cw_active;
                       rc_stall(3 downto 0) <= (others => '1'); -- extra cycle for validity check
            when 14 => null; -- Freq1
            when 15 => null; -- Freq0
            
            when 16 => 
              if c_slave_i.SEL(2) = '1' then
                rc_cq_channel <= c_slave_i.DAT(rc_cq_channel'left+16 downto rc_cq_channel'right+16);
              end if;
              rc_cq_index <= update(rc_cq_index);
              
              -- Wait a reallly long time.
              -- It takes time for rc_cq_index to stabilize as input in a_clk_i
              -- It takes time for the M9K to spit out the result
              -- It takes time for the result to stabilize back into c_clk_i
              rc_stall <= (others => '1');
              
            when 17 => 
              for channel_idx in 0 to g_num_channels-1 loop
                if channel_idx = channel then
                  rc_ci_mask  (channel_idx) <= toggle(rc_ci_mask  (channel_idx), 2);
                  rc_cq_freeze(channel_idx) <= toggle(rc_cq_freeze(channel_idx), 1);
                  rc_cq_drain (channel_idx) <= toggle(rc_cq_drain (channel_idx), 0);
                end if;
              end loop;
            
            when 18 =>
              for channel_idx in 0 to g_num_channels-1 loop
                if channel_idx = channel then
                  rc_ci_dest(channel_idx) <= update(rc_ci_dest(channel_idx));
                end if;
              end loop;
              
            when 19 => -- reserved
              
            when 20 => 
              for channel_idx in 0 to g_num_channels-1 loop
                if channel_idx = channel then
                  rc_max_fill(channel_idx) <= update(rc_max_fill(channel_idx));
                end if;
              end loop;
            
            when 21 =>
              for channel_idx in 0 to g_num_channels-1 loop
                if channel_idx = channel then
                  rc_valid_count(channel_idx) <= update(rc_valid_count(channel_idx));
                end if;
              end loop;
              
            when 22 =>
              for channel_idx in 0 to g_num_channels-1 loop
                if channel_idx = channel then
                  rc_conflict_count(channel_idx) <= update(rc_conflict_count(channel_idx));
                end if;
              end loop;
            
            when 23 =>
              for channel_idx in 0 to g_num_channels-1 loop
                if channel_idx = channel then
                  rc_late_count(channel_idx) <= update(rc_late_count(channel_idx));
                end if;
              end loop;
            
            when 24 => null; -- Event1
            when 25 => null; -- Event0
            when 26 => null; -- Param1
            when 27 => null; -- Param0
            when 28 => null; -- Tag
            when 29 => null; -- Tef
            when 30 => null; -- Time1
            when 31 => null; -- Time0
            
            when others => null; -- No other cases
          end case;
        end if; -- cyc+stb+we+!stall
      end if; -- reset
    end if;
  end process;
  
  a_a2c : process(a_clk_i) is
  begin
    if rising_edge(a_clk_i) then
      -- No reset; logic is acyclic
      ra1_cs_page    <= rc_cs_page;
      ra1_cf_enabled <= rc_cf_enabled;
      ra1_cq_drain   <= rc_cq_drain;
      ra1_cq_freeze  <= rc_cq_freeze;
      ra0_cs_page    <= ra1_cs_page;
      ra0_cf_enabled <= ra1_cf_enabled;
      ra0_cq_drain   <= ra1_cq_drain;
      ra0_cq_freeze  <= ra1_cq_freeze;
      
      for channel_idx in 0 to g_num_channels-1 loop
        ra_qc_fill_gray(channel_idx) <= f_eca_gray_encode(sa_qc_fill(channel_idx));
      end loop;
      
      ra_time_gray(63 downto 32) <= f_eca_gray_encode(a_time_i(63 downto 32));
      ra_time_gray(31 downto  0) <= f_eca_gray_encode(a_time_i(31 downto  0));
      
      for channel_idx in 0 to g_num_channels-1 loop
        ra_qc_channel(channel_idx).valid <= 
          f_eca_active_high(g_inspect_queue) and
          ra0_cq_freeze(channel_idx) and
          sa_qc_inspect(channel_idx).valid;
        ra_qc_channel(channel_idx).conflict <= 
          f_eca_active_high(g_inspect_queue) and
          ra0_cq_freeze(channel_idx) and
          sa_qc_inspect(channel_idx).conflict;
        ra_qc_channel(channel_idx).late <= 
          f_eca_active_high(g_inspect_queue) and
          ra0_cq_freeze(channel_idx) and
          sa_qc_inspect(channel_idx).late;
          
        if g_inspect_queue then
          ra_qc_channel(channel_idx).event <= sa_qc_inspect(channel_idx).event;
          ra_qc_channel(channel_idx).param <= sa_qc_inspect(channel_idx).param;
          ra_qc_channel(channel_idx).tag   <= sa_qc_inspect(channel_idx).tag;
          ra_qc_channel(channel_idx).tef   <= sa_qc_inspect(channel_idx).tef;
          ra_qc_channel(channel_idx).time  <= sa_qc_inspect(channel_idx).time;
        else
          ra_qc_channel(channel_idx).valid <= '0';
          ra_qc_channel(channel_idx).event <= (others => '0');
          ra_qc_channel(channel_idx).param <= (others => '0');
          ra_qc_channel(channel_idx).tag   <= (others => '0');
          ra_qc_channel(channel_idx).tef   <= (others => '0');
          ra_qc_channel(channel_idx).time  <= (others => '0');
        end if;
        
        ra_qc_valid(channel_idx)    <= sa_qc_channel(channel_idx).valid;
        ra_qc_conflict(channel_idx) <= sa_qc_channel(channel_idx).valid and sa_qc_channel(channel_idx).conflict;
        ra_qc_late(channel_idx)     <= sa_qc_channel(channel_idx).valid and sa_qc_channel(channel_idx).late;
      
        if ra_qc_valid(channel_idx) = '1' then
          ra_qc_valid_cross(channel_idx) <= f_eca_add(ra_qc_valid_cross(channel_idx), 1);
        end if;
        if ra_qc_conflict(channel_idx) = '1' then
          ra_qc_conflict_cross(channel_idx)  <= f_eca_add(ra_qc_conflict_cross(channel_idx), 1);
        end if;
        if ra_qc_late(channel_idx) = '1' then
          ra_qc_late_cross(channel_idx)  <= f_eca_add(ra_qc_late_cross(channel_idx), 1);
        end if;
        
        ra_qc_valid_cross_gray(channel_idx)    <= f_eca_gray_encode(ra_qc_valid_cross(channel_idx));
        ra_qc_conflict_cross_gray(channel_idx) <= f_eca_gray_encode(ra_qc_conflict_cross(channel_idx));
        ra_qc_late_cross_gray(channel_idx)     <= f_eca_gray_encode(ra_qc_late_cross(channel_idx));
      end loop;
    end if;
  end process;
  
  c_a2c : process(c_clk_i) is
  begin
    if rising_edge(c_clk_i) then
      rc1_qc_fill_gray <= ra_qc_fill_gray;
      rc0_qc_fill_gray <= rc1_qc_fill_gray;
      for channel_idx in 0 to g_num_channels-1 loop
        rc_qc_fill(channel_idx) <= f_eca_gray_decode(rc0_qc_fill_gray(channel_idx), 1);
      end loop;
      
      rc1_time_gray <= ra_time_gray;
      rc0_time_gray <= rc1_time_gray;
      
      rc1_qc_valid_cross_gray <= ra_qc_valid_cross_gray;
      rc0_qc_valid_cross_gray <= rc1_qc_valid_cross_gray;
      rc1_qc_conflict_cross_gray <= ra_qc_conflict_cross_gray;
      rc0_qc_conflict_cross_gray <= rc1_qc_conflict_cross_gray;
      rc1_qc_late_cross_gray <= ra_qc_late_cross_gray;
      rc0_qc_late_cross_gray <= rc1_qc_late_cross_gray;
      
      for channel_idx in 0 to g_num_channels-1 loop
        rc_qc_valid_cross(channel_idx)    <= f_eca_gray_decode(rc0_qc_valid_cross_gray(channel_idx), 1);
        rc_qc_conflict_cross(channel_idx) <= f_eca_gray_decode(rc0_qc_conflict_cross_gray(channel_idx), 1);
        rc_qc_late_cross(channel_idx)     <= f_eca_gray_decode(rc0_qc_late_cross_gray(channel_idx), 1);
      end loop;
      
      -- We use the difference between done and cross to increase the counter in process 'wb'
      
      rc_qc_valid_done <= rc_qc_valid_cross;
      rc_qc_conflict_done <= rc_qc_conflict_cross;
      rc_qc_late_done <= rc_qc_late_cross;
    end if;
  end process;
  
  interrupts : for channel_idx in 0 to g_num_channels-1 generate
  
    gen : process(c_clk_i) is
    begin
      if rising_edge(c_clk_i) then
        if rc_qc_late_cross    (channel_idx) /= rc_qc_late_done    (channel_idx) or
           rc_qc_conflict_cross(channel_idx) /= rc_qc_conflict_done(channel_idx) then
          rc_ci_ready(channel_idx) <= rc_ci_enable and rc_ci_mask(channel_idx);
        end if;
        
        if sc_ci_send(channel_idx) = '1' then
          rc_ci_ready(channel_idx) <= '0';
        end if;
      end if;
    end process;
    
    sc_ci_send(channel_idx) <= rc_ci_ready(channel_idx) and sc_ci_clear(channel_idx);
    
    sync : gc_pulse_synchronizer2
      port map(
        clk_in_i    => c_clk_i,
        rst_in_n_i  => c_rst_n_i,
        clk_out_i   => i_clk_i,
        rst_out_n_i => i_rst_n_i,
        d_ready_o   => sc_ci_clear(channel_idx),
        d_p_i       => sc_ci_send (channel_idx),
        q_p_o       => si_interrupt(channel_idx));
  
  end generate;
  
  irq : irqm_core
    generic map(
      g_channels => g_num_channels)
    port map(
      clk_i         => i_clk_i,
      rst_n_i       => i_rst_n_i,
      irq_master_o  => i_master_o,
      irq_master_i  => i_master_i,
      msi_dst_array => rc_ci_dest, -- guarded by rc_ci_mask
      msi_msg_array => (others => (others => '0')),
      en_i          => '1',
      mask_i        => (others => '1'),
      irq_i         => si_interrupt);
  
  search : eca_search
    generic map(
      g_log_table_size => g_log_table_size)
    port map(
      clk_i      => a_clk_i,
      rst_n_i    => ra0_cf_enabled,
      
      e_stb_i    => e_stb_i,
      e_stall_o  => e_stall_o,
      e_page_i   => ra0_cs_page,
      e_event_i  => e_event_i,
      e_param_i  => e_param_i,
      e_tef_i    => e_tef_i,
      e_time_i   => e_time_i,
      
      w_stb_o    => sa_sw_stb,
      w_stall_i  => sa_ws_stall,
      w_page_o   => sa_sw_page,
      w_first_o  => sa_sw_first,
      w1_event_o => sa_sw_event,
      w1_param_o => sa_sw_param,
      w1_tef_o   => sa_sw_tef,
      w1_time_o  => sa_sw_time,
      
      t_clk_i    =>  c_clk_i,
      t_page_i   => sc_cs_program_page,
      t_addr_i   => rc_cs_addr,
      tw_en_i    => rc_cs_wen,
      tw_valid_i => rc_cs_valid,
      tw_first_i => rc_cs_first,
      tw_event_i => rc_cs_event,
      tr_valid_o => sc_sc_valid,
      tr_first_o => sc_sc_first,
      tr_event_o => sc_sc_event);
  
  walker : eca_walker
    generic map(
      g_log_table_size => g_log_table_size,
      g_num_channels   => g_num_channels)
    port map(
      clk_i        => a_clk_i,
      rst_n_i      => ra0_cf_enabled,
      time_Q_i     => ra_aq_time_Q,
      
      b_stb_i      => sa_sw_stb,
      b_stall_o    => sa_ws_stall,
      b_page_i     => sa_sw_page,
      b_first_i    => sa_sw_first,
      b1_event_i   => sa_sw_event,
      b1_param_i   => sa_sw_param,
      b1_tef_i     => sa_sw_tef,
      b1_time_i    => sa_sw_time,
      
      q_channel_o  => sa_wq_channel,
      q_full_i     => sa_qw_full,
      q_freeze_i   => ra0_cq_freeze,
      
      t_clk_i      =>  c_clk_i,
      t_page_i     => sc_cw_program_page,
      t_addr_i     => rc_cw_addr,
      tw_en_i      => rc_cw_wen,
      tw_valid_i   => rc_cw_valid,
      tw_next_i    => rc_cw_next,
      tw_time_i    => rc_cw_time,
      tw_tag_i     => rc_cw_tag,
      tw_channel_i => rc_cw_channel,
      tr_valid_o   => sc_wc_valid,
      tr_next_o    => sc_wc_next,
      tr_time_o    => sc_wc_time,
      tr_tag_o     => sc_wc_tag,
      tr_channel_o => sc_wc_channel);

  timeX : process(a_clk_i) is
  begin
    if rising_edge(a_clk_i) then
      -- No reset; logic is acyclic
      ra_aq_time_Q <= sa_aq_time_off;
    end if;
  end process;

  timeQ : eca_offset
    generic map(
      g_data_bits => 64,
      g_parts     => 4,
      g_offset    => 2**(g_log_queue_len+1))
    port map(
      clk_i  => a_clk_i,
      a_i    => a_time_i,
      c1_o   => open,
      x2_o   => sa_aq_time_off,
      c2_o   => open);
  
  channels : for channel_idx in 0 to g_num_channels-1 generate
    channel : eca_channel
      generic map(
        g_log_table_size  => g_log_queue_len,
        g_log_latency     => g_log_queue_len,
        g_log_queue_depth => g_log_queue_len+1)
      port map(
        clk_i     =>  a_clk_i,
        rst_n_i   =>  a_rst_n_i,
        drain_i   => ra0_cq_drain (channel_idx),
        freeze_i  => ra0_cq_freeze(channel_idx),
        addr_i    => rc_cq_index, -- crosses clock domains, but held stable
        fill_o    => sa_qc_fill   (channel_idx),
        full_o    => sa_qw_full   (channel_idx),
        time_i    =>  a_time_i,
        time_Q_i  => ra_aq_time_Q,
        channel_i => sa_wq_channel(channel_idx),
        channel_o => sa_qc_channel(channel_idx),
        inspect_o => sa_qc_inspect(channel_idx));
  end generate;
  
  a_channel_o <= sa_qc_channel;
  
end rtl;
