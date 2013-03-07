--! @file eca_pkg.vhd
--! @brief Event-Condition-Action package
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This package defines all the needed types and components for the ECA unit.
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

library work;
use work.wishbone_pkg.all;

package eca_pkg is

 constant c_eca_sdb : t_sdb_device := (
    abi_class     => x"0000", -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"00",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"7", -- 8/16/32-bit port granularity
    sdb_component => (
    addr_first    => x"0000000000000000",
    addr_last     => x"00000000000003ff",
    product => (
    vendor_id     => x"0000000000000651",
    device_id     => x"8752bf44",
    version       => x"00000001",
    date          => x"20130204",
    name          => "ECA_UNIT:CONTROL   ")));
    
 constant c_eca_evt_sdb : t_sdb_device := (
    abi_class     => x"0000", -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"00",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"4", -- 32-bit port granularity
    sdb_component => (
    addr_first    => x"0000000000000000",
    addr_last     => x"0000000000000003",
    product => (
    vendor_id     => x"0000000000000651",
    device_id     => x"8752bf45",
    version       => x"00000001",
    date          => x"20130204",
    name          => "ECA_UNIT:EVENTS_IN ")));

  constant c_time_bits  : natural := 64;
  constant c_event_bits : natural := 64;
  constant c_param_bits : natural := 32;
  constant c_tag_bits   : natural := 32;
  
  subtype t_ascii is std_logic_vector(6 downto 0);
  subtype t_time  is std_logic_vector(c_time_bits-1  downto 0);
  subtype t_event is std_logic_vector(c_event_bits-1 downto 0);
  subtype t_param is std_logic_vector(c_param_bits-1 downto 0);
  subtype t_tag   is std_logic_vector(c_tag_bits-1   downto 0);
  
  type t_channel is record
    valid : std_logic;
    time  : t_time;
    tag   : t_tag;
    param : t_param;
    event : t_event;
  end record t_channel;
  
  type t_name          is array(63 downto 0)      of t_ascii;
  type t_name_array    is array(natural range <>) of t_name;
  type t_channel_array is array(natural range <>) of t_channel;
  type t_time_array    is array(natural range <>) of t_time;
  type t_tag_array     is array(natural range <>) of t_tag;
  type t_param_array   is array(natural range <>) of t_param;
  type t_event_array   is array(natural range <>) of t_event;

  -- Convert a string into a name (aka, pad it)
  function f_name(name : string) return t_name;
  
  -- White-Rabbit variant of Event-Condition-Action Unit
  component wr_eca is
    generic(
      g_eca_name       : t_name;
      g_channel_names  : t_name_array;
      g_log_table_size : natural := 7; -- 128 entries -- condition table
      g_log_queue_len  : natural := 8; -- 256 entries -- action queue size
      g_num_channels   : natural := 4; -- max 30 due to WB address space
      g_num_streams    : natural := 1;
      g_inspect_queue  : boolean := true;
      g_inspect_table  : boolean := true);
    port(
      -- Stream events to the ECA unit (lower index has priority)
      e_clk_i     : in  std_logic_vector          (g_num_streams-1 downto 0);
      e_rst_n_i   : in  std_logic_vector          (g_num_streams-1 downto 0);
      e_slave_i   : in  t_wishbone_slave_in_array (g_num_streams-1 downto 0);
      e_slave_o   : out t_wishbone_slave_out_array(g_num_streams-1 downto 0);
      -- ECA control registers
      c_clk_i     : in  std_logic;
      c_rst_n_i   : in  std_logic;
      c_slave_i   : in  t_wishbone_slave_in;
      c_slave_o   : out t_wishbone_slave_out;
      -- Actions output according to time
      a_clk_i     : in  std_logic;
      a_rst_n_i   : in  std_logic; -- Hold for at least 10 cycles
      a_tai_i     : in  std_logic_vector(39 downto 0);
      a_cycles_i  : in  std_logic_vector(27 downto 0);
      a_channel_o : out t_channel_array(g_num_channels-1 downto 0));
  end component;
  
  -- Raw Event-Condition-Action Unit
  component eca is
    generic(
      g_eca_name       : t_name;
      g_channel_names  : t_name_array;
      g_log_table_size : natural := 7; -- 128 entries -- condition table
      g_log_queue_len  : natural := 8; -- 256 entries -- action queue size
      g_num_channels   : natural := 4;  -- max 30 due to WB address space
      g_inspect_queue  : boolean := true;
      g_inspect_table  : boolean := true;
      g_frequency_mul  : natural := 1; -- 125MHz = 1*5^9*2^6/1
      g_frequency_5s   : natural := 9; -- mul:32, 5s:8, 2s:8, div:16 bits
      g_frequency_2s   : natural := 6;
      g_frequency_div  : natural := 1);
    port(
      -- Push events to the ECA unit (a_clk_i domain)
      e_stb_i     : in  std_logic;
      e_stall_o   : out std_logic;
      e_event_i   : in  t_event;
      e_time_i    : in  t_time;
      e_param_i   : in  t_param;
      e_index_o   : out std_logic_vector(7 downto 0);
      -- ECA control registers
      c_clk_i     : in  std_logic;
      c_rst_n_i   : in  std_logic;
      c_slave_i   : in  t_wishbone_slave_in;
      c_slave_o   : out t_wishbone_slave_out;
      -- Actions output according to time
      a_clk_i     : in  std_logic;
      a_rst_n_i   : in  std_logic;
      a_time_i    : in  t_time;
      a_channel_o : out t_channel_array(g_num_channels-1 downto 0));
  end component;
  
  -- Execute a WB write to address "tag" with data "param"
  component eca_wb_channel is
    port(
      clk_i     : in  std_logic;
      rst_n_i   : in  std_logic;
      channel_i : in  t_channel;
      master_o  : out t_wishbone_master_out;
      master_i  : in  t_wishbone_master_in);
  end component;
  
  -- Set bits selected by "tag" to value in "param"
  component eca_gpio_channel is
    port(
      clk_i     : in  std_logic;
      rst_n_i   : in  std_logic;
      channel_i : in  t_channel;
      gpio_o    : out std_logic_vector(15 downto 0));
  end component;
  
  ---------------------- Internals ------------------------
  
  function f_eca_active_high(x : boolean) return std_logic;
  function f_eca_max(a, b : natural) return natural;
  function f_eca_ripple(a, b : std_logic_vector; c : std_logic) return std_logic_vector;
  function f_eca_gray_encode(x : std_logic_vector) return std_logic_vector;
  function f_eca_gray_decode(x : std_logic_vector; step : natural) return std_logic_vector;
  
  -- Registers its inputs. Async outputs. 
  -- When r_addr_i=w_addr_i, r_data_o is undefined.
  component eca_sdp is
    generic(
      g_addr_bits  : natural := 8;
      g_data_bits  : natural := 8;
      g_dual_clock : boolean);
    port(
      r_clk_i  : in  std_logic;
      r_addr_i : in  std_logic_vector(g_addr_bits-1 downto 0);
      r_data_o : out std_logic_vector(g_data_bits-1 downto 0);
      w_clk_i  : in  std_logic;
      w_en_i   : in  std_logic;
      w_addr_i : in  std_logic_vector(g_addr_bits-1 downto 0);
      w_data_i : in  std_logic_vector(g_data_bits-1 downto 0));
  end component;
  
  -- Registers its inputs. Async outputs. 
  -- When aw_en_i=1, ar_data_o returns old data (not aw_data_i).
  -- When a_clk_i=b_clk_i, aw_en_i=1, bw_en_i=0, and a_addr_i=b_addr_i,
  --  then br_data_o returns old data (not aw_data_i).
  -- When a_clk_i /= b_clk_i, aw_en_i=1, bw_en_i=0, and a_addr_i=b_addr_i,
  --   then br_data_o is undefined.
  -- When aw_en_i=bw_en_i=1 and a_addr_i=b_addr_i, the cell is corrupted.
  -- !!! unsupported by Arria5. Fuck. !!!
  component eca_tdp is
    generic(
      g_addr_bits : natural := 8;
      g_data_bits : natural := 8);
    port(
      a_clk_i   : in  std_logic;
      a_addr_i  : in  std_logic_vector(g_addr_bits-1 downto 0);
      aw_en_i   : in  std_logic;
      aw_data_i : in  std_logic_vector(g_data_bits-1 downto 0);
      ar_data_o : out std_logic_vector(g_data_bits-1 downto 0);
      
      b_clk_i   : in  std_logic;
      b_addr_i  : in  std_logic_vector(g_addr_bits-1 downto 0);
      bw_en_i   : in  std_logic;
      bw_data_i : in  std_logic_vector(g_data_bits-1 downto 0);
      br_data_o : out std_logic_vector(g_data_bits-1 downto 0));
  end component;
  
  -- Expects registers for inputs. Async outputs.
  -- c1_o is available after 1 cycle (2 once registered)
  -- c2_o, x2_o are available after 2 cycles (3 once registered)
  component eca_adder is
    generic(
      g_data_bits : natural := 64;
      g_parts     : natural := 4);
    port(
      clk_i   : in  std_logic;
      stall_i : in  std_logic := '0';
      a_i     : in  std_logic_vector(g_data_bits-1 downto 0);
      b_i     : in  std_logic_vector(g_data_bits-1 downto 0);
      c_i     : in  std_logic := '0';
      c1_o    : out std_logic;
      x2_o    : out std_logic_vector(g_data_bits-1 downto 0);
      c2_o    : out std_logic);
  end component;
  
  -- Expects a register for inputs. Async output.
  -- c1_o is available after 1 cycle (2 once registered)
  -- c2_o, x2_o are available after 2 cycles (3 once registered)
  component eca_offset is
    generic(
      g_data_bits : natural := 64;
      g_parts     : natural := 4;
      g_offset    : natural := 1);
    port(
      clk_i   : in  std_logic;
      stall_i : in  std_logic := '0';
      a_i     : in  std_logic_vector(g_data_bits-1 downto 0);
      c1_o    : out std_logic;
      x2_o    : out std_logic_vector(g_data_bits-1 downto 0);
      c2_o    : out std_logic);
  end component;
  
  -- Convert White Rabbit time to ECA time
  component eca_wr_time is
    port(
      clk_i    : in std_logic;
      tai_i    : in std_logic_vector(39 downto 0);
      cycles_i : in std_logic_vector(27 downto 0);
      time_o   : out t_time);
  end component;
  
  -- Convert WB writes into inbound ECA events
  component eca_wb_event is
    port(
      w_clk_i   : in  std_logic;
      w_rst_n_i : in  std_logic;
      w_slave_i : in  t_wishbone_slave_in;
      w_slave_o : out t_wishbone_slave_out;
      e_clk_i   : in  std_logic;
      e_rst_n_i : in  std_logic;
      e_stb_o   : out std_logic;
      e_stall_i : in  std_logic;
      e_event_o : out t_event;
      e_time_o  : out t_time;
      e_param_o : out t_param;
      e_index_i : in  std_logic_vector(7 downto 0));
  end component;
  
  component eca_search is
    generic(
      g_log_table_size : natural := 8);
    port(
      clk_i      : in  std_logic;
      rst_n_i    : in  std_logic;
      -- Accept external events
      e_stb_i    : in  std_logic;
      e_stall_o  : out std_logic;
      e_page_i   : in  std_logic;
      e_event_i  : in  t_event;
      e_time_i   : in  t_time;
      e_param_i  : in  t_param;
      -- Feed located event rules to the walker
      w_stb_o    : out std_logic;
      w_stall_i  : in  std_logic;
      w_page_o   : out std_logic;
      w_first_o  : out std_logic_vector(g_log_table_size-1 downto 0);
      w1_event_o : out t_event;
      w1_time_o  : out t_time;
      w1_param_o : out t_param;
      -- Access the search table
      t_clk_i    : in  std_logic;
      t_page_i   : in  std_logic;
      t_addr_i   : in  std_logic_vector(g_log_table_size downto 0);
      tw_en_i    : in  std_logic;
      tw_valid_i : in  std_logic;
      tw_first_i : in  std_logic_vector(g_log_table_size-1 downto 0);
      tw_event_i : in  t_event;
      tr_valid_o : out std_logic;
      tr_first_o : out std_logic_vector(g_log_table_size-1 downto 0);
      tr_event_o : out t_event);
  end component;
  
  component eca_walker is
    generic(
      g_log_table_size : natural := 8;
      g_num_channels   : natural := 4);
    port(
      clk_i        : in  std_logic;
      rst_n_i      : in  std_logic;
      -- Feed in an index to scan from binary search
      b_stb_i      : in  std_logic;
      b_stall_o    : out std_logic;
      b_page_i     : in  std_logic;
      b_first_i    : in  std_logic_vector(g_log_table_size-1 downto 0);
      b1_event_i   : in  t_event;
      b1_time_i    : in  t_time;
      b1_param_i   : in  t_param;
      -- Outputs for the channel queue
      q_channel_o  : out t_channel_array (g_num_channels-1 downto 0);
      q_full_i     : in  std_logic_vector(g_num_channels-1 downto 0);
      q_freeze_i   : in  std_logic_vector(g_num_channels-1 downto 0);
      -- Access the walker table
      t_clk_i      : in  std_logic;
      t_page_i     : in  std_logic;
      t_addr_i     : in  std_logic_vector(g_log_table_size-1 downto 0);
      tw_en_i      : in  std_logic;
      tw_valid_i   : in  std_logic;
      tw_next_i    : in  std_logic_vector(g_log_table_size-1 downto 0);
      tw_time_i    : in  t_time;
      tw_tag_i     : in  t_tag;
      tw_channel_i : in  std_logic_vector(f_ceil_log2(g_num_channels)-1 downto 0);
      tr_valid_o   : out std_logic;
      tr_next_o    : out std_logic_vector(g_log_table_size-1 downto 0);
      tr_time_o    : out t_time;
      tr_tag_o     : out t_tag;
      tr_channel_o : out std_logic_vector(f_ceil_log2(g_num_channels)-1 downto 0));
  end component;
  
  component eca_channel is
    generic(
      g_log_table_size  : natural := 8;
      g_log_latency     : natural := 8;  -- Must be <= g_log_table_size
      g_log_queue_depth : natural := 9); -- Must be >  g_log_latency
    port(
      clk_i     : in  std_logic;
      rst_n_i   : in  std_logic;
      freeze_i  : in  std_logic; -- stop action outflow and use addr_i=>inspect_o
      drain_i   : in  std_logic; -- stop action in+outflow and erase tables
      addr_i    : in  std_logic_vector(g_log_table_size-1 downto 0);
      fill_o    : out std_logic_vector(g_log_table_size   downto 0); 
      full_o    : out std_logic;
      -- Timestamps used for pipeline stages
      time_i    : in  t_time;
      time_Q_i  : in  t_time; -- time_i + 2**g_log_queue_depth
      -- Push a record to the queue
      channel_i : in  t_channel;
      channel_o : out t_channel;
      inspect_o : out t_channel);
  end component;
  
end eca_pkg;

package body eca_pkg is

  function f_name(name : string) return t_name is
    variable result : t_name;
  begin
    for i in 1 to 63 loop
      if i > name'length then
        result(64-i) := (others => '0');
      else
        result(64-i) := std_logic_vector(to_unsigned(character'pos(name(i)), 7));
      end if;
    end loop;
    result(0) := (others => '0');
    return result;
  end f_name;
  
  function f_eca_active_high(x : boolean) return std_logic is
  begin
    if x then
      return '1';
    else
      return '0';
    end if;
  end f_eca_active_high;

  function f_eca_ripple(a, b : std_logic_vector; c : std_logic) return std_logic_vector is
    constant len : natural := a'length;
    variable aw, bw, rw : std_logic_vector(len+1 downto 0);
    variable x : std_logic_vector(len downto 0);
  begin
    aw := "0" & a & c;
    bw := "0" & b & c;
    rw := std_logic_vector(unsigned(aw) + unsigned(bw));
    x := rw(len+1 downto 1);
    return x;
  end f_eca_ripple;
  
  function f_eca_max(a, b : natural) return natural is
  begin
    if a > b then 
      return a; 
    else 
      return b;
    end if;
  end f_eca_max;
  
  function f_eca_gray_encode(x : std_logic_vector) return std_logic_vector is
    variable o : std_logic_vector(x'length downto 0);
  begin
    o := (x & '0') xor ('0' & x);
    return o(x'length downto 1);
  end f_eca_gray_encode;
  
  -- Call with step=1
  function f_eca_gray_decode(x : std_logic_vector; step : natural) return std_logic_vector is
    constant len : natural := x'length;
    alias    y : std_logic_vector(len-1 downto 0) is x;
    variable z : std_logic_vector(len-1 downto 0) := (others => '0');
  begin
    if step >= len then
      return y;
    else
      z(len-step-1 downto 0) := y(len-1 downto step);
      return f_eca_gray_decode(y xor z, step+step);
    end if;
  end f_eca_gray_decode;

end eca_pkg;
