--! @file wr_eca.vhd
--! @brief Event-Condition-Action package
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This component wraps the raw ECA suitably for use with White-Rabbit.
--! It adds Wishbone event channels and translates WR time.
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

entity wr_eca is
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
    a_rst_n_i   : in  std_logic;
    a_tai_i     : in  std_logic_vector(39 downto 0);
    a_cycles_i  : in  std_logic_vector(27 downto 0);
    a_channel_o : out t_channel_array(g_num_channels-1 downto 0));
end wr_eca;

architecture rtl of wr_eca is
  -- Out of principle, tell quartus to leave my design alone.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";
  
  signal sa_time0    : t_time;
  signal sa_index    : std_logic_vector(7 downto 0);
  
  signal sa_stb      : std_logic_vector(g_num_streams-1 downto 0);
  signal sa_event    : t_event_array   (g_num_streams-1 downto 0);
  signal sa_time     : t_time_array    (g_num_streams-1 downto 0);
  signal sa_param    : t_param_array   (g_num_streams-1 downto 0);
  
  signal sa_full_stb   : std_logic_vector(g_num_streams downto 0);
  signal sa_full_stall : std_logic_vector(g_num_streams downto 0);
  signal sa_full_event : t_event_array   (g_num_streams downto 0);
  signal sa_full_time  : t_time_array    (g_num_streams downto 0);
  signal sa_full_param : t_param_array   (g_num_streams downto 0);
  
begin

  E0 : eca
    generic map(
      g_eca_name       => g_eca_name,
      g_channel_names  => g_channel_names,
      g_log_table_size => g_log_table_size,
      g_log_queue_len  => g_log_queue_len,
      g_num_channels   => g_num_channels,
      g_inspect_queue  => g_inspect_queue,
      g_inspect_table  => g_inspect_table,
      g_frequency_mul  => 1, -- White-Rabbit has 125MHz clock
      g_frequency_5s   => 9, -- 125MHz = 1*5^9*2^6/1
      g_frequency_2s   => 6,
      g_frequency_div  => 1)
    port map(
      e_stb_i     => sa_full_stb  (0),
      e_stall_o   => sa_full_stall(0),
      e_event_i   => sa_full_event(0),
      e_time_i    => sa_full_time (0),
      e_param_i   => sa_full_param(0),
      e_index_o   => sa_index,
      c_clk_i     => c_clk_i,
      c_rst_n_i   => c_rst_n_i,
      c_slave_i   => c_slave_i,
      c_slave_o   => c_slave_o,
      a_clk_i     => a_clk_i,
      a_rst_n_i   => a_rst_n_i,
      a_time_i    => sa_time0,
      a_channel_o => a_channel_o);

  T0 : eca_wr_time
    port map(
      clk_i    => a_clk_i,
      tai_i    => a_tai_i,
      cycles_i => a_cycles_i,
      time_o   => sa_time0);
  
  sa_full_stb  (g_num_streams) <= '0';
  sa_full_event(g_num_streams) <= (others => '-');
  sa_full_time (g_num_streams) <= (others => '-');
  sa_full_param(g_num_streams) <= (others => '-');
  
  -- Priority access goes to #0
  Sx : for stream in 0 to g_num_streams-1 generate
    S : eca_wb_event
      port map(
        w_clk_i   => e_clk_i  (stream),
        w_rst_n_i => e_rst_n_i(stream),
        w_slave_i => e_slave_i(stream),
        w_slave_o => e_slave_o(stream),
        
        e_clk_i   => a_clk_i,
        e_rst_n_i => a_rst_n_i,
        e_stb_o   => sa_stb  (stream),
        e_stall_i => sa_full_stall(stream),
        e_event_o => sa_event(stream),
        e_time_o  => sa_time (stream),
        e_param_o => sa_param(stream),
        e_index_i => sa_index);
    
    sa_full_stall(stream+1) <= sa_stb  (stream) or                           sa_full_stall(stream);
    sa_full_stb  (stream)   <= sa_stb  (stream) or                           sa_full_stb  (stream+1);
    sa_full_event(stream)   <= sa_event(stream) when sa_stb(stream)='1' else sa_full_event(stream+1);
    sa_full_time (stream)   <= sa_time (stream) when sa_stb(stream)='1' else sa_full_time (stream+1);
    sa_full_param(stream)   <= sa_param(stream) when sa_stb(stream)='1' else sa_full_param(stream+1);
  end generate;
  
end rtl;
