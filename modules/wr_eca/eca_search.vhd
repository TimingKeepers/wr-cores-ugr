--! @file eca_search.vhd
--! @brief ECA Binary search
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! Search the condition table for the largest record smaller than the input.
--! Due to comparison latency, this binary-search algorithm has a pipeline=4.
--! To maximize search bandwidth each pipeline stage handles a distinct search.
--! The binary search takes g_log_table_size+2 iterations; 
--!   the first +1 is because we have 2**(g_log_table_size+1) entries
--!   the second +1 is due to pipeline delay feeding the result to the walker
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

entity eca_search is
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
end eca_search;

architecture rtl of eca_search is
  -- Quartus 11+ goes crazy and infers 7 M9Ks in an altshift_taps! Stop it.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";
  
  constant c_list_ptr_bits    : natural := g_log_table_size;
  constant c_table_ptr_bits   : natural := c_list_ptr_bits+1;
  constant c_table_index_bits : natural := c_table_ptr_bits+1;
  
  subtype t_table_ptr is std_logic_vector(c_table_ptr_bits-1 downto 0);
  subtype t_list_ptr  is std_logic_vector(c_list_ptr_bits -1 downto 0);
  
  type t_state is (search, output, input);
  
  -- Stage 0 registers+signals
  signal r0_page  : std_logic;
  signal r0_event : t_event;
  signal r0_time  : t_time;
  signal r0_param : t_param;
  signal r0_known : t_table_ptr;
  signal r0_try   : t_table_ptr := (others => '0');
  signal r0_state : t_state     := input;
  
  signal s0_valid : std_logic;
  signal s0_first : t_list_ptr;
  signal s0_test  : t_event;
  
  -- Stage 1 registers+signals
  signal r1_page  : std_logic;
  signal r1_event : t_event;
  signal r1_time  : t_time;
  signal r1_param : t_param;
  signal r1_known : t_table_ptr;
  signal r1_try   : t_table_ptr := (others => '0');
  signal r1_state : t_state     := input;
  
  signal r1_first : t_list_ptr;
  signal r1_test_n: t_event;
  signal r1_stb   : std_logic;
  signal r1_done  : std_logic;
  
  signal s1_state : t_state;
  
  -- Stage 2 registers+signals
  signal r2_page  : std_logic;
  signal r2_event : t_event;
  signal r2_time  : t_time;
  signal r2_param : t_param;
  signal r2_known : t_table_ptr;
  signal r2_try   : t_table_ptr := (others => '0');
  signal r2_state : t_state     := input;
  
  signal r2_stall : std_logic;
  
  signal s2_less  : std_logic;
  
  -- Stage 3 registers+signals
  signal r3_page  : std_logic;
  signal r3_event : t_event;
  signal r3_time  : t_time;
  signal r3_param : t_param;
  signal r3_known : t_table_ptr;
  signal r3_try   : t_table_ptr := (others => '0');
  signal r3_state : t_state     := input;
  
  signal r3_shift : std_logic;
  signal r3_less  : std_logic;

  signal s3_less  : t_table_ptr;
  signal s3_known : t_table_ptr;
  signal s3_try   : t_table_ptr;
  signal s3_probe : t_table_ptr;
  signal s3_state : t_state;
  
  constant c_valid_offset : natural := 0;
  subtype  c_first_range is natural range c_list_ptr_bits+c_valid_offset     downto c_valid_offset    +1;
  subtype  c_event_range is natural range c_event_bits   +c_first_range'left downto c_first_range'left+1;
  
  constant s3_probe_length : natural := s3_probe'length;
  
  subtype  t_data_type is std_logic_vector(c_event_range);
  constant c_data_bits : natural := t_data_type'left+1; --'
  
  signal active_r_addr_i : std_logic_vector(c_table_index_bits-1 downto 0);
  signal active_w_addr_i : std_logic_vector(c_table_index_bits-1 downto 0);
  signal active_r_data_o : std_logic_vector(c_data_bits       -1 downto 0);
  signal active_w_data_i : std_logic_vector(c_data_bits       -1 downto 0);
  
  signal program_r_addr_i : std_logic_vector(c_table_index_bits-1 downto 0);
  signal program_w_addr_i : std_logic_vector(c_table_index_bits-1 downto 0);
  signal program_r_data_o : std_logic_vector(c_data_bits       -1 downto 0);
  signal program_w_data_i : std_logic_vector(c_data_bits       -1 downto 0);

begin

  active_r_addr_i(s3_probe_length) <= r3_page;
  active_r_addr_i(s3_probe'range)  <= s3_probe;
  active_w_addr_i(t_addr_i'length) <= t_page_i;
  active_w_addr_i(t_addr_i'range)  <= t_addr_i;
  active_w_data_i(c_valid_offset)  <= tw_valid_i;
  active_w_data_i(c_first_range)   <= tw_first_i;
  active_w_data_i(c_event_range)   <= tw_event_i;
  s0_valid <= active_r_data_o(c_valid_offset);
  s0_first <= active_r_data_o(c_first_range);
  s0_test  <= active_r_data_o(c_event_range);
  
  Active : eca_sdp
    generic map(
      g_addr_bits  => c_table_index_bits,
      g_data_bits  => c_data_bits,
      g_dual_clock => true)
    port map(
      r_clk_i  => clk_i,
      r_addr_i => active_r_addr_i,
      r_data_o => active_r_data_o,
      w_clk_i  => t_clk_i,
      w_addr_i => active_w_addr_i,
      w_en_i   => tw_en_i,
      w_data_i => active_w_data_i);
  
  program_r_addr_i(t_addr_i'length) <= t_page_i;
  program_r_addr_i(t_addr_i'range)  <= t_addr_i;
  program_w_addr_i(t_addr_i'length) <= t_page_i;
  program_w_addr_i(t_addr_i'range)  <= t_addr_i;
  program_w_data_i(c_valid_offset)  <= tw_valid_i;
  program_w_data_i(c_first_range)   <= tw_first_i;
  program_w_data_i(c_event_range)   <= tw_event_i;
  tr_valid_o <= program_r_data_o(c_valid_offset);
  tr_first_o <= program_r_data_o(c_first_range);
  tr_event_o <= program_r_data_o(c_event_range);
  
  Program : eca_sdp
    generic map(
      g_addr_bits  => c_table_index_bits,
      g_data_bits  => c_data_bits,
      g_dual_clock => false)
    port map(
      r_clk_i  => t_clk_i,
      r_addr_i => program_r_addr_i,
      r_data_o => program_r_data_o,
      w_clk_i  => t_clk_i,
      w_addr_i => program_w_addr_i,
      w_en_i   => tw_en_i,
      w_data_i => program_w_data_i);
  
  -- c_o=1 iff    r1_event - r1_test >= 0  ...   r1_test <= r1_event
  compare : eca_adder
    port map(
      clk_i => clk_i,
      a_i   => r1_event,
      b_i   => r1_test_n,
      c_i   => '1',
      c1_o  => s2_less,
      x2_o  => open,
      c2_o  => open);
  
  -- Goal: find the largest rX_known, such that event[rX_known] <= rX_event
  -- 
  -- Pipeline has 4 stages:
  --   0: probe                                      => output
  --   1: compare  first   w_stb_o
  --   2: ...              e_stall_o                 => input
  --   3: less             page/event/time/param     => search
  
  main : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      r0_page  <= r3_page;
      r0_event <= r3_event;
      r0_time  <= r3_time;
      r0_param <= r3_param;
      r0_known <= s3_known;
      r0_try   <= s3_try;
      r0_state <= s3_state;
      
      r1_page  <= r0_page;
      r1_event <= r0_event;
      r1_time  <= r0_time;
      r1_param <= r0_param;
      r1_known <= r0_known;
      -- A boring pipeline-stage: a good place to feed in the reset
      if rst_n_i = '0' then
        r1_try   <= (others => '0');
        r1_state <= input;
      else
        r1_try   <= r0_try;
        r1_state <= r0_state;
      end if;
      
      r1_first <= s0_first;
      r1_test_n<= not s0_test;
      r1_stb   <= f_eca_active_high(r0_state = output) and s0_valid;
      r1_done  <= f_eca_active_high(r0_state = output) and not s0_valid;
      
      r2_page  <= r1_page;
      r2_event <= r1_event;
      r2_time  <= r1_time;
      r2_param <= r1_param;
      r2_known <= r1_known;
      r2_try   <= r1_try;
      r2_state <= s1_state;
      
      r2_stall <= f_eca_active_high(s1_state /= input);
      
      if e_stb_i = '1' and r2_stall = '0' then
        r3_page  <= e_page_i;
        r3_event <= e_event_i;
        r3_time  <= e_time_i;
        r3_param <= e_param_i;
        r3_known <= (others => '0');
        r3_try   <= r2_try;
        r3_state <= search;
        
        r3_shift <= '1';
        r3_less  <= s2_less;
      else
        r3_page  <= r2_page;
        r3_event <= r2_event;
        r3_time  <= r2_time;
        r3_param <= r2_param;
        r3_known <= r2_known;
        r3_try   <= r2_try;
        r3_state <= r2_state;
        
        r3_shift <= '0';
        r3_less  <= s2_less;
      end if;
    end if;
  end process;
  
  s1_state <= input when (r1_done = '1' or (r1_stb = '1' and w_stall_i = '0')) else r1_state;
  e_stall_o <= r2_stall;
  
  s3_less  <= (others => r3_less);
  s3_known <= r3_known or (s3_less and r3_try);
  s3_try   <= r3_shift & r3_try(c_table_ptr_bits-1 downto 1);
  s3_probe <= s3_known or s3_try;
  s3_state <= output when r3_try(0)='1' else r3_state;

  w_stb_o   <= r1_stb;
  w_page_o  <= r1_page;
  w_first_o <= r1_first;
  w1_event_o <= r2_event;
  w1_time_o  <= r2_time;
  w1_param_o <= r2_param;
  
end rtl;
