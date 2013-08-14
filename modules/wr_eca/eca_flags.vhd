--! @file eca_flags.vhd
--! @brief ECA Flag memory
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! Channels require a component with these operations:
--!   Random access store   (for scanner)
--!   Sequential read+clear (for dispatcher)
--!
--! To implement read+clear requires a read+write port.
--! Thus we need 2 write ports and 1 read port, a costly component in general.
--! Fortunately, because the reads are (mostly) sequential, we can get by with
--! two dual ported RAMs. When the sequential reading hiccups, we opt to fail
--! to report data rather than fail to clear already read data. A hiccup in the
--! sequence correponds to a clock jump, where the ECA cannot guarantee timely
--! delivery anyway. Therefore, prefer to keep the free list correct instead.
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
use work.genram_pkg.all;

-- Registers its inputs. Async outputs. 
entity eca_flags is
  generic(
    g_addr_bits : natural := 8;
    g_data_bits : natural := 8);
  port(
    clk_i    : in  std_logic;
    
    -- Port A: random access store/fill
    a_addr_i : in  std_logic_vector(g_addr_bits-1 downto 0);
    a_en_i   : in  std_logic;
    a_data_i : in  std_logic_vector(g_data_bits-1 downto 0);
    
    -- Port B: sequential read/clear
    b_addr_i : in  std_logic_vector(g_addr_bits-1 downto 0);
    b_full_o : out std_logic;
    b_data_o : out std_logic_vector(g_data_bits-1 downto 0));
end eca_flags;

architecture rtl of eca_flags is

  signal last_addr : std_logic_vector(g_addr_bits-1 downto 0);
  signal execute   : std_logic;
  
  signal q0a_w, q1a_w, q0b_w, q1b_w : std_logic;
  signal q0a_a, q1a_a, q0b_a, q1b_a : std_logic_vector(g_addr_bits-1 downto 1);
  signal q0a_d, q1a_d, q0b_d, q1b_d : std_logic_vector(g_data_bits   downto 0);
  
begin

  Q0 : generic_dpram
    generic map(
      g_data_width               => g_data_bits+1,
      g_size                     => 2**(g_addr_bits-1),
      g_with_byte_enable         => false,
      g_addr_conflict_resolution => "dont_care", -- this file's raison d'etre
      g_dual_clock               => false)
    port map(
      clka_i => clk_i,
      bwea_i => (others => '1'),
      wea_i  => q0a_w,
      aa_i   => q0a_a,
      da_i   => q0a_d,
      qa_o   => open,
      clkb_i => clk_i,
      bweb_i => (others => '1'),
      web_i  => q0b_w,
      ab_i   => q0b_a,
      db_i   => (others => '0'),
      qb_o   => q0b_d);
  
  Q1 : generic_dpram
    generic map(
      g_data_width               => g_data_bits+1,
      g_size                     => 2**(g_addr_bits-1),
      g_with_byte_enable         => false,
      g_addr_conflict_resolution => "dont_care", -- this file's raison d'etre
      g_dual_clock               => false)
    port map(
      clka_i => clk_i,
      bwea_i => (others => '1'),
      wea_i  => q1a_w,
      aa_i   => q1a_a,
      da_i   => q1a_d,
      qa_o   => open,
      clkb_i => clk_i,
      bweb_i => (others => '1'),
      web_i  => q1b_w,
      ab_i   => q1b_a,
      db_i   => (others => '0'),
      qb_o   => q1b_d);
  
  -- Chip select based on random access low bit
  q0a_w <= a_en_i and not a_addr_i(0);
  q1a_w <= a_en_i and     a_addr_i(0);
  q0a_a <= a_addr_i(g_addr_bits-1 downto 1); 
  q1a_a <= a_addr_i(g_addr_bits-1 downto 1); 
  q0a_d <= '1' & a_data_i;
  q1a_d <= '1' & a_data_i;
  
  -- If the address switched banks (continuous clock), execute request
  execute <= b_addr_i(0) xor last_addr(0);
  
  main : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      q0b_w <= execute and not b_addr_i(0);
      q1b_w <= execute and     b_addr_i(0);
      last_addr <= b_addr_i;
    end if;
  end process;
  
  q0b_a <= last_addr(g_addr_bits-1 downto 1) when q0b_w='1' else b_addr_i(g_addr_bits-1 downto 1);
  q1b_a <= last_addr(g_addr_bits-1 downto 1) when q1b_w='1' else b_addr_i(g_addr_bits-1 downto 1);
  
  b_data_o <= q0b_d(g_data_bits-1 downto 0) when last_addr(0) = '0' else q1b_d(g_data_bits-1 downto 0);
  b_full_o <= (q0b_d(g_data_bits) and q0b_w) or (q1b_d(g_data_bits) and q1b_w);

end rtl;
