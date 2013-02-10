--! @file eca_wr_time.vhd
--! @brief Convert White-Rabbit time to ECA time
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! The ECA unit needs a smoothly increasing timestamp.
--! White-rabbit uses a cycles counter and a UTC counter.
--! The cycles counter jumps from 124999999 to 0, not a smooth transition.
--! This calculates time=UTC*125000000 + cycles for the ECA timestamp.
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

entity eca_wr_time is
  port(
    clk_i    : in std_logic;
    tai_i    : in std_logic_vector(39 downto 0);
    cycles_i : in std_logic_vector(27 downto 0);
    time_o   : out t_time);
end eca_wr_time;

architecture rtl of eca_wr_time is
  -- Quartus 11+ infers an entire M9K to save 64 registers from the r*_cycles
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";
  
  subtype t_cycles is std_logic_vector(27 downto 0);
  
  -- Registers
  signal r0_state    : unsigned(4 downto 0); -- range: 29 downto 0
  signal r2_cycles   : t_cycles;
  signal r1_cycles   : t_cycles;
  signal r0_cycles   : t_cycles;
  signal r0_time_out : t_time;
  signal r0_a        : t_time;
  signal r0_b        : t_time;
  
  -- Signals
  signal s2_time_off : t_time;
  signal s2_sum      : t_time;
  signal s0_load     : boolean;
  signal s0_cycles   : boolean;
begin

  s0_load   <= r0_state < 3;
  s0_cycles <= r0_state < 6;

  adder : eca_adder
    port map(
      clk_i => clk_i,
      a_i   => r0_a,
      b_i   => r0_b,
      c1_o  => open,
      x2_o  => s2_sum,
      c2_o  => open);
  
  multiplier : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      r0_a <= (others => '0');
      r0_b <= (others => '0');
      
      if s0_load then
        -- Multiply tai_i by 5
        r0_a(39 downto  0) <= tai_i;
        r0_b(41 downto  2) <= tai_i;
        r0_cycles <= cycles_i;
      elsif s0_cycles then
        -- Multiply by 2^6, add offset (31), add cycles
        r0_a <= s2_sum(c_time_bits-7 downto 0) & "011111";
        r0_b(r2_cycles'range) <= r2_cycles;
        r0_cycles <= r2_cycles;
      else
        -- Multiply by 5
        r0_a <= s2_sum;
        r0_b <= s2_sum(c_time_bits-3 downto 0) & "00";
        r0_cycles <= r2_cycles;
      end if;
      
      r1_cycles <= r0_cycles;
      r2_cycles <= r1_cycles;
      
      if r0_state = 0 then
        r0_state <= to_unsigned(29, r0_state'length);
      else
        r0_state <= r0_state-1;
      end if;
    end if;
  end process;

  
  counter : eca_offset
    generic map(
      g_data_bits => c_time_bits,
      g_parts     => 4,
      g_offset    => 0)
    port map(
      clk_i => clk_i,
      a_i   => r0_time_out,
      c1_o  => open,
      x2_o  => s2_time_off,
      c2_o  => open);
  
  clock : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      if s0_load then
        r0_time_out <= s2_sum;
      else
        r0_time_out <= s2_time_off;
      end if;
    end if;
  end process;

  time_o <= r0_time_out;
  
end rtl;
