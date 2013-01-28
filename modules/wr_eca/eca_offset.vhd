--! @file eca_offset.vhd
--! @brief Given a counter, produce a counter shift by some offset
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This expects a registered counter input.
--! The calculation is simplified in comparison to eca_adder by the assumption
--! that the input value is incremented by 1 on each cycle. This allows us to
--! forgo storing intermediate values beyond the carry bits.
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

-- Expects registers for inputs. Async outputs.
-- c1_o is available after 1 cycle (2 once registered)
-- c2_o, x2_o are available after 2 cycles (3 once registered)
entity eca_offset is
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
end eca_offset;

architecture rtl of eca_offset is
  -- Out of principle, tell quartus to leave my design alone.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";
  
  constant c_parts    : natural := g_parts; -- Must divide g_data_bits
  constant c_sub_bits : natural := g_data_bits / c_parts;
  
  subtype t_part  is std_logic_vector(c_sub_bits-1 downto 0);
  subtype t_carry is std_logic_vector(c_parts   -1 downto 1);
  type t_part_array is array(c_parts-1 downto 0) of t_part;
  
  constant c_b3 : t_part := std_logic_vector(to_unsigned(g_offset+3, c_sub_bits));
  constant c_b1 : t_part := std_logic_vector(to_unsigned(g_offset+1, c_sub_bits));
  constant c_p0 : t_part := (others => '0');
  constant c_c0 : t_carry := (others => '0');
  
  -- Pipeline:
  --   s1: prop+gen
  --   s2: carry bits
  --   s3: full sum

  -- Registers
  signal r1_p    : t_carry;
  signal r1_g    : std_logic;
  signal r2_c    : t_carry;
  signal r2_cH   : std_logic;

  signal r1_low : std_logic;
  signal r2_low : std_logic;
  
  -- Signals
  signal s1_r : std_logic_vector(c_parts downto 1);
  
begin
  s1_r <= f_eca_ripple(r1_p, c_c0, r1_g);
  
  main : process(clk_i) is
    variable sum : std_logic_vector(c_sub_bits downto 0);
  begin
    if rising_edge(clk_i) then
      if stall_i = '0' then
        -- Does a+offset+3 cause a carry?
        sum := f_eca_ripple(a_i(c_b3'range), c_b3, '0');
        r1_g <= sum(c_sub_bits); -- does the sum generate?
        
        for i in 1 to c_parts-1 loop
          -- Will the other parts propogate a carry?
          sum := f_eca_ripple(a_i((i+1)*c_sub_bits-1 downto i*c_sub_bits), c_p0, '1');
          r1_p(i)  <= sum(c_sub_bits);
        end loop;
        
        r2_c  <= s1_r(c_parts-1 downto 1) xor r1_p;
        r2_cH <= s1_r(c_parts);
        
        r1_low <= a_i(c_sub_bits);
        r2_low <= r1_low;
      end if;
    end if;
  end process;
  
  c1_o <= s1_r(c_parts);
  c2_o <= r2_cH;
  
  -- Assume offset+2 does not overflow low word.
  -- 
  -- If a+(offset+2) overflows low word:
  -- Either:  a+2         overflows
  --     OR: (a+2)+offset overflows
  -- ... but never both
  -- 
  -- So check for overflow of a+2; (r2_low xor a_i(c_sub_bits))
  -- If it did NOT overflow, use computed carries.
  -- If it DID overflow, use NO carries.
  -- ie: carry when r2_c AND
  
  x2_o(c_b1'range) <= 
    f_eca_ripple(a_i(c_b1'range), c_b1, '0')
    (t_part'range);
  
  output : for i in 1 to c_parts-1 generate
    x2_o((i+1)*c_sub_bits-1 downto i*c_sub_bits) <= 
      f_eca_ripple(a_i((i+1)*c_sub_bits-1 downto i*c_sub_bits), c_p0, 
                  r2_c(i) and not (r2_low xor a_i(c_sub_bits)))
      (t_part'range);
  end generate;
  
end rtl;
