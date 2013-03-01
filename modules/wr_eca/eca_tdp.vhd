--! @file eca_tdp.vhd
--! @brief ECA True dual-ported memory
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! Unfortunately, this requires Arria2 due to "old-data" behaviour.
--! Will replace this in the future.
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

-- Registers its inputs. Async outputs. 
-- When aw_en_i=1, ar_data_o returns old data (not aw_data_i).
-- When a_clk_i=b_clk_i, aw_en_i=1, bw_en_i=0, and a_addr_i=b_addr_i,
--  then br_data_o returns old data (not aw_data_i).
-- When a_clk_i /= b_clk_i, aw_en_i=1, bw_en_i=0, and a_addr_i=b_addr_i,
--   then br_data_o is undefined.
-- When aw_en_i=bw_en_i=1 and a_addr_i=b_addr_i, the cell is corrupted.
entity eca_tdp is
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
end eca_tdp;

architecture rtl of eca_tdp is
  type ram_t is array(2**g_addr_bits-1 downto 0) of 
    std_logic_vector(g_data_bits-1 downto 0);
    
  signal ram : ram_t := (others => (others => '0'));
begin
  
  a : process(a_clk_i)
  begin
    if rising_edge(a_clk_i) then
      ar_data_o <= ram(to_integer(unsigned(a_addr_i)));
      
      if aw_en_i = '1' then
        ram(to_integer(unsigned(a_addr_i))) <= aw_data_i;
      end if;
    end if;
  end process;

  b : process(b_clk_i)
  begin
    if rising_edge(b_clk_i) then
      br_data_o <= ram(to_integer(unsigned(b_addr_i)));
      
      if bw_en_i = '1' then
        ram(to_integer(unsigned(b_addr_i))) <= bw_data_i;
      end if;
    end if;
  end process;
  

end rtl;
