--! @file eca_sdp.vhd
--! @brief ECA Simple dual-ported memory
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! Both Altera and Xilinx can provide wider data words when there
--! is only one reader and one writer. This component provides a
--! memory interface that can be implemented on an FPGA efficiently.
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
-- When r_clk_i=w_clk_i and r_addr_i=w_addr_i, r_data_o return old data (not w_data_i).
-- If r_clk_i /= w_clk_i, then r_data_o is undefined.
entity eca_sdp is
  generic(
    g_addr_bits : natural := 8;
    g_data_bits : natural := 8);
  port(
    r_clk_i  : in  std_logic;
    r_addr_i : in  std_logic_vector(g_addr_bits-1 downto 0);
    r_data_o : out std_logic_vector(g_data_bits-1 downto 0);
    w_clk_i  : in  std_logic;
    w_en_i   : in  std_logic;
    w_addr_i : in  std_logic_vector(g_addr_bits-1 downto 0);
    w_data_i : in  std_logic_vector(g_data_bits-1 downto 0));
end eca_sdp;

architecture rtl of eca_sdp is
  type ram_t is array(2**g_addr_bits-1 downto 0) of 
    std_logic_vector(g_data_bits-1 downto 0);
    
  signal ram : ram_t;
begin
  
  r : process(r_clk_i)
  begin
    if rising_edge(r_clk_i) then
      r_data_o <= ram(to_integer(unsigned(r_addr_i)));
    end if;
  end process;
  
  w : process(w_clk_i)
  begin
    if rising_edge(w_clk_i) then
      if w_en_i = '1' then
        ram(to_integer(unsigned(w_addr_i))) <= w_data_i;
      end if;
    end if;
  end process;
  

end rtl;
