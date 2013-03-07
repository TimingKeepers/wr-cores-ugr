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

library work;
use work.wishbone_pkg.all;
use work.eca_pkg.all;
use work.genram_pkg.all;

-- Registers its inputs. Async outputs. 
-- When r_clk_i=w_clk_i, r_data_o is undefined.
entity eca_sdp is
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
end eca_sdp;

architecture rtl of eca_sdp is
begin
  
  ram : generic_simple_dpram
    generic map(
      g_data_width               => g_data_bits,
      g_size                     => 2**g_addr_bits,
      g_with_byte_enable         => false,
      g_addr_conflict_resolution => "dont_care",
      g_dual_clock               => g_dual_clock)
    port map(
      clka_i => w_clk_i,
      wea_i  => w_en_i,
      aa_i   => w_addr_i,
      da_i   => w_data_i,
      clkb_i => r_clk_i,
      ab_i   => r_addr_i,
      qb_o   => r_data_o);

end rtl;
