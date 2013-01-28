--! @file eca_gpio_channel.vhd
--! @brief ECA-GPIO Adapter
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This component takes an action channel and turns it into a GPIO controller.
--! The tag determines the action taken on the outputs; the state of the IO is:
--!   io(x) <= (tag(16+x) and io(x)) xor tag(x);
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

entity eca_gpio_channel is
  port(
    clk_i     : in  std_logic;
    rst_n_i   : in  std_logic;
    channel_i : in  t_channel;
    gpio_o    : out std_logic_vector(15 downto 0));  
end eca_gpio_channel;

architecture rtl of eca_gpio_channel is
  -- Out of principle, tell quartus to leave my design alone.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";
  
  signal r_gpio : std_logic_vector(15 downto 0) := (others => '0');
begin

  gpio_o <= r_gpio;

  main : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        r_gpio <= (others => '0');
      else
        if channel_i.valid = '1' then
          r_gpio <= (r_gpio and channel_i.tag(31 downto 16)) xor channel_i.tag(15 downto 0);
        end if;
      end if;
    end if;
  end process;
  
end rtl;
