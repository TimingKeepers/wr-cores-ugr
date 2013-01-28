--! @file eca_wb_channel.vhd
--! @brief ECA-WB Adapter
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This component takes an action channel and turns it into a Wishbone master.
--! The address written to is taken from the ECA condition tag.
--! The value written is the event parameter.
--! Since actions cannot be delayed, this adapter drops actions when stalled.
--! It also only allows 100 writes in-flight without ack before dropping actions.
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

entity eca_wb_channel is
  port(
    clk_i     : in  std_logic;
    rst_n_i   : in  std_logic;
    channel_i : in  t_channel;
    master_o  : out t_wishbone_master_out;
    master_i  : in  t_wishbone_master_in);
end eca_wb_channel;

architecture rtl of eca_wb_channel is
  -- Out of principle, tell quartus to leave my design alone.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";
  
  -- Allow for a lot of inflight requests
  signal r_flight : unsigned(6 downto 0) := (others => '0');
  signal r_cyc    : std_logic            := '0';
  signal r_stb    : std_logic            := '0';
begin

  master_o.cyc <= r_cyc;
  master_o.stb <= r_stb;
  master_o.we  <= '1';
  master_o.sel <= (others => '1');

  main : process(clk_i) is
    variable push : boolean;
    variable done : boolean;
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        r_flight <= (others => '0');
        r_cyc <= '0';
        r_stb <= '0';
      else
        push := r_stb = '1' and master_i.stall = '0';
        done := master_i.ack = '1' or master_i.err = '1' or master_i.rty = '1';
        
        if push then
          r_stb <= '0';
          if not done then
            r_flight <= r_flight + 1;
          end if;
        else
          if done then
            if r_flight = 1 then
              r_cyc <= '0';
            end if;
            r_flight <= r_flight - 1;
          end if;
        end if;
        
        if channel_i.valid = '1' and r_flight /= 100 then
          r_cyc <= '1';
          r_stb <= '1';
          master_o.adr <= channel_i.tag;
          master_o.dat <= channel_i.param;
        end if;
      end if;
    end if;
  end process;
  
end rtl;
