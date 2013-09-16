-------------------------------------------------------------------------------
-- Title      : Cleanly reset PLLs at power-on
-- Project    : White Rabbit
-------------------------------------------------------------------------------
-- File       : altera_reset.vhd
-- Author     : Wesley W. Terpstra
-- Company    : GSI
-- Created    : 2013-08-23
-- Last update: 2013-08-23
-- Platform   : Altera
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Synchronize PLLs and generate reset lines
-------------------------------------------------------------------------------
--
-- Copyright (c) 2013 GSI / Wesley W. Terpstra
--
-- This source file is free software; you can redistribute it   
-- and/or modify it under the terms of the GNU Lesser General   
-- Public License as published by the Free Software Foundation; 
-- either version 2.1 of the License, or (at your option) any   
-- later version.                                               
--
-- This source is distributed in the hope that it will be       
-- useful, but WITHOUT ANY WARRANTY; without even the implied   
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR      
-- PURPOSE.  See the GNU Lesser General Public License for more 
-- details.                                                     
--
-- You should have received a copy of the GNU Lesser General    
-- Public License along with this source; if not, download it   
-- from http://www.gnu.org/licenses/lgpl-2.1.html
-- 
--
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author    Description
-- 2013-09-16  1.0      terpstra  First version
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity altera_reset is
  generic(
    g_plls    : natural;
    g_clocks  : natural);
  port(
    clk_free  : in  std_logic; -- external free running clock
    rstn_i    : in  std_logic; -- external reset button
    locked_i  : in  std_logic_vector(g_plls-1 downto 0);
    pll_rst_o : out std_logic;
    clocks_i  : in  std_logic_vector(g_clocks-1 downto 0);
    rstn_o    : out std_logic_vector(g_clocks-1 downto 0));
end altera_reset;

architecture rtl of altera_reset is

  constant zeros : std_logic_vector(g_plls-1 downto 0) := (others => '0');
  constant ones  : std_logic_vector(g_plls-1 downto 0) := (others => '1');
  
  subtype t_sync is std_logic_vector(2 downto 0);
  type t_sync_array is array (natural range <>) of t_sync;

  -- async registers
  signal rstn_trig : std_logic := '0';
  signal lock_loss : std_logic_vector(g_plls-1 downto 0) := (others => '0');
  signal s_restart : std_logic;
  signal s_locked  : std_logic;
  
  -- clk_free registers
  signal restart : t_sync := (others => '1');
  signal locked  : t_sync := (others => '0');
  signal relock  : std_logic := '1';
  signal reset   : std_logic := '1';
  signal count   : unsigned(7 downto 0) := (others => '1');
  signal stable  : boolean   := false;
  signal nresets : t_sync_array(g_clocks-1 downto 0) := (others => (others => '0'));
  
begin

  -- Catch any dip in the rstn line
  button : process(rstn_i, relock) is
  begin
    if relock = '1' then
      rstn_trig <= '0';
    elsif falling_edge(rstn_i) then
      rstn_trig <= '1';
    end if;
  end process;
  
  -- Catch any dips in the PLL lock line
  locks : for i in g_plls-1 downto 0 generate
    lock : process(locked_i(i), relock) is
    begin
      if relock = '1' then
        lock_loss(i) <= '0';
      elsif falling_edge(locked_i(i)) then
        lock_loss(i) <= '1';
      end if;
    end process;
  end generate;
  
  -- If any of these lines are true, restart!
  s_restart <= rstn_trig when lock_loss = zeros else '1';
  s_locked <= '1' when locked_i = ones else '0';
  
  -- Reset PLLs and wait till all have locked.
  -- If any PLL loses lock, reset all of them.
  main : process(clk_free) is
  begin
    if rising_edge(clk_free) then
      restart <= s_restart & restart(restart'left downto 1);
      locked  <= s_locked  & locked(locked'left downto 1);
      stable <= count = 0;
      
      -- We don't use a traditional state machine here.
      -- This code has to be clk_free glitch safe!
      -- Every case has at most 6 inputs (ie: fits in one 6-LUT)
      -- (reset, relock, stable, locked(0), restart(0), count(i-1))
      
      if reset = '1' then
        if stable then
          reset  <= '0';
          relock <= '1';
          count  <= (others => '1');
        else
          reset  <= '1';
          relock <= '1';
          count  <= count - 1;
        end if;
      elsif relock = '1' then
        if stable then
          reset  <= '0';
          relock <= '0';
          count  <= (others => '1');
        elsif locked(0) = '0' then
          reset  <= '0';
          relock <= '1';
          count  <= (others => '1');
        else
          reset  <= '0';
          relock <= '1';
          count  <= count - 1;
        end if;
      else
        if restart(0) = '1' then
          reset  <= '1';
          relock <= '1';
          count  <= (others => '1');
        else
          reset  <= '0';
          relock <= '0';
          count  <= (others => '1');
        end if;
      end if;
    end if;
  end process;
  
  pll_rst_o <= reset;
  
  -- Generate per-clock reset lines
  syncs : for i in g_clocks-1 downto 0 generate
    rstn_o(i) <= nresets(i)(0);
    sync : process(relock, clocks_i(i)) is
    begin
      if relock = '1' then
        nresets(i) <= (others => '0');
      elsif rising_edge(clocks_i(i)) then
        nresets(i) <= '1' & nresets(i)(t_sync'left downto 1);
      end if;
    end process;
  end generate;

end rtl;
