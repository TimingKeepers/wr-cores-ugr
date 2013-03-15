-------------------------------------------------------------------------------
-- Entity: pulse_gen
-- File: pulse_gen.vhd
-- Description: a pulse generator which produces a 1-tick-long pulse in its
-- output when the UTC time passed to it through a vector equals a
-- pre-programmed UTC time.
-- Author: Javier Serrano (Javier.Serrano@cern.ch)
-- Date: 24 January 2012
-- Version: 0.01
-- Todo: change gen_out process for complete coincidence (see comments above
-- that process). Substitute ready_for_trig process by a state machine.
-- Factor out synchronizer in a separate reusable block.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--               GNU LESSER GENERAL PUBLIC LICENSE                             
--              -----------------------------------                            
-- This source file is free software; you can redistribute it and/or modify it
-- under the terms of the GNU Lesser General Public License as published by the
-- Free Software Foundation; either version 2.1 of the License, or (at your
-- option) any later version.                           
-- This source is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
-- for more details. You should have received a copy of the GNU Lesser General
-- Public License along with this source; if not, download it from
-- http://www.gnu.org/licenses/lgpl-2.1.html                  
-------------------------------------------------------------------------------



library ieee;
use ieee.std_logic_1164.all;

entity pulse_gen is
  
  generic (
-- reference clock frequency
    g_ref_clk_rate : integer := 125000000);

  port (
    clk_ref_i : in std_logic;           -- timing reference clock
    clk_sys_i : in std_logic;           -- data output reference clock
    rst_n_i   : in std_logic;           -- system reset

    pulse_o : out std_logic;            -- pulse output

    -------------------------------------------------------------------------------
    -- Timing input (from WRPC), clk_ref_i domain
    ------------------------------------------------------------------------------

    -- 1: time given on tm_utc_i and tm_cycles_i is valid (otherwise, don't
    -- produce pulses and keep trig_ready_o line permamaently active)
    tm_time_valid_i : in std_logic;
    -- number of seconds
    tm_tai_i        : in std_logic_vector(39 downto 0);
    -- number of clk_ref_i cycles
    tm_cycles_i     : in std_logic_vector(27 downto 0);


    ---------------------------------------------------------------------------
    -- Time tag output (clk_sys_i domain)
    ---------------------------------------------------------------------------

    -- 1: input is ready to accept next trigger time tag
    trig_ready_o : out std_logic;

    -- time at which the pulse will be produced + a single-cycle strobe to
    -- latch it in
    trig_tai_i      : in std_logic_vector(39 downto 0);
    trig_cycles_i   : in std_logic_vector(27 downto 0);
    trig_valid_i : in std_logic
    );
end pulse_gen;

architecture rtl of pulse_gen is
  
  -- Internal registers to hold trigger time
  signal trig_utc, trig_utc_ref    : std_logic_vector(39 downto 0);
  signal trig_cycles, trig_cycles_ref : std_logic_vector(27 downto 0);

  -- Signals for the synchronizer
  signal trig_valid_sys_d1, trig_valid_sys_d2 : std_logic;
  signal rst_from_sync, rst_from_sync_d1 : std_logic;
  signal trig_valid_ref : std_logic_vector(2 downto 0);
  signal trig_valid_back : std_logic_vector(2 downto 0);
  signal trig_valid_ref_p1 : std_logic;
  
begin  -- architecture rtl

  -- Get trigger time into internal registers
  trig_regs: process (clk_sys_i) is
  begin  -- process trig_regs
    if clk_sys_i'event and clk_sys_i = '1' then
     if rst_n_i='0' then
      trig_utc <= (others=>'0');
      trig_cycles <= (others=>'0');
     elsif trig_valid_i='1' then
      trig_utc <= trig_tai_i;
      trig_cycles <= trig_cycles_i;
     end if; 
    end if;
  end process trig_regs;

  -- Synchronizer to pass UTC register data to the reference clock domain
  -- This synchronizer is made with the following four processes

  -- First one FF with async reset, still in the clk_sys_i domain
  sync_first_ff: process (clk_sys_i, rst_n_i, rst_from_sync)
  begin
   if rst_n_i='0' or rst_from_sync='1' then
     trig_valid_sys_d1 <= '0';
   elsif clk_sys_i'event and clk_sys_i='1' then
     if trig_valid_i='1' then
       trig_valid_sys_d1 <= '1';
     end if;
   end if;
  end process sync_first_ff;

  -- OK this is just for the UTC and cycle registers to have time to settle
  -- in the pathological case of a very fast ref clock and very long
  -- combinational delays in the UTC and cycle registers
  delay_sys: process (clk_sys_i)
  begin
   if clk_sys_i'event and clk_sys_i='1' then
     trig_valid_sys_d2 <= trig_valid_sys_d1;
   end if;
  end process delay_sys;

  -- Then three FFs to take the strobe safely into the clk_ref_i domain
  sync_ref: process (clk_ref_i)
  begin
   if clk_ref_i'event and clk_ref_i='1' then
    trig_valid_ref <= trig_valid_ref(1 downto 0) & trig_valid_sys_d2;
    trig_valid_ref_p1 <= trig_valid_ref(1) and not trig_valid_ref(2);
   end if;
  end process sync_ref;

  -- And then back into the clk_sys_i domain
  sync_sys: process (clk_sys_i)
  begin
   if clk_sys_i'event and clk_sys_i='1' then
     trig_valid_back <= trig_valid_back(1 downto 0) & trig_valid_ref(2);
     rst_from_sync <= trig_valid_back(2);
     rst_from_sync_d1 <= rst_from_sync;
   end if;
  end process sync_sys;

  -- Now get the trig registers into the clk_ref_i domain
  trig_regs_ref: process (clk_ref_i)
  begin
   if clk_ref_i'event and clk_ref_i='1' then
     if trig_valid_ref_p1='1' then
      trig_utc_ref <= trig_utc;
      trig_cycles_ref <= trig_cycles;
     end if;
   end if;
  end process trig_regs_ref;

  -- Notify we're ready to receive another trigger time write
  -- Having the reset set trig_ready_o to '1' is a kludge.
  -- A proper state machine would be better.
  ready_for_trig: process (rst_n_i, clk_sys_i)
  begin
   if rst_n_i='0' then
     trig_ready_o <= '1';
   elsif clk_sys_i'event and clk_sys_i='1' then
     if trig_valid_i='1' then
       trig_ready_o <= '0';
     elsif rst_from_sync_d1='1' and rst_from_sync='0' then
     -- falling edge of reset_from_sync
       trig_ready_o <= '1';
     end if;
   end if;
  end process ready_for_trig;

  -- Produce output
  -- Note rst_n_i is used as an async reset because it comes from the
  -- clk_sys_i domain. Not the most elegant but it ensures no glitches
  -- in the output after startup.
  -- This block actually creates a pulse one tick after the programmed
  -- time matches the current time. Changing it for complete
  -- coincidence would be trivial.
  gen_out: process (rst_n_i, clk_ref_i)
  begin
   if rst_n_i='0' then
    pulse_o <= '0';
   elsif clk_ref_i'event and clk_ref_i='1' then
    if tm_time_valid_i ='0' then
      pulse_o <= '0';
    elsif tm_tai_i=trig_utc_ref and tm_cycles_i=trig_cycles_ref then
      pulse_o <= '1';
    else
      pulse_o <= '0';
    end if;
   end if;
  end process gen_out;
  
end architecture rtl;
