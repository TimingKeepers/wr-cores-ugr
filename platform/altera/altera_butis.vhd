-------------------------------------------------------------------------------
-- Title      : Butis Altera clock alignment
-- Project    : White Rabbit
-------------------------------------------------------------------------------
-- File       : altera_butis.vhd
-- Author     : Wesley W. Terpstra
-- Company    : GSI
-- Created    : 2013-08-23
-- Last update: 2013-08-23
-- Platform   : Altera
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Aligns a 200 MHz clock to the PPS
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
-- 2013-08-23  1.0      terpstra  First stab at state machine
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.gencores_pkg.all;
use work.disparity_gen_pkg.all;

entity altera_butis is
  generic(
    g_select_bits : natural := 4;
    g_200_sel     : natural := 3;
    g_25_sel      : natural := 4);
  port(
    clk_ref_i   : in  std_logic;
    clk_25m_i   : in  std_logic;
    clk_scan_i  : in  std_logic;
    locked_i    : in  std_logic;
    pps_i       : in  std_logic; -- ref_clk
    phasedone_i : in  std_logic;
    phasesel_o  : out std_logic_vector(g_select_bits-1 downto 0);
    phasestep_o : out std_logic);
end altera_butis;

-- It is not possible to reliably align 200MHz and 125MHz directly.
-- The problem is that periods of 8ns and 5ns have a gcd=1ns.
-- So, any attempt to measure one clock with the other will have
--  some edges which are within 0.5ns of each other. Too fast.
--
-- Instead, we take the following approach:
--   Setup the PLL to output 125MHz, 200MHz, and 25MHz clocks.
--   Require that all are phase aligned when the PLL is locked.
--   This means that every 40ns they have a common rising edge.
--   Once we find the PPS on the 125MHz clock, latch it's (mod 8ns) period.
--   Now, compare a 25 MHz toggling flip-flop to the position of the PPS.
--   We can safely inspect the 12.5 MHz toggle signal in the 125MHz domain.
--   It will look like five '1's then five '0's, repeating.
--   We KNOW that the rising (and falling) edge of the toggle line-up
--     with the 200MHz clock, because the PLL starts up locked this way.
--   So, we repeatedly shift both the 200MHz and 25MHz clocks by 8ns.
--   Eventually, the five '1's will line up with the PPS.
--   Then we know the 200MHz clock is also aligned to the PPS.

architecture rtl of altera_butis is
  signal toggle_25m  : std_logic;
  signal clk25_shift : std_logic_vector(4 downto 0);
  signal clk25_reg   : std_logic_vector(4 downto 0);
  signal pps_count    : unsigned(3 downto 0);
  signal r_pps        : std_logic;
  
  signal saw_rise, saw_rise1, saw_rise2 : std_logic;
  signal saw_fall, saw_fall1, saw_fall2 : std_logic;
  signal clk25_scan1 : std_logic_vector(4 downto 0);
  signal clk25_scan2 : std_logic_vector(4 downto 0);
  signal rst         : std_logic_vector(2 downto 0);
  signal rst0        : std_logic;
  
  type t_state is (IDLE, PRIME_TRAP, WAIT_TRAP1, WAIT_TRAP2, SHIFT_125P, WAIT_LOW, WAIT_HIGH, DONE_125P);
  signal state       : t_state   := IDLE;
  signal prime       : std_logic;
  signal shift_count : unsigned(5 downto 0);
  signal fail_count  : unsigned(4 downto 0); -- 1 bit more than pps_count
  signal phasesel    : std_logic_vector(g_select_bits-1 downto 0);
  signal phasestep   : std_logic;
  
  constant phase_200m : std_logic_vector(g_select_bits-1 downto 0) := std_logic_vector(to_unsigned(g_200_sel, g_select_bits));
  constant phase_25m  : std_logic_vector(g_select_bits-1 downto 0) := std_logic_vector(to_unsigned(g_25_sel,  g_select_bits));
  constant phasesteps : unsigned(5 downto 0) := (others => '1'); -- 64*125ps = 8ns
  
  -- We ensure timing between these nodes via the state machine
  ATTRIBUTE altera_attribute : string;
  ATTRIBUTE altera_attribute OF rtl : architecture is 
    ("-name SDC_STATEMENT ""set_false_path -from {altera_butis:*|prime}    -to {altera_butis:*|saw_rise}"";"  &
     "-name SDC_STATEMENT ""set_false_path -from {altera_butis:*|prime}    -to {altera_butis:*|saw_fall}"";"  &
     "-name SDC_STATEMENT ""set_false_path -from {altera_butis:*|saw_rise} -to {altera_butis:*|saw_rise1}"";" &
     "-name SDC_STATEMENT ""set_false_path -from {altera_butis:*|saw_fall} -to {altera_butis:*|saw_fall1}""");
begin

  toggle : process(clk_25m_i) is
  begin
    if rising_edge(clk_25m_i) then
      toggle_25m <= not toggle_25m;
    end if;
  end process;

  sample : process(clk_ref_i) is
  begin
    if rising_edge(clk_ref_i) then
      clk25_shift <= clk25_shift(clk25_shift'length-2 downto 0) & toggle_25m;
      
      r_pps <= pps_i;
      if (pps_i = '1' and r_pps = '0') or pps_count = 0 then
        pps_count <= to_unsigned(9, pps_count'length);
      else
        pps_count <= pps_count - 1;
      end if;
      
      if pps_count = 1 then
        clk25_reg <= clk25_shift;
      end if;
    end if;
  end process;
  
  -- Unfortunately, phasedone_i is asynchronous to scanclk 
  -- Furthermore, the pulse might have duration less than one scanclk cycle.
  -- We need to detect both the rising and falling edge
  trap : process(phasedone_i, prime) is
  begin
    if prime = '1' then
      saw_rise <= '0';
    elsif rising_edge(phasedone_i) then
      saw_rise <= '1';
    end if;
    
    if prime = '1' then
      saw_fall <= '0';
    elsif falling_edge(phasedone_i) then
      saw_fall <= '1';
    end if;
  end process;

  sync : process(clk_scan_i, locked_i) is
  begin
    if locked_i = '0' then
      rst <= (others => '1');
    elsif rising_edge(clk_scan_i) then
      rst <= '0' & rst(rst'length-1 downto 1);
    end if;
    
    if rising_edge(clk_scan_i) then
      clk25_scan1 <= clk25_reg;
      clk25_scan2 <= clk25_scan1;
      saw_rise1 <= saw_rise;
      saw_rise2 <= saw_rise1;
      saw_fall1 <= saw_fall;
      saw_fall2 <= saw_fall1;
    end if;
  end process;
  
  rst0 <= rst(0);
  shift : process(clk_scan_i, rst0) is
  begin
    if rst0 = '1' then
      state       <= IDLE;
      prime       <= '0';
      shift_count <= (others => '1');
      fail_count  <= (others => '1');
      phasesel    <= (others => '1');
      phasestep   <= '0';
    elsif rising_edge(clk_scan_i) then
      case state is
        when IDLE =>
          if clk25_scan2 /= "11111" then
            fail_count <= fail_count - 1;
          else
            fail_count <= (others => '1');
          end if;
          
          if fail_count = 0 then
            phasesel <= phase_25m;
            shift_count <= phasesteps;
            state <= PRIME_TRAP;
          end if;
          
        when PRIME_TRAP =>
          prime <= '1';
          state <= WAIT_TRAP1;
	
        -- take some time to let the synchronizers clear
        when WAIT_TRAP1 =>
          state <= WAIT_TRAP2;
        
        when WAIT_TRAP2 =>
          prime <= '0';
          state <= SHIFT_125P;
        
        when SHIFT_125P => -- shift 125ps
          phasestep <= '1';
          state <= WAIT_LOW;
            
        when WAIT_LOW =>
          if saw_fall2 = '1' then
            phasestep <= '0';
            state <= WAIT_HIGH;
          end if;
	
	when WAIT_HIGH =>
	  if saw_rise2 = '1' then
            state <= DONE_125P;
	  end if;
        
        when DONE_125P =>
          if shift_count = 0 then
            if phasesel = phase_25m then
              phasesel <= phase_200m;
              shift_count <= phasesteps;
              state <= PRIME_TRAP;
            else
              state <= IDLE;
            end if;
          else
            state <= PRIME_TRAP;
            shift_count <= shift_count - 1;
          end if;
          
      end case;
    end if;
  end process;
  
  phasesel_o  <= phasesel;
  phasestep_o <= phasestep;

end rtl;
