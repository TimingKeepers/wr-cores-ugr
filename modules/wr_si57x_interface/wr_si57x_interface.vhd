-------------------------------------------------------------------------------
-- Title      : Silicon Labs Si57x oscillator I2C controller
-- Project    : White Rabbit
-------------------------------------------------------------------------------
-- File       : wr_si57x_interface.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-Co-HT
-- Created    : 2010-04-26
-- Last update: 2013-03-19
-- Platform   : FPGA-generic
-- Standard   : VHDL '93
-------------------------------------------------------------------------------
-- Description: Silabs Si57x series oscillator hardware interface for the WR
-- Core SoftPLL, allowing it to discipline Si57x oscillators to WR clock.
-- The core performs two tasks:
-- - allows the WR core to access I2C registers of attached Si57x oscillator
--   through bit-banging (e.g. to set base frequency)
-- - controls the Si57x PLL tuning in atomic (HW-only) way via tm_dac_value port.
--   The tuning value is added to pre-programmed RFREQ bias value set via
--   Wishbone.
-------------------------------------------------------------------------------
--
-- Copyright (c) 2013 CERN
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
-------------------------------------------------------------------------------

library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wishbone_pkg.all;
use work.si570_wbgen2_pkg.all;

entity wr_si57x_interface is
  
  generic (
    g_simulation : integer := 0);

  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    -- WR Core timing interface: aux clock tune port
    tm_dac_value_i    : in std_logic_vector(23 downto 0) := x"000000";
    tm_dac_value_wr_i : in std_logic := '0';

    -- I2C bus: output enable (active low)
    scl_pad_oen_o : out std_logic;
    sda_pad_oen_o : out std_logic;

    -- I2C bus: input pads
    scl_pad_i : in std_logic;
    sda_pad_i : in std_logic;

    -- Wishbone
    wb_adr_i   : in  std_logic_vector(c_wishbone_address_width-1 downto 0)   := (others => '0');
    wb_dat_i   : in  std_logic_vector(c_wishbone_data_width-1 downto 0)      := (others => '0');
    wb_dat_o   : out std_logic_vector(c_wishbone_data_width-1 downto 0);
    wb_sel_i   : in  std_logic_vector(c_wishbone_address_width/8-1 downto 0) := (others => '0');
    wb_we_i    : in  std_logic                                               := '0';
    wb_cyc_i   : in  std_logic                                               := '0';
    wb_stb_i   : in  std_logic                                               := '0';
    wb_ack_o   : out std_logic;
    wb_err_o   : out std_logic;
    wb_stall_o : out std_logic
    );

end wr_si57x_interface;

architecture rtl of wr_si57x_interface is

  component si570_if_wb
    port (
      rst_n_i    : in  std_logic;
      clk_sys_i  : in  std_logic;
      wb_adr_i   : in  std_logic_vector(1 downto 0);
      wb_dat_i   : in  std_logic_vector(31 downto 0);
      wb_dat_o   : out std_logic_vector(31 downto 0);
      wb_cyc_i   : in  std_logic;
      wb_sel_i   : in  std_logic_vector(3 downto 0);
      wb_stb_i   : in  std_logic;
      wb_we_i    : in  std_logic;
      wb_ack_o   : out std_logic;
      wb_stall_o : out std_logic;
      regs_i     : in  t_si570_in_registers;
      regs_o     : out t_si570_out_registers);
  end component;

  signal regs_in  : t_si570_in_registers;
  signal regs_out : t_si570_out_registers;

  signal new_rfreq                            : std_logic;
  signal rfreq_base, rfreq_adj, rfreq_current : unsigned(37 downto 0);

  signal i2c_tick    : std_logic;
  signal i2c_divider : unsigned(4 downto 0);

  signal scl_int : std_logic;
  signal sda_int : std_logic;

  signal seq_count : unsigned(8 downto 0);


  type t_i2c_transaction is (START, STOP, SEND_BYTE);

  type t_state is (IDLE, SI_START0, SI_START1, SI_START2, SI_ADDR0, SI_ADDR1, SI_ADDR2, SI_REG0, SI_REG1, SI_REG2, SI_RF0, SI_RF1, SI_RF2, SI_RF3, SI_RF4, SI_STOP0, SI_STOP1, SI_STOP2, SI_FREEZE0, SI_FREEZE2);

  signal state : t_state;

  signal scl_out_host, scl_out_fsm : std_logic;
  signal sda_out_host, sda_out_fsm : std_logic;

  signal n1 : std_logic_vector(1 downto 0);

  procedure f_i2c_iterate(tick : std_logic; signal counter : inout unsigned; value : std_logic_vector(7 downto 0); trans_type : t_i2c_transaction; signal scl : out std_logic; signal sda : out std_logic; signal state_var : out t_state; next_state : t_state) is
    variable last : boolean;
  begin

    last := false;

    if(tick = '0') then
      return;
    end if;


    case trans_type is
      when START =>
        case counter(1 downto 0) is
          -- states 0..2: start condition
          when "00" =>
            scl <= '1';
            sda <= '1';
          when "01" =>
            sda <= '0';
          when "10" =>
            scl  <= '0';
            last := true;
          when others => null;
        end case;

      when STOP =>
        case counter(1 downto 0) is
          -- states 0..2: start condition
          when "00" =>
            sda <= '0';
          when "01" =>
            scl <= '1';
          when "10" =>
            sda  <= '1';
            last := true;
          when others => null;
        end case;
        
      when SEND_BYTE =>
        
        case counter(1 downto 0) is
          when "00" =>
            sda <= value(7-to_integer(counter(4 downto 2)));
          when "01" =>
            scl <= '1';
          when "10" =>
            scl <= '0';
            if(counter(5) = '1') then
              last := true;
            end if;
          when others => null;
        end case;
    end case;

    if(last) then
      state_var <= next_state;
      counter   <= "000000000";
    else
      counter <= counter + 1;
    end if;
    
  end f_i2c_iterate;

  
begin  -- rtl

  U_WB_Slave : si570_if_wb
    port map (
      rst_n_i    => rst_n_i,
      clk_sys_i  => clk_sys_i,
      wb_adr_i   => wb_adr_i(3 downto 2),
      wb_dat_i   => wb_dat_i,
      wb_dat_o   => wb_dat_o,
      wb_cyc_i   => wb_cyc_i,
      wb_sel_i   => wb_sel_i,
      wb_stb_i   => wb_stb_i,
      wb_we_i    => wb_we_i,
      wb_ack_o   => wb_ack_o,
      wb_stall_o => wb_stall_o,
      regs_i     => regs_in,
      regs_o     => regs_out);

  rfreq_base(31 downto 0)  <= unsigned(regs_out.rfreql_o);
  rfreq_base(37 downto 32) <= unsigned(regs_out.rfreqh_o(5 downto 0));

  rfreq_adj (15 downto 0)  <= unsigned(tm_dac_value_i(15 downto 0));
  rfreq_adj (37 downto 16) <= (others => tm_dac_value_i(15));

  n1 <= regs_out.rfreqh_o(7 downto 6);

  p_rfreq : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        new_rfreq <= '0';
      else
        if(tm_dac_value_wr_i = '1') then
          rfreq_current <= rfreq_base + rfreq_adj;
        end if;
        new_rfreq <= tm_dac_value_wr_i;
      end if;
    end if;
  end process;

  p_i2c_divider : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        i2c_divider <= (others => '0');
        i2c_tick    <= '0';
      else
        i2c_divider <= i2c_divider + 1;
        if(i2c_divider = 0) then
          i2c_tick <= '1';
        else
          i2c_tick <= '0';
        end if;
      end if;
    end if;
  end process;

  p_i2c_fsm : process(clk_sys_i)
    variable i2c_last : boolean;
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        state       <= IDLE;
        seq_count   <= (others => '0');
        scl_out_fsm <= '1';
        sda_out_fsm <= '1';
      else
        case state is
          when IDLE =>
            if(new_rfreq = '1') then
              state <= SI_START0;
            end if;

          when SI_START0 =>
            f_i2c_iterate(i2c_tick, seq_count, x"00", START, scl_out_fsm, sda_out_fsm, state, SI_ADDR0);
          when SI_ADDR0 =>
            f_i2c_iterate(i2c_tick, seq_count, x"aa", SEND_BYTE, scl_out_fsm, sda_out_fsm, state, SI_REG0);
          when SI_REG0 =>
            f_i2c_iterate(i2c_tick, seq_count, x"87", SEND_BYTE, scl_out_fsm, sda_out_fsm, state, SI_FREEZE0);
          when SI_FREEZE0 =>
            f_i2c_iterate(i2c_tick, seq_count, x"20", SEND_BYTE, scl_out_fsm, sda_out_fsm, state, SI_STOP0);
          when SI_STOP0 =>
            f_i2c_iterate(i2c_tick, seq_count, x"00", STOP, scl_out_fsm, sda_out_fsm, state, SI_START1);
          when SI_START1 =>
            f_i2c_iterate(i2c_tick, seq_count, x"00", START, scl_out_fsm, sda_out_fsm, state, SI_ADDR1);
          when SI_ADDR1 =>
            f_i2c_iterate(i2c_tick, seq_count, x"aa", SEND_BYTE, scl_out_fsm, sda_out_fsm, state, SI_REG1);
          when SI_REG1 =>
            f_i2c_iterate(i2c_tick, seq_count, x"08", SEND_BYTE, scl_out_fsm, sda_out_fsm, state, SI_RF0);
          when SI_RF0 =>
            f_i2c_iterate(i2c_tick, seq_count, n1 & std_logic_vector(rfreq_current(37 downto 32)), SEND_BYTE, scl_out_fsm, sda_out_fsm, state, SI_RF1);

          when SI_RF1 =>
            f_i2c_iterate(i2c_tick, seq_count, std_logic_vector(rfreq_current(31 downto 24)), SEND_BYTE, scl_out_fsm, sda_out_fsm, state, SI_RF2);

          when SI_RF2 =>
            f_i2c_iterate(i2c_tick, seq_count, std_logic_vector(rfreq_current(23 downto 16)), SEND_BYTE, scl_out_fsm, sda_out_fsm, state, SI_RF3);

          when SI_RF3 =>
            f_i2c_iterate(i2c_tick, seq_count, std_logic_vector(rfreq_current(15 downto 8)), SEND_BYTE, scl_out_fsm, sda_out_fsm, state, SI_RF4);

          when SI_RF4 =>
            f_i2c_iterate(i2c_tick, seq_count, std_logic_vector(rfreq_current(7 downto 0)), SEND_BYTE, scl_out_fsm, sda_out_fsm, state, SI_STOP1);

          when SI_STOP1 =>
            f_i2c_iterate(i2c_tick, seq_count, x"00", STOP, scl_out_fsm, sda_out_fsm, state, SI_START2);
          when SI_START2 =>
            f_i2c_iterate(i2c_tick, seq_count, x"00", START, scl_out_fsm, sda_out_fsm, state, SI_ADDR2);
          when SI_ADDR2 =>
            f_i2c_iterate(i2c_tick, seq_count, x"aa", SEND_BYTE, scl_out_fsm, sda_out_fsm, state, SI_REG2);
          when SI_REG2 =>
            f_i2c_iterate(i2c_tick, seq_count, x"87", SEND_BYTE, scl_out_fsm, sda_out_fsm, state, SI_FREEZE2);
          when SI_FREEZE2 =>
            f_i2c_iterate(i2c_tick, seq_count, x"00", SEND_BYTE, scl_out_fsm, sda_out_fsm, state, SI_STOP2);
          when SI_STOP2 =>
            f_i2c_iterate(i2c_tick, seq_count, x"00", STOP, scl_out_fsm, sda_out_fsm, state, IDLE);
            
          when others => null;
        end case;
      end if;
    end if;
  end process;

  regs_in.gpsr_scl_i <= scl_pad_i;
  regs_in.gpsr_sda_i <= sda_pad_i;

  p_host_i2c : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0') then
        scl_out_host <= '1';
        sda_out_host <= '1';
      else
        if(regs_out.gpsr_scl_load_o = '1' and regs_out.gpsr_scl_o = '1') then
          scl_out_host <= '1';
        end if;

        if(regs_out.gpsr_sda_load_o = '1' and regs_out.gpsr_sda_o = '1') then
          sda_out_host <= '1';
        end if;

        if(regs_out.gpcr_scl_o = '1') then
          scl_out_host <= '0';
        end if;

        if(regs_out.gpcr_sda_o = '1') then
          sda_out_host <= '0';
        end if;
        
      end if;
    end if;
  end process;

  p_mux_i2c : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(state = IDLE) then
        scl_pad_oen_o <= scl_out_host;
        sda_pad_oen_o <= sda_out_host;
      else
        scl_pad_oen_o <= scl_out_fsm;
        sda_pad_oen_o <= sda_out_fsm;
      end if;
    end if;
  end process;

  wb_err_o <= '0';
  
end rtl;
