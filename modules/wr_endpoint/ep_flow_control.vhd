-------------------------------------------------------------------------------
-- Title      : Ethernet Flow Control Unit
-- Project    : White Rabbit MAC/Endpoint
-------------------------------------------------------------------------------
-- File       : ep_flow_control.vhd
-- Author     : Tomasz Włostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2010-11-18
-- Last update: 2011-10-18
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Module implements the flow control unit, governing the both the
-- TX and RX path of the MAC. 
-------------------------------------------------------------------------------
--
-- Copyright (c) 2009 - 2011 CERN / BE-CO-HT
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
-- Revisions  :
-- Date        Version  Author    Description
-- 2010-11-18  0.4      twlostow  Initial release
-- 2011-02-07  0.5      twlostow  Tested on Spartan6 GTP
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.endpoint_private_pkg.all;

entity ep_flow_control is

  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

-- RX Pause: if 1, we have received a valid PAUSE frame. The delay is in "rx_pause_delay_i"
    rx_pause_p1_i    : in std_logic;
    rx_pause_delay_i : in std_logic_vector(15 downto 0);

-- TX Pause: if tx_pause_o == 1, the FC unit wants the MAC to send a PAUSE frame
-- with delay tx_pause_delay_o. The transmission should be acked by asserting
-- tx_pause_delay_o.
    tx_pause_o       : out std_logic;
    tx_pause_delay_o : out std_logic_vector(15 downto 0);
    tx_pause_ack_i   : in  std_logic;

-- TX flow enable: if 1, the TX part of the Endpoint is allowed to send packets
    tx_flow_enable_o : out std_logic;

-- Amount of data in the RX buffer (0 = empty, max = full), at which the flow control
-- is triggered
    rx_buffer_used_i : in std_logic_vector(7 downto 0);

    ep_fcr_txpause_i   : in std_logic;
    ep_fcr_rxpause_i   : in std_logic;
    ep_fcr_tx_thr_i    : in std_logic_vector(7 downto 0);
    ep_fcr_tx_quanta_i : in std_logic_vector(15 downto 0);

    rmon_rcvd_pause_o: out std_logic;
    rmon_sent_pause_o: out std_logic

    );

end ep_flow_control;

architecture behavioral of ep_flow_control is

  type t_rxpause_fsm_state is (S_CHECK_BUFFER, S_ISSUE_PAUSE, S_WAIT_COUNTER_EXPIRE);

  signal div512           : unsigned(4 downto 0);
  signal advance_counter  : std_logic;
  signal tx_pause_counter : unsigned(15 downto 0);
  signal rx_pause_counter : unsigned(15 downto 0);

  signal state : t_rxpause_fsm_state;
  
begin  -- behavioral

  rmon_rcvd_pause_o <= rx_pause_p1_i and ep_fcr_rxpause_i;
  
  gen_pause_timing : process (clk_sys_i, rst_n_i)
  begin  -- process
    if rising_edge(clk_sys_i) then
      if (rst_n_i = '0') then
        div512          <= (others => '0');
        advance_counter <= '0';
      else
        div512 <= div512 + 1;
        if(div512 = to_unsigned(0, div512'length)) then
          advance_counter <= '1';
        else
          advance_counter <= '0';
        end if;
      end if;
    end if;
  end process;


  rx_pause_proc : process (clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0') then
        tx_pause_counter <= (others => '0');
        tx_flow_enable_o <= '1';
      else
        if(ep_fcr_rxpause_i = '1') then
          if rx_pause_p1_i = '1' then
            tx_pause_counter <= unsigned(rx_pause_delay_i);
            tx_flow_enable_o <= '0';
          elsif (advance_counter = '1') then
            if(tx_pause_counter = to_unsigned(0, tx_pause_counter'length)) then
              tx_flow_enable_o <= '1';
            else
              tx_pause_counter <= tx_pause_counter - 1;
            end if;  -- if tx_pause_counter == 0
          end if;  -- if advance_counter = '1'
        else
          tx_flow_enable_o <= '1';
        end if;
      end if;
    end if;
  end process;

  tx_pause_proc : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if (rst_n_i = '0' or ep_fcr_txpause_i = '0') then
        rx_pause_counter <= (others => '0');
        state            <= S_CHECK_BUFFER;
        tx_pause_o       <= '0';
        rmon_sent_pause_o<= '0';
        tx_pause_delay_o <= (others => '0');
      else

        case state is
          when S_CHECK_BUFFER =>
            if(unsigned(rx_buffer_used_i) >= unsigned(ep_fcr_tx_thr_i)) then
              tx_pause_o       <= '1';
              rmon_sent_pause_o  <= '1';
              tx_pause_delay_o <= ep_fcr_tx_quanta_i;
              state            <= S_ISSUE_PAUSE;
            end if;

          when S_ISSUE_PAUSE =>
            rmon_sent_pause_o<='0';
            if(tx_pause_ack_i = '1') then
              tx_pause_o       <= '0';
              rx_pause_counter <= unsigned(ep_fcr_tx_quanta_i);
              state            <= S_WAIT_COUNTER_EXPIRE;
            end if;

          when S_WAIT_COUNTER_EXPIRE =>
            if(advance_counter = '1') then
              if(rx_pause_counter = to_unsigned(0, rx_pause_counter'length)) then
                state <= S_CHECK_BUFFER;
              else
                rx_pause_counter <= rx_pause_counter - 1;
              end if;
            end if;
          when others => null;
        end case;
      end if;
    end if;
  end process;

  
  
end behavioral;
