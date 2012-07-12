-------------------------------------------------------------------------------
-- Title      : 16-bit transmit path for 1000Base-X PCS
-- Project    : White Rabbit MAC/Endpoint
-------------------------------------------------------------------------------
-- File       : ep_tx_pcs_16bit.vhd
-- Author     : Tomasz Włostowski
-- Company    : CERN BE-CO-HT section
-- Created    : 2009-06-16
-- Last update: 2012-07-12
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Module implements a 16-bit transmit path for 802.3z 1000BaseX PCS.
-- This block interfaces the Ethernet framer to TX PMA (Physical Medium Attachment).
-- It performs preamble generation, insertion of idle patterns, all the low-level
-- signalling. Strobing signal for taking TX timestamps is also generated.
--
-- Module uses two separate clocks: 62.5 MHz phy_tx_clk_i
-- (Transmit clock for PHY) which clocks 8b10b signalling layer, and an async
-- (clk_sys_i) which is used for data exchange with the rest of MAC data path. Data
-- exchange between these clock domains is done using an async FIFO.
-------------------------------------------------------------------------------
--
-- Copyright (c) 2011 CERN / BE-CO-HT
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
-- Date        Version  Author   Description
-- 2011-10-15  0.2      twlostow 16-bit version for Virtex 6 GTX
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.gencores_pkg.all;
use work.genram_pkg.all;
use work.endpoint_private_pkg.all;


entity ep_tx_pcs_16bit is

  port (
-- reset (synchronous to clk_sys_i, active LO)
    rst_n_i : in std_logic;

-- system clock (faster than reference)
    clk_sys_i : in std_logic;

-------------------------------------------------------------------------------
-- TX Framer inteface
-------------------------------------------------------------------------------    

-- TX Fabric input
    pcs_fab_i : in t_ep_internal_fabric;

-- HI pulse indicates an error during transmission of a frame (buffer underrun)
    pcs_error_o : out std_logic;

-- HI indicates that the PCS is busy (transmitting a frame or during autonegotiation)
    pcs_busy_o : out std_logic;

-- HI indicates that PCS FIFO is almost full.
    pcs_dreq_o : out std_logic;

-------------------------------------------------------------------------------
-- WB controller control signals
-------------------------------------------------------------------------------

-- Transmit Control Register, EN_PCS field
    mdio_mcr_pdown_i      : in std_logic;
-- Transmit Control Register, TX_CAL field
    mdio_wr_spec_tx_cal_i : in std_logic;

-- autonegotiation control
    an_tx_en_i  : in std_logic;
    an_tx_val_i : in std_logic_vector(15 downto 0);

-- Timestamp strobe
    timestamp_trigger_p_a_o : out std_logic;

-- RMON counters
    rmon_o : inout t_rmon_triggers;

-------------------------------------------------------------------------------
-- PHY Interface
-------------------------------------------------------------------------------

    phy_tx_clk_i       : in  std_logic;
    phy_tx_data_o      : out std_logic_vector(15 downto 0);
    phy_tx_k_o         : out std_logic_vector(1 downto 0);
    phy_tx_disparity_i : in  std_logic;
    phy_tx_enc_err_i   : in  std_logic
    );

end ep_tx_pcs_16bit;


architecture behavioral of ep_tx_pcs_16bit is

-- TX state machine definitions
  type t_tbif_tx_state is (TX_COMMA_IDLE, TX_CAL, TX_CR12, TX_CR34, TX_SPD_PREAMBLE, TX_DATA, TX_PREAMBLE, TX_SFD, TX_EPD, TX_EXTEND, TX_GEN_ERROR);

-- TX state machine signals

  signal tx_is_k            : std_logic_vector(1 downto 0);
  signal tx_odata_reg       : std_logic_vector(15 downto 0);
  signal tx_catch_disparity : std_logic;
  signal tx_state           : t_tbif_tx_state;
  signal tx_cntr            : unsigned(3 downto 0);
  signal tx_cr_alternate    : std_logic;

-- TX clock alignment FIFO signals
  signal fifo_packed_in, fifo_packed_out : std_logic_vector(17 downto 0);
  signal fifo_empty                      : std_logic;
  signal fifo_almost_empty               : std_logic;
  signal fifo_almost_full                : std_logic;
  signal fifo_enough_data                : std_logic;
  signal fifo_wr                         : std_logic;
  signal fifo_rd                         : std_logic := '0';
  signal fifo_ready                      : std_logic;
  signal fifo_clear_n                    : std_logic;
  signal fifo_read_int                   : std_logic;
  signal fifo_fab                        : t_ep_internal_fabric;

  signal tx_busy            : std_logic;
  signal tx_error           : std_logic;
  signal reset_synced_txclk : std_logic;

  signal mdio_mcr_pdown_synced : std_logic;
  signal s_one                 : std_logic := '1';

  signal an_tx_en_synced : std_logic;
begin

  U_sync_an_tx_en : gc_sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => phy_tx_clk_i,
      rst_n_i  => rst_n_i,
      data_i   => an_tx_en_i,
      synced_o => an_tx_en_synced);

  U_sync_pcs_busy_o : gc_sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => clk_sys_i,
      rst_n_i  => rst_n_i,
      data_i   => tx_busy,
      synced_o => pcs_busy_o);

  U_sync_pcs_error_o : gc_sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => clk_sys_i,
      rst_n_i  => rst_n_i,
      data_i   => tx_error,
      ppulse_o => pcs_error_o);

  U_sync_tx_reset : gc_sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => phy_tx_clk_i,
      rst_n_i  => '1',
      data_i   => rst_n_i,
      synced_o => reset_synced_txclk);

  U_sync_power_down : gc_sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => phy_tx_clk_i,
      rst_n_i  => '1',
      data_i   => mdio_mcr_pdown_i,
      synced_o => mdio_mcr_pdown_synced);

  phy_tx_data_o <= tx_odata_reg;
  phy_tx_k_o    <= tx_is_k;

-------------------------------------------------------------------------------
-- Clock alignment FIFO
-------------------------------------------------------------------------------  

  fifo_clear_n <= '0' when (rst_n_i = '0') or (mdio_mcr_pdown_synced = '1') else '1';

  f_pack_fifo_contents(pcs_fab_i, fifo_packed_in, fifo_wr, true);

  fifo_read_int <= fifo_rd and not (fifo_fab.eof or fifo_fab.error or fifo_fab.sof);

  U_TX_FIFO : generic_async_fifo
    generic map (
      g_data_width             => 18,
      g_size                   => 64,
      g_with_rd_empty          => true,
      g_with_rd_almost_empty   => true,
      g_with_wr_almost_full    => true,
      g_almost_empty_threshold => 20,
      g_almost_full_threshold  => 58)  -- fixme: make this a generic (or WB register)
    port map (
      rst_n_i           => fifo_clear_n,
      clk_wr_i          => clk_sys_i,
      d_i               => fifo_packed_in,
      we_i              => fifo_wr,
      wr_empty_o        => open,
      wr_full_o         => open,
      wr_almost_empty_o => open,
      wr_almost_full_o  => fifo_almost_full,
      wr_count_o        => open,
      clk_rd_i          => phy_tx_clk_i,
      q_o               => fifo_packed_out,
      rd_i              => fifo_read_int,
      rd_empty_o        => fifo_empty,
      rd_full_o         => open,
      rd_almost_empty_o => fifo_almost_empty,
      rd_almost_full_o  => open,
      rd_count_o        => open);

  fifo_enough_data <= not fifo_almost_empty;

  p_gen_fifo_ready_flag : process(phy_tx_clk_i)
  begin
    if rising_edge(phy_tx_clk_i) then
      fifo_ready <= fifo_read_int;
    end if;
  end process;

  f_unpack_fifo_contents(fifo_packed_out, fifo_ready, fifo_fab, true);

  -----------------------------------------------------------------------------
  -- TX PCS state machine
  -----------------------------------------------------------------------------
  p_tx_fsm : process (phy_tx_clk_i)
  begin
    
    if rising_edge(phy_tx_clk_i) then

-- The PCS is reset or disabled
      if(reset_synced_txclk = '0' or mdio_mcr_pdown_synced = '1') then
        tx_state           <= TX_COMMA_IDLE;
        timestamp_trigger_p_a_o  <= '0';
        fifo_rd            <= '0';
        tx_error           <= '0';
        tx_odata_reg       <= (others => '0');
        tx_is_k            <= "00";
        tx_cr_alternate    <= '0';
        tx_catch_disparity <= '0';
        tx_cntr            <= (others => '0');
        rmon_o.tx_underrun <= '0';
      else
        case tx_state is
-------------------------------------------------------------------------------
-- State COMMA: sends the /I/ sequence (K28.5 + D5.6/D16.2)
-------------------------------------------------------------------------------            
          when TX_COMMA_IDLE =>

            -- clear the RMON/error pulse after 2 cycles (DATA->COMMA->IDLE) to
            -- make sure is't long enough to trigger the event counter
            rmon_o.tx_underrun <= '0';
            tx_error           <= '0';

            tx_is_k                   <= "10";
            tx_odata_reg(15 downto 8) <= c_K28_5;

            if (phy_tx_disparity_i = '1' and tx_catch_disparity = '1') then
              tx_odata_reg(7 downto 0) <= c_d5_6;
            else
              tx_odata_reg(7 downto 0) <= c_d16_2;
            end if;


-- endpoint wants to send Config_Reg
            if(an_tx_en_synced = '1') then
              tx_state        <= TX_CR12;
              tx_cr_alternate <= '0';
              fifo_rd         <= '0';

-- we've got a new frame in the FIFO
            elsif (fifo_fab.sof = '1' and fifo_ready = '1' and tx_cntr = "0000")then
              fifo_rd  <= '0';
              tx_state <= TX_SPD_PREAMBLE;
              tx_cntr  <= "0001";

-- host requested a calibration pattern
            elsif(mdio_wr_spec_tx_cal_i = '1') then
              tx_state        <= TX_CAL;
              fifo_rd         <= '0';
              tx_cr_alternate <= '0';
            else
-- continue sending idle sequences and checking if something has arrived in the
-- FIFO
              if(tx_cntr /= "0000") then
                fifo_rd <= '0';
              else
                fifo_rd <= (not fifo_empty) and fifo_enough_data;
              end if;
              tx_state <= TX_COMMA_IDLE;
            end if;

            tx_catch_disparity <= '0';

            if(tx_cntr /= "0000") then
              tx_cntr <= tx_cntr - 1;
            end if;
-------------------------------------------------------------------------------
-- State: CAL: transmit the calibration sequence
-------------------------------------------------------------------------------

          when TX_CAL =>
            tx_is_k         <= "11";
            tx_odata_reg    <= c_k28_7 & c_k28_7;
            tx_cr_alternate <= '1';
            if(mdio_wr_spec_tx_cal_i = '0' and tx_cr_alternate = '1') then
              tx_state <= TX_COMMA_IDLE;
            end if;

-------------------------------------------------------------------------------
-- States: CR1, CR2, CR3, CR4: send the /C/ Configuration code set
-------------------------------------------------------------------------------

          when TX_CR12 =>
            fifo_rd <= not fifo_empty;

            tx_is_k                   <= "10";
            tx_odata_reg(15 downto 8) <= c_k28_5;

            if (tx_cr_alternate = '1') then
              tx_odata_reg(7 downto 0) <= c_d21_5;
            else
              tx_odata_reg(7 downto 0) <= c_d2_2;
            end if;

            tx_cr_alternate <= not tx_cr_alternate;
            tx_state        <= TX_CR34;

          when TX_CR34 =>
            fifo_rd <= not fifo_empty;

            tx_is_k                   <= "00";
            tx_odata_reg(15 downto 8) <= an_tx_val_i(7 downto 0);
            tx_odata_reg(7 downto 0)  <= an_tx_val_i(15 downto 8);

            if(an_tx_en_synced = '1') then
              tx_state <= TX_CR12;
            else
              tx_state <= TX_COMMA_IDLE;
            end if;


-------------------------------------------------------------------------------
-- State SPD: sends a start-of-packet delimeter
-------------------------------------------------------------------------------
          when TX_SPD_PREAMBLE =>
            fifo_rd      <= '0';
            tx_is_k      <= "10";
            tx_odata_reg <= c_k27_7 & c_preamble_char;
            tx_state     <= TX_PREAMBLE;

-------------------------------------------------------------------------------
-- State PREAMBLE: produces an Ethernet preamble
-------------------------------------------------------------------------------
          when TX_PREAMBLE =>
            tx_is_k      <= "00";
            tx_odata_reg <= c_preamble_char & c_preamble_char;

            if (tx_cntr = "0000") then
              tx_state          <= TX_SFD;
              fifo_rd           <= '1';
            end if;

            tx_cntr <= tx_cntr - 1;

-------------------------------------------------------------------------------
-- State SFD: outputs the start-of-frame delimeter (last byte of the preamble)
-------------------------------------------------------------------------------            
          when TX_SFD =>
            tx_is_k      <= "00";
            tx_odata_reg <= c_preamble_char & c_preamble_sfd;
            timestamp_trigger_p_a_o <= '1';
            tx_state     <= TX_DATA;

          when TX_DATA =>

            if((fifo_empty = '1' or fifo_fab.error = '1') and fifo_fab.eof = '0') then  -- FIFO underrun?
              tx_odata_reg       <= c_k30_7 & c_k23_7;  -- emit error propagation code
              tx_is_k            <= "11";
              tx_state           <= TX_GEN_ERROR;
              tx_error           <= not fifo_fab.error;
              rmon_o.tx_underrun <= '1';
              fifo_rd            <= '0';
            else

              if(fifo_fab.bytesel = '1') then
                tx_odata_reg <= fifo_fab.DATA(15 downto 8) & c_k29_7;
                tx_is_k      <= "01";
              else
                tx_odata_reg <= fifo_fab.DATA;
                tx_is_k      <= "00";
              end if;

              if (fifo_fab.eof = '1') then
                if(fifo_fab.bytesel = '1') then
                  tx_state <= TX_EXTEND;
                else
                  tx_state <= TX_EPD;
                end if;
                fifo_rd <= '0';
              end if;
            end if;

-------------------------------------------------------------------------------
-- State EPD: send End-of-frame delimeter
-------------------------------------------------------------------------------
          when TX_EPD =>
            timestamp_trigger_p_a_o <= '0';
            tx_is_k            <= "11";
            tx_odata_reg       <= c_k29_7 & c_k23_7;
            tx_catch_disparity <= '1';
            tx_cntr            <= "1000";
            tx_state           <= TX_COMMA_IDLE;

--------------------------------------------------------------------------------
-- State EXTEND: send the carrier extension
-------------------------------------------------------------------------------
          when TX_EXTEND =>
            timestamp_trigger_p_a_o <= '0';
            tx_is_k            <= "11";
            tx_odata_reg       <= c_k23_7 & c_k23_7;
            tx_catch_disparity <= '1';
            tx_cntr            <= "0100";
            tx_state           <= TX_COMMA_IDLE;

-------------------------------------------------------------------------------
-- State GEN_ERROR: entered when an error occured. Just terminates the frame.
-------------------------------------------------------------------------------            
          when TX_GEN_ERROR =>
            tx_state <= TX_EPD;

        end case;
      end if;
    end if;
  end process;

  gen_tx_busy : process(tx_state)
  begin
    case (tx_state) is
      when TX_CR12 =>
        tx_busy <= '0';
      when TX_CR34 =>
        tx_busy <= '0';
      when TX_COMMA_IDLE =>
        tx_busy <= '0';
      when others =>
        tx_busy <= '1';
    end case;
  end process;

--  tx_busy    <= '1' when (fifo_empty = '0') or (tx_state /= TX_COMMA_IDLE) else '0';
  pcs_dreq_o <= not fifo_almost_full;
end behavioral;


