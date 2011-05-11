-------------------------------------------------------------------------------
-- Title      : 1000BaseT/X MAC Endpoint - receive path PCS for 1000BaseX
-- Project    : White Rabbit Switch
-------------------------------------------------------------------------------
-- File       : ep_tx_pcs_tbi.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-CO-HT section
-- Created    : 2009-06-16
-- Last update: 2011-02-01
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Module implements the transmit path for 802.3z 1000BaseX PCS.
-- This block interfaces the Ethernet framer to TX PMA (Physical Medium Attachment).
-- It performs preamble generation, insertion of idle patterns, all the low-level
-- signalling (including 8b10b coding). Strobing signal for taking TX timestamps
-- is also generated.
--
-- Module uses two separate clocks: 125 MHz tbi_tx_clk (or gtp_tx_clk)
-- (Transmit clock for PHY) which clocks 8b10b signalling layer, and 62.5 MHz
-- (clk_sys_i) which is used for data exchange with the rest of switch. Data
-- exchange between these clock domains  is done using an async FIFO.
--
-------------------------------------------------------------------------------
--
-- Copyright (c) 2009 Tomasz Wlostowski / CERN
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
-- 2009-06-16  0.1      twlostow Created (no error propagation supported yet)
-- 2010-04-06  0.2      twlostow Cleanup, new timestamping/LCR scheme
-- 2010-07-30  0.2      twlostow Fixed preamble length bug
-- 2010-11-18  0.4      twlostow Added support for Xilinx GTP transceivers.
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


library work;
use work.common_components.all;
use work.endpoint_pkg.all;
use work.genram_pkg.all;


entity ep_tx_pcs_tbi is
  generic(
    g_phy_mode : string := "TBI"
    );

  port (
-- reset (synchronous to refclk2, active LO)
    rst_n_i : in std_logic;

-- 62.5 MHz clock (refclk/2)
    clk_sys_i : in std_logic;

-------------------------------------------------------------------------------
-- TX Framer inteface
-------------------------------------------------------------------------------    

-- TX FIFO input
    pcs_data_i : in std_logic_vector(15 downto 0);

-- HI means that the frame has odd length and that currently transferred
-- word contains the last byte of the frame payload
    pcs_bytesel_i : in std_logic;

-- HI state begins transmission of the frame (the first word to be sent
-- has this line asserted
    pcs_sof_i : in std_logic;

-- HI means indicates the end of currently transmitted frame
    pcs_eof_i : in std_logic;

-- HI causes transmit abort (PCS injects /V/ ordered sets and terminates the frame)
    pcs_abort_i : in std_logic;

-- HI pulse indicates an error during transmission of a frame (buffer underrun)
    pcs_error_p_o : out std_logic;

-- HI indicates that the PCS is busy (transmitting a frame or during autonegotiation)
    pcs_busy_o : out std_logic;

-- HI indicates that pcs_data_i, pcs_bytesel_i, pcs_sof_i, pcs_eof_i,
-- pcs_abort_i contain valid values and should be written to the TX FIFO.
    pcs_valid_i : in std_logic;

-- HI indicates that PCS FIFO is almost full.
    pcs_fifo_almostfull_o : out std_logic;

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
    timestamp_stb_p_o : out std_logic;

-- RMON counters
    rmon_tx_underrun_o : out std_logic;

-------------------------------------------------------------------------------
-- Xilinx GTP interface (bypassing 8b10b encoder)
-------------------------------------------------------------------------------

    gtp_tx_clk_i       : in  std_logic;
    gtp_tx_data_o      : out std_logic_vector(7 downto 0);
    gtp_tx_k_o         : out std_logic;
    gtp_tx_disparity_i : in  std_logic;
    gtp_tx_enc_err_i   : in  std_logic;

-------------------------------------------------------------------------------                 -- TBI interface 
-------------------------------------------------------------------------------

-- TBI transmit clock
    tbi_txclk_i  : in  std_logic;
-- TBI 8b10b-encoded data output
    tbi_txdata_o : out std_logic_vector(9 downto 0)

    );

end ep_tx_pcs_tbi;



architecture  behavioral of ep_tx_pcs_tbi is

-- TX state machine definitions
  type t_tbif_tx_state is (TX_COMMA, TX_CAL, TX_CR1, TX_CR2, TX_CR3, TX_CR4, TX_SPD, TX_IDLE, TX_DATA, TX_PREAMBLE, TX_SFD, TX_EPD, TX_EXTEND, TX_GOTO_COMMA, TX_GEN_ERROR);

-- TX state machine signals

  signal tx_is_k, tx_enc_err, tx_disparity : std_logic;
  signal tx_catch_disparity                : std_logic;
  signal tx_odata_reg                      : std_logic_vector(7 downto 0);
  signal tx_state                          : t_tbif_tx_state;
  signal tx_preamble_cntr                  : unsigned(2 downto 0);
  signal tx_cr_alternate                   : std_logic;
  signal tx_fifo_rdreq                     : std_logic;
  signal tx_newframe                       : std_logic;

-- TX clock alignment FIFO signals

  signal tx_fifo_data_in, tx_fifo_data_out : std_logic_vector(19 downto 0);
  signal tx_fifo_msb, tx_fifo_lsb          : std_logic_vector(7 downto 0);
  signal tx_fifo_singlebyte, tx_fifo_end   : std_logic;
  signal tx_fifo_start                     : std_logic;
  signal tx_fifo_rdempty                   : std_logic;
  signal tx_fifo_almostempty : std_logic;
  
  signal tx_fifo_abort                     : std_logic;
  signal tx_rdreq_toggle                   : std_logic;
  signal tx_odd_length                     : std_logic;
  signal tx_fifo_clear_n                     : std_logic;

  signal fifo_rdy           : std_logic;
  signal tx_busy            : std_logic;
  signal tx_error           : std_logic;
  signal reset_synced_txclk : std_logic;

  signal txdata_encoded    : std_logic_vector(9 downto 0);
  signal txdata_encoded_d0 : std_logic_vector(9 downto 0);

  signal mdio_mcr_pdown_synced : std_logic;

  signal tx_clk : std_logic;

--  signal tx_fifo_usedw       : std_logic_vector(31 downto 0);
  signal tx_fifo_enough_data : std_logic;
  
  
  
begin


  
  U_sync_pcs_busy_o : sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => clk_sys_i,
      rst_n_i  => rst_n_i,
      data_i   => tx_busy,
      synced_o => pcs_busy_o,
      npulse_o => open,
      ppulse_o => open);

  U_sync_pcs_error_o : sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => clk_sys_i,
      rst_n_i  => rst_n_i,
      data_i   => tx_error,
      synced_o => open,
      npulse_o => open,
      ppulse_o => pcs_error_p_o);

  U_sync_tx_reset : sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => tx_clk,
      rst_n_i  => '1',
      data_i   => rst_n_i,
      synced_o => reset_synced_txclk,
      npulse_o => open,
      ppulse_o => open);

  U_sync_power_down : sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => tx_clk,
      rst_n_i  => '1',
      data_i   => mdio_mcr_pdown_i,
      synced_o => mdio_mcr_pdown_synced,
      npulse_o => open,
      ppulse_o => open);

  -----------------------------------------------------------------------------
  -- TBI version
  -----------------------------------------------------------------------------

  gen_tbi : if(g_phy_mode = "TBI") generate
    
    U_ENC : ep_enc_8b10b
      port map (
        clk_i     => tx_clk,
        rst_n_i   => reset_synced_txclk,
        ctrl_i    => tx_is_k,
        in_8b_i   => tx_odata_reg,
        err_o     => tx_enc_err,
        dispar_o  => tx_disparity,
        out_10b_o => txdata_encoded);

    

    tbi_txdata_o <= txdata_encoded;
    tx_clk       <= tbi_txclk_i;
    
  end generate gen_tbi;


  gen_gtp : if(g_phy_mode = "GTP") generate

    tx_disparity <= gtp_tx_disparity_i; 
    tx_enc_err   <= gtp_tx_enc_err_i;

    gtp_tx_data_o <= tx_odata_reg;
    gtp_tx_k_o    <= tx_is_k;

    tx_clk <= gtp_tx_clk_i;
  end generate gen_gtp;


-------------------------------------------------------------------------------
-- Clock alignment FIFO
-------------------------------------------------------------------------------  

  tx_fifo_data_in(15 downto 0) <= pcs_data_i;
  tx_fifo_data_in(16)          <= pcs_bytesel_i;
  tx_fifo_data_in(17)          <= pcs_abort_i;
  tx_fifo_data_in(18)          <= pcs_sof_i;
  tx_fifo_data_in(19)          <= pcs_eof_i;

  tx_fifo_clear_n <= '0' when (rst_n_i = '0') or (mdio_mcr_pdown_synced = '1') else '1';


  U_TX_FIFO: generic_async_fifo
    generic map (
      g_data_width             => 20,
      g_size                   => 64,
      g_with_rd_empty          => true,
      g_with_rd_full           => false,
      g_with_rd_almost_empty   => true,
      g_with_rd_almost_full    => false,
      g_with_rd_count          => true,
      g_with_wr_empty          => false,
      g_with_wr_full           => false,
      g_with_wr_almost_empty   => false,
      g_with_wr_almost_full    => true,
      g_with_wr_count          => false,
      g_almost_empty_threshold => 16,
      g_almost_full_threshold  => 60)   -- fixme: make this a generic (or WB register)
    port map (
      rst_n_i           => tx_fifo_clear_n, 
      clk_wr_i          => clk_sys_i,
      d_i               => tx_fifo_data_in,
      we_i              => pcs_valid_i,
      wr_empty_o        => open,
      wr_full_o         => open,
      wr_almost_empty_o => open,
      wr_almost_full_o  => pcs_fifo_almostfull_o,
      wr_count_o        => open,
      clk_rd_i          => tx_clk,
      q_o               => tx_fifo_data_out,
      rd_i              => tx_fifo_rdreq,
      rd_empty_o        => tx_fifo_rdempty,
      rd_full_o         => open,
      rd_almost_empty_o => tx_fifo_almostempty,
      rd_almost_full_o  => open,
      rd_count_o        => open);

  tx_fifo_enough_data <= not tx_fifo_almostempty;
  
  tx_fifo_msb        <= tx_fifo_data_out(15 downto 8);
  tx_fifo_lsb        <= tx_fifo_data_out(7 downto 0);
  tx_fifo_singlebyte <= tx_fifo_data_out(16);
  tx_fifo_abort      <= tx_fifo_data_out(17);
  tx_fifo_start      <= tx_fifo_data_out(18);
  tx_fifo_end        <= tx_fifo_data_out(19);
  
  -----------------------------------------------------------------------------
  -- Main TX PCS state machine
  -----------------------------------------------------------------------------

  p_tx_fsm : process (tx_clk)
  begin
    
    if rising_edge(tx_clk) then
-- PCS is reset or disabled
      if(reset_synced_txclk = '0' or mdio_mcr_pdown_synced = '1') then
        tx_state           <= TX_COMMA;
        timestamp_stb_p_o  <= '0';
        tx_fifo_rdreq      <= '0';
        tx_error           <= '0';
        tx_odata_reg       <= (others => '0');
        tx_is_k            <= '0';
        tx_busy            <= '0';
        tx_cr_alternate    <= '0';
        tx_newframe        <= '0';
        tx_catch_disparity <= '0';
        tx_preamble_cntr   <= (others => '0');
        tx_odd_length      <= '0';
        tx_rdreq_toggle    <= '0';
        rmon_tx_underrun_o <= '0';
        
      else
        
        case tx_state is

-------------------------------------------------------------------------------
-- State COMMA: sends K28.5 comma character (first byte of /I/ sequence)
-------------------------------------------------------------------------------            
          when TX_COMMA =>
            tx_is_k       <= '1';
            tx_odata_reg  <= c_K28_5;
            tx_state      <= TX_IDLE;
            tx_fifo_rdreq <= '0';
            fifo_rdy      <= tx_fifo_rdreq;

-------------------------------------------------------------------------------
-- State IDLE: sends the second code of the /I/ sequence with proper disparity\
-------------------------------------------------------------------------------
          when TX_IDLE =>

            -- clear the RMON/error pulse after 2 cycles (DATA->COMMA->IDLE) to
            -- make sure is't long enough to trigger the event counter
            rmon_tx_underrun_o <= '0';
            tx_error           <= '0';

-- endpoint wants to send Config_Reg
            if(an_tx_en_i = '1') then
              tx_busy         <= '1';
              tx_state        <= TX_CR1;
              tx_cr_alternate <= '0';
              tx_fifo_rdreq   <= '0';

-- we've got a new frame in the FIFO
            elsif (tx_fifo_start = '1' and fifo_rdy = '1')then
              tx_fifo_rdreq    <= '0';
              tx_newframe      <= '1';
              tx_state         <= TX_SPD;
              tx_preamble_cntr <= "101";
              tx_busy          <= '1';

-- host requested a calibration pattern
            elsif(mdio_wr_spec_tx_cal_i = '1') then
              tx_state        <= TX_CAL;
              tx_fifo_rdreq   <= '0';
              tx_busy         <= '1';
              tx_cr_alternate <= '0';
            else
-- continue sending idle sequences and checking if something has arrived in the
-- FIFO
              tx_fifo_rdreq <= (not tx_fifo_rdempty) and tx_fifo_enough_data;
              tx_state      <= TX_COMMA;
              tx_busy       <= '0';
            end if;

            tx_is_k <= '0';

-- check the disparity of the previously emitted code and choose whether to send
-- /I1/ or /I2/
            if (tx_disparity = '1' and tx_catch_disparity = '1') then
              tx_odata_reg <= c_d5_6;
            else
              tx_odata_reg <= c_d16_2;
            end if;

            tx_catch_disparity <= '0';

-------------------------------------------------------------------------------
-- State: CAL: transmit the calibration sequence
-------------------------------------------------------------------------------

          when TX_CAL =>
            tx_odata_reg <= c_k28_7;
            tx_is_k      <= '1';
            if(mdio_wr_spec_tx_cal_i = '0' and tx_cr_alternate = '1') then
              tx_state <= TX_COMMA;
            end if;

            tx_cr_alternate <= not tx_cr_alternate;

-------------------------------------------------------------------------------
-- States: CR1, CR2, CR3, CR4: send the /C/ Configuration code set
-------------------------------------------------------------------------------            

          when TX_CR1 =>
            tx_is_k      <= '1';
            tx_odata_reg <= c_k28_5;
            tx_state     <= TX_CR2;

          when TX_CR2 =>
            tx_is_k <= '0';
            if (tx_cr_alternate = '1') then
              tx_odata_reg <= c_d21_5;
            else
              tx_odata_reg <= c_d2_2;
            end if;
            tx_cr_alternate <= not tx_cr_alternate;
            tx_state        <= TX_CR3;

          when TX_CR3 =>
            tx_odata_reg <= an_tx_val_i(7 downto 0);
            tx_state     <= TX_CR4;

          when TX_CR4 =>
            tx_odata_reg <= an_tx_val_i(15 downto 8);

-- check if the autonegotiation control still wants the Config_Reg to be sent
            if(an_tx_en_i = '1') then
              tx_state <= TX_CR1;
            else
              tx_busy  <= '0';
              tx_state <= TX_COMMA;
            end if;

-------------------------------------------------------------------------------
-- State SPD: sends a start-of-packet delimeter
-------------------------------------------------------------------------------
          when TX_SPD =>
            tx_is_k      <= '1';
            tx_odata_reg <= c_k27_7;
            tx_state     <= TX_PREAMBLE;

-------------------------------------------------------------------------------
-- State PREAMBLE: produces an Ethernet preamble
-------------------------------------------------------------------------------
          when TX_PREAMBLE =>
            tx_is_k      <= '0';
            tx_odata_reg <= c_preamble_char;

            if (tx_preamble_cntr = "000") then
              tx_state          <= TX_SFD;
              timestamp_stb_p_o <= '1';
              tx_rdreq_toggle   <= '1';
            end if;

            tx_preamble_cntr <= tx_preamble_cntr - 1;


-------------------------------------------------------------------------------
-- State SFD: outputs the start-of-frame delimeter (last byte of the preamble)
-------------------------------------------------------------------------------            
          when TX_SFD =>

            tx_newframe     <= '0';
            tx_odata_reg    <= c_preamble_sfd;
            tx_rdreq_toggle <= '1';
            tx_state        <= TX_DATA;

          when TX_DATA =>

            timestamp_stb_p_o <= '0';

            -- toggle the TX FIFO request line, so we read a 16-bit word
            -- every 2 tx_clk periods

            tx_fifo_rdreq <= tx_rdreq_toggle and not tx_fifo_rdempty;

            if((tx_fifo_rdempty = '1' or tx_fifo_abort = '1') and tx_fifo_end = '0') then  --
              -- FIFO underrun?
              tx_odata_reg       <= c_k30_7;  -- emit error propagation code
              tx_is_k            <= '1';
              tx_state           <= TX_GEN_ERROR;
              tx_error           <= not tx_fifo_abort;
              rmon_tx_underrun_o <= '1';
            else

              if tx_rdreq_toggle = '1' then  -- send 16-bit word MSB or LSB
                tx_odata_reg <= tx_fifo_msb;
              else
                tx_odata_reg <= tx_fifo_lsb;
              end if;

              tx_rdreq_toggle <= not tx_rdreq_toggle;

              -- handle the end of frame both for even- and odd-length frames
              tx_odd_length <= tx_fifo_singlebyte;

              if (tx_fifo_end = '1' and (tx_rdreq_toggle = '0' or (tx_rdreq_toggle = '1' and tx_fifo_singlebyte = '1'))) then
                tx_state <= TX_EPD;
              end if;
            end if;

-------------------------------------------------------------------------------
-- State EPD: send End-of-frame delimeter
-------------------------------------------------------------------------------            
          when TX_EPD =>

            tx_is_k      <= '1';
            tx_odata_reg <= c_k29_7;
            tx_state     <= TX_EXTEND;

--------------------------------------------------------------------------------
-- State EXTEND: send the carrier extension
-------------------------------------------------------------------------------
          when TX_EXTEND =>
            tx_odata_reg <= c_k23_7;
            if(tx_odd_length = '0')then
              tx_busy            <= '0';
              tx_state           <= TX_COMMA;
              tx_catch_disparity <= '1';
            else
              tx_odd_length <= '0';
            end if;

-------------------------------------------------------------------------------
-- State GEN_ERROR: entered when an error occured. Just terminates the frame.
-------------------------------------------------------------------------------            
          when TX_GEN_ERROR =>
            tx_state <= TX_EPD;

          when others => null;
        end case;
      end if;
    end if;
  end process;


end behavioral;


