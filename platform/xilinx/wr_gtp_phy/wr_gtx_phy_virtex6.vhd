-------------------------------------------------------------------------------
-- Title      : Deterministic Xilinx GTP wrapper - Spartan-6 top module
-- Project    : White Rabbit Switch
-------------------------------------------------------------------------------
-- File       : wr_gtp_phy_spartan6.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2010-11-18
-- Last update: 2012-07-18
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Dual channel wrapper for Xilinx Spartan-6 GTP adapted for
-- deterministic delays at 1.25 Gbps.
-------------------------------------------------------------------------------
--
-- Copyright (c) 2010 CERN / Tomasz Wlostowski
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
-- 2010-11-18  0.4      twlostow  Initial release
-- 2011-02-07  0.5      twlostow  Verified on Spartan6 GTP (single channel only)
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library unisim;
use unisim.vcomponents.all;

library work;
use work.gencores_pkg.all;
use work.disparity_gen_pkg.all;

entity wr_gtx_phy_virtex6 is

  generic (
    -- set to non-zero value to speed up the simulation by reducing some delays
    g_simulation         : integer := 0;
    g_use_slave_tx_clock : integer := 0;
    g_use_bufr           : boolean := false
    );

  port (

    -- Reference 62.5 MHz clock input for the TX/RX logic (not the GTX itself)
    clk_ref_i : in std_logic;
    -- Reference 62.5 MHz clock for the GTX transceiver
    clk_gtx_i : in std_logic;

    -- TX path, clk_ref_i - synchronous:

    -- data input (8 bits, not 8b10b-encoded)
    tx_data_i : in std_logic_vector(15 downto 0);

    -- 1 when tx_data_i contains a control code, 0 when it's a data byte
    tx_k_i : in std_logic_vector(1 downto 0);

    -- disparity of the currently transmitted 8b10b code (1 = plus, 0 = minus).
    -- Necessary for the PCS to generate proper frame termination sequences.
    -- Generated for the 2nd byte (LSB) of tx_data_i.
    tx_disparity_o : out std_logic;

    -- Encoding error indication (1 = error, 0 = no error)
    tx_enc_err_o : out std_logic;

    -- RX path, synchronous to ch0_rx_rbclk_o.

    -- RX recovered clock
    rx_rbclk_o : out std_logic;

    -- 8b10b-decoded data output. The data output must be kept invalid before
    -- the transceiver is locked on the incoming signal to prevent the EP from
    -- detecting a false carrier.
    rx_data_o : out std_logic_vector(15 downto 0);

    -- 1 when the byte on rx_data_o is a control code
    rx_k_o : out std_logic_vector(1 downto 0);

    -- encoding error indication
    rx_enc_err_o : out std_logic;

    -- RX bitslide indication, indicating the delay of the RX path of the
    -- transceiver (in UIs). Must be valid when ch0_rx_data_o is valid.
    rx_bitslide_o : out std_logic_vector(4 downto 0);

    -- reset input, active hi
    rst_i    : in std_logic;
    loopen_i : in std_logic;

    pad_txn_o : out std_logic;
    pad_txp_o : out std_logic;

    pad_rxn_i : in std_logic := '0';
    pad_rxp_i : in std_logic := '0'

    );


end wr_gtx_phy_virtex6;

architecture rtl of wr_gtx_phy_virtex6 is

  component WHITERABBITGTX_WRAPPER_GTX
    generic (
      GTX_SIM_GTXRESET_SPEEDUP : integer;
      GTX_TX_CLK_SOURCE        : string;
      GTX_POWER_SAVE           : bit_vector);
    port (
      LOOPBACK_IN           : in  std_logic_vector(2 downto 0);
      RXCHARISK_OUT         : out std_logic_vector(1 downto 0);
      RXDISPERR_OUT         : out std_logic_vector(1 downto 0);
      RXNOTINTABLE_OUT      : out std_logic_vector(1 downto 0);
      RXBYTEISALIGNED_OUT   : out std_logic;
      RXCOMMADET_OUT        : out std_logic;
      RXSLIDE_IN            : in  std_logic;
      RXDATA_OUT            : out std_logic_vector(15 downto 0);
      RXRECCLK_OUT          : out std_logic;
      RXUSRCLK2_IN          : in  std_logic;
      RXCDRRESET_IN         : in  std_logic;
      RXN_IN                : in  std_logic;
      RXP_IN                : in  std_logic;
      GTXRXRESET_IN         : in  std_logic;
      MGTREFCLKRX_IN        : in  std_logic_vector(1 downto 0);
      PLLRXRESET_IN         : in  std_logic;
      RXPLLLKDET_OUT        : out std_logic;
      RXRESETDONE_OUT       : out std_logic;
      TXCHARISK_IN          : in  std_logic_vector(1 downto 0);
      GTXTEST_IN            : in  std_logic_vector(12 downto 0);
      TXDATA_IN             : in  std_logic_vector(15 downto 0);
      TXOUTCLK_OUT          : out std_logic;
      TXUSRCLK2_IN          : in  std_logic;
      TXRUNDISP_OUT         : out std_logic_vector(1 downto 0);
      TXN_OUT               : out std_logic;
      TXP_OUT               : out std_logic;
      TXDLYALIGNDISABLE_IN  : in  std_logic;
      TXDLYALIGNMONENB_IN   : in  std_logic;
      TXDLYALIGNMONITOR_OUT : out std_logic_vector(7 downto 0);
      TXDLYALIGNRESET_IN    : in  std_logic;
      TXENPMAPHASEALIGN_IN  : in  std_logic;
      TXPMASETPHASE_IN      : in  std_logic;
      GTXTXRESET_IN         : in  std_logic;
      MGTREFCLKTX_IN        : in  std_logic_vector(1 downto 0);
      PLLTXRESET_IN         : in  std_logic;
      TXPLLLKDET_OUT        : out std_logic;
      TXRESETDONE_OUT       : out std_logic);
  end component;

  component BUFG
    port (
      O : out std_ulogic;
      I : in  std_ulogic);
  end component;

  component BUFR
    generic (
      BUFR_DIVIDE : string := "BYPASS";
      SIM_DEVICE  : string := "VIRTEX6");
    port (
      O   : out std_ulogic;
      CE  : in  std_ulogic := '1';
      CLR : in  std_ulogic := '0';
      I   : in  std_ulogic);
  end component;

  component gtp_phase_align_virtex6
    generic (
      g_simulation : integer);
    port (
      gtp_rst_i                   : in  std_logic;
      gtp_tx_clk_i                : in  std_logic;
      gtp_tx_en_pma_phase_align_o : out std_logic;
      gtp_tx_pma_set_phase_o      : out std_logic;
      gtp_tx_dly_align_disable_o  : out std_logic;
      gtp_tx_dly_align_reset_o    : out std_logic;
      align_en_i                  : in  std_logic;
      align_done_o                : out std_logic);
  end component;

  component gtp_bitslide
    generic (
      g_simulation : integer;
      g_target     : string := "virtex6");
    port (
      gtp_rst_i                : in  std_logic;
      gtp_rx_clk_i             : in  std_logic;
      gtp_rx_comma_det_i       : in  std_logic;
      gtp_rx_byte_is_aligned_i : in  std_logic;
      serdes_ready_i           : in  std_logic;
      gtp_rx_slide_o           : out std_logic;
      gtp_rx_cdr_rst_o         : out std_logic;
      bitslide_o               : out std_logic_vector(4 downto 0);
      synced_o                 : out std_logic);
  end component;

  component gtx_reset
    port (
      clk_tx_i        : in  std_logic;
      rst_i           : in  std_logic;
      txpll_lockdet_i : in  std_logic;
      gtx_test_o      : out std_logic_vector(12 downto 0));
  end component;

  signal trig0, trig1, trig2, trig3 : std_logic_vector(31 downto 0);
  signal gtx_rst                    : std_logic;
  signal gtx_loopback               : std_logic_vector(2 downto 0) := "000";
  signal gtx_reset_done             : std_logic;
  signal gtx_pll_lockdet            : std_logic;
  signal rst_synced                 : std_logic;
  signal rst_d0                     : std_logic;
  signal reset_counter              : unsigned(9 downto 0);
  signal gtx_test                   : std_logic_vector(12 downto 0);

  signal rx_rec_clk_bufin   : std_logic;
  signal rx_rec_clk         : std_logic;
  signal rx_comma_det       : std_logic;
  signal rx_byte_is_aligned : std_logic;

  signal tx_dly_align_disable  : std_logic;
  signal tx_dly_align_reset    : std_logic;
  signal tx_en_pma_phase_align : std_logic;
  signal tx_pma_set_phase      : std_logic;

  signal align_enable : std_logic;
  signal align_done   : std_logic;

  signal tx_rst_done, rx_rst_done : std_logic;

  signal txpll_lockdet, rxpll_lockdet : std_logic;
  signal pll_lockdet                  : std_logic;
  signal serdes_ready                 : std_logic;
  signal rx_slide                     : std_logic;
  signal rx_cdr_rst                   : std_logic;
  signal rx_synced                    : std_logic;
  signal rst_done                     : std_logic;
  signal everything_ready             : std_logic;

  signal mgtrefclk_in : std_logic_vector(1 downto 0);


  signal rx_k_int    : std_logic_vector(1 downto 0);
  signal rx_data_int : std_logic_vector(15 downto 0);

  signal rx_disp_err, rx_code_err : std_logic_vector(1 downto 0);

  signal tx_is_k_swapped : std_logic_vector(1 downto 0);
  signal tx_data_swapped : std_logic_vector(15 downto 0);

  signal cur_disp : t_8b10b_disparity;

  signal tx_rundisp_v6 : std_logic_vector(1 downto 0);
begin  -- rtl

  tx_enc_err_o <= '0';

  p_gen_reset : process(clk_ref_i)
  begin
    if rising_edge(clk_ref_i) then

      rst_d0     <= rst_i;
      rst_synced <= rst_d0;

      if(rst_synced = '1') then
        reset_counter <= (others => '0');
      else
        if(reset_counter(reset_counter'left) = '0') then
          reset_counter <= reset_counter + 1;
        end if;
      end if;
    end if;
  end process;

  gtx_rst <= rst_synced or std_logic(not reset_counter(reset_counter'left));

  U_Twice_Reset_Gen : gtx_reset
    port map (
      clk_tx_i        => clk_ref_i,
      rst_i           => gtx_rst,
      txpll_lockdet_i => txpll_lockdet,
      gtx_test_o      => gtx_test);

  gen_rx_bufg : if(g_use_bufr = false) generate
    
    U_BUF_RxRecClk : BUFG
      port map (
        I => rx_rec_clk_bufin,
        O => rx_rec_clk);

  end generate gen_rx_bufg;

  gen_rx_bufr : if(g_use_bufr = true) generate
    U_BUF_RxRecClk : BUFR
      port map (
        I => rx_rec_clk_bufin,
        O => rx_rec_clk);

  end generate gen_rx_bufr;


  rx_rbclk_o <= rx_rec_clk;

  tx_is_k_swapped <= tx_k_i(0) & tx_k_i(1);
  tx_data_swapped <= tx_data_i(7 downto 0) & tx_data_i(15 downto 8);

  U_GTX_INST : WHITERABBITGTX_WRAPPER_GTX
    generic map (
      GTX_SIM_GTXRESET_SPEEDUP => 1,
      GTX_TX_CLK_SOURCE        => "TXPLL",
      GTX_POWER_SAVE           => "0000110000")
    port map (
      LOOPBACK_IN           => gtx_loopback,
      RXCHARISK_OUT         => rx_k_int,
      RXDISPERR_OUT         => rx_disp_err,
      RXNOTINTABLE_OUT      => rx_code_err,
      RXBYTEISALIGNED_OUT   => rx_byte_is_aligned,
      RXCOMMADET_OUT        => rx_comma_det,
      RXSLIDE_IN            => rx_slide,
      RXDATA_OUT            => rx_data_int,
      RXRECCLK_OUT          => rx_rec_clk_bufin,
      RXUSRCLK2_IN          => rx_rec_clk,
      RXCDRRESET_IN         => rx_cdr_rst,
      RXN_IN                => pad_rxn_i,
      RXP_IN                => pad_rxp_i,
      GTXRXRESET_IN         => gtx_rst,
      MGTREFCLKRX_IN        => mgtrefclk_in,
      PLLRXRESET_IN         => '0',
      RXPLLLKDET_OUT        => rxpll_lockdet,
      RXRESETDONE_OUT       => rx_rst_done,
      TXCHARISK_IN          => tx_is_k_swapped,
      GTXTEST_IN            => gtx_test,
      TXDATA_IN             => tx_data_swapped,
      TXOUTCLK_OUT          => open,
      TXUSRCLK2_IN          => clk_ref_i,
      TXRUNDISP_OUT         => tx_rundisp_v6,
      TXN_OUT               => pad_txn_o,
      TXP_OUT               => pad_txp_o,
      TXDLYALIGNDISABLE_IN  => tx_dly_align_disable,
      TXDLYALIGNMONENB_IN   => '1',
      TXDLYALIGNMONITOR_OUT => open,
      TXDLYALIGNRESET_IN    => tx_dly_align_reset,
      TXENPMAPHASEALIGN_IN  => tx_en_pma_phase_align,
      TXPMASETPHASE_IN      => tx_pma_set_phase,
      GTXTXRESET_IN         => gtx_rst,
      MGTREFCLKTX_IN        => mgtrefclk_in,
      PLLTXRESET_IN         => '0',
      TXPLLLKDET_OUT        => txpll_lockdet,
      TXRESETDONE_OUT       => tx_rst_done);

  mgtrefclk_in <= '0' & clk_gtx_i;

  U_Phase_Align : gtp_phase_align_virtex6
    generic map (
      g_simulation => g_simulation)
    port map (
      gtp_rst_i                   => gtx_rst,
      gtp_tx_clk_i                => clk_ref_i,
      gtp_tx_en_pma_phase_align_o => tx_en_pma_phase_align,
      gtp_tx_pma_set_phase_o      => tx_pma_set_phase,
      gtp_tx_dly_align_disable_o  => tx_dly_align_disable,
      gtp_tx_dly_align_reset_o    => tx_dly_align_reset,
      align_en_i                  => align_enable,
      align_done_o                => align_done);

  U_Bitslide : gtp_bitslide
    generic map (
      g_simulation => g_simulation,
      g_target     => "virtex6")
    port map (
      gtp_rst_i                => gtx_rst,
      gtp_rx_clk_i             => rx_rec_clk,
      gtp_rx_comma_det_i       => rx_comma_det,
      gtp_rx_byte_is_aligned_i => rx_byte_is_aligned,
      serdes_ready_i           => everything_ready,
      gtp_rx_slide_o           => rx_slide,
      gtp_rx_cdr_rst_o         => rx_cdr_rst,
      bitslide_o               => rx_bitslide_o,
      synced_o                 => rx_synced);

  rst_done         <= rx_rst_done and tx_rst_done;
  pll_lockdet      <= txpll_lockdet and rxpll_lockdet;
  serdes_ready     <= rst_done and pll_lockdet;
  align_enable     <= serdes_ready;
  everything_ready <= serdes_ready and align_done;

  trig2(3) <= rx_rst_done;
  trig2(4) <= tx_rst_done;
  trig2(5) <= txpll_lockdet;
  trig2(6) <= rxpll_lockdet;
  trig2(7) <= align_done;



  p_gen_rx_outputs : process(rx_rec_clk, gtx_rst)
  begin
    if(gtx_rst = '1') then
      rx_data_o    <= (others => '0');
      rx_k_o       <= (others => '0');
      rx_enc_err_o <= '0';
    elsif rising_edge(rx_rec_clk) then
      if(everything_ready = '1' and rx_synced = '1') then
        rx_data_o    <= rx_data_int(7 downto 0) & rx_data_int(15 downto 8);
        rx_k_o       <= rx_k_int(0) & rx_k_int(1);
        rx_enc_err_o <= rx_disp_err(0) or rx_disp_err(1) or rx_code_err(0) or rx_code_err(1);
      else
        rx_data_o    <= (others => '1');
        rx_k_o       <= (others => '1');
        rx_enc_err_o <= '1';
      end if;
    end if;
  end process;

  p_gen_tx_disparity : process(clk_ref_i)
  begin
    if rising_edge(clk_ref_i) then
      if gtx_rst = '1' then
        cur_disp <= RD_MINUS;
      else
        cur_disp <= f_next_8b10b_disparity16(cur_disp, tx_k_i, tx_data_i);
      end if;
    end if;
  end process;

  tx_disparity_o <= to_std_logic(cur_disp);
end rtl;
