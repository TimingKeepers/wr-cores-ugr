-------------------------------------------------------------------------------
-- Title      : Deterministic Altera PHY wrapper - Arria 5
-- Project    : White Rabbit Switch
-------------------------------------------------------------------------------
-- File       : wr_arria5_phy.vhd
-- Author     : Wesley W. Terpstra
-- Company    : GSI
-- Created    : 2013-05-14
-- Last update: 2013-05-14
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Single channel wrapper for deterministic PHY
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
-- 2013-03-12  1.0      terpstra  Rewrote using deterministic mode
-- 2013-08-22  1.1      terpstra  Now runs on arria5 hardware
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.gencores_pkg.all;
use work.disparity_gen_pkg.all;
use work.altera_networks_pkg.all;

entity wr_arria5_phy is
  generic (
    g_tx_latch_edge : std_logic := '1';
    g_rx_latch_edge : std_logic := '0');
  port (
    clk_reconf_i : in  std_logic; -- 100 MHz
    clk_phy_i    : in  std_logic; -- feeds transmitter CMU and CRU
    locked_o     : out std_logic; -- Is the rx_rbclk valid?
    loopen_i     : in  std_logic;  -- local loopback enable (Tx->Rx), active hi
    drop_link_i  : in  std_logic; -- Kill the link?

    tx_clk_i       : in  std_logic;  -- clock used for TX data; MUST HAVE FIXED PHASE wrt. clk_phy_i
    tx_data_i      : in  std_logic_vector(7 downto 0);   -- data input (8 bits, not 8b10b-encoded)
    tx_k_i         : in  std_logic;  -- 1 when tx_data_i contains a control code, 0 when it's a data byte
    tx_disparity_o : out std_logic;  -- disparity of the currently transmitted 8b10b code (1 = plus, 0 = minus).
    tx_enc_err_o   : out std_logic;  -- error encoding

    rx_rbclk_o    : out std_logic;  -- RX recovered clock
    rx_data_o     : out std_logic_vector(7 downto 0);  -- 8b10b-decoded data output. 
    rx_k_o        : out std_logic;   -- 1 when the byte on rx_data_o is a control code
    rx_enc_err_o  : out std_logic;   -- encoding error indication
    rx_bitslide_o : out std_logic_vector(3 downto 0); -- RX bitslide indication, indicating the delay of the RX path of the transceiver (in UIs). Must be valid when rx_data_o is valid.

    pad_txp_o : out std_logic;
    pad_rxp_i : in std_logic := '0');

end wr_arria5_phy;

architecture rtl of wr_arria5_phy is

  component arria5_phy_reconf
    port(
      reconfig_busy             : out std_logic;
      mgmt_clk_clk              : in  std_logic;
      mgmt_rst_reset            : in  std_logic;
      reconfig_mgmt_address     : in  std_logic_vector(6 downto 0);
      reconfig_mgmt_read        : in  std_logic;
      reconfig_mgmt_readdata    : out std_logic_vector(31 downto 0);
      reconfig_mgmt_waitrequest : out std_logic;
      reconfig_mgmt_write       : in  std_logic;
      reconfig_mgmt_writedata   : in  std_logic_vector(31 downto 0);
      reconfig_to_xcvr          : out std_logic_vector(139 downto 0);
      reconfig_from_xcvr        : in  std_logic_vector(91 downto 0));
  end component;
  
  component arria5_phy
    port(
      phy_mgmt_clk                : in  std_logic;
      phy_mgmt_clk_reset          : in  std_logic;
      phy_mgmt_address            : in  std_logic_vector(8 downto 0);
      phy_mgmt_read               : in  std_logic;
      phy_mgmt_readdata           : out std_logic_vector(31 downto 0);
      phy_mgmt_waitrequest        : out std_logic;
      phy_mgmt_write              : in  std_logic;
      phy_mgmt_writedata          : in  std_logic_vector(31 downto 0);
      tx_ready                    : out std_logic;
      rx_ready                    : out std_logic;
      pll_ref_clk                 : in  std_logic_vector(0 downto 0);
      tx_serial_data              : out std_logic_vector(0 downto 0);
      tx_bitslipboundaryselect    : in  std_logic_vector(4 downto 0);
      pll_locked                  : out std_logic_vector(0 downto 0);
      rx_serial_data              : in  std_logic_vector(0 downto 0);
      rx_bitslipboundaryselectout : out std_logic_vector(4 downto 0);
      tx_clkout                   : out std_logic_vector(0 downto 0);
      rx_clkout                   : out std_logic_vector(0 downto 0);
      tx_parallel_data            : in  std_logic_vector(9 downto 0);
      rx_parallel_data            : out std_logic_vector(9 downto 0);
      reconfig_from_xcvr          : out std_logic_vector(91 downto 0);
      reconfig_to_xcvr            : in  std_logic_vector(139 downto 0));
  end component;

  component dec_8b10b
    port (
      clk_i       : in  std_logic;
      rst_n_i     : in  std_logic;
      in_10b_i    : in  std_logic_vector(9 downto 0);
      ctrl_o      : out std_logic;
      code_err_o  : out std_logic;
      rdisp_err_o : out std_logic;
      out_8b_o    : out std_logic_vector(7 downto 0));
  end component;

  component enc_8b10b
    port (
      clk_i     : in  std_logic;
      rst_n_i   : in  std_logic;
      ctrl_i    : in  std_logic;
      in_8b_i   : in  std_logic_vector(7 downto 0);
      err_o     : out std_logic;
      dispar_o  : out std_logic;
      out_10b_o : out std_logic_vector(9 downto 0));
  end component;

  signal clk_rx_gxb    : std_logic; -- external fabric
  signal clk_rx        : std_logic; -- regional clock
  signal clk_tx_gxb    : std_logic; -- external fabric
  signal clk_tx        : std_logic; -- regional clock
  signal pll_locked    : std_logic;
  signal rx_ready      : std_logic;
  signal tx_ready      : std_logic;
  signal reconfig_busy : std_logic;
  
  signal tx_8b10b_rstn : std_logic_vector(2 downto 0); -- tx domain
  signal rx_8b10b_rstn : std_logic_vector(2 downto 0); -- rx domain
  
  signal xcvr_to_reconfig : std_logic_vector(91 downto 0);
  signal reconfig_to_xcvr : std_logic_vector(139 downto 0);
  
  signal rx_bitslipboundaryselectout : std_logic_vector (4 downto 0);
  signal rx_gxb_dataout              : std_logic_vector (9 downto 0); -- signal out of GXB
  signal rx_reg_dataout              : std_logic_vector (9 downto 0); -- regional clocked FPGA register (clk_rx)
  signal tx_enc_datain               : std_logic_vector (9 downto 0); -- registered encoder output      (tx_clk_i)
  signal tx_reg_datain               : std_logic_vector (9 downto 0); -- transfer register from fabric  (tx_clk_i)
  signal tx_gxb_datain               : std_logic_vector (9 downto 0); -- transfer register to GXB       (clk_tx)
  
begin

  rx_rbclk_o <= clk_rx;
  
  U_RxClkout : single_region
    port map (
      inclk  => clk_rx_gxb,
      outclk => clk_rx);
  
  U_TxClk : single_region
    port map (
      inclk  => clk_tx_gxb,
      outclk => clk_tx);
  
  -- Altera PHY calibration block
  U_Reconf : arria5_phy_reconf
    port map (
      reconfig_busy             => reconfig_busy,
      mgmt_clk_clk              => clk_reconf_i,
      mgmt_rst_reset            => '0',
      reconfig_mgmt_address     => (others => '0'),
      reconfig_mgmt_read        => '0',
      reconfig_mgmt_readdata    => open,
      reconfig_mgmt_waitrequest => open,
      reconfig_mgmt_write       => '0',
      reconfig_mgmt_writedata   => (others => '0'),
      reconfig_to_xcvr          => reconfig_to_xcvr,
      reconfig_from_xcvr        => xcvr_to_reconfig);

  --- The serializer and byte aligner
  U_The_PHY : arria5_phy
    port map (
      phy_mgmt_clk                => clk_reconf_i,
      phy_mgmt_clk_reset          => '0',
      phy_mgmt_address            => "010000101", -- 0x085
      phy_mgmt_read               => '0',
      phy_mgmt_readdata           => open,
      phy_mgmt_waitrequest        => open,
      phy_mgmt_write              => '1',
      phy_mgmt_writedata          => (0 => '1', others => '0'),
      tx_ready                    => tx_ready,
      rx_ready                    => rx_ready,
      pll_ref_clk(0)              => clk_phy_i,
      tx_serial_data(0)           => pad_txp_o,
      tx_bitslipboundaryselect    => (others => '0'),
      pll_locked(0)               => pll_locked,
      rx_serial_data(0)           => pad_rxp_i,
      rx_bitslipboundaryselectout => rx_bitslipboundaryselectout,
      tx_clkout(0)                => clk_tx_gxb,
      rx_clkout(0)                => clk_rx_gxb,
      tx_parallel_data            => tx_gxb_datain,
      rx_parallel_data            => rx_gxb_dataout,
      reconfig_from_xcvr          => xcvr_to_reconfig,
      reconfig_to_xcvr            => reconfig_to_xcvr);
  
  -- Encode the TX data
  encoder : enc_8b10b
    port map(
      clk_i     => tx_clk_i,
      rst_n_i   => tx_8b10b_rstn(0),
      ctrl_i    => tx_k_i,
      in_8b_i   => tx_data_i,
      err_o     => tx_enc_err_o,
      dispar_o  => tx_disparity_o,
      out_10b_o => tx_enc_datain);
  
  -- Decode the RX data
  decoder : dec_8b10b
    port map(
      clk_i       => clk_rx,
      rst_n_i     => rx_8b10b_rstn(0),
      in_10b_i    => rx_reg_dataout,
      ctrl_o      => rx_k_o,
      code_err_o  => rx_enc_err_o,
      rdisp_err_o => open,
      out_8b_o    => rx_data_o);
  
  locked_o <= pll_locked and tx_ready and not reconfig_busy;
  
  -- Generate reset for 8b10b encoder
  p_pll_reset : process(tx_clk_i) is
  begin
    if rising_edge(tx_clk_i) then
      tx_8b10b_rstn <= (not drop_link_i and tx_ready) & tx_8b10b_rstn(tx_8b10b_rstn'left downto 1);
    end if;
  end process;
  
  -- Generate reset for the 8b10b decoder and ep_sync_detect
  -- should use global version of clk_rx
  p_rx_reset : process(clk_rx) is
  begin
    if rising_edge(clk_rx) then
      rx_8b10b_rstn <= (not drop_link_i and rx_ready) & rx_8b10b_rstn(rx_8b10b_rstn'left downto 1);
    end if;
  end process;
  
  -- Cross clock domain from tx_clk_i to clk_tx
  -- These clocks must be phase aligned
  -- Registers tx_reg_datain and tx_gxb_datain must be logic locked
  -- to the same ALM, preferrably directly beside the GXB.
  p_tx_path0 : process(tx_clk_i) is
  begin
    if tx_clk_i'event and tx_clk_i = (not g_tx_latch_edge) then
      tx_reg_datain <= tx_enc_datain;
    end if;
  end process;
  p_tx_path1 : process(clk_tx) is
  begin
    if clk_tx'event and clk_tx = g_tx_latch_edge then
      tx_gxb_datain <= tx_reg_datain;
    end if;
  end process;
  
  -- Additional register to improve timings
  p_rx_path : process(clk_rx) is
  begin
    if clk_rx'event and clk_rx = g_rx_latch_edge then
      rx_reg_dataout <= rx_gxb_dataout;
    end if;
  end process;
  
  -- Slow registered signals out of the GXB
  p_rx_regs : process(clk_rx) is
  begin
    if rising_edge(clk_rx) then
      rx_bitslide_o <= rx_bitslipboundaryselectout(3 downto 0);
    end if;
  end process;
  
end rtl;
