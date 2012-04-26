-------------------------------------------------------------------------------
-- Title      : Deterministic Xilinx GTP wrapper - Spartan-6 top module
-- Project    : White Rabbit Switch
-------------------------------------------------------------------------------
-- File       : wr_gtp_phy_spartan6.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2010-11-18
-- Last update: 2012-02-09
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
-- 2011-05-15  0.6      twlostow  Added reference clock output
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.gencores_pkg.all;
use work.disparity_gen_pkg.all;


entity wr_gxb_phy_arriaii is

  generic (
    -- set to non-zero value to speed up the simulation by reducing some delays
    g_simulation      : integer := 1;
    g_force_disparity : integer := 0
    );

  port (

    clk_reconf_i : in std_logic;

    clk_ref_i : in std_logic;

    -- TX path, synchronous to tx_clk_o

    tx_clk_o : out std_logic;

    -- data input (8 bits, not 8b10b-encoded)
    tx_data_i : in std_logic_vector(7 downto 0);

    -- 1 when tx_data_i contains a control code, 0 when it's a data byte
    tx_k_i : in std_logic;

    -- disparity of the currently transmitted 8b10b code (1 = plus, 0 = minus).
    -- Necessary for the PCS to generate proper frame termination sequences.
    tx_disparity_o : out std_logic;

    -- Encoding error indication (1 = error, 0 = no error)
    tx_enc_err_o : out std_logic;


    -- RX recovered clock
    rx_rbclk_o : out std_logic;

    -- 8b10b-decoded data output. The data output must be kept invalid before
    -- the transceiver is locked on the incoming signal to prevent the EP from
    -- detecting a false carrier.
    rx_data_o : out std_logic_vector(7 downto 0);

    -- 1 when the byte on rx_data_o is a control code
    rx_k_o : out std_logic;

    -- encoding error indication
    rx_enc_err_o : out std_logic;

    -- RX bitslide indication, indicating the delay of the RX path of the
    -- transceiver (in UIs). Must be valid when ch0_rx_data_o is valid.
    rx_bitslide_o : out std_logic_vector(3 downto 0);

    -- reset input, active hi
    rst_i : in std_logic;

    -- local loopback enable (Tx->Rx), active hi
    loopen_i : in std_logic;


    pad_txp_o : out std_logic;

    pad_rxp_i : in std_logic := '0'
    );


end wr_gxb_phy_arriaii;

architecture rtl of wr_gxb_phy_arriaii is

  

  
  component arria_phy
    generic (
      starting_channel_number : natural := 0);
    port (
      cal_blk_clk                 : in  std_logic;
      pll_inclk                   : in  std_logic;
      reconfig_clk                : in  std_logic;
      reconfig_togxb              : in  std_logic_vector (3 downto 0);
      rx_analogreset              : in  std_logic_vector (0 downto 0);
      rx_datain                   : in  std_logic_vector (0 downto 0);
      rx_digitalreset             : in  std_logic_vector (0 downto 0);
      tx_bitslipboundaryselect    : in  std_logic_vector (4 downto 0);
      tx_ctrlenable               : in  std_logic_vector (0 downto 0);
      tx_datain                   : in  std_logic_vector (7 downto 0);
      tx_digitalreset             : in  std_logic_vector (0 downto 0);
      tx_dispval                  : in  std_logic_vector (0 downto 0);
      tx_forcedisp                : in  std_logic_vector (0 downto 0);
      reconfig_fromgxb            : out std_logic_vector (16 downto 0);
      rx_bitslipboundaryselectout : out std_logic_vector (4 downto 0);
      rx_clkout                   : out std_logic_vector (0 downto 0);
      rx_ctrldetect               : out std_logic_vector (0 downto 0);
      rx_dataout                  : out std_logic_vector (7 downto 0);
      rx_errdetect                : out std_logic_vector (0 downto 0);
      tx_clkout                   : out std_logic_vector (0 downto 0);
      tx_dataout                  : out std_logic_vector (0 downto 0));
  end component;

  component altgx_reconf
    port (
      reconfig_clk     : in  std_logic;
      reconfig_fromgxb : in  std_logic_vector (16 downto 0);
      busy             : out std_logic;
      reconfig_togxb   : out std_logic_vector (3 downto 0));
  end component;

  signal cal_blk_clk                 : std_logic;
  signal pll_inclk                   : std_logic;
  signal reconfig_clk                : std_logic;
  signal reconfig_togxb              : std_logic_vector (3 downto 0);
  signal tx_ctrlenable               : std_logic;
  signal tx_datain                   : std_logic_vector (7 downto 0);
  signal tx_dispval                  : std_logic;
  signal tx_forcedisp                : std_logic;
  signal reconfig_fromgxb            : std_logic_vector (16 downto 0);
  signal rx_bitslipboundaryselectout : std_logic_vector (4 downto 0);
  signal rx_clkout                   : std_logic_vector (0 downto 0);
  signal rx_ctrldetect               : std_logic_vector (0 downto 0);
  signal rx_dataout                  : std_logic_vector (7 downto 0);
  signal rx_errdetect                : std_logic_vector (0 downto 0);
  signal tx_clkout                   : std_logic_vector (0 downto 0);
  signal tx_dataout                  : std_logic_vector (0 downto 0);
  signal reconfig_busy               : std_logic;

  signal disp_pipe     : std_logic_vector(1 downto 0);
  signal cur_disp      : t_8b10b_disparity;
  signal disparity_set : std_logic;
  signal rx_clk_int    : std_logic;

  signal rst_pipe : std_logic_vector(6 downto 0);
  
  
  function f_sl_to_slv(x : std_logic)
    return std_logic_vector is
    variable tmp : std_logic_vector(0 downto 0);
  begin
    tmp(0) := x;
    return tmp;
  end f_sl_to_slv;
  
begin  -- rtl

  p_rst_pipe: process(clk_ref_i)
    begin
      if rising_edge(clk_ref_i) then
        rst_pipe <= rst_pipe(rst_pipe'left-1 downto 0) & rst_i;
      end if;
    end process;

  U_Reconf : altgx_reconf
    port map (
      reconfig_clk     => clk_reconf_i,
      reconfig_fromgxb => reconfig_fromgxb,
      busy             => reconfig_busy,
      reconfig_togxb   => reconfig_togxb);

  U_The_PHY : arria_phy
    port map (
      cal_blk_clk      => clk_reconf_i,
      pll_inclk        => clk_ref_i,
      reconfig_clk     => clk_reconf_i,
      reconfig_togxb   => reconfig_togxb,
      reconfig_fromgxb => reconfig_fromgxb,

      rx_datain                => f_sl_to_slv(pad_rxp_i),
      tx_bitslipboundaryselect => "00000",
      tx_ctrlenable            => f_sl_to_slv(tx_k_i),
      tx_datain                => tx_data_i,
      tx_dispval               => f_sl_to_slv(tx_dispval),
      tx_forcedisp             => f_sl_to_slv(tx_forcedisp),
      tx_dataout               => tx_dataout,

      tx_digitalreset => f_sl_to_slv(rst_pipe(2)),
      rx_analogreset  => f_sl_to_slv(rst_pipe(4)),
      rx_digitalreset  => f_sl_to_slv(rst_pipe(6)),

      rx_bitslipboundaryselectout => open,
      rx_clkout                   => rx_clkout,
      rx_ctrldetect               => rx_ctrldetect,
      rx_dataout                  => rx_dataout,
      rx_errdetect                => rx_errdetect,
      tx_clkout                   => tx_clkout);

  rx_clk_int <= rx_clkout(0);
  pad_txp_o  <= tx_dataout(0);

  gen_disp : process(clk_ref_i)
  begin
    if rising_edge(clk_ref_i) then
      if(rst_i = '1') then
        if(g_force_disparity = 0) then
          cur_disp <= RD_MINUS;
        else
          cur_disp <= RD_PLUS;
        end if;
        disp_pipe <= (others => '0');
      else
        cur_disp     <= f_next_8b10b_disparity8(cur_disp, tx_k_i, tx_data_i);
        disp_pipe(0) <= to_std_logic(cur_disp);
        disp_pipe(1) <= disp_pipe(0);
      end if;
    end if;
  end process;

  tx_disparity_o <= disp_pipe(1);
  tx_enc_err_o <= '0';


  p_force_proper_disparity : process(clk_ref_i, rst_i)
  begin
    if (rst_i = '1') then
      disparity_set <= '0';

      tx_dispval   <= '0';
      tx_forcedisp <= '0';
    elsif rising_edge(clk_ref_i) then
      if(disparity_set = '0' and tx_k_i = '1' and tx_data_i = x"bc") then
        disparity_set <= '1';
        if(g_force_disparity = 0) then
          tx_dispval <= '0';
        else
          tx_dispval <= '1';
        end if;
        tx_forcedisp <= '1';
      else
        tx_forcedisp <= '0';
        tx_dispval   <= '0';
      end if;
    end if;
  end process;

  p_gen_output : process(rx_clk_int, rst_i)
  begin
    if(rst_i = '1') then
      rx_data_o    <= (others => '0');
      rx_k_o       <= '0';
      rx_enc_err_o <= '0';
    elsif rising_edge(rx_clk_int) then
      rx_data_o    <= rx_dataout;
      rx_k_o       <= rx_ctrldetect(0);
      rx_enc_err_o <= rx_errdetect(0);
    end if;
  end process;

  rx_rbclk_o <= rx_clk_int;
  tx_clk_o   <= tx_clkout(0);
end rtl;
