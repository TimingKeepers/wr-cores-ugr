-------------------------------------------------------------------------------
-- Title      : Deterministic Altera GXB wrapper - Arria 2
-- Project    : White Rabbit Switch
-------------------------------------------------------------------------------
-- File       : wr_gxb_phy_arriaii.vhd
-- Author     : Wesley W. Terpstra
-- Company    : GSI
-- Created    : 2010-11-18
-- Last update: 2013-03-12
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Single channel wrapper for deterministic GXB
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
-------------------------------------------------------------------------------


-- Before you edit this file, read all of the following documents:
--   Transceiver Architecture in Arria II Devices     <http://www.altera.com/literature/hb/arria-ii-gx/aiigx_52001.pdf>
--   Transceiver Clocking in Arria II Devices         <http://www.altera.com/literature/hb/arria-ii-gx/aiigx_52002.pdf>
--   Reset Control and Power Down in Arria II Devices <http://www.altera.com/literature/hb/arria-ii-gx/aiigx_52004.pdf>
--   Recommended Design Practices (Clock Gating)      <http://www.altera.com/literature/hb/qts/qts_qii51006.pdf>
--   Achieving Timing Closure in Basic (PMA Direct) Functional Mode 
--                                                    <http://www.altera.com/literature/an/an580.pdf>
--   AN 610: Implementing Deterministic Latency for CPRI and OBSAI Protocols in Altera Devices
--                                                    <http://www.altera.com/literature/an/an610.pdf>

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.gencores_pkg.all;
use work.disparity_gen_pkg.all;

entity wr_gxb_phy_arriaii is

  port (
    clk_reconf_i : in  std_logic; -- 50 MHz
    clk_pll_i    : in  std_logic; -- feeds transmitter PLL
    clk_cru_i    : in  std_logic; -- trains data recovery clock
    clk_sys_i    : in  std_logic; -- Used to reset the core
    rstn_sys_i   : in  std_logic; -- must last >= 1us
    locked_o     : out std_logic; -- Is the rx_rbclk valid? (clk_sys domain)
    loopen_i     : in  std_logic;  -- local loopback enable (Tx->Rx), active hi
    drop_link_i  : in  std_logic; -- Kill the link?

    -- clocked by clk_pll_i
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
    pad_rxp_i : in std_logic := '0';
    
    dbg_tx_clk_o : out std_logic);  -- do not use for anything other than an output on an oscilloscope

end wr_gxb_phy_arriaii;

architecture rtl of wr_gxb_phy_arriaii is

  component rxclkout
    port(
      inclk  : in  std_logic;
      outclk : out std_logic);
  end component;
  
  component arria_phy
    port (
      cal_blk_clk                 : in  std_logic;
      pll_inclk                   : in  std_logic;
      pll_powerdown               : in  std_logic_vector (0 downto 0);
      reconfig_clk                : in  std_logic;
      reconfig_togxb              : in  std_logic_vector (3 downto 0);
      rx_analogreset              : in  std_logic_vector (0 downto 0);
      rx_cruclk                   : in  std_logic_vector (0 downto 0);
      rx_datain                   : in  std_logic_vector (0 downto 0);
      rx_digitalreset             : in  std_logic_vector (0 downto 0);
      rx_enapatternalign          : in  std_logic_vector (0 downto 0);
      rx_seriallpbken             : in  std_logic_vector (0 downto 0);
      tx_bitslipboundaryselect    : in  std_logic_vector (4 downto 0);
      tx_datain                   : in  std_logic_vector (9 downto 0);
      tx_digitalreset             : in  std_logic_vector (0 downto 0);
      pll_locked                  : out std_logic_vector (0 downto 0);
      reconfig_fromgxb            : out std_logic_vector (16 downto 0);
      rx_bitslipboundaryselectout : out std_logic_vector (4 downto 0);
      rx_clkout                   : out std_logic_vector (0 downto 0);
      rx_dataout                  : out std_logic_vector (9 downto 0);
      rx_freqlocked               : out std_logic_vector (0 downto 0);
      rx_patterndetect            : out std_logic_vector (0 downto 0);
      rx_pll_locked               : out std_logic_vector (0 downto 0);
      rx_syncstatus               : out std_logic_vector (0 downto 0);
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

  signal clk_rx        : std_logic; -- local  clock
  signal clk_rx_glbl   : std_logic; -- global clock
  signal clk_tx        : std_logic; -- local  clock
  signal pll_locked    : std_logic;
  signal rx_freqlocked : std_logic;
  
  type t_state is (WAIT_POWER, WAIT_CMU, WAIT_CONFIG, WAIT_LOCK, DONE);
  
  signal rst_state         : t_state := WAIT_POWER;
  signal rst_delay         : unsigned(8 downto 0); -- must span >= 4us
  signal pll_powerdown     : std_logic;
  signal tx_digitalreset   : std_logic; -- sys domain
  signal rx_analogreset    : std_logic; -- sys domain
  signal rx_digitalreset   : std_logic; -- sys domain
  
  signal sys_pll_locked    : std_logic_vector(2 downto 0);
  signal sys_reconfig_busy : std_logic_vector(2 downto 0);
  signal sys_rx_freqlocked : std_logic_vector(2 downto 0);
  signal sys_drop_link     : std_logic_vector(2 downto 0);
  
  signal tx_8b10b_rstn : std_logic_vector(2 downto 0); -- tx domain
  signal rx_8b10b_rstn : std_logic_vector(2 downto 0); -- rx domain
  
  signal reconfig_busy    : std_logic;
  signal reconfig_togxb   : std_logic_vector (3 downto 0);
  signal reconfig_fromgxb : std_logic_vector (16 downto 0);
  
  signal rx_dump_link                : std_logic_vector(6 downto 0); -- Long enough to kill ep_sync_detect
  signal rx_enc_err                  : std_logic;
  signal tx_disp_pipe                : std_logic_vector (2 downto 0);
  signal rx_bitslipboundaryselectout : std_logic_vector (4 downto 0);
  
  signal rx_gxb_syncstatus           : std_logic;
  signal rx_lcln_syncstatus          : std_logic;
  signal rx_lclp_syncstatus          : std_logic;
  signal rx_glbl_syncstatus          : std_logic;
  
  signal rx_gxb_dataout              : std_logic_vector (9 downto 0); -- signal
  signal rx_lcln_dataout             : std_logic_vector (9 downto 0); -- local neg-edged register
  signal rx_lclp_dataout             : std_logic_vector (9 downto 0); -- local pos-edged register
  signal rx_glbl_dataout             : std_logic_vector (9 downto 0); -- global register (+1 inside decoder)
  
  signal tx_enc_datain               : std_logic_vector (9 downto 0); -- signal copy of register in encoder
  signal tx_glbl_datain              : std_logic_vector (9 downto 0); -- global register
  signal tx_lcln_datain              : std_logic_vector (9 downto 0); -- local neg-edged register
  signal tx_lclp_datain              : std_logic_vector (9 downto 0); -- local pos-edged register
  signal tx_gxb_datain               : std_logic_vector (9 downto 0); -- signal
  
begin

  rx_rbclk_o   <= clk_rx_glbl;
  dbg_tx_clk_o <= clk_tx; -- NOT FOR USE WITH tx_* signals
  
  U_RxClkout : rxclkout
    port map (
      inclk  => clk_rx,
      outclk => clk_rx_glbl);
  
  -- Altera PHY calibration block
  U_Reconf : altgx_reconf
    port map (
      reconfig_clk     => clk_reconf_i,
      reconfig_fromgxb => reconfig_fromgxb,
      busy             => reconfig_busy,
      reconfig_togxb   => reconfig_togxb);

  --- The serializer and byte aligner
  U_The_PHY : arria_phy
    port map (
      -- Clocks feeding the CMU and CRU of the transceiver
      pll_inclk                   => clk_pll_i,
      rx_cruclk(0)                => clk_cru_i,
      -- Derived clocks used for tx/rx lines
      tx_clkout(0)                => clk_tx,
      pll_locked(0)               => pll_locked,
      rx_clkout(0)                => clk_rx,
      rx_freqlocked(0)            => rx_freqlocked,
      rx_pll_locked(0)            => open,
      -- Calibration control of the GXB
      cal_blk_clk                 => clk_reconf_i,
      reconfig_clk                => clk_reconf_i,
      reconfig_togxb              => reconfig_togxb,
      reconfig_fromgxb            => reconfig_fromgxb,
      rx_seriallpbken(0)          => loopen_i,
      -- Reset logic of the GXB
      pll_powerdown(0)            => pll_powerdown,
      tx_digitalreset(0)          => tx_digitalreset,
      rx_analogreset(0)           => rx_analogreset,
      rx_digitalreset(0)          => rx_digitalreset,
      -- Word alignment of the serializer
      rx_enapatternalign(0)       => '1',
      rx_patterndetect(0)         => open,
      rx_syncstatus(0)            => rx_gxb_syncstatus,
      rx_bitslipboundaryselectout => rx_bitslipboundaryselectout,
      tx_bitslipboundaryselect    => (others => '0'),
      -- Actual data lines
      rx_datain(0)                => pad_rxp_i,
      rx_dataout                  => rx_gxb_dataout,
      tx_dataout(0)               => pad_txp_o,
      tx_datain                   => tx_gxb_datain);
  
  -- Encode the TX data
  encoder : enc_8b10b
    port map(
      clk_i     => clk_pll_i,
      rst_n_i   => tx_8b10b_rstn(0),
      ctrl_i    => tx_k_i,
      in_8b_i   => tx_data_i,
      err_o     => tx_enc_err_o,
      dispar_o  => tx_disp_pipe(0),
      out_10b_o => tx_enc_datain);
  
  -- Decode the RX data
  decoder : dec_8b10b
    port map(
      clk_i       => clk_rx_glbl,
      rst_n_i     => rx_8b10b_rstn(0),
      in_10b_i    => rx_glbl_dataout,
      ctrl_o      => rx_k_o,
      code_err_o  => rx_enc_err,
      rdisp_err_o => open,
      out_8b_o    => rx_data_o);
  rx_enc_err_o <= rx_enc_err or rx_dump_link(0);
  
  -- Reset procedure follows Figure 4-4 of Reset Control and Power Down in Arria II Devices
  p_reset : process(clk_sys_i) is
  begin
    if rising_edge(clk_sys_i) then
      -- Synchronize foreign signals
      sys_pll_locked    <= pll_locked    & sys_pll_locked   (sys_pll_locked'left    downto 1);
      sys_reconfig_busy <= reconfig_busy & sys_reconfig_busy(sys_reconfig_busy'left downto 1);
      sys_rx_freqlocked <= rx_freqlocked & sys_rx_freqlocked(sys_rx_freqlocked'left downto 1);
      sys_drop_link     <= drop_link_i   & sys_drop_link    (sys_drop_link'left     downto 1);
      
      if rstn_sys_i = '0' then
        rst_state       <= WAIT_POWER;
        rst_delay       <= (others => '1');
        pll_powerdown   <= '1';
        rx_analogreset  <= '1';
        locked_o        <= '0';
        tx_digitalreset <= '1';
        rx_digitalreset <= '1';
      else
        case rst_state is
          when WAIT_POWER =>
            pll_powerdown   <= '1';
            rx_analogreset  <= '1';
            locked_o        <= '0';
            tx_digitalreset <= '1';
            rx_digitalreset <= '1';
            
            rst_delay <= rst_delay - 1;
            
            if rst_delay = 0 then
              rst_delay <= (others => '1');
              rst_state <= WAIT_CMU;
            end if;
            
          when WAIT_CMU =>
            pll_powerdown <= '0';
            
            if sys_pll_locked(0) = '0' then
              rst_delay <= (others => '1');
            else
              rst_delay <= rst_delay - 1;
            end if;
            
            if rst_delay = 0 then
              rst_delay <= (others => '1');
              rst_state <= WAIT_CONFIG;
            end if;
            
          when WAIT_CONFIG =>
            if sys_reconfig_busy(0) = '1' then
              rst_delay <= (others => '1');
            else
              rst_delay <= rst_delay - 1;
            end if;
            
            if rst_delay = 0 then
              rst_delay <= (others => '1');
              rst_state <= WAIT_LOCK;
            end if;
            
            if sys_pll_locked(0) = '0' then
              rst_delay <= (others => '1');
              rst_state <= WAIT_POWER;
            end if;
          
          when WAIT_LOCK =>
            rx_analogreset <= '0';
            
            if sys_rx_freqlocked(0) = '0' then
              rst_delay <= (others => '1');
            else
              rst_delay <= rst_delay - 1;
            end if;
            
            if rst_delay = 0 then
              rst_delay <= (others => '1');
              rst_state <= DONE;
            end if;
            
            if sys_pll_locked(0) = '0' then
              rst_delay <= (others => '1');
              rst_state <= WAIT_POWER;
            end if;
          
          when DONE =>
            -- RX clock is now locked and safe
            locked_o <= '1';
            
            -- Kill the link upon request
            tx_digitalreset <= sys_drop_link(0);
            rx_digitalreset <= sys_drop_link(0);
            
            if sys_pll_locked(0) = '0' then
              rst_delay <= (others => '1');
              rst_state <= WAIT_POWER;
            end if;
            
        end case;
      end if;
    end if;
  end process;
  
  
  -- Generate reset for 8b10b encoder
  p_pll_reset : process(clk_pll_i) is
  begin
    if rising_edge(clk_pll_i) then
      tx_8b10b_rstn <= (not tx_digitalreset) & tx_8b10b_rstn(tx_8b10b_rstn'left downto 1);
    end if;
  end process;
  
  -- Generate reset for the 8b10b decoder and ep_sync_detect
  -- should use global version of clk_rx
  p_rx_reset : process(clk_rx_glbl) is
  begin
    if rising_edge(clk_rx_glbl) then
      rx_8b10b_rstn <= (not rx_digitalreset) & rx_8b10b_rstn(rx_8b10b_rstn'left downto 1);
    end if;
  end process;
  
  -- Dump the link if the bitslide changes
  p_dump_link : process(clk_rx_glbl) is
  begin
    if rising_edge(clk_rx_glbl) then
      if rx_glbl_syncstatus = '1' then
        rx_dump_link <= (others => '1');
      else
        rx_dump_link <= '0' & rx_dump_link(rx_dump_link'left downto 1);
      end if;
    end if;
  end process;
  
  -- A slow signal that doesn't traverse the full RX sync path
  -- should use a global version of clk_rx
  p_rx_bitslide : process(clk_rx_glbl) is
  begin
    if rising_edge(clk_rx_glbl) then
      rx_bitslide_o <= rx_bitslipboundaryselectout(3 downto 0);
    end if;
  end process;
   
  -- The disparity should be delayed for WR
  tx_disparity_o <= tx_disp_pipe(2);
  p_delay_disp : process(clk_pll_i)
  begin
    if rising_edge(clk_pll_i) then
      tx_disp_pipe(1) <= tx_disp_pipe(0);
      tx_disp_pipe(2) <= tx_disp_pipe(1);
    end if;
  end process;
  
  -- The extra registers are to allow signals enough time to reach the GXB
  -- clk_pll_i may be a global clock
  p_tx_path_pll : process(clk_pll_i) is
  begin
    if rising_edge(clk_pll_i) then
      tx_glbl_datain <= tx_enc_datain;
      -- tx_enc_datain is a registered output of enc_8b10b
      -- Two back-to-back global registers should be enough to cross FPGA
    end if;
  end process;
  
  -- Cross clock domain from pll_clk_i to tx_clk
  -- Because they are async registers according to TimeQuest, they get placed
  -- side-by-side like a synchronizer. However, they are actually in phase.
  -- Thus we would get a hold violation unless we flip the clock edge.
  -- tx_lcln_datain should use a local clock
  p_tx_path_neg : process(clk_tx) is
  begin
    if falling_edge(clk_tx) then
      tx_lcln_datain <= tx_glbl_datain;
    end if;
  end process;
  
  -- tx_lclp_datain should use a local clock
  p_tx_path_pos : process(clk_tx) is
  begin
    if rising_edge(clk_tx) then
      tx_lclp_datain <= tx_lcln_datain;
    end if;
  end process;
  tx_gxb_datain <= tx_lclp_datain;
  
  -- Use the negative edge to improve insertion timing from GXB
  -- (the clock line from the GXB is slower than the data)
  -- these register should use a local clock
  p_rx_path_neg : process(clk_rx) is
  begin
    if falling_edge(clk_rx) then
      rx_lcln_dataout    <= rx_gxb_dataout;
      rx_lcln_syncstatus <= rx_gxb_syncstatus;
    end if;
  end process;
  
  -- these should use the local clock
  p_rx_path_pos : process(clk_rx) is
  begin
    if rising_edge(clk_rx) then
      rx_lclp_dataout    <= rx_lcln_dataout;
      rx_lclp_syncstatus <= rx_lcln_syncstatus;
    end if;
  end process;
  
  -- these should use the global clock
  p_rx_path_gbl : process(clk_rx_glbl) is
  begin
    if rising_edge(clk_rx_glbl) then
      rx_glbl_dataout    <= rx_lclp_dataout;
      rx_glbl_syncstatus <= rx_lclp_syncstatus;
      
      -- There is another register of rx_dataout inside dec_8b10b
      -- Two back-to-back global registers should be enough to cross FPGA
    end if;
  end process;

end rtl;
