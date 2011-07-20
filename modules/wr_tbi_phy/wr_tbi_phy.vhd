-------------------------------------------------------------------------------
-- Title      : TBI (Ten-bit-inteface) PHY adapter
-- Project    : White Rabbit 
-------------------------------------------------------------------------------
-- File       : wr_tbi_phy.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-Co-HT
-- Created    : 2011-05-27
-- Last update: 2011-05-28
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Copyright (c) 2010 Tomasz Wlostowski
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2010-05-27  1.0      twlostow        Imported from wrsw_endpoint
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;

use work.gencores_pkg.all;

entity wr_tbi_phy is
  
  port(
    ---------------------------------------------------------------------------
    -- Endpoint interface (serdes)
    ---------------------------------------------------------------------------

    serdes_rst_i    : in std_logic;
    serdes_loopen_i : in std_logic;
    serdes_prbsen_i : in std_logic;
    serdes_enable_i : in std_logic;
    serdes_syncen_i : in std_logic;

    serdes_tx_data_i      : in  std_logic_vector(7 downto 0);
    serdes_tx_k_i         : in  std_logic;
    serdes_tx_disparity_o : out std_logic;
    serdes_tx_enc_err_o   : out std_logic;

    serdes_rx_data_o     : out std_logic_vector(7 downto 0);
    serdes_rx_k_o        : out std_logic;
    serdes_rx_enc_err_o  : out std_logic;
    serdes_rx_bitslide_o : out std_logic_vector(3 downto 0);    

    ---------------------------------------------------------------------------
    -- TBI PHY I/O pins
    ---------------------------------------------------------------------------

    tbi_refclk_i : in std_logic;
    tbi_rbclk_i  : in std_logic;

    tbi_td_o     : out std_logic_vector(9 downto 0);
    tbi_rd_i     : in  std_logic_vector(9 downto 0);
    tbi_syncen_o : out std_logic;
    tbi_loopen_o : out std_logic;
    tbi_prbsen_o : out std_logic;
    tbi_enable_o : out std_logic
    );

end wr_tbi_phy;

architecture rtl of wr_tbi_phy is

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
  
  signal rx_reg        : std_logic_vector(9 downto 0);
  signal tx_reg        : std_logic_vector(9 downto 0);
  signal txdata_encoded : std_logic_vector(9 downto 0);
  signal dec_err_enc   : std_logic;
  signal dec_err_rdisp : std_logic;

  signal rst_synced_rbclk   : std_logic;
  signal rst_synced_rbclk_n : std_logic;
  signal serdes_rst_n           : std_logic;

begin  -- rtl

-------------------------------------------------------------------------------
-- RX path
-------------------------------------------------------------------------------

  U_sync_reset : gc_sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => tbi_refclk_i,
      rst_n_i  => '1',
      data_i   => serdes_rst_i,
      synced_o => rst_synced_rbclk,
      npulse_o => open,
      ppulse_o => open);

  rst_synced_rbclk_n <= not rst_synced_rbclk;

  p_register_input : process(tbi_rbclk_i)
  begin
    if rising_edge(tbi_rbclk_i) then
      rx_reg <= tbi_rd_i;
    end if;
  end process;

  U_DEC : dec_8b10b
    port map (
      clk_i    => tbi_rbclk_i,
      rst_n_i  => rst_synced_rbclk_n,
      in_10b_i => rx_reg,

      ctrl_o      => serdes_rx_k_o,
      out_8b_o    => serdes_rx_data_o,
      code_err_o  => dec_err_enc,
      rdisp_err_o => dec_err_rdisp);

  serdes_rx_enc_err_o  <= dec_err_rdisp or dec_err_rdisp;
  serdes_rx_bitslide_o <= (others => '0');

-------------------------------------------------------------------------------
-- TX Path
-------------------------------------------------------------------------------

  serdes_rst_n <= not serdes_rst_i;

  U_ENC : enc_8b10b
    port map (
      clk_i     => tbi_refclk_i,
      rst_n_i   => serdes_rst_n,
      ctrl_i    => serdes_tx_k_i,
      in_8b_i   => serdes_tx_data_i,
      err_o     => serdes_tx_enc_err_o,
      dispar_o  => serdes_tx_disparity_o,
      out_10b_o => txdata_encoded);

  p_register_output : process(tbi_refclk_i)
  begin
    if rising_edge(tbi_refclk_i) then
      tx_reg <= txdata_encoded;
    end if;
  end process;

  tbi_td_o <= tx_reg;

end rtl;

