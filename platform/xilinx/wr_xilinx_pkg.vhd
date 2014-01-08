library ieee;
use ieee.std_logic_1164.all;

library work;
use work.genram_pkg.all;
use work.wishbone_pkg.all;
use work.sysc_wbgen2_pkg.all;
use work.wr_fabric_pkg.all;

package wr_xilinx_pkg is

  component wr_gtp_phy_spartan6
    generic (
      g_enable_ch0 : integer := 1;
      g_enable_ch1 : integer := 1;
      g_simulation : integer := 0);
    port (
      gtp_clk_i          : in  std_logic;
      ch0_ref_clk_i      : in  std_logic := '0';
      ch0_tx_data_i      : in  std_logic_vector(7 downto 0) := "00000000";
      ch0_tx_k_i         : in  std_logic                    := '0';
      ch0_tx_disparity_o : out std_logic;
      ch0_tx_enc_err_o   : out std_logic;
      ch0_rx_rbclk_o     : out std_logic;
      ch0_rx_data_o      : out std_logic_vector(7 downto 0);
      ch0_rx_k_o         : out std_logic;
      ch0_rx_enc_err_o   : out std_logic;
      ch0_rx_bitslide_o  : out std_logic_vector(3 downto 0);
      ch0_rst_i          : in  std_logic := '0';
      ch0_loopen_i       : in  std_logic := '0';
      ch1_ref_clk_i      : in  std_logic;
      ch1_tx_data_i      : in  std_logic_vector(7 downto 0) := "00000000";
      ch1_tx_k_i         : in  std_logic                    := '0';
      ch1_tx_disparity_o : out std_logic;
      ch1_tx_enc_err_o   : out std_logic;
      ch1_rx_data_o      : out std_logic_vector(7 downto 0);
      ch1_rx_rbclk_o     : out std_logic;
      ch1_rx_k_o         : out std_logic;
      ch1_rx_enc_err_o   : out std_logic;
      ch1_rx_bitslide_o  : out std_logic_vector(3 downto 0);
      ch1_rst_i          : in  std_logic                    := '0';
      ch1_loopen_i       : in  std_logic                    := '0';
      pad_txn0_o         : out std_logic;
      pad_txp0_o         : out std_logic;
      pad_rxn0_i         : in  std_logic                    := '0';
      pad_rxp0_i         : in  std_logic                    := '0';
      pad_txn1_o         : out std_logic;
      pad_txp1_o         : out std_logic;
      pad_rxn1_i         : in  std_logic                    := '0';
      pad_rxp1_i         : in  std_logic                    := '0');
  end component;

  component wr_gtx_phy_kintex7 is
    generic (
      -- set to non-zero value to speed up the simulation by reducing some delays
      g_simulation : integer := 0);
    port (
      clk_gtx_i      : in  std_logic;
      tx_out_clk_o   : out std_logic;
      tx_data_i      : in  std_logic_vector(15 downto 0);
      tx_k_i         : in  std_logic_vector(1 downto 0);
      tx_disparity_o : out std_logic;
      tx_enc_err_o   : out std_logic;
      rx_rbclk_o     : out std_logic;
      rx_data_o      : out std_logic_vector(15 downto 0);
      rx_k_o         : out std_logic_vector(1 downto 0);
      rx_enc_err_o   : out std_logic;
      rx_bitslide_o  : out std_logic_vector(4 downto 0);
      rst_i          : in  std_logic;
      loopen_i       : in  std_logic;
      pad_txn_o      : out std_logic;
      pad_txp_o      : out std_logic;
      pad_rxn_i      : in  std_logic := '0';
      pad_rxp_i      : in  std_logic := '0');
  end component;

end wr_xilinx_pkg;
