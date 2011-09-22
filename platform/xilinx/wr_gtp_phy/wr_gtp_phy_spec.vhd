library ieee;
use ieee.std_logic_1164.all;

entity wr_gtp_phy_spec_wrapper is

  generic (
    g_simulation : integer := 0);

  port(
    sfp_ref_clk_i      : in  std_logic;
    sfp_ref_clk_o      : out std_logic;
    sfp_tx_data_i      : in  std_logic_vector(7 downto 0);
    sfp_tx_k_i         : in  std_logic;
    sfp_tx_disparity_o : out std_logic;
    sfp_tx_enc_err_o   : out std_logic;
    sfp_rx_rbclk_o     : out std_logic;
    sfp_rx_data_o      : out std_logic_vector(7 downto 0);
    sfp_rx_k_o         : out std_logic;
    sfp_rx_enc_err_o   : out std_logic;
    sfp_rx_bitslide_o  : out std_logic_vector(3 downto 0);
    sfp_rst_i          : in  std_logic;
    sfp_loopen_i       : in  std_logic;

    sfp_txn_o : out std_logic;
    sfp_txp_o : out std_logic;
    
    sfp_rxn_i : in std_logic;
    sfp_rxp_i : in std_logic;

    sata0_ref_clk_i      : in  std_logic;
    sata0_ref_clk_o      : out std_logic;
    sata0_tx_data_i      : in  std_logic_vector(7 downto 0);
    sata0_tx_k_i         : in  std_logic;
    sata0_tx_disparity_o : out std_logic;
    sata0_tx_enc_err_o   : out std_logic;
    sata0_rx_rbclk_o     : out std_logic;
    sata0_rx_data_o      : out std_logic_vector(7 downto 0);
    sata0_rx_k_o         : out std_logic;
    sata0_rx_enc_err_o   : out std_logic;
    sata0_rx_bitslide_o  : out std_logic_vector(3 downto 0);
    sata0_rst_i          : in  std_logic;
    sata0_loopen_i       : in  std_logic;

    sata0_txn_o : out std_logic;
    sata0_txp_o : out std_logic;
    sata0_rxn_i : in  std_logic;
    sata0_rxp_i : in  std_logic;

    sata1_ref_clk_i : in  std_logic;
    sata1_ref_clk_o : out std_logic;

    sata1_tx_data_i      : in  std_logic_vector(7 downto 0);
    sata1_tx_k_i         : in  std_logic;
    sata1_tx_disparity_o : out std_logic;
    sata1_tx_enc_err_o   : out std_logic;
    sata1_rx_rbclk_o     : out std_logic;
    sata1_rx_data_o      : out std_logic_vector(7 downto 0);
    sata1_rx_k_o         : out std_logic;
    sata1_rx_enc_err_o   : out std_logic;
    sata1_rx_bitslide_o  : out std_logic_vector(3 downto 0);
    sata1_rst_i          : in  std_logic;
    sata1_loopen_i       : in  std_logic;

    sata1_txn_o : out std_logic;
    sata1_txp_o : out std_logic;
    sata1_rxn_i : in  std_logic;
    sata1_rxp_i : in  std_logic;

    fmc_ref_clk_i : in  std_logic;
    fmc_ref_clk_o : out std_logic;

    fmc_tx_data_i      : in  std_logic_vector(7 downto 0);
    fmc_tx_k_i         : in  std_logic;
    fmc_tx_disparity_o : out std_logic;
    fmc_tx_enc_err_o   : out std_logic;
    fmc_rx_rbclk_o     : out std_logic;
    fmc_rx_data_o      : out std_logic_vector(7 downto 0);
    fmc_rx_k_o         : out std_logic;
    fmc_rx_enc_err_o   : out std_logic;
    fmc_rx_bitslide_o  : out std_logic_vector(3 downto 0);
    fmc_rst_i          : in  std_logic;
    fmc_loopen_i       : in  std_logic;

    fmc_txn_o : out std_logic;
    fmc_txp_o : out std_logic;
    fmc_rxn_i : in  std_logic;
    fmc_rxp_i : in  std_logic
    );

end wr_gtp_phy_spec_wrapper;

architecture rtl of wr_gtp_phy_spec_wrapper is

  component wr_gtp_phy_spartan6
    generic (
      g_simulation : integer);
    port (
      ch0_ref_clk_i      : in  std_logic;
      ch0_ref_clk_o      : out std_logic;
      ch0_tx_data_i      : in  std_logic_vector(7 downto 0);
      ch0_tx_k_i         : in  std_logic;
      ch0_tx_disparity_o : out std_logic;
      ch0_tx_enc_err_o   : out std_logic;
      ch0_rx_rbclk_o     : out std_logic;
      ch0_rx_data_o      : out std_logic_vector(7 downto 0);
      ch0_rx_k_o         : out std_logic;
      ch0_rx_enc_err_o   : out std_logic;
      ch0_rx_bitslide_o  : out std_logic_vector(3 downto 0);
      ch0_rst_i          : in  std_logic;
      ch0_loopen_i       : in  std_logic;
      ch1_ref_clk_i      : in  std_logic;
      ch1_ref_clk_o      : out std_logic;
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
  
begin  -- rtl


  U_GTP1 : wr_gtp_phy_spartan6
    generic map (
      g_simulation => g_simulation)
    port map (
      ch0_ref_clk_i      => fmc_ref_clk_i,
      ch0_ref_clk_o      => fmc_ref_clk_o,
      ch0_tx_data_i      => fmc_tx_data_i,
      ch0_tx_k_i         => fmc_tx_k_i,
      ch0_tx_disparity_o => fmc_tx_disparity_o,
      ch0_tx_enc_err_o   => fmc_tx_enc_err_o,
      ch0_rx_rbclk_o     => fmc_rx_rbclk_o,
      ch0_rx_data_o      => fmc_rx_data_o,
      ch0_rx_k_o         => fmc_rx_k_o,
      ch0_rx_enc_err_o   => fmc_rx_enc_err_o,
      ch0_rx_bitslide_o  => fmc_rx_bitslide_o,
      ch0_rst_i          => fmc_rst_i,
      ch0_loopen_i       => fmc_loopen_i,
      ch1_ref_clk_i      => sata0_ref_clk_i,
      ch1_ref_clk_o      => sata0_ref_clk_o,
      ch1_tx_data_i      => sata0_tx_data_i,
      ch1_tx_k_i         => sata0_tx_k_i,
      ch1_tx_disparity_o => sata0_tx_disparity_o,
      ch1_tx_enc_err_o   => sata0_tx_enc_err_o,
      ch1_rx_data_o      => sata0_rx_data_o,
      ch1_rx_rbclk_o     => sata0_rx_rbclk_o,
      ch1_rx_k_o         => sata0_rx_k_o,
      ch1_rx_enc_err_o   => sata0_rx_enc_err_o,
      ch1_rx_bitslide_o  => sata0_rx_bitslide_o,
      ch1_rst_i          => sata0_rst_i,
      ch1_loopen_i       => sata0_loopen_i,
      pad_txn0_o         => fmc_txn_o,
      pad_txp0_o         => fmc_txp_o,
      pad_rxn0_i         => fmc_rxn_i,
      pad_rxp0_i         => fmc_rxp_i,
      pad_txn1_o         => sata0_txn_o,
      pad_txp1_o         => sata0_txp_o,
      pad_rxn1_i         => sata0_rxn_i,
      pad_rxp1_i         => sata0_rxp_i);


  U_GTP2 : wr_gtp_phy_spartan6
    generic map (
      g_simulation => g_simulation)
    port map (
      ch0_ref_clk_i      => sata1_ref_clk_i,
      ch0_ref_clk_o      => sata1_ref_clk_o,
      ch0_tx_data_i      => sata1_tx_data_i,
      ch0_tx_k_i         => sata1_tx_k_i,
      ch0_tx_disparity_o => sata1_tx_disparity_o,
      ch0_tx_enc_err_o   => sata1_tx_enc_err_o,
      ch0_rx_rbclk_o     => sata1_rx_rbclk_o,
      ch0_rx_data_o      => sata1_rx_data_o,
      ch0_rx_k_o         => sata1_rx_k_o,
      ch0_rx_enc_err_o   => sata1_rx_enc_err_o,
      ch0_rx_bitslide_o  => sata1_rx_bitslide_o,
      ch0_rst_i          => sata1_rst_i,
      ch0_loopen_i       => sata1_loopen_i,

      ch1_ref_clk_i      => sfp_ref_clk_i,
      ch1_ref_clk_o      => sfp_ref_clk_o,
      ch1_tx_data_i      => sfp_tx_data_i,
      ch1_tx_k_i         => sfp_tx_k_i,
      ch1_tx_disparity_o => sfp_tx_disparity_o,
      ch1_tx_enc_err_o   => sfp_tx_enc_err_o,
      ch1_rx_data_o      => sfp_rx_data_o,
      ch1_rx_rbclk_o     => sfp_rx_rbclk_o,
      ch1_rx_k_o         => sfp_rx_k_o,
      ch1_rx_enc_err_o   => sfp_rx_enc_err_o,
      ch1_rx_bitslide_o  => sfp_rx_bitslide_o,
      ch1_rst_i          => sfp_rst_i,
      ch1_loopen_i       => sfp_loopen_i,
      pad_txn0_o         => sata1_txn_o,
      pad_txp0_o         => sata1_txp_o,
      pad_rxn0_i         => sata1_rxn_i,
      pad_rxp0_i         => sata1_rxp_i,
      pad_txn1_o         => sfp_txn_o,
      pad_txp1_o         => sfp_txp_o,
      pad_rxn1_i         => sfp_rxn_i,
      pad_rxp1_i         => sfp_rxp_i);

end rtl;
