library ieee;
use ieee.std_logic_1164.all;

package wr_altera_pkg is

  component dmtd_pll is  -- arria2
    port(
      inclk0 : in  std_logic := '0'; -- 20   MHz
      c0     : out std_logic;        -- 62.5 MHz
      locked : out std_logic);
  end component;

  component dmtd_pll5 is -- arria5
    port(
      refclk   : in  std_logic := 'X'; -- 20   MHz
      outclk_0 : out std_logic;        -- 62.5 MHz
      rst      : in  std_logic := 'X';
      locked   : out std_logic);
  end component;
  
  component ref_pll is   -- arria2
    port(
      inclk0 : in  std_logic := '0'; -- 125 MHz
      c0     : out std_logic;        -- 125 MHz
      c1     : out std_logic;        -- 200 MHz
      c2     : out std_logic;        --  25 MHz
      locked : out std_logic;
      scanclk            : in  std_logic;
      phasecounterselect : in  std_logic_vector(3 downto 0);
      phasestep          : in  std_logic;
      phaseupdown        : in  std_logic;
      phasedone          : out std_logic);
  end component;

  component ref_pll5 is  -- arria5
    port(
      refclk   : in  std_logic := 'X'; -- 125 MHz
      outclk_0 : out std_logic;        -- 125 MHz
      rst      : in  std_logic := 'X';
      locked   : out std_logic);
  end component;
  
  component sys_pll is   -- arria2
    port(
      inclk0 : in  std_logic := '0'; -- 125   MHz
      c0     : out std_logic;        --  62.5 MHz
      c1     : out std_logic;        --  50   MHz
      c2     : out std_logic;        --  20   MHz
      locked : out std_logic);
  end component;

  component sys_pll5 is  -- arria5
    port(
      refclk   : in  std_logic := 'X'; -- 125   MHz
      outclk_0 : out std_logic;        --  62.5 MHz
      outclk_1 : out std_logic;        --  20   MHz
      outclk_2 : out std_logic;        -- 100   MHz
      outclk_3 : out std_logic;        -- 100   MHz
      rst      : in  std_logic := 'X';
      locked   : out std_logic);
  end component;

  component dual_region5
    port(
      inclk  : in std_logic;
      outclk : out std_logic);
  end component;
  
  component altera_butis is
    port(
      clk_ref_i   : in  std_logic;
      clk_25m_i   : in  std_logic;
      clk_scan_i  : in  std_logic;
      locked_i    : in  std_logic;
      pps_i       : in  std_logic; -- ref_clk
      phasedone_i : in  std_logic;
      phasesel_o  : out std_logic_vector(3 downto 0);
      phasestep_o : out std_logic);
  end component;
  
  component wr_arria2_phy
    generic (
      g_tx_latch_edge : std_logic := '1';
      g_rx_latch_edge : std_logic := '0');
    port (
      clk_reconf_i   : in  std_logic;
      clk_pll_i      : in  std_logic;
      clk_cru_i      : in  std_logic;
      clk_sys_i      : in  std_logic;
      rstn_sys_i     : in  std_logic;
      locked_o       : out std_logic;
      loopen_i       : in  std_logic;
      drop_link_i    : in  std_logic;
      tx_data_i      : in  std_logic_vector(7 downto 0);
      tx_k_i         : in  std_logic;
      tx_disparity_o : out std_logic;
      tx_enc_err_o   : out std_logic;
      rx_rbclk_o     : out std_logic;
      rx_data_o      : out std_logic_vector(7 downto 0);
      rx_k_o         : out std_logic;
      rx_enc_err_o   : out std_logic;
      rx_bitslide_o  : out std_logic_vector(3 downto 0);
      pad_txp_o      : out std_logic;
      pad_rxp_i      : in  std_logic := '0');
  end component;

  component wr_arria5_phy is
    generic (
      g_tx_latch_edge : std_logic := '1';
      g_rx_latch_edge : std_logic := '1');
    port (
      clk_reconf_i   : in  std_logic;
      clk_pll_i      : in  std_logic;
      clk_sys_i      : in  std_logic;
      rstn_sys_i     : in  std_logic;
      locked_o       : out std_logic;
      loopen_i       : in  std_logic;
      drop_link_i    : in  std_logic;
      tx_data_i      : in  std_logic_vector(7 downto 0);
      tx_k_i         : in  std_logic;
      tx_disparity_o : out std_logic;
      tx_enc_err_o   : out std_logic;
      rx_rbclk_o     : out std_logic;
      rx_data_o      : out std_logic_vector(7 downto 0);
      rx_k_o         : out std_logic;
      rx_enc_err_o   : out std_logic;
      rx_bitslide_o  : out std_logic_vector(3 downto 0);
      pad_txp_o      : out std_logic;
      pad_rxp_i      : in  std_logic := '0');
  end component;

end wr_altera_pkg;
