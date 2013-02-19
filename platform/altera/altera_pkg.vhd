library ieee;
use ieee.std_logic_1164.all;

library work;
use work.genram_pkg.all;
use work.wishbone_pkg.all;
use work.sysc_wbgen2_pkg.all;
use work.wr_fabric_pkg.all;

package wr_altera_pkg is

  component flash_loader
     port (
        noe_in      : in std_logic
     );
  end component;

  component pow_reset is
    port (
      clk    : in     std_logic;        -- 125Mhz
      nreset : buffer std_logic
      );
  end component;

  component dmtd_pll
    port
      (
        inclk0 : in  std_logic := '0'; -- 20   MHz
        c0     : out std_logic;        -- 62.5 MHz
        locked : out std_logic
        );
  end component;

  component ref_pll
    port
      (
        inclk0 : in  std_logic := '0'; -- 125 MHz
        c0     : out std_logic;        -- 125 MHz
        locked : out std_logic
        );
  end component;

  component sys_pll
    port
      (
        inclk0 : in  std_logic := '0'; -- 125   MHz
        c0     : out std_logic;        -- 125   MHz
        c1     : out std_logic;        --  50   MHz
        c2     : out std_logic;        --  62.5 MHz
        c3     : out std_logic;        --  20   MHz
        c4     : out std_logic;        -- 100   MHz
        locked : out std_logic
        );
  end component;

  component wr_gxb_phy_arriaii
    generic (
      g_simulation      : integer;
      g_force_disparity : integer);
    port (
      clk_reconf_i   : in  std_logic;
      clk_ref_i      : in  std_logic;
      tx_clk_o       : out std_logic;
      tx_data_i      : in  std_logic_vector(7 downto 0);
      tx_k_i         : in  std_logic;
      tx_disparity_o : out std_logic;
      tx_enc_err_o   : out std_logic;
      rx_rbclk_o     : out std_logic;
      rx_data_o      : out std_logic_vector(7 downto 0);
      rx_k_o         : out std_logic;
      rx_enc_err_o   : out std_logic;
      rx_bitslide_o  : out std_logic_vector(3 downto 0);
      rst_i          : in  std_logic;
      loopen_i       : in  std_logic;
      pad_txp_o      : out std_logic;
      pad_rxp_i      : in  std_logic := '0');
  end component;

end wr_altera_pkg;
