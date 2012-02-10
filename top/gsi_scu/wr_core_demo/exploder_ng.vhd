
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.gencores_pkg.all;
use work.wrcore_pkg.all;
use work.wr_fabric_pkg.all;

library work;
use work.wishbone_pkg.all;


entity EXPLODER_ng is
  port
    (
      clk_20m_vcxo_i    : in std_logic;  -- 20MHz VCXO clock
      clk_125m_pllref_p : in std_logic;  -- 125 MHz PLL reference

      L_CLKp : in std_logic;            -- local clk from 125Mhz oszillator
      nres   : in std_logic;            -- powerup reset

      -----------------------------------------
      --UART
      -----------------------------------------
      uart_rxd_i : in  std_logic;
      uart_txd_o : out std_logic;

      serial_to_cb_o : out std_logic;

      sfp_tx_disable_o : out std_logic;
      sfp_txp_o        : out std_logic;
      sfp_rxp_i        : in  std_logic;

      leds_o : out std_logic_vector(3 downto 0)
      );

end EXPLODER_ng;

architecture rtl of EXPLODER_ng is

  component pow_reset is
    port (
      clk    : in     std_logic;        -- 125Mhz
      nreset : buffer std_logic
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

  component xwr_core is
    generic(
      g_simulation          : integer                        := 0;
      g_phys_uart           : boolean                        := true;
      g_virtual_uart        : boolean                        := false;
      g_ep_rxbuf_size       : integer                        := 12;
      g_dpram_initf         : string                         := "";
      g_dpram_size          : integer                        := 16384;  --in 32-bit words
      g_interface_mode      : t_wishbone_interface_mode      := CLASSIC;
      g_address_granularity : t_wishbone_address_granularity := WORD
      );
    port(
      clk_sys_i  : in std_logic;
      clk_dmtd_i : in std_logic;
      clk_ref_i  : in std_logic;
      clk_aux_i  : in std_logic;
      rst_n_i    : in std_logic;

      dac_hpll_load_p1_o : out std_logic;
      dac_hpll_data_o    : out std_logic_vector(15 downto 0);
      dac_dpll_load_p1_o : out std_logic;
      dac_dpll_data_o    : out std_logic_vector(15 downto 0);

      phy_ref_clk_i      : in  std_logic;
      phy_tx_data_o      : out std_logic_vector(7 downto 0);
      phy_tx_k_o         : out std_logic;
      phy_tx_disparity_i : in  std_logic;
      phy_tx_enc_err_i   : in  std_logic;
      phy_rx_data_i      : in  std_logic_vector(7 downto 0);
      phy_rx_rbclk_i     : in  std_logic;
      phy_rx_k_i         : in  std_logic;
      phy_rx_enc_err_i   : in  std_logic;
      phy_rx_bitslide_i  : in  std_logic_vector(3 downto 0);
      phy_rst_o          : out std_logic;
      phy_loopen_o       : out std_logic;

      led_red_o   : out std_logic;
      led_green_o : out std_logic;
      scl_o       : out std_logic;
      scl_i       : in  std_logic;
      sda_o       : out std_logic;
      sda_i       : in  std_logic;
      btn1_i      : in  std_logic;
      btn2_i      : in  std_logic;

      uart_rxd_i : in  std_logic;
      uart_txd_o : out std_logic;

      owr_en_o : out std_logic;
      owr_i    : in  std_logic;

      slave_i : in  t_wishbone_slave_in;
      slave_o : out t_wishbone_slave_out;

      wrf_src_o : out t_wrf_source_out;
      wrf_src_i : in  t_wrf_source_in := c_dummy_src_in;
      wrf_snk_o : out t_wrf_sink_out;
      wrf_snk_i : in  t_wrf_sink_in   := c_dummy_snk_in;

      timestamps_o     : out t_txtsu_timestamp;
      timestamps_ack_i : in  std_logic := '1';

      tm_dac_value_o       : out std_logic_vector(23 downto 0);
      tm_dac_wr_o          : out std_logic;
      tm_clk_aux_lock_en_i : in  std_logic := '0';
      tm_clk_aux_locked_o  : out std_logic;
      tm_time_valid_o      : out std_logic;
      tm_utc_o             : out std_logic_vector(39 downto 0);
      tm_cycles_o          : out std_logic_vector(27 downto 0);
      pps_p_o              : out std_logic;

      dio_o       : out std_logic_vector(3 downto 0);
      rst_aux_n_o : out std_logic
      );
  end component;

  component xmini_bone
    generic (
      g_class_mask    : std_logic_vector(7 downto 0);
      g_our_ethertype : std_logic_vector(15 downto 0));
    port (
      clk_sys_i : in  std_logic;
      rst_n_i   : in  std_logic;
      src_o     : out t_wrf_source_out;
      src_i     : in  t_wrf_source_in;
      snk_o     : out t_wrf_sink_out;
      snk_i     : in  t_wrf_sink_in;
      master_o  : out t_wishbone_master_out;
      master_i  : in  t_wishbone_master_in);
  end component;


  -- LCLK from GN4124 used as system clock
  signal l_clk : std_logic;

  -- P2L colck PLL status
  signal p2l_pll_locked : std_logic;

  -- Reset
  signal rst_a : std_logic;
  signal rst   : std_logic;

  -- SPI
  signal spi_slave_select : std_logic_vector(7 downto 0);


  signal pllout_clk_sys       : std_logic;
  signal pllout_clk_dmtd      : std_logic;
  signal pllout_clk_fb_pllref : std_logic;
  signal pllout_clk_fb_dmtd   : std_logic;

  signal clk_20m_vcxo_buf : std_logic;
  signal clk_125m_pllref  : std_logic;
  signal clk_sys          : std_logic;
  signal clk_dmtd         : std_logic;
  signal dac_rst_n        : std_logic;
  signal led_divider      : unsigned(23 downto 0);

  signal wrc_scl_o : std_logic;
  signal wrc_scl_i : std_logic;
  signal wrc_sda_o : std_logic;
  signal wrc_sda_i : std_logic;
  signal dio       : std_logic_vector(3 downto 0);

  signal dac_hpll_load_p1 : std_logic;
  signal dac_dpll_load_p1 : std_logic;
  signal dac_hpll_data    : std_logic_vector(15 downto 0);
  signal dac_dpll_data    : std_logic_vector(15 downto 0);

  signal pps : std_logic;

  signal phy_tx_clk       : std_logic;
  signal phy_tx_data      : std_logic_vector(7 downto 0);
  signal phy_tx_k         : std_logic;
  signal phy_tx_disparity : std_logic;
  signal phy_tx_enc_err   : std_logic;
  signal phy_rx_data      : std_logic_vector(7 downto 0);
  signal phy_rx_rbclk     : std_logic;
  signal phy_rx_k         : std_logic;
  signal phy_rx_enc_err   : std_logic;
  signal phy_rx_bitslide  : std_logic_vector(3 downto 0);
  signal phy_rst          : std_logic;
  signal phy_loopen       : std_logic;

  signal dio_in  : std_logic_vector(4 downto 0);
  signal dio_out : std_logic_vector(4 downto 0);
  signal dio_clk : std_logic;

  signal local_reset_n  : std_logic;
  signal button1_synced : std_logic_vector(2 downto 0);

  signal wrc_slave_in  : t_wishbone_slave_in;
  signal wrc_slave_out : t_wishbone_slave_out;
  signal nreset        : std_logic := '0';

  signal clk_reconf : std_logic;
  signal divctr     : unsigned(3 downto 0);

  signal mb_src_out    : t_wrf_source_out;
  signal mb_src_in     : t_wrf_source_in;
  signal mb_snk_out    : t_wrf_sink_out;
  signal mb_snk_in     : t_wrf_sink_in;
  signal mb_master_out : t_wishbone_master_out;
  signal mb_master_in  : t_wishbone_master_in;

  signal dummy_gpio, gpio_out : std_logic_vector(31 downto 0);
  
begin

  process(l_clkp)
  begin
    if rising_edge(L_CLKp) then
      divctr <= divctr + 1;
    end if;
  end process;

  -- this is ugly, use PLL instead, I'm too lazy.
  clk_reconf <= std_logic(divctr(3));


  serial_to_cb_o   <= '0';
  wrc_slave_in.cyc <= '0';
  sfp_tx_disable_o <= '0';

  reset : pow_reset
    port map (
      clk    => l_cLKp,
      nreset => nreset
      );

  U_WR_CORE : xwr_core
    generic map (
      g_simulation          => 0,
      g_phys_uart           => true,
      g_virtual_uart        => false,
      g_ep_rxbuf_size       => 4096,
      g_dpram_initf         => "",
      g_dpram_size          => 16384,
      g_interface_mode      => PIPELINED,
      g_address_granularity => BYTE)
    port map (
      clk_sys_i  => l_cLKp,
      clk_dmtd_i => l_cLKp,
      clk_ref_i  => l_cLKp,
      clk_aux_i  => '0',
      rst_n_i    => nreset,

      phy_ref_clk_i      => phy_tx_clk,
      phy_tx_data_o      => phy_tx_data,
      phy_tx_k_o         => phy_tx_k,
      phy_tx_disparity_i => phy_tx_disparity,
      phy_tx_enc_err_i   => phy_tx_enc_err,
      phy_rx_data_i      => phy_rx_data,
      phy_rx_rbclk_i     => phy_rx_rbclk,
      phy_rx_k_i         => phy_rx_k,
      phy_rx_enc_err_i   => phy_rx_enc_err,
      phy_rx_bitslide_i  => phy_rx_bitslide,
      phy_rst_o          => phy_rst,
      phy_loopen_o       => phy_loopen,

      scl_i  => '0',
      sda_i  => '0',
      btn1_i => '0',
      btn2_i => '0',

      uart_rxd_i => uart_rxd_i,
      uart_txd_o => uart_txd_o,

      owr_i => '0',

      slave_i => wrc_slave_in,
      slave_o => wrc_slave_out,

      wrf_src_i => mb_snk_out,
      wrf_src_o => mb_snk_in,

      wrf_snk_i => mb_src_out,
      wrf_snk_o => mb_src_in
      );

  U_Altera_PHY : wr_gxb_phy_arriaii
    generic map (
      g_simulation      => 0,
      g_force_disparity => 1)
    port map (
      clk_reconf_i   => clk_reconf,
      clk_ref_i      => l_cLKp,
      tx_clk_o       => phy_tx_clk,
      tx_data_i      => phy_tx_data,
      tx_k_i         => phy_tx_k,
      tx_disparity_o => phy_tx_disparity,
      tx_enc_err_o   => phy_tx_enc_err,
      rx_rbclk_o     => phy_rx_rbclk,
      rx_data_o      => phy_rx_data,
      rx_k_o         => phy_rx_k,
      rx_enc_err_o   => phy_rx_enc_err,
      rx_bitslide_o  => phy_rx_bitslide,
      rst_i          => phy_rst,
      loopen_i       => '0',
      pad_txp_o      => sfp_txp_o,
      pad_rxp_i      => sfp_rxp_i);


  U_mbone : xmini_bone
    generic map (
      g_class_mask    => x"f0",
      g_our_ethertype => x"a0a0")
    port map (
      clk_sys_i => l_cLKp,
      rst_n_i   => nreset,
      src_o     => mb_src_out,
      src_i     => mb_src_in,
      snk_o     => mb_snk_out,
      snk_i     => mb_snk_in,
      master_o  => mb_master_out,
      master_i  => mb_master_in);

  U_GPIO : xwb_gpio_port
    generic map (
      g_interface_mode         => CLASSIC,
      g_address_granularity    => BYTE,
      g_num_pins               => 32,
      g_with_builtin_tristates => false)
    port map (
      clk_sys_i  => l_cLKp,
      rst_n_i    => nreset,
      slave_i    => mb_master_out,
      slave_o    => mb_master_in,
      gpio_b     => dummy_gpio,
      gpio_out_o => gpio_out,
      gpio_in_i  => x"00000000");

  leds_o <= gpio_out(3 downto 0);
end rtl;


