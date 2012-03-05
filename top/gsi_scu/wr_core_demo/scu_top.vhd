
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.gencores_pkg.all;
use work.wrcore_pkg.all;
use work.wr_fabric_pkg.all;

library work;
use work.wishbone_pkg.all;


entity scu_top is
  port
    (
      clk_20m_vcxo_i    : in std_logic;  -- 20MHz VCXO clock
      clk_125m_pllref_p : in std_logic;  -- 125 MHz PLL reference

      L_CLKp : in std_logic;            -- local clk from 125Mhz oszillator
      nres   : in std_logic;            -- powerup reset

      -----------------------------------------
      -- UART on front panel
      -----------------------------------------
      uart_rxd_i : in  std_logic_vector(1 downto 0);
      uart_txd_o : out std_logic_vector(1 downto 0);

      serial_to_cb_o : out std_logic;

      sfp_tx_disable_o : out std_logic;
      sfp_txp_o        : out std_logic;
      sfp_rxp_i        : in  std_logic;
      ------------------------------------------------------------------------
      -- WR DAC signals
      ------------------------------------------------------------------------
      dac_sclk         : out std_logic;
      dac_din          : out std_logic;
      ndac_cs          : out std_logic_vector(2 downto 1);

      -----------------------------------------
      -- LEMO on front panel
      -----------------------------------------
      lemo_io    : out std_logic_vector(2 downto 1);
      lemo_en_in : out std_logic_vector(2 downto 1);
      lemo_led   : out std_logic_vector(2 downto 1);
		
		LPC_AD			: inout std_logic_vector(3 downto 0);
		LPC_FPGA_CLK	: in std_logic;
		LPC_SERIRQ		: in std_logic;
		nLPC_DRQ0		: in std_logic;
		nLPC_FRAME		: in std_logic;
		nPCI_RESET		: in std_logic
      );

end scu_top;

architecture rtl of scu_top is

  component pow_reset is
    port (
      clk    : in     std_logic;        -- 125Mhz
      nreset : buffer std_logic
      );
  end component;

  component dmtd_clk_pll
    port
      (
        inclk0 : in  std_logic := '0';
        c0     : out std_logic
        );
  end component;

  component sys_pll
    port
      (
        inclk0 : in  std_logic := '0';
        c0     : out std_logic;
        c1     : out std_logic;
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

  component spec_serial_dac_arb
    generic(
      g_invert_sclk    : boolean;
      g_num_extra_bits : integer);        
    port (
      clk_i       : in  std_logic;
      rst_n_i     : in  std_logic;
      val1_i      : in  std_logic_vector(15 downto 0);
      load1_i     : in  std_logic;
      val2_i      : in  std_logic_vector(15 downto 0);
      load2_i     : in  std_logic;
      dac_cs_n_o  : out std_logic_vector(1 downto 0);
      dac_clr_n_o : out std_logic;
      dac_sclk_o  : out std_logic;
      dac_din_o   : out std_logic);
  end component;
  
	component lpc_uart is
	port (

		lpc_clk:			in std_logic;
		lpc_serirq:		out std_logic;
		lpc_ad:			inout std_logic_vector(3 downto 0);
		lpc_frame_n:	in std_logic;
		lpc_reset_n:	in std_logic;
		
		serial_rxd:		in std_logic;
		serial_txd:		out std_logic;
		serial_dtr:		out std_logic;
		serial_dcd:		in std_logic;
		serial_dsr:		in std_logic;
		serial_ri:		in std_logic;
		serial_cts:		in std_logic;
		serial_rts:		out std_logic;
		
		      
		seven_seg_L:	out std_logic_vector(7 downto 0);    -- SSeg Data output
		seven_seg_H:	out std_logic_vector(7 downto 0)    -- SSeg Data output  
	
	);
	end component lpc_uart;


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
  
  signal lpc_oe : std_logic;
  signal lad_o : std_logic_vector(3 downto 0);
  
begin
  
  reset : pow_reset
    port map (
      clk    => l_cLKp,
      nreset => nreset
      );

  
  dmtd_clk_pll_inst : dmtd_clk_pll port map (
    inclk0 => clk_20m_vcxo_i,           -- 20Mhz 
    c0     => pllout_clk_dmtd           -- 125Mhz
    );

  sys_pll_inst : sys_pll port map (
    inclk0 => L_CLKp,                   -- 125Mhz 
    c0     => pllout_clk_sys,           -- 125Mhy sys clk
    c1     => clk_reconf,               -- 40Mhz for reconfig block
    locked => open
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
      clk_sys_i  =>  l_clkp,
      clk_dmtd_i => pllout_clk_dmtd,
      clk_ref_i  => clk_125m_pllref_p,
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

      uart_rxd_i => uart_rxd_i(0),
      uart_txd_o => uart_txd_o(0),

      owr_i => '0',

      slave_i => wrc_slave_in,
      slave_o => wrc_slave_out,

      wrf_src_i => c_dummy_src_in,
      wrf_snk_i => c_dummy_snk_in,

      pps_p_o => pps,

      dac_hpll_load_p1_o => dac_hpll_load_p1,
      dac_hpll_data_o    => dac_hpll_data,

      dac_dpll_load_p1_o => dac_dpll_load_p1,
      dac_dpll_data_o    => dac_dpll_data
      );

  wr_gxb_phy_arriaii_1 : wr_gxb_phy_arriaii
    generic map (
      g_simulation      => 0,
      g_force_disparity => 1)
    port map (
      clk_reconf_i   => clk_reconf,
      clk_ref_i      => clk_125m_pllref_p,
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

  U_DAC_ARB : spec_serial_dac_arb
    generic map (
      g_invert_sclk    => false,
      g_num_extra_bits => 8)            -- AD DACs with 24bit interface

    port map (
      clk_i   => l_clkp,
      rst_n_i => nreset,

      val1_i  => dac_dpll_data,
      load1_i => dac_dpll_load_p1,

      val2_i  => dac_hpll_data,
      load2_i => dac_hpll_load_p1,

      dac_cs_n_o(0) => ndac_cs(1),
      dac_cs_n_o(1) => ndac_cs(2),
      dac_clr_n_o   => open,
      dac_sclk_o    => dac_sclk,
      dac_din_o     => dac_din);

  U_Extend_PPS : gc_extend_pulse
    generic map (
      g_width => 10000000)
    port map (
      clk_i      => l_clkp,
      rst_n_i    => nreset,
      pulse_i    => pps,
      extended_o => lemo_led(1));
		
		
	lpc_slave: lpc_uart
		port map (
						lpc_clk => LPC_FPGA_CLK,
						lpc_serirq => open,
						lpc_ad => LPC_AD,
						lpc_frame_n => nLPC_FRAME,
						lpc_reset_n => nPCI_RESET,
		
						serial_rxd => uart_rxd_i(1),
						serial_txd => uart_txd_o(1),
						serial_dtr => open,
						serial_dcd => '0',
						serial_dsr => '0',
						serial_ri => '0',
						serial_cts => '0',
						serial_rts => open,
		
		      
						seven_seg_L => open,
						seven_seg_H => open
						);
						
	--LPC_AD <= lad_o when lpc_oe = '1' else "ZZZZ";

  serial_to_cb_o   <= '0';
  wrc_slave_in.cyc <= '0';

  sfp_tx_disable_o <= '0';

  lemo_en_in <= "00";                   -- configured as output
  lemo_io(1) <= pps;
  lemo_io(2) <= '0';

	--LPC_SERIRQ <= '0';
	--nLPC_DRQ0 <= '1';
	
  
end rtl;


