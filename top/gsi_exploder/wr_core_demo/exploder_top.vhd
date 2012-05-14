
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.gencores_pkg.all;
use work.wrcore_pkg.all;
use work.wr_fabric_pkg.all;

library work;
use work.wishbone_pkg.all;
use work.wb_cores_pkg_gsi.all;
--use work.wrc_bin_pkg.all;
use work.xwr_eca_pkg.all;

entity exploder_top is
  port
    (
      clk_20m_vcxo_i    : in std_logic;  -- 20MHz VCXO clock
      clk_125m_pllref_p : in std_logic;  -- 125 MHz PLL reference

      L_CLKp : in std_logic;            -- local clk from 125Mhz oszillator
      nres   : in std_logic;            -- powerup reset

      -----------------------------------------
      -- UART on front panel
      -----------------------------------------
      uart_rxd_i : in  std_logic;
      uart_txd_o : out std_logic;

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
      -- LED on baseboard
		-- hpv0: red
		-- hpv1: green
		-- hpv2: orange
		-- hpv3: blue
      -----------------------------------------
      hpv   : out std_logic_vector(7 downto 0);
		
		la_port_o : out std_logic_vector(3 downto 0);
		la_port_i : in std_logic_vector(1 downto 0)


      );

end exploder_top;

architecture rtl of exploder_top is

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
      g_dpram_initv         : t_xwb_dpram_init               := c_xwb_dpram_init_nothing;
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

      owr_en_o : out std_logic_vector(1 downto 0);
      owr_i    : in  std_logic_vector(1 downto 0);

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
  
  component xetherbone_core
   
    port (
      clk_sys_i : in  std_logic;
      rst_n_i   : in  std_logic;
      snk_i     : in  t_wrf_sink_in;
      snk_o     : out t_wrf_sink_out;
      src_i     : in  t_wrf_source_in;
      src_o     : out t_wrf_source_out;
      master_i  : in  t_wishbone_master_in;
      master_o  : out t_wishbone_master_out);
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
  
  constant c_xwr_gpio_32_sdb : t_sdb_device := (
    abi_class     => x"0000", -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"4", -- 8/16/32-bit port granularity
    sdb_component => (
    addr_first    => x"0000000000000000",
    addr_last     => x"000000000000001f",
    product => (
    vendor_id     => x"0000000000000651", -- GSI
    device_id     => x"35aa6b95",
    version       => x"00000001",
    date          => x"20120305",
    name          => "GSI_GPIO_32        ")));
    
  component flash_loader
     port (
        noe_in      : in std_logic
     );
  end component;

  -- WR core layout
  constant c_wrcore_bridge_sdb : t_sdb_bridge := f_xwb_bridge_manual_sdb(x"0003ffff", x"00030000");
  
  -- Ref clock crossbar
  constant c_ref_slaves  : natural := 3;
  constant c_ref_masters : natural := 1;
  constant c_ref_layout : t_sdb_record_array(c_ref_slaves-1 downto 0) :=
   (0 => f_sdb_embed_device(c_xwr_gpio_32_sdb,            x"00000000"),
    1 => f_sdb_embed_device(c_xwr_eca_sdb,                x"00040000"),
    2 => f_sdb_embed_device(c_xwr_wb_timestamp_latch_sdb, x"00080000"));
  constant c_ref_sdb_address : t_wishbone_address := x"000C0000";
  constant c_ref_bridge : t_sdb_bridge := 
    f_xwb_bridge_layout_sdb(true, c_ref_layout, c_ref_sdb_address);
  
  signal cbar_ref_slave_i  : t_wishbone_slave_in_array (c_ref_masters-1 downto 0);
  signal cbar_ref_slave_o  : t_wishbone_slave_out_array(c_ref_masters-1 downto 0);
  signal cbar_ref_master_i : t_wishbone_master_in_array(c_ref_slaves-1 downto 0);
  signal cbar_ref_master_o : t_wishbone_master_out_array(c_ref_slaves-1 downto 0);
  
  -- Top crossbar layout
  constant c_slaves : natural := 3;
  constant c_masters : natural := 1;
  constant c_test_dpram_size : natural := 2048;
  constant c_layout : t_sdb_record_array(c_slaves-1 downto 0) :=
   (0 => f_sdb_embed_device(f_xwb_dpram(c_test_dpram_size), x"00000000"),
    1 => f_sdb_embed_bridge(c_ref_bridge,                   x"00100000"),
    2 => f_sdb_embed_bridge(c_wrcore_bridge_sdb ,           x"00200000"));
  constant c_sdb_address : t_wishbone_address := x"00300000";

  signal cbar_slave_i  : t_wishbone_slave_in_array (c_masters-1 downto 0);
  signal cbar_slave_o  : t_wishbone_slave_out_array(c_masters-1 downto 0);
  signal cbar_master_i : t_wishbone_master_in_array(c_slaves-1 downto 0);
  signal cbar_master_o : t_wishbone_master_out_array(c_slaves-1 downto 0);
  
  
  
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

  signal nreset        : std_logic := '0';
  signal led_green		: std_logic;
  signal led_red			: std_logic;

  signal clk_reconf : std_logic;
  
  signal mb_src_out    : t_wrf_source_out;
  signal mb_src_in     : t_wrf_source_in;
  signal mb_snk_out    : t_wrf_sink_out;
  signal mb_snk_in     : t_wrf_sink_in;
  signal mb_master_out : t_wishbone_master_out;
  signal mb_master_in  : t_wishbone_master_in;
  
  signal dummy_gpio, gpio_out : std_logic_vector(31 downto 0);
  signal pio_reg:	std_logic_vector(7 downto 0);
  signal ext_pps: std_logic;
  
  signal tm_utc    : std_logic_vector(39 downto 0);
  signal tm_cycles : std_logic_vector(27 downto 0);

  signal fake_tm_utc    : std_logic_vector(39 downto 0);
  signal fake_tm_cycles : std_logic_vector(27 downto 0);
  
  signal triggers : std_logic_vector(3 downto 0);
  signal eca_out	: std_logic_vector(31 downto 0);
  
begin
  
  Inst_flash_loader_v01 : flash_loader
    port map (
      noe_in   => '0'
    );
   
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
      g_dpram_initv         => c_xwb_dpram_init_nothing, -- wrc_bin_init,
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

		
      led_green_o => led_green,
      led_red_o => led_red,
      scl_i  => '0',
      sda_i  => '0',
      btn1_i => '0',
      btn2_i => '0',

      uart_rxd_i => uart_rxd_i,
      uart_txd_o => uart_txd_o,

      owr_i => (others => '0'),

      slave_i => cbar_master_o(2),
      slave_o => cbar_master_i(2),

      wrf_src_i => mb_snk_out,
      wrf_src_o => mb_snk_in,

      wrf_snk_i => mb_src_out,
      wrf_snk_o => mb_src_in,

      pps_p_o => pps,

      dac_hpll_load_p1_o => dac_hpll_load_p1,
      dac_hpll_data_o    => dac_hpll_data,

      dac_dpll_load_p1_o => dac_dpll_load_p1,
      dac_dpll_data_o    => dac_dpll_data,
      
      tm_utc_o    => tm_utc,
      tm_cycles_o => tm_cycles
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
      extended_o => ext_pps);
		
  test_ram : xwb_dpram
    generic map(
      g_size                  => c_test_dpram_size,
      g_init_file             => "",
      g_must_have_init_file   => false,
      g_slave1_interface_mode => PIPELINED,
      g_slave2_interface_mode => PIPELINED,
      g_slave1_granularity    => BYTE,
      g_slave2_granularity    => WORD)  
    port map(
      clk_sys_i => l_clkp,
      rst_n_i   => nreset,

      slave1_i => cbar_master_o(0),
      slave1_o => cbar_master_i(0),
      slave2_i => cc_dummy_slave_in,
      slave2_o => open
    );
    
  U_ebone : xetherbone_core
    port map (
      clk_sys_i => l_clkp,
      rst_n_i   => nreset,
      src_o     => mb_src_out,
      src_i     => mb_src_in,
      snk_o     => mb_snk_out,
      snk_i     => mb_snk_in,
      master_o  => cbar_slave_i(0),
      master_i  => cbar_slave_o(0));
  
--fake_timestamp_1: fake_timestamp
--   port map (
--     ref_clk_i   => clk_125m_pllref_p,
--     nRSt_i      => nreset,
--     cnt_clr     => '0',
--     tm_utc_o    => fake_tm_utc,
--     tm_cycles_o => fake_tm_cycles);
--	  

  
  triggers <= pio_reg(0 downto 0) & la_port_i(0 downto 0) & eca_out(0) & pps;
  
  TLU : wb_timestamp_latch
    generic map (
      g_num_triggers => 4,
      g_fifo_depth   => 10)
    port map (
      ref_clk_i       => clk_125m_pllref_p,
      sys_clk_i       => clk_125m_pllref_p,
      nRSt_i          => nreset,
      triggers_i      => triggers,
      tm_time_valid_i => '0',
      tm_utc_i        => tm_utc,
      tm_cycles_i     => tm_cycles,
      wb_slave_i      => cbar_ref_master_o(2),
      wb_slave_o      => cbar_ref_master_i(2));
  
  ECA : xwr_eca
    port map(
      clk_i      => clk_125m_pllref_p,
      rst_n_i    => nreset,
      slave_i    => cbar_ref_master_o(1),
      slave_o    => cbar_ref_master_i(1),
      tm_utc_i   => tm_utc,
      tm_cycle_i => tm_cycles,
      toggle_o   => eca_out);
  
  cbar_ref_master_i(0) <= mb_master_in;
  mb_master_out <= cbar_ref_master_o(0);
  gpio : process(clk_125m_pllref_p)
  begin
    if rising_edge(clk_125m_pllref_p) then
      if mb_master_out.cyc = '1' and mb_master_out.stb = '1' and mb_master_out.we = '1' then
        pio_reg <= mb_master_out.dat(7 downto 0);
      end if;
      mb_master_in.ack <= mb_master_out.cyc and mb_master_out.stb;
    end if;
  end process;
  mb_master_in.int <= '0';
  mb_master_in.err <= '0';
  mb_master_in.rty <= '0';
  mb_master_in.stall <= '0';
  mb_master_in.dat <= std_logic_vector(to_unsigned(0,mb_master_in.dat'length-pio_reg'length)) & pio_reg;
	
  GSI_REF_CON : xwb_sdb_crossbar
   generic map(
     g_num_masters => c_ref_masters,
     g_num_slaves  => c_ref_slaves,
     g_registered  => true,
     g_wraparound  => true,
     g_layout      => c_ref_layout,
     g_sdb_addr    => c_ref_sdb_address)
   port map(
     clk_sys_i     => clk_125m_pllref_p,
     rst_n_i       => nreset,
     -- Master connections (INTERCON is a slave)
     slave_i       => cbar_ref_slave_i,
     slave_o       => cbar_ref_slave_o,
     -- Slave connections (INTERCON is a master)
     master_i      => cbar_ref_master_i,
     master_o      => cbar_ref_master_o);
  
  cross_my_clocks : xwb_clock_crossing
    port map(
      rst_n_i      => nreset,
      slave_clk_i  => l_clkp,
      slave_i      => cbar_master_o(1),
      slave_o      => cbar_master_i(1),
      master_clk_i => clk_125m_pllref_p,
      master_i     => cbar_ref_slave_o(0),
      master_o     => cbar_ref_slave_i(0));
   
  GSI_CON : xwb_sdb_crossbar
   generic map(
     g_num_masters => c_masters,
     g_num_slaves  => c_slaves,
     g_registered  => true,
     g_wraparound  => true,
     g_layout      => c_layout,
     g_sdb_addr    => c_sdb_address)
   port map(
     clk_sys_i     => l_clkp,
     rst_n_i       => nreset,
     -- Master connections (INTERCON is a slave)
     slave_i       => cbar_slave_i,
     slave_o       => cbar_slave_o,
     -- Slave connections (INTERCON is a master)
     master_i      => cbar_master_i,
     master_o      => cbar_master_o);

-- gpio_out to leds
	
	hpv(3) <= not ext_pps;
		
	-- unused leds off	
	hpv(2) <= '1';
	
	
	
	-- 4 more gpios
	--la_port_o(3 downto 1) <= pio_reg(6 downto 4);
	la_port_o(0) <= eca_out(0); 
	hpv(7) 		 <= eca_out(1);
	hpv(6 downto 4) <= pio_reg(2 downto 0);
	
	-- wr status leds
	hpv(1) <= not led_green;
	hpv(0) <= not led_red;

  sfp_tx_disable_o <= '0';

  
end rtl;


