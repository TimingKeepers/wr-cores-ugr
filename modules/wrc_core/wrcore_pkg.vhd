library ieee;
use ieee.std_logic_1164.all;

library work;
use work.genram_pkg.all;
use work.wishbone_pkg.all;
use work.sysc_wbgen2_pkg.all;
use work.wr_fabric_pkg.all;
use work.endpoint_pkg.all;

package wrcore_pkg is


  ----------------------------------------------------------------------------- 
  --PPS generator
  -----------------------------------------------------------------------------
  constant c_xwr_pps_gen_sdb : t_sdb_device := (
    abi_class     => x"0000", -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"7", -- 8/16/32-bit port granularity
    sdb_component => (
    addr_first    => x"0000000000000000",
    addr_last     => x"00000000000000ff",
    product => (
    vendor_id     => x"000000000000CE42", -- CERN
    device_id     => x"de0d8ced",
    version       => x"00000001",
    date          => x"20120305",
    name          => "WR-PPS-Generator   ")));
  component xwr_pps_gen is
    generic(
      g_interface_mode       : t_wishbone_interface_mode;
      g_address_granularity  : t_wishbone_address_granularity;
      g_ref_clock_rate       : integer;
      g_ext_clock_rate       : integer;
      g_with_ext_clock_input : boolean
      );
    port (
      clk_ref_i       : in  std_logic;
      clk_sys_i       : in  std_logic;
      clk_ext_i       : in  std_logic := '0';
      rst_n_i         : in  std_logic;
      slave_i         : in  t_wishbone_slave_in;
      slave_o         : out t_wishbone_slave_out;
      pps_in_i        : in  std_logic;
      pps_csync_o     : out std_logic;
      pps_out_o       : out std_logic;
			pps_led_o				: out std_logic;
      pps_valid_o     : out std_logic;
      tm_utc_o        : out std_logic_vector(39 downto 0);
      tm_cycles_o     : out std_logic_vector(27 downto 0);
      tm_time_valid_o : out std_logic
      );
  end component;

  -----------------------------------------------------------------------------
  --Mini NIC
  -----------------------------------------------------------------------------
  constant c_xwr_mini_nic_sdb : t_sdb_device := (
    abi_class     => x"0000", -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"7", -- 8/16/32-bit port granularity
    sdb_component => (
    addr_first    => x"0000000000000000",
    addr_last     => x"00000000000000ff",
    product => (
    vendor_id     => x"000000000000CE42", -- CERN
    device_id     => x"ab28633a",
    version       => x"00000001",
    date          => x"20120305",
    name          => "WR-Mini-NIC        ")));
  component xwr_mini_nic
    generic (
      g_interface_mode       : t_wishbone_interface_mode;
      g_address_granularity  : t_wishbone_address_granularity;
      g_memsize_log2         : integer;
      g_buffer_little_endian : boolean);
    port (
      clk_sys_i           : in  std_logic;
      rst_n_i             : in  std_logic;
      mem_data_o          : out std_logic_vector(31 downto 0);
      mem_addr_o          : out std_logic_vector(g_memsize_log2-1 downto 0);
      mem_data_i          : in  std_logic_vector(31 downto 0);
      mem_wr_o            : out std_logic;
      src_o               : out t_wrf_source_out;
      src_i               : in  t_wrf_source_in;
      snk_o               : out t_wrf_sink_out;
      snk_i               : in  t_wrf_sink_in;
      txtsu_port_id_i     : in  std_logic_vector(4 downto 0);
      txtsu_frame_id_i    : in  std_logic_vector(16 - 1 downto 0);
      txtsu_tsval_i       : in  std_logic_vector(28 + 4 - 1 downto 0);
      txtsu_tsincorrect_i : in  std_logic;
      txtsu_stb_i         : in  std_logic;
      txtsu_ack_o         : out std_logic;
      wb_i                : in  t_wishbone_slave_in;
      wb_o                : out t_wishbone_slave_out);
  end component;

  -----------------------------------------------------------------------------
  -- PERIPHERIALS
  -----------------------------------------------------------------------------
  component xwr_syscon_wb
    generic(
      g_interface_mode      : t_wishbone_interface_mode;
      g_address_granularity : t_wishbone_address_granularity
      );
    port (
      rst_n_i   : in std_logic;
      clk_sys_i : in std_logic;

      slave_i : in  t_wishbone_slave_in;
      slave_o : out t_wishbone_slave_out;

      regs_i : in  t_sysc_in_registers;
      regs_o : out t_sysc_out_registers
      );
  end component;

  constant c_wrc_periph0_sdb : t_sdb_device := (
    abi_class     => x"0000", -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"7", -- 8/16/32-bit port granularity
    sdb_component => (
    addr_first    => x"0000000000000000",
    addr_last     => x"00000000000000ff",
    product => (
    vendor_id     => x"000000000000CE42", -- CERN
    device_id     => x"ff07fc47",
    version       => x"00000001",
    date          => x"20120305",
    name          => "WR-Periph-Syscon   ")));
  constant c_wrc_periph1_sdb : t_sdb_device := (
    abi_class     => x"0000", -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"7", -- 8/16/32-bit port granularity
    sdb_component => (
    addr_first    => x"0000000000000000",
    addr_last     => x"00000000000000ff",
    product => (
    vendor_id     => x"000000000000CE42", -- CERN
    device_id     => x"e2d13d04",
    version       => x"00000001",
    date          => x"20120305",
    name          => "WR-Periph-UART     ")));

  constant c_wrc_periph2_sdb : t_sdb_device := (
    abi_class     => x"0000", -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"7", -- 8/16/32-bit port granularity
    sdb_component => (
    addr_first    => x"0000000000000000",
    addr_last     => x"00000000000000ff",
    product => (
    vendor_id     => x"000000000000CE42", -- CERN
    device_id     => x"779c5443",
    version       => x"00000001",
    date          => x"20120305",
    name          => "WR-Periph-1Wire    ")));


constant c_wrc_periph3_sdb : t_sdb_device := (
    abi_class     => x"0000", -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"7", -- 8/16/32-bit port granularity
    sdb_component => (
    addr_first    => x"0000000000000000",
    addr_last     => x"00000000000000ff",
    product => (
    vendor_id     => x"000000000000CE42", -- CERN
    device_id     => x"779c5445",
    version       => x"00000001",
    date          => x"20120615",
    name          => "WR-Periph-AuxWB    ")));
  
  component wrc_periph is
    generic(
      g_phys_uart    : boolean := true;
      g_virtual_uart : boolean := false;
      g_cntr_period  : integer := 62500;
      g_mem_words    : integer := 16384
      );
    port(
      clk_sys_i   : in  std_logic;
      rst_n_i     : in  std_logic;
      rst_net_n_o : out std_logic;
      rst_wrc_n_o : out std_logic;
      led_red_o   : out std_logic;
      led_green_o : out std_logic;
      scl_o       : out std_logic;
      scl_i       : in  std_logic;
      sda_o       : out std_logic;
      sda_i       : in  std_logic;
      sfp_scl_o   : out std_logic;
      sfp_scl_i   : in  std_logic;
      sfp_sda_o   : out std_logic;
      sfp_sda_i   : in  std_logic;
      sfp_det_i   : in  std_logic;
      memsize_i   : in  std_logic_vector(3 downto 0);
      btn1_i      : in  std_logic;
      btn2_i      : in  std_logic;
      slave_i     : in  t_wishbone_slave_in_array(0 to 2);
      slave_o     : out t_wishbone_slave_out_array(0 to 2);
      uart_rxd_i  : in  std_logic;
      uart_txd_o  : out std_logic;
      owr_pwren_o : out std_logic_vector(1 downto 0);
      owr_en_o    : out std_logic_vector(1 downto 0);
      owr_i       : in  std_logic_vector(1 downto 0)
      );
  end component;

  -----------------------------------------------------------------------------
  -- Soft-PLL
  -----------------------------------------------------------------------------
  constant c_xwr_softpll_ng_sdb : t_sdb_device := (
    abi_class     => x"0000", -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"7", -- 8/16/32-bit port granularity
    sdb_component => (
    addr_first    => x"0000000000000000",
    addr_last     => x"00000000000000ff",
    product => (
    vendor_id     => x"000000000000CE42", -- CERN
    device_id     => x"65158dc0",
    version       => x"00000001",
    date          => x"20120305",
    name          => "WR-Soft-PLL        ")));
  component xwr_softpll_ng
    generic (
      g_tag_bits             : integer;
      g_num_ref_inputs       : integer;
      g_num_outputs          : integer;
      g_with_period_detector : boolean := false;
      g_with_debug_fifo      : boolean := false;
      g_with_ext_clock_input : boolean := false;
      g_with_undersampling   : boolean := false;
      g_reverse_dmtds        : boolean := false;
      g_bb_ref_divider       : integer := 1;
      g_bb_feedback_divider  : integer := 1;
      g_bb_log2_gating       : integer := 1;
      g_divide_input_by_2 : boolean := false;
      g_interface_mode       : t_wishbone_interface_mode;
      g_address_granularity  : t_wishbone_address_granularity);
    port (
      clk_sys_i       : in  std_logic;
      rst_n_i         : in  std_logic;
      clk_ref_i       : in  std_logic_vector(g_num_ref_inputs-1 downto 0);
      clk_fb_i        : in  std_logic_vector(g_num_outputs-1 downto 0);
      clk_dmtd_i      : in  std_logic;
      clk_ext_i       : in  std_logic;
      sync_p_i        : in  std_logic;
      dac_dmtd_data_o : out std_logic_vector(15 downto 0);
      dac_dmtd_load_o : out std_logic;
      dac_out_data_o  : out std_logic_vector(15 downto 0);
      dac_out_sel_o   : out std_logic_vector(3 downto 0);
      dac_out_load_o  : out std_logic;
      out_enable_i    : in  std_logic_vector(g_num_outputs-1 downto 0);
      out_locked_o    : out std_logic_vector(g_num_outputs-1 downto 0);
      slave_i         : in  t_wishbone_slave_in;
      slave_o         : out t_wishbone_slave_out;
      debug_o         : out std_logic_vector(3 downto 0);
      dbg_fifo_irq_o  : out std_logic);
  end component;

  constant cc_unused_master_in : t_wishbone_master_in :=
    ('1', '0', '0', '0', '0', cc_dummy_data);

  
  -----------------------------------------------------------------------------
  -- Public WR component definitions
  -----------------------------------------------------------------------------
  component xwr_core is
    generic(
      g_simulation                : integer                        := 0;
      g_phys_uart                 : boolean                        := true;
      g_virtual_uart              : boolean                        := false;
      g_with_external_clock_input : boolean                        := false;
      g_aux_clks                  : integer                        := 1;
      g_ep_rxbuf_size             : integer                        := 1024;
      g_dpram_initf               : string                         := "";
      g_dpram_initv               : t_xwb_dpram_init               := c_xwb_dpram_init_nothing;
      g_dpram_size                : integer                        := 20480;  --in 32-bit words
      g_interface_mode            : t_wishbone_interface_mode      := CLASSIC;
      g_address_granularity       : t_wishbone_address_granularity := WORD;
      g_aux_sdb                   : t_sdb_device                   := c_wrc_periph3_sdb
      );
    port(
      clk_sys_i  : in std_logic;
      clk_dmtd_i : in std_logic;
      clk_ref_i  : in std_logic;
      clk_aux_i  : in std_logic_vector(g_aux_clks-1 downto 0) := (others => '0');
      clk_ext_i  : in std_logic                               := '0';
      pps_ext_i  : in std_logic                               := '0';
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
      sfp_scl_o   : out std_logic;
      sfp_scl_i   : in  std_logic;
      sfp_sda_o   : out std_logic;
      sfp_sda_i   : in  std_logic;
      sfp_det_i   : in  std_logic;
      btn1_i      : in  std_logic;
      btn2_i      : in  std_logic;

      uart_rxd_i : in  std_logic;
      uart_txd_o : out std_logic;

      owr_pwren_o : out std_logic_vector(1 downto 0);
      owr_en_o    : out std_logic_vector(1 downto 0);
      owr_i       : in  std_logic_vector(1 downto 0);

      slave_i : in  t_wishbone_slave_in;
      slave_o : out t_wishbone_slave_out;

      aux_master_o : out  t_wishbone_master_out;
      aux_master_i : in t_wishbone_master_in := cc_unused_master_in;

      wrf_src_o : out t_wrf_source_out;
      wrf_src_i : in  t_wrf_source_in := c_dummy_src_in;
      wrf_snk_o : out t_wrf_sink_out;
      wrf_snk_i : in  t_wrf_sink_in   := c_dummy_snk_in;

      timestamps_o     : out t_txtsu_timestamp;
      timestamps_ack_i : in  std_logic := '1';

      tm_link_up_o         : out std_logic;
      tm_dac_value_o       : out std_logic_vector(23 downto 0);
      tm_dac_wr_o          : out std_logic;
      tm_clk_aux_lock_en_i : in  std_logic;
      tm_clk_aux_locked_o  : out std_logic;
      tm_time_valid_o      : out std_logic;
      tm_utc_o             : out std_logic_vector(39 downto 0);
      tm_cycles_o          : out std_logic_vector(27 downto 0);
      pps_p_o              : out std_logic;
			pps_led_o						 : out std_logic;

      dio_o       : out std_logic_vector(3 downto 0);
      rst_aux_n_o : out std_logic;
      
      link_ok_o : out std_logic
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

end wrcore_pkg;
