library ieee;
use ieee.std_logic_1164.all;

library work;
use work.genram_pkg.all;
use work.wbconmax_pkg.all;
use work.wishbone_pkg.all;
use work.wr_fabric_pkg.all;


package wrcore_pkg is

  component wb_conmax_top
    generic (
      g_rf_addr : integer range 0 to 15);
    port (
      clk_i        :     std_logic;
      rst_i        :     std_logic;
      wb_masters_i : in  t_conmax_masters_i;
      wb_masters_o : out t_conmax_masters_o;
      wb_slaves_i  : in  t_conmax_slaves_i;
      wb_slaves_o  : out t_conmax_slaves_o);
  end component;


  -----------------------------------------------------------------------------
  --Dual-port RAM
  -----------------------------------------------------------------------------
  component wrc_dpram is
  generic(
    g_size        : natural := 16;  -- 16 * 32bit = 64kB
    g_init_file   : string  := ""
  );
  port(
    clk_i      : in std_logic;
    rst_n_i    : in std_logic;

    --PORT A (Wishbone)
    wb_addr_i  : in  std_logic_vector(f_log2_size(g_size)-1 downto 0); 
    wb_data_i  : in  std_logic_vector(31 downto 0); 
    wb_data_o  : out std_logic_vector(31 downto 0); 
    wb_sel_i   : in  std_logic_vector(3 downto 0); 
    wb_cyc_i   : in  std_logic;
    wb_stb_i   : in  std_logic;
    wb_we_i    : in  std_logic;
    wb_ack_o   : out std_logic;
    --PORT B (miniNIC)
    mem_addr_i : in  std_logic_vector(f_log2_size(g_size)-1 downto 0);
    mem_data_i : in  std_logic_vector(31 downto 0);
    mem_data_o : out std_logic_vector(31 downto 0);
    mem_wr_i   : in  std_logic
  );
  end component;

  component wrc_periph
    generic (
      g_gpio_pins    : natural;
      g_virtual_uart : natural;
      g_tics_period  : integer);
    port (
      clk_sys_i  : in  std_logic;
      clk_ref_i  : in  std_logic;
      rst_n_i    : in  std_logic;
      gpio_o     : out std_logic_vector(g_gpio_pins-1 downto 0);
      gpio_i     : in  std_logic_vector(g_gpio_pins-1 downto 0);
      gpio_dir_o : out std_logic_vector(g_gpio_pins-1 downto 0);
      uart_rxd_i : in  std_logic;
      uart_txd_o : out std_logic;
      rst_cpu_n_o : out std_logic;
      rst_net_n_o : out std_logic;
      wb_addr_i  : in  std_logic_vector(11 downto 0);
      wb_data_i  : in  std_logic_vector(31 downto 0);
      wb_data_o  : out std_logic_vector(31 downto 0);
      wb_sel_i   : in  std_logic_vector(3 downto 0);
      wb_stb_i   : in  std_logic;
      wb_cyc_i   : in  std_logic;
      wb_we_i    : in  std_logic;
      wb_ack_o   : out std_logic);
  end component;

  component wb_reset
    port (
      clk_i      : in  std_logic;
      rst_n_i    : in  std_logic;
      genrst_n_o : out std_logic;
      wb_addr_i  : in  std_logic_vector(1 downto 0);
      wb_data_i  : in  std_logic_vector(31 downto 0);
      wb_data_o  : out std_logic_vector(31 downto 0);
      wb_sel_i   : in  std_logic_vector(3 downto 0);
      wb_stb_i   : in  std_logic;
      wb_cyc_i   : in  std_logic;
      wb_we_i    : in  std_logic;
      wb_ack_o   : out std_logic);
  end component;        


  component wrc_lm32
    generic (
      g_addr_width : integer;
      g_num_irqs   : integer);
    port (
      clk_i            : in  std_logic;
      rst_n_i          : in  std_logic;
      irq_i            : in  std_logic_vector(g_num_irqs-1 downto 0);
      iwb_adr_o        : out std_logic_vector(g_addr_width-1 downto 0);
      iwb_dat_o        : out std_logic_vector(31 downto 0);
      iwb_dat_i        : in  std_logic_vector(31 downto 0);
      iwb_cyc_o        : out std_logic;
      iwb_stb_o        : out std_logic;
      iwb_sel_o        : out std_logic_vector(3 downto 0);
      iwb_we_o         : out std_logic;
      iwb_ack_i        : in  std_logic;
      dwb_adr_o        : out std_logic_vector(g_addr_width-1 downto 0);
      dwb_dat_o        : out std_logic_vector(31 downto 0);
      dwb_dat_i        : in  std_logic_vector(31 downto 0);
      dwb_cyc_o        : out std_logic;
      dwb_stb_o        : out std_logic;
      dwb_sel_o        : out std_logic_vector(3 downto 0);
      dwb_we_o         : out std_logic;
      dwb_ack_i        : in  std_logic;
      jwb_adr_i        : in  std_logic_vector(g_addr_width-1 downto 0);
      jwb_dat_i        : in  std_logic_vector(31 downto 0);
      jwb_dat_o        : out std_logic_vector(31 downto 0);
      jwb_cyc_i        : in  std_logic;
      jwb_stb_i        : in  std_logic;
      jwb_sel_i        : in  std_logic_vector(3 downto 0);
      jwb_we_i         : in  std_logic;
      jwb_ack_o        : out std_logic;
      trace_pc_o       : out std_logic_vector(31 downto 0);
      trace_pc_valid_o : out std_logic;
      trace_eret_o     : out std_logic);
  end component;



  component xwr_mini_nic
    generic (
      g_interface_mode       : t_wishbone_interface_mode;
      g_address_granularity  : t_wishbone_address_granularity;
      g_memsize_log2         : integer;
      g_buffer_little_endian : boolean);
    port (
      clk_sys_i        : in  std_logic;
      rst_n_i          : in  std_logic;
      mem_data_o       : out std_logic_vector(31 downto 0);
      mem_addr_o       : out std_logic_vector(g_memsize_log2-1 downto 0);
      mem_data_i       : in  std_logic_vector(31 downto 0);
      mem_wr_o         : out std_logic;
      src_o            : out t_wrf_source_out;
      src_i            : in  t_wrf_source_in;
      snk_o            : out t_wrf_sink_out;
      snk_i            : in  t_wrf_sink_in;
      txtsu_port_id_i  : in  std_logic_vector(4 downto 0);
      txtsu_frame_id_i : in  std_logic_vector(16 - 1 downto 0);
      txtsu_tsval_i    : in  std_logic_vector(28 + 4 - 1 downto 0);
      txtsu_valid_i    : in  std_logic;
      txtsu_ack_o      : out std_logic;
      wb_i             : in  t_wishbone_slave_in;
      wb_o             : out t_wishbone_slave_out);
  end component;

  component wr_softpll
    generic (
      g_deglitcher_threshold : integer;
      g_tag_bits             : integer;
      g_interface_mode       : t_wishbone_interface_mode := CLASSIC;
      g_address_granularity  : t_wishbone_address_granularity := WORD);
    port (
      clk_sys_i         : in  std_logic;
      rst_n_i           : in  std_logic;
      clk_ref_i         : in  std_logic;
      clk_dmtd_i        : in  std_logic;
      clk_rx_i          : in  std_logic;
      clk_aux_i         : in  std_logic := '0';
      dac_hpll_data_o   : out std_logic_vector(15 downto 0);
      dac_hpll_load_o   : out std_logic;
      dac_dmpll_data_o  : out std_logic_vector(15 downto 0);
      dac_dmpll_load_o  : out std_logic;
      dac_aux_data_o    : out std_logic_vector(23 downto 0);
      dac_aux_load_o    : out std_logic;
      clk_aux_lock_en_i : in  std_logic;
      clk_aux_locked_o  : out std_logic;
      wb_adr_i          : in  std_logic_vector(6 downto 0);
      wb_dat_i          : in  std_logic_vector(31 downto 0);
      wb_dat_o          : out std_logic_vector(31 downto 0);
      wb_cyc_i          : in  std_logic;
      wb_sel_i          : in  std_logic_vector(3 downto 0);
      wb_stb_i          : in  std_logic;
      wb_we_i           : in  std_logic;
      wb_ack_o          : out std_logic;
      wb_stall_o        : out std_logic;
      wb_irq_o          : out std_logic;
      debug_o           : out std_logic_vector(3 downto 0));
  end component;

  component wr_pps_gen
    generic (
      g_interface_mode      : t_wishbone_interface_mode := CLASSIC;
      g_address_granularity : t_wishbone_address_granularity := WORD);
    port (
      clk_ref_i       : in  std_logic;
      clk_sys_i       : in  std_logic;
      rst_n_i         : in  std_logic;
      wb_adr_i        : in  std_logic_vector(4 downto 0);
      wb_dat_i        : in  std_logic_vector(31 downto 0);
      wb_dat_o        : out std_logic_vector(31 downto 0);
      wb_cyc_i        : in  std_logic;
      wb_sel_i        : in  std_logic_vector(3 downto 0);
      wb_stb_i        : in  std_logic;
      wb_we_i         : in  std_logic;
      wb_ack_o        : out std_logic;
      wb_stall_o      : out std_logic;
      pps_in_i        : in  std_logic;
      pps_csync_o     : out std_logic;
      pps_out_o       : out std_logic;
      tm_utc_o        : out std_logic_vector(39 downto 0);
      tm_cycles_o     : out std_logic_vector(27 downto 0);
      tm_time_valid_o : out std_logic);
  end component;
  
end wrcore_pkg;
