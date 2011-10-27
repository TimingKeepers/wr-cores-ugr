library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gencores_pkg.all;
use work.wishbone_pkg.all;

entity xwr_softpll is
  generic(
    g_deglitcher_threshold : integer;
    g_tag_bits             : integer;
    g_interface_mode       : t_wishbone_interface_mode      := CLASSIC;
    g_address_granularity  : t_wishbone_address_granularity := WORD
    );

  port(
    clk_sys_i  : in std_logic;
    rst_n_i    : in std_logic;
    clk_ref_i  : in std_logic;
    clk_dmtd_i : in std_logic;
    clk_rx_i   : in std_logic;
    clk_aux_i  : in std_logic := '0';

    dac_hpll_data_o : out std_logic_vector(15 downto 0);
    dac_hpll_load_o : out std_logic;

    dac_dmpll_data_o : out std_logic_vector(15 downto 0);
    dac_dmpll_load_o : out std_logic;
    
    dac_aux_data_o : out std_logic_vector(23 downto 0);
    dac_aux_load_o : out std_logic;

    clk_aux_lock_en_i : in  std_logic := '0';
    clk_aux_locked_o  : out std_logic;

    slave_i  : in  t_wishbone_slave_in;
    slave_o  : out t_wishbone_slave_out;
    wb_irq_o : out std_logic;
    debug_o  : out std_logic_vector(3 downto 0)
    );

end xwr_softpll;

architecture wrapper of xwr_softpll is

  component wr_softpll is
    generic(
      g_deglitcher_threshold : integer;
      g_tag_bits             : integer;
      g_interface_mode       : t_wishbone_interface_mode      := CLASSIC;
      g_address_granularity  : t_wishbone_address_granularity := WORD
      );
    port(
      clk_sys_i        : in  std_logic;
      rst_n_i          : in  std_logic;
      clk_ref_i        : in  std_logic;
      clk_dmtd_i       : in  std_logic;
      clk_rx_i         : in  std_logic;
      clk_aux_i        : in  std_logic := '0';
      dac_hpll_data_o  : out std_logic_vector(15 downto 0);
      dac_hpll_load_o  : out std_logic;
      dac_dmpll_data_o : out std_logic_vector(15 downto 0);
      dac_dmpll_load_o : out std_logic;
      dac_aux_data_o : out std_logic_vector(23 downto 0);
      dac_aux_load_o : out std_logic;
      clk_aux_lock_en_i : in  std_logic;
      clk_aux_locked_o  : out std_logic;
      wb_adr_i        : in  std_logic_vector(6 downto 0);
      wb_dat_i        : in  std_logic_vector(31 downto 0);
      wb_dat_o        : out std_logic_vector(31 downto 0);
      wb_cyc_i         : in  std_logic;
      wb_sel_i         : in  std_logic_vector(3 downto 0);
      wb_stb_i         : in  std_logic;
      wb_we_i          : in  std_logic;
      wb_ack_o         : out std_logic;
      wb_stall_o       : out std_logic;
      wb_irq_o         : out std_logic;
      debug_o          : out std_logic_vector(3 downto 0)
    );
  end component;
  
begin  -- behavioral

  WRAPPED_SOFTPLL : wr_softpll
    generic map(
      g_deglitcher_threshold => g_deglitcher_threshold,
      g_tag_bits             => g_tag_bits,
      g_interface_mode       => g_interface_mode,
      g_address_granularity  => g_address_granularity)
    port map (
      clk_sys_i        => clk_sys_i,
      rst_n_i          => rst_n_i,
      clk_ref_i        => clk_ref_i,
      clk_dmtd_i       => clk_dmtd_i,
      clk_rx_i         => clk_rx_i,
      clk_aux_i        => clk_aux_i,
      dac_hpll_data_o  => dac_hpll_data_o,
      dac_hpll_load_o  => dac_hpll_load_o,
      dac_dmpll_data_o => dac_dmpll_data_o,
      dac_dmpll_load_o => dac_dmpll_load_o,
      dac_aux_load_o => dac_aux_load_o,
      dac_aux_data_o => dac_aux_data_o,
      clk_aux_locked_o => clk_aux_locked_o,
      clk_aux_lock_en_i => clk_aux_lock_en_i,
      wb_adr_i        => slave_i.adr(6 downto 0),
      wb_dat_i        => slave_i.dat,
      wb_dat_o        => slave_o.dat,
      wb_cyc_i         => slave_i.cyc,
      wb_sel_i         => slave_i.sel,
      wb_stb_i         => slave_i.stb,
      wb_we_i          => slave_i.we,
      wb_ack_o         => slave_o.ack,
      wb_stall_o       => slave_o.stall,
      wb_irq_o         => wb_irq_o,
      debug_o          => debug_o
    );

end wrapper;
