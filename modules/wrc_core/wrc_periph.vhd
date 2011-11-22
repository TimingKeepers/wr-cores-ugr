-------------------------------------------------------------------------------
-- Title      : WhiteRabbit PTP Core peripherials
-- Project    : WhiteRabbit
-------------------------------------------------------------------------------
-- File       : wrc_periph.vhd
-- Author     : Grzegorz Daniluk
-- Company    : Elproma
-- Created    : 2011-04-04
-- Last update: 2011-11-09
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description:
-- WRC_PERIPH is a single WB slave which includes all 'small' WB peripherials
-- needed for WR PTP Core. It has: wb_gpio_port, wb_simple_uart, wb_tics and
-- wb_reset.
-- All those modules share Wishbone Slave interface, and the address bus is
-- used to choose one of them at a time.
-- 
-- wb_addr_i(11:0):
-- (11) -> select wb_reset
-- (10) -> select wb_tics
-- (9)  -> select wb_uart
-- (8)  -> select wb_gpio
-- (7:0)-> address shared between wb modules inside wrc_periph
-------------------------------------------------------------------------------
-- Copyright (c) 2011 Grzegorz Daniluk
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2011-04-04  1.0      greg.d          Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library work;
use work.wrcore_pkg.all;
use work.wishbone_pkg.all;

entity wrc_periph is
  generic(
    g_gpio_pins     : natural := 8;
    g_virtual_uart  : natural := 0;
    g_tics_period: integer
  );
  port(
    clk_sys_i : in  std_logic;
    clk_ref_i : in  std_logic;
    rst_n_i   : in  std_logic;

    gpio_o    : out std_logic_vector(g_gpio_pins-1 downto 0);
    gpio_i    : in std_logic_vector(g_gpio_pins-1 downto 0);
    gpio_dir_o    : out std_logic_vector(g_gpio_pins-1 downto 0);

    uart_rxd_i: in  std_logic;
    uart_txd_o: out std_logic;

    rst_cpu_n_o: out std_logic;
    rst_net_n_o: out std_logic;

    owr_en_o: out std_logic;
    owr_i: in std_logic;
    
    wb_addr_i : in  std_logic_vector(12 downto 0); 
    wb_data_i : in  std_logic_vector(31 downto 0); 
    wb_data_o : out std_logic_vector(31 downto 0); 
    wb_sel_i  : in  std_logic_vector(3 downto 0); 
    wb_stb_i  : in  std_logic;
    wb_cyc_i  : in  std_logic;
    wb_we_i   : in  std_logic;
    wb_ack_o  : out std_logic
  );
end wrc_periph;

architecture struct of wrc_periph is

  
  component wb_reset
    port (
      clk_i      : in  std_logic;
      rst_n_i    : in  std_logic;
      wb_addr_i  : in  std_logic_vector(1 downto 0);
      wb_data_i  : in  std_logic_vector(31 downto 0);
      wb_data_o  : out std_logic_vector(31 downto 0);
      wb_sel_i   : in  std_logic_vector(3 downto 0);
      wb_stb_i   : in  std_logic;
      wb_cyc_i   : in  std_logic;
      wb_we_i    : in  std_logic;
      wb_ack_o   : out std_logic;
      rst_cpu_n_o : out std_logic;
      rst_net_n_o: out std_logic
      );
  end component;

  component wb_onewire_master
    generic (
      g_interface_mode      : t_wishbone_interface_mode;
      g_address_granularity : t_wishbone_address_granularity;
      g_num_ports           : integer;
      g_ow_btp_normal       : string;
      g_ow_btp_overdrive    : string);
    port (
      clk_sys_i   : in  std_logic;
      rst_n_i     : in  std_logic;
      wb_cyc_i    : in  std_logic;
      wb_sel_i    : in  std_logic_vector(c_wishbone_data_width/8-1 downto 0);
      wb_stb_i    : in  std_logic;
      wb_we_i     : in  std_logic;
      wb_adr_i    : in  std_logic_vector(2 downto 0);
      wb_dat_i    : in  std_logic_vector(c_wishbone_data_width-1 downto 0);
      wb_dat_o    : out std_logic_vector(c_wishbone_data_width-1 downto 0);
      wb_ack_o    : out std_logic;
      wb_int_o    : out std_logic;
      wb_stall_o  : out std_logic;
      owr_pwren_o : out std_logic_vector(g_num_ports -1 downto 0);
      owr_en_o    : out std_logic_vector(g_num_ports -1 downto 0);
      owr_i       : in  std_logic_vector(g_num_ports -1 downto 0));
  end component;
  
  type t_wbdata is array(4 downto 0) of std_logic_vector(31 downto 0);

  signal wb_cycs_i  : std_logic_vector(4 downto 0);
  signal wb_stbs_i  : std_logic_vector(4 downto 0);
  signal wb_acks_o  : std_logic_vector(4 downto 0);
  signal wb_dats_o  : t_wbdata;
 
  signal wb_ack_int : std_logic;
begin

 



  
  GENWB: 
  for I in 0 to 4 generate
    wb_cycs_i(I) <= wb_cyc_i and wb_addr_i(8+I);
    wb_stbs_i(I) <= wb_stb_i and wb_addr_i(8+I);
  end generate;

  wb_ack_int <= wb_acks_o(0) when (wb_addr_i(12 downto 8)="00001") else
               wb_acks_o(1) when (wb_addr_i(12 downto 8)="00010") else
               wb_acks_o(2) when (wb_addr_i(12 downto 8)="00100") else
               wb_acks_o(3) when (wb_addr_i(12 downto 8)="01000") else
               wb_acks_o(4) when (wb_addr_i(12 downto 8)="10000") else
               '0';
  
  wb_data_o <= wb_dats_o(0) when (wb_addr_i(12 downto 8)="00001") else
               wb_dats_o(1) when (wb_addr_i(12 downto 8)="00010") else
               wb_dats_o(2) when (wb_addr_i(12 downto 8)="00100") else
               wb_dats_o(3) when (wb_addr_i(12 downto 8)="01000") else
               wb_dats_o(4) when (wb_addr_i(12 downto 8)="10000") else
               (others=>'0');

  GPIO: wb_gpio_port
    generic map(
      g_num_pins => g_gpio_pins,
      g_with_builtin_tristates => false)
    port map(
      rst_n_i => rst_n_i,
      clk_sys_i    => clk_sys_i,
      
      wb_sel_i    => wb_sel_i,
      wb_cyc_i    => wb_cycs_i(0),
      wb_stb_i    => wb_stb_i,
      wb_we_i     => wb_we_i,
      wb_adr_i   => wb_addr_i(7 downto 0), 
      wb_dat_i   => wb_data_i,
      wb_dat_o   => wb_dats_o(0), 
      wb_ack_o    => wb_acks_o(0),
      
      gpio_out_o    => gpio_o,
      gpio_in_i => gpio_i,
      gpio_oen_o => gpio_dir_o
    );

  UART: wb_simple_uart
    generic map (
      g_with_virtual_uart  => true,
      g_with_physical_uart => true)
    port map(
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_i,
      
      wb_adr_i  => wb_addr_i(4 downto 0),
      wb_dat_i  => wb_data_i,
      wb_dat_o  => wb_dats_o(1),
      wb_cyc_i   => wb_cycs_i(1),
      wb_sel_i   => wb_sel_i,
      wb_stb_i   => wb_stb_i,
      wb_we_i    => wb_we_i,
      wb_ack_o   => wb_acks_o(1),

      uart_rxd_i => uart_rxd_i,
      uart_txd_o => uart_txd_o
      );
 
  TICS: wb_tics
    generic map (
      g_period => g_tics_period)
    port map(
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_i,

      wb_adr_i  => wb_addr_i(3 downto 0),
      wb_dat_i  => wb_data_i,
      wb_dat_o  => wb_dats_o(2),
      wb_cyc_i   => wb_cycs_i(2),
      wb_sel_i   => wb_sel_i,
      wb_stb_i   => wb_stb_i,
      wb_we_i    => wb_we_i,
      wb_ack_o   => wb_acks_o(2)
    );

  RST_GEN: wb_reset
    port map(
      clk_i      => clk_sys_i,
      rst_n_i    => rst_n_i,
      rst_net_n_o => rst_net_n_o,
      rst_cpu_n_o => rst_cpu_n_o,
      
      wb_addr_i  => wb_addr_i(1 downto 0),
      wb_data_i  => wb_data_i,
      wb_data_o  => wb_dats_o(3),
      wb_sel_i   => wb_sel_i,
      wb_stb_i   => wb_stbs_i(3),
      wb_cyc_i   => wb_cycs_i(3),
      wb_we_i    => wb_we_i,
      wb_ack_o   => wb_acks_o(3)
    );  

  U_OW: wb_onewire_master
    generic map (
      g_interface_mode      => CLASSIC,
      g_address_granularity => WORD,
      g_num_ports           => 1,
      g_ow_btp_normal       => "5.0",
      g_ow_btp_overdrive    => "1.0")
    port map (
      clk_sys_i   => clk_sys_i,
      rst_n_i     => rst_n_i,
      wb_cyc_i    => wb_cycs_i(4),
      wb_sel_i    => "1111",
      wb_stb_i    => wb_stbs_i(4),
      wb_we_i     => wb_we_i,
      wb_adr_i    => wb_addr_i(2 downto 0),
      wb_dat_i    => wb_data_i,
      wb_dat_o    => wb_dats_o(4),
      wb_ack_o    => wb_acks_o(4),
      owr_en_o(0)    => owr_en_o,
      owr_i(0)       => owr_i);
  
wb_ack_o <= wb_ack_int;
  
end struct;
