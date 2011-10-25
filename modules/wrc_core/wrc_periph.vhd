-------------------------------------------------------------------------------
-- Title      : WhiteRabbit PTP Core peripherials
-- Project    : WhiteRabbit
-------------------------------------------------------------------------------
-- File       : wrc_periph.vhd
-- Author     : Grzegorz Daniluk
-- Company    : Elproma
-- Created    : 2011-04-04
-- Last update: 2011-07-18
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
    genrst_n_o: out std_logic;

    wb_addr_i : in  std_logic_vector(11 downto 0); 
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

  
  component wb_tics
    generic (
      g_period : integer);
    port (
      rst_n_i   : in  std_logic;
      clk_sys_i : in  std_logic;
      wb_addr_i : in  std_logic_vector(1 downto 0);
      wb_data_i : in  std_logic_vector(31 downto 0);
      wb_data_o : out std_logic_vector(31 downto 0);
      wb_cyc_i  : in  std_logic;
      wb_sel_i  : in  std_logic_vector(3 downto 0);
      wb_stb_i  : in  std_logic;
      wb_we_i   : in  std_logic;
      wb_ack_o  : out std_logic);
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

  
  component wb_simple_uart
    port (
      clk_sys_i  : in  std_logic;
      rst_n_i    : in  std_logic;
      wb_addr_i  : in  std_logic_vector(1 downto 0);
      wb_data_i  : in  std_logic_vector(31 downto 0);
      wb_data_o  : out std_logic_vector(31 downto 0);
      wb_cyc_i   : in  std_logic;
      wb_sel_i   : in  std_logic_vector(3 downto 0);
      wb_stb_i   : in  std_logic;
      wb_we_i    : in  std_logic;
      wb_ack_o   : out std_logic;
      uart_rxd_i : in  std_logic;
      uart_txd_o : out std_logic);
  end component;

  component wb_virtual_uart
    port(
      rst_n_i     : in  std_logic;
      clk_sys_i    : in  std_logic;
      wb_addr_i   : in  std_logic_vector(2 downto 0);
      wb_data_i   : in  std_logic_vector(31 downto 0);
      wb_data_o   : out std_logic_vector(31 downto 0);
      wb_cyc_i    : in  std_logic;
      wb_sel_i    : in  std_logic_vector(3 downto 0);
      wb_stb_i    : in  std_logic;
      wb_we_i     : in  std_logic;
      wb_ack_o    : out std_logic
    );
  end component;

  component wb_gpio_port
    generic (
      g_num_pins               : natural;
      g_with_builtin_tristates : boolean);
    port (
      clk_sys_i  : in    std_logic;
      rst_n_i    : in    std_logic;
      wb_sel_i   : in    std_logic;
      wb_cyc_i   : in    std_logic;
      wb_stb_i   : in    std_logic;
      wb_we_i    : in    std_logic;
      wb_adr_i   : in    std_logic_vector(5 downto 0);
      wb_dat_i   : in    std_logic_vector(31 downto 0);
      wb_dat_o   : out   std_logic_vector(31 downto 0);
      wb_ack_o   : out   std_logic;
      gpio_out_o : out   std_logic_vector(g_num_pins-1 downto 0);
      gpio_in_i  : in    std_logic_vector(g_num_pins-1 downto 0);
      gpio_oen_o : out   std_logic_vector(g_num_pins-1 downto 0));
  end component;

  type t_wbdata is array(3 downto 0) of std_logic_vector(31 downto 0);

  signal wb_cycs_i  : std_logic_vector(3 downto 0);
  signal wb_stbs_i  : std_logic_vector(3 downto 0);
  signal wb_acks_o  : std_logic_vector(3 downto 0);
  signal wb_dats_o  : t_wbdata;
 

  signal wb_ack_int : std_logic;
  
begin

 


 --TRIG3(11 downto 0)  <= wb_addr_i(11 downto 0);
 -- TRIG3(21 downto 12) <= (others => '0');
 -- TRIG3(31 downto 28) <= wb_sel_i;
 -- TRIG3(22)           <= wb_cyc_i;
 -- TRIG3(23)           <= wb_stb_i;
 -- TRIG3(24)           <= wb_we_i;
 -- TRIG3(25)           <= wb_ack_int;
 -- TRIG0               <= wb_data_i;
 -- TRIG2(3 downto 0) <= wb_cycs_i;
 -- TRIG2(7 downto 4) <= wb_acks_o;
  
  wb_ack_o <= wb_ack_int;
  
  GENWB: 
  for I in 0 to 3 generate
    wb_cycs_i(I) <= wb_cyc_i and wb_addr_i(8+I);
    wb_stbs_i(I) <= wb_stb_i and wb_addr_i(8+I);
  end generate;

  wb_ack_int  <= wb_acks_o(0) when (wb_addr_i(11 downto 8)="0001") else
               wb_acks_o(1) when (wb_addr_i(11 downto 8)="0010") else
               wb_acks_o(2) when (wb_addr_i(11 downto 8)="0100") else
               wb_acks_o(3) when (wb_addr_i(11 downto 8)="1000") else
               '0';

  wb_data_o <= wb_dats_o(0) when (wb_addr_i(11 downto 8)="0001") else
               wb_dats_o(1) when (wb_addr_i(11 downto 8)="0010") else
               wb_dats_o(2) when (wb_addr_i(11 downto 8)="0100") else
               wb_dats_o(3) when (wb_addr_i(11 downto 8)="1000") else
               (others=>'0');

  GPIO: wb_gpio_port
    generic map(
      g_num_pins => g_gpio_pins,
      g_with_builtin_tristates => false)
    port map(
      rst_n_i => rst_n_i,
      clk_sys_i    => clk_sys_i,
      
      wb_sel_i    => wb_sel_i(0),
      wb_cyc_i    => wb_cycs_i(0),
      wb_stb_i    => wb_stb_i,
      wb_we_i     => wb_we_i,
      wb_adr_i   => wb_addr_i(5 downto 0), 
      wb_dat_i   => wb_data_i,
      wb_dat_o   => wb_dats_o(0), 
      wb_ack_o    => wb_acks_o(0),
      
      gpio_out_o    => gpio_o,
      gpio_in_i => gpio_i,
      gpio_oen_o => gpio_dir_o
    );

  GEN_UART: if(g_virtual_uart = 0) generate
    UART: wb_simple_uart
      port map(
        clk_sys_i  => clk_sys_i,
        rst_n_i    => rst_n_i,

        wb_addr_i  => wb_addr_i(1 downto 0),
        wb_data_i  => wb_data_i,
        wb_data_o  => wb_dats_o(1),
        wb_cyc_i   => wb_cycs_i(1),
        wb_sel_i   => wb_sel_i,
        wb_stb_i   => wb_stb_i,
        wb_we_i    => wb_we_i,
        wb_ack_o   => wb_acks_o(1),

        uart_rxd_i => uart_rxd_i,
        uart_txd_o => uart_txd_o
      );
  end generate;

  GEN_VIRTUART: if(g_virtual_uart = 1) generate
    VIRTUAL_UART: wb_virtual_uart
      port map(
        rst_n_i    => rst_n_i,
        clk_sys_i  => clk_sys_i,
        wb_addr_i  => wb_addr_i(2 downto 0),
        wb_data_i  => wb_data_i,
        wb_data_o  => wb_dats_o(1),
        wb_cyc_i   => wb_cycs_i(1),
        wb_sel_i   => wb_sel_i,
        wb_stb_i   => wb_stb_i,  
        wb_we_i    => wb_we_i,       
        wb_ack_o   => wb_acks_o(1)  
      );
  end generate;


  TICS: wb_tics
    generic map (
      g_period => g_tics_period)
    port map(
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_i,

      wb_addr_i  => wb_addr_i(1 downto 0),
      wb_data_i  => wb_data_i,
      wb_data_o  => wb_dats_o(2),
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
      genrst_n_o => genrst_n_o,
    
      wb_addr_i  => wb_addr_i(1 downto 0),
      wb_data_i  => wb_data_i,
      wb_data_o  => wb_dats_o(3),
      wb_sel_i   => wb_sel_i,
      wb_stb_i   => wb_stbs_i(3),
      wb_cyc_i   => wb_cycs_i(3),
      wb_we_i    => wb_we_i,
      wb_ack_o   => wb_acks_o(3)
    );  

end struct;
