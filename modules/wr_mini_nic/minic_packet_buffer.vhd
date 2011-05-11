-------------------------------------------------------------------------------
-- Title      : Mini Embedded DMA Network Interface Controller
-- Project    : WhiteRabbit Core
-------------------------------------------------------------------------------
-- File       : minic_packet_buffer.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-Co-HT
-- Created    : 2010-07-26
-- Last update: 2011-05-09
-- Platform   : FPGA-generic
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description: RAM-based packet buffer for miNIC implementations which don't
-- use the DMA access to the system memory
-------------------------------------------------------------------------------
-- Copyright (c) 2010 Tomasz Wlostowski
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2010-07-26  1.0      twlostow        Created
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;

use work.genram_pkg.all;

entity minic_packet_buffer is
  generic (
    g_memsize_log2 : integer := 14);
  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    minic_addr_i : in std_logic_vector(g_memsize_log2-1 downto 0);
    minic_data_i : in  std_logic_vector(31 downto 0);
    minic_wr_i   : in  std_logic;
    minic_data_o : out std_logic_vector(31 downto 0);

    wb_data_i : in  std_logic_vector(31 downto 0);
    wb_data_o : out std_logic_vector(31 downto 0);
    wb_addr_i : in  std_logic_vector(g_memsize_log2-1 downto 0);
    wb_cyc_i  : in  std_logic;
    wb_stb_i  : in  std_logic;
    wb_we_i   : in  std_logic;
    wb_ack_o  : out std_logic
    );

end minic_packet_buffer;

architecture syn of minic_packet_buffer is

  signal host_we  : std_logic;
  signal host_ack : std_logic;

begin  -- syn

  ack_gen : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        host_ack <= '0';
      else
        host_ack <= (wb_cyc_i and wb_stb_i) and (not host_ack);
      end if;
    end if;
  end process;

  wb_ack_o <= host_ack;

  host_we <= wb_cyc_i and wb_stb_i and (wb_we_i and not host_ack);

  U_RAM: generic_dpram
    generic map (
      g_data_width               => 32,
      g_size                     => 2**g_memsize_log2,
      g_with_byte_enable         => false,
      g_dual_clock               => false)
    port map (
      rst_n_i => rst_n_i,
      clka_i  => clk_sys_i,
      bwea_i  => "0000",
      wea_i   => host_we,
      aa_i    => wb_addr_i,
      da_i    => wb_data_i,
      qa_o    => wb_data_o,
      clkb_i  => clk_sys_i,
      bweb_i  => "0000",
      web_i   => minic_wr_i,
      ab_i    => minic_addr_i,
      db_i    => minic_data_i,
      qb_o    => minic_data_o);

end syn;
