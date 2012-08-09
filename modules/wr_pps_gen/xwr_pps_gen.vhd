-------------------------------------------------------------------------------
-- Title      : PPS Generator & UTC Realtime clock
-- Project    : WhiteRabbit Switch
-------------------------------------------------------------------------------
-- File       : xwb_pps_gen.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-Co-HT
-- Created    : 2010-09-02
-- Last update: 2012-07-09
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright (c) 2010 Tomasz Wlostowski
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2010-09-02  1.0      twlostow        Created
-- 2011-05-09  1.1      twlostow        Added external PPS input
-- 2011-10-26  1.2      greg.d          xwb module
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.gencores_pkg.all;
use work.wishbone_pkg.all;

entity xwr_pps_gen is
  generic(
    g_interface_mode       : t_wishbone_interface_mode      := CLASSIC;
    g_address_granularity  : t_wishbone_address_granularity := WORD;
    g_ref_clock_rate       : integer                        := 125000000;
    g_ext_clock_rate       : integer                        := 10000000;
    g_with_ext_clock_input : boolean                        := false
    );
  port (
    clk_ref_i : in std_logic;
    clk_sys_i : in std_logic;
    clk_ext_i : in std_logic := '0';
    rst_n_i   : in std_logic;

    slave_i : in  t_wishbone_slave_in;
    slave_o : out t_wishbone_slave_out;

    pps_in_i : in std_logic;

    -- Single-pulse PPS output for synchronizing endpoints to
    pps_csync_o : out std_logic;
    pps_out_o   : out std_logic;
		pps_led_o		: out std_logic;

    pps_valid_o : out std_logic;

    tm_utc_o        : out std_logic_vector(39 downto 0);
    tm_cycles_o     : out std_logic_vector(27 downto 0);
    tm_time_valid_o : out std_logic

    );
end xwr_pps_gen;

architecture behavioral of xwr_pps_gen is

  component wr_pps_gen is
    generic(
      g_interface_mode       : t_wishbone_interface_mode;
      g_address_granularity  : t_wishbone_address_granularity;
      g_ref_clock_rate       : integer;
      g_ext_clock_rate       : integer := 10000000;
      g_with_ext_clock_input : boolean := false
      );
    port (
      clk_ref_i       : in  std_logic;
      clk_sys_i       : in  std_logic;
      clk_ext_i       : in  std_logic;
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
			pps_led_o				: out std_logic;
      pps_valid_o     : out std_logic;
      tm_utc_o        : out std_logic_vector(39 downto 0);
      tm_cycles_o     : out std_logic_vector(27 downto 0);
      tm_time_valid_o : out std_logic
      );
  end component;
  
begin  -- behavioral

  
  WRAPPED_PPSGEN : wr_pps_gen
    generic map(
      g_interface_mode       => g_interface_mode,
      g_address_granularity  => g_address_granularity,
      g_ref_clock_rate       => g_ref_clock_rate,
      g_ext_clock_rate       => g_ext_clock_rate,
      g_with_ext_clock_input => g_with_ext_clock_input
      )
    port map(
      clk_ref_i       => clk_ref_i,
      clk_sys_i       => clk_sys_i,
      clk_ext_i       => clk_ext_i,
      rst_n_i         => rst_n_i,
      wb_adr_i        => slave_i.adr(4 downto 0),
      wb_dat_i        => slave_i.dat,
      wb_dat_o        => slave_o.dat,
      wb_cyc_i        => slave_i.cyc,
      wb_sel_i        => slave_i.sel,
      wb_stb_i        => slave_i.stb,
      wb_we_i         => slave_i.we,
      wb_ack_o        => slave_o.ack,
      wb_stall_o      => slave_o.stall,
      pps_in_i        => pps_in_i,
      pps_csync_o     => pps_csync_o,
      pps_out_o       => pps_out_o,
			pps_led_o				=> pps_led_o,
      pps_valid_o     => pps_valid_o,
      tm_utc_o        => tm_utc_o,
      tm_cycles_o     => tm_cycles_o,
      tm_time_valid_o => tm_time_valid_o
      );


  slave_o.err <= '0';
  slave_o.rty <= '0';
  slave_o.int <= '0';
  
end behavioral;
