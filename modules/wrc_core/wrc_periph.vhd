-------------------------------------------------------------------------------
-- Title      : WhiteRabbit PTP Core peripherials
-- Project    : WhiteRabbit
-------------------------------------------------------------------------------
-- File       : wrc_periph.vhd
-- Author     : Grzegorz Daniluk
-- Company    : Elproma
-- Created    : 2011-04-04
-- Last update: 2011-10-28
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description:
-- WRC_PERIPH integrates WRC_SYSCON, UART/VUART, 1-Wire Master
-- 
-------------------------------------------------------------------------------
-- Copyright (c) 2011 Grzegorz Daniluk
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2011-04-04  1.0      greg.d          Created
-- 2011-10-26  2.0      greg.d          Redesigned
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library work;
use work.wrcore_pkg.all;
use work.wishbone_pkg.all;
use work.sysc_wbgen2_pkg.all;

entity wrc_periph is
  generic(
    g_phys_uart     : boolean := true;
    g_virtual_uart  : boolean := false;
    g_owr_num_ports : natural := 1
  );
  port(
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    rst_ext_n_i : in  std_logic;
    rst_net_n_o : out std_logic;
    rst_wrc_n_o : out std_logic;

    led_red_o   : out std_logic;
    led_green_o : out std_logic;
    scl_o       : out std_logic;
    scl_i       : in  std_logic;
    sda_o       : out std_logic;
    sda_i       : in  std_logic;
    memsize_i   : in  std_logic_vector(3 downto 0);
    btn1_i      : in  std_logic;
    btn2_i      : in  std_logic;

    slave_i : in  t_wishbone_slave_in_array(0 to 2);
    slave_o : out t_wishbone_slave_out_array(0 to 2);

    uart_rxd_i : in  std_logic;
    uart_txd_o : out std_logic;

    -- 1-Wire
    owr_pwren_o : out std_logic_vector(g_owr_num_ports-1 downto 0);
    owr_en_o    : out std_logic_vector(g_owr_num_ports-1 downto 0);
    owr_i       : in  std_logic_vector(g_owr_num_ports-1 downto 0)
  );
end wrc_periph;

architecture struct of wrc_periph is

  signal sysc_regs_i : t_sysc_in_registers;
  signal sysc_regs_o : t_sysc_out_registers;

begin

  -- reset wrc
  process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_ext_n_i = '0') then
        rst_wrc_n_o <= '0';
        rst_net_n_o <= '0';
      elsif(sysc_regs_o.rstr_hrst_wr_o = '1' and sysc_regs_o.rstr_hrst_o = x"deadbeef") then
        rst_wrc_n_o <= '0';
        rst_net_n_o <= '0';
      elsif(sysc_regs_o.gpsr_net_rst_o = '1') then
        rst_wrc_n_o <= '1';
        rst_net_n_o <= '0';
      else
        rst_wrc_n_o <= '1';
        rst_net_n_o <= '1';
      end if;
    end if;
  end process;

  -- LEDs
  process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(sysc_regs_o.gpsr_led_link_o = '1') then
        led_red_o <= '1';
      elsif(sysc_regs_o.gpcr_led_link_o = '1') then
        led_red_o <= '0';
      end if;

      if(sysc_regs_o.gpsr_led_stat_o = '1') then
        led_green_o <= '1';
      elsif(sysc_regs_o.gpcr_led_stat_o = '1') then
        led_green_o <= '0';
      end if;
    end if;
  end process;

  -- buttons
  sysc_regs_i.gpsr_btn1_i <= btn1_i;
  sysc_regs_i.gpsr_btn2_i <= btn2_i;

  -- SCL/SDA
  scl_o <= '1' when (sysc_regs_o.gpsr_fmc_scl_o = '1' and sysc_regs_o.gpsr_fmc_scl_load_o = '1') else
           '0' when (sysc_regs_o.gpcr_fmc_scl_o = '0');
  sda_o <= '1' when (sysc_regs_o.gpsr_fmc_sda_o = '1' and sysc_regs_o.gpsr_fmc_sda_load_o = '1') else
           '0' when (sysc_regs_o.gpcr_fmc_sda_o = '0');
  sysc_regs_i.gpsr_fmc_scl_i <= scl_i;
  sysc_regs_i.gpsr_fmc_sda_i <= sda_i;

  -- Memsize
  sysc_regs_i.hwfr_memsize_i <= memsize_i;

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

  ----------------------------------------
  -- SYSCON
  ----------------------------------------
  SYSCON : wrc_syscon_wb
    port map(
      rst_n_i   => rst_n_i,
      wb_clk_i  => clk_sys_i,
      wb_addr_i => slave_i(0).adr(4 downto 2),  -- because address has byte granularity
      wb_data_i => slave_i(0).dat,
      wb_data_o => slave_o(0).dat,
      wb_cyc_i  => '0',                 --slave_i(0).cyc,
      wb_sel_i  => (others => '0'),     --slave_i(0).sel,
      wb_stb_i  => '0',                 --slave_i(0).stb,
      wb_we_i   => '0',                 --slave_i(0).we,
      wb_ack_o  => slave_o(0).ack,
      regs_i    => sysc_regs_i,
      regs_o    => sysc_regs_o
    );

  --------------------------------------
  -- UART
  --------------------------------------
  UART : xwb_simple_uart
    generic map(
      g_with_virtual_uart   => g_virtual_uart,
      g_with_physical_uart  => g_phys_uart,
      g_interface_mode      => CLASSIC,
      g_address_granularity => BYTE
    )
    port map(
      clk_sys_i => clk_sys_i,
      rst_n_i   => rst_n_i,

      -- Wishbone
      slave_i => slave_i(1),
      slave_o => slave_o(1),
      desc_o  => open,

      uart_rxd_i => uart_rxd_i,
      uart_txd_o => uart_txd_o
  );

  --------------------------------------
  -- 1-WIRE
  --------------------------------------
  ONEWIRE : xwb_onewire_master
    generic map(
      g_interface_mode      => CLASSIC,
      g_address_granularity => BYTE,
      g_num_ports           => g_owr_num_ports,
      g_ow_btp_normal       => "5.0",
      g_ow_btp_overdrive    => "1.0"
    )
    port map(
      clk_sys_i => clk_sys_i,
      rst_n_i   => rst_n_i,

      -- Wishbone
      slave_i => slave_i(2),
      slave_o => slave_o(2),
      desc_o  => open,

      owr_pwren_o => owr_pwren_o,
      owr_en_o    => owr_en_o,
      owr_i       => owr_i
    );

end struct;
