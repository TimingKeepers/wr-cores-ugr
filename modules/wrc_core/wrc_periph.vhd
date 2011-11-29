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
    g_phys_uart    : boolean := true;
    g_virtual_uart : boolean := false
  );
  port(
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

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
    owr_en_o : out std_logic;
    owr_i    : in  std_logic
  );
end wrc_periph;

architecture struct of wrc_periph is

  signal sysc_regs_i : t_sysc_in_registers;
  signal sysc_regs_o : t_sysc_out_registers;

  signal owr_en_slv : std_logic_vector(0 downto 0);
  signal owr_in_slv : std_logic_vector(0 downto 0);


begin

  -- reset wrc
  process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0') then
        rst_wrc_n_o <= '0';
        rst_net_n_o <= '0';
      else

        if(sysc_regs_o.rstr_trig_wr_o = '1' and sysc_regs_o.rstr_trig_o = x"deadbee") then
          rst_wrc_n_o <= not sysc_regs_o.rstr_rst_o;
        end if;

        rst_net_n_o <= not sysc_regs_o.gpsr_net_rst_o;
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



  p_drive_i2c : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        scl_o <= '1';
        sda_o <= '1';
      else
        if(sysc_regs_o.gpsr_fmc_sda_load_o = '1' and sysc_regs_o.gpsr_fmc_sda_o = '1') then
          sda_o <= '1';
        elsif(sysc_regs_o.gpcr_fmc_sda_o = '1') then
          sda_o <= '0';
        end if;

        if(sysc_regs_o.gpsr_fmc_scl_load_o = '1' and sysc_regs_o.gpsr_fmc_scl_o = '1') then
          scl_o <= '1';
        elsif(sysc_regs_o.gpcr_fmc_scl_o = '1') then
          scl_o <= '0';
        end if;
      end if;
    end if;
  end process;

  sysc_regs_i.gpsr_fmc_sda_i <= sda_i;
  sysc_regs_i.gpsr_fmc_scl_i <= scl_i;

  -- Memsize
  sysc_regs_i.hwfr_memsize_i <= memsize_i;

  ----------------------------------------
  -- SYSCON
  ----------------------------------------
  SYSCON: xwr_syscon_wb
    generic map(
      g_interface_mode      => PIPELINED,
      g_address_granularity => BYTE
    )
    port map(
      rst_n_i   => rst_n_i,
      clk_sys_i => clk_sys_i,
  
      slave_i   => slave_i(0),
      slave_o   => slave_o(0),
  
      regs_i    => sysc_regs_i,
      regs_o    => sysc_regs_o
    );

  --slave_o(0).ack <= '0';
  --slave_o(0).stall <= '0';

  --------------------------------------
  -- UART
  --------------------------------------
  UART : xwb_simple_uart
    generic map(
      g_with_virtual_uart   => g_virtual_uart,
      g_with_physical_uart  => g_phys_uart,
      g_interface_mode      => PIPELINED,
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
      g_interface_mode      => PIPELINED,
      g_address_granularity => BYTE,
      g_num_ports           => 1,
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

      owr_en_o => owr_en_slv,
      owr_i    => owr_in_slv
    );

  owr_in_slv(0) <= owr_i;
  owr_en_o      <= owr_en_slv(0);

end struct;
