-------------------------------------------------------------------------------
-- Title      : Dual-port RAM for WR core
-- Project    : WhiteRabbit
-------------------------------------------------------------------------------
-- File       : wrc_dpram.vhd
-- Author     : Grzegorz Daniluk
-- Company    : Elproma
-- Created    : 2011-02-15
-- Last update: 2011-07-18
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description:
-- Dual port RAM from genrams with wishbone interface
--
-------------------------------------------------------------------------------
-- Copyright (c) 2011 Grzegorz Daniluk
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2011-02-15  1.0      greg.d          Created
-- 2011-06-09  1.01     twlostow        Removed unnecessary generics
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library work;
use work.genram_pkg.all;

entity wrc_dpram is
  generic(
    g_size                     : natural := 16384;  -- 16 * 32bit = 64kB
    g_init_file                : string  := ""
    );
  port(
    clk_i   : in std_logic;
    rst_n_i : in std_logic;

    --PORT A (Wishbone)
    wb_addr_i  : in    std_logic_vector(f_log2_size(g_size)-1 downto 0);
    wb_data_i  : in    std_logic_vector(31 downto 0);
    wb_data_o  : out   std_logic_vector(31 downto 0);
    wb_sel_i   : in    std_logic_vector(3 downto 0);
    wb_cyc_i   : in    std_logic;
    wb_stb_i   : in    std_logic;
    wb_we_i    : in    std_logic;
    wb_ack_o   : out   std_logic;
    --PORT B (miniNIC)
    mem_addr_i : in    std_logic_vector(f_log2_size(g_size)-1 downto 0);
    mem_data_i : in    std_logic_vector(31 downto 0);
    mem_data_o : out   std_logic_vector(31 downto 0);
    mem_wr_i   : in    std_logic
    );
end wrc_dpram;

architecture struct of wrc_dpram is

  signal s_wb_ack_o : std_logic;
  signal muxed_we   : std_logic;
  signal s_bwea : std_logic_vector(3 downto 0);
  signal s_bweb : std_logic_vector(3 downto 0);
begin
  wb_ack_o            <= s_wb_ack_o;

  DPRAM : generic_dpram
    generic map(
      -- standard parameters
      g_data_width               => 32,
      g_size                     => g_size,
      g_with_byte_enable         => true,
      g_addr_conflict_resolution => "dont_care",
      g_init_file                => g_init_file,
      g_dual_clock               => false
      )
    port map(
      rst_n_i => rst_n_i,
      -- Port A
      clka_i  => clk_i,
      bwea_i  => s_bwea,
      wea_i   => muxed_we,  --wb_we_i,
      aa_i    => wb_addr_i,
      da_i    => wb_data_i,
      qa_o    => wb_data_o,
      -- Port B
      clkb_i  => clk_i,
      bweb_i  => s_bweb,
      web_i   => mem_wr_i,
      ab_i    => mem_addr_i,
      db_i    => mem_data_i,
      qb_o    => mem_data_o
      );

  s_bwea <= wb_sel_i when (wb_we_i = '1' and wb_stb_i = '1' and wb_cyc_i = '1') else "0000";
  s_bweb <= (others => mem_wr_i);
            
  
  muxed_we <= wb_we_i when (wb_stb_i='1' and wb_cyc_i='1') else '0';

  process(clk_i)
  begin
    if(rising_edge(clk_i)) then
      if(rst_n_i = '0') then
        s_wb_ack_o <= '0';
      else
        if(s_wb_ack_o = '1') then
          s_wb_ack_o <= '0';
        else
          s_wb_ack_o <= wb_cyc_i and wb_stb_i;
        end if;
      end if;
    end if;
  end process;
  
end struct;

