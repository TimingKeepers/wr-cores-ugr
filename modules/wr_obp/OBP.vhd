-- Title      : On Board Programer (OBP, WB-debuger simplification)
-- Project    : OBP
-------------------------------------------------------------------------------
-- File       : obp.vhd
-- Author     : Jose Jimenez Montañez, Miguel Jimenez Lopez
-- Company    : University of Granada (UGR)
-- Created    : 2014-06-12
-- Last update: 2014-06-12
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description:
-- OBP is a HDL module implementing a On Board Programer component that allows 
-- to program the LM32 inside the WRPC via USB port. In addition, some debug
-- functions have been added (to read/write WB registers, show the SDB structure, etc).
-------------------------------------------------------------------------------
--
-- Copyright (c) 2014, University of Granada (UGR)
--
-- This source file is free software; you can redistribute it   
-- and/or modify it under the terms of the GNU Lesser General   
-- Public License as published by the Free Software Foundation; 
-- either version 2.1 of the License, or (at your option) any   
-- later version.                                               
--
-- This source is distributed in the hope that it will be       
-- useful, but WITHOUT ANY WARRANTY; without even the implied   
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR      
-- PURPOSE.  See the GNU Lesser General Public License for more 
-- details.                                                     
--
-- You should have received a copy of the GNU Lesser General    
-- Public License along with this source; if not, download it   
-- from http://www.gnu.org/licenses/lgpl-2.1.html
--
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          	 Description
-- 2014-06-12  1.0      JJimenez,klyone          Created and first version
-------------------------------------------------------------------------------

-- Memory map:

-- Master interconnect:
--  0x00000000: OBP RAM Memory (Firmware)
--  0x00400000  Sec Crossbar
--  0x00A00000: To-external device bridge (Main)

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.wrcore_pkg.all;
use work.genram_pkg.all;
use work.wishbone_pkg.all;
use work.endpoint_pkg.all;
use work.wr_fabric_pkg.all;
use work.sysc_wbgen2_pkg.all;
use work.softpll_pkg.all;
use work.obp_pkg.all;
use work.obp_wbgen2_pkg.all;

entity OBP is
generic(
    g_dpram_initf               : string                         := "obp.ram";
    g_dpram_size                : integer;
    g_cram_size                : integer;
    g_bridge_sdb  : t_sdb_bridge
    );
port(
    clk_sys_i : in std_logic;
    rst_n_i : in std_logic;
    enable_obp : in std_logic;

    wbs_i  : in t_wishbone_slave_in;
    wbs_o  : out t_wishbone_slave_out;

    wbm_i  : in t_wishbone_master_in;
    wbm_o  : out t_wishbone_master_out
    );
end OBP;

architecture Behavioral of OBP is

  component obp_wb_slave
  port (
    rst_n_i                                  : in     std_logic;
    clk_sys_i                                : in     std_logic;
    wb_adr_i                                 : in     std_logic_vector(0 downto 0);
    wb_dat_i                                 : in     std_logic_vector(31 downto 0);
    wb_dat_o                                 : out    std_logic_vector(31 downto 0);
    wb_cyc_i                                 : in     std_logic;
    wb_sel_i                                 : in     std_logic_vector(3 downto 0);
    wb_stb_i                                 : in     std_logic;
    wb_we_i                                  : in     std_logic;
    wb_ack_o                                 : out    std_logic;
    wb_stall_o                               : out    std_logic;
    regs_i                                   : in     t_obp_in_registers;
    regs_o                                   : out    t_obp_out_registers
  );
  end component;

 constant c_secobp_layout : t_sdb_record_array(1 downto 0) := f_secobp_layout(g_cram_size);

 constant c_secobp_bridge_sdb  : t_sdb_bridge       :=
 f_xwb_bridge_layout_sdb(true, c_secobp_layout, c_secobp_sdb_address);

  constant c_mobp_layout : t_sdb_record_array(2 downto 0) :=
    (0 => f_sdb_embed_device(f_xwb_dpram_obp(g_dpram_size), x"00000000"),
     1 => f_sdb_embed_bridge(c_secobp_bridge_sdb, x"00200000"),
     2 => f_sdb_embed_bridge(g_bridge_sdb, x"00400000"));
  constant c_mobp_sdb_address : t_wishbone_address := x"01000000";

  signal cbar2_slave_i  : t_wishbone_slave_in_array (1 downto 0);
  signal cbar2_slave_o  : t_wishbone_slave_out_array(1 downto 0);
  signal cbar2_master_i : t_wishbone_master_in_array(1 downto 0);
  signal cbar2_master_o : t_wishbone_master_out_array(1 downto 0);

  signal cbar_slave_i  : t_wishbone_slave_in_array (1 downto 0);
  signal cbar_slave_o  : t_wishbone_slave_out_array(1 downto 0);
  signal cbar_master_i : t_wishbone_master_in_array(2 downto 0);
  signal cbar_master_o : t_wishbone_master_out_array(2 downto 0);

  signal dpram_wbb_i_dummy : t_wishbone_slave_in;
  signal dpram_wbb_o_dummy : t_wishbone_slave_out;
  
  signal rst_n_obp : std_logic;

  signal cnt_words : unsigned(31 downto 0);
  signal recv_ok : std_logic;

  signal obp_regs_in : t_obp_in_registers := c_obp_in_registers_init_value;
  signal obp_regs_out : t_obp_out_registers := c_obp_out_registers_init_value;

  signal lm32_irqs : std_logic_vector(31 downto 0);
  
begin

-----------------------------------------------------------------------------
  -- LM32
  -----------------------------------------------------------------------------  
  LM32_CORE : xwb_lm32
    generic map(g_profile => "medium_icache_debug")
    port map(
      clk_sys_i => clk_sys_i,
      rst_n_i   => rst_n_obp,
      irq_i     => lm32_irqs,

      dwb_o => cbar_slave_i(0),
      dwb_i => cbar_slave_o(0),
      iwb_o => cbar_slave_i(1),
      iwb_i => cbar_slave_o(1)
      );

  -----------------------------------------------------------------------------
  -- Dual-port RAM
  -----------------------------------------------------------------------------  
  DPRAM : xwb_dpram
    generic map(
      g_size                  => g_dpram_size,
      g_init_file             => g_dpram_initf,
      g_must_have_init_file   => true,
      g_slave1_interface_mode => PIPELINED,
      g_slave2_interface_mode => PIPELINED,
      g_slave1_granularity    => BYTE,
      g_slave2_granularity    => WORD)  
    port map(
      clk_sys_i => clk_sys_i,
      rst_n_i   => rst_n_obp,

      slave1_i => cbar_master_o(0),
      slave1_o => cbar_master_i(0),
      slave2_i => dpram_wbb_i_dummy,
      slave2_o => dpram_wbb_o_dummy
      );

  -----------------------------------------------------------------------------
  -- Dual-port Configuration RAM
  -----------------------------------------------------------------------------  
  CRAM : xwb_dpram
    generic map(
      g_size                  => g_cram_size,
      --g_init_file             => g_dpram_initf,
      --g_must_have_init_file   => true,
      g_slave1_interface_mode => PIPELINED,
      g_slave2_interface_mode => PIPELINED,
      g_slave1_granularity    => BYTE,
      g_slave2_granularity    => WORD)  
    port map(
      clk_sys_i => clk_sys_i,
      rst_n_i   => rst_n_obp,

      slave1_i => cbar2_master_o(0),
      slave1_o => cbar2_master_i(0),
      slave2_i => dpram_wbb_i_dummy,
      slave2_o => dpram_wbb_o_dummy
      );

    -- Recv counter
    cnt_recv_words: process (clk_sys_i)
    begin
        if (rising_edge(clk_sys_i)) then
		if (rst_n_obp = '0') then
		    cnt_words <= to_unsigned(0,32);
		elsif (enable_obp = '1') then
		    if (cbar2_master_o(0).cyc = '1' and cbar2_master_o(0).stb = '1') then
		        cnt_words <= cnt_words+1;
                    end if;
		end if;
	end if;
    end process; 

    -- This bit must be read by LM32 in order to decide if the program operation must be performed
    recv_ok <= '1' WHEN cnt_words = 2*obp_regs_out.n_prog_w_n_prog_w_o ELSE '0'; 

    OBP_WB_REGS: obp_wb_slave
        port map (
            rst_n_i => rst_n_obp,
            clk_sys_i => clk_sys_i,
            wb_adr_i  => cbar2_master_o(1).adr(0 downto 0),
            wb_dat_i  => cbar2_master_o(1).dat,
            wb_dat_o  => cbar2_master_i(1).dat,
            wb_cyc_i  => cbar2_master_o(1).cyc,
            wb_sel_i  => cbar2_master_o(1).sel,
            wb_stb_i  => cbar2_master_o(1).stb,
            wb_we_i   => cbar2_master_o(1).we,
            wb_ack_o  => cbar2_master_i(1).ack,
            wb_stall_o => cbar2_master_i(1).stall,
    	    regs_i => obp_regs_in,
            regs_o => obp_regs_out
        );

 -----------------------------------------------------------------------------
  -- WB intercon
  -----------------------------------------------------------------------------
  WB_CON : xwb_sdb_crossbar
    generic map(
      g_num_masters => 2,
      g_num_slaves  => 3,
      g_registered  => true,
      g_wraparound  => true,
      g_layout      => c_mobp_layout,
      g_sdb_addr    => c_mobp_sdb_address
      )  
    port map(
      clk_sys_i => clk_sys_i,
      rst_n_i   => rst_n_obp,
      slave_i   => cbar_slave_i,
      slave_o   => cbar_slave_o,
      master_i  => cbar_master_i,
      master_o  => cbar_master_o
      );
		
    cbar_master_i(2) <= wbm_i;
    wbm_o <= cbar_master_o(2);

-----------------------------------------------------------------------------
  -- WB intercon (Secondary)
  -----------------------------------------------------------------------------
  WB_CON_SEC : xwb_sdb_crossbar
    generic map(
      g_num_masters => 2,
      g_num_slaves  => 2,
      g_registered  => true,
      g_wraparound  => true,
      g_layout      => c_secobp_layout,
      g_sdb_addr    => c_secobp_sdb_address
      )  
    port map(
      clk_sys_i => clk_sys_i,
      rst_n_i   => rst_n_obp,
      slave_i   => cbar2_slave_i,
      slave_o   => cbar2_slave_o,
      master_i  => cbar2_master_i,
      master_o  => cbar2_master_o
      );

    cbar2_slave_i(0) <= cbar_master_o(1);
    cbar2_slave_o(0) <= cbar_master_i(1);

    wbs_o <= cbar2_slave_o(1);
    cbar2_slave_i(1) <= wbs_i;

    lm32_irqs(31 downto 1) <= (others => '0');
    lm32_irqs(0) <= obp_regs_out.cflags_stp_o;
		
    rst_n_obp <= (enable_obp and rst_n_i);
    
		
end Behavioral;
