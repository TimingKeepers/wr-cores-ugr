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
--  0x00000000: OBP RAM Memory
--  0x00400000: To-external device bridge (Main)

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

entity OBP is
generic(
    g_dpram_initf               : string                         := "obp.ram";
    g_dpram_size                : integer                        := 20480/4;
	 g_bridge_sdb  : t_sdb_bridge
    );
	 port(
    clk_sys_i : in std_logic;
    rst_n_i : in std_logic;
	 enable_obp : in std_logic;
	 wb_i  : in t_wishbone_master_in;
	 wb_o  : out t_wishbone_master_out
    );
end OBP;

architecture Behavioral of OBP is
-----------------------------------------------------------------------------
  --WB intercon
  -----------------------------------------------------------------------------
  constant c_layout : t_sdb_record_array(1 downto 0) :=
    (0 => f_sdb_embed_device(f_xwb_dpram_obp(g_dpram_size), x"00000000"),
     1 => f_sdb_embed_bridge(g_bridge_sdb, x"00400000"));
  constant c_sdb_address : t_wishbone_address := x"00c00000";

  signal cbar_slave_i  : t_wishbone_slave_in_array (1 downto 0);
  signal cbar_slave_o  : t_wishbone_slave_out_array(1 downto 0);
  signal cbar_master_i : t_wishbone_master_in_array(1 downto 0);
  signal cbar_master_o : t_wishbone_master_out_array(1 downto 0);
  
  signal dpram_wbb_i_dummy : t_wishbone_slave_in;
  signal dpram_wbb_o_dummy : t_wishbone_slave_out;
  
  signal rst_n_obp : std_logic;
  
begin

-----------------------------------------------------------------------------
  -- LM32
  -----------------------------------------------------------------------------  
  LM32_CORE : xwb_lm32
    generic map(g_profile => "medium_icache_debug")
    port map(
      clk_sys_i => clk_sys_i,
      rst_n_i   => rst_n_obp,
      irq_i     => (others => '0'),

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
      rst_n_i   => rst_n_i,

      slave1_i => cbar_master_o(0),
      slave1_o => cbar_master_i(0),
      slave2_i => dpram_wbb_i_dummy,
      slave2_o => dpram_wbb_o_dummy
      );

 -----------------------------------------------------------------------------
  -- WB intercon
  -----------------------------------------------------------------------------
  WB_CON : xwb_sdb_crossbar
    generic map(
      g_num_masters => 2,
      g_num_slaves  => 2,
      g_registered  => true,
      g_wraparound  => true,
      g_layout      => c_layout,
      g_sdb_addr    => c_sdb_address
      )  
    port map(
      clk_sys_i => clk_sys_i,
      rst_n_i   => rst_n_i,
      slave_i   => cbar_slave_i,
      slave_o   => cbar_slave_o,
      master_i  => cbar_master_i,
      master_o  => cbar_master_o
      );
		
    cbar_master_i(1) <= wb_i;
    wb_o <= cbar_master_o(1);
		
    rst_n_obp <= (enable_obp and rst_n_i);
		
end Behavioral;
