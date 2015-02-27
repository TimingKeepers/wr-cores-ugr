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
--  0x00200000: Mini NIC for OBP
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

        ep_txtsu_port_id           : in std_logic_vector(4 downto 0);
        ep_txtsu_frame_id          : in std_logic_vector(15 downto 0);
        ep_txtsu_ts_value          : in std_logic_vector(31 downto 0);
        ep_txtsu_ts_incorrect      : in std_logic;
	ep_txtsu_stb : in std_logic;

	src_out : out t_wrf_source_out;
	src_in  : in t_wrf_source_in;
	snk_out : out t_wrf_sink_out;
	snk_in  : in t_wrf_sink_in;

	mnic_txtsu_ack : out std_logic;

	 wb_i  : in t_wishbone_master_in;
	 wb_o  : out t_wishbone_master_out
    );
end OBP;

architecture Behavioral of OBP is
-----------------------------------------------------------------------------
  --WB intercon
  -----------------------------------------------------------------------------
  constant c_layout : t_sdb_record_array(2 downto 0) :=
    (0 => f_sdb_embed_device(f_xwb_dpram_obp(g_dpram_size), x"00000000"),
     1 => f_sdb_embed_device(c_xwr_mini_nic_sdb, x"00200000"),
     2 => f_sdb_embed_bridge(g_bridge_sdb, x"00400000"));
  constant c_sdb_address : t_wishbone_address := x"00c00000";

  signal cbar_slave_i  : t_wishbone_slave_in_array (1 downto 0);
  signal cbar_slave_o  : t_wishbone_slave_out_array(1 downto 0);
  signal cbar_master_i : t_wishbone_master_in_array(2 downto 0);
  signal cbar_master_o : t_wishbone_master_out_array(2 downto 0);
  
  signal dpram_wbb_i_dummy : t_wishbone_slave_in;
  signal dpram_wbb_o_dummy : t_wishbone_slave_out;
  
  signal rst_n_obp : std_logic;

constant c_mnic_memsize_log2 : integer := f_log2_size(g_dpram_size);

-----------------------------------------------------------------------------
--Mini-NIC
-----------------------------------------------------------------------------
signal mnic_mem_data_o : std_logic_vector(31 downto 0);
signal mnic_mem_addr_o : std_logic_vector(c_mnic_memsize_log2-1 downto 0);
signal mnic_mem_data_i : std_logic_vector(31 downto 0);
signal mnic_mem_wr_o   : std_logic;

signal minic_wb_in  : t_wishbone_slave_in;
signal minic_wb_out : t_wishbone_slave_out;

-----------------------------------------------------------------------------
--Dual-port RAM
-----------------------------------------------------------------------------
signal dpram_wbb_i : t_wishbone_slave_in;
signal dpram_wbb_o : t_wishbone_slave_out;
  
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
	slave2_i => dpram_wbb_i,
	slave2_o => dpram_wbb_o
      );

dpram_wbb_i.cyc                                 <= '1';
dpram_wbb_i.stb                                 <= '1';
dpram_wbb_i.adr(c_mnic_memsize_log2-1 downto 0) <= mnic_mem_addr_o;
dpram_wbb_i.sel                                 <= "1111";
dpram_wbb_i.we                                  <= mnic_mem_wr_o;
dpram_wbb_i.dat                                 <= mnic_mem_data_o;
mnic_mem_data_i                                 <= dpram_wbb_o.dat;

-----------------------------------------------------------------------------
-- Mini-NIC
-----------------------------------------------------------------------------
MINI_NIC : xwr_mini_nic
 generic map (
	g_interface_mode       => PIPELINED,
	g_address_granularity  => BYTE,
	g_memsize_log2         => f_log2_size(g_dpram_size),
	g_buffer_little_endian => false)
 port map (
	clk_sys_i => clk_sys_i,
	rst_n_i   => rst_n_i,

	mem_data_o => mnic_mem_data_o,
	mem_addr_o => mnic_mem_addr_o,
	mem_data_i => mnic_mem_data_i,
	mem_wr_o   => mnic_mem_wr_o,

	src_o => src_out,
	src_i => src_in,
	snk_o => snk_out,
	snk_i => snk_in,

	txtsu_port_id_i     => ep_txtsu_port_id,
	txtsu_frame_id_i    => ep_txtsu_frame_id,
	txtsu_tsval_i       => ep_txtsu_ts_value,
	txtsu_tsincorrect_i => ep_txtsu_ts_incorrect,
	txtsu_stb_i         => ep_txtsu_stb,
	txtsu_ack_o         => mnic_txtsu_ack,

	wb_i => minic_wb_in,
	wb_o => minic_wb_out
	);
	
	minic_wb_in <= cbar_master_o(1);
	cbar_master_i(1) <= minic_wb_out;

 -----------------------------------------------------------------------------
  -- WB intercon
  -----------------------------------------------------------------------------
  WB_CON : xwb_sdb_crossbar
    generic map(
      g_num_masters => 2,
      g_num_slaves  => 3,
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
		
    cbar_master_i(2) <= wb_i;
    wb_o <= cbar_master_o(2);
		
    rst_n_obp <= (enable_obp and rst_n_i);
		
end Behavioral;
