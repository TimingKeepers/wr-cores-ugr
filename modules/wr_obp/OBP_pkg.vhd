-- Title      : On Board Programer Package (OBP, WB-debuger simplification)
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

library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.genram_pkg.all;
use work.wishbone_pkg.all;

package obp_pkg is

function f_xwb_dpram_obp(g_size : natural) return t_sdb_device;
function f_xwb_cram_obp(g_size : natural) return t_sdb_device;
function f_secobp_layout(g_size : natural) return t_sdb_record_array;

component OBP is
generic(
    g_dpram_initf               : string                         := "obp.ram";
    g_dpram_size                : integer                        := 20480/4;
    g_cram_size                : integer                        := 20480/4;
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
end component;	

constant c_obp_wb_sdb : t_sdb_device := (
    abi_class     => x"0000", -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"7", -- 8/16/32-bit port granularity
    sdb_component => (
    addr_first    => x"0000000000000000",
    addr_last     => x"000000000000000f",  -- I think this is overestimated
    product => (
    vendor_id     => x"000000000000CE42", -- CERN
    device_id     => x"00000099",
    version       => x"00000001",
    date          => x"20000101",         -- UNKNOWN
    name          => "OBP-WBS            ")));

constant c_secobp_sdb_address : t_wishbone_address := x"00030000";

end obp_pkg;

package body obp_pkg is
function f_xwb_dpram_obp(g_size : natural) return t_sdb_device
  is
    variable result : t_sdb_device;
  begin
    result.abi_class     := x"0001";    -- RAM device
    result.abi_ver_major := x"01";
    result.abi_ver_minor := x"00";
    result.wbd_width     := x"7";       -- 32/16/8-bit supported
    result.wbd_endian    := c_sdb_endian_big;

    result.sdb_component.addr_first := (others => '0');
    result.sdb_component.addr_last  := std_logic_vector(to_unsigned(g_size*4-1, 64));

    result.sdb_component.product.vendor_id := x"000000000000CE42";  -- CERN
    result.sdb_component.product.device_id := x"66554433";
    result.sdb_component.product.version   := x"00000001";
    result.sdb_component.product.date      := x"20120305";
    result.sdb_component.product.name      := "WB4-BlockRAM OBP   ";

    return result;
  end f_xwb_dpram_obp;

function f_xwb_cram_obp(g_size : natural) return t_sdb_device
  is
    variable result : t_sdb_device;
  begin
    result.abi_class     := x"0001";    -- RAM device
    result.abi_ver_major := x"01";
    result.abi_ver_minor := x"00";
    result.wbd_width     := x"7";       -- 32/16/8-bit supported
    result.wbd_endian    := c_sdb_endian_big;

    result.sdb_component.addr_first := (others => '0');
    result.sdb_component.addr_last  := std_logic_vector(to_unsigned(g_size*4-1, 64));

    result.sdb_component.product.vendor_id := x"000000000000CE42";  -- CERN
    result.sdb_component.product.device_id := x"66554432";
    result.sdb_component.product.version   := x"00000001";
    result.sdb_component.product.date      := x"20120305";
    result.sdb_component.product.name      := "WB4-BlockRAM OBP C ";

    return result;
  end f_xwb_cram_obp;

function f_secobp_layout(g_size : natural) return t_sdb_record_array
is
   variable result : t_sdb_record_array(1 downto 0);
begin
   result(0) := f_sdb_embed_device(f_xwb_cram_obp(g_size), x"00000000");
   result(1) := f_sdb_embed_device(c_obp_wb_sdb, x"00020000");

   return result;

end f_secobp_layout;

end obp_pkg;
