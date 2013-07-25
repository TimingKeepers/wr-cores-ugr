-------------------------------------------------------------------------------
-- Title      : Silicon Labs Si57x oscillator I2C controller
-- Project    : White Rabbit
-------------------------------------------------------------------------------
-- File       : xwr_si57x_interface.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-Co-HT
-- Created    : 2010-04-26
-- Last update: 2013-03-19
-- Platform   : FPGA-generic
-- Standard   : VHDL '93
-------------------------------------------------------------------------------
-- Description: Wrapper for wr_si57x_interface using Wishbone records in
-- entity interface. See wr_si57x_interface.vhd for description.
-------------------------------------------------------------------------------
--
-- Copyright (c) 2013 CERN
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
library ieee;

use ieee.std_logic_1164.all;
use work.wishbone_pkg.all;

entity xwr_si57x_interface is
  generic (
    g_simulation : integer := 0);
  port (
    clk_sys_i         : in  std_logic;
    rst_n_i           : in  std_logic;
    
    tm_dac_value_i    : in  std_logic_vector(23 downto 0) := x"000000";
    tm_dac_value_wr_i : in  std_logic                     := '0';
    
    scl_pad_oen_o     : out std_logic;
    sda_pad_oen_o     : out std_logic;
    scl_pad_i         : in  std_logic;
    sda_pad_i         : in  std_logic;

    slave_i : in  t_wishbone_slave_in;
    slave_o : out t_wishbone_slave_out
    );
end xwr_si57x_interface;

architecture wrapper of xwr_si57x_interface is

  component wr_si57x_interface
    generic (
      g_simulation : integer);
    port (
      clk_sys_i         : in  std_logic;
      rst_n_i           : in  std_logic;
      tm_dac_value_i    : in  std_logic_vector(23 downto 0)                           := x"000000";
      tm_dac_value_wr_i : in  std_logic                                               := '0';
      scl_pad_oen_o     : out std_logic;
      sda_pad_oen_o     : out std_logic;
      scl_pad_i         : in  std_logic;
      sda_pad_i         : in  std_logic;
      wb_adr_i          : in  std_logic_vector(c_wishbone_address_width-1 downto 0)   := (others => '0');
      wb_dat_i          : in  std_logic_vector(c_wishbone_data_width-1 downto 0)      := (others => '0');
      wb_dat_o          : out std_logic_vector(c_wishbone_data_width-1 downto 0);
      wb_sel_i          : in  std_logic_vector(c_wishbone_address_width/8-1 downto 0) := (others => '0');
      wb_we_i           : in  std_logic                                               := '0';
      wb_cyc_i          : in  std_logic                                               := '0';
      wb_stb_i          : in  std_logic                                               := '0';
      wb_ack_o          : out std_logic;
      wb_err_o          : out std_logic;
      wb_rty_o          : out std_logic;
      wb_stall_o        : out std_logic);
  end component;
  
begin  -- wrapper

  U_Wrapped_si57x : wr_si57x_interface
    generic map (
      g_simulation => g_simulation)
    port map (
      clk_sys_i         => clk_sys_i,
      rst_n_i           => rst_n_i,
      tm_dac_value_i    => tm_dac_value_i,
      tm_dac_value_wr_i => tm_dac_value_wr_i,
      scl_pad_oen_o     => scl_pad_oen_o,
      sda_pad_oen_o     => sda_pad_oen_o,
      scl_pad_i         => scl_pad_i,
      sda_pad_i         => sda_pad_i,
      wb_adr_i          => slave_i.adr,
      wb_dat_i          => slave_i.dat,
      wb_dat_o          => slave_o.dat,
      wb_sel_i          => slave_i.sel,
      wb_we_i           => slave_i.we,
      wb_cyc_i          => slave_i.cyc,
      wb_stb_i          => slave_i.stb,
      wb_ack_o          => slave_o.ack,
      wb_err_o          => slave_o.err,
      wb_stall_o        => slave_o.stall);


end wrapper;
