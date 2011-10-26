-------------------------------------------------------------------------------
-- Title      : RX Packet Buffer
-- Project    : White Rabbit MAC/Endpoint
-------------------------------------------------------------------------------
-- File       : ep_rx_buffer.vhd
-- Author     : Tomasz Włostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2010-11-18
-- Last update: 2011-10-26
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: A simple RX packet buffer, optimized for 18-bit Block RAM-based
-- FIFOs.
-------------------------------------------------------------------------------
--
-- Copyright (c) 2011 CERN / BE-CO-HT
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
-- from http://www.gnu.org/licenses/lgpl-2.1l.html
--
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;

use work.genram_pkg.all;
use work.endpoint_private_pkg.all;
use work.wr_fabric_pkg.all;
use work.ep_wbgen2_pkg.all;

entity ep_rx_buffer is
  generic (
    g_size : integer := 1024
    );
  port(
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    snk_fab_i  : in  t_ep_internal_fabric;
    snk_dreq_o : out std_logic;
    src_fab_o  : out t_ep_internal_fabric;
    src_dreq_i : in  std_logic;

    level_o : out std_logic_vector(7 downto 0);
    regs_i  : in  t_ep_out_registers;
    rmon_o  : out t_rmon_triggers
    );

end ep_rx_buffer;

architecture behavioral of ep_rx_buffer is

  constant c_drop_threshold    : integer := g_size * 7 / 8;
  constant c_release_threshold : integer := g_size * 6 / 8;

  procedure f_pack_rbuf_contents
    (
      signal fab        : in  t_ep_internal_fabric;
      signal prev_addr  : in  std_logic_vector;
      signal dout       : out std_logic_vector;
      signal dout_valid : out std_logic) is
  begin
    if(fab.sof = '1' or fab.error = '1' or fab.eof = '1') then
      -- tag = 11
      dout(17)          <= '1';
      dout(16)          <= '1';
      dout(15)          <= fab.sof;
      dout(14)          <= fab.eof;
      dout(13)          <= fab.error;
      dout(12 downto 0) <= (others => 'X');
      dout_valid        <= '1';
    elsif(fab.dvalid = '1') then

      if(prev_addr /= fab.addr) then
        dout(17 downto 16) <= "10";     -- reg-change
      else
        dout(17 downto 16) <= '0' & fab.bytesel;
      end if;

      dout(15 downto 0) <= fab.data;
      dout_valid        <= '1';
    else
      dout(17 downto 0) <= (others => 'X');
      dout_valid        <= '0';
    end if;

  end f_pack_rbuf_contents;

  procedure f_unpack_rbuf_contents
    (
      signal din       : in  std_logic_vector;
      signal cur_addr  : in  std_logic_vector;
      signal din_valid : in  std_logic;
      signal fab       : out t_ep_internal_fabric;
      early_eof        :     boolean := false) is
  begin

    fab.data <= din(15 downto 0);
    if(din_valid = '1') then

      if(din(17 downto 16) = "10") then  -- some fancy encoding is necessary here
        case cur_addr(1 downto 0) is
          when c_WRF_DATA =>
            fab.addr <= c_WRF_OOB;
          when c_WRF_STATUS =>
            fab.addr <= c_WRF_DATA;
          when others => fab.addr <= "XX";
        end case;

      else
        fab.addr <= cur_addr;
      end if;

      fab.dvalid  <= not din(17) or (din(17) and not din(16));
      fab.sof     <= din(15) and din(17) and din(16);
      fab.eof     <= din(14) and din(17) and din(16);
      fab.error   <= din(13) and din(17) and din(16);
      fab.bytesel <= not din(17) and din(16);

    else
      fab.bytesel <= 'X';
      fab.addr <= (others => 'X');
      fab.dvalid <= '0';
      fab.sof    <= '0';
      fab.eof    <= '0';
      fab.error  <= '0';
      fab.addr <= (others => 'X');
      fab.data   <= (others => 'X');
    end if;
  end f_unpack_rbuf_contents;


  signal q_in, q_out             : std_logic_vector(17 downto 0);
  signal q_usedw                 : std_logic_vector(f_log2_size(g_size)-1 downto 0);
  signal q_empty                 : std_logic;
  signal q_reset                 : std_logic;
  signal q_wr, q_rd              : std_logic;
  signal q_drop                  : std_logic;
  signal q_in_valid, q_out_valid : std_logic;


  type t_write_state is(WAIT_FRAME, DATA);
  signal state         : t_write_state;
  signal fab_to_encode : t_ep_internal_fabric;
  signal src_fab_int   : t_ep_internal_fabric;

  signal in_prev_addr : std_logic_vector(1 downto 0);
  signal out_cur_addr : std_logic_vector(1 downto 0);

begin

  p_fifo_write : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        q_wr         <= '0';
        q_drop       <= '0';
        state        <= WAIT_FRAME;
        in_prev_addr <= (others => '0');
      else

        if(snk_fab_i.dvalid = '1') then
          in_prev_addr <= snk_fab_i.addr;
        end if;

        if(unsigned(q_usedw) = c_drop_threshold) then
          q_drop <= '1';
        end if;

        if(unsigned(q_usedw) = c_release_threshold) then
          q_drop <= '0';
        end if;

        case state is
          when WAIT_FRAME =>
            in_prev_addr <= c_WRF_STATUS;
            if(snk_fab_i.sof = '1' and q_drop = '0') then
              state <= DATA;
            end if;

          when DATA =>
            if(q_drop = '1' or snk_fab_i.eof = '1' or snk_fab_i.error = '1') then
              state <= WAIT_FRAME;
            end if;
            
          when others => null;
        end case;
      end if;
    end if;
  end process;

  f_pack_rbuf_contents(fab_to_encode, in_prev_addr, q_in, q_in_valid);


  p_encode_fifo_in : process(snk_fab_i, state, q_drop)
    variable fab_pre_encode : t_ep_internal_fabric;
    
  begin
    fab_pre_encode := snk_fab_i;

    if(fab_pre_encode.sof = '1' and q_drop = '1') then
      fab_pre_encode.sof := '0';
    end if;

    if(state = DATA and q_drop = '1') then
      fab_pre_encode.dvalid := '0';
      fab_pre_encode.error  := '1';
    end if;

    fab_to_encode <= fab_pre_encode;
  end process;

  q_reset <= rst_n_i or regs_i.ecr_rx_en_o;

  BUF_FIFO : generic_sync_fifo
    generic map (
      g_data_width => 18,
      g_size       => g_size,
      g_with_count => true)
    port map (
      rst_n_i        => q_reset,
      clk_i          => clk_sys_i,
      d_i            => q_in,
      we_i           => q_in_valid,
      q_o            => q_out,
      rd_i           => q_rd,
      empty_o        => q_empty,
      full_o         => open,
      almost_empty_o => open,
      almost_full_o  => open,
      count_o        => q_usedw);

  q_rd <= (not q_empty) and src_dreq_i;
  
  rd_valid_gen : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0') then
        q_out_valid <= '0';
      else
        q_out_valid <= q_rd;
        if(src_fab_int.sof = '1')then
          out_cur_addr <= c_WRF_STATUS;
        end if;
        if(src_fab_int.dvalid = '1') then
          out_cur_addr <= src_fab_int.addr;
        end if;
      end if;
    end if;
  end process;

  f_unpack_rbuf_contents(q_out, out_cur_addr, q_out_valid, src_fab_int);

  src_fab_o  <= src_fab_int;
  snk_dreq_o <= '1';

  level_o <= q_usedw(q_usedw'left downto q_usedw'left - 7);

end behavioral;
