-------------------------------------------------------------------------------
-- Title      : RX Packet Buffer
-- Project    : White Rabbit MAC/Endpoint
-------------------------------------------------------------------------------
-- File       : ep_rx_buffer.vhd
-- Author     : Tomasz Włostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2010-11-18
-- Last update: 2011-10-29
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
      dout(12 downto 0) <= (others => '0');
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
      dout(17 downto 0) <= (others => '0');
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
            fab.addr <= c_WRF_OOB after 1ns;
          when c_WRF_STATUS =>
            fab.addr <= c_WRF_DATA after 1ns;
          when others => fab.addr <= c_WRF_DATA after 1ns;
        end case;

      else
        fab.addr <= cur_addr after 1ns;
      end if;

      fab.dvalid  <= not din(17) or (din(17) and not din(16));
      fab.sof     <= din(15) and din(17) and din(16);
      fab.eof     <= din(14) and din(17) and din(16);
      fab.error   <= din(13) and din(17) and din(16);
      fab.bytesel <= not din(17) and din(16);

    else
      fab.bytesel <= '0';
      fab.addr    <= cur_addr after 1ns;
      fab.dvalid  <= '0';
      fab.sof     <= '0';
      fab.eof     <= '0';
      fab.error   <= '0';
      fab.data    <= (others => '0');
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

  component chipscope_ila
    port (
      CONTROL : inout std_logic_vector(35 downto 0);
      CLK     : in    std_logic;
      TRIG0   : in    std_logic_vector(31 downto 0);
      TRIG1   : in    std_logic_vector(31 downto 0);
      TRIG2   : in    std_logic_vector(31 downto 0);
      TRIG3   : in    std_logic_vector(31 downto 0));
  end component;

  component chipscope_icon
    port (
      CONTROL0 : inout std_logic_vector (35 downto 0));
  end component;

  signal CONTROL : std_logic_vector(35 downto 0);
  signal CLK     : std_logic;
  signal TRIG0   : std_logic_vector(31 downto 0);
  signal TRIG1   : std_logic_vector(31 downto 0);
  signal TRIG2   : std_logic_vector(31 downto 0);
  signal TRIG3   : std_logic_vector(31 downto 0);

  signal crappify : unsigned(10 downto 0);
  
begin
  --chipscope_ila_1 : chipscope_ila
  --  port map (
  --    CONTROL => CONTROL,
  --    CLK     => clk_sys_i,
  --    TRIG0   => TRIG0,
  --    TRIG1   => TRIG1,
  --    TRIG2   => TRIG2,
  --    TRIG3   => TRIG3);

  --chipscope_icon_1 : chipscope_icon
  --  port map (
  --    CONTROL0 => CONTROL);

  TRIG0(15 downto 0)  <= snk_fab_i.data;
  trig0(16)           <= snk_fab_i.sof;
  trig0(17)           <= snk_fab_i.eof;
  trig0(18)           <= snk_fab_i.error;
  trig0(19)           <= snk_fab_i.bytesel;
  trig0(20)           <= snk_fab_i.has_rx_timestamp;
  trig0(21)           <= snk_fab_i.dvalid;
  trig0(24 downto 23) <= snk_fab_i.addr;

  TRIG1(15 downto 0)  <= src_fab_int.data;
  trig1(16)           <= src_fab_int.sof;
  trig1(17)           <= src_fab_int.eof;
  trig1(18)           <= src_fab_int.error;
  trig1(19)           <= src_fab_int.bytesel;
  trig1(20)           <= src_fab_int.has_rx_timestamp;
  trig1(21)           <= src_fab_int.dvalid;
  trig1(24 downto 23) <= src_fab_int.addr;


  trig2(17 downto 0) <= q_in;
  trig2(18)          <= q_in_valid;
  trig3(17 downto 0) <= q_out;
  trig3(18)          <= q_empty;
  trig3(19)          <= q_wr;
  trig3(20)          <= q_rd;
  trig3(21)          <= q_drop;
  trig3(22) <= q_in_valid;
  trig3(23) <= q_out_valid;

  trig3(25 downto 24) <= in_prev_addr;
  trig3(27 downto 26) <= out_cur_addr;




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

  p_pack_rbuf: process(fab_to_encode, in_prev_addr)
    begin
      f_pack_rbuf_contents(fab_to_encode, in_prev_addr, q_in, q_in_valid);
    end process;


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

  q_reset <= rst_n_i and regs_i.ecr_rx_en_o;

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

 
  
  q_rd <=  (not q_empty) and src_dreq_i;

  rd_valid_gen : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0') then
        q_out_valid <= '0';
        out_cur_addr <= c_WRF_STATUS;
      else
        q_out_valid <= q_rd;

        if(src_fab_int.sof = '1' or src_fab_int.eof = '1' or src_fab_int.error = '1')then
          out_cur_addr <= c_WRF_STATUS;
        else
          out_cur_addr <= src_fab_int.addr;
        end if;
      end if;
    end if;
  end process;

  p_unpack: process(q_out, out_cur_addr, q_out_valid)
  begin
    f_unpack_rbuf_contents(q_out, out_cur_addr, q_out_valid, src_fab_int);
  end process;

  src_fab_o  <= src_fab_int;
  snk_dreq_o <= '1';

  level_o <= q_usedw(q_usedw'left downto q_usedw'left - 7);

end behavioral;
