--! @file eca_gpio_channel.vhd
--! @brief ECA-GPIO Adapter
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This component takes an action channel and pumps it into a queue.
--! Naturally, this loses a ton of the determinism of the ECA.
--! However, the intent is to connect this to a soft real-time CPU.
--! When an action arrives, an interrupt is sent out i_master.
--!
--! 0x00 RW : Control
--!   0x0-1 R : queue depth
--!   0x2-3  W: 1=pop
--! 0x04 RW : interrupt enable (1=arrival, 2=error): MUST DISABLE before changing address
--! 0x08 RW : Queue arrival  interrupt address (i_master_o.adr=reg, i_master_o.dat=a_channel_i.tag)
--! 0x0C RW : Queue overflow interrupt address (i_master_o.adr=reg, i_master_o.dat=[0=dropped])
--! 0x10 R  : Queued actions
--! 0x14 RW : Dropped actions
--! 0x18 -- reserved --
--! 0x1C R  : flags (0x1=late, 0x2=conflict)
--! 0x20 R  : Event1
--! 0x24 R  : Event0
--! 0x28 R  : Param1
--! 0x2C R  : Param0
--! 0x30 R  : Tag
--! 0x34 R  : Tef
--! 0x38 R  : Time1
--! 0x3C R  : Time0
--!
--------------------------------------------------------------------------------
--! This library is free software; you can redistribute it and/or
--! modify it under the terms of the GNU Lesser General Public
--! License as published by the Free Software Foundation; either
--! version 3 of the License, or (at your option) any later version.
--!
--! This library is distributed in the hope that it will be useful,
--! but WITHOUT ANY WARRANTY; without even the implied warranty of
--! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--! Lesser General Public License for more details.
--!  
--! You should have received a copy of the GNU Lesser General Public
--! License along with this library. If not, see <http://www.gnu.org/licenses/>.
---------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wishbone_pkg.all;
use work.eca_pkg.all;

entity eca_queue_channel is
  generic(
    g_log_queue_depth  : natural := 8;
    g_counter_bits     : natural := 24;
    g_log_clock_factor : natural := 8; -- clocks a, q, i are within 256*
    a2i_dual_clock     : boolean := true;
    a2q_dual_clock     : boolean := true);
  port(
    a_clk_i     : in  std_logic;
    a_rst_n_i   : in  std_logic;
    a_channel_i : in  t_channel;
    i_clk_i     : in  std_logic;
    i_rst_n_i   : in  std_logic;
    i_master_o  : out t_wishbone_master_out;
    i_master_i  : in  t_wishbone_master_in;
    q_clk_i     : in  std_logic;
    q_rst_n_i   : in  std_logic;
    q_slave_i   : in  t_wishbone_slave_in;
    q_slave_o   : out t_wishbone_slave_out);
end eca_queue_channel;

architecture rtl of eca_queue_channel is
  -- Out of principle, tell quartus to leave my design alone.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";
  
  impure function f_update(x : std_logic_vector) return std_logic_vector is
    variable v_sel : std_logic_vector(x'range);
    variable v_dat : std_logic_vector(x'range);
  begin
    for i in x'range loop
      v_sel(i) := q_slave_i.sel(i / 8);
      v_dat(i) := q_slave_i.dat(i);
    end loop;
    return (x and not v_sel) or (v_dat and v_sel);
  end function;
  
  constant c_conflict_bit : natural := 0;
  constant c_late_bit     : natural := c_conflict_bit+1;
  subtype c_event_range is natural range c_event_bits+c_late_bit         downto c_late_bit+1;
  subtype c_param_range is natural range c_param_bits+c_event_range'left downto c_event_range'left+1;
  subtype c_tag_range   is natural range c_tag_bits  +c_param_range'left downto c_param_range'left+1;
  subtype c_tef_range   is natural range c_tef_bits  +c_tag_range'left   downto c_tag_range'left  +1;
  subtype c_time_range  is natural range c_time_bits +c_tef_range'left   downto c_tef_range'left  +1;
  constant c_data_bits : natural := c_time_range'left+1;
  
  subtype t_fifo_index is std_logic_vector(g_log_queue_depth-1 downto 0);
  subtype t_crossing   is std_logic_vector(g_log_clock_factor  downto 0);
  subtype t_counter    is std_logic_vector(g_counter_bits-1    downto 0);
  subtype t_data       is std_logic_vector(c_data_bits-1       downto 0);
  
  -- FIFO index registers; ridx <= iidx <= widx <= ridx+queue_depth-1
  signal ra_widx       : t_fifo_index;
  signal ra_widx_gray0 : t_fifo_index;
  signal ri_widx_gray1 : t_fifo_index;
  signal ri_widx_gray2 : t_fifo_index;
  signal ri_widx       : t_fifo_index; -- I: iidx <= widx
  
  signal ri_iidx       : t_fifo_index;
  signal ri_iidx_gray0 : t_fifo_index;
  signal rq_iidx_gray1 : t_fifo_index;
  signal rq_iidx_gray2 : t_fifo_index;
  signal rq_iidx       : t_fifo_index; -- Q: ridx <= iidx
  
  signal rq_ridx       : t_fifo_index;
  signal rq_ridx_gray0 : t_fifo_index;
  signal ra_ridx_gray1 : t_fifo_index;
  signal ra_ridx_gray2 : t_fifo_index;
  signal ra_ridx       : t_fifo_index; -- A: widx <= ridx-1 (mod queue_depth)
  
  -- Drop count, delivered to both i an q
  signal ra_drops       : t_crossing;
  signal ra_drops_gray0 : t_crossing;
  signal ri_drops_gray1 : t_crossing;
  signal ri_drops_gray2 : t_crossing;
  signal ri_drops_todo  : t_crossing;
  signal ri_drops_done  : t_crossing;
  signal rq_drops_gray1 : t_crossing;
  signal rq_drops_gray2 : t_crossing;
  signal rq_drops_todo  : t_crossing;
  signal rq_drops_done  : t_crossing;
  
  -- Interrupt enable bits
  signal rq_en_arrival  : std_logic;
  signal ri_en_arrival1 : std_logic;
  signal ri_en_arrival2 : std_logic;
  signal rq_en_dropped  : std_logic;
  signal ri_en_dropped1 : std_logic;
  signal ri_en_dropped2 : std_logic;
  
  -- a_clk_i registers+signals
  signal ra_full        : std_logic;
  signal sa_wen         : std_logic;
  signal sa_widx_next   : t_fifo_index;
  signal sa_data        : t_data;
  
  -- i_clk_i registers+signals
  signal ri_cyc         : std_logic;
  signal ri_stb         : std_logic;
  signal ri_adr         : t_wishbone_address;
  signal ri_dat         : t_wishbone_data;
  
  signal ri_dropped     : std_logic;
  signal ri_empty       : std_logic;
  signal si_iidx_next   : t_fifo_index;
  signal si_tag         : t_tag;
  
  -- q_clk_i wishbone registers
  signal rq_int_arrival : t_wishbone_address;
  signal rq_int_dropped : t_wishbone_address;
  signal rq_queued      : t_fifo_index;
  signal rq_dropped     : t_counter;
  signal rq_empty       : std_logic;
  signal rq_stall       : std_logic;
  signal rq_stall_n     : unsigned(g_log_clock_factor downto 0);
  
  signal sq_data        : t_data;
  signal sq_conflict    : std_logic;
  signal sq_late        : std_logic;
  signal sq_event       : t_event;
  signal sq_param       : t_param;
  signal sq_tag         : t_tag;
  signal sq_tef         : t_tef;
  signal sq_time        : t_time;
  
begin

  ------------------------------------------------------------------------------ FIFO RAMs
  
  tags : eca_sdp
    generic map(
      g_addr_bits  => g_log_queue_depth,
      g_data_bits  => c_tag_bits,
      g_dual_clock => a2i_dual_clock)
    port map(
      r_clk_i  => i_clk_i,
      r_addr_i => si_iidx_next,
      r_data_o => si_tag,
      w_clk_i  => a_clk_i,
      w_en_i   => sa_wen,
      w_addr_i => ra_widx,
      w_data_i => a_channel_i.tag);
  
  actions : eca_sdp
    generic map(
      g_addr_bits  => g_log_queue_depth,
      g_data_bits  => c_data_bits,
      g_dual_clock => a2q_dual_clock)
    port map(
      r_clk_i  => q_clk_i,
      r_addr_i => rq_ridx,
      r_data_o => sq_data,
      w_clk_i  => a_clk_i,
      w_en_i   => sa_wen,
      w_addr_i => ra_widx,
      w_data_i => sa_data);
  
  ------------------------------------------------------------------------------ a_clk_i
  
  q2a : process(a_clk_i) is
  begin
    if rising_edge(a_clk_i) then
      ra_ridx_gray1 <= rq_ridx_gray0;
      ra_ridx_gray2 <= ra_ridx_gray1;
      ra_ridx <= f_eca_gray_decode(ra_ridx_gray2, 1);
    end if;
  end process;
  
  sa_wen <= a_channel_i.valid and not ra_full;
  sa_widx_next <= f_eca_add(ra_widx, 1) when sa_wen='1' else ra_widx;
  
  sa_data(c_conflict_bit) <= a_channel_i.conflict;
  sa_data(c_late_bit)     <= a_channel_i.late;
  sa_data(c_event_range)  <= a_channel_i.event;
  sa_data(c_param_range)  <= a_channel_i.param;
  sa_data(c_tag_range)    <= a_channel_i.tag;
  sa_data(c_tef_range)    <= a_channel_i.tef;
  sa_data(c_time_range)   <= a_channel_i.time;
  
  a_fifo : process(a_clk_i, a_rst_n_i) is
  begin
    if a_rst_n_i = '0' then
      ra_full       <= '0';
      ra_widx       <= (others => '0');
      ra_widx_gray0 <= (others => '0');
    elsif rising_edge(a_clk_i) then
      ra_full       <= f_eca_active_high(sa_widx_next = f_eca_add(ra_ridx, -1));
      ra_widx       <= sa_widx_next;
      ra_widx_gray0 <= f_eca_gray_encode(ra_widx);
    end if;
  end process;
  
  a_drops : process(a_clk_i, a_rst_n_i) is
  begin
    if a_rst_n_i = '0' then
      ra_drops       <= (others => '0');
      ra_drops_gray0 <= (others => '0');
    elsif rising_edge(a_clk_i) then
      if (a_channel_i.valid and ra_full) = '1' then
        ra_drops <= f_eca_add(ra_drops, 1);
      end if;
      ra_drops_gray0 <= f_eca_gray_encode(ra_drops);
    end if;
  end process;
  
  ------------------------------------------------------------------------------ i_clk_i
  
  a2i : process(i_clk_i) is
  begin
    if rising_edge(i_clk_i) then
      ri_widx_gray1 <= ra_widx_gray0;
      ri_widx_gray2 <= ri_widx_gray1;
      ri_widx <= f_eca_gray_decode(ri_widx_gray2, 1);
      
      ri_drops_gray1 <= ra_drops_gray0;
      ri_drops_gray2 <= ri_drops_gray1;
      ri_drops_todo <= f_eca_gray_decode(ri_drops_gray2, 1);
    end if;
  end process;
  
  q2i : process(i_clk_i) is
  begin
    if rising_edge(i_clk_i) then
      ri_en_arrival1 <= rq_en_arrival;
      ri_en_arrival2 <= ri_en_arrival1;
      ri_en_dropped1 <= rq_en_dropped;
      ri_en_dropped2 <= ri_en_dropped1;
    end if;
  end process;
  
  i_master_o.cyc <= ri_cyc;
  i_master_o.stb <= ri_stb;
  i_master_o.adr <= ri_adr;
  i_master_o.dat <= ri_dat;
  i_master_o.sel <= (others => '1');
  i_master_o.we  <= '1';
  
  si_iidx_next <= 
    ri_iidx when (ri_cyc or ri_dropped or ri_empty) = '1'
    else f_eca_add(ri_iidx, 1);
  
  i_master : process(i_clk_i, i_rst_n_i) is
  begin
    if i_rst_n_i = '0' then
      ri_cyc <= '0';
      ri_stb <= '0';
      ri_adr <= (others => '0');
      ri_dat <= (others => '0');
      ri_dropped <= '0';
      ri_drops_done <= (others => '0');
      ri_empty   <= '0';
      ri_iidx       <= (others => '0');
      ri_iidx_gray0 <= (others => '0');
    elsif rising_edge(i_clk_i) then
      ri_dropped <= (ri_dropped and ri_cyc) or 
                    f_eca_active_high(ri_drops_done /= ri_drops_todo);
      ri_drops_done <= ri_drops_todo;
      
      ri_empty <= f_eca_active_high(si_iidx_next = ri_widx);
      ri_iidx  <= si_iidx_next;
      ri_iidx_gray0 <= f_eca_gray_encode(ri_iidx);
        
      if ri_cyc = '1' then
        if i_master_i.stall = '0' then
          ri_stb <= '0';
        end if;
        if (i_master_i.ack or i_master_i.err or i_master_i.rty) = '1' then
          ri_cyc <= '0';
        end if;
      else
        if ri_dropped = '1' then
          if ri_en_dropped2 = '1' then
            ri_cyc <= '1';
            ri_stb <= '1';
            ri_adr <= rq_int_dropped; -- crossing
            ri_dat <= (others => '0');
          end if;
        elsif ri_empty = '0' then
          if ri_en_arrival2 = '1' then
            ri_cyc <= '1';
            ri_stb <= '1';
            ri_adr <= rq_int_arrival; -- crossing
            ri_dat(si_tag'range) <= si_tag;
          end if;
        end if;
      end if;
    end if;
  end process;
  
  ------------------------------------------------------------------------------ q_clk_i
  
  i2q : process(q_clk_i) is
  begin
    if rising_edge(q_clk_i) then
      rq_iidx_gray1 <= ri_iidx_gray0;
      rq_iidx_gray2 <= rq_iidx_gray1;
      rq_iidx <= f_eca_gray_decode(rq_iidx_gray2, 1);
    end if;
  end process;
  
  a2q : process(q_clk_i) is
  begin
    if rising_edge(q_clk_i) then
      rq_drops_gray1 <= ra_drops_gray0;
      rq_drops_gray2 <= rq_drops_gray1;
      rq_drops_todo <= f_eca_gray_decode(rq_drops_gray2, 1);
    end if;
  end process;
  
  sq_conflict <= sq_data(c_conflict_bit);
  sq_late     <= sq_data(c_late_bit);
  sq_event    <= sq_data(c_event_range);
  sq_param    <= sq_data(c_param_range);
  sq_tag      <= sq_data(c_tag_range);
  sq_tef      <= sq_data(c_tef_range);
  sq_time     <= sq_data(c_time_range);
  
  q_slave_o.stall <= rq_stall;
  q_slave_o.err <= '0';
  q_slave_o.rty <= '0';
  q_slave_o.int <= '0';
  
  q_slave : process(q_clk_i, q_rst_n_i) is
  begin
    if q_rst_n_i = '0' then
      rq_ridx        <= (others => '0');
      rq_ridx_gray0  <= (others => '0');
      rq_en_arrival  <= '0';
      rq_en_dropped  <= '0';
      rq_int_arrival <= (others => '0');
      rq_int_dropped <= (others => '0');
      rq_drops_done  <= (others => '0');
      rq_queued      <= (others => '0');
      rq_dropped     <= (others => '0');
      rq_empty       <= '0';
      rq_stall       <= '0';
      rq_stall_n     <= (others => '0');
    elsif rising_edge(q_clk_i) then
      q_slave_o.ack <= q_slave_i.cyc and q_slave_i.stb and not rq_stall;
      q_slave_o.dat <= (others => '0');
      
      if rq_stall_n = 0 then
        rq_stall <= '0';
      else
        rq_stall_n <= rq_stall_n - 1;
      end if;
      
      rq_ridx_gray0 <= f_eca_gray_encode(rq_ridx);
      rq_queued <= std_logic_vector(unsigned(rq_iidx) - unsigned(rq_ridx));
      rq_empty <= f_eca_active_high(rq_iidx = rq_ridx);
      
      rq_dropped <= f_eca_delta(rq_dropped, rq_drops_done, rq_drops_todo);
      rq_drops_done <= rq_drops_todo;
      
      case to_integer(unsigned(q_slave_i.adr(5 downto 2))) is
        when  0 => q_slave_o.dat(31 downto 16) <= std_logic_vector(to_unsigned(2**g_log_queue_depth-1, 16));
        when  1 => q_slave_o.dat(1) <= rq_en_dropped;
                   q_slave_o.dat(0) <= rq_en_arrival;
        when  2 => q_slave_o.dat(rq_int_arrival'range) <= rq_int_arrival;
        when  3 => q_slave_o.dat(rq_int_dropped'range) <= rq_int_dropped;
        when  4 => q_slave_o.dat(rq_queued'range)   <= std_logic_vector(rq_queued);
        when  5 => q_slave_o.dat(rq_dropped'range)  <= std_logic_vector(rq_dropped);
        when  6 => null; -- reserved
        when  7 => q_slave_o.dat(0) <= sq_late;
                   q_slave_o.dat(1) <= sq_conflict;
        -- Protect clock crossing (rq_widx updates well after memory written)
        when  8 => q_slave_o.dat <= sq_event(63 downto 32);
        when  9 => q_slave_o.dat <= sq_event(31 downto  0);
        when 10 => q_slave_o.dat <= sq_param(63 downto 32);
        when 11 => q_slave_o.dat <= sq_param(31 downto  0);
        when 12 => q_slave_o.dat <= sq_tag;
        when 13 => q_slave_o.dat <= sq_tef;
        when 14 => q_slave_o.dat <= sq_time(63 downto 32);
        when 15 => q_slave_o.dat <= sq_time(31 downto  0);
        when others => null;
      end case;
      
      if (q_slave_i.cyc and q_slave_i.stb and not rq_stall and q_slave_i.we) = '1' then
        case to_integer(unsigned(q_slave_i.adr(5 downto 2))) is
          when  0 => 
            if (q_slave_i.sel(0) and q_slave_i.dat(0) and not rq_empty) = '1' then
              rq_ridx  <= f_eca_add(rq_ridx, 1);
              rq_stall <= '1';
            end if;
          when 1 =>
            if q_slave_i.sel(0) = '1' then
              rq_en_dropped <= q_slave_i.dat(1);
              rq_en_arrival <= q_slave_i.dat(0);
              -- Stall for a long time to ensure changing rq_int_* is safe while en low.
              rq_stall   <= '1';
              rq_stall_n <= (others => '1');
            end if;
          when  2 => if rq_en_arrival = '0' then rq_int_arrival <= f_update(rq_int_arrival); end if;
          when  3 => if rq_en_dropped = '0' then rq_int_dropped <= f_update(rq_int_dropped); end if;
          when  4 => null; -- queued (RO)
          when  5 => rq_dropped <= f_update(rq_dropped);
          when  6 => null; -- reserved
          when  7 => null; -- Flags  (RO)
          when  8 => null; -- Event1 (RO)
          when  9 => null; -- Event0 (RO)
          when 10 => null; -- Param1 (RO)
          when 11 => null; -- Param0 (RO)
          when 12 => null; -- Tag    (RO)
          when 13 => null; -- Tef    (RO)
          when 14 => null; -- Time1  (RO)
          when 15 => null; -- Time0  (RO)
          when others => null;
        end case;
      end if;
    end if;
  end process;
  
end rtl;
