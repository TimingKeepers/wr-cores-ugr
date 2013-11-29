--! @file eca_channel.vhd
--! @brief ECA Action Channel
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This component receives actions to be executed, in any order.
--! It outputs actions when their deadline is due, essentially sorting them.
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

-- The channel receives actions to be output at correct times.
--
-- It is comprised of four memory components:
--  Table - An unsorted array of actions which are queued
--  Queue - A map from (low) timestamp bits to buffer index
--  Free  - A stack of unused buffer indexes
-- 
-- There are processes to manage the memory components:
--  Dispatcher - Fetches index from the Queue based on the current time.
--               Retrieves the matching entry from the Table.
--               If the entry is valid:
--                 output the action
--                 tell the manager to free the buffer entry
--  Scanner    - Scan the Table to see if the deadline is near.
--               Write valid actions into the Queue.
--  Manager    - Accepts new actions to the buffer
--               Clears/frees entries in the Table
--
-- The table has to provide multiple ports to different processes.
-- In principle it has these fields: (time, tag, param, event)
-- The time field must be read both by Dispatch and Scan and is duplicated.
-- There are c_scanners* parallel Scan processes, which partition the table vertically.
-- A timestamp = "fff..." indicates that the entry is invalid/free.
--
-- /=================================\
-- | time | event param tag tef time |
-- | time | event param tag tef time |
-- |------|                          |
-- | time | event param tag tef time |
-- | time | event param tag tef time |
-- \=================================/
--                     ^--- Data component of table (td)
--    ^---- scan components (ts0, ts1); used by scanner0 and scanner1

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wishbone_pkg.all;
use work.eca_pkg.all;

entity eca_channel is
  generic(
    g_log_table_size  : natural := 8;
    g_log_latency     : natural := 8;  -- Must be <= g_log_table_size
    g_log_queue_depth : natural := 9); -- Must be >  g_log_latency
  port(
    clk_i     : in  std_logic;
    rst_n_i   : in  std_logic;
    freeze_i  : in  std_logic; -- stop action outflow and use addr_i=>inspect_o
    drain_i   : in  std_logic; -- stop action in+outflow and erase tables
    addr_i    : in  std_logic_vector(g_log_table_size-1 downto 0);
    fill_o    : out std_logic_vector(g_log_table_size   downto 0);
    full_o    : out std_logic;
    -- Timestamps used for pipeline stages
    time_i    : in  t_time;
    time_Q_i  : in  t_time; -- time_i + 2**g_log_queue_depth
    -- Push a record to the queue
    channel_i : in  t_channel;
    channel_o : out t_channel;
    inspect_o : out t_channel);
end eca_channel;

architecture rtl of eca_channel is
  -- Out of principle, tell quartus to leave my design alone.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";
  
  constant c_table_index_bits    : natural := g_log_table_size;
  constant c_free_index_bits     : natural := c_table_index_bits + 1;
  constant c_table_lo_index_bits : natural := g_log_latency;
  constant c_table_hi_index_bits : natural := c_table_index_bits - c_table_lo_index_bits;
  constant c_queue_index_bits    : natural := g_log_queue_depth;
  constant c_scanners            : natural := 2 ** c_table_hi_index_bits;
  constant c_counter_bits        : natural := f_eca_max(c_queue_index_bits, c_table_index_bits);
  
  subtype t_free_index     is std_logic_vector(c_free_index_bits    -1 downto 0);
  subtype t_table_index    is std_logic_vector(c_table_index_bits   -1 downto 0);
  subtype t_queue_index    is std_logic_vector(c_queue_index_bits   -1 downto 0);
  subtype t_table_lo_index is std_logic_vector(c_table_lo_index_bits-1 downto 0);
  subtype t_scan_data      is std_logic_vector(c_table_lo_index_bits   downto 0);
  subtype t_counter        is std_logic_vector(c_counter_bits       -1 downto 0);
  
  type t_table_index_array    is array(natural range <>) of t_table_index;
  type t_queue_index_array    is array(natural range <>) of t_queue_index;
  type t_table_lo_index_array is array(natural range <>) of t_table_lo_index;
  type t_scan_data_array      is array(natural range <>) of t_scan_data;
  
  constant c_free_last_index : t_free_index := (others => '1');
  constant c_last_time       : t_time       := (others => '1');
  
  -- Select a valid record from an array
  type t_mux_record is record
    valid     : std_logic;
    index     : t_table_index;
    violation : std_logic;
  end record t_mux_record;
  type t_mux_record_array is array(natural range <>) of t_mux_record;
  
  function f_mux_select(records : t_mux_record_array) 
    return t_mux_record 
  is
    constant len : natural := records'length;
    constant mid : natural := len / 2; -- ronded down
    alias input : t_mux_record_array(len-1 downto 0) is records;
    variable lo, hi : t_mux_record;
  begin
    if len = 1 then 
      return input(0);
    else 
      hi := f_mux_select(input(len-1 downto mid));
      lo := f_mux_select(input(mid-1 downto   0));
      if hi.valid = '1' then
        return hi;
      else
        return lo;
      end if;
    end if;
  end f_mux_select;
  
  -- Queue signals
  signal q_scan_valid         : std_logic_vector      (c_scanners-1 downto 0);
  signal q_scan_time          : t_queue_index_array   (c_scanners-1 downto 0);
  signal q_scan_index         : t_table_lo_index_array(c_scanners-1 downto 0);
  signal q_scan_violation     : std_logic_vector      (c_scanners-1 downto 0);
  signal q_scan_data          : t_scan_data_array     (c_scanners-1 downto 0);
  signal q_dispatch_valid     : std_logic_vector      (c_scanners-1 downto 0);
  signal q_dispatch_time      : t_queue_index_array   (c_scanners-1 downto 0);
  signal q_dispatch_index     : t_table_lo_index_array(c_scanners-1 downto 0);
  signal q_dispatch_violation : std_logic_vector      (c_scanners-1 downto 0);
  signal q_dispatch_data      : t_scan_data_array     (c_scanners-1 downto 0);
  
  -- TableS signals
  signal ts_manage_write : std_logic_vector      (c_scanners-1 downto 0);
  signal ts_manage_index : t_table_lo_index_array(c_scanners-1 downto 0);
  signal ts_manage_time  : t_time_array          (c_scanners-1 downto 0);
  signal ts_manage_valid : std_logic_vector      (c_scanners-1 downto 0);
  signal ts_scan_index   : t_table_lo_index_array(c_scanners-1 downto 0);
  signal ts_scan_time    : t_time_array          (c_scanners-1 downto 0);
  signal ts_scan_valid   : std_logic_vector      (c_scanners-1 downto 0);
  
  -- TableD signals
  signal td_manage_write   : std_logic;
  signal td_manage_index   : t_table_index;
  signal td_manage_valid   : std_logic;
  signal td_manage_late    : std_logic;
  signal td_manage_event   : t_event;
  signal td_manage_param   : t_param;
  signal td_manage_tag     : t_tag;
  signal td_manage_tef     : t_tef;
  signal td_manage_time    : t_time;
  signal td_dispatch_index : t_table_index;
  signal td_dispatch_valid : std_logic;
  signal td_dispatch_late  : std_logic;
  signal td_dispatch_event : t_event;
  signal td_dispatch_param : t_param;
  signal td_dispatch_tag   : t_tag;
  signal td_dispatch_tef   : t_tef;
  signal td_dispatch_time  : t_time;
  
  -- Free signals
  signal fw_manage_free  : std_logic;
  signal fw_manage_index : t_table_index;
  signal fw_manage_freed : t_table_index;
  signal fr_manage_index : t_table_index;
  signal fr_manage_alloc : t_table_index;
  
  -- Counter signals
  signal counter      : t_counter;
  signal counter_next : t_counter;
  
  -- Dispatch registers
  signal dispatch_valid2     : std_logic_vector      (c_scanners-1 downto 0);
  signal dispatch_index2     : t_table_lo_index_array(c_scanners-1 downto 0);
  signal dispatch_violation2 : std_logic_vector      (c_scanners-1 downto 0);
  signal dispatch_valid1     : std_logic;
  signal dispatch_index1     : t_table_index;
  signal dispatch_violation1 : std_logic;
  
  -- Dispatch signals
  signal dispatch_manage_kill  : std_logic;
  signal dispatch_manage_index : t_table_index;
  signal dispatch_mux_records  : t_mux_record_array(c_scanners-1 downto 0);
  signal dispatch_mux_record   : t_mux_record;
  
  -- Scan registers
  signal scan_time_n    : t_time_array       (c_scanners-1 downto 0);
  signal scan_time_idx  : t_queue_index_array(c_scanners-1 downto 0);
  signal scan_index     : t_table_lo_index_array(3 downto 1) := (others => (others => '0'));
  
  -- Scan signals
  signal scan_next    : t_table_lo_index;
  signal scan_time_p4 : t_queue_index;
  signal scan_time_m4 : t_queue_index;
  signal scan_valid3  : std_logic_vector(c_scanners-1 downto 0);
  signal scan_valid2  : std_logic_vector(c_scanners-1 downto 0);
  signal scan_valid1  : std_logic_vector(c_scanners-1 downto 0);
  signal scan_lesseq  : std_logic_vector(c_scanners-1 downto 0);
  signal scan_hazard  : std_logic_vector(c_scanners-1 downto 0);
  
  -- Manage registers
  signal manage_idx   : t_free_index  := (others => '0');
  signal manage_free  : std_logic;
  signal manage_freed : t_table_index;
  
  -- Manage signals
  signal manage_idx_next : t_free_index;
  signal manage_flags    : std_logic_vector(1 downto 0);
  
  -- Manage table-write registers
  signal manage_write : std_logic;
  signal manage_valid : std_logic;
  signal manage_late  : std_logic;
  signal manage_index : t_table_index;
  signal manage_event : t_event;
  signal manage_param : t_param;
  signal manage_tag   : t_tag;
  signal manage_tef   : t_tef;
  signal manage_time  : t_time;
  
  constant cd_valid_offset : natural := 0;
  constant cd_late_offset  : natural := cd_valid_offset+1;
  subtype  cd_event_range is natural range c_event_bits+cd_late_offset      downto cd_late_offset      +1;
  subtype  cd_param_range is natural range c_param_bits+cd_event_range'left downto cd_event_range'left +1;
  subtype  cd_tag_range   is natural range c_tag_bits  +cd_param_range'left downto cd_param_range'left +1;
  subtype  cd_tef_range   is natural range c_tef_bits  +cd_tag_range'left   downto cd_tag_range'left   +1;
  subtype  cd_time_range  is natural range c_time_bits +cd_tef_range'left   downto cd_tef_range'left   +1;

  subtype  cd_data_type is std_logic_vector(cd_time_range);
  constant cd_data_bits : natural := cd_data_type'left + 1; --'
  
begin

  -- Write conflicts could happen, but we eliminate the race with a hazard guard.
  -- Futhermore, because c_table_lo_index_bits < c_queue_index_bits, any conflict
  --          will be rewritten before the Q rolls over.
  Qx : for table_hi_idx in 0 to c_scanners-1 generate
  
    Q : eca_flags
      generic map(
        g_addr_bits => c_queue_index_bits,
        g_data_bits => t_scan_data'length)
      port map(
        clk_i    => clk_i,
        a_addr_i => q_scan_time     (table_hi_idx),
        a_en_i   => q_scan_valid    (table_hi_idx),
        a_data_i => q_scan_data     (table_hi_idx),
        b_addr_i => q_dispatch_time (table_hi_idx),
        b_full_o => q_dispatch_valid(table_hi_idx),
        b_data_o => q_dispatch_data (table_hi_idx));
    
    q_scan_data         (table_hi_idx)(t_table_lo_index'length) <= q_scan_violation(table_hi_idx);
    q_scan_data         (table_hi_idx)(t_table_lo_index'range)  <= q_scan_index    (table_hi_idx);
    q_dispatch_violation(table_hi_idx) <= q_dispatch_data(table_hi_idx)(t_table_lo_index'length);
    q_dispatch_index    (table_hi_idx) <= q_dispatch_data(table_hi_idx)(t_table_lo_index'range);
    
  end generate;
  
  -- The replicated part of the table
  TSx : for table_hi_idx in 0 to c_scanners-1 generate
    TS : eca_sdp
      generic map(
        g_addr_bits  => c_table_lo_index_bits,
        g_data_bits  => c_time_bits+1,
        g_dual_clock => false)
      port map(
        w_clk_i                 => clk_i,
        w_en_i                  => ts_manage_write(table_hi_idx),
        w_addr_i                => ts_manage_index(table_hi_idx),
        w_data_i(t_time'length) => ts_manage_valid(table_hi_idx),
        w_data_i(t_time'range)  => ts_manage_time (table_hi_idx),
        r_clk_i                 => clk_i,
        r_addr_i                => ts_scan_index  (table_hi_idx),
        r_data_o(t_time'length) => ts_scan_valid  (table_hi_idx),
        r_data_o(t_time'range)  => ts_scan_time   (table_hi_idx));
  end generate;

  -- The data part of the table
  TD : eca_sdp
    generic map(
      g_addr_bits  => c_table_index_bits,
      g_data_bits  => cd_data_bits,
      g_dual_clock => false)
    port map(
      w_clk_i                  => clk_i,
      w_en_i                   => td_manage_write,
      w_addr_i                 => td_manage_index,
      w_data_i(cd_valid_offset)=> td_manage_valid,
      w_data_i(cd_late_offset) => td_manage_late,
      w_data_i(cd_event_range) => td_manage_event,
      w_data_i(cd_param_range) => td_manage_param,
      w_data_i(cd_tag_range)   => td_manage_tag,
      w_data_i(cd_tef_range)   => td_manage_tef,
      w_data_i(cd_time_range)  => td_manage_time,
      r_clk_i                  => clk_i,
      r_addr_i                 => td_dispatch_index,
      r_data_o(cd_valid_offset)=> td_dispatch_valid,
      r_data_o(cd_late_offset) => td_dispatch_late,
      r_data_o(cd_event_range) => td_dispatch_event,
      r_data_o(cd_param_range) => td_dispatch_param,
      r_data_o(cd_tag_range)   => td_dispatch_tag,
      r_data_o(cd_tef_range)   => td_dispatch_tef,
      r_data_o(cd_time_range)  => td_dispatch_time);
   
  -- The free queue
  F : eca_sdp
    generic map(
      g_addr_bits  => c_table_index_bits,
      g_data_bits  => c_table_index_bits,
      g_dual_clock => false)
    port map(
      w_clk_i  => clk_i,
      w_en_i   => fw_manage_free,
      w_addr_i => fw_manage_index,
      w_data_i => fw_manage_freed,
      r_clk_i  => clk_i,
      r_addr_i => fr_manage_index,
      r_data_o => fr_manage_alloc);
  
  counter_next <= f_eca_add(counter, 1);
  
  Count : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        counter <= (others => '0');
      else
        counter <= counter_next;
      end if;
    end if;
  end process;
  
  -- The dispatch pipeline latches these registers:
  -- t3:   q_dispatch_time(*)
  -- t2:   dispatch_valid2(*)  dispatch_index2(*)
  -- t1:   dispatch_valid1     dispatch_index1=td_dispatch_index
  -- t0:               channel_o (async)
  Dispatch : process(clk_i)
  begin
    if rising_edge(clk_i) then
      -- No reset; logic is acyclic
      dispatch_valid2     <= q_dispatch_valid;
      dispatch_index2     <= q_dispatch_index;
      dispatch_violation2 <= q_dispatch_violation;
      -- Block output when draining or frozen
      dispatch_valid1     <= dispatch_mux_record.valid and not (drain_i or freeze_i);
      dispatch_index1     <= dispatch_mux_record.index;
      dispatch_violation1 <= dispatch_mux_record.violation;
    end if;
  end process;

  DispatchQ : for table_hi_idx in 0 to c_scanners-1 generate
    -- When drain_i='1' there may not be a valid time_i...
    q_dispatch_time(table_hi_idx) <= 
      counter(c_queue_index_bits-1 downto 0) when drain_i='1' else 
      scan_time_p4;
  end generate;
  
  DispatchMux : for table_hi_idx in 0 to c_scanners-1 generate
    dispatch_mux_records(table_hi_idx).valid <= dispatch_valid2(table_hi_idx);
    dispatch_mux_records(table_hi_idx).index(c_table_lo_index_bits-1 downto 0) <=
      dispatch_index2(table_hi_idx);
    HighBits : if c_table_hi_index_bits > 0 generate
      dispatch_mux_records(table_hi_idx).index(c_table_index_bits-1 downto c_table_lo_index_bits) <=
        std_logic_vector(to_unsigned(table_hi_idx, c_table_hi_index_bits));
    end generate;
    dispatch_mux_records(table_hi_idx).violation <= dispatch_violation2(table_hi_idx);
  end generate;
  dispatch_mux_record <= f_mux_select(dispatch_mux_records);
  
  td_dispatch_index <= addr_i when freeze_i='1' else dispatch_mux_record.index;
  
  channel_o.valid    <= dispatch_valid1;
  channel_o.conflict <= dispatch_violation1 and not td_dispatch_late;
  channel_o.late     <= td_dispatch_late;
  channel_o.event    <= td_dispatch_event;
  channel_o.param    <= td_dispatch_param;
  channel_o.tag      <= td_dispatch_tag;
  channel_o.tef      <= td_dispatch_tef;
  channel_o.time     <= td_dispatch_time;
  
  inspect_o.valid    <= td_dispatch_valid;
  inspect_o.conflict <= '0';
  inspect_o.late     <= td_dispatch_late;
  inspect_o.event    <= td_dispatch_event;
  inspect_o.param    <= td_dispatch_param;
  inspect_o.tag      <= td_dispatch_tag;
  inspect_o.tef      <= td_dispatch_tef;
  inspect_o.time     <= td_dispatch_time;
  
  dispatch_manage_kill  <= dispatch_valid1;
  dispatch_manage_index <= dispatch_index1;
  
  -- The scan pipeline latches these registers (for depth=2):
  --   t3: ts_scan_index(*)                     scan_index(3)
  --   t2: scan_time(*)                         scan_index(2)
  --   t1:                   scan_time_idx(*)   scan_index(1)
  --   t0: q_scan_valid(*)   q_scan_time(*)     q_scan_index(*)
  scan_next <= counter_next(scan_next'range);
  ScanIndex : process(clk_i)
  begin
    if rising_edge(clk_i) then
      -- scan_index is acyclic
      scan_index(3 downto 1) <= scan_next & scan_index(3 downto 2);
      
      scan_time_p4 <= f_eca_add(time_i(scan_time_p4'range), 5);
      scan_time_m4 <= f_eca_add(time_i(scan_time_m4'range), -3);
    end if;
  end process;
  
  Sx : for table_hi_idx in 0 to c_scanners-1 generate
    Scan : process(clk_i)
      variable time_idx : t_queue_index;
    begin
      if rising_edge(clk_i) then
        -- No reset; logic is acyclic
        
        scan_valid3(table_hi_idx) <= -- beware of RW conflict on memory
          f_eca_active_high(ts_manage_index(table_hi_idx) /= ts_scan_index(table_hi_idx));
        
        scan_valid2(table_hi_idx) <= ts_scan_valid(table_hi_idx) and scan_valid3(table_hi_idx);
        scan_time_n(table_hi_idx) <= not ts_scan_time(table_hi_idx);
        
        scan_valid1(table_hi_idx) <= scan_valid2(table_hi_idx);
        time_idx := not scan_time_n(table_hi_idx)(time_idx'range);
        scan_time_idx(table_hi_idx) <= time_idx;
        -- pipeline hazard if: (time_idx - (time_i - 4)) <= 12
        scan_hazard(table_hi_idx) <= 
          f_eca_active_high(
            (unsigned(time_idx) - unsigned(scan_time_m4))
            <= 12);
      end if;
    end process;
    
    ts_scan_index(table_hi_idx) <= scan_next;
    
    -- c_o=1 iff    time_Q_i - scan_time >= 0  ...   scan_time <= time_Q_i
    ScanCompare : eca_adder
      port map(
        clk_i => clk_i,
        a_i   => time_Q_i,
        b_i   => scan_time_n(table_hi_idx),
        c_i   => '1',
        c1_o  => scan_lesseq(table_hi_idx),
        x2_o  => open,
        c2_o  => open);
    
    -- This is used to detect conflicts where two actions happen at once.
    -- We already tested for a late arriving event in the eca_walker.
    -- The only possible reason an action could be late if it arrived on time 
    -- is if there was a conflict.
    -- If there was a conflict, then the scanner will see it at least twice
    -- before it reachs the dispatcher again. Thus we can be pretty sloppy
    -- and just compare the current time and the target time.
    ScanViolation : eca_adder
      port map(
        clk_i => clk_i,
        a_i   => time_i,
        b_i   => scan_time_n(table_hi_idx),
        c_i   => '1',
        c1_o  => q_scan_violation(table_hi_idx),
        x2_o  => open,
        c2_o  => open);
    
    q_scan_valid(table_hi_idx) <= scan_lesseq(table_hi_idx) and 
                                  scan_valid1(table_hi_idx) and
                                  not scan_hazard(table_hi_idx);
    q_scan_index(table_hi_idx) <= scan_index(1);
    q_scan_time (table_hi_idx) <= scan_time_idx(table_hi_idx);
  end generate;
  
  -- The manager is not pipelined
  Manage : process(clk_i)
  begin
    if rising_edge(clk_i) then
      manage_free  <= fw_manage_free;
      
      -- In case channel_i.valid immediate after drain_i goes to '0'
      -- preload manage_freed with 0s to be used in bypassed manage_index
      if drain_i = '1' then
        manage_idx <= (others => '0');
        manage_freed <= (others => '0');
      else
        manage_idx <= manage_idx_next;
        manage_freed <= dispatch_manage_index;
      end if;
    end if;
  end process;
  
  fill_o <= manage_idx;
  full_o <= manage_idx_next(c_free_index_bits-1);
  
  manage_flags(1) <= channel_i.valid;
  manage_flags(0) <= dispatch_manage_kill;
  
  with manage_flags select manage_idx_next <=
    manage_idx                when "00",   -- No alloc, No free
    f_eca_add(manage_idx, -1) when "01",   -- No alloc, FREE
    f_eca_add(manage_idx,  1) when "10",   -- ALLOC,    No free
    manage_idx                when others; -- ALLOC,    FREE
  
  fr_manage_index <= manage_idx_next(c_table_index_bits-1 downto 0);
  fw_manage_free  <= (not channel_i.valid and dispatch_manage_kill) or drain_i;
  fw_manage_index <= counter(fw_manage_index'range) when drain_i = '1' else
                     manage_idx_next(c_table_index_bits-1 downto 0);
  fw_manage_freed <= counter(fw_manage_freed'range) when drain_i ='1' else
                     dispatch_manage_index;

  -- What to write to the table?
  -- We register this because the fanout is too large otherwise
  Writer : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      manage_write <= dispatch_manage_kill or channel_i.valid or drain_i;
      
      if drain_i = '1' then
        manage_index <= counter(manage_index'range);
      elsif dispatch_manage_kill = '1' then
        manage_index <= dispatch_manage_index;
      elsif manage_free = '1' then -- new-data bypass
        manage_index <= manage_freed;
      else
        manage_index <= fr_manage_alloc;
      end if;
      
      manage_valid <= not drain_i and channel_i.valid;
      manage_late  <= channel_i.late;
      manage_event <= channel_i.event;
      manage_param <= channel_i.param;
      manage_tag   <= channel_i.tag;
      manage_tef   <= channel_i.tef;
      manage_time  <= channel_i.time;
    end if;
  end process;
  
  -- Replicate the record write to all table components:
  ManageTS : for table_hi_idx in 0 to c_scanners-1 generate
    NoCompare : if c_table_hi_index_bits = 0 generate
      ts_manage_write(table_hi_idx) <= manage_write;
    end generate;
    HighCompare : if c_table_hi_index_bits > 0 generate
      ts_manage_write(table_hi_idx) <= 
        manage_write and 
        f_eca_active_high(to_unsigned(table_hi_idx, c_table_hi_index_bits) =
                        unsigned(manage_index(c_table_index_bits-1 downto c_table_lo_index_bits)));
    end generate;
    
    ts_manage_index(table_hi_idx) <= manage_index(c_table_lo_index_bits-1 downto 0);
    ts_manage_valid(table_hi_idx) <= manage_valid;
    ts_manage_time (table_hi_idx) <= manage_time;
  end generate;
  
  td_manage_write <= manage_write;
  td_manage_index <= manage_index;
  td_manage_valid <= manage_valid;
  td_manage_late  <= manage_late;
  td_manage_event <= manage_event;
  td_manage_param <= manage_param;
  td_manage_tag   <= manage_tag;
  td_manage_tef   <= manage_tef;
  td_manage_time  <= manage_time;
    
end rtl;
