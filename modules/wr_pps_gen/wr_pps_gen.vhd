-------------------------------------------------------------------------------
-- Title      : PPS Generator & UTC Realtime clock
-- Project    : WhiteRabbit Switch
-------------------------------------------------------------------------------
-- File       : wrsw_pps_gen.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-Co-HT
-- Created    : 2010-09-02
-- Last update: 2013-08-05
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright (c) 2010 Tomasz Wlostowski
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2010-09-02  1.0      twlostow        Created
-- 2011-05-09  1.1      twlostow        Added external PPS input
-- 2011-10-26  1.2      greg.d          Added wb slave adapter
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.gencores_pkg.all;
use work.genram_pkg.all;
use work.wishbone_pkg.all;

entity wr_pps_gen is
  generic(
    g_interface_mode       : t_wishbone_interface_mode      := CLASSIC;
    g_address_granularity  : t_wishbone_address_granularity := WORD;
    g_ref_clock_rate       : integer                        := 125000000;
    g_ext_clock_rate       : integer                        := 10000000;
    g_with_ext_clock_input : boolean                        := false
    );
  port (
    clk_ref_i : in std_logic;
    clk_sys_i : in std_logic;
    clk_ext_i : in std_logic := '0';

    rst_n_i : in std_logic;

    wb_adr_i   : in  std_logic_vector(4 downto 0);
    wb_dat_i   : in  std_logic_vector(31 downto 0);
    wb_dat_o   : out std_logic_vector(31 downto 0);
    wb_cyc_i   : in  std_logic;
    wb_sel_i   : in  std_logic_vector(3 downto 0);
    wb_stb_i   : in  std_logic;
    wb_we_i    : in  std_logic;
    wb_ack_o   : out std_logic;
    wb_stall_o : out std_logic;

    -- External PPS input. Warning! This signal is treated as synchronous to
    -- the clk_ref_i (or the external 10 MHz reference) to prevent sync chain
    -- delay uncertainities. Setup/hold times must be respected!
    pps_in_i : in std_logic;

    -- Single-pulse PPS output for synchronizing endpoints to
    pps_csync_o : out std_logic;
    pps_out_o   : out std_logic;
    pps_led_o   : out std_logic;

    pps_valid_o : out std_logic;

    tm_utc_o        : out std_logic_vector(39 downto 0);
    tm_cycles_o     : out std_logic_vector(27 downto 0);
    tm_time_valid_o : out std_logic
    );
end wr_pps_gen;

architecture behavioral of wr_pps_gen is

  constant c_PERIOD : integer := g_ref_clock_rate;

  component pps_gen_wb
    port (
      rst_n_i                : in  std_logic;
      clk_sys_i              : in  std_logic;
      wb_adr_i               : in  std_logic_vector(2 downto 0);
      wb_dat_i               : in  std_logic_vector(31 downto 0);
      wb_dat_o               : out std_logic_vector(31 downto 0);
      wb_cyc_i               : in  std_logic;
      wb_sel_i               : in  std_logic_vector(3 downto 0);
      wb_stb_i               : in  std_logic;
      wb_we_i                : in  std_logic;
      wb_ack_o               : out std_logic;
      refclk_i               : in  std_logic;
      ppsg_cr_cnt_rst_o      : out std_logic;
      ppsg_cr_cnt_en_o       : out std_logic;
      ppsg_cr_cnt_adj_o      : out std_logic;
      ppsg_cr_cnt_adj_i      : in  std_logic;
      ppsg_cr_cnt_adj_load_o : out std_logic;
      ppsg_cr_cnt_set_o      : out std_logic;
      ppsg_cr_pwidth_o       : out std_logic_vector(27 downto 0);
      ppsg_cntr_nsec_i       : in  std_logic_vector(27 downto 0);
      ppsg_cntr_utclo_i      : in  std_logic_vector(31 downto 0);
      ppsg_cntr_utchi_i      : in  std_logic_vector(7 downto 0);
      ppsg_adj_nsec_o        : out std_logic_vector(27 downto 0);
      ppsg_adj_nsec_wr_o     : out std_logic;
      ppsg_adj_utclo_o       : out std_logic_vector(31 downto 0);
      ppsg_adj_utclo_wr_o    : out std_logic;
      ppsg_adj_utchi_o       : out std_logic_vector(7 downto 0);
      ppsg_adj_utchi_wr_o    : out std_logic;
      ppsg_escr_sync_o       : out std_logic;
      ppsg_escr_sync_i       : in  std_logic;
      ppsg_escr_sync_load_o  : out std_logic;
      ppsg_escr_pps_valid_o  : out std_logic;
      ppsg_escr_tm_valid_o   : out std_logic;
      ppsg_escr_sec_set_o    : out std_logic;
      ppsg_escr_nsec_set_o   : out std_logic);
  end component;


-- Wisbone slave signals
  signal ppsg_cr_cnt_rst : std_logic;
  signal ppsg_cr_cnt_en  : std_logic;

  signal ppsg_cr_cnt_adj_o    : std_logic;
  signal ppsg_cr_cnt_adj_i    : std_logic;
  signal ppsg_cr_cnt_adj_load : std_logic;

  signal ppsg_cr_cnt_set_p : std_logic;
  signal ppsg_cr_pwidth    : std_logic_vector(27 downto 0);

  signal ppsg_cntr_nsec  : std_logic_vector(27 downto 0);
  signal ppsg_cntr_utclo : std_logic_vector(31 downto 0);
  signal ppsg_cntr_utchi : std_logic_vector(7 downto 0);

  signal ppsg_adj_nsec       : std_logic_vector(27 downto 0);
  signal ppsg_adj_nsec_wr    : std_logic;
  signal ppsg_adj_utclo      : std_logic_vector(31 downto 0);
  signal ppsg_adj_utclo_wr   : std_logic;
  signal ppsg_adj_utchi      : std_logic_vector(7 downto 0);
  signal ppsg_adj_utchi_wr   : std_logic;
  signal ppsg_escr_sync_load : std_logic;
  signal ppsg_escr_sync_in   : std_logic;
  signal ppsg_escr_sync_out  : std_logic;
  signal ppsg_escr_sec_set   : std_logic;
  signal ppsg_escr_nsec_set  : std_logic;

  signal ppsg_escr_pps_valid : std_logic;
  signal ppsg_escr_tm_valid  : std_logic;

  signal cntr_nsec    : unsigned (27 downto 0);
  signal cntr_utc     : unsigned (39 downto 0);
  signal cntr_pps_ext : unsigned (24 downto 0);

  signal ns_overflow     : std_logic;
  signal ns_overflow_adv : std_logic;
  signal cntr_adjust_p   : std_logic;

  signal adj_nsec : unsigned(27 downto 0);
  signal adj_utc  : unsigned(39 downto 0);

  signal rst_synced_refclk : std_logic;

  signal adjust_in_progress_nsec : std_logic;
  signal adjust_done_nsec        : std_logic;

  signal adjust_in_progress_utc : std_logic;
  signal adjust_done_utc        : std_logic;

  signal width_cntr : unsigned(27 downto 0);

  signal sync_in_progress : std_logic;
  signal ext_sync_p       : std_logic;

  signal resized_addr : std_logic_vector(c_wishbone_address_width-1 downto 0);
  signal wb_out       : t_wishbone_slave_out;
  signal wb_in        : t_wishbone_slave_in;

  signal ns_overflow_2nd                        : std_logic;
  signal pps_in_d0, pps_ext_d0, pps_ext_retimed : std_logic;

  signal retime_counter : unsigned(4 downto 0);
  signal pps_valid_int  : std_logic;

  signal pps_out_int : std_logic;


  component chipscope_icon
    port (
      CONTROL0 : inout std_logic_vector(35 downto 0));
  end component;

  component chipscope_ila
    port (
      CONTROL : inout std_logic_vector(35 downto 0);
      CLK     : in    std_logic;
      TRIG0   : in    std_logic_vector(31 downto 0);
      TRIG1   : in    std_logic_vector(31 downto 0);
      TRIG2   : in    std_logic_vector(31 downto 0);
      TRIG3   : in    std_logic_vector(31 downto 0));
  end component;

  signal control0                   : std_logic_vector(35 downto 0);
  signal trig0, trig1, trig2, trig3 : std_logic_vector(31 downto 0);
  
begin  -- behavioral


  --CS_ICON : chipscope_icon
  --  port map (
  --    CONTROL0 => CONTROL0);
  --CS_ILA : chipscope_ila
  --  port map (
  --    CONTROL => CONTROL0,
  --    CLK     => clk_sys_i,
  --    TRIG0   => TRIG0,
  --    TRIG1   => TRIG1,
  --    TRIG2   => TRIG2,
  --    TRIG3   => TRIG3);

  TRIG0(cntr_pps_ext'length-1 downto 0) <= std_logic_vector(cntr_pps_ext);
  TRIG1(0)                              <= pps_ext_retimed;
  TRIG1(1)                              <= pps_in_i;
  TRIG1(6 downto 2)                     <= std_logic_vector(retime_counter);
  TRIG1(7)                              <= pps_ext_d0;


  resized_addr(4 downto 0)                          <= wb_adr_i;
  resized_addr(c_wishbone_address_width-1 downto 5) <= (others => '0');

  U_Adapter : wb_slave_adapter
    generic map (
      g_master_use_struct  => true,
      g_master_mode        => CLASSIC,
      g_master_granularity => WORD,
      g_slave_use_struct   => false,
      g_slave_mode         => g_interface_mode,
      g_slave_granularity  => g_address_granularity)
    port map (
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_i,
      master_i   => wb_out,
      master_o   => wb_in,
      sl_adr_i   => resized_addr,
      sl_dat_i   => wb_dat_i,
      sl_sel_i   => wb_sel_i,
      sl_cyc_i   => wb_cyc_i,
      sl_stb_i   => wb_stb_i,
      sl_we_i    => wb_we_i,
      sl_dat_o   => wb_dat_o,
      sl_ack_o   => wb_ack_o,
      sl_stall_o => wb_stall_o);

  
  sync_reset_refclk : gc_sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => clk_ref_i,
      rst_n_i  => '1',
      data_i   => rst_n_i,
      synced_o => rst_synced_refclk,
      npulse_o => open,
      ppulse_o => open);


  ppsg_cntr_nsec  <= std_logic_vector(cntr_nsec);
  ppsg_cntr_utclo <= std_logic_vector(cntr_utc(31 downto 0));
  ppsg_cntr_utchi <= std_logic_vector(cntr_utc(39 downto 32));


  -- loads adjustment values into internal regsiters
  p_wishbone_loads : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        adj_nsec <= (others => '0');
        adj_utc  <= (others => '0');
      else
        if(ppsg_adj_utchi_wr = '1') then
          adj_utc(39 downto 32) <= unsigned(ppsg_adj_utchi);
        end if;

        if(ppsg_adj_utclo_wr = '1') then
          adj_utc(31 downto 0) <= unsigned(ppsg_adj_utclo);
        end if;

        if(ppsg_adj_nsec_wr = '1') then
          adj_nsec <= unsigned(ppsg_adj_nsec);
        end if;
      end if;
    end if;
  end process;

  gen_with_external_clock_input : if(g_with_ext_clock_input) generate

    -- retime the external PPS pulse. The output (pps_ext_retimed) is:
    -- single clk_ext_i cycle-wide
    -- produced one cycle in advance with respect to the original PPS
    p_retime_external_pps : process(clk_ext_i)
    begin
      if rising_edge(clk_ext_i) then
        if rst_n_i = '0' then
          cntr_pps_ext    <= (others => '0');
          pps_ext_d0      <= '0';
          pps_ext_retimed <= '0';
        else
          pps_ext_d0 <= pps_in_i;

          if(cntr_pps_ext = g_ext_clock_rate-1) then
            pps_ext_retimed <= '1';
          else
            pps_ext_retimed <= '0';
          end if;

          if(pps_in_i = '1' and pps_ext_d0 = '0') then
            cntr_pps_ext <= to_unsigned(1, cntr_pps_ext'length);
          elsif(cntr_pps_ext /= g_ext_clock_rate) then
            cntr_pps_ext <= cntr_pps_ext + 1;
          end if;
        end if;
      end if;
    end process;

    p_retime_counter : process(clk_ref_i)
    begin
      if falling_edge(clk_ref_i) then
        if rst_synced_refclk = '0' or sync_in_progress = '0' or pps_ext_retimed = '0' then
          retime_counter <= (others => '0');
        else
          retime_counter <= retime_counter + 1;
        end if;
      end if;
    end process;


    -- Warning! this state machine inputs pps_ext_retimed signal,
    -- which is produced in different clock domain than clk_ref_i.
    -- Run only when EXT channel of the SoftPLL is LOCKED! 
    p_external_sync : process(clk_ref_i)
    begin
      if falling_edge(clk_ref_i) then
        if(rst_synced_refclk = '0') then
          ext_sync_p        <= '0';
          sync_in_progress  <= '0';
          ppsg_escr_sync_in <= '0';
        else
          if(ppsg_escr_sync_load = '1') then
            sync_in_progress  <= ppsg_escr_sync_out;
            ppsg_escr_sync_in <= '0';
          end if;

          -- retime counter == last faster clock edge inside the retimed PPS
          -- pulse -> we should sync ourselves
          if(sync_in_progress = '1' and pps_ext_retimed = '1' and retime_counter = (g_ref_clock_rate / g_ext_clock_rate - 1)) then
            ext_sync_p        <= '1';
            sync_in_progress  <= '0';
            ppsg_escr_sync_in <= '1';
          else
            ext_sync_p <= '0';
          end if;
        end if;
      end if;
    end process;




  end generate gen_with_external_clock_input;
-- Nanosecond counter. Counts from 0 to c_PERIOD-1 every clk_ref_i cycle.

  p_count_nsec : process(clk_ref_i, rst_synced_refclk)
  begin
    if rising_edge(clk_ref_i) then
      if rst_synced_refclk = '0' or ppsg_cr_cnt_rst = '1' then
        cntr_nsec               <= (others => '0');
        ns_overflow             <= '0';
        ns_overflow_adv         <= '0';
        adjust_in_progress_nsec <= '0';
        adjust_done_nsec        <= '1';

        -- counter is enabled?
      elsif(ppsg_cr_cnt_en = '1') then

        -- got ADJUST OFFSET command
        if(cntr_adjust_p = '1') then

-- start waiting for next counter overflow
          adjust_done_nsec        <= '0';
          adjust_in_progress_nsec <= '1';
        end if;

-- got SET TIME command - load the counter with new value
        if(ppsg_cr_cnt_set_p = '1' or ext_sync_p = '1' or ppsg_escr_nsec_set = '1') then
          cntr_nsec        <= adj_nsec;
          adjust_done_nsec <= '1';
          ns_overflow      <= '0';
          ns_overflow_adv  <= '0';

-- got counter overflow:
        elsif(cntr_nsec = to_unsigned(c_PERIOD-3, cntr_nsec'length)) then
          ns_overflow     <= '0';
          ns_overflow_adv <= '1';
          cntr_nsec       <= cntr_nsec + 1;
        elsif(cntr_nsec = to_unsigned(c_PERIOD-2, cntr_nsec'length)) then
          ns_overflow     <= '1';
          ns_overflow_adv <= '0';
          cntr_nsec       <= cntr_nsec + 1;
        elsif(cntr_nsec = to_unsigned(c_PERIOD-1, cntr_nsec'length)) then
          ns_overflow     <= '0';
          ns_overflow_adv <= '0';
          -- we're in the middle of offset adjustment - load the counter with
          -- offset value instead of resetting it. This equals to subtracting the offset
          -- but takes less logic. 
          if(adjust_in_progress_nsec = '1') then
            cntr_nsec               <= adj_nsec;
            adjust_done_nsec        <= '1';  -- assert done flag at the end
            adjust_in_progress_nsec <= '0';
          else
            -- normal counter reset. Generate overflow pulse.
            cntr_nsec <= (others => '0');
          end if;
        else
          ns_overflow     <= '0';
          ns_overflow_adv <= '0';
          cntr_nsec       <= cntr_nsec + 1;
        end if;
      end if;
    end if;
  end process;


  p_drive_pps_valid : process(clk_ref_i)
  begin
    if rising_edge(clk_ref_i) then
      if rst_synced_refclk = '0' then
        pps_valid_int   <= '1';
        ns_overflow_2nd <= '0';
      else
        if(sync_in_progress = '1' or adjust_in_progress_nsec = '1' or adjust_in_progress_utc = '1') then
          pps_valid_int   <= '0';
          ns_overflow_2nd <= '0';
        elsif(adjust_in_progress_utc = '0' and adjust_in_progress_nsec = '0' and sync_in_progress = '0') then

          if(ns_overflow = '1') then
            ns_overflow_2nd <= '1';
            if(ns_overflow_2nd = '1') then
              pps_valid_int <= '1';
            end if;
          end if;
        end if;
      end if;
    end if;
  end process;

  p_count_utc : process(clk_ref_i, rst_synced_refclk)
  begin
    if rising_edge(clk_ref_i) then
      if rst_synced_refclk = '0' or ppsg_cr_cnt_rst = '1' then
        cntr_utc               <= (others => '0');
        adjust_done_utc        <= '1';
        adjust_in_progress_utc <= '0';
      elsif(ppsg_cr_cnt_en = '1') then

        if(ppsg_cr_cnt_set_p = '1' or ppsg_escr_sec_set = '1') then
          cntr_utc        <= adj_utc;
          adjust_done_utc <= '1';
        elsif(cntr_adjust_p = '1') then
          adjust_in_progress_utc <= '1';
          adjust_done_utc        <= '0';

          if(ns_overflow = '1') then
            cntr_utc <= cntr_utc +1;
          end if;

        elsif(adjust_in_progress_utc = '1' and ns_overflow = '1') then
          cntr_utc               <= cntr_utc + adj_utc + 1;
          adjust_done_utc        <= '1';
          adjust_in_progress_utc <= '0';
        elsif(ns_overflow = '1') then
          cntr_utc <= cntr_utc + 1;
        end if;
      end if;
    end if;
  end process;

-- generate single-cycle PPS pulses for synchronizing endpoint TS counters
  pps_csync_o <= ns_overflow;

  -- generates variable-width PPS pulses for PPS external output
  p_gen_pps_out : process(clk_ref_i, rst_synced_refclk)
  begin
    if rising_edge(clk_ref_i) then
      if rst_synced_refclk = '0' then
        pps_out_int <= '0';
        pps_led_o   <= '0';
        width_cntr  <= (others => '0');
      else

        if(ns_overflow_adv = '1') then
          pps_out_int <= ppsg_escr_pps_valid;
          width_cntr  <= unsigned(ppsg_cr_pwidth);
        elsif(ns_overflow = '1') then
          pps_led_o <= ppsg_escr_pps_valid;
        else
          if(width_cntr = to_unsigned(0, width_cntr'length)) then
            pps_out_int <= '0';
            pps_led_o   <= '0';
          else
            width_cntr <= width_cntr -1;
          end if;
        end if;
      end if;
    end if;

  end process;

  process(clk_ref_i, rst_synced_refclk)
  begin
    if rising_edge(clk_ref_i) then
      if rst_synced_refclk = '0' then
        pps_out_o <= '0';
      else
        pps_out_o <= pps_out_int;
      end if;
    end if;
  end process;

--  pps_out_o <=pps_ext_retimed;

  Uwb_slave : pps_gen_wb
    port map (
      rst_n_i                => rst_n_i,
      clk_sys_i              => clk_sys_i,
      wb_adr_i               => wb_in.adr(2 downto 0),
      wb_dat_i               => wb_in.dat,
      wb_dat_o               => wb_out.dat,
      wb_cyc_i               => wb_in.cyc,
      wb_sel_i               => wb_in.sel,
      wb_stb_i               => wb_in.stb,
      wb_we_i                => wb_in.we,
      wb_ack_o               => wb_out.ack,
      refclk_i               => clk_ref_i,
      ppsg_cr_cnt_rst_o      => ppsg_cr_cnt_rst,
      ppsg_cr_cnt_en_o       => ppsg_cr_cnt_en,
      ppsg_cr_cnt_adj_o      => ppsg_cr_cnt_adj_o,
      ppsg_cr_cnt_adj_i      => ppsg_cr_cnt_adj_i,
      ppsg_cr_cnt_adj_load_o => ppsg_cr_cnt_adj_load,
      ppsg_escr_sync_o       => ppsg_escr_sync_out,
      ppsg_escr_sync_i       => ppsg_escr_sync_in,
      ppsg_escr_sync_load_o  => ppsg_escr_sync_load,
      ppsg_cr_cnt_set_o      => ppsg_cr_cnt_set_p,
      ppsg_cr_pwidth_o       => ppsg_cr_pwidth,
      ppsg_cntr_nsec_i       => ppsg_cntr_nsec,
      ppsg_cntr_utclo_i      => ppsg_cntr_utclo,
      ppsg_cntr_utchi_i      => ppsg_cntr_utchi,
      ppsg_adj_nsec_o        => ppsg_adj_nsec,
      ppsg_adj_nsec_wr_o     => ppsg_adj_nsec_wr,
      ppsg_adj_utclo_o       => ppsg_adj_utclo,
      ppsg_adj_utclo_wr_o    => ppsg_adj_utclo_wr,
      ppsg_adj_utchi_o       => ppsg_adj_utchi,
      ppsg_adj_utchi_wr_o    => ppsg_adj_utchi_wr,
      ppsg_escr_pps_valid_o  => ppsg_escr_pps_valid,
      ppsg_escr_tm_valid_o   => ppsg_escr_tm_valid,
      ppsg_escr_sec_set_o    => ppsg_escr_sec_set,
      ppsg_escr_nsec_set_o   => ppsg_escr_nsec_set);

-- start the adjustment upon write of 1 to CNT_ADJ bit
  cntr_adjust_p <= ppsg_cr_cnt_adj_load and ppsg_cr_cnt_adj_o;

-- drive the readout value of CNT_ADJ to 1 when the adjustment is over
  ppsg_cr_cnt_adj_i <= pps_valid_int;

  pps_valid_o <= pps_valid_int;

  tm_utc_o        <= std_logic_vector(cntr_utc);
  tm_cycles_o     <= std_logic_vector(cntr_nsec);
  tm_time_valid_o <= ppsg_escr_tm_valid;

end behavioral;
