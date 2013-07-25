-------------------------------------------------------------------------------
-- Title      : Bang-bang phase/frequency detector
-- Project    : White Rabbit
-------------------------------------------------------------------------------
-- File       : spll_bangbang_pd.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-Co-HT
-- Created    : 2010-06-14
-- Last update: 2013-03-19
-- Platform   : FPGA-generic
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description: Bang-bang type phase detector. clk_ref_i and clk_fbck_i clocks
-- are divided div_ref_i and div_fb_i respectively and
-- compared. The phase error is outputted every (2^pd_gate_i + 10)
-- clk_fbck_i cycles. Divider counters can be synchronized at any moment 
-- by pulsing the sync_p_i signal.
-------------------------------------------------------------------------------
-- Copyright (c) 2010 Tomasz Wlostowski
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2010-06-14  1.0      twlostow        Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gencores_pkg.all;

entity spll_bangbang_pd is

  generic(
    g_error_bits       : integer
    );
  port (
-------------------------------------------------------------------------------
-- Clocks & resets
-------------------------------------------------------------------------------

-- reference clock
    clk_ref_i : in std_logic;

-- fed-back (VCO) clock
    clk_fb_i : in std_logic;

-- system clock (wishbone and I/O)
    clk_sys_i : in std_logic;

-- reset signals (the same reset synced to different clocks)
    rst_n_refclk_i : in std_logic;
    rst_n_fbck_i   : in std_logic;
    rst_n_sysclk_i : in std_logic;

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

    cfg_div_ref_i: in std_logic_vector(5 downto 0);
    cfg_div_fb_i: in std_logic_vector(5 downto 0);
    cfg_gating_i: in std_logic_vector(3 downto 0);
    
-------------------------------------------------------------------------------
-- I/O
-------------------------------------------------------------------------------

    -- sync input (clk_ref_i domain)
    sync_p_i    : in  std_logic;
    sync_en_i   : in  std_logic;
    sync_done_o : out std_logic;

    err_wrap_o : out std_logic;
    err_o      : out std_logic_vector(g_error_bits-1 downto 0);
    err_stb_o  : out std_logic;

    ref_present_o : out std_logic
    );

  attribute rom_extract                     : string;
  attribute rom_extract of spll_bangbang_pd : entity is "no";
end spll_bangbang_pd;

architecture rtl of spll_bangbang_pd is

  
  signal gate_counter : unsigned(15 downto 0);
  signal gate_p       : std_logic;

  signal ph_sreg_delay : std_logic_vector(4 downto 0);
  signal ph_err_p      : std_logic;

  signal ph_err, ph_err_snap : unsigned(g_error_bits downto 0);
  signal ph_err_wrap         : std_logic;

  -- phase detector input signals (after division)
  signal pd_in_ref  : std_logic;
  signal pd_in_fbck : std_logic;

  -- phase detector outputs
  signal pd_a, pd_b, pd_t, pd_ta : std_logic;
  signal pd_up, pd_down          : std_logic;
  signal atb_together            : std_logic_vector(2 downto 0);


  -- divider counters
  signal div_ctr_fbck : unsigned(5 downto 0);
  signal div_ctr_ref  : unsigned(5 downto 0);

  -- disable RAM extraction (XST is trying to implement the phase detector in a
  -- RAM)
  signal sync_p_d0  : std_logic;
  signal sync_en_d0 : std_logic;

  signal ref_presence_counter        : unsigned(7 downto 0);
  signal ref_present, ref_presence_p : std_logic;

  
begin  -- rtl

-- divide the reference clock by g_ref_divider. Note that the reference clock must be 2x slower
-- than the VCO clock for proper operation of the BB detector
  p_divide_ref : process (clk_ref_i, rst_n_refclk_i)
  begin  -- process
    if rising_edge(clk_ref_i) then

      sync_p_d0  <= sync_p_i;
      sync_en_d0 <= sync_en_i;

      if rst_n_refclk_i = '0'then
        div_ctr_ref <= to_unsigned(1, div_ctr_ref'length);
        pd_in_ref   <= '0';
        sync_done_o <= '0';
        
      elsif(sync_p_d0 = '0' and sync_p_i = '1' and sync_en_d0 = '1' and sync_en_i = '1') then
        div_ctr_ref <= to_unsigned(1, div_ctr_ref'length);
        pd_in_ref   <= '0';
        sync_done_o <= '1';
      else

        if(sync_en_i = '0') then
          sync_done_o <= '0';
        end if;


        -- the divider itself
        if (div_ctr_ref = unsigned(cfg_div_ref_i)) then
          div_ctr_ref <= to_unsigned(1, div_ctr_ref'length);
          pd_in_ref   <= not pd_in_ref;
        else
          div_ctr_ref <= div_ctr_ref + 1;
        end if;
      end if;
    end if;
  end process;


-- Divides the VCO clock by g_feedback_divider, output signal: pd_in_fbck
  p_divide_fb : process (clk_fb_i, rst_n_fbck_i)
  begin  -- process
    if rising_edge(clk_fb_i) then

      if rst_n_fbck_i = '0' then
        div_ctr_fbck <= to_unsigned(1, div_ctr_fbck'length);
        pd_in_fbck   <= '0';
      else
        if (div_ctr_fbck = unsigned(cfg_div_fb_i)) then
          div_ctr_fbck <= to_unsigned(1, div_ctr_fbck'length);
          pd_in_fbck   <= not pd_in_fbck;  -- divide the clock :)
        else
          div_ctr_fbck <= div_ctr_fbck + 1;
        end if;
      end if;
    end if;
  end process;


  U_Detect_Ref_Pulses : gc_sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => clk_sys_i,
      rst_n_i  => rst_n_sysclk_i,
      data_i   => pd_in_ref,
      synced_o => open,
      npulse_o => open,
      ppulse_o => ref_presence_p);

-- Checks for presence of the reference clock

  p_check_ref_presence : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_sysclk_i = '0' then
        ref_present_o        <= '0';
        ref_presence_counter <= (others => '0');
      else
        if(ref_presence_p = '1') then
          ref_presence_counter <= (others => '0');
          ref_present_o        <= '1';
        elsif(ref_presence_counter /= x"ff") then
          ref_present_o        <= '1';
          ref_presence_counter <= ref_presence_counter + 1;
        else
          ref_present_o <= '0';
        end if;
      end if;
    end if;
  end process;

-------------------------------------------------------------------------------
-- Bang-bang PD
-------------------------------------------------------------------------------  

--  pd_in_ref <= clk_ref_div(0);

  bb_pd_negedge : process(pd_in_fbck)
  begin
    if falling_edge(pd_in_fbck) then
      pd_ta <= pd_in_ref;
    end if;
  end process;


  bb_pd_posedge : process(pd_in_fbck)
  begin
    if rising_edge(pd_in_fbck) then
      pd_b <= pd_in_ref;
      pd_a <= pd_b;
      pd_t <= pd_ta;
    end if;
  end process;

  atb_together <= pd_a & pd_t & pd_b;

  decode_bangbang : process(atb_together)
  begin
    case (atb_together) is
      when "000" =>                     -- no transition
        pd_up   <= '0';
        pd_down <= '0';
      when "001" =>                     -- too fast
        pd_up   <= '0';
        pd_down <= '1';
      when "010" =>                     -- invalid
        pd_up   <= '1';
        pd_down <= '1';
      when "011" =>                     -- too slow
        pd_up   <= '1';
        pd_down <= '0';
      when "100" =>                     -- too slow
        pd_up   <= '1';
        pd_down <= '0';
      when "101" =>                     -- invalid
        pd_up   <= '1';
        pd_down <= '1';
      when "110" =>                     -- too fast
        pd_up   <= '0';
        pd_down <= '1';
      when "111" =>                     -- no transition
        pd_up   <= '0';
        pd_down <= '0';
      when others => null;
    end case;
  end process;


-- decodes the PD_GATE field from PCR register and generates the gating pulse
-- on gate_p.
  phase_gating_decode : process (gate_counter)
    variable decoded_gating : integer;
  begin
    decoded_gating := to_integer(unsigned(cfg_gating_i));
    if(gate_counter(decoded_gating) = '1') then
      gate_p <= '1';
    else
      gate_p <= '0';
    end if;
  end process;

-- error counter: accumulates UP/DOWN pulses from the phase detector in
-- updown_counter. The counter value is outputted to ph_err when there's a
-- pulse on gate_p signal and then the accumulating counter is reset.

  count_updown : process(clk_fb_i, rst_n_fbck_i)
  begin
    if rising_edge(clk_fb_i) then
      if rst_n_fbck_i = '0' then
        gate_counter <= to_unsigned(1, gate_counter'length);
        ph_err       <= (others => '0');
        ph_err_snap  <= (others => '0');
        
      else


        if(gate_p = '1') then
-- got a gating pulse? output the new phase value
          ph_err_snap <= ph_err;
          ph_err_wrap <= ph_err_snap(ph_err_snap'length-1) xor ph_err(ph_err'length-1);

          gate_counter  <= to_unsigned(1, gate_counter'length);
          ph_sreg_delay <= (others => '1');
        else
          gate_counter  <= gate_counter + 1;
          ph_sreg_delay <= '0' & ph_sreg_delay (ph_sreg_delay'length -1 downto 1);
        end if;


-- count the PD detector pulses
        if(pd_up = '1' and pd_down = '0') then
          ph_err <= ph_err + 1;
        elsif (pd_up = '0' and pd_down = '1') then
          ph_err <= ph_err - 1;
        end if;
      end if;
    end if;
  end process;

-- sync chain (from clk_fb_i to clk_sys_i) for the new phase error pulse (ph_err_p).
  sync_ffs_phase_p : gc_sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => clk_sys_i,
      rst_n_i  => rst_n_sysclk_i,
      data_i   => ph_sreg_delay(0),
      synced_o => open,
      npulse_o => open,
      ppulse_o => ph_err_p);

  error_output : process (clk_sys_i, rst_n_sysclk_i)
  begin  -- process
    if rising_edge(clk_sys_i) then
      if (rst_n_sysclk_i = '0') then
        err_o      <= (others => '0');
        err_wrap_o <= '0';
        err_stb_o  <= '0';
      else
        err_o      <= std_logic_vector(ph_err_snap(g_error_bits-1 downto 0));
        err_wrap_o <= ph_err_wrap;
        err_stb_o  <= ph_err_p;
      end if;
    end if;
  end process;

end rtl;

