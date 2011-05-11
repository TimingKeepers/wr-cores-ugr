library ieee;
use ieee.std_logic_1164.all;
use ieee.NUMERIC_STD.all;

library work;
use work.gencores_pkg.all;

entity multi_dmtd_with_deglitcher is
  generic (
    g_counter_bits     : natural := 17;
    g_log2_replication : natural := 2
    );

  port (
    rst_n_dmtdclk_i : in std_logic;
    rst_n_sysclk_i  : in std_logic;

    -- DMTD sampling clock
    clk_dmtd_i : in std_logic;

    -- system clock
    clk_sys_i : in std_logic;

    -- input clock
    clk_in_i : in std_logic;

    -- edge tag (clk_sys_i domain)
    tag_o       : out std_logic_vector(g_counter_bits-1 downto 0);
    tag_stb_p_o : out std_logic;


    -- phase shifter enable, HI level shifts the tag forward/backward by
    -- 1 clk_dmtd_i with respect to the expected tag value.
    shift_en_i : in std_logic;

    -- phase shift direction: 1 - forward, 0 - backward
    shift_dir_i : in std_logic;

    -- deglitcher parameters (clk_dmtd_i domain)
    deglitch_threshold_i: in std_logic_vector(15 downto 0);

    dbg_dmtdout_o : out std_logic
    );

end multi_dmtd_with_deglitcher;

architecture rtl of multi_dmtd_with_deglitcher is
  constant c_num_dmtds : natural := 2**g_log2_replication;

  function f_count_zeroes (x : std_logic_vector) return unsigned is
    variable i   : integer;
    variable cnt : unsigned(g_log2_replication downto 0);
  begin
    cnt := (others => '0');

    for i in 0 to x'left loop
      if(x(i) = '0') then
        cnt := cnt + 1;
      end if;
    end loop;  -- i

    return cnt;
  end function f_count_zeroes;

  type t_state is (WAIT_STABLE_0, WAIT_EDGE, GOT_EDGE, ROUND_TAG);

  signal state : t_state;

  signal stab_cntr : unsigned(deglitch_threshold_i'left downto 0);
  signal free_cntr : unsigned(g_counter_bits-1 downto 0);

  signal new_edge_sreg : std_logic_vector(3 downto 0);
  signal new_edge_p    : std_logic;

  signal tag_int : unsigned(g_counter_bits + g_log2_replication -1 downto 0);


  signal clk_i_d0, clk_i_d1, clk_i_d2 : std_logic_vector(c_num_dmtds-1 downto 0);
  signal dmtd_in                      : std_logic_vector(c_num_dmtds-1 downto 0);
  signal n_zeroes                     : unsigned(g_log2_replication downto 0);

  signal in_is_0, in_is_1 : std_logic;
  signal ones             : std_logic_vector(31 downto 0) := x"ffffffff";
  signal zeroes           : std_logic_vector(31 downto 0) := x"00000000";
  
  
begin  -- rtl

  gen_dmtds : for i in 0 to c_num_dmtds-1 generate
    the_dmtd_itself : process(clk_dmtd_i)
    begin
      if rising_edge(clk_dmtd_i) then
        clk_i_d0(i) <= clk_in_i;
        clk_i_d1(i) <= clk_i_d0(i);
        clk_i_d2(i) <= clk_i_d1(i);
        dmtd_in(i)  <= clk_i_d2(i);
      end if;
    end process;
  end generate gen_dmtds;



  dmtd_postprocess : process(clk_dmtd_i)
  begin
    if rising_edge(clk_dmtd_i) then
      
      if(dmtd_in = ones(dmtd_in'left downto 0)) then
        in_is_1 <= '1';
      else
        in_is_1 <= '0';
      end if;

      if(dmtd_in = zeroes(dmtd_in'left downto 0)) then
        in_is_0 <= '1';
      else
        in_is_0 <= '0';
      end if;

      n_zeroes <= f_count_zeroes(dmtd_in);
      dbg_dmtdout_o <= n_zeroes(0);
    end if;
  end process;


  deglitch : process (clk_dmtd_i)       -- glitchproof DMTD output edge
-- detection
  begin  -- process deglitch
    
    
    if rising_edge(clk_dmtd_i) then     -- rising clock edge

      if rst_n_dmtdclk_i = '0' then     -- asynchronous reset (active low)
        stab_cntr     <= (others => '0');
        free_cntr     <= (others => '0');
        state         <= WAIT_STABLE_0;
        new_edge_sreg <= (others => '0');
      else

        if(shift_en_i = '0') then
          free_cntr <= free_cntr + 1;
        elsif(shift_dir_i = '1') then
          free_cntr <= free_cntr + 2;
        end if;

        case state is

          when WAIT_STABLE_0 =>         -- out-of-sync

            new_edge_sreg <= '0' & new_edge_sreg(new_edge_sreg'left downto 1);

            if (in_is_0 = '0') then
              stab_cntr <= (others => '0');
            else
              stab_cntr <= stab_cntr + 1;
            end if;

            -- DMTD output stable counter hit the LOW level threshold?
            if stab_cntr = unsigned(deglitch_threshold_i) then
              state <= WAIT_EDGE;
            end if;

          when WAIT_EDGE =>

            if (in_is_0 = '0') then     -- got a glitch?
              state   <= GOT_EDGE;
              tag_int <= free_cntr & to_unsigned(0, g_log2_replication);
            end if;

          when GOT_EDGE =>

            tag_int <= tag_int + n_zeroes;

            if (in_is_1 = '0') then
              stab_cntr <= (others => '0');
            else
              stab_cntr <= stab_cntr + 1;
            end if;

            if stab_cntr = unsigned(deglitch_threshold_i) then
              state         <= ROUND_TAG;
            end if;

          when ROUND_TAG =>

            if(tag_int(g_log2_replication-1) = '1') then
              tag_int <= tag_int + to_unsigned(2**g_log2_replication, g_log2_replication+1);
            end if;
            
            new_edge_sreg <= (others => '1');
            state <= WAIT_STABLE_0;
        end case;
      end if;
    end if;
  end process deglitch;

--n_zeroes <= f_count_zeroes(dmtd_in);

  sync_reset_refclk : gc_sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => clk_sys_i,
      rst_n_i  => rst_n_sysclk_i,
      data_i   => new_edge_sreg(0),
      synced_o => open,
      npulse_o => open,
      ppulse_o => new_edge_p);

  tag_stb_p_o   <= new_edge_p;
 
  tag_o         <= std_logic_vector(tag_int(tag_int'left downto g_log2_replication));


end rtl;
