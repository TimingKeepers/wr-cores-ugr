library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.global_defs.all;
use work.endpoint_pkg.all;

entity ep_address_match is
  generic(
    g_num_matching_rules : integer := 4
    );
  port(
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    enable_i : in std_logic;

    snk_data_i   : in std_logic_vector(15 downto 0);
    snk_sof_p1_i : in std_logic;
    snk_valid_i  : in std_logic;

    ep_macl_i : in std_logic_vector(31 downto 0);
    ep_mach_i : in std_logic_vector(15 downto 0);

    ep_afcr_enable           : in std_logic;
    ep_afcr_rule_sel_i       : in std_logic_vector(2 downto 0);
    ep_afcr_matrix_addr_i    : in std_logic_vector(7 downto 0);
    ep_afcr_matrix_data_i    : in std_logic_vector(7 downto 0);
    ep_afcr_matrix_write_p_i : in std_logic;

    ep_afr0_load_i     : in std_logic;
    ep_afr0_dmac_en_i  : in std_logic;
    ep_afr0_vid_en_i   : in std_logic;
    ep_afr0_etype_en_i : in std_logic;
    ep_afr0_vid_i      : in std_logic_vector(11 downto 0);

    ep_afr1_load_i    : in std_logic;
    ep_afr1_dmac_lo_i : in std_logic_vector(31 downto 0);

    ep_afr2_load_i    : in std_logic;
    ep_afr2_dmac_hi_i : in std_logic_vector(15 downto 0);
    ep_afr2_etype_i   : in std_logic_vector(15 downto 0);

    r_done_o         : out std_logic;
    r_drop_o         : out std_logic;
    r_match_o        : out std_logic_vector(g_num_matching_rules+1 downto 0);
    r_is_pause_o     : out std_logic;
    r_pause_quanta_o : out std_logic_vector(15 downto 0)

    );

end ep_address_match;

architecture behavioral of ep_address_match is

  component ep_address_compare
    generic (
      g_userval_match_offset   : integer := 0;
      g_userval_match_enable   : boolean := false;
      g_userval_extract_offset : integer := 0;
      g_userval_extract_enable : boolean := false);
    port (
      clk_sys_i        : in  std_logic;
      rst_n_i          : in  std_logic;
      enable_i         : in  std_logic;
      snk_data_i       : in  std_logic_vector(15 downto 0);
      snk_sof_p1_i     : in  std_logic;
      snk_valid_i      : in  std_logic;
      dmac_en_i        : in  std_logic                     := '0';
      dmac_i           : in  std_logic_vector(47 downto 0) := x"000000000000";
      vid_en_i         : in  std_logic                     := '0';
      vid_i            : in  std_logic_vector(11 downto 0) := x"000";
      etype_en_i       : in  std_logic                     := '0';
      etype_i          : in  std_logic_vector(15 downto 0) := x"0000";
      user_val_i       : in  std_logic_vector(15 downto 0) := x"0000";
      user_en_i        : in  std_logic                     := '0';
      done_o           : out std_logic;
      match_o          : out std_logic;
      user_extracted_o : out std_logic_vector(15 downto 0));
  end component;

-- rule array layout:
-- slot 0: broadcast
-- slot 1: own MAC address
-- slot 2: user rule 0
-- slot 3: user rule 1
-- slot 4: user rule 2
-- slot 5: user rule 3
-- slot N: user rule g_num_matching_rules-1

  type t_matching_rule is record
    dmac_en : std_logic;
    dmac    : std_logic_vector(47 downto 0);

    vid_en : std_logic;
    vid    : std_logic_vector(11 downto 0);

    etype_en : std_logic;
    etype    : std_logic_vector(15 downto 0);
  end record;

  type t_rule_array is array(0 to g_num_matching_rules+1) of t_matching_rule;
  type t_matching_matrix is array(0 to g_num_matching_rules+1) of std_logic_vector(g_num_matching_rules+1 downto 0);

  signal user_rules : t_rule_array;
  signal matrix     : t_matching_matrix;

  signal match_pause  : std_logic;
  signal compare_done : std_logic;
  signal own_mac      : std_logic_vector(47 downto 0);
  signal matrix_out   : std_logic_vector(g_num_matching_rules+1 downto 0);
  signal user_match   : std_logic_vector(g_num_matching_rules+1 downto 0);

  signal enable_int : std_logic;
  signal rule_addr  : integer;
  

  
  
  
begin  -- behavioral


  enable_int <= ep_afcr_enable and enable_i;

  own_mac <= ep_mach_i & ep_macl_i;

  -- special case for PAUSE frames
  U_Compare_PAUSE : ep_address_compare
    generic map (
      g_userval_match_offset   => 9,
      g_userval_match_enable   => true,
      g_userval_extract_offset => 10,
      g_userval_extract_enable => true)
    port map (
      clk_sys_i        => clk_sys_i,
      rst_n_i          => rst_n_i,
      enable_i         => enable_int,
      snk_data_i       => snk_data_i,
      snk_sof_p1_i     => snk_sof_p1_i,
      snk_valid_i      => snk_valid_i,
      dmac_en_i        => '1',
      dmac_i           => x"0180c2000001",
      etype_en_i       => '1',
      etype_i          => x"8808",
      user_en_i        => '1',
      user_val_i       => x"0001",
      done_o           => compare_done,
      match_o          => match_pause,
      user_extracted_o => r_pause_quanta_o
      );


  U_Compare_BCAST : ep_address_compare
    port map (
      clk_sys_i        => clk_sys_i,
      rst_n_i          => rst_n_i,
      enable_i         => enable_int,
      snk_data_i       => snk_data_i,
      snk_sof_p1_i     => snk_sof_p1_i,
      snk_valid_i      => snk_valid_i,
      dmac_en_i        => '1',
      dmac_i           => x"ffffffffffff",
      vid_en_i         => user_rules(0).vid_en,
      vid_i            => user_rules(0).vid,
      done_o           => open,
      match_o          => user_match(0),
      user_extracted_o => open);

  U_Compare_OWN_MAC : ep_address_compare
    port map (
      clk_sys_i        => clk_sys_i,
      rst_n_i          => rst_n_i,
      enable_i         => enable_int,
      snk_data_i       => snk_data_i,
      snk_sof_p1_i     => snk_sof_p1_i,
      snk_valid_i      => snk_valid_i,
      dmac_en_i        => '1',
      dmac_i           => own_mac,
      vid_en_i         => user_rules(1).vid_en,
      vid_i            => user_rules(1).vid,
      done_o           => open,
      match_o          => user_match(1),
      user_extracted_o => open);

  gen_user_rules : for i in 0 to g_num_matching_rules-1 generate
    U_Compare_USER : ep_address_compare
      port map (
        clk_sys_i        => clk_sys_i,
        rst_n_i          => rst_n_i,
        enable_i         => enable_i,
        snk_data_i       => snk_data_i,
        snk_sof_p1_i     => snk_sof_p1_i,
        snk_valid_i      => snk_valid_i,
        dmac_en_i        => user_rules(2+i).dmac_en,
        dmac_i           => user_rules(2+i).dmac,
        vid_en_i         => user_rules(2+i).vid_en,
        vid_i            => user_rules(2+i).vid,
        etype_en_i       => user_rules(2+i).etype_en,
        etype_i          => user_rules(2+i).etype,
        done_o           => open,
        match_o          => user_match(2+i),
        user_extracted_o => open);
  end generate gen_user_rules;

  rule_addr <= to_integer(unsigned(ep_afcr_rule_sel_i));

  p_update_rules : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0') then
        for i in 0 to g_num_matching_rules+1 loop
          user_rules(i).dmac     <= (others => '0');
          user_rules(i).dmac_en  <= '0';
          user_rules(i).vid      <= (others => '0');
          user_rules(i).vid_en   <= '0';
          user_rules(i).etype    <= (others => '0');
          user_rules(i).etype_en <= '0';
        end loop;  -- i
      else

        -- handle writes to the rule table
        if(ep_afr0_load_i = '1') then
          user_rules(rule_addr).dmac_en  <= ep_afr0_dmac_en_i;
          user_rules(rule_addr).vid_en   <= ep_afr0_vid_en_i;
          user_rules(rule_addr).etype_en <= ep_afr0_etype_en_i;
          user_rules(rule_addr).vid      <= ep_afr0_vid_i;
        end if;

        if(ep_afr1_load_i = '1') then
          user_rules(rule_addr).dmac(31 downto 0) <= ep_afr1_dmac_lo_i;
        end if;

        if(ep_afr2_load_i = '1') then
          user_rules(rule_addr).dmac(47 downto 32) <= ep_afr2_dmac_hi_i;
          user_rules(rule_addr).etype              <= ep_afr2_etype_i;
        end if;

      end if;
    end if;
  end process;

  p_update_matrix : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(ep_afcr_matrix_write_p_i = '1') then
        matrix(to_integer(unsigned(ep_afcr_matrix_addr_i(g_num_matching_rules+1 downto 0)))) <=
          ep_afcr_matrix_data_i(g_num_matching_rules+1 downto 0);
      end if;
    end if;
  end process;



  matrix_out <= matrix(to_integer(unsigned(user_match)));

  p_decision_output : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0' or enable_int = '0') then
        r_done_o    <= '0';
        r_drop_o  <= '0';
        r_match_o <= (others => '0');
      else
        r_done_o <= compare_done;
        if(unsigned(matrix_out) = 0) then
          r_drop_o <= '1';
        else
          r_drop_o <= '0';
        end if;

        r_match_o    <= matrix_out;
        r_is_pause_o <= match_pause;
        
      end if;
    end if;
  end process;

  
  
end behavioral;

