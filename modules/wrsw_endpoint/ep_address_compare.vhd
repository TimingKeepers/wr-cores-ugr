library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.endpoint_pkg.all;

entity ep_address_compare is
  generic(
    g_userval_match_offset   : integer := 0;
    g_userval_match_enable   : boolean := false;
    g_userval_extract_offset : integer := 0;
    g_userval_extract_enable : boolean := false
    );
  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    enable_i : in std_logic;

    snk_data_i   : in std_logic_vector(15 downto 0);
    snk_sof_p1_i : in std_logic;
    snk_valid_i  : in std_logic;

    dmac_en_i : in std_logic;
    dmac_i    : in std_logic_vector(47 downto 0);

    vid_en_i : in std_logic;
    vid_i    : in std_logic_vector(11 downto 0);

    etype_en_i : in std_logic;
    etype_i    : in std_logic_vector(15 downto 0);

    user_val_i : in std_logic_vector(15 downto 0);
    user_en_i  : in std_logic;

    done_o  : out std_logic;
    match_o : out std_logic;

    user_extracted_o : out std_logic_vector(15 downto 0)
    );    

end ep_address_compare;

architecture behavioral of ep_address_compare is

  function to_sl(x : boolean) return std_logic is
  begin
    if(x = true) then
      return '1';
    else
      return '0';
    end if;
  end function to_sl;

  function f_eval_match (matches : std_logic; enable_matching : std_logic) return std_logic is
  begin
    if(enable_matching = '0') then
      return '1';
    else
      return matches;
    end if;
  end function f_eval_match;

  function f_max(x : integer; y : integer) return integer is
  begin
    if(x > y) then
      return x;
    else
      return y;
    end if;
  end function f_max;


  signal offset_sreg : unsigned(f_max(9, g_userval_extract_offset + 1) downto 0);
  signal is_802_1q   : std_logic;

  signal matches_dmac  : std_logic_vector(2 downto 0);
  signal matches_vid   : std_logic;
  signal matches_etype : std_logic;
  signal matches_user  : std_logic;
  
begin  -- behavioral

  main_fsm : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' or enable_i = '0' then
        done_o      <= '0';
        match_o     <= '0';
        offset_sreg <= (others => '0');
        is_802_1q   <= '0';
      else
        if (snk_sof_p1_i = '1') then
          offset_sreg(0) <= '1';
          done_o         <= '0';
          match_o        <= '0';
        elsif(offset_sreg /= 0) then

          if(snk_valid_i = '1') then

            if(offset_sreg(0) = '1') then
              matches_dmac(0) <= to_sl(snk_data_i = dmac_i(47 downto 32));
            end if;

            if(offset_sreg(1) = '1') then
              matches_dmac(1) <= to_sl(snk_data_i = dmac_i(31 downto 16));
            end if;

            if(offset_sreg(2) = '1') then
              matches_dmac(2) <= to_sl(snk_data_i = dmac_i(15 downto 0));
            end if;

            if(offset_sreg(6) = '1') then
              is_802_1q     <= to_sl(snk_data_i = x"8100");
              matches_etype <= to_sl(snk_data_i = etype_i);
            end if;

            if(offset_sreg(7) = '1' and is_802_1q = '1') then
              matches_etype <= to_sl(snk_data_i = etype_i);
            end if;

            if(offset_sreg(8) = '1') then
              if(is_802_1q = '1') then
                matches_vid <= to_sl(snk_data_i(11 downto 0) = vid_i);
              else
                matches_vid <= to_sl((vid_i = x"fff") or (vid_i = x"000"));
              end if;
            end if;

            if(g_userval_extract_enable) then
              if(offset_sreg(g_userval_extract_offset) = '1') then
                user_extracted_o <= snk_data_i;
              end if;
            end if;

            if(offset_sreg(offset_sreg'left) = '1') then
              done_o  <= '1';
              match_o <= f_eval_match(to_sl(matches_dmac = "111"), dmac_en_i)
                         and f_eval_match(matches_etype, etype_en_i)
                         and f_eval_match(matches_vid, vid_en_i)
                         and f_eval_match(matches_user, user_en_i);
              offset_sreg <= (others => '0');
            else
              offset_sreg <= offset_sreg sll 1;
            end if;
            
          end if;
        end if;
      end if;
    end if;
  end process;

end behavioral;
