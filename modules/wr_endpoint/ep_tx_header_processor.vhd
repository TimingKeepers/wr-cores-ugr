library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.genram_pkg.all;
use work.endpoint_private_pkg.all;
use work.ep_wbgen2_pkg.all;


entity ep_tx_header_processor is
  
  generic (
    g_with_vlans : boolean);
  port(
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    sof_p1_i      : in std_logic;
    has_mac_i     : in std_logic;
    is_pause_i    : in std_logic;
    pause_delay_i : in std_logic_vector(15 downto 0);

    en_i :in std_logic;
    d_i         : in std_logic_vector(15 downto 0);


    q_o        : out std_logic_vector(15 downto 0);
    valid_o    : out std_logic;
    done_o     : out std_logic;
    send_pad_o : out std_logic;
    stall_o : out std_logic;

    regs_b : inout t_ep_registers
    );

end ep_tx_header_processor;

architecture behavioral of ep_tx_header_processor is

  signal vut_rd_vid     : integer;
  signal vut_wr_vid     : integer;
  signal vut_untag      : std_logic;
  signal vut_is_802_1q  : std_logic;
  signal prev_word0     : std_logic_vector(15 downto 0);
  signal prev_word1     : std_logic_vector(15 downto 0);
  signal prev_word2     : std_logic_vector(15 downto 0);
  signal comb_is_802_1q : std_logic;
  signal offset : unsigned(3 downto 0);
  
signal stall_int : std_logic;
  
  
begin  -- behavioral

  regs_b <= c_ep_registers_init_value;
  
  p_process_packet_header : process(clk_sys_i)
    begin

      if rising_edge(clk_sys_i) then
        if rst_n_i = '0' then
          valid_o    <= '0';
          q_o        <= (others => 'X');
          done_o     <= '0';
          send_pad_o <= '0';
          stall_int <= '0';
        else
          if(is_pause_i = '1') then
            case offset_i is
                when x"0" =>
                  q_o <= x"0180";
                when x"1"=>
          q_o <= x"c200";
        when x"2" =>
          q_o <= x"0001";
        when x"3" =>
          q_o <= regs_b.mach_o;
        when x"4" =>
          q_o <= regs_b.macl_o(31 downto 16);
        when x"5" =>
          q_o <= regs_b.macl_o(15 downto 0);
        when x"6" =>
          q_o <= x"8808";
        when x"7" =>
          q_o        <= pause_delay_i;
          done_o     <= '1';
          send_pad_o <= '1';
        when others => null;
      end case;
        end if;
    else
      
      case offset_i is
        when x"0" =>
          q_o     <= d_i;
          valid_o <= en_i;
        when x"1" =>
          q_o     <= d_i;
          valid_o <= en_i;
        when x"2" =>
          q_o     <= d_i;
          valid_o <= en_i;
        when x"3" =>
          if(has_mac_i = '0') then
            q_o <= regs_b.mach_o;
          else
            q_o <= d_i;
          end if;
          valid_o <= en_i;
          
        when x"4" =>
          if(has_mac_i = '0') then
            q_o <= regs_b.macl_o(31 downto 16);
          else
            q_o <= d_i;
          end if;
          valid_o <= en_i;

        when x"5" =>
          if(has_mac_i = '0') then
            q_o <= regs_b.macl_o(15 downto 0);
          else
            q_o <= d_i;
          end if;
          valid_o <= en_i;

          -- no VLAN tagging support - just treat the eventual 802.1q header as
          -- a payload
          if (not g_with_vlans) then
            done_o <= '1';
          end if;

        when x"6" =>
          
          if(d_i = x"8100" and g_with_vlans = true) then
            comb_is_802_1q <= '1';
            valid_o        <= '0';
          else
            q_o <= d_i;
            valid_o <= en_i;
            done_o  <= '1';
          end if;
          
        when x"7" =>                    -- prev0 = 0x8100,
          valid_o <= '0';
        when x"8" =>                    -- prev0 = TPID, prev1 = 0x8100
          valid_o <= '0';
        when x"9" =>  -- prev0 = VID, prev1 = TPID, prev2 = 0x8100
          if(vut_untag = '1') then
            stall_int <= '1';
            done_o  <= '0';
            valid_o <= '0';
          else
            stall_int <= '1';
            valid_o <= '1';
            q_o     <= prev_word1;      -- output the TPID as ethertype
          end if;

        when x"a" =>
          stall_int <= '1';
          q_o     <= prev_word1;
          valid_o <= '1';

        when x"b" =>
          stall_int <= '0';
          q_o     <= d_i;
          valid_o <= '1';
          done_o <= '1';
          
        when others => null;
      end case;
    end if;

        end if;
      end if;

  end process;


  gen_with_vlans : if(g_with_vlans) generate

    vut_rd_vid <= to_integer(unsigned(d_i(11 downto 0)));
    vut_wr_vid <= to_integer(unsigned(regs_b.vcr1_vid_o));

    p_untagged_set_access : process(clk_sys_i)
      variable vut_set : std_logic_vector(4095 downto 0);
    begin
      if rising_edge(clk_sys_i) then
        if(regs_b.vcr1_value_wr_o = '1') then
          vut_set(vut_wr_vid) := regs_b.vcr1_value_o;
        end if;
--vut_untag <= vut_set(vut_rd_vid);
vut_untag <= '1';
      end if;
    end process;

    p_latch_and_delay: process(clk_sys_i)
    begin
      if rising_edge(clk_sys_i) then
        if(valid_i ='1') then
          prev_word0 <= d_i;
          prev_word1 <= prev_word0;
          prev_word2 <= prev_word1;
        end if;

        vut_is_802_1q <= comb_is_802_1q;
      end if;
    end process;

  end generate gen_with_vlans;

  stall_o <= stall_int;

end behavioral;
