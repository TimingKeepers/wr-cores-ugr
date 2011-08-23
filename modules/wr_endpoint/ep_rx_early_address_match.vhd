library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.gencores_pkg.all;              -- for gc_crc_gen
use work.endpoint_private_pkg.all;
use work.ep_wbgen2_pkg.all;

-- 1st stage in the RX pipeline: early address matching/header parsing
-- to filter out pause and HP frames in advance.

entity ep_rx_early_address_match is
  port(clk_sys_i : in std_logic;
       rst_n_i   : in std_logic;

       snk_fab_i  : in  t_ep_internal_fabric;
       snk_dreq_o : out std_logic;

       src_fab_o  : out t_ep_internal_fabric;
       src_dreq_i : in  std_logic;

       match_done_o         : out std_logic;
       match_is_hp_o        : out std_logic;
       match_is_pause_o     : out std_logic;
       match_pause_quanta_o : out std_logic_vector(15 downto 0);

       regs_b : inout t_ep_registers
       );

end ep_rx_early_address_match;

architecture behavioral of ep_rx_early_address_match is

  signal hdr_offset : std_logic_vector(11 downto 0);

  signal at_ethertype    : std_logic;
  signal at_vid          : std_logic;
  signal is_tagged       : std_logic;
  signal pause_match_int : std_logic_vector(7 downto 0);


  signal comb_pcp_matches_hp : std_logic;

  function f_compare_slv (a : std_logic_vector; b : std_logic_vector) return std_logic is
  begin
    if(a = b) then
      return '1';
    else
      return '0';
    end if;
  end f_compare_slv;

  signal q_in : std_logic_vector(20 downto 0);
  signal q_out : std_logic_vector(20 downto 0);

  signal q_in_valid : std_logic;
  signal q_out_valid : std_logic;

 
  
begin  -- behavioral

  at_ethertype <= hdr_offset(5) and snk_fab_i.dvalid and src_dreq_i;
  at_vid       <= hdr_offset(7) and snk_fab_i.dvalid and is_tagged;

  regs_b <= c_ep_registers_init_value;

  q_in <= snk_fab_i.dvalid & snk_fab_i.bytesel & snk_fab_i.sof & snk_fab_i.eof & snk_fab_i.error & snk_fab_i.data;
  q_in_valid <= snk_fab_i.eof or snk_fab_i.sof or snk_fab_i.error or snk_fab_i.dvalid;
    
  U_bypass_queue : ep_rx_bypass_queue
    generic map (
      g_size  => 16,
      g_width => 21)
    port map (
      rst_n_i => rst_n_i,
      clk_i   => clk_sys_i,
      d_i     => q_in,
      valid_i => q_in_valid,
      dreq_o  => snk_dreq_o,
      q_o     => q_out,
      valid_o => q_out_valid,
      dreq_i  => src_dreq_i,
      flush_i => snk_fab_i.eof,
      purge_i => '0');

  src_fab_o.dvalid <= q_out_valid and q_out(20);
  src_fab_o.bytesel <= q_out(19);
  src_fab_o.sof <= q_out_valid and q_out(18);
  src_fab_o.eof <= q_out_valid and q_out(17);
  src_fab_o.error <= q_out_valid and q_out(16);

  src_fab_o.data <= q_out(15 downto 0);
  
  p_hdr_offset_sreg : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if (rst_n_i = '0' or snk_fab_i.sof = '1') then
        hdr_offset(hdr_offset'left downto 1) <= (others => '0');
        hdr_offset(0)                        <= '1';
      elsif(snk_fab_i.dvalid = '1') then
        hdr_offset <= hdr_offset(hdr_offset'left-1 downto 0) & '0';
      end if;
    end if;
  end process;

  p_match_pause : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' or snk_fab_i.sof = '1' then
        pause_match_int      <= (others => '0');
        match_pause_quanta_o <= (others => '0');
        match_is_pause_o     <= '0';
      else
        if(snk_fab_i.dvalid = '1') then
          if(hdr_offset(0) = '1') then
            pause_match_int (0) <= f_compare_slv(snk_fab_i.data, x"0180");
          end if;
          if(hdr_offset(1) = '1') then
            pause_match_int (1) <= f_compare_slv(snk_fab_i.data, x"c200");
          end if;
          if(hdr_offset(2) = '1') then
            pause_match_int (2) <= f_compare_slv(snk_fab_i.data, x"0001");
          end if;
          if(hdr_offset(3) = '1') then
            pause_match_int (3) <= f_compare_slv(snk_fab_i.data, regs_b.mach_o);
          end if;
          if(hdr_offset(4) = '1') then
            pause_match_int (4) <= f_compare_slv(snk_fab_i.data, regs_b.macl_o(31 downto 16));
          end if;
          if(hdr_offset(5) = '1') then
            pause_match_int (5) <= f_compare_slv(snk_fab_i.data, regs_b.macl_o(15 downto 0));
          end if;
          if(hdr_offset(6) = '1') then
            pause_match_int (6) <= f_compare_slv(snk_fab_i.data, x"8808");
          end if;
          if(hdr_offset(7) = '1') then
            pause_match_int (7) <= f_compare_slv(snk_fab_i.data, x"0001");
          end if;
          if(hdr_offset(8) = '1') then
            match_is_pause_o     <= f_compare_slv(pause_match_int, x"ff");
            match_pause_quanta_o <= snk_fab_i.data;
          end if;
        end if;
      end if;
    end if;
  end process;

  p_match_hp : process(clk_sys_i)
    variable index : integer;
  begin
    
    if rising_edge(clk_sys_i) then
      index := to_integer(unsigned(snk_fab_i.data(15 downto 13)));

      if rst_n_i = '0' or snk_fab_i.sof = '1' then
        is_tagged     <= '0';
        match_is_hp_o <= '0';
      else
        if(at_ethertype = '1') then
          is_tagged <= f_compare_slv(snk_fab_i.data, x"8100");
        end if;

        if (at_vid = '1') then
          if(regs_b.rfcr_a_hp_o = '1' and regs_b.rfcr_hpap_o(index) = '1') then
            match_is_hp_o <= '1';
          else
            match_is_hp_o <= '0';
          end if;
        end if;
      end if;
    end if;
  end process;

  p_gen_done : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' or snk_fab_i.sof = '1' then
        match_done_o <= '0';
      else
        if hdr_offset(8) = '1' then
          match_done_o <= '1';
        end if;
      end if;
    end if;
  end process;



end behavioral;




