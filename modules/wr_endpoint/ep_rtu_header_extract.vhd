library ieee;
use ieee.std_logic_1164.all;
use work.endpoint_private_pkg.all;


entity ep_rtu_header_extract is
  generic(
    g_with_rtu : boolean);
  port(
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    snk_fab_i  : in  t_ep_internal_fabric;
    snk_dreq_o : out std_logic;

    src_fab_o  : out t_ep_internal_fabric;
    src_dreq_i : in  std_logic;

    rtu_rq_o       : out t_ep_internal_rtu_request;
    rtu_full_i     : in  std_logic;
    rtu_rq_valid_o : out std_logic
    );

end ep_rtu_header_extract;

architecture rtl of ep_rtu_header_extract is

  signal hdr_offset : std_logic_vector(11 downto 0);
  signal in_packet  : std_logic;
  
  procedure f_extract_rtu(signal q         : out std_logic_vector;
                          signal fab       :     t_ep_internal_fabric;
                          signal at_offset :     std_logic) is
  begin
    if(at_offset = '1' and fab.dvalid = '1') then
      q <= fab.data;
    end if;
  end f_extract_rtu;

begin  -- rtl

  gen_with_rtu : if(g_with_rtu) generate

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

    p_gen_rtu_request : process(clk_sys_i)
    begin
      if rising_edge(clk_sys_i) then
        if rst_n_i = '0' then
          rtu_rq_o.smac     <= (others => '0');
          rtu_rq_o.dmac     <= (others => '0');
          rtu_rq_o.vid      <= (others => '0');
          rtu_rq_o.has_vid  <= '0';
          rtu_rq_o.prio     <= (others => '0');
          rtu_rq_o.has_prio <= '0';
          in_packet         <= '0';
        else
          if(snk_fab_i.sof = '1' and rtu_full_i = '0') then
            in_packet <= '1';
          end if;

          if(snk_fab_i.eof = '1' or snk_fab_i.error = '1') then
            in_packet <= '0';
          end if;

          f_extract_rtu(rtu_rq_o.dmac(47 downto 32), snk_fab_i, hdr_offset(0));
          f_extract_rtu(rtu_rq_o.dmac(31 downto 16), snk_fab_i, hdr_offset(1));
          f_extract_rtu(rtu_rq_o.dmac(15 downto 0), snk_fab_i, hdr_offset(2));
          f_extract_rtu(rtu_rq_o.smac(47 downto 32), snk_fab_i, hdr_offset(3));
          f_extract_rtu(rtu_rq_o.smac(31 downto 16), snk_fab_i, hdr_offset(4));
          f_extract_rtu(rtu_rq_o.smac(15 downto 0), snk_fab_i, hdr_offset(5));

          if(hdr_offset(5) = '1' and in_packet = '1') then
            rtu_rq_valid_o <= '1';
          else
            rtu_rq_valid_o <= '0';
          end if;
          
        end if;
      end if;
    end process;

    src_fab_o.sof <= snk_fab_i.sof and not rtu_full_i;

  end generate gen_with_rtu;

  gen_without_rtu : if (not g_with_rtu) generate
    src_fab_o.sof <= snk_fab_i.sof;
  end generate gen_without_rtu;

  snk_dreq_o <= src_dreq_i;

  src_fab_o.eof      <= snk_fab_i.eof;
  src_fab_o.dvalid   <= snk_fab_i.dvalid;
  src_fab_o.error    <= snk_fab_i.error;
  src_fab_o.bytesel  <= snk_fab_i.bytesel;
  src_fab_o.data     <= snk_fab_i.data;
  src_fab_o.addr     <= snk_fab_i.addr;
  src_fab_o.has_rx_timestamp <= snk_fab_i.has_rx_timestamp;
  
end rtl;
