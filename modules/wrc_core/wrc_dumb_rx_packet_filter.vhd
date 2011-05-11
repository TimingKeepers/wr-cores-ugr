
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.endpoint_pkg.all;

entity wrc_dumb_rx_packet_filter is
  
  generic (
    g_match_etype0 : std_logic_vector(15 downto 0) := x"0000";
    g_match_etype1 : std_logic_vector(15 downto 0) := x"0000";
    g_match_etype2 : std_logic_vector(15 downto 0) := x"0000";
    g_match_etype3 : std_logic_vector(15 downto 0) := x"0000"
    );
  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    src_dat_o   : out std_logic_vector(15 downto 0);
    src_adr_o   : out std_logic_vector(1 downto 0);
    src_sel_o   : out std_logic_vector(1 downto 0);
    src_cyc_o   : out std_logic;
    src_stb_o   : out std_logic;
    src_we_o    : out std_logic;
    src_stall_i : in  std_logic;
    src_ack_i   : in  std_logic;

    snk_dat_i   : in  std_logic_vector(15 downto 0);
    snk_adr_i   : in  std_logic_vector(1 downto 0);
    snk_sel_i   : in  std_logic_vector(1 downto 0);
    snk_cyc_i   : in  std_logic;
    snk_stb_i   : in  std_logic;
    snk_we_i    : in  std_logic;
    snk_stall_o : out std_logic;
    snk_ack_o   : out std_logic
    );

end wrc_dumb_rx_packet_filter;

architecture behavioral of wrc_dumb_rx_packet_filter is

  type t_wrf_xfer is record
    d         : std_logic_vector(15 downto 0);
    a         : std_logic_vector(1 downto 0);
    sel       : std_logic_vector(1 downto 0);
    end_cycle : std_logic;
    valid     : std_logic;
  end record;

  constant c_sreg_size : integer := 8;

  type t_wrf_sreg is array(c_sreg_size-1 downto 0) of t_wrf_xfer;

  signal sreg              : t_wrf_sreg;
  signal ethertype_counter : unsigned(9 downto 0);

  type t_state is (S_WAIT_CYCLE, S_DATA, S_TERM_CYCLE, S_WAIT_ACKS);
  signal state     : t_state;
  signal stall_src : std_logic;

  signal ack_counter : unsigned(3 downto 0);

  signal src_stb_int : std_logic;
  signal src_cyc_int : std_logic;

  function f_classify_etype(etype : std_logic_vector) return std_logic_vector is
  begin

    if(etype = g_match_etype0) then
      return "00000001";
    end if;
    if(etype = g_match_etype1) then
      return "00000010";
    end if;
    if(etype = g_match_etype2) then
      return "00000100";
    end if;
    if(etype = g_match_etype3) then
      return "00001000";
    end if;

    return "00000000";
  end function;
  
  
begin  -- behavioral

  snk_stall_o <= src_stall_i or stall_src;

  process(clk_sys_i)
    variable tmp : t_wrf_status_reg;
    
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0') then
        for i in 0 to 7 loop
          sreg(i).end_cycle <= '0';
        end loop;  -- i in 0 to 7
        ethertype_counter <= to_unsigned(1, ethertype_counter'length);
        snk_ack_o         <= '0';

        src_cyc_int <= '0';
        src_stb_int <= '0';
        stall_src   <= '0';
        src_dat_o   <= (others => '0');
        src_sel_o   <= (others => '0');
        src_adr_o   <= (others => '0');
      else
        
        if(src_cyc_int = '0') then
          ack_counter <= (others => '0');
        else
          if(src_stb_int = '1' and src_ack_i = '0') then
            ack_counter <= ack_counter + 1;
          elsif(src_stb_int = '0' and src_ack_i = '1') then
            ack_counter <= ack_counter - 1;
          end if;
        end if;


        case state is
          when S_WAIT_CYCLE =>

            stall_src <= '0';

            if(snk_cyc_i = '1') then    -- beginning of a new frame
              if(snk_stb_i = '1') then
                sreg(0).a         <= snk_adr_i;
                sreg(0).d         <= snk_dat_i;
                sreg(0).sel       <= snk_sel_i;
                sreg(0).end_cycle <= '0';
                sreg(0).valid     <= '1';

                sreg(c_sreg_size-1 downto 1) <= sreg(c_sreg_size-2 downto 0);
                if(snk_adr_i = c_WRF_DATA) then
                  ethertype_counter <= ethertype_counter sll 1;
                end if;
                snk_ack_o <= '1';
                state     <= S_DATA;
              else
                stall_src         <= '0';
                snk_ack_o         <= '0';
                ethertype_counter <= to_unsigned(1, ethertype_counter'length);
              end if;
              src_cyc_int <= '1';
            else
              for i in 0 to 7 loop
                sreg(i).end_cycle <= '0';
                sreg(i).valid     <= '0';
              end loop;  -- i in 0 to 7
              src_cyc_int <= '0';
            end if;

          when S_DATA =>
            if(snk_cyc_i = '0') then
              state             <= S_TERM_CYCLE;
              stall_src         <= '1';
              sreg(0).end_cycle <= '1';
-- sreg(0).valid <= '0';
--              sreg(c_sreg_size-1 downto 1) <= sreg(c_sreg_size-1-1 downto 0);
            elsif(snk_stb_i = '1') then
              sreg(0).a         <= snk_adr_i;
              sreg(0).d         <= snk_dat_i;
              sreg(0).sel       <= snk_sel_i;
              sreg(0).end_cycle <= '0';
              sreg(0).valid     <= '1';


              sreg(c_sreg_size-1 downto 1) <= sreg(c_sreg_size-1-1 downto 0);

              if(ethertype_counter(6) = '1') then
                tmp.match_class   := f_classify_etype(snk_dat_i);
                tmp.rx_error      := '0';
                tmp.is_hp         := '0';
                sreg(7).valid     <= '1';
                sreg(7).a         <= c_WRF_STATUS;
                sreg(7).d         <= f_marshall_wrf_status(tmp);
                sreg(7).end_cycle <= '0';
                sreg(7).sel       <= "11";
              end if;


              src_stb_int <= sreg(c_sreg_size-1).valid;

              src_dat_o <= sreg(c_sreg_size-1).d;
              src_adr_o <= sreg(c_sreg_size-1).a;
              src_sel_o <= sreg(c_sreg_size-1).sel;

              if(snk_adr_i = c_WRF_DATA) then
                ethertype_counter <= ethertype_counter sll 1;
              end if;
              snk_ack_o <= '1';
            else
              src_stb_int <= '0';
              snk_ack_o   <= '0';
            end if;

          when S_TERM_CYCLE =>
            if(src_stall_i = '0') then
              if(sreg(c_sreg_size-1).end_cycle = '1') then
                state <= S_WAIT_ACKS;
              end if;
              src_dat_o                    <= sreg(c_sreg_size-1).d;
              src_adr_o                    <= sreg(c_sreg_size-1).a;
              src_sel_o                    <= sreg(c_sreg_size-1).sel;
              src_stb_int                  <= sreg(c_sreg_size-1).valid;
              sreg(c_sreg_size-1 downto 1) <= sreg(c_sreg_size-1-1 downto 0);
            else
              src_stb_int <= '0';
            end if;

          when S_WAIT_ACKS =>
            src_stb_int <= '0';
            if(ack_counter = 0) then
              src_cyc_int <= '0';
              state       <= S_WAIT_CYCLE;
            end if;
            
        end case;
      end if;
    end if;
  end process;

  src_cyc_o <= src_cyc_int;
  src_stb_o <= src_stb_int;
  src_we_o  <= '1';
  

end behavioral;

