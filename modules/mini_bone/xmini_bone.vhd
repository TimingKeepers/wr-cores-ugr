library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wishbone_pkg.all;
use work.wr_fabric_pkg.all;

entity xmini_bone is
  generic(
    g_class_mask    : std_logic_vector(7 downto 0);
    g_our_ethertype : std_logic_vector(15 downto 0));
  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    -- WBP Master (TX)
    src_o : out t_wrf_source_out;
    src_i : in  t_wrf_source_in;

    -- WBP Slave (RX)
    snk_o : out t_wrf_sink_out;
    snk_i : in  t_wrf_sink_in;

    --
    master_o : out t_wishbone_master_out;
    master_i : in  t_wishbone_master_in
    );

end xmini_bone;

architecture behavioral of xmini_bone is

  type t_state is (IDLE, RX_STATUS, RX_DATA, RX_CHECK, RX_DROP, RX_WAIT_EOP, WB_ISSUE, TX_START_PACKET, TX_STATUS, TX_DATA, TX_FINISH_PACKET, TX_STALL);


  function f_sl(state : t_state) return std_logic_vector is
  begin
    case state is
      when IDLE             => return "0000";
      when RX_STATUS        => return "0001";
      when RX_DATA          => return "0010";
      when RX_CHECK         => return "0011";
      when RX_DROP          => return "0100";
      when RX_WAIT_EOP      => return "0101";
      when WB_ISSUE         => return "0110";
      when TX_START_PACKET  => return "1011";
      when TX_DATA          => return "0111";
      when TX_FINISH_PACKET => return "1000";
      when TX_STATUS        => return "1001";
      when TX_STALL         => return "1010";
    end case;

    return "0000";
  end f_sl;



  signal snk_out : t_wrf_sink_out;
  signal src_out : t_wrf_source_out;


  signal trans_addr  : std_logic_vector(31 downto 0);
  signal trans_wdata : std_logic_vector(31 downto 0);
  signal trans_rdata : std_logic_vector(31 downto 0);

  signal trans_rx_hdr : std_logic_vector(15 downto 0);

  alias trans_sel is trans_rx_hdr(3 downto 0);
  alias trans_we is trans_rx_hdr(4);

  signal trans_tx_hdr : std_logic_vector(15 downto 0);

  alias trans_readback is trans_tx_hdr(0);
  alias trans_error is trans_tx_hdr(1);

  signal rx_remote_mac : std_logic_vector(47 downto 0);
  signal rx_ethertype  : std_logic_vector(15 downto 0);

  signal state      : t_state;
  signal snk_offset : std_logic_vector(31 downto 0);
  signal src_offset : std_logic_vector(31 downto 0);

  signal d_valid    : std_logic;
  signal d_status   : std_logic;
  signal d_rx_error : std_logic;

  signal decoded_status : t_wrf_status_reg;

  signal stall_int : std_logic;

  signal src_ack_count : unsigned(4 downto 0);

  signal s_zeroes        : std_logic_vector(15 downto 0) := x"0000";
  signal s_our_ethertype : std_logic_vector(15 downto 0) := g_our_ethertype;


  function f_sl(x : boolean) return std_logic is
  begin
    if(x) then
      return '1';
    else
      return '0';
    end if;
  end f_sl;

  procedure f_extract_rx(signal q       : out std_logic_vector;
                         signal d       : in  std_logic_vector;
                         signal d_valid : in  std_logic;
                         offset         :     integer) is
  begin

    if(snk_offset(offset) = '1' and d_valid = '1') then
      q <= d;
    end if;
  end f_extract_rx;


  signal tx_ser_counter : unsigned(6 downto 0);
  
  procedure f_serialize_tx(
    signal q : out std_logic_vector;
    signal x : in  std_logic_vector;
    offset   :     integer;
    cnt: unsigned) is
  begin
    if (offset >= 0 and cnt = to_unsigned(offset, cnt'length)) then
      q <= x;
    end if;
  end f_serialize_tx;

  component chipscope_ila
    port (
      CONTROL : inout std_logic_vector(35 downto 0);
      CLK     : in    std_logic;
      TRIG0   : in    std_logic_vector(31 downto 0);
      TRIG1   : in    std_logic_vector(31 downto 0);
      TRIG2   : in    std_logic_vector(31 downto 0);
      TRIG3   : in    std_logic_vector(31 downto 0));
  end component;

  component chipscope_icon
    port (
      CONTROL0 : inout std_logic_vector (35 downto 0));
  end component;


  signal CONTROL : std_logic_vector(35 downto 0);
  signal CLK     : std_logic;
  signal TRIG0   : std_logic_vector(31 downto 0);
  signal TRIG1   : std_logic_vector(31 downto 0);
  signal TRIG2   : std_logic_vector(31 downto 0);
  signal TRIG3   : std_logic_vector(31 downto 0);


begin  -- behavioral

  trans_tx_hdr(15 downto 2) <= (others => '0');

  decoded_status <= f_unmarshall_wrf_status(snk_i.dat);

  d_valid    <= '1' when (snk_i.cyc and snk_i.we and snk_i.stb) = '1' and (snk_i.adr = c_WRF_DATA)   else '0';
  d_status   <= '1' when (snk_i.cyc and snk_i.we and snk_i.stb) = '1' and (snk_i.adr = c_WRF_STATUS) else '0';
  d_rx_error <= d_status and decoded_status.error;


  p_snk_gen_ack : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        snk_out.ack <= '0';
      else
        snk_out.ack <= snk_i.stb and snk_i.cyc and snk_i.we and not snk_out.stall;
      end if;
    end if;
  end process;

  p_offset_sreg : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        snk_offset <= (0 => '1', others => '0');
        src_offset <= (0 => '1', others => '0');
      else
        if(snk_i.cyc = '0') then
          snk_offset(0)                        <= '1';
          snk_offset(snk_offset'left downto 1) <= (others => '0');
        elsif(d_valid = '1') then
          snk_offset <= snk_offset(snk_offset'left-1 downto 0) & '0';
        end if;

        if(src_out.cyc = '0') then
          src_offset <= (0 => '1', others => '0');
        elsif(src_i.stall = '0' and src_out.stb = '1') then
          src_offset <= src_offset(src_offset'left-1 downto 0) & '0';
        end if;
        
      end if;
    end if;
  end process;

  p_src_count_acks : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        src_ack_count <= (others => '0');
      else
        if(src_out.cyc = '0') then
          src_ack_count <= (others => '0');
        elsif(src_i.ack = '1') then
          src_ack_count <= src_ack_count + 1;
        end if;
      end if;
    end if;
    
  end process;



  p_fsm : process(clk_sys_i)
    variable tx_offset : integer;
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        state     <= IDLE;
        stall_int <= '0';

        master_o.cyc <= '0';
        master_o.stb <= '0';

        src_out.cyc <= '0';
        src_out.stb <= '0';
        src_out.we  <= '1';
        src_out.sel <= "11";
        src_out.adr <= (others => '0');
        src_out.dat <= (others => '0');

        trans_readback <= '0';
        trans_we       <= '0';
        trans_rdata    <= (others => '0');
        trans_wdata    <= (others => '0');
        trans_addr     <= (others => '0');
        trans_sel      <= (others => '0');
        trans_error    <= '0';

        rx_remote_mac <= (others => '0');
        rx_ethertype  <= (others => '0');

        tx_ser_counter <= (others => '0');
        
        
      else

        case state is
          when IDLE =>
            stall_int <= '0';
            tx_ser_counter <= (others => '0');

            if(snk_i.cyc = '1') then
              state <= RX_DATA;--STATUS;
            end if;

            --fixme: RX status decoding in WBP mux is broken

          --when RX_STATUS =>
          --  if(d_rx_error = '1' or snk_i.cyc = '0') then
          --    state <= RX_DROP;
          --  elsif(d_status = '1') then
          --    if((decoded_status.match_class and g_class_mask) /= x"00") then
          --      state <= RX_DATA;       -- rep
          --    else
          --      state <= RX_DATA;
          --    end if;
          --  end if;

          when RX_DATA =>

            f_extract_rx(rx_remote_mac(47 downto 32), snk_i.dat, d_valid, 3);
            f_extract_rx(rx_remote_mac(31 downto 16), snk_i.dat, d_valid, 4);
            f_extract_rx(rx_remote_mac(15 downto 0), snk_i.dat, d_valid, 5);
            f_extract_rx(rx_ethertype, snk_i.dat, d_valid, 6);
            f_extract_rx(trans_rx_hdr, snk_i.dat, d_valid, 7);
            f_extract_rx(trans_addr(31 downto 16), snk_i.dat, d_valid, 8);
            f_extract_rx(trans_addr(15 downto 0), snk_i.dat, d_valid, 9);
            f_extract_rx(trans_wdata(31 downto 16), snk_i.dat, d_valid, 10);
            f_extract_rx(trans_wdata(15 downto 0), snk_i.dat, d_valid, 11);

            if(d_rx_error = '1' or snk_i.cyc = '0') then
              state <= RX_DROP;
            elsif(d_valid = '1' and snk_offset(12) = '1') then
              state <= RX_WAIT_EOP;
            end if;

          when RX_WAIT_EOP =>
            if(d_rx_error = '1') then
              state <= RX_DROP;
            elsif(snk_i.cyc = '0') then  -- end-of-packet?
              stall_int <= '1';  -- stop reception while the cycle is issued
              state     <= RX_CHECK;
            end if;

          when RX_CHECK =>
            if(rx_ethertype = g_our_ethertype) then
              state <= WB_ISSUE;
            else
              state <= RX_DROP;
            end if;

          when WB_ISSUE =>
            if(master_i.ack = '0') then
              master_o.cyc <= '1';
              master_o.stb <= '1';
            else
              master_o.cyc <= '0';
              master_o.stb <= '0';
              trans_rdata  <= master_i.dat;
              state        <= TX_START_PACKET;
            end if;

            trans_readback <= not trans_we;
            trans_error    <= '0';


          when TX_START_PACKET =>

            if(src_i.stall = '0') then
              state       <= TX_STATUS;
              src_out.stb <= '1';
              src_out.cyc <= '1';
              src_out.stb <= '1';
              src_out.adr <= c_WRF_STATUS;
              src_out.dat <= (others => '0');
            else
              src_out.stb <= '0';
              src_out.cyc <= '0';
            end if;


          when TX_STATUS =>

            if(src_i.stall = '0') then
              src_out.stb <= '1';
              src_out.adr <= c_WRF_DATA;
              state       <= TX_DATA;
              f_serialize_tx(src_out.dat, rx_remote_mac(47 downto 32), 0, tx_ser_counter);
            end if;


          when TX_DATA =>
            src_out.stb <= '1';

            if(src_i.stall = '1')then
              tx_offset := 0;
            else
              tx_offset := -1;
            end if;

            f_serialize_tx(src_out.dat, rx_remote_mac(47 downto 32), tx_offset + 0, tx_ser_counter);
            f_serialize_tx(src_out.dat, rx_remote_mac(31 downto 16), tx_offset + 1, tx_ser_counter);
            f_serialize_tx(src_out.dat, rx_remote_mac(15 downto 0), tx_offset + 2, tx_ser_counter);  -- DST MAC
            f_serialize_tx(src_out.dat, s_zeroes, tx_offset + 3, tx_ser_counter);
            f_serialize_tx(src_out.dat, s_zeroes, tx_offset + 4, tx_ser_counter);
            f_serialize_tx(src_out.dat, s_zeroes, tx_offset + 5, tx_ser_counter);
            f_serialize_tx(src_out.dat, s_our_ethertype, tx_offset + 6, tx_ser_counter);
            f_serialize_tx(src_out.dat, trans_tx_hdr, tx_offset + 7, tx_ser_counter);
            f_serialize_tx(src_out.dat, trans_rdata(31 downto 16), tx_offset + 8, tx_ser_counter);
            f_serialize_tx(src_out.dat, trans_rdata(15 downto 0), tx_offset + 9, tx_ser_counter);

            if(src_i.stall = '0') then
              tx_ser_counter <= tx_ser_counter + 1;
            end if;

            if(tx_ser_counter = 30 and src_i.stall = '0') then
              state       <= TX_FINISH_PACKET;
              src_out.stb <= '0';
            end if;
            

          when TX_FINISH_PACKET =>
            if(src_ack_count = 31) then
              src_out.stb <= '0';
              src_out.cyc <= '0';
              state       <= IDLE;
            end if;

          when RX_DROP =>
            if(snk_i.cyc = '0') then
              state <= IDLE;
            end if;
          when others => null;
        end case;
      end if;
    end if;
  end process;

  master_o.dat <= trans_wdata;
  master_o.adr <= trans_addr;
  master_o.we  <= trans_we;
  master_o.sel <= trans_sel;


  snk_o <= snk_out;

  snk_out.err   <= '0';
  snk_out.rty   <= '0';
  snk_out.stall <= stall_int or (snk_i.cyc and snk_i.stb and f_sl(state = IDLE));

  src_o <= src_out;


  chipscope_ila_1 : chipscope_ila
    port map (
      CONTROL => CONTROL,
      CLK     => clk_sys_i,
      TRIG0   => TRIG0,
      TRIG1   => TRIG1,
      TRIG2   => TRIG2,
      TRIG3   => TRIG3);

  chipscope_icon_1 : chipscope_icon
    port map (
      CONTROL0 => CONTROL);

  TRIG0(15 downto 0)  <= src_out.dat;
  trig0(17 downto 16) <= src_out.adr;
  trig0(19 downto 18) <= src_out.sel;
  trig0(20)           <= src_out.cyc;
  trig0(21)           <= src_out.stb;
  trig0(22)           <= src_out.we;
  trig0(23)           <= src_i.ack;
  trig0(24)           <= src_i.stall;
  trig0(26)           <= src_i.err;

  trig1(4) <= d_valid;
  trig1(5) <= d_status;
  trig1(6) <= d_rx_error;

  trig1(3 downto 0)   <= f_sl(state);
  trig1(31 downto 16) <= rx_ethertype;

  trig3 <= snk_offset;

  TRIG2(15 downto 0)  <= snk_i.dat;
  trig2(17 downto 16) <= snk_i.adr;
  trig2(19 downto 18) <= snk_i.sel;
  trig2(20)           <= snk_i.cyc;
  trig2(21)           <= snk_i.stb;
  trig2(22)           <= snk_i.we;
  trig2(23)           <= snk_out.ack;
  trig2(24)           <= snk_out.stall;
  trig2(26)           <= snk_out.err;

  

end behavioral;
