library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.endpoint_private_pkg.all;
use work.wr_fabric_pkg.all;

entity ep_rx_status_reg_insert is
  
  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    snk_fab_i  : in  t_ep_internal_fabric;
    snk_dreq_o : out std_logic;

    src_fab_o  : out t_ep_internal_fabric;
    src_dreq_i : in  std_logic;

    pfilter_drop_i   : in std_logic;
    pfilter_pclass_i : in std_logic_vector(7 downto 0);
    pfilter_done_i   : in std_logic;

    ematch_done_i     : in std_logic;
    ematch_is_hp_i    : in std_logic;
    ematch_is_pause_i : in std_logic;

    rmon_o : out t_rmon_triggers
    );

end ep_rx_status_reg_insert;

architecture rtl of ep_rx_status_reg_insert is

  type t_state is (WAIT_FRAME, GEN_STATUS);

  signal dreq_mask    : std_logic;
  signal embed_status : std_logic;
  signal sreg         : t_wrf_status_reg;
  signal state : t_state;
  signal src_fab_out : t_ep_internal_fabric;
  

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

  signal sof_mask : std_logic;
  
begin  -- rtl

  -- chipscope_icon_1 : chipscope_icon
  --  port map (
  --    CONTROL0 => CONTROL);

  --chipscope_ila_1 : chipscope_ila
  --  port map (
  --    CONTROL => CONTROL,
  --    CLK     => clk_sys_i,
  --    TRIG0   => TRIG0,
  --    TRIG1   => TRIG1,
  --    TRIG2   => TRIG2,
  --    TRIG3   => TRIG3);

  -- trig3(1)<=src_fab_out.dvalid;
  -- trig3(2)<=src_fab_out.sof;
  -- trig3(3)<=src_fab_out.eof;
  -- trig3(4)<=src_fab_out.error;
  -- trig3(31 downto 16) <= src_fab_out.data;
  -- trig3(5) <= embed_status;
  -- trig3(7 downto 6) <= src_fab_out.addr;
  -- trig3(8) <= pfilter_done_i;
  -- trig3(9) <= ematch_done_i;
  -- trig3(10) <=pfilter_drop_i;
  -- trig3(11)<=ematch_is_pause_i;
  -- trig3(12) <= force_error;
  
   
  embed_status <= '1' when (state = GEN_STATUS ) else '0';
  snk_dreq_o     <= src_dreq_i and dreq_mask;
  src_fab_out.data <= f_marshall_wrf_status(sreg) when (embed_status = '1') else snk_fab_i.data;
  src_fab_out.addr <= c_WRF_STATUS                when (embed_status = '1') else snk_fab_i.addr;

  src_fab_out.sof    <= snk_fab_i.sof and sof_mask;
  src_fab_out.eof    <= snk_fab_i.eof;
  src_fab_out.error  <= snk_fab_i.error;
  src_fab_out.bytesel <= snk_fab_i.bytesel;
  src_fab_out.dvalid <= snk_fab_i.dvalid or (embed_status and src_dreq_i);

  src_fab_o <= src_fab_out;

  sof_mask <= (pfilter_done_i and ematch_done_i and not (pfilter_drop_i or ematch_is_pause_i));
  
  p_gen_status : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        state       <= WAIT_FRAME;
        dreq_mask   <= '1';
      else
        case state is
          when WAIT_FRAME =>
            if(snk_fab_i.sof = '1') then
              if(pfilter_done_i = '0' or ematch_done_i = '0') then
                rmon_o.rx_path_timing_failure <= '1';
              else
                rmon_o.rx_pause        <= ematch_is_pause_i;
                rmon_o.rx_pfilter_drop <= pfilter_drop_i;

                if(pfilter_drop_i = '0' and ematch_is_pause_i = '0') then
                  state     <= GEN_STATUS;
                  dreq_mask <= '0';
                end if;

                sreg.match_class <= pfilter_pclass_i;
                sreg.is_hp       <= ematch_is_hp_i;
                sreg.has_crc     <= '1';
                sreg.has_smac    <= '1';
                sreg.error       <= '0';
              end if;
            else
              rmon_o.rx_pfilter_drop        <= '0';
              rmon_o.rx_pause               <= '0';
              rmon_o.rx_path_timing_failure <= '0';
              dreq_mask                     <= '1';
            end if;
            

          when GEN_STATUS =>
            if(src_dreq_i = '1') then
              state     <= WAIT_FRAME;
              dreq_mask <= '1';
            end if;

        end case;

      end if;
    end if;
  end process;


end rtl;
