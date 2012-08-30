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

    mbuf_valid_i    : in  std_logic;
    mbuf_ack_o      : out std_logic;
    mbuf_drop_i     : in  std_logic;
    mbuf_pclass_i   : in  std_logic_vector(7 downto 0);
    mbuf_is_hp_i    : in  std_logic;
    mbuf_is_pause_i : in  std_logic;

    rmon_o : out t_rmon_triggers
    );

end ep_rx_status_reg_insert;

architecture rtl of ep_rx_status_reg_insert is

  type t_state is (WAIT_FRAME, WAIT_MBUF, GEN_STATUS);

  signal dreq_mask    : std_logic;
  signal embed_status : std_logic;
  signal sreg         : t_wrf_status_reg;
  signal state        : t_state;
  signal src_fab_out  : t_ep_internal_fabric;

  signal sof_mask : std_logic;
  
begin  -- rtl
  
  embed_status     <= '1'                         when (state = GEN_STATUS) else '0';
  src_fab_out.data <= f_marshall_wrf_status(sreg) when (embed_status = '1') else snk_fab_i.data;
  src_fab_out.addr <= c_WRF_STATUS                when (embed_status = '1') else snk_fab_i.addr;

  src_fab_out.eof     <= snk_fab_i.eof;
  src_fab_out.error   <= snk_fab_i.error;
  src_fab_out.bytesel <= snk_fab_i.bytesel;
  src_fab_out.dvalid  <= snk_fab_i.dvalid or (embed_status and src_dreq_i);

  src_fab_o <= src_fab_out;

  src_fab_out.sof <= '1' when (mbuf_valid_i = '1' and state = WAIT_MBUF and mbuf_drop_i = '0' and mbuf_is_pause_i = '0') else '0';
  mbuf_ack_o      <= '1' when (mbuf_valid_i = '1' and state = WAIT_MBUF)                                                 else '0';

  snk_dreq_o <= src_dreq_i and dreq_mask and not snk_fab_i.sof;

  p_gen_status : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        state     <= WAIT_FRAME;
        dreq_mask <= '1';
      else
        case state is
          when WAIT_FRAME =>
            if(snk_fab_i.sof = '1') then
              state     <= WAIT_MBUF;
              dreq_mask <= '0';
            end if;
            
          when WAIT_MBUF =>
            if(mbuf_valid_i = '1') then
                rmon_o.rx_pause        <= mbuf_is_pause_i;
                rmon_o.rx_pfilter_drop <= mbuf_drop_i;

                if(mbuf_drop_i = '0' and mbuf_is_pause_i = '0') then
                  state <= GEN_STATUS;
                else
                  state <= WAIT_FRAME;
                  dreq_mask <= '1';
                end if;

                sreg.match_class <= mbuf_pclass_i;
                sreg.is_hp       <= mbuf_is_hp_i;
                sreg.has_crc     <= '0';
                sreg.has_smac    <= '1';
                sreg.error       <= '0';
            else
              rmon_o.rx_pfilter_drop        <= '0';
              rmon_o.rx_pause               <= '0';
              rmon_o.rx_path_timing_failure <= '0';
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
