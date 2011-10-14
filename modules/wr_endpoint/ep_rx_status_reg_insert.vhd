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
  signal force_error  : std_logic;
  signal embed_status : std_logic;
  signal sreg         : t_wrf_status_reg;
  signal state : t_state;

begin  -- rtl

  embed_status <= '1' when (state = GEN_STATUS ) else '0';
  snk_dreq_o     <= src_dreq_i and dreq_mask;
  src_fab_o.data <= f_marshall_wrf_status(sreg) when (embed_status = '1') else snk_fab_i.data;
  src_fab_o.addr <= c_WRF_STATUS                when (embed_status = '1') else snk_fab_i.addr;

  src_fab_o.sof    <= snk_fab_i.sof;
  src_fab_o.eof    <= snk_fab_i.eof;
  src_fab_o.error  <= snk_fab_i.error or force_error;
  src_fab_o.bytesel <= snk_fab_i.bytesel;
  src_fab_o.dvalid <= snk_fab_i.dvalid or (embed_status and src_dreq_i);

  p_gen_status : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        state       <= WAIT_FRAME;
        force_error <= '0';
        dreq_mask   <= '1';
      else
        case state is
          when WAIT_FRAME =>
            if(snk_fab_i.sof = '1') then
              if(pfilter_done_i = '0' or ematch_done_i = '0') then
                force_error                   <= '1';
                rmon_o.rx_path_timing_failure <= '1';
              else
                force_error            <= pfilter_drop_i or ematch_is_pause_i;
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
              force_error                   <= '0';
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
