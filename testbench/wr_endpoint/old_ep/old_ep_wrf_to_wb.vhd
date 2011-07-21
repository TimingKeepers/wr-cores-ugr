library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.global_defs.all;
use work.endpoint_pkg.all;

entity ep_wrf_to_wb is

  port(
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

-- WRF sink
    snk_data_i     : in  std_logic_vector(15 downto 0);
    snk_ctrl_i     : in  std_logic_vector(c_wrsw_ctrl_size-1 downto 0);
    snk_bytesel_i  : in  std_logic;
    snk_dreq_o     : out std_logic;
    snk_valid_i    : in  std_logic;
    snk_sof_p1_i   : in  std_logic;
    snk_eof_p1_i   : in  std_logic;
    snk_error_p1_i : in  std_logic;

-- Pipelined Wishbone master
    wb_dat_o   : out std_logic_vector(15 downto 0);
    wb_adr_o   : out std_logic_vector(1 downto 0);
    wb_sel_o   : out std_logic_vector(1 downto 0);
    wb_cyc_o   : out std_logic;
    wb_stb_o   : out std_logic;
    wb_we_o    : out std_logic;
    wb_stall_i : in  std_logic;
    wb_ack_i   : in  std_logic);

end ep_wrf_to_wb;

architecture behavioral of ep_wrf_to_wb is

  function to_sl(x : boolean) return std_logic is
  begin
    if(x = true) then
      return '1';
    else
      return '0';
    end if;
  end function to_sl;

  type t_arb_state is (ST_WAIT_FRAME, ST_DATA, ST_RX_ERROR, ST_WAIT_END_CYCLE);


  signal is_data : std_logic;
  signal is_oob  : std_logic;

  signal term_cycle : std_logic;

  signal ack_counter : unsigned(3 downto 0);
  signal wb_stb_int  : std_logic;
  signal wb_cyc_int  : std_logic;
  signal wb_stall_d0 : std_logic;
  signal stalled_data_in_buffer : std_logic;
  signal state : t_arb_state;
  
begin  -- behavioral

  is_data <= '1' when snk_ctrl_i = c_wrsw_ctrl_none
             or snk_ctrl_i = c_wrsw_ctrl_dst_mac
             or snk_ctrl_i = c_wrsw_ctrl_src_mac
             or snk_ctrl_i = c_wrsw_ctrl_ethertype
             or snk_ctrl_i = c_wrsw_ctrl_vid_prio
             or snk_ctrl_i = c_wrsw_ctrl_payload
             or snk_ctrl_i = c_wrsw_ctrl_fcs
             else '0';

  is_oob <= '1' when snk_ctrl_i = c_wrsw_ctrl_tx_oob
            or snk_ctrl_i = c_wrsw_ctrl_rx_oob
            else '0';

  p_count_acks : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        ack_counter <= (others => '0');
      else
        if(wb_cyc_int = '0') then
          ack_counter <= (others => '0');
        else
          if(wb_stb_int = '1' and wb_ack_i = '0') then
            ack_counter <= ack_counter + 1;
          elsif(wb_stb_int = '0' and wb_ack_i = '1') then
            ack_counter <= ack_counter - 1;
          end if;
        end if;
      end if;
    end if;
  end process;

  arb_fsm : process(clk_sys_i)
    variable tmp : t_wrf_status_reg;
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0') then
        state      <= ST_WAIT_FRAME;
        wb_stb_int <= '0';
        wb_cyc_int <= '0';

        wb_sel_o <= (others => '0');
        wb_adr_o <= (others => '0');
        wb_dat_o <= (others => '0');
        stalled_data_in_buffer <= '0';
      else

        case state is
          when ST_WAIT_FRAME =>

            if(snk_sof_p1_i = '1') then
              wb_cyc_int <= '1';
              state      <= ST_DATA;
            end if;

          when ST_DATA =>

            
            if(snk_eof_p1_i = '1') then
              state <= ST_WAIT_END_CYCLE;
            end if;

            if(snk_error_p1_i = '1') then
              tmp.rx_error := '1';
              wb_adr_o     <= c_WRF_STATUS;
              wb_sel_o     <= "11";
              wb_dat_o     <= f_marshall_wrf_status(tmp);

              if(wb_stall_i = '0') then
                wb_stb_int <= '1';
                state      <= ST_WAIT_END_CYCLE;
              else
                wb_stb_int <= '0';
                state      <= ST_RX_ERROR;
              end if;

            elsif(snk_valid_i = '1') then
              wb_dat_o <= snk_data_i;

              if(wb_stall_i = '0') then
                wb_stb_int <= '1';
              else
                wb_stb_int <= '0';
                stalled_data_in_buffer <= '1';
              end if;

              if(is_data = '1') then
                wb_adr_o    <= c_WRF_DATA;
                wb_sel_o(0) <= not snk_bytesel_i;
                wb_sel_o(1) <= '1';
              elsif(is_oob = '1') then
                wb_sel_o <= "11";
                wb_adr_o <= c_WRF_OOB;
              end if;
            elsif(stalled_data_in_buffer = '1') then
              if(wb_stall_i = '0') then
                wb_stb_int <= '1';
                stalled_data_in_buffer <= '0';
              else
                wb_stb_int <= '0';
                stalled_data_in_buffer <= '1';
              end if;
            else
              wb_stb_int <= '0';
            end if;


          when ST_RX_ERROR =>
            if(wb_stall_i = '0') then
              wb_stb_int <= '1';
              state      <= ST_WAIT_END_CYCLE;
            end if;

          when ST_WAIT_END_CYCLE =>

            if(ack_counter = 0 or (ack_counter = 1 and wb_ack_i = '1')) then
              wb_cyc_int <= '0';
              wb_stb_int <= '0';
              state      <= ST_WAIT_FRAME;
            end if;

          when others => null;
        end case;

      end if;

    wb_stall_d0 <= wb_stall_i;
      
    end if;

    
  end process;

  
  wb_cyc_o <= wb_cyc_int;
  wb_stb_o <= wb_stb_int;

  snk_dreq_o <= not wb_stall_i;
  wb_we_o    <= '1';
  

end behavioral;
