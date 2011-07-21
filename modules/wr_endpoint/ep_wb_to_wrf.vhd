library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.global_defs.all;
use work.endpoint_pkg.all;

entity ep_wb_to_wrf is
  port(
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

-- WRF sink
    src_data_o     : out std_logic_vector(15 downto 0);
    src_ctrl_o     : out std_logic_vector(c_wrsw_ctrl_size-1 downto 0);
    src_bytesel_o  : out std_logic;
    src_dreq_i     : in  std_logic;
    src_valid_o    : out std_logic;
    src_sof_p1_o   : out std_logic;
    src_eof_p1_o   : out std_logic;
    src_error_p1_i : in  std_logic;
    src_abort_p1_o : out std_logic;

-- Pipelined Wishbone slave
    wb_dat_i   : in  std_logic_vector(15 downto 0);
    wb_adr_i   : in  std_logic_vector(1 downto 0);
    wb_sel_i   : in  std_logic_vector(1 downto 0);
    wb_cyc_i   : in  std_logic;
    wb_stb_i   : in  std_logic;
    wb_we_i    : in  std_logic;
    wb_stall_o : out std_logic;
    wb_ack_o   : out std_logic;
    wb_err_o   : out std_logic;
    wb_rty_o   : out std_logic

    );

end ep_wb_to_wrf;

architecture behavioral of ep_wb_to_wrf is

  function f_gen_ctrl(sreg : unsigned; addr : std_logic_vector) return std_logic_vector is
    variable tmp : std_logic_vector(3 downto 0);
  begin
    if(addr = c_WRF_DATA) then
      if(sreg(6 downto 0) /= "0000000") then
        return c_wrsw_ctrl_none;
      else
        return c_wrsw_ctrl_payload;
      end if;
    elsif(addr = c_WRF_OOB) then
      return c_wrsw_ctrl_tx_oob;
    end if;
  end function;


  function to_sl(x : boolean) return std_logic is
  begin
    if(x = true) then
      return '1';
    else
      return '0';
    end if;
  end function to_sl;

  type t_arb_state is (ST_WAIT_CYC, ST_BEGIN_CYC, ST_DATA, ST_TERMINATE_CYC);

  signal hdr_sreg : unsigned(9 downto 0);
  signal state    : t_arb_state;

  signal src_ctrl_int : std_logic_vector(3 downto 0);

  signal stat : t_wrf_status_reg;
  
begin  -- behavioral

  wb_stall_o <= not src_dreq_i or to_sl(state = ST_WAIT_CYC and wb_stb_i = '1' and wb_cyc_i = '1');

  stat <= f_unmarshall_wrf_status(wb_dat_i);

  arb_fsm : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0'then
        hdr_sreg       <= (others => '0');
        state          <= ST_WAIT_CYC;
        src_sof_p1_o   <= '0';
        src_data_o     <= (others => '0');
        src_ctrl_int   <= (others => '0');
        src_eof_p1_o   <= '0';
        src_abort_p1_o <= '0';
        src_valid_o <= '0';

        wb_ack_o <= '0';
        wb_err_o <= '0';
        wb_rty_o <= '0';
        
      else
        case state is
          when ST_WAIT_CYC =>
            src_eof_p1_o   <= '0';
            src_abort_p1_o <= '0';

            hdr_sreg <= to_unsigned(1, hdr_sreg'length);

            if(wb_cyc_i = '1') then
              if(wb_stb_i = '1') then
                state        <= ST_BEGIN_CYC;
                src_data_o   <= wb_dat_i;
                src_bytesel_o <= '0';
                src_ctrl_int <= c_wrsw_ctrl_dst_mac;
                wb_ack_o     <= '1';
              else
                state <= ST_DATA;
              end if;

              src_sof_p1_o <= '1';
            end if;


          when ST_BEGIN_CYC =>
            src_sof_p1_o <= '0';

            if(src_dreq_i = '1') then

              src_valid_o <= '1';
              wb_ack_o    <= '1';

              if(stat.rx_error = '1' and wb_adr_i = c_WRF_STATUS) then
                src_abort_p1_o <= '1';
                state          <= ST_WAIT_CYC;
                wb_ack_o       <= '1';
                state          <= ST_TERMINATE_CYC;
              else
                
                src_valid_o <= '1';
                state       <= ST_DATA;
                hdr_sreg    <= hdr_sreg sll 1;
                
              end if;
            else
              src_valid_o <= '0';
              wb_ack_o    <= '0';
            end if;

            if(src_error_p1_i = '1') then
              wb_err_o <= '1';
              state    <= ST_TERMINATE_CYC;
            end if;
            
          when ST_DATA =>
            src_sof_p1_o <= '0';

            if(wb_cyc_i = '1') then
              if(wb_stb_i = '1') then
                case wb_adr_i is
                  when c_WRF_STATUS =>
                    if(stat.rx_error = '1') then
                      src_abort_p1_o <= '1';
                      src_valid_o <= '0';
                      state          <= ST_TERMINATE_CYC;
                      wb_ack_o       <= '1';
                    else
                      src_valid_o <= '0';
                    end if;

                  when c_WRF_OOB =>
                    hdr_sreg <= hdr_sreg sll 1;

                    src_ctrl_int <= f_gen_ctrl(hdr_sreg, wb_adr_i);
                    src_data_o   <= wb_dat_i;
                    src_bytesel_o <= not wb_sel_i(0);
                    src_valid_o  <= '1';
                    wb_ack_o     <= '1';
                  when c_WRF_DATA =>

                    hdr_sreg <= hdr_sreg sll 1;

                    src_ctrl_int <= f_gen_ctrl(hdr_sreg, wb_adr_i);
                    src_data_o   <= wb_dat_i;
                    src_bytesel_o <= not wb_sel_i(0);
                    src_valid_o  <= '1';
                    wb_ack_o     <= '1';
                  when others =>
                    
                end case;

              else
                wb_ack_o    <= '0';
                src_valid_o <= '0';
              end if;
            else
              src_eof_p1_o <= '1';
              src_valid_o <= '0';
              state        <= ST_TERMINATE_CYC;
            end if;

            if(src_error_p1_i = '1') then
              wb_err_o <= '1';
              state    <= ST_TERMINATE_CYC;
            end if;

            
          when ST_TERMINATE_CYC =>
            wb_err_o <= '0';
            if(wb_cyc_i = '0') then
              state <= ST_WAIT_CYC;
            end if;
            
        end case;
      end if;
    end if;
  end process;

  src_ctrl_o <= src_ctrl_int;

end behavioral;
