library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.gencores_pkg.all;              -- for gc_crc_gen
use work.endpoint_private_pkg.all;
use work.ep_wbgen2_pkg.all;
use work.wr_fabric_pkg.all;


entity ep_rx_oob_insert is
  port(clk_sys_i : in std_logic;
       rst_n_i   : in std_logic;

       snk_fab_i  : in  t_ep_internal_fabric;
       snk_dreq_o : out std_logic;

       src_fab_o  : out t_ep_internal_fabric;
       src_dreq_i : in  std_logic;

       oob_data_i  : in  std_logic_vector(31 downto 0);
       oob_valid_i : in  std_logic;
       oob_ack_o   : out std_logic;

       regs_i : in  t_ep_out_registers
     );

end ep_rx_oob_insert;

architecture behavioral of ep_rx_oob_insert is

  type t_state is (FLUSH_STALL, DISCARD_FRAME, END_FRAME, WAIT_EOF, INSERT_1, INSERT_2, INSERT_3);
  signal state : t_state;

  signal dreq_mask  : std_logic;
  signal stored_fab : t_ep_internal_fabric;

  procedure f_insert_oob(signal dreq_i : in  std_logic;
                         signal fab_o  : out t_ep_internal_fabric;
                         data          :     std_logic_vector(15 downto 0)) is
  begin
    
    if(dreq_i = '1') then
      fab_o.addr   <= c_WRF_OOB;
      fab_o.DATA   <= data;
      fab_o.dvalid <= '1';
    else
      fab_o.dvalid <= '0';
    end if;
  end f_insert_oob;


  
begin  -- behavioral

  snk_dreq_o <= src_dreq_i and dreq_mask;

  p_insert_oob : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' or regs_i.ecr_rx_en_o = '0' then
        state     <= WAIT_EOF;
        dreq_mask <= '0';

        src_fab_o.eof    <= '0';
        src_fab_o.error  <= '0';
        src_fab_o.dvalid <= '0';

        oob_ack_o <= '0';
      else

        if(snk_fab_i.error = '1') then
          state <= DISCARD_FRAME;
        else

          case state is
            when WAIT_EOF =>
              dreq_mask <= '1';

              if(snk_fab_i.eof = '1')then
                if(regs_i.tscr_en_rxts_o = '1' and oob_valid_i = '1') then
                  state     <= INSERT_1;
                  oob_ack_o <= '1';
                  dreq_mask <= '0';
                else
                  state <= END_FRAME;
                end if;
              elsif (snk_fab_i.dvalid = '1' and src_dreq_i = '0') then
                stored_fab.bytesel <= snk_fab_i.bytesel;
                stored_fab.data    <= snk_fab_i.data;
                stored_fab.dvalid  <= '1';
                stored_fab.addr    <= c_WRF_DATA;
                state              <= FLUSH_STALL;
              else
                src_fab_o.data    <= snk_fab_i.data;
                src_fab_o.dvalid  <= src_dreq_i and snk_fab_i.dvalid;
                src_fab_o.bytesel <= snk_fab_i.bytesel;
                src_fab_o.addr    <= c_WRF_DATA;
              end if;
              

            when END_FRAME =>
              if(src_dreq_i = '1')then
                src_fab_o.eof    <= '1';
                src_fab_o.dvalid <= '0';
                state            <= WAIT_EOF;
              end if;
              
            when DISCARD_FRAME =>
              if(src_dreq_i = '1') then
                src_fab_o.error  <= '1';
                src_fab_o.dvalid <= '0';
                state            <= WAIT_EOF;
              end if;

            when FLUSH_STALL =>
              if(src_dreq_i = '1')then
                src_fab_o.addr    <= stored_fab.addr;
                src_fab_o.data    <= stored_fab.data;
                src_fab_o.dvalid  <= stored_fab.dvalid;
                src_fab_o.bytesel <= stored_fab.bytesel;
                state             <= WAIT_EOF;
              else
                src_fab_o.DATA   <= (others => 'X');
                src_fab_o.dvalid <= '0';
              end if;

            when INSERT_1 =>
              oob_ack_o <= '0';
              f_insert_oob(src_dreq_i, src_fab_o, c_WRF_OOB_TYPE_RX & "0000000" & regs_i.ecr_portid_o);
              if(src_dreq_i = '1')then
                state <= INSERT_2;
              end if;
            when INSERT_2 =>

              f_insert_oob(src_dreq_i, src_fab_o, oob_data_i(31 downto 16));
              if(src_dreq_i = '1')then
                state <= INSERT_3;
              end if;

            when INSERT_3 =>
              f_insert_oob(src_dreq_i, src_fab_o, oob_data_i(15 downto 0));
              if(src_dreq_i = '1')then
                state <= END_FRAME;
              end if;

          end case;
        end if;
      end if;
    end if;
  end process;

  src_fab_o.sof <= regs_i.ecr_rx_en_o and snk_fab_i.sof;

  
end behavioral;




