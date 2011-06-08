-------------------------------------------------------------------------------
-- Title      : 1000base-X MAC/Endpoint 
-- Project    : White Rabbit Switch 
-------------------------------------------------------------------------------
-- File       : ep_rx_deframer.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2009-06-22
-- Last update: 2011-05-27
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: RX deframing module:
-- - checks frame CRC
-- - inserts 802.1q headers when necessary
-- - parses packet headers and generates RTU requests
-- - embeds RX OOB block with timestamp information
-------------------------------------------------------------------------------
-- Copyright (c) 2009 Tomasz Wlostowski
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2009-06-22  0.1      twlostow  Created
------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.endpoint_private_pkg.all;

entity ep_rx_deframer is

  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

-- physical coding sublayer (PCS) interface
    pcs_data_i  : in  std_logic_vector(17 downto 0);
    pcs_valid_i : in  std_logic;
    pcs_dreq_o  : out std_logic;
    pcs_busy_i  : in  std_logic;

-- OOB frame tag value and strobing signal
    oob_data_i  : in  std_logic_vector(47 downto 0);
    oob_valid_i : in  std_logic;
    oob_ack_o   : out std_logic;

-- RX Buffer interface
    rbuf_sof_p1_o    : out std_logic;
    rbuf_eof_p1_o    : out std_logic;
    rbuf_ctrl_o      : out std_logic_vector(4 - 1 downto 0);
    rbuf_data_o      : out std_logic_vector(15 downto 0);
    rbuf_bytesel_o   : out std_logic;
    rbuf_valid_o     : out std_logic;
    rbuf_drop_i      : in  std_logic;
    rbuf_rerror_p1_o : out std_logic;

-- flow control signals
    fc_pause_p_o     : out std_logic;
    fc_pause_delay_o : out std_logic_vector(15 downto 0);

-- RMON/statistic counters signals
    rmon_o : inout t_rmon_triggers;
    regs_b : inout t_ep_registers;

    ------------------------------------------------------------------------------
-- RTU interface
-------------------------------------------------------------------------------

-- source/MAC address
    rtu_rq_smac_o : out std_logic_vector(48 - 1 downto 0);
    rtu_rq_dmac_o : out std_logic_vector(48 - 1 downto 0);

-- VLAN Id/VID present flag
    rtu_rq_vid_o     : out std_logic_vector(12 - 1 downto 0);
    rtu_rq_has_vid_o : out std_logic;

-- packet priority / priority present flag
    rtu_rq_prio_o     : out std_logic_vector(3 - 1 downto 0);
    rtu_rq_has_prio_o : out std_logic;

-- RTU idle input (indicates that the RTU is ready to serve another request)
    rtu_full_i : in std_logic;

-- RTU request output: HI pulse initiates RTU matching procedure
    rtu_rq_strobe_p1_o : out std_logic
    );

end ep_rx_deframer;

architecture behavioral of ep_rx_deframer is

  component ep_rx_crc_size_check
    port (
      clk_sys_i   : in    std_logic;
      rst_n_i     : in    std_logic;
      enable_i    : in    std_logic;
      snk_data_i  : in    std_logic_vector(17 downto 0);
      snk_valid_i : in    std_logic;
      snk_dreq_o  : out    std_logic;
      src_data_o  : out   std_logic_vector(17 downto 0);
      src_valid_o : out   std_logic;
      src_dreq_i  : in    std_logic;
      rmon_o      : inout t_rmon_triggers;
      regs_b      : inout t_ep_registers);
  end component;

  type t_rx_deframer_state is (RXF_IDLE, RXF_HEADER, RXF_DATA, RXF_ERROR, RXF_OOB);

  signal state : t_rx_deframer_state;

  signal gap_cntr : unsigned(3 downto 0);

  -- new sigs
  signal counter : unsigned(7 downto 0);

  signal rxdata_saved : std_logic_vector(15 downto 0);
  signal next_hdr     : std_logic;
  signal is_pause     : std_logic;

  signal data_firstword : std_logic;
  signal snk_dreq_int   : std_logic;

  signal snk_data      : std_logic_vector(15 downto 0);
  signal snk_idata      : std_logic_vector(17 downto 0);
  signal snk_dreq      : std_logic;
  signal snk_dvalid     : std_logic;
  signal snk_ivalid     : std_logic;
  signal snk_sof    : std_logic;
  signal snk_eof    : std_logic;
  signal snk_error : std_logic;
  signal snk_bytesel   : std_logic;

  signal ep_rfcr_qmode_i : std_logic_vector(1 downto 0) := c_QMODE_PORT_NONE;
  signal ep_rfcr_vid_val_i : std_logic_vector(11 downto 0) := x"000";
  signal ep_rfcr_prio_val_i : std_logic_vector(2 downto 0) := "000";
  signal ep_rfcr_fix_prio_i : std_logic := '0';
  
  
begin  -- behavioral


  U_crc_size_checker : ep_rx_crc_size_check
    port map (
      clk_sys_i   => clk_sys_i,
      rst_n_i     => rst_n_i,
      enable_i    => regs_b.ecr_rx_en,
      snk_data_i  => pcs_data_i,
      snk_valid_i => pcs_valid_i,
      snk_dreq_o  => pcs_dreq_o,
      src_dreq_i  => snk_dreq,
      src_data_o  => snk_idata,
      src_valid_o => snk_ivalid,
      rmon_o      => rmon_o,
      regs_b      => regs_b);

  snk_sof     <= f_is_sof(snk_idata, snk_ivalid);
  snk_eof     <= f_is_eof(snk_idata, snk_ivalid);
  snk_bytesel <= f_is_single_byte(snk_idata, snk_ivalid);
  snk_error   <= f_is_error(snk_idata, snk_ivalid);
  snk_dvalid  <= f_is_data(snk_idata, snk_ivalid);

  snk_data <= snk_idata(15 downto 0);
  
  RX_FSM : process (clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0') then
        state <= RXF_IDLE;

        oob_ack_o <= '0';

        rbuf_sof_p1_o    <= '0';
        rbuf_eof_p1_o    <= '0';
        rbuf_rerror_p1_o <= '0';
        rbuf_ctrl_o      <= (others => '0');
        rbuf_data_o      <= (others => '0');
        rbuf_valid_o     <= '0';
        rbuf_bytesel_o   <= '0';

        rtu_rq_strobe_p1_o <= '0';
        rtu_rq_smac_o      <= (others => '0');
        rtu_rq_dmac_o      <= (others => '0');
        rtu_rq_vid_o       <= (others => '0');
        rtu_rq_has_vid_o   <= '0';
        rtu_rq_prio_o      <= (others => '0');
        rtu_rq_has_prio_o  <= '0';

        rmon_o.rx_buffer_overrun <= '0';
        rmon_o.rx_ok       <= '0';
        rmon_o.rx_rtu_overrun <= '0';

        fc_pause_p_o     <= '0';
        fc_pause_delay_o <= (others => '0');

        snk_dreq_int <= '0';
        next_hdr     <= '0';
        is_pause     <= '1';

        data_firstword <= '0';
      else

        case state is
          when RXF_IDLE =>

            oob_ack_o <= '0';

            rbuf_rerror_p1_o <= '0';
            rbuf_eof_p1_o    <= '0';
            rbuf_valid_o     <= '0';

            fc_pause_p_o <= '0';

            rmon_o.rx_buffer_overrun <= '0';
            rmon_o.rx_ok      <= '0';
            rmon_o.rx_rtu_overrun <= '0';

            snk_dreq_int <= '1';

            next_hdr       <= '0';
            is_pause       <= '1';
            counter        <= (others => '0');
            data_firstword <= '1';

            if(regs_b.ecr_rx_en = '1') then
              if(snk_sof = '1') then
                if(rbuf_drop_i = '0' and rtu_full_i = '0') then
                  state         <= RXF_HEADER;
                  rbuf_sof_p1_o <= '1';
                else
                  rmon_o.rx_buffer_overrun <= rbuf_drop_i;
                  rmon_o.rx_rtu_overrun<= rtu_full_i;
                  rbuf_sof_p1_o        <= '0';
                end if;
              else
                rbuf_sof_p1_o <= '0';
              end if;
            end if;

          when RXF_HEADER =>
            rbuf_sof_p1_o <= '0';

            if(snk_error = '1') then
              state <= RXF_ERROR;
            end if;

            if(rbuf_drop_i = '1') then
              state                <= RXF_ERROR;
              rmon_o.rx_buffer_overrun <= '1';
            end if;

            if(snk_dvalid = '1' or next_hdr = '1') then

              counter <= counter + 1;

              case counter is
                when x"00" =>           -- DST MAC ADDR
                  if(snk_data /= x"0180") then
                    is_pause <= '0';
                  end if;

                  rtu_rq_dmac_o(47 downto 32) <= snk_data;
                  rbuf_ctrl_o                 <= c_wrsw_ctrl_dst_mac;
                  rbuf_data_o                 <= snk_data;
                  rbuf_valid_o                <= '1';
                  snk_dreq_int                <= '1';
                when x"01" =>
                  if(snk_data /= x"c200") then
                    is_pause <= '0';
                  end if;

                  rtu_rq_dmac_o(31 downto 16) <= snk_data;
                  rbuf_ctrl_o                 <= c_wrsw_ctrl_dst_mac;
                  rbuf_data_o                 <= snk_data;
                  rbuf_valid_o                <= '1';
                  snk_dreq_int                <= '1';
                when x"02" =>
                  if(snk_data /= x"0001") then
                    is_pause <= '0';
                  end if;

                  rtu_rq_dmac_o(15 downto 0) <= snk_data;
                  rbuf_ctrl_o                <= c_wrsw_ctrl_dst_mac;
                  rbuf_data_o                <= snk_data;
                  rbuf_valid_o               <= '1';
                  snk_dreq_int               <= '1';
                when x"03" =>                     -- SRC MAC ADDR
                  rtu_rq_smac_o(47 downto 32) <= snk_data;
                  rbuf_ctrl_o                 <= c_wrsw_ctrl_src_mac;
                  rbuf_data_o                 <= snk_data;
                  rbuf_valid_o                <= '1';
                  snk_dreq_int                <= '1';
                when x"04" =>
                  rtu_rq_smac_o(31 downto 16) <= snk_data;
                  rbuf_ctrl_o                 <= c_wrsw_ctrl_src_mac;
                  rbuf_data_o                 <= snk_data;
                  rbuf_valid_o                <= '1';
                  snk_dreq_int                <= '1';
                when x"05" =>
                  rtu_rq_smac_o(15 downto 0) <= snk_data;
                  rbuf_ctrl_o                <= c_wrsw_ctrl_src_mac;
                  rbuf_data_o                <= snk_data;
                  rbuf_valid_o               <= '1';
                  snk_dreq_int               <= '0';
                when x"06" =>                     -- ETHERTYPE
                  if(snk_data = x"8808") then
                    state        <= RXF_DATA;
                    rbuf_data_o  <= snk_data;
                    rbuf_ctrl_o  <= c_wrsw_ctrl_ethertype;
                    rbuf_valid_o <= '1';
                    snk_dreq_int <= '1';
                  elsif(snk_data = x"8100") then  -- vlan frame
                    is_pause <= '0';

-- case 1: got a VLAN-tagged frame on ACCESS port - drop it
                    if(ep_rfcr_qmode_i = c_QMODE_PORT_ACCESS) then
                      state        <= RXF_ERROR;
                      rbuf_valid_o <= '0';
                      snk_dreq_int <= '0';
                    else
-- case 2: got a VLAN-tagged frame on a TRUNK or UNQUALIFIED port - pass it
                      rtu_rq_has_prio_o <= '1';
                      rtu_rq_has_vid_o  <= '1';
                      rbuf_ctrl_o       <= c_wrsw_ctrl_none;
                      rbuf_data_o       <= snk_data;
                      snk_dreq_int      <= '1';
                      rbuf_valid_o      <= '1';
                    end if;

                  else                  -- no vlan header
                    rbuf_ctrl_o <= c_wrsw_ctrl_ethertype;
                    is_pause    <= '0';
-- case 3: got a non-802.1q frame on ACCESS port: insert VLAN header
--         with appropriate VID/PCP
                    if(ep_rfcr_qmode_i = c_QMODE_PORT_ACCESS) then
                      -- insert VLAN header
                      rxdata_saved      <= snk_data;
                      snk_dreq_int      <= '0';
                      rbuf_valid_o      <= '0';
                      rtu_rq_has_vid_o  <= '1';
                      rtu_rq_has_prio_o <= '1';
                      rtu_rq_vid_o      <= ep_rfcr_vid_val_i;
                      rtu_rq_prio_o     <= ep_rfcr_prio_val_i;
                      next_hdr          <= '1';
                      
                    elsif (ep_rfcr_qmode_i = c_QMODE_PORT_TRUNK) then
-- case 4: got a non-802.1q frame on TRUNK port: drop
                      
                      state <= RXF_ERROR;
                    else
-- case 5: unqualified port: pass.
                      rbuf_data_o       <= snk_data;
                      rbuf_valid_o      <= '1';
                      snk_dreq_int      <= '1';
                      rtu_rq_has_vid_o  <= '0';
                      rtu_rq_has_prio_o <= ep_rfcr_fix_prio_i;
                      rtu_rq_prio_o     <= ep_rfcr_prio_val_i;
                      state             <= RXF_DATA;
                    end if;
                    

                  end if;
                when x"07" =>           -- new ethertype

                  if(ep_rfcr_qmode_i = c_QMODE_PORT_ACCESS) then  -- access:
                                                                  -- insert
                    snk_dreq_int <= '0';
                    rbuf_valid_o <= '1';
                    rbuf_ctrl_o  <= c_wrsw_ctrl_none;
                    rbuf_data_o  <= x"8100";
                    next_hdr     <= '1';
                  else
                    rbuf_ctrl_o  <= c_wrsw_ctrl_ethertype;
                    rbuf_data_o  <= snk_data;
                    rbuf_valid_o <= '1';
                    snk_dreq_int <= '1';
                  end if;

                when x"08" =>

                  if(ep_rfcr_qmode_i = c_QMODE_PORT_ACCESS) then  -- access:
                                                                  -- insert
                                                                  -- vlan tag
--                    snk_dreq_int <= '0';
                    rbuf_valid_o <= '1';
                    rbuf_ctrl_o  <= c_wrsw_ctrl_ethertype;
                    rbuf_data_o  <= rxdata_saved;  -- old ethertype
                    next_hdr     <= '1';
                  else
                    rbuf_ctrl_o  <= c_wrsw_ctrl_vid_prio;
                    rbuf_data_o  <= snk_data;
                    rbuf_valid_o <= '1';
                    state        <= RXF_DATA;
                  end if;
                  snk_dreq_int <= '1';
                  

                when x"09" =>
                  rbuf_ctrl_o <= c_wrsw_ctrl_vid_prio;
                  rbuf_data_o <= ep_rfcr_prio_val_i & '0' & ep_rfcr_vid_val_i;

                  rbuf_valid_o <= '1';
                  next_hdr     <= '0';
                  state        <= RXF_DATA;
                when others => null;
              end case;

            else
              rbuf_valid_o <= '0';
              snk_dreq_int <= '1';
            end if;

          when RXF_DATA =>

            if(snk_error = '1') then
              state <= RXF_ERROR;
            end if;

            if(rbuf_drop_i = '1') then
              state                <= RXF_ERROR;
              rmon_o.rx_buffer_overrun <= '1';
            end if;

            rbuf_ctrl_o <= c_wrsw_ctrl_payload;

            if(snk_eof = '1') then

              if(oob_valid_i = '1' and regs_b.tscr_en_rxts = '1') then
                state   <= RXF_OOB;
                counter <= (others => '0');
              else
                rmon_o.rx_ok <= '1';
                rbuf_eof_p1_o  <= '1';
                state          <= RXF_IDLE;
              end if;

              snk_dreq_int <= '0';

            end if;

            -- got a valid word from the PCS
            if(snk_dvalid = '1') then

              if(data_firstword = '1') then
                rtu_rq_strobe_p1_o <= not is_pause;
                fc_pause_delay_o   <= snk_data;
              -- record the PAUSE delay, it's the first 2 bytes of the payload
              else
                rtu_rq_strobe_p1_o <= '0';
              end if;

              data_firstword <= '0';

              -- end-of-frame?
              rbuf_data_o    <= snk_data;
              rbuf_bytesel_o <= snk_bytesel;
              rbuf_valid_o   <= '1';
              snk_dreq_int   <= '1';
            else
              rbuf_valid_o <= '0';
            end if;

          when RXF_OOB =>
            counter <= counter + 1;
            if(rbuf_drop_i = '1') then
              state                <= RXF_ERROR;
              rmon_o.rx_buffer_overrun <= '1';
            else
              case counter is
                when x"00" =>
                  rbuf_ctrl_o  <= c_wrsw_ctrl_rx_oob;
                  rbuf_data_o  <= oob_data_i(47 downto 32);
                  rbuf_valid_o <= '1';
                when x"01" =>
                  rbuf_ctrl_o  <= c_wrsw_ctrl_rx_oob;
                  rbuf_data_o  <= oob_data_i(31 downto 16);
                  rbuf_valid_o <= '1';
                when x"02" =>
                  rbuf_ctrl_o  <= c_wrsw_ctrl_rx_oob;
                  rbuf_data_o  <= oob_data_i(15 downto 0);
                  rbuf_valid_o <= '1';
                  oob_ack_o    <= '1';
                when x"03" =>
                  rbuf_valid_o   <= '0';
                  oob_ack_o      <= '0';
                  rbuf_eof_p1_o  <= '1';
                  snk_dreq_int   <= '1';
                  rmon_o.rx_ok <= '1';
                  state          <= RXF_IDLE;

                when others => null;
              end case;
            end if;

          when RXF_ERROR =>
            fc_pause_p_o     <= '0';
            rbuf_rerror_p1_o <= '1';
            state            <= RXF_IDLE;
          when others => null;
        end case;


      end if;
    end if;
  end process;

  snk_dreq <= snk_dreq_int and not (snk_eof or snk_error);

end behavioral;

