-------------------------------------------------------------------------------
-- Title      : 1000base-X MAC/Endpoint 
-- Project    : White Rabbit Switch 
-------------------------------------------------------------------------------
-- File       : ep_rx_deframer.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2009-06-22
-- Last update: 2011-08-11
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
use work.old_endpoint_pkg.all;

entity old_ep_rx_deframer is

  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

-- physical coding sublayer (PCS) interface
    pcs_data_i    : in  std_logic_vector(15 downto 0);
    pcs_bytesel_i : in  std_logic;
    pcs_sof_i     : in  std_logic;
    pcs_eof_i     : in  std_logic;
    pcs_valid_i   : in  std_logic;
    pcs_error_i   : in  std_logic;
    pcs_dreq_o    : out std_logic;

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
    rmon_rx_ok_p_o       : out std_logic;
    rmon_rx_buf_drop_p_o : out std_logic;
    rmon_rx_rtu_drop_p_o : out std_logic;
    rmon_rx_crc_err_p_o  : out std_logic;
    rmon_rx_runt_p_o     : out std_logic;
    rmon_rx_giant_p_o    : out std_logic;
    rmon_rx_pcs_err_p_o  : out std_logic;

-------------------------------------------------------------------------------
-- control registers
-------------------------------------------------------------------------------

    -- 802.1q access/trunk mode
    ep_rfcr_qmode_i : in std_logic_vector(1 downto 0);
    ep_rcr_en_fra_i : in std_logic;

    ep_tscr_en_rxts_i  : in std_logic;
    ep_rfcr_fix_prio_i : in std_logic;
    ep_rfcr_prio_val_i : in std_logic_vector(2 downto 0);
    ep_rfcr_vid_val_i  : in std_logic_vector(11 downto 0);

    ep_rfcr_a_runt_i  : in std_logic;
    ep_rfcr_a_giant_i : in std_logic;
--    ep_rfcr_keep_crc_i : in std_logic;

-------------------------------------------------------------------------------
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

end old_ep_rx_deframer;

architecture behavioral of old_ep_rx_deframer is

  component old_ep_rx_crc_size_check
    port (
      clk_sys_i           : in  std_logic;
      rst_n_i             : in  std_logic;
      enable_i            : in  std_logic;
      snk_data_i          : in  std_logic_vector(15 downto 0);
      snk_bytesel_i       : in  std_logic;
      snk_sof_p1_i        : in  std_logic;
      snk_eof_p1_i        : in  std_logic;
      snk_valid_i         : in  std_logic;
      snk_rerror_p1_i     : in  std_logic;
      snk_dreq_o          : out std_logic;
      src_dreq_i          : in  std_logic;
      src_data_o          : out std_logic_vector(15 downto 0);
      src_bytesel_o       : out std_logic;
      src_sof_p1_o        : out std_logic;
      src_eof_p1_o        : out std_logic;
      src_valid_o         : out std_logic;
      src_rerror_p1_o     : out std_logic;
      src_is_crc_o        : out std_logic;
      rmon_rx_crc_err_p_o : out std_logic;
      rmon_rx_runt_p_o    : out std_logic;
      rmon_rx_giant_p_o   : out std_logic;
      rmon_rx_pcs_err_p_o : out std_logic;
      ep_rfcr_a_runt_i    : in  std_logic;
      ep_rfcr_a_giant_i   : in  std_logic;
      ep_rfcr_keep_crc_i  : in  std_logic);
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
  signal snk_dreq      : std_logic;
  signal snk_valid     : std_logic;
  signal snk_sof_p1    : std_logic;
  signal snk_eof_p1    : std_logic;
  signal snk_rerror_p1 : std_logic;
  signal snk_is_crc    : std_logic;
  signal snk_bytesel   : std_logic;
  
begin  -- behavioral


  U_crc_size_checker : old_ep_rx_crc_size_check
    port map (
      clk_sys_i => clk_sys_i,
      rst_n_i   => rst_n_i,

      enable_i => ep_rcr_en_fra_i,

      snk_data_i      => pcs_data_i,
      snk_bytesel_i   => pcs_bytesel_i,
      snk_sof_p1_i    => pcs_sof_i,
      snk_eof_p1_i    => pcs_eof_i,
      snk_valid_i     => pcs_valid_i,   --checker_valid,
      snk_rerror_p1_i => pcs_error_i,
      snk_dreq_o      => pcs_dreq_o,

      src_dreq_i      => snk_dreq,
      src_data_o      => snk_data,
      src_bytesel_o   => snk_bytesel,
      src_sof_p1_o    => snk_sof_p1,
      src_eof_p1_o    => snk_eof_p1,
      src_valid_o     => snk_valid,
      src_rerror_p1_o => snk_rerror_p1,
      src_is_crc_o    => snk_is_crc,

      rmon_rx_crc_err_p_o => rmon_rx_crc_err_p_o,
      rmon_rx_runt_p_o    => rmon_rx_runt_p_o,
      rmon_rx_giant_p_o   => rmon_rx_giant_p_o,
      rmon_rx_pcs_err_p_o => rmon_rx_pcs_err_p_o,
      ep_rfcr_a_runt_i    => ep_rfcr_a_runt_i,
      ep_rfcr_a_giant_i   => ep_rfcr_a_giant_i,
      ep_rfcr_keep_crc_i  => '0');


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

        rmon_rx_buf_drop_p_o <= '0';
        rmon_rx_ok_p_o       <= '0';
        rmon_rx_rtu_drop_p_o <= '0';

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

            rmon_rx_buf_drop_p_o <= '0';
            rmon_rx_ok_p_o       <= '0';
            rmon_rx_rtu_drop_p_o <= '0';

            snk_dreq_int <= '1';

            next_hdr       <= '0';
            is_pause       <= '1';
            counter        <= (others => '0');
            data_firstword <= '1';

            if(ep_rcr_en_fra_i = '1') then
              if(snk_sof_p1 = '1') then
                if(rbuf_drop_i = '0' and rtu_full_i = '0') then
                  state         <= RXF_HEADER;
                  rbuf_sof_p1_o <= '1';
                else
                  rmon_rx_buf_drop_p_o <= rbuf_drop_i;
                  rmon_rx_rtu_drop_p_o <= rtu_full_i;
                  rbuf_sof_p1_o        <= '0';
                end if;
              else
                rbuf_sof_p1_o <= '0';
              end if;
            end if;

          when RXF_HEADER =>
            rbuf_sof_p1_o <= '0';

            if(snk_rerror_p1 = '1') then
              state <= RXF_ERROR;
            end if;

            if(rbuf_drop_i = '1') then
              state                <= RXF_ERROR;
              rmon_rx_buf_drop_p_o <= '1';
            end if;

            if(snk_valid = '1' or next_hdr = '1') then

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

            if(snk_rerror_p1 = '1') then
              state <= RXF_ERROR;
            end if;

            if(rbuf_drop_i = '1') then
              state                <= RXF_ERROR;
              rmon_rx_buf_drop_p_o <= '1';
            end if;

            rbuf_ctrl_o <= c_wrsw_ctrl_payload;

            if(snk_eof_p1 = '1') then

              if(oob_valid_i = '1' and ep_tscr_en_rxts_i = '1') then
                state   <= RXF_OOB;
                counter <= (others => '0');
              else
                rmon_rx_ok_p_o <= '1';
                rbuf_eof_p1_o <= '1';
                state          <= RXF_IDLE;
              end if;

              snk_dreq_int <= '0';

            end if;

            -- got a valid word from the PCS
            if(snk_valid = '1') then

              if(data_firstword = '1') then
                rtu_rq_strobe_p1_o <= not is_pause;
                fc_pause_delay_o   <= snk_data;
              -- record the PAUSE delay, it's the first 2 bytes of the payload
              else
                rtu_rq_strobe_p1_o <= '0';
              end if;

              data_firstword <= '0';

              -- end-of-frame?
              rbuf_data_o  <= snk_data;
              rbuf_bytesel_o <= snk_bytesel;
              rbuf_valid_o <= '1';
              snk_dreq_int <= '1';
            else
              rbuf_valid_o <= '0';
            end if;

          when RXF_OOB =>
            counter <= counter + 1;
            if(rbuf_drop_i = '1') then
              state                <= RXF_ERROR;
              rmon_rx_buf_drop_p_o <= '1';
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
                  rmon_rx_ok_p_o <= '1';
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

  snk_dreq <= snk_dreq_int and not (snk_eof_p1 or snk_rerror_p1);

end behavioral;

