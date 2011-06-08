-------------------------------------------------------------------------------
-- Title      : 1000base-X MAC/Endpoint
-- Project    : White Rabbit Switch
-------------------------------------------------------------------------------
-- File       : ep_tx_framer.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2009-06-22
-- Last update: 2011-05-28
-- Platform   : FPGA-generic
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description: TX Framing module:
-- - provides a WRF interface to the host
-- - embeds source MAC addresses if they aren't defined by the host
-- - calculates frame CRC checksum
-- - strips 802.1q headers when necessary
-- - decodes TX OOB data and passes it to the timestamping unit
-------------------------------------------------------------------------------
-- Copyright (c) 2009 Tomasz Wlostowski
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2009-06-22  0.1      twlostow        Created
-- 2010-10-25  0.2      twlostow        Updated to WRF-compatible ports
-------------------------------------------------------------------------------



library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.gencores_pkg.all;
use work.endpoint_private_pkg.all;

entity ep_tx_framer is

  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

------------------------------------------------------------------------------
-- Physical Coding Sublayer (PCS) interface
------------------------------------------------------------------------------

    pcs_error_i : in std_logic;
    pcs_busy_i  : in std_logic;

    pcs_data_o  : out std_logic_vector(17 downto 0);
    pcs_dreq_i  : in  std_logic;
    pcs_valid_o : out std_logic;

-------------------------------------------------------------------------------
-- WRF Sink (see WRF specification for the details)
-------------------------------------------------------------------------------    

-- TX data input
    tx_data_i : in std_logic_vector(15 downto 0);

-- RX control bus: indicates type of word currently present on rx_data_o:
-- SRC_MAC, DST_MAC, VID_PRIO, PAYLOAD, CRC, OOB, END_OF_FRAME
    tx_ctrl_i : in std_logic_vector(4 - 1 downto 0);

-- active HI: indicates the last byte of odd-sized frame. Byte is transferred
-- on MSB of tx_data_i.
    tx_bytesel_i : in std_logic;

-- start of frame signal. HI pulse indicates the beginning of new frame. Upon
-- assertion of tx_sof_p_i, tx_ready_o shall become active, allowing the frame
-- data to be sent.
    tx_sof_p1_i : in std_logic;

-- end-of-frame pulse: indicates end of the current frame on fabric i/f. When rx_valid_o
-- is active, rx_ctrl_o and rx_data_o contain the last data word of the current
-- frame.
    tx_eof_p1_i : in std_logic;

-- active HI: TX fabric is ready to accept data.
    tx_dreq_o : out std_logic;

-- active HI: indicates that tx_data_i, tx_ctrl_i, tx_bytesel_i are valid
    tx_valid_i : in std_logic;

-- Source error: kept only for the comptibility with WRF spec. Ignored by the endpoint.
    tx_rerror_p1_i : in std_logic;

-- TX abort: HI pulse immediately aborts transmission of current frame. 
    tx_tabort_p1_i : in std_logic;

-- TX error strobe: HI pulse indicates that an TX error occured. Error code is
-- present on tx_error_code_o.
    tx_terror_p1_o : out std_logic;

-------------------------------------------------------------------------------
-- Flow Control Unit signals
-------------------------------------------------------------------------------    

-- TX send pause frame - when active, the framer will send a PAUSE frame
-- as soon as possible. The pause quanta must be provided on tx_pause_delay_i input.
    tx_pause_i       : in std_logic;
    tx_pause_delay_i : in std_logic_vector(15 downto 0);

-- TX send pause acknowledge - active after the current pause send request has
-- been completed
    tx_pause_ack_o : out std_logic;

-- When active, the framer will allow for packet transmission.
    tx_flow_enable_i : in std_logic;

-------------------------------------------------------------------------------
-- OOB/TSU signals
-------------------------------------------------------------------------------    

    -- OOB frame tag value and strobing signal
    oob_fid_value_o : out std_logic_vector(15 downto 0);
    oob_fid_stb_o   : out std_logic;

-------------------------------------------------------------------------------
-- control registers
-------------------------------------------------------------------------------

    regs_b : inout t_ep_registers

    );


end ep_tx_framer;

architecture behavioral of ep_tx_framer is

  constant c_IFG_LENGTH : integer := 8;

  type t_tx_framer_state is (TXF_IDLE, TXF_HEADER, TXF_DATA, TXF_OOB, TXF_WAIT_CRC, TXF_EMBED_CRC1, TXF_EMBED_CRC2, TXF_EMBED_CRC3, TXF_GAP, TXF_PAD, TXF_ABORT);

-- general signals
  signal state      : t_tx_framer_state;
  signal counter    : unsigned(7 downto 0);
  signal tx_ready_t : std_logic;

-- CRC generator signals
  signal crc_gen_reset                       : std_logic;
  signal crc_gen_force_reset                 : std_logic;
  signal crc_gen_enable, crc_gen_enable_mask : std_logic;
  signal crc_value                           : std_logic_vector(31 downto 0);

-- Framer <-> PCS FIFO signals
  signal q_bytesel  : std_logic;
  signal q_valid    : std_logic;
  signal write_mask : std_logic;
  signal tx_data    : std_logic_vector(15 downto 0);
  signal odd_length : std_logic;
  signal q_sof      : std_logic;
  signal q_eof      : std_logic;
  signal q_abort    : std_logic;

-- Flow Control-related signals
  signal tx_pause_mode  : std_logic;
  signal tx_pause_delay : std_logic_vector(15 downto 0);
    signal ep_rfcr_qmode_i : std_logic_vector(1 downto 0) := c_QMODE_PORT_NONE;

begin  -- behavioral

  crc_gen_reset  <= '1' when rst_n_i = '0' else ((tx_sof_p1_i and (not tx_pause_mode)) or crc_gen_force_reset);
  crc_gen_enable <= q_valid and crc_gen_enable_mask;

  U_tx_crc_generator : gc_crc_gen
    generic map (
      g_polynomial => x"04C11DB7",
      g_init_value => x"ffffffff",
      g_residue    => x"38fb2284",
      g_data_width => 16,
      g_half_width => 8,
      g_sync_reset => 1,
      g_dual_width => 1,
      g_registered_match_output => false)
    port map (
      clk_i   => clk_sys_i,
      rst_i   => crc_gen_reset,
      en_i    => crc_gen_enable,
      half_i  => q_bytesel,
      data_i  => tx_data,
      match_o => open,
      crc_o   => crc_value);

  -- process: p_tx_fsm
  -- inputs: everything
  -- outputs: everything
  -- description: the main, big TX framing state machine

  p_tx_fsm : process (clk_sys_i)
  begin  -- process
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0') then
        state      <= TXF_IDLE;
        q_bytesel  <= '0';
        write_mask <= '0';
        q_valid    <= '0';
        tx_data    <= (others => '0');
        q_abort    <= '0';
        q_sof      <= '0';
        q_eof      <= '0';
        tx_ready_t <= '0';

        tx_pause_mode  <= '0';
        tx_terror_p1_o <= '0';
        tx_pause_ack_o <= '0';

        crc_gen_enable_mask <= '1';
        crc_gen_force_reset <= '0';

        oob_fid_stb_o <= '0';
      else

        -- we are in the middle of the frame and the framer has got suddenly
        -- disabled or we've received an ABORT command or an error occured in the PCS:
        if((state /= TXF_IDLE and state /= TXF_GAP) and (regs_b.ecr_tx_en = '0' or tx_tabort_p1_i = '1')) then
          -- abort the current frame
          state      <= TXF_ABORT;
          tx_ready_t <= '0';
          q_sof      <= '0';
          q_valid    <= '0';
          
        elsif (pcs_error_i = '1') then
          tx_ready_t     <= '0';
          counter        <= (others => '0');
          tx_terror_p1_o <= '1';
          state          <= TXF_GAP;
        else

          case state is

-------------------------------------------------------------------------------
-- TX FSM state IDLE: awaits incoming TX frames
-------------------------------------------------------------------------------

            when TXF_IDLE =>            -- idle state - wait for the next frame

              tx_terror_p1_o <= '0';
              q_abort        <= '0';

              tx_ready_t <= tx_flow_enable_i;

              -- Check start-of-frame and send-pause signals and eventually
              -- commence frame transmission

              if((tx_sof_p1_i = '1' or tx_pause_i = '1') and regs_b.ecr_tx_en = '1') then
                -- enable writing to PCS FIFO
                q_eof      <= '0';
                write_mask <= '1';

                tx_pause_ack_o <= tx_pause_i;
                tx_pause_mode  <= tx_pause_i;
                tx_pause_delay <= tx_pause_delay_i;

                crc_gen_force_reset <= '1';
                crc_gen_enable_mask <= '1';

                counter <= (others => '0');
                state   <= TXF_HEADER;
              else
                write_mask <= '0';
                q_valid    <= '0';
              end if;

-------------------------------------------------------------------------------
-- TX FSM state HEADER: processes the frame header
-------------------------------------------------------------------------------

            when TXF_HEADER =>
              tx_pause_ack_o <= '0';
              tx_ready_t     <= not tx_pause_mode;

              crc_gen_force_reset <= '0';

              if(tx_valid_i = '1' or tx_pause_mode = '1') then
                counter <= counter + 1;

-- Header generation state machine:
-- - produces PAUSE frame headers
-- - inserts source MAC addressess (when SRC MAC field is empty)
-- - strips 802.1q headers if required

                case counter is
-- DST MAC bits [47:32]
                  when x"00" =>
                    q_sof <= '1';       -- indicate the beginning of new frame
                                        -- to the PCS 

                    if(tx_pause_mode = '1') then
                      tx_data <= x"0180";
                    else
                      tx_data <= tx_data_i;
                    end if;

                    q_valid <= '1';

-- DST MAC bits [31:16]
                  when x"01" =>
                    q_sof <= '0';


                    if(tx_pause_mode = '1') then
                      tx_data <= x"c200";
                    else
                      tx_data <= tx_data_i;
                    end if;

                    q_valid <= '1';

-- DST MAC bits [15:0]
                  when x"02" =>

                    if(tx_pause_mode = '1') then
                      tx_data <= x"0001";
                    else
                      tx_data <= tx_data_i;
                    end if;

                    q_valid <= '1';


-- source MAC: if not present (ctrl = NONE) or we are sending a pause frame,
-- insert the endpoint MAC address value from MACL and MACH registers

-- SRC MAC bits [47:32]
                  when x"03" =>
                    if(tx_ctrl_i = c_wrsw_ctrl_none or tx_pause_mode = '1') then
                      tx_data <= regs_b.mach;
                    else
                      tx_data <= tx_data_i;
                    end if;

                    q_valid <= '1';

-- SRC MAC bits [31:16]
                  when x"04" =>
                    if(tx_ctrl_i = c_wrsw_ctrl_none or tx_pause_mode = '1') then
                      tx_data <= regs_b.macl(31 downto 16);
                    else
                      tx_data <= tx_data_i;
                    end if;
                    q_valid <= '1';

-- SRC MAC bits [15:0]
                  when x"05" =>
                    if(tx_ctrl_i = c_wrsw_ctrl_none or tx_pause_mode = '1') then
                      tx_data <= regs_b.macl(15 downto 0);
                    else
                      tx_data <= tx_data_i;
                    end if;
                    q_valid <= '1';

-- Ethertype                  
                  when x"06" =>         -- ethertype
                    if(tx_pause_mode = '1') then  -- we are sending a PAUSE frame
                      tx_data <= x"8808";         -- SIMONLY
                      q_valid <= '1';
                    else
                      if (tx_ctrl_i = c_wrsw_ctrl_none) then
                        if(ep_rfcr_qmode_i = c_QMODE_PORT_ACCESS) then
                          q_valid <= '0';         -- strip 802.1q tag on
                                                  -- access ports
                        else
                          tx_data <= tx_data_i;
                          q_valid <= '1';
                        end if;
                      else
                        tx_data <= tx_data_i;
                        q_valid <= '1';
                        state   <= TXF_DATA;
                      end if;
                    end if;

-- 802.1q Ethertype or TX PAUSE delay value
                  when x"07" =>

                    if(tx_pause_mode = '1') then  -- we have a pause frame?
                      tx_data <= tx_pause_delay;
                      state   <= TXF_PAD;
                    else
                      tx_data <= tx_data_i;
                    end if;

                    q_valid <= '1';

-- 802.1q CPC/VID field
                  when x"08" =>
                    tx_data <= tx_data_i;

                    if(ep_rfcr_qmode_i = c_QMODE_PORT_ACCESS) then  -- strip
                      q_valid <= '0';
                    else
                      q_valid <= '1';
                    end if;

                    state <= TXF_DATA;
                  when others => null;
                end case;
              else
                q_sof   <= '0';         -- tx_valid == 0
                q_valid <= '0';
              end if;

-------------------------------------------------------------------------------
-- TX FSM state PAD: pads the pause frame to 64 bytes.
-------------------------------------------------------------------------------

            when TXF_PAD =>

              counter <= counter + 1;

              if(counter = x"1e") then
                state   <= TXF_WAIT_CRC;
                q_valid <= '0';
              else
                tx_data <= x"0000";
                q_valid <= '1';
              end if;

-------------------------------------------------------------------------------
-- TX FSM state DATA: trasmits frame payload
-------------------------------------------------------------------------------
              
            when TXF_DATA =>

              if(tx_eof_p1_i = '1') then
                state      <= TXF_WAIT_CRC;
                tx_ready_t <= '0';
              end if;

              if(tx_valid_i = '1') then
                if(tx_ctrl_i = c_wrsw_ctrl_tx_oob) then
                  oob_fid_value_o     <= tx_data_i (15 downto 0);
                  oob_fid_stb_o       <= '1';
                  q_valid             <= '0';
                  crc_gen_enable_mask <= '0';
                elsif (tx_ctrl_i = c_wrsw_ctrl_payload) then
                  tx_data    <= tx_data_i;
                  q_valid    <= '1';
                  q_bytesel  <= tx_bytesel_i;
                  write_mask <= not tx_bytesel_i;
                  odd_length <= tx_bytesel_i;
                elsif(tx_ctrl_i = c_wrsw_ctrl_rx_oob) then
                  q_valid <= '0';
                  state   <= TXF_WAIT_CRC;
                end if;
              else
                q_valid <= '0';
              end if;


-------------------------------------------------------------------------------
-- TX FSM states: WAIT_CRC, EMBED_CRC: dealing with frame checksum field
-------------------------------------------------------------------------------            
              
            when TXF_WAIT_CRC =>
              oob_fid_stb_o       <= '0';
              q_valid             <= '0';
              state               <= TXF_EMBED_CRC1;
              crc_gen_enable_mask <= '0';
              
            when TXF_EMBED_CRC1 =>
              if(odd_length = '1') then  -- CRC at odd position
                tx_data(7 downto 0) <= crc_value(31 downto 24);
              else                       -- CRC at even position
                tx_data(15 downto 0) <= crc_value(31 downto 16);
              end if;

              q_valid    <= '1';
              write_mask <= '1';
              q_bytesel  <= '0';
              state      <= TXF_EMBED_CRC2;
              
            when TXF_EMBED_CRC2 =>
              if(odd_length = '1') then  -- CRC at odd position
                tx_data(15 downto 0) <= crc_value(23 downto 8);
                state                <= TXF_EMBED_CRC3;
              else                       -- CRC at even position
                q_eof                <= '1';
                tx_data(15 downto 0) <= crc_value(15 downto 0);

                counter    <= (others => '0');
                tx_ready_t <= '0';
                state      <= TXF_GAP;
              end if;

            when TXF_EMBED_CRC3 =>
              tx_data(15 downto 8) <= crc_value(7 downto 0);
              q_bytesel            <= '1';
              q_eof                <= '1';

              counter    <= (others => '0');
              tx_ready_t <= '0';
              state      <= TXF_GAP;

-------------------------------------------------------------------------------
-- TX FSM states: WAIT_CRC, EMBED_CRC: dealing with frame checksum field
-------------------------------------------------------------------------------            
              
            when TXF_GAP =>
              q_abort        <= '0';
              q_valid        <= '0';
              tx_terror_p1_o <= '0';
              q_bytesel      <= '0';

              if(counter = c_IFG_LENGTH) then
                if(pcs_busy_i = '0') then
                  state <= TXF_IDLE;
                end if;
              else
                counter <= counter + 1;
              end if;

-------------------------------------------------------------------------------
-- TX FSM state ABORT: signalize underlying PCS block to abort the frame
-- immediately, corrupting its contents
-------------------------------------------------------------------------------            
            when TXF_ABORT =>
              q_sof   <= '0';
              q_valid <= '1';
              q_abort <= '1';

              counter <= (others => '0');
              state   <= TXF_GAP;

            when others => null;
          end case;
        end if;
      end if;
    end if;

  end process;

  tx_dreq_o <= tx_ready_t or (not regs_b.ecr_tx_en);  -- /dev/null if disabled

  pcs_data_o <= f_encode_fabric_int(
    tx_data,
    q_sof,
    q_eof,
    q_bytesel,
    q_abort);

  pcs_valid_o <= q_sof or (q_valid and write_mask);
  
end behavioral;

