-------------------------------------------------------------------------------
-- Title      : 1000base-X MAC/Endpoint
-- Project    : White Rabbit Switch
-------------------------------------------------------------------------------
-- File       : ep_tx_framer.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2009-06-22
-- Last update: 2011-08-22
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
use work.genram_pkg.all;
use work.wr_fabric_pkg.all;
use work.endpoint_private_pkg.all;
use work.ep_wbgen2_pkg.all;

entity ep_tx_framer is
  generic(
    g_with_vlans       : boolean;
    g_with_timestamper : boolean
    );

  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

------------------------------------------------------------------------------
-- Physical Coding Sublayer (PCS) interface
------------------------------------------------------------------------------

    pcs_fab_o : out t_ep_internal_fabric;
    pcs_error_i : in  std_logic;
    pcs_busy_i  : in  std_logic;
    pcs_dreq_i  : in  std_logic;

-------------------------------------------------------------------------------
-- WRF Sink (see WRF specification for the details)
-------------------------------------------------------------------------------    

    snk_i : in  t_wrf_sink_in;
    snk_o : out t_wrf_sink_out;

-------------------------------------------------------------------------------
-- Flow Control Unit signals
-------------------------------------------------------------------------------    

    
-- TX send pause frame - when active, the framer will send a PAUSE frame
-- as soon as possible. The pause quanta must be provided on tx_pause_delay_i input.
    fc_pause_p_i       : in std_logic;
    fc_pause_delay_i : in std_logic_vector(15 downto 0);

-- TX send pause acknowledge - active after the current pause send request has
-- been completed
    fc_pause_ack_o : out std_logic;

-- When active, the framer will allow for packet transmission.
    fc_flow_enable_i : in std_logic;

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

  constant c_IFG_LENGTH : integer := 1;

  type t_tx_framer_state is (TXF_IDLE, TXF_ADDR, TXF_PAUSE, TXF_QHEADER, TXF_DATA, TXF_OOB, TXF_WAIT_CRC, TXF_EMBED_CRC1, TXF_EMBED_CRC2, TXF_EMBED_CRC3, TXF_GAP, TXF_PAD, TXF_ABORT);

-- general signals
  signal state    : t_tx_framer_state;
  signal counter  : unsigned(7 downto 0);
  signal tx_ready : std_logic;

-- CRC generator signals
  signal crc_gen_reset                       : std_logic;
  signal crc_gen_force_reset                 : std_logic;
  signal crc_gen_enable, crc_gen_enable_mask : std_logic;
  signal crc_value                           : std_logic_vector(31 downto 0);

-- Framer <-> PCS FIFO signals
  signal q_bytesel : std_logic;
  signal q_valid   : std_logic;
  signal q_data    : std_logic_vector(15 downto 0);
  signal q_sof     : std_logic;
  signal q_eof     : std_logic;
  signal q_abort   : std_logic;

  signal write_mask : std_logic;
  signal odd_length : std_logic;

-- Flow Control-related signals
  signal tx_pause_mode  : std_logic;
  signal tx_pause_delay : std_logic_vector(15 downto 0);

  signal snk_valid : std_logic;

  signal sof_p1, eof_p1, abort_p1, error_p1 : std_logic;
  signal snk_cyc_d0                         : std_logic;

  signal decoded_status : t_wrf_status_reg;
  signal stored_status  : t_wrf_status_reg;

  type t_oob_fsm_state is (OOB_IDLE, OOB_1, OOB_2);

  signal oob_state : t_oob_fsm_state;
  signal oob       : t_wrf_oob;

  signal snk_out : t_wrf_sink_out;

  signal abort_now : std_logic;
  signal stall_int : std_logic;
  signal stall_int_d0: std_logic;
  signal untagging : std_logic;



  function b2s (x : boolean)
    return std_logic is
  begin
    if(x) then
      return '1';
    else
      return '0';
    end if;
  end function;



  signal vut_rd_vid    : integer;
  signal vut_wr_vid    : integer;
  signal vut_untag     : std_logic;
  signal vut_is_802_1q : std_logic;

  signal vut_stored_tag       : std_logic_vector(15 downto 0);
  signal vut_stored_ethertype : std_logic_vector(15 downto 0);
  
  
begin  -- behavioral

  -----------------------------------------------------------------------------
  -- VLAN Functionality
  -----------------------------------------------------------------------------

  gen_with_vlans : if(g_with_vlans) generate

    vut_rd_vid <= to_integer(unsigned(snk_i.dat(11 downto 0)));
    vut_wr_vid <= to_integer(unsigned(regs_b.vcr1_vid_o));

    p_untagged_set_access : process(clk_sys_i)
      variable vut_set : std_logic_vector(4095 downto 0);
    begin
      if rising_edge(clk_sys_i) then
        if(regs_b.vcr1_value_wr_o = '1') then
          vut_set(vut_wr_vid) := regs_b.vcr1_value_o;
        end if;

        if(snk_valid = '1') then
          vut_untag <= vut_set(vut_rd_vid);
        end if;
      end if;
    end process;

  end generate gen_with_vlans;

  regs_b <= c_ep_registers_init_value;

  crc_gen_reset <= '1' when rst_n_i = '0' else ((sof_p1 and (not tx_pause_mode)) or crc_gen_force_reset);

  crc_gen_enable <= q_valid and crc_gen_enable_mask;

  U_tx_crc_generator : gc_crc_gen
    generic map (
      g_polynomial              => x"04C11DB7",
      g_init_value              => x"ffffffff",
      g_residue                 => x"38fb2284",
      g_data_width              => 16,
      g_half_width              => 8,
      g_sync_reset              => 1,
      g_dual_width              => 1,
      g_registered_match_output => false)
    port map (
      clk_i   => clk_sys_i,
      rst_i   => crc_gen_reset,
      en_i    => crc_gen_enable,
      half_i  => q_bytesel,
      data_i  => q_data,
      match_o => open,
      crc_o   => crc_value);

  
  p_detect_frame : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        snk_cyc_d0 <= '0';
      else
        snk_cyc_d0 <= snk_i.cyc;
      end if;
    end if;
  end process;

  sof_p1    <= not snk_cyc_d0 and snk_i.cyc;
  eof_p1    <= snk_cyc_d0 and not snk_i.cyc;
  snk_valid <= (snk_i.cyc and snk_i.stb and snk_i.we) and not stall_int;

  decoded_status <= f_unmarshall_wrf_status(snk_i.dat);

  error_p1 <= snk_valid and b2s(snk_i.adr = c_WRF_STATUS) and decoded_status.error;

  abort_now <= '1' when (state /= TXF_IDLE and state /= TXF_GAP) and (regs_b.ecr_tx_en_o = '0' or error_p1 = '1') else '0';


  p_store_status : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      
      if rst_n_i = '0' then
        stored_status.has_smac <= '0';
        stored_status.is_hp    <= '0';
        stored_status.has_crc  <= '0';
      else
        if(snk_valid = '1' and snk_i.adr = c_WRF_STATUS) then
          stored_status <= decoded_status;
        end if;
      end if;
    end if;
  end process;

  gen_ts_supported : if(g_with_timestamper) generate
    p_oob_fsm : process(clk_sys_i)
    begin
      if rising_edge(clk_sys_i) then
        if (rst_n_i = '0') then
          oob_state    <= OOB_IDLE;
          oob.valid    <= '0';
          oob.oob_type <= (others => '0');
        else
          
          case oob_state is
            when OOB_IDLE =>
              if sof_p1 = '1' then
                oob_state <= OOB_1;
                OOB.valid <= '0';
              end if;
            when OOB_1 =>
              if(snk_valid = '1' and snk_i.adr = c_WRF_OOB) then
                oob.oob_type <= snk_i.dat(15 downto 12);
                oob_state    <= OOB_2;
              end if;
            when OOB_2 =>
              if(snk_valid = '1' and snk_i.adr = c_WRF_OOB) then
                oob.frame_id <= snk_i.dat(15 downto 0);
                oob_state    <= OOB_IDLE;
                oob.valid    <= '1';
              end if;
          end case;
        end if;
      end if;
    end process;
  end generate gen_ts_supported;

  p_tx_fsm : process (clk_sys_i)
  begin  -- process
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0') then
        state      <= TXF_IDLE;
        write_mask <= '0';

        q_valid   <= '0';
        q_data    <= (others => '0');
        q_bytesel <= '0';
        q_abort   <= '0';
        q_sof     <= '0';
        q_eof     <= '0';

        tx_ready      <= '0';
        tx_pause_mode <= '0';

        snk_out.err <= '0';
        snk_out.rty <= '0';

        fc_pause_ack_o <= '0';

        crc_gen_enable_mask <= '1';
        crc_gen_force_reset <= '0';

        oob_fid_stb_o <= '0';

      else

        -- we are in the middle of the frame and the framer has got suddenly
        -- disabled or we've received an ABORT command or an error occured in the PCS:

        if(abort_now = '1') then
          -- abort the current frame
          state    <= TXF_ABORT;
          tx_ready <= '0';
          q_sof    <= '0';
          q_valid  <= '0';
          
        elsif (state /= TXF_IDLE and pcs_error_i = '1') then
          tx_ready    <= '0';
          counter     <= (others => '0');
          snk_out.err <= '1';
          state       <= TXF_GAP;
        else

          case state is

-------------------------------------------------------------------------------
-- TX FSM state IDLE: awaits incoming TX frames
-------------------------------------------------------------------------------

            when TXF_IDLE =>            -- idle state - wait for the next frame

              snk_out.err <= '0';
              snk_out.rty <= '0';

              q_abort <= '0';

              tx_ready <= fc_flow_enable_i;

                                        -- Check start-of-frame and send-pause signals and eventually
                                        -- commence frame transmission

              if(pcs_dreq_i = '1' and (sof_p1 = '1' or fc_pause_p_i = '1') and regs_b.ecr_tx_en_o = '1') then
                                        -- enable writing to PCS FIFO
                q_sof      <= '1';
                q_eof      <= '0';
                write_mask <= '1';


                fc_pause_ack_o <= fc_pause_p_i;
                tx_pause_mode  <= fc_pause_p_i;
                tx_pause_delay <= fc_pause_delay_i;

                crc_gen_force_reset <= '1';
                crc_gen_enable_mask <= '1';

                counter <= (others => '0');
                if(fc_pause_p_i = '1') then
                  state <= TXF_PAUSE;
                else
                  state <= TXF_ADDR;
                end if;
                
              else
                write_mask <= '0';
                q_valid    <= '0';
              end if;

--            when TXF_FLOW_BLOCKED => none 


-------------------------------------------------------------------------------
-- TX FSM state HEADER: processes the frame header
-------------------------------------------------------------------------------
            when TXF_PAUSE =>
              tx_ready            <= '0';
              fc_pause_ack_o      <= '0';
              q_sof               <= '0';
              crc_gen_force_reset <= '0';

              if(pcs_dreq_i = '1') then
                q_valid <= '1';
                case counter(3 downto 0) is
                  when x"0" => q_data <= x"0180";
                  when x"1" => q_data <= x"c200";
                  when x"2" => q_data <= x"0001";
                  when x"3" => q_data <= regs_b.mach_o;
                  when x"4" => q_data <= regs_b.macl_o(31 downto 16);
                  when x"5" => q_data <= regs_b.macl_o(15 downto 0);
                  when x"6" => q_data <= x"8808";
                  when x"7" => q_data <= tx_pause_delay;
                               state <= TXF_PAD;
                  when others => null;
                end case;
                counter <= counter + 1;
              else
                q_valid <= '0';
              end if;

            when TXF_ADDR =>
              q_sof               <= '0';
              crc_gen_force_reset <= '0';

              if (pcs_dreq_i = '1' and snk_valid = '1' and snk_i.adr = c_WRF_DATA) then
                counter <= counter + 1;
                case counter(3 downto 0) is
                  when x"0" => q_data <= snk_i.dat; q_valid <= '1';
                  when x"1" => q_data <= snk_i.dat; q_valid <= '1';
                  when x"2" => q_data <= snk_i.dat; q_valid <= '1';
                  when x"3" =>
                    if(stored_status.has_smac = '1') then
                      q_data <= snk_i.dat;
                    else
                      q_data <= regs_b.mach_o;
                    end if;
                    q_valid <= '1';
                  when x"4" =>
                    if(stored_status.has_smac = '1') then
                      q_data <= snk_i.dat;
                    else
                      q_data <= regs_b.macl_o(31 downto 16);
                    end if;
                    q_valid <= '1';
                  when x"5" =>
                    if(stored_status.has_smac = '1') then
                      q_data <= snk_i.dat;
                    else
                      q_data <= regs_b.macl_o(15 downto 0);
                    end if;
                    q_valid <= '1';
                  when x"6" =>
                    if(g_with_vlans and snk_i.dat = x"8100") then  -- 802.1q frame
                      q_data  <= (others => 'X');
                      q_valid <= '0';
                      counter <= (others => '0');
                      state   <= TXF_QHEADER;
                    else
                      q_data  <= snk_i.dat;
                      q_valid <= '1';
                      state   <= TXF_DATA;
                    end if;
                  when others => null;
                end case;
                
              else
                q_valid <= '0';
                q_data  <= (others => 'X');
              end if;

            when TXF_QHEADER =>
              if(g_with_vlans and pcs_dreq_i = '1') then
                case counter(3 downto 0) is
                  when x"0" =>          -- TPID
                    if(snk_valid = '1' and snk_i.adr = c_WRF_DATA) then
                      counter <= counter + 1;
                      vut_stored_ethertype <= snk_i.dat;
                    end if;

                  when x"1" =>          -- VLAN Tag
                    if(snk_valid = '1' and snk_i.adr = c_WRF_DATA) then
                      counter             <= counter + 1;
                      vut_stored_tag <= snk_i.dat;
                      tx_ready <= '0';
                    end if;

                  when x"2" =>

                    if(vut_untag = '1' or untagging = '1') then  -- VID is in the untagged set
                      untagging           <= '1';
                      tx_ready <= '1';
                      q_data              <= vut_stored_ethertype;
                      q_valid             <= '1';
                      counter  <= counter  +1;
                    else
                      untagging           <= '0';
                      q_data              <= x"8100";
                      q_valid             <= '1';
                      counter             <= counter + 1;
                    end if;

                  when x"3" =>
                    if(untagging = '1') then
                      if(snk_valid = '1') then
                        q_data <= snk_i.dat;
                        q_valid <= '1';
                        state <= TXF_DATA;
                      else
                        q_valid <= '0';
                      end if;
                    elsif(untagging = '0') then
                      q_data  <= vut_stored_ethertype;
                      q_valid <= '1';
                      counter <= counter + 1;
                    else
                      q_valid  <= '0';
                      tx_ready <= '1';
                    end if;
                    
                  when x"4" =>
                    if(untagging = '0') then
                      q_data   <= vut_stored_tag;
                      q_valid  <= '1';
                      tx_ready <= '1';
                      state <= TXF_DATA;
                    end if;
                  when others => null;
                end case;
              else
                q_valid <= '0';
              end if;



-------------------------------------------------------------------------------
-- TX FSM state PAD: pads a pause frame to 64 bytes.
-------------------------------------------------------------------------------

            when TXF_PAD =>

              counter <= counter + 1;

              if(counter = x"1e") then
                state   <= TXF_WAIT_CRC;
                q_valid <= '0';
              else
                q_data  <= x"0000";
                q_valid <= '1';
              end if;

-------------------------------------------------------------------------------
-- TX FSM state DATA: trasmits frame payload
-------------------------------------------------------------------------------

            when TXF_DATA =>

              tx_ready <= '1';
              
              if(eof_p1 = '1') then

                if(stored_status.has_crc = '0') then
                  state <= TXF_WAIT_CRC;
                else
                  q_eof   <= '1';
                  counter <= (others => '0');
                  state   <= TXF_GAP;
                end if;

                tx_ready <= '0';

                                        -- check if we have an OOB block
                if(oob.valid = '1' and oob.oob_type = c_WRF_OOB_TYPE_TX and g_with_timestamper) then
                  oob_fid_stb_o <= '1';
                end if;
              end if;

              if(snk_valid = '1' and snk_i.adr = c_WRF_DATA) then
                q_data     <= snk_i.dat;
                q_valid    <= '1';
                q_bytesel  <= not snk_i.sel(0);  --tx_bytesel_i;
                write_mask <= snk_i.sel(0);
                odd_length <= not snk_i.sel(0);  --tx_bytesel_i;
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
                q_data(7 downto 0) <= crc_value(31 downto 24);
              else                       -- CRC at even position
                q_data(15 downto 0) <= crc_value(31 downto 16);
              end if;

              q_valid    <= '1';
              write_mask <= '1';
              q_bytesel  <= '0';
              state      <= TXF_EMBED_CRC2;

            when TXF_EMBED_CRC2 =>
              if(odd_length = '1') then  -- CRC at odd position
                q_data(15 downto 0) <= crc_value(23 downto 8);
                state               <= TXF_EMBED_CRC3;
              else                       -- CRC at even position
                q_eof               <= '1';
                q_data(15 downto 0) <= crc_value(15 downto 0);

                counter  <= (others => '0');
                tx_ready <= '0';
                state    <= TXF_GAP;
              end if;

            when TXF_EMBED_CRC3 =>
              q_data(15 downto 8) <= crc_value(7 downto 0);
              q_bytesel           <= '1';
              q_eof               <= '1';

              counter  <= (others => '0');
              tx_ready <= '0';
              state    <= TXF_GAP;

-------------------------------------------------------------------------------
-- TX FSM states: WAIT_CRC, EMBED_CRC: dealing with frame checksum field
-------------------------------------------------------------------------------            

            when TXF_GAP =>
              q_eof       <= '0';
              q_abort     <= '0';
              q_valid     <= '0';
              snk_out.err <= '0';
              snk_out.rty <= '0';
              q_bytesel   <= '0';

              if(oob.valid = '1') then
                if(pcs_busy_i = '0') then
                  state <= TXF_IDLE;
                end if;
                else
                  state <= TXF_IDLE;
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

  oob_fid_value_o <= oob.frame_id;


  stall_int <= not (pcs_dreq_i and tx_ready) and regs_b.ecr_tx_en_o;  -- /dev/null if disabled

  snk_out.stall <= stall_int;

  p_gen_ack : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      snk_out.ack <= snk_valid;
    end if;
  end process;

  snk_o <= snk_out;

  pcs_fab_o.data <= q_data;
  pcs_fab_o.sof <= q_sof;
  pcs_fab_o.eof <= q_eof;
  pcs_fab_o.bytesel <= q_bytesel;
  pcs_fab_o.error <= q_abort;
  pcs_fab_o.dvalid <= (q_valid and write_mask);

end behavioral;

