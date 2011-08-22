-------------------------------------------------------------------------------
-- Title      : 1000base-X MAC/Endpoint 
-- Project    : White Rabbit Switch 
-------------------------------------------------------------------------------
-- File       : ep_rx_deframer.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2009-06-22
-- Last update: 2011-08-20
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
use work.ep_wbgen2_pkg.all;
use work.wr_fabric_pkg.all;

entity ep_rx_deframer is
  generic (
    g_with_vlans          : boolean;
    g_with_dpi_classifier : boolean;
    g_with_rtu            : boolean);
  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

-- physical coding sublayer (PCS) interface
    pcs_fab_i :in t_ep_internal_fabric;
    pcs_dreq_o  : out std_logic;
    pcs_busy_i  : in  std_logic;

-- OOB frame tag value and strobing signal
    oob_data_i  : in  std_logic_vector(47 downto 0);
    oob_valid_i : in  std_logic;
    oob_ack_o   : out std_logic;

-- RX Buffer interface
    rbuf_sof_p1_o  : out std_logic;
    rbuf_eof_p1_o  : out std_logic;
    rbuf_bytesel_o : out std_logic;
    rbuf_is_oob_o  : out std_logic;
    rbuf_dat_o     : out std_logic_vector(15 downto 0);
    rbuf_we_o      : out std_logic;
    rbuf_full_i    : in  std_logic;
    rbuf_accept_o  : out std_logic;
    rbuf_drop_o    : out std_logic;

    tmp_src_o : out t_wrf_source_out;
    tmp_src_i : in  t_wrf_source_in;

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
      clk_sys_i  : in    std_logic;
      rst_n_i    : in    std_logic;
      snk_fab_i  : in    t_ep_internal_fabric;
      snk_dreq_o : out   std_logic;
      src_fab_o  : out   t_ep_internal_fabric;
      src_dreq_i : in    std_logic;
      rmon_o     : inout t_rmon_triggers;
      regs_b     : inout t_ep_registers);
  end component;
  
  type t_rx_deframer_state is (RXF_IDLE, RXF_DATA, RXF_FLUSH_STALL, RXF_FINISH_CYCLE, RXF_THROW_ERROR);

  signal state : t_rx_deframer_state;

  signal gap_cntr : unsigned(3 downto 0);

  -- new sigs
  signal counter : unsigned(7 downto 0);

  signal rxdata_saved : std_logic_vector(15 downto 0);
  signal next_hdr     : std_logic;
  signal is_pause     : std_logic;

  signal data_firstword : std_logic;
  signal snk_dreq_int   : std_logic;
  signal snk_dreq   : std_logic;

 
  signal flush_stall : std_logic;
  signal stb_int : std_logic;
  signal fab_int : t_ep_internal_fabric;
  
  signal ack_count   : unsigned(7 downto 0);
  signal src_out_int : t_wrf_source_out;

  signal tmp_sel : std_logic;
  signal tmp_dat : std_logic_vector(15 downto 0);
  
  
begin  -- behavioral
  regs_b <= c_ep_registers_init_value;

  U_crc_size_checker : ep_rx_crc_size_check
    port map (
      clk_sys_i   => clk_sys_i,
      rst_n_i     => rst_n_i,
      snk_fab_i   => pcs_fab_i,
      snk_dreq_o  => pcs_dreq_o,
      src_dreq_i  => snk_dreq,
      src_fab_o  =>fab_int,
      rmon_o      => rmon_o,
      regs_b      => regs_b);

  
  snk_dreq <= not tmp_src_i.stall;

  p_count_acks : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        ack_count <= (others => '0');
      else
        if(src_out_int.stb = '1' and tmp_src_i.stall = '0' and tmp_src_i.ack = '0') then
          ack_count <= ack_count + 1;
        elsif(tmp_src_i.ack = '1' and not( src_out_int.stb = '1' and tmp_src_i.stall = '0')) then
          ack_count <= ack_count - 1;
        end if;
        
      end if;
    end if;
  end process;


  process(clk_sys_i)
    variable stat:t_wrf_status_reg;
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        state           <= RXF_IDLE;
        stb_int <= '0';
        src_out_int.we  <= '1';
        src_out_int.adr <= c_WRF_DATA;
        src_out_int.cyc <= '0';
        flush_stall <= '0';
        
      else
        case state is
          when RXF_IDLE =>
            if(tmp_src_i.stall = '0' and fab_int.sof = '1') then
              src_out_int.cyc <= '1';
              state           <= RXF_DATA;
            end if;
            
          when RXF_DATA =>
            if(tmp_src_i.stall = '0') then
              src_out_int.dat <= fab_int.data;
              stb_int <= fab_int.dvalid;
              src_out_int.sel(1) <= '1';
              src_out_int.sel(0) <= not fab_int.bytesel;
            end if;

            if(tmp_src_i.stall = '1' and fab_int.dvalid = '1') then
              state <= RXF_FLUSH_STALL;
              tmp_dat <= fab_int.data;
              tmp_sel <= fab_int.bytesel;
            end if;
            
            if(fab_int.eof = '1')then
              state <= RXF_FINISH_CYCLE;
            end if;

            if(fab_int.error = '1') then
              state <= RXF_THROW_ERROR;
            end if;

          when RXF_FLUSH_STALL =>
            if(tmp_src_i.stall = '0') then
              src_out_int.dat <= tmp_dat;
              stb_int <= '1';
              src_out_int.sel(1) <= '1';
              src_out_int.sel(0) <= not tmp_sel;
              state <= RXF_DATA;
            end if;

          when RXF_FINISH_CYCLE =>
            if(tmp_src_i.stall = '0') then
              stb_int <= '0';
            end if;

            if(ack_count = 0) then
              src_out_int.cyc <= '0';
              state           <= RXF_IDLE;
            end if;
          when others => null;
        end case;
      end if;
    end if;
  end process;


  tmp_src_o.dat <= src_out_int.dat;
  tmp_src_o.sel <= src_out_int.sel;
  tmp_src_o.adr <= src_out_int.adr;
  src_out_int.stb <= stb_int;
  tmp_src_o.stb <= src_out_int.stb;
  tmp_src_o.we <= src_out_int.we;
  tmp_src_o.cyc <= src_out_int.cyc;
--  RX_FSM : process (clk_sys_i, rst_n_i)
--  begin
--    if rising_edge(clk_sys_i) then
--      if(rst_n_i = '0') then
--        state <= RXF_IDLE;

--        oob_ack_o <= '0';

--        rbuf_sof_p1_o    <= '0';
--        rbuf_eof_p1_o    <= '0';
--        rbuf_rerror_p1_o <= '0';
--        rbuf_ctrl_o      <= (others => '0');
--        rbuf_data_o      <= (others => '0');
--        rbuf_valid_o     <= '0';
--        rbuf_bytesel_o   <= '0';

--        rtu_rq_strobe_p1_o <= '0';
--        rtu_rq_smac_o      <= (others => '0');
--        rtu_rq_dmac_o      <= (others => '0');
--        rtu_rq_vid_o       <= (others => '0');
--        rtu_rq_has_vid_o   <= '0';
--        rtu_rq_prio_o      <= (others => '0');
--        rtu_rq_has_prio_o  <= '0';

--        rmon_o.rx_buffer_overrun <= '0';
--        rmon_o.rx_ok             <= '0';
--        rmon_o.rx_rtu_overrun    <= '0';

--        fc_pause_p_o     <= '0';
--        fc_pause_delay_o <= (others => '0');

--        snk_dreq_int <= '0';
--        next_hdr     <= '0';
--        is_pause     <= '1';

--        data_firstword <= '0';
--      else

--        case state is
--          when RXF_IDLE =>

--            oob_ack_o <= '0';

--            rbuf_rerror_p1_o <= '0';
--            rbuf_eof_p1_o    <= '0';
--            rbuf_valid_o     <= '0';

--            fc_pause_p_o <= '0';

--            rmon_o.rx_buffer_overrun <= '0';
--            rmon_o.rx_ok             <= '0';
--            rmon_o.rx_rtu_overrun    <= '0';

--            snk_dreq_int <= '1';

--            next_hdr       <= '0';
--            is_pause       <= '1';
--            counter        <= (others => '0');
--            data_firstword <= '1';

--            if(regs_b.ecr_rx_en_o = '1') then
--              if(fab_int.sof = '1') then
--                if(rbuf_drop_i = '0' and rtu_full_i = '0') then
--                  state         <= RXF_HEADER;
--                  rbuf_sof_p1_o <= '1';
--                else
--                  rmon_o.rx_buffer_overrun <= rbuf_drop_i;
--                  rmon_o.rx_rtu_overrun    <= rtu_full_i;
--                  rbuf_sof_p1_o            <= '0';
--                end if;
--              else
--                rbuf_sof_p1_o <= '0';
--              end if;
--            end if;

--          when RXF_HEADER =>
--            rbuf_sof_p1_o <= '0';

--            if(fab_int.error = '1') then
--              state <= RXF_ERROR;
--            end if;

--            if(rbuf_drop_i = '1') then
--              state                    <= RXF_ERROR;
--              rmon_o.rx_buffer_overrun <= '1';
--            end if;

--            if(fab_int.dvalid = '1' or next_hdr = '1') then

--              counter <= counter + 1;

--              case counter is
--                when x"00" =>           -- DST MAC ADDR
--                  if(fab_int.data /= x"0180") then
--                    is_pause <= '0';
--                  end if;

--                  rtu_rq_dmac_o(47 downto 32) <= fab_int.data;
--                  rbuf_ctrl_o                 <= c_wrsw_ctrl_dst_mac;
--                  rbuf_data_o                 <= fab_int.data;
--                  rbuf_valid_o                <= '1';
--                  snk_dreq_int                <= '1';
--                when x"01" =>
--                  if(fab_int.data /= x"c200") then
--                    is_pause <= '0';
--                  end if;

--                  rtu_rq_dmac_o(31 downto 16) <= fab_int.data;
--                  rbuf_ctrl_o                 <= c_wrsw_ctrl_dst_mac;
--                  rbuf_data_o                 <= fab_int.data;
--                  rbuf_valid_o                <= '1';
--                  snk_dreq_int                <= '1';
--                when x"02" =>
--                  if(fab_int.data /= x"0001") then
--                    is_pause <= '0';
--                  end if;

--                  rtu_rq_dmac_o(15 downto 0) <= fab_int.data;
--                  rbuf_ctrl_o                <= c_wrsw_ctrl_dst_mac;
--                  rbuf_data_o                <= fab_int.data;
--                  rbuf_valid_o               <= '1';
--                  snk_dreq_int               <= '1';
--                when x"03" =>                     -- SRC MAC ADDR
--                  rtu_rq_smac_o(47 downto 32) <= fab_int.data;
--                  rbuf_ctrl_o                 <= c_wrsw_ctrl_src_mac;
--                  rbuf_data_o                 <= fab_int.data;
--                  rbuf_valid_o                <= '1';
--                  snk_dreq_int                <= '1';
--                when x"04" =>
--                  rtu_rq_smac_o(31 downto 16) <= fab_int.data;
--                  rbuf_ctrl_o                 <= c_wrsw_ctrl_src_mac;
--                  rbuf_data_o                 <= fab_int.data;
--                  rbuf_valid_o                <= '1';
--                  snk_dreq_int                <= '1';
--                when x"05" =>
--                  rtu_rq_smac_o(15 downto 0) <= fab_int.data;
--                  rbuf_ctrl_o                <= c_wrsw_ctrl_src_mac;
--                  rbuf_data_o                <= fab_int.data;
--                  rbuf_valid_o               <= '1';
--                  snk_dreq_int               <= '0';
--                when x"06" =>                     -- ETHERTYPE
--                  if(fab_int.data = x"8808") then
--                    state        <= RXF_DATA;
--                    rbuf_data_o  <= fab_int.data;
--                    rbuf_ctrl_o  <= c_wrsw_ctrl_ethertype;
--                    rbuf_valid_o <= '1';
--                    snk_dreq_int <= '1';
--                  elsif(fab_int.data = x"8100") then  -- vlan frame
--                    is_pause <= '0';

---- case 1: got a VLAN-tagged frame on ACCESS port - drop it
--                    if(ep_rfcr_qmode_i = c_QMODE_PORT_ACCESS) then
--                      state        <= RXF_ERROR;
--                      rbuf_valid_o <= '0';
--                      snk_dreq_int <= '0';
--                    else
---- case 2: got a VLAN-tagged frame on a TRUNK or UNQUALIFIED port - pass it
--                      rtu_rq_has_prio_o <= '1';
--                      rtu_rq_has_vid_o  <= '1';
--                      rbuf_ctrl_o       <= c_wrsw_ctrl_none;
--                      rbuf_data_o       <= fab_int.data;
--                      snk_dreq_int      <= '1';
--                      rbuf_valid_o      <= '1';
--                    end if;

--                  else                  -- no vlan header
--                    rbuf_ctrl_o <= c_wrsw_ctrl_ethertype;
--                    is_pause    <= '0';
---- case 3: got a non-802.1q frame on ACCESS port: insert VLAN header
----         with appropriate VID/PCP
--                    if(ep_rfcr_qmode_i = c_QMODE_PORT_ACCESS) then
--                      -- insert VLAN header
--                      rxdata_saved      <= fab_int.data;
--                      snk_dreq_int      <= '0';
--                      rbuf_valid_o      <= '0';
--                      rtu_rq_has_vid_o  <= '1';
--                      rtu_rq_has_prio_o <= '1';
--                      rtu_rq_vid_o      <= ep_rfcr_vid_val_i;
--                      rtu_rq_prio_o     <= ep_rfcr_prio_val_i;
--                      next_hdr          <= '1';

--                    elsif (ep_rfcr_qmode_i = c_QMODE_PORT_TRUNK) then
---- case 4: got a non-802.1q frame on TRUNK port: drop

--                      state <= RXF_ERROR;
--                    else
---- case 5: unqualified port: pass.
--                      rbuf_data_o       <= fab_int.data;
--                      rbuf_valid_o      <= '1';
--                      snk_dreq_int      <= '1';
--                      rtu_rq_has_vid_o  <= '0';
--                      rtu_rq_has_prio_o <= ep_rfcr_fix_prio_i;
--                      rtu_rq_prio_o     <= ep_rfcr_prio_val_i;
--                      state             <= RXF_DATA;
--                    end if;


--                  end if;
--                when x"07" =>           -- new ethertype

--                  if(ep_rfcr_qmode_i = c_QMODE_PORT_ACCESS) then  -- access:
--                                                                  -- insert
--                    snk_dreq_int <= '0';
--                    rbuf_valid_o <= '1';
--                    rbuf_ctrl_o  <= c_wrsw_ctrl_none;
--                    rbuf_data_o  <= x"8100";
--                    next_hdr     <= '1';
--                  else
--                    rbuf_ctrl_o  <= c_wrsw_ctrl_ethertype;
--                    rbuf_data_o  <= fab_int.data;
--                    rbuf_valid_o <= '1';
--                    snk_dreq_int <= '1';
--                  end if;

--                when x"08" =>

--                  if(ep_rfcr_qmode_i = c_QMODE_PORT_ACCESS) then  -- access:
--                                                                  -- insert
--                                                                  -- vlan tag
----                    snk_dreq_int <= '0';
--                    rbuf_valid_o <= '1';
--                    rbuf_ctrl_o  <= c_wrsw_ctrl_ethertype;
--                    rbuf_data_o  <= rxdata_saved;  -- old ethertype
--                    next_hdr     <= '1';
--                  else
--                    rbuf_ctrl_o  <= c_wrsw_ctrl_vid_prio;
--                    rbuf_data_o  <= fab_int.data;
--                    rbuf_valid_o <= '1';
--                    state        <= RXF_DATA;
--                  end if;
--                  snk_dreq_int <= '1';


--                when x"09" =>
--                  rbuf_ctrl_o <= c_wrsw_ctrl_vid_prio;
--                  rbuf_data_o <= ep_rfcr_prio_val_i & '0' & ep_rfcr_vid_val_i;

--                  rbuf_valid_o <= '1';
--                  next_hdr     <= '0';
--                  state        <= RXF_DATA;
--                when others => null;
--              end case;

--            else
--              rbuf_valid_o <= '0';
--              snk_dreq_int <= '1';
--            end if;

--          when RXF_DATA =>

--            if(fab_int.error = '1') then
--              state <= RXF_ERROR;
--            end if;

--            if(rbuf_drop_i = '1') then
--              state                    <= RXF_ERROR;
--              rmon_o.rx_buffer_overrun <= '1';
--            end if;

--            rbuf_ctrl_o <= c_wrsw_ctrl_payload;

--            if(fab_int.eof = '1') then

--              if(oob_valid_i = '1' and regs_b.tscr_en_rxts_o = '1') then
--                state   <= RXF_OOB;
--                counter <= (others => '0');
--              else
--                rmon_o.rx_ok  <= '1';
--                rbuf_eof_p1_o <= '1';
--                state         <= RXF_IDLE;
--              end if;

--              snk_dreq_int <= '0';

--            end if;

--            -- got a valid word from the PCS
--            if(fab_int.dvalid = '1') then

--              if(data_firstword = '1') then
--                rtu_rq_strobe_p1_o <= not is_pause;
--                fc_pause_delay_o   <= fab_int.data;
--              -- record the PAUSE delay, it's the first 2 bytes of the payload
--              else
--                rtu_rq_strobe_p1_o <= '0';
--              end if;

--              data_firstword <= '0';

--              -- end-of-frame?
--              rbuf_data_o    <= fab_int.data;
--              rbuf_bytesel_o <= fab_int.bytesel;
--              rbuf_valid_o   <= '1';
--              snk_dreq_int   <= '1';
--            else
--              rbuf_valid_o <= '0';
--            end if;

--          when RXF_OOB =>
--            counter <= counter + 1;
--            if(rbuf_drop_i = '1') then
--              state                    <= RXF_ERROR;
--              rmon_o.rx_buffer_overrun <= '1';
--            else
--              case counter is
--                when x"00" =>
--                  rbuf_ctrl_o  <= c_wrsw_ctrl_rx_oob;
--                  rbuf_data_o  <= oob_data_i(47 downto 32);
--                  rbuf_valid_o <= '1';
--                when x"01" =>
--                  rbuf_ctrl_o  <= c_wrsw_ctrl_rx_oob;
--                  rbuf_data_o  <= oob_data_i(31 downto 16);
--                  rbuf_valid_o <= '1';
--                when x"02" =>
--                  rbuf_ctrl_o  <= c_wrsw_ctrl_rx_oob;
--                  rbuf_data_o  <= oob_data_i(15 downto 0);
--                  rbuf_valid_o <= '1';
--                  oob_ack_o    <= '1';
--                when x"03" =>
--                  rbuf_valid_o  <= '0';
--                  oob_ack_o     <= '0';
--                  rbuf_eof_p1_o <= '1';
--                  snk_dreq_int  <= '1';
--                  rmon_o.rx_ok  <= '1';
--                  state         <= RXF_IDLE;

--                when others => null;
--              end case;
--            end if;

--          when RXF_ERROR =>
--            fc_pause_p_o     <= '0';
--            rbuf_rerror_p1_o <= '1';
--            state            <= RXF_IDLE;
--          when others => null;
--        end case;


--      end if;
--    end if;
--  end process;

--  snk_dreq <= snk_dreq_int and not (fab_int.eof or fab_int.error);

end behavioral;

