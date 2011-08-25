-------------------------------------------------------------------------------
-- Title      : 1000base-X MAC/Endpoint 
-- Project    : White Rabbit Switch 
-------------------------------------------------------------------------------
-- File       : ep_rx_deframer.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-CO-HT
-- Created    : 2009-06-22
-- Last update: 2011-08-25
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
    g_with_vlans          : boolean:=true;  
    g_with_dpi_classifier : boolean:=true;
    g_with_rtu            : boolean:=true);
  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

-- physical coding sublayer (PCS) interface
    pcs_fab_i  : in  t_ep_internal_fabric;
    pcs_dreq_o : out std_logic;
    pcs_busy_i : in  std_logic;

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

  component ep_packet_filter
    port (
      clk_sys_i  : in    std_logic;
      rst_n_i    : in    std_logic;
      snk_fab_i  : in    t_ep_internal_fabric;
      snk_dreq_o : out   std_logic;
      src_fab_o  : out   t_ep_internal_fabric;
      src_dreq_i : in    std_logic;
      done_o     : out   std_logic;
      pclass_o   : out   std_logic_vector(7 downto 0);
      drop_o     : out   std_logic;
      regs_b     : inout t_ep_registers);
  end component;
  
  component ep_rx_early_address_match
    port (
      clk_sys_i            : in    std_logic;
      rst_n_i              : in    std_logic;
      snk_fab_i            : in    t_ep_internal_fabric;
      snk_dreq_o           : out   std_logic;
      src_fab_o            : out   t_ep_internal_fabric;
      src_dreq_i           : in    std_logic;
      match_done_o         : out   std_logic;
      match_is_hp_o        : out   std_logic;
      match_is_pause_o     : out   std_logic;
      match_pause_quanta_o : out   std_logic_vector(15 downto 0);
      regs_b               : inout t_ep_registers);
  end component;

  component ep_rx_vlan_unit
    port (
      clk_sys_i      : in    std_logic;
      rst_n_i        : in    std_logic;
      snk_fab_i      : in    t_ep_internal_fabric;
      snk_dreq_o     : out   std_logic;
      src_fab_o      : out   t_ep_internal_fabric;
      src_dreq_i     : in    std_logic;
      tclass_o       : out   std_logic_vector(2 downto 0);
      vid_o : out   std_logic_vector(11 downto 0);
      tag_done_o : out std_logic;
      rmon_o         : inout t_rmon_triggers;
      regs_b         : inout t_ep_registers);
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


  signal flush_stall : std_logic;
  signal stb_int     : std_logic;

  signal fab_int  : t_ep_internal_fabric;
  signal dreq_int : std_logic;

  signal ack_count   : unsigned(7 downto 0);
  signal src_out_int : t_wrf_source_out;

  signal tmp_sel : std_logic;
  signal tmp_dat : std_logic_vector(15 downto 0);


  type t_fab_pipe is array(integer range <>) of t_ep_internal_fabric;

  signal fab_pipe  : t_fab_pipe(0 to 4);
  signal dreq_pipe : std_logic_vector(4 downto 0);

  signal ematch_done         : std_logic;
  signal ematch_is_hp        : std_logic;
  signal ematch_is_pause     : std_logic;
  signal ematch_pause_quanta : std_logic_vector(15 downto 0);

  signal pfilter_pclass : std_logic_vector(7 downto 0);
  signal pfilter_drop   : std_logic;
  signal pfilter_done   : std_logic;
  
  signal vlan_tclass : std_logic_vector(2 downto 0);
  signal vlan_vid : std_logic_vector(11 downto 0);
  signal vlan_tag_done : std_logic;

  
begin  -- behavioral
  regs_b <= c_ep_registers_init_value;

  fab_pipe(0) <= pcs_fab_i;
  pcs_dreq_o  <= dreq_pipe(0);

  U_early_addr_match : ep_rx_early_address_match
    port map (
      clk_sys_i            => clk_sys_i,
      rst_n_i              => rst_n_i,
      snk_fab_i            => fab_pipe(0),
      snk_dreq_o           => dreq_pipe(0),
      src_fab_o            => fab_pipe(1),
      src_dreq_i           => dreq_pipe(1),
      match_done_o         => ematch_done,
      match_is_hp_o        => ematch_is_hp,
      match_is_pause_o     => ematch_is_pause,
      match_pause_quanta_o => ematch_pause_quanta,
      regs_b               => regs_b);

  gen_with_packet_filter : if(g_with_dpi_classifier) generate
    U_packet_filter : ep_packet_filter
      port map (
        clk_sys_i  => clk_sys_i,
        rst_n_i    => rst_n_i,
        snk_fab_i  => fab_pipe(1),
        snk_dreq_o => dreq_pipe(1),
        src_fab_o  => fab_pipe(2),
        src_dreq_i => dreq_pipe(2),
        done_o     => pfilter_done,
        pclass_o   => pfilter_pclass,
        drop_o     => pfilter_drop,
        regs_b     => regs_b);
  end generate gen_with_packet_filter;

  gen_without_packet_filter : if(not g_with_dpi_classifier) generate
    fab_pipe(2)  <= fab_pipe(1);
    dreq_pipe(1) <= dreq_pipe(2);
  end generate gen_without_packet_filter;

  U_crc_size_checker : ep_rx_crc_size_check
    port map (
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_i,
      snk_fab_i  => fab_pipe(2),
      snk_dreq_o => dreq_pipe(2),
      src_dreq_i => dreq_pipe(3),
      src_fab_o  => fab_pipe(3),
      rmon_o     => rmon_o,
      regs_b     => regs_b);


  gen_with_vlan_unit: if(g_with_vlans) generate
    U_vlan_unit: ep_rx_vlan_unit
      port map (
        clk_sys_i      => clk_sys_i,
        rst_n_i        => rst_n_i,
        snk_fab_i      => fab_pipe(3),
        snk_dreq_o     => dreq_pipe(3),
        src_fab_o      => fab_pipe(4),
        src_dreq_i     => dreq_pipe(4),
        tclass_o       => vlan_tclass,
        vid_o => vlan_vid,
        tag_done_o => vlan_tag_done,
        rmon_o         => rmon_o,
        regs_b         => regs_b);
  end generate gen_with_vlan_unit;
  

  fab_int <= fab_pipe(4);
  dreq_pipe(4) <= dreq_int;
  
  dreq_int <= not tmp_src_i.stall;

  p_count_acks : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        ack_count <= (others => '0');
      else
        if(src_out_int.stb = '1' and tmp_src_i.stall = '0' and tmp_src_i.ack = '0') then
          ack_count <= ack_count + 1;
        elsif(tmp_src_i.ack = '1' and not(src_out_int.stb = '1' and tmp_src_i.stall = '0')) then
          ack_count <= ack_count - 1;
        end if;
        
      end if;
    end if;
  end process;


  process(clk_sys_i)
    variable stat : t_wrf_status_reg;
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        state           <= RXF_IDLE;
        stb_int         <= '0';
        src_out_int.we  <= '1';
        src_out_int.adr <= c_WRF_DATA;
        src_out_int.cyc <= '0';
        flush_stall     <= '0';
        
      else
        case state is
          when RXF_IDLE =>
            src_out_int.adr <= c_WRF_DATA;

            if(tmp_src_i.stall = '0' and fab_int.sof = '1') then
              src_out_int.cyc <= '1';
              state           <= RXF_DATA;
              assert ematch_done = '1' report "EarlyMatchDone is 0 at SOF" severity failure;
            end if;
            
          when RXF_DATA =>
            if(tmp_src_i.stall = '0') then
              src_out_int.dat    <= fab_int.data;
              stb_int            <= fab_int.dvalid;
              src_out_int.sel(1) <= '1';
              src_out_int.sel(0) <= not fab_int.bytesel;
            end if;

            if(tmp_src_i.stall = '1' and fab_int.dvalid = '1') then
              state   <= RXF_FLUSH_STALL;
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
              src_out_int.dat    <= tmp_dat;
              stb_int            <= '1';
              src_out_int.sel(1) <= '1';
              src_out_int.sel(0) <= not tmp_sel;
              state              <= RXF_DATA;
            end if;

          when RXF_THROW_ERROR =>
            if(tmp_src_i.stall = '0') then
              stat.error := '1';
              src_out_int.adr <= c_WRF_STATUS;
              src_out_int.dat    <= f_marshall_wrf_status(stat);
              stb_int <= '1';
              state <= RXF_FINISH_CYCLE;        
            end if;
            
          when RXF_FINISH_CYCLE =>
            if(tmp_src_i.stall = '0') then
              stb_int <= '0';
            end if;

            if(ack_count = 0 and stb_int = '0') then
              src_out_int.cyc <= '0';
              state           <= RXF_IDLE;
            end if;
          when others => null;
        end case;
      end if;
    end if;
  end process;


  tmp_src_o.dat   <= src_out_int.dat;
  tmp_src_o.sel   <= src_out_int.sel;
  tmp_src_o.adr   <= src_out_int.adr;
  src_out_int.stb <= stb_int;
  tmp_src_o.stb   <= src_out_int.stb;
  tmp_src_o.we    <= src_out_int.we;
  tmp_src_o.cyc   <= src_out_int.cyc;

end behavioral;

