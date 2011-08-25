library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.ep_wbgen2_pkg.all;
use work.wr_fabric_pkg.all;

package endpoint_private_pkg is
  
  constant c_endpoint_rx_buffer_size      : integer := 4096;
  constant c_endpoint_rx_buffer_size_log2 : integer := 12;

  -- special/control characters
  constant c_k28_5 : std_logic_vector(7 downto 0) := "10111100";  -- bc
  constant c_k23_7 : std_logic_vector(7 downto 0) := "11110111";  -- f7
  constant c_k27_7 : std_logic_vector(7 downto 0) := "11111011";  -- fb
  constant c_k29_7 : std_logic_vector(7 downto 0) := "11111101";  -- fd
  constant c_k30_7 : std_logic_vector(7 downto 0) := "11111110";  -- fe
  constant c_k28_7 : std_logic_vector(7 downto 0) := "11111100";  -- fc
  constant c_d21_5 : std_logic_vector(7 downto 0) := "10110101";  -- b5

  constant c_d2_2          : std_logic_vector(7 downto 0) := "01000010";  -- 42
  constant c_d5_6          : std_logic_vector(7 downto 0) := "11000101";  -- c5
  constant c_d16_2         : std_logic_vector(7 downto 0) := "01010000";  -- 50
  constant c_preamble_char : std_logic_vector(7 downto 0) := "01010101";
  constant c_preamble_sfd  : std_logic_vector(7 downto 0) := "11010101";

  constant c_QMODE_PORT_ACCESS : std_logic_vector(1 downto 0) := "00";
  constant c_QMODE_PORT_TRUNK  : std_logic_vector(1 downto 0) := "01";
  constant c_QMODE_PORT_UNQUALIFIED   : std_logic_vector(1 downto 0) := "11";
  constant c_QMODE_PORT_VLAN_DISABLED   : std_logic_vector(1 downto 0) := "10";

  -- fixme: remove these along with the non-WB version of the endpoint
  constant c_wrsw_ctrl_none      : std_logic_vector(4 - 1 downto 0) := x"0";
  constant c_wrsw_ctrl_dst_mac   : std_logic_vector(4 - 1 downto 0) := x"1";
  constant c_wrsw_ctrl_src_mac   : std_logic_vector(4 - 1 downto 0) := x"2";
  constant c_wrsw_ctrl_ethertype : std_logic_vector(4 - 1 downto 0) := x"3";
  constant c_wrsw_ctrl_vid_prio  : std_logic_vector(4 - 1 downto 0) := x"4";
  constant c_wrsw_ctrl_tx_oob    : std_logic_vector(4 - 1 downto 0) := x"5";
  constant c_wrsw_ctrl_rx_oob    : std_logic_vector(4 - 1 downto 0) := x"6";
  constant c_wrsw_ctrl_payload   : std_logic_vector(4 - 1 downto 0) := x"7";
  constant c_wrsw_ctrl_fcs       : std_logic_vector(4 - 1 downto 0) := x"8";

  type t_rmon_triggers is record
    rx_sync_lost      : std_logic;
    rx_invalid_code   : std_logic;
    rx_overrun        : std_logic;
    rx_crc_err        : std_logic;
    rx_ok             : std_logic;
    rx_runt           : std_logic;
    rx_giant          : std_logic;
    rx_pause          : std_logic;
    rx_pcs_err        : std_logic;
    rx_buffer_overrun : std_logic;
    rx_rtu_overrun    : std_logic;

    tx_pause    : std_logic;
    tx_underrun : std_logic;
  end record;

  -- Endpoint's internal fabric used to connect the submodules with each other.
  -- Easier to handle than pipelined Wishbone.
  type t_ep_internal_fabric is record
    sof     : std_logic;
    eof     : std_logic;
    error   : std_logic;
    dvalid  : std_logic;
    bytesel : std_logic;
    data    : std_logic_vector(15 downto 0);
  end record;



  component ep_1000basex_pcs
    generic (
      g_simulation : boolean);
    port (
      rst_n_i                 : in    std_logic;
      clk_sys_i               : in    std_logic;
      rxpcs_fab_o             : out   t_ep_internal_fabric;
      rxpcs_busy_o            : out   std_logic;
      rxpcs_dreq_i            : in    std_logic;
      rxpcs_timestamp_stb_p_o : out   std_logic;
      txpcs_fab_i             : in    t_ep_internal_fabric;
      txpcs_error_o           : out   std_logic;
      txpcs_busy_o            : out   std_logic;
      txpcs_dreq_o            : out   std_logic;
      txpcs_timestamp_stb_p_o : out   std_logic;
      link_ok_o               : out   std_logic;
      serdes_rst_o            : out   std_logic;
      serdes_syncen_o         : out   std_logic;
      serdes_loopen_o         : out   std_logic;
      serdes_prbsen_o         : out   std_logic;
      serdes_enable_o         : out   std_logic;
      serdes_tx_clk_i         : in    std_logic;
      serdes_tx_data_o        : out   std_logic_vector(7 downto 0);
      serdes_tx_k_o           : out   std_logic;
      serdes_tx_disparity_i   : in    std_logic;
      serdes_tx_enc_err_i     : in    std_logic;
      serdes_rx_data_i        : in    std_logic_vector(7 downto 0);
      serdes_rx_clk_i         : in    std_logic;
      serdes_rx_k_i           : in    std_logic;
      serdes_rx_enc_err_i     : in    std_logic;
      serdes_rx_bitslide_i    : in    std_logic_vector(3 downto 0);
      rmon_o                  : inout t_rmon_triggers;
      mdio_addr_i             : in    std_logic_vector(15 downto 0);
      mdio_data_i             : in    std_logic_vector(15 downto 0);
      mdio_data_o             : out   std_logic_vector(15 downto 0);
      mdio_stb_i              : in    std_logic;
      mdio_rw_i               : in    std_logic;
      mdio_ready_o            : out   std_logic);
  end component;

 
  component ep_rmon_counters
    generic (
      g_num_counters   : integer;
      g_ram_addr_width : integer);
    port (
      clk_sys_i       : in  std_logic;
      rst_n_i         : in  std_logic;
      cntr_rst_i      : in  std_logic;
      cntr_pulse_i    : in  std_logic_vector(g_num_counters-1 downto 0);
      ram_addr_o      : out std_logic_vector(g_ram_addr_width-1 downto 0);
      ram_data_i      : in  std_logic_vector(31 downto 0);
      ram_data_o      : out std_logic_vector(31 downto 0);
      ram_wr_o        : out std_logic;
      cntr_overflow_o : out std_logic);
  end component;

  component ep_rx_deframer
    generic (
      g_with_vlans          : boolean;
      g_with_dpi_classifier : boolean;
      g_with_rtu            : boolean);
    port (
      clk_sys_i          : in    std_logic;
      rst_n_i            : in    std_logic;
      pcs_fab_i          : in    t_ep_internal_fabric;
      pcs_dreq_o         : out   std_logic;
      pcs_busy_i         : in    std_logic;
      oob_data_i         : in    std_logic_vector(47 downto 0);
      oob_valid_i        : in    std_logic;
      oob_ack_o          : out   std_logic;
      rbuf_sof_p1_o      : out   std_logic;
      rbuf_eof_p1_o      : out   std_logic;
      rbuf_bytesel_o     : out   std_logic;
      rbuf_is_oob_o      : out   std_logic;
      rbuf_dat_o         : out   std_logic_vector(15 downto 0);
      rbuf_we_o          : out   std_logic;
      rbuf_full_i        : in    std_logic;
      rbuf_accept_o      : out   std_logic;
      rbuf_drop_o        : out   std_logic;
      tmp_src_o          : out   t_wrf_source_out;
      tmp_src_i          : in    t_wrf_source_in;
      fc_pause_p_o       : out   std_logic;
      fc_pause_delay_o   : out   std_logic_vector(15 downto 0);
      rmon_o             : inout t_rmon_triggers;
      regs_b             : inout t_ep_registers;
      rtu_rq_smac_o      : out   std_logic_vector(48 - 1 downto 0);
      rtu_rq_dmac_o      : out   std_logic_vector(48 - 1 downto 0);
      rtu_rq_vid_o       : out   std_logic_vector(12 - 1 downto 0);
      rtu_rq_has_vid_o   : out   std_logic;
      rtu_rq_prio_o      : out   std_logic_vector(3 - 1 downto 0);
      rtu_rq_has_prio_o  : out   std_logic;
      rtu_full_i         : in    std_logic;
      rtu_rq_strobe_p1_o : out   std_logic);
  end component;

  component ep_tx_framer
    generic (
      g_with_vlans       : boolean;
      g_with_timestamper : boolean);
    port (
      clk_sys_i        : in    std_logic;
      rst_n_i          : in    std_logic;
      pcs_fab_o        : out   t_ep_internal_fabric;
      pcs_error_i      : in    std_logic;
      pcs_busy_i       : in    std_logic;
      pcs_dreq_i       : in    std_logic;
      snk_i            : in    t_wrf_sink_in;
      snk_o            : out   t_wrf_sink_out;
      fc_pause_p_i     : in    std_logic;
      fc_pause_delay_i : in    std_logic_vector(15 downto 0);
      fc_pause_ack_o   : out   std_logic;
      fc_flow_enable_i : in    std_logic;
      oob_fid_value_o  : out   std_logic_vector(15 downto 0);
      oob_fid_stb_o    : out   std_logic;
      regs_b           : inout t_ep_registers);
  end component;

  component ep_timestamping_unit
    generic (
      g_timestamp_bits_r : natural;
      g_timestamp_bits_f : natural);
    port (
      clk_ref_i            : in  std_logic;
      clk_sys_i            : in  std_logic;
      rst_n_i              : in  std_logic;
      pps_csync_p1_i       : in  std_logic;
      tx_timestamp_stb_p_i : in  std_logic;
      rx_timestamp_stb_p_i : in  std_logic;
      txoob_fid_i          : in  std_logic_vector(16 - 1 downto 0);
      txoob_stb_p_i        : in  std_logic;
      rxoob_data_o         : out std_logic_vector(47 downto 0);
      rxoob_valid_o        : out std_logic;
      rxoob_ack_i          : in  std_logic;
      txtsu_port_id_o      : out std_logic_vector(4 downto 0);
      txtsu_fid_o          : out std_logic_vector(16 -1 downto 0);
      txtsu_tsval_o        : out std_logic_vector(28 + 4 - 1 downto 0);
      txtsu_valid_o        : out std_logic;
      txtsu_ack_i          : in  std_logic;
      ep_tscr_en_txts_i    : in  std_logic;
      ep_tscr_en_rxts_i    : in  std_logic;
      ep_tscr_cs_start_i   : in  std_logic;
      ep_tscr_cs_done_o    : out std_logic;
      ep_ecr_portid_i      : in  std_logic_vector(4 downto 0));
  end component;

  component ep_flow_control
    port (
      clk_sys_i          : in  std_logic;
      rst_n_i            : in  std_logic;
      rx_pause_p1_i      : in  std_logic;
      rx_pause_delay_i   : in  std_logic_vector(15 downto 0);
      tx_pause_o         : out std_logic;
      tx_pause_delay_o   : out std_logic_vector(15 downto 0);
      tx_pause_ack_i     : in  std_logic;
      tx_flow_enable_o   : out std_logic;
      rx_buffer_used_i   : in  std_logic_vector(7 downto 0);
      ep_fcr_txpause_i   : in  std_logic;
      ep_fcr_rxpause_i   : in  std_logic;
      ep_fcr_tx_thr_i    : in  std_logic_vector(7 downto 0);
      ep_fcr_tx_quanta_i : in  std_logic_vector(15 downto 0);
      rmon_rcvd_pause_o  : out std_logic;
      rmon_sent_pause_o  : out std_logic);
  end component;

  component ep_rx_buffer
    generic (
      g_size_log2 : integer);
    port (
      clk_sys_i          : in  std_logic;
      rst_n_i            : in  std_logic;
      fra_data_i         : in  std_logic_vector(15 downto 0);
      fra_ctrl_i         : in  std_logic_vector(4 -1 downto 0);
      fra_sof_p_i        : in  std_logic;
      fra_eof_p_i        : in  std_logic;
      fra_error_p_i      : in  std_logic;
      fra_valid_i        : in  std_logic;
      fra_drop_o         : out std_logic;
      fra_bytesel_i      : in  std_logic;
      fab_data_o         : out std_logic_vector(15 downto 0);
      fab_ctrl_o         : out std_logic_vector(4 -1 downto 0);
      fab_sof_p_o        : out std_logic;
      fab_eof_p_o        : out std_logic;
      fab_error_p_o      : out std_logic;
      fab_valid_o        : out std_logic;
      fab_bytesel_o      : out std_logic;
      fab_dreq_i         : in  std_logic;
      ep_ecr_rx_en_fra_i : in  std_logic;
      buffer_used_o      : out std_logic_vector(7 downto 0);
      rmon_rx_overflow_o : out std_logic);
  end component;

  component ep_wishbone_controller
    port (
      rst_n_i            : in    std_logic;
      wb_clk_i           : in    std_logic;
      wb_addr_i          : in    std_logic_vector(5 downto 0);
      wb_data_i          : in    std_logic_vector(31 downto 0);
      wb_data_o          : out   std_logic_vector(31 downto 0);
      wb_cyc_i           : in    std_logic;
      wb_sel_i           : in    std_logic_vector(3 downto 0);
      wb_stb_i           : in    std_logic;
      wb_we_i            : in    std_logic;
      wb_ack_o           : out   std_logic;
      tx_clk_i           : in    std_logic;
      ep_rmon_ram_addr_i : in    std_logic_vector(4 downto 0);
      ep_rmon_ram_data_o : out   std_logic_vector(31 downto 0);
      ep_rmon_ram_rd_i   : in    std_logic;
      ep_rmon_ram_data_i : in    std_logic_vector(31 downto 0);
      ep_rmon_ram_wr_i   : in    std_logic;
      regs_b             : inout t_ep_registers);
  end component;

  component ep_rx_bypass_queue
    generic (
      g_size  : integer;
      g_width : integer);
    port (
      rst_n_i : in  std_logic;
      clk_i   : in  std_logic;
      d_i     : in  std_logic_vector(g_width-1 downto 0);
      valid_i : in  std_logic;
      dreq_o  : out std_logic;
      q_o     : out std_logic_vector(g_width-1 downto 0);
      valid_o : out std_logic;
      dreq_i  : in  std_logic;
      flush_i : in  std_logic;
      purge_i : in  std_logic);
  end component;

  function f_pack_fifo_contents (
    data      : std_logic_vector;
    sof       : std_logic;
    eof       : std_logic;
    bytesel   : std_logic;
    error     : std_logic;
    early_eof : boolean := false
    ) return std_logic_vector;

  function f_fifo_is_data(data        : in std_logic_vector; valid : in std_logic; early_eof : boolean := false) return std_logic;
  function f_fifo_is_sof(data         : in std_logic_vector; valid : in std_logic; early_eof : boolean := false) return std_logic;
  function f_fifo_is_eof(data         : in std_logic_vector; valid : in std_logic; early_eof : boolean := false) return std_logic;
  function f_fifo_is_error(data       : in std_logic_vector; valid : in std_logic; early_eof : boolean := false) return std_logic;
  function f_fifo_is_single_byte(data : in std_logic_vector; valid : in std_logic; early_eof : boolean := false) return std_logic;
  
end endpoint_private_pkg;

-------------------------------------------------------------------------------

package body endpoint_private_pkg is


  -- FIFO "packer" function. Packs frame data and status signals into a single
  -- 18-bit word, matching the optimum data width of most FPGA RAMs/FIFOs. (2x
  -- 8+1 bits)
  function f_pack_fifo_contents (
    data      : std_logic_vector;
    sof       : std_logic;
    eof       : std_logic;
    bytesel   : std_logic;
    error     : std_logic;
    early_eof : boolean := false) return std_logic_vector is

    variable dout : std_logic_vector(17 downto 0);
  begin

    -- the encodings are slightly different:
    -- - if early_eof == 1, the target needs the EOF information along with the last data word.
    --   This is the case for ep_tx_pcs_tbi.
    -- - if early_eof == 0, EOF is an independent transfer
    if(early_eof) then
      if(sof = '1' or error = '1') then
        -- tag = 01
        dout(17 downto 16) := "01";
        dout(15)           := sof;
        dout(14)           := 'X';
        dout(13)           := error;
        dout(12 downto 0)  := (others => 'X');
      elsif(eof = '1') then
        -- tag = 1x
        dout(17)          := '1';
        dout(16)          := bytesel;
        dout(15 downto 0) := data;
      else
        -- tag = 00
        dout(17)          := '0';
        dout(16)          := '0';
        dout(15 downto 0) := data;
      end if;
    else
      if(sof = '1' or error = '1' or eof = '1') then
        -- tag = 01
        dout(16)          := '1';
        dout(15)          := sof;
        dout(14)          := eof;
        dout(13)          := error;
        dout(12 downto 0) := (others => 'X');
      else
        dout(17)          := bytesel;
        dout(16)          := '0';
        dout(15 downto 0) := data;
      end if;

    end if;
    return dout;
  end f_pack_fifo_contents;

  function f_fifo_is_data(data      : in std_logic_vector;
                          valid     : in std_logic;
                          early_eof :    boolean := false)
    return std_logic is
  begin
    if(early_eof) then
      return not (not data(17) and data(16)) and valid;
    else
      return not data(16) and valid;
    end if;
  end f_fifo_is_data;


  function f_fifo_is_sof
    (data      : in std_logic_vector;
     valid     : in std_logic;
     early_eof :    boolean := false) return std_logic is
  begin
    
    if (early_eof and valid = '1' and data(17) = '0' and data(16) = '1' and data(15) = '1') then
      return '1';
    elsif(not early_eof and valid = '1' and data(16) = '1' and data(15) = '1') then
      return '1';
    else
      return '0';
    end if;
  end f_fifo_is_sof;

  function f_fifo_is_eof
    (data      : in std_logic_vector;
     valid     : in std_logic;
     early_eof :    boolean := false) return std_logic is
  begin
    if (early_eof and valid = '1' and data(17) = '1') then
      return '1';
    elsif(not early_eof and valid = '1' and data(16) = '1' and data(14) = '1') then
      return '1';
    else
      return '0';
    end if;
  end f_fifo_is_eof;

  function f_fifo_is_error
    (data      : in std_logic_vector;
     valid     : in std_logic;
     early_eof :    boolean := false) return std_logic is
  begin
    if (early_eof and valid = '1' and data(17) = '0' and data(16) = '1' and data(13) = '1') then
      return '1';
    elsif(not early_eof and valid = '1' and data(16) = '1' and data(13) = '1') then
      return '1';
    else
      return '0';
    end if;
  end f_fifo_is_error;

  function f_fifo_is_single_byte
    (data      : in std_logic_vector;
     valid     : in std_logic;
     early_eof :    boolean := false) return std_logic is
  begin
    if (early_eof and valid = '1' and data(17) = '1') then
      return data(16);
    elsif(not early_eof and valid = '1' and data(16) = '0') then
      return data(17);
    else
      return '0';
    end if;
  end f_fifo_is_single_byte;

end endpoint_private_pkg;

-------------------------------------------------------------------------------
