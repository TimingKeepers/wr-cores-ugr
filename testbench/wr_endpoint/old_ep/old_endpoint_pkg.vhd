library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package old_endpoint_pkg is
  
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
  constant c_QMODE_PORT_NONE   : std_logic_vector(1 downto 0) := "11";


  constant c_WRF_STATUS : std_logic_vector(1 downto 0) := "11";
  constant c_WRF_DATA   : std_logic_vector(1 downto 0) := "00";
  constant c_WRF_OOB    : std_logic_vector(1 downto 0) := "01";

  -- fixme: remove these along with the non-WB version of the endpoint
  constant c_wrsw_ctrl_none : std_logic_vector(4 - 1 downto 0) := x"0";
  constant c_wrsw_ctrl_dst_mac   : std_logic_vector(4 - 1 downto 0) := x"1";
  constant c_wrsw_ctrl_src_mac   : std_logic_vector(4 - 1 downto 0) := x"2";
  constant c_wrsw_ctrl_ethertype : std_logic_vector(4 - 1 downto 0) := x"3";
  constant c_wrsw_ctrl_vid_prio  : std_logic_vector(4 - 1 downto 0) := x"4";
  constant c_wrsw_ctrl_tx_oob    : std_logic_vector(4 - 1 downto 0) := x"5";
  constant c_wrsw_ctrl_rx_oob    : std_logic_vector(4 - 1 downto 0) := x"6";
  constant c_wrsw_ctrl_payload   : std_logic_vector(4 - 1 downto 0) := x"7";
  constant c_wrsw_ctrl_fcs       : std_logic_vector(4 - 1 downto 0) := x"8";


  
  type t_wrf_status_reg is record
    is_hp       : std_logic;
    has_smac    : std_logic;
    has_crc     : std_logic;
    rx_error    : std_logic;
    match_class : std_logic_vector(7 downto 0);
  end record;

  component old_ep_wishbone_controller
    port (
      rst_n_i               : in  std_logic;
      wb_clk_i              : in  std_logic;
      wb_addr_i             : in  std_logic_vector(5 downto 0);
      wb_data_i             : in  std_logic_vector(31 downto 0);
      wb_data_o             : out std_logic_vector(31 downto 0);
      wb_cyc_i              : in  std_logic;
      wb_sel_i              : in  std_logic_vector(3 downto 0);
      wb_stb_i              : in  std_logic;
      wb_we_i               : in  std_logic;
      wb_ack_o              : out std_logic;
      tx_clk_i              : in  std_logic;
      ep_ecr_portid_o       : out std_logic_vector(4 downto 0);
      ep_ecr_rst_cnt_o      : out std_logic;
      ep_ecr_tx_en_fra_o    : out std_logic;
      ep_ecr_rx_en_fra_o    : out std_logic;
      ep_tscr_en_txts_o     : out std_logic;
      ep_tscr_en_rxts_o     : out std_logic;
      ep_tscr_cs_start_o    : out std_logic;
      ep_tscr_cs_done_i     : in  std_logic;
      ep_rfcr_a_runt_o      : out std_logic;
      ep_rfcr_a_giant_o     : out std_logic;
      ep_rfcr_a_hp_o        : out std_logic;
      ep_rfcr_a_frag_o      : out std_logic;
      ep_rfcr_qmode_o       : out std_logic_vector(1 downto 0);
      ep_rfcr_fix_prio_o    : out std_logic;
      ep_rfcr_prio_val_o    : out std_logic_vector(2 downto 0);
      ep_rfcr_vid_val_o     : out std_logic_vector(11 downto 0);
      ep_fcr_rxpause_o      : out std_logic;
      ep_fcr_txpause_o      : out std_logic;
      ep_fcr_tx_thr_o       : out std_logic_vector(7 downto 0);
      ep_fcr_tx_quanta_o    : out std_logic_vector(15 downto 0);
      ep_mach_o             : out std_logic_vector(15 downto 0);
      ep_macl_o             : out std_logic_vector(31 downto 0);
      ep_dmcr_en_o          : out std_logic;
      ep_dmcr_n_avg_o       : out std_logic_vector(11 downto 0);
      ep_dmsr_ps_val_i      : in  std_logic_vector(23 downto 0);
      ep_dmsr_ps_rdy_o      : out std_logic;
      ep_dmsr_ps_rdy_i      : in  std_logic;
      ep_dmsr_ps_rdy_load_o : out std_logic;
      ep_mdio_cr_data_o     : out std_logic_vector(15 downto 0);
      ep_mdio_cr_data_wr_o  : out std_logic;
      ep_mdio_cr_addr_o     : out std_logic_vector(7 downto 0);
      ep_mdio_cr_rw_o       : out std_logic;
      ep_mdio_sr_rdata_i    : in  std_logic_vector(15 downto 0);
      ep_mdio_sr_ready_i    : in  std_logic;
      ep_dsr_lstatus_i      : in  std_logic;
      ep_dsr_lact_o         : out std_logic;
      ep_dsr_lact_i         : in  std_logic;
      ep_dsr_lact_load_o    : out std_logic;
      ep_rmon_ram_addr_i    : in  std_logic_vector(4 downto 0);
      ep_rmon_ram_data_o    : out std_logic_vector(31 downto 0);
      ep_rmon_ram_rd_i      : in  std_logic;
      ep_rmon_ram_data_i    : in  std_logic_vector(31 downto 0);
      ep_rmon_ram_wr_i      : in  std_logic);
  end component;


  component old_ep_rmon_counters
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

  component old_ep_tx_framer
    port (
      clk_sys_i             : in  std_logic;
      rst_n_i               : in  std_logic;
      pcs_data_o            : out std_logic_vector(15 downto 0);
      pcs_bytesel_o         : out std_logic;
      pcs_sof_o             : out std_logic;
      pcs_eof_o             : out std_logic;
      pcs_abort_o           : out std_logic;
      pcs_error_i           : in  std_logic;
      pcs_busy_i            : in  std_logic;
      pcs_fifo_write_o      : out std_logic;
      pcs_fifo_almostfull_i : in  std_logic;
      tx_data_i             : in  std_logic_vector(15 downto 0);
      tx_ctrl_i             : in  std_logic_vector(4 -1 downto 0);
      tx_bytesel_i          : in  std_logic;
      tx_sof_p1_i           : in  std_logic;
      tx_eof_p1_i           : in  std_logic;
      tx_dreq_o             : out std_logic;
      tx_valid_i            : in  std_logic;
      tx_rerror_p1_i        : in  std_logic;
      tx_tabort_p1_i        : in  std_logic;
      tx_terror_p1_o        : out std_logic;
      tx_pause_i            : in  std_logic;
      tx_pause_delay_i      : in  std_logic_vector(15 downto 0);
      tx_pause_ack_o        : out std_logic;
      tx_flow_enable_i      : in  std_logic;
      oob_fid_value_o       : out std_logic_vector(15 downto 0);
      oob_fid_stb_o         : out std_logic;
      ep_tcr_en_fra_i       : in  std_logic;
      ep_rfcr_qmode_i       : in  std_logic_vector(1 downto 0);
      ep_macl_i             : in  std_logic_vector(31 downto 0);
      ep_mach_i             : in  std_logic_vector(15 downto 0));
  end component;


  component old_ep_rx_deframer
    port (
      clk_sys_i            : in  std_logic;
      rst_n_i              : in  std_logic;
      pcs_data_i           : in  std_logic_vector(15 downto 0);
      pcs_bytesel_i        : in  std_logic;
      pcs_sof_i            : in  std_logic;
      pcs_eof_i            : in  std_logic;
      pcs_dreq_o           : out std_logic;
      pcs_valid_i          : in  std_logic;
      pcs_error_i          : in  std_logic;
      oob_data_i           : in  std_logic_vector(47 downto 0);
      oob_valid_i          : in  std_logic;
      oob_ack_o            : out std_logic;
      rbuf_sof_p1_o        : out std_logic;
      rbuf_eof_p1_o        : out std_logic;
      rbuf_ctrl_o          : out std_logic_vector(4 - 1 downto 0);
      rbuf_data_o          : out std_logic_vector(15 downto 0);
      rbuf_bytesel_o       : out std_logic;
      rbuf_valid_o         : out std_logic;
      rbuf_drop_i          : in  std_logic;
      rbuf_rerror_p1_o     : out std_logic;
      fc_pause_p_o         : out std_logic;
      fc_pause_delay_o     : out std_logic_vector(15 downto 0);
      rmon_rx_crc_err_p_o  : out std_logic;
      rmon_rx_ok_p_o       : out std_logic;
      rmon_rx_runt_p_o     : out std_logic;
      rmon_rx_giant_p_o    : out std_logic;
      rmon_rx_pcs_err_p_o  : out std_logic;
      rmon_rx_buf_drop_p_o : out std_logic;
      rmon_rx_rtu_drop_p_o : out std_logic;
      ep_rfcr_qmode_i      : in  std_logic_vector(1 downto 0);
      ep_rcr_en_fra_i      : in  std_logic;
      ep_tscr_en_rxts_i    : in  std_logic;
      ep_rfcr_a_runt_i     : in  std_logic;
      ep_rfcr_a_giant_i    : in  std_logic;
      ep_rfcr_fix_prio_i   : in  std_logic;
      ep_rfcr_prio_val_i   : in  std_logic_vector(2 downto 0);
      ep_rfcr_vid_val_i    : in  std_logic_vector(11 downto 0);
      rtu_rq_smac_o        : out std_logic_vector(48 - 1 downto 0);
      rtu_rq_dmac_o        : out std_logic_vector(48 - 1 downto 0);
      rtu_rq_vid_o         : out std_logic_vector(12 - 1 downto 0);
      rtu_rq_has_vid_o     : out std_logic;
      rtu_rq_prio_o        : out std_logic_vector(3 - 1 downto 0);
      rtu_rq_has_prio_o    : out std_logic;
      rtu_full_i           : in  std_logic;
      rtu_rq_strobe_p1_o   : out std_logic);
  end component;





  component old_ep_timestamping_unit
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

  component old_ep_flow_control
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

  component old_ep_rx_buffer
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


  component old_ep_enc_8b10b
    port (
      clk_i     : in  std_logic;
      rst_n_i   : in  std_logic;
      ctrl_i    : in  std_logic;
      in_8b_i   : in  std_logic_vector(7 downto 0);
      err_o     : out std_logic;
      dispar_o  : out std_logic;
      out_10b_o : out std_logic_vector(9 downto 0));
  end component;

  component old_ep_dec_8b10b
    port (
      clk_i       : in  std_logic;
      rst_n_i     : in  std_logic;
      in_10b_i    : in  std_logic_vector(9 downto 0);
      ctrl_o      : out std_logic;
      code_err_o  : out std_logic;
      rdisp_err_o : out std_logic;
      out_8b_o    : out std_logic_vector(7 downto 0));
  end component;

  component old_ep_1000basex_pcs
    generic (
      g_simulation : integer;
      g_phy_mode   : string);
    port (
      rst_n_i                 : in  std_logic;
      clk_sys_i               : in  std_logic;
      rxpcs_busy_o            : out std_logic;
      rxpcs_data_o            : out std_logic_vector(15 downto 0);
      rxpcs_bytesel_o         : out std_logic;
      rxpcs_sof_o             : out std_logic;
      rxpcs_eof_o             : out std_logic;
      rxpcs_error_o           : out std_logic;
      rxpcs_dreq_i            : in  std_logic;
      rxpcs_valid_o           : out std_logic;
      rxpcs_timestamp_stb_p_o : out std_logic;
      txpcs_data_i            : in  std_logic_vector(15 downto 0);
      txpcs_bytesel_i         : in  std_logic;
      txpcs_sof_i             : in  std_logic;
      txpcs_eof_i             : in  std_logic;
      txpcs_abort_i           : in  std_logic;
      txpcs_error_p_o         : out std_logic;
      txpcs_busy_o            : out std_logic;
      txpcs_valid_i           : in  std_logic;
      txpcs_fifo_almostfull_o : out std_logic;
      txpcs_timestamp_stb_p_o : out std_logic;
      link_ok_o               : out std_logic;
      tbi_rbclk_i             : in  std_logic;
      tbi_rxdata_i            : in  std_logic_vector(9 downto 0);
      tbi_txclk_i             : in  std_logic;
      tbi_txdata_o            : out std_logic_vector(9 downto 0);
      tbi_syncen_o            : out std_logic;
      tbi_loopen_o            : out std_logic;
      tbi_prbsen_o            : out std_logic;
      tbi_enable_o            : out std_logic;
      gtp_tx_clk_i            : in  std_logic;
      gtp_tx_data_o           : out std_logic_vector(7 downto 0);
      gtp_tx_k_o              : out std_logic;
      gtp_tx_disparity_i      : in  std_logic;
      gtp_tx_enc_err_i        : in  std_logic;
      gtp_rx_data_i           : in  std_logic_vector(7 downto 0);
      gtp_rx_clk_i            : in  std_logic;
      gtp_rx_k_i              : in  std_logic;
      gtp_rx_enc_err_i        : in  std_logic;
      gtp_rx_bitslide_i       : in  std_logic_vector(3 downto 0);
      gtp_rst_o               : out std_logic;
      gtp_loopen_o            : out std_logic;
      rmon_syncloss_p_o       : out std_logic;
      rmon_invalid_code_p_o   : out std_logic;
      rmon_rx_overrun_p_o     : out std_logic;
      rmon_tx_underrun_o      : out std_logic;
      mdio_addr_i             : in  std_logic_vector(7 downto 0);
      mdio_data_i             : in  std_logic_vector(15 downto 0);
      mdio_data_o             : out std_logic_vector(15 downto 0);
      mdio_stb_i              : in  std_logic;
      mdio_rw_i               : in  std_logic;
      mdio_ready_o            : out std_logic);
  end component;

  component old_wrsw_endpoint
    generic (
      g_simulation          : integer := 0;
      g_phy_mode            : string  := "TBI";
      g_rx_buffer_size_log2 : integer);
    port (
      clk_ref_i          : in  std_logic;
      clk_sys_i          : in  std_logic;
      clk_dmtd_i         : in  std_logic;
      rst_n_i            : in  std_logic;
      pps_csync_p1_i     : in  std_logic;
      tbi_td_o           : out std_logic_vector(9 downto 0);
      tbi_enable_o       : out std_logic;
      tbi_syncen_o       : out std_logic;
      tbi_loopen_o       : out std_logic;
      tbi_prbsen_o       : out std_logic;
      tbi_rbclk_i        : in  std_logic                    := '0';
      tbi_rd_i           : in  std_logic_vector(9 downto 0) := "0000000000";
      tbi_sync_pass_i    : in  std_logic                    := '0';
      gtp_tx_clk_i       : in  std_logic                    := '0';
      gtp_tx_data_o      : out std_logic_vector(7 downto 0);
      gtp_tx_k_o         : out std_logic;
      gtp_tx_disparity_i : in  std_logic                    := '0';
      gtp_tx_enc_err_i   : in  std_logic                    := '0';
      gtp_rx_data_i      : in  std_logic_vector(7 downto 0) := "00000000";
      gtp_rx_clk_i       : in  std_logic                    := '0';
      gtp_rx_k_i         : in  std_logic                    := '0';
      gtp_rx_enc_err_i   : in  std_logic                    := '0';
      gtp_rx_bitslide_i  : in  std_logic_vector(3 downto 0) := "0000";
      gtp_rst_o          : out std_logic;
      gtp_loopen_o       : out std_logic;
      rx_data_o          : out std_logic_vector(15 downto 0);
      rx_ctrl_o          : out std_logic_vector(4 - 1 downto 0);
      rx_bytesel_o       : out std_logic;
      rx_sof_p1_o        : out std_logic;
      rx_eof_p1_o        : out std_logic;
      rx_dreq_i          : in  std_logic;
      rx_valid_o         : out std_logic;
      rx_rabort_p1_i     : in  std_logic;
      rx_idle_o          : out std_logic;
      rx_rerror_p1_o     : out std_logic;
      tx_data_i          : in  std_logic_vector(15 downto 0);
      tx_ctrl_i          : in  std_logic_vector(4 -1 downto 0);
      tx_bytesel_i       : in  std_logic;
      tx_sof_p1_i        : in  std_logic;
      tx_eof_p1_i        : in  std_logic;
      tx_dreq_o          : out std_logic;
      tx_valid_i         : in  std_logic;
      tx_rerror_p1_i     : in  std_logic;
      tx_tabort_p1_i     : in  std_logic;
      tx_terror_p1_o     : out std_logic;
      txtsu_port_id_o    : out std_logic_vector(4 downto 0);
      txtsu_frame_id_o   : out std_logic_vector(16 -1 downto 0);
      txtsu_tsval_o      : out std_logic_vector(28 + 4 - 1 downto 0);
      txtsu_valid_o      : out std_logic;
      txtsu_ack_i        : in  std_logic;
      rtu_full_i         : in  std_logic                    := '0';
      rtu_almost_full_i  : in  std_logic                    := '0';
      rtu_rq_strobe_p1_o : out std_logic;
      rtu_rq_smac_o      : out std_logic_vector(48 - 1 downto 0);
      rtu_rq_dmac_o      : out std_logic_vector(48 - 1 downto 0);
      rtu_rq_vid_o       : out std_logic_vector(12 - 1 downto 0);
      rtu_rq_has_vid_o   : out std_logic;
      rtu_rq_prio_o      : out std_logic_vector(3-1 downto 0);
      rtu_rq_has_prio_o  : out std_logic;
      wb_cyc_i           : in  std_logic;
      wb_stb_i           : in  std_logic;
      wb_we_i            : in  std_logic;
      wb_sel_i           : in  std_logic_vector(3 downto 0);
      wb_addr_i          : in  std_logic_vector(5 downto 0);
      wb_data_i          : in  std_logic_vector(31 downto 0);
      wb_data_o          : out std_logic_vector(31 downto 0);
      wb_ack_o           : out std_logic);
  end component;

  function f_marshall_wrf_status (stat  : t_wrf_status_reg) return std_logic_vector;
  function f_unmarshall_wrf_status(stat : std_logic_vector) return t_wrf_status_reg;
  
end old_endpoint_pkg;

-------------------------------------------------------------------------------

package body old_endpoint_pkg is

  function f_marshall_wrf_status(stat : t_wrf_status_reg)
    return std_logic_vector is
    variable tmp : std_logic_vector(15 downto 0);
  begin
    tmp(0)           := stat.is_hp;
    tmp(1)           := stat.rx_error;
    tmp(2)           := stat.has_smac;
    tmp(15 downto 8) := stat.match_class;
    return tmp;
  end function;

  function f_unmarshall_wrf_status(stat : std_logic_vector) return t_wrf_status_reg is
    variable tmp : t_wrf_status_reg;
  begin
    tmp.is_hp       := stat(0);
    tmp.rx_error    := stat(1);
    tmp.has_smac    := stat(2);
    tmp.match_class := stat(15 downto 8);
    return tmp;
    
  end function;
    
end old_endpoint_pkg;

-------------------------------------------------------------------------------
