library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gencores_pkg.all;

entity wr_softpll is
  generic(
    g_deglitcher_threshold : integer;
    g_tag_bits             : integer
    );

  port(
    clk_sys_i  : in std_logic;
    rst_n_i    : in std_logic;
    clk_ref_i  : in std_logic;
    clk_dmtd_i : in std_logic;
    clk_rx_i   : in std_logic;

    dac_hpll_data_o : out std_logic_vector(15 downto 0);
    dac_hpll_load_o : out std_logic;

    dac_dmpll_data_o : out std_logic_vector(15 downto 0);
    dac_dmpll_load_o : out std_logic;

    wb_addr_i : in  std_logic_vector(3 downto 0);
    wb_data_i : in  std_logic_vector(31 downto 0);
    wb_data_o : out std_logic_vector(31 downto 0);
    wb_cyc_i  : in  std_logic;
    wb_sel_i  : in  std_logic_vector(3 downto 0);
    wb_stb_i  : in  std_logic;
    wb_we_i   : in  std_logic;
    wb_ack_o  : out std_logic;
    wb_irq_o  : out std_logic;
    debug_o   : out std_logic_vector(3 downto 0)
    );

end wr_softpll;

architecture rtl of wr_softpll is


  component multi_dmtd_with_deglitcher
    generic (
      g_counter_bits     : natural;
      g_log2_replication : natural);
    port (
      rst_n_dmtdclk_i      : in  std_logic;
      rst_n_sysclk_i       : in  std_logic;
      clk_dmtd_i           : in  std_logic;
      clk_sys_i            : in  std_logic;
      clk_in_i             : in  std_logic;
      tag_o                : out std_logic_vector(g_counter_bits-1 downto 0);
      tag_stb_p_o          : out std_logic;
      shift_en_i           : in  std_logic;
      shift_dir_i          : in  std_logic;
      deglitch_threshold_i : in  std_logic_vector(15 downto 0);
      dbg_dmtdout_o        : out std_logic);
  end component;

  component dmtd_with_deglitcher
    generic (
      g_counter_bits : natural);
    port (
      rst_n_dmtdclk_i      : in  std_logic;
      rst_n_sysclk_i       : in  std_logic;
      clk_in_i             : in  std_logic;
      clk_dmtd_i           : in  std_logic;
      clk_sys_i            : in  std_logic;
      shift_en_i           : in  std_logic;
      shift_dir_i          : in  std_logic;
      deglitch_threshold_i : in  std_logic_vector(15 downto 0);
      dbg_dmtdout_o        : out std_logic;
      tag_o                : out std_logic_vector(g_counter_bits-1 downto 0);
      tag_stb_p1_o         : out std_logic);
  end component;
  
  component softpll_wb
    port (
      rst_n_i            : in  std_logic;
      wb_clk_i           : in  std_logic;
      wb_addr_i          : in  std_logic_vector(3 downto 0);
      wb_data_i          : in  std_logic_vector(31 downto 0);
      wb_data_o          : out std_logic_vector(31 downto 0);
      wb_cyc_i           : in  std_logic;
      wb_sel_i           : in  std_logic_vector(3 downto 0);
      wb_stb_i           : in  std_logic;
      wb_we_i            : in  std_logic;
      wb_ack_o           : out std_logic;
      wb_irq_o           : out std_logic;
      spll_csr_tag_en_o  : out std_logic_vector(3 downto 0);
      spll_csr_tag_rdy_i : in  std_logic_vector(3 downto 0);

      spll_per_hpll_i      : in  std_logic_vector(31 downto 0);
      tag_hpll_rd_period_o : out std_logic;

      spll_tag_ref_i   : in  std_logic_vector(31 downto 0);
      tag_ref_rd_ack_o : out std_logic;

      spll_tag_fb_i       : in  std_logic_vector(31 downto 0);
      tag_fb_rd_ack_o     : out std_logic;
      spll_dac_hpll_o     : out std_logic_vector(15 downto 0);
      spll_dac_hpll_wr_o  : out std_logic;
      spll_dac_dmpll_o    : out std_logic_vector(15 downto 0);
      spll_dac_dmpll_wr_o : out std_logic;
      spll_deglitch_thr_o : out std_logic_vector(15 downto 0);

      irq_tag_i : in std_logic);
  end component;

  component hpll_period_detect
    generic (
      g_freq_err_frac_bits : integer := 1);
    port (
      clk_ref_i            : in  std_logic;
      clk_fbck_i           : in  std_logic;
      clk_sys_i            : in  std_logic;
      rst_n_refclk_i       : in  std_logic;
      rst_n_fbck_i         : in  std_logic;
      rst_n_sysclk_i       : in  std_logic;
      freq_err_o           : out std_logic_vector(11 downto 0);
      freq_err_stb_p_o     : out std_logic;
      hpll_fbcr_fd_gate_i  : in  std_logic_vector(2 downto 0);
      hpll_fbcr_ferr_set_i : in  std_logic_vector(11 downto 0));
  end component;

  signal per_hpll : std_logic_vector(g_tag_bits-1 downto 0);
  signal tag_ref  : std_logic_vector(g_tag_bits-1 downto 0);
  signal tag_fb   : std_logic_vector(g_tag_bits-1 downto 0);

  signal per_hpll_p : std_logic;
  signal tag_ref_p  : std_logic;
  signal tag_fb_p   : std_logic;

  signal rst_n_refclk  : std_logic;
  signal rst_n_dmtdclk : std_logic;
  signal rst_n_rxclk   : std_logic;

  signal deglitch_thr_slv : std_logic_vector(15 downto 0);

  signal spll_per_hpll : std_logic_vector(31 downto 0);
  signal spll_tag_ref  : std_logic_vector(31 downto 0);
  signal spll_tag_fb   : std_logic_vector(31 downto 0);

  signal tag_hpll_rd_period_ack : std_logic;
  signal tag_ref_rd_ack         : std_logic;
  signal tag_fb_rd_ack          : std_logic;

  signal spll_csr_tag_en   : std_logic_vector(3 downto 0);
  signal spll_csr_tag_rdy  : std_logic_vector(3 downto 0);
  signal spll_dac_hpll     : std_logic_vector(15 downto 0);
  signal spll_dac_hpll_wr  : std_logic;
  signal spll_dac_dmpll    : std_logic_vector(15 downto 0);
  signal spll_dac_dmpll_wr : std_logic;
  signal irq_tag           : std_logic;

  signal freq_err       : std_logic_vector(11 downto 0);
  signal freq_err_stb_p : std_logic;

  constant c_log2_replication : integer := 2;
  constant c_use_multi_dmtd   : boolean := false;

  signal clk_ref_buf : std_logic;
  signal clk_rx_buf  : std_logic;

  component BUFG
    port (
      O : out std_logic;
      I : in  std_logic);
  end component;
  
begin  -- rtl

  sync_ffs_rst1 : gc_sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => clk_ref_i,
      rst_n_i  => '1',
      data_i   => rst_n_i,
      synced_o => rst_n_refclk,
      npulse_o => open,
      ppulse_o => open);

  sync_ffs_rst2 : gc_sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => clk_dmtd_i,
      rst_n_i  => '1',
      data_i   => rst_n_i,
      synced_o => rst_n_dmtdclk,
      npulse_o => open,
      ppulse_o => open);

  sync_ffs_rst3 : gc_sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => clk_rx_i,
      rst_n_i  => '1',
      data_i   => rst_n_i,
      synced_o => rst_n_rxclk,
      npulse_o => open,
      ppulse_o => open);

  debug_o(3) <= clk_ref_i;
  debug_o(2) <= clk_rx_i;
  debug_o(1) <= tag_ref_p;
  debug_o(0) <= '0';

  gen_with_multi_dmtd : if(c_use_multi_dmtd = true) generate

    MDMTD_REF : multi_dmtd_with_deglitcher
      generic map (
        g_counter_bits     => g_tag_bits,
        g_log2_replication => c_log2_replication)
      port map (
        rst_n_dmtdclk_i      => rst_n_dmtdclk,
        rst_n_sysclk_i       => rst_n_i,
        clk_dmtd_i           => clk_dmtd_i,
        clk_sys_i            => clk_sys_i,
        clk_in_i             => clk_rx_i,
        tag_o                => tag_ref,
        tag_stb_p_o          => tag_ref_p,
        shift_en_i           => '0',
        shift_dir_i          => '0',
        deglitch_threshold_i => deglitch_thr_slv,
        dbg_dmtdout_o        => open);

    MDMTD_FB : multi_dmtd_with_deglitcher
      generic map (
        g_counter_bits     => g_tag_bits,
        g_log2_replication => c_log2_replication)
      port map (
        rst_n_dmtdclk_i      => rst_n_dmtdclk,
        rst_n_sysclk_i       => rst_n_i,
        clk_dmtd_i           => clk_dmtd_i,
        clk_sys_i            => clk_sys_i,
        clk_in_i             => clk_ref_i,
        tag_o                => tag_fb,
        tag_stb_p_o          => tag_fb_p,
        shift_en_i           => '0',
        shift_dir_i          => '0',
        deglitch_threshold_i => deglitch_thr_slv,
        dbg_dmtdout_o        => open);


  end generate gen_with_multi_dmtd;


  gen_with_single_dmtd : if(c_use_multi_dmtd = false) generate
    
    DMTD_REF : dmtd_with_deglitcher
      generic map (
        g_counter_bits => g_tag_bits)
      port map (
        rst_n_dmtdclk_i => rst_n_dmtdclk,
        rst_n_sysclk_i  => rst_n_i,

        clk_dmtd_i => clk_dmtd_i,
        clk_sys_i  => clk_sys_i,
        clk_in_i   => clk_rx_buf,

        tag_o                 => tag_ref,
        tag_stb_p1_o           => tag_ref_p,
        shift_en_i            => '0',
        shift_dir_i           => '0',
        deglitch_threshold_i => deglitch_thr_slv,
        dbg_dmtdout_o         => open);

    DMTD_FB : dmtd_with_deglitcher
      generic map (
        g_counter_bits => g_tag_bits)
      port map (
        rst_n_dmtdclk_i => rst_n_dmtdclk,
        rst_n_sysclk_i  => rst_n_i,

        clk_dmtd_i => clk_dmtd_i,
        clk_sys_i  => clk_sys_i,
        clk_in_i   => clk_ref_buf,

        tag_o                 => tag_fb,
        tag_stb_p1_o           => tag_fb_p,
        shift_en_i            => '0',
        shift_dir_i           => '0',
        
        deglitch_threshold_i => deglitch_thr_slv,
        dbg_dmtdout_o         => open);
  end generate gen_with_single_dmtd;

  --buf_rx_clk : BUFG
  --  port map (
  --    O => clk_rx_buf,
  --    I => clk_rx_i);

  --buf_ref_clk : BUFG
  --  port map (
  --    O => clk_ref_buf,
  --    I => clk_ref_i);


  clk_ref_buf <= clk_ref_i;
  clk_rx_buf <= clk_rx_i;
  
  PERIOD_DET : hpll_period_detect
    port map (
      clk_ref_i  => clk_rx_buf,
      clk_fbck_i => clk_dmtd_i,
      clk_sys_i  => clk_sys_i,

      rst_n_refclk_i       => rst_n_rxclk,
      rst_n_fbck_i         => rst_n_dmtdclk,
      rst_n_sysclk_i       => rst_n_i,
      freq_err_o           => freq_err,
      freq_err_stb_p_o     => freq_err_stb_p,
      hpll_fbcr_fd_gate_i  => "011",
      hpll_fbcr_ferr_set_i => "000000000000");

  U_WB_SLAVE : softpll_wb
    port map (
      rst_n_i   => rst_n_i,
      wb_clk_i  => clk_sys_i,
      wb_addr_i => wb_addr_i,
      wb_data_i => wb_data_i,
      wb_data_o => wb_data_o,
      wb_cyc_i  => wb_cyc_i,
      wb_sel_i  => wb_sel_i,
      wb_stb_i  => wb_stb_i,
      wb_we_i   => wb_we_i,
      wb_ack_o  => wb_ack_o,
      wb_irq_o  => wb_irq_o,

      spll_csr_tag_en_o    => spll_csr_tag_en,
      spll_csr_tag_rdy_i   => spll_csr_tag_rdy,
      spll_tag_ref_i       => spll_tag_ref,
      tag_ref_rd_ack_o     => tag_ref_rd_ack,
      spll_tag_fb_i        => spll_tag_fb,
      tag_fb_rd_ack_o      => tag_fb_rd_ack,
      spll_per_hpll_i      => spll_per_hpll,
      tag_hpll_rd_period_o => tag_hpll_rd_period_ack,

      spll_dac_hpll_o     => spll_dac_hpll,
      spll_dac_hpll_wr_o  => spll_dac_hpll_wr,
      spll_dac_dmpll_o    => spll_dac_dmpll,
      spll_dac_dmpll_wr_o => spll_dac_dmpll_wr,
      spll_deglitch_thr_o => deglitch_thr_slv,
      irq_tag_i           => irq_tag);


  dac_hpll_load_o <= spll_dac_hpll_wr;
  dac_hpll_data_o <= spll_dac_hpll;

  dac_dmpll_load_o <= spll_dac_dmpll_wr;
  dac_dmpll_data_o <= spll_dac_dmpll;

  collect_tags : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0') then
        spll_per_hpll    <= (others => '0');
        spll_tag_ref     <= (others => '0');
        spll_tag_fb      <= (others => '0');
        spll_csr_tag_rdy <= (others => '0');
      else

        if(freq_err_stb_p = '1') then
          spll_per_hpll       <= std_logic_vector(to_unsigned(0, 32-12)) & freq_err;
          spll_csr_tag_rdy(1) <= spll_csr_tag_en(1);
        elsif(tag_hpll_rd_period_ack = '1') then
          spll_csr_tag_rdy(1) <= '0';
        end if;

        if(tag_ref_p = '1') then
          spll_tag_ref        <= std_logic_vector(to_unsigned(0, 32-g_tag_bits)) & tag_ref;
          spll_csr_tag_rdy(2) <= spll_csr_tag_en(2);
        elsif(tag_ref_rd_ack = '1') then
          spll_csr_tag_rdy(2) <= '0';
        end if;

        if(tag_fb_p = '1') then
          spll_tag_fb         <= std_logic_vector(to_unsigned(0, 32-g_tag_bits)) & tag_fb;
          spll_csr_tag_rdy(3) <= spll_csr_tag_en(3);
        elsif(tag_fb_rd_ack = '1') then
          spll_csr_tag_rdy(3) <= '0';
        end if;
      end if;
    end if;
  end process;

  irq_tag <= '1' when spll_csr_tag_rdy /= "0000" else '0';

  -- debug_o(3) <= spll_csr_tag_rdy(0);
  
  
end rtl;
