library ieee;
use ieee.std_logic_1164.all;

use work.Wishbone_pkg.all;
use work.wr_fabric_pkg.all;

entity mini_bone is
  generic(
    g_class_mask    : std_logic_vector(7 downto 0) := x"ff";
    g_our_ethertype : std_logic_vector(15 downto 0) := x"a0a0");
  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    snk_cyc_i   : in  std_logic;
    snk_stb_i   : in  std_logic;
    snk_sel_i   : in  std_logic_vector(1 downto 0);
    snk_adr_i   : in  std_logic_vector(1 downto 0);
    snk_dat_i   : in  std_logic_vector(15 downto 0);
    snk_we_i    : in  std_logic;
    snk_stall_o : out std_logic;
    snk_ack_o   : out std_logic;
    snk_err_o   : out std_logic;

    src_cyc_o   : out std_logic;
    src_stb_o   : out std_logic;
    src_dat_o   : out std_logic_vector(15 downto 0);
    src_adr_o   : out std_logic_vector(1 downto 0);
    src_we_o    : out std_logic;
    src_ack_i   : in  std_logic;
    src_err_i   : in  std_logic;
    src_sel_o   : out std_logic_vector(1 downto 0);
    src_stall_i : in  std_logic;

    master_cyc_o : out std_logic;
    master_we_o  : out std_logic;
    master_stb_o : out std_logic;
    master_sel_o : out std_logic_vector(3 downto 0);
    master_adr_o : out std_logic_vector(31 downto 0);
    master_dat_o : out std_logic_vector(31 downto 0);
    master_dat_i : in  std_logic_vector(31 downto 0);
    master_ack_i : in  std_logic
    );

end mini_bone;


architecture wrapper of mini_bone is

  component xmini_bone
    generic (
      g_class_mask    : std_logic_vector(7 downto 0);
      g_our_ethertype : std_logic_vector(15 downto 0));
    port (
      clk_sys_i : in  std_logic;
      rst_n_i   : in  std_logic;
      src_o     : out t_wrf_source_out;
      src_i     : in  t_wrf_source_in;
      snk_o     : out t_wrf_sink_out;
      snk_i     : in  t_wrf_sink_in;
      master_o  : out t_wishbone_master_out;
      master_i  : in  t_wishbone_master_in);
  end component;


  signal src_out    : t_wrf_source_out;
  signal src_in     : t_wrf_source_in;
  signal snk_out    : t_wrf_sink_out;
  signal snk_in     : t_wrf_sink_in;
  signal master_out : t_wishbone_master_out;
  signal master_in  : t_wishbone_master_in;

begin  -- wrapper

  U_Wrapped_MB : xmini_bone
    generic map (
      g_class_mask    => g_class_mask,
      g_our_ethertype => g_our_ethertype)
    port map (
      clk_sys_i => clk_sys_i,
      rst_n_i   => rst_n_i,
      src_o     => src_out,
      src_i     => src_in,
      snk_o     => snk_out,
      snk_i     => snk_in,
      master_o  => master_out,
      master_i  => master_in);

  src_cyc_o <= src_out.cyc;
  src_stb_o <= src_out.stb;
  src_we_o  <= src_out.we;
  src_adr_o <= src_out.adr;
  src_dat_o <= src_out.dat;
  src_sel_o <= src_out.sel;

  src_in.ack   <= src_ack_i;
  src_in.stall <= src_stall_i;


  snk_in.cyc <= snk_cyc_i;
  snk_in.stb <= snk_stb_i;
  snk_in.we  <= snk_we_i;
  snk_in.sel <= snk_sel_i;
  snk_in.adr <= snk_adr_i;
  snk_in.dat <= snk_dat_i;

  snk_ack_o   <= snk_out.ack;
  snk_stall_o <= snk_out.stall;

  master_cyc_o <= master_out.cyc;
  master_stb_o <= master_out.stb;
  master_we_o  <= master_out.we;
  master_sel_o <= master_out.sel;
  master_adr_o <= master_out.adr;
  master_dat_o <= master_out.dat;

  master_in.dat <= master_dat_i;
  master_in.ack <= master_ack_i;
  
end wrapper;
