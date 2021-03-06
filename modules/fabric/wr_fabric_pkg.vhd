library ieee;
use ieee.std_logic_1164.all;


package wr_fabric_pkg is

  constant c_WRF_DATA   : std_logic_vector(1 downto 0) := "00";
  constant c_WRF_OOB    : std_logic_vector(1 downto 0) := "01";
  constant c_WRF_STATUS : std_logic_vector(1 downto 0) := "10";
  constant c_WRF_USER   : std_logic_vector(1 downto 0) := "11";

  constant c_WRF_OOB_TYPE_RX : std_logic_vector(3 downto 0) := "0000";
  constant c_WRF_OOB_TYPE_TX : std_logic_vector(3 downto 0) := "0001";

  type t_wrf_mux_class is array (natural range <>) of std_logic_vector(7 downto 0);

  type t_wrf_status_reg is record
    is_hp       : std_logic;
    has_smac    : std_logic;
    has_crc     : std_logic;
    error       : std_logic;
    tag_me      : std_logic;
    match_class : std_logic_vector(7 downto 0);
  end record;

  type t_wrf_source_out is record
    adr : std_logic_vector(1 downto 0);
    dat : std_logic_vector(15 downto 0);
    cyc : std_logic;
    stb : std_logic;
    we  : std_logic;
    sel : std_logic_vector(1 downto 0);
  end record;

  type t_wrf_source_in is record
    ack   : std_logic;
    stall : std_logic;
    err   : std_logic;
    rty   : std_logic;
  end record;


  type t_wrf_oob is record
    valid: std_logic;
    oob_type : std_logic_vector(3 downto 0);
    ts_r     : std_logic_vector(27 downto 0);
    ts_f     : std_logic_vector(3 downto 0);
    frame_id : std_logic_vector(15 downto 0);
    port_id  : std_logic_vector(5 downto 0);
  end record;

  subtype t_wrf_sink_in is t_wrf_source_out;
  subtype t_wrf_sink_out is t_wrf_source_in;

  type t_wrf_source_in_array is array (natural range <>) of t_wrf_source_in;
  type t_wrf_source_out_array is array (natural range <>) of t_wrf_source_out;

  subtype t_wrf_sink_in_array is t_wrf_source_out_array;
  subtype t_wrf_sink_out_array is t_wrf_source_in_array;

  function f_marshall_wrf_status (stat  : t_wrf_status_reg) return std_logic_vector;
  function f_unmarshall_wrf_status(stat : std_logic_vector) return t_wrf_status_reg;

  constant c_dummy_src_in : t_wrf_source_in :=
    ('0', '0', '0', '0');
  constant c_dummy_snk_in : t_wrf_sink_in :=
    ("XX", "XXXXXXXXXXXXXXXX", '0', '0', '0', "XX");


  -----------------------------------------------------------------------------
  -- WRF MUX
  -----------------------------------------------------------------------------
  component xwrf_mux is
    generic(
      g_muxed_ports	:	integer := 2);
    port(
      clk_sys_i	: in std_logic;
      rst_n_i		: in std_logic;
      --ENDPOINT
      ep_src_o	:	out t_wrf_source_out;
      ep_src_i	: in  t_wrf_source_in;
      ep_snk_o	: out t_wrf_sink_out;
      ep_snk_i  : in  t_wrf_sink_in;
      --Muxed ports
      mux_src_o : out t_wrf_source_out_array(g_muxed_ports-1 downto 0);
      mux_src_i : in  t_wrf_source_in_array(g_muxed_ports-1 downto 0);
      mux_snk_o : out t_wrf_sink_out_array(g_muxed_ports-1 downto 0);
      mux_snk_i : in  t_wrf_sink_in_array(g_muxed_ports-1 downto 0);
      --
      mux_class_i : in t_wrf_mux_class(g_muxed_ports-1 downto 0)
    );
  end component;

end wr_fabric_pkg;

package body wr_fabric_pkg is

  function f_marshall_wrf_status(stat : t_wrf_status_reg)
    return std_logic_vector is
    variable tmp : std_logic_vector(15 downto 0);
  begin
    tmp(0)           := stat.is_hp;
    tmp(1)           := stat.error;
    tmp(2)           := stat.has_smac;
    tmp(3)           := stat.has_crc;
    tmp(15 downto 8) := stat.match_class;
    return tmp;
  end function;

  function f_unmarshall_wrf_status(stat : std_logic_vector) return t_wrf_status_reg is
    variable tmp : t_wrf_status_reg;
  begin
    tmp.is_hp       := stat(0);
    tmp.error       := stat(1);
    tmp.has_smac    := stat(2);
    tmp.has_crc     := stat(3);
    tmp.match_class := stat(15 downto 8);
    return tmp;
    
  end function;


end wr_fabric_pkg;
