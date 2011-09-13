library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package endpoint_pkg is
  
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

  constant c_QMODE_PORT_ACCESS          : std_logic_vector(1 downto 0) := "00";
  constant c_QMODE_PORT_TRUNK           : std_logic_vector(1 downto 0) := "01";
  constant c_QMODE_PORT_UNQUALIFIED     : std_logic_vector(1 downto 0) := "11";


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


  function f_marshall_wrf_status (stat  : t_wrf_status_reg) return std_logic_vector;
  function f_unmarshall_wrf_status(stat : std_logic_vector) return t_wrf_status_reg;
  
end endpoint_pkg;

-------------------------------------------------------------------------------

package body endpoint_pkg is

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
    
end endpoint_pkg;

-------------------------------------------------------------------------------
