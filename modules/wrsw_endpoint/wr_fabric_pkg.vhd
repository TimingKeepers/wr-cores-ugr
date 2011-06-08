package wr_fabric_pkg is

  constant c_WRF_DATA   : std_logic_vector(1 downto 0) := "00";
  constant c_WRF_OOB    : std_logic_vector(1 downto 0) := "01";
  constant c_WRF_STATUS : std_logic_vector(1 downto 0) := "10";
  constant c_WRF_USER   : std_logic_vector(1 downto 0) := "11";

  type t_wrf_status_reg is record
    is_hp       : std_logic;
    has_smac    : std_logic;
    has_crc     : std_logic;
    rx_error    : std_logic;
    tag_me      : std_logic;
    match_class : std_logic_vector(7 downto 0);
  end record;

  type t_wrf_fromsource is record
    adr : std_logic_vector(1 downto 0);
    dat : std_logic_vector(15 downto 0);
    cyc : std_logic;
    stb : std_logic;
    we  : std_logic;
    sel : std_logic_vector(1 downto 0);
  end record;

  type t_wrf_tosource is record
    ack   : std_logic;
    stall : std_logic;
  end record;
  
end wr_fabric_pkg;
