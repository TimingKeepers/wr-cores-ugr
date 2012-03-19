library IEEE;
--! Standard packages    
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library work;
use work.wishbone_pkg.all;

package wb_cores_pkg_gsi is

 constant c_xwr_wb_timestamp_latch_sdwb : t_sdwb_device := (
    wbd_begin     => x"0000000000000000",
    wbd_end       => x"00000000000007ff",
    sdwb_child    => x"0000000000000000",
    wbd_flags     => x"01", -- big-endian, no-child, present
    wbd_width     => x"04", -- 8/16/32-bit port granularity
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    abi_class     => x"00000000", -- undocumented device
    dev_vendor    => x"00000651", -- GSI
    dev_device    => x"10051981",
    dev_version   => x"00000001",
    dev_date      => x"20120308",
    description   => "GSI_TM_LATCH    ");

 component fake_timestamp
   port (
     ref_clk_i   : in  std_logic;
     nRSt_i      : in  std_logic;
     cnt_clr     : in  std_logic;
     tm_utc_o    : out std_logic_vector(39 downto 0);
     tm_cycles_o : out std_logic_vector(27 downto 0));
 end component;
 
component wb_timestamp_latch
  generic (
    g_num_triggers : natural;
    g_fifo_depth   : natural);
  port (
    ref_clk_i       : in  std_logic;
    sys_clk_i       : in  std_logic;
    nRSt_i          : in  std_logic;
    triggers_i      : in  std_logic_vector(g_num_triggers-1 downto 0);
    tm_time_valid_i : in  std_logic;
    tm_utc_i        : in  std_logic_vector(39 downto 0);
    tm_cycles_i     : in  std_logic_vector(27 downto 0);
    wb_slave_i      : in  t_wishbone_slave_in;
    wb_slave_o      : out t_wishbone_slave_out);
end component;

end wb_cores_pkg_gsi;
