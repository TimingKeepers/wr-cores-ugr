library IEEE;
--! Standard packages    
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library work;
use work.wishbone_pkg.all;

package wb_cores_pkg_gsi is

 constant c_xwr_wb_timestamp_latch_sdb : t_sdb_device := (
    abi_class     => x"0000", -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"7", -- 8/16/32-bit port granularity
    sdb_component => (
    addr_first    => x"0000000000000000",
    addr_last     => x"00000000000007ff",
    product => (
    vendor_id     => x"0000000000000651", -- GSI
    device_id     => x"10051981",
    version       => x"00000001",
    date          => x"20120308",
    name          => "GSI_TM_LATCH       ")));


 
component wb_timestamp_latch
  generic (
    g_num_triggers : natural;
    g_fifo_depth   : natural);
  port (
    ref_clk_i       : in  std_logic;
    ref_rstn_i      : in  std_logic;
    sys_clk_i       : in  std_logic;
    sys_rstn_i      : in  std_logic;
    triggers_i      : in  std_logic_vector(g_num_triggers-1 downto 0);
    tm_time_valid_i : in  std_logic;
    tm_tai_i        : in  std_logic_vector(39 downto 0);
    tm_cycles_i     : in  std_logic_vector(27 downto 0);
    wb_slave_i      : in  t_wishbone_slave_in;
    wb_slave_o      : out t_wishbone_slave_out);
end component;

end wb_cores_pkg_gsi;
