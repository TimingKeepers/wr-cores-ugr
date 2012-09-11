library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.wishbone_pkg.all;
use work.gencores_pkg.all;

package xwr_eca_pkg is
 constant c_xwr_eca_sdb : t_sdb_device := (
    abi_class     => x"0000", -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"00",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"4", -- 32-bit port granularity
    sdb_component => (
    addr_first    => x"0000000000000000",
    addr_last     => x"000000000000001f",
    product => (
    vendor_id     => x"0000000000000651",
    device_id     => x"8752bf44",
    version       => x"00000001",
    date          => x"20120319",
    name          => "GSI_ECA_UNIT       ")));

  component xwr_eca is
    generic(
      logFifoLen : integer := 4);
    port(
      -- Common wishbone signals
      clk_i       : in  std_logic;
      rst_n_i     : in  std_logic;
      -- Slave control port
      slave_i     : in  t_wishbone_slave_in;
      slave_o     : out t_wishbone_slave_out;
      -- Current timestamp
      tm_utc_i    : in  std_logic_vector(39 downto 0);
      tm_cycle_i  : in  std_logic_vector(27 downto 0);
      -- Current status pins
      toggle_o    : out t_wishbone_data);
  end component;
end xwr_eca_pkg;
