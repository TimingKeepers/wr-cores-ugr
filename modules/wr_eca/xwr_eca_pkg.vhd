library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wishbone_pkg.all;
use work.gencores_pkg.all;

package xwr_eca_pkg is
 constant c_xwr_eca_sdwb : t_sdwb_device := (
    wbd_begin     => x"0000000000000000",
    wbd_end       => x"000000000000001f",
    sdwb_child    => x"0000000000000000",
    wbd_flags     => x"01", -- big-endian, no-child, present
    wbd_width     => x"04", -- 32-bit port granularity
    abi_ver_major => x"01",
    abi_ver_minor => x"00",
    abi_class     => x"00000000", -- undocumented device
    dev_vendor    => x"00000651", -- GSI
    dev_device    => x"8752bf44",
    dev_version   => x"00000001",
    dev_date      => x"20120319",
    description   => "GSI_ECA_UNIT    ");

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
