library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.genram_pkg.all;
use work.wishbone_pkg.all;

package obp_pkg is

function f_xwb_dpram_obp(g_size : natural) return t_sdb_device;
component OBP is
generic(
    g_dpram_initf               : string                         := "obp.ram";
    g_dpram_size                : integer                        := 90112/4;
	 g_bridge_sdb  : t_sdb_bridge
    );
	 port(
    clk_sys_i : in std_logic;
    rst_n_i : in std_logic;
	 enable_obp : in std_logic;
	 wb_i  : in t_wishbone_master_in;
	 wb_o  : out t_wishbone_master_out
    );
end component;

end obp_pkg;

package body obp_pkg is
function f_xwb_dpram_obp(g_size : natural) return t_sdb_device
  is
    variable result : t_sdb_device;
  begin
    result.abi_class     := x"0001";    -- RAM device
    result.abi_ver_major := x"01";
    result.abi_ver_minor := x"00";
    result.wbd_width     := x"7";       -- 32/16/8-bit supported
    result.wbd_endian    := c_sdb_endian_big;

    result.sdb_component.addr_first := (others => '0');
    result.sdb_component.addr_last  := std_logic_vector(to_unsigned(g_size*4-1, 64));

    result.sdb_component.product.vendor_id := x"000000000000CE42";  -- CERN
    result.sdb_component.product.device_id := x"66554433";
    result.sdb_component.product.version   := x"00000001";
    result.sdb_component.product.date      := x"20120305";
    result.sdb_component.product.name      := "WB4-BlockRAM OBP   ";

    return result;
  end f_xwb_dpram_obp;

end obp_pkg;
