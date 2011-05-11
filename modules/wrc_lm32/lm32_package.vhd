library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.wishbone_package.all;

package lm32_package is
   component lm32_vhdl is
      port(
         clk		: in	std_logic;
         rst		: in	std_logic;
         interrupt	: in	std_logic_vector(31 downto 0);
         -- Data bus wishbone master:
         data_o		: out	wishbone_master_out;
         data_i		: in	wishbone_master_in;
         -- Instruction bus wishbone master:
         inst_o		: out	wishbone_master_out;
         inst_i		: in	wishbone_master_in);
   end component;
end;
