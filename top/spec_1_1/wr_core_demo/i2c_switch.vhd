library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

library UNISIM;
use UNISIM.vcomponents.all;

entity i2c_switch is
	port(
		-- Clock and Reset
		clk: in std_logic;
		rst: in std_logic;

		-- Slave I2C bus
		slave_scl_i: in std_logic;
		slave_scl_o: out std_logic;
		slave_sda_i: in std_logic;
		slave_sda_o: out std_logic;

		-- Master I2C buses
		master_scl_i: in std_logic_vector(1 downto 0);
		master_scl_o: out std_logic_vector(1 downto 0);
		master_sda_i: in std_logic_vector(1 downto 0);
		master_sda_o: out std_logic_vector(1 downto 0)
		
		--ch_enable : out std_logic_vector(1 downto 0)
	);
end i2c_switch;

architecture behavioral of i2c_switch is
	signal ch_enable_reg: std_logic_vector(1 downto 0);

	component i2c_start_stop_detector
	generic(
		N: natural := 2
	);
	port(
		-- Clock and Reset
		clk: in std_logic;
		rst: in std_logic;

		ch_enable: out std_logic_vector(N-1 downto 0);

		-- Master I2C buses
		master_scl_i: in std_logic_vector(N-1 downto 0);
		master_sda_i: in std_logic_vector(N-1 downto 0)
	);
	end component;

begin
	
ss_detector: i2c_start_stop_detector
generic map(
	N => 2
)
port map(
	clk => clk,
	rst => rst,
	ch_enable => ch_enable_reg,
	master_scl_i => master_scl_i,
	master_sda_i => master_sda_i
);

process (clk,rst)
begin
	if rst = '1' then
		slave_scl_o <= '1';
		slave_sda_o <= '1';
		master_scl_o(0) <= '1';
		master_sda_o(0) <= '1';
		master_scl_o(1) <= '1';
		master_sda_o(1) <= '1';
	else
		if clk'event and clk = '1' then
			if ch_enable_reg(0) = '1' then
				slave_scl_o <= master_scl_i(0);
				slave_sda_o <= master_sda_i(0);
				master_scl_o(0) <= slave_scl_i;
				master_sda_o(0) <= slave_sda_i;
				master_scl_o(1) <= '1';
				master_sda_o(1) <= '1';
			else
				if ch_enable_reg(1) = '1' then
					slave_scl_o <= master_scl_i(1);
					slave_sda_o <= master_sda_i(1);
					master_scl_o(1) <= slave_scl_i;
					master_sda_o(1) <= slave_sda_i;
					master_scl_o(0) <= '1';
					master_sda_o(0) <= '1';
				else
					slave_scl_o <= '1';
					slave_sda_o <= '1';
					master_scl_o(0) <= '1';
					master_sda_o(0) <= '1';
					master_scl_o(1) <= '1';
					master_sda_o(1) <= '1';
				end if;
			end if;
		end if;
	end if;
end process;

-- El maestro que no tenga el turno tambien vería el bus (puede creer 
-- que el esclavo le está contestando!!)
--master_scl_o(0) <= slave_scl_i;
--master_sda_o(0) <= slave_sda_i;
--master_scl_o(1) <= slave_scl_i;
--master_sda_o(1) <= slave_sda_i;

--ch_enable <= ch_enable_reg;

end behavioral;
