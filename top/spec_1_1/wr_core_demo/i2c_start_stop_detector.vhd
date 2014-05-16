library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

library UNISIM;
use UNISIM.vcomponents.all;

entity i2c_start_stop_detector is
	generic(
		N: natural := 2
	);
	port(
		-- Clock and Reset
		clk: in std_logic;
		rst_n: in std_logic;

		ch_enable: out std_logic_vector(N-1 downto 0);

		-- Master I2C buses
		master_scl_i: in std_logic_vector(N-1 downto 0);
		master_sda_i: in std_logic_vector(N-1 downto 0)
	);
end i2c_start_stop_detector;

architecture behavioral of i2c_start_stop_detector is
	signal ch_start_reg: std_logic_vector(N-1 downto 0);
	signal ch_stop_reg: std_logic_vector(N-1 downto 0);
	signal ch_enable_reg: std_logic_vector(N-1 downto 0);
begin
	
start_detectors: for I in 0 to N-1 generate
	process (clk)
	begin
		if clk'event and clk = '1' then
			if rst_n = '0' then
				ch_start_reg(I) <= '0';
			else
				if master_sda_i(I)'event and master_sda_i(I) = '0' then
					if master_scl_i(I) = '1' then
						ch_start_reg(I) <= '1';
					end if;
				end if;
			
				if ch_start_reg(I) = '1' then
					ch_start_reg(I) <= '0';
				end if;
			end if;
		end if;
	end process;
end generate start_detectors;

stop_detectors: for I in 0 to N-1 generate
	process (clk)
	begin
		if clk'event and clk = '1' then
			if rst_n = '0' then
				ch_stop_reg(I) <= '0';
			else
				if master_sda_i(I)'event and master_sda_i(I) = '1' then
					if master_scl_i(I) = '1' then
						ch_stop_reg(I) <= '1';
					end if;
				end if;
			
				if ch_stop_reg(I) = '1' then
					ch_stop_reg(I) <= '0';
				end if;
			end if;
		end if;
	end process;
end generate stop_detectors;

enable_ch: for I in 0 to N-1 generate
process(clk)
begin
	if clk'event and clk = '1' then
		if rst_n = '0' then
			ch_enable_reg(I) <= '0';
		else
			if ch_enable_reg(I) = '0' and ch_start_reg(I) = '1' then
				ch_enable_reg(I) <= '1';
			elsif ch_enable_reg(I) = '1' and ch_stop_reg(I) = '1' then
				ch_enable_reg(I) <= '0';
			end if;
		end if;
	end if;
end process;

end generate enable_ch;

ch_enable <= ch_enable_reg;

end behavioral;