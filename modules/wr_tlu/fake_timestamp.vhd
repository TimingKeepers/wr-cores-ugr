--! Standard library
library IEEE;
--! Standard packages    
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity fake_timestamp is

  port (
    ref_clk_i       : in  std_logic;    -- tranceiver clock domain

    nRSt_i          : in  std_logic;

    cnt_clr : in std_logic;
    tm_utc_o        : out  std_logic_vector(39 downto 0);  -- UTC Timestamp
    tm_cycles_o     : out  std_logic_vector(27 downto 0)  -- refclock cycle count
);

end entity fake_timestamp;

architecture behavioral of fake_timestamp is

  signal tm_utc    : unsigned(tm_utc_o'left downto 0);
  signal tm_cycles : unsigned(tm_cycles_o'left downto 0);
  constant c_1second : unsigned(tm_utc'left downto 0) := to_unsigned(124999999, tm_utc'length);  -- 125 MHz clock
  
  begin  -- behavioral
      tm_utc_o    <=  std_logic_vector(tm_utc);
      tm_cycles_o <=  std_logic_vector(tm_cycles);
      
     count : process (ref_clk_i)
     begin  -- process count

        if(ref_clk_i'event and ref_clk_i = '1') then  -- rising clock edge
          if( nRST_i = '0' or cnt_clr = '1') then       -- synchronous reset (active low)
            tm_utc <= (others => '0');
            tm_cycles <= (others => '0');
          else
            if(tm_cycles = c_1second) then
              tm_cycles <= (others => '0');
              tm_utc <= tm_utc +1;
            else
              tm_cycles <= tm_cycles +1;
            end if;
          end if;
        end if;
     end process count;
      

  end architecture behavioral;  
