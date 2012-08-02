-- Register map:
-- 0x00 = FIFO control register
-- 0x04 = current toggle value (as see at output pins)

-- 0x10 = UTChi
-- 0x14 = UTClo
-- 0x18 = cycle counter
-- 0x1C = value to XOR

-- Reading from the control register reports 0 if room in FIFO
-- Writing to the control register pushes the entry to the FIFO

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wishbone_pkg.all;
use work.gencores_pkg.gc_wfifo;

entity xwr_eca is
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
end xwr_eca;

architecture rtl of xwr_eca is
  procedure update(signal o : out t_wishbone_address) is
  begin
    for i in (c_wishbone_data_width/8)-1 downto 0 loop
      if slave_i.SEL(i) = '1' then
        o(i*8+7 downto i*8) <= slave_i.DAT(i*8+7 downto i*8);
      end if;
    end loop;
  end update;
  
  function idx(v : std_logic_vector) return std_logic_vector is
    alias result : std_logic_vector(v'length-1 downto 0) is v;
  begin
    return result;
  end idx;
  
  signal r_toggle : t_wishbone_data;
  signal r_utchi  : t_wishbone_data;
  signal r_utclo  : t_wishbone_data;
  signal r_cycle  : t_wishbone_data;
  signal r_val    : t_wishbone_data;
  signal r_valid  : std_logic;
  signal r_tm_full  : std_logic_vector(67 downto 0);
  
  signal slave_o_ACK : std_logic;
  signal slave_o_DAT : t_wishbone_data;
  
  signal w_rdy, w_en, r_rdy, r_en : std_logic;
  signal w_data, r_data : std_logic_vector(c_wishbone_data_width*4-1 downto 0);
  
  -- Front of FIFO
  signal f_tm_utc   : std_logic_vector(39 downto 0);
  signal f_tm_cycle : std_logic_vector(27 downto 0);
  signal f_val      : t_wishbone_data;
  
  signal tm_full_i : std_logic_vector(67 downto 0);
  signal f_tm_full : std_logic_vector(67 downto 0);
begin
  -- Hard-wired slave pins
  slave_o.ACK   <= slave_o_ACK;
  slave_o.ERR   <= '0';
  slave_o.RTY   <= '0';
  slave_o.STALL <= '0';
  slave_o.DAT   <= slave_o_DAT;
  
  -- Output pins
  toggle_o <= r_toggle;
  
  fifo : gc_wfifo 
    generic map(
      sync_depth => 1,
      gray_code  => false,
      addr_width => logFifoLen,
      data_width => c_wishbone_data_width*4)
    port map(
      w_clk_i   => clk_i,
      w_rst_n_i => rst_n_i,
      w_rdy_o   => w_rdy,
      w_en_i    => w_en,
      w_data_i  => w_data,
      a_clk_i   => '0',
      a_rst_n_i => '0',
      a_rdy_o   => open,
      a_en_i    => '0',
      r_clk_i   => clk_i,
      r_rst_n_i => rst_n_i,
      r_rdy_o   => r_rdy,
      r_en_i    => r_en,
      r_data_o  => r_data);
  w_data(c_wishbone_data_width*4-1 downto c_wishbone_data_width*3) <= r_utchi;
  w_data(c_wishbone_data_width*3-1 downto c_wishbone_data_width*2) <= r_utclo;
  w_data(c_wishbone_data_width*2-1 downto c_wishbone_data_width*1) <= r_cycle;
  w_data(c_wishbone_data_width*1-1 downto c_wishbone_data_width*0) <= r_val;
  
  f_tm_utc   <= idx(r_data(c_wishbone_data_width*4-1 downto c_wishbone_data_width*3))(7 downto 0) &
                idx(r_data(c_wishbone_data_width*3-1 downto c_wishbone_data_width*2))(31 downto 0);
  f_tm_cycle <= idx(r_data(c_wishbone_data_width*2-1 downto c_wishbone_data_width*1))(27 downto 0);
  f_val      <= idx(r_data(c_wishbone_data_width*1-1 downto c_wishbone_data_width*0));
  
  f_tm_full  <= f_tm_utc & f_tm_cycle;
  tm_full_i  <= tm_utc_i & tm_cycle_i;
  
  main : process(clk_i)
    variable valid : std_logic;
    variable toggle : t_wishbone_data;
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        r_toggle  <= (others => '0');
        r_utchi   <= (others => '0');
        r_utclo   <= (others => '0');
        r_cycle   <= (others => '0');
        r_val     <= (others => '0');
        r_valid   <= '0';
        r_tm_full <= (others => '0');
      else
        -- Check for record to pop
        if valid = '1' and unsigned(f_tm_full) <= unsigned(r_tm_full) then
          toggle := r_toggle xor f_val;
          valid := '0';
        else
          toggle := r_toggle;
          valid := r_valid;
        end if;
        
        -- Compensate the clock by two cycles to meet the deadline exactly
        r_tm_full <= std_logic_vector(unsigned(tm_full_i) + to_unsigned(2, 68));
        
        if valid = '0' and r_rdy = '1' then
          r_en <= '1';
          r_valid <= '1';
        else
          r_en <= '0';
          r_valid <= valid;
        end if;
        
        -- Output requested data
        case to_integer(unsigned(slave_i.ADR(4 downto 2))) is
          when 0 => slave_o_DAT <= std_logic_vector(to_unsigned(0, c_wishbone_data_width-1)) & w_rdy;
          when 1 => slave_o_DAT <= r_toggle;
          when 4 => slave_o_DAT <= r_utchi;
          when 5 => slave_o_DAT <= r_utclo;
          when 6 => slave_o_DAT <= r_cycle;
          when 7 => slave_o_DAT <= r_val;
          when others => slave_o_DAT <= (others => '0');
        end case;
        
        -- Process input data
        w_en <= '0';
        if (slave_i.CYC = '1' and slave_i.STB = '1' and slave_i.WE = '1') then
          case to_integer(unsigned(slave_i.ADR(4 downto 2))) is
            when 0 => w_en <= '1';
            when 1 => update(r_toggle);
            when 4 => update(r_utchi);
            when 5 => update(r_utclo);
            when 6 => update(r_cycle);
            when 7 => update(r_val);
            when others => r_toggle <= toggle;
          end case;
	else
	  r_toggle <= toggle;
        end if;
        
        slave_o_ACK <= slave_i.CYC and slave_i.STB;
      end if;
    end if;
  end process;
end rtl;
