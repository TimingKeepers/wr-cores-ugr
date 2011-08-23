library ieee;
use ieee.std_logic_1164.all;

library unisim;
use unisim.vcomponents.all;
  
entity ep_rx_bypass_queue is
  generic(
    g_size  : integer := 3;
    g_width : integer := 18);

  port(
    rst_n_i : in std_logic;
    clk_i   : in std_logic;

    d_i     : in  std_logic_vector(g_width-1 downto 0);
    valid_i : in  std_logic;
    dreq_o  : out std_logic;

    q_o     : out std_logic_vector(g_width-1 downto 0);
    valid_o : out std_logic;
    dreq_i  : in  std_logic;

    flush_i : in std_logic;
    purge_i : in std_logic
    );

end ep_rx_bypass_queue;

architecture behavioral of ep_rx_bypass_queue is

  component ep_shift_reg
    generic (
      g_size : integer);
    port (
      clk_i : in  std_logic;
      ce_i  : in  std_logic;
      d_i   : in  std_logic;
      q_o   : out std_logic);
  end component;
  
  function f_queue_occupation(q : std_logic_vector; check_empty : std_logic) return std_logic is
    variable i : integer;
  begin
    for i in 0 to q'length-1 loop
      if(q(i) = check_empty) then
        return '0';
      end if;
    end loop;  -- i
    return '1';
  end function;

  type t_queue_array is array(0 to g_width-1) of std_logic_vector(g_size-1 downto 0);

  signal q_data  : t_queue_array;
  signal q_valid : std_logic_vector(g_size-1 downto 0);


  signal qempty, qfull : std_logic;
  signal flushing      : std_logic;
  signal valid_mask    : std_logic;
  signal valid_int     : std_logic;

  signal sreg_enable : std_logic;
  
begin  -- behavioral

  qempty <= f_queue_occupation(q_valid, '1');
  qfull  <= f_queue_occupation(q_valid, '0');

  gen_sreg : for i in 0 to g_width-1 generate

    U_sreg: ep_shift_reg
      generic map (
        g_size => g_size)
      port map (
        clk_i => clk_i,
        ce_i  => sreg_enable,
        d_i   => d_i(i),
        q_o   => q_o(i));
    
  end generate gen_sreg;


  sreg_enable <= '1' when ((valid_i = '1') or (qempty = '0' and (flushing = '1') and valid_int = '1')) else '0';

  p_queue : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' or purge_i = '1' then
        flushing   <= '0';
        valid_mask <= '0';
        q_valid    <= (others => '0');
      else
        if(flushing = '1' and qempty = '1') then
          flushing <= '0';
        elsif(flush_i = '1' and qempty = '0') then
          flushing <= '1';
        end if;

        valid_mask <= dreq_i;

        if sreg_enable = '1' then
          q_valid(0)                         <= valid_i;
          q_valid(q_valid'length-1 downto 1) <= q_valid(q_valid'length-2 downto 0);
        end if;
      end if;
    end if;
  end process;

  dreq_o    <= dreq_i and not flushing;
  valid_int <= (qfull and valid_i) or (not qempty and flushing and valid_mask);
  valid_o   <= valid_int;
  
end behavioral;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ep_shift_reg is
  generic(g_size : integer := 16);
  port(
    clk_i : in  std_logic;
    ce_i  : in  std_logic;
    d_i   : in  std_logic;
    q_o   : out std_logic);
end ep_shift_reg;

architecture rtl of ep_shift_reg is
  signal sreg : std_logic_vector(g_size-1 downto 0);
  signal size : std_logic_vector(3 downto 0);

  component SRL16E
    generic (
      INIT : bit_vector :=x"0000");
    port (
      Q   : out STD_ULOGIC;
      A0  : in  STD_ULOGIC;
      A1  : in  STD_ULOGIC;
      A2  : in  STD_ULOGIC;
      A3  : in  STD_ULOGIC;
      CE  : in  STD_ULOGIC;
      CLK : in  STD_ULOGIC;
      D   : in  STD_ULOGIC);
  end component;
  
begin  -- rtl
  size <= std_logic_vector(to_unsigned(g_size-1, 4));

  cmp_sreg: SRL16E
    port map (
      D   => d_i,
      Q   => q_o,
      CE  => ce_i,
      CLK => clk_i,
      A0  => size(0),
      A1  => size(1),
      A2  => size(2),
      A3  => size(3));
  end rtl;
