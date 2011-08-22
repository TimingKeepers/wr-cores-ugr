library ieee;
use ieee.std_logic_1164.all;

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

  type t_queue_entry is record
    d     : std_logic_vector(g_width-1 downto 0);
    valid : std_logic;
  end record;

  type t_queue_array is array(0 to g_size-1) of t_queue_entry;

  function f_queue_occupation(q : t_queue_array; check_empty : std_logic) return std_logic is
    variable i : integer;
  begin
    for i in 0 to q'length-1 loop
      if(q(i).valid = check_empty) then
        return '0';
      end if;
    end loop;  -- i
    return '1';
  end function;

  signal queue         : t_queue_array;
  signal qempty, qfull : std_logic;
  signal flushing      : std_logic;
  signal valid_mask    : std_logic;
  signal valid_int : std_logic;
  
  
begin  -- behavioral

  qempty <= f_queue_occupation(queue, '1');
  qfull  <= f_queue_occupation(queue, '0');

  p_queue : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' or purge_i = '1' then
        flushing <= '0';
        valid_mask <= '0';
        for i in 0 to queue'length-1 loop
          queue(i).valid <= '0';
          queue(i).d     <= (others => '0');
        end loop;  -- i
        
      else
        if(flushing = '1' and qempty = '1') then
          flushing <= '0';
        elsif(flush_i = '1' and qempty = '0') then
          flushing <= '1';
        end if;

        valid_mask <= dreq_i;
        
        if ((valid_i = '1') or (qempty = '0' and (flushing = '1' or flush_i = '1') and valid_int = '1')) then
          for i in 0 to queue'length-2 loop
            queue(i+1) <= queue(i);--
          end loop;  -- i
          queue(0).d     <= d_i;
          queue(0).valid <= valid_i;
        end if;
      end if;
    end if;
  end process;

  q_o     <= queue(queue'length-1).d;
  dreq_o  <= dreq_i and not flushing;
  valid_int <= (qfull and valid_i) or (not qempty and flushing and valid_mask);
  valid_o <= valid_int;
  
end behavioral;
