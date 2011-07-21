library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.genram_pkg.all;
use work.wr_fabric_pkg.all;

entity ep_packet_filter is
  
  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    snk_data_i: in std_logic_vector(17 downto 0);
    snk_valid_i: in std_logic;
    snk_dreq_o: out std_logic;

    src_data_o: out std_logic_vector(17 downto 0);
    src_valid_o : out std_logic(17 downto 0);
    src_dreq_i: in std_logic;

    match_done_o: out std_logic;
    match_class_o: out std_logic_vector(7 downto 0);

    regs_b: inout t_ep_registers
    );

end ep_packet_filter;

architecture behavioral of ep_packet_filter is

  constant c_BACKLOG_SIZE      : integer := 128;
  constant c_BACKLOG_SIZE_LOG2 : integer := 7;

  constant c_OPC_AND : std_logic_vector(1 downto 0) : = "00";
  constant c_OPC_OR  : std_logic_vector(1 downto 0) : = "01";
  constant c_OPC_XOR : std_logic_vector(1 downto 0) : = "10";
  constant c_OPC_FIN : std_logic_vector(1 downto 0) := "11";

  type t_microcode_instruction is record
-- comparison value
    cmp_val    : std_logic_vector(15 downto 0);
-- comparison mask (nibbles)
    cmp_mask   : std_logic_vector(3 downto 0);
-- comparison offset (0 = 1st word of the frame)
    cmp_offset : std_logic_vector(5 downto 0);  -- 26
-- opcode
    cmp_en: std_logic;
    opcode : std_logic_vector(1 downto 0);  
    op_b   : std_logic_vector(4 downto 0);  
    op_dst : std_logic_vector(4 downto 0);
  end record;

  signal fifo_wr_ptr : unsigned(c_BACKLOG_SIZE_LOG2 - 1 downto 0);
  signal fifo_rd_ptr : unsigned(c_BACKLOG_SIZE_LOG2 - 1 downto 0);
  signal fifo_full : std_logic;
  signal fifo_next_wr_ptr : unsigned(c_BACKLOG_SIZE_LOG2 -1 downto 0);
  signal fifo_next_rd_ptr : unsigned(c_BACKLOG_SIZE_LOG2 -1 downto 0);
  
  signal pc        : unsigned(c_BACKLOG_SIZE_LOG2-1 downto 0);
  signal op        : t_microcode_instruction;
  signal R         : std_logic_vector(31 downto 0);
  
begin  -- behavioral
  R(0) <= '0';
  R(1) <= '1';

  snk_o.stall <= src_i.stall or fifo_full;


  fifo_next_rd_ptr <= fifo_rd_ptr + 1;
  fifo_next_wr_ptr <= fifo_wr_ptr + 1;

  fifo_full <= '1' when fifo_next_wr_ptr = fifo_rd_ptr else '0';
  

end behavioral;
