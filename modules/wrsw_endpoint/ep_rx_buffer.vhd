-------------------------------------------------------------------------------
-- Title      : 
-- Project    : WhiteRabbit Switch
-------------------------------------------------------------------------------
-- File       : ep_rx_buffer.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-Co-HT
-- Created    : 2010-04-26
-- Last update: 2011-05-27
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2010 Tomasz Wlostowski
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2010-04-26  0.2      twlostow        Created
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;

use work.genram_pkg.all;
use work.endpoint_private_pkg.all;

entity ep_rx_buffer is
  generic (
    g_size_log2 : integer := 12
    );
  port(
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

-------------------------------------------------------------------------------
-- Framer interface
-------------------------------------------------------------------------------

    fra_data_i    : in  std_logic_vector(15 downto 0);
    fra_ctrl_i    : in  std_logic_vector(4 -1 downto 0);
    fra_sof_p_i   : in  std_logic;
    fra_eof_p_i   : in  std_logic;
    fra_error_p_i : in  std_logic;
    fra_valid_i   : in  std_logic;
    fra_drop_o    : out std_logic;
    fra_bytesel_i : in  std_logic;

-------------------------------------------------------------------------------
-- WRF source
-------------------------------------------------------------------------------

    fab_data_o    : out std_logic_vector(15 downto 0);
    fab_ctrl_o    : out std_logic_vector(4 -1 downto 0);
    fab_sof_p_o   : out std_logic;
    fab_eof_p_o   : out std_logic;
    fab_error_p_o : out std_logic;
    fab_valid_o   : out std_logic;
    fab_bytesel_o : out std_logic;
    fab_dreq_i    : in  std_logic;

    ep_ecr_rx_en_fra_i : in std_logic;

    buffer_used_o : out std_logic_vector(7 downto 0);

    rmon_rx_overflow_o : out std_logic
    );

end ep_rx_buffer;

architecture behavioral of ep_rx_buffer is

  constant c_drop_threshold    : integer := 2**g_size_log2 - 800;
  constant c_release_threshold : integer := 2**(g_size_log2-1);

  constant c_ctrl_sof        : std_logic_vector(4 - 1 downto 0) := x"8";
  constant c_ctrl_payload_1b : std_logic_vector(4 - 1 downto 0) := x"9";
  constant c_ctrl_eof        : std_logic_vector(4 - 1 downto 0) := x"a";
  constant c_ctrl_eof_1b     : std_logic_vector(4 - 1 downto 0) := x"b";
  constant c_ctrl_eof_2b     : std_logic_vector(4 - 1 downto 0) := x"c";
  constant c_ctrl_eof_error  : std_logic_vector(4 - 1 downto 0) := x"d";

  signal threshold_hit : std_logic;

  signal wr_packed_ctrl : std_logic_vector(4-1 downto 0);
  signal wr_valid       : std_logic;

  signal rd_packed_ctrl : std_logic_vector(4-1 downto 0);
  signal rd_valid       : std_logic;

  signal fifo_reset_n                 : std_logic;
  signal fifo_usedw                 : std_logic_vector(g_size_log2-1 downto 0);
  signal fifo_wr_req, fifo_rd_req   : std_logic;
  signal fifo_empty                 : std_logic;
  signal fifo_rd_data, fifo_wr_data : std_logic_vector(19 downto 0);

  signal s_ones       : std_logic_vector(31 downto 0) := (others => '1');
  signal fra_drop_int : std_logic;
  
begin

  -- pack the control lines (sof_p, eof_p, error_p, valid, bytesel, ctrl) into
  -- 4-bit control field to reduce the memory size

  pack_ctrl : process(fra_sof_p_i, fra_eof_p_i, fra_error_p_i, fra_valid_i, fra_bytesel_i, fra_ctrl_i)
  begin
    if(fra_sof_p_i = '1') then
      wr_packed_ctrl <= c_ctrl_sof;
      wr_valid       <= '1';
    elsif(fra_eof_p_i = '1') then
      if(fra_valid_i = '1') then        -- valid data on EOF?
        if(fra_bytesel_i = '1') then
          wr_packed_ctrl <= c_ctrl_eof_1b;
        else
          wr_packed_ctrl <= c_ctrl_eof_2b;
        end if;
      else                              -- no data on EOF
        wr_packed_ctrl <= c_ctrl_eof;
      end if;
      wr_valid <= '1';
    elsif(fra_error_p_i = '1') then     -- fabric error
      wr_packed_ctrl <= c_ctrl_eof_error;
      wr_valid       <= '1';
    elsif(fra_valid_i = '1') then       -- valid data
      if(fra_bytesel_i = '1' and fra_ctrl_i = c_wrsw_ctrl_payload) then
        wr_packed_ctrl <= c_ctrl_payload_1b;
      else
        wr_packed_ctrl <= fra_ctrl_i;
      end if;
      wr_valid <= '1';
    else
      wr_packed_ctrl <= (others => 'X');
      wr_valid       <= '0';
    end if;
  end process;

  fifo_reset_n <= '0' when (rst_n_i = '0' or ep_ecr_rx_en_fra_i = '0') else '1';
  fifo_wr_req  <= wr_valid and not fra_drop_int;
  fifo_wr_data <= wr_packed_ctrl & fra_data_i;

  BUF_FIFO : generic_sync_fifo
    generic map (
      g_data_width => 20,
      g_size       => 2 ** g_size_log2,
      g_with_count => true)
    port map (
      rst_n_i        => fifo_reset_n,
      clk_i          => clk_sys_i,
      d_i            => fifo_wr_data,
      we_i           => fifo_wr_req,
      q_o            => fifo_rd_data,
      rd_i           => fifo_rd_req,
      empty_o        => fifo_empty,
      full_o         => open,
      almost_empty_o => open,
      almost_full_o  => open,
      count_o        => fifo_usedw);

  fifo_rd_req <= (not fifo_empty) and fab_dreq_i;

  rd_valid_gen : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0') then
        rd_valid <= '0';
      else
        rd_valid <= fifo_rd_req;
      end if;
    end if;
  end process;

  rd_packed_ctrl <= fifo_rd_data(19 downto 16);
  fab_data_o     <= fifo_rd_data(15 downto 0);

  unpack_ctrl : process(rd_packed_ctrl, rd_valid)
  begin
    if(rd_valid = '1') then
      case rd_packed_ctrl is
        when c_ctrl_eof =>
          fab_eof_p_o   <= '1';
          fab_sof_p_o   <= '0';
          fab_error_p_o <= '0';
          fab_bytesel_o <= '0';
          fab_valid_o   <= '0';
          fab_ctrl_o    <= c_wrsw_ctrl_none;

        when c_ctrl_eof_error =>
          fab_eof_p_o   <= '0';
          fab_sof_p_o   <= '0';
          fab_error_p_o <= '1';
          fab_bytesel_o <= '0';
          fab_valid_o   <= '0';
          fab_ctrl_o    <= c_wrsw_ctrl_none;

        when c_ctrl_eof_1b =>
          fab_eof_p_o   <= '1';
          fab_sof_p_o   <= '0';
          fab_error_p_o <= '0';
          fab_bytesel_o <= '1';
          fab_valid_o   <= '1';
          fab_ctrl_o    <= c_wrsw_ctrl_payload;

        when c_ctrl_eof_2b =>
          fab_eof_p_o   <= '1';
          fab_sof_p_o   <= '0';
          fab_error_p_o <= '0';
          fab_bytesel_o <= '0';
          fab_valid_o   <= '1';
          fab_ctrl_o    <= c_wrsw_ctrl_payload;

        when c_ctrl_sof =>
          fab_eof_p_o   <= '0';
          fab_sof_p_o   <= '1';
          fab_error_p_o <= '0';
          fab_bytesel_o <= '0';
          fab_valid_o   <= '0';
          fab_ctrl_o    <= c_wrsw_ctrl_none;

        when c_ctrl_payload_1b =>
          fab_eof_p_o   <= '0';
          fab_sof_p_o   <= '0';
          fab_error_p_o <= '0';
          fab_bytesel_o <= '1';
          fab_valid_o   <= '1';
          fab_ctrl_o    <= c_wrsw_ctrl_payload;
        when others =>
          fab_sof_p_o   <= '0';
          fab_eof_p_o   <= '0';
          fab_bytesel_o <= '0';
          fab_error_p_o <= '0';
          fab_valid_o   <= '1';
          fab_ctrl_o    <= rd_packed_ctrl;
      end case;
      
    else
      fab_sof_p_o   <= '0';
      fab_eof_p_o   <= '0';
      fab_error_p_o <= '0';
      fab_valid_o   <= '0';
      fab_bytesel_o <= '0';
      fab_ctrl_o    <= (others => 'X');
    end if;
  end process;

  check_overflow : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' or ep_ecr_rx_en_fra_i = '0' then
        fra_drop_int  <= '0';
        buffer_used_o <= (others => '0');
      else

-- check if we've reached the flow control threshold
        buffer_used_o <= fifo_usedw(fifo_usedw'high downto fifo_usedw'high - 8 + 1);

-- check if the buffer is almost full (3 words remaining) and eventually start
-- dropping packets

        if(fra_eof_p_i = '1') then
          if(unsigned(fifo_usedw) > to_unsigned(c_drop_threshold, fifo_usedw'length)) then
            fra_drop_int <= '1';
          end if;
        elsif(unsigned(fifo_usedw) = to_unsigned(c_release_threshold, fifo_usedw'length)) then
          fra_drop_int <= '0';
        end if;
      end if;
    end if;
  end process;


  rmon_rx_overflow_o <= fra_drop_int;
  fra_drop_o         <= fra_drop_int;

end behavioral;
