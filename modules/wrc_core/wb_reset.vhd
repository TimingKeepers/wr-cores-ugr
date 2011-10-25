-------------------------------------------------------------------------------
-- Title      : WhiteRabbit PTP Core ZPU reset generator
-- Project    : WhiteRabbit
-------------------------------------------------------------------------------
-- File       : wb_reset.vhd
-- Author     : Grzegorz Daniluk
-- Company    : Elproma
-- Created    : 2011-04-04
-- Last update: 2011-06-16
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description:
-- WB_RESET is a reset signal generator for ZPU. It is controlled by wishbone
-- and is used by ZPU firmware loader(zpu-loader) to reset the processor during 
-- copying the binary to dpram.
-------------------------------------------------------------------------------
-- Copyright (c) 2011 Grzegorz Daniluk
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2011-04-04  1.0      greg.d          Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity wb_reset is
  port(
    clk_i      : in  std_logic;
    rst_n_i    : in  std_logic;
    genrst_n_o : out std_logic;

    wb_addr_i : in  std_logic_vector(1 downto 0);
    wb_data_i : in  std_logic_vector(31 downto 0);
    wb_data_o : out std_logic_vector(31 downto 0);
    wb_sel_i  : in  std_logic_vector(3 downto 0);
    wb_stb_i  : in  std_logic;
    wb_cyc_i  : in  std_logic;
    wb_we_i   : in  std_logic;
    wb_ack_o  : out std_logic
    );
end wb_reset;

architecture behaviour of wb_reset is

  constant c_RST_REG : std_logic_vector(1 downto 0) := "00";

  signal rst_reg : std_logic_vector(7 downto 0);
  signal grst_n  : std_logic_vector(20 downto 0);
  signal ack_int : std_logic;
  
begin

  process(clk_i, rst_n_i)
  begin
    if(rst_n_i = '0') then
      ack_int <= '0';
      rst_reg <= (others => '0');
    elsif(rising_edge(clk_i)) then
      if(wb_stb_i = '1' and wb_cyc_i = '1' and ack_int = '0') then
        if(wb_we_i = '1') then
          case wb_addr_i is
            when c_RST_REG =>
              rst_reg <= wb_data_i(7 downto 0);
            when others =>
          end case;
        end if;
        ack_int <= '1';
      else
        ack_int <= '0';
      end if;
    end if;
  end process;

  wb_ack_o <= ack_int;

  process(clk_i, rst_n_i)
  begin
    if(rst_n_i = '0') then
      grst_n <= (others => '1');
    elsif(rising_edge(clk_i)) then

      if(rst_reg(0) = '1') then
        grst_n(0) <= '0';
      else
        grst_n(0) <= '1';
      end if;

      grst_n(grst_n'left downto 1) <= grst_n(grst_n'left-1 downto 0);
      
    end if;
  end process;
  genrst_n_o <= grst_n(grst_n'left);

end behaviour;
