--! @file eca_wb_event.vhd
--! @brief ECA Event stream
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! Feed chunks of 5 words in from the Wishbone clock domain.
--! If the queue cannot fit 5 words, spit out errors until cycle end.
--! If a cycle terminates without writing 5 words, discard partial write.
--!
--------------------------------------------------------------------------------
--! This library is free software; you can redistribute it and/or
--! modify it under the terms of the GNU Lesser General Public
--! License as published by the Free Software Foundation; either
--! version 3 of the License, or (at your option) any later version.
--!
--! This library is distributed in the hope that it will be useful,
--! but WITHOUT ANY WARRANTY; without even the implied warranty of
--! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--! Lesser General Public License for more details.
--!  
--! You should have received a copy of the GNU Lesser General Public
--! License along with this library. If not, see <http://www.gnu.org/licenses/>.
---------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wishbone_pkg.all;
use work.eca_pkg.all;

entity eca_wb_event is
 port(
   w_clk_i   : in  std_logic;
   w_rst_n_i : in  std_logic;
   w_slave_i : in  t_wishbone_slave_in;
   w_slave_o : out t_wishbone_slave_out;
   e_clk_i   : in  std_logic;
   e_rst_n_i : in  std_logic;
   e_stb_o   : out std_logic;
   e_stall_i : in  std_logic;
   e_event_o : out t_event;
   e_time_o  : out t_time;
   e_param_o : out t_param;
   e_index_i : in  std_logic_vector(7 downto 0));
end eca_wb_event;

architecture rtl of eca_wb_event is
  -- Out of principle, tell quartus to leave my design alone.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";
  
  -- We track two kinds of pointers:
  --   Event pointers count completed writes of 5 words
  --   Word pointers are indexes into the FIFO
  --
  -- At any point in time, these event pointer relationships hold:
  --    re_sent <= re_ready <= rw_ready <= rw_limit <= rw_sent+4
  -- where re_ready is an old value of rw_ready; behind only by clock-crossing delay
  --   and rw_limit is an old value of rw_sent+4; behind by 1 clock cycle
  --   and rw_sent  is an old value of re_sent;  behind only by clock-crossing delay
  
  constant c_addr1_bits : natural := 5;
  constant c_addr5_bits : natural := c_addr1_bits - 2;
  
  subtype t_addr1 is unsigned(c_addr1_bits-1 downto 0);
  subtype t_addr5 is unsigned(c_addr5_bits-1 downto 0);
  
  type t_state is (S0, S1, S2, S3, S4, SERR);
  
  -- Registers in clock domain w_clk_i
  signal rw_ready_gray : t_addr5;
  signal rw1_sent_gray : t_addr5;
  signal rw0_sent_gray : t_addr5;
  signal rw_sent       : t_addr5;
  signal rw_limit      : t_addr5;
  
  signal rw_ready : t_addr5         := (others => '0');
  signal rw_state : t_state         := S0;
  signal rw_addr  : t_addr1         := (others => '0');
  signal rw_ok    : t_addr1         := (others => '0');
  
  -- Registers in clock domain e_clk_i
  signal re_sent_gray   : t_addr5;
  signal re1_ready_gray : t_addr5;
  signal re0_ready_gray : t_addr5;
  signal re_ready       : t_addr5;
  
  signal re_sent  : t_addr5   := (others => '0');
  signal re_state : t_state   := S0;
  signal re_stb   : std_logic := '0';
  signal re_addr  : t_addr1   := (others => '0');
  
  -- Signals in clock domain e_clk_i
  signal se_exit_s0 : boolean;
  signal se_addr    : t_addr1;
  signal se_data    : t_wishbone_data;
begin

  -- e_index_i is effectively constant and thus safe to cross clock domains
  w_slave_o.DAT(31 downto 8) <= (others => '0');
  w_slave_o.DAT( 7 downto 0) <= e_index_i;
  
  w_slave_o.STALL <= '0'; -- Never stall; report error if something bad happens
  w_slave_o.RTY <= '0';
  w_slave_o.INT <= '0'; -- This doesn't belong in the struct :-/
  
  Q : eca_sdp
    generic map(
      g_addr_bits  => c_addr1_bits,
      g_data_bits  => 32,
      g_dual_clock => true)
    port map(
      r_clk_i  =>  e_clk_i,
      r_addr_i => std_logic_vector(se_addr),
      r_data_o => se_data,
      w_clk_i  =>  w_clk_i,
      w_en_i   => '1',
      w_addr_i => std_logic_vector(rw_addr),
      w_data_i => w_slave_i.DAT);

  W : process(w_clk_i) is
    variable vw_addr1 : t_addr1;
  begin
    if rising_edge(w_clk_i) then
      rw_ready_gray <= unsigned(f_eca_gray_encode(std_logic_vector(rw_ready)));-- To domain E
      
      rw1_sent_gray <= re_sent_gray; -- From domain E
      rw0_sent_gray <= rw1_sent_gray;
      rw_sent <= unsigned(f_eca_gray_decode(std_logic_vector(rw0_sent_gray), 1));
      
      rw_limit <= rw_sent + 4;
      vw_addr1 := rw_addr + 1;
      
      w_slave_o.ACK <= '0';
      w_slave_o.ERR <= '0';
      
      if w_rst_n_i = '0' then
        rw_ready <= (others => '0');
        rw_state <= S0;
        rw_addr  <= (others => '0');
        rw_ok    <= (others => '0');
      else
        
        if w_slave_i.CYC = '0' then
          rw_state <= S0;
          rw_addr  <= rw_ok;
        else
          if w_slave_i.STB = '1' then
            if w_slave_i.WE = '0' then
              w_slave_o.ACK <= '1'; -- Reading always works
            else
              case rw_state is
                when S0 => 
                  -- If w_clk_i is fast, then fanout from comparison might cost...
                  if rw_ready = rw_limit then
                    rw_state <= SERR;
                    w_slave_o.ERR <= '1';
                  else
                    rw_state <= S1;
                    w_slave_o.ACK <= '1';
                    rw_addr <= vw_addr1;
                  end if;
                  
                when S1 =>
                  rw_state <= S2;
                  w_slave_o.ACK <= '1';
                  rw_addr <= vw_addr1;
                  
                when S2 =>
                  rw_state <= S3;
                  w_slave_o.ACK <= '1';
                  rw_addr <= vw_addr1;
                
                when S3 =>
                  rw_state <= S4;
                  w_slave_o.ACK <= '1';
                  rw_addr <= vw_addr1;
                  
                when S4 =>
                  rw_state <= S0;
                  w_slave_o.ACK <= '1';
                  rw_addr <= vw_addr1;
                  
                  rw_ready <= rw_ready + 1;
                  rw_ok    <= vw_addr1;
                
                when SERR =>
                  w_slave_o.ERR <= '1';
              end case;
            end if; -- WE
          end if; -- STB
        end if; -- CYC
      end if; -- reset
    end if; -- clock
  end process;
  
  e_stb_o <= re_stb;
  
  -- This cannot be inside the process because both re_addr and Q.r_addr_i latch se_addr
  se_exit_s0 <= (re_sent /= re_ready) and (re_stb = '0' or e_stall_i = '0');
  se_addr <= 
    re_addr when (re_state=S0 and not se_exit_s0) else
    (re_addr + 1);
  
  E : process(e_clk_i) is
  begin
    if rising_edge(e_clk_i) then
      re_sent_gray <= unsigned(f_eca_gray_encode(std_logic_vector(re_sent))); -- to domain W
    
      re1_ready_gray <= rw_ready_gray; -- from domain W
      re0_ready_gray <= re1_ready_gray;
      re_ready <= unsigned(f_eca_gray_decode(std_logic_vector(re0_ready_gray), 1));
      
      if e_rst_n_i = '0' then
        e_event_o <= (others => '0');
        e_time_o  <= (others => '0');
        e_param_o <= (others => '0');
        
        re_sent  <= (others => '0');
        re_state <= S0;
        re_stb   <= '0';
        re_addr  <= (others => '0');
      else
        
        re_addr <= se_addr;
        case re_state is
          when S0 => 
            if re_stb = '1' and e_stall_i = '0' then
              re_stb <= '0';
            end if;
            
            if re_stb = '0' or e_stall_i = '0' then
              e_event_o(63 downto 32) <= se_data;
            end if;
            
            if se_exit_s0 then
              re_state <= S1;
            end if;
            
          when S1 =>
            e_event_o(31 downto  0) <= se_data;
            re_state <= S2;
            
          when S2 =>
            e_time_o(63 downto 32) <= se_data;
            re_state <= S3;
            
          when S3 =>
            e_time_o(31 downto  0) <= se_data;
            re_state <= S4;
            
          when S4 =>
            e_param_o(31 downto  0) <= se_data;
            re_state <= S0;
            
            re_stb <= '1';
            re_sent <= re_sent + 1;
          
          when SERR => null; -- unreachable
        end case;
      end if;
    end if;
  end process;
  
end rtl;
