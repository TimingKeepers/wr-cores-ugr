library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.endpoint_private_pkg.all;
use work.gencores_pkg.all;

entity ep_leds_controller is
  
  generic (
    g_blink_period_log2 : integer := 21);

  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    -- data valid (from PCS <> framers)
    dvalid_tx_i : in std_logic;
    dvalid_rx_i : in std_logic;

    link_ok_i : in std_logic;

    led_link_o : out std_logic;
    led_act_o  : out std_logic
    );  

end ep_leds_controller;

architecture rtl of ep_leds_controller is

  type t_state is (INACTIVE, BLINKING);

  signal cnt                    : unsigned(g_blink_period_log2-1 downto 0);
  signal cnt_reset, cnt_expired : std_logic;
  signal state                  : t_state;
  signal led_act                : std_logic;
  signal last_cycle_act         : std_logic;
  signal txrx, txrx_synced      : std_logic;
begin  -- rtl

  led_link_o <= link_ok_i;

  txrx <= dvalid_rx_i or dvalid_tx_i;

  U_Sync_Activity : gc_sync_ffs
    port map (
      clk_i    => clk_sys_i,
      rst_n_i  => rst_n_i,
      data_i   => txrx,
      synced_o => txrx_synced);

  p_counter : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      
      if(rst_n_i = '0' or cnt_reset = '1')then
        cnt <= (others => '0');
      else
        cnt <= cnt + 1;
        if((not cnt) = 0) then
          cnt_expired <= '1';
        else
          cnt_expired <= '0';
        end if;
      end if;
    end if;
  end process;

  p_gen_act_led : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        state     <= INACTIVE;
        led_act   <= '0';
        cnt_reset <= '0';
      else
        case state is
          when INACTIVE =>
            if(txrx_synced = '1') then
              state     <= BLINKING;
              led_act   <= '1';
              cnt_reset <= '1';
            end if;
          when BLINKING =>
            cnt_reset <= '0';
            if(cnt_expired = '1') then
              led_act <= not led_act;
              if(last_cycle_act = '0') then
                led_act <= '0';
                state   <= INACTIVE;
              end if;
              last_cycle_act <= '0';
            else
              if(txrx_synced = '1') then
                last_cycle_act <= '1';
              end if;
            end if;
        end case;
      end if;
    end if;
  end process;

  led_act_o <= led_act;
  
end rtl;
