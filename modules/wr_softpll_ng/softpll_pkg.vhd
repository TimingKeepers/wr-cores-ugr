library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package softpll_pkg is

  type t_softpll_phase_detector_type is (CH_DDMTD, CH_BANGBANG, CH_DISABLED);

  type t_softpll_channel_config is
  record
    -- type of the phase detector to be used:
    -- - CH_DISABLED: channel is not used.
    -- - CH_DDMTD: same frequency as WR reference (125 MHz), but with phase shift
    -- control
    -- - CH_BANGBANG: Accumulating Bang-Bang detector: allows for N:M frequency ratios, but
    -- without fine phase control. BB lock frequency is equal to 125 MHz *
    -- bb_div_ref / bb_div_fb * 2.
    channel_mode : t_softpll_phase_detector_type;

    -- Index of the reference input. Relevant only for BB detectors, usually
    -- set to 0 (local WR clock)
    ref_input      : integer;
    -- Reference divider.
    bb_div_ref     : integer;
    -- Feedback divider.
    bb_div_fb      : integer;
    -- BB detector gating period, determines the bandwidth of the PLL. Phase
    -- error samples are passed to SoftPLL software every 2**g_log2_gating reference
    -- clock cycles.
    bb_log2_gating : integer;
  end record;

  type t_softpll_channel_config_array is array(0 to 7) of t_softpll_channel_config;

  constant c_softpll_channel_ddmtd : t_softpll_channel_config :=
    (channel_mode => CH_DDMTD, others => 0);

  constant c_softpll_channel_disabled : t_softpll_channel_config :=
    (channel_mode => CH_DISABLED, others => 0);

  constant c_softpll_channel_aux_100mhz : t_softpll_channel_config :=
    (channel_mode    => CH_BANGBANG,
      ref_input      => 0,
      bb_div_ref     => 5,
      bb_div_fb      => 8,
      bb_log2_gating => 15);

  constant c_softpll_default_channel_config : t_softpll_channel_config_array :=
    (0      => c_softpll_channel_ddmtd,
     1      => c_softpll_channel_aux_100mhz,
     others => c_softpll_channel_disabled);

  -- External 10 MHz input divider parameters. 
  constant c_softpll_ext_div_ref     : integer := 8;
  constant c_softpll_ext_div_fb      : integer := 50;
  constant c_softpll_ext_log2_gating : integer := 13;
  
  
end package;

package body softpll_pkg is

end softpll_pkg;
