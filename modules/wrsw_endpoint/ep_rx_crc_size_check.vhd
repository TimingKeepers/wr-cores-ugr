library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.gencores_pkg.all;              -- for gc_crc_gen
use work.endpoint_private_pkg.all;

-- 1st deframing pipeline stage - CRC/PCS error/Size checker

entity ep_rx_crc_size_check is
  port(clk_sys_i : in std_logic;
       rst_n_i   : in std_logic;

       enable_i : in std_logic;

       snk_data_i  : in std_logic_vector(17 downto 0);
       snk_valid_i : in std_logic;
       snk_dreq_o  : out std_logic;

       src_data_o  : out std_logic_vector(17 downto 0);
       src_valid_o : out std_logic;
       src_dreq_i  : in  std_logic;

       rmon_o : inout t_rmon_triggers;
       regs_b : inout t_ep_registers
       );

end ep_rx_crc_size_check;

architecture behavioral of ep_rx_crc_size_check is

  constant c_MIN_FRAME_SIZE : integer := 64;

  component ep_crc_bypass_queue
    generic (
      g_size  : integer;
      g_width : integer);
    port (
      rst_n_i : in  std_logic;
      clk_i   : in  std_logic;
      d_i     : in  std_logic_vector(g_width-1 downto 0);
      valid_i : in  std_logic;
      dreq_o  : out std_logic;
      q_o     : out std_logic_vector(g_width-1 downto 0);
      valid_o : out std_logic;
      dreq_i  : in  std_logic;
      flush_i : in  std_logic;
      purge_i : in  std_logic);
  end component;

  type t_state is (ST_WAIT_FRAME, ST_DATA);

  signal crc_gen_enable : std_logic;
  signal crc_gen_reset  : std_logic;
  signal crc_match      : std_logic;

  signal byte_cntr     : unsigned(11 downto 0);
  signal is_runt       : std_logic;
  signal is_giant      : std_logic;
  signal size_check_ok : std_logic;

  signal state : t_state;

  signal snk_sof, snk_eof, snk_bytesel, snk_error, snk_dvalid : std_logic;

  signal q_flush : std_logic;
  signal q_purge : std_logic;
  signal q_valid : std_logic;
  signal q_data : std_logic_vector(17 downto 0);

  signal src_eof, src_bytesel, src_error : std_logic;
  signal valid_mask : std_logic;
  signal last_bytesel : std_logic;

  
begin  -- behavioral

  snk_sof     <= f_is_sof(snk_data_i, snk_valid_i);
  snk_eof     <= f_is_eof(snk_data_i, snk_valid_i);
  snk_bytesel <= f_is_single_byte(snk_data_i, snk_valid_i);
  snk_error   <= f_is_error(snk_data_i, snk_valid_i);
  snk_dvalid  <= f_is_data(snk_data_i, snk_valid_i);

  crc_gen_reset <= snk_sof or (not rst_n_i);

  U_rx_crc_generator : gc_crc_gen
    generic map (
      g_polynomial              => x"04C11DB7",
      g_init_value              => x"ffffffff",
      g_residue                 => x"1cdf4421",
      g_data_width              => 16,
      g_half_width              => 8,
      g_sync_reset              => 1,
      g_dual_width              => 1,
      g_registered_match_output => false)
    port map (
      clk_i   => clk_sys_i,
      rst_i   => crc_gen_reset,
      en_i    => snk_dvalid,
      half_i  => snk_bytesel,
      data_i  => snk_data_i(15 downto 0),
      match_o => crc_match,
      crc_o   => open);

  U_bypass_queue : ep_crc_bypass_queue
    generic map (
      g_size => 3,
      g_width => 18)
    port map (
      rst_n_i => rst_n_i,
      clk_i   => clk_sys_i,
      d_i     => snk_data_i,
      valid_i => snk_dvalid,
      dreq_o  => snk_dreq_o,
      q_o     => q_data,
      valid_o => q_valid,
      dreq_i  => src_dreq_i,
      flush_i => q_flush,
      purge_i => q_purge);

  
  p_count_bytes : process (clk_sys_i, rst_n_i)
  begin  -- process
    if rising_edge(clk_sys_i) then
      if (rst_n_i = '0' or enable_i = '0') then
        byte_cntr <= (others => '0');
        is_runt   <= '0';
        is_giant  <= '0';
      else
        if(snk_sof = '1') then
          byte_cntr <= (others => '0');
        end if;

        if(snk_dvalid = '1') then
          if(snk_bytesel = '1') then
            byte_cntr <= byte_cntr + 1;
          else
            byte_cntr <= byte_cntr + 2;
          end if;
        end if;

        if(byte_cntr < to_unsigned(c_MIN_FRAME_SIZE, byte_cntr'length)) then
          is_runt <= '1';
        else
          is_runt <= '0';
        end if;

        if(byte_cntr > unsigned(regs_b.rfcr_mru)) then
          is_giant <= '1';
        else
          is_giant <= '0';
        end if;
        
      end if;
    end if;
  end process;

  size_check_ok <= '0' when (is_runt = '1' and regs_b.rfcr_a_runt = '0') or
                   (is_giant = '1' and regs_b.rfcr_a_giant = '0') else '1';


  p_gen_output : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then

      if rst_n_i = '0' or enable_i = '0' then

        q_flush    <= '0';
        q_purge    <= '0';
        valid_mask <= '0';

        state <= ST_WAIT_FRAME;

        rmon_o.rx_pcs_err <= '0';
        rmon_o.rx_giant   <= '0';
        rmon_o.rx_runt    <= '0';
        rmon_o.rx_crc_err <= '0';

      else
        case state is
          when ST_WAIT_FRAME =>
            q_flush         <= '0';
            q_purge         <= '0';
            valid_mask      <= '1';
            rmon_o.rx_pcs_err <= '0';
            rmon_o.rx_giant   <= '0';
            rmon_o.rx_runt    <= '0';
            rmon_o.rx_crc_err <= '0';

            src_eof     <= '0';
            src_error   <= '0';
            src_bytesel <= '0';

            if(snk_sof = '1') then
              state   <= ST_DATA;
            end if;

          when ST_DATA =>

            if (snk_dvalid = '1') then
              last_bytesel <= snk_bytesel;
              valid_mask   <= '1';
            end if;

            if(snk_error = '1') then    -- an error from the source?

              src_error       <= '1';
              rmon_o.rx_pcs_err <= '1';
              state           <= ST_WAIT_FRAME;
              q_purge         <= '1';

            end if;

            if(snk_eof = '1') then      -- we've got a valid frame
              state <= ST_WAIT_FRAME;

              if(size_check_ok = '0' or crc_match = '0') then  -- bad frame?
                src_error <= '1';
              else
                src_eof <= '1';
              end if;

              if(regs_b.rfcr_keep_crc = '0') then
                valid_mask <= '0';
                q_purge    <= '1';
              else
                q_flush <= '1';
              end if;

              rmon_o.rx_runt    <= is_runt and (not regs_b.rfcr_a_runt);
              rmon_o.rx_giant   <= is_giant and (not regs_b.rfcr_a_giant);
              rmon_o.rx_crc_err <= not crc_match;
              
            end if;
        end case;
      end if;
    end if;
  end process;

  
end behavioral;




