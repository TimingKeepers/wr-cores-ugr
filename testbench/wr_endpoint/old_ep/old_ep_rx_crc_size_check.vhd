library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.gencores_pkg.all;              -- for gc_crc_gen
use work.old_endpoint_pkg.all;

-- 1st deframing pipeline stage - CRC/PCS error/Size checker

entity old_ep_rx_crc_size_check is
  port(clk_sys_i : in std_logic;
       rst_n_i   : in std_logic;

       enable_i : in std_logic;

       -- WRF input (no CTRL line - not necessary for this module)
       snk_data_i      : in  std_logic_vector(15 downto 0);
       snk_bytesel_i   : in  std_logic;
       snk_sof_p1_i    : in  std_logic;
       snk_eof_p1_i    : in  std_logic;
       snk_valid_i     : in  std_logic;
       snk_rerror_p1_i : in  std_logic;
       snk_dreq_o      : out std_logic;

       src_dreq_i      : in  std_logic;
       src_data_o      : out std_logic_vector(15 downto 0);
       src_bytesel_o   : out std_logic;
       src_sof_p1_o    : out std_logic;
       src_eof_p1_o    : out std_logic;
       src_valid_o     : out std_logic;
       src_rerror_p1_o : out std_logic;
       src_is_crc_o    : out std_logic;

       rmon_rx_crc_err_p_o : out std_logic;
       rmon_rx_runt_p_o    : out std_logic;
       rmon_rx_giant_p_o   : out std_logic;
       rmon_rx_pcs_err_p_o : out std_logic;

       ep_rfcr_a_runt_i   : in std_logic;
       ep_rfcr_a_giant_i  : in std_logic;
       ep_rfcr_keep_crc_i : in std_logic

       );

end old_ep_rx_crc_size_check;

architecture behavioral of old_ep_rx_crc_size_check is

  constant c_MIN_FRAME_SIZE : integer := 64;
  constant c_MAX_FRAME_SIZE : integer := 1522;


-- helper record type for delaying WRF signals 
  type t_delay_entry is record
    valid   : std_logic;
    data    : std_logic_vector(15 downto 0);
    bytesel : std_logic;
  end record t_delay_entry;

  type t_state is (ST_WAIT_FRAME, ST_DATA, ST_MASK_CRC);

  signal d0, d1   : t_delay_entry;
  signal last_bytesel : std_logic;

  signal crc_gen_enable : std_logic;
  signal crc_gen_reset  : std_logic;
  signal crc_match      : std_logic;

  signal byte_cntr     : unsigned(11 downto 0);
  signal is_runt       : std_logic;
  signal is_giant      : std_logic;
  signal size_check_ok : std_logic;

  signal state        : t_state;

  signal src_bytesel_int : std_logic;  
  signal bytesel_mask : std_logic;
  signal valid_mask   : std_logic;
  signal flush_delay  : std_logic;
  
begin  -- behavioral


  crc_gen_reset  <= snk_sof_p1_i or (not rst_n_i);
  crc_gen_enable <= snk_valid_i;

  U_rx_crc_generator : gc_crc_gen
    generic map (
      g_polynomial => x"04C11DB7",
      g_init_value => x"ffffffff",
      g_residue    => x"1cdf4421",
      g_data_width => 16,
      g_half_width => 8,
      g_sync_reset => 1,
      g_dual_width => 1)
    port map (
      clk_i   => clk_sys_i,
      rst_i   => crc_gen_reset,
      en_i    => crc_gen_enable,
      half_i  => snk_bytesel_i,
      data_i  => snk_data_i,
      match_o => crc_match,
      crc_o   => open);

  p_count_bytes : process (clk_sys_i, rst_n_i)
  begin  -- process
    if rising_edge(clk_sys_i) then
      if (rst_n_i = '0' or enable_i = '0') then
        byte_cntr <= (others => '0');
        is_runt   <= '0';
        is_giant  <= '0';
      else
        if(snk_sof_p1_i = '1') then
          byte_cntr <= (others => '0');
        end if;

        if(snk_valid_i = '1') then
          if(snk_bytesel_i = '1') then
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

        if(byte_cntr > to_unsigned(c_MAX_FRAME_SIZE, byte_cntr'length)) then
          is_giant <= '1';
        else
          is_giant <= '0';
        end if;
        
      end if;
    end if;
  end process;

  size_check_ok <= '0' when (is_runt = '1' and ep_rfcr_a_runt_i = '0') or
                   (is_giant = '1' and ep_rfcr_a_giant_i = '0') else '1';


  p_gen_output : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then

      if rst_n_i = '0' or enable_i = '0' then
        flush_delay <= '0';
        state       <= ST_WAIT_FRAME;

        src_eof_p1_o    <= '0';
        src_sof_p1_o <= '0';
        src_rerror_p1_o <= '0';

        rmon_rx_pcs_err_p_o <= '0';
        rmon_rx_giant_p_o   <= '0';
        rmon_rx_runt_p_o    <= '0';
        rmon_rx_crc_err_p_o <= '0';



        src_is_crc_o <= '0';
        valid_mask   <= '1';
--        bytesel_mask <= '0';
        flush_delay  <= '0';
      else
        case state is
          when ST_WAIT_FRAME =>
            flush_delay <= '0';

          
            valid_mask          <= '1';
--            bytesel_mask        <= '0';
            rmon_rx_pcs_err_p_o <= '0';
            rmon_rx_giant_p_o   <= '0';
            rmon_rx_runt_p_o    <= '0';
            rmon_rx_crc_err_p_o <= '0';

            src_eof_p1_o <= '0';
            src_sof_p1_o <= '0';
            src_rerror_p1_o <= '0';
            
            if(snk_sof_p1_i = '1') then
              state        <= ST_DATA;
              src_sof_p1_o <= '1';
            end if;

          when ST_DATA =>
            src_sof_p1_o <= '0';
            if(snk_valid_i = '1') then
              last_bytesel <= snk_bytesel_i;
            end if;
            
            if(snk_rerror_p1_i = '1') then  -- an error from the source?

              src_rerror_p1_o <= '1';
              rmon_rx_pcs_err_p_o <= '1';  -- account for the error in RMON block
              state <= ST_WAIT_FRAME;
              flush_delay <= '1';

            elsif(snk_eof_p1_i = '1') then  -- we've got a valid frame


                if(ep_rfcr_keep_crc_i = '0') then
--                  src_eof_p1_o <= '1';
                  valid_mask   <= '0';
                else
                  src_is_crc_o <= '1';
                end if;

                state       <= ST_MASK_CRC;
                flush_delay <= '1';
              end if;
            
          when ST_MASK_CRC =>
                rmon_rx_runt_p_o  <= is_runt and (not ep_rfcr_a_runt_i);
                rmon_rx_giant_p_o <= is_giant and (not ep_rfcr_a_giant_i);
                rmon_rx_crc_err_p_o <= not crc_match;

                if(size_check_ok = '0' or crc_match = '0') then  -- bad frame?
                  src_rerror_p1_o <= '1';
                  state           <= ST_WAIT_FRAME;
                else
                  src_eof_p1_o <= '1';
                end if;

                if(ep_rfcr_keep_crc_i = '1') then  -- preserve the CRC field in the
                  src_is_crc_o <= '1';
                else
                  src_is_crc_o <= '0';
                end if;

              
              
            if(d1.valid = '0') then
              src_eof_p1_o <= '0';
              src_rerror_p1_o <= '0';
              state       <= ST_WAIT_FRAME;
              flush_delay <= '0';
            end if;
        end case;
      end if;
    end if;
  end process;


  bytesel_mask <= last_bytesel when snk_eof_p1_i = '1' else '0';
  
-- write: d_write and (not full or d_read)


--  d_advance = src_dreq_i and snk_valid_o;

  p_delay : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0') then
        d0.valid    <= '0';
        d1.valid    <= '0';
--        d2.valid    <= '0';
        src_valid_o <= '0';
      else

        if(flush_delay = '1') then
          d0.valid <= '0';
          d1.valid <= '0';
        elsif(snk_valid_i = '1') then
          d0.valid      <= snk_valid_i;
          d0.data       <= snk_data_i;
          d0.bytesel    <= snk_bytesel_i;
          d1            <= d0;
--          d2            <= d1;
          src_data_o    <= d1.data;
          src_valid_o   <= d1.valid and valid_mask;
          src_bytesel_int <= d1.bytesel;
        else
          src_valid_o <= '0';
        end if;
      end if;
    end if;
  end process;

  snk_dreq_o <= src_dreq_i and (not flush_delay);
  src_bytesel_o <= src_bytesel_int or d0.bytesel;
-- comb i/o assignments

end behavioral;




