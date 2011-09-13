library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.gencores_pkg.all;              -- for gc_crc_gen
use work.endpoint_private_pkg.all;
use work.ep_wbgen2_pkg.all;

-- 1st deframing pipeline stage - CRC/PCS error/Size checker

entity ep_rx_crc_size_check is
  port(clk_sys_i : in std_logic;
       rst_n_i   : in std_logic;

       snk_fab_i:in t_ep_internal_fabric;
       snk_dreq_o  : out std_logic;

       src_fab_o:out t_ep_internal_fabric;
       src_dreq_i  : in  std_logic;
      
       rmon_o : inout t_rmon_triggers;
       regs_i : in t_ep_out_registers
       );

end ep_rx_crc_size_check;

architecture behavioral of ep_rx_crc_size_check is

  constant c_MIN_FRAME_SIZE : integer := 64;

  component ep_rx_bypass_queue
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

  signal byte_cntr     : unsigned(13 downto 0);
  signal is_runt       : std_logic;
  signal is_giant      : std_logic;
  signal size_check_ok : std_logic;

  signal state : t_state;


  signal q_flush : std_logic;
  signal q_purge : std_logic;
  signal q_valid : std_logic;
  signal q_data  : std_logic_vector(15 downto 0);
  signal q_bytesel : std_logic;
  signal q_dvalid : std_logic;
  
begin  -- behavioral

  crc_gen_reset <= snk_fab_i.sof or (not rst_n_i);
  
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
      en_i    => snk_fab_i.dvalid,
      half_i  => snk_fab_i.bytesel,
      data_i  => snk_fab_i.data(15 downto 0),
      match_o => crc_match,
      crc_o   => open);

  U_bypass_queue : ep_rx_bypass_queue
    generic map (
      g_size  => 2,
      g_width => 16)
    port map (
      rst_n_i => rst_n_i,
      clk_i   => clk_sys_i,
      d_i     => snk_fab_i.data,
      valid_i => snk_fab_i.dvalid,
      dreq_o  => snk_dreq_o,
      q_o     => q_data,
      valid_o => q_valid,
      dreq_i  => src_dreq_i,
      flush_i => q_flush,
      purge_i => q_purge);

  
  p_count_bytes : process (clk_sys_i, rst_n_i)
  begin  -- process
    if rising_edge(clk_sys_i) then
      if (rst_n_i = '0' or regs_i.ecr_rx_en_o = '0') then
        byte_cntr <= (others => '0');
        is_runt   <= '0';
        is_giant  <= '0';
      else
        if(snk_fab_i.sof = '1') then
          byte_cntr <= (others => '0');
        end if;

        if(snk_fab_i.dvalid = '1') then
          if(snk_fab_i.bytesel = '1') then
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

        if(byte_cntr > unsigned(regs_i.rfcr_mru_o)) then
          is_giant <= '1';
        else
          is_giant <= '0';
        end if;
        
      end if;
    end if;
  end process;

  size_check_ok <= '0' when (is_runt = '1' and regs_i.rfcr_a_runt_o = '0') or
                   (is_giant = '1' and regs_i.rfcr_a_giant_o = '0') else '1';


  p_gen_output : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then

      if rst_n_i = '0' or regs_i.ecr_rx_en_o = '0' then

        q_flush    <= '0';
        q_purge    <= '0';
        q_bytesel <= '0';

        state <= ST_WAIT_FRAME;

        rmon_o.rx_pcs_err <= '0';
        rmon_o.rx_giant   <= '0';
        rmon_o.rx_runt    <= '0';
        rmon_o.rx_crc_err <= '0';

        src_fab_o.sof <= '0';

      else
        case state is
          when ST_WAIT_FRAME =>
            q_flush           <= '0';
            q_purge           <= '0';
            rmon_o.rx_pcs_err <= '0';
            rmon_o.rx_giant   <= '0';
            rmon_o.rx_runt    <= '0';
            rmon_o.rx_crc_err <= '0';
            q_bytesel <='0';
            src_fab_o.eof     <= '0';
            src_fab_o.error   <= '0';
            src_fab_o.sof <= '0';

            if(snk_fab_i.sof = '1') then
              state <= ST_DATA;
              src_fab_o.sof <= '1';
            end if;

          when ST_DATA =>

            src_fab_o.sof<='0';
            
            if(snk_fab_i.dvalid= '1') then
              q_bytesel<=snk_fab_i.bytesel;
            end if;
            
            if(snk_fab_i.error = '1') then    -- an error from the source?

              src_fab_o.error         <= '1';
              rmon_o.rx_pcs_err <= '1';
              state             <= ST_WAIT_FRAME;
              q_purge           <= '1';

            end if;

            if(snk_fab_i.eof = '1') then

              if(regs_i.rfcr_keep_crc_o = '0') then
                q_purge    <= '1';
              else
                q_flush <= '1';
              end if;
        
            state <= ST_WAIT_FRAME;

            if(size_check_ok = '0' or crc_match = '0') then  -- bad frame?
              src_fab_o.error <= '1';
            else
              src_fab_o.eof <= '1';
            end if;


            rmon_o.rx_runt    <= is_runt and (not regs_i.rfcr_a_runt_o);
            rmon_o.rx_giant   <= is_giant and (not regs_i.rfcr_a_giant_o);
            rmon_o.rx_crc_err <= not crc_match;
            
            end if;
   
        end case;
      end if;
    end if;
  end process;

--  src_fab_o.sof <= regs_b.ecr_rx_en_o and snk_fab_i.sof;
  src_fab_o.dvalid<=q_valid;
  src_fab_o.data <=q_data;
  src_fab_o.bytesel<=snk_fab_i.bytesel or q_bytesel;
  
end behavioral;




