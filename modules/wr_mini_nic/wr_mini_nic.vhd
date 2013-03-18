-------------------------------------------------------------------------------
-- Title      : Mini Embedded DMA Network Interface Controller
-- Project    : WhiteRabbit Core
-------------------------------------------------------------------------------
-- File       : wrsw_mini_nic.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-Co-HT
-- Created    : 2010-07-26
-- Last update: 2012-08-28
-- Platform   : FPGA-generic
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description: Module implements a simple NIC with DMA controller. It
-- sends/receives the packets using WR switch fabric interface (see the
-- wrsw_endpoint.vhd for the details). Packets are stored and read from the
-- system memory via simple memory bus. WR endpoint-compatible TX timestamping
-- unit is also included.
-------------------------------------------------------------------------------
-- Copyright (c) 2010, 2011 CERN
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2010-07-26  1.0      twlostow        Created
-- 2010-08-16  1.0      twlostow        Bugfixes, linux compatibility added
-- 2011-08-03  2.0      greg.d          rewritten to use pipelined Wishbone
-- 2011-10-45  2.1      twlostow        bugfixes...
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wr_fabric_pkg.all;
use work.wishbone_pkg.all;
use work.minic_wbgen2_pkg.all;


entity wr_mini_nic is

  generic (
    g_interface_mode       : t_wishbone_interface_mode      := CLASSIC;
    g_address_granularity  : t_wishbone_address_granularity := WORD;
    g_memsize_log2         : integer                        := 14;
    g_buffer_little_endian : boolean                        := false);

  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

-------------------------------------------------------------------------------
-- System memory i/f
-------------------------------------------------------------------------------

    mem_data_o : out std_logic_vector(31 downto 0);
    mem_addr_o : out std_logic_vector(g_memsize_log2-1 downto 0);
    mem_data_i : in  std_logic_vector(31 downto 0);
    mem_wr_o   : out std_logic;

-------------------------------------------------------------------------------
-- Pipelined Wishbone interface
-------------------------------------------------------------------------------

    -- WBP Master (TX)
    src_dat_o   : out std_logic_vector(15 downto 0);
    src_adr_o   : out std_logic_vector(1 downto 0);
    src_sel_o   : out std_logic_vector(1 downto 0);
    src_cyc_o   : out std_logic;
    src_stb_o   : out std_logic;
    src_we_o    : out std_logic;
    src_stall_i : in  std_logic;
    src_err_i   : in  std_logic;
    src_ack_i   : in  std_logic;

    -- WBP Slave (RX)
    snk_dat_i   : in  std_logic_vector(15 downto 0);
    snk_adr_i   : in  std_logic_vector(1 downto 0);
    snk_sel_i   : in  std_logic_vector(1 downto 0);
    snk_cyc_i   : in  std_logic;
    snk_stb_i   : in  std_logic;
    snk_we_i    : in  std_logic;
    snk_stall_o : out std_logic;
    snk_err_o   : out std_logic;
    snk_ack_o   : out std_logic;

-------------------------------------------------------------------------------
-- TXTSU i/f
-------------------------------------------------------------------------------

    txtsu_port_id_i     : in  std_logic_vector(4 downto 0);
    txtsu_frame_id_i    : in  std_logic_vector(16 - 1 downto 0);
    txtsu_tsval_i       : in  std_logic_vector(28 + 4 - 1 downto 0);
    txtsu_tsincorrect_i : in  std_logic;
    txtsu_stb_i         : in  std_logic;
    txtsu_ack_o         : out std_logic;

-------------------------------------------------------------------------------
-- Wishbone slave
-------------------------------------------------------------------------------    

    wb_cyc_i   : in  std_logic;
    wb_stb_i   : in  std_logic;
    wb_we_i    : in  std_logic;
    wb_sel_i   : in  std_logic_vector(c_wishbone_data_width/8-1 downto 0);
    wb_adr_i   : in  std_logic_vector(c_wishbone_address_width-1 downto 0);
    wb_dat_i   : in  std_logic_vector(c_wishbone_data_width-1 downto 0);
    wb_dat_o   : out std_logic_vector(c_wishbone_data_width-1 downto 0);
    wb_ack_o   : out std_logic;
    wb_stall_o : out std_logic;
    wb_int_o   : out std_logic
    );
end wr_mini_nic;

architecture behavioral of wr_mini_nic is

  component minic_wb_slave
    port (
      rst_n_i          : in  std_logic;
      clk_sys_i        : in  std_logic;
      wb_adr_i         : in  std_logic_vector(4 downto 0);
      wb_dat_i         : in  std_logic_vector(31 downto 0);
      wb_dat_o         : out std_logic_vector(31 downto 0);
      wb_cyc_i         : in  std_logic;
      wb_sel_i         : in  std_logic_vector(3 downto 0);
      wb_stb_i         : in  std_logic;
      wb_we_i          : in  std_logic;
      wb_ack_o         : out std_logic;
      wb_stall_o       : out std_logic;
      wb_int_o         : out std_logic;
      tx_ts_read_ack_o : out std_logic;
      irq_tx_i         : in  std_logic;
      irq_tx_ack_o     : out std_logic;
      irq_tx_mask_o    : out std_logic;
      irq_rx_i         : in  std_logic;
      irq_rx_ack_o     : out std_logic;
      irq_txts_i       : in  std_logic;
      regs_i           : in  t_minic_in_registers;
      regs_o           : out t_minic_out_registers
    );
  end component;

  --type t_wrf_status_reg is record
  --  is_hp       : std_logic;
  --  has_smac    : std_logic;
  --  has_crc     : std_logic;
  --  error       : std_logic;
  --  tag_me      : std_logic;
  --  match_class : std_logic_vector(7 downto 0);
  --end record;

  function f_buf_swap_endian_32
    (
      data : std_logic_vector(31 downto 0)
      ) return std_logic_vector is
  begin
    if(g_buffer_little_endian = true) then
      return data(7 downto 0) & data(15 downto 8) & data(23 downto 16) & data(31 downto 24);
    else
      return data;
    end if;
  end function f_buf_swap_endian_32;


  signal src_cyc_int   : std_logic;
  signal src_stb_int   : std_logic;
  signal snk_stall_int : std_logic;


-----------------------------------------------------------------------------
-- memory interface signals
-----------------------------------------------------------------------------

  signal ntx_mem_d  : std_logic_vector(31 downto 0);
  signal ntx_mem_a  : unsigned(g_memsize_log2-1 downto 0);
  signal nrx_mem_d  : std_logic_vector(31 downto 0);
  signal nrx_mux_d  : std_logic;
  signal nrx_mem_a  : unsigned(g_memsize_log2-1 downto 0);
  signal nrx_mem_wr : std_logic;
  signal mem_arb_rx : std_logic;
  signal mem_arb_tx : std_logic;


-------------------------------------------------------------------------------
-- TX FSM stuff
-------------------------------------------------------------------------------

  type t_tx_fsm_state is (TX_IDLE, TX_READ_DESC, TX_STATUS, TX_START_PACKET, TX_HWORD, TX_LWORD, TX_END_PACKET, TX_OOB1, TX_OOB2);

  alias ntx_desc_size is ntx_mem_d(11 downto 0);
  alias ntx_desc_oob is ntx_mem_d(27 downto 12);
  alias ntx_desc_valid is ntx_mem_d(31);
  alias ntx_desc_with_oob is ntx_mem_d(30);
  alias ntx_desc_802_1q is ntx_mem_d(29);
  alias ntx_desc_has_src_mac is ntx_mem_d(28);

  --STATUS Reg for TX path
  --signal ntx_status_reg : std_logic_vector(15 downto 0);
  --alias ntx_status_class is ntx_status_reg(7 downto 0);
  --alias ntx_status_tagme is ntx_status_reg(8);
  --alias ntx_status_err is ntx_status_reg(9);
  --alias ntx_status_crc is ntx_status_reg(10);
  --alias ntx_status_smac is ntx_status_reg(11);
  --alias ntx_status_hp is ntx_status_reg(12);

  signal ntx_status_reg : t_wrf_status_reg;

  signal ntx_data_reg : std_logic_vector(31 downto 0);

  signal ntx_cntr_is_zero    : std_logic;
  signal ntx_cntr_is_one     : std_logic;
  signal ntx_timeout_is_zero : std_logic;
  signal ntx_cntr            : unsigned(11 downto 0);
  signal ntx_timeout         : unsigned(7 downto 0);

  signal ntx_ack_count     : unsigned(2 downto 0);
  signal ntx_has_oob       : std_logic;
  signal ntx_state         : t_tx_fsm_state;
  signal ntx_start_delayed : std_logic;
  signal ntx_size_odd      : std_logic;
  signal ntx_oob_reg       : std_logic_vector(15 downto 0);
  signal snk_cyc_d0        : std_logic;

-------------------------------------------------------------------------------
-- RX FSM stuff
-------------------------------------------------------------------------------  

  type t_rx_fsm_state is (RX_WAIT_SOF, RX_MEM_RESYNC, RX_MEM_FLUSH, RX_ALLOCATE_DESCRIPTOR, RX_DATA, RX_UPDATE_DESC, RX_IGNORE, RX_BUF_FULL);

  signal nrx_state    : t_rx_fsm_state;
  signal nrx_avail    : unsigned(g_memsize_log2-1 downto 0);
  signal nrx_bufstart : unsigned(g_memsize_log2-1 downto 0);
  signal nrx_bufsize  : unsigned(g_memsize_log2-1 downto 0);
  signal nrx_toggle   : std_logic;
  signal nrx_oob_reg  : std_logic_vector(15 downto 0);

  --STATUS Reg for RX path
  signal nrx_status_reg : t_wrf_status_reg;


  signal nrx_error                : std_logic;
  signal nrx_mem_a_saved          : unsigned(g_memsize_log2-1 downto 0);
  signal nrx_has_oob              : std_logic;
  signal nrx_bytesel              : std_logic;
  signal nrx_size                 : unsigned(g_memsize_log2-1 downto 0);
  signal nrx_rdreg                : std_logic_vector(31 downto 0);
  signal nrx_buf_full             : std_logic;
  signal nrx_stall_mask           : std_logic;
  signal nrx_valid                : std_logic;
  signal nrx_done                 : std_logic;
  signal nrx_drop, nrx_stat_error : std_logic;
  signal nrx_class                : std_logic_vector(7 downto 0);

-------------------------------------------------------------------------------
-- Classic Wishbone slave signals
-------------------------------------------------------------------------------  

  signal regs_in  : t_minic_in_registers;
  signal regs_out : t_minic_out_registers;

  signal wb_in  : t_wishbone_master_in;
  signal wb_out : t_wishbone_master_out;

  signal irq_tx     : std_logic;
  signal irq_rx_ack : std_logic;
  signal irq_rx     : std_logic;

  signal nrx_newpacket, nrx_newpacket_d0 : std_logic;

  signal irq_txts    : std_logic;
  signal irq_tx_ack  : std_logic;
  signal irq_tx_mask : std_logic;

  signal txtsu_ack_int : std_logic;
  signal mem_addr_int  : std_logic_vector(g_memsize_log2-1 downto 0);
  signal mem_wr_int    : std_logic;
  signal bad_addr      : std_logic;


begin  -- behavioral

  --chipscope_ila_1 : chipscope_ila
  --  port map (
  --    CONTROL => CONTROL,
  --    CLK     => clk_sys_i,
  --    TRIG0   => TRIG0,
  --    TRIG1   => TRIG1,
  --    TRIG2   => TRIG2,
  --    TRIG3   => TRIG3);

  --chipscope_icon_1 : chipscope_icon
  --  port map (
  --    CONTROL0 => CONTROL);


-------------------------------------------------------------------------------
-- memory access arbitration
-----------------------------------------------------------------------------

  arbitrate_mem : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        mem_arb_rx <= '0';
      else
        mem_arb_rx <= not mem_arb_rx;
      end if;
    end if;
  end process;

  mem_arb_tx   <= not mem_arb_rx;
  mem_addr_int <= std_logic_vector(ntx_mem_a) when mem_arb_rx = '0' else std_logic_vector(nrx_mem_a);
  mem_data_o   <= nrx_mem_d;
  ntx_mem_d    <= mem_data_i;
  --mem_wr_int   <= (nrx_mem_wr and not bad_addr) when mem_arb_rx = '1' else '0';
  --mem_wr_o <= mem_wr_int;
  mem_wr_o     <= nrx_mem_wr                  when mem_arb_rx = '1' else '0';

  mem_addr_o <= mem_addr_int;



  p_gen_bad_addr : process(mem_addr_int, regs_out)
  begin
    if(unsigned(mem_addr_int) < unsigned(regs_out.mprot_lo_o(g_memsize_log2-1 downto 0))) then
      bad_addr <= '1';
    elsif(unsigned(mem_addr_int) > unsigned(regs_out.mprot_hi_o(g_memsize_log2-1 downto 0))) then
      bad_addr <= '1';
    else
      bad_addr <= '0';
    end if;
  end process;

-------------------------------------------------------------------------------
-- TX Path  (Host -> Fabric)
-------------------------------------------------------------------------------  

-- helper signals to avoid big IF conditions in the FSM
  ntx_cntr_is_zero    <= '1' when (ntx_cntr = to_unsigned(0, ntx_cntr'length))       else '0';
  ntx_cntr_is_one     <= '1' when (ntx_cntr = to_unsigned(1, ntx_cntr'length))       else '0';
  ntx_timeout_is_zero <= '1' when (ntx_timeout = to_unsigned(0, ntx_timeout'length)) else '0';

  p_count_acks : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' or src_cyc_int = '0' or src_err_i = '1' then
        ntx_ack_count <= (others => '0');
      else
        if(src_stb_int = '1' and src_stall_i = '0' and src_ack_i = '0') then
          ntx_ack_count <= ntx_ack_count + 1;
        elsif(src_ack_i = '1' and not(src_stb_int = '1' and src_stall_i = '0')) then
          ntx_ack_count <= ntx_ack_count - 1;
        end if;
      end if;
    end if;
  end process;


  p_tx_fsm : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        src_dat_o         <= (others => '0');
        src_adr_o         <= (others => '0');
        src_cyc_int       <= '0';
        src_stb_int       <= '0';
        irq_tx            <= '0';
        ntx_has_oob       <= '0';
        ntx_mem_a         <= (others => '0');
        ntx_cntr          <= (others => '0');
        ntx_data_reg      <= (others => '0');
        ntx_start_delayed <= '0';
        ntx_state         <= TX_IDLE;

        regs_in.mcr_tx_error_i <= '0';
        regs_in.mcr_tx_idle_i  <= '0';

      else
        case ntx_state is

-------------------------------------------------------------------------------
-- Idle state: we wait until the host starts the DMA transfer
-------------------------------------------------------------------------------
          when TX_IDLE =>
            src_cyc_int <= '0';
            src_stb_int <= '0';
            -- keep the TX start bit (it's active for single clock cycle) in
            -- case we needed to align the FSM cycle with the memory arbiter
            if(regs_out.mcr_tx_start_o = '1') then
              ntx_start_delayed <= '1';
            end if;

            -- TX interrupt is disabled. Just assert the TX_IDLE.
            if(irq_tx_mask = '0') then
              regs_in.mcr_tx_idle_i <= '1';
            elsif(irq_tx_ack = '1') then
              irq_tx                <= '0';
              regs_in.mcr_tx_idle_i <= '1';
            end if;

            -- initialize timeout of TX_END_PACKET
            ntx_timeout <= to_unsigned(100, ntx_timeout'length);

            -- the host loaded new TX DMA buffer address
            if(regs_out.tx_addr_load_o = '1') then
              ntx_mem_a <= unsigned(regs_out.tx_addr_o(g_memsize_log2+1 downto 2));
            end if;

            -- the host started the DMA xfer (and we are "phased" with the arbiter)
            if(src_err_i = '0' and ntx_start_delayed = '1' and mem_arb_tx = '0') then
              ntx_state              <= TX_READ_DESC;
              -- clear the TX flags
              regs_in.mcr_tx_error_i <= '0';
              regs_in.mcr_tx_idle_i  <= '0';
              ntx_start_delayed      <= '0';
            end if;


-------------------------------------------------------------------------------
-- Read descriptor header state: check if there's another descriptor in the buffer
-- and eventually, start transmitting it
-------------------------------------------------------------------------------
          when TX_READ_DESC =>

            if(mem_arb_tx = '0') then   -- memory is ready?
              -- feed the current TX DMA address as the readback value of TX_ADDR Wishbone
              -- register
              regs_in.tx_addr_i(g_memsize_log2+1 downto 0)                      <= std_logic_vector(ntx_mem_a) & "00";
              regs_in.tx_addr_i(regs_in.tx_addr_i'high downto g_memsize_log2+2) <= (others => '0');

              -- if we have no more valid TX descriptors, trigger an interrupt and wait for
              -- another DMA transfer
              if(ntx_desc_valid = '0') then
                ntx_state <= TX_IDLE;
                irq_tx    <= '1';
              else
                -- read the descriptor contents (size, 802.1q/OOB enables)
                ntx_cntr                   <= unsigned(ntx_desc_size);
                ntx_oob_reg                <= ntx_desc_oob;
                ntx_has_oob                <= ntx_desc_with_oob;
                ntx_status_reg.is_hp       <= '0';
                ntx_status_reg.has_smac    <= ntx_desc_has_src_mac;
                ntx_status_reg.has_crc     <= '0';
                ntx_status_reg.error       <= '0';
                ntx_status_reg.tag_me      <= '0';
                ntx_status_reg.match_class <= (others => '0');
                ntx_state                  <= TX_STATUS;
                ntx_mem_a                  <= ntx_mem_a + 1;
              end if;
            end if;

-------------------------------------------------------------------------------
-- State status: send status word
-------------------------------------------------------------------------------
          when TX_STATUS =>

            src_cyc_int <= '1';
            src_adr_o   <= c_WRF_STATUS;

            if(src_stall_i = '0') then
              src_stb_int <= '1';
              src_dat_o   <= f_marshall_wrf_status(ntx_status_reg);
              ntx_state   <= TX_START_PACKET;
            end if;


-------------------------------------------------------------------------------
-- Start packet state: asserts CYC signal and reads first word to transmit
-------------------------------------------------------------------------------            
          when TX_START_PACKET =>

            src_cyc_int <= '1';
            src_stb_int <= '0';
            -- check if the memory is ready, read the 1st word of the payload
            if(src_stall_i = '0' and mem_arb_tx = '0') then
              ntx_data_reg <= f_buf_swap_endian_32(ntx_mem_d);
              src_cyc_int  <= '1';
              ntx_state    <= TX_HWORD;
              ntx_mem_a    <= ntx_mem_a + 1;
            end if;



--------------------------------------------------------------------------------
-- State "Transmit HI word" - transmit the most significant word of the packet
-------------------------------------------------------------------------------
          when TX_HWORD =>


            src_cyc_int <= '1';
            src_adr_o   <= c_WRF_DATA;

            if(src_err_i = '1') then
              regs_in.mcr_tx_error_i <= '1';
              irq_tx                 <= '1';
              ntx_state              <= TX_IDLE;
            else
              if(src_stall_i = '0') then
                src_dat_o   <= ntx_data_reg(31 downto 16);
                src_stb_int <= '1';

                if(ntx_cntr_is_zero = '1') then
                  src_stb_int <= '0';
                  ntx_state   <= TX_OOB1;
                  --  ntx_mem_a <= ntx_mem_a + 1;
                else
                  ntx_cntr  <= ntx_cntr - 1;
                  ntx_state <= TX_LWORD;
                end if;
              end if;
            end if;


--------------------------------------------------------------------------------
-- State "Transmit LO word" - transmit the least significant word of the packet
-------------------------------------------------------------------------------
          when TX_LWORD =>

            src_cyc_int <= '1';
            src_adr_o   <= c_WRF_DATA;

            if(src_err_i = '1') then
              regs_in.mcr_tx_error_i <= '1';
              irq_tx                 <= '1';
              ntx_state              <= TX_IDLE;
            end if;


            -- the TX fabric is ready, the memory is ready and we haven't reached the end
            -- of the packet yet:

            if(src_stall_i = '0') then
              src_stb_int <= '1';

              src_dat_o <= ntx_data_reg (15 downto 0);

              if(mem_arb_tx = '0' and ntx_cntr_is_zero = '0') then
                ntx_data_reg <= f_buf_swap_endian_32(ntx_mem_d);
                ntx_cntr     <= ntx_cntr - 1;
                if(ntx_cntr_is_one = '0') then
                  ntx_mem_a <= ntx_mem_a + 1;
                end if;
                ntx_state <= TX_HWORD;

              elsif(ntx_cntr_is_zero = '1') then
                -- We're at the end of the packet
                src_stb_int <= '0';
                ntx_state   <= TX_OOB1;
              else
                src_stb_int <= '0';
              end if;
            elsif(src_stall_i = '1' and ntx_cntr_is_one = '1' and ntx_size_odd = '1') then
              src_dat_o <= ntx_data_reg(15 downto 0);
              ntx_state <= TX_OOB1;
            else
              ntx_state <= TX_LWORD;
            end if;

          when TX_OOB1 =>

            if(ntx_has_oob = '1')then
              src_dat_o   <= c_WRF_OOB_TYPE_TX & x"000";
              src_adr_o   <= c_WRF_OOB;
              src_stb_int <= '1';
              ntx_state   <= TX_OOB2;
            elsif(src_stall_i = '0') then
              src_stb_int <= '0';
              ntx_state   <= TX_END_PACKET;
            end if;
            
            
          when TX_OOB2 =>
            
            if(src_stall_i = '0') then
              src_stb_int <= '1';
              src_adr_o   <= c_WRF_OOB;
              src_dat_o   <= ntx_oob_reg;
              ntx_state   <= TX_END_PACKET;
            end if;


-------------------------------------------------------------------------------
-- State end-of-packet: wait for ACKs and generate an inter-packet gap
-------------------------------------------------------------------------------
          when TX_END_PACKET =>


            if(src_stall_i = '0') then
              src_stb_int <= '0';

              --disable CYC if all ACKs received
              if(ntx_ack_count = 0) then
                src_cyc_int <= '0';
              end if;

              --timeout in case something went wrong and we won't ever
              --get those ACKs

              if(ntx_timeout_is_zero = '1') then
                regs_in.mcr_tx_error_i <= '1';
              else
                ntx_timeout <= ntx_timeout - 1;
              end if;

              --finish when gap generated and all ACKs received or timeout expired
              if((ntx_ack_count = 0 or regs_in.mcr_tx_error_i = '1')) then
                ntx_state <= TX_READ_DESC;
              end if;
            end if;
          when others => null;
        end case;
      end if;
    end if;
  end process;

-- these are never used:
  src_sel_o <= "11";
  src_we_o  <= '1';
  src_stb_o <= src_stb_int;
  src_cyc_o <= src_cyc_int;

-------------------------------------------------------------------------------
-- RX Path (Fabric ->  Host)
-------------------------------------------------------------------------------  



  nrx_status_reg <= f_unmarshall_wrf_status(snk_dat_i);

  nrx_valid <= '1' when snk_cyc_i = '1' and snk_stall_int = '0' and snk_stb_i = '1' and
               (snk_adr_i = c_WRF_DATA or snk_adr_i = c_WRF_OOB) else '0';

  nrx_stat_error <= '1' when snk_cyc_i = '1' and snk_stall_int = '0' and snk_stb_i = '1' and
                    (snk_adr_i = c_WRF_STATUS) and nrx_status_reg.error = '1' else '0';

  p_rx_gen_ack : process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        snk_ack_o <= '0';
      else
        if(snk_cyc_i = '1' and snk_stb_i = '1' and snk_stall_int = '0') then
          snk_ack_o <= '1';
        else
          snk_ack_o <= '0';
        end if;
      end if;
    end if;

  end process;

  p_rx_fsm : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        nrx_state      <= RX_WAIT_SOF;
        nrx_mem_a      <= (others => '0');
        nrx_mem_wr     <= '0';
        nrx_avail      <= (others => '0');
        nrx_bufstart   <= (others => '0');
        nrx_bufsize    <= (others => '0');
        nrx_rdreg      <= (others => '0');
        nrx_toggle     <= '0';
        nrx_stall_mask <= '0';
        nrx_bytesel    <= '0';
        nrx_size       <= (others => '0');
        nrx_buf_full   <= '0';
        nrx_error      <= '0';
        nrx_done       <= '0';
        nrx_has_oob    <= '0';

        nrx_mem_a_saved        <= (others => '0');
        regs_in.rx_addr_i      <= (others => '0');
        regs_in.mcr_rx_ready_i <= '0';
        regs_in.mcr_rx_full_i  <= '0';
        nrx_newpacket          <= '0';
        snk_err_o              <= '0';
      else

-- Host can modify the RX DMA registers only when the DMA engine is disabled
-- (MCR_RX_EN = 0)


        if(regs_out.mcr_rx_en_o = '0') then
          
          nrx_newpacket <= '0';

          -- mask out the data request line on the fabric I/F, so the endpoint
          -- can cut the traffic using 802.1 flow control
          nrx_stall_mask         <= '0';
          nrx_state              <= RX_WAIT_SOF;
          regs_in.mcr_rx_ready_i <= '0';

          -- handle writes to RX_ADDR and RX_AVAIL
          if(regs_out.rx_addr_load_o = '1') then
            nrx_mem_a_saved   <= unsigned(regs_out.rx_addr_o(g_memsize_log2+1 downto 2));
            nrx_bufstart      <= unsigned(regs_out.rx_addr_o(g_memsize_log2+1 downto 2));
            regs_in.rx_addr_i <= (others => '0');
            --      nrx_mem_a <= unsigned(minic_rx_addr_new(g_memsize_log2+1 downto 2));
          end if;

          if(regs_out.rx_size_load_o = '1') then
            nrx_buf_full          <= '0';
            regs_in.mcr_rx_full_i <= '0';
            nrx_avail             <= unsigned(regs_out.rx_size_o(nrx_avail'high downto 0));
            nrx_bufsize           <= unsigned(regs_out.rx_size_o(nrx_bufsize'high downto 0));
          end if;
        else
          if(regs_out.rx_avail_load_o = '1') then
            nrx_buf_full          <= '0';
            regs_in.mcr_rx_full_i <= '0';
            nrx_avail             <= nrx_avail + unsigned(regs_out.rx_avail_o(nrx_avail'high downto 0));
          end if;

          -- main RX FSM
          case nrx_state is

-------------------------------------------------------------------------------
-- State "Wait for start of frame". We wait until there's a start-of-frame condition
-- on the RX fabric and then we commence reception of the packet.
-------------------------------------------------------------------------------

            --when RX_BUFFER_FULL =>
            --  if(nrx_buf_full = '0') then
            --    nrx_stall_mask <= '0';
            --    nrx_state <= RX_WAIT_SOF;
            --  else
            --    nrx_stall_mask <= '1';
            --  end if;
            
            when RX_WAIT_SOF =>

              nrx_newpacket <= '0';
              nrx_done      <= '0';
              nrx_mem_wr    <= '0';
              nrx_has_oob   <= '0';
              nrx_bytesel   <= '0';
              nrx_error     <= '0';
              nrx_size      <= (others => '0');
              nrx_mem_a     <= nrx_mem_a_saved;
              nrx_toggle    <= '0';

              regs_in.mcr_rx_full_i <= nrx_buf_full;
              snk_err_o             <= nrx_buf_full;

              if(snk_cyc_i = '1' and nrx_buf_full = '0') then  -- got start-of-frame?
                nrx_stall_mask <= '1';
                nrx_state      <= RX_ALLOCATE_DESCRIPTOR;
              else
                nrx_stall_mask <= '0';  --nrx_buf_full;
              end if;
-------------------------------------------------------------------------------
-- State "Allocate RX descriptor": puts an empty (invalid) RX descriptor at the
-- current location in the RX buffer and then starts receiving the data
-------------------------------------------------------------------------------              
              
            when RX_ALLOCATE_DESCRIPTOR =>

-- wait until we have memory access
              if(mem_arb_rx = '0') then

-- make sure the bit 31 is 0 (RX descriptor is invalid)
                nrx_mem_d       <= (others => '0');
                nrx_mem_a_saved <= nrx_mem_a;
                if(nrx_avail /= to_unsigned(0, nrx_avail'length)) then
                  nrx_avail  <= nrx_avail - 1;
                  nrx_mem_wr <= '1';
                end if;


-- allow the fabric to receive the data
                nrx_stall_mask <= '0';

                nrx_state <= RX_DATA;
              end if;

-------------------------------------------------------------------------------
-- State "Receive data". Receive data, write it into the DMA memory
-------------------------------------------------------------------------------              
              
            when RX_DATA =>

              nrx_mem_wr <= '0';

              if(snk_stb_i = '1' and snk_cyc_i = '1' and snk_stall_int = '0' and snk_we_i = '1' and snk_adr_i = c_WRF_STATUS) then
                nrx_class <= nrx_status_reg.match_class;
              end if;

              -- check if we have enough space in the buffer
              if(nrx_avail = to_unsigned(0, nrx_avail'length)) then
                nrx_buf_full <= '1';
              end if;

              -- we've got a valid data word or end-of-frame/error condition
              if(nrx_valid = '1' or nrx_stat_error = '1' or snk_cyc_i = '0') then

                -- latch the bytesel signal to support frames having odd lengths
                if(snk_sel_i = "10") then
                  nrx_bytesel <= '1';
                end if;

                -- abort/error/end-of-frame?
                if(nrx_stat_error = '1' or snk_cyc_i = '0' or nrx_avail = to_unsigned(0, nrx_avail'length)) then

                  if(nrx_stat_error = '1' or nrx_avail = to_unsigned(0, nrx_avail'length)) then
                    nrx_error <= '1';
                  else
                    nrx_error <= '0';
                  end if;
                  --nrx_error <= '1' when nrx_stat_error='1' or nrx_avail=to_unsigned(0, nrx_avail'length) else '0';
                  nrx_done <= '1';

                  -- flush the remaining packet data into the DMA buffer
                  nrx_state <= RX_MEM_FLUSH;

                  if(nrx_valid = '1' or nrx_toggle = '1') then
                    if nrx_mem_a + 1 >= nrx_bufstart + nrx_bufsize then
                      nrx_mem_a <= nrx_bufstart;
                    else
                      nrx_mem_a <= nrx_mem_a + 1;
                    end if;
                  end if;

                  -- disable the RX fabric reception, so we won't get another
                  -- packet before we are done with the RX descriptor update
                  nrx_stall_mask <= '1';
                end if;

                if(nrx_valid = '1') then
                  nrx_size <= nrx_size + 1;

                  -- pack two 16-bit words received from the fabric I/F into one
                  -- 32-bit DMA memory word

                  if(g_buffer_little_endian = false) then
                    -- big endian RX buffer
                    if(nrx_toggle = '0') then
                      nrx_mem_d(31 downto 16) <= snk_dat_i;
                    else
                      nrx_mem_d(15 downto 0) <= snk_dat_i;
                    end if;
                  else
                    -- little endian RX buffer
                    if(nrx_toggle = '0') then
                      nrx_mem_d(15 downto 8) <= snk_dat_i(7 downto 0);
                      nrx_mem_d(7 downto 0)  <= snk_dat_i(15 downto 8);
                    else
                      nrx_mem_d(31 downto 24) <= snk_dat_i(7 downto 0);
                      nrx_mem_d(23 downto 16) <= snk_dat_i(15 downto 8);
                    end if;
                  end if;

                  -- we've got RX OOB tag? Remember it and later put it in the
                  -- descriptor header
                  if(snk_adr_i = c_WRF_OOB) then
                    nrx_has_oob <= '1';
                  end if;


                  nrx_toggle <= not nrx_toggle;
                end if;
              end if;

              -- we've got the second valid word of the payload, write it to the
              -- memory
              if(nrx_toggle = '1' and nrx_valid = '1' and snk_cyc_i = '1' and nrx_stat_error = '0') then
                if nrx_mem_a + 1 >= nrx_bufstart + nrx_bufsize then
                  nrx_mem_a <= nrx_bufstart;
                else
                  nrx_mem_a <= nrx_mem_a + 1;
                end if;
                nrx_mem_wr <= '1';
                nrx_avail  <= nrx_avail - 1;

                -- check if we are synchronized with the memory write arbiter.
                if(mem_arb_rx = '1') then
                  nrx_state <= RX_MEM_RESYNC;
                end if;
              else
                -- nothing to write
                nrx_mem_wr <= '0';
              end if;

-------------------------------------------------------------------------------
-- State "Memory resync": a "wait state" entered when the miNIC tries to write the RX
-- payload, but the memory access is given for the TX path at the moment.
-------------------------------------------------------------------------------
              
            when RX_MEM_RESYNC =>

              -- check for error/abort conditions, they may appear even when
              -- the fabric is not accepting the data (tx_dreq_o = 0)
              if(nrx_stat_error = '1') then
                nrx_error <= '1';
                nrx_done  <= '1';
                nrx_state <= RX_MEM_FLUSH;
                if nrx_mem_a + 1 >= nrx_bufstart + nrx_bufsize then
                  nrx_mem_a <= nrx_bufstart;
                else
                  nrx_mem_a <= nrx_mem_a + 1;
                end if;
                nrx_stall_mask <= '1';
                nrx_size       <= nrx_size + 2;
              else
                nrx_state <= RX_DATA;
              end if;

-------------------------------------------------------------------------------
-- State "Memory flush": flushes the remaining contents of RX data register
-- into the DMA buffer after end-of-packet/error/abort
-------------------------------------------------------------------------------
              
            when RX_MEM_FLUSH =>
              nrx_stall_mask <= '1';

              if(nrx_buf_full = '0') then
                nrx_mem_wr <= '1';
              end if;

              if(mem_arb_rx = '0') then
                if(nrx_buf_full = '0') then
                  nrx_avail <= nrx_avail - 1;
                else
                	nrx_size <= nrx_size - 1;  --the buffer is full, last word was not written
                	nrx_mem_a <= nrx_mem_a - 1;
                end if;
                nrx_state <= RX_UPDATE_DESC;
              end if;

-------------------------------------------------------------------------------
-- State "Update RX descriptor": writes the frame size, OOB presence and error
-- flags into the empty RX descriptor allocated at the beginning of the reception
-- of the frame, and marks the descriptor as valid. Also triggers the RX ready
-- interrupt.
-------------------------------------------------------------------------------
              
            when RX_UPDATE_DESC =>
              nrx_stall_mask <= '1';

              if(nrx_avail = to_unsigned(0, nrx_avail'length)) then
                nrx_buf_full <= '1';
              end if;

              if(mem_arb_rx = '0') then


                -- store the current write pointer as a readback value of RX_ADDR register, so
-- the host can determine the RX descriptor we're actually working on
                regs_in.rx_addr_i(g_memsize_log2+1 downto 0)                      <= std_logic_vector(nrx_mem_a_saved) & "00";
                regs_in.rx_addr_i(regs_in.rx_addr_i'high downto g_memsize_log2+2) <= (others => '0');

                nrx_mem_a <= nrx_mem_a_saved;
                if nrx_mem_a + 1 >= nrx_bufstart + nrx_bufsize then
                  nrx_mem_a_saved <= nrx_bufstart;
                else
                  nrx_mem_a_saved <= nrx_mem_a + 1;
                end if;

-- compose the RX descriptor
                nrx_mem_d(31)                      <= '1';
                nrx_mem_d(30)                      <= nrx_error;
                nrx_mem_d(29)                      <= nrx_has_oob;
--                nrx_mem_d(28 downto g_memsize_log2+1) <= (others => '0');
                nrx_mem_d(28 downto 21)            <= nrx_class;
                nrx_mem_d(g_memsize_log2 downto 1) <= std_logic_vector(nrx_size);
                nrx_mem_d(0)                       <= nrx_bytesel;

                nrx_mem_wr <= '1';


                -- wait for another packet
                if(snk_cyc_i = '1') then
                  nrx_state <= RX_IGNORE;
                elsif(nrx_buf_full = '1') then
                  nrx_state      <= RX_BUF_FULL;
                  nrx_stall_mask <= '1';
                else
                  -- trigger the RX interrupt and assert RX_READY flag to inform
                  -- the host that we've received something
                  nrx_newpacket          <= '1';
                  regs_in.mcr_rx_ready_i <= '1';
                  nrx_state <= RX_WAIT_SOF;
                end if;
              else
                nrx_mem_wr <= '0';
              end if;
              
            when RX_IGNORE =>
              nrx_mem_wr <= '0';
              --drop the rest of the packet
              nrx_stall_mask <= '0';
              if(snk_cyc_i = '0') then
                if(nrx_buf_full = '1') then
                  nrx_stall_mask <= '1';
                  nrx_state      <= RX_BUF_FULL;
                else
                  nrx_state <= RX_WAIT_SOF;
                end if;
              end if;

            when RX_BUF_FULL =>
                nrx_mem_wr 						 <= '0';
                regs_in.mcr_rx_full_i  <= nrx_buf_full;
                nrx_newpacket          <= '1';
                regs_in.mcr_rx_ready_i <= '1';
              if(nrx_buf_full = '0') then
                nrx_stall_mask <= '0';
                nrx_state      <= RX_WAIT_SOF;
              else
                nrx_stall_mask <= '1';
              end if;
              
          end case;
        end if;
      end if;
    end if;
  end process;



-------------------------------------------------------------------------------
-- helper process for producing the RX fabric data request signal (combinatorial)
-------------------------------------------------------------------------------  
  gen_rx_dreq : process(nrx_stall_mask, nrx_state, mem_arb_rx, nrx_toggle, regs_out.mcr_rx_en_o, snk_cyc_i)
  begin
    -- make sure we don't have any incoming data when the reception is masked (e.g.
    -- the miNIC is updating the descriptors of finishing the memory write. 
    if(snk_cyc_i = '1' and nrx_state = RX_WAIT_SOF) then
      snk_stall_int <= '1';
    elsif(regs_out.mcr_rx_en_o = '0' or nrx_state = RX_ALLOCATE_DESCRIPTOR or nrx_state = RX_UPDATE_DESC or nrx_state = RX_MEM_FLUSH) then
      snk_stall_int <= '1';
      -- the condition below forces the RX FSM to go into RX_MEM_RESYNC state. Don't
      -- receive anything during this time
    elsif(nrx_state = RX_IGNORE or nrx_state = RX_BUF_FULL) then
      snk_stall_int <= nrx_stall_mask;
    elsif(nrx_toggle = '1' and mem_arb_rx = '1') then
      snk_stall_int <= '1';
    elsif(nrx_stall_mask = '1') then
      snk_stall_int <= '1';
    else
      snk_stall_int <= '0';
    end if;
  end process;

  snk_stall_o <= snk_stall_int;


-------------------------------------------------------------------------------
-- TX Timestamping unit
-------------------------------------------------------------------------------  
  tsu_fsm : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0') then
        regs_in.mcr_tx_ts_ready_i <= '0';
        regs_in.tsr0_valid_i <= '0';
        regs_in.tsr0_pid_i   <= (others => '0');
        regs_in.tsr0_fid_i   <= (others => '0');
        regs_in.tsr1_tsval_i <= (others => '0');
        txtsu_ack_int        <= '0';
      else
        -- Make sure the timestamp is written to the FIFO only once.

        if(txtsu_stb_i = '1' and txtsu_ack_int = '0') then
          regs_in.mcr_tx_ts_ready_i <= '1';
          regs_in.tsr0_valid_i <= not txtsu_tsincorrect_i;
          regs_in.tsr0_fid_i   <= txtsu_frame_id_i;
          regs_in.tsr0_pid_i   <= txtsu_port_id_i;
          regs_in.tsr1_tsval_i <= txtsu_tsval_i;
          txtsu_ack_int        <= '1';
        else
          -- clear the TS ready flag when a transmission begins
          regs_in.mcr_tx_ts_ready_i <= regs_in.mcr_tx_ts_ready_i and regs_in.mcr_tx_idle_i;
          txtsu_ack_int <= '0';
        end if;
      end if;
    end if;
  end process;

  txtsu_ack_o <= txtsu_ack_int;

  handle_rx_interrupt : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        irq_rx           <= '0';
        nrx_newpacket_d0 <= '0';
      else
        nrx_newpacket_d0 <= nrx_newpacket;

        if (nrx_newpacket_d0 = '0' and nrx_newpacket = '1') then
          irq_rx <= '1';
        elsif (regs_in.mcr_rx_full_i = '1' and (nrx_state = RX_WAIT_SOF or nrx_state = RX_BUF_FULL)) then
          --if buf_full occured during packet reception RX state machine will generate erronous packet
          --so the interrupt will either be there
          irq_rx <= '1';
        elsif (irq_rx_ack = '1') then
          irq_rx <= '0';
        end if;
      end if;
    end if;
  end process;

  irq_txts <= '0';

  U_Slave_Adapter : wb_slave_adapter
    generic map (
      g_master_use_struct  => true,
      g_master_mode        => CLASSIC,
      g_master_granularity => WORD,
      g_slave_use_struct   => false,
      g_slave_mode         => g_interface_mode,
      g_slave_granularity  => g_address_granularity)
    port map (
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_i,
      sl_adr_i   => wb_adr_i,
      sl_dat_i   => wb_dat_i,
      sl_sel_i   => wb_sel_i,
      sl_cyc_i   => wb_cyc_i,
      sl_stb_i   => wb_stb_i,
      sl_we_i    => wb_we_i,
      sl_dat_o   => wb_dat_o,
      sl_ack_o   => wb_ack_o,
      sl_stall_o => wb_stall_o,
      master_i   => wb_in,
      master_o   => wb_out);

  U_WB_Slave : minic_wb_slave
    port map (
      rst_n_i          => rst_n_i,
      clk_sys_i        => clk_sys_i,
      wb_adr_i         => wb_out.adr(4 downto 0),
      wb_dat_i         => wb_out.dat,
      wb_dat_o         => wb_in.dat,
      wb_cyc_i         => wb_out.cyc,
      wb_sel_i         => wb_out.sel,
      wb_stb_i         => wb_out.stb,
      wb_we_i          => wb_out.we,
      wb_ack_o         => wb_in.ack,
      wb_stall_o       => wb_in.stall,
      wb_int_o         => wb_int_o,
      regs_i           => regs_in,
      regs_o           => regs_out,
      tx_ts_read_ack_o => open,
      irq_tx_i         => irq_tx,
      irq_tx_ack_o     => irq_tx_ack,
      irq_tx_mask_o    => irq_tx_mask,
      irq_rx_i         => irq_rx,
      irq_rx_ack_o     => irq_rx_ack,
      irq_txts_i       => irq_txts);


  regs_in.rx_avail_i(nrx_avail'high downto 0)                         <= std_logic_vector(nrx_avail);
  regs_in.rx_avail_i(regs_in.rx_avail_i'high downto nrx_avail'high+1) <= (others => '0');
  regs_in.rx_size_i(nrx_size'high downto 0)                           <= std_logic_vector(nrx_bufsize);
  regs_in.rx_size_i(regs_in.rx_size_i'high downto nrx_size'high+1)    <= (others => '0');

end behavioral;
