-------------------------------------------------------------------------------
-- Title      : Mini Embedded DMA Network Interface Controller
-- Project    : WhiteRabbit Core
-------------------------------------------------------------------------------
-- File       : wrsw_mini_nic.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-Co-HT
-- Created    : 2010-07-26
-- Last update: 2011-09-05
-- Platform   : FPGA-generic
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description: Module implements a simple NIC with DMA controller. It
-- sends/receives the packets using WR switch fabric interface (see the
-- wrsw_endpoint.vhd for the details). Packets are stored and read from the
-- system memory via simple memory bus. WR endpoint-compatible TX timestamping
-- unit is also included.
-------------------------------------------------------------------------------
-- Copyright (c) 2010 Tomasz Wlostowski
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2010-07-26  1.0      twlostow        Created
-- 2010-08-16  1.0      twlostow        Bugfixes, linux compatibility added
-- 2011-08-03  2.0      greg.d          rewritten to use pipelined Wishbone
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--use work.endpoint_pkg.all;
use work.wr_fabric_pkg.all;

entity wr_mini_nic is

  generic (
    g_memsize_log2         : integer := 14;
    g_buffer_little_endian : boolean := true;
    g_class_mask           : std_logic_vector(7 downto 0) := "00000001");

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
    wbm_dat_o     : out std_logic_vector(15 downto 0);
    wbm_adr_o     : out std_logic_vector(1 downto 0);
    wbm_sel_o     : out std_logic_vector(1 downto 0);
    wbm_cyc_o     : out std_logic;
    wbm_stb_o     : out std_logic;
    wbm_we_o      : out std_logic;
    wbm_stall_i   : in  std_logic;
    wbm_err_i     : in  std_logic;
    wbm_ack_i     : in  std_logic;

    -- WBP Slave (RX)
    wbs_dat_i     : in  std_logic_vector(15 downto 0);
    wbs_adr_i     : in  std_logic_vector(1 downto 0);
    wbs_sel_i     : in  std_logic_vector(1 downto 0);
    wbs_cyc_i     : in  std_logic;
    wbs_stb_i     : in  std_logic;
    wbs_we_i      : in  std_logic;
    wbs_stall_o   : out std_logic;
    wbs_err_o     : out std_logic;
    wbs_ack_o     : out std_logic;

-------------------------------------------------------------------------------
-- TXTSU i/f
-------------------------------------------------------------------------------

    txtsu_port_id_i  : in  std_logic_vector(4 downto 0);
    txtsu_frame_id_i : in  std_logic_vector(16 - 1 downto 0);
    txtsu_tsval_i    : in  std_logic_vector(28 + 4 - 1 downto 0);
    txtsu_valid_i    : in  std_logic;
    txtsu_ack_o      : out std_logic;

-------------------------------------------------------------------------------
-- Wishbone slave
-------------------------------------------------------------------------------    

    wb_cyc_i  : in  std_logic;
    wb_stb_i  : in  std_logic;
    wb_we_i   : in  std_logic;
    wb_sel_i  : in  std_logic_vector(3 downto 0);
    wb_addr_i : in  std_logic_vector(3 downto 0);
    wb_data_i : in  std_logic_vector(31 downto 0);
    wb_data_o : out std_logic_vector(31 downto 0);
    wb_ack_o  : out std_logic;
    wb_irq_o  : out std_logic
  );
end wr_mini_nic;

architecture behavioral of wr_mini_nic is

  -- inter-packet gap (for the TX)
  constant c_GAP_SIZE   : integer := 10;

  component minic_wb_slave
    port (
      rst_n_i                 : in  std_logic;
      wb_clk_i                : in  std_logic;
      wb_addr_i               : in  std_logic_vector(3 downto 0);
      wb_data_i               : in  std_logic_vector(31 downto 0);
      wb_data_o               : out std_logic_vector(31 downto 0);
      wb_cyc_i                : in  std_logic;
      wb_sel_i                : in  std_logic_vector(3 downto 0);
      wb_stb_i                : in  std_logic;
      wb_we_i                 : in  std_logic;
      wb_ack_o                : out std_logic;
      wb_irq_o                : out std_logic;
      minic_mcr_tx_start_o    : out std_logic;
      minic_mcr_tx_idle_i     : in  std_logic;
      minic_mcr_tx_error_i    : in  std_logic;
      minic_mcr_rx_ready_i    : in  std_logic;
      minic_mcr_rx_full_i     : in  std_logic;
      minic_mcr_rx_en_o       : out std_logic;
      minic_tx_addr_o         : out std_logic_vector(23 downto 0);
      minic_tx_addr_i         : in  std_logic_vector(23 downto 0);
      minic_tx_addr_load_o    : out std_logic;
      minic_rx_addr_o         : out std_logic_vector(23 downto 0);
      minic_rx_addr_i         : in  std_logic_vector(23 downto 0);
      minic_rx_addr_load_o    : out std_logic;
      minic_rx_avail_o        : out std_logic_vector(23 downto 0);
      minic_rx_avail_i        : in  std_logic_vector(23 downto 0);
      minic_rx_avail_load_o   : out std_logic;
      minic_tsfifo_wr_req_i   : in  std_logic;
      minic_tsfifo_wr_full_o  : out std_logic;
      minic_tsfifo_wr_empty_o : out std_logic;
      minic_tsfifo_tsval_i    : in  std_logic_vector(31 downto 0);
      minic_tsfifo_pid_i      : in  std_logic_vector(4 downto 0);
      minic_tsfifo_fid_i      : in  std_logic_vector(15 downto 0);
      minic_dbgr_irq_cnt_i    : in  std_logic_vector(23 downto 0);
      minic_dbgr_wb_irq_val_i : in  std_logic;
      irq_tx_i                : in  std_logic;
      irq_tx_ack_o            : out std_logic;
      irq_tx_mask_o           : out std_logic;
      irq_rx_i                : in  std_logic;
      irq_rx_ack_o            : out std_logic;
      irq_txts_i              : in  std_logic);
  end component;

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

  type t_tx_fsm_state is (TX_IDLE, TX_READ_DESC, TX_STATUS, TX_START_PACKET, TX_HWORD, TX_LWORD, TX_END_PACKET);

  alias ntx_desc_size     is ntx_mem_d(g_memsize_log2 downto 0);
  alias ntx_desc_odd      is ntx_mem_d(0);
  alias ntx_desc_valid    is ntx_mem_d(31);
  alias ntx_desc_with_oob is ntx_mem_d(30);
  alias ntx_desc_802_1q   is ntx_mem_d(29);
  alias ntx_desc_has_src_mac is ntx_mem_d(28);

  --STATUS Reg for TX path
  signal ntx_status_reg     : t_wrf_status_reg;
  
  signal ntx_data_reg      : std_logic_vector(31 downto 0);

  signal ntx_cntr_is_zero  : std_logic;
  signal ntx_cntr_is_one   : std_logic;
  signal ntx_datcntr_is_zero : std_logic;
  signal ntx_datcntr_is_one  : std_logic;
  signal ntx_ackcntr_is_zero : std_logic;
  signal ntx_timeout_is_zero : std_logic;
  signal ntx_cntr          : unsigned(g_memsize_log2 downto 0);
  signal ntx_datcntr       : unsigned(g_memsize_log2 downto 0);
  signal ntx_ackcntr       : unsigned(g_memsize_log2 downto 0);
  signal ntx_timeout       : unsigned(7 downto 0);
  signal ntx_has_oob       : std_logic;
  signal ntx_state         : t_tx_fsm_state;
  signal ntx_start_delayed : std_logic;
  signal ntx_size_odd      : std_logic;

  signal zero_status_reg : std_logic_vector(15 downto 0);
-------------------------------------------------------------------------------
-- RX FSM stuff
-------------------------------------------------------------------------------  

  type t_rx_fsm_state is (RX_WAIT_SOF, RX_MEM_RESYNC, RX_MEM_FLUSH, RX_ALLOCATE_DESCRIPTOR, RX_DATA, RX_UPDATE_DESC);

  signal nrx_state        : t_rx_fsm_state;
  signal nrx_avail        : unsigned(g_memsize_log2-1 downto 0);
  signal nrx_toggle       : std_logic;
  signal nrx_oob_toggle   : std_logic;
  signal nrx_ptoggle      : std_logic;
  signal nrx_class_match  : std_logic_vector(7 downto 0);

  --STATUS Reg for RX path
  signal nrx_status_reg   : t_wrf_status_reg;

  signal nrx_mem_a_saved : unsigned(g_memsize_log2-1 downto 0);
  signal nrx_has_oob     : std_logic;
  signal nrx_bytesel     : std_logic;
  signal nrx_size        : unsigned(g_memsize_log2-1 downto 0);
  signal nrx_acksize     : unsigned(g_memsize_log2-1 downto 0);
  signal nrx_buf_full    : std_logic;
  signal nrx_stall_mask  : std_logic;

-------------------------------------------------------------------------------
-- Classic Wishbone slave signals
-------------------------------------------------------------------------------  

  signal minic_mcr_tx_start : std_logic;
  signal minic_mcr_tx_idle  : std_logic;
  signal minic_mcr_tx_error : std_logic;
  signal minic_mcr_rx_ready : std_logic;
  signal minic_mcr_rx_en    : std_logic;
  signal minic_mcr_rx_full  : std_logic;

  signal minic_tx_addr_new  : std_logic_vector(23 downto 0);
  signal minic_tx_addr_cur  : std_logic_vector(23 downto 0);
  signal minic_tx_addr_load : std_logic;

  signal minic_rx_addr_new  : std_logic_vector(23 downto 0);
  signal minic_rx_addr_cur  : std_logic_vector(23 downto 0);
  signal minic_rx_addr_load : std_logic;

  signal minic_rx_avail_cur  : std_logic_vector(23 downto 0);
  signal minic_rx_avail_new  : std_logic_vector(23 downto 0);
  signal minic_rx_avail_load : std_logic;

  signal minic_tsfifo_wr_req   : std_logic;
  signal minic_tsfifo_wr_full  : std_logic;
  signal minic_tsfifo_wr_empty : std_logic;
  signal minic_tsfifo_tsval    : std_logic_vector(31 downto 0);
  signal minic_tsfifo_pid      : std_logic_vector(4 downto 0);
  signal minic_tsfifo_fid      : std_logic_vector(15 downto 0);

  signal irq_tx     : std_logic;
  signal irq_rx_ack : std_logic;
  signal irq_rx     : std_logic;

  signal nrx_newpacket, nrx_newpacket_d0 : std_logic;

  signal irq_txts    : std_logic;
  signal irq_tx_ack  : std_logic;
  signal irq_tx_mask : std_logic;

  signal txtsu_ack_int : std_logic;

begin  -- behavioral

  zero_status_reg <= (others=>'0');

-----------------------------------------------------------------------------
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

  mem_arb_tx <= not mem_arb_rx;
  mem_addr_o <= std_logic_vector(ntx_mem_a) when mem_arb_rx = '0' else std_logic_vector(nrx_mem_a);
  mem_data_o <= (others=>'0') when nrx_mux_d = '0'
                else nrx_mem_d;
  ntx_mem_d  <= mem_data_i;
  mem_wr_o   <= nrx_mem_wr                  when mem_arb_rx = '1' else '0';

-------------------------------------------------------------------------------
-- TX Path  (Host -> Fabric)
-------------------------------------------------------------------------------  

-- helper signals to avoid big IF conditions in the FSM
  ntx_cntr_is_zero    <= '1' when (ntx_cntr = to_unsigned(0, ntx_cntr'length)) else '0';
  ntx_cntr_is_one     <= '1' when (ntx_cntr = to_unsigned(1, ntx_cntr'length)) else '0';
  ntx_datcntr_is_zero <= '1' when (ntx_datcntr = to_unsigned(0, ntx_datcntr'length)) else '0';
  ntx_datcntr_is_one  <= '1' when (ntx_datcntr = to_unsigned(1, ntx_datcntr'length)) else '0';
  ntx_ackcntr_is_zero <= '1' when (ntx_ackcntr = to_unsigned(0, ntx_ackcntr'length)) else '0';
  ntx_timeout_is_zero <= '1' when (ntx_timeout = to_unsigned(0, ntx_timeout'length)) else '0';

  tx_fsm : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        wbm_dat_o          <= (others => '0');
        wbm_adr_o          <= (others => '0');
        wbm_cyc_o          <= '0';
        wbm_stb_o          <= '0';
        minic_mcr_tx_error <= '0';
        minic_mcr_tx_idle  <= '0';
        irq_tx             <= '0';
        ntx_has_oob        <= '0';
        ntx_mem_a          <= (others => '0');
        ntx_cntr           <= (others => '0');
        ntx_datcntr        <= (others => '0');
        ntx_ackcntr        <= (others => '0');
        ntx_data_reg       <= (others => '0');
        ntx_status_reg     <= f_unmarshall_wrf_status( zero_status_reg );
        ntx_start_delayed  <= '0';
        ntx_state          <= TX_IDLE;
      else
        case ntx_state is

-------------------------------------------------------------------------------
-- Idle state: we wait until the host starts the DMA transfer
-------------------------------------------------------------------------------
          when TX_IDLE =>
            wbm_cyc_o <= '0';
            wbm_stb_o <= '0';
            wbm_sel_o <= "11";
            -- keep the TX start bit (it's active for single clock cycle) in
            -- case we needed to align the FSM cycle with the memory arbiter
            if(minic_mcr_tx_start = '1') then
              ntx_start_delayed <= '1';
            end if;

            -- TX interrupt is disabled. Just assert the TX_IDLE.
            if(irq_tx_mask = '0') then
              minic_mcr_tx_idle <= '1';
            elsif(irq_tx_ack = '1') then
              irq_tx            <= '0';
              minic_mcr_tx_idle <= '1';
            end if;

            -- initialize timeout of TX_END_PACKET
            ntx_timeout <= to_unsigned(100, ntx_timeout'length);

            -- the host loaded new TX DMA buffer address
            if(minic_tx_addr_load = '1') then
              ntx_mem_a <= unsigned(minic_tx_addr_new(g_memsize_log2+1 downto 2));
            end if;

            -- the host started the DMA xfer (and we are "phased" with the arbiter)
            if(wbm_err_i = '0' and ntx_start_delayed = '1' and mem_arb_tx = '0') then
              ntx_state <= TX_READ_DESC;
              -- clear the TX flags
              minic_mcr_tx_error <= '0';
              minic_mcr_tx_idle  <= '0';
            end if;


-------------------------------------------------------------------------------
-- Read descriptor header state: check if there's another descriptor in the buffer
-- and eventually, start transmitting it
-------------------------------------------------------------------------------
          when TX_READ_DESC =>
            wbm_sel_o <= "11";
            ntx_start_delayed <= '0';

            if(mem_arb_tx = '0') then   -- memory is ready?
              -- feed the current TX DMA address as the readback value of TX_ADDR Wishbone
              -- register
              minic_tx_addr_cur(g_memsize_log2+1 downto 0)                      <= std_logic_vector(ntx_mem_a) & "00";
              minic_tx_addr_cur(minic_tx_addr_cur'high downto g_memsize_log2+2) <= (others => '0');

              -- if we have no more valid TX descriptors, trigger an interrupt and wait for
              -- another DMA transfer
              if(ntx_desc_valid = '0') then
                ntx_state <= TX_IDLE;
                irq_tx    <= '1';
              else
                -- read the descriptor contents (size, 802.1q/OOB enables)
                ntx_size_odd    <= ntx_desc_odd;
                ntx_cntr        <= unsigned(ntx_desc_size);
                if(ntx_desc_with_oob = '1') then
                  ntx_datcntr  <= unsigned(ntx_desc_size) - 2;
                else
                  ntx_datcntr  <= unsigned(ntx_desc_size);
                end if;
                ntx_ackcntr     <= unsigned(ntx_desc_size) + 1; --+1 for status reg
                ntx_has_oob     <= ntx_desc_with_oob;
                ntx_status_reg.is_hp    <= '0';
                ntx_status_reg.has_smac <= ntx_desc_has_src_mac;
                ntx_status_reg.has_crc  <= '0';
                ntx_status_reg.error    <= '0';
                ntx_status_reg.match_class <= g_class_mask;
                ntx_state       <= TX_STATUS;
                ntx_mem_a       <= ntx_mem_a + 1;
              end if;
            end if;

-------------------------------------------------------------------------------
-- State status: send status word
-------------------------------------------------------------------------------
          when TX_STATUS =>

            wbm_cyc_o <= '1';
            wbm_stb_o <= '1';
            wbm_sel_o <= "11";
            wbm_adr_o <= c_WRF_STATUS;
            if(wbm_stall_i = '0') then
              wbm_dat_o <= f_marshall_wrf_status(ntx_status_reg);
              ntx_state <= TX_START_PACKET;
            end if;

            if(wbm_ack_i = '1') then
              ntx_ackcntr <= ntx_ackcntr - 1;
            end if;

-------------------------------------------------------------------------------
-- Start packet state: asserts CYC signal and reads first word to transmit
-------------------------------------------------------------------------------            
          when TX_START_PACKET =>

            wbm_cyc_o <= '1';
            wbm_stb_o <= '0';
            wbm_sel_o <= "11";
            -- check if the memory is ready, read the 1st word of the payload
            if(wbm_stall_i = '0' and mem_arb_tx = '0') then
              ntx_data_reg  <= f_buf_swap_endian_32(ntx_mem_d);
              wbm_cyc_o     <= '1';
              ntx_state     <= TX_HWORD;
              ntx_mem_a     <= ntx_mem_a + 1;
            end if;


--------------------------------------------------------------------------------
-- State "Transmit HI word" - transmit the most significant word of the packet
-------------------------------------------------------------------------------
          when TX_HWORD =>
            wbm_cyc_o <= '1';
            wbm_stb_o <= '1';
            if(ntx_datcntr_is_one='1' and ntx_size_odd='1' and wbm_stall_i='0') then
              wbm_sel_o <= "10";
            elsif(wbm_stall_i='0') then
              wbm_sel_o <= "11";
            end if;
            if(wbm_err_i = '1') then
              minic_mcr_tx_error <= '1';
              irq_tx             <= '1';
              ntx_state          <= TX_IDLE;
            end if;

            if(ntx_datcntr_is_zero='1' and ntx_has_oob='1' and wbm_stall_i='0') then
              wbm_adr_o <= c_WRF_OOB;
            else
              wbm_adr_o <= c_WRF_DATA;
            end if;

            if(wbm_err_i = '0' and wbm_stall_i = '0') then
              wbm_dat_o <= ntx_data_reg(31 downto 16);
              if(ntx_cntr_is_zero = '1') then
                ntx_cntr     <= to_unsigned(c_GAP_SIZE, ntx_cntr'length);
                wbm_stb_o    <= '0';
                ntx_state    <= TX_END_PACKET;
              elsif(ntx_cntr = to_unsigned(1, ntx_cntr'length) ) then
                --seems like odd number of words, so don't send here, prepare 
                --immediately new word and jump to TX_LWORD to send it out
                wbm_stb_o    <= '0';
                wbm_dat_o    <= ntx_data_reg(15 downto 0);
                ntx_state    <= TX_LWORD;
              else
                ntx_cntr    <= ntx_cntr - 1;
                if(ntx_datcntr_is_zero='1') then
                  ntx_datcntr <= to_unsigned(0, ntx_datcntr'length);
                else
                  ntx_datcntr <= ntx_datcntr - 1;
                end if;
                ntx_state   <= TX_LWORD;
              end if;
            end if;

            if( wbm_ack_i = '1' ) then
              ntx_ackcntr  <= ntx_ackcntr - 1;
            end if;

--------------------------------------------------------------------------------
-- State "Transmit LO word" - transmit the least significant word of the packet
-------------------------------------------------------------------------------
          when TX_LWORD =>
            wbm_cyc_o <= '1';
            wbm_stb_o <= '1';
            if(ntx_datcntr_is_one='1' and ntx_size_odd='1' and wbm_stall_i='0') then
              wbm_sel_o <= "10";
            elsif(wbm_stall_i='0') then
              wbm_sel_o <= "11";
            end if;
            if(wbm_err_i = '1') then
              minic_mcr_tx_error <= '1';
              irq_tx             <= '1';
              ntx_state          <= TX_IDLE;
            end if;

            --set OOB adr when we are sending last word of the packet, descriptor says there is oob and
            --there was a immediate jump from TX_HWORD(odd number of words) or there is no stall
            --why? because if there was no immediate jump and STALL='1' then we have to remain with c_WRF_DATA
            --so that Slave could get the word sent by TX_HWORD after deactivating STALL
            if( ntx_datcntr_is_zero='1' and ntx_has_oob='1' and wbm_stall_i='0') then
              wbm_adr_o <= c_WRF_OOB;
            else
              wbm_adr_o <= c_WRF_DATA;
            end if;

            -- the TX fabric is ready, the memory is ready and we haven't reached the end
            -- of the packet yet:

            if(wbm_stall_i = '0') then
              wbm_dat_o <= ntx_data_reg (15 downto 0);
              if( mem_arb_tx = '0' and ntx_cntr_is_one = '0') then
                ntx_data_reg <= f_buf_swap_endian_32(ntx_mem_d);
                ntx_cntr      <= ntx_cntr - 1;
                if(ntx_datcntr_is_zero = '1') then
                  ntx_datcntr   <= to_unsigned(0, ntx_datcntr'length);
                else
                  ntx_datcntr   <= ntx_datcntr - 1;
                end if;
                ntx_mem_a     <= ntx_mem_a + 1;
                ntx_state     <= TX_HWORD;

              elsif(ntx_cntr_is_one='1') then
                -- We're at the end of the packet
                ntx_cntr     <= to_unsigned(c_GAP_SIZE, ntx_cntr'length);
                wbm_stb_o    <= '1';
                ntx_state    <= TX_END_PACKET;
              else
                wbm_stb_o    <= '0';
              end if;
            elsif( wbm_stall_i='1' and ntx_cntr_is_one='1' and ntx_size_odd='1') then
              wbm_dat_o      <= ntx_data_reg(15 downto 0);
              ntx_cntr       <= to_unsigned(c_GAP_SIZE, ntx_cntr'length);
              ntx_state      <= TX_END_PACKET;
            else
              ntx_state      <= TX_LWORD;
            end if;

            if( wbm_ack_i = '1' ) then
              ntx_ackcntr  <= ntx_ackcntr - 1;
            end if;

-------------------------------------------------------------------------------
-- State end-of-packet: wait for ACKs and generate an inter-packet gap
-------------------------------------------------------------------------------
          when TX_END_PACKET =>
            if(ntx_datcntr_is_one='1' and ntx_size_odd='1' and wbm_stall_i='0') then
              wbm_sel_o <= "10";
            elsif(wbm_stall_i='0') then
              wbm_sel_o <= "11";
            end if;

            --ACKs reception
            if( wbm_ack_i = '1') then
              ntx_ackcntr <= ntx_ackcntr - 1;
            end if;
            
            wbm_stb_o  <= '0';
            if( wbm_stall_i = '1' and ntx_ackcntr_is_zero='0' ) then
              wbm_stb_o <= '1';
            else
              --inter-packet gap generation
              if( ntx_cntr_is_zero = '0') then
                ntx_cntr   <= ntx_cntr - 1;
              end if;
              --disable CYC if all ACKs received
              if( ntx_ackcntr_is_zero='1' ) then
                wbm_cyc_o <= '0';
              end if;
              --timeout in case something went wrong and we won't ever
              --get those ACKs
              if(ntx_timeout_is_zero = '1') then
                minic_mcr_tx_error <= '1';
              else
                ntx_timeout <= ntx_timeout - 1;
              end if;
              --finish when gap generated and all ACKs received or timeout expired
              if(ntx_cntr_is_zero = '1' and (ntx_ackcntr_is_zero='1' or minic_mcr_tx_error='1' )) then
                ntx_state <= TX_READ_DESC;
              end if;
            end if;
          when others => null;
        end case;
      end if;
    end if;
  end process;

-- these are never used:
  wbm_we_o  <= '1';

-------------------------------------------------------------------------------
-- RX Path (Fabric ->  Host)
-------------------------------------------------------------------------------  
  nrx_class_match <= nrx_status_reg.match_class and g_class_mask;

  rx_fsm : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        nrx_state        <= RX_WAIT_SOF;
        nrx_mem_a        <= (others => '0');
        nrx_mem_d        <= (others => '0');
        nrx_mux_d        <= '0';
        nrx_mem_wr       <= '0';
        nrx_avail        <= (others => '0');
        nrx_status_reg   <= f_unmarshall_wrf_status( zero_status_reg );
        --nrx_oob_reg      <= (others => '0');
        nrx_toggle       <= '0';
        nrx_stall_mask   <= '0';
        nrx_bytesel      <= '0';
        nrx_size         <= (others => '0');
        nrx_acksize      <= (others => '0');
        nrx_buf_full     <= '0';
        nrx_has_oob      <= '0';

        minic_rx_addr_cur  <= (others => '0');
        minic_mcr_rx_ready <= '0';
        minic_mcr_rx_full  <= '0';
        nrx_newpacket      <= '0';

        wbs_ack_o          <= '0';
      else
        -- Host can modify the RX DMA registers only when the DMA engine is disabled
        -- (MCR_RX_EN = 0)
        if(minic_mcr_rx_en = '0') then

          nrx_newpacket       <= '0';
          -- mask out the stall line on the fabric I/F, so the endpoint
          -- can cut the traffic using 802.1 flow control
          nrx_stall_mask      <= '0';
          nrx_state           <= RX_WAIT_SOF;
          minic_mcr_rx_ready  <= '0';

          -- handle writes to RX_ADDR and RX_AVAIL
          if(minic_rx_addr_load = '1') then
            nrx_mem_a_saved   <= unsigned(minic_rx_addr_new(g_memsize_log2+1 downto 2));
            minic_rx_addr_cur <= (others => '0');
          end if;

          if(minic_rx_avail_load = '1') then
            nrx_buf_full      <= '0';
            minic_mcr_rx_full <= '0';
            nrx_avail         <= unsigned(minic_rx_avail_new(nrx_avail'high downto 0));
          end if;

          wbs_ack_o <= '0';
        else

          case nrx_state is

-------------------------------------------------------------------------------
-- State "Wait for start of frame". We wait until CYC goes high
-- on the RX fabric and then we commence reception of the packet.
-------------------------------------------------------------------------------
            when RX_WAIT_SOF =>

              nrx_newpacket    <= '0';
              nrx_mem_wr       <= '0';
              nrx_has_oob      <= '0';
              nrx_bytesel      <= '0';
              nrx_size         <= (others => '0');

              nrx_mux_d        <= '0';
              nrx_mem_a        <= nrx_mem_a_saved;
              wbs_ack_o        <= '0';
              nrx_acksize      <= (others => '0');
              nrx_toggle       <= '0';
              nrx_oob_toggle   <= '0';

              --================--
              ----    DATA    ----
              if( wbs_adr_i = c_WRF_DATA ) then
                if( g_buffer_little_endian = false ) then
                  nrx_mem_d(15 downto 0) <= wbs_dat_i;
                else
                  nrx_mem_d(15 downto 8) <= wbs_dat_i(7 downto 0);
                  nrx_mem_d( 7 downto 0) <= wbs_dat_i(15 downto 8);
                end if;
                nrx_ptoggle       <= '1';
              --================--
              ----   STATUS   ----
              elsif( wbs_adr_i = c_WRF_STATUS) then
								if(g_buffer_little_endian = false) then
                  nrx_status_reg <= f_unmarshall_wrf_status( wbs_dat_i(7 downto 0) & wbs_dat_i(15 downto 8) );
                else
                  nrx_status_reg <= f_unmarshall_wrf_status( wbs_dat_i );
                end if;
                nrx_ptoggle       <= '0';
              --===============--
              ------- OOB -------
							elsif(wbs_adr_i = c_WRF_OOB) then
                nrx_size  <= nrx_size + 1;
              	nrx_has_oob <= '1';
								if(g_buffer_little_endian = false) then
									--nrx_oob_reg(15 downto 8)  <= wbs_dat_i(7 downto 0);
                  --nrx_oob_reg(7 downto 0)   <= wbs_dat_i(15 downto 8);
									nrx_mem_d(15 downto 8)  <= wbs_dat_i(7 downto 0);
                  nrx_mem_d(7 downto 0)   <= wbs_dat_i(15 downto 8);
                else
                  --nrx_oob_reg(15 downto 0)  <= wbs_dat_i(15 downto 0);
                  nrx_mem_d(15 downto 0)  <= wbs_dat_i(15 downto 0);
                end if;
                nrx_ptoggle       <= '0';
              end if;

              minic_mcr_rx_full <= nrx_buf_full;

              if(wbs_cyc_i = '1') then
                if(wbs_adr_i /= c_WRF_STATUS) then
                  nrx_size   <= nrx_size + 1;
                end if;
                nrx_stall_mask <= '0';
                nrx_state     <= RX_ALLOCATE_DESCRIPTOR;
              else
                nrx_stall_mask <= not nrx_buf_full;
              end if;

-------------------------------------------------------------------------------
-- State "Allocate RX descriptor": puts an empty (invalid) RX descriptor at the
-- current location in the RX buffer and then starts receiving the data
-------------------------------------------------------------------------------              
            when RX_ALLOCATE_DESCRIPTOR =>

              wbs_ack_o        <= '0';
              nrx_toggle       <= nrx_ptoggle;
              nrx_oob_toggle   <= '0';

              -- wait until we have memory access
              if(mem_arb_rx = '0') then
                nrx_mem_a_saved <= nrx_mem_a;
                if(nrx_avail = to_unsigned(0, nrx_avail'length)) then
                  nrx_mux_d    <= '0';
                  nrx_mem_wr   <= '0';
                  wbs_ack_o    <= '0';
                  nrx_buf_full <= '1';

                  nrx_state    <= RX_WAIT_SOF;
                else
                  nrx_avail       <= nrx_avail - 1;
                  nrx_mux_d       <= '0';
                  nrx_mem_wr      <= '1';

                  wbs_ack_o       <= '1';
                  nrx_acksize     <= nrx_acksize + 1;

                  nrx_state     <= RX_DATA;
                  -- allow the fabric to receive the data
                  nrx_stall_mask <= '1';
                end if;
              end if;

-------------------------------------------------------------------------------
-- State "Receive data". Receive data, write it into the DMA memory
-------------------------------------------------------------------------------              
            when RX_DATA =>

              nrx_mux_d  <= '1';
              nrx_mem_wr <= '0';

              -- if we have no more space in memory then generate error
              -- and finish reception
              if(nrx_avail = to_unsigned(0, nrx_avail'length)) then
                nrx_status_reg.error <= '1';
                nrx_buf_full <= '1';
                nrx_state <= RX_UPDATE_DESC;
              end if;

              ------------------------------------------------------
              -- error/end-of-frame?
              ------------------------------------------------------
              if(wbs_cyc_i='0' or nrx_status_reg.error='1') then
                wbs_ack_o <= '0';

                -- flush the remaining packet data into the DMA buffer
                nrx_state <= RX_MEM_FLUSH;
                if(nrx_toggle = '1') then
                  nrx_mem_a <= nrx_mem_a + 1;
                end if;

                -- disable the RX fabric reception, so we won't get another
                -- packet before we are done with the RX descriptor update
                nrx_stall_mask <= '0';
              end if;

              ------------------------------------------------------
              -- normal operation
              ------------------------------------------------------
              if(wbs_stb_i = '1') then

                -- latch the bytesel signal to support frames having odd lengths
                if(wbs_sel_i = "10") then
                  nrx_bytesel <= '1';
                end if;

                wbs_ack_o <= '1';
                nrx_acksize <= nrx_acksize + 1;

                --================--
                ----  DATA REG  ----
								if(wbs_adr_i = c_WRF_DATA) then
                  nrx_size  <= nrx_size + 1;
                	-- pack two 16-bit words received from the fabric I/F into one
                	-- 32-bit DMA memory word
                	if(g_buffer_little_endian = false) then
                	  -- big endian RX buffer
                	  if(nrx_toggle = '0') then
                	    nrx_mem_d(31 downto 16) <= wbs_dat_i;
                      nrx_mem_d(15 downto 0)  <= (others=>'0');
                	  else
                	    nrx_mem_d(15 downto 0) <= wbs_dat_i;
                	  end if;
                	else
                	  -- little endian RX buffer
                	  if(nrx_toggle = '0') then
                      nrx_mem_d(31 downto 16)<= (others=>'0');
                	    nrx_mem_d(15 downto 8) <= wbs_dat_i(7 downto 0);
                	    nrx_mem_d(7 downto 0)  <= wbs_dat_i(15 downto 8);
                	  else
                	    nrx_mem_d(31 downto 24) <= wbs_dat_i(7 downto 0);
                	    nrx_mem_d(23 downto 16) <= wbs_dat_i(15 downto 8);
                	  end if;
                	end if;
                	nrx_toggle <= not nrx_toggle;
                --================--
                ---- STATUS REG ----
								elsif(wbs_adr_i = c_WRF_STATUS) then
									if(g_buffer_little_endian = false) then
										--nrx_status_reg(15 downto 8)  <= wbs_dat_i(7 downto 0);
                    --nrx_status_reg(7 downto 0)   <= wbs_dat_i(15 downto 8);
                    nrx_status_reg <= f_unmarshall_wrf_status( wbs_dat_i(7 downto 0) & wbs_dat_i(15 downto 8) );
                  else
                    --nrx_status_reg(15 downto 0)  <= wbs_dat_i(15 downto 0);
                    nrx_status_reg <= f_unmarshall_wrf_status( wbs_dat_i );
                  end if;
                --===============--
                ------- OOB -------
								elsif(wbs_adr_i = c_WRF_OOB) then
                  nrx_size  <= nrx_size + 1;
                	-- we've got RX OOB tag? Remember it and later put it in the
                	-- descriptor header
                	nrx_has_oob <= '1';

                	if(g_buffer_little_endian = false) then
                	  -- big endian RX buffer
                	  if(nrx_toggle = '0') then
                	    nrx_mem_d(31 downto 16) <= wbs_dat_i;
                      nrx_mem_d(15 downto 0)  <= (others=>'0');
                	  else
                	    nrx_mem_d(15 downto 0) <= wbs_dat_i;
                	  end if;
                	else
                	  -- little endian RX buffer
                	  if(nrx_toggle = '0') then
                      nrx_mem_d(31 downto 16)<= (others=>'0');
                	    nrx_mem_d(15 downto 8) <= wbs_dat_i(7 downto 0);
                	    nrx_mem_d(7 downto 0)  <= wbs_dat_i(15 downto 8);
                	  else
                	    nrx_mem_d(31 downto 24) <= wbs_dat_i(7 downto 0);
                	    nrx_mem_d(23 downto 16) <= wbs_dat_i(15 downto 8);
                	  end if;
                	end if;
                  nrx_toggle <= not nrx_toggle;
								end if;
              elsif(wbs_stb_i = '0') then
                wbs_ack_o <= '0';
              end if;

              -- we've got the second valid word of the payload or the last data word, write it to the
              -- memory
              if( nrx_avail/=to_unsigned(0, nrx_avail'length) and nrx_toggle = '1' and wbs_stb_i = '1' and wbs_cyc_i = '1') then
                nrx_mem_a  <= nrx_mem_a + 1;
                nrx_mem_wr <= '1';
                nrx_avail  <= nrx_avail - 1;

                -- check if we are synchronized with the memory write arbiter.
                if(mem_arb_rx = '1' and wbs_cyc_i = '1') then
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
              wbs_ack_o  <= '0';
              nrx_state  <= RX_DATA;

-------------------------------------------------------------------------------
-- State "Memory flush": flushes the remaining contents of RX data register
-- into the DMA buffer after end-of-packet
-------------------------------------------------------------------------------
            when RX_MEM_FLUSH =>
              wbs_ack_o   <= '0';
              nrx_mem_wr  <= '1';
              if(mem_arb_rx = '0') then
                nrx_avail <= nrx_avail - 1;
                nrx_state <= RX_UPDATE_DESC;
              end if;

-------------------------------------------------------------------------------
-- State "Update RX descriptor": writes the frame size, OOB presence and error
-- flags into the empty RX descriptor allocated at the beginning of the reception
-- of the frame, and marks the descriptor as valid. Also triggers the RX ready
-- interrupt.
-------------------------------------------------------------------------------
              
            when RX_UPDATE_DESC =>
              wbs_ack_o   <= '0';

              ------------------------------------
              -- Discard packets other than PTP --
              ------------------------------------
              if( nrx_class_match /= g_class_mask )then
                nrx_mem_wr    <= '0';
                nrx_newpacket <= '0';
                minic_mcr_rx_ready <= '0';
                nrx_state <= RX_WAIT_SOF;
              elsif(mem_arb_rx = '0') then

                -- store the current write pointer as a readback value of RX_ADDR register, so
                -- the host can determine the RX descriptor we're actually working on
                minic_rx_addr_cur(g_memsize_log2+1 downto 0)                      <= std_logic_vector(nrx_mem_a_saved) & "00";
                minic_rx_addr_cur(minic_rx_addr_cur'high downto g_memsize_log2+2) <= (others => '0');

                nrx_mem_a       <= nrx_mem_a_saved;
                nrx_mem_a_saved <= nrx_mem_a + 1;

                -- compose the RX descriptor
                nrx_mem_d(31)                         <= '1';
                nrx_mem_d(30)                         <= nrx_status_reg.error;
                nrx_mem_d(29)                         <= nrx_has_oob;
                nrx_mem_d(28 downto g_memsize_log2+1) <= (others => '0');
                nrx_mem_d(g_memsize_log2 downto 1)    <= std_logic_vector(nrx_size);
                nrx_mem_d(0)                          <= nrx_bytesel;

                nrx_mem_wr <= '1';

                -- trigger the RX interrupt and assert RX_READY flag to inform
                -- the host that we've received something
                nrx_newpacket      <= '1';
                minic_mcr_rx_ready <= '1';

                -- wait for another packet
                nrx_state <= RX_WAIT_SOF;
              else
                nrx_mem_wr <= '0';
              end if;
              
            when others => null;
          end case;
        end if;
      end if;
    end if;
  end process;

  wbs_err_o <= nrx_buf_full;

-------------------------------------------------------------------------------
-- helper process for producing the RX fabric data request signal (combinatorial)
-------------------------------------------------------------------------------  
  gen_rx_dreq : process(nrx_stall_mask, nrx_state, mem_arb_rx, nrx_toggle, minic_mcr_rx_en)
  begin
    -- make sure we don't have any incoming data when the reception is masked (e.g.
    -- the miNIC is updating the descriptors of finishing the memory write. 
    if(minic_mcr_rx_en = '0' or nrx_state=RX_ALLOCATE_DESCRIPTOR or nrx_state=RX_UPDATE_DESC or nrx_state=RX_MEM_FLUSH) then
      wbs_stall_o <= '1';
    elsif(nrx_stall_mask = '0') then
      wbs_stall_o <= '0';

    -- the condition below forces the RX FSM to go into RX_MEM_RESYNC state. Don't
    -- receive anything during this time
    elsif(nrx_toggle = '1' and mem_arb_rx = '1') then
      wbs_stall_o <= '1';
    else
      wbs_stall_o <= '0';
    end if;
  end process;


-------------------------------------------------------------------------------
-- TX Timestamping unit
-------------------------------------------------------------------------------  
  tsu_fsm : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i = '0') then
        minic_tsfifo_wr_req <= '0';
        minic_tsfifo_fid    <= (others => '0');
        minic_tsfifo_pid    <= (others => '0');
        minic_tsfifo_tsval  <= (others => '0');
        txtsu_ack_int       <= '0';
      else
        -- Make sure the timestamp is written to the FIFO only once.

        if(txtsu_valid_i = '1' and txtsu_ack_int = '0' and minic_tsfifo_wr_full = '0') then
          minic_tsfifo_wr_req <= '1';
          minic_tsfifo_tsval  <= txtsu_tsval_i;
          minic_tsfifo_fid    <= txtsu_frame_id_i;
          minic_tsfifo_pid    <= txtsu_port_id_i;
          txtsu_ack_int       <= '1';
        else
          txtsu_ack_int       <= '0';
          minic_tsfifo_wr_req <= '0';
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
        elsif (minic_mcr_rx_full = '1') then
          irq_rx <= '1';
        elsif (irq_rx_ack = '1') then
          irq_rx <= '0';
        end if;
      end if;
    end if;
  end process;

  irq_txts <= not minic_tsfifo_wr_empty;

  WB_SLAVE : minic_wb_slave
    port map (
      rst_n_i                 => rst_n_i,
      wb_clk_i                => clk_sys_i,
      wb_addr_i               => wb_addr_i,
      wb_data_i               => wb_data_i,
      wb_data_o               => wb_data_o,
      wb_cyc_i                => wb_cyc_i,
      wb_sel_i                => wb_sel_i,
      wb_stb_i                => wb_stb_i,
      wb_we_i                 => wb_we_i,
      wb_ack_o                => wb_ack_o,
      wb_irq_o                => wb_irq_o,
      minic_mcr_tx_start_o    => minic_mcr_tx_start,
      minic_mcr_rx_ready_i    => minic_mcr_rx_ready,
      minic_mcr_rx_full_i     => minic_mcr_rx_full,
      minic_mcr_rx_en_o       => minic_mcr_rx_en,
      minic_mcr_tx_idle_i     => minic_mcr_tx_idle,
      minic_mcr_tx_error_i    => minic_mcr_tx_error,
      minic_tx_addr_o         => minic_tx_addr_new,
      minic_tx_addr_i         => minic_tx_addr_cur,
      minic_tx_addr_load_o    => minic_tx_addr_load,
      minic_rx_addr_o         => minic_rx_addr_new,
      minic_rx_addr_i         => minic_rx_addr_cur,
      minic_rx_addr_load_o    => minic_rx_addr_load,
      minic_rx_avail_o        => minic_rx_avail_new,
      minic_rx_avail_i        => minic_rx_avail_cur,
      minic_rx_avail_load_o   => minic_rx_avail_load,
      minic_tsfifo_wr_req_i   => minic_tsfifo_wr_req,
      minic_tsfifo_wr_full_o  => minic_tsfifo_wr_full,
      minic_tsfifo_wr_empty_o => minic_tsfifo_wr_empty,
      minic_tsfifo_tsval_i    => minic_tsfifo_tsval,
      minic_tsfifo_pid_i      => minic_tsfifo_pid,
      minic_tsfifo_fid_i      => minic_tsfifo_fid,
      minic_dbgr_irq_cnt_i    => x"000000",
      minic_dbgr_wb_irq_val_i => '0',
      irq_tx_i                => irq_tx,
      irq_tx_ack_o            => irq_tx_ack,
      irq_tx_mask_o           => irq_tx_mask,
      irq_rx_i                => irq_rx,
      irq_rx_ack_o            => irq_rx_ack,
      irq_txts_i              => irq_txts);


  minic_rx_avail_cur (nrx_avail'high downto 0)                         <= std_logic_vector(nrx_avail);
  minic_rx_avail_cur (minic_rx_avail_cur'high downto nrx_avail'high+1) <= (others => '0');

end behavioral;


--==========================================================--
--      ENTITY USING RECORDS DEFINED FOR PIPELINED WB       --
--==========================================================--
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.wr_fabric_pkg.all;

entity wr_mini_nic_rec is

  generic (
    g_memsize_log2         : integer := 14;
    g_buffer_little_endian : boolean := true;
    g_class_mask           : std_logic_vector(7 downto 0) := "00000001");

  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    ---------------------------------------------------------------------------
    -- System memory i/f
    ---------------------------------------------------------------------------
    mem_data_o : out std_logic_vector(31 downto 0);
    mem_addr_o : out std_logic_vector(g_memsize_log2-1 downto 0);
    mem_data_i : in  std_logic_vector(31 downto 0);
    mem_wr_o   : out std_logic;

    ---------------------------------------------------------------------------
    -- Pipelined Wishbone interface
    ---------------------------------------------------------------------------
    wbm_o      : out t_wrf_source_out;
    wbm_i      : in  t_wrf_source_in;
    wbs_o      : out t_wrf_sink_out;
    wbs_i      : in  t_wrf_sink_in;

    ---------------------------------------------------------------------------
    -- TXTSU i/f
    ---------------------------------------------------------------------------
    txtsu_port_id_i  : in  std_logic_vector(4 downto 0);
    txtsu_frame_id_i : in  std_logic_vector(16 - 1 downto 0);
    txtsu_tsval_i    : in  std_logic_vector(28 + 4 - 1 downto 0);
    txtsu_valid_i    : in  std_logic;
    txtsu_ack_o      : out std_logic;

    ---------------------------------------------------------------------------
    -- Wishbone slave
    ---------------------------------------------------------------------------    

    wb_cyc_i  : in  std_logic;
    wb_stb_i  : in  std_logic;
    wb_we_i   : in  std_logic;
    wb_sel_i  : in  std_logic_vector(3 downto 0);
    wb_addr_i : in  std_logic_vector(3 downto 0);
    wb_data_i : in  std_logic_vector(31 downto 0);
    wb_data_o : out std_logic_vector(31 downto 0);
    wb_ack_o  : out std_logic;
    wb_irq_o  : out std_logic
  );
end wr_mini_nic_rec;


architecture behavioral of wr_mini_nic_rec is

  component wr_mini_nic
  generic (
    g_memsize_log2         : integer := 14;
    g_buffer_little_endian : boolean := true;
    g_class_mask           : std_logic_vector(7 downto 0) := "00000001");
  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;
    mem_data_o : out std_logic_vector(31 downto 0);
    mem_addr_o : out std_logic_vector(g_memsize_log2-1 downto 0);
    mem_data_i : in  std_logic_vector(31 downto 0);
    mem_wr_o   : out std_logic;

    wbm_dat_o     : out std_logic_vector(15 downto 0);
    wbm_adr_o     : out std_logic_vector(1 downto 0);
    wbm_sel_o     : out std_logic_vector(1 downto 0);
    wbm_cyc_o     : out std_logic;
    wbm_stb_o     : out std_logic;
    wbm_we_o      : out std_logic;
    wbm_stall_i   : in  std_logic;
    wbm_err_i     : in  std_logic;
    wbm_ack_i     : in  std_logic;

    wbs_dat_i     : in  std_logic_vector(15 downto 0);
    wbs_adr_i     : in  std_logic_vector(1 downto 0);
    wbs_sel_i     : in  std_logic_vector(1 downto 0);
    wbs_cyc_i     : in  std_logic;
    wbs_stb_i     : in  std_logic;
    wbs_we_i      : in  std_logic;
    wbs_stall_o   : out std_logic;
    wbs_err_o     : out std_logic;
    wbs_ack_o     : out std_logic;

    txtsu_port_id_i  : in  std_logic_vector(4 downto 0);
    txtsu_frame_id_i : in  std_logic_vector(16 - 1 downto 0);
    txtsu_tsval_i    : in  std_logic_vector(28 + 4 - 1 downto 0);
    txtsu_valid_i    : in  std_logic;
    txtsu_ack_o      : out std_logic;

    wb_cyc_i  : in  std_logic;
    wb_stb_i  : in  std_logic;
    wb_we_i   : in  std_logic;
    wb_sel_i  : in  std_logic_vector(3 downto 0);
    wb_addr_i : in  std_logic_vector(3 downto 0);
    wb_data_i : in  std_logic_vector(31 downto 0);
    wb_data_o : out std_logic_vector(31 downto 0);
    wb_ack_o  : out std_logic;
    wb_irq_o  : out std_logic);
  end component;

begin
  WR_MINI_NIC_STDLOGIC: wr_mini_nic
    generic map(
        g_memsize_log2          => g_memsize_log2,
        g_buffer_little_endian  => g_buffer_little_endian,
        g_class_mask            => g_class_mask
      )
    port map(
        clk_sys_i => clk_sys_i,
        rst_n_i   => rst_n_i,
        
        mem_data_o  => mem_data_o,
        mem_addr_o  => mem_addr_o,
        mem_data_i  => mem_data_i,
        mem_wr_o    => mem_wr_o,

        wbm_dat_o   => wbm_o.dat,
        wbm_adr_o   => wbm_o.adr,
        wbm_sel_o   => wbm_o.sel,
        wbm_cyc_o   => wbm_o.cyc,
        wbm_stb_o   => wbm_o.stb,
        wbm_we_o    => wbm_o.we,
        wbm_stall_i => wbm_i.stall,
        wbm_err_i   => wbm_i.err,
        wbm_ack_i   => wbm_i.ack,

        wbs_dat_i   => wbs_i.dat, 
        wbs_adr_i   => wbs_i.adr,
        wbs_sel_i   => wbs_i.sel,
        wbs_cyc_i   => wbs_i.cyc,
        wbs_stb_i   => wbs_i.stb,
        wbs_we_i    => wbs_i.we,
        wbs_stall_o => wbs_o.stall,
        wbs_err_o   => wbs_o.err,
        wbs_ack_o   => wbs_o.ack,
        
        txtsu_port_id_i  => txtsu_port_id_i,
        txtsu_frame_id_i => txtsu_frame_id_i,
        txtsu_tsval_i    => txtsu_tsval_i,
        txtsu_valid_i    => txtsu_valid_i,
        txtsu_ack_o      => txtsu_ack_o,
        
        wb_cyc_i    => wb_cyc_i,
        wb_stb_i    => wb_stb_i,
        wb_we_i     => wb_we_i,
        wb_sel_i    => wb_sel_i,
        wb_addr_i   => wb_addr_i,
        wb_data_i   => wb_data_i,
        wb_data_o   => wb_data_o,
        wb_ack_o    => wb_ack_o,
        wb_irq_o    => wb_irq_o
      );

end behavioral;
