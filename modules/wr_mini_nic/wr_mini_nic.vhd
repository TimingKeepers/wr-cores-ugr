-------------------------------------------------------------------------------
-- Title      : Mini Embedded DMA Network Interface Controller
-- Project    : WhiteRabbit Core
-------------------------------------------------------------------------------
-- File       : wrsw_mini_nic.vhd
-- Author     : Tomasz Wlostowski
-- Company    : CERN BE-Co-HT
-- Created    : 2010-07-26
-- Last update: 2011-03-27
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
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.global_defs.all;

entity wr_mini_nic is

  generic (
    g_memsize_log2         : integer := 14;
    g_buffer_little_endian : boolean := true);

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
-- WRF source/sink
-------------------------------------------------------------------------------
    
    src_data_o     : out std_logic_vector(15 downto 0);
    src_ctrl_o     : out std_logic_vector(c_wrsw_ctrl_size - 1 downto 0);
    src_bytesel_o  : out std_logic;
    src_sof_p1_o   : out std_logic;
    src_eof_p1_o   : out std_logic;
    src_dreq_i     : in  std_logic;
    src_valid_o    : out std_logic;
    src_error_p1_o : out std_logic;
    src_error_p1_i : in  std_logic;

    snk_data_i     : in  std_logic_vector(15 downto 0);
    snk_ctrl_i     : in  std_logic_vector(c_wrsw_ctrl_size -1 downto 0);
    snk_bytesel_i  : in  std_logic;
    snk_sof_p1_i   : in  std_logic;
    snk_eof_p1_i   : in  std_logic;
    snk_dreq_o     : out std_logic;
    snk_valid_i    : in  std_logic;
    snk_error_p1_o : out std_logic;
    snk_error_p1_i : in  std_logic;

-------------------------------------------------------------------------------
-- TXTSU i/f
-------------------------------------------------------------------------------

    txtsu_port_id_i  : in  std_logic_vector(4 downto 0);
    txtsu_frame_id_i : in  std_logic_vector(c_wrsw_oob_frame_id_size -1 downto 0);
    txtsu_tsval_i    : in  std_logic_vector(c_wrsw_timestamp_size_r + c_wrsw_timestamp_size_f - 1 downto 0);
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
  constant c_GAP_SIZE : integer := 10;

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

  -- generates the TX control field depending on the current TX offset and the packet
  -- type (802.1q, TX OOB presence)
  function f_generate_tx_ctrl
    (is_802_1q  : std_logic;
     has_oob    : std_logic;
     has_mac    : std_logic;
     tx_counter : unsigned(g_memsize_log2 downto 0);
     sreg       : std_logic_vector(8 downto 0)
     ) return std_logic_vector is
  begin

    -- we've already transmitted the header 
    if(sreg = "000000000") then
      if(tx_counter = to_unsigned(0, tx_counter'length) and has_oob = '1') then
        return c_wrsw_ctrl_tx_oob;
      else
        return c_wrsw_ctrl_payload;
      end if;
    end if;

    if(sreg(0) = '1' or sreg(1) = '1' or sreg(2) = '1') then
      return c_wrsw_ctrl_dst_mac;
    end if;

    if(sreg(3) = '1' or sreg(4) = '1' or sreg(5) = '1') then
      if(has_mac = '1') then
        return c_wrsw_ctrl_src_mac;
      else
        return c_wrsw_ctrl_none;
      end if;
    end if;

    if(sreg(6) = '1') then
      if(is_802_1q = '1') then
        return c_wrsw_ctrl_none;
      else
        return c_wrsw_ctrl_ethertype;
      end if;

    end if;

    if(sreg(7) = '1') then
      if(is_802_1q = '1') then
        return c_wrsw_ctrl_vid_prio;
      else
        return c_wrsw_ctrl_payload;
      end if;
    end if;

    if(sreg(8) = '1') then
      if(is_802_1q = '1') then
        return c_wrsw_ctrl_ethertype;
      else
        return c_wrsw_ctrl_payload;
      end if;
    end if;

    return c_wrsw_ctrl_payload;
  end function f_generate_tx_ctrl;

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
  signal nrx_mem_a  : unsigned(g_memsize_log2-1 downto 0);
  signal nrx_mem_wr : std_logic;
  signal mem_arb_rx : std_logic;
  signal mem_arb_tx : std_logic;


-------------------------------------------------------------------------------
-- TX FSM stuff
-------------------------------------------------------------------------------

  type t_tx_fsm_state is (TX_IDLE, TX_READ_DESC, TX_START_PACKET, TX_HWORD, TX_LWORD, TX_END_PACKET, TX_OOB);

  alias ntx_desc_size is ntx_mem_d(g_memsize_log2 downto 0);
  alias ntx_desc_odd is ntx_mem_d(0);

  alias ntx_desc_valid is ntx_mem_d(31);
  alias ntx_desc_with_oob is ntx_mem_d(30);
  alias ntx_desc_802_1q is ntx_mem_d(29);
  alias ntx_desc_has_src_mac is ntx_mem_d(28);

  signal ntx_cntr_is_zero  : std_logic;
  signal ntx_ctrl_sreg     : std_logic_vector(8 downto 0);
  signal ntx_data_reg      : std_logic_vector(31 downto 0);
  signal ntx_cntr          : unsigned(g_memsize_log2 downto 0);
  signal ntx_has_oob       : std_logic;
  signal ntx_has_src_mac   : std_logic;
  signal ntx_is_802_1q     : std_logic;
  signal ntx_state         : t_tx_fsm_state;
  signal ntx_start_delayed : std_logic;
  signal ntx_size_odd      : std_logic;

-------------------------------------------------------------------------------
-- RX I/F signals
-------------------------------------------------------------------------------  

  type t_rx_fsm_state is (RX_WAIT_SOF, RX_MEM_RESYNC, RX_MEM_FLUSH, RX_ALLOCATE_DESCRIPTOR, RX_DATA, RX_UPDATE_DESC);

  signal nrx_state        : t_rx_fsm_state;
  signal nrx_avail        : unsigned(g_memsize_log2-1 downto 0);
  signal nrx_rdreg        : std_logic_vector(31 downto 0);
  signal nrx_rdreg_toggle : std_logic;


  signal nrx_mem_a_saved : unsigned(g_memsize_log2-1 downto 0);

  signal nrx_has_oob   : std_logic;
  signal nrx_bytesel   : std_logic;
  signal nrx_size      : unsigned(g_memsize_log2-1 downto 0);
  signal nrx_error     : std_logic;
  signal nrx_buf_full  : std_logic;
  signal nrx_done      : std_logic;
  signal nrx_dreq_mask : std_logic;



-------------------------------------------------------------------------------
-- Wishbone slave signals
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
  mem_data_o <= nrx_mem_d;
  ntx_mem_d  <= mem_data_i;
  mem_wr_o   <= nrx_mem_wr                  when mem_arb_rx = '1' else '0';

-------------------------------------------------------------------------------
-- TX Path  (Host -> Fabric)
-------------------------------------------------------------------------------  

-- helper signal to avoid big IF conditions in the FSM
  ntx_cntr_is_zero <= '1' when (ntx_cntr = to_unsigned(0, ntx_cntr'length)) else '0';

  tx_fsm : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        ntx_state          <= TX_IDLE;
        irq_tx             <= '0';
        src_sof_p1_o       <= '0';
        src_eof_p1_o       <= '0';
        src_valid_o        <= '0';
        minic_mcr_tx_error <= '0';
        minic_mcr_tx_idle  <= '0';
        ntx_mem_a          <= (others => '0');
        ntx_cntr           <= (others => '0');
        ntx_start_delayed  <= '0';
      else
        case ntx_state is

-------------------------------------------------------------------------------
-- Idle state: we wait until the host starts the DMA transfer
-------------------------------------------------------------------------------
          
          when TX_IDLE =>

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

-- the host loaded new TX DMA buffer address
            if(minic_tx_addr_load = '1') then
              ntx_mem_a <= unsigned(minic_tx_addr_new(g_memsize_log2+1 downto 2));
            end if;

-- the host started the DMA xfer (and we are "phased" with the arbiter)
            if(ntx_start_delayed = '1' and mem_arb_tx = '0') then
              ntx_state <= TX_READ_DESC;

-- clear the TX flags
              minic_mcr_tx_error <= '0';
              minic_mcr_tx_idle  <= '0';
            --          else
--              minic_mcr_tx_idle <= not irq_tx_mask;
            end if;


-------------------------------------------------------------------------------
-- Read descriptor header state: check if there's another descriptor in the buffer
-- and eventually, start transmitting it
-------------------------------------------------------------------------------
            
          when TX_READ_DESC =>
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
                ntx_cntr        <= unsigned(ntx_desc_size);
                ntx_has_oob     <= ntx_desc_with_oob;
                ntx_is_802_1q   <= ntx_desc_802_1q;
                ntx_has_src_mac <= ntx_desc_has_src_mac;
--                ntx_size_odd    <= ntx_desc_odd;
                ntx_state       <= TX_START_PACKET;
                ntx_mem_a       <= ntx_mem_a + 1;
              end if;
            end if;

-------------------------------------------------------------------------------
-- Start packet state: generates a start-of-packet condition on the TX fabric
-------------------------------------------------------------------------------            
          when TX_START_PACKET =>

-- check if the memory is ready, read the 1st word of the payload
            if(src_dreq_i = '1' and mem_arb_tx = '0') then
              ntx_data_reg  <= f_buf_swap_endian_32(ntx_mem_d);
              src_sof_p1_o  <= '1';
              ntx_state     <= TX_HWORD;
              ntx_ctrl_sreg <= "000000001";
              ntx_mem_a     <= ntx_mem_a + 1;
            end if;


--------------------------------------------------------------------------------
-- State "Transmit HI word" - transmit the most significant word of the packet
-------------------------------------------------------------------------------
            
          when TX_HWORD =>

-- check for errors
            if(snk_error_p1_i = '1') then
              minic_mcr_tx_error <= '1';
              irq_tx             <= '1';
              ntx_state          <= TX_IDLE;
            end if;

-- generate the control value depending on the packet type, OOB and the current
-- transmission offset.
            src_ctrl_o <= f_generate_tx_ctrl(ntx_is_802_1q, ntx_has_oob, ntx_has_src_mac, ntx_cntr, ntx_ctrl_sreg);
            src_data_o <= ntx_data_reg(31 downto 16);

            if(src_dreq_i = '1') then
              ntx_ctrl_sreg <= ntx_ctrl_sreg(ntx_ctrl_sreg'high-1 downto 0) & '0';

              if(ntx_cntr_is_zero = '1') then
                ntx_cntr     <= to_unsigned(c_GAP_SIZE, ntx_cntr'length);
                src_eof_p1_o <= '1';
                ntx_state    <= TX_END_PACKET;
                src_valid_o  <= '1';    --ntx_size_odd;
              else
                ntx_cntr    <= ntx_cntr - 1;
                ntx_state   <= TX_LWORD;
                src_valid_o <= '1';
              end if;

            else
              src_valid_o <= '0';
            end if;

            src_sof_p1_o <= '0';

--------------------------------------------------------------------------------
-- State "Transmit HI word" - transmit the least significant word of the packet
-------------------------------------------------------------------------------
          when TX_LWORD =>

-- check for errors
            if(snk_error_p1_i = '1') then
              minic_mcr_tx_error <= '1';
              irq_tx             <= '1';
              ntx_state          <= TX_IDLE;
            end if;

            src_ctrl_o <= f_generate_tx_ctrl(ntx_is_802_1q, ntx_has_oob, ntx_has_src_mac, ntx_cntr, ntx_ctrl_sreg);
            src_data_o <= ntx_data_reg (15 downto 0);

-- the TX fabric is ready, the memory is ready and we haven't reached the end
-- of the packet yet:

            if(src_dreq_i = '1' and mem_arb_tx = '0' and ntx_cntr_is_zero = '0') then
              ntx_data_reg <= f_buf_swap_endian_32(ntx_mem_d);

-- advance the control word generator shift register. Each bit in this register
-- represents a word of the packet header (bit 0 = bits 47:32 of the
-- destination MAC, bit 1 = bits 31:16 of DMAC, etc). This is to speed-up
-- the control word generation
              ntx_ctrl_sreg <= ntx_ctrl_sreg(ntx_ctrl_sreg'high-1 downto 0) & '0';
              src_valid_o   <= '1';
              ntx_cntr      <= ntx_cntr - 1;
              ntx_mem_a     <= ntx_mem_a + 1;
              ntx_state     <= TX_HWORD;

-- We're at the end of the packet. Generate an end-of-packet condition on the
-- fabric I/F
            elsif(ntx_cntr_is_zero = '1') then
              src_eof_p1_o <= '1';
              src_valid_o  <= '1';
              ntx_state    <= TX_END_PACKET;
            else
-- the fabric is not ready, don't send anything
              src_valid_o <= '0';
            end if;



-------------------------------------------------------------------------------
-- State end-of-packet: generate an inter-packet gap
-------------------------------------------------------------------------------
          when TX_END_PACKET =>
            src_valid_o  <= '0';
            src_eof_p1_o <= '0';
            ntx_cntr     <= ntx_cntr - 1;

            if(ntx_cntr_is_zero = '1') then
              ntx_state <= TX_READ_DESC;
            end if;
          when others => null;
        end case;
      end if;
    end if;
  end process;

-- these are never used:
  src_bytesel_o <= '0';

-------------------------------------------------------------------------------
-- RX Path (Fabric ->  Host)
-------------------------------------------------------------------------------  

  rx_fsm : process(clk_sys_i, rst_n_i)
  begin
    if rising_edge(clk_sys_i) then
      if rst_n_i = '0' then
        nrx_state        <= RX_WAIT_SOF;
        nrx_mem_a        <= (others => '0');
        nrx_mem_wr       <= '0';
        nrx_avail        <= (others => '0');
        nrx_rdreg        <= (others => '0');
        nrx_rdreg_toggle <= '0';
        nrx_dreq_mask    <= '0';
        nrx_bytesel      <= '0';
        nrx_size         <= (others => '0');
        nrx_buf_full     <= '0';
        nrx_error        <= '0';
        nrx_done         <= '0';
        nrx_has_oob      <= '0';

        minic_rx_addr_cur  <= (others => '0');
        minic_mcr_rx_ready <= '0';
        minic_mcr_rx_full  <= '0';
        nrx_newpacket      <= '0';
      else
-- Host can modify the RX DMA registers only when the DMA engine is disabled
-- (MCR_RX_EN = 0)
        if(minic_mcr_rx_en = '0') then


          nrx_newpacket <= '0';

          -- mask out the data request line on the fabric I/F, so the endpoint
          -- can cut the traffic using 802.1 flow control
          nrx_dreq_mask      <= '0';
          nrx_state          <= RX_WAIT_SOF;
          minic_mcr_rx_ready <= '0';

          -- handle writes to RX_ADDR and RX_AVAIL
          if(minic_rx_addr_load = '1') then
            nrx_mem_a_saved   <= unsigned(minic_rx_addr_new(g_memsize_log2+1 downto 2));
            minic_rx_addr_cur <= (others => '0');
          --      nrx_mem_a <= unsigned(minic_rx_addr_new(g_memsize_log2+1 downto 2));
          end if;

          if(minic_rx_avail_load = '1') then
            nrx_buf_full      <= '0';
            minic_mcr_rx_full <= '0';
            nrx_avail         <= unsigned(minic_rx_avail_new(nrx_avail'high downto 0));
          end if;
        else

          -- main RX FSM
          case nrx_state is

-------------------------------------------------------------------------------
-- State "Wait for start of frame". We wait until there's a start-of-frame condition
-- on the RX fabric and then we commence reception of the packet.
-------------------------------------------------------------------------------
            when RX_WAIT_SOF =>

              nrx_newpacket    <= '0';
              nrx_done         <= '0';
              nrx_mem_wr       <= '0';
              nrx_has_oob      <= '0';
              nrx_bytesel      <= '0';
              nrx_error        <= '0';
              nrx_size         <= (others => '0');
              nrx_mem_a        <= nrx_mem_a_saved;
              nrx_rdreg_toggle <= '0';

              minic_mcr_rx_full <= nrx_buf_full;

              if(snk_sof_p1_i = '1') then  -- got start-of-frame?
                nrx_dreq_mask <= '0';
                nrx_state     <= RX_ALLOCATE_DESCRIPTOR;
              else
                nrx_dreq_mask <= not nrx_buf_full;
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
                nrx_avail       <= nrx_avail - 1;
                nrx_mem_wr      <= '1';


                nrx_state     <= RX_DATA;
-- allow the fabric to receive the data
                nrx_dreq_mask <= '1';
              end if;

-------------------------------------------------------------------------------
-- State "Receive data". Receive data, write it into the DMA memory
-------------------------------------------------------------------------------              
              
            when RX_DATA =>

              nrx_mem_wr <= '0';

              -- check if we have enough space in the buffer
              if(nrx_avail = to_unsigned(0, nrx_avail'length)) then
                nrx_buf_full <= '1';
              end if;

              -- we've got a valid data word or end-of-frame/error condition
              if(snk_valid_i = '1' or snk_eof_p1_i = '1' or snk_error_p1_i = '1') then

                -- latch the bytesel signal to support frames having odd lengths
                if(snk_bytesel_i = '1') then
                  nrx_bytesel <= '1';
                end if;

                -- abort/error/end-of-frame?
                if(snk_error_p1_i = '1' or snk_eof_p1_i = '1') then
                  nrx_error <=  snk_error_p1_i;
                  nrx_done  <= '1';

                  -- flush the remaining packet data into the DMA buffer
                  nrx_state <= RX_MEM_FLUSH;

                  if(snk_valid_i = '1' or nrx_rdreg_toggle = '1') then
                    nrx_mem_a <= nrx_mem_a + 1;
                  end if;

                  -- disable the RX fabric reception, so we won't get another
                  -- packet before we are done with the RX descriptor update
                  nrx_dreq_mask <= '0';
                end if;

                if(snk_valid_i = '1') then
                  nrx_size <= nrx_size + 1;

                  -- pack two 16-bit words received from the fabric I/F into one
                  -- 32-bit DMA memory word

                  if(g_buffer_little_endian = false) then
                    -- big endian RX buffer
                    if(nrx_rdreg_toggle = '0') then
                      nrx_mem_d(31 downto 16) <= snk_data_i;
                    else
                      nrx_mem_d(15 downto 0) <= snk_data_i;
                    end if;
                  else
                    -- little endian RX buffer
                    if(nrx_rdreg_toggle = '0') then
                      nrx_mem_d(15 downto 8) <= snk_data_i(7 downto 0);
                      nrx_mem_d(7 downto 0)  <= snk_data_i(15 downto 8);
                    else
                      nrx_mem_d(31 downto 24) <= snk_data_i(7 downto 0);
                      nrx_mem_d(23 downto 16) <= snk_data_i(15 downto 8);
                    end if;
                  end if;

                  -- we've got RX OOB tag? Remember it and later put it in the
                  -- descriptor header
                  if(snk_ctrl_i = c_wrsw_ctrl_rx_oob) then
                    nrx_has_oob <= '1';
                  end if;

                  nrx_rdreg_toggle <= not nrx_rdreg_toggle;
                end if;
              end if;

              -- we've got the second valid word of the payload, write it to the
              -- memory
              if(nrx_rdreg_toggle = '1' and snk_valid_i = '1' and snk_eof_p1_i = '0' and snk_error_p1_i = '0') then
                nrx_mem_a  <= nrx_mem_a + 1;
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
              if(snk_error_p1_i = '1') then
                nrx_error     <= '1';
                nrx_done      <= '1';
                nrx_state     <= RX_MEM_FLUSH;
                nrx_mem_a     <= nrx_mem_a + 1;
                nrx_dreq_mask <= '0';
                nrx_size      <= nrx_size + 2;
              else
                nrx_state <= RX_DATA;
              end if;

-------------------------------------------------------------------------------
-- State "Memory flush": flushes the remaining contents of RX data register
-- into the DMA buffer after end-of-packet/error/abort
-------------------------------------------------------------------------------
              
            when RX_MEM_FLUSH =>
              nrx_mem_wr <= '1';
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
              if(mem_arb_rx = '0') then


                -- store the current write pointer as a readback value of RX_ADDR register, so
-- the host can determine the RX descriptor we're actually working on
                minic_rx_addr_cur(g_memsize_log2+1 downto 0)                      <= std_logic_vector(nrx_mem_a_saved) & "00";
                minic_rx_addr_cur(minic_rx_addr_cur'high downto g_memsize_log2+2) <= (others => '0');

                nrx_mem_a       <= nrx_mem_a_saved;
                nrx_mem_a_saved <= nrx_mem_a + 1;

-- compose the RX descriptor
                nrx_mem_d(31)                         <= '1';
                nrx_mem_d(30)                         <= nrx_error;
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

-------------------------------------------------------------------------------
-- helper process for producing the RX fabric data request signal (combinatorial)
-------------------------------------------------------------------------------  
  gen_rx_dreq : process(nrx_dreq_mask, nrx_state, mem_arb_rx, nrx_rdreg_toggle, snk_sof_p1_i, snk_valid_i, minic_mcr_rx_en)
  begin
-- make sure we don't have any incoming data when the reception is masked (e.g.
-- the miNIC is updating the descriptors of finishing the memory write. 
    if(minic_mcr_rx_en = '0') then
      snk_dreq_o <= '0';
    elsif(snk_sof_p1_i = '1' or nrx_dreq_mask = '0') then
      snk_dreq_o <= '0';

-- the condition below forces the RX FSM to go into RX_MEM_RESYNC state. Don't
-- receive anything during this time
    elsif(nrx_rdreg_toggle = '1' and mem_arb_rx = '1') then
      snk_dreq_o <= '0';
    else
      snk_dreq_o <= '1';
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

--  wb_irq_o <= wb_irq_int;

  minic_rx_avail_cur (nrx_avail'high downto 0)                         <= std_logic_vector(nrx_avail);
  minic_rx_avail_cur (minic_rx_avail_cur'high downto nrx_avail'high+1) <= (others => '0');


-- drive the unused lines

  src_error_p1_o <= '0';
  snk_error_p1_o <='0';
  
  
  
end behavioral;
