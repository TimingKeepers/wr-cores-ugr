-------------------------------------------------------------------------------
-- Title      : Simple Pipelined Wishbone MUX/DEMUX for WRPC
-- Project    : WhiteRabbit
-------------------------------------------------------------------------------
-- File       : wbp_mux.vhd
-- Author     : Grzegorz Daniluk
-- Company    : Elproma
-- Created    : 2011-08-11
-- Last update: 2011-08-11
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description:
-- This is the simple multiplexer/demultiplexer for WR Fabric interface 
-- (Pipelined Wishbone interface). It forwards ethernet frames between 
-- WR endpoint, Mini-NIC and external Fabric interface in both directions. 
-- In the direction 'from' WR endpoint it also decides whether the packet 
-- has to be forwarded to Mini-NIC (if it is the PTP message) or to the 
-- external interface (others).
-------------------------------------------------------------------------------
-- Copyright (c) 2011 Grzegorz Daniluk
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2011-08-11  1.0      greg.d          Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity wbp_mux is
  generic(
    g_aw        : integer := 2;
    g_dw        : integer := 16;
    g_sw        : integer := 2
  );
  port(
    clk_sys_i        : in  std_logic;
    rst_n_i      : in  std_logic; 

    --ENDPOINT
    ep_wbm_adr_i   : in  std_logic_vector(g_aw-1 downto 0);
    ep_wbm_dat_i   : in  std_logic_vector(g_dw-1 downto 0);
    ep_wbm_sel_i   : in  std_logic_vector(g_sw-1 downto 0);
    ep_wbm_cyc_i   : in  std_logic;
    ep_wbm_stb_i   : in  std_logic;
    ep_wbm_ack_o   : out std_logic;
    ep_wbm_err_o   : out std_logic;
    ep_wbm_stall_o : out std_logic;
    
    ep_wbs_adr_o  : out std_logic_vector(g_aw-1 downto 0);
    ep_wbs_dat_o  : out std_logic_vector(g_dw-1 downto 0);
    ep_wbs_sel_o  : out std_logic_vector(g_sw-1 downto 0);
    ep_wbs_cyc_o  : out std_logic;
    ep_wbs_stb_o  : out std_logic;
    ep_wbs_ack_i  : in  std_logic;
    ep_wbs_err_i  : in  std_logic;
    ep_wbs_stall_i: in  std_logic;

    --PTP packets eg. for Mini-NIC
    ptp_wbm_adr_i   : in  std_logic_vector(g_aw-1 downto 0);
    ptp_wbm_dat_i   : in  std_logic_vector(g_dw-1 downto 0);
    ptp_wbm_sel_i   : in  std_logic_vector(g_sw-1 downto 0);
    ptp_wbm_cyc_i   : in  std_logic;
    ptp_wbm_stb_i   : in  std_logic;
    ptp_wbm_ack_o   : out std_logic;
    ptp_wbm_err_o   : out std_logic;
    ptp_wbm_stall_o : out std_logic;

    ptp_wbs_adr_o  : out std_logic_vector(g_aw-1 downto 0);
    ptp_wbs_dat_o  : out std_logic_vector(g_dw-1 downto 0);
    ptp_wbs_sel_o  : out std_logic_vector(g_sw-1 downto 0);
    ptp_wbs_cyc_o  : out std_logic;
    ptp_wbs_stb_o  : out std_logic;
    ptp_wbs_ack_i  : in  std_logic;
    ptp_wbs_err_i  : in  std_logic;
    ptp_wbs_stall_i: in  std_logic;

    --External WBP port
    ext_wbm_adr_i   : in  std_logic_vector(g_aw-1 downto 0);
    ext_wbm_dat_i   : in  std_logic_vector(g_dw-1 downto 0);
    ext_wbm_sel_i   : in  std_logic_vector(g_sw-1 downto 0);
    ext_wbm_cyc_i   : in  std_logic;
    ext_wbm_stb_i   : in  std_logic;
    ext_wbm_ack_o   : out std_logic;
    ext_wbm_err_o   : out std_logic;
    ext_wbm_stall_o : out std_logic;
    
    ext_wbs_adr_o  : out std_logic_vector(g_aw-1 downto 0);
    ext_wbs_dat_o  : out std_logic_vector(g_dw-1 downto 0);
    ext_wbs_sel_o  : out std_logic_vector(g_sw-1 downto 0);
    ext_wbs_cyc_o  : out std_logic;
    ext_wbs_stb_o  : out std_logic;
    ext_wbs_ack_i  : in  std_logic;
    ext_wbs_err_i  : in  std_logic;
    ext_wbs_stall_i: in  std_logic
  );
end wbp_mux;

architecture behaviour of wbp_mux is

  --==================================--
  -- WBP fabtic interface definitions --
  --==================================--
  -- WBP available addresses
  constant c_WBP_STATUS : std_logic_vector(1 downto 0) := "11";
  constant c_WBP_DATA   : std_logic_vector(1 downto 0) := "00";
  constant c_WBP_OOB    : std_logic_vector(1 downto 0) := "01";
  constant c_WBP_USER   : std_logic_vector(1 downto 0) := "10";
  -- WBP available packet classes (PTP and Etherbone)
  constant c_CLASS_PTP  : std_logic_vector(7 downto 0) := "00000001";
  constant c_CLASS_EB   : std_logic_vector(7 downto 0) := "00000010";

  --==================================--
  --   Masters to Slave mux signals   --
  --==================================--
  constant c_LAST_EXT : std_logic := '0';
  constant c_LAST_PTP : std_logic := '1';
  type t_mux is (MUX_SEL, MUX_EXT, MUX_PTP);

  signal mux                 : t_mux;
  signal mux_last            : std_logic;
  signal mux_extdat_reg      : std_logic_vector(g_dw-1 downto 0);
  signal mux_ptpdat_reg      : std_logic_vector(g_dw-1 downto 0);
  signal mux_extadr_reg      : std_logic_vector(g_aw-1 downto 0);
  signal mux_ptpadr_reg      : std_logic_vector(g_aw-1 downto 0);
  signal mux_extsel_reg      : std_logic_vector(g_sw-1 downto 0);
  signal mux_ptpsel_reg      : std_logic_vector(g_sw-1 downto 0);
  signal mux_extcyc_reg      : std_logic;
  signal mux_ptpcyc_reg      : std_logic;
  signal mux_extstb_reg      : std_logic;
  signal mux_ptpstb_reg      : std_logic;
  signal mux_pend_ext        : std_logic;
  signal mux_pend_ptp        : std_logic;
  signal force_stall         : std_logic;

  --==================================--
  --  Master to Slaves demux signals  --
  --==================================--
  type t_demux is (DMUX_WAIT, DMUX_STATUS, DMUX_PAYLOAD);
  signal demux                 : t_demux;
  signal dmux_stall_mask       : std_logic;
  signal dmux_status_reg       : std_logic_vector(g_dw-1 downto 0);
  alias  dmux_status_class is dmux_status_reg(7 downto 0);
  signal ep_stall_mask         : std_logic;
  signal ptp_select,ext_select : std_logic;
  signal ptp_send_status, ext_send_status : std_logic;

begin

  --===============================================--
  --                                               --
  -- Two WBP Masters talking to a single WBP Slave --
  --                                               --
  --===============================================--
  process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i='0') then
        mux_pend_ext <= '0';
        mux_pend_ptp <= '0';
        mux_last     <= '1';

        mux    <= MUX_SEL; 
      else
        case(mux) is
          when MUX_SEL =>
            if( ext_wbm_cyc_i='1' and ptp_wbm_cyc_i='0' ) then 
              mux <= MUX_EXT;
            elsif( ext_wbm_cyc_i='0' and ptp_wbm_cyc_i='1' ) then 
              mux <= MUX_PTP;
            --little Round Robin here
            elsif( ext_wbm_cyc_i='1' and ptp_wbm_cyc_i='1' and mux_last=c_LAST_PTP ) then 
              mux <= MUX_EXT;
            elsif( ext_wbm_cyc_i='1' and ptp_wbm_cyc_i='1' and mux_last=c_LAST_EXT ) then 
              mux <= MUX_PTP;
            end if;

            if( ext_wbm_cyc_i='1' and ext_wbm_stb_i='1' ) then
              mux_pend_ext <= '1';
            elsif( ext_wbm_cyc_i='0' ) then
              mux_pend_ext <= '0';
            end if;
            if( ptp_wbm_cyc_i='1' and ptp_wbm_stb_i='1' ) then
              mux_pend_ptp <= '1';
            elsif( ptp_wbm_cyc_i='0' ) then
              mux_pend_ptp <= '0';
            end if;

          --Transfer from EXT WBP interface in progress
          when MUX_EXT =>
            mux_last     <= c_LAST_EXT;
            mux_pend_ext <= '0';
            if( ptp_wbm_cyc_i='1' and ptp_wbm_stb_i='1' ) then
              mux_pend_ptp <= '1';
            elsif( ptp_wbm_cyc_i='0' ) then
              mux_pend_ptp <= '0';
            end if;

            if( ext_wbm_cyc_i='0' ) then 
              mux <= MUX_SEL;
            end if;

          --Transfer from PTP WBP interface in progress
          when MUX_PTP =>
            mux_last <= c_LAST_PTP;
            mux_pend_ptp <= '0';
            if( ext_wbm_cyc_i='1' and ext_wbm_stb_i='1' ) then
              mux_pend_ext <= '1';
            elsif( ext_wbm_cyc_i='0' ) then
              mux_pend_ext <= '0';
            end if;
            if( ptp_wbm_cyc_i='0' ) then 
              mux <= MUX_SEL;
            end if;

          --Just in case
          when others=>
            mux <= MUX_SEL;
        end case;
      end if;
    end if;
  end process;

  force_stall <= '1' when( mux=MUX_SEL and ext_wbm_cyc_i='1' and ptp_wbm_cyc_i='1' ) else
                 '0';

  --buffers
  process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if(rst_n_i='0') then
        mux_extdat_reg <= (others=>'0');
        mux_ptpdat_reg <= (others=>'0');
        mux_extadr_reg <= (others=>'0');
        mux_ptpadr_reg <= (others=>'0');
        mux_extsel_reg <= (others=>'0');
        mux_ptpsel_reg <= (others=>'0');
        mux_extcyc_reg <= '0';
        mux_ptpcyc_reg <= '0';
        mux_extstb_reg <= '0';
        mux_ptpstb_reg <= '0';
      else
        if(ep_wbs_stall_i='0') then
          mux_extdat_reg <= ext_wbm_dat_i;
          mux_ptpdat_reg <= ptp_wbm_dat_i;
          mux_extadr_reg <= ext_wbm_adr_i;
          mux_ptpadr_reg <= ptp_wbm_adr_i;
          mux_extsel_reg <= ext_wbm_sel_i;
          mux_ptpsel_reg <= ptp_wbm_sel_i;
          mux_extcyc_reg <= ext_wbm_cyc_i;
          mux_ptpcyc_reg <= ptp_wbm_cyc_i;
          mux_extstb_reg <= ext_wbm_stb_i;
          mux_ptpstb_reg <= ptp_wbm_stb_i;
        end if;
      end if;
    end if;
  end process;

  ep_wbs_adr_o    <= mux_extadr_reg  when(mux=MUX_EXT) else
                     mux_ptpadr_reg  when(mux=MUX_PTP) else
                     (others=>'0');
  ep_wbs_dat_o    <= mux_extdat_reg  when(mux=MUX_EXT) else
                     mux_ptpdat_reg  when(mux=MUX_PTP) else
                     (others=>'0');
  ep_wbs_sel_o    <= mux_extsel_reg  when(mux=MUX_EXT) else
                     mux_ptpsel_reg  when(mux=MUX_PTP) else
                     (others=>'0');
  ep_wbs_cyc_o    <= mux_extcyc_reg  when(mux=MUX_EXT) else
                     mux_ptpcyc_reg  when(mux=MUX_PTP) else
                     '0';
  ep_wbs_stb_o    <= mux_extstb_reg or mux_pend_ext  when(mux=MUX_EXT) else
                     mux_ptpstb_reg or mux_pend_ptp  when(mux=MUX_PTP) else
                     '0';

  ext_wbm_ack_o   <= ep_wbs_ack_i   when(mux=MUX_EXT) else
                     '0';
  ptp_wbm_ack_o   <= ep_wbs_ack_i   when(mux=MUX_PTP) else
                     '0';

  ext_wbm_err_o   <= ep_wbs_err_i   when(mux=MUX_EXT) else
                     '0';
  ptp_wbm_err_o   <= ep_wbs_err_i   when(mux=MUX_PTP) else
                     '0';

  ext_wbm_stall_o <= ep_wbs_stall_i when(mux=MUX_EXT) else
                     '1'            when(mux=MUX_PTP) else
                     '1'            when(force_stall='1') else
                     '0';
  ptp_wbm_stall_o <= ep_wbs_stall_i when(mux=MUX_PTP) else
                     '1'            when(mux=MUX_EXT) else
                     '1'            when(force_stall='1') else
                     '0';


  --=============================================--
  --                                             --
  --     WBP Master talking to two WBP Slaves    --
  --                                             --
  --=============================================--
  process(clk_sys_i)
  begin
    if rising_edge(clk_sys_i) then
      if( rst_n_i='0' ) then
        ep_wbm_ack_o    <= '0';
        ep_wbm_err_o    <= '0';
        dmux_stall_mask <= '0';
        ptp_select      <= '0';
        ptp_send_status <= '0';
        ext_select      <= '0';
        ext_send_status <= '0';
        dmux_status_reg <= (others => '0');
        ep_stall_mask   <= '0';
        demux <= DMUX_WAIT;
      else
        case demux is
          ---------------------------------------------------------------
          --State DMUX_WAIT: Wait for the WBP cycle to start and then 
          --                 wait for the STATUS word
          ---------------------------------------------------------------
          when DMUX_WAIT =>
            ptp_select      <= '0';
            ptp_send_status <= '0';
            ext_select      <= '0';
            ext_send_status <= '0';
            if( ep_wbm_cyc_i='1' and ep_wbm_stb_i='1' and ep_wbm_adr_i=c_WBP_STATUS ) then
              ep_stall_mask   <= '1';
              dmux_status_reg <= ep_wbm_dat_i;
              demux <= DMUX_STATUS;
            else
              ep_stall_mask  <= '0';
            end if;

          ---------------------------------------------------------------
          --State DMUX_STATUS: Send Status word to appropriate interface
          ---------------------------------------------------------------
          when DMUX_STATUS =>

            ep_stall_mask   <= '1';

            if( dmux_status_class=c_CLASS_PTP  ) then
              ptp_select      <= '1';
              ptp_send_status <= '1';
              if( ptp_wbs_stall_i='0' ) then
                demux <= DMUX_PAYLOAD;
              end if;
            else 
              ext_select      <= '1';
              ext_send_status <= '1';
              if( ext_wbs_stall_i='0' ) then
                demux <= DMUX_PAYLOAD;
              end if;
            end if;

          ---------------------------------------------------------------
          --State DMUX_PAYLOAD: Just wait here till the end of the 
          --                    current transfer
          ---------------------------------------------------------------
          when DMUX_PAYLOAD =>
            ptp_send_status <= '0';
            ext_send_status <= '0';
            ep_stall_mask   <= '0';

            if(ep_wbm_cyc_i = '0') then
              demux <= DMUX_WAIT;
            end if;

          when others =>
            demux <= DMUX_WAIT;
        end case; 
      end if;
    end if;
  end process;


  ptp_wbs_cyc_o  <= ep_wbm_cyc_i when(ptp_select = '1') else 
                    '0';
  ptp_wbs_stb_o  <= '1'          when(ptp_send_status = '1') else
                    ep_wbm_stb_i when(ptp_select = '1') else 
                    '0';
  ptp_wbs_adr_o  <= c_WBP_STATUS when(ptp_send_status = '1') else
                    ep_wbm_adr_i when(ptp_select = '1') else 
                    (others=>'0');
  ptp_wbs_dat_o  <= dmux_status_reg when(ptp_send_status = '1') else
                    ep_wbm_dat_i    when(ptp_select = '1') else 
                    (others=>'0');
  ptp_wbs_sel_o  <= (others=>'1') when(ptp_send_status = '1') else 
                    ep_wbm_sel_i  when(ptp_select = '1') else 
                    (others=>'1');


  ep_wbm_ack_o   <= ptp_wbs_ack_i when(ptp_select = '1') else
                    ext_wbs_ack_i when(ext_select = '1') else
                    '0';
  ep_wbm_err_o   <= ptp_wbs_err_i when(ptp_select = '1') else
                    ext_wbs_err_i when(ext_select = '1') else
                    '0';

  ep_wbm_stall_o <= '1'             when(ep_stall_mask = '1') else
                    ptp_wbs_stall_i when(ptp_select = '1') else
                    ext_wbs_stall_i when(ext_select = '1') else
                    '0';


  ext_wbs_cyc_o  <= ep_wbm_cyc_i when(ext_select = '1') else 
                    '0';
  ext_wbs_stb_o  <= '1'          when(ext_send_status = '1') else
                    ep_wbm_stb_i when(ext_select = '1') else 
                    '0';
  ext_wbs_adr_o  <= c_WBP_STATUS when(ext_send_status = '1') else
                    ep_wbm_adr_i when(ext_select = '1') else 
                    (others=>'0');
  ext_wbs_dat_o  <= dmux_status_reg when(ext_send_status = '1') else
                    ep_wbm_dat_i    when(ext_select = '1') else 
                    (others=>'0');
  ext_wbs_sel_o  <= (others=>'1') when(ext_send_status = '1') else 
                    ep_wbm_sel_i  when(ext_select = '1') else 
                    (others=>'1');

end behaviour;
