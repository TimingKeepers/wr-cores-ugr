-------------------------------------------------------------------------------
-- Title      : Simple Pipelined Wishbone MUX/DEMUX for WRPC
-- Project    : WhiteRabbit
-------------------------------------------------------------------------------
-- File       : wbp_mux.vhd
-- Author     : Grzegorz Daniluk
-- Company    : Elproma
-- Created    : 2011-08-11
-- Last update: 2011-10-27
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

use work.wr_fabric_pkg.all;

entity wbp_mux is
  generic(
    g_aw        : integer := 2;
    g_dw        : integer := 16;
    g_sw        : integer := 2
  );
  port(
    clk_sys_i      : in  std_logic;
    rst_n_i        : in  std_logic; 

    --ENDPOINT
    ep_snk_adr_i   : in  std_logic_vector(g_aw-1 downto 0);
    ep_snk_dat_i   : in  std_logic_vector(g_dw-1 downto 0);
    ep_snk_sel_i   : in  std_logic_vector(g_sw-1 downto 0);
    ep_snk_cyc_i   : in  std_logic;
    ep_snk_stb_i   : in  std_logic;
    ep_snk_ack_o   : out std_logic;
    ep_snk_err_o   : out std_logic;
    ep_snk_stall_o : out std_logic;
    
    ep_src_adr_o  : out std_logic_vector(g_aw-1 downto 0);
    ep_src_dat_o  : out std_logic_vector(g_dw-1 downto 0);
    ep_src_sel_o  : out std_logic_vector(g_sw-1 downto 0);
    ep_src_cyc_o  : out std_logic;
    ep_src_stb_o  : out std_logic;
    ep_src_ack_i  : in  std_logic;
    ep_src_err_i  : in  std_logic;
    ep_src_stall_i: in  std_logic;

    --PTP packets eg. for Mini-NIC
    ptp_snk_adr_i   : in  std_logic_vector(g_aw-1 downto 0);
    ptp_snk_dat_i   : in  std_logic_vector(g_dw-1 downto 0);
    ptp_snk_sel_i   : in  std_logic_vector(g_sw-1 downto 0);
    ptp_snk_cyc_i   : in  std_logic;
    ptp_snk_stb_i   : in  std_logic;
    ptp_snk_ack_o   : out std_logic;
    ptp_snk_err_o   : out std_logic;
    ptp_snk_stall_o : out std_logic;

    ptp_src_adr_o  : out std_logic_vector(g_aw-1 downto 0);
    ptp_src_dat_o  : out std_logic_vector(g_dw-1 downto 0);
    ptp_src_sel_o  : out std_logic_vector(g_sw-1 downto 0);
    ptp_src_cyc_o  : out std_logic;
    ptp_src_stb_o  : out std_logic;
    ptp_src_ack_i  : in  std_logic;
    ptp_src_err_i  : in  std_logic;
    ptp_src_stall_i: in  std_logic;

    --External WBP port
    ext_snk_adr_i   : in  std_logic_vector(g_aw-1 downto 0);
    ext_snk_dat_i   : in  std_logic_vector(g_dw-1 downto 0);
    ext_snk_sel_i   : in  std_logic_vector(g_sw-1 downto 0);
    ext_snk_cyc_i   : in  std_logic;
    ext_snk_stb_i   : in  std_logic;
    ext_snk_ack_o   : out std_logic;
    ext_snk_err_o   : out std_logic;
    ext_snk_stall_o : out std_logic;
    
    ext_src_adr_o  : out std_logic_vector(g_aw-1 downto 0);
    ext_src_dat_o  : out std_logic_vector(g_dw-1 downto 0);
    ext_src_sel_o  : out std_logic_vector(g_sw-1 downto 0);
    ext_src_cyc_o  : out std_logic;
    ext_src_stb_o  : out std_logic;
    ext_src_ack_i  : in  std_logic;
    ext_src_err_i  : in  std_logic;
    ext_src_stall_i: in  std_logic;

    class_core_i: in std_logic_vector(7 downto 0)
  );
end wbp_mux;

architecture behaviour of wbp_mux is

  --==================================--
  -- WBP fabtic interface definitions --
  --==================================--
  -- WBP available addresses

  -- WBP available packet classes (PTP and Etherbone)
  

  --==================================--
  --   Masters to Slave mux signals   --
  --==================================--
  constant c_LAST_EXT : std_logic := '0';
  constant c_LAST_PTP : std_logic := '1';
  type t_mux is (MUX_SEL, MUX_EXT, MUX_PTP,MUX_END);

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
  signal ep_snk_stall_out : std_logic;
  

  --==================================--
  --  Master to Slaves demux signals  --
  --==================================--
  type t_demux is (DMUX_WAIT, DMUX_STATUS, DMUX_PAYLOAD);
  signal demux                 : t_demux;
  signal dmux_stall_mask       : std_logic;
  signal dmux_status_reg       : std_logic_vector(g_dw-1 downto 0);
  signal dmux_status_class : std_logic_vector(7 downto 0);
  signal ep_stall_mask         : std_logic;
  signal ptp_select,ext_select : std_logic;
  signal ptp_send_status, ext_send_status : std_logic;
  signal ep_src_stall_d0 : std_logic;

begin

dmux_status_class <= f_unmarshall_wrf_status(dmux_status_reg).match_class;
  
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
        ep_src_stall_d0 <= '0';
      else
        ep_src_stall_d0<=ep_src_stall_i;
        case(mux) is
          when MUX_SEL =>
            if( ext_snk_cyc_i='1' and ptp_snk_cyc_i='0' ) then 
              mux <= MUX_EXT;
            elsif( ext_snk_cyc_i='0' and ptp_snk_cyc_i='1' ) then 
              mux <= MUX_PTP;
            --little Round Robin here
            elsif( ext_snk_cyc_i='1' and ptp_snk_cyc_i='1' and mux_last=c_LAST_PTP ) then 
              mux <= MUX_EXT;
            elsif( ext_snk_cyc_i='1' and ptp_snk_cyc_i='1' and mux_last=c_LAST_EXT ) then 
              mux <= MUX_PTP;
            end if;

            if( ext_snk_cyc_i='1' and ext_snk_stb_i='1' ) then
              mux_pend_ext <= '1';
            elsif( ext_snk_cyc_i='0' ) then
              mux_pend_ext <= '0';
            end if;
            if( ptp_snk_cyc_i='1' and ptp_snk_stb_i='1' ) then
              mux_pend_ptp <= '1';
            elsif( ptp_snk_cyc_i='0' ) then
              mux_pend_ptp <= '0';
            end if;

          --Transfer from EXT WBP interface in progress
          when MUX_EXT =>
            mux_last     <= c_LAST_EXT;
            mux_pend_ext <= '0';
            if( ptp_snk_cyc_i='1' and ptp_snk_stb_i='1' ) then
              mux_pend_ptp <= '1';
            elsif( ptp_snk_cyc_i='0' ) then
              mux_pend_ptp <= '0';
            end if;

            if( ext_snk_cyc_i = '0' and ep_src_stall_i = '0' and ep_src_stall_d0 ='0') then 
              mux <= MUX_SEL;
            end if;

          --Transfer from PTP WBP interface in progress
          when MUX_PTP =>
            mux_last <= c_LAST_PTP;
            mux_pend_ptp <= '0';
            if( ext_snk_cyc_i='1' and ext_snk_stb_i='1' ) then
              mux_pend_ext <= '1';
            elsif( ext_snk_cyc_i='0' ) then
              mux_pend_ext <= '0';
            end if;

        if( ptp_snk_cyc_i='0' and ep_src_stall_i = '0' and ep_src_stall_d0= '0') then 
              mux <= MUX_SEL;
            end if;

          when MUX_END =>
        mux <= MUX_SEL;
          --Just in case
          when others=>
            mux <= MUX_SEL;
        end case;
      end if;
    end if;
  end process;

  force_stall <= '1' when( mux=MUX_SEL and ext_snk_cyc_i='1' and ptp_snk_cyc_i='1' ) else
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
        if(ep_src_stall_i='0') then
          mux_extdat_reg <= ext_snk_dat_i;
          mux_ptpdat_reg <= ptp_snk_dat_i;
          mux_extadr_reg <= ext_snk_adr_i;
          mux_ptpadr_reg <= ptp_snk_adr_i;
          mux_extsel_reg <= ext_snk_sel_i;
          mux_ptpsel_reg <= ptp_snk_sel_i;
          mux_extcyc_reg <= ext_snk_cyc_i;
          mux_ptpcyc_reg <= ptp_snk_cyc_i;
          mux_extstb_reg <= ext_snk_stb_i;
          mux_ptpstb_reg <= ptp_snk_stb_i;
        end if;
      end if;
    end if;
  end process;

  ep_src_adr_o    <= mux_extadr_reg  when(mux=MUX_EXT) else
                     mux_ptpadr_reg  when(mux=MUX_PTP) else
                     (others=>'0');
  ep_src_dat_o    <= mux_extdat_reg  when(mux=MUX_EXT) else
                     mux_ptpdat_reg  when(mux=MUX_PTP) else
                     (others=>'0');
  ep_src_sel_o    <= mux_extsel_reg  when(mux=MUX_EXT) else
                     mux_ptpsel_reg  when(mux=MUX_PTP) else
                     (others=>'0');
  ep_src_cyc_o    <= mux_extcyc_reg  when(mux=MUX_EXT) else
                     mux_ptpcyc_reg  when(mux=MUX_PTP) else
                     '0';
  ep_src_stb_o    <= mux_extstb_reg or mux_pend_ext  when(mux=MUX_EXT) else
                     mux_ptpstb_reg or mux_pend_ptp  when(mux=MUX_PTP) else
                     '0';

  ext_snk_ack_o   <= ep_src_ack_i   when(mux=MUX_EXT) else
                     '0';
  ptp_snk_ack_o   <= ep_src_ack_i   when(mux=MUX_PTP) else
                     '0';

  ext_snk_err_o   <= ep_src_err_i   when(mux=MUX_EXT) else
                     '0';
  ptp_snk_err_o   <= ep_src_err_i   when(mux=MUX_PTP) else
                     '0';

  ext_snk_stall_o <= ep_src_stall_i when(mux=MUX_EXT) else
                     '1'            when(mux=MUX_PTP) else
                     '1'            when(force_stall='1') else
                     '0';

  ptp_snk_stall_o <= ep_src_stall_i when(mux=MUX_PTP) else
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
            if( ep_snk_cyc_i='1' and ep_snk_stb_i='1' and ep_snk_adr_i=c_WRF_STATUS ) then
              ep_stall_mask   <= '1';
              dmux_status_reg <= ep_snk_dat_i;
              demux <= DMUX_STATUS;
            else
              dmux_status_reg<=(others => '0');
              ep_stall_mask  <= '0';
            end if;

          ---------------------------------------------------------------
          --State DMUX_STATUS: Send Status word to appropriate interface
          ---------------------------------------------------------------
          when DMUX_STATUS =>

            ep_stall_mask   <= '1';

            if(( dmux_status_class = x"00") or ((dmux_status_class and class_core_i) /= "00000000")) then
              ptp_select      <= '1';
              ptp_send_status <= '1';
              if( ptp_src_stall_i='0' ) then
                demux <= DMUX_PAYLOAD;
              end if;
            else 
              ext_select      <= '1';
              ext_send_status <= '1';
              if( ext_src_stall_i='0' ) then
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

            if(ep_snk_cyc_i = '0') then
              demux <= DMUX_WAIT;
            end if;

          when others =>
            demux <= DMUX_WAIT;
        end case; 
      end if;
    end if;
  end process;


  ptp_src_cyc_o  <= ep_snk_cyc_i when(ptp_select = '1') else 
                    '0';
  ptp_src_stb_o  <= '1'          when(ptp_send_status = '1') else
                    ep_snk_stb_i when(ptp_select = '1') else 
                    '0';
  ptp_src_adr_o  <= c_WRF_STATUS when(ptp_send_status = '1') else
                    ep_snk_adr_i when(ptp_select = '1') else 
                    (others=>'0');
  ptp_src_dat_o  <= dmux_status_reg when(ptp_send_status = '1') else
                    ep_snk_dat_i    when(ptp_select = '1') else 
                    (others=>'0');
  ptp_src_sel_o  <= (others=>'1') when(ptp_send_status = '1') else 
                    ep_snk_sel_i  when(ptp_select = '1') else 
                    (others=>'1');


  ep_snk_ack_o   <= ptp_src_ack_i when(ptp_select = '1') else
                    ext_src_ack_i when(ext_select = '1') else
                    (ep_snk_cyc_i and ep_snk_stb_i and not ep_snk_stall_out);

  ep_snk_err_o   <= ptp_src_err_i when(ptp_select = '1') else
                    ext_src_err_i when(ext_select = '1') else
                    '0';

  ep_snk_stall_out <= '1'             when(ep_stall_mask = '1') else
                    ptp_src_stall_i when(ptp_select = '1') else
                    ext_src_stall_i when(ext_select = '1') else
                    '0';

ep_snk_stall_o <= ep_snk_stall_out;


  ext_src_cyc_o  <= ep_snk_cyc_i when(ext_select = '1') else 
                    '0';
  ext_src_stb_o  <= '1'          when(ext_send_status = '1') else
                    ep_snk_stb_i when(ext_select = '1') else 
                    '0';
  ext_src_adr_o  <= c_WRF_STATUS when(ext_send_status = '1') else
                    ep_snk_adr_i when(ext_select = '1') else 
                    (others=>'0');
  ext_src_dat_o  <= dmux_status_reg when(ext_send_status = '1') else
                    ep_snk_dat_i    when(ext_select = '1') else 
                    (others=>'0');
  ext_src_sel_o  <= (others=>'1') when(ext_send_status = '1') else 
                    ep_snk_sel_i  when(ext_select = '1') else 
                    (others=>'1');

end behaviour;




--==========================================================--
--      ENTITY USING RECORDS DEFINED FOR PIPELINED WB       --
--==========================================================--
library ieee;
use ieee.std_logic_1164.all;
use work.wr_fabric_pkg.all;


entity xwbp_mux is
  port(
    clk_sys_i      : in  std_logic;
    rst_n_i        : in  std_logic; 

    --ENDPOINT
    ep_src_o       : out t_wrf_source_out;
    ep_src_i       : in  t_wrf_source_in;
    ep_snk_o       : out t_wrf_sink_out;
    ep_snk_i       : in  t_wrf_sink_in;
    --PTP packets eg. from Mini-NIC
    ptp_src_o      : out t_wrf_source_out;
    ptp_src_i      : in  t_wrf_source_in;
    ptp_snk_o      : out t_wrf_sink_out;
    ptp_snk_i      : in  t_wrf_sink_in;
    --External WBP port
    ext_src_o      : out t_wrf_source_out;
    ext_src_i      : in  t_wrf_source_in;
    ext_snk_o      : out t_wrf_sink_out;
    ext_snk_i      : in  t_wrf_sink_in;
    class_core_i : in std_logic_vector(7 downto 0)
  );
end xwbp_mux;

architecture behaviour of xwbp_mux is

  component wbp_mux
    generic(
      g_aw        : integer := 2;
      g_dw        : integer := 16;
      g_sw        : integer := 2
    );
    port(
      clk_sys_i      : in  std_logic;
      rst_n_i        : in  std_logic; 
  
      --ENDPOINT
      ep_snk_adr_i   : in  std_logic_vector(g_aw-1 downto 0);
      ep_snk_dat_i   : in  std_logic_vector(g_dw-1 downto 0);
      ep_snk_sel_i   : in  std_logic_vector(g_sw-1 downto 0);
      ep_snk_cyc_i   : in  std_logic;
      ep_snk_stb_i   : in  std_logic;
      ep_snk_ack_o   : out std_logic;
      ep_snk_err_o   : out std_logic;
      ep_snk_stall_o : out std_logic;
      
      ep_src_adr_o  : out std_logic_vector(g_aw-1 downto 0);
      ep_src_dat_o  : out std_logic_vector(g_dw-1 downto 0);
      ep_src_sel_o  : out std_logic_vector(g_sw-1 downto 0);
      ep_src_cyc_o  : out std_logic;
      ep_src_stb_o  : out std_logic;
      ep_src_ack_i  : in  std_logic;
      ep_src_err_i  : in  std_logic;
      ep_src_stall_i: in  std_logic;
  
      --PTP packets eg. for Mini-NIC
      ptp_snk_adr_i   : in  std_logic_vector(g_aw-1 downto 0);
      ptp_snk_dat_i   : in  std_logic_vector(g_dw-1 downto 0);
      ptp_snk_sel_i   : in  std_logic_vector(g_sw-1 downto 0);
      ptp_snk_cyc_i   : in  std_logic;
      ptp_snk_stb_i   : in  std_logic;
      ptp_snk_ack_o   : out std_logic;
      ptp_snk_err_o   : out std_logic;
      ptp_snk_stall_o : out std_logic;
  
      ptp_src_adr_o  : out std_logic_vector(g_aw-1 downto 0);
      ptp_src_dat_o  : out std_logic_vector(g_dw-1 downto 0);
      ptp_src_sel_o  : out std_logic_vector(g_sw-1 downto 0);
      ptp_src_cyc_o  : out std_logic;
      ptp_src_stb_o  : out std_logic;
      ptp_src_ack_i  : in  std_logic;
      ptp_src_err_i  : in  std_logic;
      ptp_src_stall_i: in  std_logic;
  
      --External WBP port
      ext_snk_adr_i   : in  std_logic_vector(g_aw-1 downto 0);
      ext_snk_dat_i   : in  std_logic_vector(g_dw-1 downto 0);
      ext_snk_sel_i   : in  std_logic_vector(g_sw-1 downto 0);
      ext_snk_cyc_i   : in  std_logic;
      ext_snk_stb_i   : in  std_logic;
      ext_snk_ack_o   : out std_logic;
      ext_snk_err_o   : out std_logic;
      ext_snk_stall_o : out std_logic;
      
      ext_src_adr_o  : out std_logic_vector(g_aw-1 downto 0);
      ext_src_dat_o  : out std_logic_vector(g_dw-1 downto 0);
      ext_src_sel_o  : out std_logic_vector(g_sw-1 downto 0);
      ext_src_cyc_o  : out std_logic;
      ext_src_stb_o  : out std_logic;
      ext_src_ack_i  : in  std_logic;
      ext_src_err_i  : in  std_logic;
      ext_src_stall_i: in  std_logic;
      class_core_i : in std_logic_vector(7 downto 0)
    );
  end component;

begin
  
  WBP_MUX_STDLOGIC: wbp_mux 
    generic map(
        g_aw => 2,
        g_dw => 16,
        g_sw => 2
      )
    port map(
        clk_sys_i       => clk_sys_i,
        rst_n_i         => rst_n_i,
                       
        ep_snk_adr_i    => ep_snk_i.adr,
        ep_snk_dat_i    => ep_snk_i.dat,
        ep_snk_sel_i    => ep_snk_i.sel,
        ep_snk_cyc_i    => ep_snk_i.cyc,
        ep_snk_stb_i    => ep_snk_i.stb,
        ep_snk_ack_o    => ep_snk_o.ack,
        ep_snk_err_o    => ep_snk_o.err,
        ep_snk_stall_o  => ep_snk_o.stall,
        
        ep_src_adr_o    => ep_src_o.adr,
        ep_src_dat_o    => ep_src_o.dat,
        ep_src_sel_o    => ep_src_o.sel,
        ep_src_cyc_o    => ep_src_o.cyc,
        ep_src_stb_o    => ep_src_o.stb,
        ep_src_ack_i    => ep_src_i.ack,
        ep_src_err_i    => ep_src_i.err,
        ep_src_stall_i  => ep_src_i.stall,
                       
        ptp_snk_adr_i   => ptp_snk_i.adr,
        ptp_snk_dat_i   => ptp_snk_i.dat,
        ptp_snk_sel_i   => ptp_snk_i.sel,
        ptp_snk_cyc_i   => ptp_snk_i.cyc,
        ptp_snk_stb_i   => ptp_snk_i.stb,
        ptp_snk_ack_o   => ptp_snk_o.ack,
        ptp_snk_err_o   => ptp_snk_o.err,
        ptp_snk_stall_o => ptp_snk_o.stall,
                       
        ptp_src_adr_o   => ptp_src_o.adr, 
        ptp_src_dat_o   => ptp_src_o.dat,
        ptp_src_sel_o   => ptp_src_o.sel,
        ptp_src_cyc_o   => ptp_src_o.cyc,
        ptp_src_stb_o   => ptp_src_o.stb,
        ptp_src_ack_i   => ptp_src_i.ack,
        ptp_src_err_i   => ptp_src_i.err,
        ptp_src_stall_i => ptp_src_i.stall,
                       
        ext_snk_adr_i   => ext_snk_i.adr, 
        ext_snk_dat_i   => ext_snk_i.dat,
        ext_snk_sel_i   => ext_snk_i.sel,
        ext_snk_cyc_i   => ext_snk_i.cyc,
        ext_snk_stb_i   => ext_snk_i.stb,
        ext_snk_ack_o   => ext_snk_o.ack,
        ext_snk_err_o   => ext_snk_o.err,
        ext_snk_stall_o => ext_snk_o.stall,
        
        ext_src_adr_o   => ext_src_o.adr, 
        ext_src_dat_o   => ext_src_o.dat,
        ext_src_sel_o   => ext_src_o.sel,
        ext_src_cyc_o   => ext_src_o.cyc,
        ext_src_stb_o   => ext_src_o.stb,
        ext_src_ack_i   => ext_src_i.ack,
        ext_src_err_i   => ext_src_i.err,
        ext_src_stall_i => ext_src_i.stall,
        class_core_i=> class_core_i
      );

  ext_src_o.we <= '1';
  ptp_src_o.we <= '1';
  ep_src_o.we <= '1';
  
end behaviour;
