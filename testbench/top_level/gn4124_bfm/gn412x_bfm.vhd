library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

use work.util.all;
use work.textutil.all;
use work.mem_model.all;



--==========================================================================--
--
-- MODULE << gn412x_bfm >>
--
-- Description : This module generates local bus signals from a text file
--
-- History: 
--
--==========================================================================--
--
-- To Do:
--      Implement rd_outstanding_in
--      Implement gpio for interrupts
--      Implement response_delay

entity GN412X_BFM is
  generic
    (
      STRING_MAX     : integer := 256;  -- Command string maximum length
      T_LCLK         : time    := 10 ns;         -- Local Bus Clock Period
      T_P2L_CLK_DLY  : time    := 2 ns;          -- Delay from LCLK to P2L_CLK
      INSTANCE_LABEL : string  := "GN412X_BFM";  -- Label string to be used as a prefix for messages from the model
      MODE_PRIMARY   : boolean := true  -- TRUE for BFM acting as GN412x, FALSE for BFM acting as the DUT
      );
  port
    (
      --=========================================================--
      -------------------------------------------------------------
      -- CMD_ROUTER Interface
      --
      CMD_INT            : in    t_cmd_array;
      CMD_REQ            : in    bit;
      CMD_ACK            : out   bit;
      CMD_CLOCK_EN       : in    boolean;
      CMD_RD_DATA: out std_ulogic_vector(31 downto 0);
      CMD_RD_DATA_VALID: out std_logic;
      --=========================================================--
      -------------------------------------------------------------
      -- GN412x Signal I/O
      -------------------------------------------------------------
      -- This is the reset input to the BFM
      --
      RSTINn             : in    std_logic;
      -------------------------------------------------------------
      -- Reset outputs to DUT
      --
      RSTOUT18n          : out   std_logic;
      RSTOUT33n          : out   std_logic;
      -------------------------------------------------------------
      ----------------- Local Bus Clock ---------------------------
      -------------------------------------------------------------  __ Direction for primary mode
      --                                                            / \
      LCLK, LCLKn        : inout std_logic;      -- Out
      -------------------------------------------------------------
      ----------------- Local-to-PCI Dataflow ---------------------
      -------------------------------------------------------------
      -- Transmitter Source Synchronous Clock.
      --
      L2P_CLKp, L2P_CLKn : inout std_logic;      -- In  
      -------------------------------------------------------------
      -- L2P DDR Link
      --
      L2P_DATA           : inout std_logic_vector(15 downto 0);  -- In  -- Parallel Transmit Data.
      L2P_DFRAME         : inout std_logic;  -- In  -- Transmit Data Frame.
      L2P_VALID          : inout std_logic;  -- In  -- Transmit Data Valid. 
      L2P_EDB            : inout std_logic;  -- In  -- End-of-Packet Bad Flag.
      -------------------------------------------------------------
      -- L2P SDR Controls
      --
      L_WR_RDY           : inout std_logic_vector(1 downto 0);  -- Out -- Local-to-PCIe Write.
      P_RD_D_RDY         : inout std_logic_vector(1 downto 0);  -- Out -- PCIe-to-Local Read Response Data Ready.
      L2P_RDY            : inout std_logic;  -- Out -- Tx Buffer Full Flag.
      TX_ERROR           : inout std_logic;  -- Out -- Transmit Error.
      -------------------------------------------------------------
      ----------------- PCIe-to-Local Dataflow ---------------------
      -------------------------------------------------------------
      -- Transmitter Source Synchronous Clock.
      --
      P2L_CLKp, P2L_CLKn : inout std_logic;  -- Out -- P2L Source Synchronous Clock.
      -------------------------------------------------------------
      -- P2L DDR Link
      --
      P2L_DATA           : inout std_logic_vector(15 downto 0);  -- Out -- Parallel Receive Data.
      P2L_DFRAME         : inout std_logic;  -- Out -- Receive Frame.
      P2L_VALID          : inout std_logic;  -- Out -- Receive Data Valid.
      -------------------------------------------------------------
      -- P2L SDR Controls
      --
      P2L_RDY            : inout std_logic;  -- In  -- Rx Buffer Full Flag.
      P_WR_REQ           : inout std_logic_vector(1 downto 0);  -- Out -- PCIe Write Request.
      P_WR_RDY           : inout std_logic_vector(1 downto 0);  -- In  -- PCIe Write Ready.
      RX_ERROR           : inout std_logic;  -- In  -- Receive Error.
      VC_RDY             : inout std_logic_vector(1 downto 0);  -- Out -- Virtual Channel Ready Status.
      -------------------------------------------------------------
      -- GPIO signals
      --
      GPIO               : inout std_logic_vector(15 downto 0)
      );
end GN412X_BFM;

architecture MODEL of GN412X_BFM is


  function f_cmd_to_string (s : t_cmd_array) return string is
    variable str : string(1 to STRING_MAX);
    variable tmp : integer;
  begin
    for i in 1 to 256 loop
      tmp := s(i);
      if(tmp < 0) then
        tmp := 0;
      elsif(tmp > 255) then
        tmp :=  255;
      end if;
      str(i) := character'val(tmp);
    end loop;  -- i
    return str;
  end f_cmd_to_string;


  signal CMD : string(1 to STRING_MAX);
--=========================================================================--
-- Routine for Data-Comparison
--=========================================================================--

  procedure read_cmp(L : inout line; PREFIX : in string; DATA, CMP_VAL, MASK : in std_ulogic_vector; ERR : out integer) is
    variable X   : std_ulogic_vector(DATA'range);
    variable STR : string(1 to 32);
  begin
    X                  := (others => 'X');
    X                  := (not MASK and X) or (MASK and CMP_VAL);
    STR(1 to X'length) := to_str(X);
    if((DATA and MASK) /= (CMP_VAL and MASK)) then
      write(L, PREFIX & string'("ERROR @ "));
      write(L, now);
      write(L, string'(" : "));
      write(L, LF);
      write(L, string'("       Expected      => "));
      write(L, STR(1 to X'length));

      write(L, string'(" (0x"));
      write_hex_vector(L, X);
      write(L, string'(")"));

      STR(1 to X'length) := to_str(DATA);
      write(L, LF);
      write(L, string'("       Actually Read => "));
      write(L, STR(1 to X'length));

      write(L, string'(" (0x"));
      write_hex_vector(L, DATA);
      write(L, string'(")"));

      ERR := 1;
    else
                      write(L, string'("Read OK @ "));
                      write(L, now);
                      write(L, string'(" : "));
                      write(L, LF);
                      write(L, string'("       Expected      => "));
                      write(L, STR(1 to X'length));
      ERR := 0;
    end if;
  end read_cmp;

--=========================================================================--
-- Routine to read a boolean from a string
--
-- it looks for true, false, on, off, 1, 0
--=========================================================================--
  procedure sget_boolean(S : in string; P : inout integer; X : out boolean) is
    variable char : character;
    variable q    : integer;
  begin
    if(S'length > P) then
      char := S(P);
      while(char = ' ') loop            -- Skip spaces
        P    := P + 1;
        char := S(P);
      end loop;
      if(S(P to P+4) = "true ") or (S(P to P+2) = "on ") or (S(P to P+1) = "1 ") then
        X := true;
      elsif(S(P to P+5) = "false ") or (S(P to P+3) = "off ") or (S(P to P+1) = "0 ") then
        X := false;
      else
        assert false report "ERROR: Couldn't read a boolean" severity error;
      end if;

      -- skip over token
      while((char /= ' ') and (P < S'length)) loop
        char := S(P);
        P    := P + 1;
      end loop;

    else
      assert false report "ERROR: Couldn't read a boolean" severity error;

    end if;

  end sget_boolean;

--=========================================================================--
-- Produce Another Random Number
--
-- RNDOUT is a number between MIN and MAX.  SR must be stored and fed back each call.
--=========================================================================--
  procedure next_random(SR : inout integer) is
    variable SRV : std_ulogic_vector(22 downto 0);
    variable SRI : integer;
  begin
    SRV := To_Vector(SR, SRV'length);
    SRV := SRV(21 downto 0) & (SRV(22) xor SRV(17));
    SRI := conv_integer(SRV);
    SR  := SRI;
  end next_random;

--      function next_random(SR : integer) return integer is
--              variable  SRV : std_ulogic_vector(22 downto 0);
--              variable  SRI : integer;
--      begin
--              SRV := To_Vector(SR, SRV'length);
--              SRV := SRV(21 downto 0) & (SRV(22) xor SRV(17));
--              SRI := conv_integer(SRV);
--              return(SRI);
--      end next_random;

--=========================================================================--
-- Produce Another Random Number
--
-- RNDOUT is a number between MIN and MAX.  SR must be stored and fed back each call.
--=========================================================================--
  procedure get_random(SR : inout integer; min : in integer; MAX : in integer; RNDOUT : out integer) is
    variable SRV : std_ulogic_vector(22 downto 0);
    variable SRI : integer;
  begin
    SRV    := To_Vector(SR, SRV'length);
    SRV    := SRV(21 downto 0) & (SRV(22) xor SRV(17));
    SRI    := conv_integer(SRV);
    RNDOUT := min + (SRI mod (MAX - min + 1));
    SR     := SRI;
  end get_random;

--=========================================================================--
-- Test a Random Number and test it
--
-- RNDOUT is a number between MIN and MAX.  SR must be stored and fed back each call.
--=========================================================================--
  function test_random(SR : integer; PROBABILITY : integer) return boolean is
  begin
    return((SR mod 101) < PROBABILITY);
  end test_random;


--=========================================================================--
-- Produce a Random Number between MIN and MAX
--=========================================================================--
  function range_random(SR : integer; min : integer; MAX : integer) return integer is
  begin
    return(min + (SR mod (MAX - min + 1)));
  end range_random;


--=========================================================================--
-- Signal Declarations
--=========================================================================--
-----------------------------------------------------------------------------
-- Global Settings
-----------------------------------------------------------------------------

  constant N_BARS    : integer := 2;
  constant N_RAM_MAX : integer := 30;  -- Maximim size of BFM RAM address = 2**N_RAM_MAX

  constant N_OUTBOUND_RD_OUTSTANDING : integer := 3;  -- Maximim number of outstanding reads the BFM will generate
  constant N_INBOUND_RD_OUTSTANDING  : integer := 3;  -- Maximim number of outstanding reads the BFM will accept
  constant N_COMPLETION_ID           : integer := 4;  -- Maximim number of completion IDs


  -------------------------------------------------------------------------
  -- Settable versions of the generic constants
  --
  signal T_LCLKi                 : time    := T_LCLK;  -- Local Bus Clock Period
  signal T_P2L_CLK_DLYi          : time    := T_P2L_CLK_DLY;  -- Delay from LCLK to P2L_CLK
  --
  -------------------------------------------------------------------------
  -- BFM Mode settings
  --
  signal PRIMARY                 : boolean;
  signal SECONDARY               : boolean;
  signal GENERATE_X              : boolean := false;
  signal EXPECT_ERROR            : boolean := false;
  signal OUTBOUND_RD_OUTSTANDING : integer := N_OUTBOUND_RD_OUTSTANDING;
  signal RESPONSE_DELAY          : integer := 0;
--      signal  BURST_LENGTH       : integer := 0;
--      signal  BURST_MODULO       : integer := 0;
  -- Acceptable values for TYPE are: "0000" for 32-bit memory read
  -- Acceptable values for TYPE are: "0001" for 64-bit memory read
  -- Acceptable values for TYPE are: "0010" for 32-bit memory write
  -- Acceptable values for TYPE are: "0011" for 64-bit memory write
  -- Acceptable values for TYPE are: "0100" for Completions without data
  -- Acceptable values for TYPE are: "0101" for Completions with data

  signal GPIOi : std_ulogic_vector(GPIO'range);
  signal GPIOo : std_ulogic_vector(GPIO'range);

-----------------------------------------------------------------------------
-- Top Level I/O signals PRIMARY MODE
-----------------------------------------------------------------------------
  -------------------------------------------------------------
  ----------------- Local Bus Clock ---------------------------
  -------------------------------------------------------------
  --
  signal LCLKo, LCLKno        : std_ulogic;
  -------------------------------------------------------------
  ----------------- Local-to-PCI Dataflow ---------------------
  -------------------------------------------------------------
  -- Transmitter Source Synchronous Clock.
  --
  signal L2P_CLKpi, L2P_CLKni : std_ulogic;
  signal L2P_CLKi_90          : std_ulogic;
  -------------------------------------------------------------
  -- L2P DDR Link
  --
  signal L2P_DATAi            : std_ulogic_vector(15 downto 0);  -- Parallel Transmit Data.
  signal L2P_DFRAMEi          : std_ulogic;  -- Transmit Data Frame.
  signal L2P_VALIDi           : std_ulogic;  -- Transmit Data Valid. 
  signal L2P_EDBi             : std_ulogic;  -- End-of-Packet Bad Flag.
  -------------------------------------------------------------
  -- L2P SDR Controls
  --
  signal L_WR_RDYo            : std_ulogic_vector(1 downto 0);  -- Local-to-PCIe Write.
  signal P_RD_D_RDYo          : std_ulogic_vector(1 downto 0);  -- PCIe-to-Local Read Response Data Ready.
  signal L2P_RDYo             : std_ulogic;  -- Tx Buffer Full Flag.
  signal TX_ERRORo            : std_ulogic;  -- Transmit Error.
  -------------------------------------------------------------
  ----------------- PCIe-to-Local Dataflow ---------------------
  -------------------------------------------------------------
  -- Transmitter Source Synchronous Clock.
  --
  signal P2L_CLKpo, P2L_CLKno : std_ulogic;  -- P2L Source Synchronous Clock.
  -------------------------------------------------------------
  -- P2L DDR Link
  --
  signal P2L_DATAo            : std_ulogic_vector(15 downto 0);  -- Parallel Receive Data.
  signal P2L_DFRAMEo          : std_ulogic;  -- Receive Frame.
  signal P2L_VALIDo           : std_ulogic;  -- Receive Data Valid.
  -------------------------------------------------------------
  -- P2L SDR Controls
  --
  signal P2L_RDYi             : std_ulogic;  -- Rx Buffer Full Flag.
  signal P_WR_REQo            : std_ulogic_vector(1 downto 0);  -- PCIe Write Request.
  signal P_WR_RDYi            : std_ulogic_vector(1 downto 0);  -- PCIe Write Ready.
  signal RX_ERRORi            : std_ulogic;  -- Receive Error.
  signal VC_RDYo              : std_ulogic_vector(1 downto 0);  -- Virtual Channel Ready Status.

-----------------------------------------------------------------------------
-- Top Level I/O signals SECONDARY MODE
-----------------------------------------------------------------------------
  -------------------------------------------------------------
  ----------------- Local Bus Clock ---------------------------
  -------------------------------------------------------------
  --
  signal LCLKi, LCLKni        : std_ulogic;
  -------------------------------------------------------------
  ----------------- Local-to-PCI Dataflow ---------------------
  -------------------------------------------------------------
  -- Transmitter Source Synchronous Clock.
  --
  signal L2P_CLKpo, L2P_CLKno : std_ulogic;
  -------------------------------------------------------------
  -- L2P DDR Link
  --
  signal L2P_DATAo            : std_ulogic_vector(15 downto 0);  -- Parallel Transmit Data.
  signal L2P_DFRAMEo          : std_ulogic;  -- Transmit Data Frame.
  signal L2P_VALIDo           : std_ulogic;  -- Transmit Data Valid. 
  signal L2P_EDBo             : std_ulogic;  -- End-of-Packet Bad Flag.
  -------------------------------------------------------------
  -- L2P SDR Controls
  --
  signal L_WR_RDYi            : std_ulogic_vector(1 downto 0);  -- Local-to-PCIe Write.
  signal P_RD_D_RDYi          : std_ulogic_vector(1 downto 0);  -- PCIe-to-Local Read Response Data Ready.
  signal L2P_RDYi             : std_ulogic;  -- Tx Buffer Full Flag.
  signal TX_ERRORi            : std_ulogic;  -- Transmit Error.
  -------------------------------------------------------------
  ----------------- PCIe-to-Local Dataflow ---------------------
  -------------------------------------------------------------
  -- Transmitter Source Synchronous Clock.
  --
  signal P2L_CLKpi, P2L_CLKni : std_ulogic;  -- P2L Source Synchronous Clock.
  -------------------------------------------------------------
  -- P2L DDR Link
  --
  signal P2L_DATAi            : std_ulogic_vector(15 downto 0);  -- Parallel Receive Data.
  signal P2L_DFRAMEi          : std_ulogic;  -- Receive Frame.
  signal P2L_VALIDi           : std_ulogic;  -- Receive Data Valid.
  -------------------------------------------------------------
  -- P2L SDR Controls
  --
  signal P2L_RDYo             : std_ulogic;  -- Rx Buffer Full Flag.
  signal P_WR_REQi            : std_ulogic_vector(1 downto 0);  -- PCIe Write Request.
  signal P_WR_RDYo            : std_ulogic_vector(1 downto 0);  -- PCIe Write Ready.
  signal RX_ERRORo            : std_ulogic;  -- Receive Error.
  signal VC_RDYi              : std_ulogic_vector(1 downto 0);  -- Virtual Channel Ready Status.

  -------------------------------------------------------------
  -- Used by the Inbound State Machine
  --
  signal ICLK        : std_ulogic;      -- Internal Inbound clock
  signal LCLK_PERIOD : time := T_LCLK;

-----------------------------------------------------------------------------
-- Internal Model Signals
-----------------------------------------------------------------------------
  type     integer_vector is array (natural range <>) of integer;
  type     boolean_vector is array (natural range <>) of boolean;
  -------------------------------------------------------------------------
  -- Largest Cache line size supported
  --
  constant N_MAX_LENGTH : integer    := 12;
  constant MAX_LENGTH   : integer    := 2**N_MAX_LENGTH;
  -------------------------------------------------------------------------
  -- An array of MAX_CACHE * 32 bits
  --
  type     ARRAY_OF_32 is array (natural range <>) of std_ulogic_vector(31 downto 0);
  type     ARRAY_OF_2 is array (natural range <>) of std_ulogic_vector(1 downto 0);
--      signal  CACHE_BUFFER       : ARRAY_OF_32(MAX_CACHE-1 downto 0);
  -------------------------------------------------------------------------
  --
  signal   CLK0o        : std_ulogic;
  signal   CLK90o       : std_ulogic;
  signal   CLK          : std_ulogic;   -- This one is used by the main loop
  signal   RSTOUTo      : std_ulogic := '0';


  constant T_HOLD_OUT  : time := 1 ns;
  signal   T_HOLD_OUTi : time := T_HOLD_OUT;


  constant WR_FLUSH_POST      : integer := 0;
  constant WR_POST            : integer := 1;
  constant WR_POST_FLUSH      : integer := 2;
  constant WR_FLUSH_POST_LAST : integer := 3;



  constant RD_POST          : integer := 0;
  constant RD_POST_IPR      : integer := 1;
  constant RD_IPR_POST      : integer := 2;
  constant RD_IPR_POST_LAST : integer := 3;



  constant MAX_DW_PACKET_SIZE : integer := 1024;  -- Maximum packet size in units of DW (32 bits)

  type INT_VECTOR is array (natural range <>) of integer;
  type DATA32 is array (natural range <>) of std_ulogic_vector(31 downto 0);

  --=======================================================================
  --==
  --== Data Structures for INBOUND Read Requests that generate Outbound Completions
  --==
  --=======================================================================
  -------------------------------------------------------------------------
  -- Data Structure used to store inbound read requests
  --  note: there are N_COMPLETION_ID size for this data structure rather 
  --        than N_INBOUND_RD_OUTSTANDING.  Only up to N_INBOUND_RD_OUTSTANDING can be active at a time
  type INBOUND_READ_REQUEST_TYPE is
  record
    ADDRESS : std_ulogic_vector(63 downto 0);  -- BAR base address
    BAR_HIT : std_ulogic_vector(1 downto 0);   -- This is for secondary mode
    TC      : std_ulogic_vector(2 downto 0);   -- Trafic class
    V       : std_ulogic;               -- Virtual channel
    LENGTH  : integer;                  -- Transfer length in DW
    STATE   : boolean;  -- Used to indicate if the entry is in use or not
  end record;
  type   INBOUND_READ_REQUEST_ARRAY_TYPE is array (N_COMPLETION_ID-1 downto 0) of INBOUND_READ_REQUEST_TYPE;
  signal INBOUND_READ_REQUEST_ARRAY : INBOUND_READ_REQUEST_ARRAY_TYPE;

  -------------------------------------------------------------------------
  -- This is the state of the CID for inbound read requests. 
  -- Used to signal back to the inbound process which completions have been serviced
  -- When INBOUND_READ_REQUEST_ARRAY(CID).STATE = INBOUND_READ_REQUEST_CPL_STATE(CID) then the CID may be re-used
  signal INBOUND_READ_REQUEST_CPL_STATE : boolean_vector(N_COMPLETION_ID-1 downto 0);

  signal CURRENT_INBOUND_RD_IPR : integer;  -- Indicates how many inbound reads are currently outstanding


  --=======================================================================
  --==
  --== Data Structures for OUTBOUND Read Requests that result in Inbound Completions
  --==
  --=======================================================================
  -- This data structure is written by the outbound process and 
  -- read by the inbound process to verify completion packets
  type RD_BUFFER_TYPE is
  record
    ADDRESS : std_ulogic_vector(63 downto 0);
    BAR_HIT : std_ulogic_vector(1 downto 0);
    FBE     : std_ulogic_vector(3 downto 0);
    LBE     : std_ulogic_vector(3 downto 0);
    STATE   : boolean;
    DATA    : DATA32(MAX_DW_PACKET_SIZE-1 downto 0);
    MASK    : DATA32(MAX_DW_PACKET_SIZE-1 downto 0);
  end record;

  type   RD_BUFFER_ARRAY_TYPE is array (N_COMPLETION_ID-1 downto 0) of RD_BUFFER_TYPE;
  signal RD_BUFFER : RD_BUFFER_ARRAY_TYPE;

  -------------------------------------------------------------------------
  -- Used to signal back to the inbound process which completions have been serviced
  -- An entry is in use when OUTBOUND_READ_COMPLETION_STATE(i) /= INBOUND_READ_REQUEST_ARRAY(i).STATE
  signal OUTBOUND_READ_REQUEST_CPL_STATE : boolean_vector(N_COMPLETION_ID-1 downto 0);

  signal CURRENT_OUTBOUND_RD_IPR : integer;  -- Indicates how many inbound reads are currently outstanding


  --=======================================================================
  --==
  --== Data Structures for OUTBOUND Write Requests
  --==
  --=======================================================================

  -- There is only a single write buffer and it lives as a variable in the outbound process
  -- since the inbound process doesn't need to know anything about it
  type WR_BUFFER_TYPE is
  record
    ADDRESS : std_ulogic_vector(63 downto 0);
    BE      : std_ulogic_vector(3 downto 0);
    DATA    : std_ulogic_vector(31 downto 0);
  end record;


  type WR_BUFFER_ARRAY_TYPE is array (MAX_DW_PACKET_SIZE-1 downto 0) of WR_BUFFER_TYPE;

--      signal WR_BUFFER : WR_BUFFER_ARRAY_TYPE;

  signal RANDOM_NUMBER : integer := 0;


  -------------------------------------------------------------------------
  -- Data Structure used to store info about BARs that generate outbound
  -- read/write packets
  --
  type BAR_ATTRIBUTE_TYPE is
  record
    BASE : std_ulogic_vector(63 downto 0);  -- BAR base address
    MASK : std_ulogic_vector(31 downto 0);  -- BAR mask
    VC   : std_ulogic;                      -- Virtual Channel
    TC   : std_ulogic_vector(2 downto 0);   -- Trafic class
    S    : std_ulogic;                      -- No Snoop Bit
  end record;
  type   BAR_ATTRIBUTE_ARRAY_TYPE is array (1 downto 0) of BAR_ATTRIBUTE_TYPE;
  signal BAR_ATTRIBUTE_ARRAY : BAR_ATTRIBUTE_ARRAY_TYPE;

  -------------------------------------------------------------------------
  -- Data Structure used to store info about memory BARs for internal BFM
  --
  type BFM_BAR_ATTRIBUTE_TYPE is
  record
    BASE : std_ulogic_vector(63 downto 0);  -- BAR base address
    MASK : std_ulogic_vector(31 downto 0);  -- BAR mask
  end record;
  type   BFM_BAR_ATTRIBUTE_ARRAY_TYPE is array (1 downto 0) of BFM_BAR_ATTRIBUTE_TYPE;
  signal BFM_BAR_ATTRIBUTE_ARRAY : BFM_BAR_ATTRIBUTE_ARRAY_TYPE;





--      type VC_SIZE_VECTOR is array (natural range <>) of std_ulogic_vector(1 downto 0);


-----------------------------------------------------------------------------
-- Signals used by the outbound and inbound process to read the BFM RAM
-----------------------------------------------------------------------------
  signal RAM_REQ0, RAM_REQ1         : boolean := false;  -- Request
  signal RAM_WR0, RAM_WR1           : boolean;           -- Write mode
  signal RAM_ACK0, RAM_ACK1         : boolean;           -- Acknowledge
  signal RAM_ADDR0, RAM_ADDR1       : std_ulogic_vector(N_RAM_MAX-1 downto 0);
  signal RAM_WR_DATA0, RAM_WR_DATA1 : std_ulogic_vector(7 downto 0);
  signal RAM_RD_DATA0, RAM_RD_DATA1 : std_ulogic_vector(7 downto 0);

-----------------------------------------------------------------------------
-- Signals Related to the InBound Process
-----------------------------------------------------------------------------
  signal IN_DATA                  : std_ulogic_vector(31 downto 0);
  signal IN_DATA_LOW              : std_ulogic_vector(15 downto 0);
  signal IN_DFRAME                : std_ulogic;
  signal IN_VALID                 : std_ulogic;
  signal Q_IN_DFRAME              : std_ulogic;
-----------------------------------------------------------------------------
-- Signals Related to the OutBound Process
-----------------------------------------------------------------------------
  signal Q_OUT_DATA, OUT_DATA     : std_ulogic_vector(31 downto 0);
  signal Q_OUT_DFRAME, OUT_DFRAME : std_ulogic;
  signal Q_OUT_VALID, OUT_VALID   : std_ulogic;
  signal OUT_WR_REQ               : std_ulogic_vector(P_WR_REQ'range);
  signal OUT_WR_RDY               : std_ulogic_vector(P_WR_RDY'range);
  signal Q_OUT_WR_RDY             : std_ulogic_vector(P_WR_RDY'range);



--#########################################################################--
--########################## Start of Code ################################--
--#########################################################################--

begin
CMD <= f_cmd_to_string(CMD_INT);
--=========================================================================--
-- Reset Outputs
--=========================================================================--
  RSTOUT18n <= not RSTOUTo when PRIMARY else 'Z';
  RSTOUT33n <= not RSTOUTo when PRIMARY else 'Z';

--=========================================================================--
-- Generate the Internal LCLK
--=========================================================================--
  process
  begin
    CLK0o  <= '0';
    CLK90o <= '0';
    loop
      if (CMD_CLOCK_EN and PRIMARY) then
        CLK0o <= '1';
        CLK0o <= transport '0' after (T_LCLKi/2);

        CLK90o <= transport '1' after (T_LCLKi/4);
        CLK90o <= transport '0' after ((T_LCLKi*3)/4);

        wait for T_LCLKi;
      else
        wait until(CMD_CLOCK_EN'event and CMD_CLOCK_EN and PRIMARY);
      end if;
    end loop;
  end process;

  PRIMARY   <= MODE_PRIMARY;
  SECONDARY <= not MODE_PRIMARY;

--=========================================================================--
-- Top Level I/O signals with timing information
--=========================================================================--

--*************************************************************************--
-- Top Level I/O signals PRIMARY MODE
--*************************************************************************--
  -------------------------------------------------------------
  ----------------- Local Bus Clock ---------------------------
  -------------------------------------------------------------
  --
  LCLKo       <= CLK0o;
  LCLKno      <= not CLK0o;
  LCLK        <= LCLKo                                                       when PRIMARY else 'Z';
  LCLKn       <= LCLKno                                                      when PRIMARY else 'Z';
  -------------------------------------------------------------
  ----------------- Local-to-PCI Dataflow ---------------------
  -----------------  (GN412x is an input) ---------------------
  -------------------------------------------------------------
  -- Receiver Source Synchronous Clock.
  --
  L2P_CLKpi   <= To_X01(L2P_CLKp);
  L2P_CLKni   <= To_X01(L2P_CLKn);
  -------------------------------------------------------------
  -- L2P DDR Link
  --
  L2P_DATAi   <= To_X01(To_StdULogicVector(L2P_DATA));
  L2P_DFRAMEi <= To_X01(L2P_DFRAME);
  L2P_VALIDi  <= To_X01(L2P_VALID);
  L2P_EDBi    <= To_X01(L2P_EDB);
  -------------------------------------------------------------
  -- L2P SDR Controls
  --
  L_WR_RDY    <= To_StdLogicVector(L_WR_RDYo)           after T_HOLD_OUTi    when PRIMARY else (others => 'Z');
  P_RD_D_RDY  <= To_StdLogicVector(P_RD_D_RDYo)         after T_HOLD_OUTi    when PRIMARY else (others => 'Z');
  L2P_RDY     <= L2P_RDYo                               after T_HOLD_OUTi    when PRIMARY else 'Z';
  TX_ERROR    <= TX_ERRORo                              after T_HOLD_OUTi    when PRIMARY else 'Z';
  -------------------------------------------------------------
  ----------------- PCIe-to-Local Dataflow ---------------------
  ----------------- (GN412x is an output) ---------------------
  -------------------------------------------------------------
  -- Transmitter Source Synchronous Clock.
  --
  P2L_CLKp    <= P2L_CLKpo                                                   when PRIMARY else 'Z';
  P2L_CLKn    <= P2L_CLKno                                                   when PRIMARY else 'Z';
  P2L_CLKpo   <= transport not CLK0o                    after T_P2L_CLK_DLYi when PRIMARY else 'Z';  -- Note that P2L_CLK is effectively inverted
  P2L_CLKno   <= transport CLK0o                        after T_P2L_CLK_DLYi when PRIMARY else 'Z';
  -------------------------------------------------------------
  -- P2L DDR Link
  --
  P2L_DATA    <= transport To_StdLogicVector(P2L_DATAo) after T_P2L_CLK_DLYi when PRIMARY else (others => 'Z');
  P2L_DFRAME  <= transport P2L_DFRAMEo                  after T_P2L_CLK_DLYi when PRIMARY else 'Z';
  P2L_VALID   <= transport P2L_VALIDo                   after T_P2L_CLK_DLYi when PRIMARY else 'Z';
  -------------------------------------------------------------
  -- P2L SDR Controls
  --
  P2L_RDYi    <= To_X01(P2L_RDY);
  P_WR_REQ    <= To_StdLogicVector(P_WR_REQo)                                when PRIMARY else (others => 'Z');
  P_WR_RDYi   <= To_X01(To_StdULogicVector(P_WR_RDY));
  RX_ERRORi   <= To_X01(RX_ERROR);
  VC_RDY      <= To_StdLogicVector(VC_RDYo)                                  when PRIMARY else (others => 'Z');

  CLK <= CLK0o when PRIMARY else LCLK;

-----------------------------------------------------------------------------
-- PRIMARY MODE Internal Signals
-----------------------------------------------------------------------------
  process
  begin
    wait until(CLK90o'event and (CLK90o = '1'));
    P2L_DATAo   <= Q_OUT_DATA(15 downto 0);
    P2L_DFRAMEo <= Q_OUT_DFRAME;
    P2L_VALIDo  <= Q_OUT_VALID;
    P_WR_REQo   <= OUT_WR_REQ;
    if(RSTOUTo = '1') then
      VC_RDYo <= (others => '0');
    else
      VC_RDYo <= (others => '1');
    end if;

    wait until(CLK90o'event and (CLK90o = '0'));
    P2L_DATAo <= Q_OUT_DATA(31 downto 16);
  end process;


  L_WR_RDYo   <= (others => '1');
  P_RD_D_RDYo <= (others => '1');
  L2P_RDYo    <= '1';
  TX_ERRORo   <= '0';

  GPIO  <= To_StdLogicVector(GPIOo);
  GPIOo <= (others => 'Z');
  GPIOi <= To_StdULogicVector(GPIO);


--*************************************************************************--
-- Top Level I/O signals SECONDARY MODE
--*************************************************************************--
  -------------------------------------------------------------
  ----------------- Local-to-PCI Dataflow ---------------------
  ----------------- (GN412x is an output) ---------------------
  -------------------------------------------------------------
  -- Transmitter Source Synchronous Clock.
  --
  L2P_CLKp    <= transport L2P_CLKpo                    after T_P2L_CLK_DLYi when SECONDARY else 'Z';
  L2P_CLKn    <= transport L2P_CLKno                    after T_P2L_CLK_DLYi when SECONDARY else 'Z';
  -------------------------------------------------------------
  -- L2P DDR Link
  --
  L2P_DATA    <= transport To_StdLogicVector(L2P_DATAo) after T_P2L_CLK_DLYi when SECONDARY else (others => 'Z');
  L2P_DFRAME  <= transport L2P_DFRAMEo                  after T_P2L_CLK_DLYi when SECONDARY else 'Z';
  L2P_VALID   <= transport L2P_VALIDo                   after T_P2L_CLK_DLYi when SECONDARY else 'Z';
  L2P_EDB     <= transport L2P_EDBo                     after T_P2L_CLK_DLYi when SECONDARY else 'Z';
  -------------------------------------------------------------
  -- L2P SDR Controls
  --
  L_WR_RDYi   <= To_X01(To_StdULogicVector(L_WR_RDY));
  P_RD_D_RDYi <= To_X01(To_StdULogicVector(P_RD_D_RDY));
  L2P_RDYi    <= To_X01(L2P_RDY);
  TX_ERRORi   <= To_X01(TX_ERROR);

  -------------------------------------------------------------
  ----------------- PCIe-to-Local Dataflow ---------------------
  -----------------  (GN412x is an input) ---------------------
  -------------------------------------------------------------
  -- Receiver Source Synchronous Clock.
  --
  P2L_CLKpi   <= To_X01(P2L_CLKp);
  P2L_CLKni   <= To_X01(P2L_CLKn);
  -------------------------------------------------------------
  -- P2L DDR Link
  --
  P2L_DATAi   <= To_X01(To_StdULogicVector(P2L_DATA));
  P2L_DFRAMEi <= To_X01(P2L_DFRAME);
  P2L_VALIDi  <= To_X01(P2L_VALID);
  -------------------------------------------------------------
  -- P2L SDR Controls
  --
  P2L_RDY     <= 'Z'             when PRIMARY else P2L_RDYo                                 after T_HOLD_OUTi;
  P_WR_REQi   <= To_X01(To_StdULogicVector(P_WR_REQ));  -- INPUT
  P_WR_RDY    <= (others => 'Z') when PRIMARY else To_StdLogicVector(P_WR_RDYo) after T_HOLD_OUTi;  -- OUTPUT
  VC_RDYi     <= To_X01(To_StdULogicVector(VC_RDY));    -- INPUT
  RX_ERROR    <= 'Z'             when PRIMARY else RX_ERRORo                                after T_HOLD_OUTi;  -- OUTPUT

-----------------------------------------------------------------------------
-- SECONDARY MODE Internal Signals
-----------------------------------------------------------------------------
  L2P_CLKpo <= CLK;
  L2P_CLKno <= not CLK;

  L2P_DATAo   <= OUT_DATA(15 downto 0) when (L2P_CLKpo = '1') else OUT_DATA(31 downto 16);
  L2P_DFRAMEo <= OUT_DFRAME;
  L2P_VALIDo  <= OUT_VALID;
  L2P_EDBo    <= '0';                   -- CHANGE: Add L2P_EDB functionality




--*************************************************************************--
-- Signals Related to the OutBound Process
--*************************************************************************--
  process
  begin
    wait until(CLK0o'event and (CLK0o = '1'));
    Q_OUT_DATA   <= OUT_DATA;
    Q_OUT_DFRAME <= OUT_DFRAME;
    Q_OUT_VALID  <= OUT_VALID;
  end process;

  process
  begin
    wait until(CLK'event and (CLK = '1'));
    if(PRIMARY) then
      Q_OUT_WR_RDY <= P_WR_RDYi;
    else
      Q_OUT_WR_RDY <= L_WR_RDYi;
    end if;
    OUT_WR_RDY <= Q_OUT_WR_RDY;
  end process;



--*************************************************************************--
-- Signals Related to the InBound Process
--*************************************************************************--
  ICLK <= not L2P_CLKpi after T_LCLKi/4 when PRIMARY else not P2L_CLKpi;

--      process -- Determine LCLK period for when in secondary mode
--              variable DELTA : time;
--      begin
--              wait until(LCLKi'event and (LCLKi = '1'));
--              DELTA := NOW;
--              wait until(LCLKi'event and (LCLKi = '1'));
--              DELTA := NOW - DELTA;
--              LCLK_PERIOD <= DELTA;
--      end process;

  process
  begin
    wait until(ICLK'event and (ICLK = '0'));
    if(PRIMARY) then
      IN_DATA_LOW <= L2P_DATAi;
    else
      IN_DATA_LOW <= P2L_DATAi;
    end if;
    wait until(ICLK'event and (ICLK = '1'));
    if(IN_VALID = '1') then
      Q_IN_DFRAME <= IN_DFRAME;
    end if;
  end process;

  IN_DATA(31 downto 16) <= L2P_DATAi when PRIMARY else P2L_DATAi;
  IN_DATA(15 downto 0)  <= IN_DATA_LOW;

  IN_DFRAME <= L2P_DFRAMEi when PRIMARY else P2L_DFRAMEi;
  IN_VALID  <= L2P_VALIDi  when PRIMARY else P2L_VALIDi;

-- CHANGE: add ability to de-assert RDY

  P_WR_RDYo <= P_WR_REQi;

--*************************************************************************--
-- Signals involving interaction between InBound/Outbound Processes
--*************************************************************************--
-----------------------------------------------------------------------------
-- Keep track of how many inbound reads requests are in progress
-----------------------------------------------------------------------------
  process(INBOUND_READ_REQUEST_ARRAY, INBOUND_READ_REQUEST_CPL_STATE)
    variable COUNT : integer;
  begin
    COUNT := 0;
    for I in 0 to N_COMPLETION_ID-1 loop
      if(INBOUND_READ_REQUEST_ARRAY(I).STATE /= INBOUND_READ_REQUEST_CPL_STATE(I)) then
        COUNT := COUNT + 1;
      end if;
    end loop;
    CURRENT_INBOUND_RD_IPR <= COUNT;
  end process;


-----------------------------------------------------------------------------
-- Keep track of how many outbound reads requests are in progress
-----------------------------------------------------------------------------
  process(RD_BUFFER, OUTBOUND_READ_REQUEST_CPL_STATE)
    variable COUNT : integer;
  begin
    COUNT := 0;
    for I in 0 to N_COMPLETION_ID-1 loop
      if(RD_BUFFER(I).STATE /= OUTBOUND_READ_REQUEST_CPL_STATE(I)) then
        COUNT := COUNT + 1;
      end if;
    end loop;
    CURRENT_OUTBOUND_RD_IPR <= COUNT;
  end process;


--#########################################################################--
--
-- OutBound State Machine 
--
-- (Handles TX from the BFM: P2L for Primary and L2P for secondary)
--
--#########################################################################--
  process
    --file      OUT_FILE : text is out "STD_OUTPUT";
    file OUT_FILE           : text open write_mode is "NullFile";
    variable OUTPUT_LINE    : line;
    variable ERR_CNT        : integer;
    variable L_CMD          : string(1 to 80);
    variable TMP_STR        : string(1 to 80);
    variable QCMD           : string(CMD'range);
    variable L_NUM          : integer;
    variable L_ADDR         : std_ulogic_vector(63 downto 0);
    variable L_BE           : std_ulogic_vector(3 downto 0);
    variable L_DATA         : std_ulogic_vector(31 downto 0);
    variable MODULO_MASK    : std_ulogic_vector(31 downto 2);
    variable L_MASK         : std_ulogic_vector(31 downto 0);
    variable AMASK_TMP      : std_ulogic_vector(31 downto 0);  -- Will become 63:0
    variable CHAR_PTR       : integer;
    variable I_TMP          : integer;
    variable I_TMP2         : integer;
    variable I_TMP3         : integer;
    variable I_TMP4         : integer;
    variable I_TMP5         : integer;
    variable vERR           : integer;
    variable B_TMP          : boolean;
    variable START          : time;
    variable N_BURST_MODULO : INT_VECTOR(0 to N_BARS-1);
    variable BURST_LENGTH   : INT_VECTOR(0 to N_BARS-1);
    variable CURRENT_BAR    : integer;
    variable CURRENT_VC     : integer;
    variable IWAIT_RANDOM   : integer;
    variable IWAIT_RANDOM_N : integer;
    variable RNDNUM         : integer;
    variable BAR_HIT        : boolean_vector(1 downto 0);
    variable BFM_BAR_HIT    : boolean_vector(1 downto 0);
    variable DATA_TMP8      : std_ulogic_vector(7 downto 0);
    variable DATA_TMP32     : std_ulogic_vector(31 downto 0);
    variable A_TMP          : std_ulogic_vector(L_ADDR'range);
    variable RD_ADDRESS     : std_ulogic_vector(L_ADDR'range);

    --
    -- Write Buffer Management
    variable WR_BUFFER_COUNT : integer;
    variable WR_BUFFER_PTR   : integer;
    variable WR_CASE         : integer;
    variable RW_SEQUENTIAL   : boolean;
    variable WR_BUFFER       : WR_BUFFER_ARRAY_TYPE;
    variable WR_TYPE         : std_ulogic_vector(3 downto 0);

    --
    -- Read Request Buffer Management
--              variable  RD_BUFFER_COUNT : integer;
    variable RD_BUFFER_PTR : integer;
    variable CURRENT_CID   : integer;   -- Current read completion ID
    variable NEXT_CID      : integer;   -- Next read completion ID
    variable RD_CASE       : integer;
    variable RD_TYPE       : std_ulogic_vector(3 downto 0);

    variable RW_BLAST    : boolean;
    --
    -- OUT variables
    variable OUT_NOSNOOP : std_ulogic;
--              variable  OUT_VC          : std_ulogic;
    variable OUT_TC      : std_ulogic_vector(2 downto 0);

    --
    -- Read Completion Management
    variable CPL_MODULO  : integer;     -- Modulo boundary for 
    variable CPL_ORDER   : boolean;
    variable CPL_LENGTH  : integer;
    variable CPL_LAST    : std_ulogic;
    variable CPL_CID     : integer;
    variable CPL_POP_PTR : INT_VECTOR(N_COMPLETION_ID-1 downto 0);  -- DW pointer for each CID

    variable IWAIT_RND_SEED : integer;


    variable DEBUG : integer;


  begin

    -- Signal Initialization
    OUT_DATA   <= (others => 'Z');
    OUT_DFRAME <= '0';
    OUT_VALID  <= '0';
    OUT_WR_REQ <= (others => '0');


    -- Variable Initialization
    ERR_CNT                        := 0;
    WR_BUFFER_COUNT                := 0;
    WR_BUFFER_PTR                  := 0;
    RD_BUFFER_PTR                  := 0;
    CURRENT_BAR                    := 0;
    BURST_LENGTH                   := (others => 512);
    NEXT_CID                       := N_COMPLETION_ID - 1;
    CURRENT_CID                    := N_COMPLETION_ID - 1;
    DEBUG                          := 0;
    CPL_POP_PTR                    := (others => 0);
    CPL_CID                        := 0;
    CPL_MODULO                     := 64;
    INBOUND_READ_REQUEST_CPL_STATE <= (others => false);

    loop
      CMD_ACK <= '0';

      wait on CMD_REQ, CLK;

      --============================================================--
      -- Load in a command
      --============================================================--
      if(CMD_REQ = '1') then

        START := now;
--                              if (CMD_REQ /= '1') then
--                                      wait until (CMD_REQ'EVENT and CMD_REQ = '1');
--                              end if;

        QCMD    := CMD;
        CMD_ACK <= '1';
        if (CMD_REQ /= '0') then
          wait until (CMD_REQ'event and CMD_REQ = '0');
        end if;

        if (START /= now) then
          wait until(CLK'event and (CLK = '1'));
        end if;


        CHAR_PTR := 1;                  -- Point to beginning of line

--        report "Q_CMD " & QCMD;
        
        SGET_TOKEN(QCMD, CHAR_PTR, L_CMD);  -- Strip off line number

--        report "Line " & L_CMD;

        SGET_TOKEN(QCMD, CHAR_PTR, L_CMD);  -- Load the command

--        report "Command " & L_CMD;

        
        if(not MODE_PRIMARY) then
          DEBUG := DEBUG + 1;
        end if;

        --============================================================--
        -- Command Decode --
        --============================================================--

        --*-------------------------------------------------------------
        --* wr ADDR BE DATA
        --*
        --*   - handles the wr/wrb command
        --*
        --* Write Cases:
        --*
        --*    Single Buffer   Write    Action
        --*    Write  Empty  Burstable  Taken
        --*
        --*      N      N        N        FLUSH, POST      : WR_CASE=WR_FLUSH_POST
        --*      N      N        Y        POST             : WR_CASE=WR_POST
        --*      N      Y        -        POST             : WR_CASE=WR_POST
        --*      Y      N        N        FLUSH, POST, LAST: WR_CASE=WR_FLUSH_POST_LAST
        --*      Y      N        Y        POST, FLUSH      : WR_CASE=WR_POST_FLUSH
        --*      Y      Y        -        POST, FLUSH      : WR_CASE=WR_POST_FLUSH
        --*
        --*
        --*-------------------------------------------------------------

--        report "L_CMD" & L_CMD;
        
        if (L_CMD(1 to 2) = "wr") or (L_CMD(1 to 3) = "rd ") or (L_CMD(1 to 3) = "rdb") then
          SGET_VECTOR_64(QCMD, CHAR_PTR, L_ADDR);
          SGET_VECTOR(QCMD, CHAR_PTR, L_BE);
          SGET_VECTOR(QCMD, CHAR_PTR, L_DATA);
          if(L_CMD(1 to 2) = "rd") then
            while(QCMD(CHAR_PTR) = ' ') loop  -- Skip spaces
              CHAR_PTR := CHAR_PTR + 1;
              if(CHAR_PTR > QCMD'length) then
                exit;
              end if;
            end loop;
            if(CHAR_PTR > QCMD'length) then
              L_MASK := (others => '0');
            else
              SGET_VECTOR(QCMD, CHAR_PTR, L_MASK);
            end if;
          end if;

          BAR_HIT(0) := (((L_ADDR(31 downto 0) and BAR_ATTRIBUTE_ARRAY(0).MASK) = BAR_ATTRIBUTE_ARRAY(0).BASE(31 downto 0))
                             and (L_ADDR(63 downto 32) = BAR_ATTRIBUTE_ARRAY(0).BASE(63 downto 32)));
          BAR_HIT(1) := (((L_ADDR(31 downto 0) and BAR_ATTRIBUTE_ARRAY(1).MASK) = BAR_ATTRIBUTE_ARRAY(1).BASE(31 downto 0))
                             and (L_ADDR(63 downto 32) = BAR_ATTRIBUTE_ARRAY(1).BASE(63 downto 32)));

          BFM_BAR_HIT(0) := (((L_ADDR(31 downto 0) and BFM_BAR_ATTRIBUTE_ARRAY(0).MASK) = BFM_BAR_ATTRIBUTE_ARRAY(0).BASE(31 downto 0))
                             and (L_ADDR(63 downto 32) = BFM_BAR_ATTRIBUTE_ARRAY(0).BASE(63 downto 32)));
          BFM_BAR_HIT(1) := (((L_ADDR(31 downto 0) and BFM_BAR_ATTRIBUTE_ARRAY(1).MASK) = BFM_BAR_ATTRIBUTE_ARRAY(1).BASE(31 downto 0))
                             and (L_ADDR(63 downto 32) = BFM_BAR_ATTRIBUTE_ARRAY(1).BASE(63 downto 32)));

          if(BAR_HIT(1)) then
            CURRENT_BAR := 1;
          else
            CURRENT_BAR := 0;
          end if;

          CURRENT_VC := to_int(BAR_ATTRIBUTE_ARRAY(CURRENT_BAR).VC);


          if(BAR_HIT(0) or BAR_HIT(1)) then  -- Address hits one of the external BARs

                                        -- Burst-last

            MODULO_MASK := to_vector((((2**N_BURST_MODULO(CURRENT_BAR))/4) - 1), MODULO_MASK'length);

            RW_BLAST := (L_BE /= "1111")
                        or ((WR_BUFFER_COUNT-1) = (BURST_LENGTH(CURRENT_VC)/4))
                        or ((MODULO_MASK and L_ADDR(31 downto 2)) = MODULO_MASK)
                        or (L_CMD(3) /= 'b');



                                            --========================================================--
                                            -- Do Write Cycles 
                                            --========================================================--
            if (L_CMD(1 to 2) = "wr") then  -- Do Write Cycles

              if(WR_BUFFER_COUNT > 0) then
                I_TMP := To_Int(L_ADDR(31 downto 2));

-- CHANGE to do proper 64 bit address

                RW_SEQUENTIAL := ((To_Int(WR_BUFFER(WR_BUFFER_PTR-1).ADDRESS(31 downto 2)) + 1) = I_TMP);

              else
                RW_SEQUENTIAL := false;
              end if;

              if (L_CMD(3) = 'b') then  -- See if it will go into the write cache
                if (WR_BUFFER_COUNT = 0) then    -- Buffer Empty
                  WR_CASE := WR_POST;
                else                    -- Buffer Not Empty
                  if RW_SEQUENTIAL and (WR_CASE /= WR_FLUSH_POST_LAST) then
                    if RW_BLAST then
                      WR_CASE := WR_POST_FLUSH;  -- POST, FLUSH
                    else
                      WR_CASE := WR_POST;        -- POST
                    end if;
                  else
                    WR_CASE := WR_FLUSH_POST;    -- FLUSH, POST
                  end if;
                end if;
              else                      -- Single Write
                if(WR_BUFFER_COUNT = 0) then
                  WR_CASE := WR_POST_FLUSH;
                elsif RW_SEQUENTIAL then  -- Buffer not Empty and burstable
                  WR_CASE := WR_POST_FLUSH;
                else
                  WR_CASE := WR_FLUSH_POST_LAST;
                end if;
              end if;

                                                                       --========================================================--
                                                                       -- Actualy generate the bus cycles
                                                                       --========================================================--
                                                                       ------------------------------------------------------------
                                                                       -- Post to the buffer
                                                                       ------------------------------------------------------------
              if(WR_CASE = WR_POST) or (WR_CASE = WR_POST_FLUSH) then  -- POST
                WR_BUFFER(WR_BUFFER_PTR).ADDRESS := L_ADDR;
                WR_BUFFER(WR_BUFFER_PTR).BE      := L_BE;
                WR_BUFFER(WR_BUFFER_PTR).DATA    := L_DATA;
                WR_BUFFER_COUNT                  := WR_BUFFER_COUNT + 1;
                WR_BUFFER_PTR                    := WR_BUFFER_PTR + 1;
              end if;

                                                                                                               ------------------------------------------------------------
                                                                                                               -- Flush the buffer (write it out to the interface)
                                                                                                               ------------------------------------------------------------
              if(WR_CASE = WR_FLUSH_POST) or (WR_CASE = WR_POST_FLUSH) or (WR_CASE = WR_FLUSH_POST_LAST) then  -- FLUSH

                                        -- CHANGE: ADD CHECK FOR PROPER RDY


                loop

                  if(or_reduce(WR_BUFFER(0).ADDRESS(63 downto 32)) = '0') then  -- 32 bit address
                    WR_TYPE := "0010";
                  elsif(PRIMARY) then  -- Primary mode addresses are always 32 bit
                    WR_TYPE := "0010";
                  else                  -- 64 bit address
                    WR_TYPE := "0011";
                  end if;

                                                   --------------------------------------------------------
                                                   -- Header Phase
                                                   --------------------------------------------------------
                  OUT_DATA <= BAR_ATTRIBUTE_ARRAY(CURRENT_BAR).TC  -- Bits 31:29
                                & BAR_ATTRIBUTE_ARRAY(CURRENT_BAR).S  -- Bit  28
                                & WR_TYPE          -- Bits 27:24
                                & WR_BUFFER(WR_BUFFER_COUNT-1).BE  -- Bits 23:20
                                & WR_BUFFER(0).BE  -- Bits 19:16
                                & "000"            -- Bits 15:13 
                                & BAR_ATTRIBUTE_ARRAY(CURRENT_BAR).VC  -- Bit  12
                                & "00"  -- Bits 11:10
                                & to_vector(WR_BUFFER_COUNT, 10);  -- Bits 9:0
                  OUT_VALID  <= '1';
                  OUT_DFRAME <= '1';
                  OUT_WR_REQ <= BAR_ATTRIBUTE_ARRAY(CURRENT_BAR).VC & (not BAR_ATTRIBUTE_ARRAY(CURRENT_BAR).VC);

                  wait until(CLK'event and (CLK = '1'));

                                             -- See if we want to insert random wait states
                  if(IWAIT_RANDOM > 0) then  -- Insert wait states
                    next_random(IWAIT_RND_SEED);
                    if(test_random(IWAIT_RND_SEED, IWAIT_RANDOM)) then
                      OUT_VALID <= '0';
                      OUT_DATA  <= (others => 'X');
                      next_random(IWAIT_RND_SEED);
                      I_TMP     := range_random(IWAIT_RND_SEED, 1, IWAIT_RANDOM_N);
                      for J in 1 to I_TMP loop
                        wait until(CLK'event and (CLK = '1'));
                      end loop;
                      OUT_VALID <= '1';
                    end if;
                  end if;



                                             --------------------------------------------------------
                                             -- Address Phase
                                             --------------------------------------------------------
                  if(WR_TYPE = "0011") then  -- Do upper 32 bits of the address
                    OUT_DATA <= WR_BUFFER(0).ADDRESS(63 downto 32);
                    wait until(CLK'event and (CLK = '1'));


                                               -- See if we want to insert random wait states
                    if(IWAIT_RANDOM > 0) then  -- Insert wait states
                      next_random(IWAIT_RND_SEED);
                      if(test_random(IWAIT_RND_SEED, IWAIT_RANDOM)) then
                        OUT_VALID <= '0';
                        OUT_DATA  <= (others => 'X');
                        next_random(IWAIT_RND_SEED);
                        I_TMP     := range_random(IWAIT_RND_SEED, 1, IWAIT_RANDOM_N);
                        for J in 1 to I_TMP loop
                          wait until(CLK'event and (CLK = '1'));
                        end loop;
                        OUT_VALID <= '1';
                      end if;
                    end if;


                  end if;

                  if(PRIMARY) then
                    OUT_DATA <= WR_BUFFER(0).ADDRESS(31 downto 2) & '0' & to_mvl(CURRENT_BAR);
                  else
                    OUT_DATA <= WR_BUFFER(0).ADDRESS(31 downto 2) & "00";
                  end if;

                  wait until(CLK'event and (CLK = '1'));

                                        --------------------------------------------------------
                                        -- Data Phase
                                        --------------------------------------------------------
                  for I in 0 to WR_BUFFER_COUNT-1 loop

                                               -- See if we want to insert random wait states
                    if(IWAIT_RANDOM > 0) then  -- Insert wait states
                      next_random(IWAIT_RND_SEED);
                      if(test_random(IWAIT_RND_SEED, IWAIT_RANDOM)) then
                        OUT_VALID <= '0';
                        OUT_DATA  <= (others => 'X');
                        next_random(IWAIT_RND_SEED);
                        I_TMP     := range_random(IWAIT_RND_SEED, 1, IWAIT_RANDOM_N);
                        for J in 1 to I_TMP loop
                          wait until(CLK'event and (CLK = '1'));
                        end loop;
                        OUT_VALID <= '1';
                      end if;
                    end if;

                    OUT_DATA <= WR_BUFFER(I).DATA;
                    if (I = (WR_BUFFER_COUNT-1)) then
                      OUT_DFRAME <= '0';
                    end if;
                    wait until(CLK'event and (CLK = '1'));

                  end loop;
                  OUT_VALID  <= '0';
                  OUT_DFRAME <= '0';
                  OUT_DATA   <= (others => 'Z');
                  OUT_WR_REQ <= (others => '0');

                  WR_BUFFER_COUNT := 0;
                  WR_BUFFER_PTR   := 0;

                  if(WR_CASE = WR_FLUSH_POST_LAST) then  -- The flush must be executed twice
                    WR_CASE                          := 99;
                    WR_BUFFER(WR_BUFFER_PTR).ADDRESS := L_ADDR;
                    WR_BUFFER(WR_BUFFER_PTR).BE      := L_BE;
                    WR_BUFFER(WR_BUFFER_PTR).DATA    := L_DATA;
                    WR_BUFFER_COUNT                  := WR_BUFFER_COUNT + 1;
                    WR_BUFFER_PTR                    := WR_BUFFER_PTR + 1;
                  else
                    exit;
                  end if;

                end loop;

              end if;

                                                ------------------------------------------------------------
                                                -- Post to the buffer
                                                ------------------------------------------------------------
              if(WR_CASE = WR_FLUSH_POST) then  --POST
                WR_BUFFER(WR_BUFFER_PTR).ADDRESS := L_ADDR;
                WR_BUFFER(WR_BUFFER_PTR).BE      := L_BE;
                WR_BUFFER(WR_BUFFER_PTR).DATA    := L_DATA;
                WR_BUFFER_COUNT                  := WR_BUFFER_COUNT + 1;
                WR_BUFFER_PTR                    := WR_BUFFER_PTR + 1;
              end if;

                                        --========================================================--
                                        -- Do Read Request Cycles 
                                        --========================================================--
                                        --*-------------------------------------------------------------
                                        --* rd ADDR BE DATA MASK
                                        --*
                                        --*   - handles the rd/rdb command
                                        --*
                                        --* Read Cases:
                                        --*
                                        --*     rdb   Buffer   Read    Action
                                        --*   command Empty  Burstable  Taken
                                        --*
                                        --*      N      Y        -      POST, IPR        : RD_CASE=RD_POST_IPR*
                                        --*      N      N        Y      POST, IPR        : RD_CASE=RD_POST_IPR*
                                        --*      N      N        N      IPR, POST_LAST   : RD_CASE=RD_IPR_POST_LAST*
                                        --*      Y      Y        -      POST             : RD_CASE=RD_POST*
                                        --*      Y      N        Y      POST             : RD_CASE=RD_POST*
                                        --*      Y      N        N      IPR, POST        : RD_CASE=RD_IPR_POST*
                                        --*
                                        --* Note: POST means store the given read into the current CID buffer
                                        --*       POST_LAST store the given read into the current CID buffer and mark as LAST (cannot be combined) 
                                        --*       IPR means set the current buffer state to "In Progress" and load the next CID as current CID
                                        --*       NEXT_CID means allocate the next CID and set as current CID
                                        --*
                                        --*-------------------------------------------------------------
            else                        -- do read cycles

              write(OUTPUT_LINE, string'("-- rd("));
              write_hex_vector(OUTPUT_LINE, L_ADDR(63 downto 32));
              write_hex_vector(OUTPUT_LINE, L_ADDR(31 downto 0));
              write(OUTPUT_LINE, string'(", "));
              write_hex_vector(OUTPUT_LINE, L_BE);
              write(OUTPUT_LINE, string'(", "));
              write_hex_vector(OUTPUT_LINE, L_DATA);
              write(OUTPUT_LINE, string'(", "));
              write_hex_vector(OUTPUT_LINE, L_MASK);
              write(OUTPUT_LINE, string'(")"));
              writeline(OUT_FILE, OUTPUT_LINE);

              

              if(RD_BUFFER_PTR > 0) then  -- current buffer has data

                I_TMP         := To_Int(L_ADDR(31 downto 2));
                RW_SEQUENTIAL := (To_Int(RD_ADDRESS(31 downto 2)) = I_TMP);
              else

                RD_ADDRESS    := L_ADDR;
                RW_SEQUENTIAL := false;
              end if;





              if (L_CMD(3) = 'b') then  -- command is for a burst

                if (RD_BUFFER_PTR = 0) then  -- Buffer Empty
                  RD_CASE := RD_POST;
                else                         -- Buffer Not Empty
                  if(RW_SEQUENTIAL and not RW_BLAST) then
                    RD_CASE := RD_POST;
                  else
                    RD_CASE := RD_IPR_POST;
                  end if;
                end if;
              else                           -- Single Write

                if(RD_BUFFER_PTR = 0) then  -- Buffer Empty
                  RD_CASE := RD_POST_IPR;
                elsif RW_SEQUENTIAL and (RD_CASE /= RD_IPR_POST_LAST) then  -- Buffer not Empty and burstable
                  RD_CASE := RD_POST_IPR;
                else
                  RD_CASE := RD_IPR_POST_LAST;
                end if;
              end if;


                                        ------------------------------------------------------------
                                        -- Stall until it is safe to post another buffer
                                        ------------------------------------------------------------
              if(RD_BUFFER_PTR = 0) then

                while(CURRENT_OUTBOUND_RD_IPR >= OUTBOUND_RD_OUTSTANDING) loop  -- we must stall
                  wait until(CLK'event and (CLK = '1'));
                end loop;

                                        -- Allocate a new CID
                NEXT_CID := CURRENT_CID;
                for I in 0 to N_COMPLETION_ID-1 loop
                  NEXT_CID := NEXT_CID + 1;
                  if(NEXT_CID > (N_COMPLETION_ID-1)) then
                    NEXT_CID := 0;
                  end if;
                  if(RD_BUFFER(NEXT_CID).STATE = OUTBOUND_READ_REQUEST_CPL_STATE(NEXT_CID)) then
                    exit;
                  end if;
                end loop;
                CURRENT_CID := NEXT_CID;

              end if;


                                                                     ------------------------------------------------------------
                                                                     -- Post to the buffer
                                                                     ------------------------------------------------------------
              if(RD_CASE = RD_POST_IPR) or (RD_CASE = RD_POST) then  -- POST

                if(RD_BUFFER_PTR = 0) then  -- buffer empty
                  RD_BUFFER(CURRENT_CID).ADDRESS <= L_ADDR;
                  RD_BUFFER(CURRENT_CID).BAR_HIT <= to_mvl(BAR_HIT(1)) & to_mvl(BAR_HIT(1));
                  RD_BUFFER(CURRENT_CID).FBE     <= L_BE;
                end if;

                RD_BUFFER(CURRENT_CID).LBE                 <= L_BE;
                RD_BUFFER(CURRENT_CID).DATA(RD_BUFFER_PTR) <= L_DATA;
                RD_BUFFER(CURRENT_CID).MASK(RD_BUFFER_PTR) <= L_MASK;
                RD_BUFFER_PTR                              := RD_BUFFER_PTR + 1;
                RD_ADDRESS(RD_ADDRESS'high downto 2)       := L_ADDR(RD_ADDRESS'high downto 2) + '1';

                wait on RD_BUFFER;

              end if;

                                                                                                         ------------------------------------------------------------
                                                                                                         -- Flush the read buffer (send request packet and set the buffer state to in-progress)
                                                                                                         ------------------------------------------------------------
              if(RD_CASE = RD_POST_IPR) or (RD_CASE = RD_IPR_POST_LAST) or (RD_CASE = RD_IPR_POST) then  -- IPR
                RD_BUFFER(CURRENT_CID).STATE <= not RD_BUFFER(CURRENT_CID).STATE;

                                     --------------------------------------------------------
                                     -- Do the Read Request
                                     --------------------------------------------------------
                if(or_reduce(RD_BUFFER(CURRENT_CID).ADDRESS(63 downto 32)) = '0') then  -- 32 bit address
                  RD_TYPE := "0000";
                elsif(PRIMARY) then  -- Primary mode addresses are always 32 bit
                  RD_TYPE := "0000";
                else                    -- 64 bit address
                  RD_TYPE := "0001";
                end if;


                                                                 --------------------------------------------------------
                                        -- Header Phase
                                                                 --------------------------------------------------------
                OUT_DATA <= BAR_ATTRIBUTE_ARRAY(CURRENT_BAR).TC  -- Bits 31:29
                              & BAR_ATTRIBUTE_ARRAY(CURRENT_BAR).S   -- Bit  28
                              & RD_TYPE                          -- Bits 27:24
                              & RD_BUFFER(CURRENT_CID).LBE       -- Bits 23:20
                              & RD_BUFFER(CURRENT_CID).FBE       -- Bits 19:16
                              & "000"   -- Bits 15:13 
                              & BAR_ATTRIBUTE_ARRAY(CURRENT_BAR).VC  -- Bit  12
                              & to_vector(CURRENT_CID, 2)        -- Bits 11:10
                              & to_vector(RD_BUFFER_PTR, 10);    -- Bits 9:0
                OUT_VALID  <= '1';
                OUT_DFRAME <= '1';

                wait until(CLK'event and (CLK = '1'));


                                           -- See if we want to insert random wait states
                if(IWAIT_RANDOM > 0) then  -- Insert wait states
                  next_random(IWAIT_RND_SEED);
                  if(test_random(IWAIT_RND_SEED, IWAIT_RANDOM)) then
                    OUT_VALID <= '0';
                    OUT_DATA  <= (others => 'X');
                    next_random(IWAIT_RND_SEED);
                    I_TMP     := range_random(IWAIT_RND_SEED, 1, IWAIT_RANDOM_N);
                    for J in 1 to I_TMP loop
                      wait until(CLK'event and (CLK = '1'));
                    end loop;
                    OUT_VALID <= '1';
                  end if;
                end if;


                                           --------------------------------------------------------
                                           -- Address Phase
                                           --------------------------------------------------------
                if(RD_TYPE = "0001") then  -- Do upper 32 bits of the address
                  OUT_DATA <= RD_BUFFER(CURRENT_CID).ADDRESS(63 downto 32);
                  wait until(CLK'event and (CLK = '1'));


                                             -- See if we want to insert random wait states
                  if(IWAIT_RANDOM > 0) then  -- Insert wait states
                    next_random(IWAIT_RND_SEED);
                    if(test_random(IWAIT_RND_SEED, IWAIT_RANDOM)) then
                      OUT_VALID <= '0';
                      OUT_DATA  <= (others => 'X');
                      next_random(IWAIT_RND_SEED);
                      I_TMP     := range_random(IWAIT_RND_SEED, 1, IWAIT_RANDOM_N);
                      for J in 1 to I_TMP loop
                        wait until(CLK'event and (CLK = '1'));
                      end loop;
                      OUT_VALID <= '1';
                    end if;
                  end if;

                end if;

                if(PRIMARY) then
                  OUT_DATA <= RD_BUFFER(CURRENT_CID).ADDRESS(31 downto 2) & '0' & to_mvl(CURRENT_BAR);
                else
                  OUT_DATA <= RD_BUFFER(CURRENT_CID).ADDRESS(31 downto 2) & "00";
                end if;
                OUT_DFRAME <= '0';

                wait until(CLK'event and (CLK = '1'));
                OUT_VALID <= '0';
                OUT_DATA  <= (others => 'Z');


                RD_BUFFER_PTR := 0;

              end if;

                                                                              ------------------------------------------------------------
                                        -- Post to the buffer
                                                                              ------------------------------------------------------------
           
              if(RD_CASE = RD_IPR_POST_LAST) or (RD_CASE = RD_IPR_POST) then  -- POST

                if(RD_BUFFER_PTR = 0) then  -- buffer empty
                  RD_BUFFER(CURRENT_CID).ADDRESS <= L_ADDR;
                  RD_BUFFER(CURRENT_CID).FBE     <= L_BE;
                end if;

                RD_BUFFER(CURRENT_CID).LBE                 <= L_BE;
                RD_BUFFER(CURRENT_CID).DATA(RD_BUFFER_PTR) <= L_DATA;
                RD_BUFFER(CURRENT_CID).MASK(RD_BUFFER_PTR) <= L_MASK;
                RD_BUFFER_PTR                              := RD_BUFFER_PTR + 1;
                RD_ADDRESS(RD_ADDRESS'high downto 2)       := L_ADDR(RD_ADDRESS'high downto 2) + '1';

              end if;


            end if;

            
          elsif(BFM_BAR_HIT(0) or BFM_BAR_HIT(1)) then  -- Address hits one of the internal BFM RAM BARs

             
                                        ------------------------------------------------------------
                                        -- Write data to the local BFM memory
                                        ------------------------------------------------------------
            A_TMP := L_ADDR(L_ADDR'high downto 2) & "00";
            if (L_CMD(1 to 2) = "wr") then
              for i in 0 to L_BE'high loop
               
                if (L_BE(i) = '1') then
                  DATA_TMP8 := L_DATA(8*i+7 downto 8*i);


write(OUTPUT_LINE, string'("-- Mem_Write("));
write_hex_vector(OUTPUT_LINE, A_TMP(29 downto 0));
write(OUTPUT_LINE, string'(", "));
write_hex_vector(OUTPUT_LINE, DATA_TMP8);
write(OUTPUT_LINE, string'(")"));
writeline(OUT_FILE, OUTPUT_LINE);

                  RAM_WR0      <= true;
                  RAM_ADDR0    <= A_TMP(N_RAM_MAX-1 downto 0);
                  RAM_WR_DATA0 <= DATA_TMP8;
                  RAM_REQ0     <= not RAM_REQ0;
                  wait on RAM_ACK0;

                end if;
                A_TMP := A_TMP + '1';
              end loop;
--              report "ENDLOOP";
                                        ------------------------------------------------------------
                                        -- Read data from the local BFM memory
                                        ------------------------------------------------------------
            else
              for i in 0 to L_BE'high loop
                if (L_BE(i) = '1') then
                  RAM_WR0   <= false;
                  RAM_ADDR0 <= A_TMP(N_RAM_MAX-1 downto 0);
                  RAM_REQ0  <= not RAM_REQ0;
                  wait on RAM_ACK0;
                  DATA_TMP8 := RAM_RD_DATA0;

--write(OUTPUT_LINE, string'("-- Mem_Read("));
--write_hex_vector(OUTPUT_LINE, A_TMP(29 downto 0));
--write(OUTPUT_LINE, string'(", "));
--write_hex_vector(OUTPUT_LINE, DATA_TMP8);
--write(OUTPUT_LINE, string'(")"));
--writeline(OUT_FILE, OUTPUT_LINE);

                  DATA_TMP32(8*i+7 downto 8*i) := DATA_TMP8;
                  A_TMP                        := A_TMP + '1';
                end if;
              end loop;
              read_cmp(OUTPUT_LINE, (INSTANCE_LABEL & "BFM Local Readback "), DATA_TMP32, L_DATA, L_MASK, vERR);


            
              if(vERR = 1) then writeline(OUT_FILE, OUTPUT_LINE); end if;
              
            end if;

          else
            write(OUTPUT_LINE, "-- ERROR: " & INSTANCE_LABEL);
            write(OUTPUT_LINE, NOW, left, 0, ps);
            if (L_CMD(1 to 2) = "wr") then
              write(OUTPUT_LINE, string'(" The write command didn't match any BAR address :"));
              write_hex_vector(OUTPUT_LINE, L_ADDR);
            else
              write(OUTPUT_LINE, string'(" The read command didn't match any BAR address :"));
              write_hex_vector(OUTPUT_LINE, L_ADDR);
            end if;
            writeline(OUT_FILE, OUTPUT_LINE);
          end if;

          --*-------------------------------------------------------------
          --* init: Set the model to all the default values
          --*-------------------------------------------------------------
        elsif (L_CMD(1 to 4) = "init") then
          GENERATE_X              <= false;
          EXPECT_ERROR            <= false;
          T_LCLKi                 <= T_LCLK;
          T_P2L_CLK_DLYi          <= T_P2L_CLK_DLY;
          T_HOLD_OUTi             <= T_HOLD_OUT;
          OUTBOUND_RD_OUTSTANDING <= 1;
          RESPONSE_DELAY          <= 0;
          BURST_LENGTH            := (others => 512);
          N_BURST_MODULO          := (others => log2(512));
          CURRENT_VC              := 0;
          CPL_MODULO              := 64;
          IWAIT_RANDOM            := 0;
          IWAIT_RND_SEED          := 5000;
                                        --CURRENT_BAR     := 0;

          --*-------------------------------------------------------------
          --* reset N: Drive RSTOUT18n, RSTOUT33n for N clocks
          --*-------------------------------------------------------------
        elsif (L_CMD(1 to 5) = "reset") then
          SGET_INT(QCMD, CHAR_PTR, I_TMP);

          RSTOUTo <= '1';

          if(CMD_CLOCK_EN) then
            for i in 1 to I_TMP loop
              wait until (CLK'event and CLK = '1');
            end loop;
          else
            wait for I_TMP * T_LCLKi;
          end if;

          RSTOUTo <= '0';

          --*-------------------------------------------------------------
          --* expect_error on|off|true|false: Expect read error
          --*
          --* When true|on read results will expect a completor error
          --* and an ERROR will be reported if it is not
          --*
          --* When false|off read results will expect the error bit to be 
          --* off and an ERROR will be reported if it is on
          --*-------------------------------------------------------------
        elsif (L_CMD(1 to 12) = "expect_error") then
          SGET_BOOLEAN(QCMD, CHAR_PTR, B_TMP);
          EXPECT_ERROR <= B_TMP;

          --*-------------------------------------------------------------
          --* generate_x on|off|true|false: Generate X when signals change
          --*
          --* When true|on output signals will go X after T_HOLD_OUT from 
          --* clock when the signal transitions.  It will return to a valid
          --* logic level after T_CO.
          --*
          --* CLK  ______~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          --*            |
          --*            |<-----------T_CO--------->|
          --*            |                          |
          --*            |<--T_HOLD_OUT-->|         |
          --*                             |         |
          --* SIG  ________________________XXXXXXXXXX~~~~~~~
          --*
          --*-------------------------------------------------------------
        elsif (L_CMD(1 to 10) = "generate_x") then
          SGET_BOOLEAN(QCMD, CHAR_PTR, B_TMP);
          GENERATE_X <= B_TMP;

          --*-------------------------------------------------------------
          --* clk N: set clock period to N ps
          --*-------------------------------------------------------------
        elsif (L_CMD(1 to 4) = "lclk") then
          SGET_INT(QCMD, CHAR_PTR, I_TMP);
          T_LCLKi <= I_TMP * 1 ps;

          --*-------------------------------------------------------------
          --* iwait_random P N: Initiator random wait state insertion. 
          --*-------------------------------------------------------------
        elsif (L_CMD(1 to 12) = "iwait_random") then
          SGET_INT(QCMD, CHAR_PTR, I_TMP);
          SGET_INT(QCMD, CHAR_PTR, I_TMP2);
          if(I_TMP < 0) or (I_TMP > 100) then
            assert false report "WARNING: Specified iwait_random probability is out of the acceptable range of 0-100: setting to 0";
            I_TMP := 0;
          end if;
          if(I_TMP2 < 0) or (I_TMP2 > 99) then
            assert false report "WARNING: Specified iwait_random max wait is out of the acceptable range of 1-32: setting to 1";
            I_TMP2 := 1;
          end if;
          IWAIT_RANDOM   := I_TMP;
          IWAIT_RANDOM_N := I_TMP2;



          --*-------------------------------------------------------------
          --* bfm_bar BAR ADDR: Used to allocate local memory inside
          --*                      the BFM for an external master to R/W.
          --*-------------------------------------------------------------
        elsif (L_CMD(1 to 7) = "bfm_bar") then
          SGET_INT(QCMD, CHAR_PTR, I_TMP);         -- BAR
          SGET_VECTOR_64(QCMD, CHAR_PTR, L_ADDR);  -- ADDR
          SGET_INT(QCMD, CHAR_PTR, I_TMP2);        -- SIZE
                                                   -- Check BAR
          if(I_TMP /= 0) then
            I_TMP := 1;
          end if;

                                        -- Check SIZE
          if(I_TMP2 /= (2**log2(I_TMP2))) then
            assert false report "WARNING: Specified SIZE is not a power of 2, rounding down.";
            I_TMP2 := (2**log2(I_TMP2));
          end if;
          I_TMP2                              := I_TMP2 - 1;
          AMASK_TMP                           := to_vector(I_TMP2, AMASK_TMP'length);
          AMASK_TMP                           := not AMASK_TMP;
          BFM_BAR_ATTRIBUTE_ARRAY(I_TMP).MASK <= AMASK_TMP;

                                        -- Check BASE
          if(L_ADDR(AMASK_TMP'range) /= (L_ADDR(AMASK_TMP'range) and AMASK_TMP)) then
            assert false report "WARNING: Specified Base Address will be alligned to match the BAR size.";
            L_ADDR(AMASK_TMP'range) := (L_ADDR(AMASK_TMP'range) and AMASK_TMP);
          end if;
          BFM_BAR_ATTRIBUTE_ARRAY(I_TMP).BASE <= L_ADDR;

                                        -- Make sure that BFM_BAR_ATTRIBUTE_ARRAY is updated before continuing
          wait until (BFM_BAR_ATTRIBUTE_ARRAY'event);

          write(OUTPUT_LINE, "-- " & INSTANCE_LABEL);
          write(OUTPUT_LINE, string'("BFM BAR set to: BAR="));
          write(OUTPUT_LINE, I_TMP);

          write(OUTPUT_LINE, string'(", BASE=0x"));
          write_hex_vector(OUTPUT_LINE, BFM_BAR_ATTRIBUTE_ARRAY(I_TMP).BASE);

          write(OUTPUT_LINE, string'(", MASK=0x"));
          write_hex_vector(OUTPUT_LINE, BFM_BAR_ATTRIBUTE_ARRAY(I_TMP).MASK);

          writeline(OUT_FILE, OUTPUT_LINE);


          --*-------------------------------------------------------------
          --* bar BAR ADDR SIZE VC TC: Sets up one of the base address 
          --*                    registers.  All read/write transactions 
          --*                    that match a BAR will be routed external.
          --*-------------------------------------------------------------
        elsif (L_CMD(1 to 3) = "bar") then
          SGET_INT(QCMD, CHAR_PTR, I_TMP);         -- BAR
          SGET_VECTOR_64(QCMD, CHAR_PTR, L_ADDR);  -- ADDR
          SGET_INT(QCMD, CHAR_PTR, I_TMP2);        -- SIZE
          SGET_INT(QCMD, CHAR_PTR, I_TMP3);        -- VC
          SGET_INT(QCMD, CHAR_PTR, I_TMP4);        -- TC
          SGET_INT(QCMD, CHAR_PTR, I_TMP5);        -- S

                                        -- Check BAR
          if(I_TMP /= 0) then
            I_TMP := 1;
          end if;

                                        -- Check SIZE
          if(I_TMP2 /= (2**log2(I_TMP2))) then
            assert false report "WARNING: Specified SIZE is not a power of 2, rounding down.";
            I_TMP2 := (2**log2(I_TMP2));
          end if;
          I_TMP2                          := I_TMP2 - 1;
          AMASK_TMP                       := to_vector(I_TMP2, AMASK_TMP'length);
          AMASK_TMP                       := not AMASK_TMP;
          BAR_ATTRIBUTE_ARRAY(I_TMP).MASK <= AMASK_TMP;

                                        -- Check BASE
          if(L_ADDR(AMASK_TMP'range) /= (L_ADDR(AMASK_TMP'range) and AMASK_TMP)) then
            assert false report "WARNING: Specified Base Address will be alligned to match the BAR size.";
            L_ADDR(AMASK_TMP'range) := (L_ADDR(AMASK_TMP'range) and AMASK_TMP);
          end if;
          BAR_ATTRIBUTE_ARRAY(I_TMP).BASE <= L_ADDR;

                                        -- Check VC
          if(I_TMP3 < 0) or (I_TMP3 > 1) then
            assert false report "WARNING: Specified VC is out of range, will be set to 0.";
            I_TMP3 := 0;
          end if;
          if(I_TMP3 /= 0) then
            BAR_ATTRIBUTE_ARRAY(I_TMP).VC <= '1';
          else
            BAR_ATTRIBUTE_ARRAY(I_TMP).VC <= '0';
          end if;

                                        -- Check Trafic Class
          if(I_TMP4 < 0) or (I_TMP4 > 7) then
            assert false report "WARNING: Specified TC is out of range, will be set to 0.";
            I_TMP4 := 0;
          end if;
          BAR_ATTRIBUTE_ARRAY(I_TMP).TC <= to_vector(I_TMP4, 3);

                                        -- Check Trafic Class
          if(I_TMP5 < 0) or (I_TMP5 > 1) then
            assert false report "WARNING: Specified NoSnoop is out of range, will be set to 0.";
            I_TMP5 := 0;
          end if;
          BAR_ATTRIBUTE_ARRAY(I_TMP).S <= to_mvl(I_TMP5);


                                        -- Make sure that BAR_ATTRIBUTE_ARRAY is updated before continuing
          wait until (BAR_ATTRIBUTE_ARRAY'event);

          write(OUTPUT_LINE, "-- " & INSTANCE_LABEL);
          write(OUTPUT_LINE, string'("BAR set to: BAR="));
          write(OUTPUT_LINE, I_TMP);

          write(OUTPUT_LINE, string'(", BASE=0x"));
          write_hex_vector(OUTPUT_LINE, BAR_ATTRIBUTE_ARRAY(I_TMP).BASE);

          write(OUTPUT_LINE, string'(", MASK=0x"));
          write_hex_vector(OUTPUT_LINE, BAR_ATTRIBUTE_ARRAY(I_TMP).MASK);

          write(OUTPUT_LINE, string'(", VC="));
          write(OUTPUT_LINE, To_bit(BAR_ATTRIBUTE_ARRAY(I_TMP).VC));

          write(OUTPUT_LINE, string'(", TC=0x"));
          write_hex_vector(OUTPUT_LINE, BAR_ATTRIBUTE_ARRAY(I_TMP).TC);

          write(OUTPUT_LINE, string'(", S="));
          write(OUTPUT_LINE, To_bit(BAR_ATTRIBUTE_ARRAY(I_TMP).S));

          writeline(OUT_FILE, OUTPUT_LINE);



          --*-------------------------------------------------------------
          --* rd_outstanding_out N: This determines how many outstanding  
          --*               reads the BFM will generate before stalling.
          --*-------------------------------------------------------------
        elsif (L_CMD(1 to 18) = "rd_outstanding_out") then
          SGET_INT(QCMD, CHAR_PTR, I_TMP);  -- N

          if(I_TMP < 1) or (I_TMP > 4) then
            assert false report "WARNING: Specified reads outstanding is out of the acceptable range of 1-4: setting to 1";
            I_TMP := 1;
          end if;

          OUTBOUND_RD_OUTSTANDING <= I_TMP;

          write(OUTPUT_LINE, "-- " & INSTANCE_LABEL);
          write(OUTPUT_LINE, string'("rd_outstanding_out set to: "));
          write(OUTPUT_LINE, I_TMP);
          writeline(OUT_FILE, OUTPUT_LINE);

          --*-------------------------------------------------------------
          --* cpl_modulo N: Sets the burst modulo boundary that 
          --*               read completion packets will get truncated to.
          --*-------------------------------------------------------------
        elsif (L_CMD(1 to 10) = "cpl_modulo") then
          SGET_INT(QCMD, CHAR_PTR, I_TMP);  -- N

          if(I_TMP < 8) or (I_TMP > 512) then
            assert false report "WARNING: Specified completion modulo is out of the acceptable range of 8-512 bytes: setting to 512 bytes";
            I_TMP := 512;
          end if;

          if((2**log2(I_TMP)) /= I_TMP) then
            I_TMP := (2**log2(I_TMP));
            write(OUTPUT_LINE, string'("-- WARNING: Specified completion modulo is not a power of 2: setting to "));
            write(OUTPUT_LINE, I_TMP);
            write(OUTPUT_LINE, string'(" bytes"));
            writeline(OUT_FILE, OUTPUT_LINE);
          end if;
          CPL_MODULO := I_TMP;

          write(OUTPUT_LINE, "-- " & INSTANCE_LABEL & "cpl_modulo set to: ");
          write(OUTPUT_LINE, I_TMP);
          write(OUTPUT_LINE, string'(" bytes"));
          writeline(OUT_FILE, OUTPUT_LINE);

          --*-------------------------------------------------------------
          --* cpl_order N: Completion order: Determines the order of 
          --*               completion packets put out by the BFM.
          --*-------------------------------------------------------------
        elsif (L_CMD(1 to 9) = "cpl_order") then
          SGET_BOOLEAN(QCMD, CHAR_PTR, B_TMP);  -- N


          CPL_ORDER := B_TMP;

          write(OUTPUT_LINE, "-- " & INSTANCE_LABEL);
          write(OUTPUT_LINE, string'("cpl_modulo set to: "));
          write(OUTPUT_LINE, I_TMP);
          write(OUTPUT_LINE, string'(" bytes"));
          writeline(OUT_FILE, OUTPUT_LINE);

          --*-------------------------------------------------------------
          --* burst_modulo N BAR: Sets the burst modulo boundary that 
          --*                    read/write packets will get truncated to.
          --*-------------------------------------------------------------
        elsif (L_CMD(1 to 12) = "burst_modulo") then
          SGET_INT(QCMD, CHAR_PTR, I_TMP);   -- N
          SGET_INT(QCMD, CHAR_PTR, I_TMP2);  -- BAR
          if(I_TMP2 < 0) or (I_TMP2 > (N_BARS-1)) then
            assert false report "WARNING: Specified Virtual Channel is out of range, defaulting to 1";
            I_TMP2 := 1;
          else
            N_BURST_MODULO(I_TMP2) := log2(I_TMP);
          end if;

          if(I_TMP < 8) or (I_TMP > 4096) then
            assert false report "WARNING: Specified burst modulo is out of the acceptable range of 8-4096 bytes: setting to 512 bytes";
            I_TMP := 512;
          end if;

          if((2**log2(I_TMP)) /= I_TMP) then
            I_TMP := (2**log2(I_TMP));
            write(OUTPUT_LINE, string'("-- WARNING: Specified burst modulo is not a power of 2: setting to "));
            write(OUTPUT_LINE, I_TMP);
            write(OUTPUT_LINE, string'(" bytes"));
            writeline(OUT_FILE, OUTPUT_LINE);
          end if;
          N_BURST_MODULO(I_TMP2) := log2(I_TMP);


          write(OUTPUT_LINE, "-- " & INSTANCE_LABEL);
          write(OUTPUT_LINE, string'("burst_modulo set to: ") & to_str(I_TMP) & " bytes for BAR" & to_str(I_TMP2));
          writeline(OUT_FILE, OUTPUT_LINE);



          --*-------------------------------------------------------------
          --* burst_length N BAR: Sets the burst length that 
          --*                    read/write packets will get truncated to.
          --*-------------------------------------------------------------
        elsif (L_CMD(1 to 12) = "burst_length") then
          SGET_INT(QCMD, CHAR_PTR, I_TMP);   -- N
          SGET_INT(QCMD, CHAR_PTR, I_TMP2);  -- BAR
          if(I_TMP2 < 0) or (I_TMP2 > (N_BARS-1)) then
            assert false report "WARNING: Specified Virtual Channel is out of range, defaulting to 1";
            I_TMP2 := 1;
          end if;

          if(I_TMP < 0) or (I_TMP > 4096) then
            assert false report "WARNING: Specified burst length is out of range: setting to 512 bytes";
            BURST_LENGTH(I_TMP2) := 512;
          else
            BURST_LENGTH(I_TMP2) := I_TMP;
          end if;

          write(OUTPUT_LINE, "-- " & INSTANCE_LABEL);
          write(OUTPUT_LINE, string'("burst_length set to: ") & to_str(BURST_LENGTH(I_TMP2)) & " for BAR" & to_str(I_TMP2));
          writeline(OUT_FILE, OUTPUT_LINE);

          --*-------------------------------------------------------------
          --* wait N: Idle for N clocks
          --*-------------------------------------------------------------
        elsif (L_CMD(1 to 4) = "wait") then
          SGET_INT(QCMD, CHAR_PTR, I_TMP);

          for i in 1 to I_TMP loop
            wait until (CLK'event and (CLK = '1'));
          end loop;
          --*-------------------------------------------------------------
          --* gpio_wait N P MASK        Wait for N local bus clock intervals for GPIO to reach a defined state.  
          --*-------------------------------------------------------------
        elsif (L_CMD(1 to 9) = "gpio_wait") then
          SGET_INT(QCMD, CHAR_PTR, I_TMP);
          SGET_VECTOR(QCMD, CHAR_PTR, L_DATA);
          SGET_VECTOR(QCMD, CHAR_PTR, L_MASK);

          for i in 1 to I_TMP loop
            wait until (CLK'event and (CLK = '1'));
            I_TMP2 := i;
                                        -- if(and_reduce(((GPIOi and L_MASK(GPIO'range)) xnor (L_DATA(GPIO'range) and L_MASK(GPIO'range)))) = 0) then
            if(((GPIOi and L_MASK(GPIO'range)) = (L_DATA(GPIO'range) and L_MASK(GPIO'range)))) then
              exit;
            end if;
          end loop;
          if(I_TMP2 = I_TMP) then       -- command timed out
            write(OUTPUT_LINE, "-- " & INSTANCE_LABEL & string'("ERROR: gpio_wait command timed out"));
            writeline(OUT_FILE, OUTPUT_LINE);
          end if;


          --*-------------------------------------------------------------
          --* flush N: Idle for N clocks
          --*-------------------------------------------------------------
        elsif (L_CMD(1 to 5) = "flush") then
          SGET_INT(QCMD, CHAR_PTR, I_TMP);

          for i in 1 to I_TMP loop
            if(CURRENT_OUTBOUND_RD_IPR = 0) then
              exit;
            end if;
            wait until (CLK'event and (CLK = '1'));
          end loop;

          if(CURRENT_OUTBOUND_RD_IPR /= 0) then
            write(OUTPUT_LINE, "-- " & INSTANCE_LABEL);
            write(OUTPUT_LINE, string'("ERROR: flush command timed out"));
            writeline(OUT_FILE, OUTPUT_LINE);
          end if;

        elsif (L_CMD(1 to 2) /= "--") and (L_CMD(1 to 2) /= "//") and (L_CMD(1) /= ' ') then
          assert false report "Unrecognized command";
        end if;

      end if;

      --============================================================--
      -- Send out a completion packet
      --============================================================--
      if(CURRENT_INBOUND_RD_IPR /= 0) then  -- Send out a read completion

        ------------------------------------------------------------
        -- Make sure we are alligned to the clock
        ------------------------------------------------------------
        if not(CLK'event and (CLK = '1')) then
          wait until (CLK'event and (CLK = '1'));
        end if;

        ------------------------------------------------------------
        -- Select the next CID to service (does a round robin and supports OoO)
        ------------------------------------------------------------

        for I in 0 to N_COMPLETION_ID-1 loop

                                        -- Point to the next possible Completion ID
          if(CPL_ORDER or (I /= 0)) then
            CPL_CID := CPL_CID + 1;
          end if;
          if(CPL_CID >= N_COMPLETION_ID) then
            CPL_CID := 0;
          end if;

          if(INBOUND_READ_REQUEST_ARRAY(CPL_CID).STATE /= INBOUND_READ_REQUEST_CPL_STATE(CPL_CID)) then
            exit;
          end if;
        end loop;

        ------------------------------------------------------------
        -- Header Phase
        ------------------------------------------------------------

        if(INBOUND_READ_REQUEST_ARRAY(CPL_CID).LENGTH = 0) then
          RD_TYPE := "0100";
        else
          RD_TYPE := "0101";
        end if;


-- Start Address = INBOUND_READ_REQUEST_ARRAY(CPL_CID).ADDRESS(31:2)

        -- Start DW Address

        A_TMP                            := (others => '0');
        A_TMP(RAM_ADDR0'high)            := INBOUND_READ_REQUEST_ARRAY(CPL_CID).BAR_HIT(1);
        A_TMP(RAM_ADDR0'high-1 downto 2) := INBOUND_READ_REQUEST_ARRAY(CPL_CID).ADDRESS(RAM_ADDR0'high-1 downto 2);

        I_TMP := to_int(A_TMP(RAM_ADDR0'high downto 2));  -- This is the DW base address relative to the BFM internal RAM
        I_TMP := I_TMP + CPL_POP_PTR(CPL_CID);  -- This is the DW base address + offset

--                              I_TMP := to_int(INBOUND_READ_REQUEST_ARRAY(CPL_CID).BAR_HIT(1) 
--                                              & INBOUND_READ_REQUEST_ARRAY(CPL_CID).ADDRESS(RAM_ADDR0'high-1 downto 2))
--                                     + CPL_POP_PTR(CPL_CID);


        A_TMP                  := (others => '0');
        A_TMP(RAM_ADDR0'range) := to_vector(I_TMP, RAM_ADDR0'length-2) & "00";

        CPL_LENGTH := (CPL_MODULO/4) - (I_TMP mod (CPL_MODULO/4));  -- This determines the modulo alligned length
        CPL_LENGTH := minimum(INBOUND_READ_REQUEST_ARRAY(CPL_CID).LENGTH - CPL_POP_PTR(CPL_CID), CPL_LENGTH);

        CPL_LAST := to_mvl((CPL_POP_PTR(CPL_CID) + CPL_LENGTH) >= INBOUND_READ_REQUEST_ARRAY(CPL_CID).LENGTH);


        OUT_DATA <= INBOUND_READ_REQUEST_ARRAY(CPL_CID).TC   -- Bits 31:29 (TC)
                      & '0'             -- Bit  28    (unused)
                      & RD_TYPE         -- Bits 27:24 (TYPE)
                      & "000000"        -- Bits 23:18 (unused)
                      & "00"  -- Bits 17:16 (STAT) -- CHANGE to add exception cases
                      & CPL_LAST        -- Bits 15    (L) 
                      & "00"            -- Bits 14:13 (unused)
                      & BAR_ATTRIBUTE_ARRAY(CURRENT_BAR).VC  -- Bit  12
                      & to_vector(CPL_CID, 2)                -- Bits 11:10
                      & to_vector(CPL_LENGTH, 10);           -- Bits 9:0
        OUT_VALID  <= '1';
        OUT_DFRAME <= '1';

        wait until(CLK'event and (CLK = '1'));

        -- See if we want to insert random wait states
        if(IWAIT_RANDOM > 0) then       -- Insert wait states
          next_random(IWAIT_RND_SEED);
          if(test_random(IWAIT_RND_SEED, IWAIT_RANDOM)) then
            OUT_VALID <= '0';
            OUT_DATA  <= (others => 'X');
            next_random(IWAIT_RND_SEED);
            I_TMP     := range_random(IWAIT_RND_SEED, 1, IWAIT_RANDOM_N);
            for J in 1 to I_TMP loop
              wait until(CLK'event and (CLK = '1'));
            end loop;
            OUT_VALID <= '1';
          end if;
        end if;



        ------------------------------------------------------------
        -- Data Phase
        ------------------------------------------------------------

        if(INBOUND_READ_REQUEST_ARRAY(CPL_CID).LENGTH = 0) then  -- no data to return
          OUT_DATA <= (others => 'X');
          wait until(CLK'event and (CLK = '1'));

        else                            -- send back data

          RAM_WR0 <= false;

          for I in 1 to CPL_LENGTH loop

            for J in 0 to 3 loop        -- Do all 4 bytes of the read

              RAM_ADDR0 <= A_TMP(RAM_ADDR0'range);
              RAM_REQ0  <= not RAM_REQ0;

              wait on RAM_ACK0;

              OUT_DATA(J*8+7 downto J*8) <= RAM_RD_DATA0;

              A_TMP := A_TMP + '1';
            end loop;

            if(I = CPL_LENGTH) then
              OUT_DFRAME <= '0';
            end if;

            wait until(CLK'event and (CLK = '1'));

            CPL_POP_PTR(CPL_CID) := CPL_POP_PTR(CPL_CID) + 1;

                                        -- See if we want to insert random wait states
            if(IWAIT_RANDOM > 0) then   -- Insert wait states
              next_random(IWAIT_RND_SEED);
              if(test_random(IWAIT_RND_SEED, IWAIT_RANDOM)) then
                OUT_VALID <= '0';
                OUT_DATA  <= (others => 'X');
                next_random(IWAIT_RND_SEED);
                I_TMP     := range_random(IWAIT_RND_SEED, 1, IWAIT_RANDOM_N);
                for J in 1 to I_TMP loop
                  wait until(CLK'event and (CLK = '1'));
                end loop;
                OUT_VALID <= '1';
              end if;
            end if;



          end loop;

        end if;

        if(CPL_LAST = '1') then         -- Retire the CID
          CPL_POP_PTR(CPL_CID)                    := 0;
          INBOUND_READ_REQUEST_CPL_STATE(CPL_CID) <= INBOUND_READ_REQUEST_ARRAY(CPL_CID).STATE;
          wait on INBOUND_READ_REQUEST_CPL_STATE;
        end if;

        OUT_VALID  <= '0';
        OUT_DFRAME <= '0';
        OUT_DATA   <= (others => 'Z');


      end if;

    end loop;
  end process;

--#########################################################################--
--
-- InBound State Machine 
--
-- (Handles RX to the BFM: L2P for Primary and P2L for secondary)
--
--#########################################################################--
  process
    --file      OUT_FILE : text is out "STD_OUTPUT";
    file OUT_FILE        : text open write_mode is "NullFile";
    variable OUTPUT_LINE : line;

    variable HEADER_TC        : std_ulogic_vector(2 downto 0);
    variable HEADER_S         : std_ulogic;
    variable HEADER_TYPE      : std_ulogic_vector(3 downto 0);
    variable HEADER_LBE       : std_ulogic_vector(3 downto 0);
    variable HEADER_FBE       : std_ulogic_vector(3 downto 0);
    variable HEADER_STAT      : std_ulogic_vector(1 downto 0);
    variable HEADER_L         : std_ulogic;
    variable HEADER_V         : std_ulogic;
    variable HEADER_CID       : std_ulogic_vector(1 downto 0);
    variable HEADER_LENGTH    : std_ulogic_vector(9 downto 0);
    variable HEADER           : std_ulogic_vector(IN_DATA'range);
    variable HEADER_ADDR_LOW  : std_ulogic_vector(IN_DATA'range);
    variable HEADER_ADDR_HI   : std_ulogic_vector(IN_DATA'range);
    variable BFM_BAR_HIT      : boolean_vector(1 downto 0);
    variable RAM_ADDR         : std_ulogic_vector(N_RAM_MAX-1 downto 0);
    variable RAM_BE           : std_ulogic_vector(3 downto 0);
    variable RAM_DATA         : std_ulogic_vector(7 downto 0);
    variable INBOUND_LABEL    : string(1 to 3);
    variable FIRST_DW         : boolean;
    variable COMPLETION_ID    : integer;  -- 
    variable CID_COUNT        : integer;
    variable I_HEADER_LENGTH  : integer;
    variable I_CID            : integer;
    variable CMP_DATA         : std_ulogic_vector(IN_DATA'range);
    variable RD_DATA, RD_MASK : std_ulogic_vector(IN_DATA'range);

    variable RD_BUFFER_PTR   : INT_VECTOR(N_COMPLETION_ID-1 downto 0);
    variable RD_CPL_RAM_ADDR : DATA32(N_COMPLETION_ID-1 downto 0);
    variable vERR            : integer;
    variable TMP_I           : integer;
    variable TMP32           : std_ulogic_vector(31 downto 0);

  begin

    if(MODE_PRIMARY) then
      INBOUND_LABEL := "L2P";
    else
      INBOUND_LABEL := "P2L";
    end if;

    COMPLETION_ID := 0;
    for I in 0 to N_COMPLETION_ID-1 loop
      INBOUND_READ_REQUEST_ARRAY(I).STATE <= false;
      RD_CPL_RAM_ADDR(I)                  := (others => '0');
    end loop;

    RD_BUFFER_PTR := (others => 0);

    loop
      CMD_RD_DATA_VALID <= '0';

      wait until(ICLK'event and (ICLK = '1'));

      if((IN_DFRAME and IN_VALID) = '1') then  -- Start of a packet
        --*-------------------------------------------------------------
        --* Header Phase
        --*-------------------------------------------------------------
        HEADER        := IN_DATA;
        HEADER_TC     := IN_DATA(31 downto 29);
        HEADER_S      := IN_DATA(28);
        HEADER_TYPE   := IN_DATA(27 downto 24);
        HEADER_LBE    := IN_DATA(23 downto 20);
        HEADER_FBE    := IN_DATA(19 downto 16);
        HEADER_STAT   := IN_DATA(17 downto 16);
        HEADER_L      := IN_DATA(15);
        HEADER_V      := IN_DATA(12);
        HEADER_CID    := IN_DATA(11 downto 10);
        HEADER_LENGTH := IN_DATA(9 downto 0);
        if(Q_IN_DFRAME = '1') then
          write(OUTPUT_LINE, "-- ERROR: " & INSTANCE_LABEL);
          write(OUTPUT_LINE, NOW);
          write(OUTPUT_LINE, ": " & INBOUND_LABEL &" Packet is Improperly Framed (Doesn't start with a L-to-H transition on DFRAME)");
          writeline(OUT_FILE, OUTPUT_LINE);
        end if;

        COMPLETION_ID := to_int(HEADER_CID);

        --*-------------------------------------------------------------
        --* Address Phase
        --*-------------------------------------------------------------
        if((HEADER_TYPE = "0000") or (HEADER_TYPE = "0010")) then  -- 32 bit address
          wait until(ICLK'event and (ICLK = '1') and (IN_VALID = '1'));
          HEADER_ADDR_LOW := IN_DATA;
          HEADER_ADDR_HI  := (others => '0');

        elsif((HEADER_TYPE = "0001") or (HEADER_TYPE = "0011")) then  -- 64 bit address
          wait until(ICLK'event and (ICLK = '1') and (IN_VALID = '1'));
          HEADER_ADDR_HI  := IN_DATA;
          wait until(ICLK'event and (ICLK = '1') and (IN_VALID = '1'));
          HEADER_ADDR_LOW := IN_DATA;

        elsif(HEADER_TYPE = "0100") then  -- Completion: No Data
--                                      wait until(ICLK'event and (ICLK = '1') and (IN_VALID = '1'));

        elsif(HEADER_TYPE = "0101") then  -- Completion: With Data

        else
          write(OUTPUT_LINE, "-- ERROR: " & INSTANCE_LABEL);
          write(OUTPUT_LINE, NOW);
          write(OUTPUT_LINE, string'(" Unrecognized TYPE=0x"));
          write_hex_vector(OUTPUT_LINE, HEADER);
          write(OUTPUT_LINE, string'(" --> Header Word=0x"));
          write_hex_vector(OUTPUT_LINE, HEADER);
          writeline(OUT_FILE, OUTPUT_LINE);
        end if;

        if(PRIMARY) then
          BFM_BAR_HIT(0) := (((HEADER_ADDR_LOW and BFM_BAR_ATTRIBUTE_ARRAY(0).MASK) = BFM_BAR_ATTRIBUTE_ARRAY(0).BASE(31 downto 0))
                             and (HEADER_ADDR_HI = BFM_BAR_ATTRIBUTE_ARRAY(0).BASE(63 downto 32)));
          BFM_BAR_HIT(1) := (((HEADER_ADDR_LOW and BFM_BAR_ATTRIBUTE_ARRAY(1).MASK) = BFM_BAR_ATTRIBUTE_ARRAY(1).BASE(31 downto 0))
                             and (HEADER_ADDR_HI = BFM_BAR_ATTRIBUTE_ARRAY(1).BASE(63 downto 32)));
        else
          if(HEADER_ADDR_LOW(1 downto 0) = "00") then
            BFM_BAR_HIT(0) := true;
            BFM_BAR_HIT(1) := false;
          elsif(HEADER_ADDR_LOW(1 downto 0) = "01") then
            BFM_BAR_HIT(0) := false;
            BFM_BAR_HIT(1) := true;
          else
            BFM_BAR_HIT(0) := false;
            BFM_BAR_HIT(1) := false;
          end if;
        end if;

        RAM_ADDR := to_mvl(BFM_BAR_HIT(1)) & HEADER_ADDR_LOW(RAM_ADDR'high-1 downto 2) & "00";
        RAM_BE   := HEADER_FBE;
        FIRST_DW := true;

        --*-------------------------------------------------------------
        --* Data Phase - Write
        --*-------------------------------------------------------------
        if((HEADER_TYPE = "0010") or (HEADER_TYPE = "0011")) then  -- Write Packet
          if(BFM_BAR_HIT(0) or BFM_BAR_HIT(1)) then
            L1 : loop
              wait until(ICLK'event and (ICLK = '1') and (IN_VALID = '1'));
              if(FIRST_DW) then
                RAM_BE := HEADER_FBE;
              elsif(IN_DFRAME /= '1') then
                RAM_BE := HEADER_LBE;
              else
                RAM_BE := (others => '1');
              end if;

              for i in 0 to RAM_BE'high loop
                if (RAM_BE(i) = '1') then
                  RAM_DATA := IN_DATA(8*i+7 downto 8*i);


--write(OUTPUT_LINE, string'("-- " & INSTANCE_LABEL & INBOUND_LABEL &" Mem_Write("));
--write_hex_vector(OUTPUT_LINE, RAM_ADDR);
--write(OUTPUT_LINE, string'(", "));
--write_hex_vector(OUTPUT_LINE, RAM_DATA);
--write(OUTPUT_LINE, string'(")"));
--writeline(OUT_FILE, OUTPUT_LINE);

                  RAM_WR1      <= true;
                  RAM_ADDR1    <= RAM_ADDR;
                  RAM_WR_DATA1 <= RAM_DATA;
                  RAM_REQ1     <= not RAM_REQ1;
                  wait on RAM_ACK1;

                end if;
                RAM_ADDR := RAM_ADDR + '1';
              end loop;

              FIRST_DW := false;

              if(IN_DFRAME /= '1') then
                exit L1;
              end if;
            end loop;
          else
            write(OUTPUT_LINE, "-- ERROR: " & INSTANCE_LABEL);
            write(OUTPUT_LINE, NOW);
            write(OUTPUT_LINE, (" " & INBOUND_LABEL & " Address didn't match any local BAR: HEADER=0x"));
            write_hex_vector(OUTPUT_LINE, HEADER);
            write(OUTPUT_LINE, string'(", ADDRESS=0x"));
            write_hex_vector(OUTPUT_LINE, HEADER_ADDR_HI);
            write_hex_vector(OUTPUT_LINE, HEADER_ADDR_LOW);
            writeline(OUT_FILE, OUTPUT_LINE);
            wait until(ICLK'event and (ICLK = '1') and (IN_VALID = '1') and (IN_DFRAME = '0'));
          end if;

        elsif((HEADER_TYPE = "0000") or (HEADER_TYPE = "0001")) then  -- Read Request
          --*-------------------------------------------------------------
          --* Process the Read Request
          --*-------------------------------------------------------------
          -- INBOUND_READ_REQUEST_ARRAY(i).ADDRESS
          -- INBOUND_READ_REQUEST_ARRAY(i).LENGTH
          -- INBOUND_READ_REQUEST_ARRAY(i).STATE
          -- constant N_INBOUND_RD_OUTSTANDING : integer := 3;  -- Maximim number of outstanding reads
          -- constant N_COMPLETION_ID  : integer := 4;  -- Maximim number of completion IDs

          if(INBOUND_READ_REQUEST_CPL_STATE(COMPLETION_ID) /= INBOUND_READ_REQUEST_ARRAY(COMPLETION_ID).STATE) then  -- ERROR: CID is in use
            write(OUTPUT_LINE, "-- ERROR: " & INSTANCE_LABEL);
            write(OUTPUT_LINE, NOW);
            write(OUTPUT_LINE, (" " & INBOUND_LABEL & " Completion ID (CID=" & to_str(COMPLETION_ID) & ") is already in use: HEADER=0x"));
            write_hex_vector(OUTPUT_LINE, HEADER);
            write(OUTPUT_LINE, string'(", ADDRESS=0x"));
            write_hex_vector(OUTPUT_LINE, HEADER_ADDR_HI);
            write_hex_vector(OUTPUT_LINE, HEADER_ADDR_LOW);
            writeline(OUT_FILE, OUTPUT_LINE);
          else

                                        -- Make sure we have a budget for completion IDs
            CID_COUNT := 0;
            for I in 0 to (N_COMPLETION_ID-1) loop
              if(INBOUND_READ_REQUEST_CPL_STATE(I) /= INBOUND_READ_REQUEST_ARRAY(I).STATE) then
                CID_COUNT := CID_COUNT + 1;
              end if;
            end loop;

            if(CID_COUNT < N_INBOUND_RD_OUTSTANDING) then  -- OK to accept the request
              INBOUND_READ_REQUEST_ARRAY(COMPLETION_ID).ADDRESS <= HEADER_ADDR_HI & HEADER_ADDR_LOW;
              INBOUND_READ_REQUEST_ARRAY(COMPLETION_ID).BAR_HIT <= to_mvl(BFM_BAR_HIT(1)) & to_mvl(BFM_BAR_HIT(0));
              INBOUND_READ_REQUEST_ARRAY(COMPLETION_ID).LENGTH  <= to_int(HEADER_LENGTH);
              INBOUND_READ_REQUEST_ARRAY(COMPLETION_ID).STATE   <= not INBOUND_READ_REQUEST_ARRAY(COMPLETION_ID).STATE;
              INBOUND_READ_REQUEST_ARRAY(COMPLETION_ID).TC      <= HEADER_TC;
              INBOUND_READ_REQUEST_ARRAY(COMPLETION_ID).V       <= HEADER_V;
              wait on INBOUND_READ_REQUEST_ARRAY;
            else
              write(OUTPUT_LINE, "-- ERROR: " & INSTANCE_LABEL);
              write(OUTPUT_LINE, NOW);
              write(OUTPUT_LINE, (" " & INBOUND_LABEL & " Number of outstanding read requests exceeded: HEADER=0x"));
              write_hex_vector(OUTPUT_LINE, HEADER);
              write(OUTPUT_LINE, string'(", ADDRESS=0x"));
              write_hex_vector(OUTPUT_LINE, HEADER_ADDR_HI);
              write_hex_vector(OUTPUT_LINE, HEADER_ADDR_LOW);
              writeline(OUT_FILE, OUTPUT_LINE);
            end if;
          end if;

        elsif((HEADER_TYPE = "0100") or (HEADER_TYPE = "0101")) then  -- Read Request
          --*-------------------------------------------------------------
          --* Process the Read Completion
          --*-------------------------------------------------------------

          I_HEADER_LENGTH := to_int(HEADER_LENGTH);
          I_CID           := to_int(HEADER_CID);

          if(RD_BUFFER_PTR(I_CID) = 0) then  -- start of a completion sequence
            RD_CPL_RAM_ADDR(I_CID)(RAM_ADDR'high)            := RD_BUFFER(I_CID).BAR_HIT(1);
            RD_CPL_RAM_ADDR(I_CID)(RAM_ADDR'high-1 downto 0) := RD_BUFFER(I_CID).ADDRESS(RAM_ADDR'high-1 downto 2) & "00";
          end if;

--                                      RAM_ADDR := RD_CPL_RAM_ADDR(I_CID)(RAM_ADDR'range);
-- CHANGE Can get rid of RD_CPL_RAM_ADDR

          loop
            wait until(ICLK'event and (ICLK = '1') and (IN_VALID = '1'));

            I_HEADER_LENGTH := I_HEADER_LENGTH - 1;

--                                              for J in 0 to 3 loop -- Do all 4 bytes of the read
--
--                                                      RAM_ADDR1 <= RAM_ADDR;
--                                                      RAM_REQ1 <= not RAM_REQ1;
--
--                                                      wait on RAM_ACK1;
--
--                                                      CMP_DATA(J*8+7 downto J*8) := RAM_RD_DATA1;
--                                                      RAM_ADDR := RAM_ADDR + '1';
--
--                                              end loop;


            CMP_DATA := IN_DATA;
            CMD_RD_DATA <= IN_DATA;
            CMD_RD_DATA_VALID <= '1';
            wait for 1ns;
                       
            CMD_RD_DATA_VALID <= '0';

            
            RD_DATA := RD_BUFFER(I_CID).DATA(RD_BUFFER_PTR(I_CID));
            RD_MASK := RD_BUFFER(I_CID).MASK(RD_BUFFER_PTR(I_CID));

            read_cmp(OUTPUT_LINE, (INSTANCE_LABEL & "BFM Master Mode Read "), CMP_DATA, RD_DATA, RD_MASK, vERR);
            if(vERR /= 0) then
              TMP_I := RD_BUFFER_PTR(I_CID)*4;
              TMP_I := to_int(RD_BUFFER(I_CID).ADDRESS(31 downto 0)) + TMP_I;
              TMP32 := to_vector(TMP_I, TMP32'length);
              write(OUTPUT_LINE, LF);
              write(OUTPUT_LINE, string'("Header=0x"));
              write_hex_vector(OUTPUT_LINE, HEADER);
              write(OUTPUT_LINE, string'(", CID=0x"));
              write_hex_vector(OUTPUT_LINE, HEADER_CID);
              write(OUTPUT_LINE, string'(", ADDRESS=0x"));
              write_hex_vector(OUTPUT_LINE, RD_BUFFER(I_CID).ADDRESS(63 downto 32));
              write_hex_vector(OUTPUT_LINE, RD_BUFFER(I_CID).DATA(0));

              write_hex_vector(OUTPUT_LINE, TMP32);
              writeline(OUT_FILE, OUTPUT_LINE);
            end if;

            RD_BUFFER_PTR(I_CID) := RD_BUFFER_PTR(I_CID) + 1;

            if(IN_DFRAME = '0') or (I_HEADER_LENGTH = 0) then
              exit;
            end if;

          end loop;

          RD_CPL_RAM_ADDR(I_CID)(RAM_ADDR'range) := RAM_ADDR;


          if not((IN_DFRAME = '0') and (I_HEADER_LENGTH = 0)) then
            write(OUTPUT_LINE, "-- ERROR: " & INSTANCE_LABEL);
            write(OUTPUT_LINE, NOW);
            write(OUTPUT_LINE, (" " & INBOUND_LABEL & "_DFRAME is not in agreement with the header length value: HEADER=0x"));
            write_hex_vector(OUTPUT_LINE, HEADER);
            write(OUTPUT_LINE, string'(", ADDRESS=0x"));
            write_hex_vector(OUTPUT_LINE, HEADER_ADDR_HI);
            write_hex_vector(OUTPUT_LINE, HEADER_ADDR_LOW);
            writeline(OUT_FILE, OUTPUT_LINE);
          end if;

          if(HEADER_L = '1') then
            OUTBOUND_READ_REQUEST_CPL_STATE(I_CID) <= RD_BUFFER(I_CID).STATE;
            RD_BUFFER_PTR(I_CID)                   := 0;
          end if;

          

        end if;
      end if;


    end loop;
  end process;


--#########################################################################--
--
-- BFM Internal RAM Handler 
--
-- Handles SRAM access from multiple processes
--
--#########################################################################--

  process
    --file      OUT_FILE : text is out "STD_OUTPUT";
--              file      OUT_FILE : text open write_mode is "STD_OUTPUT";
--              variable  OUTPUT_LINE     : line;
    variable SRAM_BFM  : MEM_ID_TYPE;
    variable vRAM_DATA : std_ulogic_vector(7 downto 0);
    
  begin

    -- Initialize RAM model
    SRAM_BFM := SRAM_INITIALIZE
                (name          => "BFM RAM",
                  length       => (2**N_RAM_MAX),
                  width        => 8,
                  default_word => std_ulogic_vector'("XXXXXXXX")
                  );

    RAM_ACK0 <= false;
    RAM_ACK1 <= false;

    loop

      wait on RAM_REQ0, RAM_REQ1;

      if(RAM_REQ0'event) then
        if(RAM_WR0) then
          Mem_Write
            (mem_id   => SRAM_BFM,
              address => RAM_ADDR0,
              data    => RAM_WR_DATA0
              );
          wait for 1ns;
        else
          Mem_Read
            (mem_id   => SRAM_BFM,
              address => RAM_ADDR0,
              data    => vRAM_DATA
              );
          RAM_RD_DATA0 <= vRAM_DATA;
          wait for 1ns;
        end if;

        RAM_ACK0 <= not RAM_ACK0;
--                              wait until(RAM_REQ0'event);
      end if;

      if(RAM_REQ1'event) then
        if(RAM_WR1) then
          Mem_Write
            (mem_id   => SRAM_BFM,
              address => RAM_ADDR1,
              data    => RAM_WR_DATA1
              );
        else
          Mem_Read
            (mem_id   => SRAM_BFM,
              address => RAM_ADDR1,
              data    => vRAM_DATA
              );
          RAM_RD_DATA1 <= vRAM_DATA;
        end if;
        RAM_ACK1 <= not RAM_ACK1;
--                              wait until(RAM_REQ1'event);
      end if;

    end loop;
  end process;

--#########################################################################--
--
-- Random # generator
--
-- Uses PRBS-23
--
--#########################################################################--

  process(CLK0o)
    variable RNDOUT : integer;
    variable SEED   : integer;
  begin
    if(RSTOUTo = '1') then
      SEED := 5000;
    elsif(CLK0o'event and CLK0o = '1') then
      get_random(SEED, 0, 100, RNDOUT);
      RANDOM_NUMBER <= RNDOUT;
    end if;
  end process;

--#########################################################################--
--
-- L2P Bus Sniffer
--
--#########################################################################--
  process
  begin
    wait until(L2P_CLKpi'event);
    L2P_CLKi_90 <= transport L2P_CLKpi after (T_LCLKi/4);
  end process;

  process
    file OUT_FILE        : text open write_mode is "NullFile";
    variable OUTPUT_LINE : line;
    variable vHEADER     : std_ulogic_vector(31 downto 0);
    variable vADDRESS    : std_ulogic_vector(63 downto 0);
    variable vTYPE       : std_ulogic_vector(3 downto 0);
    variable START       : time;
  begin
    wait until(L2P_CLKi_90'event and (L2P_CLKi_90 = '1') and (L2P_VALIDi = '1') and MODE_PRIMARY);
    START := NOW;
    if(L2P_DFRAMEi = '1') then
      vHEADER(15 downto 0)  := L2P_DATAi;
      wait until(L2P_CLKi_90'event and (L2P_CLKi_90 = '0'));
      vHEADER(31 downto 16) := L2P_DATAi;
      vTYPE                 := vHEADER(27 downto 24);
      vADDRESS              := (others => '0');

      -- Upper Address
      if((vTYPE = "0001") or (vTYPE = "0011")) then  -- address is 64 bits
        wait until(L2P_CLKi_90'event and (L2P_CLKi_90 = '1') and (L2P_VALIDi = '1'));
        vADDRESS(47 downto 32) := L2P_DATAi;
        wait until(L2P_CLKi_90'event and (L2P_CLKi_90 = '0'));
        vADDRESS(63 downto 48) := L2P_DATAi;
      end if;
      -- Lower Address
      if((vTYPE = "0000") or (vTYPE = "0001") or (vTYPE = "0010") or (vTYPE = "0011") or (vTYPE = "0100")) then  -- address is required
        wait until(L2P_CLKi_90'event and (L2P_CLKi_90 = '1') and (L2P_VALIDi = '1'));
        vADDRESS(15 downto 0)  := L2P_DATAi;
        wait until(L2P_CLKi_90'event and (L2P_CLKi_90 = '0'));
        vADDRESS(31 downto 16) := L2P_DATAi;
      end if;

--                      write(OUTPUT_LINE, ("-->> L2P Packet: " & to_string(START)));
--                      writeline(OUT_FILE, OUTPUT_LINE);
      write(OUTPUT_LINE, string'("-->>>> L2P Header: "));

      case vTYPE is
        when "0000" | "0001" =>
          write(OUTPUT_LINE, string'("(L2P Master Read Request)"));
          write(OUTPUT_LINE, string'(", FBE=0x"));
          write_hex_vector(OUTPUT_LINE, vHEADER(23 downto 20));
          write(OUTPUT_LINE, string'(", LBE=0x"));
          write_hex_vector(OUTPUT_LINE, vHEADER(19 downto 16));
          write(OUTPUT_LINE, string'(", V=" & to_str(vHEADER(12))));
          write(OUTPUT_LINE, string'(", CID="));
          write_hex_vector(OUTPUT_LINE, vHEADER(11 downto 10));
          write(OUTPUT_LINE, string'(", LENGTH=0x"));
          write_hex_vector(OUTPUT_LINE, vHEADER(9 downto 0));
--                                      writeline(OUT_FILE, OUTPUT_LINE);
          write(OUTPUT_LINE, string'("-->>>> Address: 0x"));
          write_hex_vector(OUTPUT_LINE, vADDRESS);
          write(OUTPUT_LINE, string'(" @ "));
          write(OUTPUT_LINE, START);
          writeline(OUT_FILE, OUTPUT_LINE);

        when "0010" | "0011" =>
          write(OUTPUT_LINE, string'("(L2P Master Write)"));
          write(OUTPUT_LINE, string'(", FBE=0x"));
          write_hex_vector(OUTPUT_LINE, vHEADER(23 downto 20));
          write(OUTPUT_LINE, string'(", LBE=0x"));
          write_hex_vector(OUTPUT_LINE, vHEADER(19 downto 16));
          write(OUTPUT_LINE, string'(", V=" & to_str(vHEADER(12))));
          write(OUTPUT_LINE, string'(", LENGTH=0x"));
          write_hex_vector(OUTPUT_LINE, vHEADER(9 downto 0));
--                                      writeline(OUT_FILE, OUTPUT_LINE);
          write(OUTPUT_LINE, string'("-->>>> Address: 0x"));
          write_hex_vector(OUTPUT_LINE, vADDRESS);
          write(OUTPUT_LINE, string'(" @ "));
          write(OUTPUT_LINE, START);
          writeline(OUT_FILE, OUTPUT_LINE);

        when "0100" =>
          write(OUTPUT_LINE, string'("(L2P Target Read Completion Without Data)"));
          write(OUTPUT_LINE, string'(", STAT="));
          write_hex_vector(OUTPUT_LINE, vHEADER(17 downto 16));
          write(OUTPUT_LINE, string'(", L=" & to_str(vHEADER(15))));
          write(OUTPUT_LINE, string'(", V=" & to_str(vHEADER(12))));
          write(OUTPUT_LINE, string'(", CID="));
          write_hex_vector(OUTPUT_LINE, vHEADER(11 downto 10));
          write(OUTPUT_LINE, string'(", LENGTH=0x"));
          write_hex_vector(OUTPUT_LINE, vHEADER(9 downto 0));
          write(OUTPUT_LINE, string'(" @ "));
          write(OUTPUT_LINE, START);
          writeline(OUT_FILE, OUTPUT_LINE);

        when "0101" =>
          write(OUTPUT_LINE, string'("(L2P Target Read Completion With Data)"));
          write(OUTPUT_LINE, string'(", STAT="));
          write_hex_vector(OUTPUT_LINE, vHEADER(17 downto 16));
          write(OUTPUT_LINE, string'(", L=" & to_str(vHEADER(15))));
          write(OUTPUT_LINE, string'(", V=" & to_str(vHEADER(12))));
          write(OUTPUT_LINE, string'(", CID="));
          write_hex_vector(OUTPUT_LINE, vHEADER(11 downto 10));
          write(OUTPUT_LINE, string'(", LENGTH=0x"));
          write_hex_vector(OUTPUT_LINE, vHEADER(9 downto 0));
          write(OUTPUT_LINE, string'(" @ "));
          write(OUTPUT_LINE, START);
          writeline(OUT_FILE, OUTPUT_LINE);

        when others =>
          write(OUTPUT_LINE, string'("(Undefined)"));
          write(OUTPUT_LINE, string'(" @ "));
          write(OUTPUT_LINE, START);
          writeline(OUT_FILE, OUTPUT_LINE);
          assert false report "---- ERROR: Unsupported TYPE in L2P Header"
            severity error;
      end case;



      if(L2P_DFRAMEi = '1') then
        wait until(L2P_CLKi_90'event and (L2P_CLKi_90 = '1') and (L2P_VALIDi = '1') and (L2P_DFRAMEi = '0'));
      end if;

    else
      write(OUTPUT_LINE, string'("-- ERROR: L2P Bus: P2L_VALID asserted without P2L_DFRAME @"));
      write(OUTPUT_LINE, START);
      writeline(OUT_FILE, OUTPUT_LINE);
    end if;
  end process;

--#########################################################################--
--
-- P2L Bus Sniffer
--
--#########################################################################--
  process
    file OUT_FILE        : text open write_mode is "NullFile";
    variable OUTPUT_LINE : line;
    variable vHEADER     : std_ulogic_vector(31 downto 0);
    variable vADDRESS    : std_ulogic_vector(63 downto 0);
    variable vTYPE       : std_ulogic_vector(3 downto 0);
    variable START       : time;
  begin
    wait until(P2L_CLKpi'event and (P2L_CLKpi = '1') and (P2L_VALIDi = '1') and MODE_PRIMARY);
    START := NOW;
    if(P2L_DFRAMEi = '1') then
      vHEADER(15 downto 0)  := P2L_DATAi;
      wait until(P2L_CLKpi'event and (P2L_CLKpi = '0'));
      vHEADER(31 downto 16) := P2L_DATAi;
      vTYPE                 := vHEADER(27 downto 24);
      vADDRESS              := (others => '0');

      -- Upper Address
      if((vTYPE = "0001") or (vTYPE = "0011")) then  -- address is 64 bits
        wait until(P2L_CLKpi'event and (P2L_CLKpi = '1') and (P2L_VALIDi = '1'));
        vADDRESS(47 downto 32) := P2L_DATAi;
        wait until(P2L_CLKpi'event and (P2L_CLKpi = '0'));
        vADDRESS(63 downto 48) := P2L_DATAi;
      end if;
      -- Lower Address
      if((vTYPE = "0000") or (vTYPE = "0001") or (vTYPE = "0010") or (vTYPE = "0011") or (vTYPE = "0100")) then  -- address is required
        wait until(P2L_CLKpi'event and (P2L_CLKpi = '1') and (P2L_VALIDi = '1'));
        vADDRESS(15 downto 0)  := P2L_DATAi;
        wait until(P2L_CLKpi'event and (P2L_CLKpi = '0'));
        vADDRESS(31 downto 16) := P2L_DATAi;
      end if;

      write(OUTPUT_LINE, string'("--<<<< P2L Header: "));

      case vTYPE is
        when "0000" | "0001" =>
          write(OUTPUT_LINE, string'("(P2L Target Read Request)"));
          write(OUTPUT_LINE, string'(", FBE=0x"));
          write_hex_vector(OUTPUT_LINE, vHEADER(23 downto 20));
          write(OUTPUT_LINE, string'(", LBE=0x"));
          write_hex_vector(OUTPUT_LINE, vHEADER(19 downto 16));
          write(OUTPUT_LINE, string'(", V=" & to_str(vHEADER(12))));
          write(OUTPUT_LINE, string'(", CID="));
          write_hex_vector(OUTPUT_LINE, vHEADER(11 downto 10));
          write(OUTPUT_LINE, string'(", LENGTH=0x"));
          write_hex_vector(OUTPUT_LINE, vHEADER(9 downto 0));
          write(OUTPUT_LINE, string'("--<<<< Address: 0x"));
          write_hex_vector(OUTPUT_LINE, vADDRESS);
          write(OUTPUT_LINE, string'(" @ "));
          write(OUTPUT_LINE, START);
          writeline(OUT_FILE, OUTPUT_LINE);

        when "0010" | "0011" =>
          write(OUTPUT_LINE, string'("(P2L Target Write)"));
          write(OUTPUT_LINE, string'(", FBE=0x"));
          write_hex_vector(OUTPUT_LINE, vHEADER(23 downto 20));
          write(OUTPUT_LINE, string'(", LBE=0x"));
          write_hex_vector(OUTPUT_LINE, vHEADER(19 downto 16));
          write(OUTPUT_LINE, string'(", V=" & to_str(vHEADER(12))));
          write(OUTPUT_LINE, string'(", LENGTH=0x"));
          write_hex_vector(OUTPUT_LINE, vHEADER(9 downto 0));
          write(OUTPUT_LINE, string'("--<<<< Address: 0x"));
          write_hex_vector(OUTPUT_LINE, vADDRESS);
          write(OUTPUT_LINE, string'(" @ "));
          write(OUTPUT_LINE, START);
          writeline(OUT_FILE, OUTPUT_LINE);

        when "0100" =>
          write(OUTPUT_LINE, string'("(P2L Master Read Completion Without Data)"));
          write(OUTPUT_LINE, string'(", STAT="));
          write_hex_vector(OUTPUT_LINE, vHEADER(17 downto 16));
          write(OUTPUT_LINE, string'(", L=" & to_str(vHEADER(15))));
          write(OUTPUT_LINE, string'(", V=" & to_str(vHEADER(12))));
          write(OUTPUT_LINE, string'(", CID="));
          write_hex_vector(OUTPUT_LINE, vHEADER(11 downto 10));
          write(OUTPUT_LINE, string'(", LENGTH=0x"));
          write_hex_vector(OUTPUT_LINE, vHEADER(9 downto 0));
          write(OUTPUT_LINE, string'(" @ "));
          write(OUTPUT_LINE, START);
          writeline(OUT_FILE, OUTPUT_LINE);

        when "0101" =>
          write(OUTPUT_LINE, string'("(P2L Master Read Completion With Data)"));
          write(OUTPUT_LINE, string'(", STAT="));
          write_hex_vector(OUTPUT_LINE, vHEADER(17 downto 16));
          write(OUTPUT_LINE, string'(", L=" & to_str(vHEADER(15))));
          write(OUTPUT_LINE, string'(", V=" & to_str(vHEADER(12))));
          write(OUTPUT_LINE, string'(", CID="));
          write_hex_vector(OUTPUT_LINE, vHEADER(11 downto 10));
          write(OUTPUT_LINE, string'(", LENGTH=0x"));
          write_hex_vector(OUTPUT_LINE, vHEADER(9 downto 0));
          write(OUTPUT_LINE, string'(" @ "));
          write(OUTPUT_LINE, START);
          writeline(OUT_FILE, OUTPUT_LINE);

        when others =>
          write(OUTPUT_LINE, string'("(Undefined)"));
          write(OUTPUT_LINE, string'(" @ "));
          write(OUTPUT_LINE, START);
          writeline(OUT_FILE, OUTPUT_LINE);
          assert false report "---- ERROR: Unsupported TYPE in P2L Header"
            severity error;
      end case;


      if(P2L_DFRAMEi = '1') then
        wait until(P2L_CLKpi'event and (P2L_CLKpi = '1') and (P2L_VALIDi = '1') and (P2L_DFRAMEi = '0'));
      end if;

    else
      write(OUTPUT_LINE, string'("-- ERROR: P2L Bus: P2L_VALID asserted without P2L_DFRAME @"));
      write(OUTPUT_LINE, START);
      writeline(OUT_FILE, OUTPUT_LINE);
    end if;
  end process;


end MODEL;


