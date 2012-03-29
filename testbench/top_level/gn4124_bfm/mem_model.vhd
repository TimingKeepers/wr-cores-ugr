library ieee;
use ieee.std_logic_1164.all;
library std;
use std.textio.all;

PACKAGE mem_model is

    -- 2d array to store data for each row
    type row_matrix  is array (NATURAL RANGE <>, NATURAL RANGE <>) of UX01;
    type rowptr_type is access row_matrix;
    -- record for for storing refresh and memory ptr for each row
    type row_data_type is
        record
            last_refresh : time;                -- last time row was refreshed
            rowptr       : rowptr_type;         -- ptr to 2d matrix with data
            all_xs       : BOOLEAN;             -- true if row is filled with Xs
        end record;
    -- array of refresh times and memory ptrs for the rows
    type row_data is array (NATURAL RANGE <>) of row_data_type;
    type row_data_ptr_type is access row_data;
    type strptr is access string;
    type default_ptr_type is access std_logic_vector;
    type mem_type is (DRAM, SRAM, ROM);   -- memory types
    
    -- record defining memory and holding general information

    type mem_id_rtype is

        record
            memory_type    : mem_type;            -- memory type
            refresh_period : time;                -- refresh period
            last_init      : time;                -- last time a refresh was performed
            counter        : NATURAL;             -- refresh counter
            name           : strptr;              -- pointer to memory name
            rows           : POSITIVE;            -- # of rows
            columns        : POSITIVE;            -- # of columns
            width          : POSITIVE;            -- # word length
            length         : POSITIVE;            -- # of memory locations
            row_data_ptr   : row_data_ptr_type;   -- ptr to memory ptrs.
            default        : default_ptr_type;    -- ptr to default memory word value
        end record;

    type mem_id_type is access mem_id_rtype;

    
--********************************************************************************
    --    Function Name   :  SRAM_Initialize
    --
    --    Purpose         :  To create the data structure used to store a
    --                       static RAM and to initialize it
    --
    --    Parameters      :  name    - string used to represent the memory
    --                       length  - the number of "words" in the memory
    --                       width   - the length of a "word" of memory
    --                       default_word   - value to which each word of memory
    --                                        should be initialized
    --
    --    RETURNED VALUE  :  mem_id_type - ptr to memory record
    --
    --    NOTE            :  initially the data structure is empty with no
    --                       space being allocated for the memory
    --
    --    Use             :  sram_l1 := SRAM_Initialize ("lsb_of_RAM",1048576,1,"0");
--********************************************************************************
        
	impure    Function SRAM_Initialize ( Constant name            : IN string;
                               Constant length          : IN POSITIVE;
                               Constant width           : IN POSITIVE;
                               Constant default_word    : IN std_ulogic_vector
                             ) return mem_id_type;

--********************************************************************************
    --    Procedure Name  :  Mem_Read
    --
    --    Purpose         :  To read a "word" from memory
    --
    --    Parameters      :  mem_id    -  ptr to memory data structure
    --                       address   -  address to read from
    --                       data      -  contents of memory location
    --                       
    --
    --    NOTE            :  a read refreshes row of a DRAM
    --
    --    Use             :  Mem_Read (ROM1, "100100111", data_bus);
--********************************************************************************

    Procedure Mem_Read (  Variable mem_id    : INOUT mem_id_type;
                          Constant address   : IN std_ulogic_vector;
                          Variable data      : OUT std_ulogic_vector
                       );

--********************************************************************************
    --    Procedure Name  :  Mem_Write
    --
    --    Purpose         :  To write a "word" to memory
    --
    --    Parameters      :  mem_id    -  ptr to memory data structure
    --                       address   -  address to read from
    --                       data      -  "word" to be written to memory
    --
    --    NOTE            :  a write refreshes row of a DRAM
    --
    --    Use             :  Mem_Write (ROM1, "100100111", "10X1");
--********************************************************************************
        
    Procedure Mem_Write (  Variable mem_id    : INOUT mem_id_type;
                           Constant address   : IN std_ulogic_vector;
                           Constant data      : IN std_ulogic_vector
                        );

END mem_model;






PACKAGE BODY mem_model is

    Type D1_b_ulogic_type is array(bit) of std_ulogic;
    type hex_ray is array(1 to 16) of character;
    type IDENTIFIER is (HEX_NUM1, COMMENT1, WIDTH1, DEFAULT1, COLON1, DOTDOT1, BLANK1, SYN_ERROR1);
    type digit_to_hex_type is array(0 to 15) of character;

    -- mentor doesn't like the subtype UX01  -  "resolved sybyte cannot be used as a discrete range"
    type UX01_1DRAY is array(std_ulogic range 'U' to '1') of bit;

    -------------------------------------------------------------------------------------------
    --  THE FOLLOWING CONSTANTS MAY BE CHANGED BY THE USER TO CUSTOMIZE STD_MEMPAK
    -------------------------------------------------------------------------------------------

    -- defines the number of bits used to represent an integer on the machine used to run the vhdl simulator
    CONSTANT IntegerBitLength : INTEGER := 32;

    -- defines the maximum length of strings in this package

    CONSTANT MAX_STR_LEN : NATURAL := 256;

    -- constants used to map X's and U's  in an address to valid values
    
    CONSTANT ADDRESS_X_MAP : BIT := '1';
    CONSTANT ADDRESS_U_MAP : BIT := '1';
    CONSTANT ADDRESS_MAP : UX01_1DRAY :=
                           (ADDRESS_U_MAP, ADDRESS_X_MAP, '0', '1');

    -- constants used to map X's and U's in memory locations to a bit value 
    -- when a bit or a bit_vector is returned by the memory read operation
    
    CONSTANT DATA_X_MAP : BIT := '1';
    CONSTANT DATA_U_MAP : BIT := '1';
    CONSTANT DATA_MAP : UX01_1DRAY :=
                        (DATA_U_MAP, DATA_X_MAP, '0', '1');

    -- constants setting collumn size of SRAM's and ROM's so that entire
    -- memory does not have to be allocated if it is not used.
    
    CONSTANT SRAM_COL_SIZE : NATURAL := 1024;
    CONSTANT ROM_COL_SIZE : NATURAL := 1024;

    -- constant used to enable/disable certain warning assertions
    CONSTANT MEM_WARNINGS_ON : BOOLEAN := TRUE;

    -- constant used to determine how many words per line to output when doing a memory dump
    CONSTANT WORDS_PER_LINE : POSITIVE := 16;


    ----------------------------------------------------------------------------------------------------------
    -- CONSTANTS THAT SHOULD NOT BE MODIFIED
    -- These are used by the package to perform various conversions, comparisions, etc.
    ----------------------------------------------------------------------------------------------------------

    CONSTANT DIGIT_TO_HEX : digit_to_hex_type := ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
    CONSTANT hex : hex_ray := ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
    CONSTANT bit_to_std_ulogic : D1_b_ulogic_type := ('0', '1');
    CONSTANT SPACESTR : STRING(1 to 20) := "                    ";
    CONSTANT SPACE : CHARACTER :=  ' ';
    CONSTANT TAB   : CHARACTER := HT;

    


function StrLen1 ( Constant l_str : IN string ) return NATURAL is

Variable alias_l_str : string(1 to l_str'length) := l_str;
Variable i : integer := 1;

begin
    while ( (i <= l_str'length) and (alias_l_str(i) /= NUL) ) loop
        i := i + 1;
    end loop;
    i := i - 1;
    return i;
end;    

function to_str (Constant dd : IN std_logic) return Character is

begin
    case dd is
        when '1' => return '1';
        when '0' => return '0';
        when 'U' => return 'U';
        when 'X' => return 'X';
        when 'L' => return 'L';
        when 'H' => return 'H';
        when '-' => return '-';
        when 'Z' => return 'Z';
        when 'W' => return 'W';
    end case;
end;

function to_str (Constant dd : IN bit) return character is

begin
  if dd = '0' then
      return '0';
  else
      return '1';
  end if;
end;
    
function i_to_str (Constant int : IN integer) return string is

Constant length : integer := 33;
Variable i, len, pos : integer;
Variable str : string (1 to length);
Variable tint : integer := int;
Variable temp : Character;
Variable negative : BOOLEAN := FALSE;

begin
   for i in 1 to  length loop
     str(i) := ' ';
   end loop;
   if (tint < 0 ) then
      tint := -tint;
      negative := TRUE;
   end if;
   i := length;
   while (  (i >= 1 ) and (tint /= 0)) loop
       str(i) := CHARACTER'Val(48 + (tint mod 10));
       tint := tint/10;
       i := i - 1;
   end loop;
   if (NEGATIVE) then
      str(i) := '-';
      i := i - 1;
   end if;
   len := length - i;
   pos := i + 1;
   for i in 1 to len loop
     str(i) := str(pos);
     pos := pos + 1;
   end loop;
   if (len = 0) then
       len := 1;
       str(1) := '0';
   end if;
   return  (str(1 to len));
   
end;

function v_to_str  (Constant vect : IN bit_vector) return string is

Variable str : string( 1 to vect'length);
Variable alias_vect : bit_vector(1 to vect'length) := vect;
Variable i : integer;

begin
    for i in 1 to vect'length loop
        case alias_vect(i) is
          when '1' => str(i) := '1';
          when '0' => str(i) := '0';
        end case;
    end loop;
    return(str);
end;


function v_to_str  (Constant vect : IN std_logic_vector) return string is

Variable str : string( 1 to vect'length);
Variable alias_vect : std_logic_vector(1 to vect'length) := vect;
Variable i : integer;

begin
    for i in 1 to vect'length loop
        case alias_vect(i) is
          when '1' => str(i) := '1';
          when '0' => str(i) := '0';
          when 'U' => str(i) := 'U';
          when 'X' => str(i) := 'X';
          when 'L' => str(i) := 'L';
          when 'H' => str(i) := 'H';
          when '-' => str(i) := '-';
          when 'Z' => str(i) := 'Z';
          when 'W' => str(i) := 'W';
        end case;
    end loop;
    return(str);
end;

-- used to return a printable string for the memory name
function pstr ( Constant name : in string ) return string is

   variable j : integer;

begin
   j := 1;
   while ( (j < name'length) and (name(j) /= nul) ) loop
      j := j + 1;
   end loop;
   if (name(j) = nul) then
      j := j - 1;
   end if;
   return name(1 to j);
end;



    ---------------------------------------------------------------------------
    --    Function Name   :  minimum
    --
    --    PURPOSE         :  to determine the smaller of two integers
    --                       
    --    Parameters      :  int1 - first integer
    --                    :  int2 - second integer
    --
    --    Returned Value  :  integer - the smaller of int1 and int2
    --
    ---------------------------------------------------------------------------

    Function minimum ( Constant int1 : IN integer;
                       Constant int2 : IN integer
                     ) return integer is

    begin
         if (int1 < int2) then
            return int1;
        else
            return int2;
        end if;
    end;

--+-----------------------------------------------------------------------------
--|     Procedure Name : StrCpy1
--| 1.2.3
--|     Overloading    : None
--|
--|     Purpose        : Copy r_string to l_string.
--|
--|     Parameters     :
--|                      l_str    - output,  STRING, target string
--|                      r_str    - input, STRING, source string
--|
--|     Result         : 
--|
--|     NOTE           : If the length of target string is greater than
--|                      the source string, then target string is padded
--|                      with space characters on the right side and when
--|                      the length of target string is shorter than the 
--|                      length of source string only left most characters
--|                      of the source string will be be copied to the target.
--|                      
--| 
--|     USE            :
--|                      Variable s1: string(1 TO 8);
--|
--|                       StrCpy1(s1, "123456789A");
--|                       s1 will hold "12345678"      
--|-----------------------------------------------------------------------------
    PROCEDURE StrCpy1   ( VARIABLE l_str : OUT STRING;
                          CONSTANT r_str : IN  STRING) IS
       VARIABLE  l_len     : integer := l_str'LENGTH;
       VARIABLE  r_len     : integer := r_str'LENGTH;
       VARIABLE  r         : STRING ( 1 to r_len) := r_str;
       VARIABLE result     : STRING (1 to l_len);
       VARIABLE indx       : integer := 1;
    BEGIN
       assert (l_len > 0)
          report "StrCpy:  target string is of zero length "
          severity ERROR;
              
       while ( (indx <= r_len) and (indx <= l_len) and (r(indx) /= NUL) ) loop
          result(indx) := r(indx);
          indx := indx + 1;
       end loop;
       if (indx <= l_len) then
          result(indx) := NUL;
       end if;
       l_str := result;
       return;
    END StrCpy1;

    

--+---------------------------------------------------------------------------
--|     Procedure Name : fgetline1
--| 
--|     Overloading    : None
--|
--|     Purpose        : To read a line from the input TEXT file and 
--|                      save into a string. 
--|
--|     Parameters     :
--|                         l_str    -- output, STRING,
--|                         stream   -- input, TEXT, input file 
--|
--|     result         : string.
--|
--|     Note:          : The TEXT is defined in the package TEXTIO to be 
--|                      a  file of string.
--|     USE:           :
--|                      VARIABLE  line_buf : string(1 TO 256);
--|                      FILE      in_file : TEXT IS IN "file_text_in.dat";
--|                      
--|                        fgetline1(line_buf, in_file);
--|
--|                       Will read a line  from the file
--|                       file_text_in.dat  and place  into  line_buf.
--|
--|-----------------------------------------------------------------------------
   PROCEDURE fgetline1  ( VARIABLE l_str    : OUT STRING;
                         VARIABLE stream   : IN TEXT;
                         VARIABLE line_ptr : INOUT LINE
                       ) IS
        VARIABLE str_copy   : STRING(1 TO MAX_STR_LEN + 1);
        VARIABLE ch         : character;
        VARIABLE indx       : NATURAL := 0;
   BEGIN
      If ( (line_ptr /= NULL) and (line_ptr'LENGTH > 0) ) then
          NULL;
      elsif ( not ENDFILE(stream) ) then
         READLINE(stream, line_ptr);
      else
         assert NOT MEM_WARNINGS_ON
            report " fgetline1 --- end of file text,  no text read "
            severity WARNING;
         l_str(l_str'left) := NUL;
         return;
      end if;
      while ( (line_ptr /= NULL) and (line_ptr'length /= 0) ) loop
         READ(line_ptr,ch);
         indx := indx + 1;
         str_copy(indx) := ch;
      end loop;
      str_copy(indx + 1) := NUL;
      strcpy1(l_str, str_copy);
      return;
   END;

   

--+----------------------------------------------------------------------------- 
--|     Function Name  : Is_White1
--| hidden.
--|     Overloading    : None 
--|  
--|     Purpose        : Test whether a character is a blank, a tab or 
--|                      a newline character.
--|
--|     Parameters     : 
--|                      c     - input   Character.
--| 
--|     Result         :Booelan -- True if the argument c is a blank or a tab(HT), 
--|                     or a line feed (LF), or carriage return (CR). false otherwise.   
--| 
--| 
--|     See Also       : Is_Space
--|----------------------------------------------------------------------------- 
     FUNCTION  Is_White1  ( CONSTANT c    : IN CHARACTER
                         ) RETURN BOOLEAN IS
         VARIABLE result : BOOLEAN;
     BEGIN
        IF ( (c = ' ') OR (c = HT)  OR (c = CR) OR (c=LF) ) THEN
             result := TRUE;
        ELSE
             result := FALSE;
        END IF;
        RETURN result;

     END; 
    
--+-----------------------------------------------------------------------------
--|     Function Name  : Find_NonBlank1
--| hidden 
--|     Overloading    : None
--|
--|     Purpose        : Find first non_blank character in a string.
--|
--|     Parameters     :
--|                      str_in    - input ,  
--|
--|     Result         : Natural, index of non_blank character. If string
--|                      has all the white character then str_in'LENGTH is
--|                      returned;
--|
--|     NOTE           :
--|
--|     Use            :
--|                      VARIABLE s_flag : String(1 TO 10) := "      TRUE"; 
--|                      VARIABLE idx: Natural 
--|
--|                       idx := Find_NonBlank1 (s_flag);
--|
--|-----------------------------------------------------------------------------
   FUNCTION Find_NonBlank1  ( CONSTANT str_in   : IN STRING
                           ) RETURN NATURAL IS
      VARIABLE str_copy :  STRING (1 TO str_in'LENGTH) := str_in;
      VARIABLE index    :  Natural := 1;
      VARIABLE ch       :  character;
       
    BEGIN
          loop
            EXIT WHEN (index > str_in'LENGTH);
            if Is_White1(str_copy(index)) then
                index := index + 1;
            else
                EXIT;
            end if;
          end loop;
          return index;
--      
-- old code
-- 
--        ch := str_copy(index);
--        while ( ( index < str_in'LENGTH) AND (Is_White1(ch) ) ) LOOP
--        	index := index + 1;
--              ch := str_copy(index);
--        end LOOP;
--        return index;
    END;

--+----------------------------------------------------------------------------- 
--|     Function Name  : To_Upper1
--| 1.
--|     Overloading    : None 
--|  
--|     Purpose        :Convert a string to upper case.
--|
--|     Parameters     : 
--|                      val     - input, string to be converted   
--| 
--|     Result         :  string .
--| 
--| 
--|     See Also       : To_Lower, Is_Upper, Is_Lower
--|----------------------------------------------------------------------------- 
    FUNCTION  To_Upper1  ( CONSTANT  val    : IN String
                         ) RETURN STRING IS
        VARIABLE result   : string (1 TO val'LENGTH) := val;
        VARIABLE ch       : character;
    BEGIN
        FOR i IN 1 TO val'LENGTH LOOP
            ch := result(i);
            EXIT WHEN ((ch = NUL) OR (ch = nul));
            IF ( ch >= 'a' and ch <= 'z') THEN
    	          result(i) := CHARACTER'VAL( CHARACTER'POS(ch) 
                                       - CHARACTER'POS('a')
                                       + CHARACTER'POS('A') );
            END IF;
    	END LOOP;
    	RETURN result;
    END To_Upper1;
    
--+-----------------------------------------------------------------------------
--|     Function Name  : From_HexString1
--| 
--|     Overloading    : None
--|
--|     Purpose        : Convert  from a Hex String to a bit_vector.
--|
--|     Parameters     :
--|                      str     - input ,  Hex string to be converted,
--|
--|     Result         : bit_vector
--|
--|     NOTE           : 
--|
--|     Use            :
--|                      VARIABLE b_vect : bit_vector( 15 DOWNTO 4) ; 
--|
--|                       b_vect := From_HexString1 ("   3DD   1010");
--|                       This statement will set b_vect  equal to "001111011101".
--|
--|-----------------------------------------------------------------------------
    FUNCTION From_HexString1   ( CONSTANT str   : IN STRING
                               ) RETURN bit_vector IS

      CONSTANT len         : Integer := 4 * str'LENGTH;
      CONSTANT hex_dig_len : Integer := 4;
      VARIABLE str_copy    : STRING (1 TO str'LENGTH) := To_Upper1(str);
      VARIABLE index       : Natural;
      VARIABLE ch          : character;
      VARIABLE i, idx      : Integer;
      VARIABLE invalid     : boolean := false;
      VARIABLE r           : bit_vector(1 TO len) ;
      VARIABLE result      : bit_vector(len - 1 DOWNTO 0) ;
      CONSTANT BIT_ZERO    : bit_vector(1 to 4) := "0000";
      CONSTANT BIT_ONE     : bit_vector(1 to 4) := "0001";
      CONSTANT BIT_TWO     : bit_vector(1 to 4) := "0010";
      CONSTANT BIT_THREE   : bit_vector(1 to 4) := "0011";
      CONSTANT BIT_FOUR    : bit_vector(1 to 4) := "0100";
      CONSTANT BIT_FIVE    : bit_vector(1 to 4) := "0101";
      CONSTANT BIT_SIX     : bit_vector(1 to 4) := "0110";
      CONSTANT BIT_SEVEN   : bit_vector(1 to 4) := "0111";
      CONSTANT BIT_EIGHT   : bit_vector(1 to 4) := "1000";
      CONSTANT BIT_NINE    : bit_vector(1 to 4) := "1001";
      CONSTANT BIT_TEN     : bit_vector(1 to 4) := "1010";
      CONSTANT BIT_ELEVEN  : bit_vector(1 to 4) := "1011";
      CONSTANT BIT_TWELVE  : bit_vector(1 to 4) := "1100";
      CONSTANT BIT_THIRTEEN: bit_vector(1 to 4) := "1101";
      CONSTANT BIT_FOURTEEN: bit_vector(1 to 4) := "1110";
      CONSTANT BIT_FIFTEEN : bit_vector(1 to 4) := "1111";
       
    BEGIN
      -- Check for null input
        IF (str'LENGTH = 0) THEN
		assert false
		report " From_HexString1  --- input string has zero length ";
                RETURN "";

        ELSIF  (str(str'LEFT) = NUL) THEN
		assert false
		report " From_HexString1  --- input string has nul character"
                        & " at the LEFT position "
                severity ERROR;
                RETURN "";  -- null  bit_vector
	END IF;
        -- find the position of the first non_white character
        index := Find_NonBlank1(str_copy);
        IF (index > str'length) THEN
		assert false
		report " From_HexString1  --- input string is empty  ";
                RETURN ""; 
        ELSIF (str_copy(index)=NUL) THEN
		assert false report " From_HexString1  -- first non_white character is a NUL ";
                RETURN "";
        END IF;

        i := 0;
        FOR idx IN index TO  str'length LOOP
		ch := str_copy(idx);
                EXIT WHEN ((Is_White1(ch)) OR (ch = NUL));                
		CASE ch IS
	          WHEN '0'        => r(i+1 TO i+ hex_dig_len) := BIT_ZERO;
        	  WHEN '1'        => r(i+1 TO i+ hex_dig_len) := BIT_ONE;
        	  WHEN '2'        => r(i+1 TO i+ hex_dig_len) := BIT_TWO;
        	  WHEN '3'        => r(i+1 TO i+ hex_dig_len) := BIT_THREE;
        	  WHEN '4'        => r(i+1 TO i+ hex_dig_len) := BIT_FOUR;
        	  WHEN '5'        => r(i+1 TO i+ hex_dig_len) := BIT_FIVE;
        	  WHEN '6'        => r(i+1 TO i+ hex_dig_len) := BIT_SIX;
        	  WHEN '7'        => r(i+1 TO i+ hex_dig_len) := BIT_SEVEN;
        	  WHEN '8'        => r(i+1 TO i+ hex_dig_len) := BIT_EIGHT;
        	  WHEN '9'        => r(i+1 TO i+ hex_dig_len) := BIT_NINE;
        	  WHEN 'A' | 'a'  => r(i+1 TO i+ hex_dig_len) := BIT_TEN;
        	  WHEN 'B' | 'b'  => r(i+1 TO i+ hex_dig_len) := BIT_ELEVEN;
        	  WHEN 'C' | 'c'  => r(i+1 TO i+ hex_dig_len) := BIT_TWELVE;
        	  WHEN 'D' | 'd'  => r(i+1 TO i+ hex_dig_len) := BIT_THIRTEEN;
        	  WHEN 'E' | 'e'  => r(i+1 TO i+ hex_dig_len) := BIT_FOURTEEN;
        	  WHEN 'F' | 'f'  => r(i+1 TO i+ hex_dig_len) := BIT_FIFTEEN;
       	  	  WHEN NUL        => exit;
        	  WHEN OTHERS  	  => -- a non  binary value was passed
       	  	                     invalid := TRUE;
         	       		     ASSERT FALSE
                                     REPORT "From_HexString1(str(" & i_to_str(idx) & ") => " 
                                     & ch & ") is an invalid character"
	                	     SEVERITY ERROR;
       	       	END CASE;
                i := i + hex_dig_len;
	END LOOP;
     -- check for invalid character in the string
        if ( invalid ) THEN
           r(1 TO i) := (OTHERS => '0');
        end if;
        result(i - 1 DOWNTO 0) := r(1 TO i);
        return result(i - 1 DOWNTO 0);     -- return slice of result

    END;

    -------------------------------------------------------------------------------
    --     Function Name  : RegFill1
    -- 1.7.4
    --     Overloading    : None
    --
    --     Purpose        : Fill an std_logic_vector with a given value
    --
    --     Parameters     :
    --                      SrcReg     - input  std_logic_vector, the  logic vector to be read.
    --                      DstLength  - input  NATURAL, length of the return logic vector.
    --                      FillVal    - input  std_ulogic, default is '0'
    --
    --     Result         : std_logic_vector of length DstLength
    --
    --     NOTE           : The length of the return logic vector  is specified by the
    --                      parameter 'DstLength'. The input logic vector will
    --                      be  filled with the FillVal
    --
    --     Use            :
    --                      VARIABLE vect : std_logic_vector ( 15 DOWNTO 0 );
    --                      vect := RegFill1 ( "00000101", 16, 'U');
    --
    --     See Also       : SignExtend
   -------------------------------------------------------------------------------
    FUNCTION RegFill1   ( CONSTANT SrcReg      : IN std_logic_vector;
                         CONSTANT DstLength   : IN NATURAL;
                         CONSTANT FillVal     : IN std_ulogic   := '0'
                       ) RETURN std_logic_vector IS
      CONSTANT reslen : INTEGER := DstLength;
      VARIABLE result : std_logic_vector (reslen - 1 DOWNTO 0) := (OTHERS => '0');
      VARIABLE reg    : std_logic_vector (SrcReg'LENGTH - 1 DOWNTO 0) := SrcReg;
    BEGIN
     --  null range check
      IF (SrcReg'LENGTH = 0) THEN
         IF (DstLength = 0) THEN
            ASSERT FALSE
            REPORT " RegFill1 --- input  has null range and" &
                " Destination also has null range. "
            SEVERITY ERROR;
            RETURN result ; 
         ELSE
            ASSERT FALSE
            REPORT " RegFill1 --- input  has null range"
            SEVERITY ERROR;
            result := (OTHERS => FillVal);
            RETURN result ; 
         END IF;
 
      ELSIF (DstLength = 0) THEN
          ASSERT false
          REPORT "RegFill1 --- Destination has null range "
          SEVERITY ERROR;
          RETURN result;   
 
      ELSIF (DstLength <= SrcReg'LENGTH) THEN
                        -- no need to sign extend
         ASSERT (DstLength = SrcReg'LENGTH)
         REPORT " RegFill1 ---  Destination length is less than source"
         SEVERITY ERROR;
         RETURN reg;        -- return the input data without any change
 
      ELSE
           result(SrcReg'LENGTH - 1 DOWNTO 0) := reg;
        -- Fill the MSB's of result with the given fill value.
          For i IN reslen - 1 DOWNTO SrcReg'LENGTH  Loop
             result(i) := FillVal;
          END LOOP;
      END IF;
    
      -- convert to X01
         result := To_X01(result);
    -- That's all
       RETURN result;
    END;

    
    
  --+-----------------------------------------------------------------------------
  --|     Function Name  : bv_To_StdLogicVector
  --|
  --|     Overloading    : 
  --|
  --|     Purpose        : Translate a BIt_VECTOR into an std_logic_vector.
  --|
  --|     Parameters     : SrcVect - input  bit_vector , the value to be 
  --|                                       translated.
  --|                      width   - input  NATURAL, length of the return vector.
  --|                                Default is IntegerBitLength (Machine integer length).
  --|
  --|     Result        : Std_logic_vector.
  --|
  --|     NOTE          : ****** this function not visible to the user **********
  --|-----------------------------------------------------------------------------

  -- ****  function modified so as not to produce an assertion for a zero length vector
  
  FUNCTION bv_To_StdLogicVector ( CONSTANT SrcVect  : IN Bit_Vector;
                                  CONSTANT width    : IN Natural := 0
                                ) RETURN Std_Logic_Vector IS
                                
   VARIABLE len        : INTEGER := SrcVect'LENGTH;
   VARIABLE result     : Std_Logic_Vector(width - 1 DOWNTO 0) := (OTHERS=>'0');
   VARIABLE loc_res    : Std_Logic_Vector(len  - 1 DOWNTO 0) := (OTHERS =>'0');
   VARIABLE vect_copy : Bit_Vector(len - 1 DOWNTO 0) := SrcVect;
     
   BEGIN

       IF (SrcVect'LENGTH = 0) THEN

         return loc_res;

       ELSE
           FOR i IN 0 TO len - 1  LOOP
              CASE vect_copy(i) IS 
                 WHEN '0'   =>
                                loc_res(i) := '0';
                 WHEN '1'   =>
                                loc_res(i) := '1';
              END CASE;
           END LOOP;  
           
           IF (width = 0)  THEN
               return loc_res;
           ELSIF (width <= SrcVect'LENGTH) THEN
               result := loc_res(width - 1 DOWNTO 0);
           ELSIF (width > SrcVect'LENGTH) THEN
               result := RegFill1(loc_res, width, '0');
           END IF;
           RETURN result;
        
       END IF;

    END;
    

    FUNCTION bv_to_hexstr ( CONSTANT val      : IN BIT_VECTOR
                          )  RETURN STRING IS
                          
      CONSTANT hex_len : integer := (val'LENGTH + 3) / 4;
      VARIABLE bin_str : STRING(1 to val'LENGTH);
      VARIABLE hex_str : STRING(1 to hex_len);
      VARIABLE hex_char : STRING(1 to 4);
      VARIABLE bit_index : integer;
      VARIABLE extended_bin_str : STRING(1 to hex_len * 4) := (others => '0');

            
    BEGIN
      bin_str := v_to_str (val);
      if ( (val'LENGTH mod 4) /= 0 ) then
         extended_bin_str ( 5 - (val'LENGTH mod 4) to hex_len * 4 ) := bin_str;
      else
         extended_bin_str := bin_str;
      end if;
      FOR i IN 1 TO hex_len LOOP
        bit_index := ((i - 1) * 4) + 1;
        hex_char := extended_bin_str(bit_index To bit_index + 3);
        CASE hex_char IS
          WHEN "0000" => hex_str(i) := '0'; 
	  WHEN "0001" => hex_str(i) := '1'; 
	  WHEN "0010" => hex_str(i) := '2'; 
	  WHEN "0011" => hex_str(i) := '3'; 
	  WHEN "0100" => hex_str(i) := '4'; 
	  WHEN "0101" => hex_str(i) := '5'; 
	  WHEN "0110" => hex_str(i) := '6'; 
	  WHEN "0111" => hex_str(i) := '7'; 
          WHEN "1000" => hex_str(i) := '8'; 
          WHEN "1001" => hex_str(i) := '9'; 
	  WHEN "1010" => hex_str(i) := 'A'; 
	  WHEN "1011" => hex_str(i) := 'B'; 
	  WHEN "1100" => hex_str(i) := 'C'; 
	  WHEN "1101" => hex_str(i) := 'D'; 
          WHEN "1110" => hex_str(i) := 'E'; 
	  WHEN "1111" => hex_str(i) := 'F'; 
          WHEN OTHERS => null; 
        END CASE;
      END LOOP;
      return (hex_str);
 
    END;
    
    ---------------------------------------------------------------------------
    --    Function Name   :  vector_size
    --
    --    PURPOSE         :  to determine the maximum number of bits needed to
    --                       represent an integer
    --
    --    Parameters      :  int - integer whose bit width is determined
    --
    --    Returned Value  :  NATURAL - # of bits needed
    --
    ---------------------------------------------------------------------------    

    function vector_size ( Constant int : IN integer ) return natural is

    variable i : integer := int;
    variable size : integer := 0;

    begin
        while i > 0 loop
            i := i / 2;
            size := size + 1;
        end loop;
        return size;
    end;
    
    ---------------------------------------------------------------------------
    --    Function Name   :  address_trans
    --
    --    Purpose         :  to translate an address in vector form to a
    --                       NATURAL
    --
    --    Parameters      :  addr    - address to be translated
    --
    --    Returned Value  :  NATURAL - address as a natural number
    --
    --    NOTE            :  *****  this procedure is NOT user visible *******
    --
    --    Use             :  address_trans(addr)
    ---------------------------------------------------------------------------

    Function address_trans ( Constant mem_length : IN POSITIVE;
                             Constant addr       : IN std_logic_vector
                           ) return NATURAL is

    Variable nad, power : NATURAL;
    Variable uonce : BOOLEAN := TRUE;
    Variable xonce : BOOLEAN := TRUE;
    Variable vect_size : integer := vector_size(mem_length - 1);
    Variable talias_addr : std_logic_vector(addr'length - 1 downto 0) := To_UX01(addr);
    Variable alias_addr : std_logic_vector(vect_size - 1 downto 0) := (others => To_StdULogic(ADDRESS_X_MAP));
    Variable temp_vect : bit_vector(vect_size - 1 downto 0);    
                                                      
    begin
        nad := 0;
        power := 1;
        alias_addr( minimum(vect_size, addr'length)  - 1 downto 0) :=
                                                 talias_addr( minimum(vect_size,addr'length) - 1 downto 0 );
        assert ( (vect_size >= addr'length) or NOT MEM_WARNINGS_ON )
            report "Bit width of address vector greater than that needed to access the entire memory."
                   & LF & SPACESTR & "passed address bit width:  " & i_to_str(addr'length)
                   & LF & SPACESTR & "required address bit width:  " & i_to_str(vect_size)
            severity WARNING;
        assert ( (vect_size <= addr'length) or NOT MEM_WARNINGS_ON )
            report "Bit width of address vector less than that needed to access the entire memory."
                   & LF & SPACESTR & "Resulting X's being mapped to:  " & to_str(ADDRESS_X_MAP)
                   & LF & SPACESTR & "passed address bit width:  " & i_to_str(addr'length)
                   & LF & SPACESTR & "required address bit width:  " & i_to_str(vect_size)
            severity WARNING;
            
        for i IN 0 to vect_size - 1 loop
            if ((alias_addr(i) = 'U') and MEM_WARNINGS_ON and uonce) then
                uonce := FALSE;
                assert FALSE
                   report "Address contains a U - it is being mapped to:  " & to_str(ADDRESS_U_MAP)
                          severity WARNING;
            end if;
            if ((alias_addr(i) = 'X') and MEM_WARNINGS_ON and xonce) then
                xonce := FALSE;
                assert false
                   report "Address contains an X - it is being mapped to:  " & to_str(ADDRESS_X_MAP)
                          severity WARNING;
            end if;
            temp_vect(i) := ADDRESS_MAP(alias_addr(i));
            nad := nad + (power * bit'pos(temp_vect(i)));
            power := power * 2;
        end loop;
        return nad;
    end;
           
    Function address_trans ( Constant mem_length :  IN POSITIVE;
                             Constant addr       :  IN std_ulogic_vector
                           ) return NATURAL is

    Variable nad, power : NATURAL;
    Variable uonce : BOOLEAN := TRUE;
    Variable xonce : BOOLEAN := TRUE;
    Variable talias_addr : std_ulogic_vector(addr'length - 1 downto 0) := To_UX01(addr);    
    Variable vect_size : integer := vector_size(mem_length - 1);
    Variable alias_addr : std_ulogic_vector(vect_size - 1 downto 0) := (others => To_StdULogic(ADDRESS_X_MAP));
    Variable temp_vect : bit_vector(vect_size - 1 downto 0);
                                                      
    begin
        nad := 0;
        power := 1;
        alias_addr( minimum(vect_size, addr'length)  - 1 downto 0) :=
                                                       talias_addr( minimum(vect_size,addr'length) - 1 downto 0);
        assert ( (vect_size >= addr'length) or NOT MEM_WARNINGS_ON )
            report "Bit width of address vector greater than that needed to access the entire memory."
                   & LF & SPACESTR & "passed address bit width:  " & i_to_str(addr'length)
                   & LF & SPACESTR & "required address bit width:  " & i_to_str(vect_size)
            severity WARNING;
        assert ( (vect_size <= addr'length) or NOT MEM_WARNINGS_ON )
            report "Bit width of address vector less than that needed to access the entire memory."
                   & LF & SPACESTR & "Resulting X's being mapped to:  " & to_str(ADDRESS_X_MAP)            
                   & LF & SPACESTR & "passed address bit width:  " & i_to_str(addr'length)
                   & LF & SPACESTR & "required address bit width:  " & i_to_str(vect_size)
            severity WARNING;
            
        for i IN 0 to vect_size - 1 loop
            if ((alias_addr(i) = 'U') and MEM_WARNINGS_ON and uonce) then
                uonce := FALSE;
                assert false
                   report "Address contains a U - it is being mapped to:  " & to_str(ADDRESS_U_MAP)
                          severity WARNING;
            end if;
            if ((alias_addr(i) = 'X') and MEM_WARNINGS_ON and xonce) then
                xonce := FALSE;
                assert false
                   report "Address contains an X - it is being mapped to:  " & to_str(ADDRESS_X_MAP)
                          severity WARNING;
            end if;
            temp_vect(i) := ADDRESS_MAP(alias_addr(i));
            nad := nad + (power * bit'pos(temp_vect(i)));
            power := power * 2;
        end loop;
        return nad;
    end;                           

    Function address_trans ( Constant mem_length :  IN POSITIVE;
                             Constant addr       :  IN bit_vector
                           ) return NATURAL is

    Variable nad, power : NATURAL;
    Variable vect_size : integer := vector_size(mem_length - 1);
    Variable talias_addr : bit_vector(addr'length - 1 downto 0) := addr;        
    Variable alias_addr : bit_vector(vect_size - 1 downto 0) := (others => ADDRESS_X_MAP);
                                                      
    begin
        nad := 0;
        power := 1;
        alias_addr( minimum(vect_size, addr'length)  - 1 downto 0) :=
                                                   talias_addr( minimum(vect_size,addr'length) - 1 downto 0);
        if ( MEM_WARNINGS_ON and (vect_size > addr'length) ) then
            assert false
                report "Bit width of address vector smaller than that needed to access the entire memory."
                       & LF & SPACESTR & "Resulting X's being mapped to:  " & to_str(ADDRESS_X_MAP)
                       & LF & SPACESTR & "passed address bit width:  " & i_to_str(addr'length)
                       & LF & SPACESTR & "required address bit width:  " & i_to_str(vect_size)
                severity WARNING;
        elsif ( MEM_WARNINGS_ON and (vect_size < addr'length) ) then
            assert false
                report "Bit width of address vector larger than that needed to access the entire memory."
                       & LF & SPACESTR & "passed address bit width:  " & i_to_str(addr'length)
                       & LF & SPACESTR & "required address bit width:  " & i_to_str(vect_size)
                severity WARNING;
        end if;
        for i in 0 to vect_size - 1 loop
            nad := nad + (power * bit'pos(alias_addr(i)));
            power := power * 2;
        end loop;
        return nad;
    end;
    

    ---------------------------------------------------------------------------
    --    Procedure Name  :  allocate_row
    --
    --    Purpose         :  to allocate a row of memory and initialize it
    --                       to the default value
    --
    --    Parameters      :  mem_id  -  ptr to memory data structure
    --                       row     -  row to be allocated
    --
    --    NOTE            :  allocate data space for 1 row of memory
    --                       ******  this procedure is NOT user visible *******
    --
    --    Use             :  allocate_row (ram1, 5);
    ---------------------------------------------------------------------------

    procedure allocate_row (  Variable mem_id  :  INOUT mem_id_type;
                              Constant row     :  IN NATURAL
                           ) is

        subtype constrained_matrix is
                     row_matrix (0 to mem_id.columns-1, 0 to mem_id.width-1);
        variable ptr : rowptr_type;
        variable i, j : integer;
        
    begin
        
        if mem_id.row_data_ptr(row).all_xs then    -- if row should be filled with X's then do so

            mem_id.row_data_ptr(row).rowptr := new constrained_matrix'( others => (others => 'X'));

        else                                       -- otherwise, row should be filled with the default

            mem_id.row_data_ptr(row).rowptr := new constrained_matrix;

            ptr := mem_id.row_data_ptr(row).rowptr;
            for i in 0 to mem_id.columns - 1 loop
                for j in 0 to mem_id.width - 1 loop
                    ptr(i,j) := To_UX01(mem_id.default(j));
                end loop;
            end loop;
        end if;
        -- no longer necessary to indicate that its filled with X's
        mem_id.row_data_ptr(row).all_xs := FALSE;   
    end;                           

    ---------------------------------------------------------------------------
    --    Procedure Name  :  validate_row
    --
    --    Purpose         :  if memory is a DRAM then check if refresh period
    --                       has expired.  If so, and space allocated, then
    --                       reset all locations to X's.  This is done by setting
    --                       the filed all_xs to TRUE
    --
    --    Parameters      :  mem_id   -  pointer to memory data structure
    --                       row      -  row to be validated
    --
    --    NOTE            :  ******  this procedure is NOT user visible *******
    --
    --    Use             :  validate_row (dram1, 5);
    ---------------------------------------------------------------------------

    Procedure validate_row (  Variable mem_id : INOUT mem_id_type;
                              Constant row    : IN NATURAL
                           ) IS 

    Variable rowdat : row_data_ptr_type := mem_id.row_data_ptr;
    Variable i, j : INTEGER;
                           
    begin
        -- check that it is a dram and that refresh period has expired
        if ( (mem_id.memory_type = DRAM) and (NOW > (rowdat(row).last_refresh + mem_id.refresh_period)) ) then
            if rowdat(row).all_xs then
                -- if all_xs is true already then only an assertion is necessray
                assert NOT MEM_WARNINGS_ON
                    report "Refresh time has expired on row " & i_to_str(row) & " of memory:  "
                           & pstr(mem_id.name(1 to mem_id.name'length)) & LF & SPACESTR & "however, row was not filled with valid data."
                    severity WARNING;
            elsif rowdat(row).rowptr = NULL then
                -- if all_xs is false and no space has been allocated for this row then it must be at default
                -- set all_xs to true and make an assertion
                rowdat(row).all_xs := TRUE;
                assert NOT MEM_WARNINGS_ON
                    report "Refresh time has expired on row " & i_to_str(row) & " of memory:  "
                           & pstr(mem_id.name(1 to mem_id.name'length)) & LF & SPACESTR & "Row was filled with default value."
                    severity WARNING;
            else
                -- row has valid, non-default data in it
                -- set all_xs to true and deallocate space for row
                rowdat(row).all_xs := TRUE;
                deallocate(mem_id.row_data_ptr(row).rowptr);
                mem_id.row_data_ptr(row).rowptr := NULL;
                assert NOT MEM_WARNINGS_ON
                    report "Refresh time has expired on row " & i_to_str(row) & " of memory:  "
                           & pstr(mem_id.name(1 to mem_id.name'length)) & LF & SPACESTR & "Data is lost."
                    severity WARNING;
            end if;
        end if;
    end;

    ---------------------------------------------------------------------------
    --    Procedure Name  :  refresh_row
    --
    --    Purpose         :  if memory is a DRAM then update the last_refresh
    --                       time along with last time used (last_init)
    --
    --    Parameters      :  mem_id   -  pointer to memory data structure
    --                       row      -  row to be refreshed
    --
    --    NOTE            :  ******  this procedure is NOT user visible *******
    --
    --    Use             :  refresh_row (dram1, 5);
    ---------------------------------------------------------------------------

    Procedure refresh_row ( VARIABLE mem_id  :  INOUT mem_id_type;
                            Constant row     :  IN NATURAL
                           ) is

    begin
        if ( (mem_id.memory_type = DRAM) and (mem_id.last_init + mem_id.refresh_period >= NOW)) then
            mem_id.row_data_ptr(row).last_refresh := NOW;
            mem_id.last_init := NOW;
        end if;
    end;

    ---------------------------------------------------------------------------
    --    Function Name   :  SRAM_Initialize
    --
    --    Purpose         :  To create the data structure used to store a
    --                       static RAM and to initialize it
    --
    --    Parameters      :  name    - string used to represent the memory
    --                       length  - the number of "words" in the memory
    --                       width   - the length of a "word" of memory
    --                       default_word   - value to which each word of
    --                                        memory should be initialized
    --
    --    RETURNED VALUE  :  mem_id_type - pointer to memory record
    --
    --    NOTE            :  initially the data structure is empty with no
    --                       space being allocated for the memory
    --
    --    Use             :  SRAM_Initialize (sram_l1,"lsb_of_RAM",1048576,1);
    ---------------------------------------------------------------------------
        
    impure Function SRAM_Initialize ( Constant name            : IN string;
                               Constant length          : IN POSITIVE;
                               Constant width           : IN POSITIVE;
                               Constant default_word    : IN std_logic_vector
                             ) return mem_id_type IS

    Variable i, name_len : INTEGER;
    Variable mem_id : mem_id_type;
    Variable alias_name : string (1 to name'length) := name;
                              
    begin
        -- create and initialize data structure

        mem_id := new mem_id_rtype '( memory_type => SRAM,
                                      refresh_period => 0.0 ns,
                                      last_init => 0.0 ns,
                                      counter => 0,
                                      name => NULL,
                                      rows => 1,
                                      columns => SRAM_COL_SIZE,
                                      width => width,
                                      length => length,
                                      row_data_ptr => NULL,
                                      default => NULL
                                     );

        if ( (length mod SRAM_COL_SIZE) /= 0) then
           mem_id.rows := (length/SRAM_COL_SIZE) + 1;
        else
           mem_id.rows := length/SRAM_COL_SIZE;
        end if;
        -- store name of memory
        name_len := 1;
        while ( (name_len <= alias_name'length) and (alias_name(name_len) /= nul)) loop
           name_len := name_len + 1;
        end loop;
        name_len := name_len - 1;

        mem_id.name := new string(1 to name_len);

        for i in 1 to name_len loop
           mem_id.name(i) := alias_name(i);
        end loop;
        -- create and initialize data structure for rows

        mem_id.row_data_ptr := new row_data(0 to mem_id.rows-1);

        for i in 0 to mem_id.rows - 1 loop
            mem_id.row_data_ptr(i) := (last_refresh => NOW,
                                       rowptr => NULL,
                                       all_xs => FALSE
                                      );
        end loop;
        -- set default word

        mem_id.default := new std_logic_vector(mem_id.width - 1 downto 0);

        if (default_word'length /= mem_id.width) then
            assert (default_word'length  = 0)
                report "SRAM_INITIALIZE:  Default word width does not match word width of memory:  "
                       & pstr(mem_id.name(1 to mem_id.name'length)) & LF & SPACESTR
                       & "default will be set to a word filled with 'U'"
                severity ERROR;
            for i in 0 to mem_id.width - 1 loop
                mem_id.default(i) := 'U';
            end loop;
        else

            mem_id.default.all := To_X01(default_word);

        end if;
        return mem_id;
    end;


    impure Function SRAM_Initialize (  Constant name            : IN string;
                                Constant length          : IN POSITIVE;
                                Constant width           : IN POSITIVE;
                                Constant default_word    : IN std_ulogic_vector
                              ) return mem_id_type IS

    Variable mem_id : mem_id_type;

    begin
        mem_id := SRAM_Initialize ( name,
                                    length,
                                    width,
                                    std_logic_vector (default_word)
                                  );
        return mem_id;
    end;

                                      
    ---------------------------------------------------------------------------
    --    Procedure Name  :  Mem_Wake_Up
    --
    --    Purpose         :  to initialize a DRAM for use
    --                       
    --    Parameters      :  mem_id    - ptr to memory data structure
    --
    --    NOTE            :  a DRAM must be woken up before it can be used or if
    --                       the refresh period passes without any operations
    --
    --    Use             :  Mem_Wake_Up (ROM_chip_1);
    ---------------------------------------------------------------------------

    Procedure Mem_Wake_Up (Variable mem_id     : INOUT mem_id_type) IS

    begin
        if (mem_id.memory_type = DRAM) then
            mem_id.last_init := NOW;
        else
            assert false
                report "Mem_Wake_Up:  Memory:  " & pstr(mem_id.name(1 to mem_id.name'length)) & " is a ROM or an SRAM."
                        & LF & SPACESTR &
                       "This operation only valid for DRAM's,operation ignored."
                severity ERROR;
        end if;
    end;


    
    ---------------------------------------------------------------------------
    --    Procedure Name  :  Mem_Basic_Write
    --
    --    Purpose         :  To write a "word" to memory
    --                       this procedure will write to a ROM
    --
    --    Parameters      :  mem_id     - ptr to memory data structure
    --                       address    - address to read from
    --                       data       - "word" to be written to memory
    --                                    must first be converted to X01
    --                       ingore_rom - if true then write even if ROM
    --
    --    NOTE            :  a write refreshes row of a DRAM
    --                       ***** this procedure not user visible *******
    --
    ---------------------------------------------------------------------------

    
    Procedure Mem_Basic_Write (  Variable mem_id      : INOUT mem_id_type;
                                 Constant address     : IN NATURAL;
                                 Constant data        : IN std_logic_vector;
                                 Constant ignore_rom  : IN Boolean := FALSE
                              ) IS

    Constant alias_data : std_logic_vector (data'length - 1 downto 0) := data;
    variable row, column, i : integer;
    variable short_ptr : rowptr_type;
    variable mem_word : std_logic_vector (mem_id.width - 1 downto 0)
                                                          := (others => 'X');   
                        
    begin
        if ( (mem_id.memory_type /= ROM) or (ignore_rom) ) then   -- make sure its not a rom
            if address < mem_id.length then                       -- check that its a valid address
                -- if memory is a dram make sure that it has been woken up
                if ( (mem_id.memory_type /= DRAM) or (mem_id.last_init + mem_id.refresh_period >= NOW)) then
                    -- calculate row and column
                    row := address/mem_id.columns;
                    column := address mod mem_id.columns;
                    -- validate address and report if refresh time exceeded
                    validate_row (mem_id, row);
                    -- refresh the row 
                    refresh_row (mem_id, row);
                    -- if row never allocated then allocate it
                    if (mem_id.row_data_ptr(row).rowptr = NULL) then
                        allocate_row(mem_id, row);
                    end if;
                    -- handle data of different width than memory
                    -- if data has less bits than memory than MSBs become Xs
                    assert ( (data'length = mem_id.width) OR NOT MEM_WARNINGS_ON)
                        report "Mem_Write:  passed data size does not match word size"
                               & " of mem:  " & pstr(mem_id.name(1 to mem_id.name'length))  & LF & SPACESTR &
                               "passed data size:  " & i_to_str(data'length) & " bits"
                                & LF & SPACESTR & "memory word size:  " &
                               i_to_str(mem_id.width) & " bits"
                        severity WARNING;
                    if (mem_id.width >= data'length) then
                        mem_word (data'length - 1 downto 0) := alias_data;
                    else
                        mem_word := To_X01(alias_data(mem_id.width-1 downto 0));
                    end if;
                    -- write data to memory
                    short_ptr := mem_id.row_data_ptr(row).rowptr;
                    for i IN 0 to mem_id.width - 1 loop
                        -- mem_id.row_data_ptr(row).rowptr(column,i) := mem_word(i);
                        -- *************************************
                        -- this is a bug work around for synopsys
                        -- replaces line commented out above
                        short_ptr(column,i) := mem_word(i);
                        -- end bug fix
                        -- *************************************
                    end loop;
                else
                    assert false
                        report "Mem_Write:  Device wake-up time limit exceeded for memory:  "
                               & pstr(mem_id.name(1 to mem_id.name'length))  & LF & SPACESTR &
                               "Operation ignored, device must be woken up."
                        severity WARNING;
                end if;
            else
                assert false
                     report "Mem_Write:  Passed address exceeds address " 
                            & "range of mem:  " & pstr(mem_id.name(1 to mem_id.name'length))  & LF & SPACESTR
                            & "specified address:  " & i_to_str(address)  & LF &
                            SPACESTR & "address range:  0 to " & i_to_str(mem_id.length - 1)
                    severity ERROR;
          end if;
        else
            assert false
                report "Mem_Write:  Attempt to write to memory:  " & pstr(mem_id.name(1 to mem_id.name'length))
                       & LF & SPACESTR & "Writes to ROMs are not allowed.  Operation ignored."
                severity ERROR;
        end if;
    end;


    ---------------------------------------------------------------------------
    --    Procedure Name  :  Mem_All_Reset
    --
    --    Purpose         :  To set the contents of a memory to some predetermined
    --                       value.  The locations to reset are specified by a
    --                       range.
    --
    --    Parameters      :  mem_id       -  ptr to memory data structure
    --                       reset_value  -  value to reset memory to
    --                       start_addr   -  starting address within memory
    --                       end_addr     -  ending address withim memory
    --                       ROM_too      -  allows roms to be reset as well if true
    --
    --    NOTE            :  works for all mem types.  call by Mem_Reset
    --                       ****    NOT USER VISIBLE   *****
    --
    --    Use             :  Mem_ALL_Reset (RAM1, "1010", 2048, 4096, FALSE);
    ---------------------------------------------------------------------------
        
    procedure  Mem_ALL_Reset ( Variable mem_id          : INOUT mem_id_type;
                               Constant reset_value     : IN std_logic_vector;
                               Constant start_addr      : IN NATURAL := 0;
                               Constant end_addr        : IN NATURAL := integer'high;
                               Constant ROM_too         : IN BOOLEAN := FALSE
                             ) IS

    Variable real_end : NATURAL := end_addr;
    Variable start_row, start_col, end_row, end_col : NATURAL;
    Variable row, col, rstart_col, rend_col, bit_pos : NATURAL;
    Variable row_ptr : rowptr_type;
    Variable alias_reset : std_logic_vector (mem_id.width - 1 downto 0) := (others => 'U');
    Variable xvector : std_logic_vector (mem_id.width - 1 downto 0) := (others => 'X');
    Variable i : integer;
                        
    begin
        if (reset_value'length /= mem_id.width) then
           assert (reset_value'length <= 0)
                report "Mem_Reset:  reset value of memory does not match memory width " &
                       pstr(mem_id.name(1 to mem_id.name'length)) & LF & SPACESTR
                       & "Resetting memory all all 'U's."
                severity ERROR;
           alias_reset := (others => 'U');
        else
           alias_reset := To_X01(reset_value);
        end if;
        if ( (mem_id.memory_type /= ROM) or ROM_too) then          -- make sure its not a rom
            if (end_addr < start_addr) then   -- check address ranges
                assert false
                       report "Mem_Reset:  ending address is less than starting address."
                              & LF & SPACESTR & "No operation performed."
                       severity ERROR;
            elsif (start_addr >= mem_id.length) then
                assert false
                       report "Mem_Reset:  starting address outside of address "
                              & "range of memory:  " & pstr(mem_id.name(1 to mem_id.name'length))
                              & LF & SPACESTR & "No operation performed."
                       severity ERROR;
            else
                If (end_addr >= mem_id.length) then
                    assert (end_addr = integer'high)
                           report "Mem_Reset:  ending address outside address "
                                  & "range of memory:  " & pstr(mem_id.name(1 to mem_id.name'length)) & LF & SPACESTR
                                  & "Memory will be refreshed until end is reached."
                           severity WARNING;
                    real_end := mem_id.length - 1;
                end if;
                -- if memory is a dram, then wake it up
                if mem_id.memory_type = DRAM then
                    Mem_Wake_Up (mem_id);
                end if;
                -- calculate row and column of starting address
                start_row := start_addr/mem_id.columns;
                start_col := start_addr mod mem_id.columns;
                -- calculate row and column of ending address
                end_row := real_end/mem_id.columns;
                end_col := real_end mod mem_id.columns;
                -- starting column of row presently being written to
                rstart_col := start_col;
                for row in start_row to end_row loop 
                    if row = end_row then       -- set ending collumn of row presently being written to
                        rend_col := end_col;
                    else
                        rend_col := mem_id.columns - 1;
                    end if;
                    --  check for expired time period on row 
                    if ( (rstart_col > 0) or (rend_col < mem_id.columns - 1) ) then
                        -- it is only necessary to validate row if only part of the row is being reset
                        validate_row (mem_id, row);
                    else
                       -- entire row being reset, check for expired refresh period
                       assert ( (mem_id.memory_type /= DRAM) or
                                ((mem_id.row_data_ptr(row).last_refresh + mem_id.refresh_period) >= NOW)
                                or NOT MEM_WARNINGS_ON )
                           report "Mem_Reset:  refresh period on row " &
                                  i_to_str(row) & " has expired but"
                                   & LF & SPACESTR
                                  & "no data lost since entire row is being reset"
                           severity WARNING;
                    end if;
                    -- if collumn not allocated & fill value is not the default or all x's then allocate

                    if ( (mem_id.row_data_ptr(row).rowptr = NULL) and (alias_reset /= mem_id.default.all)
                         and (alias_reset /= xvector) ) then

                        allocate_row (mem_id, row);
                    -- if filling partial row with default and currently is Xs then allocate

                    elsif ( (mem_id.row_data_ptr(row).rowptr = NULL) and (alias_reset = mem_id.default.all)
                            and mem_id.row_data_ptr(row).all_xs and
                            ( (rstart_col /= 0) or (rend_col /= mem_id.columns - 1)) ) then

                        allocate_row (mem_id, row);
                    -- if filling partial row with Xs and currently is default then allocate
                    elsif ( (mem_id.row_data_ptr(row).rowptr = NULL) and (alias_reset = xvector)
                            and (NOT mem_id.row_data_ptr(row).all_xs) and
                            ( (rstart_col /= 0) or (rend_col /= mem_id.columns - 1)) ) then
                        allocate_row (mem_id, row);
                    end if;                    
                    -- if filling entire collumn with default then deallocate it

                    If ( (alias_reset = mem_id.default.all) and (rstart_col = 0) and

                         (rend_col = mem_id.columns - 1) ) then
                        if (mem_id.row_data_ptr(row).rowptr /= NULL) then
                            Deallocate (mem_id.row_data_ptr(row).rowptr);
                            mem_id.row_data_ptr(row).rowptr := NULL;
                        end if;
                        mem_id.row_data_ptr(row).all_xs := FALSE;
                    -- if filling entire collumn with X's then deallocate it
                    elsif ( (alias_reset = xvector) and (rstart_col = 0) and (rend_col = mem_id.columns - 1) ) then
                          if (mem_id.row_data_ptr(row).rowptr /= NULL)  then
                             Deallocate (mem_id.row_data_ptr(row).rowptr);
                             mem_id.row_data_ptr(row).rowptr := NULL;
                         end if;
                         mem_id.row_data_ptr(row).all_xs := TRUE;
                    end if;
                    -- fill up the row if the entire row isn't being filled with Xs or default
                    row_ptr := mem_id.row_data_ptr(row).rowptr;
                    if (row_ptr /= NULL)  then
                        for col in rstart_col to rend_col loop
                            for bit_pos in 0 to mem_id.width - 1 loop
                                row_ptr(col,bit_pos) := alias_reset(bit_pos);
                            end loop;
                        end loop;
                    end if;
                    rstart_col := 0;              -- start at beginning of next collumn
                    refresh_row (mem_id, row);    -- refresh the current row
                end loop;
            end if;
        else
            assert false
                 report "Mem_Reset:   Reset of ROM not allowed.  Operation ignored"
                 severity ERROR;
        end if;
    end;


    --------------------------------------------------------------------------------------------------
    --  The following functions and procedures are used in the recursive descent parser that is used
    --  to parse the memory files.
    --  *****************************   THESE ROUTINES ARE NOTE USER VISIBLE   ***********************
    --------------------------------------------------------------------------------------------------    


    -- return true if character is upper case character
    function is_upper_case ( Constant ch : IN CHARACTER ) return BOOLEAN is

    begin
        return ( (ch >= 'A') and (ch <= 'Z') );
    end;

    -- return true if character is lower case character
    function is_lower_case ( Constant ch : IN CHARACTER ) return BOOLEAN is

    begin
        return ( (ch >= 'a') and (ch <= 'z') );
    end;

    -- return true if character is a decimal digit
    function is_dec_digit ( Constant ch : IN CHARACTER ) return BOOLEAN is

    begin
        return ( (ch >= '0') and (ch <= '9'));
    end;

    
    -- skip over blanks and tabs
    -- read characters and numbers until space, tab, or symbol is encountered
    -- get special identifiers such as :, --, or ..
    -- convert lower case to upper case
    -- update buffer index to point to first character after identifier that was read
    
    procedure read_word ( Variable out_str  : OUT STRING;
                          Constant in_str   : IN STRING;
                          Variable b_ind    : INOUT INTEGER
                        ) is

    Variable out_ind : integer := 1;

    begin
      -- skip over spaces and tabs
      while ( ( (in_str(b_ind) = ' ') or (in_str(b_ind) = HT) ) and (b_ind <= StrLen1(in_str)))  loop
          b_ind := b_ind + 1;
      end loop;
      if ( b_ind > StrLen1(in_str) ) then  -- return if blank line
          out_str(out_ind) := NUL;
          return;
      end if;
      -- check for --
      if ( (StrLen1(in_str) >= b_ind+1) and  (in_str(b_ind) = '-') and (in_str(b_ind + 1) = '-') ) then
          out_str(1) := '-';
          out_str(2) := '-';
          out_str(3) := NUL;
          b_ind := b_ind + 2;
          return;
      end if;
      -- check for ..
      if ( (StrLen1(in_str) >= b_ind+1) and (in_str(b_ind) = '.') and (in_str(b_ind + 1) = '.') ) then
          out_str(1) := '.';
          out_str(2) := '.';
          out_str(3) := NUL;
          b_ind := b_ind + 2;
          return;
      end if;
      -- check for :
      if ( (StrLen1(in_str) >= b_ind) and (in_str(b_ind) = ':') ) then
          out_str(1) := ':';
          out_str(2) := NUL;
          b_ind := b_ind  + 1;
          return;
      end if;
      -- get an identifier
      loop  -- accept at least one character no matter what is is
          if ( (in_str(b_ind) >= 'a') and (in_str(b_ind) <= 'z') ) then
              out_str(out_ind) := Character'Val( (Character'POS(in_str(b_ind))) - 32 );  -- convert to upper case
          else
              out_str(out_ind) := in_str(b_ind);
          end if;
          out_ind := out_ind + 1;  
          b_ind := b_ind + 1;
          exit when ( (b_ind > StrLen1(in_str)) or
                      not (is_upper_case (in_str(b_ind))  or is_lower_case(in_str(b_ind)) or
                            is_dec_digit(in_str(b_ind))
                          )
                    );
      end loop;
      out_str(out_ind) := NUL;
    end;

    -- make sure string is a valid hexadecimal number
    
    Function   valid_hex ( Constant str : IN STRING ) return BOOLEAN is

    variable i : integer;
    variable valid : BOOLEAN := TRUE;

    begin
        i := 1;
        while ( (i <= StrLen1(str)) and valid ) loop
            valid := ( (str(i) >= '0') and (str(i) <= '9') )  or
                     ( (str(i) >= 'a') and (str(i) <= 'z') ) or
                     ( (str(i) >= 'A') and (str(i) <= 'Z') );
            i := i + 1;
        end loop;
        return valid;
    end;

    -- determine what kind of identifier the sring is
    Function word_id (Constant str : IN STRING) return IDENTIFIER is

    begin
       if StrLen1(str) = 0 then
           -- assert false report "BLANK1" severity NOTE;
           return BLANK1;
       elsif ( (StrLen1(str) = 1) and (str(1) = ':') ) then
           -- assert false report "COLON1" severity NOTE;
           return COLON1;
       elsif ( (StrLen1(str) = 2) and (str(1) = '-') and (str(2) = '-') ) then
           -- assert false report "COMMENT1" severity NOTE;
           return COMMENT1;
       elsif ( (StrLen1(str) = 2) and (str(1) = '.') and (str(2) = '.') ) then
           -- assert false report "DOTDOT1" severity NOTE;
           return DOTDOT1;
       elsif ( (StrLen1(str) = 5) and (str(1 to 5) = "WIDTH") ) then
           -- assert false report "WIDTH1" severity NOTE;
           return WIDTH1;           
       elsif ( (StrLen1(str) = 7) and (str(1 to 7) = "DEFAULT") ) then
           -- assert false report "DEFAULT1" severity NOTE;
           return DEFAULT1;           
       elsif (valid_hex(str)) then
           -- assert false report "HEX_NUM1" severity NOTE;
           return HEX_NUM1;           
       else
           -- assert false report "SYN_ERROR1" severity NOTE;
           return SYN_ERROR1;           
       end if;
    end;

    -- force the parser to start parsing from the next line.
    -- Reset the string buffer index to the first element
    procedure new_line ( Variable str_buff   : INOUT string;
                         Variable b_index    : INOUT integer;
                         Variable file_error : INOUT integer;
                         Variable in_file    : IN TEXT;
                         Variable line_num   : INOUT integer
                       ) is

    Variable line_ptr : LINE;                       

    begin
        b_index := 1;
        if not endfile(in_file) then
            fgetline1(str_buff, in_file, line_ptr);
            line_num := line_num + 1;
        else
            file_error := 1;
        end if;
        DEALLOCATE(line_ptr);
    end;

    -- get the next symbol in the file going to the next line if necessary

    procedure get_sym ( Variable word       : INOUT string;
                        Variable str_buff   : INOUT string;
                        Variable b_index    : INOUT integer;
                        Variable file_error : INOUT integer;
                        Variable in_file    : IN TEXT;
                        Variable line_num   : INOUT integer
                     ) is

    Variable line_ptr : LINE;                         

    begin
        if ( b_index > StrLen1(str_buff) ) then  -- if end of line reached then get another line
           b_index := 1;
           if not endfile(in_file) then
               fgetline1(str_buff, in_file, line_ptr);
               line_num := line_num + 1;
           else
               file_error := 1;
           end if;
           word(1) := NUL;
        else
          read_word(word, str_buff, b_index);
        end if;
        DEALLOCATE(line_ptr);
    end;

    -- convert a hexadecimal string to an integer
    function from_hex_to_int (word : IN string) return integer is

    variable alias_word : string(1 to word'length) := word;
    variable digit, start, leng : integer;
    variable power : integer := 1;
    variable result : integer := 0;
    variable max_bit_num : integer := 0;  -- max number of bits needed to represent hex number
    
    begin
        leng := StrLen1(alias_word);
        start := 1;
        -- eliminate preceeding 0's
        while ( (alias_word(start) = '0') and (start < leng) ) loop  -- less than leng handles the 0 case
            start := start + 1;
        end loop;
        for i in leng downto start loop
            max_bit_num := max_bit_num + 4;
            if ( (alias_word(i) >= '0') and (alias_word(i) <= '9')) then
                digit := Character'Pos(alias_word(i)) - 48;
            else
                digit := Character'Pos(alias_word(i)) - 55;
            end if;
            if ( (max_bit_num >= IntegerBitLength) and (digit > 7) ) then
               assert FALSE
                  report "MemRead: hex value:  " & word & " is too large to represent as an integer on this machine"
                  severity ERROR;
               exit;  -- exit the loop
            end if;
            result := result + digit * power;
            if (i /= start) then           -- power will not be multiplied by 16 on the last iteration.
               power := power * 16;        -- This will prevent an integer overflow when dealing with 
            end if;                        -- the maximum number of hex digits the machine can represent.
        end loop;
        return result;
    end;

    -- parse a width statement
    procedure pwidth ( Variable word       : INOUT string;
                       Variable str_buff   : INOUT string;
                       Variable b_index    : INOUT integer;
                       Variable file_error : INOUT integer;
                       Variable in_file    : IN TEXT;
                       Variable file_width : INOUT integer;
                       Variable line_num   : INOUT integer
                     ) is

    Variable w_id : IDENTIFIER;
    variable error_line : integer;

    begin
        w_id := word_id(word);
        if (w_id /= WIDTH1) then
            file_error := 2;
            assert false
                 report "Mem_Load:  Width specification not first executable line in file.  File load aborted."
                        & LF & SPACESTR & "Occurred on line number " & i_to_str(line_num) & " of the input file."
                 severity ERROR;
        else
            get_sym (word, str_buff, b_index, file_error, in_file, line_num);
            w_id := word_id(word);
            if (w_id = COLON1) then
               get_sym (word, str_buff, b_index, file_error, in_file, line_num);
               w_id := word_id(word);
               if w_id = HEX_NUM1 then
                   file_width := from_hex_to_int(word);
                   get_sym (word, str_buff, b_index, file_error, in_file, line_num);
                   w_id := word_id(word);
                   error_line := line_num;
                   if w_id = COMMENT1 then
                        new_line (str_buff, b_index, file_error, in_file, line_num);
                   elsif w_id /= BLANK1 then
                        assert false
                            report "Mem_Load:  Additional information on width specification line ignored."
                                   & LF & SPACESTR & "File processing continuing."
                                   & LF & SPACESTR & "Occurred on line number " & i_to_str(line_num)
                                   & " of the input file."
                            severity ERROR;
                        new_line(str_buff, b_index, file_error, in_file, line_num);
                   end if;
                   if file_width = 0 then
                       file_error := 10;
                       assert false
                           report "Mem_load:  Width must be greater than 0.  File load aborted."
                                  & LF & SPACESTR & "Occurred on line number " & i_to_str(error_line)
                                  & " of the input file."
                           severity ERROR;
                   end if;
               else
                   file_error := 3;
               end if;
            else
                file_error := 3;
            end if;
        end if;
        if file_error = 3 then
            assert false
                report "Mem_load:  Syntax error in width specification.  File load aborted."
                       & LF & SPACESTR & "Occurred on line number " & i_to_str(line_num) & " of the input file."
                severity ERROR;
        end if;
    end;

    -- parse a default statement
    procedure pdefault ( Variable word       : INOUT string;          -- present word
                         Variable str_buff   : INOUT string;          -- string buffer
                         Variable b_index    : INOUT integer;         -- string buffer index
                         Variable file_error : INOUT integer;         -- error?
                         Variable in_file    : IN TEXT;               -- file
                         Variable file_width : IN integer;            -- width specified by file
                         Variable mem_id     : INOUT mem_id_type;     -- memory 
                         Constant hex_size   : IN integer;            -- number of hex digits expected
                         Constant rwidth     : IN integer;            -- # of bits to be written to memory
                         Variable line_num   : INOUT integer             -- line # of file 
                       ) is

    Variable w_id : IDENTIFIER;
    Variable tdata : bit_vector (file_width - 1 downto 0);
    Variable data : std_logic_vector (mem_id.width - 1 downto 0);
    

    begin
        get_sym (word, str_buff, b_index, file_error, in_file, line_num);
        w_id := word_id(word);
        if w_id = COLON1 then
            get_sym (word, str_buff, b_index, file_error, in_file, line_num);
            w_id := word_id(word);
            if w_id = HEX_NUM1 then
                if StrLen1(word) = hex_size then
                    data := (others => 'X');
                    tdata := From_HexString1(word)(file_width - 1 downto 0);
                    data(rwidth - 1 downto 0) := bv_To_StdLogicvector(tdata(rwidth - 1 downto 0));
                    if mem_id.default = NULL then

                        mem_id.default := new std_logic_vector(mem_id.width - 1 downto 0);

                    else
                        deallocate (mem_id.default);

                        mem_id.default := new std_logic_vector(mem_id.width - 1 downto 0);

                    end if;

                    mem_id.default.all := data;

                    get_sym (word, str_buff, b_Index, file_error, in_file, line_num);
                    w_id := word_id(word);
                    if w_id = COMMENT1 then
                        new_line (str_buff, b_index, file_error, in_file, line_num);
                    elsif w_id /= BLANK1 then
                         assert false
                             report "Mem_Load:  Additional information on default specification line ignored."
                                    & LF & SPACESTR & "File processing continuing."
                                    & LF & SPACESTR & "Occurred on line number " & i_to_str(line_num)
                                    & " of the input file."
                             severity ERROR;
                         new_line(str_buff, b_index, file_error, in_file, line_num);
                    end if;
                else
                    assert false
                        report "Mem_Load:  Default word length does not match file specification for width of memory. "
                               & " Default ignored."
                               & LF & SPACESTR & "Occurred on line number " & i_to_str(line_num)
                               & " of the input file."
                        severity ERROR;
                    new_line (str_buff, b_index, file_error, in_file, line_num);
                end if;
            
            else
                file_error := 4;
            end if;
        else
            file_error := 4;
        end if;
        if file_error = 4 then  
            assert false
                report "Mem_Load:  Syntax error in default word specification.  Line ignored."
                       & LF & SPACESTR & "Occurred on line number " & i_to_str(line_num) & " of the input file."
                severity ERROR;
           new_line (str_buff, b_index, file_error, in_file, line_num);
           file_error := 0;
        end if;
    end;

    -- parse an assignment statement
    procedure passign ( Variable addr1      : IN integer;               -- starting address of assignment
                        Variable word       : INOUT string;             -- current word
                        Variable str_buff   : INOUT string;             -- string buffer
                        Variable b_index    : INOUT integer;            -- string buffer index
                        Variable file_error : INOUT integer;            -- error?
                        Variable in_file    : IN TEXT;                  -- file
                        Variable file_width : IN integer;               -- width specified by file
                        Variable mem_id     : INOUT mem_id_type;        -- memory
                        Constant hex_size   : IN integer;               -- number of hex digits expected
                        Constant rwidth     : IN integer;               -- # of bits to be written to memory
                        Variable line_num   : INOUT integer             -- line # of file 
                       ) is

    Variable addr : integer := addr1;
    Variable w_id : IDENTIFIER := COLON1;
    Variable tdata : bit_vector (file_width - 1 downto 0);
    Variable data : std_logic_vector (mem_id.width - 1 downto 0);
                       
    begin
        while ( (file_error = 0) and (w_id /= BLANK1) and (w_id /= COMMENT1) ) loop
            get_sym (word, str_buff, b_Index, file_error, in_file, line_num);
            w_id := word_id(word);
            if w_id = HEX_NUM1 then
                if StrLen1(word) = hex_size then
                    data := (others => 'X');
                    tdata := From_HexString1(word)(file_width - 1 downto 0);
                    data(rwidth - 1 downto 0) := bv_To_StdLogicVector(tdata(rwidth - 1 downto 0));
                    Mem_Basic_Write (mem_id, addr, data, TRUE);
                else
                    assert false
                        report "Mem_Load:  Data word length does not match width specification on line:  "
                               & i_to_str(line_num) & "." & LF & SPACESTR
                               & "Data byte skipped."
                        severity ERROR;
                end if;
                addr := addr + 1;
            elsif ( (w_id /= BLANK1) and (w_id /= COMMENT1) ) then
                file_error := 5;            
                assert false
                    report "Mem_Load:  Syntax error on assignment line. Line processed up to error." &
                           LF & SPACESTR & "Occurred on line number " & i_to_str(line_num) & " of the input file."
                    severity ERROR;
                new_line (str_buff, b_index, file_error, in_file, line_num);
            end if;
        end loop;
        if w_id = COMMENT1 then
            new_line (str_buff, b_index, file_error, in_file, line_num);
        end if;
        if file_error = 5 then
             file_error := 0;
        end if;
    end;

    -- parse a range assignment
    procedure prange ( Variable addr1      : IN integer;                -- strarting address
                       Variable word       : INOUT string;              -- current word
                       Variable str_buff   : INOUT string;              -- string buffer
                       Variable b_index    : INOUT integer;             -- string buffer index
                       Variable file_error : INOUT integer;             -- error?
                       Variable in_file    : IN TEXT;                   -- file
                       Variable file_width : IN integer;                -- width specified in file
                       Variable mem_id     : INOUT mem_id_type;         -- memory
                       Constant hex_size   : IN integer;                -- number of hex digits expected
                       Constant rwidth     : IN integer;                -- # of bits to be written to memory
                       Variable line_num   : INOUT integer              -- line # of file
                      ) is

    Variable addr2, addr : integer;
    Variable w_id : IDENTIFIER;
    Variable tdata : bit_vector (file_width - 1 downto 0);
    Variable data : std_logic_vector (mem_id.width - 1 downto 0);
    Variable error_line : integer;
    
    begin
        get_sym (word, str_buff, b_index, file_error, in_file, line_num);
        w_id := word_id(word);
        if w_id = HEX_NUM1 then
            addr2 := from_hex_to_int(word);
            if addr2 < addr1 then
                file_error := 7;
                error_line := line_num;
                new_line (str_buff, b_index, file_error, in_file, line_num);
            else
                get_sym (word, str_buff, b_index, file_error, in_file, line_num);
                w_id := word_id(word);
                if w_id = COLON1 then
                    get_sym (word, str_buff, b_index, file_error, in_file, line_num);
                    w_id := word_id(word);
                    if w_id = HEX_NUM1 then
                        if StrLen1(word) = hex_size then
                            data := (others => 'X');
                            tdata := From_HexString1(word)(file_width - 1 downto 0);
                            data(rwidth - 1 downto 0) := bv_To_StdLogicVector(tdata(rwidth - 1 downto 0));
                            Mem_ALL_Reset (mem_id, data, addr1, addr2, TRUE);
                            get_sym (word, str_buff, b_Index, file_error, in_file, line_num);
                            w_id := word_id(word);
                            if w_id = COMMENT1 then
                                new_line (str_buff, b_index, file_error, in_file, line_num);
                            elsif w_id /= BLANK1 then
                                 assert false
                                     report "Mem_Load:  Additional information on range assignment line ignored."
                                            & LF & SPACESTR & "File processing continuing."
                                            & LF & SPACESTR & "Occurred on line number " & i_to_str(line_num)
                                            & " of the input file."
                                     severity ERROR;
                                 new_line(str_buff, b_index, file_error, in_file, line_num);
                            end if;
                        else
                            assert false
                                report "Mem_Load:  Data word length does not match width specification."
                                       & LF & SPACESTR & "Line skipped"
                                       & LF & SPACESTR & "Occurred on line number " & i_to_str(line_num)
                                       & " of the input file."
                                severity ERROR;
                            new_line(str_buff, b_index, file_error, in_file, line_num);    
                        end if;
                        addr := addr + 1;
                    else 
                        assert false
                            report "Mem_Load:  Syntax Error on range assignment line.  Line skipped."
                                   & LF & SPACESTR & "Occurred on line number " & i_to_str(line_num)
                                   & " of the input file."
                            severity ERROR;
                        new_line(str_buff, b_index, file_error, in_file, line_num);    
                    end if;
                      -- get_sym (word, str_buff, b_index, file_error, in_file, line_num);
                      -- w_id := word_id(word);
                else
                     file_error := 6;
                     error_line := line_num;
                     new_line (str_buff, b_index, file_error, in_file, line_num);
                end if;
            end if;
        else
            file_error := 6;
            new_line (str_buff, b_index, file_error, in_file, line_num);
        end if;
        if file_error = 7 then
            assert false
                report "Mem_load:  Addr2 < Addr1 in range specification.  Line skipped."
                       & LF & SPACESTR & "Occurred on line number " & i_to_str(error_line) & " of the input file."
                severity ERROR;
            file_error := 0;
        end if;
        if file_error = 6 then
            assert false
               report "Mem_load:  Syntax error in range specification.  Line skipped."
                      & LF & SPACESTR & "Occurred on line number " & i_to_str(error_line) & " of the input file."
                severity ERROR;
           file_error := 0;
        end if;
    end;

    -- decide if current statement is a simple assignment of a range assignment
    -- then parse that statment
    procedure p_op_statement ( Variable word       : INOUT string;
                               Variable str_buff   : INOUT string;
                               Variable b_index    : INOUT integer;
                               Variable file_error : INOUT integer;
                               Variable in_file    : IN TEXT;
                               Variable file_width : IN integer;
                               Variable mem_id     : INOUT mem_id_type;
                               Constant hex_size   : IN integer;
                               Constant rwidth     : IN integer;
                               Variable line_num   : INOUT integer
                             ) is

    Variable addr1 : integer;
    Variable w_id : IDENTIFIER;               
    
    begin
        addr1 := from_hex_to_int(word);
        get_sym (word, str_buff, b_index, file_error, in_file, line_num);
        w_id := word_id(word);
        if w_id = COLON1 then
            passign ( addr1, word, str_buff, b_index, file_error, in_file,
                      file_width, mem_id, hex_size, rwidth, line_num);
        elsif w_id = DOTDOT1 then
            prange ( addr1, word, str_buff, b_index, file_error, in_file,
                     file_width, mem_id, hex_size, rwidth, line_num);
        else
            assert false
                report "Mem_Load:   Syntax error.  Line skipped."
                       & LF & SPACESTR & "Occurred on line number " & i_to_str(line_num) & " of the input file."
                severity ERROR;
            new_line (str_buff, b_index, file_error, in_file, line_num);
        end if;
    end;

   ---------------------------------------------------------------------------
    --    Procedure Name  :  Mem_Basic_Read
    --
    --    Purpose         :  To read a "word" from memory
    --
    --    Parameters      :  data           -  contents of memory location
    --                       mem_id         -  ptr to memory data structure
    --                       address        -  address to read from
    --                       refresh_enable -  if true a refresh is performed
    --                                         for DRAMs
    --
    --    NOTE            :  a read refreshes the corresponding row of a DRAM
    --                       ***** this procedure is not exteranlly visible ****
    --
    ---------------------------------------------------------------------------
    

    Procedure Mem_Basic_Read (  Variable data           : OUT std_logic_vector;
                                Variable mem_id         : INOUT mem_id_type;
                                Constant address        : IN NATURAL;
                                Constant refresh_enable : IN BOOLEAN := TRUE
                             ) IS

    Variable alias_data : std_logic_vector(data'length - 1 downto 0) := (others=>'X');
    Variable mem_word   : std_logic_vector(mem_id.width - 1 downto 0);
    Variable row : NATURAL := address/mem_id.columns;
    Variable column : NATURAL := address mod mem_id.columns;
    Variable limit : integer;
    Variable i : NATURAL;
    variable short_ptr : rowptr_type;
                       
    begin
        if address < mem_id.length then        -- check for valid address range
            -- if dram check if woken up and make assertion
            -- nothing else has to be done since data will be invalidated due to refresh period violation
            assert ( (mem_id.memory_type /= DRAM) or (NOT MEM_WARNINGS_ON) or 
                       ( (mem_id.last_init + mem_id.refresh_period) >= NOW) )   
                report "Mem_Read:  Device wake-up time limit exceeded for memory:  "
                           & pstr(mem_id.name(1 to mem_id.name'length))  & LF & SPACESTR &
                           "device must be woken up.  All reads will return X's or default word"
                severity WARNING;
            -- invalidate data if refresh period has expired
            validate_row (mem_id, row);
            -- now refresh row
            if refresh_enable then
                refresh_row (mem_id, row);
            end if;
            --  handle data of different width than memory
            assert ( (data'length = mem_id.width) OR NOT MEM_WARNINGS_ON)
                report "Mem_Read:  return data size does not match word size"
                       & " of mem:  " & pstr(mem_id.name(1 to mem_id.name'length))  & LF & SPACESTR &
                       "return data size:  " & i_to_str(data'length) & " bits"
                        & LF & SPACESTR & "memory word size:  " &
                       i_to_str(mem_id.width) & " bits"
                severity WARNING;
            if (mem_id.row_data_ptr(row).all_xs) then   -- if all xs then return x's
                mem_word := (others => 'X');
            elsif  (mem_id.row_data_ptr(row).rowptr = NULL) then   -- if not allocated return default

                mem_word := mem_id.default.all;

            else
                short_ptr := mem_id.row_data_ptr(row).rowptr;
                for i in 0 to mem_id.width - 1 loop         -- else return word at that location
                    -- mem_word(i) := mem_id.row_data_ptr(row).rowptr(column,i);
                    -- *************************************
                    -- this is a bug work around for synopsys
                    -- replaces line commented out above
                    mem_word(i) := short_ptr(column,i);
                    -- end bug fix
                    -- *************************************
                end loop;
            end if;
            if mem_id.width >= data'length then
                limit := data'length;
            else
                limit := mem_id.width;
            end if;
            for i in 0 to limit - 1 loop
                alias_data(i) := mem_word(i);
            end loop;
        else
            assert false
                 report "Mem_Read:  Passed address exceeds address " & 
                    "range of mem:  " & pstr(mem_id.name(1 to mem_id.name'length))  & LF & SPACESTR 
                    & "specified address:  " & i_to_str(address)  & LF &
                    SPACESTR & "address range:  0 to " & i_to_str(mem_id.length - 1)
                 severity ERROR;
                 alias_data := (others => 'X');
        end if;
        data := alias_data;
    end;

    Procedure Mem_Basic_Read (  Variable data           : OUT std_ulogic;
                                Variable mem_id         : INOUT mem_id_type;
                                Constant address        : IN NATURAL;
                                Constant refresh_enable : IN BOOLEAN := TRUE
                             ) IS

    Variable row : NATURAL := address/mem_id.columns;
    Variable column : NATURAL := address mod mem_id.columns;
    variable short_ptr : rowptr_type;
                       
    begin
        if address < mem_id.length then
            assert ( (mem_id.memory_type /= DRAM) or (NOT MEM_WARNINGS_ON) or
                       ( (mem_id.last_init + mem_id.refresh_period) >= NOW) )
                report "Mem_Read:  Device wake-up time limit exceeded for memory:  "
                           & pstr(mem_id.name(1 to mem_id.name'length))  & LF & SPACESTR &
                           "device must be woken up.  All reads will return X's or default word."
                severity WARNING;
            validate_row (mem_id, row);
            if refresh_enable then
                refresh_row (mem_id, row);
            end if;
            --  handle data of different width than memory
            assert ( (mem_id.width = 1) OR NOT MEM_WARNINGS_ON)
                report "Mem_Read:  return data size does not match word size" 
                       & " of mem:  " & pstr(mem_id.name(1 to mem_id.name'length))  & LF &
                       SPACESTR & "return data size: 1" & LF & SPACESTR & "memory word size:  " &
                       i_to_str(mem_id.width) & " bits"
                severity WARNING;
            if (mem_id.row_data_ptr(row).all_xs) then
                data := 'X';
            elsif (mem_id.row_data_ptr(row).rowptr = NULL) then
                data := mem_id.default(0);
            else
                --data := mem_id.row_data_ptr(row).rowptr(column,0);
                -- *************************************
                -- this is a bug work around for synopsys
                -- replaces line commented out above
                short_ptr := mem_id.row_data_ptr(row).rowptr;
                data := short_ptr(column,0);
                -- end bug fix
                -- *************************************
            end if;
        else
            assert false
                 report "Mem_Read:  Passed address exceeds address " & 
                    "range of mem:  " & pstr(mem_id.name(1 to mem_id.name'length))  & LF & SPACESTR
                    & "specified address:  " & i_to_str(address)  & LF &
                    SPACESTR & "address range:  0 to "
                    & i_to_str(mem_id.length - 1)
                 severity ERROR;
                 data := 'X';
        end if;
    end;                                                  

                                                          
    ---------------------------------------------------------------------------
    --    Procedure Name  :  Mem_Read
    --
    --    Purpose         :  To read a "word" from memory
    --
    --    Parameters      :  mem_id    -  ptr to memory data structure
    --                       address   -  address to read from
    --                       data      -  contents of memory location
    --                       
    --
    --    NOTE            :  a read refreshes row of a DRAM
    --
    --    Use             :  Mem_Read (ROM1, "100100111", data);
    ---------------------------------------------------------------------------

    Procedure Mem_Read (  Variable mem_id    : INOUT mem_id_type;
                          Constant address   : IN std_ulogic_vector;
                          Variable data      : OUT std_ulogic_vector
                       ) IS
                       
    Variable temp : std_logic_vector(data'length - 1 downto 0);

    begin
        Mem_Basic_Read (temp, mem_id, address_trans(mem_id.length, address));
        data := std_ulogic_vector (temp);
    end;                                                  

    
    ---------------------------------------------------------------------------
    --    Procedure Name  :  Mem_Write
    --
    --    Purpose         :  To write a "word" to memory
    --
    --    Parameters      :  mem_id    -  ptr to memory data structure
    --                       address   -  address to read from
    --                       data      -  "word" to be written to memory
    --
    --    NOTE            :  a write refreshes row of a DRAM
    --
    --    Use             :  Mem_Write (ROM1, "100100111", "10X1");
    ---------------------------------------------------------------------------
        
    Procedure Mem_Write (  Variable mem_id    : INOUT mem_id_type;
                           Constant address   : IN std_ulogic_vector;
                           Constant data      : IN std_ulogic_vector
                        ) IS
                        
    begin
        Mem_Basic_Write (  mem_id,
                           Address_trans(mem_id.length, address),
                           To_X01(std_logic_vector(data))
                        );
    end;                                                  

                      
                    

                              
END mem_model;
