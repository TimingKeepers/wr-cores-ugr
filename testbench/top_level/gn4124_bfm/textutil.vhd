library IEEE;
use IEEE.std_logic_1164.all;
use std.textio.all;
use work.util.all;

-----------------------------------------------------------------------------
-- *Module      : textutil
--
-- *Description : Improved Free-format string and line manipulation
--
-- *History: M. Alford (originaly created 1993 with subsequent updates)
-----------------------------------------------------------------------------
package textutil is
	procedure read_token(L : inout line; X : out STRING);
	procedure sget_token(S : in string; P : inout integer; X : out STRING);
	procedure sget_vector(S : in string; P : inout integer; VEC : out STD_ULOGIC_VECTOR);
	procedure sget_vector_64(S : in string; P : inout integer; VEC : out STD_ULOGIC_VECTOR);
	procedure sget_int(S : in string; P : inout integer; X : out integer);
	function  hex_char_to_vector(C : in character) return STD_ULOGIC_VECTOR;
	function  vector_to_hex_char(V : in STD_ULOGIC_VECTOR) return character;
	function  is_hex(C : in character) return BOOLEAN;
	function  hex_char_to_int(C : in character) return integer;
	procedure read_vector(L : inout line; VEC : out STD_ULOGIC_VECTOR);
	procedure read_int(L : inout line; I : out integer);
	procedure write_hex_vector(L : inout line; V : in STD_ULOGIC_VECTOR);
	function  to_str(constant V: in STD_ULOGIC_VECTOR) return STRING;
	function  to_str(constant V: in STD_ULOGIC) return STRING;
	function  to_str(constant val : in INTEGER) return STRING;
	function  to_strn(constant val : in INTEGER; constant n : in INTEGER) return STRING;
end textutil;

package body textutil is

-----------------------------------------------------------------------------
-- *Module      : read_token
--
-- *Description : Skip over spaces then load a token string from a line
--                until either the string is full or the token is finished
--                (i.e. another space).  The output string is padded out
--                with blanks at the end if the token length is less then
--                the full string length.
-----------------------------------------------------------------------------
	procedure read_token(L : inout line; X : out STRING) is
	variable char : character;
	begin
		if(L'length > 0) then
			char := ' ';
			while((char = ' ') and (L'length > 0)) loop	-- Skip spaces
				read(L, char);
			end loop;
			for i in X'low to X'high loop
				X(i) := char;
				if(char /= ' ') then
					if(L'length > 0) then
						read(L, char);
					else
						char := ' ';
					end if;
				end if;
			end loop;
		else
			assert false report "Couldn't read a token from file"
			severity error;
		end if;
	end read_token;

-----------------------------------------------------------------------------
-- *Module      : sget_token
--
-- *Description : Same as read_token except for strings.
-----------------------------------------------------------------------------
	procedure sget_token(S : in string; P : inout integer; X : out STRING) is
	variable char : character;
	begin
		if(S'length > P) then
			char := ' ';
			while((char = ' ') and (S'length >= P)) loop	-- Skip spaces
				char := S(P);
				P := P + 1;
			end loop;
			for i in X'low to X'high loop
				X(i) := char;
				if(char /= ' ') then
					if(S'length > P) then
						char := S(P);
						P := P + 1;
					else
						char := ' ';
					end if;
				end if;
			end loop;
		else
			assert false report "Couldn't read a token from a string"
			severity error;
		end if;
	end sget_token;
			
-----------------------------------------------------------------------------
-- *Module      : hex_char_to_vector
--
-- *Description : Convert a hex character to a vector
-----------------------------------------------------------------------------
	function hex_char_to_vector(C : in character) return STD_ULOGIC_VECTOR is
	variable X : STD_ULOGIC_VECTOR( 3 downto 0);
	begin
		case C is
			when '0' => X := "0000";
			when '1' => X := "0001";
			when '2' => X := "0010";
			when '3' => X := "0011";
			when '4' => X := "0100";
			when '5' => X := "0101";
			when '6' => X := "0110";
			when '7' => X := "0111";
			when '8' => X := "1000";
			when '9' => X := "1001";
			when 'A' => X := "1010";
			when 'B' => X := "1011";
			when 'C' => X := "1100";
			when 'D' => X := "1101";
			when 'E' => X := "1110";
			when 'F' => X := "1111";
			when 'a' => X := "1010";
			when 'b' => X := "1011";
			when 'c' => X := "1100";
			when 'd' => X := "1101";
			when 'e' => X := "1110";
			when 'f' => X := "1111";
			when others => 
				X := "0000";
				assert false report "Invalid Hex Character"
				severity error;
		end case;
		return(X);
	end hex_char_to_vector;

-----------------------------------------------------------------------------
-- *Module      : vector_to_hex_char
--
-- *Description : Convert a vector to a hex character. Only uses low 4 bits.
-----------------------------------------------------------------------------
	function vector_to_hex_char(V : in STD_ULOGIC_VECTOR) return character is
	variable C  : character;
	variable VV : STD_ULOGIC_VECTOR(3 downto 0);
	begin
		if(V'length < 4) then
			VV := To_X01(V(V'low + 3 downto V'low));
		else
			VV := To_X01(V(V'low + V'length - 1 downto V'low));
		end if;
		case VV is
			when "0000" => C := '0';
			when "0001" => C := '1';
			when "0010" => C := '2';
			when "0011" => C := '3';
			when "0100" => C := '4';
			when "0101" => C := '5';
			when "0110" => C := '6';
			when "0111" => C := '7';
			when "1000" => C := '8';
			when "1001" => C := '9';
			when "1010" => C := 'A';
			when "1011" => C := 'B';
			when "1100" => C := 'C';
			when "1101" => C := 'D';
			when "1110" => C := 'E';
			when "1111" => C := 'F';
			when others => C := 'X';
		end case;
		return(C);
	end vector_to_hex_char;

-----------------------------------------------------------------------------
-- *Module      : is_hex
--
-- *Description : report if a char is ASCII hex
-----------------------------------------------------------------------------
	function is_hex(C : in character) return BOOLEAN is
	variable X : boolean;
	begin
		case C is
			when '0' => X := TRUE;
			when '1' => X := TRUE;
			when '2' => X := TRUE;
			when '3' => X := TRUE;
			when '4' => X := TRUE;
			when '5' => X := TRUE;
			when '6' => X := TRUE;
			when '7' => X := TRUE;
			when '8' => X := TRUE;
			when '9' => X := TRUE;
			when 'A' => X := TRUE;
			when 'B' => X := TRUE;
			when 'C' => X := TRUE;
			when 'D' => X := TRUE;
			when 'E' => X := TRUE;
			when 'F' => X := TRUE;
			when 'a' => X := TRUE;
			when 'b' => X := TRUE;
			when 'c' => X := TRUE;
			when 'd' => X := TRUE;
			when 'e' => X := TRUE;
			when 'f' => X := TRUE;
			when others => 
				X := FALSE;
		end case;
		return(X);
	end is_hex;

-----------------------------------------------------------------------------
-- *Module      : hex_char_to_int
--
-- *Description : Convert a hex character to an integer
-----------------------------------------------------------------------------
	function hex_char_to_int(C : in character) return integer is
	variable X : integer;
	begin
		case C is
			when '0' => X := 0;
			when '1' => X := 1;
			when '2' => X := 2;
			when '3' => X := 3;
			when '4' => X := 4;
			when '5' => X := 5;
			when '6' => X := 6;
			when '7' => X := 7;
			when '8' => X := 8;
			when '9' => X := 9;
			when 'A' => X := 10;
			when 'B' => X := 11;
			when 'C' => X := 12;
			when 'D' => X := 13;
			when 'E' => X := 14;
			when 'F' => X := 15;
			when 'a' => X := 10;
			when 'b' => X := 11;
			when 'c' => X := 12;
			when 'd' => X := 13;
			when 'e' => X := 14;
			when 'f' => X := 15;
			when others => 
				X := 0;
				assert false report "Invalid Hex Character"
				severity error;
		end case;
		return(X);
	end hex_char_to_int;
			
-----------------------------------------------------------------------------
-- *Module      : read_vector
--
-- *Description : load a vector from the input line in a free floating format
-----------------------------------------------------------------------------
	procedure read_vector(L : inout line; VEC : out STD_ULOGIC_VECTOR) is
	variable char : character;
	variable base : integer;
	variable q : integer;
	variable v : STD_ULOGIC_VECTOR(31 downto 0);
	begin
		if(L'length > 0) then
			char := ' ';
			while(char = ' ') loop	-- Skip spaces
				read(L, char);
			end loop;
			base := 16;			-- Hex is the default
			if(char = '%') then -- determine base
				read(L, char);
				if(char = 'b' or char = 'B') then
					base := 2;
				elsif(char = 'x' or char = 'X') then
					base := 16;
				elsif(char = 'd' or char = 'D') then
					base := 10;
				else
					assert false report "Unsupported Base detected when reading a Vector"
					severity error;
				end if;
				read(L, char);
			end if;
			q := 0;
			if(is_hex(char)) then
				q := q * base + hex_char_to_int(char);
				while(is_hex(char) and not (L'length = 0))  loop
					read(L, char);
					if(is_hex(char)) then
						q := q * base + hex_char_to_int(char);
					end if;
				end loop;
			end if;			
			if(q < 0) then
				q := q-2147483648;
				V(30 downto 0) := to_vector(q, 31);
				V(31) := '1';
			else
				V(30 downto 0) := to_vector(q, 31);
				V(31) := '0';
			end if;
			VEC := V((VEC'high - VEC'low) downto 0);
		else
			assert false report "Couldn't read a vector"
			severity error;
		end if;
	end read_vector;

-----------------------------------------------------------------------------
-- *Module      : sget_vector
--
-- *Description : Same as sget_vector except for strings
-----------------------------------------------------------------------------
	procedure sget_vector(S : in string; P : inout integer; VEC : out STD_ULOGIC_VECTOR) is
	variable char : character;
	variable base : integer;
	variable q : integer;
	variable v : STD_ULOGIC_VECTOR(31 downto 0);
	begin
		while(S(P) = ' ') loop	-- Skip spaces
			if(P >= S'length) then
				P := S'length;
				exit;
			end if;
			P := P + 1;
		end loop;
		if(S'length > P) then
			char := S(P);
			if(char = '"') then -- read in as a literal
				q := v'high;
				v := (others => 'U');
				VEC := v(VEC'range);
				char := ' ';
				P := P + 1;
				while((char /= '"') and not (S'length = P))  loop
					char := S(P);
					P := P + 1;				
					case char is
						when '0' =>
							v(q) := '0';
						when '1' =>
							v(q) := '1';
						when 'L' | 'l' =>
							v(q) := 'L';
						when 'H' | 'h' =>
							v(q) := 'H';
						when 'Z' | 'z' =>
							v(q) := 'Z';
						when 'X' | 'x' =>
							v(q) := 'X';
						when 'U' | 'u' =>
							v(q) := 'U';
						when others =>
--							char := '"';
							exit;
					end case;
					q := q - 1;
				end loop;
				if(v'high-q < 2) then -- only a single bit was read
					VEC(VEC'low) := v(v'high);
				elsif((v'high - q) > VEC'length) then -- too many bits
					VEC := v(q+VEC'length downto q+1);
				else -- the number of bits read is same or less than required
					VEC(v'high-q-1+VEC'low downto VEC'low) := v(v'high downto q+1);
				end if;
			else
				base := 16;			-- Hex is the default
				if(char = '%') then -- determine base
					P := P + 1;			
					char := S(P);
					P := P + 1;			
					if(char = 'b' or char = 'B') then
						base := 2;
					elsif(char = 'x' or char = 'X') then
						base := 16;
					elsif(char = 'd' or char = 'D') then
						base := 10;
					else
						assert false report "Unsupported Base detected when reading a Vector"
						severity error;
					end if;
				elsif((char = '0') and ((S(P+1) = 'x') or (S(P+1) = 'X'))) then
					P := P + 2;
				end if;
				q := 0;
				char := S(P);
				if(is_hex(char)) then
					while(is_hex(char) and not (S'length = P))  loop
						if(is_hex(char)) then
							q := q * base + hex_char_to_int(char);
						end if;
						P := P + 1;				
						char := S(P);
					end loop;
				end if;			
				if(q < 0) then
					q := q-2147483648;
					V(30 downto 0) := to_vector(q, 31);
					V(31) := '1';
				else
					V(30 downto 0) := to_vector(q, 31);
					V(31) := '0';
				end if;
				VEC := V((VEC'high - VEC'low) downto 0);
			end if;
		else
			assert false report "Couldn't read a vector"
			severity error;
			V := (others => '0');
			VEC := V((VEC'high - VEC'low) downto 0);
		end if;
	end sget_vector;
	
-----------------------------------------------------------------------------
-- *Module      : sget_vector_64
--
-- *Description : Same as sget_vector except can handle 64 bit quantities and hex or binary base (no base 10)
-----------------------------------------------------------------------------
	procedure sget_vector_64(S : in string; P : inout integer; VEC : out STD_ULOGIC_VECTOR) is
	variable char : character;
	variable base : integer;
	variable q : integer;
	variable v : STD_ULOGIC_VECTOR(63 downto 0);
	begin
		while(S(P) = ' ') loop	-- Skip spaces
			if(P >= S'length) then
				P := S'length;
				exit;
			end if;
			P := P + 1;
		end loop;
		if(S'length > P) then
			char := S(P);
			if(char = '"') then -- read in as a literal
				q := v'high;
				v := (others => 'U');
				VEC := v(VEC'range);
				char := ' ';
				P := P + 1;
				while((char /= '"') and not (S'length = P))  loop
					char := S(P);
					P := P + 1;				
					case char is
						when '0' =>
							v(q) := '0';
						when '1' =>
							v(q) := '1';
						when 'L' | 'l' =>
							v(q) := 'L';
						when 'H' | 'h' =>
							v(q) := 'H';
						when 'Z' | 'z' =>
							v(q) := 'Z';
						when 'X' | 'x' =>
							v(q) := 'X';
						when 'U' | 'u' =>
							v(q) := 'U';
						when others =>
--							char := '"';
							exit;
					end case;
					q := q - 1;
				end loop;
				if(v'high-q < 2) then -- only a single bit was read
					VEC(VEC'low) := v(v'high);
				elsif((v'high - q) > VEC'length) then -- too many bits
					VEC := v(q+VEC'length downto q+1);
				else -- the number of bits read is same or less than required
					VEC(v'high-q-1+VEC'low downto VEC'low) := v(v'high downto q+1);
				end if;
			else
				base := 16;			-- Hex is the default
				if(char = '%') then -- determine base
					P := P + 1;			
					char := S(P);
					P := P + 1;			
					if(char = 'b' or char = 'B') then
						base := 2;
					elsif(char = 'x' or char = 'X') then
						base := 16;
					else
						assert false report "Unsupported Base detected when reading a Vector"
						severity error;
					end if;
					char := S(P);
--					P := P + 1;	
				elsif((char = '0') and ((S(P+1) = 'x') or (S(P+1) = 'X'))) then
					P := P + 2;
				end if;
				v := (others => '0');
				char := S(P);
				if(base = 2) then
					while(((char = '0') or (char = '1')) and not (P > S'length))  loop
						if(char = '0') then
							v := v(v'high-1 downto 0) & '0';
						else
							v := v(v'high-1 downto 0) & '1';
						end if;
						P := P + 1;				
						char := S(P);
					end loop;
				else
					while(is_hex(char) and not (P > S'length))  loop
						if(is_hex(char)) then
							v := v(v'high-4 downto 0) & hex_char_to_vector(char);
						end if;
						P := P + 1;				
						char := S(P);
					end loop;
				end if;
				VEC := V((VEC'high - VEC'low) downto 0);
			end if;
		else
			assert false report "Couldn't read a vector"
			severity error;
			V := (others => '0');
			VEC := V((VEC'high - VEC'low) downto 0);
		end if;
	end sget_vector_64;
	
-----------------------------------------------------------------------------
-- *Module      : read_int
--
-- *Description : load an integer from the input line in a free floating format
-----------------------------------------------------------------------------
	procedure read_int(L : inout line; I : out integer) is
	variable char : character;
	variable base : integer;
	variable q : integer;
	begin
		if(L'length > 0) then
			char := ' ';
			while(char = ' ') loop	-- Skip spaces
				read(L, char);
			end loop;
			base := 16;			-- Hex is the default
			if(char = '%') then -- determine base
				read(L, char);
				if(char = 'b' or char = 'B') then
					base := 2;
				elsif(char = 'x' or char = 'X') then
					base := 16;
				elsif(char = 'd' or char = 'D') then
					base := 10;
				else
					assert false report "Unsupported Base detected when reading an integer"
					severity error;
				end if;
				read(L, char);
			end if;
			q := 0;
			if(is_hex(char)) then
				q := q * base + hex_char_to_int(char);
				while(is_hex(char) and not (L'length = 0))  loop
					read(L, char);
					if(is_hex(char)) then
						q := q * base + hex_char_to_int(char);
					end if;
				end loop;
			end if;
			I := q;
		else
			assert false report "Couldn't read an integer"
			severity error;
		end if;
	end read_int;

-----------------------------------------------------------------------------
-- *Module      : sget_int
--
-- *Description : Same as read_int except for strings
-----------------------------------------------------------------------------
	procedure sget_int(S : in string;  P : inout integer; X : out integer) is
	variable char : character;
	variable base : integer;
	variable q : integer;
	begin
		if(S'length > P) then
			char := ' ';
			while(char = ' ') loop	-- Skip spaces
				char := S(P);
				P := P + 1;
			end loop;
			base := 16;			-- Hex is the default
			if(char = '%') then -- determine base
				char := S(P);
				P := P + 1;			
				if(char = 'b' or char = 'B') then
					base := 2;
				elsif(char = 'x' or char = 'X') then
					base := 16;
				elsif(char = 'd' or char = 'D') then
					base := 10;
				else
					assert false report "Unsupported Base detected when reading an integer"
					severity error;
				end if;
				char := S(P);
				P := P + 1;
			end if;
			q := 0;
			if(is_hex(char)) then
				q := q * base + hex_char_to_int(char);
				while(is_hex(char) and not (S'length = P))  loop
					char := S(P);
					P := P + 1;					
					if(is_hex(char)) then
						q := q * base + hex_char_to_int(char);
					end if;
				end loop;
			end if;
			X := q;
		else
			assert false report "Couldn't read an integer"
			severity error;
		end if;
	end sget_int;

-----------------------------------------------------------------------------
-- *Module      : write_hex_vector
--
-- *Description : writes out a vector as hex
-----------------------------------------------------------------------------
	procedure write_hex_vector(L : inout line; V : in STD_ULOGIC_VECTOR) is
	variable C : character;
	variable VV : STD_ULOGIC_VECTOR(((V'length + 3)/4) * 4 - 1 downto 0);
	begin

		VV := (others => '0');
		VV(V'length -1 downto 0) := V;

		for i in VV'length/4 - 1 downto 0 loop
			C := vector_to_hex_char(VV(i*4+3 downto i*4));
			write(L, C);
		end loop;

	end write_hex_vector;

-----------------------------------------------------------------------------
-- *Module      : to_str
--
-- *Description : Converts a STD_ULOGIC_VECTOR to a string of the same length
-----------------------------------------------------------------------------
	function to_str(constant V: in STD_ULOGIC_VECTOR) return STRING is
	variable S : STRING(1 to V'length);
	variable sp : integer;
	begin
		sp := 1;
		for i in V'range loop
			case V(i) is
				when '1' | 'H' =>
					S(sp) := '1';
				when '0' | 'L' =>
					S(sp) := '0';
				when others =>
					S(sp) := 'X';
			end case;
			sp := sp + 1;
		end loop;
		return(S);
	end to_str;

-----------------------------------------------------------------------------
-- *Module      : to_str
--
-- *Description : Converts a STD_ULOGIC to a string
-----------------------------------------------------------------------------
	function to_str(constant V: in STD_ULOGIC) return STRING is
--	variable S : STRING(1);
	begin
			case V is
				when '1' | 'H' =>
					return("1");
				when '0' | 'L' =>
					return("0");
				when others =>
					return("X");
			end case;
		return("X");
	end to_str;

-----------------------------------------------------------------------------
-- *Module      : to_str
--
-- *Description : Converts a integer to a string
-----------------------------------------------------------------------------
	function  to_str(constant val : in INTEGER) return STRING is
	variable result : STRING(11 downto 1) := "-2147483648"; -- smallest integer and longest string
	variable tmp    : INTEGER;
	variable pos    : NATURAL := 1;
	variable digit  : NATURAL;
	begin
		-- for the smallest integer MOD does not seem to work...
		--if val = -2147483648 then	: compilation error with Xilinx tools...
		if val < -2147483647 then
			pos := 12;
		else
			tmp := abs(val);
			loop
		  		digit := abs(tmp MOD 10);
			   		tmp := tmp / 10;
		    		result(pos) := character'val(character'pos('0') + digit);
		    		pos := pos + 1;
		    		exit when tmp = 0;
			end loop;
			if val < 0 then
		   		result(pos) := '-';
		   		pos := pos + 1;
			end if;
		end if;
		return result((pos-1) downto 1);
	end to_str;

-----------------------------------------------------------------------------
-- *Module      : to_strn
--
-- *Description : Converts an integer to a string of length N
-----------------------------------------------------------------------------
	function  to_strn(constant val : in INTEGER; constant n : in INTEGER) return STRING is
	variable result : STRING(11 downto 1) := "-2147483648"; -- smallest integer and longest string
	variable tmp    : INTEGER;
	variable pos    : NATURAL := 1;
	variable digit  : NATURAL;
	begin
		-- for the smallest integer MOD does not seem to work...
		--if val = -2147483648 then	: compilation error with Xilinx tools...
		if val < -2147483647 then
			pos := 12;
		else
			result := (others => ' ');
			tmp := abs(val);
			loop
		  		digit := abs(tmp MOD 10);
			   		tmp := tmp / 10;
		    		result(pos) := character'val(character'pos('0') + digit);
		    		pos := pos + 1;
		    		exit when tmp = 0;
			end loop;
			if val < 0 then
		   		result(pos) := '-';
		   		pos := pos + 1;
			end if;
		end if;
		return result(n downto 1);
	end to_strn;


end textutil;
