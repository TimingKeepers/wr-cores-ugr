library ieee;
use ieee.std_logic_1164.all;
--library synopsys;
--use synopsys.arithmetic.all;

package UTIL is

  type t_cmd_array is array (1 to 256) of integer;
  
  
	function to_mvl ( b: in boolean ) return STD_ULOGIC;
	function to_mvl ( i: in integer ) return STD_ULOGIC;
	function to_vector(input,num_bits:integer) return STD_ULOGIC_VECTOR;
--	function to_signed( b: in std_ulogic_vector ) return signed;
--	function to_std_ulogic_vector( b: in signed ) return std_ulogic_vector;
--	function std_logic_to_std_ulogic( b: in std_logic ) return std_ulogic;
--	function std_ulogic_to_std_logic( b: in std_ulogic ) return std_logic;
	function "and"(l: STD_ULOGIC_VECTOR; r: STD_ULOGIC) return STD_ULOGIC_VECTOR;
	function "and"(l: STD_ULOGIC; r: STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
	function "and"(l: STD_ULOGIC_VECTOR; r: BOOLEAN) return STD_ULOGIC_VECTOR;
	function "and"(l: BOOLEAN; r: STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
	function "and"(l: BOOLEAN; r: STD_ULOGIC) return STD_ULOGIC;
	function "and"(l: STD_ULOGIC; r: BOOLEAN) return STD_ULOGIC;
	function exp(input: STD_ULOGIC; num_bits: integer) return STD_ULOGIC_VECTOR;
	function exp(input: STD_ULOGIC_VECTOR; num_bits: integer) return STD_ULOGIC_VECTOR;
	function conv_integer ( ARG: in STD_ULOGIC_VECTOR ) return integer;
	function "+"(l: STD_ULOGIC_VECTOR; r: STD_ULOGIC) return STD_ULOGIC_VECTOR;
--	function "+"(l: STD_ULOGIC_VECTOR; r: STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
--	function "-"(l: STD_ULOGIC_VECTOR; r: STD_ULOGIC) return STD_ULOGIC_VECTOR;
--	function "-"(l: STD_ULOGIC_VECTOR; r: STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
	function to_int(l: std_ulogic_vector) return natural;
	function to_int(l: std_ulogic) return natural;
	function and_reduce(ARG: STD_ULOGIC_VECTOR) return STD_ULOGIC;
	function nand_reduce(ARG: STD_ULOGIC_VECTOR) return STD_ULOGIC;
	function or_reduce(ARG: STD_ULOGIC_VECTOR) return STD_ULOGIC;
	function nor_reduce(ARG: STD_ULOGIC_VECTOR) return STD_ULOGIC;
	function xor_reduce(ARG: STD_ULOGIC_VECTOR) return STD_ULOGIC;
	function xnor_reduce(ARG: STD_ULOGIC_VECTOR) return STD_ULOGIC;
	function ge ( l, r : STD_ULOGIC_VECTOR ) return BOOLEAN;
	function gt ( l, r : STD_ULOGIC_VECTOR ) return BOOLEAN;
	function lt ( l, r : STD_ULOGIC_VECTOR ) return BOOLEAN;
	function eq ( l, r : STD_ULOGIC_VECTOR ) return BOOLEAN;
	function maximum ( arg1, arg2 : INTEGER) return INTEGER;
	function minimum ( arg1, arg2 : INTEGER) return INTEGER;
	procedure keep(signal X: inout STD_LOGIC);
	function log2(A: in integer) return integer;
    -------------------------------------------------------------------
    -- Declaration of Synthesis directive attributes
    -------------------------------------------------------------------
    ATTRIBUTE synthesis_return : string ;

end UTIL;

package body UTIL is

    --------------------------------------------------------------------
--	function to_signed ( b: in std_ulogic_vector ) return signed is
--		variable result	: signed(b'range);
--	begin
--		for i in b'range loop
--			result(i) := b(i);
--		end loop;
--		return result;
--	end to_signed;

    --------------------------------------------------------------------
--	function to_std_ulogic_vector ( b: in signed ) return std_ulogic_vector is
--		variable result	: std_ulogic_vector(b'range);
--	begin
--		for i in b'range loop
--			result(i) := b(i);
--		end loop;
--		return result;
--	end to_std_ulogic_vector;

    --------------------------------------------------------------------

	function to_mvl ( b: in boolean ) return STD_ULOGIC is
	begin
		if ( b = TRUE ) then
			return( '1' );
		else
			return( '0' );
		end if;
	end to_mvl;

    --------------------------------------------------------------------

	function to_mvl ( i: in integer ) return STD_ULOGIC is
	begin
		if ( i = 1 ) then
			return( '1' );
		else
			return( '0' );
		end if;
	end to_mvl;

    --------------------------------------------------------------------

	function "and"(l: STD_ULOGIC; r: STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
	variable rr: STD_ULOGIC_vector(r'range);
	begin
		if (l = '1') then
			rr := r;
		else
			rr := (others => '0');
		end if;
		return(rr);
	end;

    --------------------------------------------------------------------

	function "and"(l: STD_ULOGIC_VECTOR; r: STD_ULOGIC) return STD_ULOGIC_VECTOR is
	variable ll: STD_ULOGIC_vector(l'range);
	begin
		if (r = '1') then
			ll := l;
		else
			ll := (others => '0');
		end if;
		return(ll);
	end;

    --------------------------------------------------------------------

	function "and"(l: BOOLEAN; r: STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
	variable rr: STD_ULOGIC_vector(r'range);
	begin
		if (l) then
			rr := r;
		else
			rr := (others => '0');
		end if;
		return(rr);
	end;

    --------------------------------------------------------------------

	function "and"(l: STD_ULOGIC_VECTOR; r: BOOLEAN) return STD_ULOGIC_VECTOR is
	variable ll: STD_ULOGIC_vector(l'range);
	begin
		if (r) then
			ll := l;
		else
			ll := (others => '0');
		end if;
		return(ll);
	end;

    --------------------------------------------------------------------

	function "and"(l: BOOLEAN; r: STD_ULOGIC) return STD_ULOGIC is
	variable ll: STD_ULOGIC;
	begin
		if (l) then
			ll := r;
		else
			ll := '0';
		end if;
		return(ll);
	end;

    --------------------------------------------------------------------

	function "and"(l: STD_ULOGIC; r: BOOLEAN) return STD_ULOGIC is
	variable ll: STD_ULOGIC;
	begin
		if (r) then
			ll := l;
		else
			ll := '0';
		end if;
		return(ll);
	end;

    --------------------------------------------------------------------

--	function std_ulogic_to_std_logic(b : std_ulogic) return std_logic is
--	variable result: std_logic;
--	begin
--		result := b;
--		return result;
--	end;

    --------------------------------------------------------------------
 
--	function std_logic_to_std_ulogic(b : std_logic) return std_ulogic is
--	variable result: std_ulogic;
--	begin
--		result := b;
--		return result;
--	end;
 
	--------------------------------------------------------------------

	function to_vector(input,num_bits: integer) return std_ulogic_vector is
		variable vec: std_ulogic_vector(num_bits-1 downto 0);
		variable a: integer;
	begin
		a := input;
		for i in 0 to num_bits-1 loop
			if ((a mod 2) = 1) then
				vec(i) := '1';
			else
				vec(i) := '0';
			end if;
			a := a / 2;
		end loop;
		return vec;
	end to_vector;



--	FUNCTION  to_vector(input,num_bits:integer) RETURN STD_ULOGIC_VECTOR IS
--	VARIABLE result:STD_ULOGIC_VECTOR(num_bits-1 DOWNTO 0);
--	VARIABLE weight:integer;
--	VARIABLE temp:integer;
--	BEGIN
--		weight := 2**(num_bits-1);
--		temp := input;
--		FOR i in result'HIGH DOWNTO result'LOW LOOP
--			IF temp >= weight THEN
--				result(i) := '1';
--				temp := temp - weight;
--			ELSE
--				result(i) := '0';
--			END IF;
--			weight := weight/2;
--		END LOOP;
--		RETURN result;
--	END to_vector;

	--------------------------------------------------------------------
	-- exp: Expand one bit into many
	--------------------------------------------------------------------

	FUNCTION  exp(input:STD_ULOGIC; num_bits:integer) RETURN STD_ULOGIC_VECTOR IS
	VARIABLE result:STD_ULOGIC_VECTOR(num_bits-1 DOWNTO 0);
	BEGIN
		FOR i in result'HIGH DOWNTO result'LOW LOOP
			result(i) := input;
		END LOOP;
		RETURN result;
	END exp;

	--------------------------------------------------------------------
	-- exp: Expand n bits into m bits
	--------------------------------------------------------------------

	FUNCTION  exp(input:STD_ULOGIC_VECTOR; num_bits:integer) RETURN STD_ULOGIC_VECTOR IS
	VARIABLE result:STD_ULOGIC_VECTOR(num_bits-1 DOWNTO 0);
	BEGIN
		result(input'high-input'low downto 0) := input;
		result(num_bits-1 downto input'high-input'low+1) := (others => '0');
		RETURN result;
	END exp;

	--------------------------------------------------------------------
	-- conv_integer
	--------------------------------------------------------------------

	function conv_integer ( ARG: in STD_ULOGIC_VECTOR ) return integer is
	variable result: INTEGER;
	begin
		assert ARG'length <= 31
			report "ARG is too large in CONV_INTEGER"
			severity FAILURE;
		result := 0;
		for i in ARG'range loop
			result := result * 2;
			if(ARG(i) = 'H' or ARG(i) = '1') then
				result := result + 1;
			end if;
		end loop;
		return result;
	end;

	--------------------------------------------------------------------
	-- "+" Increment function
	--------------------------------------------------------------------
	function "+"(L: STD_ULOGIC_VECTOR; R: STD_ULOGIC) return STD_ULOGIC_VECTOR is
	variable Q: STD_ULOGIC_VECTOR(L'range);
	variable A: STD_ULOGIC;
	begin
		A := R;
		for i in L'low to L'high loop
			Q(i) := L(i) xor A;
			A := A and L(i);
		end loop;
		return Q;
	end;

	--------------------------------------------------------------------
	-- "+" adder function
	--------------------------------------------------------------------
--	function "+"(L: STD_ULOGIC_VECTOR; R: STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
--	variable Q	 : SIGNED(L'range);
--	variable result: STD_ULOGIC_VECTOR(L'range);
--	begin
--		Q      := to_signed(L) + to_signed(R);
--		result := to_std_ulogic_vector(Q);
--		return result;
--	end;

	--------------------------------------------------------------------
	-- "-" Decrement function
	--------------------------------------------------------------------
--	function "-"(L: STD_ULOGIC_VECTOR; R: STD_ULOGIC) return STD_ULOGIC_VECTOR is
--	variable Q: STD_ULOGIC_VECTOR(L'range);
--	variable A: STD_ULOGIC;
--	begin
--		A := R;
--		for i in L'low to L'high loop
--			Q(i) := L(i) xor A;
--			A := A and not L(i);
--		end loop;
--		return Q;
--	end;

	--------------------------------------------------------------------
	-- "-" subtractor function
	--------------------------------------------------------------------
--	function "-"(L: STD_ULOGIC_VECTOR; R: STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
--	variable Q     : SIGNED(L'range);
--	variable result: STD_ULOGIC_VECTOR(L'range);
--	begin
--		Q      := to_signed(L) - to_signed(R);
--		result := to_std_ulogic_vector(Q);
--		return result;
--	end;

	--------------------------------------------------------------------
	-- to_int : Convert std_ulogic_vector to an integer
	--------------------------------------------------------------------
	function to_int(l: std_ulogic_vector) return natural is
	variable result: natural := 0;
	begin
		for t1 in l'range loop
			result := result * 2;
			if (l(t1) = '1') or (l(t1) = 'H') then
				result := result + 1;
			end if;
		end loop;
		return result;
	end to_int;

	--------------------------------------------------------------------
	-- to_int : Convert std_ulogic_vector to an integer
	--------------------------------------------------------------------
	function to_int(l: std_ulogic) return natural is
	variable result: natural := 0;
	begin
		if (l = '1') or (l = 'H') then
			result := 1;
		else
			result := 0;
		end if;
		return result;
	end to_int;

	--------------------------------------------------------------------
	-- Reduce Functions
	--------------------------------------------------------------------
	function and_reduce(ARG: STD_ULOGIC_VECTOR) return STD_ULOGIC is
	variable result: STD_ULOGIC;
	begin
	result := '1';
	for i in ARG'range loop
		result := result and ARG(i);
	end loop;
	return result;
	end;

	function nand_reduce(ARG: STD_ULOGIC_VECTOR) return STD_ULOGIC is
	begin
	return not and_reduce(ARG);
	end;

	function or_reduce(ARG: STD_ULOGIC_VECTOR) return STD_ULOGIC is
	variable result: STD_ULOGIC;
	begin
	result := '0';
	for i in ARG'range loop
		result := result or ARG(i);
	end loop;
	return result;
	end;

	function nor_reduce(ARG: STD_ULOGIC_VECTOR) return STD_ULOGIC is
	begin
	return not or_reduce(ARG);
	end;

	function xor_reduce(ARG: STD_ULOGIC_VECTOR) return STD_ULOGIC is
	variable result: STD_ULOGIC;
	begin
	result := '0';
	for i in ARG'range loop
		result := result xor ARG(i);
	end loop;
	return result;
	end;

	function xnor_reduce(ARG: STD_ULOGIC_VECTOR) return STD_ULOGIC is
	begin
	return not xor_reduce(ARG);
	end;

	--------------------------------------------------------------------
	-- Some useful generic functions
	--------------------------------------------------------------------
	--//// Zero Extend ////
	--
	-- Function zxt
	--
	FUNCTION zxt( q : STD_ULOGIC_VECTOR; i : INTEGER ) RETURN STD_ULOGIC_VECTOR IS
		VARIABLE qs : STD_ULOGIC_VECTOR (1 TO i);
		VARIABLE qt : STD_ULOGIC_VECTOR (1 TO q'length);
		-- Hidden function. Synthesis directives are present in its callers
	BEGIN
		qt := q;
		IF i < q'length THEN
			qs := qt( (q'length-i+1) TO qt'right);
		ELSIF i > q'length THEN
			qs := (OTHERS=>'0');
			qs := qs(1 TO (i-q'length)) & qt;
		ELSE
			qs := qt;
		END IF;
		RETURN qs;
	END;

	FUNCTION maximum (arg1,arg2:INTEGER) RETURN INTEGER IS
	BEGIN
		IF(arg1 > arg2) THEN
			RETURN(arg1) ;
		ELSE
			RETURN(arg2) ;
		END IF;
	END ;

	FUNCTION minimum (arg1,arg2:INTEGER) RETURN INTEGER IS
	BEGIN
		IF(arg1 < arg2) THEN
			RETURN(arg1) ;
		ELSE
			RETURN(arg2) ;
		END IF;
	END ;

	--------------------------------------------------------------------
	-- Comparision functions
	--------------------------------------------------------------------
--
-- Equal functions.
--
	TYPE stdlogic_boolean_table IS ARRAY(std_ulogic, std_ulogic) OF BOOLEAN;

	CONSTANT eq_table : stdlogic_boolean_table := (
	--
	 ----------------------------------------------------------------------------
	--      |  U       X      0     1      Z      W      L      H      D |   |
    --
     ----------------------------------------------------------------------------
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | U |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | X |
    ( FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE ),    -- | 0 |
    ( FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE ),    -- | 1 |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | Z |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | W |
    ( FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE ),    -- | L |
    ( FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE ),    -- | H |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE )   -- | D |
    );

    FUNCTION eq  ( l, r : STD_LOGIC ) RETURN BOOLEAN IS
        -- Equal for two logic types  
        VARIABLE result : BOOLEAN ;
        ATTRIBUTE synthesis_return OF result:VARIABLE IS "EQ" ; 
    BEGIN
        result := eq_table( l, r );
        RETURN result ;
    END;

    FUNCTION eq  ( l,r : STD_ULOGIC_VECTOR ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt  : STD_ULOGIC_VECTOR ( 1 TO ml );
        VARIABLE rt  : STD_ULOGIC_VECTOR ( 1 TO ml );
        -- Arithmetic Equal for two Unsigned vectors  
        VARIABLE result : BOOLEAN ;
        ATTRIBUTE synthesis_return OF result:VARIABLE IS "EQ" ; 
    BEGIN
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'range LOOP
            IF NOT eq( lt(i), rt(i) ) THEN
               result := FALSE;
               RETURN result ;
            END IF;
        END LOOP;
        RETURN TRUE;
    END;

    TYPE std_ulogic_fuzzy_state IS ('U', 'X', 'T', 'F', 'N');
    TYPE std_ulogic_fuzzy_state_table IS ARRAY ( std_ulogic, std_ulogic ) OF std_ulogic_fuzzy_state;

    CONSTANT ge_fuzzy_table : std_ulogic_fuzzy_state_table := (
	--      ----------------------------------------------------
	--      |  U    X    0    1    Z    W    L    H    D         |   |
	--      ----------------------------------------------------
            ( 'U', 'U', 'N', 'U', 'U', 'U', 'N', 'U', 'U' ),  -- | U |
	        ( 'U', 'X', 'N', 'X', 'X', 'X', 'N', 'X', 'X' ),  -- | X |
	        ( 'U', 'X', 'N', 'F', 'X', 'X', 'N', 'F', 'X' ),  -- | 0 |
	        ( 'N', 'N', 'T', 'N', 'N', 'N', 'T', 'N', 'N' ),  -- | 1 |
	        ( 'U', 'X', 'N', 'X', 'X', 'X', 'N', 'X', 'X' ),  -- | Z |
	        ( 'U', 'X', 'N', 'X', 'X', 'X', 'N', 'X', 'X' ),  -- | W |
	        ( 'U', 'X', 'N', 'F', 'X', 'X', 'N', 'F', 'X' ),  -- | L |
	        ( 'N', 'N', 'T', 'N', 'N', 'N', 'T', 'N', 'N' ),  -- | H |
	        ( 'U', 'X', 'N', 'X', 'X', 'X', 'N', 'X', 'X' )   -- | D |
    );

    FUNCTION ge  ( L,R : std_ulogic_vector ) RETURN boolean IS
        CONSTANT ml  : integer := maximum( L'LENGTH, R'LENGTH );
        VARIABLE lt  : std_ulogic_vector ( 1 to ml );
        VARIABLE rt  : std_ulogic_vector ( 1 to ml );
        VARIABLE res : std_ulogic_fuzzy_state;
        -- Greater-than-or-equal for two Unsigned vectors
        VARIABLE result : BOOLEAN ;
        ATTRIBUTE synthesis_return OF result:VARIABLE IS "GTE" ; 
    begin
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'RANGE LOOP
            res := ge_fuzzy_table( lt(i), rt(i) );
            CASE res IS
              WHEN 'U'          => RETURN FALSE;
              WHEN 'X'          => RETURN FALSE;
              WHEN 'T'          => RETURN TRUE;
              WHEN 'F'          => RETURN FALSE;
              WHEN OTHERS       => null;
            END CASE;
        END LOOP;
        result := TRUE ;
        RETURN result;
    end ;

--
-- Greater Than functions.
--
    CONSTANT gtb_table : stdlogic_boolean_table := (
    --
     ----------------------------------------------------------------------------
    --      |  U       X      0     1      Z      W      L      H      D |   |
    --
     ----------------------------------------------------------------------------
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | U |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | X |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | 0 |
    ( FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE ),    -- | 1 |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | Z |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | W |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | L |
    ( FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE ),    -- | H |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE )   -- | D |
    );

    FUNCTION gt  ( l, r : std_logic ) RETURN BOOLEAN IS
        -- Greater-than for two logic types
        VARIABLE result : BOOLEAN ;
        ATTRIBUTE synthesis_return OF result:VARIABLE IS "GT" ; 
    BEGIN
        result := gtb_table( l, r );
        RETURN result ;
    END ;

    FUNCTION gt  ( l,r : STD_ULOGIC_VECTOR ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt  : STD_ULOGIC_VECTOR ( 1 TO ml );
        VARIABLE rt  : STD_ULOGIC_VECTOR ( 1 TO ml );
        -- Greater-than for two logic unsigned vectors
        VARIABLE result : BOOLEAN ;
        ATTRIBUTE synthesis_return OF result:VARIABLE IS "GT" ; 
    BEGIN
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'range LOOP
            IF NOT eq( lt(i), rt(i) ) THEN
               result := gt( lt(i), rt(i) );
               RETURN result ;
            END IF;
        END LOOP;
        RETURN FALSE;
    END;

--
-- Less Than functions.
--
    CONSTANT ltb_table : stdlogic_boolean_table := (
    --
     ----------------------------------------------------------------------------
    --      |  U       X      0     1      Z      W      L      H      D |   |
    --
     ----------------------------------------------------------------------------
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | U |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | X |
    ( FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE ),    -- | 0 |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | 1 |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | Z |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | W |
    ( FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE ),    -- | L |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | H |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE )   -- | D |
    );

    FUNCTION lt  ( l, r : STD_LOGIC ) RETURN BOOLEAN IS
        -- Less-than for two logic types
        VARIABLE result : BOOLEAN ;
        ATTRIBUTE synthesis_return OF result:VARIABLE IS "LT" ; 
    BEGIN
        result := ltb_table( l, r );
        RETURN result ;
    END;

    FUNCTION lt  ( l,r : STD_ULOGIC_VECTOR ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE ltt  : STD_ULOGIC_VECTOR ( 1 TO ml );
        VARIABLE rtt  : STD_ULOGIC_VECTOR ( 1 TO ml );
        -- Less-than for two Unsigned vectors
        VARIABLE result : BOOLEAN ;
        ATTRIBUTE synthesis_return OF result:VARIABLE IS "LT" ; 
    BEGIN
        ltt := zxt( l, ml );
        rtt := zxt( r, ml );
        FOR i IN ltt'range LOOP
            IF NOT eq( ltt(i), rtt(i) ) THEN
               result := lt( ltt(i), rtt(i) );
               RETURN result ;
            END IF;
        END LOOP;
        RETURN FALSE;
    END;

    --------------------------------------------------------------------
    -- "keep" Retain Last value when floated
    --------------------------------------------------------------------
    procedure keep(signal X: inout STD_LOGIC) is
    begin
		if(X = 'Z') then
			if(X'last_value = '0') then
				X <= 'L';
			elsif(X'last_value = '1') then
				X <= 'H';
			else
				X <= 'Z';
			end if;
		else
			X <= 'Z';
		end if;
    end keep;

	---------------------------------------------------------------------
	-- log base 2 function
	---------------------------------------------------------------------
	function log2 ( A: in integer ) return integer is
		variable B  : integer;
		begin
			B := 1;
			for i in 0 to 31 loop
				if not ( A > B ) then
					return ( i );
					exit;
				end if;
				B := B * 2;
			end loop;
	end log2;



end UTIL;

