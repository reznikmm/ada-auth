package ARM_Input is

    --
    -- Ada reference manual formatter.
    --
    -- This package contains the abstract definition of reading an input file
    -- or other entity, and routines to lex the input entities.
    --
    -- ---------------------------------------
    -- Copyright 2000, AXE Consultants.
    -- P.O. Box 1512, Madison WI  53701
    -- E-Mail: rbrukardt@bix.com
    --
    -- AXE Consultants grants to all users the right to use/modify this
    -- formatting tool for non-commercial purposes. (ISO/IEC JTC 1 SC 22 WG 9
    -- activities are explicitly included as "non-commercial purposes".)
    -- Commercial uses of this software and its source code, including but not
    -- limited to documents for sale and sales of modified versions of this
    -- tool, are prohibited without the prior written permission of
    -- AXE Consultants. All rights not explicitly granted above are reserved
    -- by AXE Consultants.
    --
    -- You use this tool and/or its source code on the condition that you indemnify and hold harmless
    -- AXE Consultants, its agents, and employees, from any and all liability
    -- or damages to yourself or your hardware or software, or third parties,
    -- including attorneys' fees, court costs, and other related costs and
    -- expenses, arising out of your use of this tool and/or source code irrespective of the
    -- cause of said liability.
    --
    -- AXE CONSULTANTS MAKES THIS TOOL AND SOURCE CODE AVAILABLE ON AN "AS IS"
    -- BASIS AND MAKES NO WARRANTY, EXPRESS OR IMPLIED, AS TO THE ACCURACY,
    -- CAPABILITY, EFFICIENCY, MERCHANTABILITY, OR FUNCTIONING OF THIS TOOL.
    -- IN NO EVENT WILL AXE CONSULTANTS BE LIABLE FOR ANY GENERAL,
    -- CONSEQUENTIAL, INDIRECT, INCIDENTAL, EXEMPLARY, OR SPECIAL DAMAGES,
    -- EVEN IF AXE CONSULTANTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
    -- DAMAGES.
    -- ---------------------------------------
    --
    -- Edit History:
    --
    --  5/15/00 - RLB - Created base package.

    type Input_Type is abstract tagged limited null record;

    MAX_RECORDING_SIZE : constant := 4000;

    Not_Valid_Error : exception; -- The Input_Object is not valid.

    -- procedure Open (Input_Object : in out Input_Type;
    --		       -- Other parameters) is abstract;
	-- Open an input object for an entity.
	-- Each concrete type has its own Open routine, with possibly
	-- different parameters.

    procedure Close (Input_Object : in out Input_Type) is abstract;
	-- Close the input object (entity).
	-- May propagate exceptions from the underlying implementation
	-- (that is, I/O exceptions).

    procedure Get_Char (Input_Object : in out Input_Type;
			Char : out Character) is abstract;
        -- We represent end of line by Ascii.LF, and end of file by
	-- Ascii.SUB.
        -- Raises: Not_Valid_Error if Input_Object is not valid (open).

    procedure Replace_Char (Input_Object : in out Input_Type) is abstract;
	-- Replaces the last character read (with Get_Char); the next call
	-- to Get_Char will return it.
        -- Raises: Not_Valid_Error if Input_Object is not valid (open).

    function Line_String (Input_Object : in Input_Type) return String is abstract;
        -- Returns a string representing the line number and entity.
	-- Usually used in error messages.
        -- Raises: Not_Valid_Error if Input_Object is not valid (open).

    procedure Start_Recording (Input_Object : in out Input_Type) is abstract;
        -- Start recording all characters read into a local buffer.
        -- Use this when text needs to be formatted into the output
        -- file *and* be saved for future use.
        -- Raises: Not_Valid_Error if Input_Object is not valid (open).

    procedure Stop_Recording_and_Read_Result
        (Input_Object : in out Input_Type; Result : out String;
	 Len : out Natural) is abstract;
        -- Stop recording characters read. Put the result into Result,
        -- and the number of characters written into Len.
        -- Raises: Not_Valid_Error if Input_Object is not valid (open).

    -- Lexical routines:

    subtype Command_Name_Type is String (1 .. 40);

    function Is_Open_Char (Open_Char : in Character) return Boolean;
	-- Return True if character is a parameter opening character
	--    ('<', '[', '{', '('), and False otherwise.

    function Get_Close_Char (Open_Char : in Character) return Character;
	-- Return the parameter closing character for an opening character.
	-- Raises Not_Valid_Error if Open_Char is not an opening character
	--    ('<', '[', '{', '(').

    function Get_Open_Char (Close_Char : in Character) return Character;
	-- Return the parameter opening character for an closing character.
	-- Raises Not_Valid_Error if Open_Char is not an closing character
	--    ('>', ']', '}', ')').


    procedure Get_Name (Input_Object : in out Input_Type'Class;
			Name : out ARM_Input.Command_Name_Type;
		        Null_Name_Allowed : in Boolean := False);
        -- Get a name from the Input_Object. "Names" are sequences of
        -- alphanumeric characters. If Null_Name_Allowed is False,
        -- an error is produced if no name is found.

    procedure Copy_to_String_until_Close_Char
	     (Input_Object : in out Input_Type'Class;
	      Close_Char : in Character;
	      Buffer : out String;
	      Len : out Natural);
        -- Copy text from Input_Object to Buffer until the matching
        -- Close_Char is found. Len is the number of characters copied.
        -- Use this when we only need a string; use recording when we
        -- need the string *and* we still must process the type.

    procedure Skip_until_Close_Char
	     (Input_Object : in out Input_Type'Class;
	      Close_Char : in Character);
        -- Skip text from Input_Object until the matching Close_Char is found.

    procedure Check_Parameter_Name (Input_Object : in out Input_Type'Class;
				    Param_Name : in ARM_Input.Command_Name_Type;
				    Is_First : in Boolean;
				    Param_Close_Bracket : out Character);
        -- Check that the name of a parameter (if any) is Param_Name.
        -- This is the first parameter is Is_First is True; otherwise
        -- it is a later parameter. (For a later parameter, we'll skip
        -- the comma and any whitespace.)
        -- If the parameter has an argument, the opening character will
        -- be read, and the closing character will be returned in
        -- in Param_Close_Bracket. If the parameter wasn't found, an
        -- error message will be produced, and Param_Close_Bracket will
        -- be set to ' '.

end ARM_Input;
