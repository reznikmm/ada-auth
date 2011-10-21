package ARM_Input is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package contains the abstract definition of reading an input file
    -- or other entity, and routines to lex the input entities.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2002, 2004, 2011
    --   AXE Consultants. All rights reserved.
    -- P.O. Box 1512, Madison WI  53701
    -- E-Mail: randy@rrsoftware.com
    --
    -- ARM_Form is free software: you can redistribute it and/or modify
    -- it under the terms of the GNU General Public License version 3
    -- as published by the Free Software Foundation.
    --
    -- AXE CONSULTANTS MAKES THIS TOOL AND SOURCE CODE AVAILABLE ON AN "AS IS"
    -- BASIS AND MAKES NO WARRANTY, EXPRESS OR IMPLIED, AS TO THE ACCURACY,
    -- CAPABILITY, EFFICIENCY, MERCHANTABILITY, OR FUNCTIONING OF THIS TOOL.
    -- IN NO EVENT WILL AXE CONSULTANTS BE LIABLE FOR ANY GENERAL,
    -- CONSEQUENTIAL, INDIRECT, INCIDENTAL, EXEMPLARY, OR SPECIAL DAMAGES,
    -- EVEN IF AXE CONSULTANTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
    -- DAMAGES.
    --
    -- A copy of the GNU General Public License is available in the file
    -- gpl-3-0.txt in the standard distribution of the ARM_Form tool.
    -- Otherwise, see <http://www.gnu.org/licenses/>.
    --
    -- If the GPLv3 license is not satisfactory for your needs, a commercial
    -- use license is available for this tool. Contact Randy at AXE Consultants
    -- for more information.
    --
    -- ---------------------------------------
    --
    -- Edit History:
    --
    --  5/15/00 - RLB - Created base package.
    --  7/18/02 - RLB - Added Check_One_of_Parameter_Names.
    -- 12/06/04 - RLB - Expanded Check_One_of_Parameter_Names to take up to
    --			five names.
    -- 10/18/11 - RLB - Changed to GPLv3 license.

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

    subtype Param_Num is Natural range 0 .. 5;
    procedure Check_One_of_Parameter_Names (
		Input_Object : in out Input_Type'Class;
		Param_Name_1 : in ARM_Input.Command_Name_Type;
		Param_Name_2 : in ARM_Input.Command_Name_Type;
		Param_Name_3 : in ARM_Input.Command_Name_Type := (others => ' ');
		Param_Name_4 : in ARM_Input.Command_Name_Type := (others => ' ');
		Param_Name_5 : in ARM_Input.Command_Name_Type := (others => ' ');
		Is_First : in Boolean;
		Param_Close_Bracket : out Character;
		Param_Found : out Param_Num);
        -- Check that the name of a parameter (if any) is one of the given
	-- names. If the parameter is set to all blanks, it is not used.
	-- This is the first parameter is Is_First is True;
	-- otherwise it is a later parameter. (For a later parameter, we'll
	-- skip the comma and any whitespace.)
	-- Param_Found will be set to the number of the parameter that was
	-- found.
        -- If the parameter has an argument, the opening character will
        -- be read, and the closing character will be returned in
        -- in Param_Close_Bracket. If the parameter wasn't found, an
        -- error message will be produced, Param_Close_Bracket will
        -- be set to ' ', and Param_Found will be set to 0.

end ARM_Input;
