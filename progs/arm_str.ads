with ARM_Input;
package ARM_String is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package contains the definition of reading input from a string.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2011
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
    --  5/15/00 - RLB - Created package.
    -- 10/18/11 - RLB - Changed to GPLv3 license.

    type String_Input_Type is new ARM_Input.Input_Type with private;

    procedure Open (Input_Object : in out String_Input_Type;
		    Text : in String;
		    Text_Name : in String);
	-- Open an input object for a string (Text), with a name of Text_Name.
	-- (The name is used for error messages).

    procedure Close (Input_Object : in out String_Input_Type);
	-- Close the input object (entity).
	-- May propagate exceptions from the underlying implementation
	-- (that is, I/O exceptions).

    procedure Get_Char (Input_Object : in out String_Input_Type;
			Char : out Character);
        -- We represent end of line by Ascii.LF.
        -- Raises: End_Error when the end of file is reached.
	--	   Not_Valid_Error if Input_Object is not valid (open).

    procedure Replace_Char (Input_Object : in out String_Input_Type);
	-- Replaces the last character read (with Get_Char); the next call
	-- to Get_Char will return it.
        -- Raises: Not_Valid_Error if Input_Object is not valid (open).

    function Line_String (Input_Object : in String_Input_Type) return String;
        -- Returns a string representing the line number and entity.
	-- Usually used in error messages.
        -- Raises: Not_Valid_Error if Input_Object is not valid (open).

    procedure Start_Recording (Input_Object : in out String_Input_Type);
        -- Start recording all characters read into a local buffer.
        -- Use this when text needs to be formatted into the output
        -- file *and* be saved for future use.
        -- Raises: Not_Valid_Error if Input_Object is not valid (open).

    procedure Stop_Recording_and_Read_Result
        (Input_Object : in out String_Input_Type; Result : out String;
	 Len : out Natural);
        -- Stop recording characters read. Put the result into Result,
        -- and the number of characters written into Len.
        -- Raises: Not_Valid_Error if Input_Object is not valid (open).

private
    type String_Input_Type is new ARM_Input.Input_Type with record
	Is_Valid : Boolean := False;
	Line_Counter : Natural := 0;
	Buffer : String(1..4000);
	Buffer_Len : Natural := 0;
	Buffer_Index : Natural := 0; -- Last character read from buffer.
	-- For recording:
	Recording : Boolean := False;
	Recording_Start : Natural := 0; -- First character of recorded section.
	-- Name:
	Name : String(1..120);
	Name_Len : Natural;
    end record;
end ARM_String;
