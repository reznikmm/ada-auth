with ARM_Input;
package ARM_String is

    --
    -- Ada reference manual formatter.
    --
    -- This package contains the definition of reading input from a string.
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
    --  5/15/00 - RLB - Created package.

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
