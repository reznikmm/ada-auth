with ARM_Input,
     Ada.Text_IO;
package body ARM_File is

    --
    -- Ada reference manual formatter.
    --
    -- This package contains the definition of reading an input file.
    --
    -- ---------------------------------------
    -- Copyright 2000, AXE Consultants.
    -- P.O. Box 1512, Madison WI  53704
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

    procedure Open (Input_Object : in out File_Input_Type;
		    File_Name : in String) is
	-- Open an input object for a file.
	-- This may propagate file exceptions.
    begin
        Ada.Text_IO.Open (Input_Object.Fyle, Ada.Text_IO.In_File, File_Name);
        Input_Object.Line_Counter := 0;
        Input_Object.Buffer_Last := 0;
        Input_Object.Buffer_Index := 0;
        Input_Object.Is_Valid := True;
	if File_Name'Length > Input_Object.Name'Length then
	    Input_Object.Name := File_Name(File_Name'First .. File_Name'First + Input_Object.Name'Length - 1);
	    Input_Object.Name_Len := Input_Object.Name'Length;
	else
	    Input_Object.Name(1..File_Name'Length) := File_Name;
	    Input_Object.Name_Len := File_Name'Length;
	end if;
    end Open;


    procedure Close (Input_Object : in out File_Input_Type) is
	-- Close the input object (entity).
	-- May propagate exceptions from the underlying implementation
	-- (that is, I/O exceptions).
    begin
	if not Input_Object.Is_Valid then
	    raise ARM_Input.Not_Valid_Error;
	end if;
	Input_Object.Is_Valid := False;
        Ada.Text_IO.Close (Input_Object.Fyle);
    end Close;


    procedure Get_Char (Input_Object : in out File_Input_Type;
			Char : out Character) is
        -- We represent end of line by Ascii.LF.
        -- Raises: End_Error when the end of file is reached.
	--	   Not_Valid_Error if Input_Object is not valid (open).
    begin
	if not Input_Object.Is_Valid then
	    raise ARM_Input.Not_Valid_Error;
	end if;
        if Input_Object.Buffer_Index >= Input_Object.Buffer_Last then
	    begin
		Ada.Text_IO.Get_Line (Input_Object.Fyle,
				      Input_Object.Buffer,
				      Input_Object.Buffer_Last);
	            -- Raises End_Error when the end of the file is reached.
	    exception
		when Ada.Text_IO.End_Error =>
		    -- Set so we can do a Replace_Char on this.
		    Input_Object.Buffer_Index := 1;
		    Input_Object.Buffer_Last := 1;
		    Input_Object.Buffer(1) := Ascii.SUB; -- File end marker.
		    Char := Ascii.SUB;
		    return;
	    end;
	    if Input_Object.Buffer_Last < Input_Object.Buffer'Last then
	        Input_Object.Buffer_Last := Input_Object.Buffer_Last + 1;
	        Input_Object.Buffer(Input_Object.Buffer_Last) := Ascii.LF; -- Line end marker.
	    -- else broken line, no end needed.
	    end if;
	    --Ada.Text_IO.Put(Natural'Image(Input_Object.Line_Counter) & ":");
	    --Ada.Text_IO.Put_Line ("&& " & Input_Object.Buffer(1..Input_Object.Buffer_Last));
	    Input_Object.Buffer_Index := 0;
	    Input_Object.Line_Counter := Input_Object.Line_Counter + 1;
        end if;
        Input_Object.Buffer_Index := Input_Object.Buffer_Index + 1;
        if Input_Object.Recording then
	    Input_Object.Recording_Len := Input_Object.Recording_Len + 1;
	    Input_Object.Recording_Buffer(Input_Object.Recording_Len) :=
		Input_Object.Buffer(Input_Object.Buffer_Index);
        end if;
        Char := Input_Object.Buffer(Input_Object.Buffer_Index);
    end Get_Char;


    procedure Replace_Char (Input_Object : in out File_Input_Type) is
	-- Replaces the last character read (with Get_Char); the next call
	-- to Get_Char will return it.
        -- Raises: Not_Valid_Error if Input_Object is not valid (open).
    begin
	if not Input_Object.Is_Valid then
	    raise ARM_Input.Not_Valid_Error;
	end if;
        if Input_Object.Buffer_Index = 0 then
	    raise Program_Error; -- Called twice or before any calls to Get_Char.
        end if;
        Input_Object.Buffer_Index := Input_Object.Buffer_Index - 1;
        if Input_Object.Recording then
	    Input_Object.Recording_Len := Input_Object.Recording_Len - 1;
        end if;
    end Replace_Char;


    function Line_String (Input_Object : in File_Input_Type) return String is
        -- Returns a string representing the line number and entity.
	-- Usually used in error messages.
        -- Raises: Not_Valid_Error if Input_Object is not valid (open).
    begin
	if not Input_Object.Is_Valid then
	    raise ARM_Input.Not_Valid_Error;
	end if;
        return Natural'Image(Input_Object.Line_Counter) & " - " &
		Input_Object.Name(1..Input_Object.Name_Len);
    end Line_String;


    procedure Start_Recording (Input_Object : in out File_Input_Type) is
        -- Start recording all characters read into a local buffer.
        -- Use this when text needs to be formatted into the output
        -- file *and* be saved for future use.
        -- Raises: Not_Valid_Error if Input_Object is not valid (open).
    begin
	if not Input_Object.Is_Valid then
	    raise ARM_Input.Not_Valid_Error;
	end if;
        Input_Object.Recording := True;
        Input_Object.Recording_Len := 0;
    end Start_Recording;


    procedure Stop_Recording_and_Read_Result
        (Input_Object : in out File_Input_Type; Result : out String;
	 Len : out Natural) is
        -- Stop recording characters read. Put the result into Result,
        -- and the number of characters written into Len.
        -- Raises: Not_Valid_Error if Input_Object is not valid (open).
    begin
	if not Input_Object.Is_Valid then
	    raise ARM_Input.Not_Valid_Error;
	end if;
        if Input_Object.Recording_Len > Result'Length then
	    Ada.Text_IO.Put_Line ("  ** Too many characters recorded on line " & Line_String (Input_Object));
	    Len := 0;
        else
	    Result (Result'First .. Result'First + Input_Object.Recording_Len - 1) :=
		 Input_Object.Recording_Buffer (1 .. Input_Object.Recording_Len);
	    Len := Input_Object.Recording_Len;
        end if;
        Input_Object.Recording := False;
    end Stop_Recording_and_Read_Result;

end ARM_File;
