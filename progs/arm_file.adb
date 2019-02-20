with ARM_Input,
     Ada.Text_IO;
package body ARM_File is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package contains the definition of reading an input file.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2011, 2019
    --   AXE Consultants. All rights reserved.
    -- P.O. Box 1512, Madison WI  53704
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
    --  2/15/19 - RLB - Improved error handling of recording buffer overflow.
    --  2/19/19 - RLB - Added replacement of previous line end.

    procedure Open (Input_Object : in out File_Input_Type;
		    File_Name : in String) is
	-- Open an input object for a file.
	-- This may propagate file exceptions.
    begin
        Ada.Text_IO.Open (Input_Object.Fyle, Ada.Text_IO.In_File, File_Name);
        Input_Object.Line_Counter := 0;
        Input_Object.Buffer_Last := 0;
        Input_Object.Buffer_Index := 0;
        Input_Object.Extra_LF := False;
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
	    raise ARM_Input.Not_Valid_Error with "No file open";
	end if;
	if Input_Object.Extra_LF then
	    -- A special put-back of the previous line. We don't adjust
	    -- the line counter for this character, and it is already
	    -- in the recording buffer if necessary.
            Char := Ascii.LF;
	    Input_Object.Extra_LF := False;
	    return;
        elsif Input_Object.Buffer_Index >= Input_Object.Buffer_Last then
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
	    if Input_Object.Recording_Len > Input_Object.Recording_Buffer'Last then
	        Ada.Text_IO.Put_Line ("  ** Too many characters recorded on line " & Line_String (Input_Object));
	        Ada.Text_IO.Put_Line ("     Recording started on line" & Natural'Image(Input_Object.Recording_Start_Line));
		Input_Object.Recording_Len := Input_Object.Recording_Buffer'Last;
		Input_Object.Recording := False;
	    else
	        Input_Object.Recording_Buffer(Input_Object.Recording_Len) :=
		    Input_Object.Buffer(Input_Object.Buffer_Index);
	    end if;
        end if;
        Char := Input_Object.Buffer(Input_Object.Buffer_Index);
    end Get_Char;


    procedure Replace_Char (Input_Object : in out File_Input_Type) is
	-- Replaces the last character read (with Get_Char); the next call
	-- to Get_Char will return it.
        -- Raises: Not_Valid_Error if Input_Object is not valid (open).
    begin
	if not Input_Object.Is_Valid then
	    raise ARM_Input.Not_Valid_Error with "No file open";
	end if;
        if Input_Object.Buffer_Index = 0 then
	    if Input_Object.Extra_LF then -- Extra put back already done.
	        raise Program_Error with "Replace_Char called too many times";
	    else
		Input_Object.Extra_LF := True;
		-- We don't adjust the buffer or the recording in this case.
		return;
	    end if;
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
        Input_Object.Recording_Start_Line := Input_Object.Line_Counter;
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
	    Ada.Text_IO.Put_Line ("     Recording started on line" & Natural'Image(Input_Object.Recording_Start_Line));
	    Len := 0;
        else
	    Result (Result'First .. Result'First + Input_Object.Recording_Len - 1) :=
		 Input_Object.Recording_Buffer (1 .. Input_Object.Recording_Len);
	    Len := Input_Object.Recording_Len;
        end if;
        Input_Object.Recording := False;
    end Stop_Recording_and_Read_Result;

end ARM_File;
