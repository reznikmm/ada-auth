with ARM_Input,
     Ada.Text_IO;
package body ARM_String is

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

    procedure Open (Input_Object : in out String_Input_Type;
		    Text : in String;
		    Text_Name : in String) is
	-- Open an input object for a file.
    begin
        Input_Object.Line_Counter := 0;
	if Text'Length > Input_Object.Buffer'Length then
	    Ada.Text_IO.Put_Line ("  ** Text too long for string input object for " & Text_Name);
	else
	    Input_Object.Buffer(1..Text'Length) := Text;
	    Input_Object.Buffer_Len := Text'Length;
	end if;
        Input_Object.Buffer_Index := 0;
        Input_Object.Is_Valid := True;
	if Text_Name'Length > Input_Object.Name'Length then
	    Input_Object.Name := Text_Name(Text_Name'First .. Text_Name'First + Input_Object.Name'Length - 1);
	    Input_Object.Name_Len := Input_Object.Name'Length;
	else
	    Input_Object.Name(1..Text_Name'Length) := Text_Name;
	    Input_Object.Name_Len := Text_Name'Length;
	end if;
    end Open;


    procedure Close (Input_Object : in out String_Input_Type) is
	-- Close the input object (entity).
	-- May propagate exceptions from the underlying implementation
	-- (that is, I/O exceptions).
    begin
	if not Input_Object.Is_Valid then
	    raise ARM_Input.Not_Valid_Error;
	end if;
	Input_Object.Is_Valid := False;
    end Close;


    procedure Get_Char (Input_Object : in out String_Input_Type;
			Char : out Character) is
        -- We represent end of line by Ascii.LF.
        -- Raises: End_Error when the end of file is reached.
	--	   Not_Valid_Error if Input_Object is not valid (open).
    begin
	if not Input_Object.Is_Valid then
	    raise ARM_Input.Not_Valid_Error;
	end if;
        Input_Object.Buffer_Index := Input_Object.Buffer_Index + 1;
        if Input_Object.Buffer_Index > Input_Object.Buffer_Len then
--Ada.Text_IO.Put_Line ("Get_Char EOS, index=" & Natural'Image(Input_Object.Buffer_Index));
	    Char := ASCII.Sub;
	    return;
        end if;
        Char := Input_Object.Buffer(Input_Object.Buffer_Index);
	if Char = Ascii.LF then
	    Input_Object.Line_Counter := Input_Object.Line_Counter + 1;
--Ada.Text_IO.Put_Line ("Get_Char EOL, index=" & Natural'Image(Input_Object.Buffer_Index));
--else Ada.Text_IO.Put_Line ("Get_Char " & Char & ", index=" & Natural'Image(Input_Object.Buffer_Index));
	end if;
    end Get_Char;


    procedure Replace_Char (Input_Object : in out String_Input_Type) is
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
        if Input_Object.Buffer_Index <= Input_Object.Buffer_Len and then
	   Input_Object.Buffer(Input_Object.Buffer_Index) = Ascii.LF then
	    Input_Object.Line_Counter := Input_Object.Line_Counter - 1;
	end if;
--Ada.Text_IO.Put_Line ("Replace_Char");
        Input_Object.Buffer_Index := Input_Object.Buffer_Index - 1;
    end Replace_Char;


    function Line_String (Input_Object : in String_Input_Type) return String is
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


    procedure Start_Recording (Input_Object : in out String_Input_Type) is
        -- Start recording all characters read into a local buffer.
        -- Use this when text needs to be formatted into the output
        -- file *and* be saved for future use.
        -- Raises: Not_Valid_Error if Input_Object is not valid (open).
    begin
	if not Input_Object.Is_Valid then
	    raise ARM_Input.Not_Valid_Error;
	end if;
        Input_Object.Recording := True;
        Input_Object.Recording_Start := Input_Object.Buffer_Index + 1;
    end Start_Recording;


    procedure Stop_Recording_and_Read_Result
        (Input_Object : in out String_Input_Type; Result : out String;
	 Len : out Natural) is
        -- Stop recording characters read. Put the result into Result,
        -- and the number of characters written into Len.
        -- Raises: Not_Valid_Error if Input_Object is not valid (open).
    begin
	if not Input_Object.Is_Valid then
	    raise ARM_Input.Not_Valid_Error;
	end if;
        if Input_Object.Buffer_Index - Input_Object.Recording_Start + 1 >
	    Result'Length then
	    Ada.Text_IO.Put_Line ("  ** Too many characters recorded on line " & Line_String (Input_Object));
	    Len := 0;
        else
	    Len := (Input_Object.Buffer_Index - Input_Object.Recording_Start) + 1;
	    Result (Result'First .. Result'First + Len - 1) :=
		 Input_Object.Buffer (Input_Object.Recording_Start .. Input_Object.Buffer_Index);
        end if;
        Input_Object.Recording := False;
    end Stop_Recording_and_Read_Result;

end ARM_String;
