with Ada.Text_IO,
     Ada.Characters.Handling,
     Ada.Strings.Fixed;
package body ARM_Input is

    --
    -- Ada reference manual formatter.
    --
    -- This package contains the abstract definition of reading an input file
    -- or other entity, and routines to lex the input entities.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2002  AXE Consultants.
    -- P.O. Box 1512, Madison WI  53701
    -- E-Mail: randy@rrsoftware.com
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
    --  5/16/00 - RLB - Added `' as a open/close pairing.
    --  7/18/02 - RLB - Added Check_One_of_Parameter_Names.
    -- 12/06/04 - RLB - Expanded Check_One_of_Parameter_Names to take up to
    --			five names.

    function Is_Open_Char (Open_Char : in Character) return Boolean is
	-- Return True if character is a parameter opening character
	--    ('<', '[', '{', '(', '`'), and False otherwise.
    begin
	case Open_Char is
	    when '<' | '[' | '{' | '(' | '`' => return True;
	    when others => return False;
	end case;
    end Is_Open_Char;


    function Get_Close_Char (Open_Char : in Character) return Character is
	-- Return the parameter closing character for an opening character.
	-- Raises Not_Valid_Error if Open_Char is not an opening character
	--    ('<', '[', '{', '(', '`').
    begin
	case Open_Char is
	    when '<' => return '>';
	    when '[' => return ']';
	    when '{' => return '}';
	    when '(' => return ')';
	    when '`' => return ''';
	    when others => raise ARM_Input.Not_Valid_Error;
	end case;
    end Get_Close_Char;


    function Get_Open_Char (Close_Char : in Character) return Character is
	-- Return the parameter opening character for an closing character.
	-- Raises Not_Valid_Error if Open_Char is not an closing character
	--    ('>', ']', '}', ')', ''').
    begin
	case Close_Char is
	    when '>' => return '<';
	    when ']' => return '[';
	    when '}' => return '{';
	    when ')' => return '(';
	    when ''' => return '`';
	    when others => raise ARM_Input.Not_Valid_Error;
	end case;
    end Get_Open_Char;


    procedure Get_Name (Input_Object : in out Input_Type'Class;
			Name : out ARM_Input.Command_Name_Type;
		        Null_Name_Allowed : in Boolean := False) is
        -- Get a name from the Input_Object. "Names" are sequences of
        -- alphanumeric characters. If Null_Name_Allowed is False,
        -- an error is produced if no name is found.
        Ch : Character;
        Len : Natural := 0;
    begin
        Name := (others => ' ');
        loop
	    Get_Char (Input_Object, Ch);
	    if Ada.Characters.Handling.Is_Alphanumeric (Ch) then
	        Len := Len + 1;
	        Name(Len) := Ch;
	    else -- End of the name.
	        Replace_Char (Input_Object);
	        if Len = 0 and then (not Null_Name_Allowed) then
		    Ada.Text_IO.Put_Line ("  ** Failed to find command name on line " & Line_String (Input_Object));
	        end if;
	        return;
	    end if;
        end loop;
    end Get_Name;


    procedure Copy_to_String_until_Close_Char
	     (Input_Object : in out Input_Type'Class;
	      Close_Char : in Character;
	      Buffer : out String;
	      Len : out Natural) is
        -- Copy text from Input_Object to Buffer until the matching
        -- Close_Char is found. Len is the number of characters copied.
        -- Use this when we only need a string; use recording when we
        -- need the string *and* we still must process the type.
        -- Note that we watch for matching opening characters, in
        -- case a nested command uses one.
        Ch : Character;
        Start_Ch : constant Character := ARM_Input.Get_Open_Char (Close_Char);
        Start_Ch_Count : Natural := 0;
    begin
        ARM_Input.Get_Char (Input_Object, Ch);
        Len := 0;
        loop
	    if Ch = Start_Ch then
	        -- In case an inner command uses the same
	        -- start/end character.
	        Start_Ch_Count := Start_Ch_Count + 1;
	    elsif Ch = Close_Char then
	        exit when Start_Ch_Count = 0;
	        Start_Ch_Count := Start_Ch_Count - 1;
	    end if;
	    if Len >= Buffer'Length then
	        Ada.Text_IO.Put_Line ("  ** String buffer overflow on line " &
			ARM_Input.Line_String (Input_Object));
	    else
	        Buffer (Buffer'First + Len) := Ch;
                Len := Len + 1;
	    end if;
	    ARM_Input.Get_Char (Input_Object, Ch);
        end loop;
    end Copy_to_String_until_Close_Char;


    procedure Skip_until_Close_Char
	     (Input_Object : in out Input_Type'Class;
	      Close_Char : in Character) is
        -- Skip text from Input_Object until the matching Close_Char is found.
        -- Note that we watch for matching opening characters, in
        -- case a nested command uses one.
        Ch : Character;
        Start_Ch : constant Character := ARM_Input.Get_Open_Char (Close_Char);
        Start_Ch_Count : Natural := 0;
    begin
        ARM_Input.Get_Char (Input_Object, Ch);
        loop
	    if Ch = Start_Ch then
	        -- In case an inner command uses the same
	        -- start/end character.
	        Start_Ch_Count := Start_Ch_Count + 1;
	    elsif Ch = Close_Char then
	        exit when Start_Ch_Count = 0;
	        Start_Ch_Count := Start_Ch_Count - 1;
	    end if;
	    -- Ignore everything until the end character
	    -- turns up.
	    ARM_Input.Get_Char (Input_Object, Ch);
        end loop;
    end Skip_until_Close_Char;


    procedure Check_Parameter_Name (Input_Object : in out Input_Type'Class;
				    Param_Name : in ARM_Input.Command_Name_Type;
				    Is_First : in Boolean;
				    Param_Close_Bracket : out Character) is
        -- Check that the name of a parameter (if any) is Param_Name.
        -- This is the first parameter is Is_First is True; otherwise
        -- it is a later parameter. (For a later parameter, we'll skip
        -- the comma and any whitespace.)
        -- If the parameter has an argument, the opening character will
        -- be read, and the closing character will be returned in
        -- in Param_Close_Bracket. If the parameter wasn't found, an
        -- error message will be produced, and Param_Close_Bracket will
        -- be set to ' '.
        Our_Param_Name : ARM_Input.Command_Name_Type;
        Ch : Character;
    begin
        if not Is_First then
	    -- Skip over the comma and any whitespace:
	    ARM_Input.Get_Char (Input_Object, Ch);
	    if Ch /= ',' then
	        Ada.Text_IO.Put_Line ("  ** Bad parameter separator for " &
			    Ada.Strings.Fixed.Trim (Param_Name, Ada.Strings.Right) &
			    ": " & Ch & " on line " & ARM_Input.Line_String (Input_Object));
	    else
	        ARM_Input.Get_Char (Input_Object, Ch);
	    end if;
	    while (Ch = ' ' or else Ch = Ascii.LF) loop
	        ARM_Input.Get_Char (Input_Object, Ch);
	    end loop;
	    ARM_Input.Replace_Char (Input_Object);
        -- else nothing to clean up.
        end if;
        Arm_Input.Get_Name (Input_Object, Our_Param_Name, Null_Name_Allowed => True);
        if Ada.Characters.Handling.To_Lower (Param_Name) =
	   Ada.Characters.Handling.To_Lower (Our_Param_Name) then
	    ARM_Input.Get_Char (Input_Object, Ch);
	    if Ch /= '=' then
	        Ada.Text_IO.Put_Line ("  ** Bad parameter character for " &
		    Ada.Strings.Fixed.Trim(Our_Param_Name, Ada.Strings.Right) &
		    " parameter: " & Ch & " on line " & ARM_Input.Line_String (Input_Object));
	    end if;
        elsif Ada.Strings.Fixed.Trim (Param_Name, Ada.Strings.Right) = "" then
	    null; -- No parameter name.
        else
	    Ada.Text_IO.Put_Line ("  ** Bad parameter name: " & Ada.Strings.Fixed.Trim (Our_Param_Name, Ada.Strings.Right) &
				  ", " & Ada.Strings.Fixed.Trim (Param_Name, Ada.Strings.Right) & " expected on line " &
				  ARM_Input.Line_String (Input_Object));
        end if;
        -- Now, open the parameter section:
        ARM_Input.Get_Char (Input_Object, Ch);
        if ARM_Input.Is_Open_Char (Ch) then -- Start parameter:
	    Param_Close_Bracket := ARM_Input.Get_Close_Char (Ch);
        elsif Ch = '"' then -- Start quoted parameter:
	    Param_Close_Bracket := '"';
        else -- No parameter. Weird.
	    ARM_Input.Replace_Char (Input_Object);
	    Ada.Text_IO.Put_Line ("  ** Failed to find parameter text for " &
				  Ada.Strings.Fixed.Trim (Our_Param_Name, Ada.Strings.Right) &
				  ", line " & ARM_Input.Line_String (Input_Object));
	    Param_Close_Bracket := ' ';
        end if;
    end Check_Parameter_Name;


    procedure Check_One_of_Parameter_Names (
		Input_Object : in out Input_Type'Class;
		Param_Name_1 : in ARM_Input.Command_Name_Type;
		Param_Name_2 : in ARM_Input.Command_Name_Type;
		Param_Name_3 : in ARM_Input.Command_Name_Type := (others => ' ');
		Param_Name_4 : in ARM_Input.Command_Name_Type := (others => ' ');
		Param_Name_5 : in ARM_Input.Command_Name_Type := (others => ' ');
		Is_First : in Boolean;
		Param_Close_Bracket : out Character;
		Param_Found : out Param_Num) is
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
        Our_Param_Name : ARM_Input.Command_Name_Type;
        Ch : Character;
    begin
        if not Is_First then
	    -- Skip over the comma and any whitespace:
	    ARM_Input.Get_Char (Input_Object, Ch);
	    if Ch /= ',' then
	        Ada.Text_IO.Put_Line ("  ** Bad parameter separator for " &
			    Ada.Strings.Fixed.Trim (Param_Name_1, Ada.Strings.Right) & " or " &
			    Ada.Strings.Fixed.Trim (Param_Name_2, Ada.Strings.Right) &
			    ": " & Ch & " on line " & ARM_Input.Line_String (Input_Object));
	    else
	        ARM_Input.Get_Char (Input_Object, Ch);
	    end if;
	    while (Ch = ' ' or else Ch = Ascii.LF) loop
	        ARM_Input.Get_Char (Input_Object, Ch);
	    end loop;
	    ARM_Input.Replace_Char (Input_Object);
        -- else nothing to clean up.
        end if;
        Arm_Input.Get_Name (Input_Object, Our_Param_Name, Null_Name_Allowed => True);
        if Param_Name_1 /= ARM_Input.Command_Name_Type'(others => ' ') and then
	   Ada.Characters.Handling.To_Lower (Param_Name_1) =
	     Ada.Characters.Handling.To_Lower (Our_Param_Name) then
	    ARM_Input.Get_Char (Input_Object, Ch);
	    if Ch /= '=' then
	        Ada.Text_IO.Put_Line ("  ** Bad parameter character for " &
		    Ada.Strings.Fixed.Trim(Our_Param_Name, Ada.Strings.Right) &
		    " parameter: " & Ch & " on line " & ARM_Input.Line_String (Input_Object));
	    end if;
	    Param_Found := 1;
        elsif Param_Name_2 /= ARM_Input.Command_Name_Type'(others => ' ') and then
	   Ada.Characters.Handling.To_Lower (Param_Name_2) =
	     Ada.Characters.Handling.To_Lower (Our_Param_Name) then
	    ARM_Input.Get_Char (Input_Object, Ch);
	    if Ch /= '=' then
	        Ada.Text_IO.Put_Line ("  ** Bad parameter character for " &
		    Ada.Strings.Fixed.Trim(Our_Param_Name, Ada.Strings.Right) &
		    " parameter: " & Ch & " on line " & ARM_Input.Line_String (Input_Object));
	    end if;
	    Param_Found := 2;
        elsif Param_Name_3 /= ARM_Input.Command_Name_Type'(others => ' ') and then
	   Ada.Characters.Handling.To_Lower (Param_Name_3) =
	     Ada.Characters.Handling.To_Lower (Our_Param_Name) then
	    ARM_Input.Get_Char (Input_Object, Ch);
	    if Ch /= '=' then
	        Ada.Text_IO.Put_Line ("  ** Bad parameter character for " &
		    Ada.Strings.Fixed.Trim(Our_Param_Name, Ada.Strings.Right) &
		    " parameter: " & Ch & " on line " & ARM_Input.Line_String (Input_Object));
	    end if;
	    Param_Found := 3;
        elsif Param_Name_4 /= ARM_Input.Command_Name_Type'(others => ' ') and then
	   Ada.Characters.Handling.To_Lower (Param_Name_4) =
	     Ada.Characters.Handling.To_Lower (Our_Param_Name) then
	    ARM_Input.Get_Char (Input_Object, Ch);
	    if Ch /= '=' then
	        Ada.Text_IO.Put_Line ("  ** Bad parameter character for " &
		    Ada.Strings.Fixed.Trim(Our_Param_Name, Ada.Strings.Right) &
		    " parameter: " & Ch & " on line " & ARM_Input.Line_String (Input_Object));
	    end if;
	    Param_Found := 4;
        elsif Param_Name_5 /= ARM_Input.Command_Name_Type'(others => ' ') and then
	   Ada.Characters.Handling.To_Lower (Param_Name_5) =
	     Ada.Characters.Handling.To_Lower (Our_Param_Name) then
	    ARM_Input.Get_Char (Input_Object, Ch);
	    if Ch /= '=' then
	        Ada.Text_IO.Put_Line ("  ** Bad parameter character for " &
		    Ada.Strings.Fixed.Trim(Our_Param_Name, Ada.Strings.Right) &
		    " parameter: " & Ch & " on line " & ARM_Input.Line_String (Input_Object));
	    end if;
	    Param_Found := 5;
        else
	    Ada.Text_IO.Put_Line ("  ** Bad parameter name: " & Ada.Strings.Fixed.Trim (Our_Param_Name, Ada.Strings.Right) &
				  ", " & Ada.Strings.Fixed.Trim (Param_Name_1, Ada.Strings.Right) & " or " &
				  Ada.Strings.Fixed.Trim (Param_Name_2, Ada.Strings.Right) & " expected on line " &
				  ARM_Input.Line_String (Input_Object));
	    Param_Found := 0;
        end if;
        -- Now, open the parameter section:
        ARM_Input.Get_Char (Input_Object, Ch);
        if ARM_Input.Is_Open_Char (Ch) then -- Start parameter:
	    Param_Close_Bracket := ARM_Input.Get_Close_Char (Ch);
        elsif Ch = '"' then -- Start quoted parameter:
	    Param_Close_Bracket := '"';
        else -- No parameter. Weird.
	    ARM_Input.Replace_Char (Input_Object);
	    Ada.Text_IO.Put_Line ("  ** Failed to find parameter text for " &
				  Ada.Strings.Fixed.Trim (Our_Param_Name, Ada.Strings.Right) &
				  ", line " & ARM_Input.Line_String (Input_Object));
	    Param_Close_Bracket := ' ';
        end if;
    end Check_One_of_Parameter_Names;


end ARM_Input;
