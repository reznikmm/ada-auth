with Ada.Characters.Handling;
package body ARM_Contents is

    --
    -- Ada reference manual formatter.
    --
    -- This package contains the routines to manage section/clause/subclause
    -- references.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2004  AXE Consultants.
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
    --  4/19/00 - RLB - Created base package.
    --  4/26/00 - RLB - Added Previous_Clause and Next_Clause.
    --  5/15/00 - RLB - Added rules about unnumbered sections.
    --  5/22/00 - RLB - Added Unnumbered_Section level.
    --  8/22/00 - RLB - Added Old_Title handling.
    --  9/ 9/04 - RLB - Removed unused with.

    type Title_Record is record
	Title : Title_Type; -- Title in original format.
	Search_Title : Title_Type; -- Title in all lower case.
	Level : Level_Type;
	Section_Number : Section_Number_Type;
	Clause_Number : Natural;
	Subclause_Number : Natural;
    end record;

    Title_List : array (1 .. 500) of Title_Record;
    Last_Title : Natural;

    Old_Title_List : array (1 .. 50) of Title_Record;
    Last_Old_Title : Natural;

    procedure Initialize is
	-- Initialize this package; make sure the contents are empty.
    begin
	Last_Title := 0;
    end Initialize;


    procedure Add (Title : in Title_Type;
		   Level : in Level_Type;
		   Section_Number : in Section_Number_Type;
		   Clause_Number : in Natural := 0;
		   Subclause_Number : in Natural := 0) is
	-- Add a section or clause to the contents. It has the specified
	-- characteristics.
    begin
	if Level /= Subclause and then Subclause_Number /= 0 then
	    raise Program_Error;
	end if;
	if (Level /= Subclause and then Level /= Clause and then
	    Level /= Unnumbered_Section) and then
	   Clause_Number /= 0 then
	    raise Program_Error;
	end if;
	Last_Title := Last_Title + 1;
	Title_List (Last_Title) :=
	    (Title => Title,
	     Search_Title => Ada.Characters.Handling.To_Lower (Title),
	     Level => Level,
	     Section_Number => Section_Number,
	     Clause_Number => Clause_Number,
	     Subclause_Number => Subclause_Number);
    end Add;


    procedure Add_Old (Old_Title : in Title_Type;
		       Level : in Level_Type;
		       Section_Number : in Section_Number_Type;
		       Clause_Number : in Natural := 0;
		       Subclause_Number : in Natural := 0) is
	-- Add an old title for a section or clause to the contents. It has
	-- the specified characteristics.
    begin
	if Level /= Subclause and then Subclause_Number /= 0 then
	    raise Program_Error;
	end if;
	if (Level /= Subclause and then Level /= Clause and then
	    Level /= Unnumbered_Section) and then
	   Clause_Number /= 0 then
	    raise Program_Error;
	end if;
	Last_Old_Title := Last_Old_Title + 1;
	Old_Title_List (Last_Old_Title) :=
	    (Title => Old_Title,
	     Search_Title => Ada.Characters.Handling.To_Lower (Old_Title),
	     Level => Level,
	     Section_Number => Section_Number,
	     Clause_Number => Clause_Number,
	     Subclause_Number => Subclause_Number);
    end Add_Old;


    function Make_Clause_Number (Level : in Level_Type;
		   Section_Number : in Section_Number_Type;
		   Clause_Number : in Natural := 0;
		   Subclause_Number : in Natural := 0) return String is
	-- Returns a properly formatted Section or clause number reference.
    begin
	case Level is
	    when Normative_Annex | Informative_Annex =>
		if Clause_Number /= 0 or else Subclause_Number /= 0 or else
		   Section_Number <= 20 then
		    raise Program_Error; -- Illegal numbers.
		end if;
		return "Annex " & Character'Val (Character'Pos('A') + (Section_Number - 21));
	    when Section =>
		if Clause_Number /= 0 or else Subclause_Number /= 0 or else
		   Section_Number > 20 then
		    raise Program_Error; -- Illegal numbers.
		end if;
		if Section_Number < 10 then
		    return Character'Val (Character'Pos('0') + Section_Number) & "";
		elsif Section_Number < 20 then
		    return "1" & Character'Val (Character'Pos('0') + Section_Number - 10);
		else
		    return "2" & Character'Val (Character'Pos('0') + Section_Number - 20);
		end if;
	    when Unnumbered_Section =>
		if Clause_Number = 0 or else Subclause_Number /= 0 or else
		   Section_Number /= 0 then
		    raise Program_Error; -- Illegal numbers.
		end if;
	        if Clause_Number < 10 then
		    return "0." & Character'Val (Character'Pos('0') + Clause_Number);
	        elsif Clause_Number < 20 then
		    return "0.1" & Character'Val (Character'Pos('0') + Clause_Number - 10);
	        else
		    return "0.2" & Character'Val (Character'Pos('0') + Clause_Number - 20);
	        end if;
	    when Clause =>
		if Subclause_Number /= 0 then
		    raise Program_Error; -- Illegal number.
		end if;
		if Section_Number < 10 then
		    if Clause_Number < 10 then
		        return Character'Val (Character'Pos('0') + Section_Number) &
		            "." & Character'Val (Character'Pos('0') + Clause_Number);
		    elsif Clause_Number < 20 then
		        return Character'Val (Character'Pos('0') + Section_Number) &
		            ".1" & Character'Val (Character'Pos('0') + Clause_Number - 10);
		    else
		        return Character'Val (Character'Pos('0') + Section_Number) &
		            ".2" & Character'Val (Character'Pos('0') + Clause_Number - 20);
		    end if;
		elsif Section_Number < 20 then
		    if Clause_Number < 10 then
		        return "1" & Character'Val (Character'Pos('0') + Section_Number - 10) &
		            "." & Character'Val (Character'Pos('0') + Clause_Number);
		    elsif Clause_Number < 20 then
		        return "1" & Character'Val (Character'Pos('0') + Section_Number - 10) &
		            ".1" & Character'Val (Character'Pos('0') + Clause_Number - 10);
		    else
		        return "1" & Character'Val (Character'Pos('0') + Section_Number - 10) &
		            ".2" & Character'Val (Character'Pos('0') + Clause_Number - 20);
		    end if;
		elsif Section_Number = 20 then
		    if Clause_Number < 10 then
		        return "20." & Character'Val (Character'Pos('0') + Clause_Number);
		    elsif Clause_Number < 20 then
		        return "20.1" & Character'Val (Character'Pos('0') + Clause_Number - 10);
		    else
		        return "20.2" & Character'Val (Character'Pos('0') + Clause_Number - 20);
		    end if;
		else
		    if Clause_Number < 10 then
			return Character'Val (Character'Pos('A') + (Section_Number - 21)) &
		            "." & Character'Val (Character'Pos('0') + Clause_Number);
		    elsif Clause_Number < 20 then
			return Character'Val (Character'Pos('A') + (Section_Number - 21)) &
		            ".1" & Character'Val (Character'Pos('0') + Clause_Number - 10);
		    else
			return Character'Val (Character'Pos('A') + (Section_Number - 21)) &
		            ".2" & Character'Val (Character'Pos('0') + Clause_Number - 20);
		    end if;
		end if;
	    when Subclause =>
		if Subclause_Number < 10 then
		    return Make_Clause_Number (Clause, Section_Number, Clause_Number) &
		        "." & Character'Val (Character'Pos('0') + Subclause_Number);
		elsif Subclause_Number < 20 then
		    return Make_Clause_Number (Clause, Section_Number, Clause_Number) &
		        ".1" & Character'Val (Character'Pos('0') + Subclause_Number - 10);
		else
		    return Make_Clause_Number (Clause, Section_Number, Clause_Number) &
		        ".2" & Character'Val (Character'Pos('0') + Subclause_Number - 20);
		end if;
	end case;
    end Make_Clause_Number;


    procedure Make_Clause (Clause_String : in String;
			   Section_Number : out Section_Number_Type;
			   Clause_Number : out Natural;
			   Subclause_Number : out Natural) is
	-- Returns the clause number for a properly formatted Section or
	-- clause string.

	Next : Positive;
	function Get_Section_Number return Section_Number_Type is
	    -- Extract the section number:
	begin
	    if Clause_String'Length = 1 or else
		Clause_String(Clause_String'First + 1) = '.' then
		Next := Clause_String'First + 2;
		if Clause_String (Clause_String'First) in '0' .. '9' then
	            return Character'Pos(Clause_String (Clause_String'First)) - Character'Pos('0');
		else
	            return Character'Pos(Clause_String (Clause_String'First)) - Character'Pos('A') + 21;
		end if;
	    else
		Next := Clause_String'First + 3;
	        return (Character'Pos(Clause_String (Clause_String'First)) - Character'Pos('0')) * 10 +
		    Character'Pos(Clause_String (Clause_String'First + 1)) - Character'Pos('0');
	    end if;
	end Get_Section_Number;

	function Get_Clause_Number return Natural is
	    -- Extract the clause:
	begin
	    if Clause_String'Last - Next + 1 = 1 or else
		Clause_String(Next + 1) = '.' then
		Next := Next + 2;
	        return Character'Pos(Clause_String (Next - 2)) - Character'Pos('0');
	    else
		Next := Next + 3;
	        return (Character'Pos(Clause_String (Next - 3)) - Character'Pos('0')) * 10 +
		    Character'Pos(Clause_String (Next - 3 + 1)) - Character'Pos('0');
	    end if;
	end Get_Clause_Number;

    begin
	if Clause_String'Length = 7 and then
	   Clause_String (Clause_String'First .. Clause_String'First + 5) =
	    "Annex " then -- Annex clauses.
	    Clause_Number := 0;
	    Subclause_Number := 0;
	    Section_Number := Character'Pos(Clause_String (Clause_String'First + 6)) - Character'Pos('A') + 21;
	elsif Clause_String'Length = 1 then
	    Clause_Number := 0;
	    Subclause_Number := 0;
	    Section_Number := Get_Section_Number;
	elsif Clause_String'Length = 2 then
	    Clause_Number := 0;
	    Subclause_Number := 0;
	    Section_Number := Get_Section_Number;
	else
	    Section_Number := Get_Section_Number;
	    -- Next is now the start of the Clause:
	    if Clause_String'Last - Next + 1 = 1 then
	        Clause_Number := Get_Clause_Number;
	        Subclause_Number := 0;
	    elsif Clause_String'Last - Next + 1 = 2 then
	        Clause_Number := Get_Clause_Number;
	        Subclause_Number := 0;
	    else
	        Clause_Number := Get_Clause_Number;
		-- Next is now the start of the Subclause:
	        if Clause_String'Last - Next + 1 = 1 then
	            Subclause_Number := Character'Pos(Clause_String (Next)) - Character'Pos('0');
	        else -- Two digit.
	            Subclause_Number := (Character'Pos(Clause_String (Next)) -
			Character'Pos('0')) * 10 +
			    Character'Pos(Clause_String (Next + 1)) - Character'Pos('0');
		end if;
	    end if;
	end if;
    end Make_Clause;


    function Lookup_Clause_Number (Title : in Title_Type) return String is
	-- Given the title of a clause, returns the formatted Section or
	-- clause number reference for that title. The Title must match
	-- exactly, except for case. Raises Not_Found_Error if not found.
	Lower_Title : constant Title_Type := Ada.Characters.Handling.To_Lower (Title);
    begin
	for I in 1 .. Last_Title loop
	    if Lower_Title = Title_List(I).Search_Title then
		return Make_Clause_Number (Title_List(I).Level,
					   Title_List(I).Section_Number,
					   Title_List(I).Clause_Number,
					   Title_List(I).Subclause_Number);
	    end if;
	end loop;
	raise Not_Found_Error;
    end Lookup_Clause_Number;


    function Lookup_Level (Title : in Title_Type) return Level_Type is
	-- Given the title of a clause, returns the level for that title. The Title must match
	-- exactly, except for case. Raises Not_Found_Error if not found.
	Lower_Title : constant Title_Type := Ada.Characters.Handling.To_Lower (Title);
    begin
	for I in 1 .. Last_Title loop
	    if Lower_Title = Title_List(I).Search_Title then
		return Title_List(I).Level;
	    end if;
	end loop;
	raise Not_Found_Error;
    end Lookup_Level;


    function Lookup_Title (Level : in Level_Type;
		   Section_Number : in Section_Number_Type;
		   Clause_Number : in Natural := 0;
		   Subclause_Number : in Natural := 0) return Title_Type is
	-- Given the level, section, and clause numbers, return the appropriate
	-- title. Raises Not_Found_Error if not found.
    begin
	for I in 1 .. Last_Title loop
	    if Title_List(I).Level = Level and then
	       Title_List(I).Section_Number = Section_Number and then
	       Title_List(I).Clause_Number = Clause_Number and then
	       Title_List(I).Subclause_Number = Subclause_Number then
		return Title_List(I).Title;
	    end if;
	end loop;
	raise Not_Found_Error;
    end Lookup_Title;


    function Lookup_Old_Title (Level : in Level_Type;
		   Section_Number : in Section_Number_Type;
		   Clause_Number : in Natural := 0;
		   Subclause_Number : in Natural := 0) return Title_Type is
	-- Given the level, section, and clause numbers, return the appropriate
	-- old title. Calls Lookup_Title if not found (thus returning the
	-- regular (new) title.
    begin
	for I in 1 .. Last_Old_Title loop
	    if Old_Title_List(I).Level = Level and then
	       Old_Title_List(I).Section_Number = Section_Number and then
	       Old_Title_List(I).Clause_Number = Clause_Number and then
	       Old_Title_List(I).Subclause_Number = Subclause_Number then
		return Old_Title_List(I).Title;
	    end if;
	end loop;
	return Lookup_Title (Level, Section_Number, Clause_Number, Subclause_Number);
    end Lookup_Old_Title;


    function Previous_Clause (Clause : in String) return String is
	-- Returns the string of the previous clause (in the table of contents)
	-- for the properly formatted clause string Clause.
	-- Raises Not_Found_Error if not found.
       Section_Number : Section_Number_Type;
       Clause_Number : Natural;
       Subclause_Number : Natural;
    begin
	Make_Clause (Clause, Section_Number, Clause_Number, Subclause_Number);
	for I in 1 .. Last_Title loop
	    if Title_List(I).Section_Number = Section_Number and then
	       Title_List(I).Clause_Number = Clause_Number and then
	       Title_List(I).Subclause_Number = Subclause_Number then
		if I = 1 then
		    raise Not_Found_Error;
		else
		    return Make_Clause_Number (Title_List(I-1).Level,
					       Title_List(I-1).Section_Number,
					       Title_List(I-1).Clause_Number,
					       Title_List(I-1).Subclause_Number);
	        end if;
	    end if;
	end loop;
	raise Not_Found_Error;
    end Previous_Clause;


    function Next_Clause (Clause : in String) return String is
	-- Returns the string of the next clause (in the table of contents)
	-- for the properly formatted clause string Clause.
	-- Raises Not_Found_Error if not found.
       Section_Number : Section_Number_Type;
       Clause_Number : Natural;
       Subclause_Number : Natural;
    begin
	Make_Clause (Clause, Section_Number, Clause_Number, Subclause_Number);
	for I in 1 .. Last_Title loop
	    if Title_List(I).Section_Number = Section_Number and then
	       Title_List(I).Clause_Number = Clause_Number and then
	       Title_List(I).Subclause_Number = Subclause_Number then
		if I = Last_Title then
		    raise Not_Found_Error;
		else
		    return Make_Clause_Number (Title_List(I+1).Level,
					       Title_List(I+1).Section_Number,
					       Title_List(I+1).Clause_Number,
					       Title_List(I+1).Subclause_Number);
	        end if;
	    end if;
	end loop;
	raise Not_Found_Error;
    end Next_Clause;


    procedure For_Each is
	-- Call Operate for each title in the contents, in the order that
	-- they were added to the contents. If the Quit parameter to Operate
	-- is True when Operate returns, the iteration is abandoned.
	Quit : Boolean := False;
    begin
	for I in 1 .. Last_Title loop
	    Operate (Title_List(I).Title,
		     Title_List(I).Level,
		     Title_List(I).Section_Number,
		     Title_List(I).Clause_Number,
		     Title_List(I).Subclause_Number,
		     Quit);
	    if Quit then
		return;
	    end if;
	end loop;
    end For_Each;

end ARM_Contents;
