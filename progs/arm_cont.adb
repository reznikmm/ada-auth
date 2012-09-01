with Ada.Characters.Handling;
--with Ada.Text_IO; -- Debug.
with Ada.Exceptions;
package body ARM_Contents is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package contains the routines to manage section/clause/subclause
    -- references.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2004, 2005, 2006, 2007, 2008, 2009, 2011, 2012
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
    --  4/19/00 - RLB - Created base package.
    --  4/26/00 - RLB - Added Previous_Clause and Next_Clause.
    --  5/15/00 - RLB - Added rules about unnumbered sections.
    --  5/22/00 - RLB - Added Unnumbered_Section level.
    --  8/22/00 - RLB - Added Old_Title handling.
    --  9/ 9/04 - RLB - Removed unused with.
    --  2/ 2/05 - RLB - Allowed more old titles.
    --  1/16/06 - RLB - Added debugging.
    --  9/22/06 - RLB - Created type Clause_Number_Type and added SubSubClause.
    -- 10/12/07 - RLB - Extended the range of properly formatted clause numbers.
    -- 12/18/07 - RLB - Added Plain_Annex.
    -- 10/24/08 - RLB - More old titles.
    --  5/07/09 - RLB - Added Dead_Clause.
    -- 10/18/11 - RLB - Changed to GPLv3 license.
    -- 10/19/11 - RLB - Added Parent_Clause from Stephen Leake's version.
    -- 10/25/11 - RLB - Added version to Old name strings.
    --  8/30/12 - RLB - Added traps if we're reading Section = UNKNOWN.

    function "<" (Left, Right : Clause_Number_Type) return Boolean is
	-- True if Left comes before Right in the collating order.
    begin
	if Left.Section = UNKNOWN then
	    raise Bad_Clause_Error with "Left has Unknown section";
	elsif Right.Section = UNKNOWN then
	    raise Bad_Clause_Error with "Right has Unknown section";
	elsif Left.Section < Right.Section then
	    return True;
	elsif Left.Section > Right.Section then
	    return False;
	elsif Left.Clause < Right.Clause then
	    return True;
	elsif Left.Clause > Right.Clause then
	    return False;
	elsif Left.Subclause < Right.Subclause then
	    return True;
	elsif Left.Subclause > Right.Subclause then
	    return False;
	elsif Left.Subsubclause < Right.Subsubclause then
	    return True;
	else
	    return False;
	end if;
    end "<";

    function ">" (Left, Right : Clause_Number_Type) return Boolean is
	-- True if Left comes after Right in the collating order.
    begin
	return Right < Left;
    end ">";

    function "<=" (Left, Right : Clause_Number_Type) return Boolean is
	-- True if Left comes before or is the same as Right in the
	-- collating order.
    begin
	return not (Right < Left);
    end "<=";

    function ">=" (Left, Right : Clause_Number_Type) return Boolean is
	-- True if Left comes after or is the same as Right in the
	-- collating order.
    begin
	return not (Left < Right);
    end ">=";


    type Title_Record is record
	Title : Title_Type; -- Title in original format.
	Search_Title : Title_Type; -- Title in all lower case.
	Level : Level_Type;
	Clause_Number : Clause_Number_Type;
        Version : ARM_Contents.Change_Version_Type := '0';
    end record;

    Title_List : array (1 .. 900) of Title_Record;
    Last_Title : Natural;

    Old_Title_List : array (1 .. 300) of Title_Record;
    Last_Old_Title : Natural;

    procedure Initialize is
	-- Initialize this package; make sure the contents are empty.
    begin
	Last_Title := 0;
    end Initialize;


    procedure Add (Title : in Title_Type;
		   Level : in Level_Type;
		   Clause_Number : in Clause_Number_Type;
                   Version : in ARM_Contents.Change_Version_Type := '0') is
	-- Add a section or clause to the contents. It has the specified
	-- characteristics.
    begin
	if Level /= Subsubclause and then Clause_Number.Subsubclause /= 0 then
	    raise Bad_Clause_Error with "not a subsubclause but non-zero subsubclause number";
	end if;
	if Level /= Subsubclause and then
	   Level /= Subclause and then Clause_Number.Subclause /= 0 then
	    raise Bad_Clause_Error with "not a subclause but non-zero subclause number";
	end if;
	if (Level /= Subsubclause and then Level /= Subclause and then
	    Level /= Clause and then Level /= Unnumbered_Section and then
	    Level /= Dead_Clause) and then
	   Clause_Number.Clause /= 0 then
	    raise Bad_Clause_Error with "not a clause but non-zero clause number";
	end if;
	Last_Title := Last_Title + 1;
	Title_List (Last_Title) :=
	    (Title => Title,
	     Search_Title => Ada.Characters.Handling.To_Lower (Title),
	     Level => Level,
	     Clause_Number => Clause_Number,
             Version => Version);
--Ada.Text_IO.Put_Line ("  Add " & Title &
-- " Index=" & Natural'Image(Last_Title) & " Level=" & Level_Type'Image(Level));
--Ada.Text_IO.Put_Line ("    Section" & Section_Number_Type'Image(Clause_Number.Section) &
-- " Clause" & Natural'Image(Clause_Number.Clause) & " Subclause" & Natural'Image(Clause_Number.Subclause) &
-- " Subsubclause" & Natural'Image(Clause_Number.Subsubclause));
    end Add;


    procedure Add_Old (Old_Title : in Title_Type;
		       Level : in Level_Type;
		       Clause_Number : in Clause_Number_Type;
                       Version : in ARM_Contents.Change_Version_Type := '0') is
	-- Add an old title for a section or clause to the contents. It has
	-- the specified characteristics; the version is the version for which
	-- it first was present in the document.
    begin
	if Level /= Subsubclause and then Clause_Number.Subsubclause /= 0 then
	    raise Bad_Clause_Error with "not a subsubclause but non-zero subsubclause number";
	end if;
	if Level /= Subsubclause and then
	   Level /= Subclause and then Clause_Number.Subclause /= 0 then
	    raise Bad_Clause_Error with "not a subclause but non-zero subclause number";
	end if;
	if (Level /= Subsubclause and then Level /= Subclause and then
	    Level /= Clause and then Level /= Unnumbered_Section and then
	    Level /= Dead_Clause) and then
	   Clause_Number.Clause /= 0 then
	    raise Bad_Clause_Error with "not a clause but non-zero clause number";
	end if;
	Last_Old_Title := Last_Old_Title + 1;
	Old_Title_List (Last_Old_Title) :=
	    (Title => Old_Title,
	     Search_Title => Ada.Characters.Handling.To_Lower (Old_Title),
	     Level => Level,
	     Clause_Number => Clause_Number,
             Version => Version);
--Ada.Text_IO.Put_Line ("  Add_Old " & Old_Title &
-- " Index=" & Natural'Image(Last_Old_Title) & " Level=" & Level_Type'Image(Level));
--Ada.Text_IO.Put_Line ("    Section" & Section_Number_Type'Image(Section_Number) &
-- " Clause" & Natural'Image(Clause_Number.Clause) & " Subclause" & Natural'Image(Clause_Number.Subclause) &
-- " Subsubclause" & Natural'Image(Clause_Number.Subsubclause));
    end Add_Old;


    function Make_Clause_Number (Level : in Level_Type;
		   Clause_Number : in Clause_Number_Type) return String is
	-- Returns a properly formatted Section or clause number reference.
    begin
	if Clause_Number.Section = UNKNOWN then
	    raise Bad_Clause_Error with "unknown section number";
	-- else not unknown
	end if;
	case Level is
	    when Plain_Annex | Normative_Annex | Informative_Annex =>
		if Clause_Number.Clause /= 0 or else Clause_Number.Subclause /= 0 or else
		   Clause_Number.Subsubclause /= 0 or else Clause_Number.Section <= 30 then
		    raise Bad_Clause_Error; -- Illegal numbers.
		end if;
		return "Annex " & Character'Val (Character'Pos('A') + (Clause_Number.Section - ANNEX_START));
	    when Section =>
		if Clause_Number.Clause /= 0 or else Clause_Number.Subclause /= 0 or else
		   Clause_Number.Section >= ANNEX_START then
		    raise Bad_Clause_Error; -- Illegal numbers.
		end if;
		if Clause_Number.Section < 10 then
		    return Character'Val (Character'Pos('0') + Clause_Number.Section) & "";
		elsif Clause_Number.Section < 20 then
		    return "1" & Character'Val (Character'Pos('0') + Clause_Number.Section - 10);
		elsif Clause_Number.Section < 30 then
		    return "2" & Character'Val (Character'Pos('0') + Clause_Number.Section - 20);
		else
		    return "3" & Character'Val (Character'Pos('0') + Clause_Number.Section - 30);
		end if;
	    when Unnumbered_Section =>
		if Clause_Number.Clause = 0 or else Clause_Number.Subclause /= 0 or else
		   Clause_Number.Section /= 0 then
		    raise Bad_Clause_Error; -- Illegal numbers.
		end if;
	        if Clause_Number.Clause < 10 then
		    return "0." & Character'Val (Character'Pos('0') + Clause_Number.Clause);
	        elsif Clause_Number.Clause < 20 then
		    return "0.1" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 10);
	        elsif Clause_Number.Clause < 30 then
		    return "0.2" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 20);
	        else
		    return "0.3" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 30);
	        end if;
	    when Clause =>
		if Clause_Number.Subclause /= 0 then
		    raise Bad_Clause_Error; -- Illegal number.
		end if;
		if Clause_Number.Section < 10 then
		    if Clause_Number.Clause < 10 then
		        return Character'Val (Character'Pos('0') + Clause_Number.Section) &
		            "." & Character'Val (Character'Pos('0') + Clause_Number.Clause);
		    elsif Clause_Number.Clause < 20 then
		        return Character'Val (Character'Pos('0') + Clause_Number.Section) &
		            ".1" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 10);
		    elsif Clause_Number.Clause < 30 then
		        return Character'Val (Character'Pos('0') + Clause_Number.Section) &
		            ".2" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 20);
		    elsif Clause_Number.Clause < 40 then
		        return Character'Val (Character'Pos('0') + Clause_Number.Section) &
		            ".3" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 30);
		    elsif Clause_Number.Clause < 50 then
		        return Character'Val (Character'Pos('0') + Clause_Number.Section) &
		            ".4" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 40);
		    elsif Clause_Number.Clause < 60 then
		        return Character'Val (Character'Pos('0') + Clause_Number.Section) &
		            ".5" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 50);
		    else
			raise Bad_Clause_Error; -- Out of range.
		    end if;
		elsif Clause_Number.Section < 20 then
		    if Clause_Number.Clause < 10 then
		        return "1" & Character'Val (Character'Pos('0') + Clause_Number.Section - 10) &
		            "." & Character'Val (Character'Pos('0') + Clause_Number.Clause);
		    elsif Clause_Number.Clause < 20 then
		        return "1" & Character'Val (Character'Pos('0') + Clause_Number.Section - 10) &
		            ".1" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 10);
		    elsif Clause_Number.Clause < 30 then
		        return "1" & Character'Val (Character'Pos('0') + Clause_Number.Section - 10) &
		            ".2" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 20);
		    elsif Clause_Number.Clause < 40 then
		        return "1" & Character'Val (Character'Pos('0') + Clause_Number.Section - 10) &
		            ".3" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 30);
		    elsif Clause_Number.Clause < 50 then
		        return "1" & Character'Val (Character'Pos('0') + Clause_Number.Section - 10) &
		            ".4" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 40);
		    elsif Clause_Number.Clause < 60 then
		        return "1" & Character'Val (Character'Pos('0') + Clause_Number.Section - 10) &
		            ".5" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 50);
		    else
			raise Bad_Clause_Error; -- Out of range.
		    end if;
		elsif Clause_Number.Section < 30 then
		    if Clause_Number.Clause < 10 then
		        return "2" & Character'Val (Character'Pos('0') + Clause_Number.Section - 20) &
		            "." & Character'Val (Character'Pos('0') + Clause_Number.Clause);
		    elsif Clause_Number.Clause < 20 then
		        return "2" & Character'Val (Character'Pos('0') + Clause_Number.Section - 20) &
		            ".1" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 10);
		    elsif Clause_Number.Clause < 30 then
		        return "2" & Character'Val (Character'Pos('0') + Clause_Number.Section - 20) &
		            ".2" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 20);
		    elsif Clause_Number.Clause < 40 then
		        return "2" & Character'Val (Character'Pos('0') + Clause_Number.Section - 20) &
		            ".3" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 30);
		    elsif Clause_Number.Clause < 50 then
		        return "2" & Character'Val (Character'Pos('0') + Clause_Number.Section - 20) &
		            ".4" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 40);
		    elsif Clause_Number.Clause < 60 then
		        return "2" & Character'Val (Character'Pos('0') + Clause_Number.Section - 20) &
		            ".5" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 50);
		    else
			raise Bad_Clause_Error; -- Out of range.
		    end if;
		elsif Clause_Number.Section = 30 then
		    if Clause_Number.Clause < 10 then
		        return "30." & Character'Val (Character'Pos('0') + Clause_Number.Clause);
		    elsif Clause_Number.Clause < 20 then
		        return "30.1" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 10);
		    elsif Clause_Number.Clause < 30 then
		        return "30.2" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 20);
		    elsif Clause_Number.Clause < 40 then
		        return "30.3" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 30);
		    elsif Clause_Number.Clause < 50 then
		        return "30.4" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 40);
		    elsif Clause_Number.Clause < 60 then
		        return "30.5" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 50);
		    else
			raise Bad_Clause_Error; -- Out of range.
		    end if;
		else
		    if Clause_Number.Clause < 10 then
			return Character'Val (Character'Pos('A') + (Clause_Number.Section - ANNEX_START)) &
		            "." & Character'Val (Character'Pos('0') + Clause_Number.Clause);
		    elsif Clause_Number.Clause < 20 then
			return Character'Val (Character'Pos('A') + (Clause_Number.Section - ANNEX_START)) &
		            ".1" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 10);
		    elsif Clause_Number.Clause < 30 then
			return Character'Val (Character'Pos('A') + (Clause_Number.Section - ANNEX_START)) &
		            ".2" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 20);
		    elsif Clause_Number.Clause < 40 then
			return Character'Val (Character'Pos('A') + (Clause_Number.Section - ANNEX_START)) &
		            ".3" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 30);
		    elsif Clause_Number.Clause < 50 then
			return Character'Val (Character'Pos('A') + (Clause_Number.Section - ANNEX_START)) &
		            ".4" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 40);
		    elsif Clause_Number.Clause < 60 then
			return Character'Val (Character'Pos('A') + (Clause_Number.Section - ANNEX_START)) &
		            ".4" & Character'Val (Character'Pos('0') + Clause_Number.Clause - 50);
		    else
			raise Bad_Clause_Error; -- Out of range.
		    end if;
		end if;
	    when Subclause =>
		if Clause_Number.Section = UNKNOWN then
		    raise Bad_Clause_Error with "unknown section number";
		elsif Clause_Number.Subclause < 10 then
		    return Make_Clause_Number (Clause, (Clause_Number.Section, Clause_Number.Clause, 0, 0)) &
		        "." & Character'Val (Character'Pos('0') + Clause_Number.Subclause);
		elsif Clause_Number.Subclause < 20 then
		    return Make_Clause_Number (Clause, (Clause_Number.Section, Clause_Number.Clause, 0, 0)) &
		        ".1" & Character'Val (Character'Pos('0') + Clause_Number.Subclause - 10);
		elsif Clause_Number.Subclause < 30 then
		    return Make_Clause_Number (Clause, (Clause_Number.Section, Clause_Number.Clause, 0, 0)) &
		        ".2" & Character'Val (Character'Pos('0') + Clause_Number.Subclause - 20);
		elsif Clause_Number.Subclause < 40 then
		    return Make_Clause_Number (Clause, (Clause_Number.Section, Clause_Number.Clause, 0, 0)) &
		        ".3" & Character'Val (Character'Pos('0') + Clause_Number.Subclause - 30);
		elsif Clause_Number.Subclause < 50 then
		    return Make_Clause_Number (Clause, (Clause_Number.Section, Clause_Number.Clause, 0, 0)) &
		        ".4" & Character'Val (Character'Pos('0') + Clause_Number.Subclause - 40);
	        else
		    raise Bad_Clause_Error; -- Out of range.
		end if;
	    when Subsubclause =>
		if Clause_Number.Subsubclause < 10 then
		    return Make_Clause_Number (Subclause, (Clause_Number.Section, Clause_Number.Clause, Clause_Number.Subclause, 0)) &
		        "." & Character'Val (Character'Pos('0') + Clause_Number.Subsubclause);
		elsif Clause_Number.Subclause < 20 then
		    return Make_Clause_Number (Subclause, (Clause_Number.Section, Clause_Number.Clause, Clause_Number.Subclause, 0)) &
		        ".1" & Character'Val (Character'Pos('0') + Clause_Number.Subsubclause - 10);
		elsif Clause_Number.Subclause < 30 then
		    return Make_Clause_Number (Subclause, (Clause_Number.Section, Clause_Number.Clause, Clause_Number.Subclause, 0)) &
		        ".2" & Character'Val (Character'Pos('0') + Clause_Number.Subsubclause - 20);
		elsif Clause_Number.Subclause < 40 then
		    return Make_Clause_Number (Subclause, (Clause_Number.Section, Clause_Number.Clause, Clause_Number.Subclause, 0)) &
		        ".3" & Character'Val (Character'Pos('0') + Clause_Number.Subsubclause - 30);
		elsif Clause_Number.Subclause < 50 then
		    return Make_Clause_Number (Subclause, (Clause_Number.Section, Clause_Number.Clause, Clause_Number.Subclause, 0)) &
		        ".4" & Character'Val (Character'Pos('0') + Clause_Number.Subsubclause - 40);
	        else
		    raise Bad_Clause_Error; -- Out of range.
		end if;
	    when Dead_Clause =>
		return "X.X";
	end case;
    end Make_Clause_Number;


    procedure Make_Clause (Clause_String : in String;
			   Clause_Number : out Clause_Number_Type) is
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
	            return Character'Pos(Clause_String (Clause_String'First)) - Character'Pos('A') + ANNEX_START;
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
	    Clause_Number :=
		(Section => Character'Pos(Clause_String (Clause_String'First + 6)) - Character'Pos('A') + ANNEX_START,
		 Clause | Subclause | Subsubclause => 0);
	elsif Clause_String'Length = 1 then
	    Clause_Number :=
		(Section => Get_Section_Number,
		 Clause | Subclause | Subsubclause => 0);
	elsif Clause_String'Length = 2 then
	    Clause_Number :=
		(Section => Get_Section_Number,
		 Clause | Subclause | Subsubclause => 0);
	else
	    Clause_Number :=
		(Section => Get_Section_Number,
		 Clause | Subclause | Subsubclause => 0);
	    -- Next is now the start of the Clause:
	    if Clause_String'Last - Next + 1 = 1 then
	        Clause_Number.Clause := Get_Clause_Number;
	    elsif Clause_String'Last - Next + 1 = 2 then
	        Clause_Number.Clause := Get_Clause_Number;
	    else
	        Clause_Number.Clause := Get_Clause_Number;
		-- Next is now the start of the Subclause:
	        if Clause_String'Last - Next + 1 = 1 then
	            Clause_Number.Subclause := Character'Pos(Clause_String (Next)) - Character'Pos('0');
	        elsif Clause_String'Last - Next + 1 = 2 then
	            Clause_Number.Subclause := (Character'Pos(Clause_String (Next)) -
			Character'Pos('0')) * 10 +
			    Character'Pos(Clause_String (Next + 1)) - Character'Pos('0');
		else
		    if Clause_String'Last - Next + 1 = 1 or else
			Clause_String(Next + 1) = '.' then
			Next := Next + 2;
		        Clause_Number.Subclause := Character'Pos(Clause_String (Next - 2)) - Character'Pos('0');
		    else
			Next := Next + 3;
		        Clause_Number.Subclause := (Character'Pos(Clause_String (Next - 3)) - Character'Pos('0')) * 10 +
			    Character'Pos(Clause_String (Next - 3 + 1)) - Character'Pos('0');
		    end if;
	            if Clause_String'Last - Next + 1 = 1 then
	                Clause_Number.Subsubclause := Character'Pos(Clause_String (Next)) - Character'Pos('0');
	            else -- Two digit.
	                Clause_Number.Subsubclause := (Character'Pos(Clause_String (Next)) -
			    Character'Pos('0')) * 10 +
			        Character'Pos(Clause_String (Next + 1)) - Character'Pos('0');
		    end if;
		end if;
	    end if;
	end if;
	if Clause_Number.Section = UNKNOWN then
	    raise Bad_Clause_Error with "unknown section number";
	-- else not unknown
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
					   Title_List(I).Clause_Number);
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
		           Clause_Number : in Clause_Number_Type) return Title_Type is
	-- Given the level and clause numbers, return the appropriate
	-- title. Raises Not_Found_Error if not found.
    begin
	if Clause_Number.Section = UNKNOWN then
	    raise Bad_Clause_Error with "unknown section number";
	-- else not unknown
	end if;
	for I in 1 .. Last_Title loop
	    if Title_List(I).Level = Level and then
	       Title_List(I).Clause_Number = Clause_Number then
		return Title_List(I).Title;
	    end if;
	end loop;
	raise Not_Found_Error;
    end Lookup_Title;


    function Lookup_Old_Title (Level : in Level_Type;
		   Clause_Number : in Clause_Number_Type) return Title_Type is
	-- Given the level and clause numbers, return the appropriate
	-- old title. Calls Lookup_Title if not found (thus returning the
	-- regular (new) title.
    begin
	if Clause_Number.Section = UNKNOWN then
	    raise Bad_Clause_Error with "unknown section number";
	-- else not unknown
	end if;
	for I in 1 .. Last_Old_Title loop
	    if Old_Title_List(I).Level = Level and then
	       Old_Title_List(I).Clause_Number = Clause_Number then
		return Old_Title_List(I).Title;
	    end if;
	end loop;
	return Lookup_Title (Level, Clause_Number);
    end Lookup_Old_Title;


    function Previous_Clause (Clause : in String) return String is
	-- Returns the string of the previous clause (in the table of contents)
	-- for the properly formatted clause string Clause.
	-- Raises Not_Found_Error if not found.
        Clause_Number : Clause_Number_Type;
    begin
	Make_Clause (Clause, Clause_Number);
	for I in 1 .. Last_Title loop
	    if Title_List(I).Clause_Number = Clause_Number then
		for J in reverse 1 .. I - 1 loop
		    if Title_List(J).Level /= Dead_Clause then
		        return Make_Clause_Number (Title_List(J).Level,
					           Title_List(J).Clause_Number);
		    -- else skip it and continue.
		    end if;
		end loop;
		-- If we get here, it was not found.
		raise Not_Found_Error;
	    end if;
	end loop;
	raise Not_Found_Error;
    end Previous_Clause;


    function Next_Clause (Clause : in String) return String is
	-- Returns the string of the next clause (in the table of contents)
	-- for the properly formatted clause string Clause.
	-- Raises Not_Found_Error if not found.
        Clause_Number : Clause_Number_Type;
    begin
	Make_Clause (Clause, Clause_Number);
	for I in 1 .. Last_Title loop
	    if Title_List(I).Clause_Number = Clause_Number then
		for J in I + 1 .. Last_Title loop
		    if Title_List(J).Level /= Dead_Clause then
		        return Make_Clause_Number (Title_List(J).Level,
					           Title_List(J).Clause_Number);
		    -- else skip it and continue.
		    end if;
		end loop;
		-- If we get here, it was not found.
		raise Not_Found_Error;
	    end if;
	end loop;
	raise Not_Found_Error;
    end Next_Clause;


    function Parent_Clause (Clause : in String) return String is
        -- Returns the string of the parent clause (in the table of contents)
        -- for the properly formatted clause string Clause.
        --
        -- Result is a null string if Clause is a top level clause;
        -- Section, Unnumbered_Section, Normative_Annex,
        -- Informative_Annex, Plain_Annex.
        Clause_Number : Clause_Number_Type;
    begin
	Make_Clause (Clause, Clause_Number);

	if Clause_Number.Clause = 0 then
	   -- Clause is a section; no parent
	   return "";

	elsif Clause_Number.Subclause = 0 then
	   -- Clause is a clause; parent is Section or Annex
	   if Clause_Number.Section >= ANNEX_START then
	      return Make_Clause_Number (Normative_Annex, (Clause_Number.Section, 0, 0, 0));
	   else
	      return Make_Clause_Number (Section, (Clause_Number.Section, 0, 0, 0));
	   end if;

	elsif Clause_Number.Subsubclause = 0 then
	   -- Clause is a subclause; clause is parent
	   return Make_Clause_Number (ARM_Contents.Clause, (Clause_Number.Section, Clause_Number.Clause, 0, 0));

	else
	   -- Clause is a subsubclause; subclause is parent
	   return Make_Clause_Number
	     (Subclause, (Clause_Number.Section, Clause_Number.Clause, Clause_Number.Subclause, 0));
	end if;
    end Parent_Clause;


    procedure For_Each is
	-- Call Operate for each title in the contents, in the order that
	-- they were added to the contents (other than dead clauses). If the
	-- Quit parameter to Operate is True when Operate returns, the
	-- iteration is abandoned.
	Quit : Boolean := False;
    begin
	for I in 1 .. Last_Title loop
	    if Title_List(I).Level /= Dead_Clause then
	        Operate (Title_List(I).Title,
		         Title_List(I).Level,
		         Title_List(I).Clause_Number,
		         Title_List(I).Version,
		         Quit);
	    -- else skip it.
	    end if;
	    if Quit then
		return;
	    end if;
	end loop;
    end For_Each;

end ARM_Contents;
