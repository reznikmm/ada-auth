with Ada.Unchecked_Deallocation,
     Ada.Strings.Fixed;
package body ARM_Syntax is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package contains the database to collect the syntax summary and
    -- cross-reference.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2004, 2006, 2011
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
    --  5/17/00 - RLB - Created package.
    --  5/24/00 - RLB - Updated to use revised Tabset.
    --  5/26/00 - RLB - Added a Tabset parameter.
    --  8/ 4/00 - RLB - Changed style to make font smaller (per Duff).
    --  8/16/00 - RLB - Added NoParaNum; removed junk space (which caused
    --			blank lines and paragraph numbers).
    --  9/26/00 - RLB - Revised to use SyntaxDisplay format to get more
    --			control over the formating of this section.
    --  9/27/00 - RLB - Revised XRef to decrease white space.
    --  9/28/00 - RLB - Added code to make links in HTML version.
    --  9/09/04 - RLB - Removed unused junk noted by Stephen Leake.
    --  6/22/06 - RLB - Added additional information to improve the links.
    --			Changed the cross-reference table to use the Ada 83
    --			format (which adds missing section references).
    -- 10/13/06 - RLB - Added Defined flag to cross-references to eliminate
    --			junk errors from not-quite-non-terminals.
    -- 10/18/11 - RLB - Changed to GPLv3 license.
    --  9/19/22 - RLB - Added Two_Column_Format parameter to XRef.

    type String_Ptr is access String;
    type Rule_Type;
    type Rule_Ptr is access Rule_Type;
    type Rule_Type is record
	Clause : String (1..10);
	Clause_Len : Natural;
	Rule : String_Ptr;
	Tabset : String (1..40);
	Next : Rule_Ptr;
    end record;

    Rule_List : Rule_Ptr := null;
    Rule_List_Tail : Rule_Ptr := null;

    type XRef_Type;
    type XRef_Ptr is access XRef_Type;
    type XRef_Type is record
	Clause : String (1..10);
	Clause_Len : Natural;
	Name : String (1..40);
	Name_Len : Natural;
	Used_In : String (1..40);
	Used_In_Len : Natural;
	Defined : Boolean;
	Next : XRef_Ptr;
    end record;

    XRef_List : XRef_Ptr := null;
    XRef_Count : Natural := 0;

    type NT_Type;
    type NT_Ptr is access NT_Type;
    type NT_Type is record
	Name : String (1..40);
	Name_Len : Natural;
	Clause : String (1..10);
	Clause_Len : Natural;
	Link_Target : Target_Type;
	Next : NT_Ptr;
    end record;

    NT_List : NT_Ptr := null;
    NT_Count : Natural := 0;

    procedure Free is new Ada.Unchecked_Deallocation (Rule_Type, Rule_Ptr);
    procedure Free is new Ada.Unchecked_Deallocation (XRef_Type, XRef_Ptr);
    procedure Free is new Ada.Unchecked_Deallocation (NT_Type, NT_Ptr);
    procedure Free is new Ada.Unchecked_Deallocation (String, String_Ptr);

    procedure Create is
	-- Initialize the syntax database.
    begin
	Rule_List := null;
	Rule_List_Tail := null;
    end Create;


    procedure Destroy is
	-- Destroy the syntax database.
	RTemp : Rule_Ptr;
    begin
	while Rule_List /= null loop
	    RTemp := Rule_List;
	    Rule_List := RTemp.Next;
	    Free (RTemp.Rule);
	    Free (RTemp);
	end loop;

    end Destroy;


    procedure Insert_Rule (
	For_Clause : in String;
	Rule : in String;
	Tabset : in String := "") is
	-- Add a rule for the syntax summary. The rule appears in For_Clause.
	-- Tabset provides any needed tab settings.
	Temp_Rule : Rule_Type;
    begin
	Ada.Strings.Fixed.Move (Target => Temp_Rule.Clause,
				Source => For_Clause,
				Drop   => Ada.Strings.Error,
			        Pad    => ' ');
	Temp_Rule.Clause_Len := For_Clause'Length;
	Temp_Rule.Rule := new String'(Rule);
	Ada.Strings.Fixed.Move (Target => Temp_Rule.Tabset,
				Source => Tabset,
				Drop   => Ada.Strings.Error,
			        Pad    => ' ');
	Temp_Rule.Next := null;
	if Rule_List_Tail = null then
	    Rule_List := new Rule_Type'(Temp_Rule);
	    Rule_List_Tail := Rule_List;
	else
	    Rule_List_Tail.Next := new Rule_Type'(Temp_Rule);
	    Rule_List_Tail := Rule_List_Tail.Next;
	end if;
    end Insert_Rule;


    procedure Add_Non_Terminal (
	NT_Name : in String;
	For_Clause : in String;
	Link_Target : out ARM_Syntax.Target_Type) is
	-- Add a non-terminal to the syntax list. Returns a new Link_Target
	-- for the Non-Terminal.
	Temp_NT : NT_Type;
    begin
	Ada.Strings.Fixed.Move (Target => Temp_NT.Clause,
				Source => For_Clause,
				Drop   => Ada.Strings.Error,
			        Pad    => ' ');
	Temp_NT.Clause_Len := For_Clause'Length;
	Ada.Strings.Fixed.Move (Target => Temp_NT.Name,
				Source => NT_Name,
				Drop   => Ada.Strings.Error,
			        Pad    => ' ');
	Temp_NT.Name_Len := NT_Name'Length;

	declare
	    Val : constant String := Natural'Image(NT_Count);
	begin
	    Temp_NT.Link_Target := "S0000";
	    if Val'Length <= 5 then
	        Temp_NT.Link_Target (5-(Val'Length-2)..5) :=
		    Val(2..Val'Last);
	    else
		raise Program_Error; -- Too many.
	    end if;
	    Link_Target := Temp_NT.Link_Target;
	end;

	if NT_List = null then
	    Temp_NT.Next := null;
	    NT_List := new NT_Type'(Temp_NT);
	else
	    Temp_NT.Next := NT_List;
	    NT_List := new NT_Type'(Temp_NT);
	end if;
	NT_Count := NT_Count + 1;

    end Add_Non_Terminal;


    function Non_Terminal_Clause (NT_Name : in String) return String is
	-- Return the clause where NT_Name is declared.
	-- Returns "" if NT_Name is not a declared Non_Terminal.
	Loc : NT_Ptr;
    begin
	Loc := NT_List;
	while Loc /= null loop
	    if NT_Name = Loc.Name(1..Loc.Name_Len) then
		return Loc.Clause(1..Loc.Clause_Len);
	    end if;
	    Loc := Loc.Next;
	end loop;
	return ""; -- Not found.
    end Non_Terminal_Clause;


    function Non_Terminal_Link_Target (NT_Name : in String) return Target_Type is
	-- Return the link target for NT_Name.
	-- Returns "     " if NT_Name is not a declared Non_Terminal.
	Loc : NT_Ptr;
    begin
	Loc := NT_List;
	while Loc /= null loop
	    if NT_Name = Loc.Name(1..Loc.Name_Len) then
		return Loc.Link_Target;
	    end if;
	    Loc := Loc.Next;
	end loop;
	return Target_Type'(others => ' '); -- Not found.
    end Non_Terminal_Link_Target;


    procedure Add_Xref (
	Name : in String;
	Used_In : in String;
	Clause : in String;
	Defined : in Boolean) is
	-- Add a cross-reference entry.
	-- The item referenced is Name, and it is referenced in the production
	-- for Used_In, in Clause. It is a defined non-terminal if Defined
	-- is True (thus it can be linked).
	Temp_XRef : XRef_Type;
    begin
	Ada.Strings.Fixed.Move (Target => Temp_XRef.Clause,
				Source => Clause,
				Drop   => Ada.Strings.Error,
			        Pad    => ' ');
	Temp_XRef.Clause_Len := Clause'Length;
	Ada.Strings.Fixed.Move (Target => Temp_XRef.Name,
				Source => Name,
				Drop   => Ada.Strings.Error,
			        Pad    => ' ');
	Temp_XRef.Name_Len := Name'Length;
	Ada.Strings.Fixed.Move (Target => Temp_XRef.Used_In,
				Source => Used_In,
				Drop   => Ada.Strings.Error,
			        Pad    => ' ');
	Temp_XRef.Used_In_Len := Used_In'Length;
	Temp_XRef.Defined := Defined;

	-- Check for an identical record already loaded:
	declare
	    Temp : XRef_Ptr := XRef_List;
	begin
	    -- We assume that all of the items from the current clause
	    -- are together at the top of the list. If the list is inserted
	    -- in reverse order (the default), that will be true.
	    while Temp /= null and then
		  (Temp.Clause_Len = Temp_XRef.Clause_Len) and then
	          (Temp.Clause = Temp_XRef.Clause) loop
		if (Temp.Name_Len = Temp_XRef.Name_Len) and then
	           (Temp.Used_In_Len = Temp_XRef.Used_In_Len) and then
	           (Temp.Name = Temp_XRef.Name) and then
	           (Temp.Used_In = Temp_XRef.Used_In) then
	            -- Identical to an existing item, forget it.
	            -- (We do this to eliminate multiple items from one production.
	            return;
		end if;
		Temp := Temp.Next;
	    end loop;
	end;

	if XRef_List = null then
	    Temp_XRef.Next := null;
	    XRef_List := new XRef_Type'(Temp_XRef);
	else
	    Temp_XRef.Next := XRef_List;
	    XRef_List := new XRef_Type'(Temp_XRef);
	end if;
	XRef_Count := XRef_Count + 1;
    end Add_Xref;


    --generic
    --	with procedure Format_Text (Text : in String;
    --				    Text_Name : in String);
    procedure Report is
	-- Output the fully formatted syntax summary to the
	-- "Format_Text" routine. "Format_Text" allows all commands
	-- for the full formatter. (Text_Name is an identifying name
	-- for error messages).
	Temp : Rule_Ptr;
    begin
	Format_Text ("@begin(syntaxdisplay)" & Ascii.LF, "Prefix");
	Temp := Rule_List;
	while Temp /= null loop
	    if Ada.Strings.Fixed.Trim (Temp.Tabset, Ada.Strings.Right) = "" then
	        Format_Text ("@noparanum@RefSecbyNum{" &
		    Temp.Clause(1..Temp.Clause_Len) & "}:" & Ascii.LF &
		    Temp.Rule.all & Ascii.LF & Ascii.LF,
		    Temp.Clause(1..Temp.Clause_Len));
	    else
	        Format_Text ("@noparanum@tabclear{}@tabset{" &
		    Ada.Strings.Fixed.Trim (Temp.Tabset, Ada.Strings.Right) &
		    "}@RefSecbyNum{" & Temp.Clause(1..Temp.Clause_Len) & "}:" & Ascii.LF &
		    Temp.Rule.all & Ascii.LF & Ascii.LF,
		    Temp.Clause(1..Temp.Clause_Len));
	    end if;
	    Temp := Temp.Next;
	end loop;
	Format_Text ("@end(syntaxdisplay)" & Ascii.LF, "Suffix");
    end Report;


    --generic
    --	with procedure Format_Text (Text : in String;
    --				    Text_Name : in String);
    procedure XRef (Two_Column_Format : in Boolean) is
	-- Output the fully formatted syntax cross-reference to the
	-- "Format_Text" routine. "Format_Text" allows all commands
	-- for the full formatter. (Text_Name is an identifying name
	-- for error messages). The result is formatted in two columns
        -- if Two_Column_Format is True, and one column otherwise.
	Temp : XRef_Ptr;
	Last : XRef_Ptr := null;
	Items : array (1..XRef_Count) of XRef_Ptr;
    begin
	-- Sort the items:

	-- Load the items:
	Temp := XRef_List;
	for I in Items'range loop
	    Items(I) := Temp;
	    Temp := Temp.Next;
	end loop;

	-- Sort the items array (use an insertion sort):
	declare
	    Left : Natural;  -- Left sorting stop

	    function "<" (Left, Right : XRef_Ptr) return Boolean is
	    begin
		-- We sort first on "Name", then on "Used_In".
		if Left.Name (1..Left.Name_Len) < Right.Name (1..Right.Name_Len) then
		    return True;
		elsif Left.Name (1..Left.Name_Len) > Right.Name (1..Right.Name_Len) then
		    return False;
		else
		    return Left.Used_In (1..Left.Used_In_Len) < Right.Used_In (1..Right.Used_In_Len);
		end if;
	    end "<";

	begin
	    for Right In Items'First+1 .. Items'Last loop -- Right sorting stop
		Temp := Items(Right);
		Left := Right - 1;
		while Temp < Items(Left) loop -- Switch items
		    Items(Left + 1) := Items(Left);
		    Left := Left - 1;
		    exit when Left = 0;
		end loop;
	        Items(Left + 1) := Temp;
	    end loop;
	end;

        -- Relink the items in the sorted order:
        for I in Items'First .. Items'Last - 1 loop
	    Items(I).Next := Items(I+1);
        end loop;
        if Items'Length > 0 then
	    Items(Items'Last).Next := null;
	    XRef_List := Items(1);
        else
	    XRef_List := null;
        end if;


	Format_Text ("@begin(syntaxdisplay)" & Ascii.LF, "Prefix");
        if Two_Column_Format then
            Format_Text ("@tabclear()@tabset(P4, P38)" & Ascii.LF, "Prefix");
	    Format_Text ("@begin(twocol)" & Ascii.LF, "Prefix");
        else -- One column, make wider.
            Format_Text ("@tabclear()@tabset(P4, P52)" & Ascii.LF, "Prefix");
        end if;
	Temp := XRef_List;
	while Temp /= null loop
	    if Last = null or else
		Last.Name (1..Last.Name_Len) /= Temp.Name (1..Temp.Name_Len) then
		-- New header:
		declare
		    Clause : constant String :=
			Non_Terminal_Clause (Temp.Name (1..Temp.Name_Len));
		begin
		    if Temp.Defined then
		        if Clause /= "" then
		            Format_Text ("@noparanum@trailing@nt{" & Temp.Name (1..Temp.Name_Len) &
		                 "}@\@RefSecbyNum{" & Clause & "}" & Ascii.LF,
		                 Temp.Name (1..Temp.Name_Len) & " header");
		        else -- Undefined? Weird, but don't break, just use the
			     -- Ada 83 ellipsis.
		            Format_Text ("@noparanum@trailing@nt{" & Temp.Name (1..Temp.Name_Len) &
		                 "}@\..." & Ascii.LF,
		                 Temp.Name (1..Temp.Name_Len) & " header");
		        end if;
		    else
		        if Clause /= "" then
		            Format_Text ("@noparanum@trailing@ntf{" & Temp.Name (1..Temp.Name_Len) &
		                 "}@\@RefSecbyNum{" & Clause & "}" & Ascii.LF,
		                 Temp.Name (1..Temp.Name_Len) & " header");
		        else -- Undefined? Weird, but don't break, just use the
			     -- Ada 83 ellipsis.
		            Format_Text ("@noparanum@trailing@ntf{" & Temp.Name (1..Temp.Name_Len) &
		                 "}@\..." & Ascii.LF,
		                 Temp.Name (1..Temp.Name_Len) & " header");
		        end if;
		    end if;
		end;
	        -- Original:
		--Format_Text ("@noparanum@trailing@nt{" & Temp.Name (1..Temp.Name_Len) &
		--     "}" & Ascii.LF,
		--     Temp.Name (1..Temp.Name_Len) & " header");
		Last := Temp;
	    end if;
	    if Temp.Next = null or else
		Temp.Name (1..Temp.Name_Len) /= Temp.Next.Name (1..Temp.Next.Name_Len) then
		-- Last item of a set.
	        Format_Text ("@\@nt{" & Temp.Used_In(1..Temp.Used_In_Len) & "}@\" &
		    "@RefSecbyNum{" & Temp.Clause(1..Temp.Clause_Len) & '}' & Ascii.LF & Ascii.LF,
	            Temp.Name (1..Temp.Name_Len) & " ref " & Temp.Clause(1..Temp.Clause_Len));
	    else -- Not an end item.
	        Format_Text ("@\@nt{" & Temp.Used_In(1..Temp.Used_In_Len) & "}@\" &
		    "@RefSecbyNum{" & Temp.Clause(1..Temp.Clause_Len) & '}' & Ascii.LF,
	            Temp.Name (1..Temp.Name_Len) & " ref " & Temp.Clause(1..Temp.Clause_Len));
	    end if;
	    Temp := Temp.Next;
	end loop;
        if Two_Column_Format then
	    Format_Text ("@end(twocol)" & Ascii.LF, "Suffix");
        -- else nothing needed for one column.
        end if;
	Format_Text ("@end(syntaxdisplay)" & Ascii.LF, "Suffix");
	-- Should free the XRef list here, but we won't do anything
	-- afterwards, so doing so doesn't matter.
    end XRef;

end ARM_Syntax;

