with Ada.Unchecked_Deallocation,
     Ada.Strings.Fixed,
     Ada.Characters.Handling;
package body ARM_Syntax is

    --
    -- Ada reference manual formatter.
    --
    -- This package contains the database to collect the syntax summary and
    -- cross-reference.
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
    --  5/17/00 - RLB - Created package.
    --  5/24/00 - RLB - Updated to use revised Tabset.
    --  5/26/00 - RLB - Added a Tabset parameter.
    --  8/ 4/00 - RLB - Changed style to make font smaller (per Duff).
    --  8/16/00 - RLB - Added NoParaNum; removed junk space (which caused
    --			blank lines and paragraph numbers).
    --  9/26/00 - RLB - Revised to use SyntaxDisplay format to get more
    --			control over the formating of this section.
    --  9/27/00 - RLB - Revised XRef to decrease white space.

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
	Next : XRef_Ptr;
    end record;

    XRef_List : XRef_Ptr := null;
    XRef_Count : Natural := 0;

    procedure Free is new Ada.Unchecked_Deallocation (Rule_Type, Rule_Ptr);
    procedure Free is new Ada.Unchecked_Deallocation (XRef_Type, XRef_Ptr);
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


    procedure Add_Xref (
	Name : in String;
	Used_In : in String;
	Clause : in String) is
	-- Add a cross-reference entry.
	-- The item referenced is Name, and it is referenced in the production
	-- for Used_In, in Clause.
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
	        Format_Text ("@noparanum@;" &
		    Temp.Clause(1..Temp.Clause_Len) & ":" & Ascii.LF &
		    Temp.Rule.all & Ascii.LF & Ascii.LF,
		    Temp.Clause(1..Temp.Clause_Len));
	    else
	        Format_Text ("@noparanum@tabclear{}@tabset{" &
		    Ada.Strings.Fixed.Trim (Temp.Tabset, Ada.Strings.Right) &
		    "}" & Temp.Clause(1..Temp.Clause_Len) & ":" & Ascii.LF &
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
    procedure XRef is
	-- Output the fully formatted syntax cross-reference to the
	-- "Format_Text" routine. "Format_Text" allows all commands
	-- for the full formatter. (Text_Name is an identifying name
	-- for error messages).
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
	Format_Text ("@tabclear()@tabset(P4, P38)" & Ascii.LF, "Prefix");
	Format_Text ("@begin(twocol)" & Ascii.LF, "Prefix");
	Temp := XRef_List;
	while Temp /= null loop
	    if Last = null or else
		Last.Name (1..Last.Name_Len) /= Temp.Name (1..Temp.Name_Len) then
		-- New header:
	        Format_Text ("@noparanum@trailing@nt{" & Temp.Name (1..Temp.Name_Len) &
		     "}" & Ascii.LF,
		     Temp.Name (1..Temp.Name_Len) & " header");
		Last := Temp;
	    end if;
	    if Temp.Next = null or else
		Temp.Name (1..Temp.Name_Len) /= Temp.Next.Name (1..Temp.Next.Name_Len) then
		-- Last item of a set.
	        Format_Text ("@\@nt{" & Temp.Used_In(1..Temp.Used_In_Len) & "}@\" &
		    Temp.Clause(1..Temp.Clause_Len) & Ascii.LF & Ascii.LF,
	            Temp.Name (1..Temp.Name_Len) & " ref " & Temp.Clause(1..Temp.Clause_Len));
	    else -- Not an end item.
	        Format_Text ("@\@nt{" & Temp.Used_In(1..Temp.Used_In_Len) & "}@\" &
		    Temp.Clause(1..Temp.Clause_Len) & Ascii.LF,
	            Temp.Name (1..Temp.Name_Len) & " ref " & Temp.Clause(1..Temp.Clause_Len));
	    end if;
	    Temp := Temp.Next;
	end loop;
	Format_Text ("@end(twocol)" & Ascii.LF, "Suffix");
	Format_Text ("@end(syntaxdisplay)" & Ascii.LF, "Suffix");
    end XRef;

end ARM_Syntax;


