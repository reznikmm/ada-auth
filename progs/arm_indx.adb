with ARM_Output,
     ARM_Contents,
     Ada.Exceptions,
     Ada.Characters.Handling,
     Ada.Strings.Fixed,
     Ada.Text_IO,
     Ada.Calendar,
     Ada.Unchecked_Deallocation;
package body ARM_Index is

    --
    -- Ada reference manual formatter.
    --
    -- This package contains the routines to manage and generate the index.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2002, 2003, 2004, 2005 AXE Consultants.
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
    --  5/28/00 - RLB - Created package.
    --  8/ 7/00 - RLB - Improved sorting of entries.
    --  1/31/02 - RLB - Added missing with of Unchecked_Deallocation.
    --  4/11/03 - RLB - Added a hard space in the indexing routine, in order
    --			that the empty paragraph isn't ignored.
    --  9/09/04 - RLB - Removed unused junk noted by Stephen Leake.
    -- 10/28/05 - RLB - Added key reuse.
    -- 10/30/05 - RLB - Added subtype declaration.

    Next_Index_Key : Index_Key;

    type Term_Type;
    type Term_Ptr is access Term_Type;
    type Term_Type is record
	Clause : String (1..10);
	Clause_Len : Natural;
	Section_Number : ARM_Contents.Section_Number_Type;
	Clause_Number : Natural;
	Subclause_Number : Natural;
	Paragraph : String (1..10);
	Paragraph_Len : Natural;
	Term : String (1..60);
	Term_Len : Natural;
	Subterm : String (1..80);
	Subterm_Len : Natural;
	Kind : Index_Item_Kind_Type;
	Key : Index_Key;
	Next : Term_Ptr;
    end record;

    procedure Free is new Ada.Unchecked_Deallocation (Term_Type, Term_Ptr);

    Index_List : Term_Ptr;
    Term_Count : Natural := 0;

    procedure Create is
	-- Initialize this package.
    begin
	Index_List := null;
	Next_Index_Key := 1001;
	Term_Count := 0;
    end Create;


    procedure Destroy is
	-- Finalize this package; make sure the index is empty.
	TTemp : Term_Ptr;
    begin
	while Index_List /= null loop
	    TTemp := Index_List;
	    Index_List := TTemp.Next;
	    Free (TTemp);
	end loop;
    end Destroy;


    function Clean (Item : in String) return String is
	-- Remove any commands from Item. (Except for soft hyphens.)
	Result : String (1 .. Item'Length);
	Len : Natural := 0;
	In_Command : Boolean := False;
	Close_Char : Character := ' ';
    begin
	for I in Item'Range loop
	    if Item(I) = '@' then
		if I < Item'Last and then Item(I+1) = '!' then
		    -- Allow soft hyphens.
		    Len := Len + 1;
		    Result(Len) := Item(I);
		else
		    In_Command := True;
		    Close_Char := ' ';
		    -- Skip it.
		end if;
	    elsif In_Command then
		if Item(I) = '{' then
		    Close_Char := '}';
		    In_Command := False; -- Finished command header.
		elsif Item(I) = '[' then
		    Close_Char := ']';
		    In_Command := False;
		elsif Item(I) = '(' then
		    Close_Char := ')';
		    In_Command := False;
		elsif Item(I) = '<' then
		    Close_Char := '>';
		    In_Command := False;
		-- else skip character.
		end if;
	    elsif Close_Char /= ' ' and then
		Close_Char = Item(I) then
		-- Skip the close character.
		Close_Char := ' ';
	    elsif Item(I) = Ascii.LF then
		Len := Len + 1;
		Result(Len) := ' ';
	    else
		Len := Len + 1;
		Result(Len) := Item(I);
	    end if;
	end loop;
	return Result (1..Len);
    end Clean;


    function Get_Key return Index_Key is
	-- Returns a Key value to refer to one or more index entries
	-- (for a single entity).
	Temp : Index_Key := Next_Index_Key;
    begin
	Next_Index_Key := Next_Index_Key + 1;
	return Temp;
    end Get_Key;


    procedure Add_Reusing_Key (Term  : in String;
			       Subterm : in String := "";
			       Kind : in Index_Item_Kind_Type := Primary_Term;
			       Clause : in String := "";
			       Paragraph : in String := "";
	                       Key : in Index_Key) is
	-- Add an index reference to the index, (re)using the specified Key
	-- to refer to this index entry. Key must have previously
	-- returned by Add or Get_Key.
	Temp_Term : Term_Type;
	CTerm : constant String := Clean(Term);
	CSubterm : constant String := Clean(Subterm);
    begin
        Temp_Term.Kind := Kind;
        Ada.Strings.Fixed.Move (Target => Temp_Term.Term,
			        Source => CTerm,
			        Drop   => Ada.Strings.Error,
			        Pad    => ' ');
        Temp_Term.Term_Len := CTerm'Length;
	if Kind /= Primary_Term_and_Subterm and then
	   Kind /= Partial_Term_with_Subterm and then
	   Kind /= Child_Unit_Parent and then
	   Kind /= Declaration_in_Package and then
	   Kind /= Subdeclaration_in_Package and then
	   Kind /= Subtype_Declaration_in_Package and then
	   Kind /= See_Term and then
	   Kind /= See_Also_Term and then
	   Kind /= See_Other_Term and then
	   Kind /= See_Also_Other_Term then
	    if Subterm /= "" then
	        Ada.Exceptions.Raise_Exception (Not_Valid_Error'Identity,
		    "Subterm used in kind without subterm");
	    else
		Temp_Term.Subterm_Len := 0;
	    end if;
	else
	    Ada.Strings.Fixed.Move (Target => Temp_Term.Subterm,
				    Source => CSubterm,
				    Drop   => Ada.Strings.Error,
			            Pad    => ' ');
	    Temp_Term.Subterm_Len := CSubterm'Length;
	end if;
	if (Kind = See_Other_Term or else
	    Kind = See_Also_Other_Term) then
	    if Clause /= "" or else Paragraph /= "" then
	        Ada.Exceptions.Raise_Exception (Not_Valid_Error'Identity,
		    "Clause used in kind without reference");
	    else
		Temp_Term.Clause_Len := 0;
		Temp_Term.Paragraph_Len := 0;
	    end if;
	else
	    Ada.Strings.Fixed.Move (Target => Temp_Term.Clause,
				    Source => Clause,
				    Drop   => Ada.Strings.Error,
			            Pad    => ' ');
	    Temp_Term.Clause_Len := Clause'Length;
	    Ada.Strings.Fixed.Move (Target => Temp_Term.Paragraph,
				    Source => Paragraph,
				    Drop   => Ada.Strings.Error,
			            Pad    => ' ');
	    Temp_Term.Paragraph_Len := Paragraph'Length;
	    ARM_Contents.Make_Clause (Clause, Temp_Term.Section_Number,
					Temp_Term.Clause_Number,
					Temp_Term.Subclause_Number);

	end if;
	Temp_Term.Key := Key;

	Temp_Term.Next := Index_List;
	Index_List := new Term_Type'(Temp_Term);
	Term_Count := Term_Count + 1;
    exception
	when Ada.Strings.Length_Error =>
	    Ada.Text_IO.Put_Line ("**** Index doesn't fit: Term: " & CTerm &
				  " [Subterm: " & CSubterm & "]");
    end Add_Reusing_Key;


    procedure Add (Term  : in String;
		   Subterm : in String := "";
		   Kind : in Index_Item_Kind_Type := Primary_Term;
		   Clause : in String := "";
		   Paragraph : in String := "";
                   Key : out Index_Key) is
	-- Add an index reference to the index. Returns a Key value to
	-- refer to this index entry.
	-- Raises Not_Valid_Error if Subterm, Clause, or Paragraph is not
	-- empty when the kind does not use it.
    begin
	Key := Get_Key;
	Add_Reusing_Key (Term, Subterm, Kind, Clause, Paragraph, Key);
    end Add;


    procedure Generate_Index_Body (Output_Object : in out ARM_Output.Output_Type'Class;
				   Use_Paragraphs : in Boolean := True) is
	-- Generate the index body. (The section header has already been
	-- generated). References include paragraph numbers if Use_Paragraphs
	-- is true.
	-- Note: This should not leave us in a paragraph.

	Temp : Term_Ptr;
	Last : Term_Ptr := null;
	Items : array (1..Term_Count) of Term_Ptr;
	Keep_Set : Boolean := False;

	Start : Ada.Calendar.Time := Ada.Calendar.Clock;

	function To_Lower (A : in String) return String renames
	    Ada.Characters.Handling.To_Lower;
	function To_Lower (A : in Character) return Character renames
	    Ada.Characters.Handling.To_Lower;

	procedure Clause_Ref (Item : Term_Ptr) is
	    -- Generate a clause reference:
	begin
	    if Item.Clause_Len > 5 and then Item.Clause (1..5) = "Annex" then
	        -- Strip off the "Annex".
		if Use_Paragraphs and then Item.Paragraph_Len /= 0 then
	            ARM_Output.Index_Reference (Output_Object,
		        Text => Item.Clause (Item.Clause_Len) & '(' &
		           Item.Paragraph (1..Item.Paragraph_Len) & ')',
		        Index_Key => Item.Key,
		        Clause_Number => Item.Clause (1..Item.Clause_Len));
	        else
	            ARM_Output.Index_Reference (Output_Object,
		        Text => Item.Clause (Item.Clause_Len) & "",
		        Index_Key => Item.Key,
		        Clause_Number => Item.Clause (1..Item.Clause_Len));
		end if;
	    elsif Use_Paragraphs and then Item.Paragraph_Len /= 0 then
	        ARM_Output.Index_Reference (Output_Object,
		    Text => Item.Clause (1..Item.Clause_Len) & '(' &
		       Item.Paragraph (1..Item.Paragraph_Len) & ')',
		    Index_Key => Item.Key,
		    Clause_Number => Item.Clause (1..Item.Clause_Len));
	    else
	        ARM_Output.Index_Reference (Output_Object,
		    Text => Item.Clause (1..Item.Clause_Len),
		    Index_Key => Item.Key,
		    Clause_Number => Item.Clause (1..Item.Clause_Len));
	    end if;
	end Clause_Ref;

	procedure Italic_Text (Text : in String) is
	begin
	    ARM_Output.Text_Format (Output_Object,
		   Bold => False, Italic => True, Font => ARM_Output.Default,
		   Size => 0, Change => ARM_Output.None, Location => ARM_Output.Normal);
            ARM_Output.Ordinary_Text (Output_Object, Text);
	    ARM_Output.Text_Format (Output_Object,
		   Bold => False, Italic => False, Font => ARM_Output.Default,
		   Size => 0, Change => ARM_Output.None, Location => ARM_Output.Normal);
	end Italic_Text;

	procedure Term_Text (Text : in String) is
	    A_Soft_Hyphen : Natural := Ada.Strings.Fixed.Index (Text, "@!");
	begin
	    if A_Soft_Hyphen = 0 then
		ARM_Output.Ordinary_Text (Output_Object, Text);
	    else
		ARM_Output.Ordinary_Text (Output_Object, Text(Text'First .. A_Soft_Hyphen-1));
	        ARM_Output.Soft_Hyphen_Break (Output_Object);
		Term_Text (Text(A_Soft_Hyphen+2 .. Text'Last)); -- In case there is more than one soft hyphen.
	    end if;
	end Term_Text;

	procedure New_Kind (Item : Term_Ptr; Reset_Keep : in Boolean) is
	    -- Generate and item of a new kind. Note that the term has already
	    -- been generated (at some point).
	begin
	   case Item.Kind is
	        when Primary_Term =>
		    -- ** Must be first, so can't get here.
		    Italic_Text ("*SORT ERROR*");
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Item);

		when Partial_Term =>
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => Reset_Keep);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
	            ARM_Output.Ordinary_Character (Output_Object, '[');
		    Italic_Text ("partial");
	            ARM_Output.Ordinary_Character (Output_Object, ']');
	            ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Item);

	        when Primary_Term_and_Subterm =>
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => Reset_Keep);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    Term_Text (Item.Subterm (1..Item.Subterm_Len));
	            ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Item);

	        when Partial_Term_with_Subterm =>
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => Reset_Keep);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    Term_Text (Item.Subterm (1..Item.Subterm_Len));
	            --[The "partial" does not seem to appear on subterms.]
		    --ARM_Output.Hard_Space (Output_Object);
	            --ARM_Output.Ordinary_Character (Output_Object, '[');
		    --Italic_Text ("partial");
	            --ARM_Output.Ordinary_Character (Output_Object, ']');
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Item);

	        when Syntax_NT_Used =>
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => Reset_Keep);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    Italic_Text ("used");
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Item);

	        when Child_Unit_Parent =>
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => Reset_Keep);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    Italic_Text ("child of");
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Term_Text (Item.Subterm (1..Item.Subterm_Len));
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Item);

	        when Declaration_In_Package =>
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => Reset_Keep);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    Italic_Text ("in");
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Term_Text (Item.Subterm (1..Item.Subterm_Len));
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Item);

	        when SubDeclaration_In_Package =>
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => Reset_Keep);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    declare
			In_Loc : Natural :=
			    Ada.Strings.Fixed.Index (Item.Subterm (1..Item.Subterm_Len),
				" in ");
		    begin
			if In_Loc = 0 then
			    -- Weird, "in" not found.
			    Term_Text (Item.Subterm (1..Item.Subterm_Len));
			else
			    Term_Text (Item.Subterm (1 .. In_Loc));
			    Italic_Text ("in");
			    Term_Text (Item.Subterm (In_Loc+3 .. Item.Subterm_Len));
			end if;
		    end;
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Item);

	        when Subtype_Declaration_In_Package =>
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => Reset_Keep);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    Italic_Text ("in");
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Term_Text (Item.Subterm (1..Item.Subterm_Len));
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Item);

	        when See_Term =>
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => Reset_Keep);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    Italic_Text ("See");
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Term_Text (Item.Subterm (1..Item.Subterm_Len));
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Item);

	        when See_Also_Term =>
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => Reset_Keep);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    Italic_Text ("See also");
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Term_Text (Item.Subterm (1..Item.Subterm_Len));
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Item);

	        when See_Other_Term =>
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => Reset_Keep);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    Italic_Text ("See");
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Term_Text (Item.Subterm (1..Item.Subterm_Len));
		    -- No clause reference here.

	        when See_Also_Other_Term =>
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => Reset_Keep);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    Italic_Text ("See also");
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Term_Text (Item.Subterm (1..Item.Subterm_Len));
		    -- No clause reference here.
	    end case;
	end New_Kind;

	function Is_Last_for_Term (Item : in Term_Ptr) return Boolean is
	    -- Returns True if this is the last line for Item's Term.
 	begin
	    if Item.Next = null then
		return True;
	    elsif To_Lower (Item.Term (1..Item.Term_Len)) /= To_Lower (Item.Next.Term (1..Item.Next.Term_Len)) then
		-- The next item has a different term.
		return True;
	    elsif Temp.Kind /= Temp.Next.Kind then
		-- The next item has a different kind, so another line will
		-- be generated.
		return False;
	    elsif To_Lower (Item.Subterm (1..Item.Subterm_Len)) /= To_Lower (Item.Next.Subterm (1..Item.Next.Subterm_Len)) then
		-- The next item has a different subterm, so another line will
		-- be generated.
		return False;
	    else
		-- The following term will just add another clause reference.
		-- So we must look at the term following that:
		return Is_Last_for_Term (Item.Next);
	    end if;
	end Is_Last_for_Term;

    begin
	Ada.Text_IO.Put_Line ("  -- Start index sorting - " & Natural'Image(Term_Count) & " items.");

	-- Sort the items:

	-- Load the items:
	Temp := Index_List;
	for I in Items'range loop
	    Items(I) := Temp;
	    Temp := Temp.Next;
	end loop;

	-- Sort the items array:
	declare

	    function "<" (Left, Right : Term_Ptr) return Boolean is
		function To_Lower (A : in String) return String renames
		    Ada.Characters.Handling.To_Lower;
		use type ARM_Contents.Section_Number_Type;

		type Compare_Result is (Less, Greater, Equal);
		function Compare (Left, Right : in String) return Compare_Result is
		    -- By binding the arguments, we cut the heap usage by
		    -- nearly half, and thus the runtime of the compare routine.
		begin
		    if Left < Right then
			return Less;
		    elsif Left > Right then
			return Greater;
		    else
			return Equal;
		    end if;
		end Compare;
	    begin
		-- We sort first on "Term", then on "Kind", then on "Subterm",
		-- then on "Clause", and finally on "Paragraph".
		case Compare (To_Lower (Left.Term (1..Left.Term_Len)), To_Lower (Right.Term (1..Right.Term_Len))) is
		    when Less => return True;
		    when Greater => return False;
		    when Equal => null; -- Continue to next compare.
		end case;
		-- Partial_Term_with_Subterm and Primary_Term_and_Subterm
		-- look identical, so they should compare equal for this
		-- purpose:
		if Left.Kind = Right.Kind or else
		  (Left.Kind = Partial_Term_with_Subterm and then
		   Right.Kind = Primary_Term_and_Subterm) or else
		   (Left.Kind = Primary_Term_and_Subterm and then
		   Right.Kind = Partial_Term_with_Subterm) then
		    null; -- Continue to next compare.
		elsif Left.Kind < Right.Kind then
		    return True;
		else --if Left.Kind > Right.Kind then
		    return False;
		end if;
		case Compare (To_Lower (Left.Subterm (1..Left.Subterm_Len)), To_Lower (Right.Subterm (1..Right.Subterm_Len))) is
		    when Less => return True;
		    when Greater => return False;
		    when Equal => null; -- Continue to next compare.
		end case;
		-- Note: We use the numbers, because the references don't
		-- sort right (11.1 comes before 2.8, etc.)
		if Left.Section_Number < Right.Section_Number then
		    return True;
		elsif Left.Section_Number > Right.Section_Number then
		    return False;
		elsif Left.Clause_Number < Right.Clause_Number then
		    return True;
		elsif Left.Clause_Number > Right.Clause_Number then
		    return False;
		elsif Left.Subclause_Number < Right.Subclause_Number then
		    return True;
		elsif Left.Subclause_Number > Right.Subclause_Number then
		    return False;
		else
		    -- Make sure that single digit paragraph numbers sort before
		    -- multiple digit ones:
		    if Left.Paragraph_Len <= 1 or else Left.Paragraph(2) = '.' or else Left.Paragraph(2) = '/' then
			-- Single digit number:
		        if Right.Paragraph_Len <= 1 or else Right.Paragraph(2) = '.' or else Right.Paragraph(2) = '/' then
			    -- Single digit number, normal compare:
			    return Left.Paragraph (1..Left.Paragraph_Len) < Right.Paragraph (1..Right.Paragraph_Len);
			else
			    -- Single digit is always less than multiple digits:
			    return True;
			end if;
		    else -- Not single digit number:
		        if Right.Paragraph_Len <= 1 or else Right.Paragraph(2) = '.' or else Right.Paragraph(2) = '/' then
			    -- Single digit number, always less than multiple digits:
			    return False;
			else
			    -- Else both multiple, use normal compare:
			    return Left.Paragraph (1..Left.Paragraph_Len) < Right.Paragraph (1..Right.Paragraph_Len);
			end if;
		    end if;
		end if;
	    end "<";

	    --procedure Final_Sort_Slice (Left, Right : in Natural) is
	    --	-- Use an insertion sort to sort the slice between Left and Right.
	    --    Working_Left : Natural;  -- Working Left sorting stop
	    --	Temp : Term_Ptr;
	    --begin
	    --    for Working_Right In Left+1 .. Right loop -- Right sorting stop
	    --	    Temp := Items(Working_Right);
	    --	    Working_Left := Working_Right - 1;
	    --	    while Temp < Items(Working_Left) loop -- Switch items
	    --	        Items(Working_Left + 1) := Items(Working_Left);
	    --	        Working_Left := Working_Left - 1;
	    --	        exit when Working_Left = 0;
	    --	    end loop;
	    --        Items(Working_Left + 1) := Temp;
	    --    end loop;
	    --end Final_Sort_Slice;


	    procedure Partition_Sort_Slice (Start_Index, End_Index : Natural) is
	    	-- Use quicksort partition sort to sort the slice between
		-- Start_Index and End_Index.
		Temp_Item : Term_Ptr;
	    begin
		case ((End_Index - Start_Index) + 1) is
		    when 0 | 1 => null; -- A single element is obviously sorted (trivially).
		    when 2 =>
			-- Unrolled Insertion Sort.
			if Items(Start_Index+1) < Items(Start_Index) then
			    -- Move the record down.
			    Temp_Item := Items(Start_Index+1);
			    Items(Start_Index+1) := Items(Start_Index  );
			    Items(Start_Index  ) := Temp_Item; -- Put at beginning.
			-- else Doesn't need to move.
			end if;
		    when 3 =>
			-- Unrolled Insertion Sort.
			if Items(Start_Index+1) < Items(Start_Index) then
			    -- Move the record down.
			    Temp_Item := Items(Start_Index+1);
			    Items(Start_Index+1) := Items(Start_Index  );
			    Items(Start_Index  ) := Temp_Item; -- Put at beginning.
			-- else Doesn't need to move.
			end if;
			if Items(Start_Index+2) < Items(Start_Index+1) then
			    -- Move the record down.
			    Temp_Item := Items(Start_Index+2);
			    Items(Start_Index+2) := Items(Start_Index+1);
			    if Temp_Item < Items(Start_Index) then
			        -- Move the record down.
				Items(Start_Index+1) := Items(Start_Index);
				Items(Start_Index) := Temp_Item; -- Put at beginning.
			    else
				-- Put the record here.
				Items(Start_Index+1) := Temp_Item;
			    end if;
			-- else Doesn't need to move.
			end if;
		    when 4 =>
			-- Unrolled Insertion Sort.
			if Items(Start_Index+1) < Items(Start_Index) then
			    -- Move the record down.
			    Temp_Item := Items(Start_Index+1);
			    Items(Start_Index+1) := Items(Start_Index  );
			    Items(Start_Index  ) := Temp_Item; -- Put at beginning.
			-- else Doesn't need to move.
			end if;
			if Items(Start_Index+2) < Items(Start_Index+1) then
			    -- Move the record down.
			    Temp_Item := Items(Start_Index+2);
			    Items(Start_Index+2) := Items(Start_Index+1);
			    if Temp_Item < Items(Start_Index) then
			        -- Move the record down.
				Items(Start_Index+1) := Items(Start_Index);
				Items(Start_Index) := Temp_Item; -- Put at beginning.
			    else
				-- Put the record here.
				Items(Start_Index+1) := Temp_Item;
			    end if;
			-- else Doesn't need to move.
			end if;
			if Items(Start_Index+3) < Items(Start_Index+2) then
			    -- Move the record down.
			    Temp_Item := Items(Start_Index+3);
			    Items(Start_Index+3) := Items(Start_Index+2);
			    if Temp_Item < Items(Start_Index+1) then
				-- Move the record down.
				Items(Start_Index+2) := Items(Start_Index+1);
				if Temp_Item < Items(Start_Index) then
				    -- Move the record down.
				    Items(Start_Index+1) := Items(Start_Index);
				    Items(Start_Index) := Temp_Item; -- Put at beginning.
				else -- Put the record here.
				    Items(Start_Index+1) := Temp_Item;
				end if;
			    else
				-- Put the record here.
				Items(Start_Index+2) := Temp_Item;
			    end if;
			-- else Don't move the record.
			end if;
		    when others => -- Longer partitions, quicksort.
			declare
			    Left_Index, Right_Index : Natural;
			    Pivot_Item : Term_Ptr;
			begin
			    -- Split into partitions, and sort them.
			    Left_Index := Start_Index;
			    Right_Index := End_Index;
			    -- Use the middle element for the pivot, in case the items are
			    -- somewhat sorted.
			    Pivot_Item := Items ((End_Index - Start_Index) / 2 + Start_Index);
			    loop
				loop
				    exit when not (Items(Left_Index) < Pivot_Item); -- >=
				    Left_Index := Left_Index + 1;
				end loop;
				loop
				    exit when not (Pivot_Item < Items(Right_Index));
				    Right_Index := Right_Index - 1;
				end loop;
				if Left_Index <= Right_Index then
				    if Left_Index < Right_Index then
					Temp_Item := Items(Left_Index);
					Items(Left_Index) := Items(Right_Index);
					Items(Right_Index) := Temp_Item;
				    end if;
				    Left_Index  := Left_Index + 1;
				    Right_Index := Right_Index - 1;
				end if;
				exit when Left_Index > Right_Index;
			    end loop; -- Repeat Loop
			    -- Recursive calls on partitions.
			    Partition_Sort_Slice (Left_Index, End_Index);
			    Partition_Sort_Slice (Start_Index, Right_Index);
			end;
		end case;
	    end Partition_Sort_Slice;

	begin
	    -- Use quicksort to handle most of the array, then
	    -- insertion sort for the final slices.
	    Partition_Sort_Slice (Items'First, Items'Last);
	end;

        -- Relink the items in the sorted order:
        for I in Items'First .. Items'Last - 1 loop
	    Items(I).Next := Items(I+1);
        end loop;
        if Items'Length > 0 then
	    Items(Items'Last).Next := null;
	    Index_List := Items(1);
        else
	    Index_List := null;
        end if;

	Ada.Text_IO.Put_Line ("  -- Finish index sorting - " & Duration'Image (
	    Ada.Calendar."-" (Ada.Calendar.Clock, Start)) & " secs.");

	ARM_Output.Start_Paragraph (Output_Object, ARM_Output.Index, Number => "", No_Breaks => True);
	Keep_Set := False;

	Temp := Index_List;
	while Temp /= null loop
	    -- First, check if we've changed to a new group:
	    if Last /= null and then
		To_Lower (Last.Term(1)) /= To_Lower(Temp.Term(1)) then
		-- The first character has changed.
		ARM_Output.End_Paragraph (Output_Object);
		ARM_Output.Start_Paragraph (Output_Object, ARM_Output.Index, Number => "");
		Keep_Set := False;
		if To_Lower(Temp.Term(1)) in 'a' .. 'z' then
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => False);
		    ARM_Output.Text_Format (Output_Object,
			   Bold => True, Italic => False, Font => ARM_Output.Default,
			   Size => 2, Change => ARM_Output.None, Location => ARM_Output.Normal);
		    ARM_Output.Ordinary_Character (Output_Object,
			Ada.Characters.Handling.To_Upper(Temp.Term(1)));
		    ARM_Output.Text_Format (Output_Object,
			   Bold => False, Italic => False, Font => ARM_Output.Default,
			   Size => 0, Change => ARM_Output.None, Location => ARM_Output.Normal);
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => False);
		else
		    ARM_Output.Hard_Space (Output_Object);
			-- So the paragraph isn't empty (which causes it to be
			-- ignored in HTML).
		end if;
	    end if;

	    if Last = null or else
		To_Lower(Last.Term (1..Last.Term_Len)) /= To_Lower(Temp.Term (1..Temp.Term_Len)) then
		-- New term: (Note that we ignore case differences here. Perhaps
		-- there ought to be a warning?)
		if Last /= null then
		    ARM_Output.End_Paragraph (Output_Object);
		    if Temp.Kind = Primary_Term then
		        ARM_Output.Start_Paragraph (Output_Object, ARM_Output.Index, Number => "",
					            No_Breaks => True);
			Keep_Set := False;
		    else -- The item has at least two lines; keep them together.
		        ARM_Output.Start_Paragraph (Output_Object, ARM_Output.Index, Number => "",
					            No_Breaks => True, Keep_with_Next => True);
			Keep_Set := True;
		    end if;
		end if;
		if Temp.Kind /= Subtype_Declaration_in_Package then
	            Term_Text (Temp.Term (1..Temp.Term_Len));
		else
		    declare
			Of_Loc : Natural :=
			    Ada.Strings.Fixed.Index (Temp.Term (1..Temp.Term_Len),
				" subtype of ");
		    begin
			if Of_Loc = 0 then
			    -- Weird, "subtype of" not found.
			    Term_Text (Temp.Term (1..Temp.Term_Len));
			else
			    Term_Text (Temp.Term (1 .. Of_Loc));
			    Italic_Text ("subtype of");
			    Term_Text (Temp.Term (Of_Loc+11 .. Temp.Term_Len));
			end if;
		    end;
		end if;
		if Temp.Kind = Primary_Term then
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
	            ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Temp);
		else
		    if Is_Last_for_Term(Temp) then
		        -- Last (only) item of this term, always clear Keep:
	                New_Kind (Temp, Reset_Keep => True);
		        Keep_Set := False;
		    else -- Leave keep set:
		        New_Kind (Temp, Reset_Keep => False);
		    end if;
		end if;
	    elsif Last.Kind /= Temp.Kind then
	        New_Kind (Temp, Reset_Keep => Keep_Set);
		Keep_Set := False;
	    elsif (Temp.Kind = Primary_Term_and_Subterm or else
		   Temp.Kind = Partial_Term_with_Subterm or else
		   Temp.Kind = Syntax_NT_Used or else
		   Temp.Kind = Child_Unit_Parent or else
		   Temp.Kind = Declaration_in_Package or else
		   Temp.Kind = SubDeclaration_in_Package or else
		   Temp.Kind = Subtype_Declaration_in_Package) and then
		Last.Subterm (1..Last.Subterm_Len) /= Temp.Subterm (1..Temp.Subterm_Len) then
	        New_Kind (Temp, Reset_Keep => Keep_Set);
		Keep_Set := False;
	    elsif (Temp.Kind = See_Other_Term or else
		   Temp.Kind = See_Also_Other_Term) then
		-- Just add the next reference.
	        ARM_Output.Ordinary_Character (Output_Object, ',');
	        ARM_Output.Ordinary_Character (Output_Object, ' ');
	        ARM_Output.Ordinary_Text (Output_Object,
		    Temp.Subterm (1..Temp.Subterm_Len));
		-- No clause references here.
	    elsif (Temp.Kind = See_Term or else
		   Temp.Kind = See_Also_Term) then
		-- Just add the next reference. We'll just use New_Kind for
		-- this, so we don't get the formats slightly different.
	        New_Kind (Temp, Reset_Keep => Keep_Set);
		Keep_Set := False;
	    elsif Last.Clause (1..Last.Clause_Len) = Temp.Clause (1..Temp.Clause_Len) and then
		  Last.Paragraph (1..Last.Paragraph_Len) = Temp.Paragraph (1..Temp.Paragraph_Len) then
		-- The reference and everything else is the same, so just
		-- forget this item.
		null;
	    else
		-- Just add the next clause.
	        ARM_Output.Ordinary_Character (Output_Object, ',');
	        ARM_Output.Ordinary_Character (Output_Object, ' ');
		Clause_Ref (Temp);
	    end if;
	    Last := Temp;
	    Temp := Temp.Next;
	end loop;

	ARM_Output.End_Paragraph (Output_Object);

    end Generate_Index_Body;

end ARM_Index;
