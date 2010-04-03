with ARM_Output;
with ARM_Index;
with ARM_Contents;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO; -- ** Temp.
package body ARM_Subindex is

    --
    -- Ada reference manual formatter.
    --
    -- This package contains the database to store subindex items for
    -- non-normative appendixes.
    --
    -- ---------------------------------------
    -- Copyright 2005, 2006, 2007  AXE Consultants.
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
    -- 10/28/05 - RLB - Created package.
    --  8/ 4/06 - RLB - Fixed problems if unit was missing.
    --  9/22/06 - RLB - Changed to use Clause_Number_Type.
    --  2/13/07 - RLB - Changed Start_Paragraph to use explicit indents.
    -- 12/19/07 - RLB - Revised Text_Format calls.

    type String_Ptr is access String;
    type Item is record
	Next : Item_List;
	Entity        : String_Ptr := null;
	From_Unit     : String_Ptr := null;
	Kind	      : Subindex_Item_Kind_Type := Top_Level;
	Clause	      : String (1..10);
	Clause_Len    : Natural;
	Clause_Number : ARM_Contents.Clause_Number_Type;
	Paragraph     : String (1..10);
	Paragraph_Len : Natural;
        Key	      : ARM_Index.Index_Key;
    end record;

    procedure Free is new Ada.Unchecked_Deallocation (Item, Item_List);
    procedure Free is new Ada.Unchecked_Deallocation (String, String_Ptr);


    procedure Create (Subindex_Object : in out Subindex_Type) is
	-- Initialize a Subindex object.
    begin
	Subindex_Object.Is_Valid := True;
	Subindex_Object.List := null;
	Subindex_Object.Item_Count := 0;
    end Create;


    procedure Destroy (Subindex_Object : in out Subindex_Type) is
	-- Destroy a Subindex object, freeing any resources used.
	Temp : Item_List;
    begin
	if not Subindex_Object.Is_Valid then
	    raise Not_Valid_Error;
	end if;
	while Subindex_Object.List /= null loop
	    Temp := Subindex_Object.List;
	    Subindex_Object.List := Temp.Next;
	    Free (Temp.Entity);
	    Free (Temp.From_Unit);
	    Free (Temp);
	end loop;
	Subindex_Object.Is_Valid := False;
    end Destroy;


    procedure Insert (Subindex_Object : in out Subindex_Type;
		      Entity          : in String;
		      From_Unit       : in String := "";
		      Kind	      : in Subindex_Item_Kind_Type := Top_Level;
		      Clause	      : in String := "";
		      Paragraph	      : in String := "";
                      Key	      : in ARM_Index.Index_Key) is
	-- Insert an item into the Subindex object.
	-- The Key must be one returned by ARM_Index.Add or ARM_Index.Get_Key.
	-- Raises Not_Valid_Error if From_Unit, Clause, or Paragraph is not
	-- empty when the kind does not use it; or if From_Unit is empty
	-- when the kind requires it.
	Temp_Item : Item;
    begin
	if not Subindex_Object.Is_Valid then
	    raise Not_Valid_Error;
	end if;
	if Kind = Top_Level and then From_Unit'Length /= 0 then
	    raise Not_Valid_Error; -- No subterm here.
	end if;
        Ada.Strings.Fixed.Move (Target => Temp_Item.Clause,
			        Source => Clause,
			        Drop   => Ada.Strings.Error,
			        Pad    => ' ');
        Temp_Item.Clause_Len := Clause'Length;
        Ada.Strings.Fixed.Move (Target => Temp_Item.Paragraph,
			        Source => Paragraph,
			        Drop   => Ada.Strings.Error,
			        Pad    => ' ');
        Temp_Item.Paragraph_Len := Paragraph'Length;
        ARM_Contents.Make_Clause (Clause, Temp_Item.Clause_Number);
	Temp_Item.Kind := Kind;
	Temp_Item.Key  := Key;
	Temp_Item.Entity := new String'(Entity);
	if From_Unit'Length /= 0 then
	    Temp_Item.From_Unit := new String'(From_Unit);
	else
	    if Kind = In_Unit or else
	       Kind = Child_of_Parent or else
	       Kind = Subtype_In_Unit or else
	       Kind = Description_In_Unit or else
	       Kind = Raised_Belonging_to_Unit then
		-- There must be a unit here.
		raise Not_Valid_Error;
	    end if;
	end if;
	Temp_Item.Next := Subindex_Object.List;
        Subindex_Object.List := new Item'(Temp_Item);
	Subindex_Object.Item_Count := Subindex_Object.Item_Count + 1;
    end Insert;


    procedure Write_Subindex (
		Subindex_Object : in out Subindex_Type;
		Output_Object   : in out ARM_Output.Output_Type'Class;
		Use_Paragraphs : in Boolean := True;
		Minimize_Lines : in Boolean := False) is
	-- Generate the given subindex to Output_Object.
	-- References include paragraph numbers if Use_Paragraphs is true.
	-- Try to minimize lines if Minimize_Lines is True.
	-- Note: This should not leave us in a paragraph.
	Temp : Item_List;
	Last : Item_List := null;
	Items : array (1..Subindex_Object.Item_Count) of Item_List;

	Keep_Set : Boolean := False;

	CHARS_ON_SINGLE_LINE : constant := 38;
	    -- The number of characters allowed on a single line if
	    -- "Minimize_Lines" is True.

	function To_Lower (A : in String) return String renames
	    Ada.Characters.Handling.To_Lower;
	function To_Lower (A : in Character) return Character renames
	    Ada.Characters.Handling.To_Lower;


        function "<" (Left, Right : Item_List) return Boolean is
        use type ARM_Contents.Clause_Number_Type;

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
	    -- We sort first on "Entity", then on "Kind", then on "From_Unit",
	    -- then on "Clause", and finally on "Paragraph".
	    case Compare (To_Lower (Left.Entity.all), To_Lower (Right.Entity.all)) is
	        when Less => return True;
	        when Greater => return False;
	        when Equal => null; -- Continue to next compare.
	    end case;
	    if Left.Kind = Right.Kind then
	        null; -- Continue to next compare.
	    elsif Left.Kind < Right.Kind then
	        return True;
	    else --if Left.Kind > Right.Kind then
	        return False;
	    end if;
	    if Left.From_Unit = null then
		null; -- No string to compare (Kind=Top_Level)
	    else
		case Compare (To_Lower (Left.From_Unit.all), To_Lower (Right.From_Unit.all)) is
		    when Less => return True;
		    when Greater => return False;
		    when Equal => null; -- Continue to next compare.
		end case;
	    end if;

	    -- Note: We use the numbers, because the references don't
	    -- sort right (11.1 comes before 2.8, etc.)
	    if Left.Clause_Number < Right.Clause_Number then
	        return True;
	    elsif Left.Clause_Number = Right.Clause_Number then
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
	    else -- Left.Clause_Number > Right.Clause_Number then
		return False;
	    end if;
        end "<";


        procedure Partition_Sort_Slice (Start_Index, End_Index : Natural) is
	    -- Use quicksort partition sort to sort the slice between
	    -- Start_Index and End_Index.
	    Temp_Item : Item_List;
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
		        Pivot_Item : Item_List;
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


	procedure Clause_Ref (Item : Item_List) is
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
		   Format => (Bold => False, Italic => True,
			      Font => ARM_Output.Default,
		   	      Size => 0, Color => ARM_Output.Default,
			      Change => ARM_Output.None,
			      Version | Added_Version => '0',
			      Location => ARM_Output.Normal));
            ARM_Output.Ordinary_Text (Output_Object, Text);
	    ARM_Output.Text_Format (Output_Object,
		   Format => (Bold => False, Italic => False,
			      Font => ARM_Output.Default,
		   	      Size => 0, Color => ARM_Output.Default,
			      Change => ARM_Output.None,
			      Version | Added_Version => '0',
			      Location => ARM_Output.Normal));
	end Italic_Text;


	procedure New_Kind (Item : Item_List; Reset_Keep : in Boolean;
	    Last_Had_Same_Unit : Boolean := False) is
	    -- Generate and item of a new kind. Note that the term has already
	    -- been generated (at some point).
	begin
	   case Item.Kind is
	        when Top_Level =>
		    -- ** Must be first, so can't get here.
		    Italic_Text ("*SORT ERROR*");
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Item);

	        when In_Unit =>
		    if Last_Had_Same_Unit then
		        -- ** Must be before any other items with a unit,
			-- so can't get here.
		        Italic_Text ("*SORT ERROR*");
		    end if;
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => Reset_Keep);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    if Item.From_Unit /= null then
		        Italic_Text ("in");
		        ARM_Output.Ordinary_Character (Output_Object, ' ');
		        Term_Text (Item.From_Unit.all);
		    -- else shouldn't be empty, but why crash?
		    end if;
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Item);

	        when Child_of_Parent =>
		    if Last_Had_Same_Unit then
		        -- ** Must be before any other items with a unit,
			-- so can't get here.
		        Italic_Text ("*SORT ERROR*");
		    end if;
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => Reset_Keep);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    if Item.From_Unit /= null then
		        Italic_Text ("child of");
		        ARM_Output.Ordinary_Character (Output_Object, ' ');
		        Term_Text (Item.From_Unit.all);
		    -- else shouldn't be empty, but why crash?
		    end if;
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Item);

	        when Subtype_In_Unit =>
		    if Last_Had_Same_Unit then
		        -- ** Must be before any other items with a unit,
			-- so can't get here.
		        Italic_Text ("*SORT ERROR*");
		    end if;
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => Reset_Keep);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    if Item.From_Unit /= null then
		        Italic_Text ("in");
		        ARM_Output.Ordinary_Character (Output_Object, ' ');
		        Term_Text (Item.From_Unit.all);
		    -- else shouldn't be empty, but why crash?
		    end if;
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Item);

	        when Description_In_Unit =>
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => Reset_Keep);
		    if not Last_Had_Same_Unit then
		        ARM_Output.Hard_Space (Output_Object);
		        ARM_Output.Hard_Space (Output_Object);
		        ARM_Output.Hard_Space (Output_Object);
		        if Item.From_Unit /= null then
		            Italic_Text ("in");
		            ARM_Output.Ordinary_Character (Output_Object, ' ');
		            Term_Text (Item.From_Unit.all);
		        -- else shouldn't be empty, but why crash?
			end if;
		        ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => False);
		    end if;
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    Italic_Text ("description");
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Item);

	        when Raised_Belonging_to_Unit =>
		    ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => Reset_Keep);
		    if not Last_Had_Same_Unit then
		        ARM_Output.Hard_Space (Output_Object);
		        ARM_Output.Hard_Space (Output_Object);
		        ARM_Output.Hard_Space (Output_Object);
		        if Item.From_Unit /= null then
		            Italic_Text ("in");
		            ARM_Output.Ordinary_Character (Output_Object, ' ');
		            Term_Text (Item.From_Unit.all);
		        -- else shouldn't be empty, but why crash?
			end if;
		        ARM_Output.Index_Line_Break (Output_Object, Clear_Keep_with_Next => False);
		    end if;
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
		    Italic_Text ("raised");
		    ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Item);

	    end case;
	end New_Kind;

	function Is_Last_for_Entity (Item : in Item_List) return Boolean is
	    -- Returns True if this is the last line for Item's Entity.
 	begin
--Ada.Text_IO.Put_Line("Enter Is_Last");
	    if Item.Next = null then
--Ada.Text_IO.Put_Line("  No follower (True)");
		return True;
	    elsif To_Lower (Item.Entity.all) /= To_Lower(Item.Next.Entity.all) then
		-- The next item has a different entity.
--Ada.Text_IO.Put_Line("  Different entity (True)");
		return True;
	    elsif Item.Kind /= Item.Next.Kind then
		-- The next item has a different kind, so another line will
		-- be generated.
--Ada.Text_IO.Put_Line("  Different kind (False)");
		return False;
	    elsif Item.Kind /= Top_Level and then
		  Item.From_Unit.all /= Item.Next.From_Unit.all then
		-- The next item has a different unit, so another line will
		-- be generated.
--Ada.Text_IO.Put_Line("  Different unit (False)");
		return False;
	    else
		-- The following entity will just add another clause reference.
		-- So we must look at the entity following that:
--Ada.Text_IO.Put_Line("  Recurse");
		return Is_Last_for_Entity (Item.Next);
	    end if;
	end Is_Last_for_Entity;

    begin
        if not Subindex_Object.Is_Valid then
            raise Not_Valid_Error;
        end if;

	Keep_Set := False;

	-- Load the items:
	Temp := Subindex_Object.List;
	for I in Items'range loop
	    Items(I) := Temp;
	    Temp := Temp.Next;
	end loop;

	-- Sort the items:
	Partition_Sort_Slice (Items'First, Items'Last);

        -- Relink the items in the sorted order:
        for I in Items'First .. Items'Last - 1 loop
	    Items(I).Next := Items(I+1);
        end loop;
        if Items'Length > 0 then
	    Items(Items'Last).Next := null;
	    Subindex_Object.List := Items(1);
        else
	    Subindex_Object.List := null;
        end if;

	ARM_Output.Start_Paragraph (Output_Object, ARM_Output.Index,
		Indent => 0, Number => "", No_Breaks => True);

	Temp := Subindex_Object.List;
	while Temp /= null loop
	    -- First, check for the new entity:
	    if Last = null or else
		To_Lower(Last.Entity.all) /= To_Lower(Temp.Entity.all) then
		-- New term: (Note that we ignore case differences here.
		-- Perhaps there ought to be a warning?)
		if Last /= null then
		    ARM_Output.End_Paragraph (Output_Object);
		    if Temp.Kind = Top_Level then
		        ARM_Output.Start_Paragraph (Output_Object, ARM_Output.Index,
						    Indent => 0, Number => "",
					            No_Breaks => True);
			Keep_Set := False;
--Ada.Text_IO.Put_Line("New Item: Entity=" & Temp.Entity.all &
--" Kind=" & Subindex_Item_Kind_Type'Image(Temp.Kind) &
--" Keep_Set=" & Boolean'Image(Keep_Set));
		    elsif Minimize_Lines and then
		          Temp.Kind = In_Unit and then
		          Temp.Entity'Length + 4 + Temp.From_Unit'Length < CHARS_ON_SINGLE_LINE then
			-- Write as a single line.
		        ARM_Output.Start_Paragraph (Output_Object, ARM_Output.Index,
						    Indent => 0, Number => "",
					            No_Breaks => True);
			Keep_Set := False;
--Ada.Text_IO.Put_Line("New Item: Entity=" & Temp.Entity.all &
--" Kind=" & Subindex_Item_Kind_Type'Image(Temp.Kind) &
--" From_Unit=" & Temp.From_Unit.all & " Keep_Set=" & Boolean'Image(Keep_Set));
		    else -- The item has at least two lines; keep them together.
		        ARM_Output.Start_Paragraph (Output_Object, ARM_Output.Index,
						    Indent => 0, Number => "",
					            No_Breaks => True, Keep_with_Next => True);
			Keep_Set := True;
--Ada.Text_IO.Put_Line("New Item: Entity=" & Temp.Entity.all &
--" Kind=" & Subindex_Item_Kind_Type'Image(Temp.Kind) &
--" From_Unit=" & Temp.From_Unit.all & " Keep_Set=" & Boolean'Image(Keep_Set));
		    end if;
		end if;
		if Temp.Kind /= Subtype_In_Unit then
		    Term_Text (Temp.Entity.all);
		else
		    declare
			Of_Loc : Natural :=
			    Ada.Strings.Fixed.Index (Temp.Entity.all,
				" subtype of ");
		    begin
			if Of_Loc = 0 then
			    -- Weird, "subtype of" not found.
			    Term_Text (Temp.Entity.all);
			else
			    Term_Text (Temp.Entity (Temp.Entity'First .. Of_Loc));
			    Italic_Text ("subtype of");
			    Term_Text (Temp.Entity (Of_Loc+11 .. Temp.Entity'Last));
			end if;
		    end;
		end if;
		if Temp.Kind = Top_Level then
		    ARM_Output.Hard_Space (Output_Object);
		    ARM_Output.Hard_Space (Output_Object);
	            ARM_Output.Ordinary_Character (Output_Object, ' ');
		    Clause_Ref (Temp);
		else
		    if Is_Last_for_Entity (Temp) then
		        -- Last (only) item of this term, always clear Keep:
--Ada.Text_IO.Put_Line("Only New Item: Entity=" & Temp.Entity.all &
--" Kind=" & Subindex_Item_Kind_Type'Image(Temp.Kind) &
--" From_Unit=" & Temp.From_Unit.all & " Keep_Set=" & Boolean'Image(Keep_Set));
			if Minimize_Lines and then
			   Temp.Kind = In_Unit and then
			    Temp.Entity'Length + 4 + Temp.From_Unit'Length < CHARS_ON_SINGLE_LINE then
			    -- Write this as a single line:
			    ARM_Output.Ordinary_Character (Output_Object, ' ');
			    Italic_Text ("in");
			    ARM_Output.Ordinary_Character (Output_Object, ' ');
			    Term_Text (Temp.From_Unit.all);
			    ARM_Output.Hard_Space (Output_Object);
			    ARM_Output.Hard_Space (Output_Object);
			    ARM_Output.Ordinary_Character (Output_Object, ' ');
			    Clause_Ref (Temp);
			else
	                    New_Kind (Temp, Reset_Keep => True, Last_Had_Same_Unit => False);
		            Keep_Set := False;
			end if;
		    else -- Leave keep set:
--Ada.Text_IO.Put_Line("More New Item: Entity=" & Temp.Entity.all &
--" Kind=" & Subindex_Item_Kind_Type'Image(Temp.Kind) &
--" From_Unit=" & Temp.From_Unit.all & " Keep_Set=" & Boolean'Image(Keep_Set));
		        New_Kind (Temp, Reset_Keep => False, Last_Had_Same_Unit => False);
		    end if;
		end if;
	    elsif Last.Kind /= Temp.Kind then
		If Last.Kind /= Top_Level and then
		   Temp.Kind /= Top_Level and then
		   Last.From_Unit.all = Temp.From_Unit.all then
--Ada.Text_IO.Put_Line("New Kind, same unit: Entity=" & Temp.Entity.all &
--" Kind=" & Subindex_Item_Kind_Type'Image(Temp.Kind) &
--" From_Unit=" & Temp.From_Unit.all & " Keep_Set=" & Boolean'Image(Keep_Set));
	            New_Kind (Temp, Reset_Keep => Keep_Set, Last_Had_Same_Unit => True);
		else
--if Temp.Kind /= Top_Level then
--Ada.Text_IO.Put_Line("New Kind: Entity=" & Temp.Entity.all &
--" Kind=" & Subindex_Item_Kind_Type'Image(Temp.Kind) &
--" From_Unit=" & Temp.From_Unit.all & " Keep_Set=" & Boolean'Image(Keep_Set));
--else
--Ada.Text_IO.Put_Line("New Kind: Entity=" & Temp.Entity.all &
--" Kind=" & Subindex_Item_Kind_Type'Image(Temp.Kind) &
--" Keep_Set=" & Boolean'Image(Keep_Set));
--end if;
	            New_Kind (Temp, Reset_Keep => Keep_Set, Last_Had_Same_Unit => False);
		end if;
		Keep_Set := False;
	    elsif (Temp.Kind = In_Unit or else
		   Temp.Kind = Child_of_Parent or else
		   Temp.Kind = Subtype_In_Unit or else
		   Temp.Kind = Description_In_Unit or else
		   Temp.Kind = Raised_Belonging_to_Unit) and then
		Last.From_Unit.all /= Temp.From_Unit.all then
--Ada.Text_IO.Put_Line("Same Kind, new unit: Entity=" & Temp.Entity.all &
--" Kind=" & Subindex_Item_Kind_Type'Image(Temp.Kind) &
--" From_Unit=" & Temp.From_Unit.all & " Keep_Set=" & Boolean'Image(Keep_Set));
	        New_Kind (Temp, Reset_Keep => Keep_Set, Last_Had_Same_Unit => False);
		Keep_Set := False;
	    elsif Last.Clause (1..Last.Clause_Len) = Temp.Clause (1..Temp.Clause_Len) and then
		  Last.Paragraph (1..Last.Paragraph_Len) = Temp.Paragraph (1..Temp.Paragraph_Len) then
		-- The reference and everything else is the same, so just
		-- forget this item.
		null;
	    else
--if Temp.Kind /= Top_Level then
--Ada.Text_IO.Put_Line("Same: Entity=" & Temp.Entity.all &
--" Kind=" & Subindex_Item_Kind_Type'Image(Temp.Kind) &
--" From_Unit=" & Temp.From_Unit.all & " Keep_Set=" & Boolean'Image(Keep_Set));
--else
--Ada.Text_IO.Put_Line("Same: Entity=" & Temp.Entity.all &
--" Kind=" & Subindex_Item_Kind_Type'Image(Temp.Kind) &
--" Keep_Set=" & Boolean'Image(Keep_Set));
--end if;
		-- Just add the next clause.
	        ARM_Output.Ordinary_Character (Output_Object, ',');
	        ARM_Output.Ordinary_Character (Output_Object, ' ');
		Clause_Ref (Temp);
	    end if;
	    Last := Temp;
	    Temp := Temp.Next;
	end loop;

	ARM_Output.End_Paragraph (Output_Object);

    end Write_Subindex;

end ARM_Subindex;


