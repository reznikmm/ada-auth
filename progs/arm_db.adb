with Ada.Text_IO; -- Debug.
with Ada.Unchecked_Deallocation,
     Ada.Strings.Fixed,
     Ada.Characters.Handling;
package body ARM_Database is

    --
    -- Ada reference manual formatter.
    --
    -- This package contains the database to store items for non-normative
    -- appendixes.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2004, 2005, 2006, 2009  AXE Consultants.
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
    --  5/16/00 - RLB - Created package.
    --  8/28/00 - RLB - Added revision info to database.
    -- 10/28/04 - RLB - Added Inserted_Normal_Number change kind.
    -- 11/02/04 - RLB - Added Deleted_Inserted_Number change kind.
    -- 12/06/04 - RLB - Added Revised_Inserted_Number change kind.
    -- 12/14/04 - RLB - Made the hang item bigger.
    --  1/19/05 - RLB - Added Added_Version.
    -- 10/17/05 - RLB - Fixed indexing of the Glossary.
    -- 10/18/06 - RLB - Added No_Deleted_Paragraph_Messages to Report.
    -- 11/30/09 - RLB - Made the hang item bigger again (to make room to
    --			handle commands like @ChgAdded).

    type String_Ptr is access String;
    type Item is record
	Next : Item_List;
	Sort_Key : String(1 .. 50);
	Hang : String(1 .. 75);
	Hang_Len : Natural;
	Text : String_Ptr;
	Change_Kind : Paragraph_Change_Kind_Type;
	Version : Character;
    end record;

    procedure Free is new Ada.Unchecked_Deallocation (Item, Item_List);
    procedure Free is new Ada.Unchecked_Deallocation (String, String_Ptr);

    procedure Create (Database_Object : in out Database_Type) is
	-- Initialize a database object.
    begin
	Database_Object.Is_Valid := True;
	Database_Object.List := null;
	Database_Object.Item_Count := 0;
    end Create;


    procedure Destroy (Database_Object : in out Database_Type) is
	-- Destroy a database object, freeing any resources used.
	Temp : Item_List;
    begin
	if not Database_Object.Is_Valid then
	    raise Not_Valid_Error;
	end if;
	while Database_Object.List /= null loop
	    Temp := Database_Object.List;
	    Database_Object.List := Temp.Next;
	    Free (Temp.Text);
	    Free (Temp);
	end loop;
	Database_Object.Is_Valid := False;
    end Destroy;


    procedure Insert (Database_Object : in out Database_Type;
		      Sort_Key : in String;
		      Hang_Item : in String;
		      Text : in String;
		      Change_Kind : in Paragraph_Change_Kind_Type := ARM_Database.None;
		      Version : in Character := '0') is
	-- Insert an item into the database object.
	-- Sort_Key is the string on which this item will be sorted (if it
	-- is sorted). Hang_Item is the item which hangs out for the item
	-- in the report (if any). Text is the text for the item; the text
	-- may include formatting codes. Change_Kind and Version are the
	-- revision status for this item.
	Temp_Item : Item;
    begin
	if not Database_Object.Is_Valid then
	    raise Not_Valid_Error;
	end if;
	Ada.Strings.Fixed.Move (Target => Temp_Item.Sort_Key,
				Source => Ada.Characters.Handling.To_Lower(Sort_Key),
				Drop   => Ada.Strings.Right,
			        Pad    => ' ');
	Ada.Strings.Fixed.Move (Target => Temp_Item.Hang,
				Source => Hang_Item,
				Drop   => Ada.Strings.Error,
			        Pad    => ' ');
	Temp_Item.Hang_Len := Hang_Item'Length;
	    -- Note: If this second item doesn't fit, we error so we can make
	    -- the size larger.
	Temp_Item.Text := new String'(Text);
	Temp_Item.Change_Kind := Change_Kind;
	Temp_Item.Version := Version;
	Temp_Item.Next := Database_Object.List;
        Database_Object.List := new Item'(Temp_Item);
	Database_Object.Item_Count := Database_Object.Item_Count + 1;
    end Insert;


    --generic
    --	with procedure Format_Text (Text : in String;
    --				    Text_Name : in String);
    procedure Report (Database_Object : in out Database_Type;
		      In_Format : in Format_Type;
		      Sorted : in Boolean;
		      Added_Version : Character := '0';
		      No_Deleted_Paragraph_Messages : in Boolean := False) is
	-- Output the items with the appropriate format to the
	-- "Format_Text" routine. "Format_Text" allows all commands
	-- for the full formatter. (Text_Name is an identifying name
	-- for error messages). This is an added list for Added_Version
	-- ('0' meaning it is not added); in that case, use normal numbers
	-- for items with a version less than or equal to Added_Version.
	-- (This is intended to be used to output the items to
	-- appropriate Format and Output objects; but we can't do that
	-- directly because that would make this unit recursive with
	-- ARM_Format.
	-- No paragraphs will be have deleted paragraph messages if
	-- No_Deleted_Paragraph_Messages is True.
	Temp : Item_List;

	function Change_if_Needed (Item : in Item_List) return String is
	begin
	    case Item.Change_Kind is
		when None => return "";
		when Inserted | Inserted_Normal_Number =>
		    if Item.Version <= Added_Version then
		        return "@ChgRef{Version=[" & Item.Version &
			    "],Kind=[AddedNormal]}";
		    else
		        return "@ChgRef{Version=[" & Item.Version &
			    "],Kind=[Added]}";
		    end if;
		    -- Note: In the report, we always use the insert determined
		    -- by the version number, and not the original class.
		when Revised =>
		    return "@ChgRef{Version=[" & Item.Version &
			"],Kind=[Revised]}";
		when Revised_Inserted_Number =>
		    -- Previously inserted.
		    return "@ChgRef{Version=[" & Item.Version &
			"],Kind=[RevisedInserted]}";
		when Deleted =>
		    if No_Deleted_Paragraph_Messages then
		        return "@ChgRef{Version=[" & Item.Version &
			    "],Kind=[DeletedNoDelMsg]}";
		    else
		        return "@ChgRef{Version=[" & Item.Version &
			    "],Kind=[Deleted]}";
		    end if;
		when Deleted_Inserted_Number =>
		    -- Previously inserted.
		    if No_Deleted_Paragraph_Messages then
		        return "@ChgRef{Version=[" & Item.Version &
			    "],Kind=[DeletedInsertedNoDelMsg]}";
		    else
		        return "@ChgRef{Version=[" & Item.Version &
			    "],Kind=[DeletedInserted]}";
		    end if;
		when Deleted_No_Delete_Message =>
		    return "@ChgRef{Version=[" & Item.Version &
		        "],Kind=[DeletedNoDelMsg]}";
		when Deleted_Inserted_Number_No_Delete_Message =>
		    -- Previously inserted.
		    return "@ChgRef{Version=[" & Item.Version &
			"],Kind=[DeletedInsertedNoDelMsg]}";
	    end case;
	end Change_if_Needed;

    begin
	if not Database_Object.Is_Valid then
	    raise Not_Valid_Error;
	end if;
	if Sorted then
	    declare
		Items : array (1..Database_Object.Item_Count) of Item_List;
	    begin
		-- Load the items:
		Temp := Database_Object.List;
		for I in Items'range loop
		    Items(I) := Temp;
		    Temp := Temp.Next;
		end loop;

		-- Sort the items array (use an insertion sort):
		declare
		    Left : Natural;  -- Left sorting stop
		begin
		    for Right In Items'First+1 .. Items'Last loop -- Right sorting stop
			Temp := Items(Right);
			Left := Right - 1;
			while Temp.Sort_Key < Items(Left).Sort_Key loop -- Switch items
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
		    Database_Object.List := Items(1);
		else
		    Database_Object.List := null;
		end if;
	    end;
	end if;
	case In_Format is
	    when Hanging_List =>
		Format_Text ("@begin(description)" & Ascii.LF, "Prefix");
		Temp := Database_Object.List;
		while Temp /= null loop
--Ada.Text_IO.Put_Line ("^^ " & Paragraph_Change_Kind_Type'Image(Temp.Change_Kind) &
--   " for " & Temp.Hang(1..Temp.Hang_Len) & " ref=" & Change_if_Needed (Temp));
--Ada.Text_IO.Put_Line ("   " & Change_if_Needed (Temp) &
--Temp.Hang(1..Temp.Hang_Len) & "@\" &
--Temp.Text.all & Ascii.LF & Ascii.LF);
		    Format_Text (Change_if_Needed (Temp) &
			Temp.Hang(1..Temp.Hang_Len) & "@\" &
			Temp.Text.all & Ascii.LF & Ascii.LF, Temp.Sort_Key);
		    Temp := Temp.Next;
		end loop;
		Format_Text ("@end(description)" & Ascii.LF, "Suffix");
	    when Bullet_List =>
		Format_Text ("@begin(itemize)" & Ascii.LF, "Prefix");
		Temp := Database_Object.List;
		while Temp /= null loop
		    Format_Text (Change_if_Needed (Temp) &
		    		 Temp.Text.all & Ascii.LF & Ascii.LF, Temp.Sort_Key);
		    Temp := Temp.Next;
		end loop;
		Format_Text ("@end(itemize)" & Ascii.LF, "Suffix");
	    when Normal_List =>
		Format_Text ("@begin(intro)" & Ascii.LF, "Prefix");
		Temp := Database_Object.List;
		while Temp /= null loop
		    Format_Text (Change_if_Needed (Temp) &
				 Temp.Text.all & Ascii.LF & Ascii.LF, Temp.Sort_Key);
		    Temp := Temp.Next;
		end loop;
		Format_Text ("@end(intro)" & Ascii.LF, "Suffix");
	    when Normal_Indexed_List =>
		Format_Text ("@begin(intro)" & Ascii.LF, "Prefix");
		Temp := Database_Object.List;
		while Temp /= null loop
--** Debug:
--Ada.Text_IO.Put_Line("Format " & Change_if_Needed (Temp) &
--			"@defn{" & Ada.Strings.Fixed.Trim (Temp.Sort_Key, Ada.Strings.Right) & "}" & Ascii.LF &
--			Temp.Text.all);
		    Format_Text (Change_if_Needed (Temp) &
			"@defn{" & Ada.Strings.Fixed.Trim (Temp.Sort_Key, Ada.Strings.Right) & "}" & Ascii.LF &
			Temp.Text.all & Ascii.LF & Ascii.LF, Temp.Sort_Key);
		    Temp := Temp.Next;
		end loop;
		Format_Text ("@end(intro)" & Ascii.LF, "Suffix");
	end case;
    end Report;

end ARM_Database;


