with Ada.Text_IO; -- Debug.
with Ada.Unchecked_Deallocation,
     Ada.Strings.Fixed,
     Ada.Characters.Handling;
package body ARM_Database is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package contains the database to store items for non-normative
    -- appendixes.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2004, 2005, 2006, 2009, 2011, 2012, 2019
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
    -- 10/18/11 - RLB - Changed to GPLv3 license.
    -- 10/20/11 - RLB - Added Initial_Version parameter.
    --  3/19/12 - RLB - Added code to suppress indexing of deleted glossary items.
    --  1/27/19 - RLB - Lengthened components to allow Reduce attributes.
    --  5/25/22 - RLB - Added note strings to Insert for Terms DBs. Implemented
    --                  Term formatting and output.
    --  6/ 1/22 - RLB - Redid formatting for terms in the RM.

    type String_Ptr is access String;
    type Item is record
	Next : Item_List;
	Sort_Key : String(1 .. 55);
	Hang : String(1 .. 85);
	Hang_Len : Natural;
	Text : String_Ptr;
	Note1_Text : String_Ptr;
	Note2_Text : String_Ptr;
	Note3_Text : String_Ptr;
	Change_Kind : Paragraph_Change_Kind_Type;
	Version : Character;
	Initial_Version : Character;
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
                      Note1_Text : in String := "";
                      Note2_Text : in String := "";
                      Note3_Text : in String := "";
		      Change_Kind : in Paragraph_Change_Kind_Type := ARM_Database.None;
		      Version : in Character := '0';
		      Initial_Version : in Character := '0') is
	-- Insert an item into the database object.
	-- Sort_Key is the string on which this item will be sorted (if it
	-- is sorted). Hang_Item is the item which hangs out for the item
	-- in the report (if any). Text is the text for the item; the text
	-- may include formatting codes. Note1_Text, Note2_Text, and Note3_Text
        -- are optional note text, these also may include formatting codes.
        -- Change_Kind and Version are the revision status for this item.
        -- Initial_Version is the version of the initial text for this item.
	Temp_Item : Item;
    begin
	if not Database_Object.Is_Valid then
	    raise Not_Valid_Error;
	end if;
--if Sort_Key'Length > Temp_Item.Sort_Key'Length - 10 then
--    Ada.Text_IO.Put_Line ("?? Sort_Key near or beyond size limit; Length=" &
--      Natural'Image(Sort_Key'Length) & "; Limit=" &
--     Natural'Image(Temp_Item.Sort_Key'Length) & "; Sort_Key=" & Sort_Key);
--end if;
--if Hang_Item'Length > Temp_Item.Hang'Length - 10 then
--    Ada.Text_IO.Put_Line ("?? Hang_Item near or beyond size limit; Length=" &
--      Natural'Image(Hang_Item'Length) & "; Limit=" &
--     Natural'Image(Temp_Item.Hang'Length) & "; Hang_Item=" & Hang_Item);
--end if;

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
        if Note1_Text /= "" then
	    Temp_Item.Note1_Text := new String'(Note1_Text);        
        else
            Temp_Item.Note1_Text := null;
        end if;
        if Note2_Text /= "" then
	    Temp_Item.Note2_Text := new String'(Note2_Text);        
        else
            Temp_Item.Note2_Text := null;
        end if;
        if Note3_Text /= "" then
	    Temp_Item.Note3_Text := new String'(Note3_Text);        
        else
            Temp_Item.Note3_Text := null;
        end if;
	Temp_Item.Change_Kind := Change_Kind;
	Temp_Item.Version := Version;
	Temp_Item.Initial_Version := Initial_Version;
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
            -- Return the @ChgRef command needed for item.
	begin
	    -- Note: In the report, we always decide inserted/not inserted
	    -- as determined by the initial version number, and not the
	    -- original class.
	    case Item.Change_Kind is
		when None => return "";
		when Inserted | Inserted_Normal_Number =>
		    if Item.Initial_Version <= Added_Version then
		        return "@ChgRef{Version=[" & Item.Version &
			    "],Kind=[AddedNormal]}";
		    else
		        return "@ChgRef{Version=[" & Item.Version &
			    "],Kind=[Added]}";
		    end if;
		when Revised | Revised_Inserted_Number =>
		    if Item.Initial_Version <= Added_Version then
		        return "@ChgRef{Version=[" & Item.Version &
			    "],Kind=[Revised]}";
		    else
		        return "@ChgRef{Version=[" & Item.Version &
			    "],Kind=[RevisedAdded]}";
		    end if;
		when Deleted | Deleted_Inserted_Number =>
		    if Item.Initial_Version <= Added_Version then
		        if No_Deleted_Paragraph_Messages then
		            return "@ChgRef{Version=[" & Item.Version &
			        "],Kind=[DeletedNoDelMsg]}";
		        else
		            return "@ChgRef{Version=[" & Item.Version &
			        "],Kind=[Deleted]}";
		        end if;
		    else
		        if No_Deleted_Paragraph_Messages then
		            return "@ChgRef{Version=[" & Item.Version &
			        "],Kind=[DeletedAddedNoDelMsg]}";
		        else
		            return "@ChgRef{Version=[" & Item.Version &
			        "],Kind=[DeletedAdded]}";
		        end if;
		    end if;
		when Deleted_No_Delete_Message |
		     Deleted_Inserted_Number_No_Delete_Message =>
		    if Item.Initial_Version <= Added_Version then
		        return "@ChgRef{Version=[" & Item.Version &
		            "],Kind=[DeletedNoDelMsg]}";
		    else
		        return "@ChgRef{Version=[" & Item.Version &
			    "],Kind=[DeletedAddedNoDelMsg]}";
		    end if;
	    end case;
	end Change_if_Needed;


	function Insert_and_Change_if_Needed (Item : in Item_List) return String is
            -- Return the @ChgRef and @ChgAdded commands needed for item.
        begin
            if Temp.Initial_Version = '0' then -- No ChgAdded needed.
                return Change_if_Needed (Item);
            else
                return Change_if_Needed (Item) & "@ChgAdded{Version=[" &
                   Temp.Initial_Version & "],Text=[";
            end if;
        end Insert_and_Change_if_Needed;

	function Close_Insert_and_Change_if_Needed (Item : in Item_List) return String is
            -- Close the commands generated by Insert_and_Change_if_Needed.
        begin
            if Temp.Initial_Version = '0' then -- No ChgAdded needed.
                return "";
            else
                return "]}";
            end if;
        end Close_Insert_and_Change_if_Needed;

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

		-- Sort the items array (use an insertion sort because it is
		-- stable):
		declare
		    Left : Natural;  -- Left sorting stop
		begin
		    for Right In Items'First+1 .. Items'Last loop -- Right sorting stop
			Temp := Items(Right);
			Left := Right - 1;
			while Temp.Sort_Key <= Items(Left).Sort_Key loop -- Switch items
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
--** Debug:
--Ada.Text_IO.Put_Line ("^^ " & Paragraph_Change_Kind_Type'Image(Temp.Change_Kind) &
--   " for " & Temp.Hang(1..Temp.Hang_Len) & " ref=" & Change_if_Needed (Temp));
--Ada.Text_IO.Put_Line ("   " & Change_if_Needed (Temp) &
--Temp.Hang(1..Temp.Hang_Len) & "@\" &
--Temp.Text.all & Ascii.LF & Ascii.LF);
		    Format_Text (Change_if_Needed (Temp) &
			Temp.Hang(1..Temp.Hang_Len) & "@\" &
			Temp.Text.all & Ascii.LF & Ascii.LF, Temp.Sort_Key);
                    if Temp.Note1_Text /= null or else
                       Temp.Note2_Text /= null or else
                       Temp.Note3_Text /= null then
                        -- Should not happen.
                        raise Program_Error with "Notes specified in Hanging_List";
                    end if;
		    Temp := Temp.Next;
		end loop;
		Format_Text ("@end(description)" & Ascii.LF, "Suffix");
	    when Bullet_List =>
		Format_Text ("@begin(itemize)" & Ascii.LF, "Prefix");
		Temp := Database_Object.List;
		while Temp /= null loop
		    Format_Text (Change_if_Needed (Temp) &
		    		 Temp.Text.all & Ascii.LF & Ascii.LF, Temp.Sort_Key);
                    if Temp.Note1_Text /= null or else
                       Temp.Note2_Text /= null or else
                       Temp.Note3_Text /= null then
                        -- Should not happen.
                        raise Program_Error with "Notes specified in Bullet_List";
                    elsif Temp.Hang_Len /= 0 then
                        raise Program_Error with "Hang item specified in Bullet_List";
                    end if;
		    Temp := Temp.Next;
		end loop;
		Format_Text ("@end(itemize)" & Ascii.LF, "Suffix");
	    when Normal_List =>
		Format_Text ("@begin(intro)" & Ascii.LF, "Prefix");
		Temp := Database_Object.List;
		while Temp /= null loop
		    Format_Text (Change_if_Needed (Temp) &
				 Temp.Text.all & Ascii.LF & Ascii.LF, Temp.Sort_Key);
                    if Temp.Note1_Text /= null or else
                       Temp.Note2_Text /= null or else
                       Temp.Note3_Text /= null then
                        -- Should not happen.
                        raise Program_Error with "Notes specified in Normal_List";
                    elsif Temp.Hang_Len /= 0 then
                        raise Program_Error with "Hang item specified in Normal_List";
                    end if;
		    Temp := Temp.Next;
		end loop;
		Format_Text ("@end(intro)" & Ascii.LF, "Suffix");
	    when Normal_Indexed_List =>
		Format_Text ("@begin(intro)" & Ascii.LF, "Prefix");
		Temp := Database_Object.List;
		while Temp /= null loop
		    case Temp.Change_Kind is
			when None |
			     Inserted | Inserted_Normal_Number |
			     Revised | Revised_Inserted_Number =>
--** Debug:
--Ada.Text_IO.Put_Line("Format " & Change_if_Needed (Temp) &
--			"@defn{" & Ada.Strings.Fixed.Trim (Temp.Sort_Key, Ada.Strings.Right) & "}" & Ascii.LF &
--			Temp.Text.all);
			    -- Index this item.
		            Format_Text (Change_if_Needed (Temp) &
			        "@defn{" & Ada.Strings.Fixed.Trim (Temp.Sort_Key, Ada.Strings.Right) & "}" & Ascii.LF &
			        Temp.Text.all & Ascii.LF & Ascii.LF, Temp.Sort_Key);
			when Deleted | Deleted_Inserted_Number |
			     Deleted_No_Delete_Message |
			     Deleted_Inserted_Number_No_Delete_Message =>
--** Debug:
--Ada.Text_IO.Put_Line("Format " & Change_if_Needed (Temp) & Ascii.LF &
--			Temp.Text.all);
			    -- Don't index deleted items.
		            Format_Text (Change_if_Needed (Temp) & Ascii.LF &
			        Temp.Text.all & Ascii.LF & Ascii.LF, Temp.Sort_Key);
		    end case;
                    if Temp.Note1_Text /= null or else
                       Temp.Note2_Text /= null or else
                       Temp.Note3_Text /= null then
                        -- Should not happen.
                        raise Program_Error with "Notes specified in Normal_Indexed_List";
                    elsif Temp.Hang_Len /= 0 then
                        raise Program_Error with "Hang item specified in Normal_Indexed_List";
                    end if;
		    Temp := Temp.Next;
		end loop;
		Format_Text ("@end(intro)" & Ascii.LF, "Suffix");
	    when Numbered_T_and_D_List => -- Terms and Definitions list for ISO.
		Format_Text ("@begin(intro)" & Ascii.LF, "Prefix");
		Temp := Database_Object.List;
		while Temp /= null loop
 		    Format_Text ("@subnumber" & Ascii.LF & Ascii.LF, "number for " & Temp.Sort_Key);
                        -- For ISO, these need to be numbered, based on the
                        -- enclosing subclause number. The subnumber command was
                        -- added for this purpose.
                    -- First, the item name:                   
		    case Temp.Change_Kind is
			when None |
			     Inserted | Inserted_Normal_Number |
			     Revised | Revised_Inserted_Number =>
--** Debug:
--Ada.Text_IO.Put_Line("Format " & Change_if_Needed (Temp) &
--			"@defn{" & Ada.Strings.Fixed.Trim (Temp.Sort_Key, Ada.Strings.Right) & "}");
			    -- Index this item.
		            Format_Text (Change_if_Needed (Temp) &
			        "@defn{" & Ada.Strings.Fixed.Trim (Temp.Sort_Key, Ada.Strings.Right) & "}@b{" & 
                                Temp.Hang(1..Temp.Hang_Len) & "}" & Ascii.LF & Ascii.LF, Temp.Sort_Key);
			when Deleted | Deleted_Inserted_Number |
			     Deleted_No_Delete_Message |
			     Deleted_Inserted_Number_No_Delete_Message =>
--** Debug:
--Ada.Text_IO.Put_Line("Format " & Change_if_Needed (Temp));
			    -- Don't index deleted items.
		            Format_Text (Change_if_Needed (Temp) & "@b{" & 
                                Temp.Hang(1..Temp.Hang_Len) & "}" & Ascii.LF & Ascii.LF, Temp.Sort_Key);
		    end case;
                    -- Now, the definition:
	            Format_Text (Temp.Text.all & Ascii.LF & Ascii.LF, Temp.Sort_Key);
                    if Temp.Note1_Text /= null or else
                       Temp.Note2_Text /= null or else
                       Temp.Note3_Text /= null then
		        Format_Text ("@begin{indent}@begin{small}" & Ascii.LF, Temp.Sort_Key & " note prefix" & Ascii.LF);
                        if Temp.Note1_Text /= null then
                            Format_Text ("Note 1 to entry: " & Temp.Note1_Text.all & Ascii.LF & Ascii.LF, Temp.Sort_Key);
                        end if;
                        if Temp.Note2_Text /= null then
                            Format_Text ("Note 2 to entry: " & Temp.Note2_Text.all & Ascii.LF & Ascii.LF, Temp.Sort_Key);
                        end if;
                        if Temp.Note3_Text /= null then
                            Format_Text ("Note 3 to entry:"  & Temp.Note3_Text.all & Ascii.LF & Ascii.LF, Temp.Sort_Key);
                        end if;
		        Format_Text ("@end{small}@end{indent}" & Ascii.LF, Temp.Sort_Key & " note suffix" & Ascii.LF);
                    -- else no notes.
                    end if;
	            Format_Text (Ascii.LF & "", Temp.Sort_Key);
		    Temp := Temp.Next;
		end loop;
		Format_Text ("@end(intro)" & Ascii.LF, "Suffix");
	    when T_and_D_List => -- Terms and Definitions list for RM.
                -- Format this like hanging text:
		Format_Text ("@begin(hang1list)" & Ascii.LF, "Prefix");
		Temp := Database_Object.List;
		while Temp /= null loop
                    -- First, the item name and definition:                   
		    case Temp.Change_Kind is
			when None |
			     Inserted | Inserted_Normal_Number |
			     Revised | Revised_Inserted_Number =>
--** Debug:
--Ada.Text_IO.Put_Line("Format " & Change_if_Needed (Temp) &
--			"@defn{" & Ada.Strings.Fixed.Trim (Temp.Sort_Key, Ada.Strings.Right) & "}");
			    -- Index this item.
                            Format_Text (Insert_and_Change_if_Needed (Temp) & 
                                "@defn{" & Ada.Strings.Fixed.Trim (Temp.Sort_Key, Ada.Strings.Right) & "}@b{" & 
                                Temp.Hang(1..Temp.Hang_Len) & "}@\" & Temp.Text.all &
                                Close_Insert_and_Change_if_Needed (Temp) & Ascii.LF & Ascii.LF, Temp.Sort_Key);
			when Deleted | Deleted_Inserted_Number |
			     Deleted_No_Delete_Message |
			     Deleted_Inserted_Number_No_Delete_Message =>
--** Debug:
--Ada.Text_IO.Put_Line("Format " & Change_if_Needed (Temp));
			    -- Don't index deleted items.
		            Format_Text (Insert_and_Change_if_Needed (Temp) & "@b{" & 
                                Temp.Hang(1..Temp.Hang_Len) & "}@\" & Temp.Text.all & 
                                Close_Insert_and_Change_if_Needed (Temp) & Ascii.LF & Ascii.LF, Temp.Sort_Key);
		    end case;
                    if Temp.Note1_Text /= null or else
                       Temp.Note2_Text /= null or else
                       Temp.Note3_Text /= null then
		        Format_Text ("@begin{small}@begin{indent}" & Ascii.LF, Temp.Sort_Key & " note prefix" & Ascii.LF);
                        if Temp.Note1_Text /= null then
                            if Temp.Note2_Text = null and then Temp.Note3_Text = null then
                                Format_Text (Insert_and_Change_if_Needed (Temp) & "Note: " & Temp.Note1_Text.all &
                                             Close_Insert_and_Change_if_Needed (Temp) & Ascii.LF & Ascii.LF, Temp.Sort_Key);
                            else
                                Format_Text (Insert_and_Change_if_Needed (Temp) & "Note 1: " & Temp.Note1_Text.all &
                                             Close_Insert_and_Change_if_Needed (Temp) & Ascii.LF & Ascii.LF, Temp.Sort_Key);
                            end if;
                        end if;
                        if Temp.Note2_Text /= null then
                            Format_Text (Insert_and_Change_if_Needed (Temp) & "Note 2: " & Temp.Note2_Text.all &
                                         Close_Insert_and_Change_if_Needed (Temp) & Ascii.LF & Ascii.LF, Temp.Sort_Key);
                        end if;
                        if Temp.Note3_Text /= null then
                            Format_Text (Insert_and_Change_if_Needed (Temp) & "Note 3: " & Temp.Note3_Text.all &
                                         Close_Insert_and_Change_if_Needed (Temp) & Ascii.LF & Ascii.LF, Temp.Sort_Key);
                        end if;
		        Format_Text ("@end{indent}@end{small}" & Ascii.LF, Temp.Sort_Key & " note suffix" & Ascii.LF);
                    -- else no notes.
                    end if;
	            Format_Text (Ascii.LF & "", Temp.Sort_Key);
		    Temp := Temp.Next;
		end loop;
		Format_Text ("@end(hang1list)" & Ascii.LF, "Suffix");
	end case;
    end Report;

end ARM_Database;


