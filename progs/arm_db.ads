package ARM_Database is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package contains the database to store items for non-normative
    -- appendixes.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2004, 2005, 2006, 2011  AXE Consultants.
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
    --  8/10/00 - RLB - Added Normal_Indexed_List to fix glossary problems.
    --  8/28/00 - RLB - Added revision info to database.
    -- 10/28/04 - RLB - Added Inserted_Normal_Number change kind.
    -- 11/02/04 - RLB - Added Deleted_Inserted_Number change kind.
    -- 12/06/04 - RLB - Added Revised_Inserted_Number change kind.
    --  1/19/05 - RLB - Added Added_Version.
    --  2/15/06 - RLB - Added Deleted_No_Delete_Message and
    --			Deleted_Inserted_Number_No_Delete_Message change kinds.
    -- 10/18/06 - RLB - Added No_Deleted_Paragraph_Messages to Report.
    -- 10/18/11 - RLB - Changed to GPLv3 license.

    type Database_Type is tagged limited private;

    type Paragraph_Change_Kind_Type is (None, Inserted, Inserted_Normal_Number,
	Deleted, Deleted_Inserted_Number,
	Deleted_No_Delete_Message,
	Deleted_Inserted_Number_No_Delete_Message,
	Revised, Revised_Inserted_Number);

    Not_Valid_Error : exception;

    procedure Create (Database_Object : in out Database_Type);
	-- Initialize a database object.

    procedure Destroy (Database_Object : in out Database_Type);
	-- Destroy a database object, freeing any resources used.

    procedure Insert (Database_Object : in out Database_Type;
		      Sort_Key : in String;
		      Hang_Item : in String;
		      Text : in String;
		      Change_Kind : in Paragraph_Change_Kind_Type := ARM_Database.None;
		      Version : in Character := '0');
	-- Insert an item into the database object.
	-- Sort_Key is the string on which this item will be sorted (if it
	-- is sorted). Hang_Item is the item which hangs out for the item
	-- in the report (if any). Text is the text for the item; the text
	-- may include formatting codes. Change_Kind and Version are the
	-- revision status for this item.

    type Format_Type is
	(Normal_List, Normal_Indexed_List, Bullet_List, Hanging_List);

    generic
	with procedure Format_Text (Text : in String;
				    Text_Name : in String);
    procedure Report (Database_Object : in out Database_Type;
		      In_Format : in Format_Type;
		      Sorted : in Boolean;
		      Added_Version : Character := '0';
		      No_Deleted_Paragraph_Messages : in Boolean := False);
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
private

    type Item;
    type Item_List is access all Item;
    type Database_Type is tagged limited record
	Is_Valid : Boolean := False;
	List : Item_List;
	Item_Count : Natural;
    end record;

end ARM_Database;


