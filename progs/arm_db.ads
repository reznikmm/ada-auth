package ARM_Database is

    --
    -- Ada reference manual formatter.
    --
    -- This package contains the database to store items for non-normative
    -- appendixes.
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
    --  5/16/00 - RLB - Created package.
    --  8/10/00 - RLB - Added Normal_Indexed_List to fix glossary problems.
    --  8/28/00 - RLB - Added revision info to database.

    type Database_Type is tagged limited private;

    type Paragraph_Change_Kind_Type is (None, Inserted, Deleted, Revised);

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
		      Sorted : in Boolean);
	-- Output the items with the appropriate format to the
	-- "Format_Text" routine. "Format_Text" allows all commands
	-- for the full formatter. (Text_Name is an identifying name
	-- for error messages).
	-- (This is indented to be used to output the items to
	-- appropriate Format and Output objects; but we can't do that
	-- directly because that would make this unit recursive with
	-- ARM_Format.

private

    type Item;
    type Item_List is access all Item;
    type Database_Type is tagged limited record
	Is_Valid : Boolean := False;
	List : Item_List;
	Item_Count : Natural;
    end record;

end ARM_Database;


