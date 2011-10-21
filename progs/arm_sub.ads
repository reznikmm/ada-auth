with ARM_Output;
with ARM_Index;
package ARM_Subindex is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package contains the database to store subindex items for
    -- non-normative appendixes.
    --
    -- ---------------------------------------
    -- Copyright 2005, 2011
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
    -- 10/28/05 - RLB - Created package.
    -- 10/18/11 - RLB - Changed to GPLv3 license.

    type Subindex_Type is tagged limited private;

    Not_Valid_Error : exception;

    procedure Create (Subindex_Object : in out Subindex_Type);
	-- Initialize a Subindex object.

    procedure Destroy (Subindex_Object : in out Subindex_Type);
	-- Destroy a Subindex object, freeing any resources used.

    type Subindex_Item_Kind_Type is (Top_Level, In_Unit,
	Child_of_Parent, Subtype_In_Unit,
	Description_In_Unit, Raised_Belonging_to_Unit);

    procedure Insert (Subindex_Object : in out Subindex_Type;
		      Entity          : in String;
		      From_Unit       : in String := "";
		      Kind	      : in Subindex_Item_Kind_Type := Top_Level;
		      Clause	      : in String := "";
		      Paragraph	      : in String := "";
                      Key	      : in ARM_Index.Index_Key);
	-- Insert an item into the Subindex object.
	-- The Key must be one returned by ARM_Index.Add or ARM_Index.Get_Key.
	-- Raises Not_Valid_Error if In_Unit, Clause, or Paragraph is not
	-- empty when the kind does not use it.

    procedure Write_Subindex (
		Subindex_Object : in out Subindex_Type;
		Output_Object   : in out ARM_Output.Output_Type'Class;
		Use_Paragraphs : in Boolean := True;
		Minimize_Lines : in Boolean := False);
	-- Generate the given subindex to Output_Object.
	-- References include paragraph numbers if Use_Paragraphs is true.
	-- Try to minimize lines if Minimize_Lines is True.

private

    type Item;
    type Item_List is access all Item;
    type Subindex_Type is tagged limited record
	Is_Valid : Boolean := False;
	List : Item_List;
	Item_Count : Natural;
    end record;

end ARM_Subindex;


