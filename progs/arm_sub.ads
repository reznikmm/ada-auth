with ARM_Output;
with ARM_Index;
package ARM_Subindex is

    --
    -- Ada reference manual formatter.
    --
    -- This package contains the database to store subindex items for
    -- non-normative appendixes.
    --
    -- ---------------------------------------
    -- Copyright 2005  AXE Consultants.
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


