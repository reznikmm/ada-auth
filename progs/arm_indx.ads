with ARM_Output;
package ARM_Index is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package contains the routines to manage and generate the index.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2005, 2006, 2011 AXE Consultants.
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
    --  5/28/00 - RLB - Created package.
    --  8/11/00 - RLB - Made Clean visible.
    -- 10/28/05 - RLB - Added key reuse.
    -- 10/30/05 - RLB - Added subtype declaration.
    --  2/17/06 - RLB - Added Remove_Soft_Hyphens flag to Clean (for output).
    -- 10/18/11 - RLB - Changed to GPLv3 license.

    Not_Valid_Error : exception;

    subtype Index_Key is Natural range 0 .. 20000;

    procedure Create;
	-- Initialize this package.

    procedure Destroy;
	-- Finalize this package; make sure the index is empty.

    type Index_Item_Kind_Type is (
	Primary_Term,			-- A primary index term.
	Partial_Term,			-- A partial index term.
	Primary_Term_and_Subterm,	-- A primary index term with a subterm.
	Partial_Term_with_Subterm,	-- A partial index term with a subterm.
	Syntax_NT_Used,			-- A non-terminal, with a subterm of "Used".
	Child_Unit_Parent,		-- A child unit, with a subterm of "child of <subterm>".
	Declaration_in_Package,		-- A declaration, with a subterm of "in <package>".
	SubDeclaration_in_Package,	-- A term, with a subterm of "<item> in <package>".
	Subtype_Declaration_in_Package,	-- A term (of the form "<item> subtype of <item2>, with a subterm of "in <package>".
	See_Term,			-- A "see <subterm>" with reference.
	See_Also_Term,			-- A "see also <subterm>" with reference.
	See_Other_Term,			-- A "see <subterm>" without reference.
	See_Also_Other_Term);		-- A "see also <subterm>" without reference.
	-- Note: These are declared in the sorting order.

    function Get_Key return Index_Key;
	-- Returns a Key value to refer to one or more index entries
	-- (for a single entity).

    procedure Add (Term  : in String;
		   Subterm : in String := "";
		   Kind : in Index_Item_Kind_Type := Primary_Term;
		   Clause : in String := "";
		   Paragraph : in String := "";
                   Key : out Index_Key);
	-- Add an index reference to the index. Returns a Key value to
	-- refer to this index entry.
	-- Raises Not_Valid_Error if Subterm, Clause, or Paragraph is not
	-- empty when the kind does not use it.

    procedure Add_Reusing_Key (Term  : in String;
			       Subterm : in String := "";
			       Kind : in Index_Item_Kind_Type := Primary_Term;
			       Clause : in String := "";
			       Paragraph : in String := "";
	                       Key : in Index_Key);
	-- Add an index reference to the index, (re)using the specified Key
	-- to refer to this index entry. Key must have previously
	-- returned by Add or Get_Key.
	-- Raises Not_Valid_Error if Subterm, Clause, or Paragraph is not
	-- empty when the kind does not use it.

    function Clean (Item : in String;
		    Remove_Soft_Hyphens : in Boolean) return String;
	-- Remove any commands from Item. (Except for soft hyphens
	-- if Remove_Soft_Hyphens is False.)

    procedure Generate_Index_Body (Output_Object : in out ARM_Output.Output_Type'Class;
				   Use_Paragraphs : in Boolean := True);
	-- Generate the index body. (The section header has already been
	-- generated). References include paragraph numbers if Use_Paragraphs
	-- is true.

end ARM_Index;
