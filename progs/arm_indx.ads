with ARM_Output;
package ARM_Index is

    --
    -- Ada reference manual formatter.
    --
    -- This package contains the routines to manage and generate the index.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2005, 2006 AXE Consultants.
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
    --  5/28/00 - RLB - Created package.
    --  8/11/00 - RLB - Made Clean visible.
    -- 10/28/05 - RLB - Added key reuse.
    -- 10/30/05 - RLB - Added subtype declaration.
    --  2/17/06 - RLB - Added Remove_Soft_Hyphens flag to Clean (for output).

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
