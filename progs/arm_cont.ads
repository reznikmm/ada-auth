with Ada.Strings.Unbounded;
package ARM_Contents is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package contains the routines to manage section/clause/subclause
    -- references.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2004, 2006, 2007, 2009, 2011, 2012
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
    --  4/19/00 - RLB - Created base package.
    --  4/26/00 - RLB - Added Previous_Clause and Next_Clause.
    --  5/15/00 - RLB - Added rules about unnumbered sections.
    --  5/22/00 - RLB - Added Unnumbered_Section level.
    --  8/ 7/00 - RLB - Made Make_Clause visible.
    --  8/22/00 - RLB - Added Old_Title handling.
    --  9/14/04 - RLB - Moved Change_Version_Type here, to avoid mutual
    --			dependence.
    --		- RLB - Added version to changes.
    --  9/22/06 - RLB - Created type Clause_Number_Type and added SubSubClause.
    -- 12/18/07 - RLB - Added Plain_Annex.
    --  5/06/09 - RLB - Added Versioned_String.
    --  5/07/09 - RLB - Added Dead_Clause.
    -- 10/18/11 - RLB - Changed to GPLv3 license.
    -- 10/19/11 - RLB - Added Parent_Clause from Stephen Leake's version.
    -- 10/25/11 - RLB - Added version to Old name strings.
    --  8/30/12 - RLB - Added initialization of Section to UNKNOWN to
    --			detect bugs earlier.

    subtype Title_Type is String (1 .. 80);
	-- The type of a title.

    type Section_Number_Type is range 0 .. 58;
	-- Values > 30 represent annex letters (31 => A, 32 => B, etc.)
	-- Value = 0 represents the preface, introduction, etc. No
	-- number is generated if Section_Number = 0.
    ANNEX_START : constant := 31; -- First annex section number.
    UNKNOWN : constant Section_Number_Type := 58; -- Uninitialized sections get this.

    subtype Change_Version_Type is Character range '0' .. '9';
	-- Defines the change version. Version 0 is the original text.

    type Versioned_String is array (ARM_Contents.Change_Version_Type) of
	Ada.Strings.Unbounded.Unbounded_String;

    type Clause_Number_Type is record
	Section : Section_Number_Type := UNKNOWN;
	Clause : Natural := 0;
	Subclause : Natural := 0;
	Subsubclause : Natural := 0;
    end record;

    Not_Found_Error  : exception;

    Bad_Clause_Error : exception;
	-- Raised by any of the below if the Clause_Number is
	-- invalid (potentially depending on the other parameters,
	-- like the level).

    procedure Initialize;
	-- Initialize this package; make sure the contents are empty.

    type Level_Type is (Section, Unnumbered_Section, Plain_Annex,
			Normative_Annex, Informative_Annex,
			Clause, Subclause, Subsubclause, Dead_Clause);
	-- Defines the level of a clause header.
	-- Clause is "xx.nn"; Subclause is "xx.nn.nn"; Subsubclause is "xx.nn.nn.nn".

    function "<" (Left, Right : Clause_Number_Type) return Boolean;
	-- True if Left comes before Right in the collating order.

    function ">" (Left, Right : Clause_Number_Type) return Boolean;
	-- True if Left comes after Right in the collating order.

    function "<=" (Left, Right : Clause_Number_Type) return Boolean;
	-- True if Left comes before or is the same as Right in the
	-- collating order.

    function ">=" (Left, Right : Clause_Number_Type) return Boolean;
	-- True if Left comes after or is the same as Right in the
	-- collating order.

    procedure Add (Title : in Title_Type;
		   Level : in Level_Type;
		   Clause_Number : in Clause_Number_Type;
                   Version : in ARM_Contents.Change_Version_Type := '0');
	-- Add a section or clause to the contents. It has the specified
	-- characteristics.

    procedure Add_Old (Old_Title : in Title_Type;
		       Level : in Level_Type;
		       Clause_Number : in Clause_Number_Type;
                       Version : in ARM_Contents.Change_Version_Type := '0');
	-- Add an old title for a section or clause to the contents. It has
	-- the specified characteristics; the version is the version for which
	-- it first was present in the document.

    function Make_Clause_Number (Level : in Level_Type;
		   Clause_Number : in Clause_Number_Type) return String;
	-- Returns a properly formatted Section or clause number reference.
	-- Note that an unnumbered section returns a number with a
	-- Section_Number of zero (for sorting purposes).

    procedure Make_Clause (Clause_String : in String;
			   Clause_Number : out Clause_Number_Type);
	-- Returns the clause number for a properly formatted Section or
	-- clause string.

    function Lookup_Clause_Number (Title : in Title_Type) return String;
	-- Given the title of a clause, returns the formatted Section or
	-- clause number reference for that title. The Title must match
	-- exactly, except for case. Raises Not_Found_Error if not found.

    function Lookup_Level (Title : in Title_Type) return Level_Type;
	-- Given the title of a clause, returns the level for that title. The Title must match
	-- exactly, except for case. Raises Not_Found_Error if not found.

    function Lookup_Title (Level : in Level_Type;
		           Clause_Number : in Clause_Number_Type) return Title_Type;
	-- Given the level and clause numbers, return the appropriate
	-- title. Raises Not_Found_Error if not found.

    function Lookup_Old_Title (Level : in Level_Type;
		   Clause_Number : in Clause_Number_Type) return Title_Type;
	-- Given the level and clause numbers, return the appropriate
	-- old title. Calls Lookup_Title if not found (thus returning the
	-- regular (new) title.

    function Previous_Clause (Clause : in String) return String;
	-- Returns the string of the previous clause (in the table of contents)
	-- for the properly formatted clause string Clause.
	-- Raises Not_Found_Error if not found.

    function Next_Clause (Clause : in String) return String;
	-- Returns the string of the next clause (in the table of contents)
	-- for the properly formatted clause string Clause.
	-- Raises Not_Found_Error if not found.

    function Parent_Clause (Clause : in String) return String;
        -- Returns the string of the parent clause (in the table of contents)
        -- for the properly formatted clause string Clause.
        --
        -- Result is a null string if Clause is a top level clause;
        -- Section, Unnumbered_Section, Normative_Annex,
        -- Informative_Annex, Plain_Annex.

    generic
	with procedure Operate (Title : in Title_Type;
		   Level : in Level_Type;
		   Clause_Number : in Clause_Number_Type;
                   Version : in ARM_Contents.Change_Version_Type;
		   Quit : out Boolean) is <>;
    procedure For_Each;
	-- Call Operate for each title in the contents, in the order that
	-- they were added to the contents. If the Quit parameter to Operate
	-- is True when Operate returns, the iteration is abandoned.

end ARM_Contents;
