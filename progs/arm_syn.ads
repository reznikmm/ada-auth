package ARM_Syntax is

    --
    -- Ada reference manual formatter.
    --
    -- This package contains the database to collect the syntax summary and
    -- cross-reference.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2006 AXE Consultants.
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
    --  5/17/00 - RLB - Created package.
    --  5/26/00 - RLB - Added a Tabset parameter.
    --  6/22/06 - RLB - Added additional information to improve the links
    --			and to be able to use the Ada 83 format for the
    --			cross-reference table.
    -- 10/13/06 - RLB - Added Defined flag to cross-references to eliminate
    --			junk errors from not-quite-non-terminals.

    procedure Create;
	-- Initialize the syntax database.

    procedure Destroy;
	-- Destroy the syntax database.

    procedure Insert_Rule (
	For_Clause : in String;
	Rule : in String;
	Tabset : in String := "");
	-- Add a rule for the syntax summary. The rule appears in For_Clause.
	-- Tabset provides any needed tab settings.

    subtype Target_Type is String (1..5);

    procedure Add_Non_Terminal (
	NT_Name : in String;
	For_Clause : in String;
	Link_Target : out ARM_Syntax.Target_Type);
	-- Add a non-terminal to the syntax list. Returns a new Link_Target
	-- for the Non-Terminal.

    procedure Add_Xref (
	Name : in String;
	Used_In : in String;
	Clause : in String;
	Defined : in Boolean);
	-- Add a cross-reference entry.
	-- The item referenced is Name, and it is referenced in the production
	-- for Used_In, in Clause. It is a defined non-terminal if Defined
	-- is True (thus it can be linked).

    function Non_Terminal_Clause (NT_Name : in String) return String;
	-- Return the clause where NT_Name is declared.
	-- Returns "" if NT_Name is not a declared Non_Terminal.

    function Non_Terminal_Link_Target (NT_Name : in String) return Target_Type;
	-- Return the link target for NT_Name.
	-- Returns "     " if NT_Name is not a declared Non_Terminal.

    generic
	with procedure Format_Text (Text : in String;
				    Text_Name : in String);
    procedure Report;
	-- Output the fully formatted syntax summary to the
	-- "Format_Text" routine. "Format_Text" allows all commands
	-- for the full formatter. (Text_Name is an identifying name
	-- for error messages).

    generic
	with procedure Format_Text (Text : in String;
				    Text_Name : in String);
    procedure XRef;
	-- Output the fully formatted syntax cross-reference to the
	-- "Format_Text" routine. "Format_Text" allows all commands
	-- for the full formatter. (Text_Name is an identifying name
	-- for error messages).

end ARM_Syntax;


