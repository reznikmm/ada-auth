with ARM_Output,
     ARM_Contents,
     ARM_Database;
package ARM_Format is

    --
    -- Ada reference manual formatter.
    --
    -- This package contains the routines to parse the input files, and
    -- determine what to output.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2002, AXE Consultants.
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
    --  4/14/00 - RLB - Created base package.
    --  4/18/00 - RLB - Added additional information to Object;
    --			added Scan.
    --  4/24/00 - RLB - Added Change_Kind and Display_Index_Entries.
    --		- RLB - Added Change format.
    --  4/25/00 - RLB - Added Size.
    --  4/26/00 - RLB - Added paragraph formats.
    --  5/10/00 - RLB - Added additional paragraph kinds.
    --  5/11/00 - RLB - Added numbers for enumerated paragraphs.
    --  5/12/00 - RLB - Added attribute prefix text.
    --  5/15/00 - RLB - Split input from parsing/formatting.
    --  5/16/00 - RLB - Added database objects and Destroy.
    --  5/23/00 - RLB - Added tab stops.
    --  5/24/00 - RLB - Implemented subscript/superscript commands.
    --  5/28/00 - RLB - Implemented index operations.
    --  6/ 2/00 - RLB - Added Unit.
    --  8/ 2/00 - RLB - Added Leading and Syntax_Leading styles.
    --  8/ 4/00 - RLB - Added more new styles.
    --  8/ 7/00 - RLB - Added Leading flag, removed Leading styles.
    --  8/ 8/00 - RLB - Added Attr_Leading flag.
    --  8/16/00 - RLB - Added No_Para_Num flag, removed No_Pnum formats.
    --  8/17/00 - RLB - Changed Leading flag to Space_After.
    --  8/28/00 - RLB - Added flags for ChgAttribute and ChgImpldef commands.
    --  8/31/00 - RLB - Added the New_Changes change kind.
    --  9/26/00 - RLB - Added Syntax_Display format.
    --  6/17/02 - RLB - Added Ada95 changes sections.

    type Format_Type is tagged limited private;

    type Change_Kind is (Old_Only, New_Only, Show_Changes, New_Changes);
	-- Which changes to show? If Old_Only, we will get the original
	-- Ada Reference Manual or AARM. If New_Only, we will get the
	-- reference documents with the corrigendum updates included.
	-- If Show_Changes, original RM text will be shown as deleted,
	-- and new RM text will be shown as inserted.
	-- If New_Changes, original RM text removed, but new RM text will be
	-- shown as inserted.

    procedure Create (Format_Object : in out Format_Type;
		      Document : ARM_Output.Document_Type;
		      Changes : in ARM_Format.Change_Kind;
		      Display_Index_Entries : in Boolean);
	-- Initialize an input object. Document determines the type of
	-- document to create. Changes determines which changes should
	-- be displayed. If Display_Index_Entries is True, index entries
	-- will be printed in the document; otherwise, they will not generate
	-- any visible text (although they might generate a link anchor).

    procedure Destroy (Format_Object : in out Format_Type);
	-- Destroy a format object, releasing any resources.

    procedure Scan (Format_Object : in out Format_Type;
		    File_Name : in String;
		    Section_Number : in ARM_Contents.Section_Number_Type;
		    Starts_New_Section : in Boolean);
	-- Scans the contents for File_Name, determining the table of contents
	-- for the section. The results are written to the contents package.
	-- Starts_New_Section is True if the file starts a new section.
	-- Section_Number is the number (or letter) of the section.
	-- Note: We need this scanning pass so we can process @SeeSec,
	-- @SeeSecNum, and similar commands. For that, we need the full names
	-- of the sections and clauses.

    procedure Insert_Index (Format_Object : in out Format_Type);
	-- Insert the header for the index into the table of contents.

    procedure Write_Table_of_Contents (
		        Format_Object : in out Format_Type;
		       Output_Object : in out ARM_Output.Output_Type'Class);
	-- Writes the table of contents for the document. (It will have
	-- a section name of "TOC").

    procedure Write_Index (
		        Format_Object : in out Format_Type;
		       Output_Object : in out ARM_Output.Output_Type'Class);
	-- Writes the index for the document. (It will have
	-- a section name of "IDX").

    procedure Process (Format_Object : in out Format_Type;
		       File_Name : in String;
		       Output_Object : in out ARM_Output.Output_Type'Class;
		       Section_Name : in String;
		       Section_Number : in ARM_Contents.Section_Number_Type;
		       Starts_New_Section : in Boolean);
	-- Process the contents for File_Name, writing the results to
	-- Output_Object. (Output_Object uses dispatching calls to provide
	-- the correct formatting). Section_Name is the name of the section
	-- for this file. Starts_New_Section is True if the file starts
	-- a new section. Section_Number is the number (or letter) of the
	-- section. Values > 20 represent annex letters (21 => A, 22 => B, etc.)

    procedure Format (Format_Object : in out Format_Type;
		      Text : in String;
		      Output_Object : in out ARM_Output.Output_Type'Class;
		      Text_Name : in String;
		      No_AARM_Text : in Boolean);
	-- Format the contents of Text, writing the results to
	-- Output_Object. (Output_Object uses dispatching calls to provide
	-- the correct formatting). Text is assumed to be a component of
	-- a larger section. Text_Name is an identifying name for error messages.
	-- If No_AARM_Text is true, we don't want any AARM text even if we
	-- are generating the AARM.

private
    type Paragraph_Type is (Plain, Introduction,
	Language_Design, -- AARM-only.
	Syntax, Resolution, Legality,
	Static_Semantics, Link_Time, Run_Time, Bounded_Errors,
	Erroneous, Requirements, Documentation, Metrics, Permissions, Advice,
	Notes, Examples,
	Ada83_Inconsistencies, Ada83_Incompatibilities, -- AARM-only.
	Ada83_Extensions, Ada83_Wording, -- AARM-only.
	Ada95_Inconsistencies, Ada95_Incompatibilities, -- AARM-only.
	Ada95_Extensions, Ada95_Wording, -- AARM-only.
	-- AARM annotations (no headers)
	Reason, Ramification, Proof, Imp_Note, Corr_Change, Discussion,
	Honest, Glossary_Marker, Bare_Annotation,
	-- Format only:
	Wide, Example_Text,
	Indented_Example_Text, Code_Indented, Bulleted, Nested_Bulleted,
	Display, Syntax_Display, Syntax_Indented, Syntax_Production,
	Enumerated, Hanging_Indented, In_Table);

    type Format_Type is tagged limited record
	-- Document information:
	Document : ARM_Output.Document_Type;
	Changes : ARM_Format.Change_Kind;
	Display_Index_Entries : Boolean;

	-- Clause numbers:
	Section : ARM_Contents.Section_Number_Type; -- The current section number.
	Clause : Natural; -- The current clause number.
	Subclause : Natural; -- The current subclause number.

	-- Paragraph format info:
	Next_Paragraph_Change_Kind : ARM_Database.Paragraph_Change_Kind_Type;
			     -- The change kind of the next paragraph. This is
			     -- reset to none after each paragraph.
	Next_Paragraph_Version : Character;
			     -- If the kind is not "None", this is the version
			     -- number of the changed paragraph.
	Last_Paragraph_Subhead_Type : Paragraph_Type;
			     -- The last paragraph subhead generated.
	Next_Paragraph_Subhead_Type : Paragraph_Type;
			     -- The next paragraph subhead to generate (not
			     -- necessarily the same as Next_Paragraph_Format_Type).
			     -- This indicates the current paragraph type.
	Next_Paragraph_Format_Type : Paragraph_Type;
			     -- The format type of the next paragraph to
			     -- generate. We keep this separately so that the
			     -- first paragraph of a grouping can be in a
			     -- different format than the standard one, and
			     -- still generate a subheading.
	Paragraph_Tab_Stops : ARM_Output.Tab_Info := ARM_Output.NO_TABS;
			     -- The tab stops for the next paragraph.
	In_Bundle : Boolean := False;
			     -- Are we in a bundle?

	-- Paragraph numbering info:
	Next_Note : Natural; -- The number of the next note. These are
			     -- per-section, not per-clause.
	Next_Paragraph : Positive; -- The number of the next paragraph. These
			     -- are per-(sub)clause.
	Next_Insert_Para : Positive; -- The subnumber of the next inserted
			     -- paragraph.
	Next_AARM_Sub : Character; -- The letter of the next AARM subclause.
			     -- These are reset when the paragraph number
			     -- changes.
	Next_AARM_Insert_Para : Positive; -- The subnumber of the next inserted
			     -- AARM paragraph.
	Next_Enumerated_Num : Positive;
			     -- If the format is enumerated, this is the
			     -- number of the next paragraph.
	Current_Paragraph_String : String (1 .. 10);
			     -- The current paragraph number string (only
			     -- valid if In_Paragraph is True).
	Current_Paragraph_Len : Natural;

	-- Text format info:
	Is_Bold : Boolean; -- Is the text currently bold?
	Is_Italic : Boolean; -- Is the text currently italic?
	Font : ARM_Output.Font_Family_Type; -- What is the current font family?
	Size : ARM_Output.Size_Type; -- What is the current font size?
	Change : ARM_Output.Change_Type; -- What is the current kind of change?
	Location : ARM_Output.Location_Type; -- What is the current (vertical) location?
	Format : ARM_Output.Paragraph_Type; -- What is the current paragraph type?
	In_Paragraph : Boolean; -- Are we currently in a paragraph?
	No_Prefix : Boolean; -- Should we suppress any prefix on the next paragraph?
	No_Para_Num : Boolean; -- Should we suppress the paragraph number on the next paragraph?
	Keep_with_Next : Boolean; -- Should we force this paragraph to bind to the next
				  -- (disallowing a page break between)?
	Space_After : ARM_Output.Space_After_Type; -- Space following this
			     -- paragraph.
	No_Breaks : Boolean; -- Should we allow page breaks in this paragraph?
	In_Change : Boolean; -- Are we in a change region?
	Last_Non_Space : Boolean; -- Is the last character written into the
			-- paragraph a non-space? (If nothing has been
			-- written into the paragraph, then this is False).

	-- Indexing:
	Unit : String (1..60);  -- Unit for predefined definitions. Used only
				-- by a handful of indexing commands.
	Unit_Len : Natural := 0;

	-- Syntax:
	Syntax_NT : String (1..60); -- Syntax non-terminal; used only during the
				    -- processing of the Syn command.
	Syntax_NT_Len : Natural := 0;
	Syntax_Tab : String (1..40); -- Syntax tab string; used only during the
				    -- processing of the Syn command.
	Syntax_Tab_Len : Natural := 0;

	-- Attributes:
	Prefix_Text : String (1..160) := "@b{NONE!}" & (10..160 => ' ');
	    -- This text is used as part of the attribute list text.
	    -- It is shared between multiple attributes, which is why it is
	    -- handled this way.
	Prefix_Text_Len : Natural := 9;

	Attr_Prefix : String (1..10); -- Attribute prefix text
	Attr_Prefix_Len : Natural := 0;
	Attr_Prefix_Change_Kind : ARM_Database.Paragraph_Change_Kind_Type;
	Attr_Prefix_Version : Character;
	Attr_Name : String (1..30); -- Attribute name text
	Attr_Name_Len : Natural := 0;
	Attr_Leading : Boolean := False; -- Attribute leading flag
	Attr_Change_Kind : ARM_Database.Paragraph_Change_Kind_Type;
	Attr_Version : Character;
	    -- The above nine items are used only when processing Attribute
	    -- and Attribute_Leading commands.

	Attr_DB : ARM_Database.Database_Type;

	-- Pragmas:
	Pragma_DB : ARM_Database.Database_Type;

	-- Glossary:
	Glossary_Term : String (1..20); -- Glossary term; used only when
	Glossary_Term_Len : Natural := 0; -- processing ToGlossary[Also] commands.
	Glossary_DB : ARM_Database.Database_Type;

	-- Implementation-defined:
	Impdef_DB : ARM_Database.Database_Type;
	-- The next four are used only during processing of ImplDef and ChgImplDef.
	Impdef_Change_Kind : ARM_Database.Paragraph_Change_Kind_Type;
			-- The change kind of the impldef.
	Impdef_Version : Character;
			-- If the kind is not "None", this is the version
			-- number of the changed paragraph.
	Impdef_Paragraph_String : String (1 .. 10); -- Paragraph number.
	Impdef_Paragraph_Len : Natural;
    end record;
end ARM_Format;
