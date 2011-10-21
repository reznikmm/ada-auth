with ARM_Output,
     ARM_Contents;
--private -- Ada 2005
with ARM_Input,
     ARM_Database,
     ARM_Subindex;
package ARM_Format is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package contains the routines to parse the input files, and
    -- determine what to output.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2002, 2004, 2005, 2006, 2007, 2010, 2011
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
    --  7/18/02 - RLB - Moved document type here.
    --          - RLB - Added Changes_Only and versioning for individual changes.
    --  9/10/04 - RLB - Added support for nested changes.
    --  9/14/04 - RLB - Moved Change_Version_Type to ARM_Contents.
    -- 11/03/04 - RLB - Added Nested_X2_Bulleted.
    -- 11/16/04 - RLB - Added Attr_Prefix_Text_Change_Kind.
    -- 12/06/04 - RLB - Added reference chain for Format_Type.
    -- 12/11/04 - RLB - Increased Syntax_NT length.
    -- 10/17/05 - RLB - Added Glossary change items.
    -- 10/28/05 - RLB - Added Language-Define subindexes.
    --  1/12/06 - RLB - Replaced "Document" with a number of new more general
    --			properties.
    --  1/16/06 - RLB - Added "Unnumbered_Section" counter, so we can assign
    --			names without special cases.
    --  1/18/06 - RLB - Added "Example_Font".
    --  9/22/06 - RLB - Added "Use_ISO_2004_Note_Format".
    --		- RLB - Revised to use Clause_Number_Type.
    --  9/25/06 - RLB - Added "Use_ISO_2004_Contents_Format".
    -- 10/04/06 - RLB - Added "Use_ISO_2004_List_Format".
    --  2/ 5/07 - RLB - Added Usage_Note for ASIS, and renamed Wide paragraph
    --			kinds.
    --  2/13/07 - RLB - Redid output formating to use an explict indent;
    --			added ChildExample.
    --  2/16/07 - RLB - Added Indent.
    --  2/19/07 - RLB - Added Title format.
    --  4/23/10 - RLB - Added Ada 2005 header.
    --  8/ 8/11 - RLB - Added Aspect DB.
    -- 10/18/11 - RLB - Changed to GPLv3 license.

    type Format_Type is tagged limited private;

    type Change_Kind is (Old_Only, New_Only, Changes_Only,
	Show_Changes, New_Changes);
	-- Which changes to show?
	-- If Old_Only, we will get the original Ada Reference Manual or AARM.
	-- If New_Only, we will get the reference documents with the updates
	-- up to the Change_Version specified included.
	-- If Changes_Only, we will get the reference documents with the updates
	-- up to the Change_Version specified included; and insertions and
	-- deletions will be shown for Change_Version.
	-- If Show_Changes, original RM text will be shown as deleted,
	-- and new RM text will be shown as inserted, up to and including the
	-- Change_Version specified.
	-- If New_Changes, original RM text removed, but new RM text will be
	-- shown as inserted, up to and including the Change_Version specified.
	-- In all cases, changes with versions newer than Change_Version are
	-- ignored. Thus Change_Version = '0' is the same as Old_Only no
	-- matter what command is given.

    procedure Create (Format_Object : in out Format_Type;
		      Changes : in ARM_Format.Change_Kind;
		      Change_Version : in ARM_Contents.Change_Version_Type;
		      Display_Index_Entries : in Boolean;
		      Include_Annotations : in Boolean;
		      Include_ISO : in Boolean;
		      Link_Non_Terminals : in Boolean;
		      Number_Paragraphs : in Boolean;
		      Examples_Font : in ARM_Output.Font_Family_Type;
		      Use_ISO_2004_Note_Format : in Boolean;
		      Use_ISO_2004_Contents_Format : in Boolean;
		      Use_ISO_2004_List_Format : in Boolean);
	-- Initialize an input object. Changes and Change_Version determine
	-- which changes should be displayed. If Display_Index_Entries is True,
	-- index entries will be printed in the document; otherwise, they
	-- will not generate any visible text (although they might generate
	-- a link anchor). If Include_Annotations is True, annotations (AARM
	-- text) will be included in the output; otherwise it will not be.
	-- If Include_ISO is True, ISOOnly text will be included in the output
	-- (and NotISO text will not); otherwise the reverse is true.
	-- If Link_Non_Terminals is True, links will be generated for
	-- each Non_Terminal, linking it to its definition.
	-- If Number_Paragraphs is true, paragraphs will be numbered (per
	-- subclause); otherwise they will not be.
	-- Example_Font specifies the font that examples will be set in.
	-- If Use_ISO_2004_Note_Format is true, that format will be used;
	-- else the Ada95 standard's format will be used for notes.
	-- If Use_ISO_2004_Contents_Format is true, that format will be used;
	-- else the Ada95 standard's format will be used for the table of contents.
	-- If Use_ISO_2004_List_Format is true, then lists will be lettered;
	-- else the Ada95 standard's numbering format will be used for
	-- enumerated lists.

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

    procedure Write_Table_of_Contents (
		        Format_Object : in out Format_Type;
		       Output_Object : in out ARM_Output.Output_Type'Class);
	-- Writes the table of contents for the document. (It will have
	-- a section name of "TOC").

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
		      No_Annotations : in Boolean);
	-- Format the contents of Text, writing the results to
	-- Output_Object. (Output_Object uses dispatching calls to provide
	-- the correct formatting). Text is assumed to be a component of
	-- a larger section. Text_Name is an identifying name for error messages.
	-- If No_Annotations is true, we don't want any annotations even if we
	-- are generating a document with annotations.

private
    type Paragraph_Type is (Plain, Introduction,
	Language_Design, -- AARM-only.
	Syntax, Resolution, Legality,
	Static_Semantics, Link_Time, Run_Time, Bounded_Errors,
	Erroneous, Requirements, Documentation, Metrics, Permissions, Advice,
	Notes, Single_Note, Examples,
	Ada83_Inconsistencies, Ada83_Incompatibilities, -- AARM-only.
	Ada83_Extensions, Ada83_Wording, -- AARM-only.
	Ada95_Inconsistencies, Ada95_Incompatibilities, -- AARM-only.
	Ada95_Extensions, Ada95_Wording, -- AARM-only.
	Ada2005_Inconsistencies, Ada2005_Incompatibilities, -- AARM-only.
	Ada2005_Extensions, Ada2005_Wording, -- AARM-only.
	Element_Ref, Child_Ref, Usage_Note, -- For ASIS (AASIS-only).
	-- AARM annotations (no headers)
	Reason, Ramification, Proof, Imp_Note, Corr_Change, Discussion,
	Honest, Glossary_Marker, Bare_Annotation,
	-- Format only:
	Wide_Above, Example_Text, Child_Example_Text,
	Indented_Example_Text, Code_Indented, Indent, Bulleted, Nested_Bulleted,
        Nested_X2_Bulleted,
	Display, Syntax_Display, Syntax_Indented, Syntax_Production,
	Enumerated, Nested_Enumerated, Hanging_Indented, Title, In_Table);

    type Reference;
    type Reference_Ptr is access Reference;
    type Reference is record
	Ref_Name : ARM_Input.Command_Name_Type;
	Ref_Len  : Natural; -- Length of the reference.
	Is_DR_Ref : Boolean; -- True for a DR reference; False for an AI reference.
        Next : Reference_Ptr;
    end record;

    type Format_Type is tagged limited record
	-- Document information:
	Changes : ARM_Format.Change_Kind; -- No Both here.
	Change_Version : ARM_Contents.Change_Version_Type;
	Display_Index_Entries : Boolean;
	Include_Annotations : Boolean;
	Include_ISO : Boolean;
	Link_Non_Terminals : Boolean;
	Number_Paragraphs : Boolean;
	Examples_Font : ARM_Output.Font_Family_Type;
	Use_ISO_2004_Note_Format : Boolean;
	Use_ISO_2004_Contents_Format : Boolean;
	Use_ISO_2004_List_Format : Boolean;

	-- Clause numbers:
	Clause_Number : ARM_Contents.Clause_Number_Type;
	     -- The current clause number (Section, clause, subclause, subsubclause).
	Unnumbered_Section : Natural; -- The current (if any) clause number
		-- for unnumbered sections.

	-- Paragraph format info:
	Next_Paragraph_Change_Kind : ARM_Database.Paragraph_Change_Kind_Type;
			     -- The change kind of the next paragraph. This is
			     -- reset to none after each paragraph.
	Next_Paragraph_Version : ARM_Contents.Change_Version_Type;
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
	References : Reference_Ptr := null;
			     -- Any references to generate at the start of the
			     -- next paragraph.

	-- Paragraph numbering info:
	Next_Note : Natural; -- The number of the next note. These are
			     -- per-section, not per-clause (unless ISO 2004 is set).
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
	Enumerated_Level : Natural;
			     -- Number of enumerated formats that we're in.
	Current_Paragraph_String : String (1 .. 10);
			     -- The current paragraph number string (only
			     -- valid if In_Paragraph is True).
	Current_Paragraph_Len : Natural;

	-- Text format info:
	Text_Format : ARM_Output.Format_Type; -- Holds the current text format.

	Style : ARM_Output.Paragraph_Style_Type; -- What is the current paragraph style?
	Indent : ARM_Output.Paragraph_Indent_Type; -- What is the current paragraph indent?
	In_Paragraph : Boolean; -- Are we currently in a paragraph?
	No_Start_Paragraph : Boolean; -- Did we suppress "Start_Paragraph"?
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
	Syntax_NT : String (1..80); -- Syntax non-terminal; used only during the
				    -- processing of the Syn command.
	Syntax_NT_Len : Natural := 0;
	Syntax_Tab : String (1..40); -- Syntax tab string; used only during the
				    -- processing of the Syn command.
	Syntax_Tab_Len : Natural := 0;

	-- Aspects:
	Aspect_Name : String (1..30); -- Aspect name text
	Aspect_Name_Len : Natural := 0;

	Aspect_DB : ARM_Database.Database_Type;

	-- Attributes:
	Prefix_Text : String (1..160) := "@b{NONE!}" & (10..160 => ' ');
	    -- This text is used as part of the attribute list text.
	    -- It is shared between multiple attributes, which is why it is
	    -- handled this way.
	Prefix_Text_Len : Natural := 9;

	Attr_Prefix : String (1..10); -- Attribute prefix text
	Attr_Prefix_Len : Natural := 0;
	Attr_Prefix_Change_Kind : ARM_Database.Paragraph_Change_Kind_Type;
	Attr_Prefix_Version : ARM_Contents.Change_Version_Type;
	Attr_Name : String (1..30); -- Attribute name text
	Attr_Name_Len : Natural := 0;
	Attr_Leading : Boolean := False; -- Attribute leading flag
	Attr_Change_Kind : ARM_Database.Paragraph_Change_Kind_Type;
	Attr_Prefix_Text_Change_Kind : ARM_Database.Paragraph_Change_Kind_Type;
	Attr_Version : ARM_Contents.Change_Version_Type;
	    -- The above ten items are used only when processing Attribute
	    -- and Attribute_Leading commands.

	Attr_DB : ARM_Database.Database_Type;

	-- Pragmas:
	Pragma_DB : ARM_Database.Database_Type;

	-- Glossary:
	Glossary_Term : String (1..50); -- Glossary term; used only when
	Glossary_Term_Len : Natural := 0; -- processing [Chg]ToGlossary[Also] commands.
	Glossary_Change_Kind : ARM_Database.Paragraph_Change_Kind_Type := ARM_Database.None;
			-- The change kind of the ToGlossary.
	Glossary_Version : ARM_Contents.Change_Version_Type;
			-- If the kind is not "None", this is the version
			-- number of the changed paragraph.
	Add_to_Glossary : Boolean;
			-- Add this item to the Glossary.
	Glossary_Displayed : Boolean;
			-- The text was displayed in the document.
	Glossary_DB : ARM_Database.Database_Type;

	-- Implementation advice:
	Impladv_DB : ARM_Database.Database_Type;

	-- Documentation requirements:
	Docreq_DB : ARM_Database.Database_Type;

	-- Implementation-defined:
	Impdef_DB : ARM_Database.Database_Type;
	-- The next four are used only during processing of ImplDef, ChgImplDef,
	--    ChgDocReq, ChgImplAdv, and ChgAspectDesc.
	Impdef_Change_Kind : ARM_Database.Paragraph_Change_Kind_Type;
			-- The change kind of the impldef.
	Impdef_Version : ARM_Contents.Change_Version_Type;
			-- If the kind is not "None", this is the version
			-- number of the changed paragraph.
	Impdef_Initial_Version : ARM_Contents.Change_Version_Type;
			-- This is the version number of the original paragraph.
	Impdef_Paragraph_String : String (1 .. 10); -- Paragraph number.
	Impdef_Paragraph_Len : Natural;

	-- Language-Defined entity subindexes:
	Package_Index : ARM_Subindex.Subindex_Type;
	Type_Index : ARM_Subindex.Subindex_Type;
	Subprogram_Index : ARM_Subindex.Subindex_Type;
	Exception_Index : ARM_Subindex.Subindex_Type;
	Object_Index : ARM_Subindex.Subindex_Type;

    end record;
end ARM_Format;
