with ARM_Output,
     ARM_Contents,
     Ada.Text_IO;
-- private
with Ada.Strings.Unbounded;
package ARM_RTF is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package defines the RTF output object.
    -- Output objects are responsible for implementing the details of
    -- a particular format.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2002, 2004, 2005, 2006, 2007, 2009, 2011
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
    --  5/18/00 - RLB - Created package.
    --  5/22/00 - RLB - Added Includes_Changes to Create.
    --  5/23/00 - RLB - Added Set_Column and New_Column.
    --		      - Added Tab_Info and Tab_Stops.
    --  5/24/00 - RLB - Added Location to Text_Format.
    --		- RLB - Added No_Breaks and Keep_with_Next to Start_Paragraph.
    --  5/25/00 - RLB - Added Big_Files to Create. Added Justification.
    --		- RLB - Added Separator_Lines and TOC routines.
    --  5/26/00 - RLB - Added table operations.
    --  6/ 2/00 - RLB - Added Soft_Line_Break.
    --  8/ 2/00 - RLB - Added Soft_Hyphen_Break.
    --  8/ 7/00 - RLB - Added Leading flag to Start_Paragraph.
    --  8/17/00 - RLB - Replaced "Leading" by "Space_After".
    --  8/22/00 - RLB - Added Revised_Clause_Header.
    --  7/18/02 - RLB - Removed Document parameter from Create, replaced by
    --			three strings and For_ISO boolean.
    --		- RLB - Added AI_Reference.
    --		- RLB - Added Change_Version_Type and uses.
    --  9/10/04 - RLB - Added "Both" to possible changes to handle
    --			replacement of changed text.
    --  9/14/04 - RLB - Moved Change_Version_Type to ARM_Contents.
    --  5/27/05 - RLB - Added arbitrary Unicode characters.
    --  1/11/06 - RLB - Eliminated dispatching Create in favor of tailored
    --			versions.
    --  1/13/06 - RLB - Added new Link operations.
    --  2/ 8/06 - RLB - Added additional parameters to the table command.
    --  2/10/06 - RLB - Added even more additional parameters to the
    --			table command.
    --		- RLB - Added picture command.
    --  3/30/06 - RLB - Added shape id counter.
    --  9/21/06 - RLB - Added Body_Font.
    --  9/25/06 - RLB - Added Last_Column_Width to Start_Table.
    -- 10/13/06 - RLB - Added Local_Link_Start and Local_Link_End to allow
    --			formatting in the linked text.
    --  2/ 9/07 - RLB - Changed comments on AI_Reference.
    --  2/13/07 - RLB - Revised to separate style and indent information
    --			for paragraphs.
    -- 12/19/07 - RLB - Added limited colors to Text_Format.
    --  5/ 4/09 - RLB - Added footer commands.
    --  5/ 6/09 - RLB - Added version names.
    -- 10/18/11 - RLB - Changed to GPLv3 license.
    -- 10/25/11 - RLB - Added old insertion version to Revised_Clause_Header.

    type RTF_Output_Type is new ARM_Output.Output_Type with private;

    type Page_Size is (A4, Letter, Ada95, Half_Letter);
	-- A4 is standard European letter size.
	-- Letter is standard American letter size (8.5x11).
	-- Half_Letter is standard America half size (5.5x8.5).
	-- Ada95 is the size of the existing Ada 95 standard (7x9).

    type Serif_Fonts is (Times_New_Roman, Souvenir);
    type Sans_Serif_Fonts is (Arial, Helvetica);

    procedure Create (Output_Object : in out RTF_Output_Type;
		      Page_Size : in ARM_RTF.Page_Size;
		      Includes_Changes : in Boolean;
		      Big_Files : in Boolean;
		      Primary_Sans_Serif_Font : in Sans_Serif_Fonts := Arial;
		      Primary_Serif_Font : in Serif_Fonts := Times_New_Roman;
		      Body_Font : in ARM_Output.Font_Family_Type := ARM_Output.Roman;
		      File_Prefix : in String;
		      Header_Prefix : in String := "";
		      Footer_Use_Date : in Boolean;
		      Footer_Use_Clause_Name : in Boolean;
		      Footer_Use_ISO_Format : in Boolean;
		      Footer_Text : in String := "";
		      Version_Names : in ARM_Contents.Versioned_String;
		      Title : in String := "");
	-- Create an Output_Object for a document with the specified page
	-- size. Changes from the base document are included if
	-- Includes_Changes is True (otherwise no revisions are generated).
	-- Generate a few large output files if
	-- Big_Files is True; otherwise generate smaller output files.
	-- The prefix of the output file names is File_Prefix - this
	-- should be no more then 4 characters allowed in file names.
	-- The title of the document is Title.
	-- The header prefix appears in the header (if any) before the title,
	-- separated by a dash.
	-- The footer consists of the page number, the date if Footer_Use_Date
	-- is true, and the clause name if Footer_Use_Clase_Name is True, and
	-- the Footer_Text otherwise. The font and size of the footer is as
	-- an ISO standard if Footer_Use_ISO_Format is True, and as the
	-- Ada Reference Manual otherwise.
	-- The primary font used for the Sans_Serif text, and for the Serif
	-- text, is as specified.
	-- Which font is used for the body is specified by Body_Font.
	-- The author names of the various versions is specified by the
	-- Version_Names.

    procedure Close (Output_Object : in out RTF_Output_Type);
	-- Close an Output_Object. No further output to the object is
	-- allowed after this call.


    procedure Section (Output_Object : in out RTF_Output_Type;
		       Section_Title : in String;
		       Section_Name : in String);
	-- Start a new section. The title is Section_Title (this is
	-- intended for humans). The name is Section_Name (this is
	-- intended to be suitable to be a portion of a file name).

    procedure Set_Columns (Output_Object : in out RTF_Output_Type;
			   Number_of_Columns : in ARM_Output.Column_Count);
	-- Set the number of columns.
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Start_Paragraph (Output_Object : in out RTF_Output_Type;
			       Style     : in ARM_Output.Paragraph_Style_Type;
			       Indent    : in ARM_Output.Paragraph_Indent_Type;
			       Number    : in String;
			       No_Prefix : in Boolean := False;
			       Tab_Stops : in ARM_Output.Tab_Info := ARM_Output.NO_TABS;
			       No_Breaks : in Boolean := False;
			       Keep_with_Next : in Boolean := False;
			       Space_After : in ARM_Output.Space_After_Type
				   := ARM_Output.Normal;
			       Justification : in ARM_Output.Justification_Type
				   := ARM_Output.Default);
	-- Start a new paragraph. The style and indent of the paragraph is as
	-- specified. The (AA)RM paragraph number (which might include update
	-- and version numbers as well: [12.1/1]) is Number. If the format is
	-- a type with a prefix (bullets, hangining items), the prefix is
	-- omitted if No_Prefix is true. Tab_Stops defines the tab stops for
	-- the paragraph. If No_Breaks is True, we will try to avoid page breaks
	-- in the paragraph. If Keep_with_Next is true, we will try to avoid
	-- separating this paragraph and the next one. (These may have no
	-- effect in formats that don't have page breaks). Space_After
	-- specifies the amount of space following the paragraph. Justification
	-- specifies the text justification for the paragraph. Not_Valid_Error
	-- is raised if Tab_Stops /= NO_TABS for a hanging or bulleted format.

    procedure End_Paragraph (Output_Object : in out RTF_Output_Type);
	-- End a paragraph.

    procedure Category_Header (Output_Object : in out RTF_Output_Type;
			       Header_Text : String);
	-- Output a Category header (that is, "Legality Rules",
	-- "Dynamic Semantics", etc.)
	-- (Note: We did not use a enumeration here to insure that these
	-- headers are spelled the same in all output versions).
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Clause_Header (Output_Object : in out RTF_Output_Type;
			     Header_Text : in String;
			     Level : in ARM_Contents.Level_Type;
			     Clause_Number : in String;
			     No_Page_Break : in Boolean := False);
	-- Output a Clause header. The level of the header is specified
	-- in Level. The Clause Number is as specified.
	-- These should appear in the table of contents.
	-- For hyperlinked formats, this should generate a link target.
	-- If No_Page_Break is True, suppress any page breaks.
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Revised_Clause_Header (Output_Object : in out RTF_Output_Type;
			     New_Header_Text : in String;
			     Old_Header_Text : in String;
			     Level : in ARM_Contents.Level_Type;
			     Clause_Number : in String;
			     Version : in ARM_Contents.Change_Version_Type;
			     Old_Version : in ARM_Contents.Change_Version_Type;
        		     No_Page_Break : in Boolean := False);
	-- Output a revised clause header. Both the original and new text will
	-- be output. The level of the header is specified in Level. The Clause
	-- Number is as specified.
	-- These should appear in the table of contents.
	-- For hyperlinked formats, this should generate a link target.
	-- Version is the insertion version of the new text; Old_Version is
	-- the insertion version of the old text.
	-- If No_Page_Break is True, suppress any page breaks.
	-- Raises Not_Valid_Error if in a paragraph.

    procedure TOC_Marker (Output_Object : in out RTF_Output_Type;
			  For_Start : in Boolean);
	-- Mark the start (if For_Start is True) or end (if For_Start is
	-- False) of the table of contents data. Output objects that
	-- auto-generate the table of contents can use this to do needed
	-- actions.

    procedure New_Page (Output_Object : in out RTF_Output_Type;
			Kind : ARM_Output.Page_Kind_Type := ARM_Output.Any_Page);
	-- Output a page break.
	-- Note that this has no effect on non-printing formats.
	-- Any_Page breaks to the top of the next page (whatever it is);
	-- Odd_Page_Only breaks to the top of the odd-numbered page;
	-- Soft_Page allows a page break but does not force one (use in
	-- "No_Breaks" paragraphs.)
	-- Raises Not_Valid_Error if in a paragraph if Kind = Any_Page or
	-- Odd_Page, and if not in a paragraph if Kind = Soft_Page.

    procedure New_Column (Output_Object : in out RTF_Output_Type);
	-- Output a column break.
	-- Raises Not_Valid_Error if in a paragraph, or if the number of
	-- columns is 1.

    procedure Separator_Line (Output_Object : in out RTF_Output_Type;
			      Is_Thin : Boolean := True);
	-- Output a separator line. It is thin if "Is_Thin" is true.
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Start_Table (Output_Object : in out RTF_Output_Type;
			   Columns : in ARM_Output.Column_Count;
			   First_Column_Width : in ARM_Output.Column_Count;
			   Last_Column_Width : in ARM_Output.Column_Count;
			   Alignment : in ARM_Output.Column_Text_Alignment;
			   No_Page_Break : in Boolean;
			   Has_Border : in Boolean;
			   Small_Text_Size : in Boolean;
			   Header_Kind : in ARM_Output.Header_Kind_Type);
	-- Starts a table. The number of columns is Columns; the first
	-- column has First_Column_Width times the normal column width, and
	-- the last column has Last_Column_Width times the normal column width.
	-- Alignment is the horizontal text alignment within the columns.
	-- No_Page_Break should be True to keep the table intact on a single
	-- page; False to allow it to be split across pages.
	-- Has_Border should be true if a border is desired, false otherwise.
	-- Small_Text_Size means that the contents will have the AARM size;
	-- otherwise it will have the normal size.
	-- Header_Kind determines whether the table has headers.
	-- This command starts a paragraph; the entire table is a single
	-- paragraph. Text will be considered part of the caption until the
	-- next table marker call.
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Table_Marker (Output_Object : in out RTF_Output_Type;
			    Marker : in ARM_Output.Table_Marker_Type);
	-- Marks the end of an entity in a table.
	-- If Marker is End_Caption, the table caption ends and the
	--	future text is part of the table header.
	-- If Marker is End_Header, the table header ends and the
	--	future text is part of the table body.
	-- If Marker is End_Row, a row in the table is completed, and another
	--	row started.
	-- If Marker is End_Item, an item in the table header or body is ended,
	--	and another started.
	-- If Marker is End_Table, the entire table is finished.
	-- Raises Not_Valid_Error if not in a table.

    -- Text output: These are only allowed after a Start_Paragraph and
    -- before any End_Paragraph. Raises Not_Valid_Error if not in a paragraph,
    -- or another error.

    procedure Ordinary_Text (Output_Object : in out RTF_Output_Type;
			     Text : in String);
	-- Output ordinary text.
	-- The text must end at a word break, never in the middle of a word.

    procedure Ordinary_Character (Output_Object : in out RTF_Output_Type;
			          Char : in Character);
	-- Output an ordinary character.
	-- Spaces will be used to break lines as needed.

    procedure Hard_Space (Output_Object : in out RTF_Output_Type);
	-- Output a hard space. No line break should happen at a hard space.

    procedure Line_Break (Output_Object : in out RTF_Output_Type);
	-- Output a line break. This does not start a new paragraph.
	-- This corresponds to a "<BR>" in RTF.

    procedure Index_Line_Break (Output_Object : in out RTF_Output_Type;
				Clear_Keep_with_Next : in Boolean);
	-- Output a line break for the index. This does not start a new
	-- paragraph in terms of spacing. This corresponds to a "<BR>"
	-- in HTML. If Clear_Keep_with_Next is true, insure that the next
	-- line does not require the following line to stay with it.
	-- Raises Not_Valid_Error if the paragraph is not in the index format.

    procedure Soft_Line_Break (Output_Object : in out RTF_Output_Type);
	-- Output a soft line break. This is a place (in the middle of a
	-- "word") that we allow a line break. It is usually used after
	-- underscores in long non-terminals.

    procedure Soft_Hyphen_Break (Output_Object : in out RTF_Output_Type);
	-- Output a soft line break, with a hyphen. This is a place (in the middle of
	-- a "word") that we allow a line break. If the line break is used,
	-- a hyphen will be added to the text.

    procedure Tab (Output_Object : in out RTF_Output_Type);
	-- Output a tab, inserting space up to the next tab stop.
	-- Raises Not_Valid_Error if the paragraph was created with
	-- Tab_Stops = ARM_Output.NO_TABS.

    procedure Special_Character (Output_Object : in out RTF_Output_Type;
			         Char : in ARM_Output.Special_Character_Type);
	-- Output an special character.

    procedure Unicode_Character (Output_Object : in out RTF_Output_Type;
			         Char : in ARM_Output.Unicode_Type);
	-- Output a Unicode character, with code position Char.

    procedure End_Hang_Item (Output_Object : in out RTF_Output_Type);
	-- Marks the end of a hanging item. Call only once per paragraph.
	-- Raises Not_Valid_Error if the paragraph style is not in
	-- Text_Prefixed_Style_Subtype, or if this has already been
	-- called for the current paragraph, or if the paragraph was started
	-- with No_Prefix = True.

    procedure Text_Format (Output_Object : in out RTF_Output_Type;
			   Format : in ARM_Output.Format_Type);
	-- Change the text format so that all of the properties are as specified.
	-- Note: Changes to these properties ought be stack-like; that is,
	-- Bold on, Italic on, Italic off, Bold off is OK; Bold on, Italic on,
	-- Bold off, Italic off should be avoided (as separate commands).

    procedure Clause_Reference (Output_Object : in out RTF_Output_Type;
				Text : in String;
				Clause_Number : in String);
	-- Generate a reference to a clause in the standard. The text of
	-- the reference is "text", and the number of the clause is
	-- Clause_Number. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.

    procedure Index_Target (Output_Object : in out RTF_Output_Type;
			    Index_Key : in Natural);
	-- Generate a index target. This marks the location where an index
	-- reference occurs. Index_Key names the index item involved.
	-- For hyperlinked formats, this should generate a link target;
	-- for other formats, nothing is generated.

    procedure Index_Reference (Output_Object : in out RTF_Output_Type;
			       Text : in String;
			       Index_Key : in Natural;
			       Clause_Number : in String);
	-- Generate a reference to an index target in the standard. The text
	-- of the reference is "Text", and Index_Key and Clause_Number denotes
	-- the target. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.

    procedure DR_Reference (Output_Object : in out RTF_Output_Type;
			    Text : in String;
			    DR_Number : in String);
	-- Generate a reference to an DR from the standard. The text
	-- of the reference is "Text", and DR_Number denotes
	-- the target. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.

    procedure AI_Reference (Output_Object : in out RTF_Output_Type;
			    Text : in String;
			    AI_Number : in String);
	-- Generate a reference to an AI from the standard. The text
	-- of the reference is "Text", and AI_Number denotes
	-- the target (in unfolded format). For hyperlinked formats, this should
	-- generate a link; for other formats, the text alone is generated.

    procedure Local_Target (Output_Object : in out RTF_Output_Type;
			    Text : in String;
			    Target : in String);
	-- Generate a local target. This marks the potential target of local
	-- links identified by "Target". Text is the text of the target.
	-- For hyperlinked formats, this should generate a link target;
	-- for other formats, only the text is generated.

    procedure Local_Link (Output_Object : in out RTF_Output_Type;
			  Text : in String;
			  Target : in String;
			  Clause_Number : in String);
	-- Generate a local link to the target and clause given.
	-- Text is the text of the link.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure Local_Link_Start (Output_Object : in out RTF_Output_Type;
				Target : in String;
				Clause_Number : in String);
	-- Generate a local link to the target and clause given.
	-- The link will surround text until Local_Link_End is called.
	-- Local_Link_End must be called before this routine can be used again.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure Local_Link_End (Output_Object : in out RTF_Output_Type;
			      Target : in String;
			      Clause_Number : in String);
	-- End a local link for the target and clause given.
	-- This must be in the same paragraph as the Local_Link_Start.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure URL_Link (Output_Object : in out RTF_Output_Type;
			Text : in String;
			URL : in String);
	-- Generate a link to the URL given.
	-- Text is the text of the link.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure Picture  (Output_Object : in out RTF_Output_Type;
			Name  : in String;
			Descr : in String;
			Alignment : in ARM_Output.Picture_Alignment;
			Height, Width : in Natural;
			Border : in ARM_Output.Border_Kind);
	-- Generate a picture.
	-- Name is the (simple) file name of the picture; Descr is a
	-- descriptive name for the picture (it will appear in some web
	-- browsers).
	-- We assume that it is a .PNG or .JPG and that it will be present
	-- in the same directory as the output files.
	-- Alignment specifies the picture alignment.
	-- Height and Width specify the picture size in pixels.
	-- Border specifies the kind of border.

private

    subtype Prefix_String is String(1..4);
    type RTF_Output_Type is new ARM_Output.Output_Type with record
	Is_Valid : Boolean := False;
	Is_In_Paragraph  : Boolean := False;
	Paragraph_Style  : ARM_Output.Paragraph_Style_Type := ARM_Output.Normal;
	Paragraph_Indent : ARM_Output.Paragraph_Indent_Type := 0;
	Had_Prefix : Boolean := False; -- If in paragraph, value of (not No_Prefix).
	Wrote_into_Section : Boolean := False; -- Have we written into the
		-- current section yet?
	Column_Count : ARM_Output.Column_Count := 1; -- Number of columns in current section.
	Output_File : Ada.Text_IO.File_Type;
	File_Prefix : Prefix_String; -- Blank padded.
	Title : Ada.Strings.Unbounded.Unbounded_String;
	Header_Prefix : Ada.Strings.Unbounded.Unbounded_String;
        Footer_Use_Date : Boolean;
        Footer_Use_Clause_Name : Boolean;
        Footer_Use_ISO_Format : Boolean;
	Footer_Text : Ada.Strings.Unbounded.Unbounded_String;
	Page_Size : ARM_RTF.Page_Size;
	Version_Names : ARM_Contents.Versioned_String;
	Includes_Changes : Boolean;
	Big_Files : Boolean; -- For RTF, this means to generate a single monster file.
	Primary_Sans_Serif_Font : Sans_Serif_Fonts;
	Primary_Serif_Font : Serif_Fonts;
	Body_Font : ARM_Output.Font_Family_Type;
	For_ISO : Boolean;
	Char_Count : Natural := 0; -- Characters on current line.
	Saw_Hang_End : Boolean := False; -- If we are in a hanging paragraph,
			       -- have we seen the end of the hanging part yet?
	Current_Space_After : ARM_Output.Space_After_Type := ARM_Output.Normal;
				-- The value of Space_After for the current
				-- paragraph.
	Prefix_Large_Char_Count : Natural := 0;
		-- If we're in a hanging paragraph, and Saw_Hang_End is False,
		-- this is a count of the large (capitals, mostly) characters
		-- visible in the prefix.
	Is_Bold : Boolean; -- Is the text currently bold?
	Is_Italic : Boolean; -- Is the text current italics?
	Font : ARM_Output.Font_Family_Type; -- What is the current font family?
	Size : ARM_Output.Size_Type; -- What is the current relative size?
	Real_Size : Natural; -- What is the current size in halfpoints?
	Color : ARM_Output.Color_Type; -- What is the current text color?
	Change : ARM_Output.Change_Type := ARM_Output.None;
	Version : ARM_Contents.Change_Version_Type := '0';
	Added_Version : ARM_Contents.Change_Version_Type := '0';
	Location : ARM_Output.Location_Type := ARM_Output.Normal;
	Tab_Stops : ARM_Output.Tab_Info := ARM_Output.NO_TABS;
	-- Tables:
	Is_In_Table : Boolean := False; -- Are we processing a table?
	Table_Width : Natural := 0; -- The width of the table, in twips.
	Table_Indent : Natural := 0; -- The indent of the table, in twips.
	Table_Column_Width : Natural := 0; -- The column width of the table, in twips.
	Table_First_Column_Mult : ARM_Output.Column_Count := 1; -- The multiple of the first column.
	Table_Last_Column_Mult : ARM_Output.Column_Count := 1; -- The multiple of the last column.
	Table_Alignment : ARM_Output.Column_Text_Alignment := ARM_Output.Center_All;
	Table_No_Page_Break : Boolean := False; -- Is a page break allowed in the table?
	Table_Has_Border : Boolean := False; -- Does the table have a border?
	Table_Has_Small_Text : Boolean := False; -- Does the table have small text?
	-- Pictures:
	Last_Shape_Id : Natural := 1024; -- Shape ids.
    end record;

end ARM_RTF;
