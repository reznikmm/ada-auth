with ARM_Output,
     ARM_Contents,
     Ada.Wide_Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;

package ARM_ReST is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package defines the text output object.
    -- Output objects are responsible for implementing the details of
    -- a particular format.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2002, 2004, 2005, 2006, 2007, 2011, 2012
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
    -- 10/15/20 - Max - Created base package.

    type ReST_Output_Type is new ARM_Output.Output_Type with private;

    procedure Create (Output_Object : in out ReST_Output_Type;
		      File_Prefix : in String;
		      Output_Path : in String;
		      Title : in String := "");
	-- Create an Output_Object for a document.
	-- The prefix of the output file names is File_Prefix - this
	-- should be no more then 5 characters allowed in file names.
	-- The result files will be written to Output_Path.
	-- The title of the document is Title.

    procedure Close (Output_Object : in out ReST_Output_Type);
	-- Close an Output_Object. No further output to the object is
	-- allowed after this call.


    procedure Section (Output_Object : in out ReST_Output_Type;
		       Section_Title : in String;
		       Section_Name : in String);
	-- Start a new section. The title is Section_Title (this is
	-- intended for humans). The name is Section_Name (this is
	-- intended to be suitable to be a portion of a file name).

    procedure Set_Columns (Output_Object : in out ReST_Output_Type;
			   Number_of_Columns : in ARM_Output.Column_Count);
	-- Set the number of columns.
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Start_Paragraph (Output_Object : in out ReST_Output_Type;
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

    procedure End_Paragraph (Output_Object : in out ReST_Output_Type);
	-- End a paragraph.

    procedure Category_Header (Output_Object : in out ReST_Output_Type;
			       Header_Text : String);
	-- Output a Category header (that is, "Legality Rules",
	-- "Dynamic Semantics", etc.)
	-- (Note: We did not use a enumeration here to insure that these
	-- headers are spelled the same in all output versions).
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Clause_Header (Output_Object     : in out ReST_Output_Type;
			     Header_Text       : in String;
			     Level	       : in ARM_Contents.Level_Type;
			     Clause_Number     : in String;
			     Top_Level_Subdivision_Name : in ARM_Output.Top_Level_Subdivision_Name_Kind;
			     No_Page_Break     : in Boolean := False);
	-- Output a Clause header. The level of the header is specified
	-- in Level. The Clause Number is as specified; the top-level (and
	-- other) subdivision names are as specified. These should appear in
	-- the table of contents.
	-- For hyperlinked formats, this should generate a link target.
	-- If No_Page_Break is True, suppress any page breaks.
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Revised_Clause_Header
			    (Output_Object     : in out ReST_Output_Type;
			     New_Header_Text   : in String;
			     Old_Header_Text   : in String;
			     Level	       : in ARM_Contents.Level_Type;
			     Clause_Number     : in String;
			     Version	       : in ARM_Contents.Change_Version_Type;
			     Old_Version       : in ARM_Contents.Change_Version_Type;
			     Top_Level_Subdivision_Name : in ARM_Output.Top_Level_Subdivision_Name_Kind;
        		     No_Page_Break     : in Boolean := False);
	-- Output a revised clause header. Both the original and new text will
	-- be output. The level of the header is specified in Level. The Clause
	-- Number is as specified; the top-level (and other) subdivision names
	-- are as specified. These should appear in the table of contents.
	-- For hyperlinked formats, this should generate a link target.
	-- Version is the insertion version of the new text; Old_Version is
	-- the insertion version of the old text.
	-- If No_Page_Break is True, suppress any page breaks.
	-- Raises Not_Valid_Error if in a paragraph.

    procedure TOC_Marker (Output_Object : in out ReST_Output_Type;
			  For_Start : in Boolean);
	-- Mark the start (if For_Start is True) or end (if For_Start is
	-- False) of the table of contents data. Output objects that
	-- auto-generate the table of contents can use this to do needed
	-- actions.

    procedure New_Page (Output_Object : in out ReST_Output_Type;
			Kind : ARM_Output.Page_Kind_Type := ARM_Output.Any_Page);
	-- Output a page break.
	-- Note that this has no effect on non-printing formats.
	-- Any_Page breaks to the top of the next page (whatever it is);
	-- Odd_Page_Only breaks to the top of the odd-numbered page;
	-- Soft_Page allows a page break but does not force one (use in
	-- "No_Breaks" paragraphs.)
	-- Raises Not_Valid_Error if in a paragraph if Kind = Any_Page or
	-- Odd_Page, and if not in a paragraph if Kind = Soft_Page.

    procedure New_Column (Output_Object : in out ReST_Output_Type);
	-- Output a column break.
	-- Raises Not_Valid_Error if in a paragraph, or if the number of
	-- columns is 1.

    procedure Separator_Line (Output_Object : in out ReST_Output_Type;
			      Is_Thin : Boolean := True);
	-- Output a separator line. It is thin if "Is_Thin" is true.
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Start_Table (Output_Object : in out ReST_Output_Type;
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

    procedure Table_Marker (Output_Object : in out ReST_Output_Type;
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
    -- before any End_Paragraph. Raises Not_Valid_Error if not allowed.

    procedure Ordinary_Text (Output_Object : in out ReST_Output_Type;
			     Text : in String);
	-- Output ordinary text.
	-- The text must end at a word break, never in the middle of a word.

    procedure Ordinary_Character (Output_Object : in out ReST_Output_Type;
			          Char : in Character);
	-- Output an ordinary character.
	-- Spaces will be used to break lines as needed.

    procedure Hard_Space (Output_Object : in out ReST_Output_Type);
	-- Output a hard space. No line break should happen at a hard space.

    procedure Line_Break (Output_Object : in out ReST_Output_Type);
	-- Output a line break. This does not start a new paragraph.
	-- This corresponds to a "<BR>" in HTML.

    procedure Index_Line_Break (Output_Object : in out ReST_Output_Type;
				Clear_Keep_with_Next : in Boolean);
	-- Output a line break for the index. This does not start a new
	-- paragraph in terms of spacing. This corresponds to a "<BR>"
	-- in HTML. If Clear_Keep_with_Next is true, insure that the next
	-- line does not require the following line to stay with it.

    procedure Soft_Line_Break (Output_Object : in out ReST_Output_Type);
	-- Output a soft line break. This is a place (in the middle of a
	-- "word") that we allow a line break. It is usually used after
	-- underscores in long non-terminals.

    procedure Soft_Hyphen_Break (Output_Object : in out ReST_Output_Type);
	-- Output a soft line break, with a hyphen. This is a place (in the middle of
	-- a "word") that we allow a line break. If the line break is used,
	-- a hyphen will be added to the text.

    procedure Tab (Output_Object : in out ReST_Output_Type);
	-- Output a tab, inserting space up to the next tab stop.
	-- Raises Not_Valid_Error if the paragraph was created with
	-- Tab_Stops = ARM_Output.NO_TABS.

    procedure Special_Character (Output_Object : in out ReST_Output_Type;
			         Char : in ARM_Output.Special_Character_Type);
	-- Output an special character.

    procedure Unicode_Character (Output_Object : in out ReST_Output_Type;
			         Char : in ARM_Output.Unicode_Type);
	-- Output a Unicode character, with code position Char.

    procedure End_Hang_Item (Output_Object : in out ReST_Output_Type);
	-- Marks the end of a hanging item. Call only once per paragraph.
	-- Raises Not_Valid_Error if the paragraph style is not in
	-- Text_Prefixed_Style_Subtype, or if this has already been
	-- called for the current paragraph, or if the paragraph was started
	-- with No_Prefix = True.

    procedure Text_Format (Output_Object : in out ReST_Output_Type;
			   Format : in ARM_Output.Format_Type);
	-- Change the text format so that all of the properties are as specified.
	-- Note: Changes to these properties ought be stack-like; that is,
	-- Bold on, Italic on, Italic off, Bold off is OK; Bold on, Italic on,
	-- Bold off, Italic off should be avoided (as separate commands).

    procedure Clause_Reference (Output_Object : in out ReST_Output_Type;
				Text : in String;
				Clause_Number : in String);
	-- Generate a reference to a clause in the standard. The text of
	-- the reference is "Text", and the number of the clause is
	-- Clause_Number. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.

    procedure Index_Target (Output_Object : in out ReST_Output_Type;
			    Index_Key : in Natural);
	-- Generate a index target. This marks the location where an index
	-- reference occurs. Index_Key names the index item involved.
	-- For hyperlinked formats, this should generate a link target;
	-- for other formats, nothing is generated.

    procedure Index_Reference (Output_Object : in out ReST_Output_Type;
			       Text : in String;
			       Index_Key : in Natural;
			       Clause_Number : in String);
	-- Generate a reference to an index target in the standard. The text
	-- of the reference is "Text", and Index_Key and Clause_Number denotes
	-- the target. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.

    procedure DR_Reference (Output_Object : in out ReST_Output_Type;
			    Text : in String;
			    DR_Number : in String);
	-- Generate a reference to an DR from the standard. The text
	-- of the reference is "Text", and DR_Number denotes
	-- the target. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.

    procedure AI_Reference (Output_Object : in out ReST_Output_Type;
			    Text : in String;
			    AI_Number : in String);
	-- Generate a reference to an AI from the standard. The text
	-- of the reference is "Text", and AI_Number denotes
	-- the target (in unfolded format). For hyperlinked formats, this should
	-- generate a link; for other formats, the text alone is generated.

    procedure Local_Target (Output_Object : in out ReST_Output_Type;
			    Text : in String;
			    Target : in String);
	-- Generate a local target. This marks the potential target of local
	-- links identified by "Target". Text is the text of the target.
	-- For hyperlinked formats, this should generate a link target;
	-- for other formats, only the text is generated.

    procedure Local_Link (Output_Object : in out ReST_Output_Type;
			  Text : in String;
			  Target : in String;
			  Clause_Number : in String);
	-- Generate a local link to the target and clause given.
	-- Text is the text of the link.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure Local_Link_Start (Output_Object : in out ReST_Output_Type;
				Target : in String;
				Clause_Number : in String);
	-- Generate a local link to the target and clause given.
	-- The link will surround text until Local_Link_End is called.
	-- Local_Link_End must be called before this routine can be used again.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure Local_Link_End (Output_Object : in out ReST_Output_Type;
			      Target : in String;
			      Clause_Number : in String);
	-- End a local link for the target and clause given.
	-- This must be in the same paragraph as the Local_Link_Start.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure URL_Link (Output_Object : in out ReST_Output_Type;
			Text : in String;
			URL : in String);
	-- Generate a link to the URL given.
	-- Text is the text of the link.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure Picture  (Output_Object : in out ReST_Output_Type;
			Name  : in String;
			Descr : in String;
			Alignment : in ARM_Output.Picture_Alignment;
			Height, Width : in Natural;
			Border : in ARM_Output.Border_Kind);
	-- Generate a picture.
	-- Name is the (simple) file name of the picture; Descr is a
	-- descriptive name for the picture (it will appear in some web
	-- browsers).
	-- We assume that it is a .GIF or .JPG and that it will be present
	-- in the same directory as the input files and the same directory as
	-- the .HTML output files.
	-- Alignment specifies the picture alignment.
	-- Height and Width specify the picture size in pixels.
	-- Border specifies the kind of border.

private

    subtype Buffer_String is String (1 .. 120);
    subtype Prefix_String is String (1 .. 5);

    package String_Vectors is new Ada.Containers.Vectors
      (Positive,
       Ada.Strings.Wide_Unbounded.Unbounded_Wide_String,
       Ada.Strings.Wide_Unbounded."=");

    type Column_Offset is array (ARM_Output.Column_Count range <>) of Positive;

    type Table_Offsets (Columns : ARM_Output.Column_Count'Base := 0) is record
       Start : Column_Offset (1 .. Columns);
    end record;

    type ReST_Output_Type is new ARM_Output.Output_Type with record
	Is_Valid : Boolean := False;
	Is_In_Paragraph : Boolean := False;
	Is_In_Table : Boolean := False; -- Are we processing a table?
	Is_Hanging : Boolean := False; -- If we are in a paragraph,
				       -- is it a hanging paragraph?
	Saw_Hang_End : Boolean := False; -- If we are in a hanging paragraph,
				       -- have we seen the end of the hanging part yet?
	Output_Buffer : Buffer_String;	-- Output buffer to make smarter breaks.
	Output_Buffer_Len : Natural := 0; -- This should be empty between paragraphs.
			-- The idea is that the buffer is always logically
			-- preceeded by a space. Thus it is always OK to
			-- move the text in the buffer to the next line.
	Output_Buffer_Space_Before : Boolean := False;
			-- Do we need to output a space before the buffer?
	Output_Prefix : Ada.Strings.Unbounded.Unbounded_String;
	Output_File : Ada.Wide_Text_IO.File_Type;
	Paragraph   : String_Vectors.Vector;
	Line_Block  : Boolean := False;
	Table       : Table_Offsets := (Columns => 0, others => <>);
	Last_Column : ARM_Output.Column_Count'Base := 0;
	Requested   : ARM_Output.Format_Type := ARM_Output.NORMAL_FORMAT;
	Actual    : ARM_Output.Format_Type := ARM_Output.NORMAL_FORMAT;
	Style     : ARM_Output.Paragraph_Style_Type := ARM_Output.Normal;
	Prev_Indent : ARM_Output.Paragraph_Indent_Type := 0;  --  After rubric
	Indent      : ARM_Output.Paragraph_Indent_Type := 0;
    end record;

end ARM_ReST;
