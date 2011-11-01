with ARM_Output,
     ARM_Contents,
     Ada.Text_IO;
-- private
with Ada.Strings.Unbounded;
package ARM_HTML is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package defines the HTML output object.
    -- Output objects are responsible for implementing the details of
    -- a particular format.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2001, 2002, 2004, 2005, 2006, 2007, 2011
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
    --  4/21/00 - RLB - Added line break and hard space routines.
    --  4/24/00 - RLB - Added DR references and Insert/Delete text formats.
    --  4/25/00 - RLB - Added size to format.
    --  5/10/00 - RLB - Added End_Hang_Item.
    --  5/12/00 - RLB - Added No_Prefix to Start_Paragraph.
    --  5/13/00 - RLB - Added Special_Character.
    --  5/17/00 - RLB - Added New_Page.
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
    --  9/27/00 - RLB - Added tab emulation when in the fixed font.
    --		- RLB - Added column emulation.
    --  9/29/00 - RLB - Added Any_Nonspace flag.
    --  7/18/01 - RLB - Added support for Big_Files.
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
    --  1/12/06 - RLB - Added a number of parameters to Create.
    --  1/13/06 - RLB - Added new Link operations.
    --  1/16/06 - RLB - Added Index_URL to Create.
    --  1/27/06 - RLB - Added Tab_Emulation.
    --  2/ 8/06 - RLB - Added additional parameters to the table command.
    --  2/10/06 - RLB - Added even more additional parameters to the
    --			table command.
    --		- RLB - Added picture command.
    --  2/19/06 - RLB - Added Number_Paragraphs flag and large letter count.
    --  9/21/06 - RLB - Added Body_Font.
    --  9/25/06 - RLB - Added Last_Column_Width to Start_Table.
    -- 10/13/06 - RLB - Added specifiable colors.
    --          - RLB - Added Local_Link_Start and Local_Link_End to allow
    --			formatting in the linked text.
    --  2/ 9/07 - RLB - Changed comments on AI_Reference.
    --  2/13/07 - RLB - Revised to separate style and indent information
    --			for paragraphs.
    -- 12/19/07 - RLB - Added DOS_Filename flag.
    --		- RLB - Added limited colors to Text_Format.
    -- 10/18/11 - RLB - Changed to GPLv3 license.
    -- 10/25/11 - RLB - Added old insertion version to Revised_Clause_Header.

    type HTML_Output_Type is new ARM_Output.Output_Type with private;

    type HTML_Type is (HTML_3, -- Use only HTML 3 elements.
		       HTML_4_Compatible, -- Use HTML 4 when needed, but try to look good on old browsers.
		       HTML_4_Only); -- Use only HTML 4 elements (no attempt to look good on old browsers).

    type Tab_Emulation_Type is
		(Single_Space,		-- Replace all tabs by a single space.
		 Quad_Space,		-- Replace all tabs by four hard spaces.
		 Emulate_Fixed_Only,	-- Emulate tabs in fixed font styles;
					-- replace others by a single space.
		 Emulate_Fixed_Only_Quad,--Emulate tabs in fixed font styles;
					-- replace others by four hard spaces.
		 Emulate_All);		-- Replace tabs in all styles; note that
					-- it is unlikely that they will line
					-- up perfectly for non-fixed fonts.

    subtype Color_String is String (1..7); -- "#hhhhhh" to specify a color.

    procedure Create (Output_Object : in out HTML_Output_Type;
		      Big_Files : in Boolean;
		      File_Prefix : in String;
		      DOS_Filenames : in Boolean;
		      HTML_Kind : in HTML_Type;
		      Use_Unicode : in Boolean;
		      Number_Paragraphs : in Boolean;
	              Ref_URL : in String;
	              Srch_URL : in String;
	              Index_URL : in String;
	              Use_Buttons : Boolean;
	              Nav_On_Top : Boolean;
	              Nav_On_Bottom : Boolean;
		      Tab_Emulation : Tab_Emulation_Type;
	              Header_HTML : String;
	              Footer_HTML : String;
		      Title : in String := "";
		      Body_Font : ARM_Output.Font_Family_Type;
		      Text_Color : Color_String;
		      Background_Color : Color_String;
		      Link_Color : Color_String;
		      VLink_Color : Color_String;
		      ALink_Color : Color_String);
	-- Create an Output_Object for a document.
	-- Generate a few large output files if
	-- Big_Files is True; otherwise generate smaller output files.
	-- The prefix of the output file names is File_Prefix - this
	-- should be no more than 4 characters allowed in file names.
	-- If DOS_Filename is true, use 8.3 file names;
	-- in that case, File_Prefix must be less than 4 characters in length;
	-- and no clause or subclause number may exceed 35 if Big_Files is False.
	-- The title of the document is Title.
	-- HTML_Kind determines the kind of HTML generated; HTML_3 works on
	-- every browser but has little control over formatting;
	-- HTML_4_Compatible has better control, but tries to make the results
	-- look good on older browsers; HTML_4_Only uses maximum formatting,
	-- but makes no attempt to look good on browsers older than IE 5.0 and
	-- Firefox 1.0.
	-- If Use_Unicode is true, Unicode characters available on US versions
	-- of Windows 2000 are used when appropriate; otherwise, Unicode
	-- characters are only used when explicitly requested with
	-- Unicode_Character (other characters are replaced with reasonable
	-- equivalents). [Note: It's known that IE on Windows 95/98/ME cannot
	-- display Unicode characters.] Use_Unicode has no effect if HTML_Kind
	-- is set to HTML_3.
	-- Number_Paragraphs means that paragraph numbers will be used;
	-- otherwise, the Number parameter to Start_Paragraph must be "".
	-- Ref_URL, Srch_URL, and Index_URL are the URLs (possibly relative)
	-- for the "References", "Search", and "Index" buttons/labels,
	-- respectively. If null, these buttons/labels link to sections named
	-- "References", "Search", and "Index"; if these do not exist, the
	-- buttons/labels are omitted.
	-- If Use_Buttons is true, button images are used, otherwise text labels
	-- are used for the navigation bar.
	-- If Nav_On_Top is true, the navigation bar will appear in the header
	-- of each page. If Nav_On_Bottom is true, the navigation bar will
	-- appear in the footer of each page.
	-- Tab_Emulation determines how tabs are emulated.
	-- Header_HTML gives self-contained HTML that will appear before the
	-- navigation bar in the header. Footer_HTML gives self-contained HTML
	-- that will appear after the navigation bar in the footer.
	-- Body_Font selects the default font for the document body.
	-- Text_Color specifies the default text color; Background_Color
	-- specifies the default background color; Link_Color specifies the
	-- default color of normal links; VLink_Color specifies the
	-- default color of visited links; and ALink_Color specifies the
	-- default color of active (in the act of clinking) links.


    procedure Close (Output_Object : in out HTML_Output_Type);
	-- Close an Output_Object. No further output to the object is
	-- allowed after this call.


    procedure Section (Output_Object : in out HTML_Output_Type;
		       Section_Title : in String;
		       Section_Name : in String);
	-- Start a new section. The title is Section_Title (this is
	-- intended for humans). The name is Section_Name (this is
	-- intended to be suitable to be a portion of a file name).

    procedure Set_Columns (Output_Object : in out HTML_Output_Type;
			   Number_of_Columns : in ARM_Output.Column_Count);
	-- Set the number of columns.
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Start_Paragraph (Output_Object : in out HTML_Output_Type;
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

    procedure End_Paragraph (Output_Object : in out HTML_Output_Type);
	-- End a paragraph.

    procedure Category_Header (Output_Object : in out HTML_Output_Type;
			       Header_Text : String);
	-- Output a Category header (that is, "Legality Rules",
	-- "Dynamic Semantics", etc.)
	-- (Note: We did not use a enumeration here to insure that these
	-- headers are spelled the same in all output versions).
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Clause_Header (Output_Object : in out HTML_Output_Type;
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

    procedure Revised_Clause_Header (Output_Object : in out HTML_Output_Type;
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

    procedure TOC_Marker (Output_Object : in out HTML_Output_Type;
			  For_Start : in Boolean);
	-- Mark the start (if For_Start is True) or end (if For_Start is
	-- False) of the table of contents data. Output objects that
	-- auto-generate the table of contents can use this to do needed
	-- actions.

    procedure New_Page (Output_Object : in out HTML_Output_Type;
			Kind : ARM_Output.Page_Kind_Type := ARM_Output.Any_Page);
	-- Output a page break.
	-- Note that this has no effect on non-printing formats.
	-- Any_Page breaks to the top of the next page (whatever it is);
	-- Odd_Page_Only breaks to the top of the odd-numbered page;
	-- Soft_Page allows a page break but does not force one (use in
	-- "No_Breaks" paragraphs.)
	-- Raises Not_Valid_Error if in a paragraph if Kind = Any_Page or
	-- Odd_Page, and if not in a paragraph if Kind = Soft_Page.

    procedure New_Column (Output_Object : in out HTML_Output_Type);
	-- Output a column break.
	-- Raises Not_Valid_Error if in a paragraph, or if the number of
	-- columns is 1.

    procedure Separator_Line (Output_Object : in out HTML_Output_Type;
			      Is_Thin : Boolean := True);
	-- Output a separator line. It is thin if "Is_Thin" is true.
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Start_Table (Output_Object : in out HTML_Output_Type;
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

    procedure Table_Marker (Output_Object : in out HTML_Output_Type;
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

    procedure Ordinary_Text (Output_Object : in out HTML_Output_Type;
			     Text : in String);
	-- Output ordinary text.
	-- The text must end at a word break, never in the middle of a word.

    procedure Ordinary_Character (Output_Object : in out HTML_Output_Type;
			          Char : in Character);
	-- Output an ordinary character.
	-- Spaces will be used to break lines as needed.

    procedure Hard_Space (Output_Object : in out HTML_Output_Type);
	-- Output a hard space. No line break should happen at a hard space.

    procedure Line_Break (Output_Object : in out HTML_Output_Type);
	-- Output a line break. This does not start a new paragraph.
	-- This corresponds to a "<BR>" in HTML.

    procedure Index_Line_Break (Output_Object : in out HTML_Output_Type;
				Clear_Keep_with_Next : in Boolean);
	-- Output a line break for the index. This does not start a new
	-- paragraph in terms of spacing. This corresponds to a "<BR>"
	-- in HTML. If Clear_Keep_with_Next is true, insure that the next
	-- line does not require the following line to stay with it.
	-- Raises Not_Valid_Error if the paragraph is not in the index format.

    procedure Soft_Line_Break (Output_Object : in out HTML_Output_Type);
	-- Output a soft line break. This is a place (in the middle of a
	-- "word") that we allow a line break. It is usually used after
	-- underscores in long non-terminals.

    procedure Soft_Hyphen_Break (Output_Object : in out HTML_Output_Type);
	-- Output a soft line break, with a hyphen. This is a place (in the middle of
	-- a "word") that we allow a line break. If the line break is used,
	-- a hyphen will be added to the text.

    procedure Tab (Output_Object : in out HTML_Output_Type);
	-- Output a tab, inserting space up to the next tab stop.
	-- Raises Not_Valid_Error if the paragraph was created with
	-- Tab_Stops = ARM_Output.NO_TABS.

    procedure Special_Character (Output_Object : in out HTML_Output_Type;
			         Char : in ARM_Output.Special_Character_Type);
	-- Output an special character.

    procedure Unicode_Character (Output_Object : in out HTML_Output_Type;
			         Char : in ARM_Output.Unicode_Type);
	-- Output a Unicode character, with code position Char.

    procedure End_Hang_Item (Output_Object : in out HTML_Output_Type);
	-- Marks the end of a hanging item. Call only once per paragraph.
	-- Raises Not_Valid_Error if the paragraph style is not in
	-- Text_Prefixed_Style_Subtype, or if this has already been
	-- called for the current paragraph, or if the paragraph was started
	-- with No_Prefix = True.

    procedure Text_Format (Output_Object : in out HTML_Output_Type;
			   Format : in ARM_Output.Format_Type);
	-- Change the text format so that all of the properties are as specified.
	-- Note: Changes to these properties ought be stack-like; that is,
	-- Bold on, Italic on, Italic off, Bold off is OK; Bold on, Italic on,
	-- Bold off, Italic off should be avoided (as separate commands).

    procedure Clause_Reference (Output_Object : in out HTML_Output_Type;
				Text : in String;
				Clause_Number : in String);
	-- Generate a reference to a clause in the standard. The text of
	-- the reference is "text", and the number of the clause is
	-- Clause_Number. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.

    procedure Index_Target (Output_Object : in out HTML_Output_Type;
			    Index_Key : in Natural);
	-- Generate a index target. This marks the location where an index
	-- reference occurs. Index_Key names the index item involved.
	-- For hyperlinked formats, this should generate a link target;
	-- for other formats, nothing is generated.

    procedure Index_Reference (Output_Object : in out HTML_Output_Type;
			       Text : in String;
			       Index_Key : in Natural;
			       Clause_Number : in String);
	-- Generate a reference to an index target in the standard. The text
	-- of the reference is "Text", and Index_Key and Clause_Number denotes
	-- the target. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.

    procedure DR_Reference (Output_Object : in out HTML_Output_Type;
			    Text : in String;
			    DR_Number : in String);
	-- Generate a reference to an DR from the standard. The text
	-- of the reference is "Text", and DR_Number denotes
	-- the target. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.

    procedure AI_Reference (Output_Object : in out HTML_Output_Type;
			    Text : in String;
			    AI_Number : in String);
	-- Generate a reference to an AI from the standard. The text
	-- of the reference is "Text", and AI_Number denotes
	-- the target (in folded format). For hyperlinked formats, this should
	-- generate a link; for other formats, the text alone is generated.

    procedure Local_Target (Output_Object : in out HTML_Output_Type;
			    Text : in String;
			    Target : in String);
	-- Generate a local target. This marks the potential target of local
	-- links identified by "Target". Text is the text of the target.
	-- For hyperlinked formats, this should generate a link target;
	-- for other formats, only the text is generated.

    procedure Local_Link (Output_Object : in out HTML_Output_Type;
			  Text : in String;
			  Target : in String;
			  Clause_Number : in String);
	-- Generate a local link to the target and clause given.
	-- Text is the text of the link.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure Local_Link_Start (Output_Object : in out HTML_Output_Type;
				Target : in String;
				Clause_Number : in String);
	-- Generate a local link to the target and clause given.
	-- The link will surround text until Local_Link_End is called.
	-- Local_Link_End must be called before this routine can be used again.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure Local_Link_End (Output_Object : in out HTML_Output_Type;
			      Target : in String;
			      Clause_Number : in String);
	-- End a local link for the target and clause given.
	-- This must be in the same paragraph as the Local_Link_Start.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure URL_Link (Output_Object : in out HTML_Output_Type;
			Text : in String;
			URL : in String);
	-- Generate a link to the URL given.
	-- Text is the text of the link.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure Picture  (Output_Object : in out HTML_Output_Type;
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

    type Column_Text_Item_Type;
    type Column_Text_Ptr is access Column_Text_Item_Type;
    type Column_Text_Item_Type is record
	Text : String (1..120);
	Length : Natural;
	Item : Natural; -- Which item.
	End_Para : Boolean; -- True if this item is an end paragraph.
	Next : Column_Text_Ptr;
    end record;
    type Column_Text_Ptrs_Type is array (1..5) of Column_Text_Ptr;

    subtype Prefix_String is String(1..4);
    type HTML_Output_Type is new ARM_Output.Output_Type with record
	Is_Valid : Boolean := False;

	-- Global properties:
	File_Prefix : Prefix_String; -- Blank padded.
	Big_Files : Boolean; -- For HTML, this means to generate a single monster file.
	DOS_Filenames : Boolean; -- Generate 8.3 MS-DOS filenames.
	Title : Ada.Strings.Unbounded.Unbounded_String;
        HTML_Kind : HTML_Type;
        Use_Unicode : Boolean;
        Number_Paragraphs : Boolean;
        Ref_URL : Ada.Strings.Unbounded.Unbounded_String;
        Srch_URL : Ada.Strings.Unbounded.Unbounded_String;
        Index_URL : Ada.Strings.Unbounded.Unbounded_String;
        Use_Buttons : Boolean := True;
        Nav_On_Top : Boolean := True;
        Nav_On_Bottom : Boolean := True;
	Tab_Emulation : Tab_Emulation_Type;
        Header_HTML : Ada.Strings.Unbounded.Unbounded_String;
        Footer_HTML : Ada.Strings.Unbounded.Unbounded_String;
	Body_Font : ARM_Output.Font_Family_Type := ARM_Output.Roman;
	Text_Color : Color_String;
	Background_Color : Color_String;
	Link_Color : Color_String;
	VLink_Color : Color_String;
	ALink_Color : Color_String;

	-- Current formatting properties:
	Is_In_Paragraph  : Boolean := False;
	Paragraph_Style  : ARM_Output.Paragraph_Style_Type;
	Paragraph_Indent : ARM_Output.Paragraph_Indent_Type;
	Had_Prefix : Boolean := False; -- If in paragraph, value of not No_Prefix.
	Column_Count : ARM_Output.Column_Count := 1;
	Output_File : Ada.Text_IO.File_Type;
	Section_Name : String(1..3);
	Char_Count : Natural := 0; -- Characters on current line.
	Disp_Char_Count : Natural := 0; -- Displayed characters on current line.
	Disp_Large_Char_Count : Natural := 0; -- Displayed large characters on current line (others are "small" characters).
			-- Large characters are capitals, 'm', 'w', and numbers.
	Any_Nonspace : Boolean := False; -- Have we output any non-space on this line?
	Last_was_Space : Boolean := False; -- True if the last visible character
				-- output was a space (any kind), or this is the
				-- start of a line.
	Conditional_Space : Boolean := False; -- If True, output a space if the
				-- next *visible* character is not a space or
				-- punctuation.
	Saw_Hang_End : Boolean := False; -- If we are in a hanging paragraph,
			       -- have we seen the end of the hanging part yet?
	Is_Bold : Boolean; -- Is the text currently bold?
	Is_Italic : Boolean; -- Is the text current italics?
	Font : ARM_Output.Font_Family_Type; -- What is the current font family?
	Size : ARM_Output.Size_Type; -- What is the current relative size?
	Color : ARM_Output.Color_Type := ARM_Output.Default;
	Change : ARM_Output.Change_Type := ARM_Output.None;
	Version : ARM_Contents.Change_Version_Type := '0';
	Added_Version : ARM_Contents.Change_Version_Type := '0';
	Location : ARM_Output.Location_Type := ARM_Output.Normal;
	Tab_Stops : ARM_Output.Tab_Info := ARM_Output.NO_TABS;
	Can_Emulate_Tabs : Boolean := False; -- Can we emulate tabs in the current style?

	Is_In_Table : Boolean := False; -- Are we processing a table?
	In_Header : Boolean := False; -- If Is_In_Table, are we processing the header?
	Table_Column_Alignment : ARM_Output.Column_Text_Alignment; -- If Is_In_Table, specifies the column alignment.
	Table_Has_Small_Text : Boolean := False; -- If Is_In_Table, specifies the text size.

	Current_Column : Natural := 0; -- When processing 4-column+ text, the current column number.
	Current_Item : Natural := 0; -- When processing 4-column+ text, the current item within the column.
	Column_Text : Column_Text_Ptrs_Type := (others => null);
		-- If we are processing 4-column+ text, the text for the columns.

	In_Local_Link : Boolean := False;

	Current_Clause : Ada.Strings.Unbounded.Unbounded_String;
		-- The name of the clause of the currently open file (for
		-- Big_Files = False); used to generate the navigation bar.
    end record;

end ARM_HTML;
