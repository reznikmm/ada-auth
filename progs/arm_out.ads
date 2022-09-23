with ARM_Contents;
package ARM_Output is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package defines the abstract definition of an output object.
    -- Output objects are responsible for implementing the details of
    -- a particular format.
    --
    -- We use dispatching calls to call the formatter, so the details of
    -- formatting are insulated from the code that reads the source and
    -- determines the details of the text.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2002, 2004, 2005, 2006, 2007, 2011, 2012, 2022
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
    --  4/18/00 - RLB - Added index and contents marker routines.
    --  4/21/00 - RLB - Added line break and hard space routines.
    --  4/24/00 - RLB - Added DR references and Insert/Delete text formats.
    --  4/25/00 - RLB - Added size to format.
    --  4/29/00 - RLB - Added Indented and Bulleted formats.
    --  5/10/00 - RLB - Added Syntax and Hanging formats.
    --		- RLB - Added End_Hang_Item.
    --  5/12/00 - RLB - Added No_Prefix to Start_Paragraph.
    --  5/13/00 - RLB - Added Special_Character.
    --  5/16/00 - RLB - Added more special characters.
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
    --  8/ 2/00 - RLB - Added Soft_Hyphen_Break and left and right quote
    --			characters.
    --		- RLB - Added new styles.
    --  8/ 7/00 - RLB - Added Leading flag to Start_Paragraph, removed "Leading"
    --			styles.
    --  8/11/00 - RLB - Added Hanging_in_Bulleted styles.
    --  8/16/00 - RLB - Added Code_Indented_Nested_Bulleted.
    --  8/17/00 - RLB - Replaced "Leading" by "Space_After".
    -- 		- RLB - Added Nested_Enumerated.
    --  8/22/00 - RLB - Added Revised_Clause_Header.
    --  9/26/00 - RLB - Added Syntax_Summary style.
    --  7/18/02 - RLB - Removed Document parameter from Create, replaced by
    --			three strings and For_ISO boolean.
    --		- RLB - Added AI_Reference.
    --		- RLB - Added Change_Version_Type and uses.
    --  9/10/04 - RLB - Added "Both" to possible changes to handle
    --			replacement of changed text.
    --  9/14/04 - RLB - Moved Change_Version_Type to ARM_Contents to
    --			avoid circularities.
    -- 11/03/04 - RLB - Added Nested_X2_Bulleted.
    -- 11/15/04 - RLB - Added Indented_Nested_Bulleted.
    --  1/24/05 - RLB - Added Inner_Indented.
    --  2/ 1/05 - RLB - Added Turkish chars to allow an AARM note.
    --  5/27/05 - RLB - Added arbitrary Unicode characters.
    --  1/11/06 - RLB - Eliminated dispatching Create in favor of tailored
    --			versions.
    --  1/13/06 - RLB - Added new Link operations.
    --  2/ 8/06 - RLB - Added additional parameters to the table command.
    --  2/10/06 - RLB - Added even more additional parameters to the
    --			table command.
    --		- RLB - Added picture command.
    --  3/28/06 - RLB - Updated the comments on the picture command.
    --  9/25/06 - RLB - Added Last_Column_Width to Start_Table.
    -- 10/13/06 - RLB - Added Local_Link_Start and Local_Link_End to allow
    --			formatting in the linked text.
    --  2/ 9/07 - RLB - Changed comments on AI_Reference.
    --  2/13/07 - RLB - Revised to separate style and indent information
    --			for paragraphs.
    --  2/19/07 - RLB - Added Title style to eliminate issues with title
    --			pages.
    -- 12/19/07 - RLB - Added limited colors to Text_Format.
    -- 10/18/11 - RLB - Changed to GPLv3 license.
    -- 10/25/11 - RLB - Added old insertion version to Revised_Clause_Header.
    -- 10/18/12 - RLB - Added additional hanging styles to reflect additional
    --			hang amounts.
    -- 11/26/12 - RLB - Added subdivision names to Clause_Header and
    --			Revised_Clause_Header.
    --  8/22/22 - RLB - Added All_Formats parameter to URL_Link.

    type Output_Type is abstract tagged limited null record;

    Not_Valid_Error : exception; -- Raised when an operation is invalid.

    -- Creation is handled by the individual output types.

    type Top_Level_Subdivision_Name_Kind is (Chapter, Section, Clause);


    procedure Close (Output_Object : in out Output_Type) is abstract;
	-- Close an Output_Object. No further output to the object is
	-- allowed after this call.


    procedure Section (Output_Object : in out Output_Type;
		       Section_Title : in String;
		       Section_Name : in String) is abstract;
	-- Start a new section. The title is Section_Title (this is
	-- intended for humans). The name is Section_Name (this is
	-- intended to be suitable to be a portion of a file name).

    subtype Column_Count is Natural range 1 .. 8;
    procedure Set_Columns (Output_Object : in out Output_Type;
			   Number_of_Columns : in ARM_Output.Column_Count) is abstract;
	-- Set the number of columns.
	-- Raises Not_Valid_Error if in a paragraph.

    type Paragraph_Indent_Type is range 0 .. 7;
	-- The number of units that the text is indented.
	-- The indentation of paragraphs is described in "units", a mostly
	-- fixed amount from the left margin.

    type Paragraph_Style_Type is (
	-- Styles without prefixes:
	Normal,		-- Normal paragraphs have the style of the body text
			-- for the Reference Manual.
	Wide_Above, 	-- Same as Normal, except that there is additional space
			-- before the paragraph.
	Small,		-- Like Normal, except for a smaller font. (Use for
			-- notes and annotations, for instance.)
	Small_Wide_Above, -- Like Small, except that there is additional space
			-- before the paragraph.
	Header,		-- Like Normal, but with no empty space following.
	Small_Header,   -- Like Small, but with no empty space following. (Use
			-- for the header to Notes, for instance).
	Syntax_Summary, -- The text is in a smaller font, and there is less
			-- space than normal between lines.
	Index,		-- The text is in a smaller font, and if possible,
			-- lines that wrap are indented one (additional) unit.
	Title,		-- The text is in a much larger font (+3 in terms
			-- of size units, nearly double the normal size),
			-- and with additional space before as in Wide_Above.
	Examples,	-- The text is in a fixed font.
	Small_Examples,	-- The text is in a smaller fixed font.
	Swiss_Examples, -- The text is in a swiss (sans-serif) font.
	Small_Swiss_Examples, -- The text is in a smaller swiss (sans-serif) font.
	-- Styles with prefixes:
	Bulleted,	-- The text is normal other than that it has a right
			-- indent of one unit, and each paragraph is preceeded
			-- by a bullet (a solid circle). Indent must be at least
			-- one.
	Small_Bulleted, -- Same as Bulleted, except that a smaller font (same as
			-- for Small) is used.
	Nested_Bulleted,-- Same as Bulleted, except that a small bullet is used.
	Small_Nested_Bulleted, -- Same as Small_Bulleted, except that a small
			-- bullet is used.
	-- Styles with text prefixes:
	Enumerated,	-- The text is normal other than it has a right indent
			-- of one unit, but the first part of each paragraph (up
			-- to the call of End_Hang_Item) hangs out as would a
			-- bullet. Indent must be at least one.
	Small_Enumerated, -- Same as Enumerated, except that a smaller font
			-- (same as for Small) is used.
	Giant_Hanging,	-- The text is normal, but the first part of each
			-- paragraph (up to the call of End_Hang_Item) hangs
			-- out four units. Indent must be at least four.
	Small_Giant_Hanging, -- Same as Giant_Hanging, except that the text is
			-- in a smaller font (same as Small).
	Wide_Hanging,	-- The text is normal, but the first part of each
			-- paragraph (up to the call of End_Hang_Item) hangs
			-- out three units. Indent must be at least three.
	Small_Wide_Hanging, -- Same as Wide_Hanging, except that the text is
			-- in a smaller font (same as Small).
	Medium_Hanging,	-- The text is normal, but the first part of each
			-- paragraph (up to the call of End_Hang_Item) hangs
			-- out two units. Indent must be at least two.
	Small_Medium_Hanging, -- Same as Medium_Hanging, except that the text is
			-- in a smaller font (same as Small).
	Narrow_Hanging,	-- The text is normal, but the first part of each
			-- paragraph (up to the call of End_Hang_Item) hangs
			-- out one unit. Indent must be at least one.
	Small_Narrow_Hanging, -- Same as Narrow_Hanging, except that the text is
			-- in a smaller font (same as Small).
	Hanging_in_Bulleted, -- The text is normal, but it has a right indent
			-- of one unit, and the first part of each
			-- paragraph (up to the call of End_Hang_Item) hangs
			-- out two units. Indent must be at least two.
	Small_Hanging_in_Bulleted); -- Same as Hanging_in_Bulleted, except that
			-- the text is in a smaller font (same as Small).

    subtype Unprefixed_Style_Subtype is
	Paragraph_Style_Type range Normal .. Small_Swiss_Examples;
	-- These styles have no prefix at all.
    subtype Prefixed_Style_Subtype is
	Paragraph_Style_Type range Bulleted .. Small_Hanging_in_Bulleted;
	-- These styles have some sort of prefix.
    subtype Text_Prefixed_Style_Subtype is
	Paragraph_Style_Type range Enumerated .. Small_Hanging_in_Bulleted;
	-- These styles have a text prefix.
    subtype Bullet_Prefixed_Style_Subtype is
	Paragraph_Style_Type range Bulleted .. Small_Nested_Bulleted;
	-- These styles have a bullet prefix.


    subtype Tab_Location is Natural range 0 .. 200; -- Location of tab stops, in picas.
    type Tab_Kind_Type is (Left_Fixed, Left_Proportional);
	-- Proportional tabs are adjusted based on the default font size of
	-- the paragraph; they are given as if the font is 12 points.
    type Tab_Stop_Type is record
	Stop : Tab_Location;
	Kind : Tab_Kind_Type;
    end record;
    type Tab_Stops_Type is array (1 .. 20) of Tab_Stop_Type;
    type Tab_Info is record
	Number : Natural;
	Stops : Tab_Stops_Type;
    end record;
    NO_TABS : constant Tab_Info := (Number => 0, Stops => (others =>
	(Kind => Left_Fixed, Stop => 0)));

    type Justification_Type is (Default, Justified, Left, Center, Right);
	-- Defines the text justification for the paragraph.

    type Space_After_Type is (Normal, Narrow, Wide);
	-- Defines the space following the paragraph. Narrow is about 30%
	-- less than normal; Wide is about 50% more than normal.

    type Change_Type is (None, Insertion, Deletion, Both);
	-- Defines the change state. Both means both an Insertion and Deletion.

    procedure Start_Paragraph (Output_Object : in out Output_Type;
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
				   := ARM_Output.Default) is abstract;
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

    procedure End_Paragraph (Output_Object : in out Output_Type) is abstract;
	-- End a paragraph.

    procedure Category_Header (Output_Object : in out Output_Type;
			       Header_Text : String) is abstract;
	-- Output a Category header (that is, "Legality Rules",
	-- "Dynamic Semantics", etc.)
	-- (Note: We did not use a enumeration here to insure that these
	-- headers are spelled the same in all output versions).
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Clause_Header (Output_Object     : in out Output_Type;
			     Header_Text       : in String;
			     Level	       : in ARM_Contents.Level_Type;
			     Clause_Number     : in String;
			     Top_Level_Subdivision_Name : in ARM_Output.Top_Level_Subdivision_Name_Kind;
			     No_Page_Break     : in Boolean := False) is abstract;
	-- Output a Clause header. The level of the header is specified
	-- in Level. The Clause Number is as specified; the top-level (and
	-- other) subdivision names are as specified. These should appear in
	-- the table of contents.
	-- For hyperlinked formats, this should generate a link target.
	-- If No_Page_Break is True, suppress any page breaks.
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Revised_Clause_Header
			    (Output_Object     : in out Output_Type;
			     New_Header_Text   : in String;
			     Old_Header_Text   : in String;
			     Level	       : in ARM_Contents.Level_Type;
			     Clause_Number     : in String;
			     Version	       : in ARM_Contents.Change_Version_Type;
			     Old_Version       : in ARM_Contents.Change_Version_Type;
			     Top_Level_Subdivision_Name : in ARM_Output.Top_Level_Subdivision_Name_Kind;
        		     No_Page_Break     : in Boolean := False) is abstract;
	-- Output a revised clause header. Both the original and new text will
	-- be output. The level of the header is specified in Level. The Clause
	-- Number is as specified; the top-level (and other) subdivision names
	-- are as specified. These should appear in the table of contents.
	-- For hyperlinked formats, this should generate a link target.
	-- Version is the insertion version of the new text; Old_Version is
	-- the insertion version of the old text.
	-- If No_Page_Break is True, suppress any page breaks.
	-- Raises Not_Valid_Error if in a paragraph.

    procedure TOC_Marker (Output_Object : in out Output_Type;
			  For_Start : in Boolean) is abstract;
	-- Mark the start (if For_Start is True) or end (if For_Start is
	-- False) of the table of contents data. Output objects that
	-- auto-generate the table of contents can use this to do needed
	-- actions.

    type Page_Kind_Type is (Any_Page, Odd_Page_Only, Soft_Page);

    procedure New_Page (Output_Object : in out Output_Type;
			Kind : ARM_Output.Page_Kind_Type := ARM_Output.Any_Page) is abstract;
	-- Output a page break.
	-- Note that this has no effect on non-printing formats.
	-- Any_Page breaks to the top of the next page (whatever it is);
	-- Odd_Page_Only breaks to the top of the odd-numbered page;
	-- Soft_Page allows a page break but does not force one (use in
	-- "No_Breaks" paragraphs.)
	-- Raises Not_Valid_Error if in a paragraph if Kind = Any_Page or
	-- Odd_Page, and if not in a paragraph if Kind = Soft_Page.

    procedure New_Column (Output_Object : in out Output_Type) is abstract;
	-- Output a column break.
	-- Raises Not_Valid_Error if in a paragraph, or if the number of
	-- columns is 1.

    procedure Separator_Line (Output_Object : in out Output_Type;
			      Is_Thin : Boolean := True) is abstract;
	-- Output a separator line. It is thin if "Is_Thin" is true.
	-- Raises Not_Valid_Error if in a paragraph.

    type Column_Text_Alignment is (Center_All, Left_All, Center_except_First);
    type Header_Kind_Type is (Both_Caption_and_Header, Header_Only, No_Headers);

    procedure Start_Table (Output_Object : in out Output_Type;
			   Columns : in ARM_Output.Column_Count;
			   First_Column_Width : in ARM_Output.Column_Count;
			   Last_Column_Width : in ARM_Output.Column_Count;
			   Alignment : in ARM_Output.Column_Text_Alignment;
			   No_Page_Break : in Boolean;
			   Has_Border : in Boolean;
			   Small_Text_Size : in Boolean;
			   Header_Kind : in ARM_Output.Header_Kind_Type) is abstract;
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

    type Table_Marker_Type is (End_Caption, End_Item, End_Header,
	End_Row, End_Row_Next_Is_Last, End_Table);
    procedure Table_Marker (Output_Object : in out Output_Type;
			    Marker : in ARM_Output.Table_Marker_Type) is abstract;
	-- Marks the end of an entity in a table.
	-- If Marker is End_Caption, the table caption ends and the
	--	future text is part of the table header.
	-- If Marker is End_Header, the table header ends and the
	--	future text is part of the table body.
	-- If Marker is End_Row, a row in the table is completed, and another
	--	row started.
	-- If Marker is End_Row_Next_Is_Last, a row in the table is completed,
	--	and another row started. That row is the last row in the table.
	-- If Marker is End_Item, an item in the table header or body is ended,
	--	and another started.
	-- If Marker is End_Table, the entire table is finished.
	-- Raises Not_Valid_Error if not in a table.

    -- Text output: These are only allowed after a Start_Paragraph and
    -- before any End_Paragraph. Raises Not_Valid_Error if not allowed.

    procedure Ordinary_Text (Output_Object : in out Output_Type;
			     Text : in String) is abstract;
	-- Output ordinary text.
	-- The text must end at a word break, never in the middle of a word.

    procedure Ordinary_Character (Output_Object : in out Output_Type;
			          Char : in Character) is abstract;
	-- Output an ordinary (Latin-1) character.
	-- Spaces will be used to break lines as needed.

    procedure Hard_Space (Output_Object : in out Output_Type) is abstract;
	-- Output a hard space. No line break should happen at a hard space.

    procedure Line_Break (Output_Object : in out Output_Type) is abstract;
	-- Output a line break. This does not start a new paragraph.
	-- This corresponds to a "<BR>" in HTML.

    procedure Index_Line_Break (Output_Object : in out Output_Type;
				Clear_Keep_with_Next : in Boolean) is abstract;
	-- Output a line break for the index. This does not start a new
	-- paragraph in terms of spacing. This corresponds to a "<BR>"
	-- in HTML. If Clear_Keep_with_Next is true, insure that the next
	-- line does not require the following line to stay with it.
	-- Raises Not_Valid_Error if the paragraph is not in the index format.

    procedure Soft_Line_Break (Output_Object : in out Output_Type) is abstract;
	-- Output a soft line break. This is a place (in the middle of a
	-- "word") that we allow a line break. It is usually used after
	-- underscores in long non-terminals.

    procedure Soft_Hyphen_Break (Output_Object : in out Output_Type) is abstract;
	-- Output a soft line break, with a hyphen. This is a place (in the middle of
	-- a "word") that we allow a line break. If the line break is used,
	-- a hyphen will be added to the text.

    procedure Tab (Output_Object : in out Output_Type) is abstract;
	-- Output a tab, inserting space up to the next tab stop.
	-- Raises Not_Valid_Error if the paragraph was created with
	-- Tab_Stops = ARM_Output.NO_TABS.

    type Special_Character_Type is (EM_Dash, -- EM (very long) dash.
				    EN_Dash, -- EN (long) dash
				    GEQ, -- Greater than or equal symbol.
				    LEQ, -- Less than or equal symbol.
				    NEQ, -- Not equal symbol.
				    PI,  -- PI.
				    Left_Ceiling, -- Left half of ceiling.
				    Right_Ceiling, -- Right half of ceiling.
				    Left_Floor, -- Left half of floor.
				    Right_Floor, -- Right half of floor.
				    Thin_Space, -- A thinner than usual space.
				    Left_Quote, -- A left facing single quote.
				    Right_Quote, -- A right facing single quote.
				    Left_Double_Quote, -- A left facing double quote.
				    Right_Double_Quote, -- A right facing double quote.
				    Small_Dotless_I, -- A small i without a dot (Unicode(16#0131#).
				    Capital_Dotted_I -- A large I with a dot (Unicode(16#0130#).
				   );

    procedure Special_Character (Output_Object : in out Output_Type;
			         Char : in ARM_Output.Special_Character_Type) is abstract;
	-- Output an special character.

    type Unicode_Type is range 0 .. 2**31-2;

    procedure Unicode_Character (Output_Object : in out Output_Type;
			         Char : in ARM_Output.Unicode_Type) is abstract;
	-- Output a Unicode character, with code position Char.

    procedure End_Hang_Item (Output_Object : in out Output_Type) is abstract;
	-- Marks the end of a hanging item. Call only once per paragraph.
	-- Raises Not_Valid_Error if the paragraph style is not in
	-- Text_Prefixed_Style_Subtype, or if this has already been
	-- called for the current paragraph, or if the paragraph was started
	-- with No_Prefix = True.


    type Font_Family_Type is (Roman, Swiss, Fixed, Default);
	-- Determines the font family. "Default" is the font family
	-- of a paragraph before it is changed.

    type Size_Type is range -9 .. 9;
	-- Determines the relative size. This is the change in size. For
	-- formats that allow it, this is the change in size in points.
	-- Otherwise, this is a relative change.

    type Color_Type is (Default, Black, Red, Green, Blue);
	-- Determines the text color. "Default" is usually black,
	-- although some formats may use color to indicate changes.

    type Location_Type is (Normal, Subscript, Superscript);
	-- Determines the location (vertical position) of the text: in the
	-- normal position, subscripted (below the normal position), or
	-- superscripted (above the normal position).

    type Format_Type is record
	-- A grouping of all of the format parameters.
	Bold    : Boolean;
	Italic  : Boolean;
	Font    : ARM_Output.Font_Family_Type;
	Size    : ARM_Output.Size_Type;
	Color   : ARM_Output.Color_Type;
        Change  : ARM_Output.Change_Type;
        Version : ARM_Contents.Change_Version_Type;
        Added_Version : ARM_Contents.Change_Version_Type;
	    -- Only used when Change = Both; this is then the version
	    -- of the insertion, and Version is then the version of the deletion.
        Location : ARM_Output.Location_Type;
    end record;
    NORMAL_FORMAT : constant Format_Type :=
	(Bold => False, Italic => False, Font => Default,
	 Size => 0, Color => Default, Change => None, Version => '0',
	 Added_Version => '0', Location => Normal);
	-- The format of utterly normal text. Usually used to reset
	-- the format or for initialization.

    procedure Text_Format (Output_Object : in out Output_Type;
			   Format : in ARM_Output.Format_Type) is abstract;
	-- Change the text format so that all of the properties are as specified.
	-- Note: Changes to these properties ought be stack-like; that is,
	-- Bold on, Italic on, Italic off, Bold off is OK; Bold on, Italic on,
	-- Bold off, Italic off should be avoided (as separate commands).

    procedure Clause_Reference (Output_Object : in out Output_Type;
				Text : in String;
				Clause_Number : in String) is abstract;
	-- Generate a reference to a clause in the standard. The text of
	-- the reference is "Text", and the number of the clause is
	-- Clause_Number. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.

    procedure Index_Target (Output_Object : in out Output_Type;
			    Index_Key : in Natural) is abstract;
	-- Generate a index target. This marks the location where an index
	-- reference occurs. Index_Key names the index item involved.
	-- For hyperlinked formats, this should generate a link target;
	-- for other formats, nothing is generated.

    procedure Index_Reference (Output_Object : in out Output_Type;
			       Text : in String;
			       Index_Key : in Natural;
			       Clause_Number : in String) is abstract;
	-- Generate a reference to an index target in the standard. The text
	-- of the reference is "Text", and Index_Key and Clause_Number denotes
	-- the target. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.

    procedure DR_Reference (Output_Object : in out Output_Type;
			    Text : in String;
			    DR_Number : in String) is abstract;
	-- Generate a reference to a DR from the standard. The text
	-- of the reference is "Text", and DR_Number denotes
	-- the target. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.

    procedure AI_Reference (Output_Object : in out Output_Type;
			    Text : in String;
			    AI_Number : in String) is abstract;
	-- Generate a reference to an AI from the standard. The text
	-- of the reference is "Text", and AI_Number denotes
	-- the target (in unfolded format). For hyperlinked formats, this
	-- should generate a link; for other formats, the text alone is
	-- generated.

    procedure Local_Target (Output_Object : in out Output_Type;
			    Text : in String;
			    Target : in String) is abstract;
	-- Generate a local target. This marks the potential target of local
	-- links identified by "Target". Text is the text of the target.
	-- For hyperlinked formats, this should generate a link target;
	-- for other formats, only the text is generated.

    procedure Local_Link (Output_Object : in out Output_Type;
			  Text : in String;
			  Target : in String;
			  Clause_Number : in String) is abstract;
	-- Generate a local link to the target and clause given.
	-- Text is the text of the link.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure Local_Link_Start (Output_Object : in out Output_Type;
				Target : in String;
				Clause_Number : in String) is abstract;
	-- Generate a local link to the target and clause given.
	-- The link will surround text until Local_Link_End is called.
	-- Local_Link_End must be called before this routine can be used again.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure Local_Link_End (Output_Object : in out Output_Type;
			      Target : in String;
			      Clause_Number : in String) is abstract;
	-- End a local link for the target and clause given.
	-- This must be in the same paragraph as the Local_Link_Start.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure URL_Link (Output_Object : in out Output_Type;
			Text : in String;
			URL : in String;
                        All_Formats : in Boolean) is abstract;
	-- Generate a link to the URL given.
	-- Text is the text of the link.
        -- If All_Formats is True, then the link is generated in any format that
        -- can support a link. Otherwise, a link is only generated in formats
        -- that are primarily hyperlinked (such as HTML). If no link is
        -- generated, the text still should be generated.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    type Picture_Alignment is (Inline, Float_Left, Float_Right,
	Alone_Left, Alone_Right, Alone_Center);
	-- Inline puts the picture as part of a paragraph;
	-- Alone styles put the picture as the only things on the line;
	-- Float styles wrap text around the picture. Alone styles are
	-- *not* part of paragraphs.

    type Border_Kind is (None, Thin, Thick);

    procedure Picture  (Output_Object : in out Output_Type;
			Name  : in String;
			Descr : in String;
			Alignment : in ARM_Output.Picture_Alignment;
			Height, Width : in Natural;
			Border : in ARM_Output.Border_Kind) is abstract;
	-- Generate a picture.
	-- Name is the (simple) file name of the picture; Descr is a
	-- descriptive name for the picture (it will appear in some web
	-- browsers).
	-- We assume that it is a .PNG or .JPG and that it will be present
	-- in the same directory as the output files.
	-- Alignment specifies the picture alignment.
	-- Height and Width specify the picture size in pixels.
	-- Border specifies the kind of border.

end ARM_Output;
