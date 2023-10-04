with ARM_Output,
     ARM_Contents,
     Ada.Text_IO,
     Ada.Exceptions,
     Ada.Strings.Fixed,
     Ada.Strings.Maps;
package body ARM_Blackhole is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package defines the black hole output object.
    -- Output objects are responsible for implementing the details of
    -- a particular format. The black hole object does not make any output,
    -- it is used for omitting text from output while still processing it for
    -- generated content.
    --
    -- ---------------------------------------
    -- Copyright 2023
    --   AXE Consultants. All rights reserved.
    --   621 N. Sherman Ave., Suite B6, Madison WI  53704
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
    --  8/23/23 - RLB - Created base package from text version.

    LINE_LENGTH : constant := 78;
	-- Maximum intended line length.

    Special_Set : constant Ada.Strings.Maps.Character_Set :=
         Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('>'),
           Ada.Strings.Maps.To_Set ('@'));

    procedure Create (Output_Object : in out Blackhole_Output_Type) is
	-- Create an Output_Object for a document.
    begin
	if Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Already valid object");
	end if;
	Output_Object.Is_Valid := True;
	Output_Object.Is_In_Paragraph := False;
	Output_Object.Is_Hanging := False;
	Output_Object.Saw_Hang_End := False;
    end Create;


    procedure Close (Output_Object : in out Blackhole_Output_Type) is
	-- Close an Output_Object. No further output to the object is
	-- allowed after this call.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	Output_Object.Is_Valid := False;
    end Close;


    procedure Section (Output_Object : in out Blackhole_Output_Type;
		       Section_Title : in String;
		       Section_Name : in String) is
	-- Start a new section. The title is Section_Title (this is
	-- intended for humans). The name is Section_Name (this is
	-- intended to be suitable to be a portion of a file name).
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Section in paragraph");
	end if;
    end Section;


    procedure Set_Columns (Output_Object : in out Blackhole_Output_Type;
			   Number_of_Columns : in ARM_Output.Column_Count) is
	-- Set the number of columns.
	-- Raises Not_Valid_Error if in a paragraph.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"In paragraph");
	end if;
	-- No columns in text format.
    end Set_Columns;


    procedure Start_Paragraph (Output_Object : in out Blackhole_Output_Type;
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
				   := ARM_Output.Default) is
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
	use type ARM_Output.Paragraph_Indent_Type;
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Already in paragraph");
	end if;
	Output_Object.Is_In_Paragraph := True;
	Output_Object.Is_Hanging := False;
	Output_Object.Saw_Hang_End := False;

	case Style is
	    when ARM_Output.Wide_Hanging |
                 ARM_Output.Giant_Hanging |
	         ARM_Output.Medium_Hanging |
	         ARM_Output.Narrow_Hanging |
	         ARM_Output.Hanging_in_Bulleted |
	         ARM_Output.Small_Giant_Hanging |
	         ARM_Output.Small_Wide_Hanging |
	         ARM_Output.Small_Medium_Hanging |
	         ARM_Output.Small_Narrow_Hanging |
	         ARM_Output.Small_Hanging_in_Bulleted |
	         ARM_Output.Enumerated | ARM_Output.Small_Enumerated =>
		    Output_Object.Is_Hanging := True;

            when others => null;

	end case;

    end Start_Paragraph;


    procedure End_Paragraph (Output_Object : in out Blackhole_Output_Type) is
	-- End a paragraph.
	use type ARM_Output.Paragraph_Indent_Type;
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
	Output_Object.Is_In_Paragraph := False;
    end End_Paragraph;


    procedure Category_Header (Output_Object : in out Blackhole_Output_Type;
			       Header_Text : String) is
	-- Output a Category header (that is, "Legality Rules",
	-- "Dynamic Semantics", etc.)
	-- (Note: We did not use a enumeration here to insure that these
	-- headers are spelled the same in all output versions).
	-- Raises Not_Valid_Error if in a paragraph.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Header in paragraph");
	end if;
    end Category_Header;


    procedure Clause_Header (Output_Object     : in out Blackhole_Output_Type;
			     Header_Text       : in String;
			     Level	       : in ARM_Contents.Level_Type;
			     Clause_Number     : in String;
			     Top_Level_Subdivision_Name : in ARM_Output.Top_Level_Subdivision_Name_Kind;
			     No_Page_Break     : in Boolean := False) is
	-- Output a Clause header. The level of the header is specified
	-- in Level. The Clause Number is as specified; the top-level (and
	-- other) subdivision names are as specified. These should appear in
	-- the table of contents.
	-- For hyperlinked formats, this should generate a link target.
	-- If No_Page_Break is True, suppress any page breaks.
	-- Raises Not_Valid_Error if in a paragraph.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Header in paragraph");
	end if;
    end Clause_Header;


    procedure Revised_Clause_Header
			    (Output_Object     : in out Blackhole_Output_Type;
			     New_Header_Text   : in String;
			     Old_Header_Text   : in String;
			     Level	       : in ARM_Contents.Level_Type;
			     Clause_Number     : in String;
			     Version	       : in ARM_Contents.Change_Version_Type;
			     Old_Version       : in ARM_Contents.Change_Version_Type;
			     Top_Level_Subdivision_Name : in ARM_Output.Top_Level_Subdivision_Name_Kind;
        		     No_Page_Break     : in Boolean := False) is
	-- Output a revised clause header. Both the original and new text will
	-- be output. The level of the header is specified in Level. The Clause
	-- Number is as specified; the top-level (and other) subdivision names
	-- are as specified. These should appear in the table of contents.
	-- For hyperlinked formats, this should generate a link target.
	-- Version is the insertion version of the new text; Old_Version is
	-- the insertion version of the old text.
	-- If No_Page_Break is True, suppress any page breaks.
	-- Raises Not_Valid_Error if in a paragraph.
	function Header_Text return String is
	    -- Note: Version and Old_Version are not used.
	begin
	    return '{' & New_Header_Text & "} [" & Old_Header_Text & ']';
	end Header_Text;
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Header in paragraph");
	end if;
    end Revised_Clause_Header;


    procedure TOC_Marker (Output_Object : in out Blackhole_Output_Type;
			  For_Start : in Boolean) is
	-- Mark the start (if For_Start is True) or end (if For_Start is
	-- False) of the table of contents data. Output objects that
	-- auto-generate the table of contents can use this to do needed
	-- actions.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	null; -- We don't care about this.
    end TOC_Marker;


    procedure New_Page (Output_Object : in out Blackhole_Output_Type;
			Kind : ARM_Output.Page_Kind_Type := ARM_Output.Any_Page) is
	-- Output a page break.
	-- Note that this has no effect on non-printing formats.
	-- Any_Page breaks to the top of the next page (whatever it is);
	-- Odd_Page_Only breaks to the top of the odd-numbered page;
	-- Soft_Page allows a page break but does not force one (use in
	-- "No_Breaks" paragraphs.)
	-- Raises Not_Valid_Error if in a paragraph if Kind = Any_Page or
	-- Odd_Page, and if not in a paragraph if Kind = Soft_Page.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
    end New_Page;


    procedure New_Column (Output_Object : in out Blackhole_Output_Type) is
	-- Output a column break.
	-- Raises Not_Valid_Error if in a paragraph, or if the number of
	-- columns is 1.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"New column in paragraph");
	end if;
	-- No columns in text format.
    end New_Column;


    procedure Start_Table (Output_Object : in out Blackhole_Output_Type;
			   Columns : in ARM_Output.Column_Count;
			   First_Column_Width : in ARM_Output.Column_Count;
			   Last_Column_Width : in ARM_Output.Column_Count;
			   Alignment : in ARM_Output.Column_Text_Alignment;
			   No_Page_Break : in Boolean;
			   Has_Border : in Boolean;
			   Small_Text_Size : in Boolean;
			   Header_Kind : in ARM_Output.Header_Kind_Type) is
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
    begin
	-- Alignment, No_Page_Break, Border, Small_Text_Size, and Header_Kind
	-- not used here.
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Table in paragraph");
	end if;

	Output_Object.Is_In_Paragraph := True;
	Output_Object.Is_In_Table := True;
    end Start_Table;


    procedure Table_Marker (Output_Object : in out Blackhole_Output_Type;
			    Marker : in ARM_Output.Table_Marker_Type) is
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
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if (not Output_Object.Is_In_Paragraph) or (not Output_Object.Is_In_Table) then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Table marker not in table");
	end if;
	case Marker is
	    when ARM_Output.End_Table =>
		Output_Object.Is_In_Paragraph := False;
		Output_Object.Is_In_Table := False;
            when others => null;
	end case;
     end Table_Marker;


    procedure Separator_Line (Output_Object : in out Blackhole_Output_Type;
			      Is_Thin : Boolean := True) is
	-- Output a separator line. It is thin if "Is_Thin" is true.
	-- Raises Not_Valid_Error if in a paragraph.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Separator in paragraph");
	end if;
    end Separator_Line;


    -- Text output: These are only allowed after a Start_Paragraph and
    -- before any End_Paragraph. Raises Not_Valid_Error if not allowed.

    procedure Ordinary_Text (Output_Object : in out Blackhole_Output_Type;
			     Text : in String) is
	-- Output ordinary text.
	-- The text must end at a word break, never in the middle of a word.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
    end Ordinary_Text;


    procedure Ordinary_Character (Output_Object : in out Blackhole_Output_Type;
			          Char : in Character) is
	-- Output an ordinary character.
	-- Spaces will be used to break lines as needed.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;

    end Ordinary_Character;


    procedure Hard_Space (Output_Object : in out Blackhole_Output_Type) is
	-- Output a hard space. No line break should happen at a hard space.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
    end Hard_Space;


    procedure Line_Break (Output_Object : in out Blackhole_Output_Type) is
	-- Output a line break. This does not start a new paragraph.
	-- This corresponds to a "<BR>" in HTML.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
    end Line_Break;


    procedure Index_Line_Break (Output_Object : in out Blackhole_Output_Type;
				Clear_Keep_with_Next : in Boolean) is
	-- Output a line break for the index. This does not start a new
	-- paragraph in terms of spacing. This corresponds to a "<BR>"
	-- in HTML. If Clear_Keep_with_Next is true, insure that the next
	-- line does not require the following line to stay with it.
	-- Raises Not_Valid_Error if the paragraph is not in the index format.
    begin
	Line_Break (Output_Object);
    end Index_Line_Break;


    procedure Soft_Line_Break (Output_Object : in out Blackhole_Output_Type) is
	-- Output a soft line break. This is a place (in the middle of a
	-- "word") that we allow a line break. It is usually used after
	-- underscores in long non-terminals.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
    end Soft_Line_Break;


    procedure Soft_Hyphen_Break (Output_Object : in out Blackhole_Output_Type) is
	-- Output a soft line break, with a hyphen. This is a place (in the middle of
	-- a "word") that we allow a line break. If the line break is used,
	-- a hyphen will be added to the text.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
    end Soft_Hyphen_Break;


    procedure Tab (Output_Object : in out Blackhole_Output_Type) is
	-- Output a tab, inserting space up to the next tab stop.
	-- Raises Not_Valid_Error if the paragraph was created with
	-- Tab_Stops = ARM_Output.NO_TABS.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
    end Tab;


    procedure Special_Character (Output_Object : in out Blackhole_Output_Type;
			         Char : in ARM_Output.Special_Character_Type) is
	-- Output an special character.
    begin
        null;
    end Special_Character;


    procedure Unicode_Character (Output_Object : in out Blackhole_Output_Type;
			         Char : in ARM_Output.Unicode_Type) is
	-- Output a Unicode character, with code position Char.
	Char_Code : constant String := ARM_Output.Unicode_Type'Image(Char);
    begin
        null;
    end Unicode_Character;


    procedure End_Hang_Item (Output_Object : in out Blackhole_Output_Type) is
	-- Marks the end of a hanging item. Call only once per paragraph.
	-- Raises Not_Valid_Error if the paragraph style is not in
	-- Text_Prefixed_Style_Subtype, or if this has already been
	-- called for the current paragraph, or if the paragraph was started
	-- with No_Prefix = True.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
	if not Output_Object.Is_Hanging then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not a hanging paragraph");
	end if;
	if Output_Object.Saw_Hang_End then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Already saw the end of the hanging part");
	end if;
	Output_Object.Saw_Hang_End := True;

    end End_Hang_Item;


    procedure Text_Format (Output_Object : in out Blackhole_Output_Type;
			   Format : in ARM_Output.Format_Type) is
	-- Change the text format so that all of the properties are as specified.
	-- Note: Changes to these properties ought be stack-like; that is,
	-- Bold on, Italic on, Italic off, Bold off is OK; Bold on, Italic on,
	-- Bold off, Italic off should be avoided (as separate commands).
	-- Note: We ignore colors here, no colors in !Corrigendum markup.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;

    end Text_Format;


    procedure Clause_Reference (Output_Object : in out Blackhole_Output_Type;
				Text : in String;
				Clause_Number : in String) is
	-- Generate a reference to a clause in the standard. The text of
	-- the reference is "Text", and the number of the clause is
	-- Clause_Number. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.
    begin
	Ordinary_Text (Output_Object, Text); -- Nothing special in this format.
    end Clause_Reference;


    procedure Index_Target (Output_Object : in out Blackhole_Output_Type;
			    Index_Key : in Natural) is
	-- Generate a index target. This marks the location where an index
	-- reference occurs. Index_Key names the index item involved.
	-- For hyperlinked formats, this should generate a link target;
	-- for other formats, nothing is generated.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
	null; -- Nothing to do for plain text.
    end Index_Target;


    procedure Index_Reference (Output_Object : in out Blackhole_Output_Type;
			       Text : in String;
			       Index_Key : in Natural;
			       Clause_Number : in String) is
	-- Generate a reference to an index target in the standard. The text
	-- of the reference is "Text", and Index_Key and Clause_Number denotes
	-- the target. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.
    begin
	Ordinary_Text (Output_Object, Text); -- Nothing special in this format.
    end Index_Reference;


    procedure DR_Reference (Output_Object : in out Blackhole_Output_Type;
			    Text : in String;
			    DR_Number : in String) is
	-- Generate a reference to an DR from the standard. The text
	-- of the reference is "Text", and DR_Number denotes
	-- the target. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.
    begin
	Ordinary_Text (Output_Object, Text); -- Nothing special in this format.
    end DR_Reference;


    procedure AI_Reference (Output_Object : in out Blackhole_Output_Type;
			    Text : in String;
			    AI_Number : in String) is
	-- Generate a reference to an AI from the standard. The text
	-- of the reference is "Text", and AI_Number denotes
	-- the target (in unfolded format). For hyperlinked formats, this
	-- should generate a link; for other formats, the text alone is
	-- generated.
    begin
	Ordinary_Text (Output_Object, Text); -- Nothing special in this format.
    end AI_Reference;


    procedure Local_Target (Output_Object : in out Blackhole_Output_Type;
			    Text : in String;
			    Target : in String) is
	-- Generate a local target. This marks the potential target of local
	-- links identified by "Target". Text is the text of the target.
	-- For hyperlinked formats, this should generate a link target;
	-- for other formats, only the text is generated.
    begin
	Ordinary_Text (Output_Object, Text); -- Nothing special in this format.
    end Local_Target;


    procedure Local_Link (Output_Object : in out Blackhole_Output_Type;
			  Text : in String;
			  Target : in String;
			  Clause_Number : in String) is
	-- Generate a local link to the target and clause given.
	-- Text is the text of the link.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.
    begin
	Ordinary_Text (Output_Object, Text); -- Nothing special in this format.
    end Local_Link;


    procedure Local_Link_Start (Output_Object : in out Blackhole_Output_Type;
				Target : in String;
				Clause_Number : in String) is
	-- Generate a local link to the target and clause given.
	-- The link will surround text until Local_Link_End is called.
	-- Local_Link_End must be called before this routine can be used again.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.
    begin
	null; -- No link, nothing to do.
    end Local_Link_Start;


    procedure Local_Link_End (Output_Object : in out Blackhole_Output_Type;
			      Target : in String;
			      Clause_Number : in String) is
	-- End a local link for the target and clause given.
	-- This must be in the same paragraph as the Local_Link_Start.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.
    begin
	null; -- No link, nothing to do.
    end Local_Link_End;


    procedure URL_Link (Output_Object : in out Blackhole_Output_Type;
			Text : in String;
			URL : in String;
                        All_Formats : in Boolean) is
	-- Generate a link to the URL given.
	-- Text is the text of the link.
        -- If All_Formats is True, then the link is generated in any format that
        -- can support a link. Otherwise, a link is only generated in formats
        -- that are primarily hyperlinked (such as HTML). If no link is
        -- generated, the text still should be generated.
    begin
	Ordinary_Text (Output_Object, Text); -- Nothing special in this format.
    end URL_Link;


    procedure Picture  (Output_Object : in out Blackhole_Output_Type;
			Name  : in String;
			Descr : in String;
			Alignment : in ARM_Output.Picture_Alignment;
			Height, Width : in Natural;
			Border : in ARM_Output.Border_Kind) is
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
    begin
	Ordinary_Text (Output_Object, "[Picture: " & Name &
	  " - " & Descr & "]");
    end Picture;

end ARM_Blackhole;
