with ARM_Output,
     ARM_Contents,
     Ada.Text_IO,
     Ada.Exceptions,
     Ada.Streams.Stream_IO,
     Ada.Strings.Maps,
     Ada.Strings.Fixed,
     Ada.Characters.Handling,
     Ada.Calendar,
     Ada.Unchecked_Conversion;
package body ARM_RTF is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package defines the RTF output object.
    -- Output objects are responsible for implementing the details of
    -- a particular format.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2002, 2004, 2005, 2006, 2007, 2009, 2010, 2011
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
    --  5/27/00 - RLB - Implemented table operations.
    --  5/28/00 - RLB - Added index style.
    --  6/ 2/00 - RLB - Added Soft_Line_Break.
    --		- RLB - Changed back to mostly "after" spacing, so paragraph
    --			numbers format correctly on tops of pages.
    --  8/ 2/00 - RLB - Added Soft_Hyphen_Break and left and right quote
    --			characters.
    --		- RLB - Added additional styles.
    --          - RLB - Fixed bulleted styles to have a right indent as well.
    --  8/ 4/00 - RLB - Added additional styles.
    --		- RLB - Fixed table text size.
    --  8/ 7/00 - RLB - Added Leading flag to Start_Paragraph, removed "Leading"
    --			styles.
    --  8/ 8/00 - RLB - Added "Hang_Width" in order to get more accurate
    --			hang prefix determination.
    --  8/10/00 - RLB - Style corrections.
    --  8/11/00 - RLB - Fixed footers for clauses with very long titles.
    --		- RLB - Added Hanging_in_Bulleted styles.
    --  8/16/00 - RLB - Adjusted so captial 'I' is not a large character.
    --		- RLB - Added Code_Indented_Nested_Bulleted and Notes_Nested_Bulleted.
    --  8/17/00 - RLB - Replaced "Leading" by "Space_After".
    -- 		- RLB - Added Nested_Enumerated.
    --  8/21/00 - RLB - Moved paragraph numbers in a bit for AARM.
    --  8/22/00 - RLB - Added Revised_Clause_Header.
    --  8/23/00 - RLB - Revised widths of AARM text to be more like RM.
    --  8/31/00 - RLB - Moved paragraphs in again.
    --  9/26/00 - RLB - Added Syntax_Summary style.
    --  9/27/00 - RLB - Cut the lower margin for the AARM pages.
    --  7/18/02 - RLB - Removed Document parameter, replaced by three
    --			strings.
    --		- RLB - Added AI_Reference.
    --		- RLB - Added Change_Version_Type and uses.
    --  9/10/04 - RLB - Added "Both" to possible changes to handle
    --			replacement of changed text.
    --  9/14/04 - RLB - Moved Change_Version_Type to contents.
    --  9/15/04 - RLB - Completely rewrote Text_Format to avoid problems
    --			with ordering of calls.
    -- 11/03/04 - RLB - Added Nested_X2_Bulleted.
    -- 11/15/04 - RLB - Added Indented_Nested_Bulleted.
    -- 11/16/04 - RLB - Experimented with changes to avoid the hot pink
    --			revision markers.
    -- 12/15/04 - RLB - Added Pascal's workaround to formatting bugs in
    --			Word 2000/XP/2003.
    -- 12/16/04 - RLB - Removed it after it proved not to help.
    --  1/24/05 - RLB - Added Inner_Indent.
    --  2/ 1/05 - RLB - Added Turkish chars to allow an AARM note.
    --  5/27/05 - RLB - Added arbitrary Unicode characters.
    --  1/11/06 - RLB - Eliminated dispatching Create in favor of tailored
    --			versions.
    --  1/18/06 - RLB - Added additional styles.
    --  2/ 8/06 - RLB - Added additional parameters to the table command.
    --  2/10/06 - RLB - Added even more additional parameters to the
    --			table command.
    --		- RLB - Added picture command.
    --  9/21/06 - RLB - Added Body_Font; revised styles to handle that.
    --  9/22/06 - RLB - Added Subsubclause.
    --  9/25/06 - RLB - Handled optional renaming of TOC.
    --		- RLB - Added Last_Column_Width to Start_Table.
    -- 10/10/06 - RLB - Widened bulleted and enumerated hanging a bit to make
    --			room for wider numbers for ISO 2004 format.
    -- 10/13/06 - RLB - Added Local_Link_Start and Local_Link_End to allow
    --			formatting in the linked text.
    -- 12/22/06 - RLB - Fixed glitch in number of column units.
    --  2/ 9/07 - RLB - Changed comments on AI_Reference.
    --  2/14/07 - RLB - Revised to separate style and indent information
    --			for paragraphs.
    --  2/16/07 - RLB - Added example styles for additional nesting levels.
    --  2/19/07 - RLB - Added Standard Title style.
    -- 12/18/07 - RLB - Fixed silly table bug.
    --		- RLB - Added Plain_Annex.
    -- 12/19/07 - RLB - Added limited colors to Text_Format.
    -- 12/21/07 - RLB - Removed extra "not" from "large" character counter
    --			in Ordinary_Character.
    --  5/ 4/09 - RLB - Added the footer information.
    --  5/ 6/09 - RLB - Added ISO format footer and version names.
    -- 10/28/09 - RLB - Added missing with.
    --  3/31/10 - RLB - Adjusted picture scaling to be closer to reality.
    -- 10/18/11 - RLB - Changed to GPLv3 license.
    -- 10/20/11 - RLB - Updated to handle extra-wide paragraph numbers
    --			automatically (there are too many to hand-fix now).
    -- 10/25/11 - RLB - Added old insertion version to Revised_Clause_Header.

    -- Note: We assume a lot about the Section_Names passed into
    -- Section in order to get the proper headers/footers/page numbers.
    -- Someday, that ought to be changed somehow.

    LINE_LENGTH : constant := 78;
	-- Maximum intended line length.

    LEADING_PERCENT : constant := 70;
	-- Leading is 70% of normal height.
    TRAILING_PERCENT : constant := 150;
	-- Leading is 150% of normal height.

    type Format_Info_Type is record
	Defined : Boolean := False;
	Size : Natural; -- In 1/2 pts.
	Indent : Natural; -- In Twips (.1 pt = 1/120th pica = 1/1440th inch).
	Hang_Width : Natural; -- In Twips (.1 pt = 1/120th pica = 1/1440th inch).
	Before : Natural; -- Vertical space before in Twips. (\sb)
	After : Natural; -- Vertical space after in Twips. (\sa)
	Is_Justified : Boolean; -- True if the format is justified. (\qj vs. \qc or \ql)
	Number : Natural; -- Style number.
	Format_String : String(1 .. 200);
	Format_Len : Natural := 0;
    end record;

    Paragraph_Info : array (ARM_Output.Paragraph_Style_Type,
			    ARM_Output.Paragraph_Indent_Type) of Format_Info_Type;
	-- Set by Start_RTF_File.
    Heading_1_Info : Format_Info_Type;
    Heading_2_Info : Format_Info_Type;
    Heading_3_Info : Format_Info_Type;
    Heading_4_Info : Format_Info_Type;
    Category_Header_Info : Format_Info_Type;
    Normal_Paragraph_Number_Info : Format_Info_Type;
    Wide_Paragraph_Number_Info : Format_Info_Type;
    Header_Info : Format_Info_Type;
    Footer_Info : Format_Info_Type;
    TOC_1_Info : Format_Info_Type;
    TOC_2_Info : Format_Info_Type;
    TOC_3_Info : Format_Info_Type;
    TOC_4_Info : Format_Info_Type;

    Table_C_Text_Info : Format_Info_Type;
    Table_L_Text_Info : Format_Info_Type;
    Table_C_Sml_Text_Info : Format_Info_Type;
    Table_L_Sml_Text_Info : Format_Info_Type;

    procedure Set_Style (Into : in out Format_Info_Type;
			 Font : in ARM_Output.Font_Family_Type; -- Default means use Body_Font.
			 Body_Font : in ARM_Output.Font_Family_Type;
			 Font_Size : in Natural;
			 Style_Indent : in Natural;
			 Style_Hang_Width : in Natural := 0;
			 Style_Before : in Natural;
			 Style_After : in Natural;
			 Style_Justified : in Boolean;
			 Style_String_Prefix : in String;
			 Style_String_Suffix : in String) is
	-- Internal routine.
	-- Set the indicated style information.
	-- The two part of the Style String includes all of the information
	-- except Font (\fn), Font_Size (\fsnn), Indent (\linn), Hang (\fi-nn),
	-- Before (\sbnn), and After (\sann) -- these are added appropriately
	-- between the halves of the string.
	Font_String : String(1..3) := "\f0";

	function Make (Code : in String; Value : in Natural) return String is
	    Val : constant String := Natural'Image(Value);
	begin
	    if Value /= 0 then
		return '\' & Code & Val(2..Val'Last);
	    else
		return ""; -- Make nothing.
	    end if;
	end Make;

    begin
	Into.Defined := True;
	Into.Size := Font_Size;
	case Font is
	    when ARM_Output.Default =>
		if ARM_Output."=" (Body_Font, ARM_Output.Swiss) then
		    Font_String(3) := '1';
		    Into.Size := Into.Size - 1;
		else -- Roman.
		    Font_String(3) := '0';
		end if;
	    when ARM_Output.Roman =>
		Font_String(3) := '0';
	    when ARM_Output.Swiss =>
		Font_String(3) := '1';
	    when ARM_Output.Fixed =>
		Font_String(3) := '2';
	end case;
	Into.Indent := Style_Indent;
	Into.Hang_Width := Style_Hang_Width;
	Into.Before := Style_Before;
	Into.After  := Style_After;
	Into.Is_Justified := Style_Justified;
	declare
	    Style : constant String :=
			Style_String_Prefix & " " &
			Font_String &
			Make ("fs", Into.Size) &
			Make ("li", Into.Indent) &
			Make ("fi-", Into.Hang_Width) &
			Make ("sb", Into.Before) &
			Make ("sa", Into.After) &
			Style_String_Suffix;
	begin
	    Ada.Strings.Fixed.Move (Source => Style,
				    Target => Into.Format_String);
            Into.Format_Len := Style'Length;
	end;
    end Set_Style;


    procedure Write_Style (Fyle : in Ada.Text_IO.File_Type;
			   Style : in Format_Info_Type) is
	-- Internal routine.
	-- Write a header to start a style definition for Style.
    begin
	if not Style.Defined then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Undefined Style in style table");
	end if;
        Ada.Text_IO.Put (Fyle, "{" &
	    Style.Format_String(1 .. Style.Format_Len));
    end Write_Style;


    procedure Write_Style_for_Paragraph (Fyle : in Ada.Text_IO.File_Type;
					 Style : in Format_Info_Type;
					 Count : out Natural) is
	-- Internal routine.
	-- Write a header to start a paragraph in Style. Count is set to
	-- the characters written.
    begin
	if not Style.Defined then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Undefined Style in paragraph");
	end if;
        Ada.Text_IO.Put (Fyle, "{\pard\plain " &
	    Style.Format_String(1 .. Style.Format_Len));
	Count := Style.Format_Len + 13;
    end Write_Style_for_Paragraph;


    procedure Write_Headers (Output_Object : in out RTF_Output_Type) is
	-- Write the page headers for this object into the current file.
	Junk : Natural;
    begin
        -- Default header/footer:
        Ada.Text_IO.Put (Output_Object.Output_File, "{\headerl ");
        Write_Style_for_Paragraph (Output_Object.Output_File, Header_Info, Junk);
	if Ada.Strings.Unbounded.Length (Output_Object.Header_Prefix) = 0 then
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\b\f1 " &
		Ada.Strings.Unbounded.To_String (Output_Object.Title) &
		"\par}}}");
	elsif Ada.Strings.Unbounded.Length (Output_Object.Title) = 0 then
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\b\f1 " &
		Ada.Strings.Unbounded.To_String (Output_Object.Header_Prefix) &
		"\par}}}");
	else
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\b\f1 " &
		Ada.Strings.Unbounded.To_String (Output_Object.Header_Prefix) &
		" \emdash  " &
		Ada.Strings.Unbounded.To_String (Output_Object.Title) &
		"\par}}}");
	end if;
        Ada.Text_IO.Put (Output_Object.Output_File, "{\headerr ");
        Write_Style_for_Paragraph (Output_Object.Output_File, Header_Info, Junk);
	if Ada.Strings.Unbounded.Length (Output_Object.Header_Prefix) = 0 then
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\qr\b\f1 " &
		Ada.Strings.Unbounded.To_String (Output_Object.Title) &
		"\par}}}");
	elsif Ada.Strings.Unbounded.Length (Output_Object.Title) = 0 then
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\qr\b\f1 " &
		Ada.Strings.Unbounded.To_String (Output_Object.Header_Prefix) &
		"\par}}}");
	else
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\qr\b\f1 " &
		Ada.Strings.Unbounded.To_String (Output_Object.Header_Prefix) &
		" \emdash  " &
		Ada.Strings.Unbounded.To_String (Output_Object.Title) &
		"\par}}}");
	end if;
-- Note: We don't need the default footers; none probably work better, anyway.
--	Ada.Text_IO.Put (Output_Object.Output_File, "{\footerl ");
--	Write_Style_for_Paragraph (Output_Object.Output_File, Footer_Info, Junk);
--	if Name = "00" or else Name = "TOC" or else Name = "Ttl" then
--	    -- No section number.
--	    Ada.Text_IO.Put (Output_Object.Output_File, "{\f0 ");
--	else
--            Ada.Text_IO.Put (Output_Object.Output_File, "{\b\f1 ");
--	    if Name(1) = '0' then -- Strip leading zero...
--                Ada.Text_IO.Put (Output_Object.Output_File, Name(2..Name'Last));
--	    else
--                Ada.Text_IO.Put (Output_Object.Output_File, Name);
--	    end if;
--	    Ada.Text_IO.Put (Output_Object.Output_File, "}\~\~\~{\f0 ");
--	end if;
--        Ada.Text_IO.Put (Output_Object.Output_File, Title);
--        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\tab \tab{\field{\*\fldinst { PAGE }}{\fldrslt {\lang1024 x}}}\par}}}");
--	Ada.Text_IO.Put (Output_Object.Output_File, "{\footerr ");
--	Write_Style_for_Paragraph (Output_Object.Output_File, Footer_Info, Junk);
--	Ada.Text_IO.Put (Output_Object.Output_File, "{\f0 {\field{\*\fldinst { PAGE }}{\fldrslt {\lang1024 x}}}\tab \tab ");
--        Ada.Text_IO.Put (Output_Object.Output_File, Title);
--	if Name = "00" or else Name = "TOC" or else Name = "Ttl" then
--	    null; -- No section number.
--	else
--	    Ada.Text_IO.Put (Output_Object.Output_File, "\~\~\~\b\f1 ");
--	    if Name(1) = '0' then -- Strip leading zero...
--                Ada.Text_IO.Put (Output_Object.Output_File, Name(2..Name'Last));
--	    else
--                Ada.Text_IO.Put (Output_Object.Output_File, Name);
--	    end if;
--	end if;
--	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\par}}}");
    end Write_Headers;


    procedure Start_RTF_File (Output_Object : in out RTF_Output_Type;
			      File_Name : in String;
			      Title : in String;
			      Name : in String) is
	-- Internal routine.
	-- Create an RTF file, and generate the needed text to start an RTF
	-- file. The file name is just the name portion, not the path or
	-- extension. "Name" is a short identifier, typically the clause
	-- number or letter.

	INDENT_UNIT : constant := 360;

	subtype PWidth is String(1..4);
	function Paper_Width return PWidth is
	    -- Return the paper width in twips:
	begin
	    case Output_Object.Page_Size is
		when ARM_RTF.A4 =>
		    return "9030";
		when ARM_RTF.Letter =>
		    return "9360";
		when ARM_RTF.Half_Letter =>
		    return "5760";
		when ARM_RTF.Ada95 =>
		    return "7740";
	    end case;
	end Paper_Width;

	function Half_Paper_Width return PWidth is
	    -- Return the center of the paper width in twips:
	begin
	    case Output_Object.Page_Size is
		when ARM_RTF.A4 =>
		    return "4515";
		when ARM_RTF.Letter =>
		    return "4680";
		when ARM_RTF.Half_Letter =>
		    return "2880";
		when ARM_RTF.Ada95 =>
		    return "3870";
	    end case;
	end Half_Paper_Width;


    begin
	Ada.Text_IO.Create (Output_Object.Output_File, Ada.Text_IO.Out_File,
	    ".\Output\" & File_Name & ".RTF");


	-- Note: This header (simplified) is from a Word 97 created file.
	-- File introduction:
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\rtf1\ansi\ansicpg1252\uc1 \deff0\deflang1033\deflangfe1033");
	    -- rtf1=RTF file marker;
	    -- ansi=character set is ansi;
	    -- ansicpg=code page for ansi-Unicode conversion (1252);
	    -- uc1=One character replacement for Unicode characters;
	    -- deff0=Default font to use (Font 0);
	    -- deflang=default language (1033 - American English);
	    -- deflangfe=default language (asian text) (1033 - American English).

	-- Font table:
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\fonttbl");
	case Output_Object.Primary_Serif_Font is
	    when Times_New_Roman =>
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\f0\froman\fcharset0 Times New Roman;}");
	    when Souvenir =>
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\f0\froman\fcharset0 Souvenir{\*\falt Souvienne};}");
	end case;
	case Output_Object.Primary_Sans_Serif_Font is
	    when Helvetica =>
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\f1\fswiss\fcharset0 Helvetica{\*\falt Arial};}"); -- Usually Arial on most Windows machines.
	    when Arial =>
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\f1\fswiss\fcharset0 Arial{\*\falt Helvetica};}"); -- Give Arial preference, because otherwise it screwed up Jim Moore's machine.
	end case;
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\f2\fmodern\fcharset0\fprq1 Courier New;}");
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\f3\ftech\fcharset0 Symbol;}}");
	    -- f=Font number;
	    -- fswiss,froman,fmodern,gtech=font family;
	    -- fcharset=character set (0=Ansi);
	    -- fprq=pitch (0=anything (default); 1=fixed; 2=variable);
	    -- falt=alternative font name;
	    -- followed by the font name

	-- File table: (None used.)

	-- Color table:
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\colortbl;");
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\red0\green0\blue0;"); -- Color 1
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\red0\green0\blue255;"); -- Color 2
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\red0\green255\blue255;"); -- Color 3
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\red0\green255\blue0;"); -- Color 4
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\red255\green0\blue255;"); -- Color 5
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\red255\green0\blue0;"); -- Color 6
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\red255\green255\blue0;"); -- Color 7
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\red255\green255\blue255;"); -- Color 8
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\red0\green0\blue128;"); -- Color 9
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\red0\green128\blue128;"); -- Color 10
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\red0\green128\blue0;"); -- Color 11
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\red128\green0\blue128;"); -- Color 12
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\red128\green0\blue0;"); -- Color 13
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\red128\green128\blue0;"); -- Color 14
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\red128\green128\blue128;"); -- Color 15
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\red192\green192\blue192;}"); -- Color 16

	-- Style sheet:
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\stylesheet");

	if Output_Object.Page_Size = ARM_RTF.Ada95 or else
	   Output_Object.Page_Size = ARM_RTF.Half_Letter then
	    -- These are smaller page sizes than the other sizes.
	    -- ** TBD: Consider putting this into a separate parameter (there is
	    -- ** little reason for the font size to depend on the page size).
	    Set_Style (Paragraph_Info(ARM_Output.Normal, 0),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s0\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-220\slmult0 \snext0 ");
	    Set_Style (Heading_1_Info,
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 28,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 210,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s1\keepn\widctlpar\outlinelevel0\adjustright",
		       Style_String_Suffix => "\b\kerning28\qc\cgrid \sbasedon0 \snext0 ");
	    Set_Style (Heading_2_Info,
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 24,
		       Style_Indent => 0,
		       Style_Before => 240,
		       Style_After => 120,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s2\keepn\widctlpar\outlinelevel1\adjustright",
		       Style_String_Suffix => "\b\ql\cgrid \sbasedon0 \snext0 ");
	    Set_Style (Heading_3_Info,
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 23,
		       Style_Indent => 0,
		       Style_Before => 210,
		       Style_After => 90,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s3\keepn\widctlpar\outlinelevel2\adjustright",
		       Style_String_Suffix => "\b\ql\cgrid \sbasedon0 \snext0 ");
	    Set_Style (Category_Header_Info,
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 14,
		       Style_Indent => 0,
		       Style_Before => 100,
		       Style_After => 60,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s4\keepn\adjustright",
		       Style_String_Suffix => "\cgrid\qc\i \snext0 ");
	    Set_Style (Normal_Paragraph_Number_Info,
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 12,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After =>  0,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s5\keepn\widctlpar\adjustright " &
			 "\pvpara\phpg\posxo\posy0\absw450\dxfrtext100\dfrmtxtx120\dfrmtxty120"&
			 "\cgrid\qc",
		       Style_String_Suffix => "\snext0 "); -- Note: We adjust the space before on use.
	    Set_Style (Wide_Paragraph_Number_Info,
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 12,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After =>  0,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s5\keepn\widctlpar\adjustright " &
			 "\pvpara\phpg\posxo\posy0\absw490\dxfrtext100\dfrmtxtx120\dfrmtxty120"&
			 "\cgrid\qc",
		       Style_String_Suffix => "\snext0 "); -- Note: We adjust the space before on use.
	    Set_Style (Paragraph_Info(ARM_Output.Small, 1), -- Notes
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*1,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s6\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-180\slmult0 \snext6 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small, 2), -- Annotations
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*2,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s7\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-180\slmult0 \snext7 ");
	    Set_Style (Paragraph_Info(ARM_Output.Examples, 1),
		       Font => ARM_Output.Fixed,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 16,
		       Style_Indent => INDENT_UNIT*1,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s8\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\sl-160\ql \snext8 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Examples, 3),
		       Font => ARM_Output.Fixed,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 14,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Before => 0,
		       Style_After => 70,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s9\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\sl-140\ql \snext9 ");
	    Set_Style (Paragraph_Info(ARM_Output.Normal, 1), -- Syntax indent
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*1,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s10\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-200 \snext10 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Normal, 3), -- Regular indent
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Before => 0,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s11\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-220\slmult0 \snext11 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small, 5), -- Regular annotation indent
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*5,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s12\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-180\slmult0 \snext12 ");
	    Set_Style (Paragraph_Info(ARM_Output.Wide_Hanging, 3),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Hang_Width => 1080,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s13\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-200\slmult0 \snext13 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Narrow_Hanging, 3), -- Hanging in indents
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Hang_Width => 360,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s14\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-200\slmult0 \snext14 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Wide_Hanging, 5),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*5,
		       Style_Hang_Width => 1080,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s15\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-170\slmult0 \snext15 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Narrow_Hanging, 5),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*5,
		       Style_Hang_Width => 360,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s16\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-170\slmult0 \snext16 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Bulleted, 1),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*1,
		       Style_Hang_Width => 240,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s17\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-200\slmult0\tx360 \snext17 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Nested_Bulleted, 2),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*2,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s18\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-200\slmult0\tx720 \snext18 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Bulleted, 3),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s19\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-170\slmult0\tx1080 \snext19 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Nested_Bulleted, 4),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Hang_Width => 200,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s20\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-170\slmult0\tx1440 \snext20 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Bulleted, 4), -- Indented bullets
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Hang_Width => 240,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s21\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-200\slmult0\tx1080 \snext21 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Bulleted, 2), -- Syntax indent bullets
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*2,
		       Style_Hang_Width => 240,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s22\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-200\slmult0\tx720 \snext22 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Bulleted, 3), -- Code indented bullets
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Hang_Width => 240,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s23\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-200\slmult0\tx1080 \snext23 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Normal, 2), -- Code indented
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*2,
		       Style_Before => 0,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s24\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-220\slmult0 \snext24 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small, 4), -- Code indented annotations.
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s25\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-180\slmult0 \snext25 ");
	    Set_Style (Paragraph_Info(ARM_Output.Examples, 4), -- Indented examples
		       Font => ARM_Output.Fixed,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 16,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s26\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-160 \snext26 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Examples, 6), -- Indented annotation examples.
		       Font => ARM_Output.Fixed,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 14,
		       Style_Indent => INDENT_UNIT*6,
		       Style_Before => 0,
		       Style_After => 70,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s27\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-140 \snext27 ");

            Set_Style (Header_Info,
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 17,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
		         "\s28\widctlpar\tqc\tx" & Half_Paper_Width &
			 "\tqr\tx" & Paper_Width & "\adjustright",
		       Style_String_Suffix => "\cgrid \sbasedon0 \snext28 ");
            Set_Style (Footer_Info,
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 17,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
		         "\s29\widctlpar" & -- "\tqc\tx" & Half_Paper_Width & -- We don't use or define the center tab; it causes problems with very long titles.
			 "\tqr\tx" & Paper_Width & "\adjustright",
		       Style_String_Suffix => "\cgrid \sbasedon0 \snext29 ");
	    Set_Style (Paragraph_Info(ARM_Output.Index, 0),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => 225,
		       Style_Hang_Width => 225,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s31\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-180\slmult0 \snext31 ");
	    Set_Style (Paragraph_Info(ARM_Output.Wide_Above, 0),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => 0,
		       Style_Before => 120,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s32\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-220\slmult0 \snext0 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Wide_Above, 2), -- Wide above annotations.
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*2,
		       Style_Before => 90,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s33\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-180\slmult0 \snext7 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Header, 1), -- Notes header
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*1,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s34\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-180\slmult0 \snext6 ");
			  -- Note: No extra space afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Bulleted, 2), -- Bullets in notes
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*2,
		       Style_Hang_Width => 240,
		       Style_Before => 0,
		       Style_After => 60,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s35\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-170\slmult0\tx720 \snext35 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Nested_Bulleted, 3), -- Nested bullets in notes
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Hang_Width => 230,
		       Style_Before => 0,
		       Style_After => 60,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s36\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-170\slmult0\tx1080 \snext36 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Hanging_in_Bulleted, 3),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Hang_Width => 720,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s37\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-200\slmult0 \snext14 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Hanging_in_Bulleted, 5),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*5,
		       Style_Hang_Width => 720,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s38\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-170\slmult0 \snext16 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Nested_Bulleted, 4), -- Code indent nested bullets
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s39\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-200\slmult0\tx1440 \snext39 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Syntax_Summary, 1),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*1,
		       Style_Before => 0,
		       Style_After => 65,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s40\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-170\slmult0 \snext40 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Enumerated, 1),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*1,
		       Style_Hang_Width => 240,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s41\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-200\slmult0\tx360 \snext41 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Enumerated, 3),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s42\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-170\slmult0\tx1080 \snext42 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Enumerated, 2),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*2,
		       Style_Hang_Width => 260,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s43\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-200\slmult0\tx360 \snext43 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Enumerated, 4),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Hang_Width => 240,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s44\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-170\slmult0\tx1080 \snext44 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Nested_Bulleted, 3), -- Doubly nested bullets
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s45\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-200\slmult0\tx720 \snext45 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Nested_Bulleted, 5), -- Doubly nested bullets
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*5,
		       Style_Hang_Width => 200,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s46\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-170\slmult0\tx1440 \snext46 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Nested_Bulleted, 5), -- Indented nested bullets.
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*5,
		       Style_Hang_Width => 240,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s47\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-200\slmult0\tx1080 \snext47 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Normal, 4), -- Inner indented
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Before => 0,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s48\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-220\slmult0 \snext48 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small, 6), -- Inner indented
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*6,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s49\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-180\slmult0 \snext49 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small, 3), -- Annotations in syntax indentation
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s50\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-180\slmult0 \snext50 ");
	    Set_Style (Paragraph_Info(ARM_Output.Swiss_Examples, 1),
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 16,
		       Style_Indent => INDENT_UNIT*1,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s51\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\sl-180\ql \snext51 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Swiss_Examples, 3),
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 14,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Before => 0,
		       Style_After => 70,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s52\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\sl-160\ql \snext52 ");
	    Set_Style (Paragraph_Info(ARM_Output.Swiss_Examples, 4), -- Indented
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 16,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s53\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-180 \snext53 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Swiss_Examples, 6), -- Indented
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 14,
		       Style_Indent => INDENT_UNIT*6,
		       Style_Before => 0,
		       Style_After => 70,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s54\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-160 \snext54 ");
	    Set_Style (Paragraph_Info(ARM_Output.Enumerated, 3), -- Doubly nested enumerations
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Hang_Width => 260,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s55\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-200\slmult0\tx360 \snext43 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Enumerated, 5), -- Doubly nested enumerations
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => INDENT_UNIT*5,
		       Style_Hang_Width => 240,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s56\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-170\slmult0\tx1080 \snext56 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Examples, 2), -- Syntax Indented examples
		       Font => ARM_Output.Fixed,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 16,
		       Style_Indent => INDENT_UNIT*2,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s57\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-160 \snext57 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Examples, 4), -- Syntax Indented annotation examples.
		       Font => ARM_Output.Fixed,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 14,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Before => 0,
		       Style_After => 70,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s58\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-140 \snext58 ");
	    Set_Style (Paragraph_Info(ARM_Output.Swiss_Examples, 2), -- Syntax Indented
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 16,
		       Style_Indent => INDENT_UNIT*2,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s59\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-180 \snext59 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Swiss_Examples, 4), -- Syntax Indented
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 14,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Before => 0,
		       Style_After => 70,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s60\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-160 \snext60 ");
	    Set_Style (Paragraph_Info(ARM_Output.Examples, 3), -- Code Indented examples
		       Font => ARM_Output.Fixed,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 16,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s61\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-160 \snext61 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Examples, 5), -- Code Indented annotation examples.
		       Font => ARM_Output.Fixed,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 14,
		       Style_Indent => INDENT_UNIT*5,
		       Style_Before => 0,
		       Style_After => 70,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s62\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-140 \snext62 ");
	    Set_Style (Paragraph_Info(ARM_Output.Swiss_Examples, 3), -- Code Indented
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 16,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s63\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-180 \snext63 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Swiss_Examples, 5), -- Code Indented
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 14,
		       Style_Indent => INDENT_UNIT*5,
		       Style_Before => 0,
		       Style_After => 70,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s64\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-160 \snext64 ");
	    Set_Style (Paragraph_Info(ARM_Output.Title, 0),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 34, -- Slightly less than double.
		       Style_Indent => 0,
		       Style_Before => 120,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s65\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-360\slmult0 \snext0 ");

	    -- New styles should be added here, following numbers will need adjustments.
	    Set_Style (Heading_4_Info,
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => 0,
		       Style_Before => 210,
		       Style_After => 90,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s66\keepn\widctlpar\outlinelevel3\adjustright",
		       Style_String_Suffix => "\b\ql\cgrid \sbasedon0 \snext0 ");
	    if Output_Object.Big_Files then
		-- Define the TOC styles:
                Set_Style (TOC_1_Info,
		           Font => ARM_Output.Swiss,
		           Body_Font => Output_Object.Body_Font,
		           Font_Size => 20,
		           Style_Indent => 0,
		           Style_Before => 45,
		           Style_After => 45,
		           Style_Justified => FALSE,
		           Style_String_Prefix =>
		             "\s67\widctlpar\tqr\tldot\tx" & Paper_Width & "\adjustright",
		           Style_String_Suffix => "\b\cgrid \sbasedon0 \snext0 ");
                Set_Style (TOC_2_Info,
		           Font => ARM_Output.Swiss,
		           Body_Font => Output_Object.Body_Font,
		           Font_Size => 17,
		           Style_Indent => 200,
		           Style_Before => 0,
		           Style_After => 0,
		           Style_Justified => FALSE,
		           Style_String_Prefix =>
		             "\s68\widctlpar\tqr\tldot\tx" & Paper_Width & "\adjustright",
		           Style_String_Suffix => "\b\cgrid \sbasedon0 \snext0 ");
                Set_Style (TOC_3_Info,
		           Font => ARM_Output.Swiss,
		           Body_Font => Output_Object.Body_Font,
		           Font_Size => 17,
		           Style_Indent => 400,
		           Style_Before => 0,
		           Style_After => 0,
		           Style_Justified => FALSE,
		           Style_String_Prefix =>
		             "\s69\widctlpar\tqr\tldot\tx" & Paper_Width & "\adjustright",
		           Style_String_Suffix => "\b\cgrid \sbasedon0 \snext0 ");
                Set_Style (TOC_4_Info,
		           Font => ARM_Output.Swiss,
		           Body_Font => Output_Object.Body_Font,
		           Font_Size => 17,
		           Style_Indent => 600,
		           Style_Before => 0,
		           Style_After => 0,
		           Style_Justified => FALSE,
		           Style_String_Prefix =>
		             "\s70\widctlpar\tqr\tldot\tx" & Paper_Width & "\adjustright",
		           Style_String_Suffix => "\b\cgrid \sbasedon0 \snext0 ");
	    end if;
	    Set_Style (Table_C_Text_Info,
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => 0,
		       Style_Before => 20,
		       Style_After => 20,
		       Style_Justified => FALSE,
		       Style_String_Prefix => "",
		       Style_String_Suffix => "\qc ");
		-- We use a bit of space above and below to avoid overrunning
		-- the borders of the cells.
	    Set_Style (Table_L_Text_Info,
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => 0,
		       Style_Before => 20,
		       Style_After => 20,
		       Style_Justified => FALSE,
		       Style_String_Prefix => "",
		       Style_String_Suffix => "\ql ");
	    Set_Style (Table_C_Sml_Text_Info,
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => 0,
		       Style_Before => 20,
		       Style_After => 20,
		       Style_Justified => FALSE,
		       Style_String_Prefix => "",
		       Style_String_Suffix => "\qc ");
	    Set_Style (Table_L_Sml_Text_Info,
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 15,
		       Style_Indent => 0,
		       Style_Before => 20,
		       Style_After => 20,
		       Style_Justified => FALSE,
		       Style_String_Prefix => "",
		       Style_String_Suffix => "\ql ");
	else
	    Set_Style (Paragraph_Info(ARM_Output.Normal, 0),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s0\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-260\slmult0 \snext0 ");
	    Set_Style (Heading_1_Info,
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 36,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 210,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s1\keepn\widctlpar\outlinelevel0\adjustright",
		       Style_String_Suffix => "\b\kerning36\qc\cgrid \sbasedon0 \snext0 ");
	    Set_Style (Heading_2_Info,
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 28,
		       Style_Indent => 0,
		       Style_Before => 240,
		       Style_After => 120,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s2\keepn\widctlpar\outlinelevel1\adjustright",
		       Style_String_Suffix => "\b\ql\cgrid \sbasedon0 \snext0 ");
	    Set_Style (Heading_3_Info,
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 28,
		       Style_Indent => 0,
		       Style_Before => 210,
		       Style_After => 100,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s3\keepn\widctlpar\outlinelevel2\adjustright",
		       Style_String_Suffix => "\b\ql\cgrid \sbasedon0 \snext0 ");
	    Set_Style (Category_Header_Info,
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 16,
		       Style_Indent => 0,
		       Style_Before => 120,
		       Style_After => 120,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s4\keepn\adjustright",
		       Style_String_Suffix => "\cgrid\qc\i \snext0 ");
	    Set_Style (Normal_Paragraph_Number_Info,
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 14,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s5\keepn\widctlpar\adjustright " &
			 "\pvpara\phpg\posxo\posy0\absw580\dxfrtext100\dfrmtxtx150\dfrmtxty150"&
			 "\cgrid\qc",
		       Style_String_Suffix => "\snext0 "); -- We adjust the space before for each number.
		-- Frame commands:
		-- \pvpara - positions the frame vertically with the next paragraph;
		-- \phpg - positions the frame horizonatally within the page;
		-- \posxo - positions the paragraph outside of the frame;
		-- \posy0 - positions the paragraph at the top of the frame;
		-- \absw - frame width in twips (640);
		-- \dxfrtext - distance of frame from text in all directions (twips);
		-- \dfrmtxtx - horizontal distance of text from frame (twips);
		-- \dfrmtxty - vertical distance of text from frame (twips).

	    Set_Style (Wide_Paragraph_Number_Info,
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 14,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s5\keepn\widctlpar\adjustright " &
			 "\pvpara\phpg\posxo\posy0\absw640\dxfrtext100\dfrmtxtx150\dfrmtxty150"&
			 "\cgrid\qc",
		       Style_String_Suffix => "\snext0 "); -- We adjust the space before for each number.


	    Set_Style (Paragraph_Info(ARM_Output.Small, 1), -- Notes
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*1,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s6\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-200\slmult0 \snext6 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small, 2), -- Annotations
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*2,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s7\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-200\slmult0 \snext7 ");
	    Set_Style (Paragraph_Info(ARM_Output.Examples, 1),
		       Font => ARM_Output.Fixed,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*1,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s8\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-190\slmult0 \snext8 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Examples, 3),
		       Font => ARM_Output.Fixed,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 16,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s9\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-170\slmult0 \snext9 ");
	    Set_Style (Paragraph_Info(ARM_Output.Normal, 1), -- Syntax indented
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => INDENT_UNIT*1,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s10\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-240 \snext10 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Normal, 3), -- Indented
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Before => 0,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s11\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-260\slmult0 \snext11 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small, 5), -- Indented annotations
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*5,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s12\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-200\slmult0 \snext12 ");
	    Set_Style (Paragraph_Info(ARM_Output.Wide_Hanging, 3),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Hang_Width => 1080,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s13\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-240\slmult0 \snext13 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Narrow_Hanging, 3),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Hang_Width => 360,
		       Style_Before => 0,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s14\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-240\slmult0 \snext14 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Wide_Hanging, 5),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*5,
		       Style_Hang_Width => 1080,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s15\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-190\slmult0 \snext15 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Narrow_Hanging, 5),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*5,
		       Style_Hang_Width => 360,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s16\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-190\slmult0 \snext16 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Bulleted, 1),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => INDENT_UNIT*1,
		       Style_Hang_Width => 250,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s17\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-240\slmult0\tx360 \snext17 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Nested_Bulleted, 2),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => INDENT_UNIT*2,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s18\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-240\slmult0\tx720 \snext18 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Bulleted, 3),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s19\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-190\slmult0\tx1080 \snext19 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Nested_Bulleted, 4),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Hang_Width => 200,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s20\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-190\slmult0\tx1440 \snext20 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Bulleted, 4), -- Indented bulleted
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Hang_Width => 250,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s21\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-240\slmult0\tx1080 \snext21 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Bulleted, 2), -- Bullets in syntax indenting
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => INDENT_UNIT*2,
		       Style_Hang_Width => 250,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s22\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-240\slmult0\tx720 \snext22 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Bulleted, 3), -- Bullets in syntax indenting
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Hang_Width => 250,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s23\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-240\slmult0\tx1080 \snext23 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Normal, 2), -- Code indenting
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => INDENT_UNIT*2,
		       Style_Before => 0,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s24\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-260\slmult0 \snext24 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small, 4), -- Annotations in code indenting
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s25\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-200\slmult0 \snext25 ");
	    Set_Style (Paragraph_Info(ARM_Output.Examples, 4), -- Indented examples
		       Font => ARM_Output.Fixed,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s26\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-190\slmult0 \snext26 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Examples, 6), -- Indented examples in annotations.
		       Font => ARM_Output.Fixed,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 16,
		       Style_Indent => INDENT_UNIT*6,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s27\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-170\slmult0 \snext27 ");

            Set_Style (Header_Info,
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 20,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
		         "\s28\widctlpar\tqc\tx" & Half_Paper_Width &
			 "\tqr\tx" & Paper_Width & "\adjustright",
		       Style_String_Suffix => "\cgrid \sbasedon0 \snext28 ");
            Set_Style (Footer_Info,
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 20,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
		         "\s29\widctlpar" & -- "\tqc\tx" & Half_Paper_Width & -- We don't use or define the center tab; it causes problems with very long titles.
			 "\tqr\tx" & Paper_Width & "\adjustright",
		       Style_String_Suffix => "\cgrid \sbasedon0 \snext29 ");
	    Set_Style (Paragraph_Info(ARM_Output.Index, 0),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => 270,
		       Style_Hang_Width => 270,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s31\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-200\slmult0 \snext31 ");
	    Set_Style (Paragraph_Info(ARM_Output.Wide_Above, 0),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => 0,
		       Style_Before => 120,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s32\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-260\slmult0 \snext0 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Wide_Above, 2), -- Wide above annotations.
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*2,
		       Style_Before => 90,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s33\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-200\slmult0 \snext7 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Header, 1), -- Notes header
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*1,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s34\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-200\slmult0 \snext6 ");
		      -- Note: No space afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Bulleted, 2), -- Bullets in notes
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*2,
		       Style_Hang_Width => 250,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s35\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-190\slmult0\tx720 \snext35 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Nested_Bulleted, 3), -- Nested bullets in notes
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s36\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-190\slmult0\tx1080 \snext36 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Hanging_in_Bulleted, 3),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Hang_Width => 720,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s37\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-240\slmult0 \snext14 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Hanging_in_Bulleted, 5),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*5,
		       Style_Hang_Width => 720,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s38\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-190\slmult0 \snext16 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Nested_Bulleted, 4), -- Nested bullets in code indenting
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s39\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-240\slmult0\tx1440 \snext39 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Syntax_Summary, 1),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*1,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s40\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-200\slmult0 \snext40 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Enumerated, 1),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => INDENT_UNIT*1,
		       Style_Hang_Width => 250,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s41\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-240\slmult0\tx360 \snext41 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Enumerated, 3),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s42\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-190\slmult0\tx1080 \snext42 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Enumerated, 2), -- Nested enumerations.
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => INDENT_UNIT*2,
		       Style_Hang_Width => 270,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s43\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-240\slmult0\tx360 \snext43 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Enumerated, 4), -- Small nested enumerations.
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Hang_Width => 240,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s44\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-190\slmult0\tx1080 \snext44 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Nested_Bulleted, 3), -- Doubly nested bullets
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s45\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-240\slmult0\tx720 \snext45 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Nested_Bulleted, 5), -- Doubly nested bullets
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*5,
		       Style_Hang_Width => 200,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s46\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-190\slmult0\tx1440 \snext46 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Nested_Bulleted, 5), -- Indented nested bullets
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => INDENT_UNIT*5,
		       Style_Hang_Width => 250,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s47\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-240\slmult0\tx1080 \snext47 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Normal, 4), -- Inner indented
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Before => 0,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s48\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-260\slmult0 \snext48 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small, 6), -- Inner indented
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*6,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s49\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-200\slmult0 \snext49 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small, 3), -- Annotations in syntax indenting
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s50\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-200\slmult0 \snext50 ");
	    Set_Style (Paragraph_Info(ARM_Output.Swiss_Examples, 1),
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 20,
		       Style_Indent => INDENT_UNIT*1,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s51\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-240\slmult0 \snext51 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Swiss_Examples, 3),
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 16,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s52\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-180\slmult0 \snext52 ");
	    Set_Style (Paragraph_Info(ARM_Output.Swiss_Examples, 4), -- Indented examples
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 20,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s53\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-240\slmult0 \snext53 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Swiss_Examples, 6), -- Indented examples
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 16,
		       Style_Indent => INDENT_UNIT*6,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s54\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-180\slmult0 \snext54 ");
	    Set_Style (Paragraph_Info(ARM_Output.Enumerated, 3), -- Doubly nested enumerations.
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Hang_Width => 270,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s55\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-240\slmult0\tx360 \snext55 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Enumerated, 5), -- Small doubly nested enumerations.
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*5,
		       Style_Hang_Width => 240,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s56\widctlpar\adjustright",
		       Style_String_Suffix => "\ri360\cgrid\qj\sl-190\slmult0\tx1080 \snext56 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Examples, 2), -- Syntax Indented examples
		       Font => ARM_Output.Fixed,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*2,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s57\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-190\slmult0 \snext57 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Examples, 4), -- Syntax Indented examples in annotations.
		       Font => ARM_Output.Fixed,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 16,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s58\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-170\slmult0 \snext58 ");
	    Set_Style (Paragraph_Info(ARM_Output.Swiss_Examples, 2), -- Syntax Indented examples
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 20,
		       Style_Indent => INDENT_UNIT*2,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s59\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-240\slmult0 \snext59 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Swiss_Examples, 4), -- Syntax Indented examples
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 16,
		       Style_Indent => INDENT_UNIT*4,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s60\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-180\slmult0 \snext60 ");
	    Set_Style (Paragraph_Info(ARM_Output.Examples, 3), -- Code Indented examples
		       Font => ARM_Output.Fixed,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s61\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-190\slmult0 \snext61 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Examples, 5), -- Code Indented examples in annotations.
		       Font => ARM_Output.Fixed,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 16,
		       Style_Indent => INDENT_UNIT*5,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s62\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-170\slmult0 \snext62 ");
	    Set_Style (Paragraph_Info(ARM_Output.Swiss_Examples, 3), -- Code Indented examples
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 20,
		       Style_Indent => INDENT_UNIT*3,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s63\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-240\slmult0 \snext63 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Swiss_Examples, 5), -- Code Indented examples
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 16,
		       Style_Indent => INDENT_UNIT*5,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s64\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\ql\sl-180\slmult0 \snext64 ");
	    Set_Style (Paragraph_Info(ARM_Output.Title, 0),
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 42, -- Slight less than double (3 pts larger than Heading_1).
		       Style_Indent => 0,
		       Style_Before => 120,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String_Prefix =>
			 "\s65\widctlpar\adjustright",
		       Style_String_Suffix => "\cgrid\qj\sl-440\slmult0 \snext0 ");

	    -- New styles should be added here, following numbers will need adjustments.
	    Set_Style (Heading_4_Info,
		       Font => ARM_Output.Swiss,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 27,
		       Style_Indent => 0,
		       Style_Before => 210,
		       Style_After => 100,
		       Style_Justified => FALSE,
		       Style_String_Prefix =>
			 "\s66\keepn\widctlpar\outlinelevel3\adjustright",
		       Style_String_Suffix => "\b\ql\cgrid \sbasedon0 \snext0 ");
	    if Output_Object.Big_Files then
		-- Define the TOC styles:
                Set_Style (TOC_1_Info,
		           Font => ARM_Output.Swiss,
		           Body_Font => Output_Object.Body_Font,
		           Font_Size => 24,
		           Style_Indent => 0,
		           Style_Before => 60,
			   Style_After => 60,
		           Style_Justified => FALSE,
		           Style_String_Prefix =>
		             "\s67\widctlpar\tqr\tldot\tx" & Paper_Width & "\adjustright",
		           Style_String_Suffix => "\b\cgrid \sbasedon0 \snext0 ");
                Set_Style (TOC_2_Info,
		           Font => ARM_Output.Swiss,
		           Body_Font => Output_Object.Body_Font,
		           Font_Size => 22,
		           Style_Indent => 200,
		           Style_Before => 0,
			   Style_After => 0,
		           Style_Justified => FALSE,
		           Style_String_Prefix =>
		             "\s68\widctlpar\tqr\tldot\tx" & Paper_Width & "\adjustright",
		           Style_String_Suffix => "\b\cgrid \sbasedon0 \snext0 ");
                Set_Style (TOC_3_Info,
		           Font => ARM_Output.Swiss,
		           Body_Font => Output_Object.Body_Font,
		           Font_Size => 22,
		           Style_Indent => 400,
		           Style_Before => 0,
			   Style_After => 0,
		           Style_Justified => FALSE,
		           Style_String_Prefix =>
		             "\s69\widctlpar\tqr\tldot\tx" & Paper_Width & "\adjustright",
		           Style_String_Suffix => "\b\cgrid \sbasedon0 \snext0 ");
                Set_Style (TOC_4_Info,
		           Font => ARM_Output.Swiss,
		           Body_Font => Output_Object.Body_Font,
		           Font_Size => 22,
		           Style_Indent => 600,
		           Style_Before => 0,
			   Style_After => 0,
		           Style_Justified => FALSE,
		           Style_String_Prefix =>
		             "\s70\widctlpar\tqr\tldot\tx" & Paper_Width & "\adjustright",
		           Style_String_Suffix => "\b\cgrid \sbasedon0 \snext0 ");
	    end if;
	    Set_Style (Table_C_Text_Info,
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => 0,
		       Style_Before => 10,
		       Style_After => 10,
		       Style_Justified => FALSE,
		       Style_String_Prefix => "",
		       Style_String_Suffix => "\qc ");
		-- We use a bit of space above and below to avoid overrunning
		-- the borders of the cells.
	    Set_Style (Table_L_Text_Info,
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 22,
		       Style_Indent => 0,
		       Style_Before => 20,
		       Style_After => 20,
		       Style_Justified => FALSE,
		       Style_String_Prefix => "",
		       Style_String_Suffix => "\ql ");
	    Set_Style (Table_C_Sml_Text_Info,
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => 0,
		       Style_Before => 20,
		       Style_After => 20,
		       Style_Justified => FALSE,
		       Style_String_Prefix => "",
		       Style_String_Suffix => "\qc ");
	    Set_Style (Table_L_Sml_Text_Info,
		       Font => ARM_Output.Default,
		       Body_Font => Output_Object.Body_Font,
		       Font_Size => 18,
		       Style_Indent => 0,
		       Style_Before => 20,
		       Style_After => 20,
		       Style_Justified => FALSE,
		       Style_String_Prefix => "",
		       Style_String_Suffix => "\ql ");
	end if;

	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Normal, 0));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Normal;}");
	    -- Normal style.
	Write_Style (Output_Object.Output_File, Heading_1_Info);
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Heading 1;}");
	Write_Style (Output_Object.Output_File, Heading_2_Info);
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Heading 2;}");
	Write_Style (Output_Object.Output_File, Heading_3_Info);
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Heading 3;}");
	Write_Style (Output_Object.Output_File, Category_Header_Info);
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Category Header;}");
	Write_Style (Output_Object.Output_File, Normal_Paragraph_Number_Info);
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Paragraph Number;}");
	Write_Style (Output_Object.Output_File, Wide_Paragraph_Number_Info);
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Large Paragraph Number;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small, 1));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Notes;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small, 2));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Annotations;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Examples, 1));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Examples;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Examples, 3));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Examples;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Normal, 1));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Syntax Indented;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Normal, 3));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Normal Indented;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small, 5));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Indented;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Wide_Hanging, 3));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Hanging;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Narrow_Hanging, 3));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Indented Hanging;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Wide_Hanging, 5));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Hanging;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Narrow_Hanging, 5));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Indented Hanging;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Bulleted, 1));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Nested_Bulleted, 2));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Nested Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Bulleted, 3));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Nested_Bulleted, 4));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Nested Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Bulleted, 4));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Indented Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Bulleted, 2));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Syntax Indented Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Bulleted, 3));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Code Indented Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Normal, 2));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Code Indented;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small, 4));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Code Indented;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Examples, 4));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Indented Examples;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Examples, 6));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Indented Examples;}");
	Write_Style (Output_Object.Output_File, Header_Info);
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "header;}");
	Write_Style (Output_Object.Output_File, Footer_Info);
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "footer;}");
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\*\cs30 \additive \sbasedon30 page number;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Index, 0));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Index;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Wide_Above, 0));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Wide;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Wide_Above, 2));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Wide Annotations;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Header, 1));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Notes Header;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Bulleted, 2));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Notes Bulleted;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Nested_Bulleted, 3));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Notes Nested Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Hanging_in_Bulleted, 3));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Hanging in Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Hanging_in_Bulleted, 5));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Hanging in Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Nested_Bulleted, 4));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Code Indented Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Syntax_Summary, 1));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Syntax Summary;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Enumerated, 1));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Enumerated;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Enumerated, 3));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Enumerated;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Enumerated, 2));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Nested Enumerated;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Enumerated, 4));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Nested Enumerated;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Nested_Bulleted, 3));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Nested X2 Bulleted;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Nested_Bulleted, 5));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Nested X2 Bulleted;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Nested_Bulleted, 5));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Indented Nested Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Normal, 4));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Inner Indented;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small, 6));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Inner Indented;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small, 3));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Syntax Indented;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Swiss_Examples, 1));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Swiss Examples;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Swiss_Examples, 3));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Swiss Examples;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Swiss_Examples, 4));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Swiss Indented Examples;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Swiss_Examples, 6));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Swiss Indented Examples;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Enumerated, 3));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Nested X2 Enumerated;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Enumerated, 5));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Nested X2 Enumerated;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Examples, 2));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Syntax Indented Examples;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Examples, 4));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Syntax Indented Examples;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Swiss_Examples, 2));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Swiss Syntax Indented Examples;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Swiss_Examples, 4));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Swiss Syntax Indented Examples;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Examples, 3));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Code Indented Examples;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Examples, 5));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Code Indented Examples;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Swiss_Examples, 3));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Swiss Code Indented Examples;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Swiss_Examples, 5));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Swiss Code Indented Examples;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Title, 0));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Standard Title;}");
        if Output_Object.Big_Files then
	    Write_Style (Output_Object.Output_File, Heading_4_Info);
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "Heading 4;}");
	    -- Define the TOC styles:
	    Write_Style (Output_Object.Output_File, TOC_1_Info);
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "toc 1;}");
	    Write_Style (Output_Object.Output_File, TOC_2_Info);
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "toc 2;}");
	    Write_Style (Output_Object.Output_File, TOC_3_Info);
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "toc 3;}");
	    Write_Style (Output_Object.Output_File, TOC_4_Info);
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "toc 4;}}");
	else
	    Write_Style (Output_Object.Output_File, Heading_4_Info);
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "Heading 4;}}");
	end if;
            -- \additive means that the style inherits from the previous style.
	    -- \basedon defines the style that the style was inherited from.
	    -- \snext defines the next style to use (if omitted, use same style).
	    -- \shidden - Don't show the style in the drop down menu.

	    -- Formatting properties:
	    -- \widctlpar - Widow/orphan control in this paragraph;
	    -- \adjustright - adjust right indent for document properties;
	    -- \fs - font size in halfpoints (.5 pt);
	    -- \f - font number;
	    -- \q - text alignment (j - justified, c - centered, l - left, r -right);
	    -- \cgrid - set the character grid(?) to the default;
	    -- \li - left indent, in twips (.1 pt);
	    -- \fi - first line indent, in twips (.1 pt);
	    -- \sa - space after paragraph, in twips (.1 pt);
	    -- \sb - space before paragraph, in twips (.1 pt);
	    -- \sl - space between lines, in twips. "Exactly" if negative, "At least" if positive.
	    -- \slmult0 - line space multiple - 0 - Exactly or "At least"/.
	    -- \pard - reset to default paragraph properties;
	    -- \plain - reset to default font/character properties;
	    -- \tx - set tab at location, in twips (.1 pt);
	    -- \tqc - set following tab as a centered tab;
	    -- \tqr - set following tab as a right tab.

	-- Revision table:
	Ada.Text_IO.Put (Output_Object.Output_File, "{\*\revtbl ");
        if Ada.Strings.Unbounded.Length (Output_Object.Version_Names('0')) = 0 then
	    Ada.Text_IO.Put (Output_Object.Output_File, "{Original Text;}");
	else
            Ada.Text_IO.Put (Output_Object.Output_File, "{" &
	       Ada.Strings.Unbounded.To_String(Output_Object.Version_Names('0')) &
		";}");
	end if;
	for Version in '1' .. '9' loop
            if Ada.Strings.Unbounded.Length (Output_Object.Version_Names(Version)) /= 0 then
                Ada.Text_IO.Put (Output_Object.Output_File, "{" &
	           Ada.Strings.Unbounded.To_String(Output_Object.Version_Names(Version)) &
		    ";}");
	    else exit; -- No more after the first empty one.
	    end if;
	end loop;
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "}");

	-- Information (truncated):
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\info{\title " &
	    Ada.Strings.Unbounded.To_String (Output_Object.Title) & "}");

	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\version2");

	Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\author AXE Consultants}"); -- Working.
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\operator Randall Brukardt, Principle Technician}}");
	--Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\author JTC1/SC22/WG9/ARG}"); -- Final.
	--Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\operator Randall Brukardt, ARG Editor}}");

	-- Initial setup (document properties):
	-- Paper size:
	-- Note: If changing the page size or margins, be sure to change the
	-- header and footer tab settings as well.
	case Output_Object.Page_Size is
	    when ARM_RTF.A4 =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\paperw11909\paperh16834"); -- Set paper to A4.
	    when ARM_RTF.Letter =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\paperw12240\paperh15840"); -- Set paper to US Letter.
	    when ARM_RTF.Half_Letter =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\paperw7920\paperh12240");  -- Set paper to 5.5x8.5.
	    when ARM_RTF.Ada95 =>
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "\paperw10080\paperh12960"); -- Set paper to 7x9.
	end case;

	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\facingp\margmirror"); -- Set to facing pages and mirrored margins.
	-- Margins.
	case Output_Object.Page_Size is
	    when ARM_RTF.Ada95 | ARM_RTF.Half_Letter =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\margl1440\margr900\margt1080\margb1080");
	    when ARM_RTF.Letter | ARM_RTF.A4 =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\margl1800\margr1080\margt1440\margb1440");
	end case;
	-- Revisions:
	if Output_Object.Includes_Changes then
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "\revisions\revprop3 \revbar0 ");
	        -- \revisions - Revisions marking is on;
	        -- \revprop3 - Show revisions as underlined.
	        -- \revbar0 - No revision bars.
		    -- Alternatively, use \revprop0\revbar3, using the infamous
		    -- vertical bar on the outside, and no other highlighting.
	-- else not changes, thus no revisions.
	end if;
	-- Other properties:
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\widowctrl\ftnbj\aenddoc\lytprtmet\formshade\viewkind1\viewscale100");
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\pgbrdrhead\pgbrdrfoot\fet0");
	    -- \widowctrl - Enable widow and orphan control;
	    -- \ftnbj - Footnotes at bottom of page;
	    -- \aenddoc - Endnotes at end of document;
	    -- \lytprtmet - Layout using printer metrics;
	    -- \formshade - Form field shading is on;
	    -- \viewkind - Default view of the document (1-Page Layout; 4-Normal);
	    -- \viewscale100 - Percentage zoom of the document (100%);
	    -- \pgbrdrhead - Page border surrounds header;
	    -- \pgbrdrfoot - Page border surrounds footer;
	    -- \fet0	- Footnote/endnote control: 0 - Footnotes only (or none);
	-- Section properties:
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\sectd\linex0\endnhere\sectdefaultcl\sbkodd\headery540\footery540");
	    -- \sectd	- Default section properties;
	    -- \linex0  - Line number spacing (0 - none);
	    -- \endnhere- Endnotes included;
	    -- \sectdefaultcl - Default character layout for section.
	    -- \sbkodd - Start at top of odd page.
	    -- \headery - Distance (in twips) of header from top of page.
	    -- \footery - Distance (in twips) of footerr from bottom of page.
	if Name = "Ttl" or else -- Title page
	   Name = "All" then -- Single giant file
	    -- No page numbers, headers, or footers here.
	    null;
	else
	    if Name = "00" or else Name = "TOC" then
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\pgnlcrm\pgncont "); -- Lower-case roman numeral, numbers continue.
	    elsif Name = "01" then
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\pgndec\pgnstart1\pgnrestart "); -- Decimal page number; starts at 1 here.
	    else
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\pgndec\pgncont "); -- Decimal page number, numbers continue.
	    end if;
	    -- Write the page headers:
	    Write_Headers (Output_Object);
	end if;
        Output_Object.Wrote_into_Section := False;
    end Start_RTF_File;


    procedure End_RTF_File (Output_Object : in out RTF_Output_Type) is
	-- Internal routine.
	-- Generate the needed text to end an RTF file. Also closes the file.
    begin
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "}");
	Ada.Text_IO.Close (Output_Object.Output_File);
    end End_RTF_File;


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
		      Title : in String := "") is
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
    begin
	if Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Already valid object");
	end if;
	Output_Object.Is_Valid := True;
	Output_Object.Page_Size := Page_Size;
	Output_Object.Includes_Changes := Includes_Changes;
	Output_Object.Big_Files := Big_Files;
	Output_Object.Primary_Sans_Serif_Font := Primary_Sans_Serif_Font;
	Output_Object.Primary_Serif_Font := Primary_Serif_Font;
	Output_Object.Body_Font := Body_Font;
	Ada.Strings.Fixed.Move (Target => Output_Object.File_Prefix,
			        Source => File_Prefix);
	Output_Object.Title := Ada.Strings.Unbounded.To_Unbounded_String (Title);
	Output_Object.Header_Prefix :=
		Ada.Strings.Unbounded.To_Unbounded_String (Header_Prefix);
	Output_Object.Footer_Text :=
		Ada.Strings.Unbounded.To_Unbounded_String (Footer_Text);
	Output_Object.Footer_Use_Date := Footer_Use_Date;
	Output_Object.Footer_Use_Clause_Name := Footer_Use_Clause_Name;
	Output_Object.Footer_Use_ISO_Format := Footer_Use_ISO_Format;
	Output_Object.Version_Names := Version_Names;
	if Big_Files then
	    -- We're going to generate a single giant file. Open it now.
	    Start_RTF_File (Output_Object,
			    Ada.Strings.Fixed.Trim (Output_Object.File_Prefix, Ada.Strings.Right),
			    Ada.Strings.Unbounded.To_String (Output_Object.Title),
			    "All");
	    Ada.Text_IO.New_Line (Output_Object.Output_File);
	end if;
    end Create;


    procedure Close (Output_Object : in out RTF_Output_Type) is
	-- Close an Output_Object. No further output to the object is
	-- allowed after this call.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Ada.Text_IO.Is_Open (Output_Object.Output_File) then
	    End_RTF_File (Output_Object);
	end if;
	Output_Object.Is_Valid := False;
    end Close;


    procedure Section (Output_Object : in out RTF_Output_Type;
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
	if not Output_Object.Big_Files then
	    if Ada.Text_IO.Is_Open (Output_Object.Output_File) then
	        End_RTF_File (Output_Object);
	    end if;

	    -- Create a new file for this section:
	    Start_RTF_File (Output_Object,
			    Ada.Strings.Fixed.Trim (Output_Object.File_Prefix, Ada.Strings.Right) &
				"-" & Section_Name,
			    Section_Title,
			    Section_Name);
	    Ada.Text_IO.New_Line (Output_Object.Output_File);
	else
	    if Output_Object.Wrote_into_Section then
	        -- Just a new section header (odd page break) and page number setup:
	        if Section_Name = "TOC" then
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, "\sect\sbkodd\pgnlcrm\pgnstart1\pgnrestart ");
		        -- Lower-case roman number page number; reset to 1.
		elsif Section_Name = "00" then
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, "\sect\sbkodd\pgnlcrm\pgncont ");
			-- Lower-case roman numeral, numbers continue.
	        elsif Section_Name = "01" then
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, "\sect\sbkodd\pgndec\pgnstart1\pgnrestart ");
			-- Decimal page number; starts at 1 here.
	        else
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, "\sect\sbkodd\pgndec\pgncont ");
			-- Decimal page number, numbers continue.
	        end if;
	        -- Write the page headers:
	        Write_Headers (Output_Object);
	    -- else Probably the title page: no headers or footers.
	    end if;
	    Output_Object.Wrote_into_Section := False;
	end if;
    end Section;


    procedure Set_Columns (Output_Object : in out RTF_Output_Type;
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
	if Number_of_Columns = Output_Object.Column_Count then
	    return;
	end if;
	if Output_Object.Wrote_into_Section then
	    Ada.Text_IO.Put (Output_Object.Output_File, "\sect\sbknone");
	end if;
	case Number_of_Columns is
	    when 1 => Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cols1 ");
	    when 2 => Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cols2\colsx0 ");
			-- Two columns, no space between. (Paragraph formatting
			-- will take care of that.)
	    when 3 => Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cols3\colsx0 ");
	    when 4 => Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cols4\colsx0 ");
	    when 5 => Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cols5\colsx0 ");
	    when 6 => Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cols6\colsx0 ");
	    when 7 => Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cols7\colsx0 ");
	    when 8 => Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cols8\colsx0 ");
	end case;
	Output_Object.Column_Count := Number_of_Columns;
    end Set_Columns;


    procedure Set_Tabs (Output_Object : in out RTF_Output_Type;
			Style         : in ARM_Output.Paragraph_Style_Type;
			Indent        : in ARM_Output.Paragraph_Indent_Type) is
	-- Set tabs in the current (just started) paragraph.
    begin
	case Style is
	    when ARM_Output.Normal | ARM_Output.Wide_Above |
		 ARM_Output.Small | ARM_Output.Small_Wide_Above |
		 ARM_Output.Header | ARM_Output.Small_Header |
		 ARM_Output.Index | ARM_Output.Syntax_Summary |
		 ARM_Output.Title |
		 ARM_Output.Examples | ARM_Output.Small_Examples |
		 ARM_Output.Swiss_Examples | ARM_Output.Small_Swiss_Examples =>
		if Output_Object.Tab_Stops.Number /= 0 then
		    if (Output_Object.Tab_Stops.Number * 8) + Output_Object.Char_Count >
			LINE_LENGTH then
	    	        Ada.Text_IO.New_Line (Output_Object.Output_File);
		        Output_Object.Char_Count := 0;
		    end if;
		    declare
			function Stop_in_Twips (Stop : ARM_Output.Tab_Stop_Type) return Natural is
			    -- Return the value of a tab stop in Twips:
			begin
			    if ARM_Output."="(Stop.Kind, ARM_Output.Left_Fixed) then
			        return Stop.Stop*120 +
			            Paragraph_Info(Style, Indent).Indent;
			            -- *120 is to convert picas to Twips.
			    else
				-- Scale with font size. (Stop assumes 12 pt
				-- type).
				-- Raw formula:
				-- (Stop.Stop * 120) -- Stop in twips.
				-- * (Paragraph_Info(Format).Size / 24) -- Font scale.
				-- After rearranging, we get:
				return
			            Stop.Stop * Paragraph_Info(Style, Indent).Size * 5 +
			            Paragraph_Info(Style, Indent).Indent;
			    end if;
			end Stop_in_Twips;

		    begin
		        for I in 1 .. Output_Object.Tab_Stops.Number loop
			    -- Define tab stops.
		            declare
			        Num : String := Integer'Image (Stop_in_Twips (
					    Output_Object.Tab_Stops.Stops(I)));
		            begin
	    	                Ada.Text_IO.Put (Output_Object.Output_File, "\tx");
	    	                Ada.Text_IO.Put (Output_Object.Output_File, Num(2..Num'Length));
	    	                Ada.Text_IO.Put (Output_Object.Output_File, " ");
		                Output_Object.Char_Count := Output_Object.Char_Count + 4 + Num'Length-1;
		            end;
		        end loop;
		    end;
		-- else no tabs defined.
		end if;

	    when ARM_Output.Bulleted | ARM_Output.Nested_Bulleted |
		 ARM_Output.Small_Bulleted | ARM_Output.Small_Nested_Bulleted |
		 ARM_Output.Wide_Hanging | ARM_Output.Narrow_Hanging |
		 ARM_Output.Hanging_in_Bulleted |
		 ARM_Output.Small_Wide_Hanging | ARM_Output.Small_Narrow_Hanging |
		 ARM_Output.Small_Hanging_in_Bulleted |
		 ARM_Output.Enumerated | ARM_Output.Small_Enumerated =>
		if Output_Object.Tab_Stops.Number /= 0 then
	            Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		        "Tabs in hanging/bulleted paragraph");
		end if;
	end case;
    end Set_Tabs;


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
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Already in paragraph");
	end if;
	if not Paragraph_Info(Style, Indent).Defined then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Undefined Style " & ARM_Output.Paragraph_Style_Type'Image(Style) &
		" and Indent:" & ARM_Output.Paragraph_Indent_Type'Image(Indent));
	end if;
	Output_Object.Is_In_Paragraph := True;
	Output_Object.Had_Prefix := not No_Prefix;
	Output_Object.Char_Count := 0;
	Output_Object.Saw_Hang_End := False;
	Output_Object.Wrote_into_Section := True;

	-- First, write the paragraph number, if any. This has its own style.
	if Number /= "" then -- We have a paragraph number.
	    -- Most paragraph numbers are 7 or fewer characters. The box is
	    -- sized for 7 characters. If we have 8 characters (as in 277.10/2),
	    -- we need a wider box.
	    if Number'Length > 7 then
		Write_Style_for_Paragraph (Output_Object.Output_File,
	            Wide_Paragraph_Number_Info, Output_Object.Char_Count);
	    else
		Write_Style_for_Paragraph (Output_Object.Output_File,
	            Normal_Paragraph_Number_Info, Output_Object.Char_Count);
	    end if;
	    -- Figure the space above: (We use a variable space above so the
	    -- numbers align with the bottom of the text, not the top).
	    declare
		Diff : Natural := (Paragraph_Info(Style, Indent).Size -
				   Normal_Paragraph_Number_Info.Size) +
				  (Paragraph_Info(Style, Indent).Before/10);
		-- This would seem to be double the required adjustment for the
		-- size, but it works. So why question it?
	    begin
		if Diff >= 30 then
		   Ada.Text_IO.Put (Output_Object.Output_File, "\sb3" &
			Character'Val(Diff mod 10 + Character'Pos('0')) & "0 ");
		elsif Diff >= 20 then
		   Ada.Text_IO.Put (Output_Object.Output_File, "\sb2" &
			Character'Val(Diff mod 10 + Character'Pos('0')) & "0 ");
		elsif Diff >= 10 then
		   Ada.Text_IO.Put (Output_Object.Output_File, "\sb1" &
			Character'Val(Diff mod 10 + Character'Pos('0')) & "0 ");
		else
		   Ada.Text_IO.Put (Output_Object.Output_File, "\sb" &
			Character'Val(Diff + Character'Pos('0')) & "0 ");
		end if;
	    end;
--*** Box width??

	    Ada.Text_IO.Put (Output_Object.Output_File, Number);
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "\par}");
	    Output_Object.Char_Count := 0;
	-- else no paragraph number.
	end if;
	-- Now, write the paragraph header:
	case Style is
	    when ARM_Output.Normal | ARM_Output.Wide_Above |
		 ARM_Output.Small | ARM_Output.Small_Wide_Above |
		 ARM_Output.Header | ARM_Output.Small_Header |
		 ARM_Output.Index | ARM_Output.Syntax_Summary |
		 ARM_Output.Title |
		 ARM_Output.Examples | ARM_Output.Small_Examples |
		 ARM_Output.Swiss_Examples | ARM_Output.Small_Swiss_Examples =>
		Write_Style_for_Paragraph (Output_Object.Output_File,
		    Paragraph_Info(Style, Indent),
		    Output_Object.Char_Count);
--Ada.Text_IO.Put_Line ("Start paragraph - full style");
	    when ARM_Output.Bulleted | ARM_Output.Nested_Bulleted |
		 ARM_Output.Small_Bulleted | ARM_Output.Small_Nested_Bulleted =>
		Write_Style_for_Paragraph (Output_Object.Output_File,
		    Paragraph_Info(Style, Indent),
		    Output_Object.Char_Count);
--Ada.Text_IO.Put_Line ("Start paragraph - full style (bullet)");
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "\tab ");
		    Output_Object.Char_Count := Output_Object.Char_Count + 5;
		else
		    if ARM_Output."=" (Style, ARM_Output.Nested_Bulleted) or else
		       ARM_Output."=" (Style, ARM_Output.Small_Nested_Bulleted) then
			-- Make a smaller bullet.
		        if Paragraph_Info(Style, Indent).Size = 15 then
	    	            Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\fs12\'b7}\tab ");
		        elsif Paragraph_Info(Style, Indent).Size = 16 then
	    	            Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\fs12\'b7}\tab ");
		        elsif Paragraph_Info(Style, Indent).Size = 18 then
	    	            Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\fs14\'b7}\tab ");
		        elsif Paragraph_Info(Style, Indent).Size = 20 then
	    	            Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\fs16\'b7}\tab ");
		        else --if Paragraph_Info(Style, Indent).Size = 22 then
	    	            Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\fs18\'b7}\tab ");
			end if;
		        Output_Object.Char_Count := Output_Object.Char_Count + 19;
		    else -- Normal bullet.
	    	        Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\'b7}\tab ");
		        Output_Object.Char_Count := Output_Object.Char_Count + 14;
		    end if;
		end if;

	    when ARM_Output.Wide_Hanging | ARM_Output.Narrow_Hanging |
		 ARM_Output.Hanging_in_Bulleted |
		 ARM_Output.Small_Wide_Hanging | ARM_Output.Small_Narrow_Hanging |
		 ARM_Output.Small_Hanging_in_Bulleted |
		 ARM_Output.Enumerated | ARM_Output.Small_Enumerated =>
		Write_Style_for_Paragraph (Output_Object.Output_File,
		    Paragraph_Info(Style, Indent),
		    Output_Object.Char_Count);
--Ada.Text_IO.Put_Line ("Start paragraph - full style (hang)");
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "\tab ");
		    Output_Object.Char_Count := Output_Object.Char_Count + 5;
		    Output_Object.Saw_Hang_End := True;
		else -- Has prefix.
		    Output_Object.Saw_Hang_End := False;
		    Output_Object.Prefix_Large_Char_Count := 0;
		end if;
	end case;

	Output_Object.Paragraph_Style := Style;
	Output_Object.Paragraph_Indent := Indent;
	Output_Object.Font := ARM_Output.Default;
	Output_Object.Is_Bold := False;
	Output_Object.Is_Italic := False;
	Output_Object.Size := 0;
	Output_Object.Color := ARM_Output.Default;
	Output_Object.Real_Size := Paragraph_Info(Style,Indent).Size;
	Output_Object.Tab_Stops := Tab_Stops;
	Output_Object.Current_Space_After := Space_After;

	Set_Tabs (Output_Object, Style, Indent);

	if No_Breaks or Keep_with_Next then
	    if Output_Object.Char_Count + 13 >
		LINE_LENGTH then
    	        Ada.Text_IO.New_Line (Output_Object.Output_File);
	        Output_Object.Char_Count := 0;
	    end if;
	    if No_Breaks then
    	        Ada.Text_IO.Put (Output_Object.Output_File, "\keep ");
	        Output_Object.Char_Count := Output_Object.Char_Count + 6;
	    end if;
	    if Keep_with_Next then
    	        Ada.Text_IO.Put (Output_Object.Output_File, "\keepn ");
	        Output_Object.Char_Count := Output_Object.Char_Count + 7;
	    end if;
	end if;
	if ARM_Output."=" (Space_After, ARM_Output.Narrow) then
	    -- Reduce the following space by 30%:
	    declare
		SA : constant String := Natural'Image((Paragraph_Info(Style, Indent).After*(LEADING_PERCENT/10))/10);
	    begin
	        if Output_Object.Char_Count + 4 + SA'Length - 1 >
		    LINE_LENGTH then
    	            Ada.Text_IO.New_Line (Output_Object.Output_File);
	            Output_Object.Char_Count := 0;
	        end if;
                Ada.Text_IO.Put (Output_Object.Output_File, "\sa");
                Ada.Text_IO.Put (Output_Object.Output_File, SA(2..SA'Last));
                Ada.Text_IO.Put (Output_Object.Output_File, " ");
                Output_Object.Char_Count := 4 + SA'Length - 1;
	    end;
	elsif ARM_Output."=" (Space_After, ARM_Output.Wide) then
	    -- Increase the following space by 50%:
	    declare
		SA : constant String := Natural'Image((Paragraph_Info(Style, Indent).After*(TRAILING_PERCENT/10))/10);
	    begin
	        if Output_Object.Char_Count + 4 + SA'Length - 1 >
		    LINE_LENGTH then
    	            Ada.Text_IO.New_Line (Output_Object.Output_File);
	            Output_Object.Char_Count := 0;
	        end if;
                Ada.Text_IO.Put (Output_Object.Output_File, "\sa");
                Ada.Text_IO.Put (Output_Object.Output_File, SA(2..SA'Last));
                Ada.Text_IO.Put (Output_Object.Output_File, " ");
                Output_Object.Char_Count := 4 + SA'Length - 1;
	    end;
	end if;
	if ARM_Output."/=" (Justification, ARM_Output.Default) then
	    if Output_Object.Char_Count + 4 >
		LINE_LENGTH then
    	        Ada.Text_IO.New_Line (Output_Object.Output_File);
	        Output_Object.Char_Count := 0;
	    end if;
	    case Justification is
		when ARM_Output.Default => null; -- Can't get here.
		when ARM_Output.Left =>
    	            Ada.Text_IO.Put (Output_Object.Output_File, "\ql ");
	            Output_Object.Char_Count := Output_Object.Char_Count + 4;
		when ARM_Output.Center =>
    	            Ada.Text_IO.Put (Output_Object.Output_File, "\qc ");
	            Output_Object.Char_Count := Output_Object.Char_Count + 4;
		when ARM_Output.Right =>
    	            Ada.Text_IO.Put (Output_Object.Output_File, "\qr ");
	            Output_Object.Char_Count := Output_Object.Char_Count + 4;
		when ARM_Output.Justified =>
    	            Ada.Text_IO.Put (Output_Object.Output_File, "\qj ");
	            Output_Object.Char_Count := Output_Object.Char_Count + 4;
	    end case;
	end if;
	-- Start hang (last), so we get a clean count of prefix characters:
	case Style is
	    when ARM_Output.Wide_Hanging | ARM_Output.Narrow_Hanging |
		 ARM_Output.Small_Wide_Hanging | ARM_Output.Small_Narrow_Hanging |
		 ARM_Output.Hanging_in_Bulleted | ARM_Output.Small_Hanging_in_Bulleted |
		 ARM_Output.Enumerated | ARM_Output.Small_Enumerated =>
		if not No_Prefix then
    	            Ada.Text_IO.New_Line (Output_Object.Output_File);
	            Output_Object.Char_Count := 0;
--Ada.Text_Io.Put ("Start Hang:");
		-- else no prefix, no need to count.
		end if;
	    when others => null;
	end case;
    end Start_Paragraph;


    procedure End_Paragraph (Output_Object : in out RTF_Output_Type) is
	-- End a paragraph.
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
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\par}");
	Output_Object.Char_Count := 0;
--Ada.Text_IO.Put_Line ("End paragraph '}'");
    end End_Paragraph;


    procedure Category_Header (Output_Object : in out RTF_Output_Type;
			       Header_Text : String) is
	-- Output a Category header (that is, "Legality Rules",
	-- "Dynamic Semantics", etc.)
	-- (Note: We did not use a enumeration here to insure that these
	-- headers are spelled the same in all output versions).
	-- Raises Not_Valid_Error if in a paragraph.
	Count : Natural; -- Not used after being set.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Header in paragraph");
	end if;
	Ada.Text_IO.New_Line (Output_Object.Output_File);
	Write_Style_for_Paragraph (Output_Object.Output_File,
	    Category_Header_Info, Count);
	Ada.Text_IO.Put_Line (Output_Object.Output_File,
	    Header_Text & "\par}");
	Output_Object.Char_Count := 0;
	Output_Object.Wrote_into_Section := True;
    end Category_Header;


    function Current_Date return String is
        -- Local routine:
	Date : Ada.Calendar.Time := Ada.Calendar.Clock;
        Day : constant String := Ada.Calendar.Day_Number'Image(Ada.Calendar.Day(Date));
        Year : constant String := Ada.Calendar.Year_Number'Image(Ada.Calendar.Year(Date));
        Month : constant Ada.Calendar.Month_Number := Ada.Calendar.Month(Date);
    begin
        case Month is
	    when  1 => return Day(2..Day'Last) & " January" & Year;
	    when  2 => return Day(2..Day'Last) & " February" & Year;
	    when  3 => return Day(2..Day'Last) & " March" & Year;
	    when  4 => return Day(2..Day'Last) & " April" & Year;
	    when  5 => return Day(2..Day'Last) & " May" & Year;
	    when  6 => return Day(2..Day'Last) & " June" & Year;
	    when  7 => return Day(2..Day'Last) & " July" & Year;
	    when  8 => return Day(2..Day'Last) & " August" & Year;
	    when  9 => return Day(2..Day'Last) & " September" & Year;
	    when 10 => return Day(2..Day'Last) & " October" & Year;
	    when 11 => return Day(2..Day'Last) & " November" & Year;
	    when 12 => return Day(2..Day'Last) & " December" & Year;
        end case;
    end Current_Date;


    procedure Clause_Footer (Output_Object : in out RTF_Output_Type;
			     Header_Text : in String;
			     Level : in ARM_Contents.Level_Type;
			     Clause_Number : in String;
			     No_Page_Break : in Boolean := False) is
        -- Local routine: Set up the footer for the header.
	Count : Natural; -- Not used after being set.
    begin
	-- Adjust footer.
	if Output_Object.Wrote_into_Section then
	    -- Start a new section:
	    if No_Page_Break or else
	       ARM_Contents."="(Level, ARM_Contents.Clause) or else
	       ARM_Contents."="(Level, ARM_Contents.Subclause) or else
	       ARM_Contents."="(Level, ARM_Contents.Subsubclause) then
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\sect\sbknone\pgncont ");
	    else
		-- Start sections on a new page. (Should only happen in the
		-- introduction).
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\sect\sbkpage\pgncont ");
	    end if;
	-- else just use the existing section.
	end if;
	if Clause_Number = "" or else
	   ARM_Contents."="(Level, ARM_Contents.Unnumbered_Section) then
	    Ada.Text_IO.Put (Output_Object.Output_File, "{\footerl ");
	    Write_Style_for_Paragraph (Output_Object.Output_File, Footer_Info, Count);
	    if Output_Object.Footer_Use_ISO_Format then
		Ada.Text_IO.Put (Output_Object.Output_File, "{\f1\fs16 ");
	    elsif ARM_Output."="(Output_Object.Body_Font, ARM_Output.Swiss) then
		Ada.Text_IO.Put (Output_Object.Output_File, "{\f1 ");
	    else
		Ada.Text_IO.Put (Output_Object.Output_File, "{\f0 ");
	    end if;
	    if Output_Object.Footer_Use_Clause_Name then
                Ada.Text_IO.Put (Output_Object.Output_File, Header_Text);
	    else
                Ada.Text_IO.Put (Output_Object.Output_File,
		    Ada.Strings.Unbounded.To_String (Output_Object.Footer_Text));
	    end if;
            Ada.Text_IO.Put (Output_Object.Output_File, "\tab ");
	    if Output_Object.Footer_Use_Date then
	        Ada.Text_IO.Put (Output_Object.Output_File, Current_Date);
	    -- else no date.
	    end if;
	    if Output_Object.Footer_Use_ISO_Format then
                Ada.Text_IO.Put_Line (Output_Object.Output_File, "\~\~\~\~\~\~{\f1\fs22\b {\field{\*\fldinst { PAGE }}{\fldrslt {\lang1024 x}}}}\par}}}");
	    else
                Ada.Text_IO.Put_Line (Output_Object.Output_File, "\~\~\~\~\~\~{\field{\*\fldinst { PAGE }}{\fldrslt {\lang1024 x}}}\par}}}");
	    end if;
	    Ada.Text_IO.Put (Output_Object.Output_File, "{\footerr ");
	    Write_Style_for_Paragraph (Output_Object.Output_File, Footer_Info, Count);
	    if Output_Object.Footer_Use_ISO_Format then
		Ada.Text_IO.Put (Output_Object.Output_File, "{\f1\fs22\b {\field{\*\fldinst { PAGE }}{\fldrslt {\lang1024 x}}}}\~\~\~\~\~\~");
	    elsif ARM_Output."="(Output_Object.Body_Font, ARM_Output.Swiss) then
		Ada.Text_IO.Put (Output_Object.Output_File, "{\f1 {\field{\*\fldinst { PAGE }}{\fldrslt {\lang1024 x}}}}\~\~\~\~\~\~");
	    else
		Ada.Text_IO.Put (Output_Object.Output_File, "{\f0 {\field{\*\fldinst { PAGE }}{\fldrslt {\lang1024 x}}}}\~\~\~\~\~\~");
	    end if;
	    if Output_Object.Footer_Use_Date then
	        Ada.Text_IO.Put (Output_Object.Output_File, Current_Date);
	    -- else no date.
	    end if;
	    Ada.Text_IO.Put (Output_Object.Output_File, "\tab ");
	    if Output_Object.Footer_Use_ISO_Format then
		Ada.Text_IO.Put (Output_Object.Output_File, "{\f1\fs16 ");
	    end if;
	    if Output_Object.Footer_Use_Clause_Name then
                Ada.Text_IO.Put (Output_Object.Output_File, Header_Text);
	    else
                Ada.Text_IO.Put (Output_Object.Output_File,
		    Ada.Strings.Unbounded.To_String (Output_Object.Footer_Text));
	    end if;
	    if Output_Object.Footer_Use_ISO_Format then
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\par}}}");
	    else
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\par}}");
	    end if;
	else
	    Ada.Text_IO.Put (Output_Object.Output_File, "{\footerl ");
	    Write_Style_for_Paragraph (Output_Object.Output_File, Footer_Info, Count);
	    if Output_Object.Footer_Use_Clause_Name then
                Ada.Text_IO.Put (Output_Object.Output_File, "{\b\f1 ");
	        if Level in ARM_Contents.Plain_Annex .. ARM_Contents.Normative_Annex then
		    -- Clause Number includes "Annex". Just use the letter.
		    Ada.Text_IO.Put (Output_Object.Output_File, Clause_Number(Clause_Number'Last));
	        else
		    Ada.Text_IO.Put (Output_Object.Output_File, Clause_Number);
	        end if;
	        if Output_Object.Footer_Use_ISO_Format then
		    Ada.Text_IO.Put (Output_Object.Output_File, "}\~\~\~{\f1\fs16 ");
	        elsif ARM_Output."="(Output_Object.Body_Font, ARM_Output.Swiss) then
		    Ada.Text_IO.Put (Output_Object.Output_File, "}\~\~\~{\f1 ");
	        else
		    Ada.Text_IO.Put (Output_Object.Output_File, "}\~\~\~{\f0 ");
	        end if;
                Ada.Text_IO.Put (Output_Object.Output_File, Header_Text);
	    else
	        if Output_Object.Footer_Use_ISO_Format then
		    Ada.Text_IO.Put (Output_Object.Output_File, "{\f1\fs16 ");
	        elsif ARM_Output."="(Output_Object.Body_Font, ARM_Output.Swiss) then
		    Ada.Text_IO.Put (Output_Object.Output_File, "{\f1 ");
	        else
		    Ada.Text_IO.Put (Output_Object.Output_File, "{\f0 ");
	        end if;
                Ada.Text_IO.Put (Output_Object.Output_File,
		    Ada.Strings.Unbounded.To_String (Output_Object.Footer_Text));
	    end if;
            Ada.Text_IO.Put (Output_Object.Output_File, "\tab ");
	    if Output_Object.Footer_Use_Date then
	        Ada.Text_IO.Put (Output_Object.Output_File, Current_Date);
	    -- else no date.
	    end if;
	    if Output_Object.Footer_Use_ISO_Format then
                Ada.Text_IO.Put_Line (Output_Object.Output_File, "\~\~\~\~\~\~{\f1\fs22\b {\field{\*\fldinst { PAGE }}{\fldrslt {\lang1024 x}}}}\par}}}");
	    else
                Ada.Text_IO.Put_Line (Output_Object.Output_File, "\~\~\~\~\~\~{\field{\*\fldinst { PAGE }}{\fldrslt {\lang1024 x}}}\par}}}");
	    end if;
	    Ada.Text_IO.Put (Output_Object.Output_File, "{\footerr ");
	    Write_Style_for_Paragraph (Output_Object.Output_File, Footer_Info, Count);
	    if Output_Object.Footer_Use_ISO_Format then
		Ada.Text_IO.Put (Output_Object.Output_File, "{\f1\fs22\b {\field{\*\fldinst { PAGE }}{\fldrslt {\lang1024 x}}}}\~\~\~\~\~\~");
	    elsif ARM_Output."="(Output_Object.Body_Font, ARM_Output.Swiss) then
		Ada.Text_IO.Put (Output_Object.Output_File, "{\f1 {\field{\*\fldinst { PAGE }}{\fldrslt {\lang1024 x}}}}\~\~\~\~\~\~");
	    else
		Ada.Text_IO.Put (Output_Object.Output_File, "{\f0 {\field{\*\fldinst { PAGE }}{\fldrslt {\lang1024 x}}}}\~\~\~\~\~\~");
	    end if;
	    if Output_Object.Footer_Use_Date then
	        Ada.Text_IO.Put (Output_Object.Output_File, Current_Date);
	    -- else no date.
	    end if;
	    Ada.Text_IO.Put (Output_Object.Output_File, "\tab ");
	    if Output_Object.Footer_Use_ISO_Format then
	        Ada.Text_IO.Put (Output_Object.Output_File, "{\f1\fs16 ");
	    elsif ARM_Output."="(Output_Object.Body_Font, ARM_Output.Swiss) then
	        Ada.Text_IO.Put (Output_Object.Output_File, "{\f1 ");
	    else
	        Ada.Text_IO.Put (Output_Object.Output_File, "{\f0 ");
	    end if;
	    if Output_Object.Footer_Use_Clause_Name then
                Ada.Text_IO.Put (Output_Object.Output_File, Header_Text);
	        Ada.Text_IO.Put (Output_Object.Output_File, "}\~\~\~\b\f1 ");
	        if Level in ARM_Contents.Plain_Annex .. ARM_Contents.Normative_Annex then
		    -- Clause Number includes "Annex". Just use the letter.
		    Ada.Text_IO.Put (Output_Object.Output_File, Clause_Number(Clause_Number'Last));
	        else
		    Ada.Text_IO.Put (Output_Object.Output_File, Clause_Number);
	        end if;
	    else
                Ada.Text_IO.Put (Output_Object.Output_File,
		    Ada.Strings.Unbounded.To_String (Output_Object.Footer_Text));
	        Ada.Text_IO.Put (Output_Object.Output_File, "}");
	    end if;
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "\par}}");
	end if;
	Output_Object.Wrote_into_Section := True;
    end Clause_Footer;


    procedure Clause_Header (Output_Object : in out RTF_Output_Type;
			     Header_Text : in String;
			     Level : in ARM_Contents.Level_Type;
			     Clause_Number : in String;
			     No_Page_Break : in Boolean := False) is
	-- Output a Clause header. The level of the header is specified
	-- in Level. The Clause Number is as specified.
	-- These should appear in the table of contents.
	-- For hyperlinked formats, this should generate a link target.
	-- If No_Page_Break is True, suppress any page breaks.
	-- Raises Not_Valid_Error if in a paragraph.
	Count : Natural; -- Not used after being set.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Header in paragraph");
	end if;
        Ada.Text_IO.New_Line (Output_Object.Output_File);

	-- Adjust footer.
	Clause_Footer (Output_Object, Header_Text, Level, Clause_Number, No_Page_Break);

	-- Special for table of contents:
	if Clause_Number = "" and then
		(Header_Text = "Table of Contents" or else -- Ada 95 format
		 Header_Text = "Contents") then -- ISO 2004 format.
	    if Header_Text = "Table of Contents" then
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\pard\plain \s1\sb240\sa60\keepn\widctlpar\outlinelevel0\adjustright \b\f1\fs36\kerning36\qc\cgrid Table of Contents\par}");
	    else
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\pard\plain \s1\sb240\sa60\keepn\widctlpar\outlinelevel0\adjustright \b\f1\fs36\kerning36\qc\cgrid Contents\par}");
	    end if;
	    Output_Object.Char_Count := 0;
	    return;
	end if;

	case Level is
	    when ARM_Contents.Plain_Annex =>
		Write_Style_for_Paragraph (Output_Object.Output_File,
		    Heading_1_Info, Count);
		Ada.Text_IO.Put_Line (Output_Object.Output_File,
		    Clause_Number & ": " & Header_Text & "\par}");
				-- Note: Clause_Number includes "Annex"
	    when ARM_Contents.Normative_Annex =>
		Write_Style_for_Paragraph (Output_Object.Output_File,
		    Heading_1_Info, Count);
		Ada.Text_IO.Put_Line (Output_Object.Output_File, Clause_Number & "\line ");
				-- Note: Clause_Number includes "Annex"
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\b0 (normative)}\line ");
		Ada.Text_IO.Put_Line (Output_Object.Output_File, Header_Text & "\par}");
	    when ARM_Contents.Informative_Annex =>
		Write_Style_for_Paragraph (Output_Object.Output_File,
		    Heading_1_Info, Count);
		Ada.Text_IO.Put_Line (Output_Object.Output_File, Clause_Number & "\line ");
				-- Note: Clause_Number includes "Annex"
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\b0 (informative)}\line ");
		Ada.Text_IO.Put_Line (Output_Object.Output_File, Header_Text & "\par}");
	    when ARM_Contents.Unnumbered_Section =>
	        if Header_Text /= "" then
		    Write_Style_for_Paragraph (Output_Object.Output_File,
		        Heading_1_Info, Count);
		    Ada.Text_IO.Put_Line (Output_Object.Output_File, Header_Text & "\par}");
	        end if;
	    when ARM_Contents.Section =>
	        Write_Style_for_Paragraph (Output_Object.Output_File,
		    Heading_1_Info, Count);
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Section " &
		     Clause_Number & ": " & Header_Text & "\par}");
	    when ARM_Contents.Clause =>
	        Write_Style_for_Paragraph (Output_Object.Output_File,
	            Heading_2_Info, Count);
	        Ada.Text_IO.Put_Line (Output_Object.Output_File,
		     Clause_Number & " " & Header_Text & "\par}");
	    when ARM_Contents.Subclause =>
	        Write_Style_for_Paragraph (Output_Object.Output_File,
	            Heading_3_Info, Count);
	        Ada.Text_IO.Put_Line (Output_Object.Output_File,
		     Clause_Number & " " & Header_Text & "\par}");
	    when ARM_Contents.Subsubclause =>
	        Write_Style_for_Paragraph (Output_Object.Output_File,
	            Heading_4_Info, Count);
	        Ada.Text_IO.Put_Line (Output_Object.Output_File,
		     Clause_Number & " " & Header_Text & "\par}");
	    when ARM_Contents.Dead_Clause =>
		raise Program_Error; -- No headers for dead clauses.
	end case;
	Output_Object.Char_Count := 0;
    end Clause_Header;


    procedure Revised_Clause_Header (Output_Object : in out RTF_Output_Type;
			     New_Header_Text : in String;
			     Old_Header_Text : in String;
			     Level : in ARM_Contents.Level_Type;
			     Clause_Number : in String;
			     Version : in ARM_Contents.Change_Version_Type;
			     Old_Version : in ARM_Contents.Change_Version_Type;
        		     No_Page_Break : in Boolean := False) is
	-- Output a revised clause header. Both the original and new text will
	-- be output. The level of the header is specified in Level. The Clause
	-- Number is as specified.
	-- These should appear in the table of contents.
	-- For hyperlinked formats, this should generate a link target.
	-- Version is the insertion version of the new text; Old_Version is
	-- the insertion version of the old text.
	-- If No_Page_Break is True, suppress any page breaks.
	-- Raises Not_Valid_Error if in a paragraph.
	Count : Natural; -- Not used after being set.
	function Header_Text return String is
	begin
	    if Old_Version = '0' then -- Old is original text
	        return "{\revised\revauth" & Version & " " & New_Header_Text & "}{\deleted\revauthdel" & Version & " " & Old_Header_Text & "}";
	    else
	        return "{\revised\revauth" & Version & " " & New_Header_Text &
			"}{\deleted\revauthdel" & Version &
                          "\revised\revauth" & Old_Version &  " " &
                          Old_Header_Text & "}";
	    end if;
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
        Ada.Text_IO.New_Line (Output_Object.Output_File);

	-- Adjust footer.
	Clause_Footer (Output_Object, New_Header_Text, Level,
		       Clause_Number, No_Page_Break);

	case Level is
	    when ARM_Contents.Plain_Annex =>
		Write_Style_for_Paragraph (Output_Object.Output_File,
		    Heading_1_Info, Count);
		Ada.Text_IO.Put_Line (Output_Object.Output_File,
		    Clause_Number & ": " & Header_Text & "\par}");
				-- Note: Clause_Number includes "Annex"
	    when ARM_Contents.Normative_Annex =>
		Write_Style_for_Paragraph (Output_Object.Output_File,
		    Heading_1_Info, Count);
		Ada.Text_IO.Put_Line (Output_Object.Output_File, Clause_Number & "\line ");
				-- Note: Clause_Number includes "Annex"
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\b0 (normative)}\line ");
		Ada.Text_IO.Put_Line (Output_Object.Output_File, Header_Text & "\par}");
	    when ARM_Contents.Informative_Annex =>
		Write_Style_for_Paragraph (Output_Object.Output_File,
		    Heading_1_Info, Count);
		Ada.Text_IO.Put_Line (Output_Object.Output_File, Clause_Number & "\line ");
				-- Note: Clause_Number includes "Annex"
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\b0 (informative)}\line ");
		Ada.Text_IO.Put_Line (Output_Object.Output_File, Header_Text & "\par}");
	    when ARM_Contents.Unnumbered_Section =>
	        if Header_Text /= "" then
		    Write_Style_for_Paragraph (Output_Object.Output_File,
		        Heading_1_Info, Count);
		    Ada.Text_IO.Put_Line (Output_Object.Output_File, Header_Text & "\par}");
	        end if;
	    when ARM_Contents.Section =>
	        Write_Style_for_Paragraph (Output_Object.Output_File,
		    Heading_1_Info, Count);
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Section " &
		     Clause_Number & ": " & Header_Text & "\par}");
	    when ARM_Contents.Clause =>
	        Write_Style_for_Paragraph (Output_Object.Output_File,
	            Heading_2_Info, Count);
	        Ada.Text_IO.Put_Line (Output_Object.Output_File,
		     Clause_Number & " " & Header_Text & "\par}");
	    when ARM_Contents.Subclause =>
	        Write_Style_for_Paragraph (Output_Object.Output_File,
	            Heading_3_Info, Count);
	        Ada.Text_IO.Put_Line (Output_Object.Output_File,
		     Clause_Number & " " & Header_Text & "\par}");
	    when ARM_Contents.Subsubclause =>
	        Write_Style_for_Paragraph (Output_Object.Output_File,
	            Heading_4_Info, Count);
	        Ada.Text_IO.Put_Line (Output_Object.Output_File,
		     Clause_Number & " " & Header_Text & "\par}");
	    when ARM_Contents.Dead_Clause =>
		raise Program_Error; -- No headers for dead clauses.
	end case;
	Output_Object.Char_Count := 0;
    end Revised_Clause_Header;


    procedure TOC_Marker (Output_Object : in out RTF_Output_Type;
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
	if Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"TOC in paragraph");
	end if;
	if Output_Object.Big_Files then
	    if For_Start then
		-- Create a Table of contents field:
		Write_Style_for_Paragraph (Output_Object.Output_File,
		    Paragraph_Info(ARM_Output.Normal, 0),
		    Output_Object.Char_Count);
	        Ada.Text_IO.Put_Line (Output_Object.Output_File,
		    "{\field\fldedit{\*\fldinst  TOC \\o ""1-3"" }{\fldrslt ");
		Output_Object.Char_Count := 0;
	    else -- End.
		-- Close the field:
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "}}\par}");
	    end if;
	else -- Small files:
	    null; -- We're not generating a table of contents.
	end if;
    end TOC_Marker;


    procedure New_Page (Output_Object : in out RTF_Output_Type;
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
	case Kind is
	    when ARM_Output.Any_Page =>
		if Output_Object.Is_In_Paragraph then
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
			"Page in paragraph");
		end if;
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\sect\sbkpage\pgncont ");
		    -- A section break and start on a new page, with page numbers continuing.
		    -- All other section properties are inherited.
		    -- We use a section break here, and not
		    -- "\page", because that gives the wrong footers if the next
		    -- item is a clause header (as it usually is).
	        Output_Object.Wrote_into_Section := False;
	    when ARM_Output.Odd_Page_Only =>
		if Output_Object.Is_In_Paragraph then
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
			"Page in paragraph");
		end if;
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\sect\sbkodd\pgncont ");
		    -- A section break and start on an odd page, with page numbers continuing.
		    -- All other section properties are inherited.
	        Output_Object.Wrote_into_Section := False;
	    when ARM_Output.Soft_Page =>
		if not Output_Object.Is_In_Paragraph then
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
			"Soft page not in paragraph");
		end if;
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "\softpage ");
	end case;
    end New_Page;


    procedure New_Column (Output_Object : in out RTF_Output_Type) is
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
		"New Column in paragraph");
	end if;
	if Output_Object.Column_Count <= 1 then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Column break, but no columns");
	end if;
	-- Set the font size to the most recently used one, because
	-- otherwise this takes a 12 pt. space, much too large in some cases:
	declare
	    FS : constant String := Natural'Image(
		Paragraph_Info(Output_Object.Paragraph_Style,
			       Output_Object.Paragraph_Indent).Size);
	begin
            Ada.Text_IO.Put (Output_Object.Output_File, "\fs");
            Ada.Text_IO.Put (Output_Object.Output_File, FS(2..FS'Last));
	end;
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "\column ");
    end New_Column;


    procedure Separator_Line (Output_Object : in out RTF_Output_Type;
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
	Ada.Text_IO.New_Line (Output_Object.Output_File);
	if Is_Thin then
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\pard \widctlpar\brdrb\brdrs\brdrw15\brsp20 \adjustright \fs4\par }");
		-- \brdrb - Bottom border; \brdrs - Single thickness;
		-- \brdrw15 - thickness of 15 twips (max 75);
		-- \brsp20 - spacing between border and text.
	else
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\pard \widctlpar\brdrb\brdrs\brdrw30\brsp20 \adjustright \fs4\par }");
	end if;
	Ada.Text_IO.New_Line (Output_Object.Output_File);
    end Separator_Line;


    function Format_Value (Value : in Integer) return String is
	Str : constant String := Natural'Image(Value);
    begin
	if Value < 0 then
	    return Str;
	else
	    return Str(2..Str'Last);
	end if;
    end Format_Value;


    type Table_Info_Kind is (Caption, Header, Header_no_Caption, First_Row, Row, Last_Row);

    procedure RTF_Table_Info (Output_Object : in out RTF_Output_Type;
			      Kind : in Table_Info_Kind) is
	-- Output the current table definition (Word needs this on every row):
	use type ARM_Output.Column_Text_Alignment;
    begin
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\trowd \trgaph108 ");

        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\trrh" &
	    Format_Value(Paragraph_Info(ARM_Output.Normal,0).Size * 16) &
	    "\trleft" & Format_Value(Output_Object.Table_Indent) & " ");

	if Output_Object.Table_Has_Border then
	    -- Set all of the borders to the normal:
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "\trbrdrt\brdrs\brdrw10 " &
	        "\trbrdrl\brdrs\brdrw10 " & "\trbrdrb\brdrs\brdrw10 " &
	        "\trbrdrr\brdrs\brdrw10 " & "\trbrdrh\brdrs\brdrw10 " &
	        "\trbrdrv\brdrs\brdrw10 ");
	-- else nothing.
	end if;

	if Output_Object.Table_No_Page_Break then
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "\trkeep\trkeepfollow ");
	elsif Kind = Header_no_Caption then
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "\trhdr\trkeep ");
	else
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "\trkeep ");
	end if;

	case Kind is
	    when Caption =>
		-- Now, define the cell borders:
		if Output_Object.Table_Has_Border then
	            Ada.Text_IO.Put_Line (Output_Object.Output_File,
		        "\clvertalc \clbrdrt\brdrs\brdrw10 " &
		        "\clbrdrl\brdrs\brdrw10 " &
		        "\clbrdrb\brdrs\brdrw10 " &
		        "\clbrdrr\brdrs\brdrw10 ");
		else
	            Ada.Text_IO.Put_Line (Output_Object.Output_File,
		        "\clvertalc ");
		end if;

	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cltxlrtb\cellx" &
		    Format_Value(Output_Object.Table_Indent + Output_Object.Table_Width) & " ");
		    -- Caption cell crosses entire line.

		-- Now, set up text (normal, centered):
		if Output_Object.Table_Has_Small_Text then
		    Write_Style_for_Paragraph (Output_Object.Output_File,
					       Table_C_Sml_Text_Info,
					       Output_Object.Char_Count);
		    Output_Object.Real_Size := Table_C_Sml_Text_Info.Size;
		    Output_Object.Size := 0;
		else
		    Write_Style_for_Paragraph (Output_Object.Output_File,
					       Table_C_Text_Info,
					       Output_Object.Char_Count);
		    Output_Object.Real_Size := Table_C_Text_Info.Size;
		    Output_Object.Size := 0;
		end if;
	        Ada.Text_IO.Put (Output_Object.Output_File, "\intbl ");
		Output_Object.Char_Count := Output_Object.Char_Count + 6;

	    when Header | Header_No_Caption =>
		-- Now, define the cell borders for each cell:
		for I in 1 .. Output_Object.Column_Count loop
		    if Output_Object.Table_Has_Border then
	                Ada.Text_IO.Put_Line (Output_Object.Output_File,
		            "\clvertalc \clbrdrt\brdrs\brdrw10 " &
		            "\clbrdrl\brdrs\brdrw10 " &
		            "\clbrdrb\brdrs\brdrw10 " &
		            "\clbrdrr\brdrs\brdrw10 ");
		    else
	                Ada.Text_IO.Put_Line (Output_Object.Output_File,
		            "\clvertalc ");
		    end if;
--Ada.Text_IO.Put_Line("Header:");
--Ada.Text_IO.Put_Line("Indent:" & Natural'Image(Output_Object.Table_Indent) &
--	" Count:" & Natural'Image(I+Output_Object.Table_First_Column_Mult-1));
		    if I /= Output_Object.Column_Count then
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cltxlrtb\cellx" &
		            Format_Value(Output_Object.Table_Indent +
			        Output_Object.Table_Column_Width*(Integer(I+Output_Object.Table_First_Column_Mult-1))) & " ");
		    else -- Last cell, full width.
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cltxlrtb\cellx" &
			    Format_Value(Output_Object.Table_Indent + Output_Object.Table_Width) & " ");
		    end if;
	        end loop;

		-- Now, define text format:
		if Output_Object.Table_Alignment = ARM_Output.Center_All then
		    if Output_Object.Table_Has_Small_Text then
		        Write_Style_for_Paragraph (Output_Object.Output_File,
					           Table_C_Sml_Text_Info,
					           Output_Object.Char_Count);
		        Output_Object.Real_Size := Table_C_Sml_Text_Info.Size;
		        Output_Object.Size := 0;
		    else
		        Write_Style_for_Paragraph (Output_Object.Output_File,
					           Table_C_Text_Info,
					           Output_Object.Char_Count);
		        Output_Object.Real_Size := Table_C_Text_Info.Size;
		        Output_Object.Size := 0;
		    end if;
		else
		    if Output_Object.Table_Has_Small_Text then
		        Write_Style_for_Paragraph (Output_Object.Output_File,
					           Table_L_Sml_Text_Info,
					           Output_Object.Char_Count);
		        Output_Object.Real_Size := Table_L_Sml_Text_Info.Size;
		        Output_Object.Size := 0;
		    else
		        Write_Style_for_Paragraph (Output_Object.Output_File,
					           Table_L_Text_Info,
					           Output_Object.Char_Count);
		        Output_Object.Real_Size := Table_L_Text_Info.Size;
		        Output_Object.Size := 0;
		    end if;
		end if;
	        Ada.Text_IO.Put (Output_Object.Output_File, "\intbl ");
		Output_Object.Char_Count := Output_Object.Char_Count + 6;

	    when First_Row | Row | Last_Row =>
		-- Now, define the cell borders for each cell:
		for I in 1 .. Output_Object.Column_Count loop
		    if Output_Object.Table_Has_Border then
			if Kind = First_Row then
	                    Ada.Text_IO.Put_Line (Output_Object.Output_File,
		                "\clvertalc \clbrdrt\brdrs\brdrw10 " &
		                "\clbrdrl\brdrs\brdrw10 " &
		                "\clbrdrb\brdrs\brdrw10 " &
		                "\clbrdrr\brdrs\brdrw10 ");
			elsif Kind = Row then
	                    --Ada.Text_IO.Put_Line (Output_Object.Output_File,
		            --    "\clvertalc \clbrdrl\brdrs\brdrw10 " &
		            --    "\clbrdrr\brdrs\brdrw10 ");
			    -- Why no bottom border??

			    -- No top border!
	                    Ada.Text_IO.Put_Line (Output_Object.Output_File,
		                "\clvertalc \clbrdrl\brdrs\brdrw10 " &
		                "\clbrdrb\brdrs\brdrw10 " &
		                "\clbrdrr\brdrs\brdrw10 ");
			else -- Kind = Last_Row then
			    -- No top border!
	                    Ada.Text_IO.Put_Line (Output_Object.Output_File,
		                "\clvertalc \clbrdrl\brdrs\brdrw10 " &
		                "\clbrdrb\brdrs\brdrw10 " &
		                "\clbrdrr\brdrs\brdrw10 ");
			end if;
		    else
	                Ada.Text_IO.Put_Line (Output_Object.Output_File,
		            "\clvertalc ");
		    end if;

		    if I /= Output_Object.Column_Count then
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cltxlrtb\cellx" &
		            Format_Value(Output_Object.Table_Indent +
			        Output_Object.Table_Column_Width*(Integer(I+Output_Object.Table_First_Column_Mult-1))) & " ");
		    else -- Last cell, full width.
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cltxlrtb\cellx" &
			    Format_Value(Output_Object.Table_Indent + Output_Object.Table_Width) & " ");
		    end if;
		end loop;

		-- Now, define text format:
		if Output_Object.Table_Alignment = ARM_Output.Center_All then
		    if Output_Object.Table_Has_Small_Text then
		        Write_Style_for_Paragraph (Output_Object.Output_File,
					           Table_C_Sml_Text_Info,
					           Output_Object.Char_Count);
		        Output_Object.Real_Size := Table_C_Sml_Text_Info.Size;
		        Output_Object.Size := 0;
		    else
		        Write_Style_for_Paragraph (Output_Object.Output_File,
					           Table_C_Text_Info,
					           Output_Object.Char_Count);
		        Output_Object.Real_Size := Table_C_Text_Info.Size;
		        Output_Object.Size := 0;
		    end if;
		else
		    if Output_Object.Table_Has_Small_Text then
		        Write_Style_for_Paragraph (Output_Object.Output_File,
					           Table_L_Sml_Text_Info,
					           Output_Object.Char_Count);
		        Output_Object.Real_Size := Table_L_Sml_Text_Info.Size;
		        Output_Object.Size := 0;
		    else
		        Write_Style_for_Paragraph (Output_Object.Output_File,
					           Table_L_Text_Info,
					           Output_Object.Char_Count);
		        Output_Object.Real_Size := Table_L_Text_Info.Size;
		        Output_Object.Size := 0;
		    end if;
		end if;
	        Ada.Text_IO.Put (Output_Object.Output_File, "\intbl ");
		Output_Object.Char_Count := Output_Object.Char_Count + 6;

	end case;

	-- \trowd - Start a table row.
	-- \row - End a table row.
	-- \trgaph - Half of of the gap between cells, in twips.
	-- \trhdr - Repeat line as header on following pages.
	-- \trkeep - Keep the row together (no page break).
	-- \trkeepfollow - Keep the row with the following (no page break).
	-- \trleft - Left edge of table row (in twips).
	-- \trrh - Row height (minimum if positive, absolute if negative).

	-- \clveralc - Text is centered vertically in cell.
	-- \cltxlrtb - Text flows top to bottom and left to right.
	-- \cellx - Right edge of cell, in Twips. (This is an absolute position).

	-- \intbl - Required marker for each cell.
	-- \cell - Ends cell (use instead of \par).

	-- \trbrdrt - Row Top border
	-- \trbrdrl - Row Left border
	-- \trbrdrb - Row Bottom border
	-- \trbrdrr - Row Right border
	-- \trbrdrh - Row Horizontal border
	-- \trbrdrv - Row Vertical border
	-- \clbrdrt - Cell Top border
	-- \clbrdrl - Cell Left border
	-- \clbrdrb - Cell Bottom border
	-- \clbrdrr - Cell Right border
	-- \brdrs - Single width border
	-- \brdrw - Width of the pen (can't be more than 75).

    end RTF_Table_Info;


    procedure Start_Table (Output_Object : in out RTF_Output_Type;
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
	Page_Width : Natural;
	Column_Units : constant Natural :=
	    Natural(Columns+First_Column_Width+Last_Column_Width-2);
	    -- The number of column units (a unit being a regular width column).
    begin
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
	Output_Object.Table_No_Page_Break := No_Page_Break;
	Output_Object.Table_Alignment := Alignment;
	Output_Object.Table_First_Column_Mult := First_Column_Width;
	Output_Object.Table_Last_Column_Mult := Last_Column_Width;
	Output_Object.Table_Has_Border := Has_Border;
	Output_Object.Table_Has_Small_Text := Small_Text_Size;

	case Output_Object.Page_Size is
	    when ARM_RTF.A4 =>
	        Page_Width := 9030;
	    when ARM_RTF.Letter =>
	        Page_Width := 9360;
	    when ARM_RTF.Half_Letter =>
	        Page_Width := 5040;
	    when ARM_RTF.Ada95 =>
	        Page_Width := 7740;
        end case;
	if Column_Units <= 3 then
	    Output_Object.Table_Indent := Page_Width / 6;
	elsif Column_Units <= 8 then
	    Output_Object.Table_Indent := Page_Width / 16;
	else
	    Output_Object.Table_Indent := 0;
	end if;
        Output_Object.Table_Width  := Page_Width - Output_Object.Table_Indent*2;
        Output_Object.Table_Column_Width  := Output_Object.Table_Width / (Column_Units);
	Output_Object.Column_Count := Columns;
--Ada.Text_IO.Put_Line("Table information");
--Ada.Text_IO.Put_Line("Columns:" & Natural'Image(Columns) &
--	" 1st column mult:" & Natural'Image(First_Column_Width) &
--	" last column mult:" & Natural'Image(Last_Column_Width));
--Ada.Text_IO.Put_Line("Width (twips):" & Natural'Image(Output_Object.Table_Width) &
--	" Column width:" & Natural'Image(Output_Object.Table_Column_Width));

	-- Make a blank line before, of the right size:
	Write_Style_for_Paragraph (Output_Object.Output_File,
				   Table_L_Text_Info,
				   Output_Object.Char_Count);
        Ada.Text_IO.Put (Output_Object.Output_File, "\par }");
	Output_Object.Char_Count := 0;

	case Header_Kind is
	    when ARM_Output.Both_Caption_and_Header =>
		RTF_Table_Info (Output_Object, Kind => Caption);
	    when ARM_Output.Header_Only =>
		RTF_Table_Info (Output_Object, Kind => Header_no_Caption);
	    when ARM_Output.No_Headers =>
		RTF_Table_Info (Output_Object, Kind => Row);
	end case;

    end Start_Table;


    procedure Table_Marker (Output_Object : in out RTF_Output_Type;
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
	use type ARM_Output.Column_Text_Alignment;
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
	    when ARM_Output.End_Item =>
		if Output_Object.Table_Alignment = ARM_Output.Center_except_First then
		    Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cell }");
	            Output_Object.Char_Count := 0;
		    -- Now, define text format:
		    if Output_Object.Table_Has_Small_Text then
		        Write_Style_for_Paragraph (Output_Object.Output_File,
					           Table_C_Sml_Text_Info,
					           Output_Object.Char_Count);
		        Output_Object.Real_Size := Table_C_Sml_Text_Info.Size;
		        Output_Object.Size := 0;
		    else
		        Write_Style_for_Paragraph (Output_Object.Output_File,
					           Table_C_Text_Info,
					           Output_Object.Char_Count);
		        Output_Object.Real_Size := Table_C_Text_Info.Size;
		        Output_Object.Size := 0;
		    end if;
		else -- Text format stays the same.
		    Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cell ");
	            Output_Object.Char_Count := 0;
		end if;

	    when ARM_Output.End_Caption =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cell }");
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\row "); -- End row.

		-- Start header row:
		RTF_Table_Info (Output_Object, Kind => Header); -- Repeat table definition.

	    when ARM_Output.End_Header =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cell }");
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\row "); -- End row.

		-- Start 1st body row:
		RTF_Table_Info (Output_Object, Kind => First_Row); -- Repeat table definition.

	    when ARM_Output.End_Row =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cell }");
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\row "); -- End row.

		-- Start other body rows (no top border!):
		RTF_Table_Info (Output_Object, Kind => Row); -- Repeat table definition.

	    when ARM_Output.End_Row_Next_Is_Last =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cell }");
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\row "); -- End row.

		-- Start other body rows (no top border!):
		RTF_Table_Info (Output_Object, Kind => Last_Row); -- Repeat table definition.

	    when ARM_Output.End_Table =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cell }");
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\row "); -- End last row of table.

		Output_Object.Is_In_Paragraph := False;
		Output_Object.Is_In_Table := False;
		Output_Object.Column_Count := 1;

		-- Make a blank line after, of the right size:
		Write_Style_for_Paragraph (Output_Object.Output_File,
					   Table_L_Text_Info,
					   Output_Object.Char_Count);
	        Ada.Text_IO.Put (Output_Object.Output_File, "\par }");
		Output_Object.Char_Count := 0;
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	end case;
    end Table_Marker;


    -- Text output: These are only allowed after a Start_Paragraph and
    -- before any End_Paragraph. Raises Not_Valid_Error if not allowed.

    Special_Set : constant Ada.Strings.Maps.Character_Set :=
         Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('\'),
           Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('{'),
			           Ada.Strings.Maps.To_Set ('}')));

    procedure Ordinary_Text (Output_Object : in out RTF_Output_Type;
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
	if Ada.Strings.Fixed.Count (Text, Special_Set) = 0 and then
	   (Output_Object.Paragraph_Style not in
	      ARM_Output.Text_Prefixed_Style_Subtype or else
	        Output_Object.Saw_Hang_End) then
		-- The second condition so that prefixes have their
		-- characters counted properly...
	    if Output_Object.Char_Count + Text'Length > LINE_LENGTH then
	        Ada.Text_IO.New_Line (Output_Object.Output_File);
	        Ada.Text_IO.Put (Output_Object.Output_File, Text);
	        Output_Object.Char_Count := Text'Length;
	    elsif Output_Object.Char_Count + Text'Length >= LINE_LENGTH - 10 then
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, Text);
	        Output_Object.Char_Count := 0;
	    else
	        Ada.Text_IO.Put (Output_Object.Output_File, Text);
	        Output_Object.Char_Count := Output_Object.Char_Count + Text'Length;
	    end if;
	else
	    for I in Text'range loop
		Ordinary_Character (Output_Object, Text(I));
	    end loop;
	end if;
    end Ordinary_Text;


    procedure Ordinary_Character (Output_Object : in out RTF_Output_Type;
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
	if Char = ' ' and then Output_Object.Char_Count >= LINE_LENGTH - 5 then
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, " ");
	    Output_Object.Char_Count := 0;
	else
	    if Char = '\' then
	        Ada.Text_IO.Put (Output_Object.Output_File, "\\");
	        Output_Object.Char_Count := Output_Object.Char_Count + 2;
	    elsif Char = '{' then
	        Ada.Text_IO.Put (Output_Object.Output_File, "\{");
	        Output_Object.Char_Count := Output_Object.Char_Count + 2;
	    elsif Char = '}' then
	        Ada.Text_IO.Put (Output_Object.Output_File, "\}");
	        Output_Object.Char_Count := Output_Object.Char_Count + 2;
	    elsif Char >= Character'Val(126) then -- All higher Latin-1 characters.
		declare
		    Code : constant Natural := Character'Pos(Char);
		begin
		    if Code mod 16 >= 10 then
			if Code / 16 >= 10 then
			    Ada.Text_IO.Put (Output_Object.Output_File, "\'" &
				Character'Val((Code / 16 - 10) + Character'Pos('a')) &
				Character'Val((Code mod 16 - 10) + Character'Pos('a')));
			else
			    Ada.Text_IO.Put (Output_Object.Output_File, "\'" &
				Character'Val((Code / 16) + Character'Pos('0')) &
				Character'Val((Code mod 16 - 10) + Character'Pos('a')));
			end if;
		    else
			if Code / 16 >= 10 then
			    Ada.Text_IO.Put (Output_Object.Output_File, "\'" &
				Character'Val((Code / 16 - 10) + Character'Pos('a')) &
				Character'Val((Code mod 16) + Character'Pos('0')));
			else
			    Ada.Text_IO.Put (Output_Object.Output_File, "\'" &
				Character'Val((Code / 16) + Character'Pos('0')) &
				Character'Val((Code mod 16) + Character'Pos('0')));
			end if;
		    end if;
	            Output_Object.Char_Count := Output_Object.Char_Count + 5;
		end;
		if Output_Object.Paragraph_Style in
		      ARM_Output.Text_Prefixed_Style_Subtype and then
		   (not Output_Object.Saw_Hang_End) then
		    if not Ada.Characters.Handling.Is_Lower (Char) then
		        Output_Object.Prefix_Large_Char_Count :=
		           Output_Object.Prefix_Large_Char_Count + 1;
		    -- else small character. (This isn't perfectly accurate, but
		    -- these aren't used much in prefixes.)
		    end if;
		end if;
	    else
	        Ada.Text_IO.Put (Output_Object.Output_File, Char);
	        Output_Object.Char_Count := Output_Object.Char_Count + 1;
		if Output_Object.Paragraph_Style in
			ARM_Output.Text_Prefixed_Style_Subtype and then
		   (not Output_Object.Saw_Hang_End) then
--Ada.Text_Io.Put (Char);
		    if Char in 'A' .. 'H' or else Char in 'J' .. 'Z' or else -- Capital 'I' is narrow.
		       Char in '0' .. '9' or else
			Char = '+' or else Char = '_' or else Char = '@' or else
			Char = '#' or else Char = '$' or else Char = '%' or else
			Char = '&' or else Char = '*' or else Char = '<' or else
			Char = '>' then
		        Output_Object.Prefix_Large_Char_Count :=
		           Output_Object.Prefix_Large_Char_Count + 1;
		    elsif Char = '.' then
			-- '.' is extra narrow; treat it as canceling out a
			-- a large character.
			if Output_Object.Prefix_Large_Char_Count > 0 then
		            Output_Object.Prefix_Large_Char_Count :=
		               Output_Object.Prefix_Large_Char_Count - 1;
			end if;
		    -- else small character.
		    end if;
		end if;
	    end if;
	end if;
    end Ordinary_Character;


    procedure Hard_Space (Output_Object : in out RTF_Output_Type) is
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
	if Output_Object.Paragraph_Style in ARM_Output.Examples ..
	        ARM_Output.Small_Examples then
	    -- Fixed width fonts; hard spaces seem to be a different width
	    -- than regular ones. So use regular spaces.
            Ada.Text_IO.Put (Output_Object.Output_File, " ");
            Output_Object.Char_Count := Output_Object.Char_Count + 1;
	else
            Ada.Text_IO.Put (Output_Object.Output_File, "\~");
            Output_Object.Char_Count := Output_Object.Char_Count + 2;
	end if;
    end Hard_Space;


    procedure Line_Break (Output_Object : in out RTF_Output_Type) is
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
	if Output_Object.Is_In_Table then
	    -- We can't use \Par in a table, or Word deadlocks.
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "\line ");
            Output_Object.Char_Count := 0;
	elsif not Paragraph_Info(Output_Object.Paragraph_Style, Output_Object.Paragraph_Indent).Is_Justified then
	    -- We can't use \Par, as that inserts paragraph spacing.
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "\line ");
            Output_Object.Char_Count := 0;
	else
	    -- We can't use \Line, as that will cause the line to be justified.
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "\sa0\par ");
	        -- We have to turn off the inter-paragraph spacing.
		-- Now, reset the \sa setting.
	    declare
		SA_Width : Natural := Paragraph_Info(Output_Object.Paragraph_Style, Output_Object.Paragraph_Indent).After;
	    begin
		if ARM_Output."="(Output_Object.Current_Space_After,
		   ARM_Output.Narrow) then
		   SA_Width := SA_Width*(LEADING_PERCENT/10)/10;
		elsif ARM_Output."="(Output_Object.Current_Space_After,
		       ARM_Output.Wide) then
		   SA_Width := SA_Width*(TRAILING_PERCENT/10)/10;
		end if;
	        declare
		    SA : constant String := Natural'Image(SA_Width);
	        begin
                    Ada.Text_IO.Put (Output_Object.Output_File, "\sa");
                    Ada.Text_IO.Put (Output_Object.Output_File, SA(2..SA'Last));
                    Ada.Text_IO.Put (Output_Object.Output_File, " ");
                    Output_Object.Char_Count := 4 + SA'Length - 1;
	        end;
	    end;
	    if (Output_Object.Paragraph_Style in ARM_Output.Bulleted ..
	            ARM_Output.Small_Hanging_in_Bulleted) then -- Always "NoPrefix" here.
                Ada.Text_IO.Put (Output_Object.Output_File, "\tab ");
                Output_Object.Char_Count := Output_Object.Char_Count + 5;
	    end if;
	end if;
    end Line_Break;


    procedure Index_Line_Break (Output_Object : in out RTF_Output_Type;
				Clear_Keep_with_Next : in Boolean) is
	-- Output a line break for the index. This does not start a new
	-- paragraph in terms of spacing. This corresponds to a "<BR>"
	-- in HTML. If Clear_Keep_with_Next is true, insure that the next
	-- line does not require the following line to stay with it.
	-- Raises Not_Valid_Error if the paragraph is not in the index format.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
	if ARM_Output."/=" (Output_Object.Paragraph_Style, ARM_Output.Index) or else
	   ARM_Output."/=" (Output_Object.Paragraph_Indent, 0) then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in index paragraph");
	end if;
	-- We have to use /par here, because otherwise we don't get the "undent"
	-- at the start of the paragraph.

	if Clear_Keep_with_Next then
	    -- Note: We need this special routine, because ending the paragraph
	    -- would add blank lines to the HTML.
	    End_Paragraph (Output_Object);
	    Start_Paragraph (Output_Object, ARM_Output.Index, Indent => 0,
		Number => "", No_Breaks => True, Keep_with_Next => False,
		Tab_Stops => Output_Object.Tab_Stops);
	else
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "\par ");
		-- Inherits the paragraph properties.
	end if;
        Output_Object.Char_Count := 0;
    end Index_Line_Break;


    procedure Soft_Line_Break (Output_Object : in out RTF_Output_Type) is
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
        Ada.Text_IO.Put (Output_Object.Output_File, "\softline ");
        --Ada.Text_IO.Put (Output_Object.Output_File, "\zwbo ");
	--    -- Zero-width break opportunity. (Word 7.0 [Word 95] or later).
	--    (Doesn't work).
        Output_Object.Char_Count := Output_Object.Char_Count + 10;
    end Soft_Line_Break;


    procedure Soft_Hyphen_Break (Output_Object : in out RTF_Output_Type) is
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
        Ada.Text_IO.Put (Output_Object.Output_File, "\-");
        Output_Object.Char_Count := Output_Object.Char_Count + 2;
    end Soft_Hyphen_Break;


    procedure Tab (Output_Object : in out RTF_Output_Type) is
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
	if ARM_Output."="(Output_Object.Tab_Stops, ARM_Output.NO_TABS) then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Tab, but none set");
	end if;
        Ada.Text_IO.Put (Output_Object.Output_File, "\tab ");
        Output_Object.Char_Count := Output_Object.Char_Count + 5;
    end Tab;


    procedure Special_Character (Output_Object : in out RTF_Output_Type;
			         Char : in ARM_Output.Special_Character_Type) is
	-- Output an special character.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
	case Char is
	    when ARM_Output.EM_Dash =>
	        Ada.Text_IO.Put (Output_Object.Output_File, "\emdash ");
	        Output_Object.Char_Count := Output_Object.Char_Count + 8;
	    when ARM_Output.EN_Dash =>
	        Ada.Text_IO.Put (Output_Object.Output_File, "\endash ");
	        Output_Object.Char_Count := Output_Object.Char_Count + 8;
	    when ARM_Output.GEQ =>
	        --Unicode: Doesn't work on Windows 98:
		--Ada.Text_IO.Put (Output_Object.Output_File, "\uc2\u8805 >=");
	        --Output_Object.Char_Count := Output_Object.Char_Count + 13;
		-- Character 179, Symbol font.
		Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\'B3}");
	        Output_Object.Char_Count := Output_Object.Char_Count + 9;
	    when ARM_Output.LEQ =>
	        --Unicode: Doesn't work on Windows 98:
	        --Ada.Text_IO.Put (Output_Object.Output_File, "\uc2\u8804 <=");
	        --Output_Object.Char_Count := Output_Object.Char_Count + 13;
		-- Character 163, Symbol font.
		Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\'A3}");
	        Output_Object.Char_Count := Output_Object.Char_Count + 9;
	    when ARM_Output.NEQ =>
	        --Unicode: Doesn't work on Windows 98:
	        --Ada.Text_IO.Put (Output_Object.Output_File, "\uc2\u8800 /=");
	        --Output_Object.Char_Count := Output_Object.Char_Count + 13;
		-- Character 185, Symbol font.
		Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\'B9}");
	        Output_Object.Char_Count := Output_Object.Char_Count + 9;
	    when ARM_Output.PI =>
	        --Unicode: Doesn't work on Windows 98:
	        --Ada.Text_IO.Put (Output_Object.Output_File, "\uc2\u960 PI");
	        --Output_Object.Char_Count := Output_Object.Char_Count + 12;
		-- Character 112, Symbol font.
		Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\'70}");
	        Output_Object.Char_Count := Output_Object.Char_Count + 9;
	    when ARM_Output.Left_Ceiling =>
	        --Unicode: Doesn't work on Windows 98:
	        --Ada.Text_IO.Put (Output_Object.Output_File, "\uc8\u8968 Ceiling(");
	        --Output_Object.Char_Count := Output_Object.Char_Count + 19;
		-- Character 233, Symbol font.
		Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\'E9}");
	        Output_Object.Char_Count := Output_Object.Char_Count + 9;
	    when ARM_Output.Right_Ceiling =>
	        --Unicode: Doesn't work on Windows 98:
	        --Ada.Text_IO.Put (Output_Object.Output_File, "\uc1\u8969 )");
	        --Output_Object.Char_Count := Output_Object.Char_Count + 11;
		-- Character 249, Symbol font.
		Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\'F9}");
	        Output_Object.Char_Count := Output_Object.Char_Count + 9;
	    when ARM_Output.Left_Floor =>
	        --Unicode: Doesn't work on Windows 98:
	        --Ada.Text_IO.Put (Output_Object.Output_File, "\uc6\u8970 Floor(");
	        --Output_Object.Char_Count := Output_Object.Char_Count + 17;
		-- Character 235, Symbol font.
		Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\'EB}");
	        Output_Object.Char_Count := Output_Object.Char_Count + 9;
	    when ARM_Output.Right_Floor =>
	        --Unicode: Doesn't work on Windows 98:
	        --Ada.Text_IO.Put (Output_Object.Output_File, "\uc1\u8971 )");
	        --Output_Object.Char_Count := Output_Object.Char_Count + 11;
		-- Character 251, Symbol font.
		Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\'FB}");
	        Output_Object.Char_Count := Output_Object.Char_Count + 9;
	    when ARM_Output.Thin_Space =>
	        Ada.Text_IO.Put (Output_Object.Output_File, "\qmspace ");
	        Output_Object.Char_Count := Output_Object.Char_Count + 9;
	    when ARM_Output.Left_Quote =>
	        Ada.Text_IO.Put (Output_Object.Output_File, "\lquote ");
	        Output_Object.Char_Count := Output_Object.Char_Count + 8;
	    when ARM_Output.Right_Quote =>
	        Ada.Text_IO.Put (Output_Object.Output_File, "\rquote ");
	        Output_Object.Char_Count := Output_Object.Char_Count + 8;
	    when ARM_Output.Left_Double_Quote =>
	        Ada.Text_IO.Put (Output_Object.Output_File, "\ldblquote ");
	        Output_Object.Char_Count := Output_Object.Char_Count + 11;
	    when ARM_Output.Right_Double_Quote =>
	        Ada.Text_IO.Put (Output_Object.Output_File, "\rdblquote ");
	        Output_Object.Char_Count := Output_Object.Char_Count + 11;
	    when ARM_Output.Small_Dotless_I =>
	        --Unicode: Doesn't work on Windows 98: but we have no choice here:
	        Ada.Text_IO.Put (Output_Object.Output_File, "\uc1\u305 i");
		-- Note: \uc1 means ASCII version has one character;
		-- \u305 means use Unicode character 305.
	        Output_Object.Char_Count := Output_Object.Char_Count + 11;
	    when ARM_Output.Capital_Dotted_I =>
	        --Unicode: Doesn't work on Windows 98: but we have no choice here:
	        Ada.Text_IO.Put (Output_Object.Output_File, "\uc1\u304 I");
	        Output_Object.Char_Count := Output_Object.Char_Count + 11;
	end case;
	if Output_Object.Paragraph_Style in
	      ARM_Output.Text_Prefixed_Style_Subtype and then
	   (not Output_Object.Saw_Hang_End) then
	        Output_Object.Prefix_Large_Char_Count :=
	           Output_Object.Prefix_Large_Char_Count + 1;
	end if;
    end Special_Character;


    procedure Unicode_Character (Output_Object : in out RTF_Output_Type;
			         Char : in ARM_Output.Unicode_Type) is
	-- Output a Unicode character, with code position Char.
	Char_Code : constant String := ARM_Output.Unicode_Type'Image(Char);
	Len : constant String := Natural'Image(Char_Code'Length+2);
    begin
	-- We don't check this, we just output it.
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
        Ada.Text_IO.Put (Output_Object.Output_File, "\uc" & Len(2..Len'Last) &
	   "\u" & Char_Code(2..Char_Code'Last) & " <U" &
	   Char_Code(2..Char_Code'Last) & ">");
        Output_Object.Char_Count := Output_Object.Char_Count + 9 + Len'Last-1 +
	   (Char_Code'Last-1)*2;
	if Output_Object.Paragraph_Style in
		ARM_Output.Text_Prefixed_Style_Subtype and then
	   (not Output_Object.Saw_Hang_End) then
	        Output_Object.Prefix_Large_Char_Count :=
	           Output_Object.Prefix_Large_Char_Count + 1;
	end if;
    end Unicode_Character;


    procedure End_Hang_Item (Output_Object : in out RTF_Output_Type) is
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
	if Output_Object.Paragraph_Style not in ARM_Output.Text_Prefixed_Style_Subtype then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not a hanging paragraph");
	end if;
	if Output_Object.Saw_Hang_End then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Already saw the end of the hanging part");
	end if;
	Output_Object.Saw_Hang_End := True;

--Ada.Text_Io.Put (": Cnt=" & Natural'Image(Output_Object.Char_Count) & " Lrg=" &
--   Natural'Image(Output_Object.Prefix_Large_Char_Count));
--Ada.Text_Io.Put (" Style=" & ARM_Output.Paragraph_Style_Type'Image(Output_Object.Paragraph_Style));
--Ada.Text_Io.Put (" Indent=" & ARM_Output.Paragraph_Indent_Type'Image(Output_Object.Paragraph_Indent));
--Ada.Text_Io.Put (" Count=" & Natural'Image(
--      ((Paragraph_Info(Output_Object.Paragraph_Style, Output_Object.Paragraph_Indent).Hang_Width * 6 * 2) /
--       (Paragraph_Info(Output_Object.Paragraph_Style, Output_Object.Paragraph_Indent).Size * 5   * 5)) - 1));
--Ada.Text_Io.Put (" Hang_Width=" & Natural'Image(Paragraph_Info(Output_Object.Paragraph_Style, Output_Object.Paragraph_Indent).Hang_Width));
--Ada.Text_Io.Put (" Size=" & Natural'Image(Paragraph_Info(Output_Object.Paragraph_Style, Output_Object.Paragraph_Indent).Size));

        if Output_Object.Char_Count*2 + Output_Object.Prefix_Large_Char_Count
	    <= ((Paragraph_Info(Output_Object.Paragraph_Style, Output_Object.Paragraph_Indent).Hang_Width * 6 * 2) /
		(Paragraph_Info(Output_Object.Paragraph_Style, Output_Object.Paragraph_Indent).Size * 5   * 5)) - 1 then
	    -- No line break needed. (I can't find a way to get Word to do
	    -- this properly, so we have to do it. We assume large characters
	    -- are about 1 1/2 times normal characters, and that normal
	    -- characters are about 5/6 the pt. size in width. Note that "Size"
	    -- is in 1/2 pts., while "Hang_Width" is in .1 pts., so we need
	    -- the "5" to correct the units.) The "- 1" is to allow space
	    -- for the trailing space/tab. (Running into the text looks bad!).
	    Ada.Text_IO.Put (Output_Object.Output_File, "\tab ");
	    Output_Object.Char_Count := Output_Object.Char_Count + 5;
--Ada.Text_Io.Put_Line (" No_Break");
        else -- Line break needed.
	    --Ada.Text_IO.Put_Line (Output_Object.Output_File, "\line ");
	    --Output_Object.Char_Count := 0;
	    --This idiot program JUSTIFIES the text fragment, so a line
	    --break cannot be used if the text is justified.
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "\sa0\keepn\par }");
	    Write_Style_for_Paragraph (Output_Object.Output_File,
	        Paragraph_Info(Output_Object.Paragraph_Style, Output_Object.Paragraph_Indent),
	        Output_Object.Char_Count);
	    Set_Tabs (Output_Object, Output_Object.Paragraph_Style,
				Output_Object.Paragraph_Indent);

	    -- Reset after spacing:
	    if ARM_Output."="(Output_Object.Current_Space_After,
	       ARM_Output.Narrow) then
	        declare
		    SA_Width : Natural := Paragraph_Info(Output_Object.Paragraph_Style, Output_Object.Paragraph_Indent).After*(LEADING_PERCENT/10)/10;
		    SA : constant String := Natural'Image(SA_Width);
	        begin
                    Ada.Text_IO.Put (Output_Object.Output_File, "\sa");
                    Ada.Text_IO.Put (Output_Object.Output_File, SA(2..SA'Last));
                    Ada.Text_IO.Put (Output_Object.Output_File, " ");
                    Output_Object.Char_Count := 4 + SA'Length - 1;
	        end;
	    elsif ARM_Output."="(Output_Object.Current_Space_After,
	          ARM_Output.Wide) then
	        declare
		    SA_Width : Natural := Paragraph_Info(Output_Object.Paragraph_Style, Output_Object.Paragraph_Indent).After*(TRAILING_PERCENT/10)/10;
		    SA : constant String := Natural'Image(SA_Width);
	        begin
                    Ada.Text_IO.Put (Output_Object.Output_File, "\sa");
                    Ada.Text_IO.Put (Output_Object.Output_File, SA(2..SA'Last));
                    Ada.Text_IO.Put (Output_Object.Output_File, " ");
                    Output_Object.Char_Count := 4 + SA'Length - 1;
	        end;
	    end if;
	    Ada.Text_IO.Put (Output_Object.Output_File, "\tab ");
	    Output_Object.Char_Count := Output_Object.Char_Count + 5;
--Ada.Text_Io.Put_Line (" Break");
        end if;
    end End_Hang_Item;


    procedure Text_Format (Output_Object : in out RTF_Output_Type;
			   Format : in ARM_Output.Format_Type) is
	-- Change the text format so that all of the properties are as specified.
	-- Note: Changes to these properties ought be stack-like; that is,
	-- Bold on, Italic on, Italic off, Bold off is OK; Bold on, Italic on,
	-- Bold off, Italic off should be avoided (as separate commands).
	TRACE_TF : constant Boolean := FALSE;
	use type ARM_Output.Change_Type;
	use type ARM_Contents.Change_Version_Type;
	use type ARM_Output.Location_Type;
	use type ARM_Output.Size_Type;
	use type ARM_Output.Color_Type;

	procedure Make_Size_Command (Size : in Natural) is
	    -- Write a \fs command to the file for Size.
	    -- Max 29.5 pt, min 5 pt.
	begin
	    if Output_Object.Real_Size >= 50 then
		Ada.Text_IO.Put (Output_Object.Output_File, "\fs5" &
		    Character'Val(Output_Object.Real_Size mod 10 + Character'Pos('0')));
	    elsif Output_Object.Real_Size >= 40 then
		Ada.Text_IO.Put (Output_Object.Output_File, "\fs4" &
		    Character'Val(Output_Object.Real_Size mod 10 + Character'Pos('0')));
	    elsif Output_Object.Real_Size >= 30 then
		Ada.Text_IO.Put (Output_Object.Output_File, "\fs3" &
		    Character'Val(Output_Object.Real_Size mod 10 + Character'Pos('0')));
	    elsif Output_Object.Real_Size >= 20 then
		Ada.Text_IO.Put (Output_Object.Output_File, "\fs2" &
		    Character'Val(Output_Object.Real_Size mod 10 + Character'Pos('0')));
	    elsif Output_Object.Real_Size >= 10 then
		Ada.Text_IO.Put (Output_Object.Output_File, "\fs1" &
		    Character'Val(Output_Object.Real_Size mod 10 + Character'Pos('0')));
	    else
		Ada.Text_IO.Put (Output_Object.Output_File, "\fs10");
	    end if;
	    Output_Object.Char_Count := Output_Object.Char_Count + 6;
	end Make_Size_Command;


	procedure Close_Basic_Format is
	    -- Close any open basic format (Bold/Italic/Size/Font) command.
	begin
	    if (not Output_Object.Is_Bold) and (not Output_Object.Is_Italic) and
		ARM_Output."=" (Output_Object.Font, ARM_Output.Default) and
		Output_Object.Color =  ARM_Output.Default and
	        Output_Object.Size = 0 then
		-- No format previously set, so none to close (default).
		return;
	    end if;
	    if TRACE_TF then
		Ada.Text_Io.Put (" Close basic format [");
		if Output_Object.Is_Bold then
		    Ada.Text_Io.Put ('B');
		end if;
		if Output_Object.Is_Italic then
		    Ada.Text_Io.Put ('I');
		end if;
		if Output_Object.Size /= 0 then
		    Ada.Text_Io.Put ('S');
		end if;
		if Output_Object.Color /= ARM_Output.Default then
		    Ada.Text_Io.Put ('C');
		end if;
		if ARM_Output."/=" (Output_Object.Font, ARM_Output.Default) then
		    Ada.Text_Io.Put ('F');
		end if;
		Ada.Text_Io.Put (']');
	    end if;
	    Ada.Text_IO.Put (Output_Object.Output_File, "}");
	    Output_Object.Char_Count := Output_Object.Char_Count + 1;
	    Output_Object.Real_Size := Output_Object.Real_Size -
				    (Integer(Output_Object.Size)*2);
	    Output_Object.Size := 0;
	    Output_Object.Is_Bold := False;
	    Output_Object.Is_Italic := False;
	    Output_Object.Color := ARM_Output.Default;
	    case Output_Object.Font is
		when ARM_Output.Default => null;
		when ARM_Output.Fixed => null;
		when ARM_Output.Roman => null;
		when ARM_Output.Swiss =>
		    -- Undo the size adjustment, if any.
		    if ARM_Output."/=" (Output_Object.Body_Font, ARM_Output.Swiss) then
		        Output_Object.Real_Size := Output_Object.Real_Size + 1;
		    -- else no adjustment.
		    end if;
	    end case;
	    Output_Object.Font := ARM_Output.Default;
	end Close_Basic_Format;


	procedure Make_Basic_Format is
	    -- Make any needed Bold/Italic/Size/Font command.
	begin
	    if (not Format.Bold) and (not Format.Italic) and
		ARM_Output."=" (Format.Font, ARM_Output.Default) and
		Format.Color = ARM_Output.Default and
	        Format.Size = 0 then
		-- No format needed (default).
		return;
	    end if;
	    Ada.Text_IO.Put (Output_Object.Output_File, "{");
	    Output_Object.Char_Count := Output_Object.Char_Count + 1;
            if TRACE_TF then
		Ada.Text_Io.Put (" Make basic {");
            end if;

	    -- Bold:
	    if Format.Bold then
	        if TRACE_TF then
		    Ada.Text_Io.Put (" Change bold");
	        end if;
	        Ada.Text_IO.Put (Output_Object.Output_File, "\b");
	        Output_Object.Char_Count := Output_Object.Char_Count + 2;
	        Output_Object.Is_Bold := True;
	    end if;
	    -- Italic:
	    if Format.Italic then
	        if TRACE_TF then
		    Ada.Text_Io.Put (" Change italics");
	        end if;
	        Ada.Text_IO.Put (Output_Object.Output_File, "\i");
	        Output_Object.Char_Count := Output_Object.Char_Count + 2;
	        Output_Object.Is_Italic := True;
	    end if;
	    -- Size:
	    if Format.Size /= 0 then
	        if TRACE_TF then
		    Ada.Text_Io.Put (" Change size " & ARM_Output.Size_Type'Image(Format.Size));
	        end if;
	        Output_Object.Real_Size := Output_Object.Real_Size +
						    Integer(Format.Size)*2;
		if ARM_Output."/=" (Format.Font, ARM_Output.Swiss)
		    or else ARM_Output."=" (Output_Object.Body_Font, ARM_Output.Swiss) then
	            Make_Size_Command (Output_Object.Real_Size);
		-- else it will be done by the Font, below.
		end if;
	    end if;
	    Output_Object.Size := Format.Size;
	    -- Color:
	    if Format.Color /= ARM_Output.Default then
	        if TRACE_TF then
		    Ada.Text_Io.Put (" Change color " & ARM_Output.Color_Type'Image(Format.Color));
	        end if;
	        case Format.Color is
		    when ARM_Output.Default => null;
		    when ARM_Output.Black => -- Color 1
		        Ada.Text_IO.Put (Output_Object.Output_File, "\cf1");
		        Output_Object.Char_Count := Output_Object.Char_Count + 4;
		    when ARM_Output.Red   => -- Color 13
		        Ada.Text_IO.Put (Output_Object.Output_File, "\cf13");
		        Output_Object.Char_Count := Output_Object.Char_Count + 5;
		    when ARM_Output.Green => -- Color 11
		        Ada.Text_IO.Put (Output_Object.Output_File, "\cf11");
		        Output_Object.Char_Count := Output_Object.Char_Count + 5;
		    when ARM_Output.Blue  => -- Color 9
		        Ada.Text_IO.Put (Output_Object.Output_File, "\cf9");
		        Output_Object.Char_Count := Output_Object.Char_Count + 4;
		end case;
	    end if;
	    Output_Object.Color := Format.Color;

	    -- Font:
	    case Format.Font is
		when ARM_Output.Default => null;
		when ARM_Output.Fixed =>
	            if TRACE_TF then
			Ada.Text_Io.Put (" Change font fixed");
	            end if;
		    Ada.Text_IO.Put (Output_Object.Output_File, "\f2");
		    Output_Object.Char_Count := Output_Object.Char_Count + 4;
		when ARM_Output.Roman =>
	            if TRACE_TF then
			Ada.Text_Io.Put (" Change font roman");
	            end if;
		    Ada.Text_IO.Put (Output_Object.Output_File, "\f0");
		    Output_Object.Char_Count := Output_Object.Char_Count + 4;
		when ARM_Output.Swiss =>
	            if TRACE_TF then
			Ada.Text_Io.Put (" Change font swiss");
	            end if;
		    Ada.Text_IO.Put (Output_Object.Output_File, "\f1");
		    Output_Object.Char_Count := Output_Object.Char_Count + 3;
		    -- Swiss fonts always appear too large, so shrink it a bit,
		    -- but not if the main body is a Swiss font.
		    if ARM_Output."/=" (Output_Object.Body_Font, ARM_Output.Swiss) then
		        Output_Object.Real_Size := Output_Object.Real_Size - 1;
		        Make_Size_Command (Output_Object.Real_Size);
		    end if;
	    end case;
	    Ada.Text_IO.Put (Output_Object.Output_File, " ");
	    Output_Object.Char_Count := Output_Object.Char_Count + 1;
	    Output_Object.Font := Format.Font;
	end Make_Basic_Format;


	procedure Make_Revision is
	    -- Make any needed revision:
	begin
	    -- We could "improve" this by keeping similar changes together,
	    -- especially for changes to/from Both, but its a lot more work
	    -- and unnecessary.
	    case Format.Change is
		when ARM_Output.Insertion =>
	            if TRACE_TF then
			Ada.Text_Io.Put (" Change insertion");
	            end if;
		    Ada.Text_IO.Put (Output_Object.Output_File, "{\revised\revauth" & Format.Version & ' ');
		    Output_Object.Char_Count := Output_Object.Char_Count + 18;
			-- Note: \revauthN indicates the author. Each version
			-- that we'll use needs an entry in the \revtbl.
			-- We could include a date with \revddtm??, but that's messy.
		when ARM_Output.Deletion =>
	            if TRACE_TF then
			Ada.Text_Io.Put (" Change deletion");
	            end if;
		    Ada.Text_IO.Put (Output_Object.Output_File, "{\deleted\revauthdel" & Format.Version & ' ');
		    Output_Object.Char_Count := Output_Object.Char_Count + 21;
			-- Note: \revauthdelN indicates the author. Each version
			-- that we'll use needs an entry in the \revtbl.
			-- We could include a date with \revddtmdel??, but that's messy.
		when ARM_Output.Both =>
	            if TRACE_TF then
			Ada.Text_Io.Put (" Change both");
	            end if;
		    Ada.Text_IO.Put (Output_Object.Output_File, "{\revised\revauth" & Format.Added_Version & ' ');
		    Output_Object.Char_Count := Output_Object.Char_Count + 18;
		    Ada.Text_IO.Put (Output_Object.Output_File, "{\deleted\revauthdel" & Format.Version & ' ');
		    Output_Object.Char_Count := Output_Object.Char_Count + 21;
			-- Note: \revauthdelN indicates the author. Each version
			-- that we'll use needs an entry in the \revtbl.
			-- We could include a date with \revddtmdel??, but that's messy.
		when ARM_Output.None =>
		    null;
	    end case;
	    Output_Object.Change := Format.Change;
	    Output_Object.Version := Format.Version;
	    Output_Object.Added_Version := Format.Added_Version;
	end Make_Revision;


	procedure Close_Revision is
	    -- Close any open revision:
	begin
	    -- We could "improve" this by keeping similar changes together,
	    -- especially for changes to/from Both, but its a lot more work
	    -- and unnecessary.
	    case Output_Object.Change is
		when ARM_Output.Insertion =>
	            if TRACE_TF then
			Ada.Text_Io.Put (" Unchange insertion");
	            end if;
		    Ada.Text_IO.Put (Output_Object.Output_File, "}");
		    Output_Object.Char_Count := Output_Object.Char_Count + 1;
		when ARM_Output.Deletion =>
	            if TRACE_TF then
			Ada.Text_Io.Put (" Unchange deletion");
	            end if;
		    Ada.Text_IO.Put (Output_Object.Output_File, "}");
		    Output_Object.Char_Count := Output_Object.Char_Count + 1;
		when ARM_Output.None =>
		    null;
		when ARM_Output.Both =>
	            if TRACE_TF then
			Ada.Text_Io.Put (" Unchange both");
	            end if;
	            Ada.Text_IO.Put (Output_Object.Output_File, "}}");
	            Output_Object.Char_Count := Output_Object.Char_Count + 2;
	    end case;
	end Close_Revision;


	procedure Make_Location is
	    -- Make any needed location:
	begin
	    case Format.Location is
		when ARM_Output.Subscript =>
	            if TRACE_TF then
			Ada.Text_Io.Put (" Change sub");
	            end if;
		    Ada.Text_IO.Put (Output_Object.Output_File, "{\sub ");
		    Output_Object.Char_Count := Output_Object.Char_Count + 6;
		when ARM_Output.Superscript =>
	            if TRACE_TF then
			Ada.Text_Io.Put (" Change sup");
	            end if;
		    Ada.Text_IO.Put (Output_Object.Output_File, "{\super ");
		    Output_Object.Char_Count := Output_Object.Char_Count + 8;
		when ARM_Output.Normal =>
		    null;
	    end case;
	    Output_Object.Location := Format.Location;
	end Make_Location;


	procedure Close_Location is
	    -- Close any open location:
	begin
	    case Output_Object.Location is
		when ARM_Output.Subscript =>
	            if TRACE_TF then
			Ada.Text_Io.Put (" Unchange sub");
	            end if;
		    Ada.Text_IO.Put (Output_Object.Output_File, "}");
		    Output_Object.Char_Count := Output_Object.Char_Count + 1;
		when ARM_Output.Superscript =>
	            if TRACE_TF then
			Ada.Text_Io.Put (" Unchange sup");
	            end if;
		    Ada.Text_IO.Put (Output_Object.Output_File, "}");
		    Output_Object.Char_Count := Output_Object.Char_Count + 1;
		when ARM_Output.Normal =>
		    null;
	    end case;
	end Close_Location;

    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;

        if TRACE_TF then
	    Ada.Text_Io.Put ("Text format");
        end if;
	-- We always make changes in the order:
	-- Revision;
	-- Location;
	-- Basic_Format (Bold, Italic, Font, Size, Color).
	-- Thus, we have to unstack them in the reverse order. And, if we want
	-- to change an outer one, we have to close and redo any inner
	-- ones.

	-- We do these in this order so that the changes are stacked properly.
	if Format.Change /= Output_Object.Change or else
	    Format.Version /= Output_Object.Version or else
	    Format.Added_Version /= Output_Object.Added_Version then
	    Close_Basic_Format;
	    Close_Location;
	    Close_Revision;
	    Make_Revision;
	    Make_Location;
	    Make_Basic_Format;
	elsif Format.Location /= Output_Object.Location then
	    -- We don't need to change the revision, leave it alone.
	    Close_Basic_Format;
	    Close_Location;
	    Make_Location;
	    Make_Basic_Format;
	elsif Format.Color /= Output_Object.Color or else
	   Format.Size /= Output_Object.Size or else
	   ARM_Output."/=" (Format.Font, Output_Object.Font) or else
	   Format.Bold /= Output_Object.Is_Bold or else
	   Format.Italic /= Output_Object.Is_Italic then
	    Close_Basic_Format;
	    Make_Basic_Format;
	-- else no change at all.
	end if;
        if TRACE_TF then
	    Ada.Text_Io.New_Line;
        end if;
    end Text_Format;


    procedure Clause_Reference (Output_Object : in out RTF_Output_Type;
				Text : in String;
				Clause_Number : in String) is
	-- Generate a reference to a clause in the standard. The text of
	-- the reference is "Text", and the number of the clause is
	-- Clause_Number. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.
    begin
	Ordinary_Text (Output_Object, Text); -- Nothing special in this format.
	    -- Note: We could generate genuine clause references for Word,
	    -- but that wouldn't buy us anything for the RM.
    end Clause_Reference;


    procedure Index_Target (Output_Object : in out RTF_Output_Type;
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
	null; -- Nothing to do for RTF. We could have let Word make the
	      -- index, but then we'd still have to build it for HTML, and
	      -- we couldn't get paragraph numbers in the index.
    end Index_Target;


    procedure Index_Reference (Output_Object : in out RTF_Output_Type;
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


    procedure DR_Reference (Output_Object : in out RTF_Output_Type;
			    Text : in String;
			    DR_Number : in String) is
	-- Generate a reference to an DR from the standard. The text
	-- of the reference is "Text", and DR_Number denotes
	-- the target. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.
    begin
	Ordinary_Text (Output_Object, Text); -- Nothing special in this format.
    end DR_Reference;


    procedure AI_Reference (Output_Object : in out RTF_Output_Type;
			    Text : in String;
			    AI_Number : in String) is
	-- Generate a reference to an AI from the standard. The text
	-- of the reference is "Text", and AI_Number denotes
	-- the target (in unfolded format). For hyperlinked formats, this should
	-- generate a link; for other formats, the text alone is generated.
    begin
	Ordinary_Text (Output_Object, Text); -- Nothing special in this format.
    end AI_Reference;


    procedure Local_Target (Output_Object : in out RTF_Output_Type;
			    Text : in String;
			    Target : in String) is
	-- Generate a local target. This marks the potential target of local
	-- links identified by "Target". Text is the text of the target.
	-- For hyperlinked formats, this should generate a link target;
	-- for other formats, only the text is generated.
    begin
	Ordinary_Text (Output_Object, Text); -- Nothing special in this format.
    end Local_Target;


    procedure Local_Link (Output_Object : in out RTF_Output_Type;
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


    procedure Local_Link_Start (Output_Object : in out RTF_Output_Type;
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


    procedure Local_Link_End (Output_Object : in out RTF_Output_Type;
			      Target : in String;
			      Clause_Number : in String) is
	-- End a local link for the target and clause given.
	-- This must be in the same paragraph as the Local_Link_Start.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.
    begin
	null; -- No link, nothing to do.
    end Local_Link_End;


    procedure URL_Link (Output_Object : in out RTF_Output_Type;
			Text : in String;
			URL : in String) is
	-- Generate a link to the URL given.
	-- Text is the text of the link.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.
    begin
	Ordinary_Text (Output_Object, Text); -- Nothing special in this format.
    end URL_Link;


    procedure Picture  (Output_Object : in out RTF_Output_Type;
			Name  : in String;
			Descr : in String;
			Alignment : in ARM_Output.Picture_Alignment;
			Height, Width : in Natural;
			Border : in ARM_Output.Border_Kind) is
	-- Generate a picture.
	-- Name is the (simple) file name of the picture; Descr is a
	-- descriptive name for the picture (it will appear in some web
	-- browsers).
	-- We assume that it is a .PNG or .JPG and that it will be present
	-- in the same directory as the output files.
	-- Alignment specifies the picture alignment.
	-- Height and Width specify the picture size in pixels.
	-- Border specifies the kind of border.
	use type ARM_Output.Picture_Alignment;
	use type ARM_Output.Border_Kind;

	HORIZONTAL_TWIPS_PER_PIXEL : constant := 16; -- By experiment.
	VERTICAL_TWIPS_PER_PIXEL : constant := 16; -- By experiment.
	    -- These values give us Pixels/90 = box size in inches.

	-- For reasons that I don't understand, the supposed "raw" picture
	-- size is Pixels/120. So a scaling of 75% gives exact pixels.


	type Kind is (PNG, JPEG, Unknown);
	type DWord is mod 2**32;
	Picture_Width  : DWord;
	Picture_Height : DWord;
	Picture_Kind : Kind := Unknown;
	Picture_Scaling : Natural;

	procedure Get_Picture_Dimensions is
	    -- Get the picture dimensions from the graphic file.
	    use type Ada.Streams.Stream_Element_Offset;
	    Fyle : Ada.Streams.Stream_IO.File_Type;
	    type PNG_Header is record
	        Signature_1 : DWord; -- Fixed value
	        Signature_2 : DWord; -- Fixed value
	        Header_Len  : DWord; -- Fixed value (13)
	        Header_Type : DWord; -- Fixed value ("IHDR")
	        Width       : DWord; -- In pixels.
	        Height      : DWord; -- In pixels.
	        -- Other stuff is not important here.
	    end record;
	    subtype PNG_Stream is Ada.Streams.Stream_Element_Array(1..6*4);
	    function Convert is new Ada.Unchecked_Conversion (Source => PNG_Stream,
							      Target => PNG_Header);
	    Buffer : PNG_Stream;
	    Last : Ada.Streams.Stream_Element_Offset;
	    Temp : Ada.Streams.Stream_Element;
	begin
	    begin
		Ada.Streams.Stream_IO.Open (Fyle,
		    Mode => Ada.Streams.Stream_IO.In_File,
		    Name => ".\Output\" & Name);
	    exception
		when Oops:others =>
		    Ada.Text_IO.Put_Line ("** Unable to open picture file: " &
			Ada.Exceptions.Exception_Message(Oops));
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
			"Unable to open picture file");
	    end;
	    -- Read a PNG header, to see if it is a PNG:
	    Ada.Streams.Stream_IO.Read (Fyle, Buffer, Last);
	    if Last /= 24 then
	        Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		    "Picture file too short");
	    end if;
	    if Convert(Buffer).Signature_1 = 16#89_50_4e_47# and then
	       Convert(Buffer).Signature_2 = 16#0d_0a_1a_0a# then
		-- This is a PNG file:
		Picture_Kind := PNG;
		Picture_Width := Convert(Buffer).Width;
		Picture_Height := Convert(Buffer).Height;
	        Ada.Text_IO.Put_Line ("Forward PNG: Width=" &
		    DWord'Image(Picture_Width) & " Height=" &
		    DWord'Image(Picture_Height));
	    elsif Convert(Buffer).Signature_1 = 16#47_4e_50_89# and then
	       Convert(Buffer).Signature_2 = 16#0a_1a_0a_0d# then
		-- This is a byte-swapped PNG file.
		-- Swap the bytes in the buffer, then get the width and height:
		Temp := Buffer(17);
		Buffer(17) := Buffer(20);
		Buffer(20) := Temp;
		Temp := Buffer(18);
		Buffer(18) := Buffer(19);
		Buffer(19) := Temp;
		Temp := Buffer(21);
		Buffer(21) := Buffer(24);
		Buffer(24) := Temp;
		Temp := Buffer(22);
		Buffer(22) := Buffer(23);
		Buffer(23) := Temp;
		Picture_Kind := PNG;
		Picture_Width := Convert(Buffer).Width;
		Picture_Height := Convert(Buffer).Height;
	        Ada.Text_IO.Put_Line ("Reversed PNG: Width=" &
		    DWord'Image(Picture_Width) & " Height=" &
		    DWord'Image(Picture_Height));


--	    elsif Name'Length > 5 and then
--	       (Name(Name'Last-3..Name'Last) = ".JPG" or else
--	        Name(Name'Last-3..Name'Last) = ".jpg" or else
--	        Name(Name'Last-4..Name'Last) = ".JPEG" or else
--	        Name(Name'Last-4..Name'Last) = ".Jpeg" or else
--	        Name(Name'Last-4..Name'Last) = ".jpeg") then
--		Picture_Kind := JPEG;


	    else
	        Ada.Text_IO.Put_Line ("** Unimplemented picture formatting: File type");
	        Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		    "Do not recognize picture file");
	    end if;
	end Get_Picture_Dimensions;


	function Format_Twips (Twips : in Natural) return String is
	    Flab : constant String := Natural'Image(Twips);
	begin
	    return Flab(2..Flab'Last);
	end Format_Twips;


	procedure Dump_File_in_Hex is
	    -- Read and output the graphics file to the output file
	    -- in Hexadecimal.
	    Fyle : Ada.Streams.Stream_IO.File_Type;
	    Buffer : Ada.Streams.Stream_Element_Array(1..32);
	    Last : Ada.Streams.Stream_Element_Offset;
	    use type Ada.Streams.Stream_Element;
	    use type Ada.Streams.Stream_Element_Offset;
	    Temp : Ada.Streams.Stream_Element;
	begin
	    begin
		Ada.Streams.Stream_IO.Open (Fyle,
		    Mode => Ada.Streams.Stream_IO.In_File,
		    Name => ".\Output\" & Name);
	    exception
		when Oops:others =>
		    Ada.Text_IO.Put_Line ("** Unable to open picture file: " &
			Ada.Exceptions.Exception_Message(Oops));
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
			"Unable to open picture file");
	    end;
	    loop
		Ada.Streams.Stream_IO.Read (Fyle, Buffer, Last);
		exit when Last = 0; -- Nothing read equals end of file.
		for I in 1 .. Last loop
		    Temp := Buffer(I) / 16;
		    if Temp > 9 then
			Ada.Text_IO.Put (Output_Object.Output_File,
			    Character'Val(Character'Pos('A') + (Temp - 10)));
		    else
			Ada.Text_IO.Put (Output_Object.Output_File,
			    Character'Val(Character'Pos('0') + Temp));
		    end if;
		    Temp := Buffer(I) mod 16;
		    if Temp > 9 then
			Ada.Text_IO.Put (Output_Object.Output_File,
			    Character'Val(Character'Pos('A') + (Temp - 10)));
		    else
			Ada.Text_IO.Put (Output_Object.Output_File,
			    Character'Val(Character'Pos('0') + Temp));
		    end if;
		end loop;
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    end loop;
	    Ada.Streams.Stream_IO.Close (Fyle);
	end Dump_File_in_Hex;

    begin
	Get_Picture_Dimensions;

	-- Calculate scaling needed:
	declare
	    Width_Scale, Height_Scale : Float;
	    Word_Scaling : constant Float := 6.0;
		-- Scaling so that the HTML pixel and Word pixel
		-- have the same approximate size. Word's pixels
		-- seem to be about 6 times smaller than HTML's.

	    -- Word's box size is
	begin
	    Width_Scale := Float(Width) / Float(Picture_Width) * 100.0 * Word_Scaling;
	    Height_Scale := Float(Height) / Float(Picture_Height) * 100.0 * Word_Scaling;

	    -- Then, use the smaller scale:
	    if Width_Scale < Height_Scale then
		Picture_Scaling := Natural(Width_Scale);
	    else
		Picture_Scaling := Natural(Height_Scale);
	    end if;
	    Ada.Text_IO.Put_Line ("Picture scaling (%):" & Natural'Image(Picture_Scaling));
	    Ada.Text_IO.Put_Line ("Box width=" &
	        Natural'Image(Width) & " Height=" &
	        Natural'Image(Height));
	    -- Note: Word 2000/2003 seems to ignore this scaling; it seems to
	    -- use the "picwgoal" and "pichgoal" exclusively.
	    -- As noted above, that naturally gives a 75% scaling when the
	    -- picture size and box size are the same. We remove that for this
	    -- information. (Note: The smaller the scaling the better.)
	    Ada.Text_IO.Put_Line ("Word 2003 scaling: Width=" &
	        Natural'Image(Natural(Float(Width) / Float(Picture_Width) * 100.0 / 0.75)) & " Height=" &
	        Natural'Image(Natural(Float(Height) / Float(Picture_Height) * 100.0 / 0.75)));

	end;

	-- Wrap the picture in a shape, so we can set the properties:

        Ada.Text_IO.Put (Output_Object.Output_File,
	    "{\shp{\*\shpinst"); -- Start a shape.
        Ada.Text_IO.Put (Output_Object.Output_File,
	    "\shpleft0\shptop0"); -- Left and top are the origin.
	Ada.Text_IO.Put (Output_Object.Output_File,
	    "\shpright" & Format_Twips(Width * HORIZONTAL_TWIPS_PER_PIXEL)); -- Right edge in twips.
        Ada.Text_IO.Put_Line (Output_Object.Output_File,
	    "\shpbottom" & Format_Twips(Height * VERTICAL_TWIPS_PER_PIXEL)); -- Bottom edge in twips.
        Ada.Text_IO.Put (Output_Object.Output_File,
	    "\shpfhdr0"); -- Shape is in the main document.

        Ada.Text_IO.Put (Output_Object.Output_File,
	    "\shpbxcolumn"); -- Shape is positioned relative to the column.
        Ada.Text_IO.Put (Output_Object.Output_File,
	    "\shpbxignore"); -- But use posrelh instead (column is the default).
        Ada.Text_IO.Put (Output_Object.Output_File,
	    "\shpbypara"); -- Shape is positioned relative to the paragraph.
        Ada.Text_IO.Put (Output_Object.Output_File,
	    "\shpbyignore"); -- But use posrelv instead (paragraph is the default).
	case Alignment is
	   when ARM_Output.Inline =>
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "\shpwr2\shpwrk0"); -- Wrap text around shape (rectangle), on both sides.
	   when ARM_Output.Float_Left =>
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "\shpwr2\shpwrk2"); -- Wrap text around shape (rectangle), on right only.
	   when ARM_Output.Float_Right =>
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "\shpwr2\shpwrk1"); -- Wrap text around shape (rectangle), on left only.
	   when ARM_Output.Alone_Left | ARM_Output.Alone_Center |
		ARM_Output.Alone_Right =>
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "\shpwr1"); -- Don't allow text alongside shape.
	end case;
        Ada.Text_IO.Put (Output_Object.Output_File,
	    "\shpfblwtxt0"); -- Text is below shape.
        Ada.Text_IO.Put (Output_Object.Output_File,
	    "\shpz0"); -- Z-order for shape (these don't overlap, I hope).

	Output_Object.Last_Shape_Id := Output_Object.Last_Shape_Id + 1;
	    -- These need to be unique, but the value doesn't matter much.
        Ada.Text_IO.Put (Output_Object.Output_File,
	    "\shplid" & Format_Twips(Output_Object.Last_Shape_Id));
        Ada.Text_IO.Put (Output_Object.Output_File,
	    "{\sp{\sn shapeType}{\sv 75}}"); -- "Picture frame" type.
        Ada.Text_IO.Put_Line (Output_Object.Output_File,
	    "{\sp{\sn fFlipH}{\sv 0}}{\sp{\sn fFlipV}{\sv 0}}"); -- No flipping.

        Ada.Text_IO.Put_Line (Output_Object.Output_File,
	    "{\sp{\sn pib}{\sv {\pict"); -- Start the picture data

        Ada.Text_IO.Put (Output_Object.Output_File,
	    "\picscalex" & Format_Twips(Picture_Scaling)); -- X scaling (%).
        Ada.Text_IO.Put (Output_Object.Output_File,
	    "\picscaley" & Format_Twips(Picture_Scaling)); -- Y scaling (%).
        Ada.Text_IO.Put (Output_Object.Output_File,
	    "\piccropl0"); -- Left crop (twips) [Should be zero].
        Ada.Text_IO.Put (Output_Object.Output_File,
	    "\piccropr0"); -- Right crop (twips) [Should be zero].
        Ada.Text_IO.Put (Output_Object.Output_File,
	    "\piccropt0"); -- Top crop (twips) [Should be zero].
        Ada.Text_IO.Put_Line (Output_Object.Output_File,
	    "\piccropb0"); -- Bottom crop (twips) [Should be zero].
        Ada.Text_IO.Put (Output_Object.Output_File,
	    "\picw" & Format_Twips(Natural(Picture_Width) * 5)); -- Raw picture width in ??? (doesn't seem to be used).
        Ada.Text_IO.Put (Output_Object.Output_File,
	    "\pich" & Format_Twips(Natural(Picture_Height) * 5)); -- Raw picture height in ??? (doesn't seem to be used).
	Ada.Text_IO.Put (Output_Object.Output_File,
	    "\picwgoal" & Format_Twips(Width * HORIZONTAL_TWIPS_PER_PIXEL)); -- Picture width goal in twips.
        Ada.Text_IO.Put_Line (Output_Object.Output_File,
	    "\pichgoal" & Format_Twips(Height * VERTICAL_TWIPS_PER_PIXEL)); -- Picture height goal in twips.

	-- Figure out file type, using the correct type here:
	if Picture_Kind = PNG then
            Ada.Text_IO.Put_Line (Output_Object.Output_File,
	        "\pngblip"); -- Specifies that the file is a PNG.
	elsif Picture_Kind = JPEG then
            Ada.Text_IO.Put_Line (Output_Object.Output_File,
	        "\jpegblip"); -- Specifies that the file is a JPEG.
	else
	    null; -- We should have already bombed.
	end if;

	-- Should use:
	-- \bliptagnnn - Picture ID.
	-- \blipuid XXXX - Picture Unique ID.
	-- How these are calculated is unclear (it appears to be a hash of
	-- some kind). So I left these out.

	Dump_File_in_Hex;

        Ada.Text_IO.Put (Output_Object.Output_File,
	    "}}}"); -- End the picture data.

        Ada.Text_IO.Put (Output_Object.Output_File,
	    "{\sp{\sn pibName}{\sv " & Name & "}}"); -- Picture file name
        Ada.Text_IO.Put (Output_Object.Output_File,
	    "{\sp{\sn pibFlags}{\sv 2}}"); -- No idea, a flag of "2" is not documented.
	case Border is
	    when ARM_Output.None =>
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "{\sp{\sn fLine}{\sv 0}}"); -- No line here.
	    when ARM_Output.Thin =>
		-- Default lineType of 0, solid.
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "{\sp{\sn lineWidth}{\sv 9525}}"); -- Line size (single - 0.75pt).
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "{\sp{\sn fLine}{\sv 1}}"); -- Show line here.
	    when ARM_Output.Thick =>
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "{\sp{\sn lineWidth}{\sv 19050}}"); -- Line size (double - 1.5pt).
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "{\sp{\sn fLine}{\sv 1}}"); -- Show line here.
	end case;
	case Alignment is
	   when ARM_Output.Inline =>
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "{\sp{\sn posh}{\sv 1}}"); -- Position to the left.
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "{\sp{\sn posrelh}{\sv 3}}"); -- Position to the character.
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "{\sp{\sn posh}{\sv 2}}"); -- Position to the top.
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "{\sp{\sn posrelh}{\sv 3}}"); -- Position to the line.
	   when ARM_Output.Float_Left =>
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "{\sp{\sn posh}{\sv 1}}"); -- Position to the left.
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "{\sp{\sn posrelh}{\sv 2}}"); -- Position to the column.
	   when ARM_Output.Float_Right =>
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "{\sp{\sn posh}{\sv 3}}"); -- Position to the right.
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "{\sp{\sn posrelh}{\sv 2}}"); -- Position to the column.
	   when ARM_Output.Alone_Left =>
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "{\sp{\sn posh}{\sv 1}}"); -- Position to the left.
	   when ARM_Output.Alone_Center =>
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "{\sp{\sn posh}{\sv 2}}"); -- Position to the center.
	   when ARM_Output.Alone_Right =>
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    "{\sp{\sn posh}{\sv 3}}"); -- Position to the right.
	end case;
        Ada.Text_IO.Put_Line (Output_Object.Output_File,
	    "{\sp{\sn fLayoutInCell}{\sv 0}}"); -- No nested use.

	-- Here we find {\shprslt followed by metafile junk. Not doing that.
	---- Fake Word 95 output (it shouldn't be used, but...):
        --Ada.Text_IO.Put_Line (Output_Object.Output_File,
	--    "{\shprslt\par\pard \ql \pvpara\posxr\dxfrtext180\dfrmtxtx180\dfrmtxty0\nowrap\adjustright\ \par}");

        Ada.Text_IO.Put_Line (Output_Object.Output_File,
	    "}}"); -- End the shape.
        Output_Object.Char_Count := 0;

	-- Original code:
--
--	case Alignment is
--	   when ARM_Output.Inline =>
--		null;
--	   when ARM_Output.Float_Left =>
--		null; --***??
--	   when ARM_Output.Float_Right =>
--	        Ada.Text_IO.Put_Line ("** Unimplemented picture formatting: Float Right");
--	   when ARM_Output.Alone_Left =>
--	        Ada.Text_IO.Put (Output_Object.Output_File,
--		    "{\pard\plain\sb" & Format_Twips(Height * VERTICAL_TWIPS_PER_PIXEL) & " ");
--			-- Set up a normal left-justified paragraph that is high enough for this picture.
--	   when ARM_Output.Alone_Center =>
--	        Ada.Text_IO.Put (Output_Object.Output_File,
--		    "{\pard\plain\qc\sb" & Format_Twips(Height * VERTICAL_TWIPS_PER_PIXEL) & " ");
--			-- Set up a normal centered paragraph that is high enough for this picture.
--	   when ARM_Output.Alone_Right =>
--	        Ada.Text_IO.Put (Output_Object.Output_File,
--		    "{\pard\plain\qr\sb" & Format_Twips(Height * VERTICAL_TWIPS_PER_PIXEL) & " ");
--			-- Set up a normal right-justified paragraph that is high enough for this picture.
--	end case;
--
--	-- Picture setup:
--        Ada.Text_IO.Put_Line (Output_Object.Output_File,
--	    "{\*\shppict {\pict "); -- Defines a picture (Word 97 and newer).
--
--	-- Shape Properties:
--	Output_Object.Last_Shape_Id := Output_Object.Last_Shape_Id + 1;
--	    -- These need to be unique, but what they are doesn't matter much.
--        Ada.Text_IO.Put (Output_Object.Output_File,
--	    "{\*\picprop\shplid" & Format_Twips(Output_Object.Last_Shape_Id));
--        Ada.Text_IO.Put (Output_Object.Output_File,
--	    "{\sp{\sn shapeType}{\sv 75}}"); -- "Picture frame" type.
--        Ada.Text_IO.Put_Line (Output_Object.Output_File,
--	    "{\sp{\sn fFlipH}{\sv 0}}{\sp{\sn fFlipV}{\sv 0}}"); -- No flipping.
--        Ada.Text_IO.Put (Output_Object.Output_File,
--	    "{\sp{\sn pibName}{\sv " & Name & "}}"); -- Picture file name
--        Ada.Text_IO.Put (Output_Object.Output_File,
--	    "{\sp{\sn pibFlags}{\sv 2}}"); -- No idea, a flag of "2" is not documented.
--        Ada.Text_IO.Put (Output_Object.Output_File,
--	    "{\sp{\sn fLine}{\sv 0}}"); -- No line here.
--        Ada.Text_IO.Put_Line (Output_Object.Output_File,
--	    "{\sp{\sn fLayoutInCell}{\sv 1}}}"); -- Allow nested use..
--
--
--        Ada.Text_IO.Put (Output_Object.Output_File,
--	    "\picscalex" & Format_Twips(Picture_Scaling)); -- X scaling (%).
--        Ada.Text_IO.Put (Output_Object.Output_File,
--	    "\picscaley" & Format_Twips(Picture_Scaling)); -- Y scaling (%).
--        Ada.Text_IO.Put (Output_Object.Output_File,
--	    "\piccropl0"); -- Left crop (twips) [Should be zero].
--        Ada.Text_IO.Put (Output_Object.Output_File,
--	    "\piccropr0"); -- Right crop (twips) [Should be zero].
--        Ada.Text_IO.Put (Output_Object.Output_File,
--	    "\piccropt0"); -- Top crop (twips) [Should be zero].
--        Ada.Text_IO.Put_Line (Output_Object.Output_File,
--	    "\piccropb0"); -- Bottom crop (twips) [Should be zero].
--        Ada.Text_IO.Put (Output_Object.Output_File,
--	    "\picw" & Format_Twips(Width * HORIZONTAL_TWIPS_PER_PIXEL)); -- Raw picture width in twips.
--        Ada.Text_IO.Put (Output_Object.Output_File,
--	    "\pich" & Format_Twips(Height * VERTICAL_TWIPS_PER_PIXEL)); -- Raw picture height in twips.
--	Ada.Text_IO.Put (Output_Object.Output_File,
--	    "\picwgoal" & Format_Twips(Width * HORIZONTAL_TWIPS_PER_PIXEL)); -- Picture width goal in twips.
--        Ada.Text_IO.Put_Line (Output_Object.Output_File,
--	    "\pichgoal" & Format_Twips(Height * VERTICAL_TWIPS_PER_PIXEL)); -- Picture height goal in twips.
--
--	case Border is
--	    when ARM_Output.None =>
--		null;
--	    when ARM_Output.Thin =>
--		Ada.Text_IO.Put (Output_Object.Output_File,
--	            "\brdrs\brdrw15 "); -- Single thickness border (value is in twips).
--	    when ARM_Output.Thick =>
--		Ada.Text_IO.Put (Output_Object.Output_File,
--	            "\brdrs\brdrw30 "); -- Double thickness border (value is in twips).
--	end case;
--
--	-- Figure out file type, using the correct type here:
--	if Picture_Kind = PNG then
--            Ada.Text_IO.Put (Output_Object.Output_File,
--	        "\pngblip "); -- Specifies that the file is a PNG.
--	elsif Picture_Kind = JPEG then
--            Ada.Text_IO.Put (Output_Object.Output_File,
--	        "\jpegblip "); -- Specifies that the file is a JPEG.
--	else
--	    null; -- We should have already bombed.
--	end if;
--
--	-- Should use:
--	-- \bliptagnnn - Picture ID.
--	-- \blipuid XXXX - Picture Unique ID.
--	-- How these are calculated is unclear (it appears to be a hash of
--	-- some kind). So I left these out.
--
--	Dump_File_in_Hex;
--
--        Ada.Text_IO.Put_Line (Output_Object.Output_File,
--	    "}}"); -- End the picture.
--        Output_Object.Char_Count := 0;
--
--	-- This should be followed by:
--	-- {\*\nonshppict {\pict - Defines an old format picture. This has
--	-- the graphic in metafile format. I have no idea how to convert that;
--	-- forget it.
--
--	-- An easy but incredibly crappy implementation follows. But this
--	-- needs height information in a dedicated paragraph to work at all
--	-- (without it, it ends up one line high). If we have the height
--	-- information, why bother with this; just generate it correctly.
--	-- Then we can be sure that the height and width are specified.
--        --Ada.Text_IO.Put_Line (Output_Object.Output_File,
--	--    "{\field\fldedit{\*\fldinst { INCLUDEPICTURE "".\" & Name & """ \\* MERGEFORMAT \\d }}{\fldrslt {}}}");
--        --Output_Object.Char_Count := 0;
--
--	-- Close anything opened for alignment:
--	case Alignment is
--	   when ARM_Output.Inline =>
--		null;
--	   when ARM_Output.Float_Left =>
--		null;
--	   when ARM_Output.Float_Right =>
--	        null;
--	   when ARM_Output.Alone_Left =>
--	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\par}");
--	   when ARM_Output.Alone_Center =>
--	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\par}");
--	   when ARM_Output.Alone_Right =>
--	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\par}");
--	end case;

    end Picture;


-- Notes:
-- "\_" is a non-breaking hyphen.

end ARM_RTF;
