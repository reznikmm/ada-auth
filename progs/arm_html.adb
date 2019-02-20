with ARM_Output,
     ARM_Contents,
     Ada.Text_IO,
     Ada.Exceptions,
     Ada.Strings.Maps.Constants,
     Ada.Strings.Fixed,
     Ada.Unchecked_Deallocation;
package body ARM_HTML is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package defines the HTML output object.
    -- Output objects are responsible for implementing the details of
    -- a particular format.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
    --		 2008, 2009, 2011, 2012, 2013, 2016, 2018, 2019
    -- AXE Consultants. All rights reserved.
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
    --  4/26/00 - RLB - Added paragraph formats.
    --  4/29/00 - RLB - Added more paragraph formats.
    --  5/10/00 - RLB - Added even more formats.
    --          - RLB - Added End_Hang_Item.
    --  5/12/00 - RLB - Added No_Prefix to Start_Paragraph.
    --  5/13/00 - RLB - Added Special_Character.
    --  5/16/00 - RLB - Added additional special characters.
    --  5/17/00 - RLB - Added New_Page.
    --  5/22/00 - RLB - Added Includes_Changes to Create.
    --  5/23/00 - RLB - Added multi-column formats and New_Column.
    --		      - Added Tab_Info and Tab_Stops.
    --  5/24/00 - RLB - Added Location to Text_Format.
    --		- RLB - Added No_Breaks and Keep_with_Next to Start_Paragraph.
    --  5/25/00 - RLB - Added Big_Files to Create. Added Justification.
    --		- RLB - Added Separator_Lines and TOC routines.
    --		- RLB - Added "Legal" to the footer, pointing at the title page.
    --  5/26/00 - RLB - Added table operations.
    --  5/28/00 - RLB - Added index references.
    --  6/ 2/00 - RLB - Added Soft_Line_Break.
    --  8/ 2/00 - RLB - Added Soft_Hyphen_Break and left and right quote
    --			characters.
    --		- RLB - Added additional styles.
    --  8/ 7/00 - RLB - Added Leading flag to Start_Paragraph, removed "Leading"
    --			styles.
    --  8/11/00 - RLB - Added Hanging_in_Bulleted styles.
    --  8/16/00 - RLB - Added Code_Indented_Nested_Bulleted.
    --  8/17/00 - RLB - Replaced "Leading" by "Space_After".
    -- 		- RLB - Added Nested_Enumerated.
    --  8/22/00 - RLB - Added Revised_Clause_Header.
    --  9/ 8/00 - RLB - Removed soft hyphen, as this does not work on either
    --			browser I tried.
    --  9/26/00 - RLB - Added Syntax_Summary style.
    --  9/27/00 - RLB - Added tab emulation when in the fixed font.
    --  9/28/00 - RLB - Added some style sheets.
    --		- RLB - Updated to use absolute positioning for paragraph
    --			numbers (this looks much better than floating).
    --  7/18/01 - RLB - Added "Indented" style to supported styles for
    --			multi-column.
    --		- RLB - Implemented single "Big-File" support.
    --  7/18/02 - RLB - Removed Document parameter from Create, replaced by
    --			three strings and For_ISO boolean.
    --		- RLB - Added AI_Reference.
    --		- RLB - Added Change_Version_Type and uses.
    --  1/15/03 - RLB - Removed space from DIV.paranum, as it doesn't validate
    --			with it.
    --  4/10/03 - RLB - Updated to add access to search pages (not generated
    --			here; make them by hand, it only needs to be done once).
    --		- RLB - Updated to insure that changes are separated by a space.
    --  4/11/03 - RLB - Changed some formats to meet WC3 validation requirements.
    --  9/09/04 - RLB - Removed unused junk noted by Stephen Leake.
    --  9/10/04 - RLB - Added "Both" to possible changes to handle
    --			replacement of changed text.
    --  9/14/04 - RLB - Moved Change_Version_Type to ARM_Contents.
    --		- RLB - Changed to use left/right quotes whether or not Unicode
    --			is being used. (These work on IE, but not on old
    --			Netscape.)
    --  9/16/04 - RLB - Added a charset meta in the header, so that browsers
    --			can't misinterpret these documents.
    -- 11/03/04 - RLB - Added Nested_X2_Bulleted.
    -- 11/15/04 - RLB - Added Indented_Nested_Bulleted.
    -- 12/15/04 - RLB - Added wider columns.
    --  1/24/05 - RLB - Added Inner_Indented.
    --  2/ 1/05 - RLB - Added Turkish chars to allow an AARM note.
    --  3/15/05 - RLB - Turned on Unicode characters at Pascal's insistence.
    --  3/17/05 - RLB - Removed ceiling and floor characters because they don't
    --			work on Windows.
    --  4/ 7/05 - RLB - Added "Related Documents" link, so users can go between
    --			the RM and AARM (and also so that they see the ARA
    --			sponsor ads).
    --  5/27/05 - RLB - Added arbitrary Unicode characters.
    --  1/11/06 - RLB - Eliminated dispatching Create in favor of tailored
    --			versions.
    --  1/12/06 - RLB - Added a number of parameters to Create.
    --  1/16/06 - RLB - Reduced space around button bars.
    --  1/18/06 - RLB - Added additional styles.
    --  1/19/06 - RLB - Added code so that only styles that are used are
    --		        included in the result (this decreases the minimum
    --			file size by a lot).
    --  1/21/06 - RLB - Specified leading for Swiss example styles, because
    --			they are too close otherwise.
    --  1/28/06 - RLB - Changed so that button highlights are removed correctly.
    --		- RLB - Added tab emulation settings.
    --  2/ 8/06 - RLB - Added additional parameters to the table command.
    --  2/10/06 - RLB - Added even more additional parameters to the
    --			table command.
    --		- RLB - Added picture command.
    --  2/19/06 - RLB - Added Number_Paragraphs flag and large letter count.
    --  3/01/06 - RLB - Fixed bug in Text_Format when changing fonts.
    --  3/03/06 - RLB - Moved paragraph numbers down slightly; this looks a lot
    --			better on Firefox, and better even on IE.
    --		- RLB - Added Optimize_for_Firefox flag, and associated style
    --			changes.
    --		- RLB - Added code so that spaces after an opening tag
    --			and before a closing tag are converted to non-breaking.
    --  3/28/06 - RLB - Removed unnecessary space from headers.
    --  3/30/06 - RLB - Added a bit of space around inline pictures.
    --  9/21/06 - RLB - Added Body_Font.
    --  9/22/06 - RLB - Added Subsubclause.
    --  9/23/06 - RLB - Fixed bug in borderless tables.
    --  9/25/06 - RLB - Handled optional renaming of TOC.
    --		- RLB - Added Last_Column_Width to Start_Table.
    --		- RLB - Fixed broken enumerated style.
    -- 10/13/06 - RLB - Added specifiable colors.
    --          - RLB - Added Local_Link_Start and Local_Link_End to allow
    --			formatting in the linked text.
    -- 11/10/06 - RLB - Fixed nesting of text formatting *again*. (AARM 13.11
    --			failed WC 3 validation.)
    --  2/ 9/07 - RLB - Changed comments on AI_Reference.
    --  2/14/07 - RLB - Revised to separate style and indent information
    --			for paragraphs.
    --  2/15/07 - RLB - Redid enumeration and bullet indenting to make the
    --			formats work consistently on Firefox and IE.
    --  2/19/07 - RLB - Added Title style.
    -- 12/14/07 - RLB - Added code to support multi-column text without
    --			requiring New_Column calls.
    -- 12/18/07 - RLB - Added Plain_Annex.
    -- 12/19/07 - RLB - Added DOS_Filename flag.
    --		- RLB - Added limited colors to Text_Format.
    --  1/02/08 - RLB - Made DOS filenames into all CAPS.
    --  5/ 7/09 - RLB - Added code to prevent making links to dead clauses.
    -- 10/18/11 - RLB - Changed to GPLv3 license.
    -- 10/25/11 - RLB - Added old insertion version to Revised_Clause_Header.
    --  2/15/12 - RLB - Removed horizontal rules from page breaks in HTML;
    --			they never are necessary and cause weird looks for
    --			breaks put in solely to make the "final" version look
    --			good in PDF form.
    --  5/18/12 - RLB - Added anchors to each paragraph number as suggested
    --			on comp.lang.ada.
    --  8/31/12 - RLB - Added Output_Path.
    -- 10/18/12 - RLB - Added additional hanging styles.
    -- 11/26/12 - RLB - Added subdivision names to Clause_Header and
    --			Revised_Clause_Header.
    -- 12/17/12 - RLB - Added AI12 references. Defined a change color for
    --			"version 4" (Ada 202x).
    --  3/26/13 - RLB - Added Script_HTML.
    --  2/ 8/16 - RLB - Added styles for the Ada-Auth.org header.
    --		      - Defined change colors for versions 5 and 6. (Actually
    --			"borrowed" the color for version 2, so the new colors
    --			are for version 2 and 6. Can't use dark blue, as links
    --			are that color.)
    --  3/ 3/16 - RLB - The color change is hell on the RR manuals. Redid
    --			it to switch the colors only if version 5 is in use.
    --  4/20/16 - RLB - Slightly increased the base indent as a lot of
    --			paragraph numbers are overlapping.
    --		      - Added Force_New_Revision_Colors.
    --  3/ 8/18 - RLB - Increased the size of the HTML re-read line buffer.
    --  2/19/19 - RLB - Added some (commented out) hang tracing.

    LINE_LENGTH : constant := 78;
	-- Maximum intended line length.

    SWISS_FONT_CODE : constant String := "<font face=""Arial, Helvetica"">";

    SMALL_SWISS_FONT_CODE : constant String := "<font face=""Arial, Helvetica"" size=-1>";

    TINY_SWISS_FONT_CODE : constant String := "<font face=""Arial, Helvetica"" size=-2>";

    LEADING_PERCENT : constant := 70;
	-- Leading is 70% of normal height.
    TRAILING_PERCENT : constant := 150;
	-- Leading is 150% of normal height.

    INDENT_EMS_FOR_PARANUMS : constant := 18;
	-- Indent *all* text (for HTML 4) this amount to leave (some) room for
	-- the paragraph numbers. In 0.1 EMs.

    OPTIMIZE_FOR_FIREFOX : constant Boolean := True;
	-- If True, we'll optimize for Firefox; otherwise, we'll optimize for
	-- IE 6. Note that IE generally shows the Firefox code better than
	-- Firefox shows the IE code, so we generally recommend setting to
	-- True unless IE must be perfect. (Modern versions of IE are more
	-- like Firefox, so it's unlikely that setting this to False should
	-- be needed.)

    type Tag_Kind is (DIV, UL, DL);

    type Format_Info_Type is record
	Defined: Boolean := False;
	Tag    : Tag_Kind;
	Size   : Integer; -- In relative "units" (based on the normal size). A unit is 125%/80% of normal.
	Font   : ARM_Output.Font_Family_Type;
	Indent : Natural; -- In "units". (A unit is = 2EM of the full sized font).
	Right_Indent : Natural; -- In "units". (A unit is = 2EM of the full sized font).
	Hang_Outdent : Natural; -- In "units". (A unit is = 2EM of the full sized font).
		-- This is the amount that the hanging text hangs out. Normal
		-- text starts at Hang_Outdent + Indent "units".
	Before : Integer; -- Vertical space before in 0.1 EM.
	After  : Natural; -- Vertical space after in 0.1 EM.
    end record;

    -- In the following, "Default" means the Body_Font.
    Paragraph_Info : array
	(ARM_Output.Paragraph_Style_Type, ARM_Output.Paragraph_Indent_Type) of
	   Format_Info_Type;
	-- Defined below in the body of the package.

    -- Are the various styles used??
    Paragraph_Used : array (ARM_Output.Paragraph_Style_Type,
			    ARM_Output.Paragraph_Indent_Type) of Boolean;
    Revision_Used : array (ARM_Contents.Change_Version_Type) of Boolean;
    Paranum_Used : Boolean;


    procedure Free is new Ada.Unchecked_Deallocation (Column_Text_Item_Type, Column_Text_Ptr);

    function Make_Clause_Anchor_Name (Output_Object : in HTML_Output_Type;
				      Clause_Number : in String) return String is
	-- Internal routine.
	-- Returns the Clause anchor name for the current output object and
	-- Clause_Number.
        Clause_Name : String(1..10);
        Clause_Name_Len : Natural;
    begin
        if Clause_Number'Length >= 7 and then
	   Clause_Number(Clause_Number'First .. Clause_Number'First + 5) =
	   "Annex " then
	    Clause_Name(1) := Clause_Number(Clause_Number'First + 6);
	        -- We only want the letter.
	    Clause_Name_Len := 1;
        else
	    Clause_Name_Len := Clause_Number'Length;
	    Clause_Name(1..Clause_Name_Len) := Clause_Number;
	    for I in 1 .. Clause_Name_Len loop
	        if Clause_Name(I) = '.' then
		    Clause_Name(I) := '-';
	        end if;
	    end loop;
        end if;
        if Output_Object.DOS_Filenames and (not Output_Object.Big_Files) then
	    -- If the section number is a single character, then
	    -- prefix it with '-':
	    if Clause_Name_Len = 1 then
	        Clause_Name(2) := Clause_Name(1);
		Clause_Name(1) := '-';
		Clause_Name_Len := 2;
	    elsif Clause_Name_Len = 2 then
		null;
	    else
		if Clause_Name(2) = '-' then
		    Clause_Name(2..Clause_Name_Len+1) :=
			Clause_Name(1..Clause_Name_Len);
		    Clause_Name(1) := '-';
		    Clause_Name_Len := Clause_Name_Len + 1;
		end if;
		-- OK, the section name is exactly two characters.
		-- Figure out the Clause name.
		if Clause_Name_Len < 4 or else
		   Clause_Name(3) /= '-' or else
		   Clause_Name(4) not in '0'..'9' or else
                   (Clause_Name_Len >= 5 and then
		    Clause_Name(5) /= '-' and then Clause_Name(5) not in '0'..'9') or else
                   (Clause_Name_Len >= 6 and then
		    Clause_Name(5) /= '-' and then Clause_Name(6) /= '-') then
		    -- The clause number is 1 or 2 digits following a '-',
		    -- and is either the end of the string or followed by a '-'.
		    Ada.Exceptions.Raise_Exception (Program_Error'Identity,
		       "Weird clause number:" & Clause_Number);
		elsif Clause_Name_Len = 4 or else Clause_Name(5) = '-' then
		    -- Clause is a single digit.
		    Clause_Name(3..Clause_Name_Len-1) :=
			Clause_Name(4..Clause_Name_Len);
		    Clause_Name_Len := Clause_Name_Len - 1;
		elsif Clause_Name(4) = '1' then
		    -- Clause is 10..19
		    Clause_Name(3) := Character'Val(
			Character'Pos(Clause_Name(5))-Character'Pos('0')+Character'Pos('A'));
		    Clause_Name(4..Clause_Name_Len-2) :=
			Clause_Name(6..Clause_Name_Len);
		    Clause_Name_Len := Clause_Name_Len - 2;
		elsif Clause_Name(4) = '2' then
		    -- Clause is 20..29
		    Clause_Name(3) := Character'Val(
			Character'Pos(Clause_Name(5))-Character'Pos('0')+Character'Pos('A')+10);
		    Clause_Name(4..Clause_Name_Len-2) :=
			Clause_Name(6..Clause_Name_Len);
		    Clause_Name_Len := Clause_Name_Len - 2;
		elsif Clause_Name(4) = '3' then
		    -- Clause is 30..39
		    if Clause_Name(5) > '5' then
		        Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		           "MS-DOS filename: Clause too large=" & Clause_Number);
		    end if;
		    Clause_Name(3) := Character'Val(
			Character'Pos(Clause_Name(5))-Character'Pos('0')+Character'Pos('A')+20);
		    Clause_Name(4..Clause_Name_Len-2) :=
			Clause_Name(6..Clause_Name_Len);
		    Clause_Name_Len := Clause_Name_Len - 2;
		else
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		       "MS-DOS filename: Clause too large=" & Clause_Number);
		end if;
		-- OK, the section number is exactly two characters, and the
		-- clause number is exactly one. Figure out the subclause
		-- name:
		if Clause_Name_Len = 3 then
		    null; -- We're done, no subclause.
		elsif Clause_Name_Len < 5 or else
		   Clause_Name(4) /= '-' or else
		   Clause_Name(5) not in '0'..'9' or else
                   (Clause_Name_Len >= 6 and then
		    Clause_Name(6) /= '-' and then Clause_Name(6) not in '0'..'9') or else
                   (Clause_Name_Len >= 7 and then
		    Clause_Name(6) /= '-' and then Clause_Name(7) /= '-') then
		    -- The subclause number is 1 or 2 digits following a '-',
		    -- and is either the end of the string or followed by a '-'.
		    Ada.Exceptions.Raise_Exception (Program_Error'Identity,
		       "Weird subclause number:" & Clause_Number);
		elsif Clause_Name_Len = 5 or else Clause_Name(6) = '-' then
		    -- SubClause is a single digit.
		    Clause_Name(4..Clause_Name_Len-1) :=
			Clause_Name(5..Clause_Name_Len);
		    Clause_Name_Len := Clause_Name_Len - 1;
		elsif Clause_Name(5) = '1' then
		    -- SubClause is 10..19
		    Clause_Name(4) := Character'Val(
			Character'Pos(Clause_Name(6))-Character'Pos('0')+Character'Pos('A'));
		    Clause_Name(5..Clause_Name_Len-2) :=
			Clause_Name(7..Clause_Name_Len);
		    Clause_Name_Len := Clause_Name_Len - 2;
		elsif Clause_Name(5) = '2' then
		    -- SubClause is 20..29
		    Clause_Name(4) := Character'Val(
			Character'Pos(Clause_Name(6))-Character'Pos('0')+Character'Pos('A')+10);
		    Clause_Name(5..Clause_Name_Len-2) :=
			Clause_Name(7..Clause_Name_Len);
		    Clause_Name_Len := Clause_Name_Len - 2;
		elsif Clause_Name(5) = '3' then
		    -- SubClause is 30..39
		    if Clause_Name(6) > '5' then
		        Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		           "MS-DOS filename: Subclause too large=" & Clause_Number);
		    end if;
		    Clause_Name(4) := Character'Val(
			Character'Pos(Clause_Name(6))-Character'Pos('0')+Character'Pos('A')+20);
		    Clause_Name(5..Clause_Name_Len-2) :=
			Clause_Name(7..Clause_Name_Len);
		    Clause_Name_Len := Clause_Name_Len - 2;
		else
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		       "MS-DOS filename: Subclause too large=" & Clause_Number);
		end if;
		-- OK, the section number is exactly two characters, and the
		-- clause number is one, and the subclause number is one or zero.
		-- Figure out the subsubclause name:
		if Clause_Name_Len < 5 then
		    null; -- We're done, no subsubclause.
		elsif Clause_Name_Len < 6 or else
		   Clause_Name(5) /= '-' or else
		   Clause_Name(6) not in '0'..'9' or else
                   (Clause_Name_Len >= 7 and then
		    Clause_Name(7) not in '0'..'9') or else
                   (Clause_Name_Len >= 8) then
		    -- The subsubclause number is 1 or 2 digits following a '-',
		    -- and is the end of the string
		    Ada.Exceptions.Raise_Exception (Program_Error'Identity,
		       "Weird subclause number:" & Clause_Number);
		elsif Clause_Name_Len = 6 then
		    -- SubSubClause is a single digit.
		    Clause_Name(5) := Clause_Name(6);
		    Clause_Name_Len := 5;
		elsif Clause_Name(6) = '1' then
		    -- SubSubClause is 10..19
		    Clause_Name(5) := Character'Val(
			Character'Pos(Clause_Name(7))-Character'Pos('0')+Character'Pos('A'));
		    Clause_Name_Len := 5;
		elsif Clause_Name(5) = '2' then
		    -- SubSubClause is 20..29
		    Clause_Name(5) := Character'Val(
			Character'Pos(Clause_Name(7))-Character'Pos('0')+Character'Pos('A')+10);
		    Clause_Name_Len := 5;
		elsif Clause_Name(5) = '3' then
		    -- SubSubClause is 30..39
		    if Clause_Name(6) > '5' then
		        Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		           "MS-DOS filename: Subsubclause too large=" & Clause_Number);
		    end if;
		    Clause_Name(5) := Character'Val(
			Character'Pos(Clause_Name(7))-Character'Pos('0')+Character'Pos('A')+20);
		    Clause_Name_Len := 5;
		else
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		       "MS-DOS filename: Subsubclause too large=" & Clause_Number);
		end if;
	    end if;
	end if;
	return Clause_Name(1..Clause_Name_Len);
    end Make_Clause_Anchor_Name;


    function Make_Clause_File_Name (Output_Object : in HTML_Output_Type;
				    Clause_Number : in String) return String is
	-- Internal routine.
	-- Returns the Clause file name for the current output object and
	-- Clause_Number. This does not include any path or extension.
    begin
	if Output_Object.Big_Files then -- One big file.
	    return Ada.Strings.Fixed.Trim (Output_Object.File_Prefix, Ada.Strings.Right);
	else -- Clause files.
	    if Output_Object.DOS_Filenames then
	        return Ada.Strings.Fixed.Trim (Output_Object.File_Prefix, Ada.Strings.Right) &
	            Make_Clause_Anchor_Name (Output_Object, Clause_Number);
	    else
	        return Ada.Strings.Fixed.Trim (Output_Object.File_Prefix, Ada.Strings.Right) &
	            "-" & Make_Clause_Anchor_Name (Output_Object, Clause_Number);
	    end if;
	end if;
    end Make_Clause_File_Name;


    function Make_Clause_Link_Name (Output_Object : in HTML_Output_Type;
				    Clause_Number : in String) return String is
	-- Internal routine.
	-- Returns the link name for a link to the given clause.
    begin
	if Output_Object.Big_Files then -- One big file.
	    -- Note this is a self-reference, so the file name is not needed.
	    return "#" & Make_Clause_Anchor_Name (Output_Object, Clause_Number);
	else -- Clause files.
	    if Output_Object.DOS_Filenames then
	        return Make_Clause_File_Name (Output_Object, Clause_Number) & ".HTM";
	    else
	        return Make_Clause_File_Name (Output_Object, Clause_Number) & ".html";
	    end if;
	end if;
    end Make_Clause_Link_Name;


    procedure Put_EMs (Fyle : in Ada.Text_IO.File_Type;
		       Value : in Natural) is
	-- Put an EMs Value (Value is in 0.1 EM).
    begin
	if Value <= 9 then
	    Ada.Text_IO.Put (Fyle, '0');
	elsif Value <= 99 then
	    Ada.Text_IO.Put (Fyle, Character'Val(Character'Pos('0') + (Value / 10)));
	else
	    Ada.Text_IO.Put (Fyle, Natural'Image (Value / 10));
	end if;
	Ada.Text_IO.Put (Fyle, '.');
	Ada.Text_IO.Put (Fyle, Character'Val(Character'Pos('0') + (Value Mod 10)));
	Ada.Text_IO.Put (Fyle, "em");
    end Put_EMs;


    procedure Make_Navigation_Bar (Output_Object : in out HTML_Output_Type;
				   Is_Top : in Boolean) is
	-- Internal routine.
	-- Generate a properly formatted navigation bar.
	Clause : constant String :=
	    Ada.Strings.Unbounded.To_String(Output_Object.Current_Clause);
    begin
        if Output_Object.Use_Buttons then
	    if Is_Top and then Output_Object.HTML_Kind > HTML_3 then
	        Ada.Text_IO.Put (Output_Object.Output_File, "<div style=""margin-top: 0.6em; margin-bottom: 0.0em"">");
	    elsif (not Is_Top) and then Output_Object.HTML_Kind > HTML_3 then
	        Ada.Text_IO.Put (Output_Object.Output_File, "<div style=""margin-top: 0.0em; margin-bottom: 0.6em"">");
	    else
	        Ada.Text_IO.Put (Output_Object.Output_File, "<P>");
	    end if;
	    Ada.Text_IO.Put (Output_Object.Output_File, "<A HREF=""");
	    if Output_Object.Big_Files then
	        Ada.Text_IO.Put (Output_Object.Output_File, "#TOC");
	    elsif Output_Object.DOS_Filenames then
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    Ada.Strings.Fixed.Trim (Output_Object.File_Prefix, Ada.Strings.Right) &
		       "-TOC.HTM");
	    else
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    Ada.Strings.Fixed.Trim (Output_Object.File_Prefix, Ada.Strings.Right) &
		       "-TOC.html");
	    end if;
	    if Output_Object.DOS_Filenames then
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, """><IMG SRC=""CONT.GIF"" ALT=""Contents"" BORDER=0></A>&nbsp;");
		    -- Border=0 prevents the link highlight from being applied.
	    else
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, """><IMG SRC=""cont.gif"" ALT=""Contents"" BORDER=0></A>&nbsp;");
		    -- Border=0 prevents the link highlight from being applied.
	    end if;
	    if Ada.Strings.Unbounded.Length(Output_Object.Index_URL) /= 0 then
	        Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;<A HREF=""");
	        Ada.Text_IO.Put (Output_Object.Output_File,
	            Ada.Strings.Unbounded.To_String(Output_Object.Index_URL));
	        if Output_Object.DOS_Filenames then
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, """><IMG SRC=""INDEX.GIF"" ALT=""Index"" BORDER=0></A>&nbsp;");
		else
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, """><IMG SRC=""index.gif"" ALT=""Index"" BORDER=0></A>&nbsp;");
		end if;
	    else -- Link to the section named "Index".
	        begin
		    -- Note: We do the following in one big glup so that if
		    -- Not_Found_Error is raised, nothing is output.
		    if Output_Object.DOS_Filenames then
	                Ada.Text_IO.Put_Line (Output_Object.Output_File, "&nbsp;<A HREF=""" &
			    Make_Clause_Link_Name (Output_Object,
			        ARM_Contents.Lookup_Clause_Number ("Index" & (6 .. ARM_Contents.Title_Type'Last => ' '))) &
	                    """><IMG SRC=""INDEX.GIF"" ALT=""Index"" BORDER=0></A>&nbsp;");
		    else
	                Ada.Text_IO.Put_Line (Output_Object.Output_File, "&nbsp;<A HREF=""" &
			    Make_Clause_Link_Name (Output_Object,
			        ARM_Contents.Lookup_Clause_Number ("Index" & (6 .. ARM_Contents.Title_Type'Last => ' '))) &
	                    """><IMG SRC=""index.gif"" ALT=""Index"" BORDER=0></A>&nbsp;");
		    end if;
	        exception
		    when ARM_Contents.Not_Found_Error =>
		        null; -- No section named "Index".
	        end;
	    end if;
	    if Ada.Strings.Unbounded.Length(Output_Object.Ref_URL) /= 0 then
	        Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;<A HREF=""");
	        Ada.Text_IO.Put (Output_Object.Output_File,
	            Ada.Strings.Unbounded.To_String(Output_Object.Ref_URL));
	        if Output_Object.DOS_Filenames then
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, """><IMG SRC=""LIB.GIF"" ALT=""References"" BORDER=0></A>&nbsp;");
		else
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, """><IMG SRC=""lib.gif"" ALT=""References"" BORDER=0></A>&nbsp;");
		end if;
	    else -- Link to the section named "References".
	        begin
		    -- Note: We do the following in one big glup so that if
		    -- Not_Found_Error is raised, nothing is output.
	            if Output_Object.DOS_Filenames then
	                Ada.Text_IO.Put_Line (Output_Object.Output_File, "&nbsp;<A HREF=""" &
		            Make_Clause_Link_Name (Output_Object,
			        ARM_Contents.Lookup_Clause_Number ("References" & (11 .. ARM_Contents.Title_Type'Last => ' '))) &
	                    """><IMG SRC=""LIB.GIF"" ALT=""References"" BORDER=0></A>&nbsp;");
		    else
	                Ada.Text_IO.Put_Line (Output_Object.Output_File, "&nbsp;<A HREF=""" &
		            Make_Clause_Link_Name (Output_Object,
			        ARM_Contents.Lookup_Clause_Number ("References" & (11 .. ARM_Contents.Title_Type'Last => ' '))) &
	                    """><IMG SRC=""lib.gif"" ALT=""References"" BORDER=0></A>&nbsp;");
		    end if;
	        exception
		    when ARM_Contents.Not_Found_Error =>
		        null; -- No section named "References".
	        end;
	    end if;
	    if Ada.Strings.Unbounded.Length(Output_Object.Srch_URL) /= 0 then
	        Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;<A HREF=""");
	        Ada.Text_IO.Put (Output_Object.Output_File,
	            Ada.Strings.Unbounded.To_String(Output_Object.Srch_URL));
	        if Output_Object.DOS_Filenames then
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, """><IMG SRC=""FIND.GIF"" ALT=""Search"" BORDER=0></A>&nbsp;");
		else
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, """><IMG SRC=""find.gif"" ALT=""Search"" BORDER=0></A>&nbsp;");
		end if;
	    else -- Link to the section named "References".
	        begin
		    -- Note: We do the following in one big glup so that if
		    -- Not_Found_Error is raised, nothing is output.
	            if Output_Object.DOS_Filenames then
	                Ada.Text_IO.Put_Line (Output_Object.Output_File, "&nbsp;<A HREF=""" &
			    Make_Clause_Link_Name (Output_Object,
			        ARM_Contents.Lookup_Clause_Number ("Search" & (7 .. ARM_Contents.Title_Type'Last => ' '))) &
	                    """><IMG SRC=""FIND.GIF"" ALT=""Search"" BORDER=0></A>&nbsp;");
		    else
	                Ada.Text_IO.Put_Line (Output_Object.Output_File, "&nbsp;<A HREF=""" &
			    Make_Clause_Link_Name (Output_Object,
			        ARM_Contents.Lookup_Clause_Number ("Search" & (7 .. ARM_Contents.Title_Type'Last => ' '))) &
	                    """><IMG SRC=""find.gif"" ALT=""Search"" BORDER=0></A>&nbsp;");
		    end if;
	        exception
		    when ARM_Contents.Not_Found_Error =>
		        null; -- No section named "Index".
		end;
	    end if;
	    if Clause /= "" then
	        begin
		    -- Note: We do the following in one big glup so that if
		    -- Not_Found_Error is raised, nothing is output.
		    Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;<A HREF=""" &
		        Make_Clause_Link_Name (Output_Object,
			    ARM_Contents.Previous_Clause(Clause)));
	            if Output_Object.DOS_Filenames then
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, """><IMG SRC=""PREV.GIF"" ALT=""Previous"" BORDER=0></A>&nbsp;");
		    else
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, """><IMG SRC=""prev.gif"" ALT=""Previous"" BORDER=0></A>&nbsp;");
		    end if;
	        exception
		    when ARM_Contents.Not_Found_Error =>
		        null; -- Probably the first section.
	        end;
	        begin
		    -- Note: We do the following in one big glup so that if
		    -- Not_Found_Error is raised, nothing is output.
		    Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;<A HREF=""" &
		        Make_Clause_Link_Name (Output_Object,
			    ARM_Contents.Next_Clause(Clause)));
	            if Output_Object.DOS_Filenames then
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, """><IMG SRC=""NEXT.GIF"" ALT=""Next"" BORDER=0></A>&nbsp;");
		    else
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, """><IMG SRC=""next.gif"" ALT=""Next"" BORDER=0></A>&nbsp;");
		    end if;
	        exception
		    when ARM_Contents.Not_Found_Error =>
		        null; -- Probably the last section.
	        end;
	    end if;
	    if Output_Object.HTML_Kind > HTML_3 then
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "</div>");
	    else
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "</P>");
	    end if;
        else -- Use text navigation
	    Ada.Text_IO.Put (Output_Object.Output_File, "<P><A HREF=""");
	    if Output_Object.Big_Files then
	        Ada.Text_IO.Put (Output_Object.Output_File, "#TOC");
	    elsif Output_Object.DOS_Filenames then
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    Ada.Strings.Fixed.Trim (Output_Object.File_Prefix, Ada.Strings.Right) &
		       "-TOC.HTM");
	    else
	        Ada.Text_IO.Put (Output_Object.Output_File,
		    Ada.Strings.Fixed.Trim (Output_Object.File_Prefix, Ada.Strings.Right) &
		       "-TOC.html");
	    end if;
	    Ada.Text_IO.Put (Output_Object.Output_File, """>Contents</A>");
	    if Ada.Strings.Unbounded.Length(Output_Object.Index_URL) /= 0 then
	        Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;&nbsp;&nbsp;");
	        Ada.Text_IO.Put (Output_Object.Output_File, "<A HREF=""");
	        Ada.Text_IO.Put (Output_Object.Output_File,
	            Ada.Strings.Unbounded.To_String(Output_Object.Index_URL));
	        Ada.Text_IO.Put (Output_Object.Output_File, """>Index</A>");
	    else -- Link to the section named "Index".
	        begin
		    -- Note: We do the following in one big glup so that if
		    -- Not_Found_Error is raised, nothing is output.
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, "&nbsp;&nbsp;&nbsp;<A HREF=""" &
			Make_Clause_Link_Name (Output_Object,
			    ARM_Contents.Lookup_Clause_Number ("Index" & (6 .. ARM_Contents.Title_Type'Last => ' '))) &
	                """>Index</A>");
	        exception
		    when ARM_Contents.Not_Found_Error =>
		        null; -- No section named "Index".
	        end;
	    end if;
	    if Ada.Strings.Unbounded.Length(Output_Object.Srch_URL) /= 0 then
	        Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;&nbsp;&nbsp;");
	        Ada.Text_IO.Put (Output_Object.Output_File, "<A HREF=""");
	        Ada.Text_IO.Put (Output_Object.Output_File,
	            Ada.Strings.Unbounded.To_String(Output_Object.Srch_URL));
	        Ada.Text_IO.Put (Output_Object.Output_File, """>Search</A>");
	    else -- Link to the section named "Search".
	        begin
		    -- Note: We do the following in one big glup so that if
		    -- Not_Found_Error is raised, nothing is output.
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, "&nbsp;&nbsp;&nbsp;<A HREF=""" &
			Make_Clause_Link_Name (Output_Object,
			    ARM_Contents.Lookup_Clause_Number ("Search" & (7 .. ARM_Contents.Title_Type'Last => ' '))) &
	                """>Search</A>");
	        exception
		    when ARM_Contents.Not_Found_Error =>
		        null; -- No section named "Search".
	        end;
	    end if;
	    if Ada.Strings.Unbounded.Length(Output_Object.Ref_URL) /= 0 then
	        Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;&nbsp;&nbsp;");
	        Ada.Text_IO.Put (Output_Object.Output_File, "<A HREF=""");
	        Ada.Text_IO.Put (Output_Object.Output_File,
	            Ada.Strings.Unbounded.To_String(Output_Object.Ref_URL));
	        Ada.Text_IO.Put (Output_Object.Output_File, """>Reference Documents</A>");
	    else -- Link to the section named "References".
	        begin
		    -- Note: We do the following in one big glup so that if
		    -- Not_Found_Error is raised, nothing is output.
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, "&nbsp;&nbsp;&nbsp;<A HREF=""" &
			Make_Clause_Link_Name (Output_Object,
			    ARM_Contents.Lookup_Clause_Number ("References" & (11 .. ARM_Contents.Title_Type'Last => ' '))) &
	                """>Reference Documents</A>");
	        exception
		    when ARM_Contents.Not_Found_Error =>
		        null; -- No section named "References".
	        end;
	    end if;
	    if Clause /= "" then
	        begin
		    -- Note: We do the following in one big glup so that if
		    -- Not_Found_Error is raised, nothing is output.
		    Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;&nbsp;&nbsp;<A HREF=""" &
		        Make_Clause_Link_Name (Output_Object,
			    ARM_Contents.Previous_Clause(Clause)));
		    Ada.Text_IO.Put (Output_Object.Output_File, """>Previous</A>");
	        exception
		    when ARM_Contents.Not_Found_Error =>
		        null; -- Probably the first section.
	        end;
	        begin
		    -- Note: We do the following in one big glup so that if
		    -- Not_Found_Error is raised, nothing is output.
		    Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;&nbsp;&nbsp;<A HREF=""" &
		        Make_Clause_Link_Name (Output_Object,
			    ARM_Contents.Next_Clause(Clause)));
		    Ada.Text_IO.Put (Output_Object.Output_File, """>Next</A>");
	        exception
		    when ARM_Contents.Not_Found_Error =>
		        null; -- Probably the last section.
	        end;
	    end if;

	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</P>");
        end if;
    end Make_Navigation_Bar;


    type Special_Style_Kinds is (None,
				 Hanging_Term,
				 Hanging_Body,
				 Bulleted_Item,
				 Bulleted_No_Prefix);

    procedure Make_Style (Output_Object : in out HTML_Output_Type;
			  Name : in String;
			  Style : in ARM_Output.Paragraph_Style_Type;
			  Indent : in ARM_Output.Paragraph_Indent_Type;
			  Kind : Special_Style_Kinds := None;
			  Enumerated_Adjustment : in Boolean := False) is
	-- Internal routine.
        -- Generate the style needed.

        function Units_to_EMs (Value : in Natural) return Natural is
	    -- Convert Value from indentation units to EMs. (0.1 EMs, really).
	    Normal : Boolean;
        begin
	    if Output_Object.HTML_Kind = HTML_4_Only then
	        case Paragraph_Info(Style, Indent).Font is
		    when ARM_Output.Default =>
			Normal := ARM_Output."=" (Output_Object.Body_Font, ARM_Output.Roman);
		    when ARM_Output.Roman =>
			Normal := True;
		    when ARM_Output.Fixed | ARM_Output.Swiss => -- Start at 90% (otherwise they are huge!)
			Normal := False;
		end case;
		if Normal then
		    case Paragraph_Info(Style, Indent).Size is
		        when 0 => return Value * 20;
		        when 1 => return Value * 16; -- 20/1.25.
		        when 2 => return Value * 13; -- 20/1.56.
		        when 3 => return Value * 10; -- 20/1.93.
		        when -1 => return Value * 25; -- 20/0.80.
		        when -2 => return Value * 31; -- 20/0.64.
		        when -3 => return Value * 40; -- 20/0.50.
		        when others => return Value; -- Out of range.
		    end case;
		else -- Start at 90% (otherwise they are huge!)
		    case Paragraph_Info(Style, Indent).Size is
		        when 0 => return Value * 22; -- 20/0.90
		        when 1 => return Value * 18; -- 20/1.13.
		        when 2 => return Value * 14; -- 20/1.40.
		        when 3 => return Value * 11; -- 20/1.75.
		        when -1 => return Value * 28; -- 20/0.72.
		        when -2 => return Value * 34; -- 20/0.58.
		        when -3 => return Value * 44; -- 20/0.45.
		        when others => return Value; -- Out of range.
		    end case;
	        end if;
	    elsif ARM_Output."=" (Paragraph_Info(Style, Indent).Font, ARM_Output.Fixed) then
	        -- Special case, see below.
	        case Paragraph_Info(Style, Indent).Size is
		    when 0 => return Value * 20;
		    when 1 => return Value * 16; -- 20/1.25.
		    when 2 => return Value * 13; -- 20/1.56.
		    when 3 => return Value * 10; -- 20/1.93.
		    when -1 => return Value * 25; -- 20/0.80.
		    when -2 => return Value * 31; -- 20/0.64.
		    when -3 => return Value * 40; -- 20/0.50.
		    when others => return Value; -- Out of range.
	        end case;
	    else
	        return Value * 20; -- No font sizes here.
	    end if;
        end Units_to_EMs;

    begin
	if not Paragraph_Used (Style, Indent) then
	    return; -- Not used, so don't generate.
	end if;
	if Kind = Hanging_Term and then Output_Object.HTML_Kind = HTML_4_Only then
	    -- Special case for better hanging.
            Ada.Text_IO.Put (Output_Object.Output_File, "    DIV.");
            Ada.Text_IO.Put (Output_Object.Output_File, Name & "-Term {");
	    if OPTIMIZE_FOR_FIREFOX then
		-- Tested on Firefox 1.5.
                Ada.Text_IO.Put (Output_Object.Output_File, "float: left; ");
		    -- This does not work on IE: it adds extra spaces, and leaves
		    -- it effective after a <BR>. We could probably work around
		    -- those, but then Firefox would look like crap again.
	    else
		Ada.Text_IO.Put (Output_Object.Output_File, "position: absolute; top: auto; left: 0.6em; ");
		    -- This does not work on Firefox: the text is too high by
		    -- about half a line and thus doesn't line up properly.
	    end if;
	elsif Kind = Hanging_Body and then Output_Object.HTML_Kind = HTML_4_Only then
            Ada.Text_IO.Put (Output_Object.Output_File, "    DIV.");
            Ada.Text_IO.Put (Output_Object.Output_File, Name & "-Body {");
	elsif (Kind = Bulleted_Item or else Kind = Bulleted_No_Prefix) and then
		   Output_Object.HTML_Kind = HTML_4_Only then
            Ada.Text_IO.Put (Output_Object.Output_File, "    DIV.");
            Ada.Text_IO.Put (Output_Object.Output_File, Name & " {");
	else
            case Paragraph_Info(Style, Indent).Tag is
	        when DIV =>
	            Ada.Text_IO.Put (Output_Object.Output_File, "    DIV.");
	        when UL =>
	            Ada.Text_IO.Put (Output_Object.Output_File, "    UL.");
	        when DL =>
	            Ada.Text_IO.Put (Output_Object.Output_File, "    DL.");
            end case;
            Ada.Text_IO.Put (Output_Object.Output_File, Name & " {");
	end if;
        case Paragraph_Info(Style, Indent).Font is
	    when ARM_Output.Default =>
		if ARM_Output."=" (Output_Object.Body_Font, ARM_Output.Roman) then
		    Ada.Text_IO.Put (Output_Object.Output_File, "font-family: ""Times New Roman"", Times, serif");
		else
		    Ada.Text_IO.Put (Output_Object.Output_File, "font-family: Arial, Helvetica, sans-serif");
		end if;
	    when ARM_Output.Roman => Ada.Text_IO.Put (Output_Object.Output_File, "font-family: ""Times New Roman"", Times, serif");
	    when ARM_Output.Swiss => Ada.Text_IO.Put (Output_Object.Output_File, "font-family: Arial, Helvetica, sans-serif");
	    when ARM_Output.Fixed => Ada.Text_IO.Put (Output_Object.Output_File, "font-family: ""Courier New"", monospace");
        end case;
        if Output_Object.HTML_Kind = HTML_4_Only then
	    -- The font size is set by the outer item.
	    declare
	        Normal : Boolean;
            begin
	        case Paragraph_Info(Style, Indent).Font is
		    when ARM_Output.Default =>
		        Normal := ARM_Output."=" (Output_Object.Body_Font, ARM_Output.Roman);
		    when ARM_Output.Roman =>
		        Normal := True;
		    when ARM_Output.Fixed | ARM_Output.Swiss => -- Start at 90% (otherwise they are huge!)
		        Normal := False;
	        end case;
	        if Normal then
		    case Paragraph_Info(Style, Indent).Size is
		        when 0 => null; -- Default.
		        when 1 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 125%");
		        when 2 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 156%");
		        when 3 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 195%");
		        when -1 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 80%");
		        when -2 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 64%");
		        when -3 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 50%");
		        when others => null; -- Out of range.
		    end case;
	        else -- Start at 90% (otherwise they are huge!)
		    -- Note: This size adjustment is for sections of text, not for in-line text.
		    case Paragraph_Info(Style, Indent).Size is
		        when 0 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 90%");
		        when 1 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 113%");
		        when 2 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 140%");
		        when 3 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 175%");
		        when -1 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 72%");
		        when -2 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 58%");
		        when -3 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 45%");
		        when others => null; -- Out of range.
		    end case;
	        end if;
            end;
	    -- Set the leading, because otherwise the lines are too close on IE.
	    Ada.Text_IO.Put (Output_Object.Output_File, "; line-height: 122%");
        elsif ARM_Output."=" (Paragraph_Info(Style, Indent).Font, ARM_Output.Fixed) then
	    -- Special case because the font otherwise gets too small and
	    -- loses bold-facing.
	    case Paragraph_Info(Style, Indent).Size is
	        when 0 => null; -- Default.
	        when 1 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 125%");
	        when 2 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 156%");
	        when 3 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 195%");
	        when -1 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 80%");
	        when -2 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 64%");
	        when -3 => Ada.Text_IO.Put (Output_Object.Output_File, "; font-size: 50%");
	        when others => null; -- Out of range.
	    end case;
        -- else the size will be set explicitly for HTML_4_Compatible.
        end if;
	if Kind = Hanging_Body then
	    if Output_Object.Number_Paragraphs then
	        Ada.Text_IO.Put (Output_Object.Output_File, "; margin-left: ");
	        Put_Ems (Output_Object.Output_File,
			 Units_to_EMs(Paragraph_Info(Style, Indent).Indent +
				      Paragraph_Info(Style, Indent).Hang_Outdent) +
			 INDENT_EMS_FOR_PARANUMS);
	    else
                if Paragraph_Info(Style, Indent).Indent + Paragraph_Info(Style, Indent).Hang_Outdent /= 0 then
	            Ada.Text_IO.Put (Output_Object.Output_File, "; margin-left: ");
	            Put_Ems (Output_Object.Output_File,
			     Units_to_EMs(Paragraph_Info(Style, Indent).Indent +
				          Paragraph_Info(Style, Indent).Hang_Outdent));
                end if;
	    end if;
	elsif Enumerated_Adjustment then
	    -- Adjust the left margin to indent the prefix slightly (1/4 of a unit):
	    declare
		Org_Margin : Natural :=
		    Units_to_EMs(Paragraph_Info(Style, Indent).Indent);
		Prefix_Indent : Natural :=
		   Units_to_EMs(1) / 4;
	    begin
	        Ada.Text_IO.Put (Output_Object.Output_File, "; margin-left: ");
	        if Output_Object.Number_Paragraphs then
	            Put_Ems (Output_Object.Output_File, Org_Margin + Prefix_Indent +
			     INDENT_EMS_FOR_PARANUMS);
	        else
	            Put_Ems (Output_Object.Output_File, Org_Margin + Prefix_Indent);
	        end if;
	    end;
	else
	    if Output_Object.Number_Paragraphs then
	        Ada.Text_IO.Put (Output_Object.Output_File, "; margin-left: ");
	        Put_Ems (Output_Object.Output_File, Units_to_EMs(Paragraph_Info(Style, Indent).Indent) +
			 INDENT_EMS_FOR_PARANUMS);
	    else
                if Paragraph_Info(Style, Indent).Indent /= 0 then
	            Ada.Text_IO.Put (Output_Object.Output_File, "; margin-left: ");
	            Put_Ems (Output_Object.Output_File, Units_to_EMs(Paragraph_Info(Style, Indent).Indent));
                end if;
	    end if;
	end if;
	if Kind = Hanging_Term and then Output_Object.HTML_Kind = HTML_4_Only then
	    -- We let the body provide the necessary right margin. If we don't
	    -- do this, the following item can end up with an inappropriate indent.
	    null;
	    --Ada.Text_IO.Put (Output_Object.Output_File, "; margin-bottom: 0em");
        elsif Paragraph_Info(Style, Indent).Right_Indent /= 0 then
	    Ada.Text_IO.Put (Output_Object.Output_File, "; margin-right: ");
	    Put_Ems (Output_Object.Output_File, Units_to_EMs(Paragraph_Info(Style, Indent).Right_Indent));
        end if;
        if Paragraph_Info(Style, Indent).Before /= 0 then
	    Ada.Text_IO.Put (Output_Object.Output_File, "; margin-top: ");
	    Put_Ems (Output_Object.Output_File, Paragraph_Info(Style, Indent).Before);
        elsif Paragraph_Info(Style, Indent).Tag /= DIV then
	    -- The default is non-zero.
	    Ada.Text_IO.Put (Output_Object.Output_File, "; margin-top: 0em");
        end if;
	if Kind = Hanging_Term and then Output_Object.HTML_Kind = HTML_4_Only then
	    -- We let the body provide the necessary space below. If we don't
	    -- do this, the next line can end up with an inappropriate indent.
	    Ada.Text_IO.Put (Output_Object.Output_File, "; margin-bottom: 0em");
	elsif Paragraph_Info(Style, Indent).After /= 0 then
	    Ada.Text_IO.Put (Output_Object.Output_File, "; margin-bottom: ");
	    Put_Ems (Output_Object.Output_File, Paragraph_Info(Style, Indent).After);
        end if;
	if Kind = Bulleted_Item and then Output_Object.HTML_Kind = HTML_4_Only then
	    -- Set the list item and "disc" format:
	    Ada.Text_IO.Put (Output_Object.Output_File, "; display: list-item; list-style-type: disc");
        end if;
        -- Done, close it.
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "}");
    end Make_Style;


    procedure Make_Hung_Text_Style
		     (Output_Object : in out HTML_Output_Type;
		      Name   : in String;
		      Style  : in ARM_Output.Paragraph_Style_Type;
		      Indent : in ARM_Output.Paragraph_Indent_Type) is
	-- Internal routine.
        -- Generate the style needed.
    begin
	if Output_Object.HTML_Kind = HTML_4_Only then
	    if ARM_Output."=" (Style, ARM_Output.Enumerated) or else
	       ARM_Output."=" (Style, ARM_Output.Small_Enumerated) then
		Make_Style (Output_Object, Name, Style, Indent,
		            Kind => Hanging_Term,
			    Enumerated_Adjustment => True);
	    else
		Make_Style (Output_Object, Name, Style, Indent,
		            Kind => Hanging_Term);
	    end if;
	else -- HTML_4_Compatible
            Ada.Text_IO.Put (Output_Object.Output_File, "    DD." & Name & " {");
            Ada.Text_IO.Put (Output_Object.Output_File, "margin-left: ");
            case Paragraph_Info(Style, Indent).Size is
	        when 0 => Put_Ems (Output_Object.Output_File, Paragraph_Info(Style, Indent).Hang_Outdent * 20);
	        when 1 => Put_Ems (Output_Object.Output_File, Paragraph_Info(Style, Indent).Hang_Outdent * 16); -- 20/1.25.
	        when 2 => Put_Ems (Output_Object.Output_File, Paragraph_Info(Style, Indent).Hang_Outdent * 13); -- 20/1.56.
	        when -1 => Put_Ems (Output_Object.Output_File, Paragraph_Info(Style, Indent).Hang_Outdent * 25); -- 20/0.80.
	        when -2 => Put_Ems (Output_Object.Output_File, Paragraph_Info(Style, Indent).Hang_Outdent * 31); -- 20/0.64.
	        when -3 => Put_Ems (Output_Object.Output_File, Paragraph_Info(Style, Indent).Hang_Outdent * 40); -- 20/0.50.
	        when others => null; -- Out of range.
            end case;
            -- Done, close it.
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "}");
	end if;
    end Make_Hung_Text_Style;


    function Paragraph_Name
		     (Style  : in ARM_Output.Paragraph_Style_Type;
		      Indent : in ARM_Output.Paragraph_Indent_Type) return String is
	-- Internal routine.
	-- Create the name for the Style and Indent.
	-- These had better be unique, and all possibilities (including impossible
	-- ones) had better have names.
	use type ARM_Output.Paragraph_Indent_Type;
    begin
	case Style is
	    when ARM_Output.Normal =>
                if Indent = 0 then
		    return "Normal";
		else
		    -- This was: Indent 1: "SyntaxIndented"; Indent 2:
		    -- "CodeIndented"; Indent 3: "Indented"; Indent 4: "InnerIndented".
		    return "Indented" & Character'Val(Character'Pos('0') + Indent);
		end if;
	    when ARM_Output.Wide_Above =>
                if Indent = 0 then
		    return "WideAbove";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "WideAbove";
		end if;
	    when ARM_Output.Small =>
                if Indent = 0 then
		    return "Small";
                elsif Indent = 1 then
		    return "Notes";
                elsif Indent = 2 then
		    return "Annotations";
		else
		    -- This was: Indent 3: "SmallSyntaxIndented"; Indent 4:
		    -- "SmallCodeIndented"; Indent 5: "SmallIndented"; Indent 6: "SmallInnerIndented".
		    return "SmallIndented" & Character'Val(Character'Pos('0') + Indent);
		end if;
	    when ARM_Output.Small_Wide_Above =>
                if Indent = 0 then
		    return "SmallWideAbove";
                elsif Indent = 2 then
		    return "AnnotationsWideAbove";
		else
		    return "SmallIndented" & Character'Val(Character'Pos('0') + Indent) & "WideAbove";
		end if;
	    when ARM_Output.Header =>
                if Indent = 0 then
		    return "Header";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "Header";
		end if;
	    when ARM_Output.Small_Header =>
                if Indent = 0 then
		    return "SmallHeader";
                elsif Indent = 1 then
		    return "NotesHeader";
		else
		    return "SmallIndented" & Character'Val(Character'Pos('0') + Indent) & "Header";
		end if;
	    when ARM_Output.Index =>
                if Indent = 0 then
		    return "Index";
	    	else -- Should be not used.
		    return "IndexIndented" & Character'Val(Character'Pos('0') + Indent);
		end if;
	    when ARM_Output.Syntax_Summary =>
                if Indent = 1 then
		    return "SyntaxSummary";
	    	else -- Should be not used.
		    return "SynSumInd" & Character'Val(Character'Pos('0') + Indent);
		end if;
	    when ARM_Output.Title =>
                if Indent = 0 then
		    return "Title";
	    	else -- Should be not used.
		    return "TitleIndented" & Character'Val(Character'Pos('0') + Indent);
		end if;
	    when ARM_Output.Examples =>
                if Indent = 1 then
		    return "Examples";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "Examples";
		end if;
	    when ARM_Output.Small_Examples =>
                if Indent = 3 then
		    return "SmallExamples";
		else
		    return "SmallIndented" & Character'Val(Character'Pos('0') + Indent) & "Examples";
		end if;
	    when ARM_Output.Swiss_Examples =>
                if Indent = 1 then
		    return "SwissExamples";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "SwissExamples";
		end if;
	    when ARM_Output.Small_Swiss_Examples =>
                if Indent = 3 then
		    return "SmallSwissExamples";
		else
		    return "SmallIndented" & Character'Val(Character'Pos('0') + Indent) & "SwissExamples";
		end if;
	    when ARM_Output.Bulleted =>
                if Indent = 1 then
		    return "Bulleted";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "Bulleted";
		end if;
	    when ARM_Output.Small_Bulleted =>
                if Indent = 3 then
		    return "SmallBulleted";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "SmallBulleted";
		end if;
	    when ARM_Output.Nested_Bulleted =>
                if Indent = 1 then
		    return "NestedBulleted";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "NestedBulleted";
		end if;
	    when ARM_Output.Small_Nested_Bulleted =>
                if Indent = 3 then
		    return "SmallNestedBulleted";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "SmallNestedBulleted";
		end if;
	    when ARM_Output.Enumerated =>
                if Indent = 1 then
		    return "Enumerated";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "Enumerated";
		end if;
	    when ARM_Output.Small_Enumerated =>
                if Indent = 3 then
		    return "SmallEnumerated";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "SmallEnumerated";
		end if;
	    when ARM_Output.Giant_Hanging =>
                if Indent = 4 then
		    return "GiantHanging";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "GiantHanging";
		end if;
	    when ARM_Output.Small_Giant_Hanging =>
                if Indent = 6 then
		    return "SmallGiantHanging";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "SmallGiantHanging";
		end if;
	    when ARM_Output.Wide_Hanging =>
                if Indent = 3 then
		    return "WideHanging";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "WideHanging";
		end if;
	    when ARM_Output.Small_Wide_Hanging =>
                if Indent = 5 then
		    return "SmallWideHanging";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "SmallWideHanging";
		end if;
	    when ARM_Output.Medium_Hanging =>
                if Indent = 2 then
		    return "MediumHanging";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "MediumHanging";
		end if;
	    when ARM_Output.Small_Medium_Hanging =>
                if Indent = 4 then
		    return "SmallMediumHanging";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "SmallMediumHanging";
		end if;
	    when ARM_Output.Narrow_Hanging =>
                if Indent = 1 then
		    return "NarrowHanging";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "NarrowHanging";
		end if;
	    when ARM_Output.Small_Narrow_Hanging =>
                if Indent = 3 then
		    return "SmallNarrowHanging";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "SmallNarrowHanging";
		end if;
	    when ARM_Output.Hanging_in_Bulleted =>
                if Indent = 3 then
		    return "Hanging_in_Bulleted";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "Hanging_in_Bulleted";
		end if;
	    when ARM_Output.Small_Hanging_in_Bulleted =>
                if Indent = 5 then
		    return "SmallHanging_in_Bulleted";
		else
		    return "Indented" & Character'Val(Character'Pos('0') + Indent) & "SmallHanging_in_Bulleted";
		end if;
	end case;
    end Paragraph_Name;


    procedure Make_Paragraph_Styles
		     (Output_Object : in out HTML_Output_Type) is
	-- Internal routine.
	-- Generate all of the paragraph and related styles.
    begin
	-- Basic element styles:
	if Paranum_Used then
	    if Output_Object.HTML_Kind = HTML_4_Compatible then
		if OPTIMIZE_FOR_FIREFOX then
		    Ada.Text_IO.Put_Line (Output_Object.Output_File, "    DIV.paranum {float: left; font-family: Arial, Helvetica, sans-serif; width: 2.8em; " &
								     "margin-left: -0.4em; margin-right: -3.0em; margin-top: 0.2em}");
                    -- Uses floating items. These usually don't work on IE (it
		    -- adds extra spaces for no reason). However, with the
		    -- indents, this seems to work properly on IE, too.
		else
		    -- Absolute positioning (CSS2) works better on IE, but
		    -- Firefox tends to draw these too high.
		    Ada.Text_IO.Put_Line (Output_Object.Output_File, "    DIV.paranum {position: absolute; font-family: Arial, Helvetica, sans-serif; left: 0.5em; top: auto; margin-top: 0.2em}");
		end if;
		-- If these are completely ignored, the paragraph number will
		-- end up on a line by itself. That's fine.
	    else
		if OPTIMIZE_FOR_FIREFOX then
		    Ada.Text_IO.Put_Line (Output_Object.Output_File, "    DIV.paranum {float: left; font-family: Arial, Helvetica, sans-serif; font-size: 64%; width: 2.8em; " &
								     "margin-left: -0.4em; margin-right: -3.0em; margin-top: 0.2em}");
		    -- Uses floating elements; see above.
		else
		    Ada.Text_IO.Put_Line (Output_Object.Output_File, "    DIV.paranum {position: absolute; font-family: Arial, Helvetica, sans-serif; font-size: 64%; " &
								     "left: 0.5em; top: auto; margin-top: 0.2em}");
		    -- Uses absolute positioning; see above.
		end if;
	    end if;
	end if;

        Ada.Text_IO.Put_Line (Output_Object.Output_File, "    TT {font-family: ""Courier New"", monospace}");
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "    DT {display: compact}"); -- CSS2. This doesn't seem to work on IE 4.01, but it is harmless.

        -- Revision styles:
	if Revision_Used ('0') then
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.insert0 {text-decoration: underline; color: black}");
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.delete0 {text-decoration: line-through; color: black }");
            --Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.both0 {text-decoration: underline, line-through; color: black }");
		-- Both doesn't seem to work, so forget it.
	-- else not used, don't generate it.
	end if;
	if Revision_Used ('1') then
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.insert1 {text-decoration: underline; color: rgb(0,51,51) }"); -- (Very) dark turquoise.
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.delete1 {text-decoration: line-through; color: rgb(0,51,51) }");
            --Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.both1 {text-decoration: underline, line-through; color: rgb(0,51,51) }");
		-- Both doesn't seem to work, so forget it.
	-- else not used, don't generate it.
	end if;
	if Revision_Used ('2') then
	    if Revision_Used ('5') or else Output_Object.Force_New_Revision_Colors then
		-- Ugly color here, nice color for #5
                Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.insert2 {text-decoration: underline; color: rgb(102,0,153) }"); -- Violet.
                Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.delete2 {text-decoration: line-through; color: rgb(102,0,153) }");
                --Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.both2 {text-decoration: underline, line-through; color: rgb(102,0,153) }");
		    -- Both doesn't seem to work, so forget it.
	    else -- Use the nice green here.
                Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.insert2 {text-decoration: underline; color: rgb(0,102,0) }"); -- Dark green.
                Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.delete2 {text-decoration: line-through; color: rgb(0,102,0) }");
                --Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.both2 {text-decoration: underline, line-through; color: rgb(0,102,0) }");
		    -- Both doesn't seem to work, so forget it.
	    end if;
	-- else not used, don't generate it.
	end if;
	if Revision_Used ('3') then
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.insert3 {text-decoration: underline; color: rgb(102,51,0) }"); -- Dark brown.
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.delete3 {text-decoration: line-through; color: rgb(102,51,0) }");
            --Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.both3 {text-decoration: underline, line-through; color: rgb(102,51,0) }");
		-- Both doesn't seem to work, so forget it.
	-- else not used, don't generate it.
	end if;
	if Revision_Used ('4') then
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.insert4 {text-decoration: underline; color: rgb(153,0,0) }"); -- Dark red.
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.delete4 {text-decoration: line-through; color: rgb(153,0,0) }");
            --Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.both4 {text-decoration: underline, line-through; color: rgb(153,0,0) }");
		-- Both doesn't seem to work, so forget it.
	-- else not used, don't generate it.
	end if;
	if Revision_Used ('5') then
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.insert5 {text-decoration: underline; color: rgb(0,102,0) }"); -- Dark green.
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.delete5 {text-decoration: line-through; color: rgb(0,102,0) }");
            --Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.both5 {text-decoration: underline, line-through; color: rgb(0,102,0) }");
		-- Both doesn't seem to work, so forget it.
	-- else not used, don't generate it.
	end if;
	if Revision_Used ('6') then
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.insert6 {text-decoration: underline; color: rgb(0,102,153) }"); -- Turquiose.
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.delete6 {text-decoration: line-through; color: rgb(0,102,153) }");
            --Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.both6 {text-decoration: underline, line-through; color: rgb(0,102,153) }");
		-- Both doesn't seem to work, so forget it.
	-- else not used, don't generate it.
	end if;

        -- Link styles:
	-- We don't need these (they're given in the BODY command), but I've
	-- kept them in case we want to change these in the future.
        --Ada.Text_IO.Put_Line (Output_Object.Output_File, "    A:link {color: rgb(0,0,255)}");
        --Ada.Text_IO.Put_Line (Output_Object.Output_File, "    A:visited {color: rgb(128,0,128)}");
	-- The following styles are for the Ada-Auth.Org header. It would be nice
	-- if we could skip them if they're not needed, but we don't have the
	-- information needed to do that.
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "    A.Bar:link {font-family: Arial, Helvetica, sans-serif; font-style: normal; text-decoration: none; color: rgb(204,204,51)}");
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "    A.Bar:visited {font-family: Arial, Helvetica, sans-serif; font-style: normal; text-decoration: none; color: rgb(204,204,51)}");

	-- Paragraph styles:
	for S in ARM_Output.Unprefixed_Style_Subtype loop
	    for I in ARM_Output.Paragraph_Indent_Type loop
	        Make_Style (Output_Object, Paragraph_Name (S, I), S, I);
	    end loop;
	end loop;

	for S in ARM_Output.Bullet_Prefixed_Style_Subtype loop
	    -- These styles do not allow Indent 0.
	    for I in 1 .. ARM_Output.Paragraph_Indent_Type'Last loop
		if Output_Object.HTML_Kind = HTML_4_Only then
		    Make_Style (Output_Object, Paragraph_Name (S, I) & "-NoPrefix", S, I,
			        Kind => Bulleted_No_Prefix);
		    Make_Style (Output_Object, Paragraph_Name (S, I), S, I,
			        Kind => Bulleted_Item);
		else
	            Make_Style (Output_Object, Paragraph_Name (S, I), S, I);
		end if;
	    end loop;
	end loop;

	for S in ARM_Output.Text_Prefixed_Style_Subtype loop
	    -- These styles do not allow Indent 0.
	    for I in 1 .. ARM_Output.Paragraph_Indent_Type'Last loop
		if Paragraph_Used (S, I) then
	            Make_Style (Output_Object, Paragraph_Name (S, I), S, I, Kind => Hanging_Body);
	            Make_Hung_Text_Style (Output_Object, Paragraph_Name (S, I), S, I);
		-- else not used.
		end if;
	    end loop;
	end loop;

    end Make_Paragraph_Styles;


    MAGIC_STYLE_MARKER : constant String := "&%$# STYLES GO HERE #$%&";


    procedure Start_HTML_File (Output_Object : in out HTML_Output_Type;
			       File_Name : in String;
			       Title : in String;
			       Clause : in String) is
	-- Internal routine.
	-- Create an HTML file, and generate the needed text to start an HTML
	-- file. The file name is just the name portion, not the path or
	-- extension. Clause is the properly formatted Clause number for
	-- this file, if known.
    begin
--Ada.Text_IO.Put_Line ("--Creating " & File_Name & ".html");
	if Output_Object.HTML_Kind > HTML_3 then
	    Ada.Text_IO.Create (Output_Object.Output_File, Ada.Text_IO.Out_File,
	        Ada.Strings.Unbounded.To_String (Output_Object.Output_Path) &
		    File_Name & ".$$$");
	elsif Output_Object.DOS_Filenames then
	    Ada.Text_IO.Create (Output_Object.Output_File, Ada.Text_IO.Out_File,
	        Ada.Strings.Unbounded.To_String (Output_Object.Output_Path) &
	            File_Name & ".HTM");
	else
	    Ada.Text_IO.Create (Output_Object.Output_File, Ada.Text_IO.Out_File,
	        Ada.Strings.Unbounded.To_String (Output_Object.Output_Path) &
	            File_Name & ".html");
	end if;
	-- Save the current clause:
	Output_Object.Current_Clause :=
	    Ada.Strings.Unbounded.To_Unbounded_String(Clause);
	-- File introduction:
	if Output_Object.HTML_Kind > HTML_3 then
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "<!DOCTYPE HTML PUBLIC ""-//W3C//DTD HTML 4.01 Transitional//EN""");
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, """http://www.w3.org/TR/html4/loose.dtd"">"); -- HTML 4.01 (with depreciated features)
	else
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "<!DOCTYPE HTML PUBLIC ""-//W3C//DTD HTML 3.2//EN"">"); -- HTML 3.2
	end if;
												       -- so the result can be used on version 3 browsers.)
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "<HTML>");
	-- Header information:
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "<HEAD>");
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "    <TITLE>" & Title & "</TITLE>");
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "    <META http-equiv=""Content-Type"" content=""text/html; charset=iso-8859-1"">");
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "    <META NAME=""Author"" CONTENT=""JTC1/SC22/WG9/ARG, by Randall Brukardt, ARG Editor"">");
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "    <META NAME=""GENERATOR"" CONTENT=""Arm_Form.Exe, Ada Reference Manual generator"">");
	if Output_Object.HTML_Kind = HTML_4_Only then
	     -- The style sheet.
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "    <STYLE type=""text/css"">");
	    -- Element styles:
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "    H4.centered {text-align: center}");
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.swiss {font-family: Arial, Helvetica, sans-serif; font-size: 92%}");
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "    SPAN.roman {font-family: ""Times New Roman"", Times, serif}");

	    -- Paragraph styles:
	    --Paragraph_Used := (others => (others => True)); -- Force showing all, we don't know what is used.
	    --Revision_Used := (others => True);
	    --Paranum_Used := True;
	    --Make_Paragraph_Styles (Output_Object);
	    -- Dummy line to be replaced after the file is created.
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, MAGIC_STYLE_MARKER);

	    Paragraph_Used := (others => (others => False));
	    Revision_Used := (others => False);
	    Paranum_Used := False;

	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "    </STYLE>");
	elsif Output_Object.HTML_Kind = HTML_4_Compatible then
	     -- The style sheet.
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "    <STYLE type=""text/css"">");

	    -- Paragraph styles:
	    --Paragraph_Used := (others => (others => True)); -- Force showing all, we don't know what is used.
	    --Revision_Used := (others => True);
	    --Paranum_Used := True;
	    --Make_Paragraph_Styles (Output_Object);
	    -- Dummy line to be replaced after the file is created.
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, MAGIC_STYLE_MARKER);

	    Paragraph_Used := (others => (others => False));
	    Revision_Used := (others => False);
	    Paranum_Used := False;

	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "    </STYLE>");
	end if;

 	if Ada.Strings.Unbounded.Length(Output_Object.Script_HTML) /= 0 then
	    Ada.Text_IO.Put_Line (Output_Object.Output_File,
		Ada.Strings.Unbounded.To_String(Output_Object.Script_HTML));
	end if;

	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</HEAD>");
        Ada.Text_IO.Put_Line (Output_Object.Output_File,
	    "<BODY TEXT=""" & Output_Object.Text_Color &
	    """ BGCOLOR=""" & Output_Object.Background_Color &
	    """ LINK=""" & Output_Object.Link_Color &
	    """ VLINK=""" & Output_Object.VLink_Color &
	    """ ALINK=""" & Output_Object.ALink_Color & """>");

 	if Ada.Strings.Unbounded.Length(Output_Object.Header_HTML) /= 0 then
	    Ada.Text_IO.Put_Line (Output_Object.Output_File,
		Ada.Strings.Unbounded.To_String(Output_Object.Header_HTML));
	end if;

	if Output_Object.Nav_on_Top then
	    Make_Navigation_Bar (Output_Object, Is_Top => True);
	-- else no navigation bar
	end if;

	if Output_Object.Nav_on_Top or else
	   Ada.Strings.Unbounded.Length(Output_Object.Header_HTML) /= 0 then
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "<HR>"); -- Horizontal line (rule).
	-- else nothing on top at all.
	end if;
    end Start_HTML_File;


    procedure End_HTML_File (Output_Object : in out HTML_Output_Type) is
	-- Internal routine.
	-- Generate the needed text to end an HTML file. Also closes the file.
    begin
	Ada.Text_IO.New_Line (Output_Object.Output_File); -- Blank line to set off paragraphs.

	if Output_Object.Nav_on_Bottom or else
	   Ada.Strings.Unbounded.Length(Output_Object.Footer_HTML) /= 0 then
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "<HR>"); -- Horizontal line (rule).
	-- else nothing on top at all.
	end if;

	if Output_Object.Nav_on_Bottom then
	    Make_Navigation_Bar (Output_Object, Is_Top => False);
	-- else no navigation bar.
	end if;

 	if Ada.Strings.Unbounded.Length(Output_Object.Footer_HTML) /= 0 then
	    Ada.Text_IO.Put_Line (Output_Object.Output_File,
		Ada.Strings.Unbounded.To_String(Output_Object.Footer_HTML));
	end if;

	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</BODY>");
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</HTML>");
	if Output_Object.HTML_Kind <= HTML_3 then
	    Ada.Text_IO.Close (Output_Object.Output_File);
	else -- Close and reread the file to add JUST the styles used by the
	     -- file; this decreases the minimum size of the files (by as
	     -- much as 7K as of this writing [1/2006]), which matters when
	     -- there are hundreds.
	     -- We also check spaces before end tags and after opening tags;
	     -- these should be &nbsp;. (See 9.1 in HTML 4.0: "In order to
	     -- avoid problems with SGML line break rules and inconsistencies
	     -- among extant implementations, authors should not rely on user
	     -- agents to render white space immediately after a start tag or
	     -- immediately before an end tag.") We haven't seen a problem
	     -- with this, but why ask for trouble?
	     -- Note that we assume that all occurrences of "<" and ">" in
	     -- the literal text are written as "&lt;" and "&gt;"; violations
	     -- might cause the conversion of spaces to non-breaking spaces,
	     -- which should not cause problems in general. We also assume that
	     -- all end tags are on one line (they are all very short), so
	     -- any ">" is the end of a start tag unless there is a "</" preceding it.
	    declare
		Original_Name : constant String := Ada.Text_IO.Name (Output_Object.Output_File);
		Reading_File : Ada.Text_IO.File_Type;
		Real_Name : constant String :=
		    Ada.Strings.Fixed.Head (Original_Name, Original_Name'Length-3);
		Buffer : String (1..1600);
		Len : Natural;
		Body_Seen : Boolean := False;
		Loc : Natural;
	    begin
		Ada.Text_IO.Close (Output_Object.Output_File);
	        Ada.Text_IO.Open (Reading_File, Ada.Text_IO.In_File,
	            Original_Name);
		if Output_Object.DOS_Filenames then
	            Ada.Text_IO.Create (Output_Object.Output_File, Ada.Text_IO.Out_File,
	                Real_Name & "HTM");
		else
	            Ada.Text_IO.Create (Output_Object.Output_File, Ada.Text_IO.Out_File,
	                Real_Name & "html");
		end if;
		begin
		    loop
			Ada.Text_IO.Get_Line (Reading_File, Buffer, Len);
			if Buffer(1..Len) = MAGIC_STYLE_MARKER then
			    -- Output only the styles used here.
			    Make_Paragraph_Styles (Output_Object);
			elsif not Body_Seen then
			    if Ada.Strings.Fixed.Index (Buffer(1..Len), "<BODY") /= 0 then
				Body_Seen := True;
			    end if;
			    Ada.Text_IO.Put_Line (Output_Object.Output_File, Buffer(1..Len));
			else
			    -- Replace spaces before end tags:
			    loop
				Loc := Ada.Strings.Fixed.Index (Buffer(1..Len), " </");
				exit when Loc = 0;
			        Buffer(Loc+6..Len+5) := Buffer(Loc+1..Len);
				Buffer(Loc..Loc+5) := "&nbsp;";
				Len := Len+5;
			    end loop;
			    -- Replace spaces after start tags:
			    Loc := 1;
			    while Loc < Len loop
				if Buffer(Loc..Loc+1) = "> " then
				    -- Candidate; check that this isn't an end tag.
				    for I in reverse 1..Loc-1 loop
					if Buffer(I) = '/' then
					    -- End tag, nothing to do.
					    Loc := Loc + 2;
					    exit;
					elsif Buffer(I) = '<' or else I = 1 then
					    -- Start tag (including reaching the
					    -- start of the line), replace.
				            Buffer(Loc+7..Len+5) := Buffer(Loc+2..Len);
					    Buffer(Loc+1..Loc+6) := "&nbsp;";
					    Len := Len+5;
					    Loc := Loc + 7;
					    -- If these is the *last* character on the
					    -- line, we have to "unbreak" the line, else we'd get an extra space.
					    if Loc > Len then
						Ada.Text_IO.Put (Output_Object.Output_File, Buffer(1..Len));
						goto Skip_Write;
					    end if;
					    exit;
					-- else continue.
					end if;
				    end loop;
				else
				    Loc := Loc + 1;
				end if;
			    end loop;

			    Ada.Text_IO.Put_Line (Output_Object.Output_File, Buffer(1..Len));
			<<Skip_Write>> null;
			end if;
		    end loop;
		exception
		    when Ada.Text_IO.End_Error => null; -- Done copying.
		end;
		Ada.Text_IO.Close (Output_Object.Output_File);
		Ada.Text_IO.Delete (Reading_File); -- This was temporary.
	    end;
	end if;
    end End_HTML_File;


    procedure Create (Output_Object : in out HTML_Output_Type;
		      Big_Files : in Boolean;
		      File_Prefix : in String;
		      Output_Path : in String;
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
	              Script_HTML : String;
	              Header_HTML : String;
	              Footer_HTML : String;
		      Title : in String := "";
		      Body_Font : ARM_Output.Font_Family_Type;
		      Force_New_Revision_Colors : Boolean;
		      Text_Color : Color_String;
		      Background_Color : Color_String;
		      Link_Color : Color_String;
		      VLink_Color : Color_String;
		      ALink_Color : Color_String) is
	-- Create an Output_Object for a document.
	-- Generate a few large output files if
	-- Big_Files is True; otherwise generate smaller output files.
	-- The prefix of the output file names is File_Prefix - this
	-- should be no more than 5 characters allowed in file names.
	-- The files will be written into Output_Path.
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
	-- Script_HTML gives self-contained HTML that will appear immediately
	-- before the </HEAD> of every page. This usually will contain
	-- Javascript scripts or CSS styles. The original intent was to allow
	-- adding the Google Analytics script to each page.
	-- Header_HTML gives self-contained HTML that will appear before the
	-- navigation bar in the header. Footer_HTML gives self-contained HTML
	-- that will appear after the navigation bar in the footer.
	-- Body_Font selects the default font for the document body.
	-- Text_Color specifies the default text color; Background_Color
	-- specifies the default background color; Link_Color specifies the
	-- default color of normal links; VLink_Color specifies the
	-- default color of visited links; and ALink_Color specifies the
	-- default color of active (in the act of clinking) links.
    begin
	if Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Already valid object");
	end if;
	Output_Object.Is_Valid := True;
	Ada.Strings.Fixed.Move (Target => Output_Object.File_Prefix,
			        Source => File_Prefix);
	Output_Object.Title := Ada.Strings.Unbounded.To_Unbounded_String (Title);
	Output_Object.Big_Files := Big_Files;
	Output_Object.Output_Path := Ada.Strings.Unbounded.To_Unbounded_String(Output_Path);
	Output_Object.DOS_Filenames := DOS_Filenames;
	Output_Object.HTML_Kind := HTML_Kind;
	Output_Object.Use_Unicode := Use_Unicode;
	Output_Object.Number_Paragraphs := Number_Paragraphs;
	Output_Object.Ref_URL := Ada.Strings.Unbounded.To_Unbounded_String(Ref_URL);
	Output_Object.Srch_URL := Ada.Strings.Unbounded.To_Unbounded_String(Srch_URL);
	Output_Object.Index_URL := Ada.Strings.Unbounded.To_Unbounded_String(Index_URL);
	Output_Object.Use_Buttons := Use_Buttons;
	Output_Object.Nav_on_Top := Nav_on_Top;
	Output_Object.Nav_on_Bottom := Nav_on_Bottom;
	Output_Object.Tab_Emulation := Tab_Emulation;
	Output_Object.Script_HTML := Ada.Strings.Unbounded.To_Unbounded_String(Script_HTML);
	Output_Object.Header_HTML := Ada.Strings.Unbounded.To_Unbounded_String(Header_HTML);
	Output_Object.Footer_HTML := Ada.Strings.Unbounded.To_Unbounded_String(Footer_HTML);
	Output_Object.Body_Font := Body_Font;
	Output_Object.Force_New_Revision_Colors := Force_New_Revision_Colors;
	Output_Object.Text_Color := Text_Color;
	Output_Object.Background_Color := Background_Color;
	Output_Object.Link_Color := Link_Color;
	Output_Object.VLink_Color := VLink_Color;
	Output_Object.ALink_Color := ALink_Color;

	if DOS_Filenames then
	    if File_Prefix'Length > 3 then
	        Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		    "HTML File Prefix too long - MS-DOS mode");
	    end if;
	else
	    if File_Prefix'Length > 5 then
	        Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		    "HTML File Prefix too long");
	    end if;
	end if;

	if Output_Object.Big_Files then
	    Start_HTML_File (Output_Object,
			     Ada.Strings.Fixed.Trim (Output_Object.File_Prefix, Ada.Strings.Right),
			     Ada.Strings.Unbounded.To_String (Output_Object.Title),
			     Clause => "");
	    -- Insert an anchor for the title page:
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "<A NAME=""TTL""></A>");
	end if;
    end Create;


    procedure Close (Output_Object : in out HTML_Output_Type) is
	-- Close an Output_Object. No further output to the object is
	-- allowed after this call.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Ada.Text_IO.Is_Open (Output_Object.Output_File) then
	    End_HTML_File (Output_Object);
	end if;
	Output_Object.Is_Valid := False;
    end Close;


    procedure Section (Output_Object : in out HTML_Output_Type;
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
	-- We don't generate a file here for HTML. We generate a file for each
	-- clause.
	if Section_Name'Length > 3 then
	    Output_Object.Section_Name := Section_Name (Section_Name'First .. Section_Name'First + 2);
	else
	    Output_Object.Section_Name := (others => '-');
	    Output_Object.Section_Name (1 .. Section_Name'Length) := Section_Name;
	end if;
    end Section;


    procedure Set_Columns (Output_Object : in out HTML_Output_Type;
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
	if Number_of_Columns >= 4 then
	    -- We have special processing for 4 or more columns. Note that we
	    -- assume do not contain any nested paragraph formats.
	    Output_Object.Current_Column := 1;
	    Output_Object.Current_Item := 1;
	elsif Output_Object.Column_Count >= 4 and then Number_of_Columns = 1 then
	    -- Finished processing columns, output the columns as a table.
	    if Output_Object.Current_Column = 1 then
		-- No New_Column calls; break this into columns ourselves.
		-- Current_Item represents the total number of items + 2
		-- (there is a double count for the end of the paragraph).
		declare
		    Col_Count : Natural := Output_Object.Current_Item - 2;
		    Where : Column_Text_Ptr;
		begin
		    Col_Count := (Col_Count + Output_Object.Column_Count - 1) /
			Output_Object.Column_Count;
Ada.Text_IO.Put_Line("  @@ Calc columns for" & Natural'Image(Output_Object.Column_Count) &
  " columns;" & Natural'Image(Output_Object.Current_Item - 1) & " total items;" &
  Natural'Image(Col_Count) & " per column.");

		    -- Split the item list into the appropriate columns.
		    -- Note that this list is stored in reverse order.
		    for I in reverse 2 .. Output_Object.Column_Count loop
--Ada.Text_IO.Put_Line("  @@   For column" & Natural'Image(I) & " split at item" &
--Natural'Image(Col_Count*(I-1)));
			Where := Output_Object.Column_Text(1);
			exit when Where = null; -- No columns to look at.
			if Where.Item <= Col_Count*(I-1) then
			    -- This column is empty.
			    null;
			else
			    Output_Object.Column_Text(I) := Where;
			    while Where.Next /= null and then
			        Where.Next.Item > Col_Count*(I-1) loop
				Where.Item := Where.Item - Col_Count*(I-1);
			        Where := Where.Next;
			    end loop;
			    if Where = null then
			        Output_Object.Column_Text(1) := null;
			    else
			        Output_Object.Column_Text(1) := Where.Next;
				Where.Next := null;
--Ada.Text_IO.Put_Line("    split item=" & Natural'Image(Where.Item));
				Where.Item := Where.Item - Col_Count*(I-1);
			    end if;
			end if;
		    end loop;
		end;
	    --else there were explicit New_Column calls; no need to
	    --do anything here.
	    end if;
	    if Output_Object.HTML_Kind = HTML_3 then
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "<UL><UL><TABLE Width=""70%"">"); -- Table with no border or caption, takes up 70% of the screen.
	    else
	        Ada.Text_IO.Put_Line (Output_Object.Output_File,
		    "<div class=""" & Paragraph_Name (ARM_Output.Normal, 2) & """><table width=""70%"">"); -- Table with no border or caption, takes up 70% of the screen.
		Paragraph_Used(ARM_Output.Normal, 2) := True;
	    end if;
	    -- And start the first row:
	    Ada.Text_IO.Put (Output_Object.Output_File, "<TR><TD align=""left"">");
	    for Item in 1 .. 5000 loop
		for Col in 1 .. Output_Object.Column_Count loop
		    declare
		        Find : Column_Text_Ptr := Output_Object.Column_Text(Col);
			Temp : Column_Text_Ptr;
		    begin
			-- We're going to free this item after outputting it.
			if Find = null then
			    null;
			elsif Find.Next = null then
			    if Find.Item /= Item then
				Find := null;
			    else
			        Output_Object.Column_Text(Col) := null;
			    end if;
			else
			    while Find.Next /= null and then Find.Next.Item /= Item loop
			        Find := Find.Next;
			    end loop;
			    Temp := Find;
			    Find := Find.Next;
			    Temp.Next := null;
			end if;
			if Find /= null then
			    Ada.Text_IO.Put (Output_Object.Output_File,
				Find.Text (1 .. Find.Length));
			    -- This should always be the last item:
			    if Find.Next /= null then
				Ada.Text_IO.Put_Line ("** Column Item invariant failure!");
			    end if;
			    Free (Find);
			else -- No item, make a blank.
			    Ada.Text_IO.Put (Output_Object.Output_File,
				"&nbsp;");
			end if;
		    end;
	            Ada.Text_IO.Put (Output_Object.Output_File, "<TD align=""left"">");
		end loop;
		if Output_Object.Column_Text = Column_Text_Ptrs_Type'(others => null) then
		    -- We've output everything.
		    Ada.Text_IO.New_Line (Output_Object.Output_File);
		    if Output_Object.HTML_Kind = HTML_3 then
	                Ada.Text_IO.Put_Line (Output_Object.Output_File, "</TABLE></UL></UL>");
		    else
	                Ada.Text_IO.Put_Line (Output_Object.Output_File, "</table></div>");
		    end if;
		    exit;
		end if;
		-- End the row:
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	        Ada.Text_IO.Put (Output_Object.Output_File, "<TR><TD align=""left"">");
	    end loop;
	    Output_Object.Current_Column := 0;
	    Output_Object.Current_Item := 0;
	-- else Two and Three column formats are displayed without any columns.
	-- This is mainly used for the syntax cross-reference and index, and
	-- these definitely look better without columns.
	end if;
	Output_Object.Column_Count := Number_of_Columns;
    end Set_Columns;


    procedure Check_Clause_File (Output_Object : in out HTML_Output_Type) is
	-- Check that a Clause file has been made for this clause; if not,
	-- create one.
    begin
	if not Ada.Text_IO.Is_Open (Output_Object.Output_File) then
	    if (not Output_Object.Big_Files) and then
		  Output_Object.DOS_Filenames and then
		  Output_Object.Section_Name(1) = '-' then
		    Start_HTML_File (Output_Object,
		        Ada.Strings.Fixed.Trim (Output_Object.File_Prefix, Ada.Strings.Right) &
	                    Make_Clause_Anchor_Name (Output_Object, Output_Object.Section_Name(2..3)), "", Clause => "");
	    else
		Start_HTML_File (Output_Object,
		    Ada.Strings.Fixed.Trim (Output_Object.File_Prefix, Ada.Strings.Right) &
	                "-" & Output_Object.Section_Name, "", Clause => "");
	    end if;
	end if;
    end Check_Clause_File;


    procedure Put_Compatibility_Font_Info (Output_Object : in out HTML_Output_Type;
					   Style  : in ARM_Output.Paragraph_Style_Type;
					   Indent : in ARM_Output.Paragraph_Indent_Type) is
	-- Internal:
        -- Output the font information for HTML 4.0 compatibility mode.
    begin
        if Output_Object.HTML_Kind = HTML_4_Compatible then
	    case Paragraph_Info(Style, Indent).Font is
	        when ARM_Output.Default =>
		    if ARM_Output."=" (Output_Object.Body_Font, ARM_Output.Swiss) then
		        Ada.Text_IO.Put (Output_Object.Output_File, SWISS_FONT_CODE);
		        Output_Object.Char_Count := Output_Object.Char_Count + SWISS_FONT_CODE'Length;
		    -- else nothing for Roman.
		    end if;
		when ARM_Output.Roman =>
		    null;
	        when ARM_Output.Swiss =>
		    Ada.Text_IO.Put (Output_Object.Output_File, SWISS_FONT_CODE);
		    Output_Object.Char_Count := Output_Object.Char_Count + SWISS_FONT_CODE'Length;
	        when ARM_Output.Fixed =>
		    Ada.Text_IO.Put (Output_Object.Output_File, "<TT>");
		    Output_Object.Char_Count := Output_Object.Char_Count + 4;
	    end case;
	    if ARM_Output."=" (Paragraph_Info(Style, Indent).Font, ARM_Output.Fixed) then
	        null; -- No font change here.
	    else
	        case Paragraph_Info(Style, Indent).Size is
		    when 0 => null;
		    when 1 =>
		        Ada.Text_IO.Put (Output_Object.Output_File, "<font size=""+1"">");
		        Output_Object.Char_Count := Output_Object.Char_Count + 16;
		    when 2 =>
		        Ada.Text_IO.Put (Output_Object.Output_File, "<font size=""+2"">");
		        Output_Object.Char_Count := Output_Object.Char_Count + 16;
		    when 3 =>
		        Ada.Text_IO.Put (Output_Object.Output_File, "<font size=""+3"">");
		        Output_Object.Char_Count := Output_Object.Char_Count + 16;
		    when -1 =>
		        Ada.Text_IO.Put (Output_Object.Output_File, "<font size=""-1"">");
		        Output_Object.Char_Count := Output_Object.Char_Count + 16;
		    when -2 =>
		        Ada.Text_IO.Put (Output_Object.Output_File, "<font size=""-2"">");
		        Output_Object.Char_Count := Output_Object.Char_Count + 16;
		    when -3 =>
		        Ada.Text_IO.Put (Output_Object.Output_File, "<font size=""-3"">");
		        Output_Object.Char_Count := Output_Object.Char_Count + 16;
		    when others => null; -- Not supported.
	        end case;
	    end if;
        end if;
    end Put_Compatibility_Font_Info;


    procedure Put_End_Compatibility_Font_Info (Output_Object : in out HTML_Output_Type;
					       Style  : in ARM_Output.Paragraph_Style_Type;
					       Indent : in ARM_Output.Paragraph_Indent_Type) is
	-- Internal:
        -- Output the font information for HTML 4.0 compatibility mode.
    begin
        if Output_Object.HTML_Kind = HTML_4_Compatible then
	    if ARM_Output."=" (Paragraph_Info(Style, Indent).Font, ARM_Output.Fixed) then
	        null; -- No font change here.
	    else
	        case Paragraph_Info(Style, Indent).Size is
		    when 0 => null;
		    when 1 =>
		        Ada.Text_IO.Put (Output_Object.Output_File, "</font>");
		    when 2 =>
		        Ada.Text_IO.Put (Output_Object.Output_File, "</font>");
		    when -1 =>
		        Ada.Text_IO.Put (Output_Object.Output_File, "</font>");
		    when -2 =>
		        Ada.Text_IO.Put (Output_Object.Output_File, "</font>");
		    when -3 =>
		        Ada.Text_IO.Put (Output_Object.Output_File, "</font>");
		    when others => null; -- Not supported.
	        end case;
	    end if;
	    case Paragraph_Info(Style, Indent).Font is
	        when ARM_Output.Default =>
		    if ARM_Output."=" (Output_Object.Body_Font, ARM_Output.Swiss) then
		        Ada.Text_IO.Put (Output_Object.Output_File, "</font>");
		    -- else nothing for Roman.
		    end if;
		when ARM_Output.Roman =>
		    null;
	        when ARM_Output.Swiss =>
		    Ada.Text_IO.Put (Output_Object.Output_File, "</font>");
	        when ARM_Output.Fixed =>
		    Ada.Text_IO.Put (Output_Object.Output_File, "</tt>");
	    end case;
        end if;
    end Put_End_Compatibility_Font_Info;


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

	procedure Put_Style (Name : in String;
			     Include_Compatibility : in Boolean := True;
			     Use_DIV : in Boolean := False) is
	    -- Output a style for HTML 4.0; if Include_Compatibility is True,
	    -- include compatibility font information as well.
	    -- If Use_DIV is true, ignore the contents of the
	    -- style data and use a DIV.
	begin
	    if Use_DIV then
	        Ada.Text_IO.Put (Output_Object.Output_File, "<div");
	        Output_Object.Char_Count := 4;
	    else
	        case Paragraph_Info(Style, Indent).Tag is
		    when DIV =>
		        Ada.Text_IO.Put (Output_Object.Output_File, "<div");
		        Output_Object.Char_Count := 4;
		    when UL =>
		        Ada.Text_IO.Put (Output_Object.Output_File, "<ul");
		        Output_Object.Char_Count := 3;
		    when DL =>
		        Ada.Text_IO.Put (Output_Object.Output_File, "<dl");
		        Output_Object.Char_Count := 3;
	        end case;
	    end if;
	    Ada.Text_IO.Put (Output_Object.Output_File, " class=""" & Name & """");
	    Output_Object.Char_Count := Output_Object.Char_Count + 8 + Name'Length + 1;
	    case Justification is
	        when ARM_Output.Default | ARM_Output.Left | ARM_Output.Justified =>
		    null;
	        when ARM_Output.Center =>
		    Ada.Text_IO.Put (Output_Object.Output_File, " style=""text-align: center""");
		    Output_Object.Char_Count := Output_Object.Char_Count + 27;
	        when ARM_Output.Right =>
		    Ada.Text_IO.Put (Output_Object.Output_File, " style=""text-align: right""");
		    Output_Object.Char_Count := Output_Object.Char_Count + 26;
	    end case;
	    case Space_After is
	        when ARM_Output.Normal =>
		    null;
	        when ARM_Output.Narrow =>
		    Ada.Text_IO.Put (Output_Object.Output_File, " style=""margin-bottom: ");
		    Output_Object.Char_Count := Output_Object.Char_Count + 24;
		    Put_EMs(Output_Object.Output_File, (Paragraph_Info(Style, Indent).After * LEADING_PERCENT) / 100);
		    Ada.Text_IO.Put (Output_Object.Output_File, """");
		    Output_Object.Char_Count := Output_Object.Char_Count + 6;
	        when ARM_Output.Wide =>
		    Ada.Text_IO.Put (Output_Object.Output_File, " style=""margin-bottom: ");
		    Output_Object.Char_Count := Output_Object.Char_Count + 24;
		    Put_EMs(Output_Object.Output_File, (Paragraph_Info(Style, Indent).After * TRAILING_PERCENT) / 100);
		    Ada.Text_IO.Put (Output_Object.Output_File, """");
		    Output_Object.Char_Count := Output_Object.Char_Count + 6;
	    end case;
	    Ada.Text_IO.Put (Output_Object.Output_File, ">");
	    Output_Object.Char_Count := Output_Object.Char_Count + 1;
	    if Output_Object.HTML_Kind = HTML_4_Compatible and then Include_Compatibility then
		Put_Compatibility_Font_Info (Output_Object, Style, Indent);
	    end if;
	end Put_Style;

	function Paranum_Anchor (Number : in String) return String is
	    -- Create the string for an anchor from a paragraph number.
	    -- Note that anchors cannot use "/", so we strip the
	    -- slash part. "." is allowed (as well as "-", "_", and ":", but
	    -- nothing else). We don't need the slash part (the version number),
	    -- as paragraph numbers are unique without it, and the version
	    -- is specified by the enclosing document (given by the rest of
	    -- the page).
	    Where : constant Natural := Ada.Strings.Fixed.Index (Number, "/");
	begin
	    if Where = 0 then
		return Number;
	    else
		return Number(Number'First..Where-1);
	    end if;
	end Paranum_Anchor;

    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Already in paragraph");
	end if;
	if not Output_Object.Number_Paragraphs and then
	    Number /= "" then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Paragraph number when none used");
	end if;
	if not Paragraph_Info(Style, Indent).Defined then
            Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
	        "Undefined style " & ARM_Output.Paragraph_Style_Type'Image(Style) &
		" and indent" & ARM_Output.Paragraph_Indent_Type'Image(Indent));
	end if;

	Output_Object.Is_In_Paragraph := True;
	Output_Object.Had_Prefix := not No_Prefix;
	Output_Object.Char_Count := 0;
	Output_Object.Disp_Char_Count := 0;
	Output_Object.Disp_Large_Char_Count := 0;
	Output_Object.Any_Nonspace := False;
	Output_Object.Last_Was_Space := True; -- Start of line
	Output_Object.Conditional_Space := False;
	Output_Object.Saw_Hang_End := False;
        Output_Object.In_Local_Link := False;
	Check_Clause_File (Output_Object);
	-- Note: We only support Justification for the Normal and Wide styles.
	if Output_Object.Column_Count >= 4 then
	    -- Formatting is deferred; only a few formats are supported.
	    if Tab_Stops.Number /= 0 then
	        Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		    "Tabs in 4+ column text");
	    end if;
	    case Style is
	        when ARM_Output.Normal =>
		    null;
		when others =>
	            Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		        "Unsupported format in 4+ column text - " & ARM_Output.Paragraph_Style_Type'Image(Style));
	    end case;
	    if Number /= "" then
	        Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		    "No paragraph numbers in 4+ column text");
	    end if;
	    return; -- Nothing more to do here.
	end if;

	-- Set up tabs:
	case Style is
	    when ARM_Output.Normal | ARM_Output.Wide_Above |
		 ARM_Output.Small | ARM_Output.Small_Wide_Above |
		 ARM_Output.Header | ARM_Output.Small_Header |
		 ARM_Output.Index | ARM_Output.Syntax_Summary |
		 ARM_Output.Title |
		 ARM_Output.Examples | ARM_Output.Small_Examples |
		 ARM_Output.Swiss_Examples | ARM_Output.Small_Swiss_Examples =>
		Output_Object.Tab_Stops := Tab_Stops;
		-- No tabs in HTML; we'll emulate them for fixed fonts.
		-- We'll expand proportional stops here (text characters
		-- are larger than the variable ones these are set up for).
		Output_Object.Can_Emulate_Tabs :=
		    ARM_Output."=" (Paragraph_Info(Style, Indent).Font, ARM_Output.Fixed);
		for I in 1 .. Tab_Stops.Number loop
		    if ARM_Output."=" (Tab_Stops.Stops(I).Kind,
				       ARM_Output.Left_Proportional) then
		        if ARM_Output."=" (Paragraph_Info(Style, Indent).Font, ARM_Output.Fixed) then
			    Output_Object.Tab_Stops.Stops(I).Stop :=
				(Tab_Stops.Stops(I).Stop * 13 / 12);
			else -- Proportional characters are smaller.
			    Output_Object.Tab_Stops.Stops(I).Stop :=
				(Tab_Stops.Stops(I).Stop * 5 / 4);
			end if;
		    else
		        Output_Object.Tab_Stops.Stops(I).Stop :=
				Tab_Stops.Stops(I).Stop;
		    end if;
		end loop;

	    when ARM_Output.Bulleted | ARM_Output.Nested_Bulleted |
		 ARM_Output.Small_Bulleted | ARM_Output.Small_Nested_Bulleted |
		 ARM_Output.Giant_Hanging | ARM_Output.Wide_Hanging |
		 ARM_Output.Medium_Hanging | ARM_Output.Narrow_Hanging |
		 ARM_Output.Hanging_in_Bulleted |
		 ARM_Output.Small_Giant_Hanging | ARM_Output.Small_Wide_Hanging |
		 ARM_Output.Small_Medium_Hanging | ARM_Output.Small_Narrow_Hanging |
		 ARM_Output.Small_Hanging_in_Bulleted |
		 ARM_Output.Enumerated | ARM_Output.Small_Enumerated =>
		if Tab_Stops.Number /= 0 then
	            Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		        "Tabs in hanging/bulleted paragraph");
		end if;
		Output_Object.Can_Emulate_Tabs := False;
	end case;

	if Output_Object.HTML_Kind = HTML_3 then
	    -- Note: We can't control the space below the paragraphs here, so
	    -- Space_After is ignored.
	    -- Make any indents:
	    for I in 1 .. Indent loop
	        Ada.Text_IO.Put (Output_Object.Output_File, "<UL>");
		Output_Object.Char_Count := Output_Object.Char_Count + 4;
	    end loop;
	    case Style is
	        when ARM_Output.Normal | ARM_Output.Wide_Above |
		     ARM_Output.Header =>
		    if ARM_Output."=" (Indent, 0) then
		        case Justification is
		            when ARM_Output.Default | ARM_Output.Left | ARM_Output.Justified =>
			        Ada.Text_IO.Put (Output_Object.Output_File, "<P>");
			        Output_Object.Char_Count := 3;
		            when ARM_Output.Center =>
			        Ada.Text_IO.Put (Output_Object.Output_File, "<P ALIGN=CENTER>");
			        Output_Object.Char_Count := 16;
		            when ARM_Output.Right =>
			        Ada.Text_IO.Put (Output_Object.Output_File, "<P ALIGN=RIGHT>");
			        Output_Object.Char_Count := 15;
		        end case;
		    else
			null; -- Formatting is hard in HTML 3!
		    end if;

	        when ARM_Output.Small | ARM_Output.Small_Wide_Above |
		     ARM_Output.Small_Header =>
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<FONT SIZE=-1>");
		    Output_Object.Char_Count := Output_Object.Char_Count + 14;

	        when ARM_Output.Index =>
		    -- Note: We don't put this in a smaller font.
		    if ARM_Output."=" (Indent, 0) then
		        Ada.Text_IO.Put (Output_Object.Output_File, "<P>");
		        Output_Object.Char_Count := 3;
		    else
			null; -- Formatting is hard in HTML 3!
		    end if;

	        when ARM_Output.Syntax_Summary =>
		    -- Note: We don't put this in a smaller font.
	    	    null;

	        when ARM_Output.Title =>
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<FONT SIZE=+3>");
		    Output_Object.Char_Count := Output_Object.Char_Count + 14;

	        when ARM_Output.Examples =>
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<TT>");
		    Output_Object.Char_Count := Output_Object.Char_Count + 4;
	        when ARM_Output.Small_Examples =>
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<TT><FONT SIZE=-1>");
		    Output_Object.Char_Count := Output_Object.Char_Count + 18;
	        when ARM_Output.Swiss_Examples =>
	    	    Ada.Text_IO.Put (Output_Object.Output_File, SWISS_FONT_CODE);
		    Output_Object.Char_Count := Output_Object.Char_Count + SWISS_FONT_CODE'Length;
	        when ARM_Output.Small_Swiss_Examples =>
	    	    Ada.Text_IO.Put (Output_Object.Output_File, SMALL_SWISS_FONT_CODE);
		    Output_Object.Char_Count := Output_Object.Char_Count + SMALL_SWISS_FONT_CODE'Length;

	        when ARM_Output.Bulleted | ARM_Output.Nested_Bulleted =>
		    if No_Prefix then
			null;
		    else
	    	        Ada.Text_IO.Put (Output_Object.Output_File, "<LI TYPE=DISC>");
		        Output_Object.Char_Count := Output_Object.Char_Count + 14;
		    end if;

	        when ARM_Output.Small_Bulleted | ARM_Output.Small_Nested_Bulleted =>
		    if No_Prefix then
	    	        Ada.Text_IO.Put (Output_Object.Output_File, "<FONT SIZE=-1>");
		        Output_Object.Char_Count := Output_Object.Char_Count + 14;
		    else
	    	        Ada.Text_IO.Put (Output_Object.Output_File, "<LI TYPE=DISC><FONT SIZE=-1>");
		        Output_Object.Char_Count := Output_Object.Char_Count + 28;
		    end if;

	        when ARM_Output.Giant_Hanging | ARM_Output.Wide_Hanging |
		     ARM_Output.Medium_Hanging | ARM_Output.Narrow_Hanging |
		     ARM_Output.Hanging_in_Bulleted |
		     ARM_Output.Enumerated =>
		    if No_Prefix then
	    	        Ada.Text_IO.Put (Output_Object.Output_File, "<DL><DD>");
		        Output_Object.Char_Count := Output_Object.Char_Count + 8;
		        Output_Object.Saw_Hang_End := True;
		    else -- Has prefix.
	    	        Ada.Text_IO.Put (Output_Object.Output_File, "<DL><DT>");
		        Output_Object.Char_Count := Output_Object.Char_Count + 8;
		        Output_Object.Saw_Hang_End := False;
		    end if;

	        when ARM_Output.Small_Giant_Hanging | ARM_Output.Small_Wide_Hanging |
		     ARM_Output.Small_Medium_Hanging | ARM_Output.Small_Narrow_Hanging |
		     ARM_Output.Small_Hanging_in_Bulleted |
		     ARM_Output.Small_Enumerated =>
		    if No_Prefix then
	    	        Ada.Text_IO.Put (Output_Object.Output_File, "<DL><DD><FONT SIZE=-1>");
		        Output_Object.Char_Count := Output_Object.Char_Count + 22;
		        Output_Object.Saw_Hang_End := True;
		    else -- Has prefix.
	    	        Ada.Text_IO.Put (Output_Object.Output_File, "<DL><DT><FONT SIZE=-1>");
		        Output_Object.Char_Count := Output_Object.Char_Count + 22;
		        Output_Object.Saw_Hang_End := False;
		    end if;
	    end case;
	    Output_Object.Paragraph_Style  := Style;
	    Output_Object.Paragraph_Indent := Indent;
	    Output_Object.Font := ARM_Output.Default;
	    Output_Object.Is_Bold := False;
	    Output_Object.Is_Italic := False;
	    Output_Object.Size := 0;
	    Output_Object.Color := ARM_Output.Default;
	    Output_Object.Change := ARM_Output.None;
	    Output_Object.Version := '0';
	    Output_Object.Added_Version := '0';
	    if Number /= "" then -- Has paragraph number.
	        Ada.Text_IO.Put (Output_Object.Output_File, "<A NAME=""p");
	        Ada.Text_IO.Put (Output_Object.Output_File, Paranum_Anchor(Number));
	        Ada.Text_IO.Put (Output_Object.Output_File, """>");
	        Ada.Text_IO.Put (Output_Object.Output_File, TINY_SWISS_FONT_CODE);
	        Ada.Text_IO.Put (Output_Object.Output_File, Number);
	        Ada.Text_IO.Put (Output_Object.Output_File, Number);
	        Ada.Text_IO.Put (Output_Object.Output_File, "</FONT></A> ");
	        Output_Object.Char_Count := Output_Object.Char_Count + TINY_SWISS_FONT_CODE'Length + Number'Length + 8;
	        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + ((Number'Length+1)/2) + 1;
		    -- Note: Count these as half characters, as the font is so small.
		--Output_Object.Disp_Large_Char_Count := <unchanged>;
	    end if;

	elsif Output_Object.HTML_Kind = HTML_4_Compatible then
	    if Number /= "" then -- Has paragraph number.
		Paranum_Used := True;
		Ada.Text_IO.Put (Output_Object.Output_File, "<div class=""paranum"">");
	        Ada.Text_IO.Put (Output_Object.Output_File, "<a name=""p");
	        Ada.Text_IO.Put (Output_Object.Output_File, Paranum_Anchor(Number));
	        Ada.Text_IO.Put (Output_Object.Output_File, """><font size=-2>");
	        Ada.Text_IO.Put (Output_Object.Output_File, Number);
	        Ada.Text_IO.Put (Output_Object.Output_File, "</font></a>");
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "</div>");
	        Output_Object.Char_Count := 0;
	        Output_Object.Disp_Char_Count := 0;
	        Output_Object.Disp_Large_Char_Count := 0;
		Output_Object.Any_Nonspace := False;
		Output_Object.Last_Was_Space := True; -- Start of line
		Output_Object.Conditional_Space := False; -- Don't need it here.
	    end if;

	    case Style is
		when ARM_Output.Normal | ARM_Output.Wide_Above |
		     ARM_Output.Header |
		     ARM_Output.Small  | ARM_Output.Small_Wide_Above |
		     ARM_Output.Small_Header |
	             ARM_Output.Index | ARM_Output.Syntax_Summary |
		     ARM_Output.Title |
	             ARM_Output.Examples | ARM_Output.Swiss_Examples |
	             ARM_Output.Small_Examples | ARM_Output.Small_Swiss_Examples =>
		    Put_Style (Paragraph_Name (Style, Indent));

	        when ARM_Output.Bulleted | ARM_Output.Nested_Bulleted |
	             ARM_Output.Small_Bulleted | ARM_Output.Small_Nested_Bulleted =>
		    Put_Style (Paragraph_Name (Style, Indent), Include_Compatibility => False);
		    if No_Prefix then
			null;
		    else
	    	        Ada.Text_IO.Put (Output_Object.Output_File, "<li type=disc>");
			Output_Object.Char_Count := Output_Object.Char_Count + 14;
		    end if;
		    Put_Compatibility_Font_Info (Output_Object, Style, Indent);

	        when ARM_Output.Giant_Hanging | ARM_Output.Wide_Hanging |
		     ARM_Output.Medium_Hanging | ARM_Output.Narrow_Hanging |
		     ARM_Output.Hanging_in_Bulleted |
		     ARM_Output.Enumerated |
	             ARM_Output.Small_Giant_Hanging | ARM_Output.Small_Wide_Hanging |
	             ARM_Output.Small_Medium_Hanging | ARM_Output.Small_Narrow_Hanging |
		     ARM_Output.Small_Hanging_in_Bulleted |
		     ARM_Output.Small_Enumerated =>
		    declare
			PName : constant String :=
				Paragraph_Name (Style, Indent);
		    begin
		        Put_Style (PName, Include_Compatibility => False);
		        if No_Prefix then
	    	            Ada.Text_IO.Put (Output_Object.Output_File, "<dd class="" & PName & "">");
			    Output_Object.Char_Count := Output_Object.Char_Count + 13 + PName'Length;
		            Output_Object.Saw_Hang_End := True;
--Ada.Text_IO.Put_Line("HTML: 4Comp, Hang Start, Noprefix");
		        else -- Has prefix.
	    	            Ada.Text_IO.Put (Output_Object.Output_File, "<dt>");
			    Output_Object.Char_Count := Output_Object.Char_Count + 4;
		            Output_Object.Saw_Hang_End := False;
--Ada.Text_IO.Put_Line("HTML: 4Comp, Hang Start");
		        end if;
		        Put_Compatibility_Font_Info (Output_Object, Style, Indent);
		    end;

	    end case;
	    Output_Object.Paragraph_Style  := Style;
	    Output_Object.Paragraph_Indent := Indent;
	    Output_Object.Font := ARM_Output.Default;
	    Output_Object.Is_Bold := False;
	    Output_Object.Is_Italic := False;
	    Output_Object.Size := 0;
	    Output_Object.Color := ARM_Output.Default;
	    Output_Object.Change := ARM_Output.None;
	    Output_Object.Version := '0';
	    Output_Object.Added_Version := '0';
	    if Number /= "" then -- Has paragraph number.
		if Paragraph_Info(Style, Indent).Indent = 0 and then
		   ((not No_Prefix) or else
		     Style in ARM_Output.Unprefixed_Style_Subtype) then
		    -- No indent, either a prefix or a style that doesn't
		    -- have a prefix.
		    -- We may have to make a space for the paragraph number,
		    -- as absolute positioned or floating items can overlap
		    -- others.
		    for I in 1 .. (Number'Length+2)-(INDENT_EMS_FOR_PARANUMS/5) loop
			-- We assume that each space is roughly equal to
			-- 0.5em (that should be conservative).
			Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;");
		        Output_Object.Char_Count := Output_Object.Char_Count + 1;
		        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
			--Output_Object.Disp_Large_Char_Count := <unchanged>;
		    end loop;
		-- else is indented, so we don't need to make space.
		end if;
	    end if;
	else -- HTML_4_Only.
	    if Number /= "" then -- Has paragraph number.
		Paranum_Used := True;
	        Ada.Text_IO.Put (Output_Object.Output_File, "<div class=""paranum"">");
	        Ada.Text_IO.Put (Output_Object.Output_File, "<a name=""p");
	        Ada.Text_IO.Put (Output_Object.Output_File, Paranum_Anchor(Number));
	        Ada.Text_IO.Put (Output_Object.Output_File, """>");
	        Ada.Text_IO.Put (Output_Object.Output_File, Number);
	        Ada.Text_IO.Put (Output_Object.Output_File, "</a>");
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "</div>");
	        Output_Object.Char_Count := 0;
	        Output_Object.Disp_Char_Count := 0;
	        Output_Object.Disp_Large_Char_Count := 0;
		Output_Object.Any_Nonspace := False;
		Output_Object.Last_Was_Space := True; -- Start of line
		Output_Object.Conditional_Space := False; -- Don't need it here.
	    end if;

	    case Style is
		when ARM_Output.Normal | ARM_Output.Wide_Above |
		     ARM_Output.Header |
		     ARM_Output.Small  | ARM_Output.Small_Wide_Above |
		     ARM_Output.Small_Header |
	             ARM_Output.Index | ARM_Output.Syntax_Summary |
		     ARM_Output.Title |
	             ARM_Output.Examples | ARM_Output.Swiss_Examples |
	             ARM_Output.Small_Examples | ARM_Output.Small_Swiss_Examples =>
		    Put_Style (Paragraph_Name (Style, Indent));

	        when ARM_Output.Bulleted | ARM_Output.Nested_Bulleted |
	             ARM_Output.Small_Bulleted | ARM_Output.Small_Nested_Bulleted =>
		    -- We use formatted DIVs here, as otherwise the indenting
		    -- varies wildly between Firefox and IE (and does not
		    -- match similar enumerated lists).
		    if No_Prefix then
		        Put_Style (Paragraph_Name (Style, Indent) & "-NoPrefix", Use_DIV => True);
		    else
		        Put_Style (Paragraph_Name (Style, Indent), Use_DIV => True);
		    end if;

	        when ARM_Output.Giant_Hanging | ARM_Output.Wide_Hanging |
		     ARM_Output.Medium_Hanging | ARM_Output.Narrow_Hanging |
		     ARM_Output.Hanging_in_Bulleted |
		     ARM_Output.Enumerated |
	             ARM_Output.Small_Giant_Hanging | ARM_Output.Small_Wide_Hanging |
	             ARM_Output.Small_Medium_Hanging | ARM_Output.Small_Narrow_Hanging |
		     ARM_Output.Small_Hanging_in_Bulleted |
		     ARM_Output.Small_Enumerated =>
		    if No_Prefix then
		        Put_Style (Paragraph_Name (Style, Indent) & "-Body", Use_DIV => True);
		        Output_Object.Saw_Hang_End := True;
--Ada.Text_IO.Put_Line("HTML: 4, Hang Start, Noprefix");
		    else -- Has prefix.
		        Put_Style (Paragraph_Name (Style, Indent) & "-Term", Use_DIV => True);
		        Output_Object.Saw_Hang_End := False;
--Ada.Text_IO.Put_Line("HTML: 4, Hang Start, Normal");
		    end if;
	    end case;
	    Output_Object.Paragraph_Style  := Style;
	    Output_Object.Paragraph_Indent := Indent;
	    Output_Object.Font := ARM_Output.Default;
	    Output_Object.Is_Bold := False;
	    Output_Object.Is_Italic := False;
	    Output_Object.Size := 0;
	    Output_Object.Color := ARM_Output.Default;
	    Output_Object.Change := ARM_Output.None;
	    Output_Object.Version := '0';
	    Output_Object.Added_Version := '0';
	    if Number /= "" then -- Has paragraph number.
		if Paragraph_Info(Style, Indent).Indent = 0 and then
		   ((not No_Prefix) or else
		     Style in ARM_Output.Unprefixed_Style_Subtype) then
		    -- No indent, either a prefix or a style that doesn't
		    -- have a prefix.
		    -- We may have to make a space for the paragraph number,
		    -- as absolute positioned or floating items can overlap
		    -- others.
		    for I in 1 .. (Number'Length+2)-((INDENT_EMS_FOR_PARANUMS+5)*3/10) loop
			-- We assume that each space is roughly equal to
			-- 0.33em (that should be conservative). We also assume
			-- that the normal left edge space is 1.0em (this is
			-- true on IE 5&6). Paragraph numbers are positioned
			-- at 0.5ems, so the additional difference is +5.
			Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;");
		        Output_Object.Char_Count := Output_Object.Char_Count + 1;
		        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
			--Output_Object.Disp_Large_Char_Count := <unchanged>;
		    end loop;
		-- else is indented, so we don't need to make space.
		end if;
	    end if;
	end if;
	Paragraph_Used(Style, Indent) := True;

	-- Note: No_Breaks and Keep_with_Next have no effect here, because
	-- HTML doesn't have page breaks.
    end Start_Paragraph;


    procedure End_Paragraph (Output_Object : in out HTML_Output_Type) is
	-- End a paragraph.

	procedure Put_End_Style (Style  : in ARM_Output.Paragraph_Style_Type;
				 Indent : in ARM_Output.Paragraph_Indent_Type;
				 Include_Compatibility : in Boolean := True) is
	    -- Output a end style for HTML 4.0; if Include_Compatibility is True,
	    -- include compatibility font information as well.
	begin
	    if Output_Object.HTML_Kind = HTML_4_Compatible and then Include_Compatibility then
		Put_End_Compatibility_Font_Info (Output_Object, Style, Indent);
	    end if;
	    case Paragraph_Info(Style, Indent).Tag is
		when DIV =>
		    Ada.Text_IO.Put (Output_Object.Output_File, "</div>");
		when UL =>
		    Ada.Text_IO.Put (Output_Object.Output_File, "</ul>");
		when DL =>
		    Ada.Text_IO.Put (Output_Object.Output_File, "</dl>");
	    end case;
	end Put_End_Style;

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
	if Output_Object.In_Local_Link then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Unclosed Local_Link");
	    Output_Object.In_Local_Link := False;
	end if;
	if Output_Object.Column_Count >= 4 then
	    -- Formatting is deferred; only a few formats are supported.
	    if Output_Object.Column_Text (Output_Object.Current_Column) /= null and then
	       Output_Object.Column_Text (Output_Object.Current_Column).Item = Output_Object.Current_Item then
		Output_Object.Column_Text (Output_Object.Current_Column).End_Para := True;
	    end if;
	    Output_Object.Current_Item := Output_Object.Current_Item + 2; -- Skip an item.
            Output_Object.Char_Count := 0;
            Output_Object.Disp_Char_Count := 0;
	    Output_Object.Disp_Large_Char_Count := 0;
	    Output_Object.Any_Nonspace := False;
	    Output_Object.Last_Was_Space := True; -- Start of line.
	    Output_Object.Conditional_Space := False; -- Don't need it here.
	    return; -- Nothing else to do here.
	end if;

	if Output_Object.HTML_Kind = HTML_3 then
	    case Output_Object.Paragraph_Style is
	        when ARM_Output.Normal | ARM_Output.Wide_Above | ARM_Output.Header |
	             ARM_Output.Index =>
		    if ARM_Output."=" (Output_Object.Paragraph_Indent, 0) then
		        Ada.Text_IO.Put (Output_Object.Output_File, "</P>");
		    -- else let the indent nesting handling it.
		    end if;
	        when ARM_Output.Syntax_Summary =>
	    	    null;
	        when ARM_Output.Small | ARM_Output.Small_Wide_Above |
		     ARM_Output.Small_Header =>
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "</FONT>");
	        when ARM_Output.Title =>
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "</FONT>");

	        when ARM_Output.Examples =>
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "</TT>");
	        when ARM_Output.Small_Examples =>
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "</FONT>");
	        when ARM_Output.Swiss_Examples =>
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "</FONT>");
	        when ARM_Output.Small_Swiss_Examples =>
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "</FONT>");

	        when ARM_Output.Bulleted | ARM_Output.Nested_Bulleted =>
		    if Output_Object.Had_Prefix then
	    	        Ada.Text_IO.Put (Output_Object.Output_File, "</LI>");
		    -- else nothing to do.
		    end if;
	        when ARM_Output.Small_Bulleted | ARM_Output.Small_Nested_Bulleted =>
		    if Output_Object.Had_Prefix then
	    	        Ada.Text_IO.Put (Output_Object.Output_File, "</FONT></LI>");
		    else
	    	        Ada.Text_IO.Put (Output_Object.Output_File, "</FONT>");
		    end if;

	        when ARM_Output.Giant_Hanging | ARM_Output.Wide_Hanging |
		     ARM_Output.Medium_Hanging | ARM_Output.Narrow_Hanging |
		     ARM_Output.Hanging_in_Bulleted |
	             ARM_Output.Enumerated =>
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "</DL>");

	        when ARM_Output.Small_Giant_Hanging | ARM_Output.Small_Wide_Hanging |
		     ARM_Output.Small_Medium_Hanging | ARM_Output.Small_Narrow_Hanging |
		     ARM_Output.Small_Hanging_in_Bulleted |
	             ARM_Output.Small_Enumerated =>
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "</FONT></DL>");

	    end case;
	    -- Reverse any indents:
	    for I in reverse 1 .. Output_Object.Paragraph_Indent loop
	        Ada.Text_IO.Put (Output_Object.Output_File, "</UL>");
	    end loop;
	    Ada.Text_IO.New_Line (Output_Object.Output_File, 2);

	elsif Output_Object.HTML_Kind = HTML_4_Only then
	    case Output_Object.Paragraph_Style is
		when ARM_Output.Normal | ARM_Output.Wide_Above |
		     ARM_Output.Header |
		     ARM_Output.Small  | ARM_Output.Small_Wide_Above |
		     ARM_Output.Small_Header |
	             ARM_Output.Index | ARM_Output.Syntax_Summary |
		     ARM_Output.Title |
	             ARM_Output.Examples | ARM_Output.Swiss_Examples |
	             ARM_Output.Small_Examples | ARM_Output.Small_Swiss_Examples =>
		    Put_End_Style (Output_Object.Paragraph_Style,
				   Output_Object.Paragraph_Indent);
	        when ARM_Output.Bulleted | ARM_Output.Nested_Bulleted |
	             ARM_Output.Small_Bulleted | ARM_Output.Small_Nested_Bulleted =>
		    -- We've overridden the style class here.
		    Ada.Text_IO.Put (Output_Object.Output_File, "</div>");
	        when ARM_Output.Giant_Hanging | ARM_Output.Wide_Hanging |
		     ARM_Output.Medium_Hanging | ARM_Output.Narrow_Hanging |
		     ARM_Output.Hanging_in_Bulleted |
		     ARM_Output.Enumerated |
	             ARM_Output.Small_Giant_Hanging | ARM_Output.Small_Wide_Hanging |
	             ARM_Output.Small_Medium_Hanging | ARM_Output.Small_Narrow_Hanging |
		     ARM_Output.Small_Hanging_in_Bulleted |
		     ARM_Output.Small_Enumerated =>
		    -- We've overridden the style class here.
		    Ada.Text_IO.Put (Output_Object.Output_File, "</div>");
	    end case;
	    Ada.Text_IO.New_Line (Output_Object.Output_File);
	else -- if Output_Object.HTML_Kind = HTML_4_Compatible
	    case Output_Object.Paragraph_Style is
		when ARM_Output.Normal | ARM_Output.Wide_Above |
		     ARM_Output.Header |
		     ARM_Output.Small  | ARM_Output.Small_Wide_Above |
		     ARM_Output.Small_Header |
	             ARM_Output.Index | ARM_Output.Syntax_Summary |
		     ARM_Output.Title |
	             ARM_Output.Examples | ARM_Output.Swiss_Examples |
	             ARM_Output.Small_Examples | ARM_Output.Small_Swiss_Examples =>
		    Put_End_Style (Output_Object.Paragraph_Style,
				   Output_Object.Paragraph_Indent);
	        when ARM_Output.Bulleted | ARM_Output.Nested_Bulleted |
	             ARM_Output.Small_Bulleted | ARM_Output.Small_Nested_Bulleted =>
		    Put_End_Compatibility_Font_Info (Output_Object,
						     Output_Object.Paragraph_Style,
						     Output_Object.Paragraph_Indent);
		    if Output_Object.Had_Prefix then
	    	        Ada.Text_IO.Put (Output_Object.Output_File, "</li>");
		    -- else null;
		    end if;
		    Put_End_Style (Output_Object.Paragraph_Style,
				   Output_Object.Paragraph_Indent,
				   Include_Compatibility => False);
	        when ARM_Output.Giant_Hanging | ARM_Output.Wide_Hanging |
		     ARM_Output.Medium_Hanging | ARM_Output.Narrow_Hanging |
		     ARM_Output.Hanging_in_Bulleted |
		     ARM_Output.Enumerated |
	             ARM_Output.Small_Giant_Hanging | ARM_Output.Small_Wide_Hanging |
	             ARM_Output.Small_Medium_Hanging | ARM_Output.Small_Narrow_Hanging |
		     ARM_Output.Small_Hanging_in_Bulleted |
		     ARM_Output.Small_Enumerated =>
		    Put_End_Style (Output_Object.Paragraph_Style,
				   Output_Object.Paragraph_Indent);
	    end case;
	    Ada.Text_IO.New_Line (Output_Object.Output_File);
	end if;
        Output_Object.Char_Count := 0;
        Output_Object.Disp_Char_Count := 0;
        Output_Object.Disp_Large_Char_Count := 0;
	Output_Object.Any_Nonspace := False;
        Output_Object.Last_Was_Space := True; -- Start of line.
        Output_Object.Conditional_Space := False; -- Don't need it here.
    end End_Paragraph;


    procedure Category_Header (Output_Object : in out HTML_Output_Type;
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
	Ada.Text_IO.New_Line (Output_Object.Output_File);
	if Output_Object.HTML_Kind = HTML_4_Only then
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H4 Class=""centered"">" & Header_Text & "</H4>");
	else
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H4 ALIGN=CENTER>" & Header_Text & "</H4>");
	end if;
	Output_Object.Char_Count := 0;
	Output_Object.Disp_Char_Count := 0;
        Output_Object.Disp_Large_Char_Count := 0;
	Output_Object.Any_Nonspace := False;
        Output_Object.Last_Was_Space := True; -- Start of line.
        Output_Object.Conditional_Space := False; -- Don't need it here.
    end Category_Header;


    procedure Clause_Header (Output_Object     : in out HTML_Output_Type;
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

	if not Output_Object.Big_Files then
	    if Ada.Text_IO.Is_Open (Output_Object.Output_File) then
	        End_HTML_File (Output_Object);
	    end if;

	    -- Special for table of contents:
	    if Clause_Number = "" and then
		(Header_Text = "Table of Contents" or else -- Ada 95 format
		 Header_Text = "Contents") then -- ISO 2004 format.
                Start_HTML_File (Output_Object,
		    Ada.Strings.Fixed.Trim (Output_Object.File_Prefix, Ada.Strings.Right) &
			"-TOC", Header_Text, "");
		if Header_Text = "Table of Contents" then -- Ada 95 format
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>Table of Contents</H1>");
		else
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>Contents</H1>");
		end if;
	        Output_Object.Char_Count := 0;
	        Output_Object.Disp_Char_Count := 0;
	        Output_Object.Disp_Large_Char_Count := 0;
	        Output_Object.Any_Nonspace := False;
	        Output_Object.Last_Was_Space := True; -- Start of line.
	        Output_Object.Conditional_Space := False; -- Don't need it here.
	        return;
	    end if;

	    Start_HTML_File (Output_Object,
		    Make_Clause_File_Name (Output_Object, Clause_Number),
		    Header_Text, Clause_Number);
	else -- Big Files:
	    if Clause_Number = "" and then
		(Header_Text = "Table of Contents" or else -- Ada 95 format
		 Header_Text = "Contents") then -- ISO 2004 format.
	        -- Insert an anchor:
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "<A NAME=""TOC""></A>");
		if Header_Text = "Table of Contents" then -- Ada 95 format
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>Table of Contents</H1>");
		else
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>Contents</H1>");
		end if;
	        Output_Object.Char_Count := 0;
	        Output_Object.Disp_Char_Count := 0;
	        Output_Object.Disp_Large_Char_Count := 0;
	        Output_Object.Any_Nonspace := False;
	        Output_Object.Last_Was_Space := True; -- Start of line.
	        Output_Object.Conditional_Space := False; -- Don't need it here.
	        return;
	    end if;
	    -- Insert an anchor:
	    Ada.Text_IO.Put (Output_Object.Output_File, "<A NAME=""");
	    Ada.Text_IO.Put (Output_Object.Output_File,
	        Make_Clause_Anchor_Name (Output_Object, Clause_Number));
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, """></A>");
	end if;

	case Level is
	    when ARM_Contents.Plain_Annex =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>" & Clause_Number &
		    "<BR>"); -- Note: Clause_Number includes "Annex"
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, Header_Text & "</H1>");
	    when ARM_Contents.Normative_Annex =>
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>" & Clause_Number & "</H1>");
				-- Note: Clause_Number includes "Annex"
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H2>(normative)</H2>");
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>" & Header_Text & "</H1>");
	    when ARM_Contents.Informative_Annex =>
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>" & Clause_Number & "</H1>");
				-- Note: Clause_Number includes "Annex"
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H2>(informative)</H2>");
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>" & Header_Text & "</H1>");
	    when ARM_Contents.Unnumbered_Section  =>
	        if Header_Text /= "" then
		    Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>" &
				          Header_Text & "</H1>");
	        end if;
	    when ARM_Contents.Section =>
		case Top_Level_Subdivision_Name is
		    when ARM_Output.Chapter =>
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>Chapter " &
					      Clause_Number & ": " & Header_Text & "</H1>");
		    when ARM_Output.Section =>
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>Section " &
					      Clause_Number & ": " & Header_Text & "</H1>");
		    when ARM_Output.Clause =>
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>" &
					      Clause_Number & " &nbsp; " & Header_Text & "</H1>");
		end case;
	    when ARM_Contents.Clause | ARM_Contents.Subclause |
		 ARM_Contents.Subsubclause =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>" &
				      Clause_Number & ' ' & Header_Text & "</H1>");
	    when ARM_Contents.Dead_Clause  =>
		raise Program_Error; -- No headers for dead clauses.
	end case;
	Output_Object.Char_Count := 0;
	Output_Object.Disp_Char_Count := 0;
        Output_Object.Disp_Large_Char_Count := 0;
	Output_Object.Any_Nonspace := False;
        Output_Object.Last_Was_Space := True; -- Start of line.
        Output_Object.Conditional_Space := False; -- Don't need it here.
	-- No page breaks in HTML, so we don't need to look at No_Page_Break.
    end Clause_Header;


    procedure Revised_Clause_Header
			    (Output_Object     : in out HTML_Output_Type;
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
	begin
	    if Output_Object.HTML_Kind = HTML_3 then
	        if Old_Version = '0' then -- Old text is original text
		    return "<U>" & New_Header_Text & "</U><S>" & Old_Header_Text & "</S>";
		else
		    return "<U>" & New_Header_Text & "</U><S><U>" & Old_Header_Text & "</U></S>";
		end if;
	    elsif Old_Version = '0' then -- Old text is original text
		Revision_Used(Version) := True;
		return "<span class=""insert" & Version & """>" & New_Header_Text &
		  "</span><span class=""delete" & Version & """>" & Old_Header_Text & "</span>";
	    else
		Revision_Used(Version) := True;
		Revision_Used(Old_Version) := True;
		return "<span class=""insert" & Version & """>" & New_Header_Text &
		  "</span><span class=""delete" & Version & """><span class=""insert" & Old_Version & """>" &
                  Old_Header_Text & "</span></span>";
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

	if not Output_Object.Big_Files then
	    if Ada.Text_IO.Is_Open (Output_Object.Output_File) then
	        End_HTML_File (Output_Object);
	    end if;

	    Start_HTML_File (Output_Object,
		    Make_Clause_File_Name (Output_Object, Clause_Number),
		    New_Header_Text, Clause_Number);
	else -- Big Files:
	    -- Insert an anchor:
	    Ada.Text_IO.Put (Output_Object.Output_File, "<A NAME=""");
	    Ada.Text_IO.Put (Output_Object.Output_File,
	        Make_Clause_Anchor_Name (Output_Object, Clause_Number));
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, """></A>");
	end if;

	case Level is
	    when ARM_Contents.Plain_Annex =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>" & Clause_Number &
		    "<BR>"); -- Note: Clause_Number includes "Annex"
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, Header_Text & "</H1>");
	    when ARM_Contents.Normative_Annex =>
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>" & Clause_Number & "</H1>");
				-- Note: Clause_Number includes "Annex"
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H2>(normative)</H2>");
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>" & Header_Text & "</H1>");
	    when ARM_Contents.Informative_Annex =>
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>" & Clause_Number & "</H1>");
				-- Note: Clause_Number includes "Annex"
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H2>(informative)</H2>");
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>" & Header_Text & "</H1>");
	    when ARM_Contents.Unnumbered_Section =>
	        if Header_Text /= "" then
		    Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>" &
				          Header_Text & "</H1>");
	        end if;
	    when ARM_Contents.Section =>
		case Top_Level_Subdivision_Name is
		    when ARM_Output.Chapter =>
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>Chapter " &
					      Clause_Number & ": " & Header_Text & "</H1>");
		    when ARM_Output.Section =>
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>Section " &
					      Clause_Number & ": " & Header_Text & "</H1>");
		    when ARM_Output.Clause =>
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>" &
					      Clause_Number & " &nbsp; " & Header_Text & "</H1>");
		end case;
	    when ARM_Contents.Clause | ARM_Contents.Subclause |
		 ARM_Contents.Subsubclause =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>" &
				      Clause_Number & ' ' & Header_Text & "</H1>");
	    when ARM_Contents.Dead_Clause  =>
		raise Program_Error; -- No headers for dead clauses.
	end case;
	Output_Object.Char_Count := 0;
	Output_Object.Disp_Char_Count := 0;
        Output_Object.Disp_Large_Char_Count := 0;
	Output_Object.Any_Nonspace := False;
        Output_Object.Last_Was_Space := True; -- Start of line.
        Output_Object.Conditional_Space := False; -- Don't need it here.
	-- No page breaks in HTML, so we don't need to look at No_Page_Break.
    end Revised_Clause_Header;


    procedure TOC_Marker (Output_Object : in out HTML_Output_Type;
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


    procedure New_Page (Output_Object : in out HTML_Output_Type;
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
	    when ARM_Output.Any_Page | ARM_Output.Odd_Page_Only =>
		if Output_Object.Is_In_Paragraph then
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
			"Page in paragraph");
		end if;
		-- No real page breaks supported.
		--Ada.Text_IO.Put_Line (Output_Object.Output_File, "<P><BR><BR></P>");
		--Ada.Text_IO.Put_Line (Output_Object.Output_File, "<HR>"); -- Horizontal line.
		--Ada.Text_IO.Put_Line (Output_Object.Output_File, "<P><BR></P>");
		-- This horizontal rule looks awful when inserted solely to
		-- make PDF formats look good; and it doesn't make much sense
		-- any other time, either. (Why would we want to start a page
		-- with this?) So it's been removed completely; and we have no
		-- other page breaks in HTML.
	    when ARM_Output.Soft_Page =>
		if not Output_Object.Is_In_Paragraph then
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
			"Soft page not in paragraph");
		end if;
		null; -- No page breaks in HTML.
	end case;
    end New_Page;


    procedure Separator_Line (Output_Object : in out HTML_Output_Type;
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
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "<HR SIZE=1>"); -- Horizontal line.
	else
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "<HR SIZE=2>"); -- Horizontal line.
	end if;
	Ada.Text_IO.New_Line (Output_Object.Output_File);
    end Separator_Line;


    procedure Start_Table (Output_Object : in out HTML_Output_Type;
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
	use type ARM_Output.Header_Kind_Type;
	use type ARM_Output.Column_Text_Alignment;
    begin
	-- No_Page_Break, First_Column_Width, and Last_Column_Width not used,
	-- the latter two because column width is calculated based on the
	-- contents.
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Table in paragraph");
	end if;

	if Output_Object.HTML_Kind /= HTML_3 then
            Ada.Text_IO.Put (Output_Object.Output_File, "<div class=""" &
		Paragraph_Name(ARM_Output.Normal, 1) & """>");
	    Paragraph_Used(ARM_Output.Normal, 1) := True;
	end if;
	if Has_Border then
            Ada.Text_IO.Put (Output_Object.Output_File, "<TABLE frame=""border"" rules=""all"" border=""2"" cellpadding=""4"">");
	else
            Ada.Text_IO.Put (Output_Object.Output_File, "<TABLE frame=""void"" rules=""none"" border=""0"" cellpadding=""2"">");
	end if;
	if Header_Kind = ARM_Output.Both_Caption_and_Header then
            Ada.Text_IO.Put (Output_Object.Output_File, "<CAPTION>");
	    Output_Object.Char_Count := 9;
	    Output_Object.In_Header := True;
	elsif Header_Kind = ARM_Output.Header_Only then
	    if Alignment = ARM_Output.Center_All then
	        Ada.Text_IO.Put (Output_Object.Output_File, "<TR><TH align=""center"">");
	        Output_Object.Char_Count := 24;
	    else
	        Ada.Text_IO.Put (Output_Object.Output_File, "<TR><TH align=""left"">");
	        Output_Object.Char_Count := 22;
	    end if;
	    Output_Object.In_Header := True;
	else -- Header_Kind = ARM_Output.No_Headers then
	    if Alignment = ARM_Output.Center_All then
	        Ada.Text_IO.Put (Output_Object.Output_File, "<TR><TD align=""center"">");
	        Output_Object.Char_Count := 24;
	    else
	        Ada.Text_IO.Put (Output_Object.Output_File, "<TR><TD align=""left"">");
	        Output_Object.Char_Count := 22;
	    end if;
	    Output_Object.In_Header := False;
	end if;
	Output_Object.Disp_Char_Count := 0;
        Output_Object.Disp_Large_Char_Count := 0;
	Output_Object.Any_Nonspace := False;
        Output_Object.Last_Was_Space := True; -- Start of line.
        Output_Object.Conditional_Space := False; -- Don't need it here.

	Output_Object.Is_In_Paragraph := True;
	Output_Object.Is_In_Table := True;

	Output_Object.Table_Column_Alignment := Alignment;
	Output_Object.Table_Has_Small_Text := Small_Text_Size;
	if Output_Object.Table_Has_Small_Text then
	    if Output_Object.HTML_Kind = HTML_4_Only then
	        Ada.Text_IO.Put (Output_Object.Output_File, "<SPAN STYLE=""font-size: 80%"">");
	        Output_Object.Char_Count := Output_Object.Char_Count + 29;
	    else
	        Ada.Text_IO.Put (Output_Object.Output_File, "<FONT SIZE=""-1"">");
	        Output_Object.Char_Count := Output_Object.Char_Count + 16;
	    end if;
	end if;
    end Start_Table;


    procedure Table_Marker (Output_Object : in out HTML_Output_Type;
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

	-- Close the small fonts (we always need to do this):
	if Output_Object.Table_Has_Small_Text then
	    if Output_Object.HTML_Kind = HTML_4_Only then
	        Ada.Text_IO.Put (Output_Object.Output_File, "</SPAN>");
	        Output_Object.Char_Count := Output_Object.Char_Count + 7;
	    else
	        Ada.Text_IO.Put (Output_Object.Output_File, "</FONT>");
	        Output_Object.Char_Count := Output_Object.Char_Count + 7;
	    end if;
	end if;

	case Marker is
	    when ARM_Output.End_Item =>
		-- Note: This isn't the first item on a row.
		if Output_Object.In_Header then
		    if Output_Object.Table_Column_Alignment = ARM_Output.Left_All then
	                Ada.Text_IO.Put (Output_Object.Output_File, "<TH align=""left"">");
		        Output_Object.Char_Count := Output_Object.Char_Count + 18;
		    else
	                Ada.Text_IO.Put (Output_Object.Output_File, "<TH align=""center"">");
		        Output_Object.Char_Count := Output_Object.Char_Count + 20;
		    end if;
		else
		    if Output_Object.Table_Column_Alignment = ARM_Output.Left_All then
	                Ada.Text_IO.Put (Output_Object.Output_File, "<TD align=""left"">");
		        Output_Object.Char_Count := Output_Object.Char_Count + 18;
		    else
	                Ada.Text_IO.Put (Output_Object.Output_File, "<TD align=""center"">");
		        Output_Object.Char_Count := Output_Object.Char_Count + 20;
		    end if;
		end if;
	    when ARM_Output.End_Caption =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "</CAPTION>");
	        if Output_Object.Table_Column_Alignment = ARM_Output.Center_All then
	            Ada.Text_IO.Put (Output_Object.Output_File, "<TR><TH align=""center"">");
		    Output_Object.Char_Count := 24;
		else
	            Ada.Text_IO.Put (Output_Object.Output_File, "<TR><TH align=""left"">");
		    Output_Object.Char_Count := 22;
		end if;
		Output_Object.Disp_Char_Count := 0;
	        Output_Object.Disp_Large_Char_Count := 0;
		Output_Object.Any_Nonspace := False;
	        Output_Object.Last_Was_Space := True; -- Start of line.
	        Output_Object.Conditional_Space := False; -- Don't need it here.
	    when ARM_Output.End_Header =>
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	        if Output_Object.Table_Column_Alignment = ARM_Output.Center_All then
	            Ada.Text_IO.Put (Output_Object.Output_File, "<TR><TD align=""center"">");
		    Output_Object.Char_Count := 24;
		else
	            Ada.Text_IO.Put (Output_Object.Output_File, "<TR><TD align=""left"">");
		    Output_Object.Char_Count := 22;
		end if;
		Output_Object.Disp_Char_Count := 0;
	        Output_Object.Disp_Large_Char_Count := 0;
		Output_Object.Any_Nonspace := False;
	        Output_Object.Last_Was_Space := True; -- Start of line.
	        Output_Object.Conditional_Space := False; -- Don't need it here.
		Output_Object.In_Header := False;
	    when ARM_Output.End_Row | ARM_Output.End_Row_Next_Is_Last =>
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	        if Output_Object.Table_Column_Alignment = ARM_Output.Center_All then
	            Ada.Text_IO.Put (Output_Object.Output_File, "<TR><TD align=""center"">");
		    Output_Object.Char_Count := 24;
		else
	            Ada.Text_IO.Put (Output_Object.Output_File, "<TR><TD align=""left"">");
		    Output_Object.Char_Count := 22;
		end if;
		Output_Object.Disp_Char_Count := 0;
	        Output_Object.Disp_Large_Char_Count := 0;
		Output_Object.Any_Nonspace := False;
	        Output_Object.Last_Was_Space := True; -- Start of line.
	        Output_Object.Conditional_Space := False; -- Don't need it here.
	    when ARM_Output.End_Table =>
		Ada.Text_IO.New_Line (Output_Object.Output_File);
		if Output_Object.HTML_Kind /= HTML_3 then
		    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</table></div>");
		else
		    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</TABLE>");
		end if;
		Output_Object.Is_In_Paragraph := False;
		Output_Object.Is_In_Table := False;
	end case;

	if Output_Object.Table_Has_Small_Text and then ARM_Output."/=" (Marker,
	    ARM_Output.End_Table) then
	    if Output_Object.HTML_Kind = HTML_4_Only then
	        Ada.Text_IO.Put (Output_Object.Output_File, "<SPAN STYLE=""font-size: 80%"">");
	        Output_Object.Char_Count := Output_Object.Char_Count + 29;
	    else
	        Ada.Text_IO.Put (Output_Object.Output_File, "<FONT SIZE=""-1"">");
	        Output_Object.Char_Count := Output_Object.Char_Count + 16;
	    end if;
        end if;
    end Table_Marker;


    -- Text output: These are only allowed after a Start_Paragraph and
    -- before any End_Paragraph. Raises Not_Valid_Error if not allowed.

    Special_Set : constant Ada.Strings.Maps.Character_Set :=
       Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set (Ada.Strings.Maps.Character_Range'(Low => Character'Val(127), High => Character'Val(255))),
         Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('<'),
           Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('>'),
             Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('"'),
			              Ada.Strings.Maps.To_Set ('&')))));

    No_Conditional_Set : constant Ada.Strings.Maps.Character_Set :=
         Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set (' '),
           Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('.'),
             Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set (','),
               Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set (':'),
                 Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set (';'),
                   Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('!'),
                     Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('('),
			                      Ada.Strings.Maps.To_Set (')'))))))));

    Large_Char_Set : constant Ada.Strings.Maps.Character_Set :=
	Ada.Strings.Maps."or" (Ada.Strings.Maps.Constants.Upper_Set,
	  Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set (Ada.Strings.Maps.Character_Range'(Low => '0', High => '9')),
           Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('m'),
             Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('w'),
               Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('<'),
                 Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('>'),
                   Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('&'),
                     Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('%'),
                       Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('@'),
                         Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('$'),
                           Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('*'),
                             Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('+'),
                               Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('='),
                                 Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('?'),
                                   Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('/'),
                                     Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('\'),
			                      Ada.Strings.Maps.To_Set ('#')))))))))))))))));


    procedure Output_Text (Output_Object : in out HTML_Output_Type;
			   Text : in String) is
	-- Output the text to the current output place.
    begin
	if Output_Object.Column_Count >= 4 then
	    if (Output_Object.Column_Text(Output_Object.Current_Column) = null) or else
		-- No items stored.
	       (Output_Object.Column_Text(Output_Object.Current_Column).Item /=
	        Output_Object.Current_Item) then
		-- Start a new item.
		Output_Object.Column_Text(Output_Object.Current_Column) :=
		    new Column_Text_Item_Type'(Text => (others => ' '),
			Length => 0, Item => Output_Object.Current_Item,
			End_Para => False, Next => Output_Object.Column_Text(Output_Object.Current_Column));
	    end if;
	    if Output_Object.Column_Text(Output_Object.Current_Column).Length +
		Text'Length > Output_Object.Column_Text(Output_Object.Current_Column).Text'Length then
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
			"Column item full, but more text! - " &
			Output_Object.Column_Text(Output_Object.Current_Column).Text(1..Output_Object.Column_Text(Output_Object.Current_Column).Length) & Text);
	    else
		Output_Object.Column_Text(Output_Object.Current_Column).Text(
		   Output_Object.Column_Text(Output_Object.Current_Column).Length+1..
		   Output_Object.Column_Text(Output_Object.Current_Column).Length+Text'Length) :=
			Text;
		Output_Object.Column_Text(Output_Object.Current_Column).Length :=
		   Output_Object.Column_Text(Output_Object.Current_Column).Length + Text'Length;
	    end if;
	else -- Normal, use Text_IO.
	     Ada.Text_IO.Put (Output_Object.Output_File, Text);
	     Output_Object.Char_Count := Output_Object.Char_Count + Text'Length;
	end if;
    end Output_Text;


    procedure Ordinary_Text (Output_Object : in out HTML_Output_Type;
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
	if Text'Length = 0 then
	    return; -- Nothing to do.
	end if;
	if Ada.Strings.Fixed.Count (Text, Special_Set) = 0 then
	    if Output_Object.Char_Count + Text'Length >= LINE_LENGTH - 10 then
		-- We can only break on a space.
	        for I in Text'range loop
		    Ordinary_Character (Output_Object, Text(I));
	        end loop;
	    else
		if Output_Object.Conditional_Space then
		    Output_Object.Conditional_Space := False;
		    if Ada.Strings.Maps.Is_In (Text(Text'First), No_Conditional_Set) then
			null; -- Don't need the conditional space.
		    else
		        Output_Text (Output_Object, " ");
		        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
			--Output_Object.Disp_Large_Char_Count := <unchanged>;
		    end if;
		end if;
	        Output_Text (Output_Object, Text);
	        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + Text'Length;
	        Output_Object.Disp_Large_Char_Count :=
		    Output_Object.Disp_Large_Char_Count +
		    Ada.Strings.Fixed.Count (Text, Large_Char_Set);
		Output_Object.Any_Nonspace := True;
		Output_Object.Last_was_Space := Text(Text'Last) = ' ';
	    end if;
	else
	    for I in Text'range loop
		Ordinary_Character (Output_Object, Text(I));
	    end loop;
	end if;
    end Ordinary_Text;


    procedure Ordinary_Character (Output_Object : in out HTML_Output_Type;
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
	if Output_Object.Conditional_Space then
	    Output_Object.Conditional_Space := False;
	    if Ada.Strings.Maps.Is_In (Char, No_Conditional_Set) then
		null; -- Don't need the conditional space.
	    else
	        Output_Text (Output_Object, " ");
	        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		--Output_Object.Disp_Large_Char_Count := <unchanged>;
	    end if;
	end if;
	Output_Object.Last_was_Space := False;
	if Char = ' ' then
	    if Output_Object.Char_Count >= LINE_LENGTH - 10 and then
	        Output_Object.Column_Count < 4 then
		if Output_Object.HTML_Kind > HTML_3 then
		    -- Note: We leave the space here so that later code can tell
		    -- the difference between a line broken on a space, and a
		    -- line broken for because it's convinient.
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, " ");
		else -- No later code.
	            Ada.Text_IO.New_Line (Output_Object.Output_File);
		end if;
	        Output_Object.Char_Count := 0;
	        Output_Object.Last_was_Space := True;
	    else
	        Output_Text (Output_Object, " ");
	        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		--Output_Object.Disp_Large_Char_Count := <unchanged>;
	    end if;
	    -- Output_Object.Any_Nonspace := <unchanged>;
	    Output_Object.Last_was_Space := True;
	elsif Char = '<' then
	    Output_Text (Output_Object, "&lt;");
	    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 1;
	    Output_Object.Any_Nonspace := True;
        elsif Char = '>' then
	    Output_Text (Output_Object, "&gt;");
	    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 1;
	    Output_Object.Any_Nonspace := True;
        elsif Char = '"' then
	    Output_Text (Output_Object, "&quot;");
	    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	    -- No change in Disp_Large_Char_Count.
	    Output_Object.Any_Nonspace := True;
        elsif Char = '&' then
	    Output_Text (Output_Object, "&amp;");
	    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 1;
	    Output_Object.Any_Nonspace := True;
        elsif Char >= Character'Val(126) then -- All higher Latin-1 characters.
	    case Character'Pos(Char) is
	        when 160 =>
		    Output_Text (Output_Object, "&nbsp;");
	        when 161 =>
		    Output_Text (Output_Object, "&iexcl;");
	        when 162 =>
		    Output_Text (Output_Object, "&cent;");
	        when 163 =>
		    Output_Text (Output_Object, "&pound;");
	        when 164 =>
		    Output_Text (Output_Object, "&curren;");
	        when 165 =>
		    Output_Text (Output_Object, "&yen;");
	        when 166 =>
		    Output_Text (Output_Object, "&brvbar;");
	        when 167 =>
		    Output_Text (Output_Object, "&sect;");
	        when 168 =>
		    Output_Text (Output_Object, "&uml;");
	        when 169 =>
		    Output_Text (Output_Object, "&copy;");
	        when 170 =>
		    Output_Text (Output_Object, "&ordf;");
	        when 171 =>
		    Output_Text (Output_Object, "&laquo;");
	        when 172 =>
		    Output_Text (Output_Object, "&not;");
	        when 173 =>
		    Output_Text (Output_Object, "&shy;");
	        when 174 =>
		    Output_Text (Output_Object, "&reg;");
	        when 175 =>
		    Output_Text (Output_Object, "&macr;");
	        when 176 =>
		    Output_Text (Output_Object, "&deg;");
	        when 177 =>
		    Output_Text (Output_Object, "&plusmn;");
	        when 178 =>
		    Output_Text (Output_Object, "&sup2;");
	        when 179 =>
		    Output_Text (Output_Object, "&sup3;");
	        when 180 =>
		    Output_Text (Output_Object, "&acute;");
	        when 181 =>
		    Output_Text (Output_Object, "&micro;");
	        when 182 =>
		    Output_Text (Output_Object, "&para;");
	        when 183 =>
		    Output_Text (Output_Object, "&middot;");
	        when 184 =>
		    Output_Text (Output_Object, "&cedil;");
	        when 185 =>
		    Output_Text (Output_Object, "&sup1;");
	        when 186 =>
		    Output_Text (Output_Object, "&ordm;");
	        when 187 =>
		    Output_Text (Output_Object, "&raquo;");
	        when 188 =>
		    Output_Text (Output_Object, "&frac14;");
	        when 189 =>
		    Output_Text (Output_Object, "&frac12;");
	        when 190 =>
		    Output_Text (Output_Object, "&frac34;");
	        when 191 =>
		    Output_Text (Output_Object, "&iquest;");
	        when 192 =>
		    Output_Text (Output_Object, "&Agrave;");
	        when 193 =>
		    Output_Text (Output_Object, "&Aacute;");
	        when 194 =>
		    Output_Text (Output_Object, "&Acirc;");
	        when 195 =>
		    Output_Text (Output_Object, "&Atilde;");
	        when 196 =>
		    Output_Text (Output_Object, "&Auml;");
	        when 197 =>
		    Output_Text (Output_Object, "&Aring;");
	        when 198 =>
		    Output_Text (Output_Object, "&AElig;");
	        when 199 =>
		    Output_Text (Output_Object, "&Ccedil;");
	        when 200 =>
		    Output_Text (Output_Object, "&Egrave;");
	        when 201 =>
		    Output_Text (Output_Object, "&Eacute;");
	        when 202 =>
		    Output_Text (Output_Object, "&Ecirc;");
	        when 203 =>
		    Output_Text (Output_Object, "&Euml;");
	        when 204 =>
		    Output_Text (Output_Object, "&Igrave;");
	        when 205 =>
		    Output_Text (Output_Object, "&Iacute;");
	        when 206 =>
		    Output_Text (Output_Object, "&Icirc;");
	        when 207 =>
		    Output_Text (Output_Object, "&Iuml;");
	        when 208 =>
		    Output_Text (Output_Object, "&ETH;");
	        when 209 =>
		    Output_Text (Output_Object, "&Ntilde;");
	        when 210 =>
		    Output_Text (Output_Object, "&Ograve;");
	        when 211 =>
		    Output_Text (Output_Object, "&Oacute;");
	        when 212 =>
		    Output_Text (Output_Object, "&Ocirc;");
	        when 213 =>
		    Output_Text (Output_Object, "&Otilde;");
	        when 214 =>
		    Output_Text (Output_Object, "&Ouml;");
	        when 215 =>
		    Output_Text (Output_Object, "&times;");
	        when 216 =>
		    Output_Text (Output_Object, "&Oslash;");
	        when 217 =>
		    Output_Text (Output_Object, "&Ugrave;");
	        when 218 =>
		    Output_Text (Output_Object, "&Uacute;");
	        when 219 =>
		    Output_Text (Output_Object, "&Ucirc;");
	        when 220 =>
		    Output_Text (Output_Object, "&Uuml;");
	        when 221 =>
		    Output_Text (Output_Object, "&Yacute;");
	        when 222 =>
		    Output_Text (Output_Object, "&THORN;");
	        when 223 =>
		    Output_Text (Output_Object, "&szlig;");

	        when 224 =>
		    Output_Text (Output_Object, "&agrave;");
	        when 225 =>
		    Output_Text (Output_Object, "&aacute;");
	        when 226 =>
		    Output_Text (Output_Object, "&acirc;");
	        when 227 =>
		    Output_Text (Output_Object, "&atilde;");
	        when 228 =>
		    Output_Text (Output_Object, "&auml;");
	        when 229 =>
		    Output_Text (Output_Object, "&aring;");
	        when 230 =>
		    Output_Text (Output_Object, "&aelig;");
	        when 231 =>
		    Output_Text (Output_Object, "&ccedil;");
	        when 232 =>
		    Output_Text (Output_Object, "&egrave;");
	        when 233 =>
		    Output_Text (Output_Object, "&eacute;");
	        when 234 =>
		    Output_Text (Output_Object, "&ecirc;");
	        when 235 =>
		    Output_Text (Output_Object, "&euml;");
	        when 236 =>
		    Output_Text (Output_Object, "&igrave;");
	        when 237 =>
		    Output_Text (Output_Object, "&iacute;");
	        when 238 =>
		    Output_Text (Output_Object, "&icirc;");
	        when 239 =>
		    Output_Text (Output_Object, "&iuml;");
	        when 240 =>
		    Output_Text (Output_Object, "&eth;");
	        when 241 =>
		    Output_Text (Output_Object, "&ntilde;");
	        when 242 =>
		    Output_Text (Output_Object, "&ograve;");
	        when 243 =>
		    Output_Text (Output_Object, "&oacute;");
	        when 244 =>
		    Output_Text (Output_Object, "&ocirc;");
	        when 245 =>
		    Output_Text (Output_Object, "&otilde;");
	        when 246 =>
		    Output_Text (Output_Object, "&ouml;");
	        when 247 =>
		    Output_Text (Output_Object, "&divide;");
	        when 248 =>
		    Output_Text (Output_Object, "&oslash;");
	        when 249 =>
		    Output_Text (Output_Object, "&ugrave;");
	        when 250 =>
		    Output_Text (Output_Object, "&uacute;");
	        when 251 =>
		    Output_Text (Output_Object, "&ucirc;");
	        when 252 =>
		    Output_Text (Output_Object, "&uuml;");
	        when 253 =>
		    Output_Text (Output_Object, "&yacute;");
	        when 254 =>
		    Output_Text (Output_Object, "&thorn;");
	        when 255 =>
		    Output_Text (Output_Object, "&yuml;");


	        when others =>
		    declare
		        Code : constant String :=
			    Natural'Image(Character'Pos(Char));
		    begin
		        Output_Text (Output_Object, "&#" & Code(2..4) & ';');
		    end;
	    end case;
	    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	    if Ada.Strings.Maps.Is_In (Char, Large_Char_Set) then
	        Output_Object.Disp_Large_Char_Count :=
	            Output_Object.Disp_Large_Char_Count + 1;
	    -- else not a large character.
	    end if;
	    Output_Object.Any_Nonspace := True;
        else
	    Output_Text (Output_Object, Char & "");
	    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	    if Ada.Strings.Maps.Is_In (Char, Large_Char_Set) then
	        Output_Object.Disp_Large_Char_Count :=
	            Output_Object.Disp_Large_Char_Count + 1;
	    -- else not a large character.
	    end if;
	    Output_Object.Any_Nonspace := True;
        end if;
    end Ordinary_Character;


    procedure Hard_Space (Output_Object : in out HTML_Output_Type) is
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
        Output_Text (Output_Object, "&nbsp;");
        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	-- Output_Object.Disp_Large_Char_Count := <unchanged>;
	-- Output_Object.Any_Nonspace := <unchanged>;
	Output_Object.Last_was_Space := True;
        Output_Object.Conditional_Space := False; -- Never need a conditional space here.
    end Hard_Space;


    procedure Line_Break (Output_Object : in out HTML_Output_Type) is
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
	if Output_Object.Column_Count >= 4 then
	    -- Output is deferred; mark the end of an item.
	    if Output_Object.Column_Text (Output_Object.Current_Column) /= null and then
	       Output_Object.Column_Text (Output_Object.Current_Column).Item = Output_Object.Current_Item then
		Output_Object.Column_Text (Output_Object.Current_Column).End_Para := False;
	    end if;
	    Output_Object.Current_Item := Output_Object.Current_Item + 1;
            Output_Object.Char_Count := 0;
            Output_Object.Disp_Char_Count := 0;
	    Output_Object.Disp_Large_Char_Count := 0;
	    Output_Object.Any_Nonspace := False;
	    Output_Object.Last_Was_Space := True; -- Start of line.
	    Output_Object.Conditional_Space := False; -- Don't need it here.
	else -- Normal.
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "<BR>");
            Output_Object.Char_Count := 0;
            Output_Object.Disp_Char_Count := 0;
	    Output_Object.Disp_Large_Char_Count := 0;
	    Output_Object.Any_Nonspace := False;
	    Output_Object.Last_Was_Space := True; -- Start of line.
	    Output_Object.Conditional_Space := False; -- Don't need it here.
	end if;
    end Line_Break;


    procedure Index_Line_Break (Output_Object : in out HTML_Output_Type;
				Clear_Keep_with_Next : in Boolean) is
	-- Output a line break for the index. This does not start a new
	-- paragraph in terms of spacing. This corresponds to a "<BR>"
	-- in HTML. If Clear_Keep_with_Next is true, insure that the next
	-- line does not require the following line to stay with it.
	-- Raises Not_Valid_Error if the paragraph is not in the index format.
    begin
	if ARM_Output."/=" (Output_Object.Paragraph_Style, ARM_Output.Index) then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not index format");
	end if;
	Line_Break (Output_Object);
    end Index_Line_Break;


    procedure Soft_Line_Break (Output_Object : in out HTML_Output_Type) is
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
	if Output_Object.HTML_Kind > HTML_3 and then Output_Object.Use_Unicode then
            Output_Text (Output_Object, "&#8203;");
	-- else no Soft break in HTML 3.2.
	end if;
    end Soft_Line_Break;


    procedure Soft_Hyphen_Break (Output_Object : in out HTML_Output_Type) is
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
        null; -- Soft hyphens exist, but don't work on either Internet Exploder 4
	      -- or Netcrash 3. That is, they are always displayed. (They should
	      -- only be displayed at the location of a line break).
        --Output_Text (Output_Object, "&shy;"); -- A Latin-1 char.
    end Soft_Hyphen_Break;


    procedure Tab (Output_Object : in out HTML_Output_Type) is
	-- Output a tab, inserting space up to the next tab stop.
	-- Raises Not_Valid_Error if the paragraph was created with
	-- Tab_Stops = ARM_Output.NO_TABS.
    begin
	-- HTML does not have tabs. Emulation is not successful on proportional
	-- fonts, so we let the user select how to do it.
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

        Output_Object.Conditional_Space := False; -- Never need a conditional space here.
        Output_Object.Last_was_Space := True; -- Treat this as a space.
	if Output_Object.Tab_Emulation = Single_Space then
	    -- Don't emulate these, just use a single space.
	    Output_Text (Output_Object, "&nbsp;");
	    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	    -- Output_Object.Disp_Large_Char_Count := <unchanged>;
	elsif Output_Object.Tab_Emulation = Quad_Space then
	    -- Don't emulate these, just use a single space.
	    Output_Text (Output_Object, "&nbsp;&nbsp;&nbsp;&nbsp;");
	    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 4;
	    -- Output_Object.Disp_Large_Char_Count := <unchanged>;
	elsif Output_Object.Tab_Emulation = Emulate_Fixed_Only or else
	      Output_Object.Tab_Emulation = Emulate_Fixed_Only_Quad then
	    if Output_Object.Can_Emulate_Tabs or else
	        (not Output_Object.Any_Nonspace) then -- Always can emulate if they're first on a line.
	        Output_Text (Output_Object, "&nbsp;");
	        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	        -- Output_Object.Disp_Large_Char_Count := <unchanged>;
	        for I in 1 .. Output_Object.Tab_Stops.Number loop
	            if Output_Object.Tab_Stops.Stops(I).Stop >= Output_Object.Disp_Char_Count then
		        for J in Output_Object.Disp_Char_Count+1 .. Output_Object.Tab_Stops.Stops(I).Stop loop
		            Output_Text (Output_Object, "&nbsp;");
		            Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
			    -- Output_Object.Disp_Large_Char_Count := <unchanged>;
		        end loop;
		        exit;
	            end if;
	        end loop; -- If we drop out without finding a tab, we just use the single
		          -- space already written.
	    elsif Output_Object.Tab_Emulation = Emulate_Fixed_Only then
	        -- Put in a space.
	        Output_Text (Output_Object, "&nbsp;");
	        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	        if ARM_Output."=" (Output_Object.Paragraph_Style, ARM_Output.Syntax_Summary) and then
		    Output_Object.Column_Count > 1 then
		    -- Special case (hack!) to make Syntax cross-reference look better:
	            Output_Text (Output_Object, "&nbsp;&nbsp;");
	            Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 2;
	        end if;
		-- Output_Object.Disp_Large_Char_Count := <unchanged>;
	    elsif Output_Object.Tab_Emulation = Emulate_Fixed_Only_Quad then
	        -- Put in four hard spaces.
	        Output_Text (Output_Object, "&nbsp;&nbsp;&nbsp;&nbsp;");
	        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 4;
		-- Output_Object.Disp_Large_Char_Count := <unchanged>;
	    end if;
	else -- Emulate all.
	    Output_Text (Output_Object, "&nbsp;");
	    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	    for I in 1 .. Output_Object.Tab_Stops.Number loop
	        if Output_Object.Tab_Stops.Stops(I).Stop >= Output_Object.Disp_Char_Count then
		    for J in Output_Object.Disp_Char_Count+1 .. Output_Object.Tab_Stops.Stops(I).Stop loop
		        Output_Text (Output_Object, "&nbsp;");
		        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		    end loop;
		    exit;
	        end if;
	    end loop; -- If we drop out without finding a tab, we just use the
		      -- single space already written.
	    -- Output_Object.Disp_Large_Char_Count := <unchanged>;
	end if;
    end Tab;


    procedure Special_Character (Output_Object : in out HTML_Output_Type;
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
	if Output_Object.Conditional_Space then
	    Output_Object.Conditional_Space := False;
	    Output_Text (Output_Object, " ");
	    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	    -- Output_Object.Disp_Large_Char_Count := <unchanged>;
	end if;
	case Char is
	    when ARM_Output.EM_Dash =>
		if Output_Object.HTML_Kind > HTML_3 and Output_Object.Use_Unicode then
	            Output_Text (Output_Object, "&mdash;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 1;
        	else
	            Output_Text (Output_Object, "--");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 2;
		    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 2;
		end if;
	    when ARM_Output.EN_Dash =>
		if Output_Object.HTML_Kind > HTML_3 and Output_Object.Use_Unicode then
		    Output_Text (Output_Object, "&ndash;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 1;
		else
		    Output_Text (Output_Object, "-");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		    --Output_Object.Disp_Large_Char_Count := <unchanged>;
		end if;
	    when ARM_Output.GEQ =>
		if Output_Object.HTML_Kind > HTML_3 and Output_Object.Use_Unicode then
	            Output_Text (Output_Object, "&ge;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 1;
		else
	            Output_Text (Output_Object, ">=");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 2;
		    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 2;
		end if;
	    when ARM_Output.LEQ =>
		if Output_Object.HTML_Kind > HTML_3 and Output_Object.Use_Unicode then
	            Output_Text (Output_Object, "&le;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 1;
		else
	            Output_Text (Output_Object, "<=");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 2;
		    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 2;
		end if;
	    when ARM_Output.NEQ =>
		if Output_Object.HTML_Kind > HTML_3 and Output_Object.Use_Unicode then
	            Output_Text (Output_Object, "&ne;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 1;
		else
	            Output_Text (Output_Object, "/=");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 2;
		    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 2;
		end if;
	    when ARM_Output.PI =>
		if Output_Object.HTML_Kind > HTML_3 and Output_Object.Use_Unicode then
	            Output_Text (Output_Object, "&pi;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 1;
		else
	            Output_Text (Output_Object, "PI");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 2;
		    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 2;
		end if;
	    when ARM_Output.Left_Ceiling =>
		if FALSE and (Output_Object.HTML_Kind > HTML_3 and Output_Object.Use_Unicode) then
		    -- This character doesn't display on US Windows 2000/XP.
	            Output_Text (Output_Object, "&lceil;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 1;
		else
	            Output_Text (Output_Object, "<I>Ceiling</I>(");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 8;
		    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 1;
		end if;
	    when ARM_Output.Right_Ceiling =>
		if FALSE and (Output_Object.HTML_Kind > HTML_3 and Output_Object.Use_Unicode) then
		    -- This character doesn't display on US Windows 2000/XP.
	            Output_Text (Output_Object, "&rceil;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 1;
		else
	            Output_Text (Output_Object, ")");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		    --Output_Object.Disp_Large_Char_Count := <unchanged>;
		end if;
	    when ARM_Output.Left_Floor =>
		if FALSE and (Output_Object.HTML_Kind > HTML_3 and Output_Object.Use_Unicode) then
		    -- This character doesn't display on US Windows 2000/XP.
	            Output_Text (Output_Object, "&lfloor;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 1;
		else
	            Output_Text (Output_Object, "<I>Floor</I>(");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 6;
		    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 1;
		end if;
	    when ARM_Output.Right_Floor =>
		if FALSE and (Output_Object.HTML_Kind > HTML_3 and Output_Object.Use_Unicode) then
		    -- This character doesn't display on US Windows 2000/XP.
	            Output_Text (Output_Object, "&rfloor;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		    Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 1;
		else
	            Output_Text (Output_Object, ")");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		    --Output_Object.Disp_Large_Char_Count := <unchanged>;
		end if;
	    when ARM_Output.Thin_Space =>
		if FALSE and (Output_Object.HTML_Kind > HTML_3 and Output_Object.Use_Unicode) then
		    -- This character doesn't display on US Windows 2000/XP.
	            Output_Text (Output_Object, "&thinsp;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, " ");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		end if;
	        --Output_Object.Disp_Large_Char_Count := <unchanged>;
	    when ARM_Output.Left_Quote =>
		if Output_Object.HTML_Kind > HTML_3 then
	            Output_Text (Output_Object, "&lsquo;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, "`");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		end if;
	        --Output_Object.Disp_Large_Char_Count := <unchanged>;
	    when ARM_Output.Right_Quote =>
		if Output_Object.HTML_Kind > HTML_3 then
	            Output_Text (Output_Object, "&rsquo;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, "'");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		end if;
	        --Output_Object.Disp_Large_Char_Count := <unchanged>;
	    when ARM_Output.Left_Double_Quote =>
		if Output_Object.HTML_Kind > HTML_3 then
	            Output_Text (Output_Object, "&ldquo;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, """");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		end if;
	        --Output_Object.Disp_Large_Char_Count := <unchanged>;
	    when ARM_Output.Right_Double_Quote =>
		if Output_Object.HTML_Kind > HTML_3 then
	            Output_Text (Output_Object, "&rdquo;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, """");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		end if;
	        --Output_Object.Disp_Large_Char_Count := <unchanged>;
	    when ARM_Output.Small_Dotless_I =>
		if Output_Object.HTML_Kind > HTML_3 then --and Output_Object.Use_Unicode then -- We'll use it if it might be supported.
	            Output_Text (Output_Object, "&#0305;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, "i");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		end if;
	        --Output_Object.Disp_Large_Char_Count := <unchanged>;
	    when ARM_Output.Capital_Dotted_I =>
		if Output_Object.HTML_Kind > HTML_3 then --and Output_Object.Use_Unicode then -- We'll use it if it might be supported.
	            Output_Text (Output_Object, "&#0304;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, "I");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		end if;
	        Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 1;
	end case;
	Output_Object.Any_Nonspace := True;
	Output_Object.Last_was_Space := False;
    end Special_Character;


    procedure Unicode_Character (Output_Object : in out HTML_Output_Type;
			         Char : in ARM_Output.Unicode_Type) is
	-- Output a Unicode character, with code position Char.
	Char_Code : constant String := ARM_Output.Unicode_Type'Image(Char);
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
	if Output_Object.HTML_Kind = HTML_3 then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Unicode not available for HTML 3");
	end if;
	if Output_Object.Conditional_Space then
	    Output_Object.Conditional_Space := False;
	    Output_Text (Output_Object, " ");
	    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	    --Output_Object.Disp_Large_Char_Count := <unchanged>;
	end if;
	-- We don't check if this is valid, we just use it. So be sparing!
        Output_Text (Output_Object, "&#" & Char_Code(2..Char_Code'Length) & ';');
	Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	Output_Object.Disp_Large_Char_Count := Output_Object.Disp_Large_Char_Count + 1; -- Assume it is large.
	Output_Object.Any_Nonspace := True;
	Output_Object.Last_was_Space := False;
    end Unicode_Character;


    procedure End_Hang_Item (Output_Object : in out HTML_Output_Type) is
	-- Marks the end of a hanging item. Call only once per paragraph.
	-- Raises Not_Valid_Error if the paragraph style is not in
	-- Text_Prefixed_Style_Subtype, or if this has already been
	-- called for the current paragraph, or if the paragraph was started
	-- with No_Prefix = True.
    begin
--Ada.Text_IO.Put_Line("HTML: End_Hang");
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
	if Output_Object.Paragraph_Style not in
	     ARM_Output.Text_Prefixed_Style_Subtype then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not a hanging paragraph - " & ARM_Output.Paragraph_Style_Type'Image(Output_Object.Paragraph_Style));
	end if;
	if Output_Object.Saw_Hang_End then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Already saw the end of the hanging part");
	end if;
	Output_Object.Saw_Hang_End := True;
	if Output_Object.HTML_Kind = HTML_3 then
	    case Output_Object.Paragraph_Style is
	        -- Part of a definition list.
		when ARM_Output.Small_Giant_Hanging | ARM_Output.Small_Wide_Hanging |
		     ARM_Output.Small_Medium_Hanging | ARM_Output.Small_Narrow_Hanging |
		     ARM_Output.Small_Hanging_in_Bulleted |
		     ARM_Output.Small_Enumerated =>
		    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</FONT><DD><FONT SIZE=-1>");
		when others =>
		    Ada.Text_IO.Put_Line (Output_Object.Output_File, "<DD>");
	    end case;
	elsif Output_Object.HTML_Kind = HTML_4_Only then
	    declare
		Saved_Format : ARM_Output.Format_Type;
	    begin
		-- Save original format:
		Saved_Format :=
		      (Bold => Output_Object.Is_Bold,
		       Italic => Output_Object.Is_Italic,
		       Font => Output_Object.Font,
		       Size => Output_Object.Size,
		       Color => Output_Object.Color,
		       Change => Output_Object.Change,
		       Version => Output_Object.Version,
		       Added_Version => Output_Object.Added_Version,
		       Location => Output_Object.Location);

		-- Close any open formatting (can't leave it open across a DIV):
		Text_Format (Output_Object,
			     Format => (Bold => False,
				        Italic => False,
				        Font => ARM_Output.Default,
				        Size => 0,
				        Color => ARM_Output.Default,
				        Change => ARM_Output.None,
				        Version => '0',
				        Added_Version => '0',
				        Location => ARM_Output.Normal));
		-- This has to be a hanging style, so we ignore other cases:
	        Ada.Text_IO.Put (Output_Object.Output_File, "</div><div class=""" &
		    Paragraph_Name (Output_Object.Paragraph_Style, Output_Object.Paragraph_Indent) & "-Body"">");
	        -- If the prefix is too long, add a <BR>. A "unit" is 2.0 ems;
	        -- a large character is 0.65 ems; and a small character is 0.4 ems.
	        -- That should be quite conservative.
--Ada.Text_IO.Put_Line("Break hang check: large chars=" & Natural'Image(Output_Object.Disp_Large_Char_Count) &
--" small chars=" & Natural'Image(Output_Object.Disp_Char_Count - Output_Object.Disp_Large_Char_Count) &
--" Hang_Outdent=" & Natural'Image(Paragraph_Info(Output_Object.Paragraph_Format).Hang_Outdent));
	        if (Output_Object.Disp_Large_Char_Count*13) +
	           ((Output_Object.Disp_Char_Count-Output_Object.Disp_Large_Char_Count)*8) >
	           Paragraph_Info(Output_Object.Paragraph_Style, Output_Object.Paragraph_Indent).Hang_Outdent*40 then
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, "<br clear=""left"">");
			-- We use "clear=left" so that the next line always
			-- starts at the left margin. This shouldn't be necessary,
			-- but I've seen cases where it was.
	        else
		    Ada.Text_IO.New_Line (Output_Object.Output_File);
		end if;
		-- Reopen any formatting (using the previously saved values):
		Text_Format (Output_Object, Format => Saved_Format);
	    end;
	else -- HTML 4 Compatibility
	    -- We have to close and reopen the font info here, so that we
	    -- properly nest these operations to pass the WC3 validator.
	    Put_End_Compatibility_Font_Info (Output_Object,
			Output_Object.Paragraph_Style,
			Output_Object.Paragraph_Indent);
	    -- This has to be a hanging style, so we ignore other cases:
	    Ada.Text_IO.Put (Output_Object.Output_File, "<dd class=""" &
		    Paragraph_Name (Output_Object.Paragraph_Style, Output_Object.Paragraph_Indent) & """>");
	    Put_Compatibility_Font_Info (Output_Object,
			Output_Object.Paragraph_Style,
			Output_Object.Paragraph_Indent);
	end if;
	Paragraph_Used(Output_Object.Paragraph_Style, Output_Object.Paragraph_Indent) := True;
        Output_Object.Char_Count := 0;
	Output_Object.Disp_Char_Count := 0;
	Output_Object.Disp_Large_Char_Count := 0;
	Output_Object.Any_Nonspace := False;
        Output_Object.Last_Was_Space := True; -- Start of line.
        Output_Object.Conditional_Space := False; -- Don't need it here.
    end End_Hang_Item;


    procedure New_Column (Output_Object : in out HTML_Output_Type) is
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
		"Not in a multi-column area");
	end if;
	if Output_Object.Column_Count >= 4 then
	    Output_Object.Current_Column := Output_Object.Current_Column + 1;
	    Output_Object.Current_Item := 1;
	-- else ignore it, no columns will be used.
	end if;
    end New_Column;


    procedure Text_Format (Output_Object : in out HTML_Output_Type;
			   Format : in ARM_Output.Format_Type) is
	-- Change the text format so that all of the properties are as specified.
	-- Note: Changes to these properties ought be stack-like; that is,
	-- Bold on, Italic on, Italic off, Bold off is OK; Bold on, Italic on,
	-- Bold off, Italic off should be avoided (as separate commands).
	use type ARM_Output.Change_Type;
	use type ARM_Output.Location_Type;
	use type ARM_Output.Size_Type;
	use type ARM_Output.Color_Type;

	function Change_Needs_Close_or_Open return Boolean is
	    -- Returns True if "Change" needs to open or close something, based
	    -- on the current values of Output_Object.
	begin
	    return (Format.Change /= Output_Object.Change or else
		    Format.Version /= Output_Object.Version or else
		    Format.Added_Version /= Output_Object.Added_Version);
	end Change_Needs_Close_or_Open;

	function Font_Needs_Close_or_Open return Boolean is
	    -- Returns True if "Font" needs to close something, based
	    -- on the current values of Output_Object and the new value.
	    -- Note that this depends on whether the Change needs to open
	    -- or close something; if it does, we need to close and reopen
	    -- the font even if it is not changing.
	begin
	    return (ARM_Output."/=" (Format.Font, Output_Object.Font) or else
		    Change_Needs_Close_or_Open);
	end Font_Needs_Close_or_Open;

	function Location_Needs_Close_or_Open return Boolean is
	    -- Returns True if "Location" needs to close something, based
	    -- on the current values of Output_Object and the new value.
	    -- Note that this depends on whether the Change or Font needs
	    -- to open or close something; if they do, we need to close and
	    -- reopen the location even if it is not changing.
	begin
	    return Format.Location /= Output_Object.Location or else
		   Change_Needs_Close_or_Open or else Font_Needs_Close_or_Open;
	end Location_Needs_Close_or_Open;

	function Color_Needs_Close_or_Open return Boolean is
	    -- Returns True if "Color" needs to close something, based
	    -- on the current values of Output_Object, and the new value.
	    -- Note that this depends on whether the Change, Font,
	    -- or Location needs to open or close something; if they do,
	    -- we need to close the size even if it is not changing.
	begin
	    return (Format.Color /= Output_Object.Color or else
		    Change_Needs_Close_or_Open or else Font_Needs_Close_or_Open or else
		    Location_Needs_Close_or_Open);
	end Color_Needs_Close_or_Open;

	function Size_Needs_Close_or_Open return Boolean is
	    -- Returns True if "Size" needs to close something, based
	    -- on the current values of Output_Object, and the new value.
	    -- Note that this depends on whether the Change, Color, Font,
	    -- or Location needs to open or close something; if they do,
	    -- we need to close the size even if it is not changing.
	begin
	    return (Format.Size /= Output_Object.Size or else
		    Change_Needs_Close_or_Open or else Font_Needs_Close_or_Open or else
		    Location_Needs_Close_or_Open or else Color_Needs_Close_or_Open);
	end Size_Needs_Close_or_Open;

	function Italic_Needs_Close_or_Open return Boolean is
	    -- Returns True if "Italic" needs to close something, based
	    -- on the current values of Output_Object, and the new value.
	    -- Note that this depends on whether the Change, Font, Color,
	    -- Location, or Size needs to open or close something; if they do,
	    -- we need to close the italics even if it is not changing.
	begin
	    return (Format.Italic /= Output_Object.Is_Italic or else
	    Change_Needs_Close_or_Open or else Font_Needs_Close_or_Open or else
	    Location_Needs_Close_or_Open or else Size_Needs_Close_or_Open or else
	    Color_Needs_Close_or_Open);
	end Italic_Needs_Close_or_Open;

    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
	-- We do these in this order so that the changes are stacked properly.
	-- Note that we have to open and close stuff that is not changing
	-- in order to get proper nesting in all cases.

	if Output_Object.Is_Bold and then
            ((not Format.Bold) or else
	    Italic_Needs_Close_or_Open) then
	    -- The latter so that nesting is preserved; we'll reopen
	    -- the boldfacing on the other side if needed. Otherwise, when
	    -- Bold remains on, we'd leave the markup open but close some outer
	    -- item. That's wrong (even though many browsers can handle it).
	    Output_Text (Output_Object, "</B>");
	    Output_Object.Is_Bold := False;
	end if;

	if Output_Object.Is_Italic and then
            ((not Format.Italic) or else
	    Size_Needs_Close_or_Open) then
	    -- The latter so that nesting is preserved; we'll reopen
	    -- the italics on the other side in that case.
	    Output_Text (Output_Object, "</I>");
	    Output_Object.Is_Italic := False;
	end if;

	if Format.Size /= Output_Object.Size or else
	    Color_Needs_Close_or_Open then
	    -- The latter so that nesting is preserved; we'll reopen
	    -- the size on the other side in that case.
	    if Output_Object.Size /= 0 then
	        if Output_Object.HTML_Kind = HTML_4_Only then
	            Output_Text (Output_Object, "</SPAN>");
		else
	            Output_Text (Output_Object, "</FONT>");
		end if;
	        Output_Object.Size := 0; -- That's the size now.
	    end if;
	end if;

	if Format.Color /= Output_Object.Color or else
	    Location_Needs_Close_or_Open then
	    -- The latter so that nesting is preserved; we'll reopen
	    -- the size on the other side in that case.
	    if Output_Object.Color /= ARM_Output.Default then
	        if Output_Object.HTML_Kind = HTML_4_Only then
	            Output_Text (Output_Object, "</SPAN>");
		else
	            Output_Text (Output_Object, "</FONT>");
		end if;
	        Output_Object.Color := ARM_Output.Default; -- That's the color now.
	    end if;
	end if;

	if Format.Location /= Output_Object.Location or else
	    Change_Needs_Close_or_Open then
	    -- The latter so that nesting is preserved; we'll reopen
	    -- the location on the other side in that case.
	    case Output_Object.Location is
		when ARM_Output.Superscript =>
		    if Output_Object.HTML_Kind = HTML_4_Only then
		        Output_Text (Output_Object, "</SPAN></SUP>");
		    else
		        Output_Text (Output_Object, "</FONT></SUP>");
		    end if;
		when ARM_Output.Subscript =>
		    if Output_Object.HTML_Kind = HTML_4_Only then
		        Output_Text (Output_Object, "</SPAN></SUB>");
		    else
		        Output_Text (Output_Object, "</FONT></SUB>");
		    end if;
		when ARM_Output.Normal =>
		    null;
	    end case;
	    Output_Object.Location := ARM_Output.Normal; -- That's the location now.
	end if;

	if ARM_Output."/=" (Format.Font, Output_Object.Font) or else
	    Change_Needs_Close_or_Open then
	    -- The latter so that nesting is preserved; we'll reopen
	    -- the font on the other side in that case.
	    case Output_Object.Font is
		when ARM_Output.Default => null;
		when ARM_Output.Fixed =>
		    Output_Text (Output_Object, "</TT>");
		when ARM_Output.Roman =>
		    if Output_Object.HTML_Kind = HTML_4_Only then
		        Output_Text (Output_Object, "</SPAN>");
		    else
			null; -- Default, currently.
		        --Output_Text (Output_Object, "</FONT>");
		    end if;
		when ARM_Output.Swiss =>
		    if Output_Object.HTML_Kind = HTML_4_Only then
		        Output_Text (Output_Object, "</SPAN>");
		    else
		        Output_Text (Output_Object, "</FONT>");
		    end if;
	    end case;
	    Output_Object.Font := ARM_Output.Default; -- We're now in the default state.
	end if;

	if Format.Change /= Output_Object.Change or else
	   Format.Version /= Output_Object.Version or else
	   Format.Added_Version /= Output_Object.Added_Version then
	    case Output_Object.Change is
		when ARM_Output.Insertion =>
		    if Output_Object.HTML_Kind = HTML_3 then
		        Output_Text (Output_Object, "</U>");
		    else
		        --Output_Text (Output_Object, "</ins>");
		        Output_Text (Output_Object, "</span>");
		    end if;
		    -- Note: We need to follow these with a space so that
		    -- we don't get words running together for indexing
		    -- purposes (Google, Ada Indexer). That's only a concern
		    -- for deletions directly following insertions (at least in
		    -- the absence of nesting), so we only add the extra space
		    -- after insertions. RTF needs insertions and deletions
		    -- without spaces to work properly, thus the source does not
		    -- have them.
		    -- If the last character of the displayed text is a space,
		    -- we don't need this, and don't generate it. We do
		    -- generate it for punctuation, as we want a space following
		    -- that in general.
		    -- If the next visible character is not in some sort of
		    -- change section, we'd prefer to not generate
		    -- the space, but there is no obvious way to determine that
		    -- (we don't know when the command ends here).
		    -- We can, however, generate a conditional space that is
		    -- not generated if the next visible character is a space
		    -- or punctuation (we don't usually want a space *before*
		    -- punctuation).
		    if Output_Object.Last_was_Space then
			null;
		    else
			Output_Object.Conditional_Space := True;
		    end if;
		when ARM_Output.Deletion =>
		    if Output_Object.HTML_Kind = HTML_3 then
		        Output_Text (Output_Object, "</S>");
		    else
		        --Output_Text (Output_Object, "</del>");
		        Output_Text (Output_Object, "</span>");
		    end if;
		when ARM_Output.Both =>
		    if Output_Object.HTML_Kind = HTML_3 then
		        Output_Text (Output_Object, "</S></U>");
		    else
		        --Output_Text (Output_Object, "</del></ins>");
		        --Output_Text (Output_Object, "</span>");
			-- CSS2 doesn't allow multiple decorations in a single definition, so we have
			-- to nest them. But that might not be right, either (it works on IE).
		        Output_Text (Output_Object, "</span></span>");
		    end if;
		    if Output_Object.Last_was_Space then -- See above for reasons for this.
			null;
		    else
			Output_Object.Conditional_Space := True;
		    end if;
		when ARM_Output.None =>
		    null;
	    end case;
	    case Format.Change is
		when ARM_Output.Insertion =>
		    if Output_Object.HTML_Kind = HTML_3 then
		        Output_Text (Output_Object, "<U>");
		    else
		        --Output_Text (Output_Object, "<ins>");
		        Output_Text (Output_Object, "<span class=""insert" & Format.Version & """>");
			Revision_Used(Format.Version) := True;
		    end if;
		when ARM_Output.Deletion =>
		    if Output_Object.HTML_Kind = HTML_3 then
		        Output_Text (Output_Object, "<S>");
		    else
		        --Output_Text (Output_Object, "<del>");
		        Output_Text (Output_Object, "<span class=""delete" & Format.Version & """>");
			Revision_Used(Format.Version) := True;
		    end if;
		when ARM_Output.Both =>
		    if Output_Object.HTML_Kind = HTML_3 then
		        Output_Text (Output_Object, "<U><S>");
		    else
		        --Output_Text (Output_Object, "<ins><del>");
		        --Output_Text (Output_Object, "<span class=""both" & Format.Version & """>");
			-- CSS2 doesn't allow multiple decorations in a single definition, so we have
			-- to nest them. But that might not be right, either (it works on IE).
		        Output_Text (Output_Object, "<span class=""insert" & Format.Added_Version & """>");
		        Output_Text (Output_Object, "<span class=""delete" & Format.Version & """>");
			Revision_Used(Format.Added_Version) := True;
			Revision_Used(Format.Version) := True;
		    end if;
		when ARM_Output.None =>
		    null;
	    end case;
	    Output_Object.Change := Format.Change;
	    Output_Object.Version := Format.Version;
	    Output_Object.Added_Version := Format.Added_Version;
	end if;

	if ARM_Output."/=" (Format.Font, Output_Object.Font) then
	    case Format.Font is
		when ARM_Output.Default => null;
		when ARM_Output.Fixed =>
		    Output_Text (Output_Object, "<TT>");
		when ARM_Output.Roman =>
		    if Output_Object.HTML_Kind = HTML_4_Only then
		        Output_Text (Output_Object, "<SPAN Class=""roman"">");
		    else
			null; -- Default, currently.
		        --Output_Text (Output_Object, "<FONT xxx>");
		    end if;
		when ARM_Output.Swiss =>
		    if Output_Object.HTML_Kind = HTML_4_Only then
		        Output_Text (Output_Object, "<SPAN Class=""swiss"">");
		    else
		        Output_Text (Output_Object, SWISS_FONT_CODE);
		    end if;
	    end case;
	    Output_Object.Font := Format.Font;
	end if;

	if Format.Location /= Output_Object.Location then
	    -- Note: Location needs to be changed before size, as they
	    -- typically are changed together, and <SUP> and <SUB> reset the
	    -- size.
	    case Format.Location is
		when ARM_Output.Superscript =>
		    if Output_Object.HTML_Kind = HTML_4_Only then
		        Output_Text (Output_Object, "<SUP><SPAN STYLE=""font-size: 140%"">");
			   -- This is a bit larger than +1; the text is usually too small.
		    else
		        Output_Text (Output_Object, "<SUP><FONT SIZE=""+1"">");
		    end if;
		when ARM_Output.Subscript =>
		    if Output_Object.HTML_Kind = HTML_4_Only then
		        Output_Text (Output_Object, "<SUB><SPAN STYLE=""font-size: 140%"">");
			   -- This is a bit larger than +1; the text is usually too small.
		    else
		        Output_Text (Output_Object, "<SUB><FONT SIZE=""+1"">");
		    end if;
		when ARM_Output.Normal =>
		    null;
	    end case;
	    Output_Object.Location := Format.Location;
	end if;

	if Format.Color /= Output_Object.Color then
	    if Output_Object.HTML_Kind = HTML_4_Only then
		case Format.Color is
		    when ARM_Output.Default => null;
		    when ARM_Output.Black =>
			Output_Text (Output_Object, "<SPAN STYLE=""color: rgb(0,0,0)"">");
		    when ARM_Output.Red   =>
			Output_Text (Output_Object, "<SPAN STYLE=""color: rgb(153,0,0)"">");
		    when ARM_Output.Green =>
			Output_Text (Output_Object, "<SPAN STYLE=""color: rgb(0,153,0)"">");
		    when ARM_Output.Blue  =>
			Output_Text (Output_Object, "<SPAN STYLE=""color: rgb(0,0,153)"">");
		end case;
	    else
		case Format.Color is
		    when ARM_Output.Default => null;
		    when ARM_Output.Black =>
	                Output_Text (Output_Object, "<FONT COLOR=""#000000"">");
		    when ARM_Output.Red   =>
	                Output_Text (Output_Object, "<FONT COLOR=""#990000"">");
		    when ARM_Output.Green =>
	                Output_Text (Output_Object, "<FONT COLOR=""#009900"">");
		    when ARM_Output.Blue  =>
	                Output_Text (Output_Object, "<FONT COLOR=""#000099"">");
		end case;
	    end if;
	    Output_Object.Color := Format.Color;
	end if;

	if Format.Size /= Output_Object.Size then
	    if Output_Object.HTML_Kind = HTML_4_Only then
		case Format.Size is
		    when 0 => null; -- Do nothing.
		    when 1 => Output_Text (Output_Object, "<SPAN STYLE=""font-size: 125%"">");
		    when 2 => Output_Text (Output_Object, "<SPAN STYLE=""font-size: 156%"">");
		    when 3 => Output_Text (Output_Object, "<SPAN STYLE=""font-size: 194%"">");
		    when 4 => Output_Text (Output_Object, "<SPAN STYLE=""font-size: 244%"">");
		    when 5 => Output_Text (Output_Object, "<SPAN STYLE=""font-size: 305%"">");
		    when -1 => Output_Text (Output_Object, "<SPAN STYLE=""font-size: 80%"">");
		    when -2 => Output_Text (Output_Object, "<SPAN STYLE=""font-size: 64%"">");
		    when -3 => Output_Text (Output_Object, "<SPAN STYLE=""font-size: 51%"">");
		    when -4 => Output_Text (Output_Object, "<SPAN STYLE=""font-size: 41%"">");
		    when -5 => Output_Text (Output_Object, "<SPAN STYLE=""font-size: 33%"">");
		    when others =>
			-- Too much change:
			if Format.Size > 0 then
			    Output_Text (Output_Object, "<SPAN STYLE=""font-size: 305%"">");
			else
			    Output_Text (Output_Object, "<SPAN STYLE=""font-size: 33%"">");
			end if;
		end case;
	    else
	        -- HTML sizes are 1..7, with a default of 3. So we limit the changes.
	        if Format.Size > 0 then
		    if Format.Size > 5 then
	                Output_Text (Output_Object, "<FONT SIZE=""+5"">");
		    else
	                Output_Text (Output_Object, "<FONT SIZE=""+" &
		            Character'Val(Format.Size + Character'Pos('0')) & """>");
		    end if;
	        elsif Format.Size < 0 then
		    if Format.Size < -4 then
	                Output_Text (Output_Object, "<FONT SIZE=""-4"">");
		    else
	                Output_Text (Output_Object, "<FONT SIZE=""-" &
		            Character'Val(abs Format.Size + Character'Pos('0')) & """>");
		    end if;
	        -- else Format.Size=0, nothing to do.
	        end if;
	    end if;
	    Output_Object.Size := Format.Size;
	end if;

	if Format.Italic and (not Output_Object.Is_Italic) then
	    Output_Text (Output_Object, "<I>");
	    Output_Object.Is_Italic := True;
	end if;
	if Format.Bold and (not Output_Object.Is_Bold) then
	    Output_Text (Output_Object, "<B>");
	    Output_Object.Is_Bold := True;
	end if;

    end Text_Format;


    procedure Clause_Reference (Output_Object : in out HTML_Output_Type;
				Text : in String;
				Clause_Number : in String) is
	-- Generate a reference to a clause in the standard. The text of
	-- the reference is "Text", and the number of the clause is
	-- Clause_Number. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
	if Clause_Number = "X.X" then
	    -- Link to a dead clause, just output the text (presumably this
	    -- is deleted).
            Ordinary_Text (Output_Object, Text);
	else
	    Output_Text (Output_Object, "<A HREF=""");
	    declare
	        Name : constant String :=
		    Make_Clause_Link_Name (Output_Object, Clause_Number);
	    begin
	        Output_Text (Output_Object, Name);
	    end;
	    Output_Text (Output_Object, """>");
            Ordinary_Text (Output_Object, Text);
	    Output_Text (Output_Object, "</A>");
	end if;
    end Clause_Reference;


    procedure Index_Target (Output_Object : in out HTML_Output_Type;
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
	-- Insert an anchor:
	Output_Text (Output_Object, "<A NAME=""I");
	declare
	    Key_Name : constant String := Natural'Image(Index_Key);
	begin
	    Output_Text (Output_Object, Key_Name(2..Key_Name'Last));
	end;
	Output_Text (Output_Object, """></A>");
    end Index_Target;


    procedure Index_Reference (Output_Object : in out HTML_Output_Type;
			       Text : in String;
			       Index_Key : in Natural;
			       Clause_Number : in String) is
	-- Generate a reference to an index target in the standard. The text
	-- of the reference is "Text", and Index_Key and Clause_Number denotes
	-- the target. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
	Output_Text (Output_Object, "<A HREF=""");
	if Output_Object.Big_Files then
	    null; -- No file name needed, this is a self-reference.
	else
	    if Output_Object.DOS_Filenames then
	        Output_Text (Output_Object,
			     Make_Clause_File_Name (Output_Object, Clause_Number)
				 & ".HTM");
	    else
	        Output_Text (Output_Object,
			     Make_Clause_File_Name (Output_Object, Clause_Number)
				 & ".html");
	    end if;
	end if;
	Output_Text (Output_Object, "#I");
	declare
	    Key_Name : constant String := Natural'Image(Index_Key);
	begin
	    Output_Text (Output_Object, Key_Name(2..Key_Name'Last));
	end;
	Output_Text (Output_Object, """>");
	Ordinary_Text (Output_Object, Text);
	Output_Text (Output_Object, "</A>");
    end Index_Reference;


    procedure DR_Reference (Output_Object : in out HTML_Output_Type;
			    Text : in String;
			    DR_Number : in String) is
	-- Generate a reference to an DR from the standard. The text
	-- of the reference is "Text", and DR_Number denotes
	-- the target. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
	declare
	    Num : Integer := Integer'Value(DR_Number(DR_Number'Last-3 .. DR_Number'Last));
	begin
	    Output_Text (Output_Object, "<A HREF=""");
	    if Num <= 93 then -- In Defect Reports 1. -- %%%% Update if changed.
		Output_Text (Output_Object, "defect1.html");
	    else -- In Defect Reports 2.
		Output_Text (Output_Object, "defect2.html");
	    end if;
	    Output_Text (Output_Object, "#");
	    Output_Text (Output_Object, DR_Number);
	end;
        Output_Text (Output_Object, """>");
        Ordinary_Text (Output_Object, Text);
        Output_Text (Output_Object, "</A>");
    end DR_Reference;


    function Folded_AI95_Number (AI_String : in String) return String is
	-- Internal routine.
	-- Calculate the "folded" AI number from the full version.
	-- AI_String should be in the form "AIzz-00xxx-yy", where -yy, 00,
	-- and zz are optional, and 'zz' = 95 if given.
	Result : String(1..5);
	Hyphen_1 : Natural := Ada.Strings.Fixed.Index (AI_String, "-");
	Hyphen_2 : Natural;
    begin
        if Hyphen_1 = 0 or else AI_String'Last < Hyphen_1+3 then
	    Result := "00001";
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
	        "Bad AI reference " & AI_String);
	elsif Hyphen_1 = AI_String'First+4 and then
	    AI_String(AI_String'First..Hyphen_1-1) /= "AI95" then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Unknown AI reference " & AI_String);
	    Result := "00001";
	elsif Hyphen_1 = AI_String'First+2 and then
	    AI_String(AI_String'First..Hyphen_1-1) /= "AI" then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Unknown short AI reference " & AI_String);
	    Result := "00001";
	else
	    Hyphen_2 := Ada.Strings.Fixed.Index (AI_String(Hyphen_1+1..AI_String'Last), "-");
	    if Hyphen_2 = 0 then
	        if AI_String'Last = Hyphen_1+5 then
		    Result := AI_String(Hyphen_1+1 .. Hyphen_1+5);
	        elsif AI_String'Last = Hyphen_1+4 then
		    Result(2..5) := AI_String(Hyphen_1+1 .. Hyphen_1+4);
		    Result(1) := '0';
	        elsif AI_String'Last = Hyphen_1+3 then
		    Result(3..5) := AI_String(Hyphen_1+1 .. Hyphen_1+3);
		    Result(1) := '0';
		    Result(2) := '0';
	        else
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
	                "AI reference too wrong length " & AI_String);
	            Result := "00001";
	        end if;
	    else
		if (Hyphen_2-1) - (Hyphen_1+1) = 5-1 then
		    Result := AI_String (Hyphen_1+1 .. Hyphen_2-1);
		elsif (Hyphen_2-1) - (Hyphen_1+1) = 4-1 then
		    Result(2..5) := AI_String (Hyphen_1+1 .. Hyphen_2-1);
		    Result(1) := '0';
		elsif (Hyphen_2-1) - (Hyphen_1+1) = 3-1 then
		    Result(3..5) := AI_String (Hyphen_1+1 .. Hyphen_2-1);
		    Result(1) := '0';
		    Result(2) := '0';
		else
		    Result := "00001";
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		        "Bad AI reference (hyphen dist) " & AI_String);
		end if;
		if AI_String'Last < Hyphen_2+1 or else
		   AI_String'Last > Hyphen_2+2 then
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
			"Bad AI alternative reference " & AI_String);
		elsif  AI_String'Last = Hyphen_2+1 then
		    Result(1) := Character'Pred(AI_String(Hyphen_2+1));
		elsif AI_String'Last = Hyphen_2+2 and then AI_String(Hyphen_2+1) = '0' then
		    Result(1) := Character'Pred(AI_String(Hyphen_2+2));
		elsif AI_String'Last = Hyphen_2+2 and then AI_String(Hyphen_2+1) = '1' then
		    if AI_String(Hyphen_2+2) = '0' then
		        Result(1) := '9';
		    else
		        Result(1) := Character'Val(Character'Pos(AI_String(Hyphen_2+2)) - Character'Pos('1') + Character'Pos('A'));
		    end if;
		elsif AI_String'Last = Hyphen_2+2 and then AI_String(Hyphen_2+1) = '2' then
		    Result(1) := Character'Val(Character'Pos(AI_String(Hyphen_2+2)) - Character'Pos('1') + Character'Pos('A') + 10);
	        else
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
			"Bad AI alternative reference " & AI_String);
	        end if;
	    end if;
	end if;
	return Result;
    end Folded_AI95_Number;


    procedure AI_Reference (Output_Object : in out HTML_Output_Type;
			    Text : in String;
			    AI_Number : in String) is
	-- Generate a reference to an AI from the standard. The text
	-- of the reference is "Text", and AI_Number denotes
	-- the target (in unfolded format). For hyperlinked formats, this should
	-- generate a link; for other formats, the text alone is generated.
	--
	-- We assume AI number is of the form:
	-- ZZZZ-nnnn-m whre ZZZZ=AI05, AI12, or SI99, nnnn is a four digit number,
	-- and -m is an optional number (-1 is used if it is omitted); or
	-- AIzz-nnnnn-mm where AIzz=AI95 or AI (meaning AI95);
	-- nnnnn is a five digit number, and -mm is an optional two digit number.
	-- We raise Not_Valid_Error otherwise.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
	if AI_Number'Length > 5 and then
	    AI_Number(AI_Number'First..AI_Number'First+4) = "AI12-" then
	    -- AI12:
	    if AI_Number'Length >= 9 then
		if AI_Number(AI_Number'First+5) not in '0'..'9' or else
		   AI_Number(AI_Number'First+6) not in '0'..'9' or else
		   AI_Number(AI_Number'First+7) not in '0'..'9' or else
	           AI_Number(AI_Number'First+8) not in '0'..'9' then
	            Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		        "Bad number in AI12 number: " & AI_Number);
		end if;
	    end if;
	    if AI_Number'Length = 9 then
                Output_Text (Output_Object, "<A HREF=""http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/");
                Output_Text (Output_Object, AI_Number & "-1");
	    elsif AI_Number'Length = 11 then
		if AI_Number(AI_Number'Last-1) /= '-' or else
	           AI_Number(AI_Number'Last) not in '0'..'9' then
	            Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		        "Bad sequence number in AI12 number: " & AI_Number);
		end if;
                Output_Text (Output_Object, "<A HREF=""http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/");
                Output_Text (Output_Object, AI_Number);
	    else
	        Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		    "Bad AI12 number: " & AI_Number);
	    end if;
	elsif AI_Number'Length > 5 and then
	    AI_Number(AI_Number'First..AI_Number'First+4) = "AI05-" then
	    -- AI05:
	    if AI_Number'Length >= 9 then
		if AI_Number(AI_Number'First+5) not in '0'..'9' or else
		   AI_Number(AI_Number'First+6) not in '0'..'9' or else
		   AI_Number(AI_Number'First+7) not in '0'..'9' or else
	           AI_Number(AI_Number'First+8) not in '0'..'9' then
	            Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		        "Bad number in AI05 number: " & AI_Number);
		end if;
	    end if;
	    if AI_Number'Length = 9 then
                Output_Text (Output_Object, "<A HREF=""http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI05s/");
                Output_Text (Output_Object, AI_Number & "-1");
	    elsif AI_Number'Length = 11 then
		if AI_Number(AI_Number'Last-1) /= '-' or else
	           AI_Number(AI_Number'Last) not in '0'..'9' then
	            Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		        "Bad sequence number in AI05 number: " & AI_Number);
		end if;
                Output_Text (Output_Object, "<A HREF=""http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI05s/");
                Output_Text (Output_Object, AI_Number);
	    else
	        Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		    "Bad AI05 number: " & AI_Number);
	    end if;
	elsif AI_Number'Length > 5 and then
	    AI_Number(AI_Number'First..AI_Number'First+4) = "SI99-" then
	    if AI_Number'Length >= 9 then
		if AI_Number(AI_Number'First+5) not in '0'..'9' or else
		   AI_Number(AI_Number'First+6) not in '0'..'9' or else
		   AI_Number(AI_Number'First+7) not in '0'..'9' or else
	           AI_Number(AI_Number'First+8) not in '0'..'9' then
	            Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		        "Bad number in SI99 number: " & AI_Number);
		end if;
	    end if;
	    if AI_Number'Length = 9 then
                Output_Text (Output_Object, "<A HREF=""http://www.ada-auth.org/cgi-bin/cvsweb.cgi/SI99s/");
                Output_Text (Output_Object, AI_Number & "-1");
	    elsif AI_Number'Length = 11 then
		if AI_Number(AI_Number'Last-1) /= '-' or else
	           AI_Number(AI_Number'Last) not in '0'..'9' then
	            Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		        "Bad sequence number in SI99 number: " & AI_Number);
		end if;
                Output_Text (Output_Object, "<A HREF=""http://www.ada-auth.org/cgi-bin/cvsweb.cgi/SI99s/");
                Output_Text (Output_Object, AI_Number);
	    else
	        Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		    "Bad SI99 number: " & AI_Number);
	    end if;
	else -- Must be AI95:
	    declare
		Folded : constant String :=
		    Folded_AI95_Number(AI_Number); -- We don't want to have written anything if we raise an exception.
	    begin
                Output_Text (Output_Object, "<A HREF=""http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AIs/AI-");
                Output_Text (Output_Object, Folded);
	    end;
	end if;
        Output_Text (Output_Object, ".TXT"">");
        Ordinary_Text (Output_Object, Text);
        Output_Text (Output_Object, "</A>");

    end AI_Reference;


    procedure Local_Target (Output_Object : in out HTML_Output_Type;
			    Text : in String;
			    Target : in String) is
	-- Generate a local target. This marks the potential target of local
	-- links identified by "Target". Text is the text of the target.
	-- For hyperlinked formats, this should generate a link target;
	-- for other formats, only the text is generated.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
	-- Insert an anchor:
	Output_Text (Output_Object, "<A NAME=""");
        Output_Text (Output_Object, Target);
	Output_Text (Output_Object, """>");
        Ordinary_Text (Output_Object, Text);
        Output_Text (Output_Object, "</A>");
    end Local_Target;


    procedure Local_Link (Output_Object : in out HTML_Output_Type;
			  Text : in String;
			  Target : in String;
			  Clause_Number : in String) is
	-- Generate a local link to the target and clause given.
	-- Text is the text of the link.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
	-- Insert an anchor:
	Output_Text (Output_Object, "<A HREF=""");
	if Output_Object.Big_Files then
	    null; -- No file name needed, this is a self-reference.
	else
	    if Output_Object.DOS_Filenames then
	        Output_Text (Output_Object,
			     Make_Clause_File_Name (Output_Object, Clause_Number)
				 & ".HTM");
	    else
	        Output_Text (Output_Object,
			     Make_Clause_File_Name (Output_Object, Clause_Number)
				 & ".html");
	    end if;
	end if;
	Output_Text (Output_Object, "#" & Target);
	Output_Text (Output_Object, """>");
	Ordinary_Text (Output_Object, Text);
	Output_Text (Output_Object, "</A>");
    end Local_Link;


    procedure Local_Link_Start (Output_Object : in out HTML_Output_Type;
				Target : in String;
				Clause_Number : in String) is
	-- Generate a local link to the target and clause given.
	-- The link will surround text until Local_Link_End is called.
	-- Local_Link_End must be called before this routine can be used again.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
	if Output_Object.In_Local_Link then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Already in a local link");
	end if;
        Output_Object.In_Local_Link := True;
	-- Insert an anchor:
	Output_Text (Output_Object, "<A HREF=""");
	if Output_Object.Big_Files then
	    null; -- No file name needed, this is a self-reference.
	else
	    if Output_Object.DOS_Filenames then
	        Output_Text (Output_Object,
			     Make_Clause_File_Name (Output_Object, Clause_Number)
				 & ".HTM");
	    else
	        Output_Text (Output_Object,
			     Make_Clause_File_Name (Output_Object, Clause_Number)
				 & ".html");
	    end if;
	end if;
	Output_Text (Output_Object, "#" & Target);
	Output_Text (Output_Object, """>");
    end Local_Link_Start;


    procedure Local_Link_End (Output_Object : in out HTML_Output_Type;
			      Target : in String;
			      Clause_Number : in String) is
	-- End a local link for the target and clause given.
	-- This must be in the same paragraph as the Local_Link_Start.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
	if not Output_Object.In_Local_Link then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in a local link");
	end if;
	Output_Text (Output_Object, "</A>");
        Output_Object.In_Local_Link := False;
    end Local_Link_End;


    procedure URL_Link (Output_Object : in out HTML_Output_Type;
			Text : in String;
			URL : in String) is
	-- Generate a link to the URL given.
	-- Text is the text of the link.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
	-- Insert an anchor:
	Output_Text (Output_Object, "<A HREF=""");
        Output_Text (Output_Object, URL);
	Output_Text (Output_Object, """>");
        Ordinary_Text (Output_Object, Text);
        Output_Text (Output_Object, "</A>");
    end URL_Link;


    procedure Picture  (Output_Object : in out HTML_Output_Type;
			Name : in String;
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

	procedure Make_Img (Extra_Attribs : in String) is
	    H : constant String := Natural'Image(Height);
	    W : constant String := Natural'Image(Width);
	begin
	    Output_Text (Output_Object, "<IMG src=""" & Name & """");
	    Output_Text (Output_Object, " height=""" & H(2..H'Last) &
		""" width=""" & W(2..W'Last) & """");
	    if Extra_Attribs /= "" then
	        Output_Text (Output_Object, Extra_Attribs);
	    end if;
	    Output_Text (Output_Object, " alt=""" & Descr & """");
	    case Border is
		when ARM_Output.None =>
		    Output_Text (Output_Object, " border=""0"">");
		when ARM_Output.Thin =>
		    Output_Text (Output_Object, " border=""1"">");
		when ARM_Output.Thick =>
		    Output_Text (Output_Object, " border=""2"">");
	    end case;
	end Make_Img;

    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	case Alignment is
	    when ARM_Output.Inline =>
		if not Output_Object.Is_In_Paragraph then
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
			"Not in paragraph");
		end if;
		if Output_Object.HTML_Kind = HTML_3 then
		    Make_Img ("");
		else
		    Make_Img (" style=""margin-left: 0.3em; margin-right: 0.3em""");
		end if;
	    when ARM_Output.Float_Left =>
		if not Output_Object.Is_In_Paragraph then
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
			"Not in paragraph");
		end if;
		if Output_Object.HTML_Kind = HTML_3 then
		    Make_Img (" align=""left""");
		else
		    Make_Img (" align=""left"" style=""margin-right: 0.3em""");
			-- Space on right only; left should align with containing
			-- margin.
		end if;
	    when ARM_Output.Float_Right =>
		if not Output_Object.Is_In_Paragraph then
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
			"Not in paragraph");
		end if;
		if Output_Object.HTML_Kind = HTML_3 then
		    Make_Img (" align=""right""");
		else
		    Make_Img (" align=""right"" style=""margin-left: 0.3em""");
			-- Space on right only; left should align with containing
			-- margin.
		end if;
	    when ARM_Output.Alone_Left =>
		if Output_Object.Is_In_Paragraph then
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
			"In paragraph");
		end if;
		if Output_Object.HTML_Kind = HTML_4_Only then
		    Output_Text (Output_Object, "<DIV Style=""text-align: left; margin-bottom: ");
		    Put_EMs(Output_Object.Output_File, (Paragraph_Info(ARM_Output.Normal, 0).After * LEADING_PERCENT) / 100);
		    Output_Text (Output_Object, """>");
		else
		    Output_Text (Output_Object, "<P>");
		end if;
		Make_Img("");
		if Output_Object.HTML_Kind = HTML_4_Only then
		    Output_Text (Output_Object, "</DIV>");
		else
		    Output_Text (Output_Object, "</P>");
		end if;
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	        Output_Object.Char_Count := 0;
	        Output_Object.Disp_Char_Count := 0;
	        Output_Object.Disp_Large_Char_Count := 0;
		Output_Object.Any_Nonspace := False;
	        Output_Object.Last_Was_Space := True; -- Start of line.
	        Output_Object.Conditional_Space := False; -- Don't need it here.
	    when ARM_Output.Alone_Right =>
		if Output_Object.Is_In_Paragraph then
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
			"In paragraph");
		end if;
		if Output_Object.HTML_Kind = HTML_4_Only then
		    Output_Text (Output_Object, "<DIV Style=""text-align: right; margin-bottom: ");
		    Put_EMs(Output_Object.Output_File, (Paragraph_Info(ARM_Output.Normal, 0).After * LEADING_PERCENT) / 100);
		    Output_Text (Output_Object, """>");
		else
		    Output_Text (Output_Object, "<RIGHT>");
		end if;
		Make_Img("");
		if Output_Object.HTML_Kind = HTML_4_Only then
		    Output_Text (Output_Object, "</DIV>");
		else
		    Output_Text (Output_Object, "</RIGHT>");
		end if;
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	        Output_Object.Char_Count := 0;
	        Output_Object.Disp_Char_Count := 0;
	        Output_Object.Disp_Large_Char_Count := 0;
		Output_Object.Any_Nonspace := False;
	        Output_Object.Last_Was_Space := True; -- Start of line.
	        Output_Object.Conditional_Space := False; -- Don't need it here.
	    when ARM_Output.Alone_Center =>
		if Output_Object.Is_In_Paragraph then
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
			"In paragraph");
		end if;
		if Output_Object.HTML_Kind = HTML_4_Only then
		    Output_Text (Output_Object, "<DIV Style=""text-align: center; margin-bottom: ");
		    Put_EMs(Output_Object.Output_File, (Paragraph_Info(ARM_Output.Normal, 0).After * LEADING_PERCENT) / 100);
		    Output_Text (Output_Object, """>");
		else
		    Output_Text (Output_Object, "<CENTER>");
		end if;
		Make_Img("");
		if Output_Object.HTML_Kind = HTML_4_Only then
		    Output_Text (Output_Object, "</DIV>");
		else
		    Output_Text (Output_Object, "</CENTER>");
		end if;
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	        Output_Object.Char_Count := 0;
	        Output_Object.Disp_Char_Count := 0;
	        Output_Object.Disp_Large_Char_Count := 0;
		Output_Object.Any_Nonspace := False;
	        Output_Object.Last_Was_Space := True; -- Start of line.
	        Output_Object.Conditional_Space := False; -- Don't need it here.
	end case;
    end Picture;

begin
    -- Define the styles:
    -- Normal:
    for I in ARM_Output.Paragraph_Indent_Type loop
	Paragraph_Info(ARM_Output.Normal, I) :=
		(Defined => True,
		 Tag  => DIV,
		 Size => 0, -- 18
		 Font => ARM_Output.Default,
		 Indent => Natural(I),
		 Right_Indent => 0,
		 Hang_Outdent => 0,
		 Before => 0,
		 After => 6); -- 120
    end loop;
    -- Wide_Above:
    for I in ARM_Output.Paragraph_Indent_Type loop
	Paragraph_Info(ARM_Output.Wide_Above, I) :=
		(Defined => True,
		 Tag  => DIV,
		 Size => 0, -- 18
		 Font => ARM_Output.Default,
		 Indent => Natural(I),
		 Right_Indent => 0,
		 Hang_Outdent => 0,
		 Before => 6,
		 After => 6);
    end loop;
    -- Header:
    for I in ARM_Output.Paragraph_Indent_Type loop
	Paragraph_Info(ARM_Output.Header, I) :=
		(Defined => True,
		 Tag  => DIV,
		 Size => 0, -- 18
		 Font => ARM_Output.Default,
		 Indent => Natural(I),
		 Right_Indent => 0,
		 Hang_Outdent => 0,
		 Before => 0,
		 After => 0);
    end loop;
    -- Small:
    for I in ARM_Output.Paragraph_Indent_Type loop
	Paragraph_Info(ARM_Output.Small, I) :=
		(Defined => True,
		 Tag  => DIV,
		 Size => -1, -- 15
		 Font => ARM_Output.Default,
		 Indent => Natural(I),
		 Right_Indent => 0,
		 Hang_Outdent => 0,
		 Before => 0,
		 After => 6); -- 120
    end loop;
    -- Small_Wide_Above:
    for I in ARM_Output.Paragraph_Indent_Type loop
	Paragraph_Info(ARM_Output.Small_Wide_Above, I) :=
		(Defined => True,
		 Tag  => DIV,
		 Size => -1, -- 15
		 Font => ARM_Output.Default,
		 Indent => Natural(I),
		 Right_Indent => 0,
		 Hang_Outdent => 0,
		 Before => 6,
		 After => 6);
    end loop;
    -- Small Header:
    for I in ARM_Output.Paragraph_Indent_Type loop
	Paragraph_Info(ARM_Output.Small_Header, I) :=
		(Defined => True,
		 Tag  => DIV,
		 Size => -1, -- 15
		 Font => ARM_Output.Default,
		 Indent => Natural(I),
		 Right_Indent => 0,
		 Hang_Outdent => 0,
		 Before => 0,
		 After => 0);
    end loop;

    -- Examples:
    for I in ARM_Output.Paragraph_Indent_Type loop
	Paragraph_Info(ARM_Output.Examples, I) :=
		(Defined => True,
		 Tag  => DIV,
		 Size => 0, -- 18
		 Font => ARM_Output.Fixed,
		 Indent => Natural(I),
		 Right_Indent => 0,
		 Hang_Outdent => 0,
		 Before => 0,
		 After => 6); -- 120
    end loop;
    -- Small Examples:
    for I in ARM_Output.Paragraph_Indent_Type loop
	Paragraph_Info(ARM_Output.Small_Examples, I) :=
		(Defined => True,
		 Tag  => DIV,
		 Size => -1, -- 15
		 Font => ARM_Output.Fixed,
		 Indent => Natural(I),
		 Right_Indent => 0,
		 Hang_Outdent => 0,
		 Before => 0,
		 After => 6); -- 120
    end loop;
    -- Swiss Examples:
    for I in ARM_Output.Paragraph_Indent_Type loop
	Paragraph_Info(ARM_Output.Swiss_Examples, I) :=
		(Defined => True,
		 Tag  => DIV,
		 Size => 0, -- 18
		 Font => ARM_Output.Swiss,
		 Indent => Natural(I),
		 Right_Indent => 0,
		 Hang_Outdent => 0,
		 Before => 0,
		 After => 6); -- 120
    end loop;
    -- Small Swiss Examples:
    for I in ARM_Output.Paragraph_Indent_Type loop
	Paragraph_Info(ARM_Output.Small_Swiss_Examples, I) :=
		(Defined => True,
		 Tag  => DIV,
		 Size => -1, -- 15
		 Font => ARM_Output.Swiss,
		 Indent => Natural(I),
		 Right_Indent => 0,
		 Hang_Outdent => 0,
		 Before => 0,
		 After => 6); -- 120
    end loop;

    -- Bulleted:
    -- Note: Indent = 0 is not allowed.
    for I in 1 .. ARM_Output.Paragraph_Indent_Type'Last loop
	Paragraph_Info(ARM_Output.Bulleted, I) :=
		(Defined => True,
		 Tag  => UL,
		 Size => 0, -- 18
		 Font => ARM_Output.Default,
		 Indent => Natural(I),
		 Right_Indent => 1,
		 Hang_Outdent => 0,
		 Before => 0,
		 After => 5);
    end loop;
    -- Nested Bulleted:
    -- Note: Indent = 0 is not allowed.
    for I in 1 .. ARM_Output.Paragraph_Indent_Type'Last loop
	Paragraph_Info(ARM_Output.Nested_Bulleted, I) :=
		(Defined => True,
		 Tag  => UL,
		 Size => 0, -- 18
		 Font => ARM_Output.Default,
		 Indent => Natural(I),
		 Right_Indent => 1,
		 Hang_Outdent => 0,
		 Before => 0,
		 After => 5);
    end loop;
    -- Small Bulleted:
    -- Note: Indent = 0 is not allowed.
    for I in 1 .. ARM_Output.Paragraph_Indent_Type'Last loop
	Paragraph_Info(ARM_Output.Small_Bulleted, I) :=
		(Defined => True,
		 Tag  => UL,
		 Size => -1, -- 15
		 Font => ARM_Output.Default,
		 Indent => Natural(I),
		 Right_Indent => 1,
		 Hang_Outdent => 0,
		 Before => 0,
		 After => 5);
    end loop;
    -- Small Nested Bulleted:
    -- Note: Indent = 0 is not allowed.
    for I in 1 .. ARM_Output.Paragraph_Indent_Type'Last loop
	Paragraph_Info(ARM_Output.Small_Nested_Bulleted, I) :=
		(Defined => True,
		 Tag  => UL,
		 Size => -1, -- 15
		 Font => ARM_Output.Default,
		 Indent => Natural(I),
		 Right_Indent => 1,
		 Hang_Outdent => 0,
		 Before => 0,
		 After => 5);
    end loop;

    -- Enumerated:
    -- Note: Indent = 0 is not allowed.
    for I in 1 .. ARM_Output.Paragraph_Indent_Type'Last loop
	Paragraph_Info(ARM_Output.Enumerated, I) :=
		(Defined => True,
		 Tag  => DL,
		 Size => 0, -- 18
		 Font => ARM_Output.Default,
		 Indent => Natural(I)-1,
		 Right_Indent => 1,
		 Hang_Outdent => 1,
		 Before => 0,
		 After => 5);
    end loop;
    -- Small Enumerated:
    -- Note: Indent = 0 is not allowed.
    for I in 1 .. ARM_Output.Paragraph_Indent_Type'Last loop
	Paragraph_Info(ARM_Output.Small_Enumerated, I) :=
		(Defined => True,
		 Tag  => DL,
		 Size => -1, -- 15
		 Font => ARM_Output.Default,
		 Indent => Natural(I)-1,
		 Right_Indent => 1,
		 Hang_Outdent => 1,
		 Before => 0,
		 After => 5);
    end loop;

    -- Giant Hanging
    -- Note: Indent < 4 is not allowed.
    for I in 4 .. ARM_Output.Paragraph_Indent_Type'Last loop
	Paragraph_Info(ARM_Output.Giant_Hanging, I) :=
		(Defined => True,
		 Tag  => DL,
		 Size => 0, -- 18
		 Font => ARM_Output.Default,
		 Indent => Natural(I)-4,
		 Right_Indent => 0,
		 Hang_Outdent => 4,
		 Before => 0,
		 After => 6);
    end loop;
    -- Small Giant Hanging:
    -- Note: Indent < 4 is not allowed.
    for I in 4 .. ARM_Output.Paragraph_Indent_Type'Last loop
	Paragraph_Info(ARM_Output.Small_Giant_Hanging, I) :=
		(Defined => True,
		 Tag  => DL,
		 Size => -1, -- 15
		 Font => ARM_Output.Default,
		 Indent => Natural(I)-4,
		 Right_Indent => 0,
		 Hang_Outdent => 4,
		 Before => 0,
		 After => 6);
    end loop;
    -- Wide Hanging
    -- Note: Indent < 3 is not allowed.
    for I in 3 .. ARM_Output.Paragraph_Indent_Type'Last loop
	Paragraph_Info(ARM_Output.Wide_Hanging, I) :=
		(Defined => True,
		 Tag  => DL,
		 Size => 0, -- 18
		 Font => ARM_Output.Default,
		 Indent => Natural(I)-3,
		 Right_Indent => 0,
		 Hang_Outdent => 3,
		 Before => 0,
		 After => 6);
    end loop;
    -- Small Wide Hanging:
    -- Note: Indent < 3 is not allowed.
    for I in 3 .. ARM_Output.Paragraph_Indent_Type'Last loop
	Paragraph_Info(ARM_Output.Small_Wide_Hanging, I) :=
		(Defined => True,
		 Tag  => DL,
		 Size => -1, -- 15
		 Font => ARM_Output.Default,
		 Indent => Natural(I)-3,
		 Right_Indent => 0,
		 Hang_Outdent => 3,
		 Before => 0,
		 After => 6);
    end loop;
    -- Medium Hanging
    -- Note: Indent < 2 is not allowed.
    for I in 2 .. ARM_Output.Paragraph_Indent_Type'Last loop
	Paragraph_Info(ARM_Output.Medium_Hanging, I) :=
		(Defined => True,
		 Tag  => DL,
		 Size => 0, -- 18
		 Font => ARM_Output.Default,
		 Indent => Natural(I)-2,
		 Right_Indent => 0,
		 Hang_Outdent => 2,
		 Before => 0,
		 After => 6);
    end loop;
    -- Small Medium Hanging:
    -- Note: Indent < 2 is not allowed.
    for I in 2 .. ARM_Output.Paragraph_Indent_Type'Last loop
	Paragraph_Info(ARM_Output.Small_Medium_Hanging, I) :=
		(Defined => True,
		 Tag  => DL,
		 Size => -1, -- 15
		 Font => ARM_Output.Default,
		 Indent => Natural(I)-2,
		 Right_Indent => 0,
		 Hang_Outdent => 2,
		 Before => 0,
		 After => 6);
    end loop;
    -- Narrow Hanging
    -- Note: Indent < 1 is not allowed.
    for I in 1 .. ARM_Output.Paragraph_Indent_Type'Last loop
	Paragraph_Info(ARM_Output.Narrow_Hanging, I) :=
		(Defined => True,
		 Tag  => DL,
		 Size => 0, -- 18
		 Font => ARM_Output.Default,
		 Indent => Natural(I)-1,
		 Right_Indent => 0,
		 Hang_Outdent => 1,
		 Before => 0,
		 After => 6);
    end loop;
    -- Small Narrow Hanging:
    -- Note: Indent < 1 is not allowed.
    for I in 1 .. ARM_Output.Paragraph_Indent_Type'Last loop
	Paragraph_Info(ARM_Output.Small_Narrow_Hanging, I) :=
		(Defined => True,
		 Tag  => DL,
		 Size => -1, -- 15
		 Font => ARM_Output.Default,
		 Indent => Natural(I)-1,
		 Right_Indent => 0,
		 Hang_Outdent => 1,
		 Before => 0,
		 After => 6);
    end loop;
    -- Hanging in Bulleted
    -- Note: Indent < 2 is not allowed.
    for I in 2 .. ARM_Output.Paragraph_Indent_Type'Last loop
	Paragraph_Info(ARM_Output.Hanging_in_Bulleted, I) :=
		(Defined => True,
		 Tag  => DL,
		 Size => 0, -- 18
		 Font => ARM_Output.Default,
		 Indent => Natural(I)-2,
		 Right_Indent => 1,
		 Hang_Outdent => 2,
		 Before => 0,
		 After => 5);
    end loop;
    -- Small Hanging in Bulleted
    -- Note: Indent < 2 is not allowed.
    for I in 2 .. ARM_Output.Paragraph_Indent_Type'Last loop
	Paragraph_Info(ARM_Output.Small_Hanging_in_Bulleted, I) :=
		(Defined => True,
		 Tag  => DL,
		 Size => -1, -- 15
		 Font => ARM_Output.Default,
		 Indent => Natural(I)-2,
		 Right_Indent => 1,
		 Hang_Outdent => 2,
		 Before => 0,
		 After => 5);
    end loop;

    -- Index. Only define the form that we'll use.
    Paragraph_Info(ARM_Output.Index, 0) :=
		(Defined => True,
		 Tag  => DIV,
		 Size => 0,
		 Font => ARM_Output.Default,
		 Indent => 0,
		 Right_Indent => 0,
		 Hang_Outdent => 0,
		 Before => 0,
		 After => 0);

    -- Syntax Summary. Only define the form that we'll use.
    Paragraph_Info(ARM_Output.Syntax_Summary, 1) :=
		(Defined => True,
		 Tag  => DIV,
		 Size => -1,
		 Font => ARM_Output.Default,
		 Indent => 1,
		 Right_Indent => 0,
		 Hang_Outdent => 0,
		 Before => 0,
		 After => 4);

    -- Title. Only define the form that we'll use.
    Paragraph_Info(ARM_Output.Title, 0) :=
	    (Defined => True,
	     Tag  => DIV,
	     Size => 3, -- 36
	     Font => ARM_Output.Default,
	     Indent => 0,
	     Right_Indent => 0,
	     Hang_Outdent => 0,
	     Before => 6,
	     After => 6);

end ARM_HTML;
