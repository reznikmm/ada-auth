with ARM_Output,
     ARM_Contents,
     Ada.Text_IO,
     Ada.Exceptions,
     Ada.Strings.Maps,
     Ada.Strings.Fixed,
     Ada.Characters.Handling,
     Ada.Calendar;
package body ARM_RTF is

    --
    -- Ada reference manual formatter.
    --
    -- This package defines the RTF output object.
    -- Output objects are responsible for implementing the details of
    -- a particular format.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2002, 2004, 2005, 2006  AXE Consultants.
    -- P.O. Box 1512, Madison WI  53701
    -- E-Mail: randy@rrsoftware.com
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
	Size : Natural; -- In 1/2 pts.
	Indent : Natural; -- In Twips (.1 pt = 1/120th pica = 1/1440th inch).
	Hang_Width : Natural; -- In Twips (.1 pt = 1/120th pica = 1/1440th inch).
	Before : Natural; -- Vertical space before in Twips. (\sb)
	After : Natural; -- Vertical space after in Twips. (\sa)
	Is_Justified : Boolean; -- True if the format is justified.
	Format_String : String(1 .. 200);
	Format_Len : Natural := 0;
    end record;

    Paragraph_Info : array (ARM_Output.Paragraph_Type) of Format_Info_Type;
	-- Set by Start_RTF_File.
    Heading_1_Info : Format_Info_Type;
    Heading_2_Info : Format_Info_Type;
    Heading_3_Info : Format_Info_Type;
    Category_Header_Info : Format_Info_Type;
    Paragraph_Number_Info : Format_Info_Type;
    Header_Info : Format_Info_Type;
    Footer_Info : Format_Info_Type;
    TOC_1_Info : Format_Info_Type;
    TOC_2_Info : Format_Info_Type;
    TOC_3_Info : Format_Info_Type;

    Table_Text_Info : Format_Info_Type;

    procedure Set_Style (Into : in out Format_Info_Type;
			 Font_Size : in Natural;
			 Style_Indent : in Natural;
			 Style_Hang_Width : in Natural := 0;
			 Style_Before : in Natural;
			 Style_After : in Natural;
			 Style_Justified : in Boolean;
			 Style_String : in String) is
	-- Internal routine.
	-- Set the indicated style information.
    begin
	Into.Size := Font_Size;
	Into.Indent := Style_Indent;
	Into.Hang_Width := Style_Hang_Width;
	Into.Before := Style_Before;
	Into.After  := Style_After;
	Into.Is_Justified := Style_Justified;
	Ada.Strings.Fixed.Move (Source => Style_String,
				Target => Into.Format_String);
        Into.Format_Len := Style_String'Length;
    end Set_Style;


    procedure Write_Style (Fyle : in Ada.Text_IO.File_Type;
			   Style : in Format_Info_Type) is
	-- Internal routine.
	-- Write a header to start a style definition for Style.
    begin
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

	-- Set all of the styles to a (junk) default, so we can tell if we forget
	-- to set one:
	for I in ARM_Output.Paragraph_Type loop
	    Set_Style (Paragraph_Info(I),
		       Font_Size => 32,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => FALSE,
		       Style_String => "\fs32\f0\qc ");
	end loop;

	-- Style sheet:
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\stylesheet");

	if Output_Object.Page_Size = ARM_RTF.Ada95 or else
	   Output_Object.Page_Size = ARM_RTF.Half_Letter then
	    -- These are smaller page sizes than the other sizes.
	    -- ** TBD: Consider putting this into a separate parameter (there is
	    -- ** little reason for the font size to depend on the page size).
	    Set_Style (Paragraph_Info(ARM_Output.Normal),
		       Font_Size => 18,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s0\widctlpar\adjustright \fs18\f0\cgrid\sa120\qj\sl-220\slmult0 \snext0 ");
	    Set_Style (Heading_1_Info,
		       Font_Size => 28,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 210,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s1\sa210\keepn\widctlpar\outlinelevel0\adjustright \b\f1\fs28\kerning28\qc\cgrid \sbasedon0 \snext0 ");
	    Set_Style (Heading_2_Info,
		       Font_Size => 24,
		       Style_Indent => 0,
		       Style_Before => 240,
		       Style_After => 120,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s2\sb240\sa120\keepn\widctlpar\outlinelevel1\adjustright \b\f1\fs24\ql\cgrid \sbasedon0 \snext0 ");
	    Set_Style (Heading_3_Info,
		       Font_Size => 23,
		       Style_Indent => 0,
		       Style_Before => 210,
		       Style_After => 90,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s3\sb210\sa90\keepn\widctlpar\outlinelevel2\adjustright \b\f1\fs23\ql\cgrid \sbasedon0 \snext0 ");
	    Set_Style (Category_Header_Info,
		       Font_Size => 14,
		       Style_Indent => 0,
		       Style_Before => 100,
		       Style_After => 60,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s4\sb100\sa60\keepn\adjustright \f0\fs14\cgrid\qc\i \snext0 ");
	    Set_Style (Paragraph_Number_Info,
		       Font_Size => 12,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After =>  0,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s5\keepn\widctlpar\adjustright " &
			 "\pvpara\phpg\posxo\posy0\absw450\dxfrtext100\dfrmtxtx120\dfrmtxty120"&
			 "\f1\fs12\cgrid\qc \snext0 "); -- Note: We adjust the space before on use.
	    Set_Style (Paragraph_Info(ARM_Output.Notes),
		       Font_Size => 15,
		       Style_Indent => 360,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s6\widctlpar\adjustright \li360\fs15\f0\cgrid\sa90\qj\sl-180\slmult0 \snext6 ");
	    Set_Style (Paragraph_Info(ARM_Output.Annotations),
		       Font_Size => 15,
		       Style_Indent => 720,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s7\widctlpar\adjustright \li720\fs15\f0\cgrid\sa90\qj\sl-180\slmult0 \snext7 ");
	    Set_Style (Paragraph_Info(ARM_Output.Examples),
		       Font_Size => 16,
		       Style_Indent => 360,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s8\widctlpar\adjustright \li360\fs16\f2\cgrid\sa80\sl-160\ql \snext8 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Examples),
		       Font_Size => 14,
		       Style_Indent => 1080,
		       Style_Before => 0,
		       Style_After => 70,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s9\widctlpar\adjustright \li1080\fs14\f2\cgrid\sa70\sl-140\ql \snext9 ");
	    Set_Style (Paragraph_Info(ARM_Output.Syntax_Indented),
		       Font_Size => 18,
		       Style_Indent => 360,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s10\widctlpar\adjustright \li360\fs18\f0\cgrid\sa80\ql\sl-200 \snext10 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Indented),
		       Font_Size => 18,
		       Style_Indent => 1080,
		       Style_Before => 0,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s11\widctlpar\adjustright \li1080\fs18\f0\cgrid\sa120\qj\sl-220\slmult0 \snext11 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Indented),
		       Font_Size => 15,
		       Style_Indent => 1800,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s12\widctlpar\adjustright \li1800\fs15\f0\cgrid\sa90\qj\sl-180\slmult0 \snext12 ");
	    Set_Style (Paragraph_Info(ARM_Output.Hanging),
		       Font_Size => 18,
		       Style_Indent => 1080,
		       Style_Hang_Width => 1080,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s13\widctlpar\adjustright \li1080\fi-1080\fs18\f0\cgrid\sa100\qj\sl-200\slmult0 \snext13 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Indented_Hanging),
		       Font_Size => 18,
		       Style_Indent => 1080,
		       Style_Hang_Width => 360,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s14\widctlpar\adjustright \li1080\fi-360\fs18\f0\cgrid\sa100\qj\sl-200\slmult0 \snext14 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Hanging),
		       Font_Size => 15,
		       Style_Indent => 1800,
		       Style_Hang_Width => 1080,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s15\widctlpar\adjustright \li1800\fi-1080\fs15\f0\cgrid\sa80\qj\sl-170\slmult0 \snext15 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Indented_Hanging),
		       Font_Size => 15,
		       Style_Indent => 1800,
		       Style_Hang_Width => 360,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s16\widctlpar\adjustright \li1800\fi-360\fs15\f0\cgrid\sa80\qj\sl-170\slmult0 \snext16 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Bulleted),
		       Font_Size => 18,
		       Style_Indent => 360,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s17\widctlpar\adjustright \li360\fi-220\ri360\fs18\f0\cgrid\sa100\qj\sl-200\slmult0\tx360 \snext17 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Nested_Bulleted),
		       Font_Size => 18,
		       Style_Indent => 720,
		       Style_Hang_Width => 200,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s18\widctlpar\adjustright \li720\fi-200\ri360\fs18\f0\cgrid\sa100\qj\sl-200\slmult0\tx720 \snext18 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Bulleted),
		       Font_Size => 15,
		       Style_Indent => 1080,
		       Style_Hang_Width => 200,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s19\widctlpar\adjustright \li1080\fi-200\ri360\fs15\f0\cgrid\sa80\qj\sl-170\slmult0\tx1080 \snext19 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Nested_Bulleted),
		       Font_Size => 15,
		       Style_Indent => 1440,
		       Style_Hang_Width => 180,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s20\widctlpar\adjustright \li1440\fi-180\ri360\fs15\f0\cgrid\sa80\qj\sl-170\slmult0\tx1440 \snext20 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Indented_Bulleted),
		       Font_Size => 18,
		       Style_Indent => 1440,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s21\widctlpar\adjustright \li1440\fi-220\ri360\fs18\f0\cgrid\sa100\qj\sl-200\slmult0\tx1080 \snext21 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Syntax_Indented_Bulleted),
		       Font_Size => 18,
		       Style_Indent => 720,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s22\widctlpar\adjustright \li720\fi-220\ri360\fs18\f0\cgrid\sa80\qj\sl-200\slmult0\tx720 \snext22 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Code_Indented_Bulleted),
		       Font_Size => 18,
		       Style_Indent => 1080,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s23\widctlpar\adjustright \li1080\fi-220\ri360\fs18\f0\cgrid\sa100\qj\sl-200\slmult0\tx1080 \snext23 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Code_Indented),
		       Font_Size => 18,
		       Style_Indent => 720,
		       Style_Before => 0,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s24\widctlpar\adjustright \li720\fs18\f0\cgrid\sa120\qj\sl-220\slmult0 \snext24 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Code_Indented),
		       Font_Size => 15,
		       Style_Indent => 1440,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s25\widctlpar\adjustright \li1440\fs15\f0\cgrid\sa90\qj\sl-180\slmult0 \snext25 ");
	    Set_Style (Paragraph_Info(ARM_Output.Indented_Examples),
		       Font_Size => 16,
		       Style_Indent => 1440,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s26\widctlpar\adjustright \li1440\fs16\f2\cgrid\sa80\ql\sl-160 \snext26 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Indented_Examples),
		       Font_Size => 14,
		       Style_Indent => 2160,
		       Style_Before => 0,
		       Style_After => 70,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s27\widctlpar\adjustright \li2160\fs14\f2\cgrid\sa70\ql\sl-140 \snext27 ");

            Set_Style (Header_Info,
		       Font_Size => 17,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => FALSE,
		       Style_String =>
		         "\s28\widctlpar\tqc\tx" & Half_Paper_Width &
			 "\tqr\tx" & Paper_Width & "\adjustright \fs17\cgrid \sbasedon0 \snext28 ");
            Set_Style (Footer_Info,
		       Font_Size => 17,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => FALSE,
		       Style_String =>
		         "\s29\widctlpar" & -- "\tqc\tx" & Half_Paper_Width & -- We don't use or define the center tab; it causes problems with very long titles.
			 "\tqr\tx" & Paper_Width & "\adjustright \fs17\cgrid \sbasedon0 \snext29 ");
	    Set_Style (Paragraph_Info(ARM_Output.Index),
		       Font_Size => 15,
		       Style_Indent => 225,
		       Style_Hang_Width => 225,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s31\widctlpar\adjustright \fs15\f0\cgrid\ql\li225\fi-225\sl-180\slmult0 \snext31 ");
	    Set_Style (Paragraph_Info(ARM_Output.Wide),
		       Font_Size => 18,
		       Style_Indent => 0,
		       Style_Before => 120,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s32\widctlpar\adjustright \fs18\f0\cgrid\sa120\sb120\qj\sl-220\slmult0 \snext0 ");
	    Set_Style (Paragraph_Info(ARM_Output.Wide_Annotations),
		       Font_Size => 15,
		       Style_Indent => 720,
		       Style_Before => 90,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s33\widctlpar\adjustright \li720\fs15\f0\cgrid\sa90\sb90\qj\sl-180\slmult0 \snext7 ");
	    Set_Style (Paragraph_Info(ARM_Output.Notes_Header),
		       Font_Size => 15,
		       Style_Indent => 360,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s34\widctlpar\adjustright \li360\fs15\f0\cgrid\sa0\qj\sl-180\slmult0 \snext6 ");
			  -- Note: No extra space afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Notes_Bulleted),
		       Font_Size => 15,
		       Style_Indent => 720,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 60,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s35\widctlpar\adjustright \li720\fi-220\ri360\fs15\f0\cgrid\sa60\qj\sl-170\slmult0\tx720 \snext35 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Notes_Nested_Bulleted),
		       Font_Size => 15,
		       Style_Indent => 1080,
		       Style_Hang_Width => 200,
		       Style_Before => 0,
		       Style_After => 60,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s36\widctlpar\adjustright \li1080\fi-200\ri360\fs15\f0\cgrid\sa60\qj\sl-170\slmult0\tx1080 \snext36 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Hanging_in_Bulleted),
		       Font_Size => 18,
		       Style_Indent => 1080,
		       Style_Hang_Width => 720,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s37\widctlpar\adjustright \li1080\fi-720\ri360\fs18\f0\cgrid\sa100\qj\sl-200\slmult0 \snext14 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Hanging_in_Bulleted),
		       Font_Size => 15,
		       Style_Indent => 1800,
		       Style_Hang_Width => 720,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s38\widctlpar\adjustright \li1800\fi-720\ri360\fs15\f0\cgrid\sa80\qj\sl-170\slmult0 \snext16 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Code_Indented_Nested_Bulleted),
		       Font_Size => 18,
		       Style_Indent => 1440,
		       Style_Hang_Width => 200,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s39\widctlpar\adjustright \li1440\fi-200\ri360\fs18\f0\cgrid\sa100\qj\sl-200\slmult0\tx1440 \snext39 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Syntax_Summary),
		       Font_Size => 15,
		       Style_Indent => 360,
		       Style_Before => 0,
		       Style_After => 65,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s40\widctlpar\adjustright \fs15\f0\cgrid\ql\li360\sa65\sl-170\slmult0 \snext40 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Enumerated),
		       Font_Size => 18,
		       Style_Indent => 360,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s41\widctlpar\adjustright \li360\fi-220\ri360\fs18\f0\cgrid\sa100\qj\sl-200\slmult0\tx360 \snext41 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Enumerated),
		       Font_Size => 15,
		       Style_Indent => 1080,
		       Style_Hang_Width => 200,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s42\widctlpar\adjustright \li1080\fi-200\ri360\fs15\f0\cgrid\sa80\qj\sl-170\slmult0\tx1080 \snext42 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Nested_Enumerated),
		       Font_Size => 18,
		       Style_Indent => 720,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s43\widctlpar\adjustright \li720\fi-220\ri360\fs18\f0\cgrid\sa100\qj\sl-200\slmult0\tx360 \snext43 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Nested_Enumerated),
		       Font_Size => 15,
		       Style_Indent => 1440,
		       Style_Hang_Width => 200,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s44\widctlpar\adjustright \li1440\fi-200\ri360\fs15\f0\cgrid\sa80\qj\sl-170\slmult0\tx1080 \snext44 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Nested_X2_Bulleted),
		       Font_Size => 18,
		       Style_Indent => 1080,
		       Style_Hang_Width => 200,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s45\widctlpar\adjustright \li1080\fi-200\ri360\fs18\f0\cgrid\sa100\qj\sl-200\slmult0\tx720 \snext45 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Nested_X2_Bulleted),
		       Font_Size => 15,
		       Style_Indent => 1800,
		       Style_Hang_Width => 180,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s46\widctlpar\adjustright \li1800\fi-180\ri360\fs15\f0\cgrid\sa80\qj\sl-170\slmult0\tx1440 \snext46 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Indented_Nested_Bulleted),
		       Font_Size => 18,
		       Style_Indent => 1800,
		       Style_Hang_Width => 220,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s47\widctlpar\adjustright \li1800\fi-220\ri360\fs18\f0\cgrid\sa100\qj\sl-200\slmult0\tx1080 \snext47 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Inner_Indented),
		       Font_Size => 18,
		       Style_Indent => 1440,
		       Style_Before => 0,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s48\widctlpar\adjustright \li1440\fs18\f0\cgrid\sa120\qj\sl-220\slmult0 \snext48 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Inner_Indented),
		       Font_Size => 15,
		       Style_Indent => 2160,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s49\widctlpar\adjustright \li2160\fs15\f0\cgrid\sa90\qj\sl-180\slmult0 \snext49 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Syntax_Indented),
		       Font_Size => 15,
		       Style_Indent => 1080,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s50\widctlpar\adjustright \li1080\fs15\f0\cgrid\sa90\qj\sl-180\slmult0 \snext50 ");
	    Set_Style (Paragraph_Info(ARM_Output.Swiss_Examples),
		       Font_Size => 16,
		       Style_Indent => 360,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s51\widctlpar\adjustright \li360\fs16\f1\cgrid\sa80\sl-180\ql \snext51 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Swiss_Examples),
		       Font_Size => 14,
		       Style_Indent => 1080,
		       Style_Before => 0,
		       Style_After => 70,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s52\widctlpar\adjustright \li1080\fs14\f1\cgrid\sa70\sl-160\ql \snext52 ");
	    Set_Style (Paragraph_Info(ARM_Output.Swiss_Indented_Examples),
		       Font_Size => 16,
		       Style_Indent => 1440,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s53\widctlpar\adjustright \li1440\fs16\f1\cgrid\sa80\ql\sl-180 \snext53 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Swiss_Indented_Examples),
		       Font_Size => 14,
		       Style_Indent => 2160,
		       Style_Before => 0,
		       Style_After => 70,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s54\widctlpar\adjustright \li2160\fs14\f1\cgrid\sa70\ql\sl-160 \snext54 ");
	    if Output_Object.Big_Files then
		-- Define the TOC styles:
                Set_Style (TOC_1_Info,
		           Font_Size => 20,
		           Style_Indent => 0,
		           Style_Before => 45,
		           Style_After => 45,
		           Style_Justified => FALSE,
		           Style_String =>
		             "\s55\sa45\sb45\widctlpar\tqr\tldot\tx" & Paper_Width &
				"\adjustright \b\f1\fs20\cgrid \sbasedon0 \snext0 ");
                Set_Style (TOC_2_Info,
		           Font_Size => 17,
		           Style_Indent => 200,
		           Style_Before => 0,
		           Style_After => 0,
		           Style_Justified => FALSE,
		           Style_String =>
		             "\s56\li200\widctlpar\tqr\tldot\tx" & Paper_Width &
				"\adjustright \b\f1\fs17\cgrid \sbasedon0 \snext0 ");
                Set_Style (TOC_3_Info,
		           Font_Size => 17,
		           Style_Indent => 400,
		           Style_Before => 0,
		           Style_After => 0,
		           Style_Justified => FALSE,
		           Style_String =>
		             "\s57\li400\widctlpar\tqr\tldot\tx" & Paper_Width &
				"\adjustright \b\f1\fs17\cgrid \sbasedon0 \snext0 ");
	    end if;
	    Set_Style (Table_Text_Info,
		       Font_Size => 18,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\fs18\f0\qc ");
	else
	    Set_Style (Paragraph_Info(ARM_Output.Normal),
		       Font_Size => 22,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s0\widctlpar\adjustright \fs22\f0\cgrid\sa120\qj\sl-260\slmult0 \snext0 ");
	    Set_Style (Heading_1_Info,
		       Font_Size => 36,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 210,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s1\sa210\keepn\widctlpar\outlinelevel0\adjustright \b\f1\fs36\kerning36\qc\cgrid \sbasedon0 \snext0 ");
	    Set_Style (Heading_2_Info,
		       Font_Size => 28,
		       Style_Indent => 0,
		       Style_Before => 240,
		       Style_After => 120,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s2\sb240\sa120\keepn\widctlpar\outlinelevel1\adjustright \b\f1\fs28\ql\cgrid \sbasedon0 \snext0 ");
	    Set_Style (Heading_3_Info,
		       Font_Size => 28,
		       Style_Indent => 0,
		       Style_Before => 210,
		       Style_After => 100,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s3\sb210\sa100\keepn\widctlpar\outlinelevel2\adjustright \b\f1\fs26\ql\cgrid \sbasedon0 \snext0 ");
	    Set_Style (Category_Header_Info,
		       Font_Size => 16,
		       Style_Indent => 0,
		       Style_Before => 120,
		       Style_After => 120,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s4\sb120\sa120\keepn\adjustright \f0\fs16\cgrid\qc\i \snext0 ");
	    Set_Style (Paragraph_Number_Info,
		       Font_Size => 14,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s5\keepn\widctlpar\adjustright " &
			 "\pvpara\phpg\posxo\posy0\absw580\dxfrtext100\dfrmtxtx150\dfrmtxty150"&
			 "\f1\fs14\cgrid\qc \snext0 "); -- We adjust the space before for each number.
		-- Frame commands:
		-- \pvpara - positions the frame vertically with the next paragraph;
		-- \phpg - positions the frame horizonatally within the page;
		-- \posxo - positions the paragraph outside of the frame;
		-- \posy0 - positions the paragraph at the top of the frame;
		-- \absw - frame width in twips (660);
		-- \dxfrtext - distance of frame from text in all directions (twips);
		-- \dfrmtxtx - horizontal distance of text from frame (twips);
		-- \dfrmtxty - vertical distance of text from frame (twips).


	    Set_Style (Paragraph_Info(ARM_Output.Notes),
		       Font_Size => 18,
		       Style_Indent => 360,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s6\widctlpar\adjustright \li360\fs18\f0\cgrid\sa90\qj\sl-200\slmult0 \snext6 ");
	    Set_Style (Paragraph_Info(ARM_Output.Annotations),
		       Font_Size => 18,
		       Style_Indent => 720,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s7\widctlpar\adjustright \li720\fs18\f0\cgrid\sa90\qj\sl-200\slmult0 \snext7 ");
	    Set_Style (Paragraph_Info(ARM_Output.Examples),
		       Font_Size => 18,
		       Style_Indent => 360,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s8\widctlpar\adjustright \li360\fs18\f2\cgrid\sa100\ql\sl-190\slmult0 \snext8 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Examples),
		       Font_Size => 16,
		       Style_Indent => 1080,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s9\widctlpar\adjustright \li1080\fs16\f2\cgrid\sa80\ql\sl-170\slmult0 \snext9 ");
	    Set_Style (Paragraph_Info(ARM_Output.Syntax_Indented),
		       Font_Size => 22,
		       Style_Indent => 360,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s10\widctlpar\adjustright \li360\fs22\f0\cgrid\sa100\ql\sl-240 \snext10 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Indented),
		       Font_Size => 22,
		       Style_Indent => 1080,
		       Style_Before => 0,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s11\widctlpar\adjustright \li1080\fs22\f0\cgrid\sa120\qj\sl-260\slmult0 \snext11 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Indented),
		       Font_Size => 18,
		       Style_Indent => 1800,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s12\widctlpar\adjustright \li1800\fs18\f0\cgrid\sa90\qj\sl-200\slmult0 \snext12 ");
	    Set_Style (Paragraph_Info(ARM_Output.Hanging),
		       Font_Size => 22,
		       Style_Indent => 1080,
		       Style_Hang_Width => 1080,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s13\widctlpar\adjustright \li1080\fi-1080\fs22\f0\cgrid\sa110\qj\sl-240\slmult0 \snext13 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Indented_Hanging),
		       Font_Size => 22,
		       Style_Indent => 1080,
		       Style_Hang_Width => 360,
		       Style_Before => 0,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s14\widctlpar\adjustright \li1080\fi-360\fs22\f0\cgrid\sa110\qj\sl-240\slmult0 \snext14 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Hanging),
		       Font_Size => 18,
		       Style_Indent => 1800,
		       Style_Hang_Width => 1080,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s15\widctlpar\adjustright \li1800\fi-1080\fs18\f0\cgrid\sa90\qj\sl-190\slmult0 \snext15 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Indented_Hanging),
		       Font_Size => 18,
		       Style_Indent => 1800,
		       Style_Hang_Width => 360,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s16\widctlpar\adjustright \li1800\fi-360\fs18\f0\cgrid\sa90\qj\sl-190\slmult0 \snext16 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Bulleted),
		       Font_Size => 22,
		       Style_Indent => 360,
		       Style_Hang_Width => 230,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s17\widctlpar\adjustright \li360\fi-230\ri360\fs22\f0\cgrid\sa110\qj\sl-240\slmult0\tx360 \snext17 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Nested_Bulleted),
		       Font_Size => 22,
		       Style_Indent => 720,
		       Style_Hang_Width => 200,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s18\widctlpar\adjustright \li720\fi-200\ri360\fs22\f0\cgrid\sa110\qj\sl-240\slmult0\tx720 \snext18 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Bulleted),
		       Font_Size => 18,
		       Style_Indent => 1080,
		       Style_Hang_Width => 200,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s19\widctlpar\adjustright \li1080\fi-200\ri360\fs18\f0\cgrid\sa90\qj\sl-190\slmult0\tx1080 \snext19 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Nested_Bulleted),
		       Font_Size => 18,
		       Style_Indent => 1440,
		       Style_Hang_Width => 180,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s20\widctlpar\adjustright \li1440\fi-180\ri360\fs18\f0\cgrid\sa90\qj\sl-190\slmult0\tx1440 \snext20 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Indented_Bulleted),
		       Font_Size => 22,
		       Style_Indent => 1440,
		       Style_Hang_Width => 230,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s21\widctlpar\adjustright \li1440\fi-230\ri360\fs22\f0\cgrid\sa120\qj\sl-240\slmult0\tx1080 \snext21 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Syntax_Indented_Bulleted),
		       Font_Size => 22,
		       Style_Indent => 720,
		       Style_Hang_Width => 230,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s22\widctlpar\adjustright \li720\fi-230\ri360\fs22\f0\cgrid\sa110\qj\sl-240\slmult0\tx720 \snext22 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Code_Indented_Bulleted),
		       Font_Size => 22,
		       Style_Indent => 1080,
		       Style_Hang_Width => 230,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s23\widctlpar\adjustright \li1080\fi-230\ri360\fs22\f0\cgrid\sa110\qj\sl-240\slmult0\tx1080 \snext23 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Code_Indented),
		       Font_Size => 22,
		       Style_Indent => 720,
		       Style_Before => 0,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s24\widctlpar\adjustright \li720\fs22\f0\cgrid\sa120\qj\sl-260\slmult0 \snext24 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Code_Indented),
		       Font_Size => 18,
		       Style_Indent => 1440,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s25\widctlpar\adjustright \li1440\fs18\f0\cgrid\sa90\qj\sl-200\slmult0 \snext25 ");
	    Set_Style (Paragraph_Info(ARM_Output.Indented_Examples),
		       Font_Size => 18,
		       Style_Indent => 1440,
		       Style_Before => 0,
		       Style_After => 100,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s26\widctlpar\adjustright \li1440\fs18\f2\cgrid\sa100\ql\sl-190\slmult0 \snext26 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Indented_Examples),
		       Font_Size => 16,
		       Style_Indent => 2160,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s27\widctlpar\adjustright \li2160\fs16\f2\cgrid\sa80\ql\sl-170\slmult0 \snext27 ");

            Set_Style (Header_Info,
		       Font_Size => 20,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => FALSE,
		       Style_String =>
		         "\s28\widctlpar\tqc\tx" & Half_Paper_Width &
			 "\tqr\tx" & Paper_Width & "\adjustright \fs20\cgrid \sbasedon0 \snext28 ");
            Set_Style (Footer_Info,
		       Font_Size => 20,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => FALSE,
		       Style_String =>
		         "\s29\widctlpar" & -- "\tqc\tx" & Half_Paper_Width & -- We don't use or define the center tab; it causes problems with very long titles.
			 "\tqr\tx" & Paper_Width & "\adjustright \fs20\cgrid \sbasedon0 \snext29 ");
	    Set_Style (Paragraph_Info(ARM_Output.Index),
		       Font_Size => 18,
		       Style_Indent => 270,
		       Style_Hang_Width => 270,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s31\widctlpar\adjustright \fs18\f0\cgrid\li270\fi-270\ql\sl-200\slmult0 \snext31 ");
	    Set_Style (Paragraph_Info(ARM_Output.Wide),
		       Font_Size => 22,
		       Style_Indent => 0,
		       Style_Before => 120,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s32\widctlpar\adjustright \fs22\f0\cgrid\sa120\sb120\qj\sl-260\slmult0 \snext0 ");
	    Set_Style (Paragraph_Info(ARM_Output.Wide_Annotations),
		       Font_Size => 18,
		       Style_Indent => 720,
		       Style_Before => 90,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s33\widctlpar\adjustright \li720\fs18\f0\cgrid\sa90\sb90\qj\sl-200\slmult0 \snext7 ");
	    Set_Style (Paragraph_Info(ARM_Output.Notes_Header),
		       Font_Size => 18,
		       Style_Indent => 360,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s34\widctlpar\adjustright \li360\fs18\f0\cgrid\sa0\qj\sl-200\slmult0 \snext6 ");
		      -- Note: No space afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Notes_Bulleted),
		       Font_Size => 18,
		       Style_Indent => 720,
		       Style_Hang_Width => 230,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s35\widctlpar\adjustright \li720\fi-230\ri360\fs18\f0\cgrid\sa90\qj\sl-190\slmult0\tx720 \snext35 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Notes_Nested_Bulleted),
		       Font_Size => 18,
		       Style_Indent => 1080,
		       Style_Hang_Width => 200,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s36\widctlpar\adjustright \li1080\fi-200\ri360\fs18\f0\cgrid\sa90\qj\sl-190\slmult0\tx1080 \snext36 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Hanging_in_Bulleted),
		       Font_Size => 22,
		       Style_Indent => 1080,
		       Style_Hang_Width => 720,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s37\widctlpar\adjustright \li1080\fi-720\ri360\fs22\f0\cgrid\sa110\qj\sl-240\slmult0 \snext14 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Hanging_in_Bulleted),
		       Font_Size => 18,
		       Style_Indent => 1800,
		       Style_Hang_Width => 720,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s38\widctlpar\adjustright \li1800\fi-720\ri360\fs18\f0\cgrid\sa90\qj\sl-190\slmult0 \snext16 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Code_Indented_Nested_Bulleted),
		       Font_Size => 22,
		       Style_Indent => 1440,
		       Style_Hang_Width => 200,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s39\widctlpar\adjustright \li1440\fi-200\ri360\fs22\f0\cgrid\sa110\qj\sl-240\slmult0\tx1440 \snext39 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Syntax_Summary),
		       Font_Size => 18,
		       Style_Indent => 360,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s40\widctlpar\adjustright \fs18\f0\cgrid\ql\li360\sa90\sl-200\slmult0 \snext40 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Enumerated),
		       Font_Size => 22,
		       Style_Indent => 360,
		       Style_Hang_Width => 230,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s41\widctlpar\adjustright \li360\fi-230\ri360\fs22\f0\cgrid\sa110\qj\sl-240\slmult0\tx360 \snext41 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Enumerated),
		       Font_Size => 18,
		       Style_Indent => 1080,
		       Style_Hang_Width => 200,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s42\widctlpar\adjustright \li1080\fi-200\ri360\fs18\f0\cgrid\sa90\qj\sl-190\slmult0\tx1080 \snext42 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Nested_Enumerated),
		       Font_Size => 22,
		       Style_Indent => 720,
		       Style_Hang_Width => 230,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s43\widctlpar\adjustright \li720\fi-230\ri360\fs22\f0\cgrid\sa110\qj\sl-240\slmult0\tx360 \snext43 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Nested_Enumerated),
		       Font_Size => 18,
		       Style_Indent => 1440,
		       Style_Hang_Width => 200,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s44\widctlpar\adjustright \li1440\fi-200\ri360\fs18\f0\cgrid\sa90\qj\sl-190\slmult0\tx1080 \snext44 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Nested_X2_Bulleted),
		       Font_Size => 22,
		       Style_Indent => 1080,
		       Style_Hang_Width => 200,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s45\widctlpar\adjustright \li1080\fi-200\ri360\fs22\f0\cgrid\sa110\qj\sl-240\slmult0\tx720 \snext45 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Small_Nested_Bulleted),
		       Font_Size => 18,
		       Style_Indent => 1800,
		       Style_Hang_Width => 180,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s46\widctlpar\adjustright \li1800\fi-180\ri360\fs18\f0\cgrid\sa90\qj\sl-190\slmult0\tx1440 \snext46 ");
			  -- Note: Narrower space between.
	    Set_Style (Paragraph_Info(ARM_Output.Indented_Nested_Bulleted),
		       Font_Size => 22,
		       Style_Indent => 1800,
		       Style_Hang_Width => 230,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s47\widctlpar\adjustright \li1800\fi-230\ri360\fs22\f0\cgrid\sa120\qj\sl-240\slmult0\tx1080 \snext47 ");
			  -- Note: Narrower space between and afterwards.
	    Set_Style (Paragraph_Info(ARM_Output.Inner_Indented),
		       Font_Size => 22,
		       Style_Indent => 1440,
		       Style_Before => 0,
		       Style_After => 120,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s48\widctlpar\adjustright \li1440\fs22\f0\cgrid\sa120\qj\sl-260\slmult0 \snext48 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Inner_Indented),
		       Font_Size => 18,
		       Style_Indent => 21600,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s49\widctlpar\adjustright \li1800\fs18\f0\cgrid\sa90\qj\sl-200\slmult0 \snext49 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Syntax_Indented),
		       Font_Size => 18,
		       Style_Indent => 1080,
		       Style_Before => 0,
		       Style_After => 90,
		       Style_Justified => TRUE,
		       Style_String =>
			 "\s50\widctlpar\adjustright \li1440\fs18\f0\cgrid\sa90\qj\sl-200\slmult0 \snext50 ");
	    Set_Style (Paragraph_Info(ARM_Output.Swiss_Examples),
		       Font_Size => 20,
		       Style_Indent => 360,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s51\widctlpar\adjustright \li360\fs20\f1\cgrid\sa110\ql\sl-230\slmult0 \snext51 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Swiss_Examples),
		       Font_Size => 16,
		       Style_Indent => 1080,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s52\widctlpar\adjustright \li1080\fs16\f1\cgrid\sa80\ql\sl-180\slmult0 \snext52 ");
	    Set_Style (Paragraph_Info(ARM_Output.Swiss_Indented_Examples),
		       Font_Size => 20,
		       Style_Indent => 1440,
		       Style_Before => 0,
		       Style_After => 110,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s53\widctlpar\adjustright \li1440\fs20\f1\cgrid\sa110\ql\sl-230\slmult0 \snext53 ");
	    Set_Style (Paragraph_Info(ARM_Output.Small_Swiss_Indented_Examples),
		       Font_Size => 16,
		       Style_Indent => 2160,
		       Style_Before => 0,
		       Style_After => 80,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\s54\widctlpar\adjustright \li2160\fs16\f1\cgrid\sa80\ql\sl-180\slmult0 \snext54 ");
	    if Output_Object.Big_Files then
		-- Define the TOC styles:
                Set_Style (TOC_1_Info,
		           Font_Size => 24,
		           Style_Indent => 0,
		           Style_Before => 60,
			   Style_After => 60,
		           Style_Justified => FALSE,
		           Style_String =>
		             "\s55\sb60\sa60\widctlpar\tqr\tldot\tx" & Paper_Width &
				"\adjustright \b\f1\fs24\cgrid \sbasedon0 \snext0 ");
                Set_Style (TOC_2_Info,
		           Font_Size => 22,
		           Style_Indent => 200,
		           Style_Before => 0,
			   Style_After => 0,
		           Style_Justified => FALSE,
		           Style_String =>
		             "\s56\li200\widctlpar\tqr\tldot\tx" & Paper_Width &
				"\adjustright \b\f1\fs22\cgrid \sbasedon0 \snext0 ");
                Set_Style (TOC_3_Info,
		           Font_Size => 22,
		           Style_Indent => 400,
		           Style_Before => 0,
			   Style_After => 0,
		           Style_Justified => FALSE,
		           Style_String =>
		             "\s57\li400\widctlpar\tqr\tldot\tx" & Paper_Width &
				"\adjustright \b\f1\fs22\cgrid \sbasedon0 \snext0 ");
	    end if;
	    Set_Style (Table_Text_Info,
		       Font_Size => 22,
		       Style_Indent => 0,
		       Style_Before => 0,
		       Style_After => 0,
		       Style_Justified => FALSE,
		       Style_String =>
			 "\fs22\f0\qc ");
	end if;

	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Normal));
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
	Write_Style (Output_Object.Output_File, Paragraph_Number_Info);
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Paragraph Number;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Notes));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Notes;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Annotations));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Annotations;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Examples));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Examples;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Examples));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Examples;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Syntax_Indented));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Syntax Indented;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Indented));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Normal Indented;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Indented));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Indented;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Hanging));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Hanging;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Indented_Hanging));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Indented Hanging;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Hanging));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Hanging;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Indented_Hanging));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Indented Hanging;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Bulleted));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Nested_Bulleted));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Nested Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Bulleted));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Nested_Bulleted));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Nested Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Indented_Bulleted));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Indented Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Syntax_Indented_Bulleted));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Syntax Indented Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Code_Indented_Bulleted));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Code Indented Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Code_Indented));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Code Indented;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Code_Indented));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Code Indented;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Indented_Examples));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Indented Examples;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Indented_Examples));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Indented Examples;}");
	Write_Style (Output_Object.Output_File, Header_Info);
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "header;}");
	Write_Style (Output_Object.Output_File, Footer_Info);
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "footer;}");
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\*\cs30 \additive \sbasedon30 page number;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Index));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Index;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Wide));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Wide;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Wide_Annotations));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Wide Annotations;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Notes_Header));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Notes Header;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Notes_Bulleted));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Notes Bulleted;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Notes_Nested_Bulleted));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Notes Nested Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Hanging_in_Bulleted));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Hanging in Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Hanging_in_Bulleted));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Hanging in Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Code_Indented_Nested_Bulleted));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Code Indented Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Syntax_Summary));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Syntax Summary;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Enumerated));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Enumerated;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Enumerated));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Enumerated;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Nested_Enumerated));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Nested Enumerated;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Nested_Enumerated));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Nested Enumerated;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Nested_X2_Bulleted));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Nested X2 Bulleted;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Nested_X2_Bulleted));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Nested X2 Bulleted;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Indented_Nested_Bulleted));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Indented Nested Bulleted;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Inner_Indented));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Inner Indented;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Inner_Indented));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Inner Indented;}");
        Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Syntax_Indented));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Syntax Indented;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Swiss_Examples));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Swiss Examples;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Swiss_Examples));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Swiss Examples;}");
	Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Swiss_Indented_Examples));
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "Swiss Indented Examples;}");
        if Output_Object.Big_Files then
	    Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Swiss_Indented_Examples));
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Swiss Indented Examples;}");
	    -- Define the TOC styles:
	    Write_Style (Output_Object.Output_File, TOC_1_Info);
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "toc 1;}");
	    Write_Style (Output_Object.Output_File, TOC_2_Info);
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "toc 2;}");
	    Write_Style (Output_Object.Output_File, TOC_3_Info);
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "toc 3;}}");
	else
	    Write_Style (Output_Object.Output_File, Paragraph_Info(ARM_Output.Small_Swiss_Indented_Examples));
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "Small Swiss Indented Examples;}}");
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
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\*\revtbl {Original Text;}{Technical Corrigendum 1;}{Unused Hot Pink;}{Amendment 1;}}");

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


    function Adj (Version : ARM_Contents.Change_Version_Type) return ARM_Contents.Change_Version_Type is
       -- Adjust the revision version number to match the revision table.
       use type ARM_Contents.Change_Version_Type;
    begin
        -- Code to skip over weirdly colored second revision (mostly a problem
	-- on Word 98).
        if Version = '0' or else Version = '1' then
	    return Version;
        else
	    return ARM_Contents.Change_Version_Type'Succ(Version);
        end if;
    end Adj;


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
		      File_Prefix : in String;
		      Header_Prefix : in String := "";
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
	-- The primary font used for the Sans_Serif text, and for the Serif
	-- text, is as specified.
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
	Ada.Strings.Fixed.Move (Target => Output_Object.File_Prefix,
			        Source => File_Prefix);
	Output_Object.Title := Ada.Strings.Unbounded.To_Unbounded_String (Title);
	Output_Object.Header_Prefix :=
		Ada.Strings.Unbounded.To_Unbounded_String (Header_Prefix);
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
			Format : in ARM_Output.Paragraph_Type) is
	-- Set tabs in the current (just started) paragraph.
    begin
	case Format is
	    when ARM_Output.Normal | ARM_Output.Wide |
		 ARM_Output.Notes | ARM_Output.Notes_Header |
		 ARM_Output.Annotations | ARM_Output.Wide_Annotations |
		 ARM_Output.Index | ARM_Output.Syntax_Summary |
		 ARM_Output.Examples | ARM_Output.Small_Examples |
		 ARM_Output.Indented_Examples | ARM_Output.Small_Indented_Examples |
		 ARM_Output.Swiss_Examples | ARM_Output.Small_Swiss_Examples |
		 ARM_Output.Swiss_Indented_Examples | ARM_Output.Small_Swiss_Indented_Examples |
		 ARM_Output.Syntax_Indented | ARM_Output.Small_Syntax_Indented |
		 ARM_Output.Indented | ARM_Output.Small_Indented |
		 ARM_Output.Inner_Indented | ARM_Output.Small_Inner_Indented |
		 ARM_Output.Code_Indented | ARM_Output.Small_Code_Indented =>
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
			            Paragraph_Info(Format).Indent;
			            -- *120 is to convert picas to Twips.
			    else
				-- Scale with font size. (Stop assumes 12 pt
				-- type).
				-- Raw formula:
				-- (Stop.Stop * 120) -- Stop in twips.
				-- * (Paragraph_Info(Format).Size / 24) -- Font scale.
				-- After rearranging, we get:
				return
			            Stop.Stop * Paragraph_Info(Format).Size * 5 +
			            Paragraph_Info(Format).Indent;
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

	    when ARM_Output.Bulleted | ARM_Output.Nested_Bulleted | ARM_Output.Nested_X2_Bulleted |
		 ARM_Output.Small_Bulleted | ARM_Output.Small_Nested_Bulleted | ARM_Output.Small_Nested_X2_Bulleted |
		 ARM_Output.Indented_Bulleted | ARM_Output.Indented_Nested_Bulleted |
		 ARM_Output.Code_Indented_Bulleted |
		 ARM_Output.Code_Indented_Nested_Bulleted |
		 ARM_Output.Syntax_Indented_Bulleted |
		 ARM_Output.Notes_Bulleted | ARM_Output.Notes_Nested_Bulleted |
		 ARM_Output.Hanging | ARM_Output.Indented_Hanging |
		 ARM_Output.Small_Hanging | ARM_Output.Small_Indented_Hanging |
		 ARM_Output.Hanging_in_Bulleted | ARM_Output.Small_Hanging_in_Bulleted |
		 ARM_Output.Enumerated | ARM_Output.Small_Enumerated |
		 ARM_Output.Nested_Enumerated | ARM_Output.Small_Nested_Enumerated =>
		if Output_Object.Tab_Stops.Number /= 0 then
	            Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		        "Tabs in hanging/bulleted paragraph");
		end if;
	end case;
    end Set_Tabs;


    procedure Start_Paragraph (Output_Object : in out RTF_Output_Type;
			       Format : in ARM_Output.Paragraph_Type;
			       Number : in String;
			       No_Prefix : in Boolean := False;
			       Tab_Stops : in ARM_Output.Tab_Info := ARM_Output.NO_TABS;
			       No_Breaks : in Boolean := False;
			       Keep_with_Next : in Boolean := False;
			       Space_After : in ARM_Output.Space_After_Type
				   := ARM_Output.Normal;
			       Justification : in ARM_Output.Justification_Type
				   := ARM_Output.Default) is
	-- Start a new paragraph. The format of the paragraph is as specified.
	-- The (AA)RM paragraph number (which might include update and version
	-- numbers as well: [12.1/1]) is Number. If the format is a type with
	-- a prefix (bullets, hangining items), the prefix is omitted if
	-- No_Prefix is true. Tab_Stops defines the tab stops for the
	-- paragraph. If No_Breaks is True, we will try to avoid page breaks
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
	Output_Object.Is_In_Paragraph := True;
	Output_Object.Had_Prefix := not No_Prefix;
	Output_Object.Char_Count := 0;
	Output_Object.Saw_Hang_End := False;
	Output_Object.Wrote_into_Section := True;

	-- First, write the paragraph number, if any. This has its own style.
	if Number /= "" then -- No paragraph numbers.
	    Write_Style_for_Paragraph (Output_Object.Output_File,
	        Paragraph_Number_Info, Output_Object.Char_Count);
	    -- Figure the space above: (We use a variable space above so the
	    -- numbers align with the bottom of the text, not the top).
	    declare
		Diff : Natural := (Paragraph_Info(Format).Size -
				   Paragraph_Number_Info.Size) +
				  (Paragraph_Info(Format).Before/10);
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
	    Ada.Text_IO.Put (Output_Object.Output_File, Number);
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "\par}");
	    Output_Object.Char_Count := 0;
	end if;
	-- Now, write the paragraph header:
	case Format is
	    when ARM_Output.Normal | ARM_Output.Wide |
		 ARM_Output.Notes | ARM_Output.Notes_Header |
		 ARM_Output.Annotations | ARM_Output.Wide_Annotations |
		 ARM_Output.Index | ARM_Output.Syntax_Summary |
		 ARM_Output.Examples | ARM_Output.Small_Examples |
		 ARM_Output.Indented_Examples | ARM_Output.Small_Indented_Examples |
		 ARM_Output.Swiss_Examples | ARM_Output.Small_Swiss_Examples |
		 ARM_Output.Swiss_Indented_Examples | ARM_Output.Small_Swiss_Indented_Examples |
		 ARM_Output.Syntax_Indented | ARM_Output.Small_Syntax_Indented |
		 ARM_Output.Indented | ARM_Output.Small_Indented |
		 ARM_Output.Inner_Indented | ARM_Output.Small_Inner_Indented |
		 ARM_Output.Code_Indented | ARM_Output.Small_Code_Indented =>
		Write_Style_for_Paragraph (Output_Object.Output_File,
		    Paragraph_Info(Format),
		    Output_Object.Char_Count);
	    when ARM_Output.Bulleted | ARM_Output.Nested_Bulleted | ARM_Output.Nested_X2_Bulleted |
		 ARM_Output.Small_Bulleted | ARM_Output.Small_Nested_Bulleted | ARM_Output.Small_Nested_X2_Bulleted |
		 ARM_Output.Indented_Bulleted | ARM_Output.Indented_Nested_Bulleted |
		 ARM_Output.Code_Indented_Bulleted |
		 ARM_Output.Code_Indented_Nested_Bulleted |
		 ARM_Output.Syntax_Indented_Bulleted |
		 ARM_Output.Notes_Bulleted | ARM_Output.Notes_Nested_Bulleted =>
		Write_Style_for_Paragraph (Output_Object.Output_File,
		    Paragraph_Info(Format),
		    Output_Object.Char_Count);
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "\tab ");
		    Output_Object.Char_Count := Output_Object.Char_Count + 5;
		else
		    if ARM_Output."=" (Format, ARM_Output.Nested_Bulleted) or else
		       ARM_Output."=" (Format, ARM_Output.Nested_X2_Bulleted) or else
		       ARM_Output."=" (Format, ARM_Output.Code_Indented_Nested_Bulleted) or else
		       ARM_Output."=" (Format, ARM_Output.Notes_Nested_Bulleted) or else
		       ARM_Output."=" (Format, ARM_Output.Small_Nested_Bulleted) or else
		       ARM_Output."=" (Format, ARM_Output.Small_Nested_X2_Bulleted) then
			-- Make a smaller bullet.
		        if Paragraph_Info(Format).Size = 15 then
	    	            Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\fs12\'b7}\tab ");
		        elsif Paragraph_Info(Format).Size = 16 then
	    	            Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\fs12\'b7}\tab ");
		        elsif Paragraph_Info(Format).Size = 18 then
	    	            Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\fs14\'b7}\tab ");
		        elsif Paragraph_Info(Format).Size = 20 then
	    	            Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\fs16\'b7}\tab ");
		        else --if Paragraph_Info(Format).Size = 22 then
	    	            Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\fs18\'b7}\tab ");
			end if;
		        Output_Object.Char_Count := Output_Object.Char_Count + 19;
		    else -- Normal bullet.
	    	        Ada.Text_IO.Put (Output_Object.Output_File, "{\f3\'b7}\tab ");
		        Output_Object.Char_Count := Output_Object.Char_Count + 14;
		    end if;
		end if;
	    when ARM_Output.Hanging | ARM_Output.Indented_Hanging |
		 ARM_Output.Small_Hanging | ARM_Output.Small_Indented_Hanging |
		 ARM_Output.Hanging_in_Bulleted | ARM_Output.Small_Hanging_in_Bulleted |
		 ARM_Output.Enumerated | ARM_Output.Small_Enumerated |
		 ARM_Output.Nested_Enumerated | ARM_Output.Small_Nested_Enumerated =>
		Write_Style_for_Paragraph (Output_Object.Output_File,
		    Paragraph_Info(Format),
		    Output_Object.Char_Count);
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "\tab ");
		    Output_Object.Char_Count := Output_Object.Char_Count + 5;
		    Output_Object.Saw_Hang_End := True;
		else -- Has prefix.
		    Output_Object.Saw_Hang_End := False;
		    Output_Object.Prefix_Large_Char_Count := 0;
		end if;
	end case;

	Output_Object.Paragraph_Format := Format;
	Output_Object.Font := ARM_Output.Default;
	Output_Object.Is_Bold := False;
	Output_Object.Is_Italic := False;
	Output_Object.Size := 0;
	Output_Object.Real_Size := Paragraph_Info(Format).Size;
	Output_Object.Tab_Stops := Tab_Stops;
	Output_Object.Current_Space_After := Space_After;

	Set_Tabs (Output_Object, Format);

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
		SA : constant String := Natural'Image((Paragraph_Info(Format).After*(LEADING_PERCENT/10))/10);
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
		SA : constant String := Natural'Image((Paragraph_Info(Format).After*(TRAILING_PERCENT/10))/10);
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
	case Format is
	    when ARM_Output.Hanging | ARM_Output.Indented_Hanging |
		 ARM_Output.Small_Hanging | ARM_Output.Small_Indented_Hanging |
		 ARM_Output.Hanging_in_Bulleted | ARM_Output.Small_Hanging_in_Bulleted |
		 ARM_Output.Enumerated | ARM_Output.Small_Enumerated |
		 ARM_Output.Nested_Enumerated | ARM_Output.Small_Nested_Enumerated =>
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
	       ARM_Contents."="(Level, ARM_Contents.Subclause) then
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
            Ada.Text_IO.Put (Output_Object.Output_File, "{\f0 ");
            Ada.Text_IO.Put (Output_Object.Output_File, Header_Text);
            Ada.Text_IO.Put (Output_Object.Output_File, "\tab ");
	    Ada.Text_IO.Put (Output_Object.Output_File, Current_Date);
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "\~\~\~\~\~\~{\field{\*\fldinst { PAGE }}{\fldrslt {\lang1024 x}}}\par}}}");
	    Ada.Text_IO.Put (Output_Object.Output_File, "{\footerr ");
	    Write_Style_for_Paragraph (Output_Object.Output_File, Footer_Info, Count);
	    Ada.Text_IO.Put (Output_Object.Output_File, "{\f0 {\field{\*\fldinst { PAGE }}{\fldrslt {\lang1024 x}}}\~\~\~\~\~\~");
	    Ada.Text_IO.Put (Output_Object.Output_File, Current_Date);
	    Ada.Text_IO.Put (Output_Object.Output_File, "\tab ");
            Ada.Text_IO.Put (Output_Object.Output_File, Header_Text);
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "\par}}}");
	else
	    Ada.Text_IO.Put (Output_Object.Output_File, "{\footerl ");
	    Write_Style_for_Paragraph (Output_Object.Output_File, Footer_Info, Count);
            Ada.Text_IO.Put (Output_Object.Output_File, "{\b\f1 ");
	    if ARM_Contents."="(Level, ARM_Contents.Normative_Annex) or else
	       ARM_Contents."="(Level, ARM_Contents.Informative_Annex) then
		-- Clause Number includes "Annex". Just use the letter.
		Ada.Text_IO.Put (Output_Object.Output_File, Clause_Number(Clause_Number'Last));
	    else
		Ada.Text_IO.Put (Output_Object.Output_File, Clause_Number);
	    end if;
            Ada.Text_IO.Put (Output_Object.Output_File, "}\~\~\~{\f0 ");
            Ada.Text_IO.Put (Output_Object.Output_File, Header_Text);
            Ada.Text_IO.Put (Output_Object.Output_File, "\tab ");
	    Ada.Text_IO.Put (Output_Object.Output_File, Current_Date);
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "\~\~\~\~\~\~{\field{\*\fldinst { PAGE }}{\fldrslt {\lang1024 x}}}\par}}}");
	    Ada.Text_IO.Put (Output_Object.Output_File, "{\footerr ");
	    Write_Style_for_Paragraph (Output_Object.Output_File, Footer_Info, Count);
	    Ada.Text_IO.Put (Output_Object.Output_File, "{\f0 {\field{\*\fldinst { PAGE }}{\fldrslt {\lang1024 x}}}\~\~\~\~\~\~");
	    Ada.Text_IO.Put (Output_Object.Output_File, Current_Date);
	    Ada.Text_IO.Put (Output_Object.Output_File, "\tab ");
            Ada.Text_IO.Put (Output_Object.Output_File, Header_Text);
	    Ada.Text_IO.Put (Output_Object.Output_File, "\~\~\~\b\f1 ");
	    if ARM_Contents."="(Level, ARM_Contents.Normative_Annex) or else
	       ARM_Contents."="(Level, ARM_Contents.Informative_Annex) then
		-- Clause Number includes "Annex". Just use the letter.
		Ada.Text_IO.Put (Output_Object.Output_File, Clause_Number(Clause_Number'Last));
	    else
		Ada.Text_IO.Put (Output_Object.Output_File, Clause_Number);
	    end if;
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "\par}}}");
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
	if Clause_Number = "" and then Header_Text = "Table of Contents" then
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "{\pard\plain \s1\sb240\sa60\keepn\widctlpar\outlinelevel0\adjustright \b\f1\fs36\kerning36\qc\cgrid Table of Contents\par}");
	    Output_Object.Char_Count := 0;
	    return;
	end if;

	case Level is
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
	end case;
	Output_Object.Char_Count := 0;
    end Clause_Header;


    procedure Revised_Clause_Header (Output_Object : in out RTF_Output_Type;
			     New_Header_Text : in String;
			     Old_Header_Text : in String;
			     Level : in ARM_Contents.Level_Type;
			     Clause_Number : in String;
			     Version : in ARM_Contents.Change_Version_Type;
			     No_Page_Break : in Boolean := False) is
	-- Output a revised clause header. Both the original and new text will
	-- be output. The level of the header is specified in Level. The Clause
	-- Number is as specified.
	-- These should appear in the table of contents.
	-- For hyperlinked formats, this should generate a link target.
	-- If No_Page_Break is True, suppress any page breaks.
	-- Raises Not_Valid_Error if in a paragraph.
	Count : Natural; -- Not used after being set.
	function Header_Text return String is
	begin
	    return "{\revised\revauth" & Adj(Version) & " " & New_Header_Text & "}{\deleted\revauthdel" & Adj(Version) & " " & Old_Header_Text & "}";
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
		    Paragraph_Info(ARM_Output.Normal),
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
	    FS : constant String := Natural'Image(Paragraph_Info(Output_Object.Paragraph_Format).Size);
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


    procedure RTF_Table_Info (Output_Object : in out RTF_Output_Type) is
	-- Output the current table definition (Word needs this on every row):
    begin
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\trowd \trgaph108 ");

        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\trrh" &
	    Format_Value(Paragraph_Info(ARM_Output.Normal).Size * 16) &
	    "\trleft" & Format_Value(Output_Object.Table_Indent) & " ");

	-- Set all of the borders to the normal:
        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\trbrdrt\brdrs\brdrw10 " &
	    "\trbrdrl\brdrs\brdrw10 " & "\trbrdrb\brdrs\brdrw10 " &
	    "\trbrdrr\brdrs\brdrw10 " & "\trbrdrh\brdrs\brdrw10 " &
	    "\trbrdrv\brdrs\brdrw10 ");

        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\trkeep\trkeepfollow ");
    end RTF_Table_Info;


    procedure Start_Table (Output_Object : in out RTF_Output_Type;
			   Columns : in ARM_Output.Column_Count) is
	-- Starts a table. The number of columns is Columns.
	-- This command starts a paragraph; the entire table is a single
	-- paragraph. Text will be considered part of the caption until the
	-- next table marker call.
	-- Raises Not_Valid_Error if in a paragraph.
	Page_Width : Natural;
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
	if Columns <= 3 then
	    Output_Object.Table_Indent := Page_Width / 6;
            Output_Object.Table_Width  := Page_Width - Output_Object.Table_Indent*2;
            Output_Object.Table_Column_Width  := Output_Object.Table_Width / Columns;
	else
	    -- ** Kludge alert **
	    -- Whether the first column is wider ought to be a parameter here,
	    -- not assumed.
	    Output_Object.Table_Indent := Page_Width / 16;
            Output_Object.Table_Width  := Page_Width - Output_Object.Table_Indent*2;
            Output_Object.Table_Column_Width  := Output_Object.Table_Width / (Columns+1);
	end if;
	Output_Object.Column_Count := Columns;

	-- Make a blank line before, of the right size:
	Write_Style_for_Paragraph (Output_Object.Output_File,
				   Table_Text_Info,
				   Output_Object.Char_Count);
        Ada.Text_IO.Put (Output_Object.Output_File, "\par }");
	Output_Object.Char_Count := 0;

	RTF_Table_Info (Output_Object);

	-- Now, define the cell borders:
        Ada.Text_IO.Put_Line (Output_Object.Output_File,
	    "\clvertalc \clbrdrt\brdrs\brdrw10 " &
	    "\clbrdrl\brdrs\brdrw10 " &
	    "\clbrdrb\brdrs\brdrw10 " &
	    "\clbrdrr\brdrs\brdrw10 ");

        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cltxlrtb\cellx" &
	    Format_Value(Output_Object.Table_Indent + Output_Object.Table_Width) & " ");
	    -- Caption cell crosses entire line.

	-- Now, set up text (normal, centered):
	Write_Style_for_Paragraph (Output_Object.Output_File,
				   Table_Text_Info,
				   Output_Object.Char_Count);
        Ada.Text_IO.Put (Output_Object.Output_File, "\intbl ");
	Output_Object.Char_Count := Output_Object.Char_Count + 6;

	-- \trowd - Start a table row.
	-- \row - End a table row.
	-- \trgaph - Half of of the gap between cells, in twips.
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
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cell ");
	        Output_Object.Char_Count := 0;
		-- Text format stays the same.

	    when ARM_Output.End_Caption =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cell }");
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\row "); -- End row.

		-- Start header row:
		RTF_Table_Info (Output_Object); -- Repeat table definition.

		-- Now, define the cell borders for each cell:
		for I in 1 .. Output_Object.Column_Count loop
	            Ada.Text_IO.Put_Line (Output_Object.Output_File,
		        "\clvertalc \clbrdrt\brdrs\brdrw10 " &
		        "\clbrdrl\brdrs\brdrw10 " &
		        "\clbrdrb\brdrs\brdrw10 " &
		        "\clbrdrr\brdrs\brdrw10 ");

		    if Output_Object.Column_Count > 3 then
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cltxlrtb\cellx" &
			    Format_Value(Output_Object.Table_Indent + Output_Object.Table_Column_Width*(Integer(I)+1)) & " ");
		    else
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cltxlrtb\cellx" &
			    Format_Value(Output_Object.Table_Indent + Output_Object.Table_Column_Width*Integer(I)) & " ");
		    end if;
		end loop;

		-- Now, define text format:
		Write_Style_for_Paragraph (Output_Object.Output_File,
					   Table_Text_Info,
					   Output_Object.Char_Count);
	        Ada.Text_IO.Put (Output_Object.Output_File, "\intbl ");
		Output_Object.Char_Count := Output_Object.Char_Count + 6;

	    when ARM_Output.End_Header =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cell }");
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\row "); -- End row.

		-- Start 1st body row:
		RTF_Table_Info (Output_Object); -- Repeat table definition.

		-- Now, define the cell borders for each cell:
		for I in 1 .. Output_Object.Column_Count loop
	            Ada.Text_IO.Put_Line (Output_Object.Output_File,
		        "\clvertalc \clbrdrt\brdrs\brdrw10 " &
		        "\clbrdrl\brdrs\brdrw10 " &
		        "\clbrdrr\brdrs\brdrw10 ");

		    if Output_Object.Column_Count > 3 then
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cltxlrtb\cellx" &
			    Format_Value(Output_Object.Table_Indent + Output_Object.Table_Column_Width*(Integer(I)+1)) & " ");
		    else
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cltxlrtb\cellx" &
			    Format_Value(Output_Object.Table_Indent + Output_Object.Table_Column_Width*Integer(I)) & " ");
		    end if;
		end loop;

		-- Now, define text format:
		Write_Style_for_Paragraph (Output_Object.Output_File,
					   Table_Text_Info,
					   Output_Object.Char_Count);
	        Ada.Text_IO.Put (Output_Object.Output_File, "\intbl ");
		Output_Object.Char_Count := Output_Object.Char_Count + 6;

	    when ARM_Output.End_Row =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cell }");
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\row "); -- End row.

		-- Start other body rows (no top border!):
		RTF_Table_Info (Output_Object); -- Repeat table definition.

		-- Now, define the cell borders for each cell:
		for I in 1 .. Output_Object.Column_Count loop
	            Ada.Text_IO.Put_Line (Output_Object.Output_File,
		        "\clvertalc \clbrdrl\brdrs\brdrw10 " &
		        "\clbrdrr\brdrs\brdrw10 ");

		    if Output_Object.Column_Count > 3 then
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cltxlrtb\cellx" &
			    Format_Value(Output_Object.Table_Indent + Output_Object.Table_Column_Width*(Integer(I)+1)) & " ");
		    else
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cltxlrtb\cellx" &
			    Format_Value(Output_Object.Table_Indent + Output_Object.Table_Column_Width*Integer(I)) & " ");
		    end if;
		end loop;

		-- Now, define text format:
		Write_Style_for_Paragraph (Output_Object.Output_File,
					   Table_Text_Info,
					   Output_Object.Char_Count);
	        Ada.Text_IO.Put (Output_Object.Output_File, "\intbl ");
		Output_Object.Char_Count := Output_Object.Char_Count + 6;

	    when ARM_Output.End_Row_Next_Is_Last =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cell }");
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\row "); -- End row.

		-- Start other body rows (no top border!):
		RTF_Table_Info (Output_Object); -- Repeat table definition.

		-- Now, define the cell borders for each cell:
		for I in 1 .. Output_Object.Column_Count loop
	            Ada.Text_IO.Put_Line (Output_Object.Output_File,
		        "\clvertalc \clbrdrl\brdrs\brdrw10 " &
		        "\clbrdrb\brdrs\brdrw10 " &
		        "\clbrdrr\brdrs\brdrw10 ");

		    if Output_Object.Column_Count > 3 then
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cltxlrtb\cellx" &
			    Format_Value(Output_Object.Table_Indent + Output_Object.Table_Column_Width*(Integer(I)+1)) & " ");
		    else
		        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cltxlrtb\cellx" &
			    Format_Value(Output_Object.Table_Indent + Output_Object.Table_Column_Width*Integer(I)) & " ");
		    end if;
		end loop;

		-- Now, define text format:
		Write_Style_for_Paragraph (Output_Object.Output_File,
					   Table_Text_Info,
					   Output_Object.Char_Count);
	        Ada.Text_IO.Put (Output_Object.Output_File, "\intbl ");
		Output_Object.Char_Count := Output_Object.Char_Count + 6;

	    when ARM_Output.End_Table =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\cell }");
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "\row "); -- End last row of table.

		Output_Object.Is_In_Paragraph := False;
		Output_Object.Is_In_Table := False;
		Output_Object.Column_Count := 1;

		-- Make a blank line after, of the right size:
		Write_Style_for_Paragraph (Output_Object.Output_File,
					   Table_Text_Info,
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
	   (Output_Object.Paragraph_Format not in ARM_Output.Hanging ..
	        ARM_Output.Small_Nested_Enumerated and then
	        (not Output_Object.Saw_Hang_End)) then
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
		if Output_Object.Paragraph_Format in ARM_Output.Hanging ..
		        ARM_Output.Small_Nested_Enumerated and then
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
		if Output_Object.Paragraph_Format in ARM_Output.Hanging ..
		        ARM_Output.Small_Nested_Enumerated and then
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
	if Output_Object.Paragraph_Format in ARM_Output.Examples ..
	        ARM_Output.Small_Indented_Examples then
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
	elsif not Paragraph_Info(Output_Object.Paragraph_Format).Is_Justified then
	    -- We can't use \Par, as that inserts paragraph spacing.
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "\line ");
            Output_Object.Char_Count := 0;
	else
	    -- We can't use \Line, as that will cause the line to be justified.
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "\sa0\par ");
	        -- We have to turn off the inter-paragraph spacing.
		-- Now, reset the \sa setting.
	    declare
		SA_Width : Natural := Paragraph_Info(Output_Object.Paragraph_Format).After;
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
	    if (Output_Object.Paragraph_Format in ARM_Output.Bulleted ..
	            ARM_Output.Small_Nested_Enumerated) then -- Always "NoPrefix" here.
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
	if ARM_Output."/=" (Output_Object.Paragraph_Format, ARM_Output.Index) then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in index paragraph");
	end if;
	-- We have to use /par here, because otherwise we don't get the "undent"
	-- at the start of the paragraph.

	if Clear_Keep_with_Next then
	    -- Note: We need this special routine, because ending the paragraph
	    -- would add blank lines to the HTML.
	    End_Paragraph (Output_Object);
	    Start_Paragraph (Output_Object, ARM_Output.Index,
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
	if Output_Object.Paragraph_Format in ARM_Output.Hanging ..
	        ARM_Output.Small_Nested_Enumerated and then
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
	if Output_Object.Paragraph_Format in ARM_Output.Hanging ..
	        ARM_Output.Small_Nested_Enumerated and then
	   (not Output_Object.Saw_Hang_End) then
	        Output_Object.Prefix_Large_Char_Count :=
	           Output_Object.Prefix_Large_Char_Count + 1;
	end if;
    end Unicode_Character;


    procedure End_Hang_Item (Output_Object : in out RTF_Output_Type) is
	-- Marks the end of a hanging item. Call only once per paragraph.
	-- Raises Not_Valid_Error if the paragraph format is not
	-- Hanging .. Small_Nested_Enumerated, or if this has already been
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
	if Output_Object.Paragraph_Format not in ARM_Output.Hanging ..
	        ARM_Output.Small_Nested_Enumerated then
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
--Ada.Text_Io.Put (" Format=" & ARM_Output.Paragraph_Type'Image(Output_Object.Paragraph_Format));
--Ada.Text_Io.Put (" Count=" & Natural'Image(
--      ((Paragraph_Info(Output_Object.Paragraph_Format).Hang_Width * 6 * 2) /
--       (Paragraph_Info(Output_Object.Paragraph_Format).Size * 5   * 5)) - 1));
--Ada.Text_Io.Put (" Hang_Width=" & Natural'Image(Paragraph_Info(Output_Object.Paragraph_Format).Hang_Width));
--Ada.Text_Io.Put (" Size=" & Natural'Image(Paragraph_Info(Output_Object.Paragraph_Format).Size));

        if Output_Object.Char_Count*2 + Output_Object.Prefix_Large_Char_Count
	    <= ((Paragraph_Info(Output_Object.Paragraph_Format).Hang_Width * 6 * 2) /
		(Paragraph_Info(Output_Object.Paragraph_Format).Size * 5   * 5)) - 1 then
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
	        Paragraph_Info(Output_Object.Paragraph_Format),
	        Output_Object.Char_Count);
	    Set_Tabs (Output_Object, Output_Object.Paragraph_Format);
	    -- Reset after spacing:
	    if ARM_Output."="(Output_Object.Current_Space_After,
	       ARM_Output.Narrow) then
	        declare
		    SA_Width : Natural := Paragraph_Info(Output_Object.Paragraph_Format).After*(LEADING_PERCENT/10)/10;
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
		    SA_Width : Natural := Paragraph_Info(Output_Object.Paragraph_Format).After*(TRAILING_PERCENT/10)/10;
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
			   Bold : in Boolean;
			   Italic : in Boolean;
			   Font : in ARM_Output.Font_Family_Type;
			   Size : in ARM_Output.Size_Type;
			   Change : in ARM_Output.Change_Type;
			   Version : in ARM_Contents.Change_Version_Type := '0';
			   Added_Version : in ARM_Contents.Change_Version_Type := '0';
			   Location : in ARM_Output.Location_Type) is
	-- Change the text format so that Bold, Italics, the font family,
	-- the text size, and the change state are as specified.
	-- Added_Version is only used when the change state is "Both"; it's
	-- the version of the insertion; Version is the version of the (newer)
	-- deletion.
	-- Note: Changes to these properties ought be stack-like; that is,
	-- Bold on, Italic on, Italic off, Bold off is OK; Bold on, Italic on,
	-- Bold off, Italic off should be avoided (as separate commands).
	use type ARM_Output.Change_Type;
	use type ARM_Contents.Change_Version_Type;
	use type ARM_Output.Location_Type;
	use type ARM_Output.Size_Type;

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
	        Output_Object.Size = 0 then
		-- No format was, so none to close (default).
		return;
	    end if;
--Ada.Text_Io.Put (" Close basic format");
	    Ada.Text_IO.Put (Output_Object.Output_File, "}");
	    Output_Object.Char_Count := Output_Object.Char_Count + 1;
	    Output_Object.Real_Size := Output_Object.Real_Size -
				    (Integer(Output_Object.Size)*2);
	    Output_Object.Size := 0;
	    Output_Object.Is_Bold := False;
	    Output_Object.Is_Italic := False;
	    case Output_Object.Font is
		when ARM_Output.Default => null;
		when ARM_Output.Fixed => null;
		when ARM_Output.Roman => null;
		when ARM_Output.Swiss =>
		    -- Undo the size adjustment.
		    Output_Object.Real_Size := Output_Object.Real_Size + 1;
	    end case;
	    Output_Object.Font := ARM_Output.Default;
	end Close_Basic_Format;


	procedure Make_Basic_Format is
	    -- Make any needed Bold/Italic/Size/Font command.
	begin
	    if (not Bold) and (not Italic) and
		ARM_Output."=" (Font, ARM_Output.Default) and
		Size = 0 then
		-- No format needed (default).
		return;
	    end if;
	    Ada.Text_IO.Put (Output_Object.Output_File, "{");
	    Output_Object.Char_Count := Output_Object.Char_Count + 1;

	    -- Bold:
	    if Bold then
--Ada.Text_Io.Put (" Change bold");
	        Ada.Text_IO.Put (Output_Object.Output_File, "\b");
	        Output_Object.Char_Count := Output_Object.Char_Count + 2;
	        Output_Object.Is_Bold := True;
	    end if;
	    -- Italic:
	    if Italic then
--Ada.Text_Io.Put (" Change italics");
	        Ada.Text_IO.Put (Output_Object.Output_File, "\i");
	        Output_Object.Char_Count := Output_Object.Char_Count + 2;
	        Output_Object.Is_Italic := True;
	    end if;
	    -- Size:
	    if Size /= 0 then
--Ada.Text_Io.Put (" Change size " & ARM_Output.Size_Type'Image(Size));
	        Output_Object.Real_Size := Output_Object.Real_Size +
						    Integer(Size)*2;
		if ARM_Output."/=" (Font, ARM_Output.Swiss) then
	            Make_Size_Command (Output_Object.Real_Size);
		-- else it will be done by the Font, below.
		end if;
	    end if;
	    Output_Object.Size := Size;
	    -- Font:
	    case Font is
		when ARM_Output.Default => null;
		when ARM_Output.Fixed =>
--Ada.Text_Io.Put (" Change font fixed");
		    Ada.Text_IO.Put (Output_Object.Output_File, "\f2");
		    Output_Object.Char_Count := Output_Object.Char_Count + 4;
		when ARM_Output.Roman =>
--Ada.Text_Io.Put (" Change font roman");
		    Ada.Text_IO.Put (Output_Object.Output_File, "\f0");
		    Output_Object.Char_Count := Output_Object.Char_Count + 4;
		when ARM_Output.Swiss =>
--Ada.Text_Io.Put (" Change font swiss");
		    Ada.Text_IO.Put (Output_Object.Output_File, "\f1");
		    Output_Object.Char_Count := Output_Object.Char_Count + 3;
		    -- Swiss fonts always appear too large, so shrink it a bit.
		    Output_Object.Real_Size := Output_Object.Real_Size - 1;
		    Make_Size_Command (Output_Object.Real_Size);
	    end case;
	    Ada.Text_IO.Put (Output_Object.Output_File, " ");
	    Output_Object.Char_Count := Output_Object.Char_Count + 1;
	    Output_Object.Font := Font;
	end Make_Basic_Format;


	procedure Make_Revision is
	    -- Make any needed revision:
	begin
	    -- We could "improve" this by keeping similar changes together,
	    -- especially for changes to/from Both, but its a lot more work
	    -- and unnecessary.
	    case Change is
		when ARM_Output.Insertion =>
--Ada.Text_Io.Put (" Change ins");
		    Ada.Text_IO.Put (Output_Object.Output_File, "{\revised\revauth" & Adj(Version) & ' ');
		    Output_Object.Char_Count := Output_Object.Char_Count + 18;
			-- Note: \revauthN indicates the author. Each version
			-- that we'll use needs an entry in the \revtbl.
			-- We could include a date with \revddtm??, but that's messy.
			-- (And we don't know the date of the revision yet.)
		when ARM_Output.Deletion =>
--Ada.Text_Io.Put (" Change del");
		    Ada.Text_IO.Put (Output_Object.Output_File, "{\deleted\revauthdel" & Adj(Version) & ' ');
		    Output_Object.Char_Count := Output_Object.Char_Count + 21;
			-- Note: \revauthdelN indicates the author. Each version
			-- that we'll use needs an entry in the \revtbl.
			-- We could include a date with \revddtmdel??, but that's messy.
			-- (And we don't know the date of the revision yet.)
		when ARM_Output.Both =>
--Ada.Text_Io.Put (" Change both");
		    Ada.Text_IO.Put (Output_Object.Output_File, "{\revised\revauth" & Adj(Added_Version) & ' ');
		    Output_Object.Char_Count := Output_Object.Char_Count + 18;
		    Ada.Text_IO.Put (Output_Object.Output_File, "{\deleted\revauthdel" & Adj(Version) & ' ');
		    Output_Object.Char_Count := Output_Object.Char_Count + 21;
			-- Note: \revauthdelN indicates the author. Each version
			-- that we'll use needs an entry in the \revtbl.
			-- We could include a date with \revddtmdel??, but that's messy.
			-- (And we don't know the date of the revision yet.)
		when ARM_Output.None =>
		    null;
	    end case;
	    Output_Object.Change := Change;
	    Output_Object.Version := Version;
	    Output_Object.Added_Version := Added_Version;
	end Make_Revision;


	procedure Close_Revision is
	    -- Close any open revision:
	begin
	    -- We could "improve" this by keeping similar changes together,
	    -- especially for changes to/from Both, but its a lot more work
	    -- and unnecessary.
	    case Output_Object.Change is
		when ARM_Output.Insertion =>
--Ada.Text_Io.Put (" Unchange ins");
		    Ada.Text_IO.Put (Output_Object.Output_File, "}");
		    Output_Object.Char_Count := Output_Object.Char_Count + 1;
		when ARM_Output.Deletion =>
--Ada.Text_Io.Put (" Unchange del");
		    Ada.Text_IO.Put (Output_Object.Output_File, "}");
		    Output_Object.Char_Count := Output_Object.Char_Count + 1;
		when ARM_Output.None =>
		    null;
		when ARM_Output.Both =>
--Ada.Text_Io.Put (" Unchange both");
	            Ada.Text_IO.Put (Output_Object.Output_File, "}}");
	            Output_Object.Char_Count := Output_Object.Char_Count + 2;
	    end case;
	end Close_Revision;


	procedure Make_Location is
	    -- Make any needed location:
	begin
	    case Location is
		when ARM_Output.Subscript =>
--Ada.Text_Io.Put (" Change sub");
		    Ada.Text_IO.Put (Output_Object.Output_File, "{\sub ");
		    Output_Object.Char_Count := Output_Object.Char_Count + 6;
		when ARM_Output.Superscript =>
--Ada.Text_Io.Put (" Change sup");
		    Ada.Text_IO.Put (Output_Object.Output_File, "{\super ");
		    Output_Object.Char_Count := Output_Object.Char_Count + 8;
		when ARM_Output.Normal =>
		    null;
	    end case;
	    Output_Object.Location := Location;
	end Make_Location;


	procedure Close_Location is
	    -- Close any open location:
	begin
	    case Output_Object.Location is
		when ARM_Output.Subscript =>
--Ada.Text_Io.Put (" Unchange sub");
		    Ada.Text_IO.Put (Output_Object.Output_File, "}");
		    Output_Object.Char_Count := Output_Object.Char_Count + 1;
		when ARM_Output.Superscript =>
--Ada.Text_Io.Put (" Unchange sup");
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

--Ada.Text_Io.Put ("Text format");
	-- We always make changes in the order:
	-- Revision;
	-- Location;
	-- Basic_Format (Bold, Italic, Font, Size).
	-- Thus, we have to unstack them in the reverse order. And, if we want
	-- to change an outer one, we have to close and redo any inner
	-- ones.

	-- We do these in this order so that the changes are stacked properly.
	if Change /= Output_Object.Change or else
	    Version /= Output_Object.Version or else
	    Added_Version /= Output_Object.Added_Version then
	    Close_Basic_Format;
	    Close_Location;
	    Close_Revision;
	    Make_Revision;
	    Make_Location;
	    Make_Basic_Format;
	elsif Location /= Output_Object.Location then
	    -- We don't need to change the revision, leave it alone.
	    Close_Basic_Format;
	    Close_Location;
	    Make_Location;
	    Make_Basic_Format;
	elsif Size /= Output_Object.Size or else
	   ARM_Output."/=" (Font, Output_Object.Font) or else
	   Bold /= Output_Object.Is_Bold or else
	   Italic /= Output_Object.Is_Italic then
	    Close_Basic_Format;
	    Make_Basic_Format;
	-- else no change at all.
	end if;
--Ada.Text_Io.New_Line;
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
	-- the target (in folded format). For hyperlinked formats, this should
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

-- Notes:
-- "\_" is a non-breaking hyphen.

end ARM_RTF;
