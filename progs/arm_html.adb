with ARM_Output,
     ARM_Contents,
     Ada.Text_IO,
     Ada.Exceptions,
     Ada.Strings.Maps,
     Ada.Strings.Fixed,
     Ada.Unchecked_Deallocation;
package body ARM_HTML is

    --
    -- Ada reference manual formatter.
    --
    -- This package defines the HTML output object.
    -- Output objects are responsible for implementing the details of
    -- a particular format.
    --
    -- ---------------------------------------
    -- Copyright 2000, AXE Consultants.
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

    LINE_LENGTH : constant := 78;
	-- Maximum intended line length.

    SWISS_FONT_CODE : constant String := "<FONT FACE=""Arial, Helvetica"">";

    TINY_SWISS_FONT_CODE : constant String := "<FONT FACE=""Arial, Helvetica"" SIZE=-2>";

    HTML_4 : constant Boolean := True;
	-- Use HTML 4 stuff. (Eventually, this will be a flag to the
	-- Create routine.)
    Use_Unicode : constant Boolean := False;
	-- Use Unicode characters. (Many browsers can't display these.)

    procedure Free is new Ada.Unchecked_Deallocation (Column_Text_Item_Type, Column_Text_Ptr);

    function Make_Clause_File_Name (Output_Object : in HTML_Output_Type;
				    Clause_Number : in String) return String is
	-- Internal routine.
	-- Returns the Clause file name for the current output object and
	-- Clause_Number. This does not include any path or extension.
        Clause_Name : String(1..7);
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
        case Output_Object.Document is
	    when ARM_Output.RM =>
		return "RM-" & Clause_Name(1..Clause_Name_Len);
	    when ARM_Output.RM_ISO =>
		return "RMI-" & Clause_Name(1..Clause_Name_Len);
	    when ARM_Output.AARM =>
		return "AA-" & Clause_Name(1..Clause_Name_Len);
        end case;
    end Make_Clause_File_Name;


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
	Ada.Text_IO.Create (Output_Object.Output_File, Ada.Text_IO.Out_File,
	    ".\Output\" & File_Name & ".html");
--Ada.Text_IO.Put_Line ("--Creating " & File_Name & ".html");
	-- File introduction:
	if HTML_4 then
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "<!DOCTYPE HTML PUBLIC ""-//W3C//DTD HTML 4.01 Transitional//EN""");
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, """http://www.w3.org/TR/html4/loose.dtd"">"); -- HTML 4.01 (with depreciated features,
	else
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "<!DOCTYPE HTML PUBLIC ""-//W3C//DTD HTML 3.2//EN"">"); -- HTML 3.2
	end if;
												       -- so the result can be used on version 3 browsers.)
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "<HTML>");
	-- Header information:
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "<HEAD>");
        case Output_Object.Document is
	    when ARM_Output.RM =>
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "    <TITLE>Ada95 - " & Title & "</TITLE>");
	    when ARM_Output.RM_ISO =>
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "    <TITLE>Ada95 - " & Title & "</TITLE>");
	    when ARM_Output.AARM =>
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "    <TITLE>AARM95 - " & Title & "</TITLE>");
        end case;
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "    <META NAME=""Author"" CONTENT=""JTC1/SC22/WG9/ARG, by Randall Brukardt, ARG Editor"">");
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "    <META NAME=""GENERATOR"" CONTENT=""Arm_Form.Exe, Ada Reference Manual generator"">");
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</HEAD>");
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "<BODY TEXT=""#000000"" BGCOLOR=""#FFFFF0"" LINK=""#0000FF"" VLINK=""#800080"" ALINK=""#FF0000"">");

	Ada.Text_IO.Put (Output_Object.Output_File, "<P><A HREF=""");
	case Output_Object.Document is
	    when ARM_Output.RM =>
		Ada.Text_IO.Put (Output_Object.Output_File, "RM-TOC.html");
	    when ARM_Output.RM_ISO =>
		Ada.Text_IO.Put (Output_Object.Output_File, "RMI-TOC.html");
	    when ARM_Output.AARM =>
		Ada.Text_IO.Put (Output_Object.Output_File, "AA-TOC.html");
        end case;
	Ada.Text_IO.Put (Output_Object.Output_File, """>Contents</A>");
	Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;&nbsp;&nbsp;");
	Ada.Text_IO.Put (Output_Object.Output_File, "<A HREF=""");
	case Output_Object.Document is
	    when ARM_Output.RM =>
		Ada.Text_IO.Put (Output_Object.Output_File, "RM-0-29.html");
	    when ARM_Output.RM_ISO =>
		Ada.Text_IO.Put (Output_Object.Output_File, "RMI-0-29.html");
	    when ARM_Output.AARM =>
		Ada.Text_IO.Put (Output_Object.Output_File, "AA-0-29.html");
        end case;
	Ada.Text_IO.Put (Output_Object.Output_File, """>Index</A>");
	if Clause /= "" then
	    begin
		-- Note: We do the following in one big glup so that if
		-- Not_Found_Error is raised, nothing is output.
		Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;&nbsp;&nbsp;<A HREF=""" &
		    Make_Clause_File_Name (Output_Object,
			ARM_Contents.Previous_Clause(Clause)) & ".html");
		Ada.Text_IO.Put (Output_Object.Output_File, """>Previous</A>");
	    exception
	        when ARM_Contents.Not_Found_Error =>
		    null; -- Probably the first section.
	    end;
	    begin
		-- Note: We do the following in one big glup so that if
		-- Not_Found_Error is raised, nothing is output.
		Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;&nbsp;&nbsp;<A HREF=""" &
		    Make_Clause_File_Name (Output_Object,
			ARM_Contents.Next_Clause(Clause)) & ".html");
		Ada.Text_IO.Put (Output_Object.Output_File, """>Next</A>");
	    exception
	        when ARM_Contents.Not_Found_Error =>
		    null; -- Probably the last section.
	    end;
	end if;

	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</P>");

	Ada.Text_IO.Put_Line (Output_Object.Output_File, "<HR>"); -- Horizontal line (rule).

    end Start_HTML_File;


    procedure End_HTML_File (Output_Object : in out HTML_Output_Type;
			     Clause : in String) is
	-- Internal routine.
	-- Generate the needed text to end an HTML file. Also closes the file.
	-- Clause is the properly formatted Clause number for the NEXT file,
	-- if known.
    begin
	Ada.Text_IO.New_Line (Output_Object.Output_File); -- Blank line to set off paragraphs.

	Ada.Text_IO.Put_Line (Output_Object.Output_File, "<HR>"); -- Horizontal line (rule).
	Ada.Text_IO.Put (Output_Object.Output_File, "<P><A HREF=""");
	case Output_Object.Document is
	    when ARM_Output.RM =>
		Ada.Text_IO.Put (Output_Object.Output_File, "RM-TOC.html");
	    when ARM_Output.RM_ISO =>
		Ada.Text_IO.Put (Output_Object.Output_File, "RMI-TOC.html");
	    when ARM_Output.AARM =>
		Ada.Text_IO.Put (Output_Object.Output_File, "AA-TOC.html");
        end case;
	Ada.Text_IO.Put (Output_Object.Output_File, """>Contents</A>");
	Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;&nbsp;&nbsp;");
	Ada.Text_IO.Put (Output_Object.Output_File, "<A HREF=""");
	case Output_Object.Document is
	    when ARM_Output.RM =>
		Ada.Text_IO.Put (Output_Object.Output_File, "RM-0-29.html");
	    when ARM_Output.RM_ISO =>
		Ada.Text_IO.Put (Output_Object.Output_File, "RMI-0-29.html");
	    when ARM_Output.AARM =>
		Ada.Text_IO.Put (Output_Object.Output_File, "AA-0-29.html");
        end case;
	Ada.Text_IO.Put (Output_Object.Output_File, """>Index</A>");
	if Clause /= "" then
	    begin
		-- Note: We do the following in one big glup so that if
		-- Not_Found_Error is raised, nothing is output.
		Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;&nbsp;&nbsp;<A HREF=""" &
		    Make_Clause_File_Name (Output_Object,
			ARM_Contents.Previous_Clause(
			    ARM_Contents.Previous_Clause(Clause))) & ".html");
		Ada.Text_IO.Put (Output_Object.Output_File, """>Previous</A>");
	    exception
	        when ARM_Contents.Not_Found_Error =>
		    null; -- Probably the first section.
	    end;
	    Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;&nbsp;&nbsp;<A HREF=""" &
	        Make_Clause_File_Name (Output_Object, Clause) & ".html");
	    Ada.Text_IO.Put (Output_Object.Output_File, """>Next</A>");
	end if;
        Ada.Text_IO.Put (Output_Object.Output_File, "&nbsp;&nbsp;&nbsp;<A HREF=""");
	case Output_Object.Document is
	    when ARM_Output.RM =>
		Ada.Text_IO.Put (Output_Object.Output_File, "RM-TTL.html");
	    when ARM_Output.RM_ISO =>
		Ada.Text_IO.Put (Output_Object.Output_File, "RMI-TTL.html");
	    when ARM_Output.AARM =>
		Ada.Text_IO.Put (Output_Object.Output_File, "AA-TTL.html");
        end case;
	Ada.Text_IO.Put (Output_Object.Output_File, """>Legal</A>");

	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</P>");

	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</BODY>");
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</HTML>");
	Ada.Text_IO.Close (Output_Object.Output_File);
    end End_HTML_File;


    procedure Create (Output_Object : in out HTML_Output_Type;
		      Document : in ARM_Output.Document_Type;
		      Page_Size : in ARM_Output.Page_Size;
		      Includes_Changes : in Boolean;
		      Big_Files : in Boolean) is
	-- Create an Output_Object for a document of Document type, with
	-- the specified page size. Changes from the base standard are included
	-- if Includes_Changes is True. Generate a few large output files if
	-- Big_Files is True; otherwise generate smaller output files.
    begin
	if Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Already valid object");
	end if;
	Output_Object.Is_Valid := True;
	Output_Object.Document := Document;
	-- We don't use the page size or changes flag. We could use the
	-- Big files flag, but for now we don't.
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
	    End_HTML_File (Output_Object, "");
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
	    -- assume such items are formated with explicit New_Column calls,
	    -- and do not contain any nested paragraph formats.
	    Output_Object.Current_Column := 1;
	    Output_Object.Current_Item := 1;
	elsif Output_Object.Column_Count >= 4 and then Number_of_Columns = 1 then
	    -- Finished processing columns, output the columns as a table.
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "<UL><UL><TABLE Width=""70%"">"); -- Table with no border or caption, takes up 70% of the screen.
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
	            Ada.Text_IO.Put_Line (Output_Object.Output_File, "</TABLE></UL></UL>");
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
	     case Output_Object.Document is
		when ARM_Output.RM =>
		    Start_HTML_File (Output_Object, "RM-" & Output_Object.Section_Name,
				     "", Clause => "");
		when ARM_Output.RM_ISO =>
		    Start_HTML_File (Output_Object, "RMI-" & Output_Object.Section_Name,
				     "", Clause => "");
		when ARM_Output.AARM =>
		    Start_HTML_File (Output_Object, "AA-" & Output_Object.Section_Name,
				     "", Clause => "");
	     end case;
	end if;
    end Check_Clause_File;


    procedure Start_Paragraph (Output_Object : in out HTML_Output_Type;
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
	Output_Object.Disp_Char_Count := 0;
	Output_Object.Saw_Hang_End := False;
	Check_Clause_File (Output_Object);
	-- Note: We only support Justification for the Normal and Wide styles.
	if Output_Object.Column_Count >= 4 then
	    -- Formatting is deferred; only a few formats are supported.
	    if Tab_Stops.Number /= 0 then
	        Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		    "Tabs in 4+ column text");
	    end if;
	    case Format is
	        when ARM_Output.Normal | ARM_Output.Syntax_Indented |
		     ARM_Output.Code_Indented =>
		    null;
		when others =>
	            Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		        "Unsupported format in 4+ column text - " & ARM_Output.Paragraph_Type'Image(Format));
	    end case;
	    if Number /= "" then
	        Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		    "No paragraph numbers in 4+ column text");
	    end if;
	    return; -- Nothing more to do here.
	end if;

	-- Note: In HTML 4, we might be able to control the space below for "Space_After".
	case Format is
	    when ARM_Output.Normal =>
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
	    when ARM_Output.Wide =>
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
		-- Note: In HTML 4, we might be able to control the space above.
	    when ARM_Output.Notes=>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "<UL><FONT SIZE=-1>");
		Output_Object.Char_Count := 18;
	    when ARM_Output.Notes_Header =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "<UL><FONT SIZE=-1>");
		Output_Object.Char_Count := 18;
		-- Note: In HTML 4, we might be able to control the space below.
	    when ARM_Output.Annotations =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><FONT SIZE=-1>");
		Output_Object.Char_Count := 22;
	    when ARM_Output.Wide_Annotations =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><FONT SIZE=-1>");
		Output_Object.Char_Count := 22;
		-- Note: In HTML 4, we might be able to control the space above.
	    when ARM_Output.Index =>
		-- Note: We don't put this in a smaller font.
		Ada.Text_IO.Put (Output_Object.Output_File, "<P>");
		Output_Object.Char_Count := 3;
	    when ARM_Output.Syntax_Summary =>
		-- Note: We don't put this in a smaller font.
		Ada.Text_IO.Put (Output_Object.Output_File, "<UL>");
		Output_Object.Char_Count := 4;
	    when ARM_Output.Examples =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "<UL><TT>");
		Output_Object.Char_Count := 8;
	    when ARM_Output.Small_Examples =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><TT><FONT SIZE=-1>");
		Output_Object.Char_Count := 30;
	    when ARM_Output.Indented_Examples =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><UL><TT>");
		Output_Object.Char_Count := 20;
	    when ARM_Output.Small_Indented_Examples =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><UL><UL><UL><TT><FONT SIZE=-1>");
		Output_Object.Char_Count := 42;
	    when ARM_Output.Syntax_Indented =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "<UL>");
		Output_Object.Char_Count := 4;
	    when ARM_Output.Code_Indented =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL>");
		Output_Object.Char_Count := 8;
	    when ARM_Output.Small_Code_Indented =>
	        Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><UL><FONT SIZE=-1>");
	        Output_Object.Char_Count := 30;
	    when ARM_Output.Indented =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL>");
		Output_Object.Char_Count := 12;
	    when ARM_Output.Small_Indented =>
	        Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><UL><UL><FONT SIZE=-1>");
	        Output_Object.Char_Count := 34;
	    when ARM_Output.Bulleted =>
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL>");
		    Output_Object.Char_Count := 4;
		else
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><LI TYPE=DISC>");
		    Output_Object.Char_Count := 18;
		end if;
	    when ARM_Output.Nested_Bulleted =>
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL>");
		    Output_Object.Char_Count := 8;
		else
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><LI TYPE=DISC>");
		    Output_Object.Char_Count := 22;
		end if;
	    when ARM_Output.Small_Bulleted =>
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><FONT SIZE=-1>");
		    Output_Object.Char_Count := 26;
		else
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><FONT SIZE=-1><LI TYPE=DISC>");
		    Output_Object.Char_Count := 40;
		end if;
	    when ARM_Output.Small_Nested_Bulleted =>
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><UL><FONT SIZE=-1>");
		    Output_Object.Char_Count := 30;
		else
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><UL><FONT SIZE=-1><LI TYPE=DISC>");
		    Output_Object.Char_Count := 44;
		end if;
	    when ARM_Output.Indented_Bulleted =>
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><UL>");
		    Output_Object.Char_Count := 16;
		else
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><UL><LI TYPE=DISC>");
		    Output_Object.Char_Count := 30;
		end if;
	    when ARM_Output.Code_Indented_Bulleted =>
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL>");
		    Output_Object.Char_Count := 12;
		else
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><LI TYPE=DISC>");
		    Output_Object.Char_Count := 26;
		end if;
	    when ARM_Output.Code_Indented_Nested_Bulleted =>
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><UL>");
		    Output_Object.Char_Count := 16;
		else
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><UL><LI TYPE=DISC>");
		    Output_Object.Char_Count := 30;
		end if;
	    when ARM_Output.Syntax_Indented_Bulleted =>
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL>");
		    Output_Object.Char_Count := 8;
		else
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><LI TYPE=DISC>");
		    Output_Object.Char_Count := 22;
		end if;
	    when ARM_Output.Notes_Bulleted =>
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><FONT SIZE=-1>");
		    Output_Object.Char_Count := 18;
		else
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><FONT SIZE=-1><LI TYPE=DISC>");
		    Output_Object.Char_Count := 32;
		end if;
	    when ARM_Output.Notes_Nested_Bulleted =>
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><FONT SIZE=-1>");
		    Output_Object.Char_Count := 22;
		else
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><FONT SIZE=-1><LI TYPE=DISC>");
		    Output_Object.Char_Count := 36;
		end if;
	    when ARM_Output.Hanging =>
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<DL><DD>");
		    Output_Object.Char_Count := 8;
		    Output_Object.Saw_Hang_End := True;
		else -- Has prefix.
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<DL><DT>");
		    Output_Object.Char_Count := 8;
		    Output_Object.Saw_Hang_End := False;
		end if;
	    when ARM_Output.Indented_Hanging =>
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><DL><DD>");
		    Output_Object.Char_Count := 16;
		    Output_Object.Saw_Hang_End := True;
		else -- Has prefix.
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><DL><DT>");
		    Output_Object.Char_Count := 16;
		    Output_Object.Saw_Hang_End := False;
		end if;
	    when ARM_Output.Small_Hanging =>
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><DL><FONT SIZE=-1><DD>");
		    Output_Object.Char_Count := 30;
		    Output_Object.Saw_Hang_End := True;
		else -- Has prefix.
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><DL><FONT SIZE=-1><DT>");
		    Output_Object.Char_Count := 30;
		    Output_Object.Saw_Hang_End := False;
		end if;
	    when ARM_Output.Small_Indented_Hanging =>
		if No_Prefix then
		    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><UL><DL><FONT SIZE=-1><DD>");
		    Output_Object.Char_Count := 38;
		    Output_Object.Saw_Hang_End := True;
		else -- Has prefix.
		    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><UL><DL><FONT SIZE=-1><DT>");
		    Output_Object.Char_Count := 38;
		    Output_Object.Saw_Hang_End := False;
		end if;
	    when ARM_Output.Hanging_in_Bulleted =>
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><DL><DD>");
		    Output_Object.Char_Count := 12;
		    Output_Object.Saw_Hang_End := True;
		else -- Has prefix.
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><DL><DT>");
		    Output_Object.Char_Count := 12;
		    Output_Object.Saw_Hang_End := False;
		end if;
	    when ARM_Output.Small_Hanging_in_Bulleted =>
		if No_Prefix then
		    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><DL><FONT SIZE=-1><DD>");
		    Output_Object.Char_Count := 34;
		    Output_Object.Saw_Hang_End := True;
		else -- Has prefix.
		    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><DL><FONT SIZE=-1><DT>");
		    Output_Object.Char_Count := 34;
		    Output_Object.Saw_Hang_End := False;
		end if;
	    when ARM_Output.Enumerated =>
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<DL><DD>");
		    Output_Object.Char_Count := 8;
		    Output_Object.Saw_Hang_End := True;
		else -- Has prefix.
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<DL><DT>");
		    Output_Object.Char_Count := 8;
		    Output_Object.Saw_Hang_End := False;
		end if;
	    when ARM_Output.Small_Enumerated =>
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><DL><FONT SIZE=-1><DD>");
		    Output_Object.Char_Count := 30;
		    Output_Object.Saw_Hang_End := True;
		else -- Has prefix.
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><DL><FONT SIZE=-1><DT>");
		    Output_Object.Char_Count := 30;
		    Output_Object.Saw_Hang_End := False;
		end if;
	    when ARM_Output.Nested_Enumerated =>
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><DL><DD>");
		    Output_Object.Char_Count := 12;
		    Output_Object.Saw_Hang_End := True;
		else -- Has prefix.
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><DL><DT>");
		    Output_Object.Char_Count := 12;
		    Output_Object.Saw_Hang_End := False;
		end if;
	    when ARM_Output.Small_Nested_Enumerated =>
		if No_Prefix then
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><DL><FONT SIZE=-1><DD>");
		    Output_Object.Char_Count := 34;
		    Output_Object.Saw_Hang_End := True;
		else -- Has prefix.
	    	    Ada.Text_IO.Put (Output_Object.Output_File, "<UL><UL><UL><DL><FONT SIZE=-1><DT>");
		    Output_Object.Char_Count := 34;
		    Output_Object.Saw_Hang_End := False;
		end if;
	end case;
	Output_Object.Paragraph_Format := Format;
	Output_Object.Font := ARM_Output.Default;
	Output_Object.Is_Bold := False;
	Output_Object.Is_Italic := False;
	Output_Object.Size := 0;
	case Format is
	    when ARM_Output.Normal | ARM_Output.Wide |
		 ARM_Output.Notes | ARM_Output.Notes_Header |
		 ARM_Output.Annotations | ARM_Output.Wide_Annotations |
		 ARM_Output.Index | ARM_Output.Syntax_Summary |
		 ARM_Output.Examples | ARM_Output.Small_Examples |
		 ARM_Output.Indented_Examples | ARM_Output.Small_Indented_Examples |
		 ARM_Output.Syntax_Indented |
		 ARM_Output.Indented |
		 ARM_Output.Small_Indented | ARM_Output.Code_Indented |
		 ARM_Output.Small_Code_Indented =>
		Output_Object.Tab_Stops := Tab_Stops;
		-- No tabs in HTML; we'll emulate them for fixed fonts.
		-- We'll expand proportional stops here (text characters
		-- are larger than the variable ones these are set up for).
		for I in 1 .. Tab_Stops.Number loop
		    if ARM_Output."=" (Tab_Stops.Stops(I).Kind,
				       ARM_Output.Left_Proportional) then
		        Output_Object.Tab_Stops.Stops(I).Stop :=
				(Tab_Stops.Stops(I).Stop * 13 / 12);
		    else
		        Output_Object.Tab_Stops.Stops(I).Stop :=
				Tab_Stops.Stops(I).Stop;
		    end if;
		end loop;
		case Format is
		    when ARM_Output.Examples | ARM_Output.Small_Examples |
			 ARM_Output.Indented_Examples | ARM_Output.Small_Indented_Examples =>
			Output_Object.Emulate_Tabs := True;
		    when others =>
			Output_Object.Emulate_Tabs := False;
		end case;

	    when ARM_Output.Bulleted | ARM_Output.Nested_Bulleted |
		 ARM_Output.Small_Bulleted | ARM_Output.Small_Nested_Bulleted |
		 ARM_Output.Indented_Bulleted | ARM_Output.Code_Indented_Bulleted |
		 ARM_Output.Code_Indented_Nested_Bulleted |
		 ARM_Output.Syntax_Indented_Bulleted |
		 ARM_Output.Notes_Bulleted | ARM_Output.Notes_Nested_Bulleted |
		 ARM_Output.Hanging | ARM_Output.Indented_Hanging |
		 ARM_Output.Small_Hanging | ARM_Output.Small_Indented_Hanging |
		 ARM_Output.Hanging_in_Bulleted | ARM_Output.Small_Hanging_in_Bulleted |
		 ARM_Output.Enumerated | ARM_Output.Small_Enumerated |
		 ARM_Output.Nested_Enumerated | ARM_Output.Small_Nested_Enumerated =>
		if Tab_Stops.Number /= 0 then
	            Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		        "Tabs in hanging/bulleted paragraph");
		end if;
		Output_Object.Emulate_Tabs := False;
	end case;

	if Number /= "" then -- Has paragraph number.
	    Ada.Text_IO.Put (Output_Object.Output_File, TINY_SWISS_FONT_CODE);
	    Ada.Text_IO.Put (Output_Object.Output_File, Number);
	    Ada.Text_IO.Put (Output_Object.Output_File, "</FONT> ");
	    Output_Object.Char_Count := Output_Object.Char_Count + TINY_SWISS_FONT_CODE'Length + Number'Length + 8;
	    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + ((Number'Length+1)/2) + 1;
		-- Note: Count these as half characters, as the font is so small.
	end if;
	-- Note: No_Breaks and Keep_with_Next have no effect here, because
	-- HTML doesn't have page breaks.
    end Start_Paragraph;


    procedure End_Paragraph (Output_Object : in out HTML_Output_Type) is
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
	if Output_Object.Column_Count >= 4 then
	    -- Formatting is deferred; only a few formats are supported.
	    if Output_Object.Column_Text (Output_Object.Current_Column) /= null and then
	       Output_Object.Column_Text (Output_Object.Current_Column).Item = Output_Object.Current_Item then
		Output_Object.Column_Text (Output_Object.Current_Column).End_Para := True;
	    end if;
	    Output_Object.Current_Item := Output_Object.Current_Item + 2; -- Skip an item.
            Output_Object.Char_Count := 0;
            Output_Object.Disp_Char_Count := 0;
	    return; -- Nothing else to do here.
	end if;

	case Output_Object.Paragraph_Format is
	    when ARM_Output.Normal | ARM_Output.Wide |
	         ARM_Output.Index =>
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "</P>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Syntax_Summary =>
	    	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Notes | ARM_Output.Notes_Header =>
	    	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</FONT></UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Annotations | ARM_Output.Wide_Annotations =>
	    	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</FONT></UL></UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Examples =>
	    	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</TT></UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Small_Examples =>
	    	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</FONT></TT></UL></UL></UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Indented_Examples =>
	    	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</TT></UL></UL></UL></UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Small_Indented_Examples =>
	    	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</FONT></TT></UL></UL></UL></UL></UL></UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Syntax_Indented =>
	    	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Code_Indented =>
	    	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</UL></UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Small_Code_Indented =>
	    	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</FONT></UL></UL></UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Indented =>
	    	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</UL></UL></UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Small_Indented =>
	    	Ada.Text_IO.Put_Line (Output_Object.Output_File, "</FONT></UL></UL></UL></UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Bulleted =>
		if Output_Object.Had_Prefix then
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</LI></UL>");
		else
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</UL>");
		end if;
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Nested_Bulleted =>
		if Output_Object.Had_Prefix then
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</LI></UL></UL>");
		else
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</UL></UL>");
		end if;
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Small_Bulleted =>
		if Output_Object.Had_Prefix then
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</LI></FONT></UL></UL></UL>");
		else
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</FONT></UL></UL></UL>");
		end if;
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Small_Nested_Bulleted =>
		if Output_Object.Had_Prefix then
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</LI></FONT></UL></UL></UL></UL>");
		else
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</FONT></UL></UL></UL></UL>");
		end if;
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Indented_Bulleted =>
		if Output_Object.Had_Prefix then
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</LI></UL></UL></UL></UL>");
		else
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</UL></UL></UL></UL>");
		end if;
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Code_Indented_Bulleted =>
		if Output_Object.Had_Prefix then
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</LI></UL></UL></UL>");
		else
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</UL></UL></UL>");
		end if;
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Code_Indented_Nested_Bulleted =>
		if Output_Object.Had_Prefix then
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</LI></UL></UL></UL></UL>");
		else
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</UL></UL></UL></UL>");
		end if;
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Syntax_Indented_Bulleted =>
		if Output_Object.Had_Prefix then
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</LI></UL></UL>");
		else
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</UL></UL>");
		end if;
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Notes_Bulleted =>
		if Output_Object.Had_Prefix then
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</LI></FONT></UL></UL>");
		else
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</FONT></UL></UL>");
		end if;
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Notes_Nested_Bulleted =>
		if Output_Object.Had_Prefix then
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</LI></FONT></UL></UL></UL>");
		else
	    	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "</FONT></UL></UL></UL>");
		end if;
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Hanging =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "</DL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Indented_Hanging =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "</DL></UL></UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Small_Hanging =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "</DL></FONT></UL></UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Small_Indented_Hanging =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "</DL></FONT></UL></UL></UL></UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Hanging_in_Bulleted =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "</DL></UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Small_Hanging_in_Bulleted =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "</DL></FONT></UL></UL></UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Enumerated =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "</DL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Small_Enumerated =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "</DL></FONT></UL></UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Nested_Enumerated =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "</DL></UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	    when ARM_Output.Small_Nested_Enumerated =>
	    	Ada.Text_IO.Put (Output_Object.Output_File, "</DL></FONT></UL></UL></UL>");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	end case;
	Output_Object.Char_Count := 0;
	Output_Object.Disp_Char_Count := 0;
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
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H4 ALIGN=CENTER>" & Header_Text & "</H4>");
	Output_Object.Char_Count := 0;
	Output_Object.Disp_Char_Count := 0;
    end Category_Header;


    procedure Clause_Header (Output_Object : in out HTML_Output_Type;
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
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Header in paragraph");
	end if;

	if Ada.Text_IO.Is_Open (Output_Object.Output_File) then
	    End_HTML_File (Output_Object, Clause_Number);
	end if;

	-- Special for table of contents:
	if Clause_Number = "" and then Header_Text = "Table of Contents" then
	    case Output_Object.Document is
	        when ARM_Output.RM =>
	            Start_HTML_File (Output_Object, "RM-TOC", Header_Text, "");
	        when ARM_Output.RM_ISO =>
	            Start_HTML_File (Output_Object, "RMI-TOC", Header_Text, "");
	        when ARM_Output.AARM =>
	            Start_HTML_File (Output_Object, "AA-TOC", Header_Text, "");
	    end case;
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>Table of Contents</H1>");
	    Output_Object.Char_Count := 0;
	    Output_Object.Disp_Char_Count := 0;
	    return;
	end if;

	Start_HTML_File (Output_Object,
		Make_Clause_File_Name (Output_Object, Clause_Number),
		Header_Text, Clause_Number);

	case Level is
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
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>Section " &
				      Clause_Number & ": " & Header_Text & "</H1>");
	    when ARM_Contents.Clause | ARM_Contents.Subclause =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1> " &
				      Clause_Number & ' ' & Header_Text & "</H1>");
	end case;
	Output_Object.Char_Count := 0;
	Output_Object.Disp_Char_Count := 0;
	-- No page breaks in HTML, so we don't need to look at No_Page_Break.
    end Clause_Header;


    procedure Revised_Clause_Header (Output_Object : in out HTML_Output_Type;
			     New_Header_Text : in String;
			     Old_Header_Text : in String;
			     Level : in ARM_Contents.Level_Type;
			     Clause_Number : in String;
			     No_Page_Break : in Boolean := False) is
	-- Output a revised clause header. Both the original and new text will
	-- be output. The level of the header is specified in Level. The Clause
	-- Number is as specified.
	-- These should appear in the table of contents.
	-- For hyperlinked formats, this should generate a link target.
	-- If No_Page_Break is True, suppress any page breaks.
	-- Raises Not_Valid_Error if in a paragraph.
	function Header_Text return String is
	begin
	    if HTML_4 then
		return "<INS>" & New_Header_Text & "</INS><DEL>" & Old_Header_Text & "</DEL>";
	    else
		return "<U>" & New_Header_Text & "</U><S>" & Old_Header_Text & "</S>";
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

	if Ada.Text_IO.Is_Open (Output_Object.Output_File) then
	    End_HTML_File (Output_Object, Clause_Number);
	end if;

	Start_HTML_File (Output_Object,
		Make_Clause_File_Name (Output_Object, Clause_Number),
		New_Header_Text, Clause_Number);

	case Level is
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
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1>Section " &
				      Clause_Number & ": " & Header_Text & "</H1>");
	    when ARM_Contents.Clause | ARM_Contents.Subclause =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "<H1> " &
				      Clause_Number & ' ' & Header_Text & "</H1>");
	end case;
	Output_Object.Char_Count := 0;
	Output_Object.Disp_Char_Count := 0;
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
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "<P><BR><BR></P>");
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "<HR>"); -- Horizontal line.
		Ada.Text_IO.Put_Line (Output_Object.Output_File, "<P><BR></P>");
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
			   Columns : in ARM_Output.Column_Count) is
	-- Starts a table. The number of columns is Columns.
	-- This command starts a paragraph; the entire table is a single
	-- paragraph. Text will be considered part of the caption until the
	-- next table marker call.
	-- Raises Not_Valid_Error if in a paragraph.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Table in paragraph");
	end if;

        Ada.Text_IO.Put (Output_Object.Output_File, "<TABLE frame=""border"" rules=""all"" border=""2"">");
        Ada.Text_IO.Put (Output_Object.Output_File, "<CAPTION>");
	Output_Object.Char_Count := 9;
	Output_Object.Disp_Char_Count := 0;

	Output_Object.Is_In_Paragraph := True;
	Output_Object.Is_In_Table := True;
	Output_Object.In_Header := True;
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
		if Output_Object.In_Header then
	            Ada.Text_IO.Put (Output_Object.Output_File, "<TH align=""center"">");
		    Output_Object.Char_Count := Output_Object.Char_Count + 20;
		else
	            Ada.Text_IO.Put (Output_Object.Output_File, "<TD align=""center"">");
		    Output_Object.Char_Count := Output_Object.Char_Count + 20;
		end if;
	    when ARM_Output.End_Caption =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "</CAPTION>");
	        Ada.Text_IO.Put (Output_Object.Output_File, "<TR><TH align=""center"">");
		Output_Object.Char_Count := 24;
		Output_Object.Disp_Char_Count := 0;
	    when ARM_Output.End_Header =>
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	        Ada.Text_IO.Put (Output_Object.Output_File, "<TR><TD align=""center"">");
		Output_Object.Char_Count := 24;
		Output_Object.Disp_Char_Count := 0;
		Output_Object.In_Header := False;
	    when ARM_Output.End_Row | ARM_Output.End_Row_Next_Is_Last =>
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	        Ada.Text_IO.Put (Output_Object.Output_File, "<TR><TD align=""center"">");
		Output_Object.Char_Count := 24;
		Output_Object.Disp_Char_Count := 0;
	    when ARM_Output.End_Table =>
		Ada.Text_IO.New_Line (Output_Object.Output_File);
	        Ada.Text_IO.Put_Line (Output_Object.Output_File, "</TABLE>");
		Output_Object.Is_In_Paragraph := False;
		Output_Object.Is_In_Table := False;
	end case;
    end Table_Marker;


    -- Text output: These are only allowed after a Start_Paragraph and
    -- before any End_Paragraph. Raises Not_Valid_Error if not allowed.

    Special_Set : constant Ada.Strings.Maps.Character_Set :=
         Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('<'),
           Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('>'),
             Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('"'),
			              Ada.Strings.Maps.To_Set ('&'))));

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
			"Column item full, but more text!");
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
	if Ada.Strings.Fixed.Count (Text, Special_Set) = 0 then
	    if Output_Object.Char_Count + Text'Length >= LINE_LENGTH - 10 then
		-- We can only break on a space.
	        for I in Text'range loop
		    Ordinary_Character (Output_Object, Text(I));
	        end loop;
	    else
	        Output_Text (Output_Object, Text);
	        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + Text'Length;
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
	if Char = ' ' and then Output_Object.Char_Count >= LINE_LENGTH - 10 and then
	    Output_Object.Column_Count < 4 then
	    Ada.Text_IO.New_Line (Output_Object.Output_File);
	    Output_Object.Char_Count := 0;
	else
	    if Char = '<' then
	        Output_Text (Output_Object, "&lt;");
	        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	    elsif Char = '>' then
	        Output_Text (Output_Object, "&gt;");
	        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	    elsif Char = '"' then
	        Output_Text (Output_Object, "&quot;");
	        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	    elsif Char = '&' then
	        Output_Text (Output_Object, "&amp;");
	        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
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
	    else
	        Output_Text (Output_Object, Char & "");
	        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	    end if;
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
	else -- Normal.
            Ada.Text_IO.Put_Line (Output_Object.Output_File, "<BR>");
            Output_Object.Char_Count := 0;
            Output_Object.Disp_Char_Count := 0;
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
	if ARM_Output."/=" (Output_Object.Paragraph_Format, ARM_Output.Index) then
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
	if HTML_4 and then Use_Unicode then
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

	if Output_Object.Emulate_Tabs then
	    Output_Text (Output_Object, "&nbsp;");
	    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
	    for I in 1 .. Output_Object.Tab_Stops.Number loop
	        if Output_Object.Tab_Stops.Stops(I).Stop > Output_Object.Disp_Char_Count then
		    for J in Output_Object.Disp_Char_Count+1 .. Output_Object.Tab_Stops.Stops(I).Stop-1 loop
		        Output_Text (Output_Object, "&nbsp;");
		        Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		    end loop;
		    exit;
	        end if;
	    end loop; -- If we drop out without finding a tab, we just use the single
		      -- space already written.
	else
	    -- HTML does not have tabs. We can't even emulate them for a
	    -- proportional font. Put in a space.
	    Output_Text (Output_Object, "&nbsp;");
	    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
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
	case Char is
	    when ARM_Output.EM_Dash =>
		if HTML_4 and Use_Unicode then
	            Output_Text (Output_Object, "&mdash;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, "--");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 2;
		end if;
	    when ARM_Output.EN_Dash =>
		if HTML_4 and Use_Unicode then
		    Output_Text (Output_Object, "&ndash;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
		    Output_Text (Output_Object, "-");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		end if;
	    when ARM_Output.GEQ =>
		if HTML_4 and Use_Unicode then
	            Output_Text (Output_Object, "&ge;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, ">=");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 2;
		end if;
	    when ARM_Output.LEQ =>
		if HTML_4 and Use_Unicode then
	            Output_Text (Output_Object, "&le;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, "<=");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 2;
		end if;
	    when ARM_Output.NEQ =>
		if HTML_4 and Use_Unicode then
	            Output_Text (Output_Object, "&ne;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, "/=");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 2;
		end if;
	    when ARM_Output.PI =>
		if HTML_4 and Use_Unicode then
	            Output_Text (Output_Object, "&pi;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, "PI");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 2;
		end if;
	    when ARM_Output.Left_Ceiling =>
		if HTML_4 and Use_Unicode then
	            Output_Text (Output_Object, "&lceil;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, "<I>Ceiling</I>(");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 8;
		end if;
	    when ARM_Output.Right_Ceiling =>
		if HTML_4 and Use_Unicode then
	            Output_Text (Output_Object, "&rceil;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, ")");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		end if;
	    when ARM_Output.Left_Floor =>
		if HTML_4 and Use_Unicode then
	            Output_Text (Output_Object, "&lfloor;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, "<I>Floor</I>(");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 6;
		end if;
	    when ARM_Output.Right_Floor =>
		if HTML_4 and Use_Unicode then
	            Output_Text (Output_Object, "&rfloor;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, ")");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		end if;
	    when ARM_Output.Thin_Space =>
		if HTML_4 and Use_Unicode then
	            Output_Text (Output_Object, "&thinsp;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, " ");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		end if;
	    when ARM_Output.Left_Quote =>
		if HTML_4 and Use_Unicode then
	            Output_Text (Output_Object, "&lsquo;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, "`");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		end if;
	    when ARM_Output.Right_Quote =>
		if HTML_4 and Use_Unicode then
	            Output_Text (Output_Object, "&rsquo;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, "'");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		end if;
	    when ARM_Output.Left_Double_Quote =>
		if HTML_4 and Use_Unicode then
	            Output_Text (Output_Object, "&ldquo;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, """");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		end if;
	    when ARM_Output.Right_Double_Quote =>
		if HTML_4 and Use_Unicode then
	            Output_Text (Output_Object, "&rdquo;");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		else
	            Output_Text (Output_Object, """");
		    Output_Object.Disp_Char_Count := Output_Object.Disp_Char_Count + 1;
		end if;
	end case;
    end Special_Character;


    procedure End_Hang_Item (Output_Object : in out HTML_Output_Type) is
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
	Ada.Text_IO.Put_Line (Output_Object.Output_File, "<DD>");
	    -- Part of a definition list.
        Output_Object.Char_Count := 0;
	Output_Object.Disp_Char_Count := 0;
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
			   Bold : in Boolean;
			   Italic : in Boolean;
			   Font : in ARM_Output.Font_Family_Type;
			   Size : in ARM_Output.Size_Type;
			   Change : in ARM_Output.Change_Type;
			   Location : in ARM_Output.Location_Type) is
	-- Change the text format so that Bold, Italics, the font family,
	-- the text size, and the change state are as specified.
	-- Note: Changes to these properties must be stack-like; that is,
	-- Bold on, Italic on, Italic off, Bold off is OK; Bold on, Italic on,
	-- Bold off, Italic off is not allowed (as separate commands).
	use type ARM_Output.Change_Type;
	use type ARM_Output.Location_Type;
	use type ARM_Output.Size_Type;
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
	if Change /= Output_Object.Change then
	    case Output_Object.Change is
		when ARM_Output.Insertion =>
		    if HTML_4 then
		        Output_Text (Output_Object, "</INS>");
		    else
		        Output_Text (Output_Object, "</U>");
		    end if;
		when ARM_Output.Deletion =>
		    if HTML_4 then
			Output_Text (Output_Object, "</DEL>");
		    else -- HTML 3.2
			Output_Text (Output_Object, "</S>");
		    end if;
		when ARM_Output.None =>
		    null;
	    end case;
	end if;

	if not Bold and Output_Object.Is_Bold then
	    Output_Text (Output_Object, "</B>");
	    Output_Object.Is_Bold := False;
	end if;

	if not Italic and Output_Object.Is_Italic then
	    Output_Text (Output_Object, "</I>");
	    Output_Object.Is_Italic := False;
	end if;

	if Size /= Output_Object.Size then
	    if Output_Object.Size /= 0 then
	        Output_Text (Output_Object, "</FONT>");
	    end if;
	end if;

	if Location /= Output_Object.Location then
	    case Output_Object.Location is
		when ARM_Output.Superscript =>
		    Output_Text (Output_Object, "</FONT></SUP>");
		when ARM_Output.Subscript =>
		    Output_Text (Output_Object, "</FONT></SUB>");
		when ARM_Output.Normal =>
		    null;
	    end case;
	end if;

	if ARM_Output."/=" (Font, Output_Object.Font) then
	    case Output_Object.Font is
		when ARM_Output.Default => null;
		when ARM_Output.Fixed =>
		    Output_Text (Output_Object, "</TT>");
		when ARM_Output.Roman => null; -- Default, currently.
		    --Output_Text (Output_Object, "</FONT>");
		when ARM_Output.Swiss =>
		    Output_Text (Output_Object, "</FONT>");
	    end case;
	    case Font is
		when ARM_Output.Default => null;
		when ARM_Output.Fixed =>
		    Output_Text (Output_Object, "<TT>");
		when ARM_Output.Roman => null; -- Default, currently.
		    --Output_Text (Output_Object, "<FONT xxx>");
		when ARM_Output.Swiss =>
		    Output_Text (Output_Object, SWISS_FONT_CODE);
	    end case;
	    Output_Object.Font := Font;
	end if;

	if Location /= Output_Object.Location then
	    -- Note: Location needs to be changed before size, as they
	    -- typically are changed together, and <SUP> and <SUB> reset the
	    -- size.
	    case Location is
		when ARM_Output.Superscript =>
		    Output_Text (Output_Object, "<SUP><FONT SIZE=+1>");
		when ARM_Output.Subscript =>
		    Output_Text (Output_Object, "<SUB><FONT SIZE=+1>");
		when ARM_Output.Normal =>
		    null;
	    end case;
	    Output_Object.Location := Location;
	end if;

	if Size /= Output_Object.Size then
	    -- HTML sizes are 1..7, with a default of 3. So we limit the changes.
	    if Size > 0 then
		if Size > 5 then
	            Output_Text (Output_Object, "<FONT SIZE=+5>");
		else
	            Output_Text (Output_Object, "<FONT SIZE=+" &
		        Character'Val(Size + Character'Pos('0')) & ">");
		end if;
	    elsif Size < 0 then
		if Size < -4 then
	            Output_Text (Output_Object, "<FONT SIZE=-4>");
		else
	            Output_Text (Output_Object, "<FONT SIZE=-" &
		        Character'Val(abs Size + Character'Pos('0')) & ">");
		end if;
	    -- else Size=0, nothing to do.
	    end if;
	    Output_Object.Size := Size;
	end if;

	if Italic and (not Output_Object.Is_Italic) then
	    Output_Text (Output_Object, "<I>");
	    Output_Object.Is_Italic := True;
	end if;
	if Bold and (not Output_Object.Is_Bold) then
	    Output_Text (Output_Object, "<B>");
	    Output_Object.Is_Bold := True;
	end if;

	if Change /= Output_Object.Change then
	    case Change is
		when ARM_Output.Insertion =>
		    if HTML_4 then
		        Output_Text (Output_Object, "<INS>");
		    else -- HTML 3.2
		        Output_Text (Output_Object, "<U>");
		    end if;
		when ARM_Output.Deletion =>
		    if HTML_4 then
		        Output_Text (Output_Object, "<DEL>");
		    else -- HTML 3.2
		        Output_Text (Output_Object, "<S>");
		    end if;
		when ARM_Output.None =>
		    null;
	    end case;
	    Output_Object.Change := Change;
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
	Output_Text (Output_Object, "<A HREF=""");
	declare
	    Name : constant String :=
		Make_Clause_File_Name (Output_Object, Clause_Number) & ".html";
	begin
	    Output_Text (Output_Object, Name);
	end;
	Output_Text (Output_Object, """>");
        Ordinary_Text (Output_Object, Text);
	Output_Text (Output_Object, "</A>");
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
	Output_Text (Output_Object, "<A NAME=""");
	Output_Text (Output_Object, "I");
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
	declare
	    Name : constant String :=
		Make_Clause_File_Name (Output_Object, Clause_Number) & ".html";
	begin
	    Output_Text (Output_Object, Name);
	end;
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


end ARM_HTML;
