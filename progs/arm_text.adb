with ARM_Output,
     ARM_Contents,
     Ada.Text_IO,
     Ada.Exceptions,
     Ada.Strings.Fixed;
package body ARM_Text is

    --
    -- Ada reference manual formatter.
    --
    -- This package defines the text output object.
    -- Output objects are responsible for implementing the details of
    -- a particular format.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2002, 2004, 2005, 2006  AXE Consultants.
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
    --  4/14/00 - RLB - Created base package.
    --  4/18/00 - RLB - Added index and contents marker routines.
    --		      - Improved formatting.
    --  4/21/00 - RLB - Added line break and hard space routines.
    --  4/24/00 - RLB - Added DR references and Insert/Delete text formats.
    --  4/25/00 - RLB - Added size to format.
    --  4/29/00 - RLB - Added more formats.
    --  5/10/00 - RLB - Added even more formats.
    --          - RLB - Added End_Hang_Item.
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
    --  8/ 2/00 - RLB - Added Soft_Hyphen_Break and left and right quote
    --			characters.
    --		- RLB - Added additional styles.
    --	8/ 4/00 - RLB - Added additional styles.
    --  8/ 7/00 - RLB - Added Leading flag to Start_Paragraph, removed "Leading"
    --			styles.
    --  8/11/00 - RLB - Added Hanging_in_Bulleted styles.
    --  8/16/00 - RLB - Added Code_Indented_Nested_Bulleted.
    --  8/17/00 - RLB - Replaced "Leading" by "Space_After".
    -- 		- RLB - Added Nested_Enumerated.
    --  8/22/00 - RLB - Added Revised_Clause_Header.
    --  8/23/00 - RLB - Fixed a problem with long lines in examples.
    --  9/26/00 - RLB - Added Syntax_Summary style.
    --  7/18/02 - RLB - Removed Document parameter from Create, replaced by
    --			three strings and For_ISO boolean.
    --		- RLB - Added AI_Reference.
    --		- RLB - Added Change_Version_Type and uses.
    --  9/10/04 - RLB - Added "Both" to possible changes to handle
    --			replacement of changed text.
    --  9/14/04 - RLB - Moved Change_Version_Type to ARM_Contents.
    -- 11/03/04 - RLB - Added Nested_X2_Bulleted.
    -- 11/15/04 - RLB - Added Indented_Nested_Bulleted.
    --  1/24/05 - RLB - Added Inner_Indented.
    --  2/ 1/05 - RLB - Added Turkish chars to allow an AARM note.
    --  5/27/05 - RLB - Added arbitrary Unicode characters.
    --  1/11/06 - RLB - Eliminated dispatching Create in favor of tailored
    --			versions.
    --  1/13/06 - RLB - Added new Link operations.
    --  1/18/06 - RLB - Added additional styles.
    --  2/ 8/06 - RLB - Added additional parameters to the table command.
    --  2/10/06 - RLB - Added even more additional parameters to the
    --			table command.
    --		- RLB - Added picture command.
    --  9/22/06 - RLB - Added Subsubclause.

    LINE_LENGTH : constant := 78;
	-- Maximum intended line length.

    procedure Put_Line_Centered (File : in out Ada.Text_IO.File_Type;
				 Text : in String) is
	-- Put a line of text centered.
    begin
	for I in 1 .. (LINE_LENGTH - Text'Length - 1) / 2 loop
	    -- Center the heading.
	    Ada.Text_IO.Put (File, ' ');
	end loop;
	Ada.Text_IO.Put_Line (File, Text);
    end Put_Line_Centered;


    procedure Create (Output_Object : in out Text_Output_Type;
		      File_Prefix : in String;
		      Title : in String := "") is
	-- Create an Output_Object for a document.
	-- The prefix of the output file names is File_Prefix - this
	-- should be no more then 4 characters allowed in file names.
	-- The title of the document is Title.
    begin
	if Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Already valid object");
	end if;
	Output_Object.Is_Valid := True;
	Ada.Strings.Fixed.Move (Target => Output_Object.File_Prefix,
				Source => File_Prefix);
	-- We don't use the title.
    end Create;


    procedure Close (Output_Object : in out Text_Output_Type) is
	-- Close an Output_Object. No further output to the object is
	-- allowed after this call.
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if Ada.Text_IO.Is_Open (Output_Object.Output_File) then
	    Ada.Text_IO.Close (Output_Object.Output_File);
	end if;
	Output_Object.Is_Valid := False;
    end Close;


    procedure Section (Output_Object : in out Text_Output_Type;
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
	if Ada.Text_IO.Is_Open (Output_Object.Output_File) then
	    Ada.Text_IO.Close (Output_Object.Output_File);
	end if;
	-- Create a new file for this section:
	Ada.Text_IO.Create (Output_Object.Output_File, Ada.Text_IO.Out_File,
	    ".\Output\" & Ada.Strings.Fixed.Trim (Output_Object.File_Prefix, Ada.Strings.Right) &
		"-" & Section_Name & ".TXT");
	Ada.Text_IO.New_Line (Output_Object.Output_File);
    end Section;


    procedure Set_Columns (Output_Object : in out Text_Output_Type;
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


    procedure Make_Indent (Output_Object : in out Text_Output_Type) is
	-- Internal:
	-- Output the appropriate indent after a New_Line or Put_Line.
    begin
--Ada.Text_IO.Put_Line("Make_Indent: Amount=" & Natural'Image(Output_Object.Indent_Amount));
        for I in 1 .. Output_Object.Indent_Amount loop
	    Ada.Text_IO.Put (Output_Object.Output_File, ' ');
        end loop;
        Output_Object.Char_Count := Output_Object.Indent_Amount;
        Output_Object.Out_Char_Count := Output_Object.Indent_Amount;
	Output_Object.Output_Buffer_Space_Before := False;
    end Make_Indent;


    procedure Spill (Output_Object : in out Text_Output_Type) is
	-- Internal:
	-- Empty the output buffer in preperation for a New_Line or Put_Line.
    begin
        if Output_Object.Output_Buffer_Space_Before then
	    Ada.Text_IO.Put (Output_Object.Output_File, ' ');
	    Output_Object.Char_Count := Output_Object.Char_Count + 1; -- Count the space.
	    Output_Object.Out_Char_Count := Output_Object.Out_Char_Count + 1; -- Count the space.
        end if;
	if Output_Object.Output_Buffer_Len /= 0 then
	    Ada.Text_IO.Put (Output_Object.Output_File,
		Output_Object.Output_Buffer (1 .. Output_Object.Output_Buffer_Len));
--Ada.Text_IO.Put_Line("Spill: Len=" & Natural'Image(Output_Object.Output_Buffer_Len) &
--     " Space added=" &  Boolean'Image(Output_Object.Output_Buffer_Space_Before) & " Text=" &
--     Output_Object.Output_Buffer (1 .. Output_Object.Output_Buffer_Len));
	    Output_Object.Output_Buffer_Len := 0;
	    Output_Object.Out_Char_Count := Output_Object.Out_Char_Count +
		Output_Object.Output_Buffer_Len;
	end if;
        Output_Object.Output_Buffer_Space_Before := False;
    end Spill;


    procedure Buffer (Output_Object : in out Text_Output_Type;
		      Char : in Character) is
	-- Internal:
	-- Add Char to the output buffer. Char will *not* be a word break
	-- character.
    begin
	if Output_Object.Output_Buffer_Len = Output_Object.Output_Buffer'Last then
	    -- Oops, buffer is full. Spill it, and this character.
--Ada.Text_IO.Put_Line("** Buffer overflow!!");
	    Spill (Output_Object);
	    Ada.Text_IO.Put (Output_Object.Output_File, Char);
	    Output_Object.Char_Count := Output_Object.Char_Count + 1;
	    return;
	end if;
	Output_Object.Output_Buffer_Len := Output_Object.Output_Buffer_Len + 1;
	Output_Object.Output_Buffer(Output_Object.Output_Buffer_Len) := Char;
        Output_Object.Char_Count := Output_Object.Char_Count + 1;
    end Buffer;


    procedure Start_Paragraph (Output_Object : in out Text_Output_Type;
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
	Output_Object.Is_Hanging := False;
	Output_Object.Saw_Hang_End := False;
	Output_Object.Char_Count := 0;
	Output_Object.Out_Char_Count := 0;
	Output_Object.Output_Buffer_Space_Before := False; -- Nothing in it or on the line.
	Output_Object.Output_Buffer_Len := 0;
	case Format is
	    when ARM_Output.Normal => Output_Object.Indent_Amount := 0;
	    when ARM_Output.Wide => Output_Object.Indent_Amount := 0;
	    when ARM_Output.Notes => Output_Object.Indent_Amount := 6;
	    when ARM_Output.Notes_Header => Output_Object.Indent_Amount := 6;
	    when ARM_Output.Annotations => Output_Object.Indent_Amount := 10;
		Ada.Text_IO.Put (Output_Object.Output_File, "    ");
		Output_Object.Char_Count := 4;
	    when ARM_Output.Wide_Annotations => Output_Object.Indent_Amount := 10;
		Ada.Text_IO.Put (Output_Object.Output_File, "    ");
		Output_Object.Char_Count := 4;
	    when ARM_Output.Index => Output_Object.Indent_Amount := 0;
	    when ARM_Output.Syntax_Summary => Output_Object.Indent_Amount := 6;
	    when ARM_Output.Examples => Output_Object.Indent_Amount := 6;
	    when ARM_Output.Small_Examples => Output_Object.Indent_Amount := 10;
                Ada.Text_IO.Put (Output_Object.Output_File, "    ");
		Output_Object.Char_Count := 4;
	    when ARM_Output.Indented_Examples => Output_Object.Indent_Amount := 18;
                Ada.Text_IO.Put (Output_Object.Output_File, "            ");
		Output_Object.Char_Count := 12;
	    when ARM_Output.Small_Indented_Examples => Output_Object.Indent_Amount := 26;
	        Ada.Text_IO.Put (Output_Object.Output_File, "                    "); -- Six units.
	        Output_Object.Char_Count := 20;
	    when ARM_Output.Swiss_Examples => Output_Object.Indent_Amount := 6;
	    when ARM_Output.Small_Swiss_Examples => Output_Object.Indent_Amount := 10;
                Ada.Text_IO.Put (Output_Object.Output_File, "    ");
		Output_Object.Char_Count := 4;
	    when ARM_Output.Swiss_Indented_Examples => Output_Object.Indent_Amount := 18;
                Ada.Text_IO.Put (Output_Object.Output_File, "            ");
		Output_Object.Char_Count := 12;
	    when ARM_Output.Small_Swiss_Indented_Examples => Output_Object.Indent_Amount := 26;
	        Ada.Text_IO.Put (Output_Object.Output_File, "                    "); -- Six units.
	        Output_Object.Char_Count := 20;
	    when ARM_Output.Syntax_Indented => Output_Object.Indent_Amount := 6;
	    when ARM_Output.Small_Syntax_Indented => Output_Object.Indent_Amount := 10;
                Ada.Text_IO.Put (Output_Object.Output_File, "    ");
		Output_Object.Char_Count := 4;
	    when ARM_Output.Code_Indented => Output_Object.Indent_Amount := 10;
                Ada.Text_IO.Put (Output_Object.Output_File, "    ");
		Output_Object.Char_Count := 4;
	    when ARM_Output.Small_Code_Indented => Output_Object.Indent_Amount := 14;
                Ada.Text_IO.Put (Output_Object.Output_File, "        ");
		Output_Object.Char_Count := 8;
	    when ARM_Output.Indented => Output_Object.Indent_Amount := 14;
                Ada.Text_IO.Put (Output_Object.Output_File, "        ");
		Output_Object.Char_Count := 8;
	    when ARM_Output.Small_Indented => Output_Object.Indent_Amount := 18;
                Ada.Text_IO.Put (Output_Object.Output_File, "            ");
		Output_Object.Char_Count := 12;
	    when ARM_Output.Inner_Indented => Output_Object.Indent_Amount := 18;
                Ada.Text_IO.Put (Output_Object.Output_File, "            ");
		Output_Object.Char_Count := 8;
	    when ARM_Output.Small_Inner_Indented => Output_Object.Indent_Amount := 22;
                Ada.Text_IO.Put (Output_Object.Output_File, "                ");
		Output_Object.Char_Count := 12;
	    when ARM_Output.Bulleted => Output_Object.Indent_Amount := 6;
		-- No prefix in text mode.
	    when ARM_Output.Nested_Bulleted => Output_Object.Indent_Amount := 10;
                Ada.Text_IO.Put (Output_Object.Output_File, "    ");
		-- No prefix in text mode.
		Output_Object.Char_Count := 4;
	    when ARM_Output.Nested_X2_Bulleted => Output_Object.Indent_Amount := 14;
                Ada.Text_IO.Put (Output_Object.Output_File, "        ");
		-- No prefix in text mode.
		Output_Object.Char_Count := 8;
	    when ARM_Output.Small_Bulleted => Output_Object.Indent_Amount := 10;
                Ada.Text_IO.Put (Output_Object.Output_File, "    ");
		-- No prefix in text mode.
		Output_Object.Char_Count := 4;
	    when ARM_Output.Small_Nested_Bulleted => Output_Object.Indent_Amount := 14;
                Ada.Text_IO.Put (Output_Object.Output_File, "        ");
		-- No prefix in text mode.
		Output_Object.Char_Count := 8;
	    when ARM_Output.Small_Nested_X2_Bulleted => Output_Object.Indent_Amount := 18;
                Ada.Text_IO.Put (Output_Object.Output_File, "            ");
		-- No prefix in text mode.
		Output_Object.Char_Count := 12;
	    when ARM_Output.Indented_Bulleted => Output_Object.Indent_Amount := 18;
                Ada.Text_IO.Put (Output_Object.Output_File, "            ");
		-- No prefix in text mode.
		Output_Object.Char_Count := 12;
	    when ARM_Output.Indented_Nested_Bulleted => Output_Object.Indent_Amount := 22;
                Ada.Text_IO.Put (Output_Object.Output_File, "                ");
		-- No prefix in text mode.
		Output_Object.Char_Count := 16;
	    when ARM_Output.Code_Indented_Bulleted => Output_Object.Indent_Amount := 14;
                Ada.Text_IO.Put (Output_Object.Output_File, "        ");
		-- No prefix in text mode.
		Output_Object.Char_Count := 8;
	    when ARM_Output.Code_Indented_Nested_Bulleted => Output_Object.Indent_Amount := 18;
                Ada.Text_IO.Put (Output_Object.Output_File, "            ");
		-- No prefix in text mode.
		Output_Object.Char_Count := 12;
	    when ARM_Output.Syntax_Indented_Bulleted => Output_Object.Indent_Amount := 10;
                Ada.Text_IO.Put (Output_Object.Output_File, "    ");
		-- No prefix in text mode.
		Output_Object.Char_Count := 4;
	    when ARM_Output.Notes_Bulleted => Output_Object.Indent_Amount := 10;
                Ada.Text_IO.Put (Output_Object.Output_File, "    ");
		-- No prefix in text mode.
		Output_Object.Char_Count := 4;
	    when ARM_Output.Notes_Nested_Bulleted => Output_Object.Indent_Amount := 14;
                Ada.Text_IO.Put (Output_Object.Output_File, "        ");
		-- No prefix in text mode.
		Output_Object.Char_Count := 8;
	    when ARM_Output.Hanging => Output_Object.Indent_Amount := 14;
		Output_Object.Is_Hanging := True;
		if No_Prefix then
		    Ada.Text_IO.Put (Output_Object.Output_File, "        ");
		    Output_Object.Saw_Hang_End := True;
		    Output_Object.Char_Count := 8;
		else -- Has prefix
		    -- No units on first line.
		    Output_Object.Saw_Hang_End := False;
		end if;
	    when ARM_Output.Indented_Hanging => Output_Object.Indent_Amount := 14;
		Output_Object.Is_Hanging := True;
		if No_Prefix then
		    Ada.Text_IO.Put (Output_Object.Output_File, "        ");
		    Output_Object.Saw_Hang_End := True;
		    Output_Object.Char_Count := 8;
		else -- Has prefix.
                    Ada.Text_IO.Put (Output_Object.Output_File, "    "); -- Two units on first line.
		    Output_Object.Char_Count := 4;
		    Output_Object.Saw_Hang_End := False;
		end if;
	    when ARM_Output.Small_Hanging => Output_Object.Indent_Amount := 22;
		Output_Object.Is_Hanging := True;
		if No_Prefix then
		    Ada.Text_IO.Put (Output_Object.Output_File, "                "); -- Five units.
		    Output_Object.Saw_Hang_End := True;
		    Output_Object.Char_Count := 16;
		else -- Has prefix.
                    Ada.Text_IO.Put (Output_Object.Output_File, "    "); -- Two units on first line.
		    Output_Object.Char_Count := 4;
		    Output_Object.Saw_Hang_End := False;
		end if;
	    when ARM_Output.Small_Indented_Hanging => Output_Object.Indent_Amount := 22;
		Output_Object.Is_Hanging := True;
		if No_Prefix then
		    Ada.Text_IO.Put (Output_Object.Output_File, "                "); -- Five units.
		    Output_Object.Saw_Hang_End := True;
		    Output_Object.Char_Count := 16;
		else -- Has prefix.
                    Ada.Text_IO.Put (Output_Object.Output_File, "            "); -- Four units on first line.
		    Output_Object.Char_Count := 12;
		    Output_Object.Saw_Hang_End := False;
		end if;

	    when ARM_Output.Hanging_in_Bulleted => Output_Object.Indent_Amount := 14;
		Output_Object.Is_Hanging := True;
		if No_Prefix then
		    Ada.Text_IO.Put (Output_Object.Output_File, "        ");
		    Output_Object.Saw_Hang_End := True;
		    Output_Object.Char_Count := 8;
		else -- Has prefix.
		    Output_Object.Char_Count := 0;
		    Output_Object.Saw_Hang_End := False;
		end if;
	    when ARM_Output.Small_Hanging_in_Bulleted => Output_Object.Indent_Amount := 22;
		Output_Object.Is_Hanging := True;
		if No_Prefix then
		    Ada.Text_IO.Put (Output_Object.Output_File, "                "); -- Five units.
		    Output_Object.Saw_Hang_End := True;
		    Output_Object.Char_Count := 16;
		else -- Has prefix.
                    Ada.Text_IO.Put (Output_Object.Output_File, "    "); -- Two units on first line.
		    Output_Object.Char_Count := 4;
		    Output_Object.Saw_Hang_End := False;
		end if;

	    when ARM_Output.Enumerated => Output_Object.Indent_Amount := 10;
		Output_Object.Is_Hanging := True;
		if No_Prefix then
		    Ada.Text_IO.Put (Output_Object.Output_File, "    ");
		    Output_Object.Saw_Hang_End := True;
		    Output_Object.Char_Count := 4;
		else -- Has prefix.
		    Output_Object.Saw_Hang_End := False;
		end if;
	    when ARM_Output.Small_Enumerated => Output_Object.Indent_Amount := 14;
		Output_Object.Is_Hanging := True;
		if No_Prefix then
		    Ada.Text_IO.Put (Output_Object.Output_File, "        ");
		    Output_Object.Saw_Hang_End := True;
		    Output_Object.Char_Count := 8;
		else -- Has prefix.
		    Output_Object.Saw_Hang_End := False;
		    Ada.Text_IO.Put (Output_Object.Output_File, "    ");
		    Output_Object.Char_Count := 4;
		end if;
	    when ARM_Output.Nested_Enumerated => Output_Object.Indent_Amount := 14;
		Output_Object.Is_Hanging := True;
		if No_Prefix then
		    Ada.Text_IO.Put (Output_Object.Output_File, "        ");
		    Output_Object.Saw_Hang_End := True;
		    Output_Object.Char_Count := 8;
		else -- Has prefix.
		    Output_Object.Saw_Hang_End := False;
		    Ada.Text_IO.Put (Output_Object.Output_File, "    ");
		    Output_Object.Char_Count := 4;
		end if;
	    when ARM_Output.Small_Nested_Enumerated => Output_Object.Indent_Amount := 18;
		Output_Object.Is_Hanging := True;
		if No_Prefix then
		    Ada.Text_IO.Put (Output_Object.Output_File, "            ");
		    Output_Object.Saw_Hang_End := True;
		    Output_Object.Char_Count := 12;
		else -- Has prefix.
		    Output_Object.Saw_Hang_End := False;
		    Ada.Text_IO.Put (Output_Object.Output_File, "        ");
		    Output_Object.Char_Count := 8;
		end if;
	end case;
	if Number /= "" then
	    Ada.Text_IO.Put (Output_Object.Output_File, Number);
	    Output_Object.Char_Count := Output_Object.Char_Count + Number'Length;
	    for I in Integer'Min(Number'Length + 1, 6) .. 6 loop
		Ada.Text_IO.Put (Output_Object.Output_File, ' ');
	        Output_Object.Char_Count := Output_Object.Char_Count + 1;
	    end loop;
	else -- No paragraph number:
	    -- Fill in the indent portion alloted to the paragraph number:
	    while Output_Object.Char_Count < Output_Object.Indent_Amount loop
		Ada.Text_IO.Put (Output_Object.Output_File, ' ');
	        Output_Object.Char_Count := Output_Object.Char_Count + 1;
	    end loop;
        end if;
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
		Output_Object.Tab_Stops := Tab_Stops;
		    -- We'll expand proportional stops here (text characters
		    -- are larger than the variable ones these are set up for).
		for I in 1 .. Tab_Stops.Number loop
		    if ARM_Output."=" (Tab_Stops.Stops(I).Kind,
				       ARM_Output.Left_Proportional) then
		        Output_Object.Tab_Stops.Stops(I).Stop :=
				(Tab_Stops.Stops(I).Stop * 5 / 4) + Output_Object.Indent_Amount;
		    else
		        Output_Object.Tab_Stops.Stops(I).Stop :=
				Tab_Stops.Stops(I).Stop + Output_Object.Indent_Amount;
		    end if;
		end loop;
	    when ARM_Output.Bulleted | ARM_Output.Nested_Bulleted | ARM_Output.Nested_X2_Bulleted |
		 ARM_Output.Small_Bulleted | ARM_Output.Small_Nested_Bulleted | ARM_Output.Small_Nested_X2_Bulleted |
		 ARM_Output.Indented_Bulleted | ARM_Output.Indented_Nested_Bulleted |
		 ARM_Output.Code_Indented_Bulleted | ARM_Output.Code_Indented_Nested_Bulleted |
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
	end case;
	Output_Object.Out_Char_Count := Output_Object.Char_Count;

	-- Note: No_Breaks, Keep_with_Next, and Justification have no effect
	-- here.
--Ada.Text_IO.Put_Line ("Start_Paragraph - Indent=" & Natural'Image(Output_Object.Indent_Amount) & " Cnt=" &
--    Natural'Image(Output_Object.Char_Count));
    end Start_Paragraph;


    procedure End_Paragraph (Output_Object : in out Text_Output_Type) is
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
	if Output_Object.Output_Buffer_Len /= 0 then
	    Spill (Output_Object);
	end if;
	Output_Object.Is_In_Paragraph := False;
	Ada.Text_IO.New_Line (Output_Object.Output_File, 2); -- Double space paragraphs.
    end End_Paragraph;


    procedure Category_Header (Output_Object : in out Text_Output_Type;
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
	Put_Line_Centered (Output_Object.Output_File, Header_Text);
	Ada.Text_IO.New_Line (Output_Object.Output_File);
	Output_Object.Char_Count := 0;
	Output_Object.Out_Char_Count := 0;
    end Category_Header;


    procedure Clause_Header (Output_Object : in out Text_Output_Type;
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
        Ada.Text_IO.New_Line (Output_Object.Output_File);

	-- Special for table of contents:
	if Clause_Number = "" and then Header_Text = "Table of Contents" then
	    Put_Line_Centered (Output_Object.Output_File,
			       Header_Text);
	    Ada.Text_IO.New_Line (Output_Object.Output_File, 2);
	    Output_Object.Char_Count := 0;
	    Output_Object.Out_Char_Count := 0;
	    return;
	end if;

	case Level is
	    when ARM_Contents.Normative_Annex =>
		Put_Line_Centered (Output_Object.Output_File,
				   Clause_Number); -- Note: Clause_Number includes "Annex"
		Put_Line_Centered (Output_Object.Output_File,
				   "(normative)");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
		Put_Line_Centered (Output_Object.Output_File,
				   Header_Text);
	    when ARM_Contents.Informative_Annex =>
		Put_Line_Centered (Output_Object.Output_File,
				   Clause_Number); -- Note: Clause_Number includes "Annex"
		Put_Line_Centered (Output_Object.Output_File,
				   "(informative)");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
		Put_Line_Centered (Output_Object.Output_File,
				   Header_Text);
	    when ARM_Contents.Section =>
	        Put_Line_Centered (Output_Object.Output_File,
				   "Section " & Clause_Number & ": " & Header_Text);
	    when ARM_Contents.Unnumbered_Section =>
	        if Header_Text /= "" then
		    Put_Line_Centered (Output_Object.Output_File,
				       Header_Text);
	        end if;
	    when ARM_Contents.Clause | ARM_Contents.Subclause |
		 ARM_Contents.Subsubclause =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File,
				      Clause_Number & ' ' & Header_Text);
	end case;
	Ada.Text_IO.New_Line (Output_Object.Output_File, 2);
	Output_Object.Char_Count := 0;
	Output_Object.Out_Char_Count := 0;
	-- We don't have any page breaks here to suppress.
    end Clause_Header;


    procedure Revised_Clause_Header (Output_Object : in out Text_Output_Type;
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
	function Header_Text return String is
	    -- Note: Version is not used.
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
        Ada.Text_IO.New_Line (Output_Object.Output_File);

	-- Special for table of contents:
	if Clause_Number = "" and then Header_Text = "Table of Contents" then
	    Put_Line_Centered (Output_Object.Output_File,
			       Header_Text);
	    Ada.Text_IO.New_Line (Output_Object.Output_File, 2);
	    Output_Object.Char_Count := 0;
	    Output_Object.Out_Char_Count := 0;
	    return;
	end if;

	case Level is
	    when ARM_Contents.Normative_Annex =>
		Put_Line_Centered (Output_Object.Output_File,
				   Clause_Number); -- Note: Clause_Number includes "Annex"
		Put_Line_Centered (Output_Object.Output_File,
				   "(normative)");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
		Put_Line_Centered (Output_Object.Output_File,
				   Header_Text);
	    when ARM_Contents.Informative_Annex =>
		Put_Line_Centered (Output_Object.Output_File,
				   Clause_Number); -- Note: Clause_Number includes "Annex"
		Put_Line_Centered (Output_Object.Output_File,
				   "(informative)");
		Ada.Text_IO.New_Line (Output_Object.Output_File);
		Put_Line_Centered (Output_Object.Output_File,
				   Header_Text);
	    when ARM_Contents.Section =>
	        Put_Line_Centered (Output_Object.Output_File,
				   "Section " & Clause_Number & ": " & Header_Text);
	    when ARM_Contents.Unnumbered_Section =>
	        if Header_Text /= "" then
		    Put_Line_Centered (Output_Object.Output_File,
				       Header_Text);
	        end if;
	    when ARM_Contents.Clause | ARM_Contents.Subclause |
		ARM_Contents.Subsubclause =>
	        Ada.Text_IO.Put_Line (Output_Object.Output_File,
				      Clause_Number & ' ' & Header_Text);
	end case;
	Ada.Text_IO.New_Line (Output_Object.Output_File, 2);
	Output_Object.Char_Count := 0;
	Output_Object.Out_Char_Count := 0;
	-- We don't have any page breaks here to suppress.
    end Revised_Clause_Header;


    procedure TOC_Marker (Output_Object : in out Text_Output_Type;
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


    procedure New_Page (Output_Object : in out Text_Output_Type;
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
		Ada.Text_IO.New_Line (Output_Object.Output_File, 2);
	    when ARM_Output.Soft_Page =>
		if not Output_Object.Is_In_Paragraph then
		    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
			"Soft page not in paragraph");
		end if;
		null; -- No page breaks.
		Spill (Output_Object);
	end case;
    end New_Page;


    procedure New_Column (Output_Object : in out Text_Output_Type) is
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
	Ada.Text_IO.New_Line (Output_Object.Output_File);
    end New_Column;


    procedure Start_Table (Output_Object : in out Text_Output_Type;
			   Columns : in ARM_Output.Column_Count;
			   First_Column_Width : in ARM_Output.Column_Count;
			   Alignment : in ARM_Output.Column_Text_Alignment;
			   No_Page_Break : in Boolean;
			   Has_Border : in Boolean;
			   Small_Text_Size : in Boolean;
			   Header_Kind : in ARM_Output.Header_Kind_Type) is
	-- Starts a table. The number of columns is Columns; the first
	-- column has First_Column_Width times the normal column width.
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

	Output_Object.Tab_Stops.Number := Columns;
	declare
	     Width : Natural :=
		(72/(Columns+First_Column_Width-1));
	begin
	    if Columns+First_Column_Width-1 <= 3 then -- Keep it from getting too wide.
		Width := 22;
	    end if;
	    for I in 1 .. Columns loop
	        Output_Object.Tab_Stops.Stops(I) := (Kind => ARM_Output.Left_Fixed,
						     Stop => Width*(I+First_Column_Width-1)+10);
	    end loop;
	end;

	Output_Object.Indent_Amount := 10;
        Ada.Text_IO.Put (Output_Object.Output_File, "          ");
	Output_Object.Char_Count := 10;
	Output_Object.Out_Char_Count := 10;

	Output_Object.Is_In_Paragraph := True;
	Output_Object.Is_In_Table := True;
    end Start_Table;


    procedure Table_Marker (Output_Object : in out Text_Output_Type;
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
		-- Just tab over one row:
		Spill (Output_Object);
		Ada.Text_IO.Put_Line (Output_Object.Output_File, " ");
	        Output_Object.Char_Count := Output_Object.Char_Count + 1;
	        Output_Object.Out_Char_Count := Output_Object.Out_Char_Count + 1;
		for I in 1 .. Output_Object.Tab_Stops.Number loop
		    if Output_Object.Tab_Stops.Stops(I).Stop > Output_Object.Char_Count then
			for J in Output_Object.Char_Count+1 .. Output_Object.Tab_Stops.Stops(I).Stop-1 loop
		            Ada.Text_IO.Put_Line (Output_Object.Output_File, " ");
		            Output_Object.Char_Count := Output_Object.Char_Count + 1;
		            Output_Object.Out_Char_Count := Output_Object.Out_Char_Count + 1;
			end loop;
		    end if;
		end loop;

	    when ARM_Output.End_Caption =>
		Spill (Output_Object);
		Ada.Text_IO.New_Line (Output_Object.Output_File, 2);
	        Ada.Text_IO.Put (Output_Object.Output_File, "          ");
		Output_Object.Char_Count := 10;
		Output_Object.Out_Char_Count := 10;
	    when ARM_Output.End_Header =>
		Spill (Output_Object);
		Ada.Text_IO.New_Line (Output_Object.Output_File, 2);
	        Ada.Text_IO.Put (Output_Object.Output_File, "          ");
		Output_Object.Char_Count := 10;
		Output_Object.Out_Char_Count := 10;
	    when ARM_Output.End_Row | ARM_Output.End_Row_Next_Is_Last =>
		Spill (Output_Object);
		Ada.Text_IO.New_Line (Output_Object.Output_File, 1);
	        Ada.Text_IO.Put (Output_Object.Output_File, "          ");
		Output_Object.Char_Count := 10;
		Output_Object.Out_Char_Count := 10;
	    when ARM_Output.End_Table =>
		Spill (Output_Object);
		Output_Object.Is_In_Paragraph := False;
		Output_Object.Is_In_Table := False;
		Ada.Text_IO.New_Line (Output_Object.Output_File);
		Output_Object.Tab_Stops := ARM_Output.NO_TABS;
	end case;
    end Table_Marker;


    procedure Separator_Line (Output_Object : in out Text_Output_Type;
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
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "---------------------------------------------------------------------");
	else
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "=====================================================================");
	end if;
	Ada.Text_IO.New_Line (Output_Object.Output_File);
    end Separator_Line;


    -- Text output: These are only allowed after a Start_Paragraph and
    -- before any End_Paragraph. Raises Not_Valid_Error if not allowed.

    procedure Ordinary_Text (Output_Object : in out Text_Output_Type;
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
--Ada.Text_IO.Put_Line ("Ordinary_Text: Cnt=" & Natural'Image(Output_Object.Char_Count) &
--" Buffer=" & Natural'Image(Output_Object.Output_Buffer_Len));
	if Output_Object.Char_Count + Text'Length >= LINE_LENGTH - 2 and then
	   Output_Object.Out_Char_Count > Output_Object.Indent_Amount then
	    -- We want a line break here if the line is too long and something was output:
	    Ada.Text_IO.New_Line (Output_Object.Output_File);
	    Make_Indent (Output_Object);
	    --Output_Object.Output_Buffer_Space_Before := False;
	        -- Start of line, this is done by Make_Indent.
	    Spill (Output_Object);
	else
	    Spill (Output_Object);
	end if;
        Ada.Text_IO.Put (Output_Object.Output_File, Text);
        Output_Object.Char_Count := Output_Object.Char_Count + Text'Length;
        Output_Object.Out_Char_Count := Output_Object.Out_Char_Count + Text'Length;
        Output_Object.Output_Buffer_Space_Before := False; -- No space between
							   -- this and any following text.
    end Ordinary_Text;


    procedure Ordinary_Character (Output_Object : in out Text_Output_Type;
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

	if Output_Object.Char_Count >= LINE_LENGTH and then
	   Output_Object.Out_Char_Count > Output_Object.Indent_Amount then
	    -- Insert a break here if anything has been output (but don't
	    -- Spill the buffer):
--Ada.Text_IO.Put_Line ("Ordinary_Char [Break, no spill]: Cnt=" & Natural'Image(Output_Object.Char_Count));
	    Ada.Text_IO.New_Line (Output_Object.Output_File);
	    Make_Indent (Output_Object);
	    --Output_Object.Output_Buffer_Space_Before := False;
		-- Start of line, this is done by Make_Indent.
		-- Note that this may make the space disappear.
	    -- Add the contents of the buffer to the character count for this line:
	    Output_Object.Char_Count := Output_Object.Char_Count +
		Output_Object.Output_Buffer_Len;
	    if Char /= ' ' then
	        Buffer (Output_Object, Char);
	    else -- Break character, spill on the new line:
		if Output_Object.Output_Buffer_Len /= 0 then
	            Spill (Output_Object); -- Output the buffer up to the space.
	            Output_Object.Output_Buffer_Space_Before := True; -- Mid-line now.
	        -- else nothing in buffer, so nothing to output; just skip it.
	        end if;
	    end if;
	elsif Char = ' ' then
	    -- Break character, and it fits on this line:
	    if Output_Object.Output_Buffer_Len /= 0 then
--Ada.Text_IO.Put_Line ("Ordinary_Char [Space spill]: Cnt=" & Natural'Image(Output_Object.Char_Count));
	        Spill (Output_Object); -- Output the buffer up to the space.
	        Output_Object.Output_Buffer_Space_Before := True; -- Mid-line now.
	    else -- nothing in buffer.
		-- nothing to output. But make sure we display a space before
		-- the next item.
	        Output_Object.Output_Buffer_Space_Before := True; -- Mid-line now.
	    end if;
	else
	    Buffer (Output_Object, Char);
	end if;
    end Ordinary_Character;


    procedure Hard_Space (Output_Object : in out Text_Output_Type) is
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
        Buffer (Output_Object, ' ');
    end Hard_Space;


    procedure Line_Break (Output_Object : in out Text_Output_Type) is
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
--Ada.Text_Io.Put_Line ("Line_Break");
	if Output_Object.Output_Buffer_Len /= 0 then
	    Spill (Output_Object);
	end if;
	Ada.Text_IO.New_Line (Output_Object.Output_File);
        Make_Indent (Output_Object);
    end Line_Break;


    procedure Index_Line_Break (Output_Object : in out Text_Output_Type;
				Clear_Keep_with_Next : in Boolean) is
	-- Output a line break for the index. This does not start a new
	-- paragraph in terms of spacing. This corresponds to a "<BR>"
	-- in HTML. If Clear_Keep_with_Next is true, insure that the next
	-- line does not require the following line to stay with it.
	-- Raises Not_Valid_Error if the paragraph is not in the index format.
    begin
	Line_Break (Output_Object);
    end Index_Line_Break;


    procedure Soft_Line_Break (Output_Object : in out Text_Output_Type) is
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
	if Output_Object.Char_Count >= LINE_LENGTH - 10 then
	    if Output_Object.Output_Buffer_Len /= 0 then
	        Spill (Output_Object);
	    end if;
	    Ada.Text_IO.New_Line (Output_Object.Output_File);
            Make_Indent (Output_Object);
	-- else we don't need a line break.
	end if;
    end Soft_Line_Break;


    procedure Soft_Hyphen_Break (Output_Object : in out Text_Output_Type) is
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
	if Output_Object.Char_Count >= LINE_LENGTH - 8 then
	    Spill (Output_Object);
	    Ada.Text_IO.Put_Line (Output_Object.Output_File, "-"); -- Add the hyphen and break.
            Make_Indent (Output_Object);
	-- else we don't need a line break.
	end if;
    end Soft_Hyphen_Break;


    procedure Tab (Output_Object : in out Text_Output_Type) is
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
	-- We use the tab stops as characters here, and fixed and proportional
	-- stops are treated identically.
	-- Find the first stop greater than the current character count. (After
	-- writing a space).
--Ada.Text_IO.Put_Line ("Tab");
	Spill (Output_Object);
        Ada.Text_IO.Put (Output_Object.Output_File, " ");
        Output_Object.Char_Count := Output_Object.Char_Count + 1;
        Output_Object.Out_Char_Count := Output_Object.Out_Char_Count + 1;
	for I in 1 .. Output_Object.Tab_Stops.Number loop
	    if Output_Object.Tab_Stops.Stops(I).Stop > Output_Object.Char_Count then
		for J in Output_Object.Char_Count+1 .. Output_Object.Tab_Stops.Stops(I).Stop-1 loop
	            Ada.Text_IO.Put (Output_Object.Output_File, " ");
	            Output_Object.Char_Count := Output_Object.Char_Count + 1;
		    Output_Object.Out_Char_Count := Output_Object.Out_Char_Count + 1;
--Ada.Text_IO.Put ("#");
		end loop;
		exit;
	    end if;
	end loop; -- If we drop out without finding a tab, we just use the single
		  -- space already written.
--Ada.Text_IO.New_Line;
	Output_Object.Output_Buffer_Space_Before := False; -- Spaces needed were output.
    end Tab;


    procedure Special_Character (Output_Object : in out Text_Output_Type;
			         Char : in ARM_Output.Special_Character_Type) is
	-- Output an special character.
    begin
	case Char is
	    when ARM_Output.EM_Dash =>
		Ordinary_Character (Output_Object, '-'); -- Not available in plain text.
	    when ARM_Output.EN_Dash =>
		Ordinary_Character (Output_Object, '-'); -- Not available in plain text.
	    when ARM_Output.GEQ =>
		Ordinary_Text (Output_Object, ">="); -- Not available in plain text, use the Ada one.
	    when ARM_Output.LEQ =>
		Ordinary_Text (Output_Object, "<="); -- Not available in plain text, use the Ada one.
	    when ARM_Output.NEQ =>
		Ordinary_Text (Output_Object, "/="); -- Not available in plain text, use the Ada one.
	    when ARM_Output.PI =>
		Ordinary_Text (Output_Object, "PI"); -- Not available in plain text.
	    when ARM_Output.Left_Ceiling =>
		Ordinary_Text (Output_Object, "Ceiling("); -- Not available in plain text.
	    when ARM_Output.Right_Ceiling =>
		Ordinary_Text (Output_Object, ")"); -- Not available in plain text.
	    when ARM_Output.Left_Floor =>
		Ordinary_Text (Output_Object, "Floor("); -- Not available in plain text.
	    when ARM_Output.Right_Floor =>
		Ordinary_Text (Output_Object, ")"); -- Not available in plain text.
	    when ARM_Output.Thin_Space =>
		Ordinary_Text (Output_Object, " "); -- Not available in plain text.
	    when ARM_Output.Left_Quote =>
		Ordinary_Text (Output_Object, "`"); -- Not available in plain text, use back-quote.
	    when ARM_Output.Right_Quote =>
		Ordinary_Text (Output_Object, "'"); -- Not available in plain text, use quote.
	    when ARM_Output.Left_Double_Quote =>
		Ordinary_Text (Output_Object, """"); -- Not available in plain text, use double quote.
	    when ARM_Output.Right_Double_Quote =>
		Ordinary_Text (Output_Object, """"); -- Not available in plain text, use double quote.
	    when ARM_Output.Small_Dotless_I =>
		Ordinary_Text (Output_Object, "i"); -- Not available in plain text, use the nearest text.
	    when ARM_Output.Capital_Dotted_I =>
		Ordinary_Text (Output_Object, "I"); -- Not available in plain text, use the nearest text.
	end case;
    end Special_Character;


    procedure Unicode_Character (Output_Object : in out Text_Output_Type;
			         Char : in ARM_Output.Unicode_Type) is
	-- Output a Unicode character, with code position Char.
	Char_Code : constant String := ARM_Output.Unicode_Type'Image(Char);
    begin
	-- We don't check, but we assume this is not a normal character.
	Ordinary_Text (Output_Object, "<Unicode-" & Char_Code(2..Char_Code'Last) & ">");
    end Unicode_Character;


    procedure End_Hang_Item (Output_Object : in out Text_Output_Type) is
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
	if not Output_Object.Is_Hanging then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not a hanging paragraph");
	end if;
	if Output_Object.Saw_Hang_End then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Already saw the end of the hanging part");
	end if;
	Output_Object.Saw_Hang_End := True;

	if Output_Object.Char_Count >= Output_Object.Indent_Amount then
	    if Output_Object.Output_Buffer_Len /= 0 then
	        Spill (Output_Object);
	    end if;
	    Ada.Text_IO.New_Line (Output_Object.Output_File);
            Make_Indent (Output_Object);
	else
	    Spill (Output_Object);
	    for I in Output_Object.Char_Count + 1 ..
		     Output_Object.Indent_Amount loop
	        Ada.Text_IO.Put (Output_Object.Output_File, ' ');
	    end loop;
	    Output_Object.Char_Count := Output_Object.Indent_Amount;
	    Output_Object.Out_Char_Count := Output_Object.Indent_Amount;
	    Output_Object.Output_Buffer_Space_Before := False; -- Spaces needed were output.
	end if;
    end End_Hang_Item;


    procedure Text_Format (Output_Object : in out Text_Output_Type;
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
	use type ARM_Output.Location_Type;
    begin
	if not Output_Object.Is_Valid then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not valid object");
	end if;
	if not Output_Object.Is_In_Paragraph then
	    Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
		"Not in paragraph");
	end if;
	if Location /= Output_Object.Location then
	    if Output_Object.Location /= ARM_Output.Normal then
	        Buffer(Output_Object, ')');
	    end if;
	end if;
	if Change /= Output_Object.Change then
	    if Change = ARM_Output.Both then
		-- Open only the one(s) needed:
	        case Output_Object.Change is
		    -- Note: Version is not used.
		    when ARM_Output.Insertion =>
			-- Open the deletion:
		        Buffer(Output_Object, '[');
		    when ARM_Output.Deletion =>
			-- Open the insertion:
		        Buffer(Output_Object, '{');
		    when ARM_Output.None =>
		        Buffer(Output_Object, '{');
		        Buffer(Output_Object, '[');
		    when ARM_Output.Both =>
		        null;
	        end case;
	    elsif Output_Object.Change = ARM_Output.Both then
		-- Close only the one(s) needed:
	        case Change is
		    -- Note: Version is not used.
		    when ARM_Output.Insertion =>
			-- Close the deletion:
		        Buffer(Output_Object, ']');
		    when ARM_Output.Deletion =>
			-- Close the insertion:
		        Buffer(Output_Object, '}');
		    when ARM_Output.None =>
		        Buffer(Output_Object, ']');
		        Buffer(Output_Object, '}');
		    when ARM_Output.Both =>
		        null;
	        end case;
	    else -- Both can't get here.
	        case Output_Object.Change is
		    when ARM_Output.Insertion =>
		        Buffer(Output_Object, '}');
		    when ARM_Output.Deletion =>
		        Buffer(Output_Object, ']');
		    when ARM_Output.None =>
		        null;
		    when ARM_Output.Both =>
		        Buffer(Output_Object, ']');
		        Buffer(Output_Object, '}');
	        end case;
	        case Change is
		    -- Note: Version is not used.
		    when ARM_Output.Insertion =>
		        Buffer(Output_Object, '{');
		    when ARM_Output.Deletion =>
		        Buffer(Output_Object, '[');
		    when ARM_Output.None =>
		        null;
		    when ARM_Output.Both =>
		        Buffer(Output_Object, '{');
		        Buffer(Output_Object, '[');
	        end case;
	    end if;
	    Output_Object.Change := Change;
	end if;
	if Location /= Output_Object.Location then
	    if Location /= ARM_Output.Normal then
		Buffer(Output_Object, '(');
	    end if;
	    Output_Object.Location := Location;
	end if;
	null; -- Nothing else to do for plain text.
    end Text_Format;


    procedure Clause_Reference (Output_Object : in out Text_Output_Type;
				Text : in String;
				Clause_Number : in String) is
	-- Generate a reference to a clause in the standard. The text of
	-- the reference is "Text", and the number of the clause is
	-- Clause_Number. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.
    begin
	Ordinary_Text (Output_Object, Text); -- Nothing special in this format.
    end Clause_Reference;


    procedure Index_Target (Output_Object : in out Text_Output_Type;
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


    procedure Index_Reference (Output_Object : in out Text_Output_Type;
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


    procedure DR_Reference (Output_Object : in out Text_Output_Type;
			    Text : in String;
			    DR_Number : in String) is
	-- Generate a reference to an DR from the standard. The text
	-- of the reference is "Text", and DR_Number denotes
	-- the target. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.
    begin
	Ordinary_Text (Output_Object, Text); -- Nothing special in this format.
    end DR_Reference;


    procedure AI_Reference (Output_Object : in out Text_Output_Type;
			    Text : in String;
			    AI_Number : in String) is
	-- Generate a reference to an AI from the standard. The text
	-- of the reference is "Text", and AI_Number denotes
	-- the target (in folded format). For hyperlinked formats, this should
	-- generate a link; for other formats, the text alone is generated.
    begin
	Ordinary_Text (Output_Object, Text); -- Nothing special in this format.
    end AI_Reference;


    procedure Local_Target (Output_Object : in out Text_Output_Type;
			    Text : in String;
			    Target : in String) is
	-- Generate a local target. This marks the potential target of local
	-- links identified by "Target". Text is the text of the target.
	-- For hyperlinked formats, this should generate a link target;
	-- for other formats, only the text is generated.
    begin
	Ordinary_Text (Output_Object, Text); -- Nothing special in this format.
    end Local_Target;


    procedure Local_Link (Output_Object : in out Text_Output_Type;
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


    procedure URL_Link (Output_Object : in out Text_Output_Type;
			Text : in String;
			URL : in String) is
	-- Generate a link to the URL given.
	-- Text is the text of the link.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.
    begin
	Ordinary_Text (Output_Object, Text); -- Nothing special in this format.
    end URL_Link;


    procedure Picture  (Output_Object : in out Text_Output_Type;
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

end ARM_Text;
