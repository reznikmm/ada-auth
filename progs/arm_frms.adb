    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This subprogram is part of the command processor.
    --
    -- We use dispatching calls to call the formatter, so the details of
    -- formatting are insulated from the code that reads the source and
    -- determines the details of the text.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2002, 2004, 2005, 2006, 2007, 2009, 2011, 2019, 2020, 2022
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
    --  2/10/06 - RLB - Split from base package.
    --  9/22/06 - RLB - Revised to use Clause_Number_Type, and to support
    --			Subsubclauses.
    -- 10/16/06 - RLB - Added definition of old non-terminals for NT linking.
    --  2/16/07 - RLB - Added missing code to handle comments here.
    --  7/31/07 - RLB - Added code to detect duplicated titles.
    -- 12/18/07 - RLB - Added Plain_Annex and associated commands.
    --  5/06/09 - RLB - Added Labeled_Deleted_xxx.
    --  5/07/09 - RLB - Changed above to load dead clauses.
    -- 10/18/11 - RLB - Changed to GPLv3 license.
    --  5/07/20 - RLB - Added additional tracing.
    --  1/29/22 - RLB - Added Note numbering code.
    --  5/11/22 - RLB - Added LabeledRevisedSubClauseIsoClause.
    --  5/27/22 - RLB - Added ChgTermDef.

separate(ARM_Format)
procedure Scan (Format_Object : in out Format_Type;
	        File_Name : in String;
	        Section_Number : in ARM_Contents.Section_Number_Type;
	        Starts_New_Section : in Boolean) is
    -- Scans the contents for File_Name, determining the table of contents
    -- for the section. The results are written to the contents package.
    -- Starts_New_Section is True if the file starts a new section.
    -- Section_Number is the number (or letter) of the section.
    --
    -- This also does three other jobs:
    -- (1) Inserts all nonterminals into the syntax index so that we can
    --     generate forward links to them.
    -- (2) Generates a guesstimate of how many notes appear in a Notes
    --     section so that ISO_2004 formatting can omit note numbers if there
    --     is only one.
    -- (3) Collects all of the term definitions so that they are available
    --     when the Terms and Definitions clause is generated.

    type Items is record
        Command : Command_Type;
        Close_Char : Character; -- Ought to be }, ], >, or ).
        Open_Line : String(1..12); -- For debugging only.
    end record;
    Nesting_Stack : array (1 .. 60) of Items;
    Nesting_Stack_Ptr : Natural := 0;
    Saw_a_Section_Header : Boolean := False;

    Input_Object : ARM_File.File_Input_Type;
    
    Most_Recent_Level : ARM_Contents.Level_Type;
        -- The most recent level seen. Note that we use Format_Object.Clause_Number
        -- to find the most recent clause number.
    Current_Paragraph_Count : Natural; -- Number of paragraphs since last
        -- 'interesting' command.     


    procedure Check_Paragraph is
	-- Open a paragraph if needed. We do this here to count the occurrence
        -- of paragraphs.
    begin
	if not Format_Object.In_Paragraph then
	    if Format_Object.Number_Paragraphs and then
		not Format_Object.No_Para_Num then
                -- Always displays something:
                Format_Object.In_Paragraph := True;
                Current_Paragraph_Count := Current_Paragraph_Count + 1;
	    else -- No paragraph numbers (or if the paragraph
		 -- number has been suppressed with @NoParaNum):

--Ada.Text_IO.Put_Line ("Check_Paragraph, no number: format= " & Paragraph_Type'Image(Format_Object.Next_Paragraph_Format_Type) &
--   " output style= " & ARM_Output.Paragraph_Style_Type'Image(Format_Object.Style));
		 -- Start the paragraph:
		 if (ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted_No_Delete_Message) or else
		     ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted_Inserted_Number_No_Delete_Message) or else
		     ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted) or else
		     ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted_Inserted_Number)) then
		     --ARM_Format."=" (Format_Object.Changes, ARM_Format.New_Only) then
			-- Nothing at all should be showm.
		     Format_Object.No_Start_Paragraph := True;
                 else
                     Format_Object.In_Paragraph := True;
                     Current_Paragraph_Count := Current_Paragraph_Count + 1;
                 end if;
             end if;       
        -- else already in a paragraph.
	end if;
    end Check_Paragraph;
    

    procedure Check_End_Paragraph is
	-- Check for the end of a paragraph; closing it if necessary.
	-- We will never be in a paragraph after this routine.
    begin
	if Format_Object.In_Paragraph then
	    Format_Object.In_Paragraph := False;
	    Format_Object.No_Start_Paragraph := False;
	    Format_Object.No_Para_Num := False;
        -- else already not in paragraph.
        end if;
    end Check_End_Paragraph;
    

    procedure Set_Nesting_for_Command (Command : in Command_Type;
				       Param_Ch : in Character) is
        -- Push the command onto the nesting stack.
    begin
        if Nesting_Stack_Ptr < Nesting_Stack'Last then
	    Nesting_Stack_Ptr := Nesting_Stack_Ptr + 1;
	    Nesting_Stack (Nesting_Stack_Ptr) :=
	        (Command => Command,
		 Close_Char => ARM_Input.Get_Close_Char (Param_Ch),
                 Open_Line => (1..12 => ' ')); -- Set below.
--Ada.Text_IO.Put_Line (" &Stack (" & Command_Type'Image(Command) & "); Close-Char=" &
--  Nesting_Stack(Nesting_Stack_Ptr).Close_Char);
	    Ada.Strings.Fixed.Move (Target => Nesting_Stack (Nesting_Stack_Ptr).Open_Line,
		Source => ARM_File.Line_String (Input_Object),
		Drop   => Ada.Strings.Right);
        else
	    Ada.Text_IO.Put_Line ("** Nesting stack overflow on line" & ARM_File.Line_String (Input_Object));
	    for I in reverse Nesting_Stack'range loop
	        Ada.Text_IO.Put_Line ("-- Command at" & Natural'Image(I) & " has a close char of '" &
		    Nesting_Stack (Nesting_Stack_Ptr).Close_Char & "' for " & Command_Type'Image(Nesting_Stack (Nesting_Stack_Ptr).Command));
	    end loop;
	    raise Program_Error;
        end if;
    end Set_Nesting_for_Command;


    procedure Scan_Command_with_Parameter is
        -- Scan the start of a command with a parameter.
        -- The parameter character has been scanned, and
        -- a stack item pushed.
        Title : ARM_Contents.Title_Type;
        Title_Length : Natural;

        procedure Get_Change_Version (Is_First : in Boolean;
				      Version : out ARM_Contents.Change_Version_Type) is
	    -- Get a parameter named "Version", containing a character
	    -- representing the version number.
	    Ch, Close_Ch : Character;
        begin
	    ARM_Input.Check_Parameter_Name (Input_Object,
	        Param_Name => "Version" & (8..ARM_Input.Command_Name_Type'Last => ' '),
	        Is_First => Is_First,
	        Param_Close_Bracket => Close_Ch);
	    if Close_Ch /= ' ' then
	        -- Get the version character:
	        ARM_File.Get_Char (Input_Object, Ch);
	        Version := ARM_Contents.Change_Version_Type(Ch);
	        ARM_File.Get_Char (Input_Object, Ch);
	        if Ch /= Close_Ch then
		    Ada.Text_IO.Put_Line ("  ** Bad close for change version on line " & ARM_File.Line_String (Input_Object));
		    ARM_File.Replace_Char (Input_Object);
	        end if;
	    -- else no parameter. Weird.
	    end if;
        end Get_Change_Version;


        procedure Get_Change_Kind (Kind : out ARM_Database.Paragraph_Change_Kind_Type) is
            -- Get a parameter named "Kind", containing a word representing
            -- a change kind.
            Kind_Name : ARM_Input.Command_Name_Type;
            Ch, Close_Ch : Character;
        begin
            ARM_Input.Check_Parameter_Name (Input_Object,
                Param_Name => "Kind" & (5..ARM_Input.Command_Name_Type'Last => ' '),
                Is_First => False,
                Param_Close_Bracket => Close_Ch);
            if Close_Ch /= ' ' then
                -- Get the kind word:
                Arm_Input.Get_Name (Input_Object, Kind_Name);
                ARM_File.Get_Char (Input_Object, Ch);
                if Ch /= Close_Ch then
                    Ada.Text_IO.Put_Line ("  ** Bad close for change kind on line " & ARM_File.Line_String (Input_Object));
                    Arm_File.Replace_Char (Input_Object);
                end if;
                begin
                     Kind := ARM_Paragraph.Get_Change_Kind (Kind_Name);
                exception
                    when Program_Error =>
                    Ada.Text_IO.Put_Line ("  ** Bad kind for change kind: " &
                        Ada.Strings.Fixed.Trim (Kind_Name, Ada.Strings.Right) &
                        " on line " & ARM_File.Line_String (Input_Object));
                end;
            -- else no parameter. Weird.
            end if;
        end Get_Change_Kind;


        procedure Get_Term_Group (Group : out ARM_Paragraph.Term_Group_Index) is
            -- Get a parameter named "Group",
            -- containing a character representing the term grouping.
            Ch, Close_Ch : Character;
            Grp_Ch : Character;
        begin
            ARM_Input.Check_Parameter_Name (Input_Object,
                Param_Name => "Group" & (6..ARM_Input.Command_Name_Type'Last => ' '),
                Is_First => False,
                Param_Close_Bracket => Close_Ch);
            if Close_Ch /= ' ' then
                -- Get the version character:
                Arm_File.Get_Char (Input_Object, Grp_Ch);
                ARM_File.Get_Char (Input_Object, Ch);
                if Ch /= Close_Ch then
                    Ada.Text_IO.Put_Line ("  ** Bad close for term group on line " &
                         ARM_File.Line_String (Input_Object));
                    Arm_File.Replace_Char (Input_Object);
                end if;
            -- else no parameter. Weird.
            end if;
            begin
                Group := ARM_Paragraph.Get_Term_Group (Grp_Ch);
            exception
                when Program_Error =>
                    Ada.Text_IO.Put_Line ("  ** Bad group name " & Grp_Ch & " on line " &
                        ARM_File.Line_String (Input_Object));
            end;
        end Get_Term_Group;

    begin
        case Nesting_Stack(Nesting_Stack_Ptr).Command is
	    when Labeled_Section | Labeled_Section_No_Break |
		 Labeled_Annex | Labeled_Informative_Annex |
		 Labeled_Normative_Annex | Labeled_Clause |
		 Labeled_Subclause | Labeled_Subsubclause =>
	        -- Load the title into the Title string:
	        ARM_Input.Copy_to_String_until_Close_Char (
		    Input_Object,
		    Nesting_Stack(Nesting_Stack_Ptr).Close_Char,
		    Title, Title_Length);
	        Title(Title_Length+1 .. Title'Last) :=
		    (others => ' ');
	        if Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Subclause then
		    Format_Object.Clause_Number :=
			(Section   => Format_Object.Clause_Number.Section,
			 Clause    => Format_Object.Clause_Number.Clause,
			 Subclause => Format_Object.Clause_Number.Subclause + 1,
			 Subsubclause => 0);
                     Most_Recent_Level := ARM_Contents.Subclause;
	        elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Subsubclause then
		    Format_Object.Clause_Number.Subsubclause :=
			Format_Object.Clause_Number.Subsubclause + 1;
                     Most_Recent_Level := ARM_Contents.Subsubclause;
	        elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Clause then
		    Format_Object.Clause_Number :=
			(Section   => Format_Object.Clause_Number.Section,
			 Clause    => Format_Object.Clause_Number.Clause + 1,
			 Subclause => 0, Subsubclause => 0);
                     Most_Recent_Level := ARM_Contents.Clause;
	        elsif Saw_a_Section_Header then
		    Ada.Text_IO.Put_Line ("  ** Multiple section headers in a file, line " &
			    ARM_File.Line_String (Input_Object));
	        else
		    Saw_a_Section_Header := True;
		    Format_Object.Clause_Number :=
			(Section   => Format_Object.Clause_Number.Section, -- Will be set elsewhere.
			 Clause    => 0,
			 Subclause => 0, Subsubclause => 0);
		    if Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Section or else
		       Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Section_No_Break then
                        Most_Recent_Level := ARM_Contents.Section;
		    elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Annex then
                        Most_Recent_Level := ARM_Contents.Plain_Annex;
		    elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Normative_Annex then
                        Most_Recent_Level := ARM_Contents.Normative_Annex;
		    elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Informative_Annex then
                        Most_Recent_Level := ARM_Contents.Informative_Annex;
                    else
		        Ada.Text_IO.Put_Line ("  ** Impossible command " &
                            Command_Type'Image(Nesting_Stack(Nesting_Stack_Ptr).Command) &
			    ARM_File.Line_String (Input_Object));
                        raise Program_Error;
		    end if;
	        end if;

		Check_End_Paragraph; -- End any paragraph that we're in.
		begin
		    declare
			Ref : constant String := ARM_Contents.Lookup_Clause_Number (Title);
		    begin
			-- If we get here, this title is already defined. Oops.
			Ada.Text_IO.Put_Line ("  ** Title """ &
			    Title(1..Title_Length) & """ is multiply defined on line " &
			    ARM_File.Line_String (Input_Object));
			Ada.Text_IO.Put_Line ("     Initial use is for clause " & Ref);
		    end;
		exception
		    when ARM_Contents.Not_Found_Error =>
			-- OK, not previously defined.

		        -- Load the title into the contents package:
			ARM_Contents.Add (Title, Most_Recent_Level,
				          Format_Object.Clause_Number);
		end;

                Current_Paragraph_Count := 0;
	        Nesting_Stack_Ptr := Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Header)");

	    when Unnumbered_Section =>
	        -- Load the title into the Title string:
	        ARM_Input.Copy_to_String_until_Close_Char (
		    Input_Object,
		    Nesting_Stack(Nesting_Stack_Ptr).Close_Char,
		    Title, Title_Length);
	        Title(Title_Length+1 .. Title'Last) :=
		    (others => ' ');
	        Format_Object.Unnumbered_Section :=
		    Format_Object.Unnumbered_Section + 1;
	        -- This section will be numbered 0.Unnumbered_Section:
	        Format_Object.Clause_Number :=
		    (Section   => 0,
		     Clause    => Format_Object.Unnumbered_Section,
		     Subclause => 0, Subsubclause => 0);
                Most_Recent_Level := ARM_Contents.Unnumbered_Section;

		Check_End_Paragraph; -- End any paragraph that we're in.
		begin
		    declare
			Ref : constant String := ARM_Contents.Lookup_Clause_Number (Title);
		    begin
			-- If we get here, this title is already defined. Oops.
			Ada.Text_IO.Put_Line ("  ** Title """ &
			    Title(1..Title_Length) & """ is multiply defined on line " &
			    ARM_File.Line_String (Input_Object));
			Ada.Text_IO.Put_Line ("     Initial use is for clause " & Ref);
		    end;
		exception
		    when ARM_Contents.Not_Found_Error =>
			-- OK, not previously defined.

		        -- Load the title into the contents package:
		        ARM_Contents.Add (Title, ARM_Contents.Unnumbered_Section,
					  Format_Object.Clause_Number);
		end;

                Current_Paragraph_Count := 0;
	        Nesting_Stack_Ptr := Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Header)");

	    when Labeled_Revised_Annex |
		 Labeled_Revised_Informative_Annex |
		 Labeled_Revised_Normative_Annex |
		 Labeled_Revised_Section |
		 Labeled_Revised_Clause |
		 Labeled_Revised_Subclause |
		 Labeled_Revised_Subclause_ISO_Clause |
		 Labeled_Revised_Subsubclause =>
	        declare
		    Old_Title : ARM_Contents.Title_Type;
		    Old_Title_Length : Natural;
		    Close_Ch : Character;
		    Version : ARM_Contents.Change_Version_Type := '0';
		    Initial_Version : ARM_Contents.Change_Version_Type := '0';
	        begin
		    Get_Change_Version (Is_First => True,
				        Version => Version);

		    -- Check for the optional "InitialVersion" parameter,
		    -- stopping when we reach "New":
		    declare
		        Which_Param : ARM_Input.Param_Num;
		        Ch		: Character;
		    begin
		        -- If there is no InitialVersion command, use the same
		        -- version of the rest of the command.
		        loop
		            ARM_Input.Check_One_of_Parameter_Names (Input_Object,
			        Param_Name_1 => "InitialVersion" & (15..ARM_Input.Command_Name_Type'Last => ' '),
			        Param_Name_2 => "New" & (4..ARM_Input.Command_Name_Type'Last => ' '),
			        Is_First => False,
			        Param_Found => Which_Param,
			        Param_Close_Bracket => Close_Ch);

			    if Which_Param = 1 and then Close_Ch /= ' ' then
			        -- Found InitialVersion
			        ARM_File.Get_Char (Input_Object, Ch);
			        Initial_Version := Ch;
			        ARM_File.Get_Char (Input_Object, Ch);
			        if Ch /= Close_Ch then
				    Ada.Text_IO.Put_Line ("  ** Bad close for InitialVersion parameter on line " &
				        ARM_File.Line_String (Input_Object));
				    ARM_File.Replace_Char (Input_Object);
			        end if;
			    else -- We found "New" (or an error)
			        exit; -- Handling of New is below.
			    end if;
		        end loop;
		    end;

		    if Close_Ch /= ' ' then
		        -- There is a parameter:
		        -- Load the new title into the Title string:
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
			    Close_Ch,
			    Title, Title_Length);
		        Title(Title_Length+1 .. Title'Last) :=
			    (others => ' ');
		        ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Old" & (4..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
		        if Close_Ch /= ' ' then
			    -- There is a parameter:
			    -- Load the new title into the Title string:
			    ARM_Input.Copy_to_String_until_Close_Char (
			        Input_Object,
			        Close_Ch,
			        Old_Title, Old_Title_Length);
			    Old_Title(Old_Title_Length+1 .. Old_Title'Last) :=
			        (others => ' ');
		        end if;
		    end if;
		    ARM_File.Get_Char (Input_Object, Close_Ch);
		    if Close_Ch /= Nesting_Stack(Nesting_Stack_Ptr).Close_Char then
		        Ada.Text_IO.Put_Line ("  ** Bad close for Labeled_Revised_(SubClause|Annex) on line " & ARM_File.Line_String (Input_Object));
		        ARM_File.Replace_Char (Input_Object);
		    end if;

		    if Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Revised_Subclause then
		        Format_Object.Clause_Number :=
			    (Section   => Format_Object.Clause_Number.Section,
			     Clause    => Format_Object.Clause_Number.Clause,
			     Subclause => Format_Object.Clause_Number.Subclause + 1,
			     Subsubclause => 0);
                        Most_Recent_Level := ARM_Contents.Subclause;
		    elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Revised_Subclause_ISO_Clause then
		        if Format_Object.Include_ISO then
                            Format_Object.Clause_Number :=
                                (Section   => Format_Object.Clause_Number.Section,
                                 Clause    => Format_Object.Clause_Number.Clause + 1,
                                 Subclause => 0, Subsubclause => 0);
                            Most_Recent_Level := ARM_Contents.Clause;
                        else -- No ISO, a subclause.                        
                            Format_Object.Clause_Number :=
                                (Section   => Format_Object.Clause_Number.Section,
                                 Clause    => Format_Object.Clause_Number.Clause,
                                 Subclause => Format_Object.Clause_Number.Subclause + 1,
                                 Subsubclause => 0);
                            Most_Recent_Level := ARM_Contents.Subclause;
                        end if;
		    elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Revised_Subsubclause then
		        Format_Object.Clause_Number.Subsubclause :=
			    Format_Object.Clause_Number.Subsubclause + 1;
                        Most_Recent_Level := ARM_Contents.Subsubclause;
		    elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Revised_Clause then
		        Format_Object.Clause_Number :=
			    (Section   => Format_Object.Clause_Number.Section,
			     Clause    => Format_Object.Clause_Number.Clause + 1,
			     Subclause => 0, Subsubclause => 0);
                        Most_Recent_Level := ARM_Contents.Clause;
		    elsif Saw_a_Section_Header then
		        Ada.Text_IO.Put_Line ("  ** Multiple section headers in a file, line " &
			        ARM_File.Line_String (Input_Object));
		    else
		        Saw_a_Section_Header := True;
		        Format_Object.Clause_Number :=
			    (Section   => Format_Object.Clause_Number.Section, -- Will be set elsewhere.
			     Clause    => 0,
			     Subclause => 0, Subsubclause => 0);
			if Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Revised_Section then
                            Most_Recent_Level := ARM_Contents.Section;
			elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Revised_Annex then
                            Most_Recent_Level := ARM_Contents.Plain_Annex;
			elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Revised_Normative_Annex then
                            Most_Recent_Level := ARM_Contents.Normative_Annex;
			elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Revised_Informative_Annex then
                            Most_Recent_Level := ARM_Contents.Informative_Annex;
                        else
		            Ada.Text_IO.Put_Line ("  ** Impossible command " &
                                Command_Type'Image(Nesting_Stack(Nesting_Stack_Ptr).Command) &
			           ARM_File.Line_String (Input_Object));
                            raise Program_Error;
			end if;
		    end if;

                    Check_End_Paragraph; -- End any paragraph that we're in.
		    begin
		        declare
			    Ref : constant String := ARM_Contents.Lookup_Clause_Number (Title);
		        begin
			    -- If we get here, this title is already defined. Oops.
			    Ada.Text_IO.Put_Line ("  ** Title """ &
			        Title(1..Title_Length) & """ is multiply defined on line " &
			        ARM_File.Line_String (Input_Object));
			    Ada.Text_IO.Put_Line ("     Initial use is for clause " & Ref);
		        end;
		    exception
		        when ARM_Contents.Not_Found_Error =>
			    -- OK, not previously defined.

			    -- Load the title into the contents package:
			    ARM_Contents.Add (Title, Most_Recent_Level,
					      Format_Object.Clause_Number,
				              Version => Version);
                            ARM_Contents.Add_Old (Old_Title, Most_Recent_Level,
                                                  Format_Object.Clause_Number,
						  Version => Initial_Version);

		    end;

                    Current_Paragraph_Count := 0;
		    Nesting_Stack_Ptr := Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Header)");
	        end;

	    when Labeled_Added_Annex |
		 Labeled_Added_Informative_Annex |
		 Labeled_Added_Normative_Annex |
		 Labeled_Added_Section |
		 Labeled_Added_Clause |
		 Labeled_Added_Subclause |
		 Labeled_Added_Subsubclause =>
	        declare
		    Ch : Character;
		    Version : ARM_Contents.Change_Version_Type := '0';
		    How : ARM_Output.Change_Type;
		    use type ARM_Output.Change_Type;
	        begin
		    Get_Change_Version (Is_First => True,
				        Version => Version);
		    ARM_Input.Check_Parameter_Name (Input_Object,
		        Param_Name => "Name" & (5..ARM_Input.Command_Name_Type'Last => ' '),
		        Is_First => False,
		        Param_Close_Bracket => Ch);
		    if Ch /= ' ' then
		        -- There is a parameter:
		        -- Load the new title into the Title string:
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
			    Ch,
			    Title, Title_Length);
		        Title(Title_Length+1 .. Title'Last) :=
			    (others => ' ');
		    end if;
		    ARM_File.Get_Char (Input_Object, Ch);
		    if Ch /= Nesting_Stack(Nesting_Stack_Ptr).Close_Char then
		        Ada.Text_IO.Put_Line ("  ** Bad close for Labeled_Added_(Sub)Clause on line " & ARM_File.Line_String (Input_Object));
		        ARM_File.Replace_Char (Input_Object);
		    end if;

		    -- Determine the insertion state for this label:
		    Calc_Change_Disposition (Format_Object,
			Version => Version,
			Operation => ARM_Output.Insertion,
			Text_Kind => How);

		    if How = Do_Not_Display_Text then
			null; -- Nothing to display, so we do *not* number it
			      -- or insert it into the contents database.
		    else
		        begin
		            declare
			        Ref : constant String := ARM_Contents.Lookup_Clause_Number (Title);
		            begin
			        -- If we get here, this title is already defined. Oops.
			        Ada.Text_IO.Put_Line ("  ** Title """ &
			            Title(1..Title_Length) & """ is multiply defined on line " &
			            ARM_File.Line_String (Input_Object));
			        Ada.Text_IO.Put_Line ("     Initial use is for clause " & Ref);
		            end;
		        exception
		            when ARM_Contents.Not_Found_Error =>
			        -- OK, not previously defined.

                                Check_End_Paragraph; -- End any paragraph that we're in.
			        -- Load the title into the contents package:
			        if Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Added_Subclause then
			            Format_Object.Clause_Number :=
				        (Section   => Format_Object.Clause_Number.Section,
				         Clause    => Format_Object.Clause_Number.Clause,
				         Subclause => Format_Object.Clause_Number.Subclause + 1,
				         Subsubclause => 0);
			            ARM_Contents.Add (Title, ARM_Contents.Subclause,
						      Format_Object.Clause_Number,
						      Version => Version);
			            ARM_Contents.Add_Old ((others => ' '),
						      ARM_Contents.Subclause,
						      Format_Object.Clause_Number);
                                    Most_Recent_Level := ARM_Contents.Subclause;
			        elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Added_Subsubclause then
			            Format_Object.Clause_Number.Subsubclause :=
				        Format_Object.Clause_Number.Subsubclause + 1;
			            ARM_Contents.Add (Title, ARM_Contents.Subsubclause,
						      Format_Object.Clause_Number,
						      Version => Version);
			            ARM_Contents.Add_Old ((others => ' '),
						      ARM_Contents.Subsubclause,
						      Format_Object.Clause_Number);
                                    Most_Recent_Level := ARM_Contents.Subsubclause;
			        elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Added_Clause then
			            Format_Object.Clause_Number :=
				        (Section   => Format_Object.Clause_Number.Section,
				         Clause    => Format_Object.Clause_Number.Clause + 1,
				         Subclause => 0, Subsubclause => 0);
			            ARM_Contents.Add (Title, ARM_Contents.Clause,
						      Format_Object.Clause_Number,
						      Version => Version);
			            ARM_Contents.Add_Old ((others => ' '),
						      ARM_Contents.Clause,
						      Format_Object.Clause_Number);
                                    Most_Recent_Level := ARM_Contents.Clause;
			        elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Added_Section then
			            if Saw_a_Section_Header then
				        Ada.Text_IO.Put_Line ("  ** Multiple section headers in a file, line " &
					        ARM_File.Line_String (Input_Object));
			            end if;
			            Saw_a_Section_Header := True;
			            Format_Object.Clause_Number :=
				        (Section   => Format_Object.Clause_Number.Section, -- Will be set elsewhere.
				         Clause    => 0,
				         Subclause => 0, Subsubclause => 0);
			            ARM_Contents.Add (Title,
						      ARM_Contents.Section,
						      Format_Object.Clause_Number,
						      Version => Version);
			            ARM_Contents.Add_Old ((others => ' '),
						      ARM_Contents.Section,
						      Format_Object.Clause_Number);
                                    Most_Recent_Level := ARM_Contents.Section;
			        elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Added_Annex then
			            if Saw_a_Section_Header then
				        Ada.Text_IO.Put_Line ("  ** Multiple section headers in a file, line " &
					        ARM_File.Line_String (Input_Object));
			            end if;
			            Saw_a_Section_Header := True;
			            Format_Object.Clause_Number :=
				        (Section   => Format_Object.Clause_Number.Section, -- Will be set elsewhere.
				         Clause    => 0,
				         Subclause => 0, Subsubclause => 0);
			            ARM_Contents.Add (Title,
						      ARM_Contents.Plain_Annex,
						      Format_Object.Clause_Number,
						      Version => Version);
			            ARM_Contents.Add_Old ((others => ' '),
						      ARM_Contents.Plain_Annex,
						      Format_Object.Clause_Number);
                                    Most_Recent_Level := ARM_Contents.Plain_Annex;
			        elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Added_Normative_Annex then
			            if Saw_a_Section_Header then
				        Ada.Text_IO.Put_Line ("  ** Multiple section headers in a file, line " &
					        ARM_File.Line_String (Input_Object));
			            end if;
			            Saw_a_Section_Header := True;
			            Format_Object.Clause_Number :=
				        (Section   => Format_Object.Clause_Number.Section, -- Will be set elsewhere.
				         Clause    => 0,
				         Subclause => 0, Subsubclause => 0);
			            ARM_Contents.Add (Title,
						      ARM_Contents.Normative_Annex,
						      Format_Object.Clause_Number,
						      Version => Version);
			            ARM_Contents.Add_Old ((others => ' '),
						      ARM_Contents.Normative_Annex,
						      Format_Object.Clause_Number);
                                    Most_Recent_Level := ARM_Contents.Normative_Annex;
			        elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Added_Informative_Annex then
			            if Saw_a_Section_Header then
				        Ada.Text_IO.Put_Line ("  ** Multiple section headers in a file, line " &
					        ARM_File.Line_String (Input_Object));
			            end if;
			            Saw_a_Section_Header := True;
			            Format_Object.Clause_Number :=
				        (Section   => Format_Object.Clause_Number.Section, -- Will be set elsewhere.
				         Clause    => 0,
				         Subclause => 0, Subsubclause => 0);
			            ARM_Contents.Add (Title,
						      ARM_Contents.Informative_Annex,
						      Format_Object.Clause_Number,
						      Version => Version);
			            ARM_Contents.Add_Old ((others => ' '),
						      ARM_Contents.Informative_Annex,
						      Format_Object.Clause_Number);
                                    Most_Recent_Level := ARM_Contents.Informative_Annex;
                                else
                                    Ada.Text_IO.Put_Line ("  ** Impossible command " &
                                        Command_Type'Image(Nesting_Stack(Nesting_Stack_Ptr).Command) &
                                        ARM_File.Line_String (Input_Object));
                                    raise Program_Error;
			        end if;
		        end;
		    end if;

                    Current_Paragraph_Count := 0;
		    Nesting_Stack_Ptr := Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Header)");
	        end;

	    when Labeled_Deleted_Clause |
		 Labeled_Deleted_Subclause |
		 Labeled_Deleted_Subsubclause =>
	        declare
		    Ch : Character;
		    Version : ARM_Contents.Change_Version_Type := '0';
		    How : ARM_Output.Change_Type;
		    use type ARM_Output.Change_Type;
	        begin
		    Get_Change_Version (Is_First => True,
				        Version => Version);
		    ARM_Input.Check_Parameter_Name (Input_Object,
		        Param_Name => "Name" & (5..ARM_Input.Command_Name_Type'Last => ' '),
		        Is_First => False,
		        Param_Close_Bracket => Ch);
		    if Ch /= ' ' then
		        -- There is a parameter:
		        -- Load the new title into the Title string:
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
			    Ch,
			    Title, Title_Length);
		        Title(Title_Length+1 .. Title'Last) :=
			    (others => ' ');
		    end if;
		    ARM_File.Get_Char (Input_Object, Ch);
		    if Ch /= Nesting_Stack(Nesting_Stack_Ptr).Close_Char then
		        Ada.Text_IO.Put_Line ("  ** Bad close for Labeled_Deleted_(Sub)Clause on line " & ARM_File.Line_String (Input_Object));
		        ARM_File.Replace_Char (Input_Object);
		    end if;

		    -- Determine the insertion state for this label:
		    Calc_Change_Disposition (Format_Object,
			Version => Version,
			Operation => ARM_Output.Deletion,
			Text_Kind => How);

                    Check_End_Paragraph; -- End any paragraph that we're in.
--Ada.Text_IO.Put_Line ("Labeled_Deleted disp: " & ARM_Output.Change_Type'Image(How));
		    if How = ARM_Output.None then
			-- Normal text, number normally.
		        begin
		            declare
			        Ref : constant String := ARM_Contents.Lookup_Clause_Number (Title);
		            begin
			        -- If we get here, this title is already defined. Oops.
			        Ada.Text_IO.Put_Line ("  ** Title """ &
			            Title(1..Title_Length) & """ is multiply defined on line " &
			            ARM_File.Line_String (Input_Object));
			        Ada.Text_IO.Put_Line ("     Initial use is for clause " & Ref);
		            end;
		        exception
		            when ARM_Contents.Not_Found_Error =>
			        -- OK, not previously defined.

			        -- Load the title into the contents package:
			        if Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Deleted_Subclause then
			            Format_Object.Clause_Number :=
				        (Section   => Format_Object.Clause_Number.Section,
				         Clause    => Format_Object.Clause_Number.Clause,
				         Subclause => Format_Object.Clause_Number.Subclause + 1,
				         Subsubclause => 0);
			            ARM_Contents.Add (Title, ARM_Contents.Subclause,
						      Format_Object.Clause_Number,
						      Version => '0'); -- Version here is an insertion version, and this was available from the beginning.
			            ARM_Contents.Add_Old ((others => ' '),
						      ARM_Contents.Subclause,
						      Format_Object.Clause_Number);
                                    Most_Recent_Level := ARM_Contents.Subclause;
			        elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Deleted_Subsubclause then
			            Format_Object.Clause_Number.Subsubclause :=
				        Format_Object.Clause_Number.Subsubclause + 1;
			            ARM_Contents.Add (Title, ARM_Contents.Subsubclause,
						      Format_Object.Clause_Number,
						      Version => '0');
			            ARM_Contents.Add_Old ((others => ' '),
						      ARM_Contents.Subsubclause,
						      Format_Object.Clause_Number);
                                    Most_Recent_Level := ARM_Contents.Subsubclause;
                                elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Deleted_Clause then
                                    Format_Object.Clause_Number :=
				       (Section   => Format_Object.Clause_Number.Section,
				        Clause    => Format_Object.Clause_Number.Clause + 1,
				        Subclause => 0, Subsubclause => 0);
                                    ARM_Contents.Add (Title, ARM_Contents.Clause,
						      Format_Object.Clause_Number,
						      Version => '0');
			            ARM_Contents.Add_Old ((others => ' '),
					   	          ARM_Contents.Clause,
						          Format_Object.Clause_Number);
                                    Most_Recent_Level := ARM_Contents.Clause;
                                else
                                    Ada.Text_IO.Put_Line ("  ** Impossible command " &
                                        Command_Type'Image(Nesting_Stack(Nesting_Stack_Ptr).Command) &
                                        ARM_File.Line_String (Input_Object));
                                    raise Program_Error;
			        end if;
		        end;
		    elsif How = ARM_Output.Insertion then
			-- Huh? We're deleting here.
			raise Program_Error;
		    elsif How = ARM_Output.Deletion then
			-- We'll just display the header without a number.
			-- But we need to insert it so that x-refs don't
			-- fail.
		        begin
		            declare
			        Ref : constant String := ARM_Contents.Lookup_Clause_Number (Title);
		            begin
			        -- If we get here, this title is already defined. Oops.
			        Ada.Text_IO.Put_Line ("  ** Title """ &
			            Title(1..Title_Length) & """ is multiply defined on line " &
			            ARM_File.Line_String (Input_Object));
			        Ada.Text_IO.Put_Line ("     Initial use is for clause " & Ref);
		            end;
		        exception
		            when ARM_Contents.Not_Found_Error =>
			        -- OK, not previously defined.

			        -- Load the title into the contents package as a dead clause:
			        ARM_Contents.Add (Title, ARM_Contents.Dead_Clause,
						  (Section   => 0,
					           Clause    => 1,
					           Subclause => 0,
					           Subsubclause => 0),
						  Version => '0');
			        ARM_Contents.Add_Old ((others => ' '),
						  ARM_Contents.Dead_Clause,
						  (Section   => 0,
					           Clause    => 1,
					           Subclause => 0,
					           Subsubclause => 0));
                                Most_Recent_Level := ARM_Contents.Dead_Clause;
		        end;

		    elsif How = Do_Not_Display_Text then
			null; -- Nothing to display/number.
		    end if;

                    Current_Paragraph_Count := 0;
		    Nesting_Stack_Ptr := Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Header)");
	        end;

	    when Syntax_Rule | Added_Syntax_Rule | Deleted_Syntax_Rule =>
		-- @Syn{[Tabs=<Tabset>, ]LHS=<Non-terminal>, RHS=<Production>}
		-- @AddedSyn{Version=[<Version>],[Tabs=<Tabset>, ]LHS=<Non-terminal>, RHS=<Production>}
		-- @DeletedSyn{Version=[<Version>],[Tabs=<Tabset>, ]LHS=<Non-terminal>, RHS=<Production>}
		-- We need to index the non-terminal, so we can link to it
		-- later. (If we didn't do this here, we wouldn't be able
		-- to handle forward references.)
		-- We only care about the non-terminal, so we skip the other
		-- parts.
		declare
		    Close_Ch, Ch : Character;
		    Seen_First_Param : Boolean := False;
		    Non_Terminal : String (1..120);
		    NT_Len : Natural := 0;
		begin
		    if Nesting_Stack(Nesting_Stack_Ptr).Command /= Syntax_Rule then
			-- Get and skip the Version parameter.
			Seen_First_Param := True;
			Get_Change_Version (Is_First => True,
					    Version => Ch);
		    end if;

		    -- Peek to see if Tabs parmeter is present, and skip it if
		    -- it is:
		    ARM_File.Get_Char (Input_Object, Ch);
		    ARM_File.Replace_Char (Input_Object);
		    if Ch = 'T' or else Ch = 't' then
			ARM_Input.Check_Parameter_Name (Input_Object,
			   Param_Name => "Tabs" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			   Is_First => (not Seen_First_Param),
			   Param_Close_Bracket => Close_Ch);
			Seen_First_Param := True;
			if Close_Ch /= ' ' then
			    -- Grab the tab string:
			    ARM_Input.Skip_until_Close_Char (
			        Input_Object,
			        Close_Ch);
			-- else no parameter. Weird.
			end if;
		    end if;

		    -- Get the LHS parameter and save it:
		    ARM_Input.Check_Parameter_Name (Input_Object,
			Param_Name => "LHS" & (4..ARM_Input.Command_Name_Type'Last => ' '),
			Is_First => (not Seen_First_Param),
		        Param_Close_Bracket => Close_Ch);
		    if Close_Ch /= ' ' then
		        -- Copy over the non-terminal:
		        ARM_Input.Copy_to_String_until_Close_Char (
		            Input_Object,
		            Close_Ch,
		            Non_Terminal,
	                    NT_Len);
		    -- else no parameter. Weird.
		    end if;

		    -- Skip the RHS parameter:
		    ARM_Input.Check_Parameter_Name (Input_Object,
		       Param_Name => "RHS" & (4..ARM_Input.Command_Name_Type'Last => ' '),
		       Is_First => False,
		       Param_Close_Bracket => Close_Ch);
		    Seen_First_Param := True;
		    if Close_Ch /= ' ' then
		        -- Grab the tab string:
		        ARM_Input.Skip_until_Close_Char (
			    Input_Object,
			    Close_Ch);
		    -- else no parameter. Weird.
		    end if;

		    Check_End_Paragraph; -- End the paragraph, so the
					 -- next rule gets its own.

		    declare
			The_Non_Terminal : constant String :=
			    Ada.Characters.Handling.To_Lower (
				Get_Current_Item (Format_Object, Input_Object,
				    Non_Terminal(1..NT_Len))); -- Handle embedded @Chg.
			The_Old_Non_Terminal : constant String :=
			    Ada.Characters.Handling.To_Lower (
				Get_Old_Item (Format_Object, Input_Object,
				    Non_Terminal(1..NT_Len))); -- Handle embedded @Chg.
		    begin
			if Ada.Strings.Fixed.Index (The_Non_Terminal, "@") /= 0 then
			    -- Still embedded commands, do not register.
			    Ada.Text_IO.Put_Line ("** Saw Non-Terminal with embedded commands: " &
				Non_Terminal(1..NT_Len) & " in " & Clause_String (Format_Object));
			elsif The_Non_Terminal = "" then
			    null; -- Deleted Non-Terminal, nothing to do.
			else
			    -- Save the non-terminal:
			    declare
			        Link_Target : ARM_Syntax.Target_Type;
			    begin
			         ARM_Syntax.Add_Non_Terminal
			             (NT_Name => The_Non_Terminal,
			              For_Clause => Clause_String (Format_Object),
			              Link_Target => Link_Target);
			    end;
--Ada.Text_IO.Put_Line ("%% Saw simple Non-Terminal: " & The_Non_Terminal & " in "
--   & Clause_String (Format_Object));
			end if;
			if The_Old_Non_Terminal = "" then
			    null; -- No old Non-Terminal, nothing to do.
			elsif ARM_Syntax.Non_Terminal_Clause (The_Old_Non_Terminal) /= "" then
			    null; -- This non-terminal is already defined;
				-- that presumably is a *new* definition,
				-- we'll use that instead of this one.
			else
			    -- Save the non-terminal:
			    declare
			        Link_Target : ARM_Syntax.Target_Type;
			    begin
			         ARM_Syntax.Add_Non_Terminal
			             (NT_Name => The_Old_Non_Terminal,
			              For_Clause => Clause_String (Format_Object),
			              Link_Target => Link_Target);
			    end;
--Ada.Text_IO.Put_Line ("%% Saw simple old Non-Terminal: " & The_Old_Non_Terminal & " in "
--   & Clause_String (Format_Object));
			end if;
		    end;
		end;

	    when Comment =>
--Ada.Text_IO.Put_Line("Comment with Close=" & Nesting_Stack(Nesting_Stack_Ptr).Close_Char &
--   " on line " & ARM_File.Line_String (Input_Object));
	        -- Skip the contents of this command.
	        ARM_Input.Skip_until_Close_Char (Input_Object,
		    Nesting_Stack(Nesting_Stack_Ptr).Close_Char,
		    Exit_on_Para_End => False);
		ARM_File.Replace_Char (Input_Object); -- Put the close character back.
--Ada.Text_IO.Put_Line("Comment done");

            when Text_Begin =>
		declare
		    Type_Name : ARM_Input.Command_Name_Type;
		    Ch : Character;
                begin
		    -- OK, now read the begin "type":
		    Arm_Input.Get_Name (Input_Object, Type_Name);
		    Arm_File.Get_Char (Input_Object, Ch);
		    if Ch = ',' then
		        -- Multiple parameters. The remaining
		        -- parameters appear to be format instructions,
		        -- which we ought to replace or remove.
		        Ada.Text_IO.Put_Line ("  -- Multi-parameter begin, line " & ARM_File.Line_String (Input_Object));

		        -- We ignore everything until the end of the
		        -- parameter.
			while Ch /= Nesting_Stack(Nesting_Stack_Ptr).Close_Char loop
			    -- Ignore everything until the end character
			    -- turns up.
			    ARM_File.Get_Char (Input_Object, Ch);
			end loop;
                    end if;

		    if Nesting_Stack (Nesting_Stack_Ptr).Close_Char = Ch then
		        -- Found the end of the parameter.
                        -- This always ends a paragraph.
                        Check_End_Paragraph;
                        if Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Type_Name, Ada.Strings.Right)) = "notes" then
                            -- A "notes" header. Clear the paragraph count.
                            -- Note: We assume that notes groups are not nested.
                            Current_Paragraph_Count := 0;
                        elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Type_Name, Ada.Strings.Right)) = "singlenote" then
                            -- A "singlenote" header. Clear the paragraph count.
                            -- Note: We assume that notes groups are not nested.
                            Current_Paragraph_Count := 0;
                        -- else ignore header.
                        end if;                
		        Nesting_Stack_Ptr := Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Normal Begin)");
                    else
		        ARM_File.Replace_Char (Input_Object);
		        Ada.Text_IO.Put_Line ("  ** Failed to find close for parameter to begin, line " & ARM_File.Line_String (Input_Object));
		        Nesting_Stack_Ptr := Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Bad Begin)");
		    end if;
		end;

	    when Text_End =>
		declare
		    Type_Name : ARM_Input.Command_Name_Type;
		    Ch : Character;
		begin
	            Arm_Input.Get_Name (Input_Object, Type_Name); -- Get the end "type".
		    ARM_File.Get_Char (Input_Object, Ch);
		    if Nesting_Stack (Nesting_Stack_Ptr).Close_Char = Ch then
			 -- Found end of parameter:
			 Nesting_Stack_Ptr := Nesting_Stack_Ptr - 1;
			     -- Remove the "End" record.
                        -- This always ends a paragraph.
                        Check_End_Paragraph;
                        if Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Type_Name, Ada.Strings.Right)) = "notes" then
                            -- A "notes" header. Determine how many paragraphs.
                            -- Note: We assume that notes groups are not nested.
                            declare
                                use type ARM_Contents.Note_Info_Type;
                                Note : ARM_Contents.Note_Info_Type :=
                                    ARM_Contents.Lookup_Note_Info (Most_Recent_Level,
                                                                   Format_Object.Clause_Number);
                            begin
                                if Note = ARM_Contents.No_Notes then
                                    if Current_Paragraph_Count > 1 then
                                         -- Guess that there are many notes:
                                        ARM_Contents.Update_Note_Info (Most_Recent_Level,
                                                                       Format_Object.Clause_Number,
                                                                       ARM_Contents.Many_Notes);
                                        -- Debug:
                                        Ada.Text_IO.Put_Line ("    -- Saw notes with" &
                                             Natural'Image(Current_Paragraph_Count) &
                                             " paragraphs, no other notes, line " &
                                             ARM_File.Line_String (Input_Object));
                                    else
                                         -- Only one (or zero) paragraph, set to single note:
                                        ARM_Contents.Update_Note_Info (Most_Recent_Level,
                                                                       Format_Object.Clause_Number,
                                                                       ARM_Contents.One_Note);
                                        -- Debug:
                                        Ada.Text_IO.Put_Line ("    -- Saw notes with one paragraph, no other notes, line "
                                                         & ARM_File.Line_String (Input_Object));
                                    end if;
                                else
                                    ARM_Contents.Update_Note_Info (Most_Recent_Level,
                                                                   Format_Object.Clause_Number,
                                                                   ARM_Contents.Many_Notes);
                                    Ada.Text_IO.Put_Line ("    -- Saw notes in clause with other notes, line "
                                                     & ARM_File.Line_String (Input_Object));
                                end if;
                            end;                     
                            Current_Paragraph_Count := 0;
                        elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Type_Name, Ada.Strings.Right)) = "singlenote" then
                            -- A "singlenote" header. Set note kind to be one more
                            -- than previous.
                            -- Note: We assume that notes groups are not nested.
                            declare
                                use type ARM_Contents.Note_Info_Type;
                                Note : ARM_Contents.Note_Info_Type :=
                                    ARM_Contents.Lookup_Note_Info (Most_Recent_Level,
                                                                   Format_Object.Clause_Number);
                            begin
                                if Note = ARM_Contents.No_Notes then
                                    ARM_Contents.Update_Note_Info (Most_Recent_Level,
                                                                   Format_Object.Clause_Number,
                                                                   ARM_Contents.One_Note);
                                    -- Debug:
                                    Ada.Text_IO.Put_Line ("    -- Saw single note, no other notes, line "
                                                     & ARM_File.Line_String (Input_Object));
                                else
                                    ARM_Contents.Update_Note_Info (Most_Recent_Level,
                                                                   Format_Object.Clause_Number,
                                                                   ARM_Contents.Many_Notes);
                                    Ada.Text_IO.Put_Line ("    ** Saw single note in clause with other notes, line "
                                                     & ARM_File.Line_String (Input_Object));
                                end if;
                            end;
                            Current_Paragraph_Count := 0;
                        -- else ignore header.
                        end if;                
		    else
			ARM_File.Replace_Char (Input_Object);
			Ada.Text_IO.Put_Line ("  ** Failed to find close for parameter to end, line " & ARM_File.Line_String (Input_Object));
		        Nesting_Stack_Ptr := Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Bad End)");
		    end if;
		end;
            
            when Change | Change_Added | Change_Deleted |
		 Change_Implementation_Defined |
		 Change_Implementation_Advice |
		 Change_Documentation_Requirement |
		 Change_Aspect_Description =>
                 -- These almost always have some displayed text. Rather than
                 -- overworking here, we'll just assume they have text in the
                 -- current mode. (Not always true, but a lot of work to
                 -- figure out otherwise.)
                 Check_Paragraph;      
                 
	    when Change_Term_Def =>
		-- This is a change term and definition command.
		-- It is of the form
		-- @ChgTermDef{Version=[<version>],Kind=(<kind>),Group=[<group>],Term=[<term>],
                --    [InitialVersion=[<version>],]Def=[<text>]
                --    [,Note1=[<n1text>][,Note2=[<n2text>][,Note3=[<n3text>]]]]}
                -- We process this and store the term and definition and notes
                -- if any. We do this so that the terms can be generated early
                -- in the document.
                
		declare
		    Close_Ch, Ch : Character;
                    Key : ARM_Index.Index_Key;
                    Kind : ARM_Database.Paragraph_Change_Kind_Type;
                    Group : ARM_Paragraph.Term_Group_Index;
                    Our_Version : ARM_Contents.Change_Version_Type;
                    Initial_Version : ARM_Contents.Change_Version_Type := '0';
                    use type ARM_Database.Paragraph_Change_Kind_Type;
                    Term : String(1..40);
                    Term_Length : Natural;
                    Def : String(1..250);
                    Def_Length : Natural;
                    Note1 : String(1..500);
                    Note1_Length : Natural := 0;
                    Note2 : String(1..300);
                    Note2_Length : Natural := 0;
                    Note3 : String(1..250);
                    Note3_Length : Natural := 0;
		begin
		    Get_Change_Version (Is_First => True,
			Version => Our_Version);
			-- Read a parameter named "Version".
                    Initial_Version := Our_Version; -- This defaults to the
                        -- Version parameter if not given.

		    Get_Change_Kind (Kind);
			-- Read a parameter named "Kind".

		    Get_Term_Group (Group => Group);
		        -- Read a parameter named "Group".

		    -- Get the "Term" parameter:
                    ARM_Input.Check_Parameter_Name (Input_Object,
		        Param_Name => "Term" & (5..ARM_Input.Command_Name_Type'Last => ' '),
		        Is_First => False,
		        Param_Close_Bracket => Ch);
		    if Ch /= ' ' then
		        -- There is a parameter:
		        -- Load the new title into the Title string:
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
			    Ch,
			    Term, Term_Length);
		        Term(Term_Length+1 .. Term'Last) :=
			    (others => ' ');
		    end if;

		    -- Check for the optional "InitialVersion" parameter,
		    -- stopping when we reach "Def":
		    declare
			Which_Param : ARM_Input.Param_Num;
			Ch		: Character;
		    begin
			-- If there is no InitialVersion command, use the same
			-- version of the rest of the command.
			loop
		            ARM_Input.Check_One_of_Parameter_Names (Input_Object,
			         Param_Name_1 => "InitialVersion" & (15..ARM_Input.Command_Name_Type'Last => ' '),
			         Param_Name_2 => "Def" & (4..ARM_Input.Command_Name_Type'Last => ' '),
			         Is_First => False,
			         Param_Found => Which_Param,
			         Param_Close_Bracket => Close_Ch);

			    if Which_Param = 1 and then Close_Ch /= ' ' then
				-- Found InitialVersion
			        ARM_File.Get_Char (Input_Object, Ch);
				Initial_Version := Ch;
			        ARM_File.Get_Char (Input_Object, Ch);
			        if Ch /= Close_Ch then
				    Ada.Text_IO.Put_Line ("  ** Bad close for InitialVersion parameter on line " &
				        ARM_File.Line_String (Input_Object));
				    ARM_File.Replace_Char (Input_Object);
			        end if;
			    else -- We found "Def" (or an error)
				exit; -- Handling of Def is below.
			    end if;
			end loop;
		    end;

		    -- Process the Def parameter:
		    if Ch /= ' ' then
		        -- There is a parameter:
		        -- Load the new title into the Title string:
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
			    Ch,
			    Def, Def_Length);
		        Def(Def_Length+1 .. Def'Last) :=
			    (others => ' ');
		    end if;

		    Arm_File.Get_Char (Input_Object, Ch);
		    Arm_File.Replace_Char (Input_Object);
		    if Ch = ',' then
                        ARM_Input.Check_Parameter_Name (Input_Object,
                            Param_Name => "Note1" & (6..ARM_Input.Command_Name_Type'Last => ' '),
                            Is_First => False,
                            Param_Close_Bracket => Ch);
                        if Ch /= ' ' then
                            -- There is a parameter:
                            -- Load the new title into the Title string:
                            ARM_Input.Copy_to_String_until_Close_Char (
                                Input_Object,
                                Ch,
                                Note1, Note1_Length);
                            Note1(Note1_Length+1 .. Note1'Last) :=
                                (others => ' ');
                            -- Check for another parameter:
                            Arm_File.Get_Char (Input_Object, Ch);
                            Arm_File.Replace_Char (Input_Object);
                            if Ch = ',' then
                                ARM_Input.Check_Parameter_Name (Input_Object,
                                    Param_Name => "Note2" & (6..ARM_Input.Command_Name_Type'Last => ' '),
                                    Is_First => False,
                                    Param_Close_Bracket => Ch);
                                if Ch /= ' ' then
                                    -- There is a parameter:
                                    -- Load the new title into the Title string:
                                    ARM_Input.Copy_to_String_until_Close_Char (
                                        Input_Object,
                                        Ch,
                                        Note2, Note2_Length);
                                    Note2(Note2_Length+1 .. Note2'Last) :=
                                        (others => ' ');
                                    -- Check for another parameter:
                                    Arm_File.Get_Char (Input_Object, Ch);
                                    Arm_File.Replace_Char (Input_Object);
                                    if Ch = ',' then
                                        ARM_Input.Check_Parameter_Name (Input_Object,
                                            Param_Name => "Note3" & (6..ARM_Input.Command_Name_Type'Last => ' '),
                                            Is_First => False,
                                            Param_Close_Bracket => Ch);
                                        if Ch /= ' ' then
                                            -- There is a parameter:
                                            -- Load the new title into the Title string:
                                            ARM_Input.Copy_to_String_until_Close_Char (
                                                Input_Object,
                                                Ch,
                                                Note3, Note3_Length);
                                            Note3(Note3_Length+1 .. Note3'Last) :=
                                                (others => ' ');
                                        end if;
                                    -- else no more note parameters.
                                    end if;
                                end if;
                            -- else no more note parameters.
                            end if;
                        end if;
                    -- else not more parameters (no notes).
                    end if;
                    
		    -- Now, close the command.
		    Arm_File.Get_Char (Input_Object, Ch);
		    if Ch /= Nesting_Stack(Nesting_Stack_Ptr).Close_Char then
			Ada.Text_IO.Put_Line ("  ** Bad close for ChgTermDef on line " & ARM_File.Line_String (Input_Object));
			Arm_File.Replace_Char (Input_Object);
		    end if;
		    Nesting_Stack_Ptr := Nesting_Stack_Ptr - 1;
		        -- Remove the "Change_Term_Def" record.
                        
                    -- OK, all of the data is in place.

                    -- How the term is handled depends primarily on the
                    -- InitialVersion and its relationship to the version
                    -- we're generating. It only depends on the (current)
                    -- change kind to determine whether the item is deleted
                    -- (it is treated as inserted otherwise).
                    --
                    -- Here, we only care about the cases where the item isn't
                    -- displayed at all.

		    declare
			Insert_Disposition : ARM_Output.Change_Type;
			Delete_Disposition : ARM_Output.Change_Type;
			use type ARM_Output.Change_Type;
		    begin
			-- First, we calculate the insertion
			-- disposition. This only depends upon the
			-- original number and what we're generating:
			if Initial_Version = '0' then -- Original,
						      -- never inserted.
			    Insert_Disposition := ARM_Output.None;
			else
		            Calc_Change_Disposition (
			        Format_Object => Format_Object,
			        Version   => Initial_Version,
			        Operation => ARM_Output.Insertion,
			        Text_Kind => Insert_Disposition);
			end if;

			if (ARM_Database."=" (Kind, ARM_Database.Deleted_No_Delete_Message) or else
			    ARM_Database."=" (Kind, ARM_Database.Deleted_Inserted_Number_No_Delete_Message) or else
			    ARM_Database."=" (Kind, ARM_Database.Deleted) or else
			    ARM_Database."=" (Kind, ARM_Database.Deleted_Inserted_Number)) then
			    -- The current version is some sort of delete:
		            Calc_Change_Disposition (
                                Format_Object => Format_Object,
			        Version   => Our_Version,
			        Operation => ARM_Output.Deletion,
			        Text_Kind => Delete_Disposition);
			else
			    Delete_Disposition := ARM_Output.None;
			end if;

		        if Delete_Disposition = Do_Not_Display_Text or else
		           (Delete_Disposition = ARM_Output.None and then
			    Insert_Disposition = Do_Not_Display_Text) then
                            null; -- This item is not shown at all, so don't
                                  -- insert it into the DB.
Ada.Text_IO.Put_Line ("-- Term not shown on line " & ARM_File.Line_String (Input_Object));
                        else
                            -- Otherwise, insert this into the DB:
                            ARM_Database.Insert (Term_DBs(Group),
		                Sort_Key    => Term(1..Term_Length),
                                Hang_Item   => Term(1..Term_Length),
		                Text        => Def(1..Def_Length),
                                Note1_Text  => Note1(1..Note1_Length),
                                Note2_Text  => Note2(1..Note2_Length),
                                Note3_Text  => Note3(1..Note3_Length),
                                Change_Kind => Kind,
                                Version     => Our_Version,
                                Initial_Version => Initial_Version);
Ada.Text_IO.Put_Line ("-- Term inserted to Group" & Group'Image & " on line " & ARM_File.Line_String (Input_Object));
                        end if;
                    end;
                end;
                
            
	    when others =>
	        null; -- Not in scanner.
        end case;
    end Scan_Command_with_Parameter;


    procedure Handle_End_of_Command is
        -- Unstack and handle the end of Commands.
    begin
        case Nesting_Stack(Nesting_Stack_Ptr).Command is
	    when others =>
	        -- No special handling needed.
	        null;
        end case;
--Ada.Text_IO.Put_Line (" &Unstack (Normal-"& Command_Type'Image(Nesting_Stack(Nesting_Stack_Ptr).Command) & ")");
        Nesting_Stack_Ptr := Nesting_Stack_Ptr - 1;
    end Handle_End_of_Command;


    procedure Scan_Special is
        -- Scan a special command/macro/tab.
        -- These all start with '@'.
        -- @xxxx is a command. It may have parameters delimited by
        -- (), {}, [], or <>. There does not appear to be an escape, so
        -- we don't have to worry about '}' being used in {} brackets,
        -- for example. (Must be a pain to write, though.)
        Command_Name : ARM_Input.Command_Name_Type;
        Ch : Character;
    begin
        ARM_File.Get_Char (Input_Object, Ch);
        if Ch = '\' then
	    -- This represents a tab (or the end of centered text). Start a
            -- paragraph if needed, then done.
            Check_Paragraph;
	    return;
        elsif Ch = '=' then
	    -- This marks the beginning of centered text.
	    -- No paragraph (just formatting). We're done here.
	    return;
        elsif Ch = '^' then
	    -- This represented a tab stop (these should have been
	    -- deleted from the input).	No paragraph (just formatting). We're
            -- done here.
	    return;
        elsif Ch = '@' then
	    -- This represents @ in the text. Start a
            -- paragraph if needed, then done.
            Check_Paragraph;
	    return;
        elsif Ch = ' ' then
	    -- This represents a hard space in the text. Start a
            -- paragraph if needed, then done.
            Check_Paragraph;
	    return;
        elsif Ch = ';' then
	    -- This seems to be an end of command (or substitution) marker.
	    -- For instance, it is used in Section 1:
	    -- .. the distinction between @ResolutionName@;s and ...
	    -- This converts to:
	    -- .. the distinction between Name Resolution Rules and ...
	    -- Without it, the 's' would append to the command name, and
	    -- we would get the wrong command. Thus, it itself does nothing
	    -- at all, so we're done here.
	    return;
        elsif Ch = '-' then
	    -- This represents a subscript. It has an argument.
	    ARM_File.Get_Char (Input_Object, Ch);
	    if ARM_Input.Is_Open_Char (Ch) then -- Start parameter:
	        Set_Nesting_for_Command
		    (Command  => Unknown,
		     Param_Ch => Ch);
                Check_Paragraph;
	    else -- No parameter. Weird.
	        ARM_File.Replace_Char (Input_Object);
	    end if;
	    return;
        elsif Ch = '+' then
	    -- This represents a superscript. It has an argument.
	    ARM_File.Get_Char (Input_Object, Ch);
	    if ARM_Input.Is_Open_Char (Ch) then -- Start parameter:
	        Set_Nesting_for_Command
		    (Command  => Unknown,
		     Param_Ch => Ch);
                Check_Paragraph;
	    else -- No parameter. Weird.
	        ARM_File.Replace_Char (Input_Object);
	    end if;
	    return;
        elsif Ch = ':' then
	    -- This is a period type marker. We're done here.
	    return;
        elsif Ch = '*' then
	    -- This is a line break. We're done here.
	    return;
        elsif Ch = '|' then
	    -- This is a soft line break. We're done here.
	    return;
        elsif Ch = '!' then
	    -- This is a soft hyphen break. We're done here.
	    return;
        elsif Ch = Ascii.LF then
	    -- Stand alone '@'.
	    -- I now believe this is an error. It appears in
	    -- Infosys.MSS, and seems to have something to do with formatting.
	    return;
        end if;
        ARM_File.Replace_Char (Input_Object);
        Arm_Input.Get_Name (Input_Object, Command_Name);
--Ada.Text_IO.Put_Line("Command=" & Command_Name & " Nesting=" & Natural'Image(Nesting_Stack_Ptr));

        ARM_File.Get_Char (Input_Object, Ch);
        if ARM_Input.Is_Open_Char (Ch) then -- Start parameter:
	    Set_Nesting_for_Command
	        (Command  => Command (Ada.Characters.Handling.To_Lower (Command_Name)),
		 Param_Ch => Ch);
	    Scan_Command_with_Parameter;
        else
	    ARM_File.Replace_Char (Input_Object);
	    -- We're not interested in commands with no parameters.
        end if;
    end Scan_Special;

begin
    Ada.Text_IO.Put_Line ("-- Scanning " & File_Name);
    begin
        Arm_File.Open (Input_Object, File_Name);
    exception
        when others =>
	    Ada.Text_IO.Put_Line ("** Unable to open file " & File_Name);
	    return;
    end;
    if Starts_New_Section then
        Format_Object.Clause_Number := (Section => Section_Number, others => 0);
    end if;
    loop
        declare
	    Char : Character;
        begin
	    ARM_File.Get_Char (Input_Object, Char);
--Ada.Text_IO.Put_Line("Char=" & Char & " Nesting=" & Natural'Image(Nesting_Stack_Ptr));
	    case Char is
	        when '@' =>
		    Scan_Special;
		when Ascii.LF =>
		    ARM_File.Get_Char (Input_Object, Char);
		    if Char /= Ascii.LF then
			 -- Soft line break. The details here depend on the format,
                         -- which we won't try to do here. But this does not end
                         -- or usually start a paragraph.
			 ARM_File.Replace_Char (Input_Object);			
		    else -- Hard paragraph break. Only one, no matter
			 -- how many blank lines there are:
			while Char = Ascii.LF loop
			    ARM_File.Get_Char (Input_Object, Char);
			end loop;
			ARM_File.Replace_Char (Input_Object);
			Check_End_Paragraph; -- End the paragraph.
                     end if;
	        when Ascii.SUB =>
		    exit; -- End of file.
	        when others =>
		    if Nesting_Stack_Ptr /= 0 and then
		       Nesting_Stack (Nesting_Stack_Ptr).Close_Char /= ' ' and then
		       Nesting_Stack (Nesting_Stack_Ptr).Close_Char = Char then
    		        -- Closing a command, remove it from the stack.
		        Handle_End_of_Command;
		    else
			-- Ordinary characters, just start a paragraph.
                        Check_Paragraph;
		    end if;
	    end case;
        end;
    end loop;
    -- Reached end of the file.
    Ada.Text_IO.Put_Line ("  Lines scanned: " &
		     ARM_File.Line_String (Input_Object));
    ARM_File.Close (Input_Object);
    if Nesting_Stack_Ptr /= 0 then
        Ada.Text_IO.Put_Line ("   ** Unfinished commands detected.");
	for I in reverse 1 .. Nesting_Stack_Ptr loop
	    Ada.Text_IO.Put_Line ("      Open command=" &
                Nesting_Stack(I).Command'Image & "; Close_Char=" &
                Nesting_Stack(I).Close_Char & "; Line=" &
		Nesting_Stack(I).Open_Line);
	end loop;
    end if;
end Scan;

