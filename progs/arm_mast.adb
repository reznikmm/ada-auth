with Ada.Text_IO,
     Ada.Characters.Handling,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded;
with ARM_Input,
     ARM_File,
     ARM_Format,
     ARM_Output,
     ARM_Text,
     ARM_HTML,
     ARM_RTF,
     ARM_Corr,
     ARM_Master,
     ARM_Contents;
package body ARM_Master is

    --
    -- Ada reference manual formatter.
    --
    -- This package contains the routines to parse the master file, and
    -- execute it.
    --
    -- ---------------------------------------
    -- Copyright 2006  AXE Consultants.
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
    --  1/05/06 - RLB - Created base package to replace hard-coded main program.
    --  1/11/06 - RLB - Continued expanding this; revised to handle separate
    --			creations for the various kinds of output objects.

    type Command_Type is (
	-- Source commands:
        Source, TOC, Index,
	-- Global properties:
	Show_Index_Entries,
	Hide_Index_Entries,
	Show_Annotations,
	Hide_Annotations,
	Number_Paragraphs,
	Title,
	File_Prefix,
	-- HTML properties:
	Single_HTML_Output_File,
	-- RTF properties:
	Single_RTF_Output_File,
	RTF_Header_Prefix,
	-- Other commands:
	Comment, Unknown);

    type Source_Kind is (A_File, TOC, Index, Empty);

    type Source_Item (Kind : Source_Kind := Empty) is record
	case Kind is
	    when Empty => null; 	-- This item includes no source item.
            when TOC | Index => null; -- No info for these, they just hold a place
				      -- in the collating order.
	    when A_File =>
		File_Name          : String(1..80);
		File_Name_Len      : Natural;
		Section_Name       : String(1..10);
		Section_Name_Len   : Natural;
		Section_Number     : ARM_Contents.Section_Number_Type;
		Starts_New_Section : Boolean;
		Has_Title          : Boolean;
	end case;
    end record;

    subtype Source_Count is Integer range 0 .. 100;
    subtype Source_Index is Source_Count range 1 .. Source_Count'Last;
    Source_Data : array (Source_Index) of Source_Item;
    Source_Length : Source_Count := 0;

    type Versioned_Item is array (ARM_Contents.Change_Version_Type) of
	Ada.Strings.Unbounded.Unbounded_String;

    -- Command line (global) properties:
    Change_Kind : ARM_Format.Change_Kind; -- Changes to generate.
    Change_Version : ARM_Contents.Change_Version_Type; -- Change version.
    Document : ARM_Format.Document_Type; -- Document to generate. -- *** Remove.

    -- Global properties:
    Display_Index_Entries : Boolean := False; -- Should Index entries be displayed?
    Document_Title : Versioned_Item; -- Document title.
    Output_File_Prefix : Ada.Strings.Unbounded.Unbounded_String; -- Output file prefix.
    Include_Annotations : Boolean := False; -- Should annotations be included in the output?
    Should_Number_Paragraphs : Boolean := False; -- Should paragraphs be numbered?

    -- HTML properties:
    Use_Large_HTML_Files : Boolean := False; -- Use large output files.

    -- RTF properties:
    Use_Large_RTF_Files : Boolean := False; -- Use large output files.
    Header_Prefix : Versioned_Item;

    function "+" (Source : Ada.Strings.Unbounded.Unbounded_String) return String
        renames Ada.Strings.Unbounded.To_String;

    function "+" (Source : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

    function Decode_Command (Name : in ARM_Input.Command_Name_Type) return Command_Type is
	-- Return the command value for a particular command name:
	Canonical_Name : constant String :=
	    Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Name, Ada.Strings.Right));
    begin
	if Canonical_Name = "source" then
	    return Source;
	elsif Canonical_Name = "toc" then
	    return TOC;
	elsif Canonical_Name = "index" then
	    return Index;
	elsif Canonical_Name = "showindexentries" then
	    return Show_Index_Entries;
	elsif Canonical_Name = "hideindexentries" then
	    return Hide_Index_Entries;
	elsif Canonical_Name = "showannotations" then
	    return Show_Annotations;
	elsif Canonical_Name = "hideannotations" then
	    return Hide_Annotations;
	elsif Canonical_Name = "numberparagraphs" then
	    return Number_Paragraphs;
	elsif Canonical_Name = "title" then
	    return Title;
	elsif Canonical_Name = "fileprefix" then
	    return File_Prefix;
	elsif Canonical_Name = "singlehtmloutputfile" then
	    return Single_HTML_Output_File;
	elsif Canonical_Name = "singlertfoutputfile" then
	    return Single_RTF_Output_File;
	elsif Canonical_Name = "rtfheaderprefix" then
	    return RTF_Header_Prefix;
	elsif Canonical_Name = "comment" then
	    return Comment;
	else
	    return Unknown;
	end if;
    end Decode_Command;


    function Get_Versioned_Item (Item_Details : in Versioned_Item;
			         For_Version  : in ARM_Contents.Change_Version_Type)
	return String is
	-- Get a versioned item for the appropriate version.
	use type Ada.Strings.Unbounded.Unbounded_String;
    begin
	for I in reverse ARM_Contents.Change_Version_Type'First .. For_Version loop
	    if Item_Details(I) /= Ada.Strings.Unbounded.Null_Unbounded_String then
		return +Item_Details(I);
	    -- else keep looking, not defined for this version.
	    end if;
	end loop;
	return ""; -- Not defined for any version.
    end Get_Versioned_Item;


    procedure Read_Master_File (Input_Object : in out ARM_Input.Input_Type'Class) is
	-- Read the master file, saving the information into data structures
	-- here.

	procedure Process_Command is
	    Command_Name : ARM_Input.Command_Name_Type;
	    Ch : Character;
	    use type ARM_Output.Size_Type;
	    Command : Command_Type;

	    Close_Ch : Character;
	    procedure Get_Open_Char is
		-- Get an open character, setting Close_Ch appropriately;
		-- generate an error if there isn't one.
	        Ch : Character;
	    begin
	        ARM_Input.Get_Char (Input_Object, Ch);
	        if ARM_Input.Is_Open_Char (Ch) then -- Start parameter:
		    Close_Ch := ARM_Input.Get_Close_Char (Ch);
		else
		    Ada.Text_IO.Put_Line ("** Parameter missing for command on line" & ARM_Input.Line_String (Input_Object));
		    Close_Ch := ' ';
		end if;
	    end Get_Open_Char;

	    function Get_Single_String return String is
		-- Returns the (single) parameter of a command.
		Ch : Character;
	        Item : String(1..200);
	        ILen : Natural := 0;
	    begin
		Get_Open_Char;
	        if Close_Ch /= ' ' then
		    -- Copy over the string:
		    ARM_Input.Copy_to_String_until_Close_Char (
		        Input_Object,
		        Close_Ch,
		        Item,
		        ILen);
		    return Item(1..ILen);
		else -- didn't find an opening character.
		    return "";
		end if;
	    end Get_Single_String;

	    procedure Get_Boolean (Param_Name : in ARM_Input.Command_Name_Type;
				   Result : out Boolean) is
		-- Get a boolean value from a parameter named Param_Name.
		Ch, Close_Ch : Character;
	    begin
		ARM_Input.Check_Parameter_Name (Input_Object,
		    Param_Name => Param_Name,
		    Is_First => False,
		    Param_Close_Bracket => Close_Ch);
		if Close_Ch /= ' ' then
		    -- Get the Boolean character:
		    ARM_Input.Get_Char (Input_Object, Ch);
--Ada.Text_IO.Put_Line("  Bool=" & Ch);
		    case Ch is
			when 'F' | 'f' | 'N' | 'n' =>
			    Result := False;
			when 'T' | 't' | 'Y' | 'y' =>
			    Result := True;
			when others =>
			    Ada.Text_IO.Put_Line ("  ** Bad value for boolean parameter " &
				Ada.Strings.Fixed.Trim (Param_Name, Ada.Strings.Right) &
				" on line " & ARM_Input.Line_String (Input_Object));
		    end case;
		    ARM_Input.Get_Char (Input_Object, Ch);
		    if Ch /= Close_Ch then
		        Ada.Text_IO.Put_Line ("  ** Bad close for boolean parameter " &
			    Ada.Strings.Fixed.Trim (Param_Name, Ada.Strings.Right) &
			    " on line " & ARM_Input.Line_String (Input_Object));
			ARM_Input.Replace_Char (Input_Object);
		    end if;
		-- else no parameter. Weird.
		end if;
	    end Get_Boolean;

	    procedure Process_Versioned_Item (Item : in out Versioned_Item) is
		-- @Command{Version=[<Version>],Text=[<Text>]}
	        Param_Close_Ch, Ch : Character;
	        Text : String(1..80);
	        TLen : Natural := 0;
		Version : ARM_Contents.Change_Version_Type;
	    begin
--Ada.Text_IO.Put_Line("Process versioned item command");
		Get_Open_Char;
	        ARM_Input.Check_Parameter_Name (Input_Object,
		    Param_Name => "Version" & (8..ARM_Input.Command_Name_Type'Last => ' '),
		    Is_First => True,
		    Param_Close_Bracket => Param_Close_Ch);
		if Param_Close_Ch /= ' ' then
		    -- Get the version character:
		    ARM_Input.Get_Char (Input_Object, Ch);
		    Version := ARM_Contents.Change_Version_Type(Ch);
--Ada.Text_IO.Put_Line("  Version=" & Version);
		    ARM_Input.Get_Char (Input_Object, Ch);
		    if Ch /= Param_Close_Ch then
			Ada.Text_IO.Put_Line ("  ** Bad close for change version on line " & ARM_Input.Line_String (Input_Object));
			ARM_Input.Replace_Char (Input_Object);
		    end if;
		-- else no parameter. Weird.
		end if;

	        ARM_Input.Check_Parameter_Name (Input_Object,
		    Param_Name => "Text" & (5..ARM_Input.Command_Name_Type'Last => ' '),
		    Is_First => False,
		    Param_Close_Bracket => Param_Close_Ch);
	        if Param_Close_Ch /= ' ' then
		    -- Copy over the term:
		    ARM_Input.Copy_to_String_until_Close_Char (
		        Input_Object,
		        Param_Close_Ch,
		        Text,
		        TLen);
--Ada.Text_IO.Put_Line("  Text=" & Text(1..TLen));
		    Item(Version) := +Text(1..TLen);
	        -- else no parameter, error already produced.
	        end if;

	        ARM_Input.Get_Char (Input_Object, Ch);
	        if Ch = Close_Ch then
		    null;
	        else
		    Ada.Text_IO.Put_Line ("** Missing closing character for command on line" & ARM_Input.Line_String (Input_Object));
	            ARM_Input.Replace_Char (Input_Object);
		end if;
 	    end Process_Versioned_Item;

	    procedure Process_Source_Command is
	        -- @Source{Name=<File Name>,SectionName=<Name>,
		-- SectionNumber=<AlphaNum>,NewSection=[T|F],HasTitle=[T|F]}
	        Param_Close_Ch : Character;
	        Item : String(1..80);
	        ILen : Natural := 0;
		use type ARM_Contents.Section_Number_Type;
	    begin
		Get_Open_Char;
	        if Source_Length = Source_Count'Last then
		    Ada.Text_IO.Put_Line ("** Too many source files on line" & ARM_Input.Line_String (Input_Object));
	        else
		    Source_Length := Source_Length + 1;
--Ada.Text_IO.Put_Line("Process source command - Length =" & Source_Count'Image(Source_Length));
		    Source_Data(Source_Length) :=
		        (Kind => A_File,
			 File_Name          => (others => ' '),
			 File_Name_Len      => 0,
			 Section_Name       => (others => ' '),
			 Section_Name_Len   => 0,
			 Section_Number     => 0,
			 Starts_New_Section => True,
			 Has_Title	    => True);
		    ARM_Input.Check_Parameter_Name (Input_Object,
		        Param_Name => "Name" & (5..ARM_Input.Command_Name_Type'Last => ' '),
		        Is_First => True,
		        Param_Close_Bracket => Param_Close_Ch);
		    if Param_Close_Ch /= ' ' then
		        -- Copy over the term:
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
			    Param_Close_Ch,
			    Item,
			    ILen);
--Ada.Text_IO.Put_Line("  Name=" & Item(1..ILen));
			if Ada.Strings.Fixed.Index (Item(1..ILen), ".") = 0 then
			    -- Append the extension:
		            Source_Data(Source_Length).File_Name_Len := ILen + 4;
			    Ada.Strings.Fixed.Move (
					Target => Source_Data(Source_Length).File_Name,
				        Source => Item(1..ILen) & ".MSS");
			else
		            Source_Data(Source_Length).File_Name_Len := ILen;
			    Ada.Strings.Fixed.Move (
				        Target => Source_Data(Source_Length).File_Name,
				        Source => Item(1..ILen));
			end if;
		    -- else no parameter, error already produced.
		    end if;
		    ARM_Input.Check_Parameter_Name (Input_Object,
		        Param_Name => "SectionName" & (12..ARM_Input.Command_Name_Type'Last => ' '),
		        Is_First => False,
		        Param_Close_Bracket => Param_Close_Ch);
		    if Param_Close_Ch /= ' ' then
		        -- Copy over the term:
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
			    Param_Close_Ch,
			    Item,
			    ILen);
--Ada.Text_IO.Put_Line("  Section_Name=" & Item(1..ILen));
		        Source_Data(Source_Length).Section_Name_Len := ILen;
		        Ada.Strings.Fixed.Move (
				    Target => Source_Data(Source_Length).Section_Name,
				    Source => Item(1..ILen));
		    -- else no parameter, error already produced.
		    end if;
		    ARM_Input.Check_Parameter_Name (Input_Object,
		        Param_Name => "SectionNumber" & (14..ARM_Input.Command_Name_Type'Last => ' '),
		        Is_First => False,
		        Param_Close_Bracket => Param_Close_Ch);
		    if Param_Close_Ch /= ' ' then
		        -- Copy over the term:
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
			    Param_Close_Ch,
			    Item,
			    ILen);
--Ada.Text_IO.Put_Line("  Section_Number=" & Item(1..ILen));
			if ILen = 1 and then Item(1) in 'A'..'Z' then
		            Source_Data(Source_Length).Section_Number :=
				21 + Character'Pos(Item(1)) - Character'Pos('A');
			elsif ILen = 1 and then Item(1) in 'a'..'z' then
		            Source_Data(Source_Length).Section_Number :=
				21 + Character'Pos(Item(1)) - Character'Pos('a');
			else
			    begin
				Source_Data(Source_Length).Section_Number :=
				    ARM_Contents.Section_Number_Type'Value(Item(1..Ilen));
			    exception
				when Constraint_Error =>
				    Ada.Text_IO.Put_Line ("** Illegal section number " &
					Item(1..ILen) & " on line" & ARM_Input.Line_String (Input_Object));
			    end;
			end if;
		    -- else no parameter, error already produced.
		    end if;

		    Get_Boolean ("NewSection" & (11..ARM_Input.Command_Name_Type'Last => ' '),
				 Source_Data(Source_Length).Starts_New_Section);

		    Get_Boolean ("HasTitle" & (9..ARM_Input.Command_Name_Type'Last => ' '),
				 Source_Data(Source_Length).Has_Title);

	            ARM_Input.Get_Char (Input_Object, Ch);
	            if Ch = Close_Ch then
		        null;
		    else
		        Ada.Text_IO.Put_Line ("** Missing closing character for command on line" & ARM_Input.Line_String (Input_Object));
	                ARM_Input.Replace_Char (Input_Object);
		    end if;
	        end if;
	    end Process_Source_Command;

	begin
	    -- We don't have any non-identifier commands here, so we can
	    -- go directly to getting the name:
	    ARM_Input.Get_Name (Input_Object, Command_Name);
	    Command := Decode_Command (Command_Name);
Ada.Text_IO.Put_Line("Process command=" & Command_Type'Image(Command));
--Ada.Text_IO.Put_Line("Process command=" & Command_Type'Image(Command) & " Name=" & Command_Name);

	    case Command is
		when Comment =>
		    -- @Comment{<text>}
		    -- Skip the parameter.
		    Get_Open_Char;
		    if Close_Ch /= ' ' then
			ARM_Input.Skip_until_Close_Char (Input_Object, Close_Ch);
		    end if;

		-- Global properties:

		when Show_Index_Entries =>
		    -- @ShowIndexEntries
		    Display_Index_Entries := True;

		when Hide_Index_Entries =>
		    -- @HideIndexEntries
		    Display_Index_Entries := False;

		when Show_Annotations =>
		    -- @ShowAnnotations
		    Include_Annotations := True;

		when Hide_Annotations =>
		    -- @HideAnnotations
		    Include_Annotations := False;

		when Number_Paragraphs =>
		    -- @NumberParagraphs
		    Should_Number_Paragraphs := True;

		when Title =>
		    -- @Title{Version=[<version>],Text=[<title_text>]}
		    Process_Versioned_Item (Document_Title);

		when File_Prefix =>
		    -- @FilePrefix{<File_Prefix>}
		    Output_File_Prefix := +Get_Single_String;

		-- HTML properties:

		when Single_HTML_Output_File =>
		    -- @Single_HTML_Output_File
		    Use_Large_HTML_Files := True;

		-- RTF properties:

		when Single_RTF_Output_File =>
		    -- @Single_RTF_Output_File
		    Use_Large_RTF_Files := True;

		when RTF_Header_Prefix =>
		    -- @RTFHeaderPrefix{Version=[<version>],Text=[<title_text>]}
		    Process_Versioned_Item (Header_Prefix);

		-- Source files:
		when TOC =>
		    -- @TOC
		    if Source_Length = Source_Count'Last then
			Ada.Text_IO.Put_Line ("** Too many source files on line" & ARM_Input.Line_String (Input_Object));
		    else
			Source_Length := Source_Length + 1;
			Source_Data(Source_Length) :=
			    (Kind => TOC);
		    end if;

		when Index =>
		    -- @Index
		    if Source_Length = Source_Count'Last then
			Ada.Text_IO.Put_Line ("** Too many source files on line" & ARM_Input.Line_String (Input_Object));
		    else
			Source_Length := Source_Length + 1;
			Source_Data(Source_Length) :=
			    (Kind => Index);
		    end if;

		when Source =>
		    Process_Source_Command;

		when Unknown =>
		    Ada.Text_IO.Put_Line ("  -- Unknown command (skipped) - " &
		        Ada.Strings.Fixed.Trim (Command_Name, Ada.Strings.Right) &
		        " on line " & ARM_Input.Line_String (Input_Object));
	    end case;
	end Process_Command;

    begin
        Reading_Loop: loop
	    declare
	        Char : Character;
	    begin
	        ARM_Input.Get_Char (Input_Object, Char);
	        case Char is
		    when '@' =>
		        Process_Command;
		    when Ascii.LF | ' ' =>
			null;
		    when Ascii.SUB =>
		        -- End of file.
		        exit Reading_Loop;
		    when others =>
			Ada.Text_IO.Put_Line ("** Character(s) not in a command in the master file on line" & ARM_Input.Line_String (Input_Object));
		        while (Char /= ' ' and then Char /= '@' and then
				Char /= Ascii.LF and then Char /= Ascii.SUB) loop
			    ARM_Input.Get_Char (Input_Object, Char);
		        end loop;
		        ARM_Input.Replace_Char (Input_Object);
	        end case;
	    end;
        end loop Reading_Loop;
    end Read_Master_File;


    procedure Create_Format (Format_Object : in out ARM_Format.Format_Type) is
	-- Create an appropriate format object.
    begin
	ARM_Format.Create (Format_Object, Document, Change_Kind, Change_Version,
		Display_Index_Entries);
--*** TBD: Use Include_Annotations and Should_Number_Paragraphs instead of "Document" here.
    end Create_Format;


    procedure Scan_Sources is
	-- Run the scanning pass on the source files:
	Format_Object : ARM_Format.Format_Type;
    begin
	Create_Format (Format_Object);
	for Source_Index in Source_Data'First .. Source_Length loop
	    case Source_Data(Source_Index).Kind is
		when TOC => null;
		when Index =>
		    ARM_Format.Insert_Index (Format_Object);
		when A_File =>
		    ARM_Format.Scan (Format_Object,
			Source_Data(Source_Index).File_Name(1..Source_Data(Source_Index).File_Name_Len),
			Section_Number => Source_Data(Source_Index).Section_Number,
		        Starts_New_Section => Source_Data(Source_Index).Starts_New_Section);
		when Empty => null;
	    end case;
	end loop;

	ARM_Format.Destroy (Format_Object);
    end Scan_Sources;


    procedure Generate_Sources (Output_Object : in out ARM_Output.Output_Type'Class) is
	-- Generate the results from the source files:
	Format_Object : ARM_Format.Format_Type;
    begin
	Create_Format (Format_Object);

	for Source_Index in Source_Data'First .. Source_Length loop
	    case Source_Data(Source_Index).Kind is
		when TOC =>
		    ARM_Format.Write_Table_of_Contents (Format_Object, Output_Object);
		when Index =>
		    ARM_Format.Write_Index (Format_Object, Output_Object);
		when A_File =>
		    if Source_Data(Source_Index).Has_Title then
		        ARM_Format.Process (Format_Object,
			    Source_Data(Source_Index).File_Name(1..Source_Data(Source_Index).File_Name_Len),
			    Output_Object,
		            Section_Name   => Source_Data(Source_Index).Section_Name(1..Source_Data(Source_Index).Section_Name_Len),
			    Section_Number => Source_Data(Source_Index).Section_Number,
		            Starts_New_Section => Source_Data(Source_Index).Starts_New_Section);
		    else
		        ARM_Format.Process (Format_Object,
			    Source_Data(Source_Index).File_Name(1..Source_Data(Source_Index).File_Name_Len),
			    Output_Object,
		            Section_Name   => Source_Data(Source_Index).Section_Name(1..Source_Data(Source_Index).Section_Name_Len),
			    Section_Number => 0, -- No title.
		            Starts_New_Section => Source_Data(Source_Index).Starts_New_Section);
		    end if;
		when Empty => null;
	    end case;
	end loop;

	ARM_Format.Destroy (Format_Object);
    end Generate_Sources;


    procedure Read_and_Process_Master_File (
	File_Name : in String;
	The_Document : ARM_Format.Document_Type; -- Document to generate.
	The_Change_Kind : ARM_Format.Change_Kind; -- Changes to generate.
	The_Change_Version : ARM_Contents.Change_Version_Type; -- Change version.
        Output_Format : in Output_Format_Type) is
	-- Read and process the master file given.
	Input_Object : ARM_File.File_Input_Type;
    begin
	Ada.Text_IO.Put_Line ("-- Reading master file " & File_Name);
	begin
	    Arm_File.Open (Input_Object, File_Name);
	exception
	    when others =>
		Ada.Text_IO.Put_Line ("** Unable to open master file " & File_Name);
		return;
	end;

	Document := The_Document;
	Change_Kind := The_Change_Kind;
	Change_Version := The_Change_Version;

	Source_Length := 0;
	Read_Master_File (Input_Object);

	Ada.Text_IO.Put_Line ("  Lines processed: " &
		ARM_File.Line_String (Input_Object));
	Arm_File.Close (Input_Object);

	-- Now, execute the commands from the file.

	Scan_Sources; -- Scanning pass.

	-- Create the appropriate output object, then generate the results:
	case Output_Format is
	    when HTML =>
	        declare
		    Output : ARM_HTML.HTML_Output_Type;
	        begin
		    ARM_HTML.Create (Output,
				     Big_Files => Use_Large_HTML_Files,
				     File_Prefix => +Output_File_Prefix,
				     Title => Get_Versioned_Item(Document_Title,Change_Version));
		    Generate_Sources (Output);
		    ARM_HTML.Close (Output);
	        end;
	    when RTF =>
	        declare
		    Output : ARM_RTF.RTF_Output_Type;
	        begin
--*** Temp: (Document will be removed; details will move to the .MSM file.)
		    case Document is
		        when ARM_Format.AARM =>
			    if ARM_Format."=" (Change_Kind, ARM_Format.Old_Only) or else
			       Change_Version = '0' then
			        ARM_RTF.Create (Output,
					        Page_Size => ARM_RTF.Letter,
					        Includes_Changes => False,
					        Big_Files => Use_Large_RTF_Files,
					        Primary_Serif_Font => ARM_RTF.Times_New_Roman,
					        Primary_Sans_Serif_Font => ARM_RTF.Arial,
					        File_Prefix => +Output_File_Prefix,
					        Title => Get_Versioned_Item(Document_Title,Change_Version),
					        Header_Prefix => Get_Versioned_Item(Header_Prefix,Change_Version));
			    else
			        ARM_RTF.Create (Output,
					        Page_Size => ARM_RTF.Letter,
					        Includes_Changes => True,
					        Big_Files => Use_Large_RTF_Files,
					        Primary_Serif_Font => ARM_RTF.Times_New_Roman,
					        Primary_Sans_Serif_Font => ARM_RTF.Arial,
					        File_Prefix => +Output_File_Prefix,
					        Title => Get_Versioned_Item(Document_Title,Change_Version),
					        Header_Prefix => Get_Versioned_Item(Header_Prefix,Change_Version));
			    end if;
		        when ARM_Format.RM =>
			    if ARM_Format."=" (Change_Kind, ARM_Format.Old_Only) or else
			       Change_Version = '0' then
			        ARM_RTF.Create (Output,
					        Page_Size => ARM_RTF.Ada95,
					        Includes_Changes => False,
					        Big_Files => Use_Large_RTF_Files,
					        Primary_Serif_Font => ARM_RTF.Times_New_Roman,
					        Primary_Sans_Serif_Font => ARM_RTF.Arial,
					        File_Prefix => +Output_File_Prefix,
					        Title => Get_Versioned_Item(Document_Title,Change_Version),
					        Header_Prefix => Get_Versioned_Item(Header_Prefix,Change_Version));
			    else
			        ARM_RTF.Create (Output,
					        Page_Size => ARM_RTF.Ada95,
					        Includes_Changes => True,
					        Big_Files => Use_Large_RTF_Files,
					        Primary_Serif_Font => ARM_RTF.Times_New_Roman,
					        Primary_Sans_Serif_Font => ARM_RTF.Arial,
					        File_Prefix => +Output_File_Prefix,
					        Title => Get_Versioned_Item(Document_Title,Change_Version),
					        Header_Prefix => Get_Versioned_Item(Header_Prefix,Change_Version));
			    end if;
		        when ARM_Format.RM_ISO =>
			    if ARM_Format."=" (Change_Kind, ARM_Format.Old_Only) or else
			       Change_Version = '0' then
			        ARM_RTF.Create (Output,
					        Page_Size => ARM_RTF.A4,
					        Includes_Changes => False,
					        Big_Files => Use_Large_RTF_Files,
					        Primary_Serif_Font => ARM_RTF.Times_New_Roman,
					        Primary_Sans_Serif_Font => ARM_RTF.Helvetica,
					        File_Prefix => +Output_File_Prefix,
					        Title => Get_Versioned_Item(Document_Title,Change_Version),
					        Header_Prefix => Get_Versioned_Item(Header_Prefix,Change_Version));
			    else
			        ARM_RTF.Create (Output,
					        Page_Size => ARM_RTF.A4,
					        Includes_Changes => True,
					        Big_Files => Use_Large_RTF_Files,
					        Primary_Serif_Font => ARM_RTF.Times_New_Roman,
					        Primary_Sans_Serif_Font => ARM_RTF.Helvetica,
					        File_Prefix => +Output_File_Prefix,
					        Title => Get_Versioned_Item(Document_Title,Change_Version),
					        Header_Prefix => Get_Versioned_Item(Header_Prefix,Change_Version));
			    end if;
		    end case;
		    Generate_Sources (Output);
		    ARM_RTF.Close (Output);
	        end;
	    when Text =>
	        declare
		    Output : ARM_Text.Text_Output_Type;
	        begin
		    ARM_Text.Create (Output,
				     File_Prefix => +Output_File_Prefix,
				     Title => Get_Versioned_Item(Document_Title,Change_Version));
		    Generate_Sources (Output);
		    ARM_Text.Close (Output);
	        end;
	    when Corr =>
	        declare
		    Output : ARM_Corr.Corr_Output_Type;
	        begin
		    ARM_Corr.Create (Output,
				     File_Prefix => +Output_File_Prefix,
				     Title => Get_Versioned_Item(Document_Title,Change_Version));
		    Generate_Sources (Output);
		    ARM_Corr.Close (Output);
	        end;
	    when Info =>
	        null; -- Future use.
	        --declare
	        --	Output : ARM_Info.Info_Output_Type;
	        --begin
		--      Create (Output, Use_Large_Files => False); -- The latter is not used.
	        --	Generate_Sources (Output);
	        --	ARM_Info.Close (Output);
	        --end;
        end case;

    end Read_and_Process_Master_File;


end ARM_Master;
