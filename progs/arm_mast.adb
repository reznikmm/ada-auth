with Ada.Text_IO,
     Ada.Characters.Handling,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded;
with ARM_Input,
     ARM_File,
     ARM_Format,
     ARM_Output,
     ARM_Text,
     ARM_Texinfo,
     ARM_HTML,
     ARM_RTF,
     ARM_Corr,
     ARM_Paragraph,
     ARM_Contents;
package body ARM_Master is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package contains the routines to parse the master file, and
    -- execute it.
    --
    -- ---------------------------------------
    -- Copyright 2006, 2007, 2009, 2011, 2012, 2013, 2016, 2022
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
    --  1/05/06 - RLB - Created base package to replace hard-coded main program.
    --  1/11/06 - RLB - Continued expanding this; revised to handle separate
    --			creations for the various kinds of output objects.
    --  1/12/06 - RLB - Added more commands for additional properties.
    --  1/18/06 - RLB - Added the ExampleFont command.
    --  1/27/06 - RLB - Added HTMLTabs command.
    --  2/19/06 - RLB - Set Number_Paragraphs for HTML.
    --  6/22/06 - RLB - Added LinkNonTerminals command.
    --  9/21/06 - RLB - Added the Body_Font command.
    --  9/22/06 - RLB - Added the Note_Format command.
    --  9/25/06 - RLB - Added the Contents_Format command.
    -- 10/04/06 - RLB - Added the List_Format command.
    -- 10/13/06 - RLB - Added specifiable default HTML colors.
    -- 12/19/07 - RLB - Added MS-DOS file names.
    --  5/04/09 - RLB - Added the RTFFooter command.
    --  5/06/09 - RLB - Added the RTFVersionName command.
    -- 10/18/11 - RLB - Changed to GPLv3 license.
    -- 10/19/11 - RLB - Added Texinfo output (from Stephen Leake).
    --  8/31/12 - RLB - Added Output_Path.
    -- 11/26/12 - RLB - Added Subdivision_Names.
    --  3/26/13 - RLB - Added HTMLScript.
    --  3/17/16 - RLB - Added Base_Change_Version.
    --  4/20/16 - RLB - Added HTML_New_Revision_Colors.
    --  4/ 8/22 - RLB - Added Example_Comment_Font.
    --  4/18/22 - RLB - Added Include/Exclude commands.
    --  4/20/22 - RLB - Added SingleTextOutputFile command.
    --  5/11/22 - RLB - Added SelfRefFormat command.

    type Command_Type is (
	-- Source commands:
        Source, TOC,
	-- Global properties:
	Show_Index_Entries,
	Hide_Index_Entries,
	Show_Annotations,
	Hide_Annotations,
	Show_ISO,
	Hide_ISO,
	Link_Non_Terminals,
	Number_Paragraphs,
	Title,
	File_Prefix,
	Example_Font,
	Example_Comment_Font,
	Body_Font,
	Note_Format,
	Contents_Format,
        Self_Ref_Format,
	List_Format,
	Subdivision_Names,
        Include_Groups,
        Exclude_Groups,

        -- Text properties:
	Single_Text_Output_File,
        
	-- HTML properties:
	Single_HTML_Output_File,
	Use_MS_DOS_Names,
	HTML_Kind_Command,
	HTML_Nav_Bar,
	HTML_Tabs,
	HTML_Script,
	HTML_Header,
	HTML_Footer,
	HTML_Color,
	HTML_New_Revision_Colors,
	-- RTF properties:
	Single_RTF_Output_File,
	RTF_Header_Prefix,
	RTF_Footer_Text,
	RTF_Footer,
	RTF_Page_Size,
	RTF_Fonts,
	RTF_Version_Name,
	-- Other commands:
	Comment, Unknown);

    type Source_Kind is (A_File, TOC, Empty);

    type Source_Item (Kind : Source_Kind := Empty) is record
	case Kind is
	    when Empty => null; 	-- This item includes no source item.
            when TOC => null;	        -- No info for this, it just holds a
				        -- place in the collating order.
	    when A_File =>
		File_Name          : String(1..80);
		File_Name_Len      : Natural;
		Section_Name       : String(1..10);
		Section_Name_Len   : Natural;
		Section_Number     : ARM_Contents.Section_Number_Type;
		Starts_New_Section : Boolean;
	end case;
    end record;

    subtype Source_Count is Integer range 0 .. 100;
    subtype Source_Index is Source_Count range 1 .. Source_Count'Last;
    Source_Data : array (Source_Index) of Source_Item;
    Source_Length : Source_Count := 0;

    -- Command line (global) properties:
    Change_Kind : ARM_Format.Change_Kind; -- Changes to generate.
    Change_Version : ARM_Contents.Change_Version_Type; -- Change version.
    Base_Change_Version : ARM_Contents.Change_Version_Type; -- Change version.

    -- Global properties:
    Display_Index_Entries : Boolean := False; -- Should Index entries be displayed?
    Document_Title : ARM_Contents.Versioned_String; -- Document title.
    Output_File_Prefix : Ada.Strings.Unbounded.Unbounded_String; -- Output file prefix.
    Include_Annotations : Boolean := False; -- Should annotations be included in the output?
    Include_ISO_Text : Boolean := False; -- Should ISO text be included in the output?
    Should_Link_Non_Terminals : Boolean :=  False; -- Should non-terminals be linked?
    Should_Number_Paragraphs : Boolean := False; -- Should paragraphs be numbered?
    Font_of_Examples : ARM_Output.Font_Family_Type :=
	ARM_Output.Fixed; -- Which font should be used for examples?
    Font_of_Example_Comments : ARM_Output.Font_Family_Type :=
	ARM_Output.Roman; -- Which font should be used for examples?
    Font_of_Body : ARM_Output.Font_Family_Type :=
	ARM_Output.Roman; -- Which font should be used for the body?
    Use_ISO_2004_Note_Format : Boolean := True;
	-- Should we use the ISO 2004 note format, or the one used in the
	-- Ada 95 standard??
    Use_ISO_2004_Contents_Format : Boolean := True;
	-- Should we use the ISO 2004 contents format, or the one used in the
	-- Ada 95 standard??
    Use_ISO_2004_List_Format : Boolean := True;
	-- Should we use the ISO 2004 list format, or the one used in the
	-- Ada 95 standard??
    Self_Reference_Format : ARM_Format.Self_Ref_Kind := ARM_Format.RM;
        -- Specifies the format of self-references.

    Subdivision_Name_Kind : ARM_Output.Top_Level_Subdivision_Name_Kind :=
		ARM_Output.Section;
                
    Groupings_Set_Up : Boolean := False;
        -- Have we set up the groupings yet?
    Included_Groupings : ARM_Paragraph.Grouping_Array;
        -- The included groupings.
  
    -- Text properties:
    Use_Large_Text_Files : Boolean := False; -- Use small output files by default.

    -- HTML properties:
    Use_Large_HTML_Files : Boolean := False; -- Use small output files by default.
    Use_MS_DOS_Filenames : Boolean := False; -- Use long file names by default.
    HTML_Kind : ARM_HTML.HTML_Type := ARM_HTML.HTML_4_Compatible;
    HTML_Use_Unicode : Boolean := False;
    HTML_Index_URL : Ada.Strings.Unbounded.Unbounded_String;
    HTML_Ref_URL : Ada.Strings.Unbounded.Unbounded_String;
    HTML_Srch_URL : Ada.Strings.Unbounded.Unbounded_String;
    HTML_Use_Buttons : Boolean := True;
    HTML_Nav_On_Top : Boolean := True;
    HTML_Nav_On_Bottom : Boolean := True;
    HTML_Tab_Emulation : ARM_HTML.Tab_Emulation_Type := ARM_HTML.Emulate_Fixed_Only;
    HTML_Script_Text : Ada.Strings.Unbounded.Unbounded_String; -- Empty by default.
    HTML_Header_Text : Ada.Strings.Unbounded.Unbounded_String; -- Empty by default.
    HTML_Footer_Text : Ada.Strings.Unbounded.Unbounded_String; -- Empty by default.
    HTML_Use_New_Revision_Colors : Boolean := False;
    HTML_Text_Color : ARM_HTML.Color_String := "#000000";
    HTML_Background_Color : ARM_HTML.Color_String := "#FFFFF0";
    HTML_Link_Color : ARM_HTML.Color_String := "#0000FF";
    HTML_VLink_Color : ARM_HTML.Color_String := "#800080";
    HTML_ALink_Color : ARM_HTML.Color_String := "#FF0000";

    -- RTF properties:
    Use_Large_RTF_Files : Boolean := False; -- Use small output files by default.
    Header_Prefix : ARM_Contents.Versioned_String;
    Footer_Text   : ARM_Contents.Versioned_String;
    Footer_Use_Date : Boolean := True; -- Use the date by default.
    Footer_Use_Clause_Name : Boolean := True; -- Use the clause name rather than the text above by default.
    Footer_Use_ISO_Format : Boolean := False; -- Use the normal format.
    Version_Name  : ARM_Contents.Versioned_String;

    Page_Size : ARM_RTF.Page_Size := ARM_RTF.Letter; -- Use Letter size by default.
    Serif_Font : ARM_RTF.Serif_Fonts := ARM_RTF.Times_New_Roman; -- Use Times by default.
    Sans_Serif_Font : ARM_RTF.Sans_Serif_Fonts := ARM_RTF.Arial; -- Use Arial by default.

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
	elsif Canonical_Name = "showindexentries" then
	    return Show_Index_Entries;
	elsif Canonical_Name = "hideindexentries" then
	    return Hide_Index_Entries;
	elsif Canonical_Name = "showannotations" then
	    return Show_Annotations;
	elsif Canonical_Name = "hideannotations" then
	    return Hide_Annotations;
	elsif Canonical_Name = "showiso" then
	    return Show_ISO;
	elsif Canonical_Name = "hideiso" then
	    return Hide_ISO;
	elsif Canonical_Name = "linknonterminals" then
	    return Link_Non_Terminals;
	elsif Canonical_Name = "numberparagraphs" then
	    return Number_Paragraphs;
	elsif Canonical_Name = "title" then
	    return Title;
	elsif Canonical_Name = "fileprefix" then
	    return File_Prefix;
	elsif Canonical_Name = "examplefont" then
	    return Example_Font;
	elsif Canonical_Name = "examplecommentfont" then
	    return Example_Comment_Font;
	elsif Canonical_Name = "bodyfont" then
	    return Body_Font;
	elsif Canonical_Name = "noteformat" then
	    return Note_Format;
	elsif Canonical_Name = "contentsformat" then
	    return Contents_Format;
	elsif Canonical_Name = "listformat" then
	    return List_Format;
	elsif Canonical_Name = "selfrefformat" then
	    return Self_Ref_Format;
	elsif Canonical_Name = "subdivisionnames" then
	    return Subdivision_Names;
  	elsif Canonical_Name = "include" then
	    return Include_Groups;
	elsif Canonical_Name = "exclude" then
	    return Exclude_Groups;

	elsif Canonical_Name = "singletextoutputfile" then
	    return Single_Text_Output_File;
          
	elsif Canonical_Name = "singlehtmloutputfile" then
	    return Single_HTML_Output_File;
	elsif Canonical_Name = "usemsdosfilenames" then
	    return Use_MS_DOS_Names;
	elsif Canonical_Name = "htmlkind" then
	    return HTML_Kind_Command;
	elsif Canonical_Name = "htmlnavbar" then
	    return HTML_Nav_Bar;
	elsif Canonical_Name = "htmltabs" then
	    return HTML_Tabs;
	elsif Canonical_Name = "htmlscript" then
	    return HTML_Script;
	elsif Canonical_Name = "htmlheader" then
	    return HTML_Header;
	elsif Canonical_Name = "htmlfooter" then
	    return HTML_Footer;
	elsif Canonical_Name = "htmlnewrevisioncolors" then
	    return HTML_New_Revision_Colors;
	elsif Canonical_Name = "htmlcolor" then
	    return HTML_Color;
	elsif Canonical_Name = "singlertfoutputfile" then
	    return Single_RTF_Output_File;
	elsif Canonical_Name = "rtfheaderprefix" then
	    return RTF_Header_Prefix;
	elsif Canonical_Name = "rtffootertext" then
	    return RTF_Footer_Text;
	elsif Canonical_Name = "rtffooter" then
	    return RTF_Footer;
	elsif Canonical_Name = "rtfpagesize" then
	    return RTF_Page_Size;
	elsif Canonical_Name = "rtffonts" then
	    return RTF_Fonts;
	elsif Canonical_Name = "rtfversionname" then
	    return RTF_Version_Name;
	elsif Canonical_Name = "comment" then
	    return Comment;
	else
	    return Unknown;
	end if;
    end Decode_Command;


    function Get_Versioned_String (Item_Details : in ARM_Contents.Versioned_String;
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
    end Get_Versioned_String;


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
	        Item : String(1..2000);
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
				   Result : out Boolean;
				   Is_First : in Boolean := False) is
		-- Get a boolean value from a parameter named Param_Name.
		Ch, Close_Ch : Character;
	    begin
		ARM_Input.Check_Parameter_Name (Input_Object,
		    Param_Name => Param_Name,
		    Is_First => Is_First,
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

	    procedure Process_Versioned_String (Item : in out ARM_Contents.Versioned_String) is
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
 	    end Process_Versioned_String;

	    procedure Process_RTF_Fonts is
	        -- @RTFFonts{Serif=[Times|Souvenir]},SansSerif=[Arial|Helvetica]}
	        Param_Close_Ch : Character;
	        Item : String(1..80);
	        ILen : Natural := 0;
	    begin
		Get_Open_Char;
--Ada.Text_IO.Put_Line("Process RTF Fonts");
	        ARM_Input.Check_Parameter_Name (Input_Object,
		    Param_Name => "Serif" & (6..ARM_Input.Command_Name_Type'Last => ' '),
		    Is_First => True,
		    Param_Close_Bracket => Param_Close_Ch);
	        if Param_Close_Ch /= ' ' then
		    -- Copy over the font name:
		    ARM_Input.Copy_to_String_until_Close_Char (
		        Input_Object,
		        Param_Close_Ch,
		        Item,
		        ILen);
		    Ada.Text_IO.Put_Line("RTF Serif Font=" & Item(1..ILen));
		    declare
			Name : constant String :=
			    Ada.Characters.Handling.To_Lower (Item(1..ILen));
		    begin
			if Name = "times" then
			    Serif_Font := ARM_RTF.Times_New_Roman;
			elsif Name = "souvenir" then
			    Serif_Font := ARM_RTF.Souvenir;
			else
		            Ada.Text_IO.Put_Line ("** Unknown serif font name: " & Name &
						  " on line" & ARM_Input.Line_String (Input_Object));
			end if;
		    end;
	        -- else no parameter, error already produced.
	        end if;
	        ARM_Input.Check_Parameter_Name (Input_Object,
		    Param_Name => "SansSerif" & (10..ARM_Input.Command_Name_Type'Last => ' '),
		    Is_First => False,
		    Param_Close_Bracket => Param_Close_Ch);
	        if Param_Close_Ch /= ' ' then
		    -- Copy over the font name:
		    ARM_Input.Copy_to_String_until_Close_Char (
		        Input_Object,
		        Param_Close_Ch,
		        Item,
		        ILen);
		    Ada.Text_IO.Put_Line("RTF Sans_Serif Font=" & Item(1..ILen));
		    declare
			Name : constant String :=
			    Ada.Characters.Handling.To_Lower (Item(1..ILen));
		    begin
			if Name = "arial" then
			    Sans_Serif_Font := ARM_RTF.Arial;
			elsif Name = "helvetica" then
			    Sans_Serif_Font := ARM_RTF.Helvetica;
			else
		            Ada.Text_IO.Put_Line ("** Unknown serif font name: " & Name &
						  " on line" & ARM_Input.Line_String (Input_Object));
			end if;
		    end;
	        -- else no parameter, error already produced.
	        end if;
	        ARM_Input.Get_Char (Input_Object, Ch);
	        if Ch = Close_Ch then
		    null;
	        else
		    Ada.Text_IO.Put_Line ("** Missing closing character for command on line" & ARM_Input.Line_String (Input_Object));
	            ARM_Input.Replace_Char (Input_Object);
	        end if;
	    end Process_RTF_Fonts;

	    procedure Process_HTML_Kind is
		-- @HTMLKind{Version=[3|4Comp|4],Unicode=[T|F]}
	        Param_Close_Ch : Character;
	        Item : String(1..80);
	        ILen : Natural := 0;
	    begin
		Get_Open_Char;
--Ada.Text_IO.Put_Line("Process HTML Kind");
	        ARM_Input.Check_Parameter_Name (Input_Object,
		    Param_Name => "Version" & (8..ARM_Input.Command_Name_Type'Last => ' '),
		    Is_First => True,
		    Param_Close_Bracket => Param_Close_Ch);
	        if Param_Close_Ch /= ' ' then
		    -- Copy over the version:
		    ARM_Input.Copy_to_String_until_Close_Char (
		        Input_Object,
		        Param_Close_Ch,
		        Item,
		        ILen);
		    Ada.Text_IO.Put_Line("HTML version and kind=" & Item(1..ILen));
		    declare
			Kind : constant String :=
			    Ada.Characters.Handling.To_Lower (Item(1..ILen));
		    begin
			if Kind = "3" then
			    HTML_Kind := ARM_HTML.HTML_3;
			elsif Kind = "4comp" then
			    HTML_Kind := ARM_HTML.HTML_4_Compatible;
			elsif Kind = "4" then
			    HTML_Kind := ARM_HTML.HTML_4_Only;
			else
		            Ada.Text_IO.Put_Line ("** Unknown HTML version: " & Kind &
						  " on line" & ARM_Input.Line_String (Input_Object));
			end if;
		    end;
	        -- else no parameter, error already produced.
	        end if;

	        Get_Boolean ("Unicode" & (8..ARM_Input.Command_Name_Type'Last => ' '),
			     HTML_Use_Unicode);
		if HTML_Use_Unicode then
		    Ada.Text_IO.Put_Line("HTML will use Unicode characters where appropriate");
		else
		    Ada.Text_IO.Put_Line("HTML will use Unicode characters only when explicitly requested");
		end if;

	        ARM_Input.Get_Char (Input_Object, Ch);
	        if Ch = Close_Ch then
		    null;
	        else
		    Ada.Text_IO.Put_Line ("** Missing closing character for command on line" & ARM_Input.Line_String (Input_Object));
	            ARM_Input.Replace_Char (Input_Object);
	        end if;
	    end Process_HTML_Kind;

	    procedure Process_HTML_Nav_Bar is
		--@HTMLNavBar{RefName=[<URL>],SrchName=[<URL>],
		--    UseButtons=[T|F],OnTop=[T|F],OnBottom=[T|F]}
	        Param_Close_Ch : Character;
	        Item : String(1..80);
	        ILen : Natural := 0;
	    begin
		Get_Open_Char;
--Ada.Text_IO.Put_Line("Process HTML Nav Bar");
	        ARM_Input.Check_Parameter_Name (Input_Object,
		    Param_Name => "RefName" & (8..ARM_Input.Command_Name_Type'Last => ' '),
		    Is_First => True,
		    Param_Close_Bracket => Param_Close_Ch);
	        if Param_Close_Ch /= ' ' then
		    -- Copy over the version:
		    ARM_Input.Copy_to_String_until_Close_Char (
		        Input_Object,
		        Param_Close_Ch,
		        Item,
		        ILen);
		    Ada.Text_IO.Put_Line("HTML reference URL=" & Item(1..ILen));
		    HTML_Ref_Url := + Item(1..ILen);
	        -- else no parameter, error already produced.
	        end if;

	        ARM_Input.Check_Parameter_Name (Input_Object,
		    Param_Name => "SrchName" & (9..ARM_Input.Command_Name_Type'Last => ' '),
		    Is_First => False,
		    Param_Close_Bracket => Param_Close_Ch);
	        if Param_Close_Ch /= ' ' then
		    -- Copy over the version:
		    ARM_Input.Copy_to_String_until_Close_Char (
		        Input_Object,
		        Param_Close_Ch,
		        Item,
		        ILen);
		    Ada.Text_IO.Put_Line("HTML search URL=" & Item(1..ILen));
		    HTML_Srch_Url := + Item(1..ILen);
	        -- else no parameter, error already produced.
	        end if;

	        ARM_Input.Check_Parameter_Name (Input_Object,
		    Param_Name => "IndexName" & (10..ARM_Input.Command_Name_Type'Last => ' '),
		    Is_First => False,
		    Param_Close_Bracket => Param_Close_Ch);
	        if Param_Close_Ch /= ' ' then
		    -- Copy over the version:
		    ARM_Input.Copy_to_String_until_Close_Char (
		        Input_Object,
		        Param_Close_Ch,
		        Item,
		        ILen);
		    Ada.Text_IO.Put_Line("HTML index URL=" & Item(1..ILen));
		    HTML_Index_Url := + Item(1..ILen);
	        -- else no parameter, error already produced.
	        end if;

	        Get_Boolean ("UseButtons" & (11..ARM_Input.Command_Name_Type'Last => ' '),
			     HTML_Use_Buttons);
		if HTML_Use_Buttons then
		    Ada.Text_IO.Put_Line("HTML navigation will use buttons");
		else
		    Ada.Text_IO.Put_Line("HTML navigation will use text labels");
		end if;

	        Get_Boolean ("OnTop" & (6..ARM_Input.Command_Name_Type'Last => ' '),
			     HTML_Nav_On_Top);
		if HTML_Nav_On_Top then
		    Ada.Text_IO.Put_Line("HTML navigation bar will appear on top of pages");
		end if;

	        Get_Boolean ("OnBottom" & (9..ARM_Input.Command_Name_Type'Last => ' '),
			     HTML_Nav_On_Bottom);
		if HTML_Nav_On_Bottom then
		    Ada.Text_IO.Put_Line("HTML navigation bar will appear on bottom of pages");
		end if;

	        ARM_Input.Get_Char (Input_Object, Ch);
	        if Ch = Close_Ch then
		    null;
	        else
		    Ada.Text_IO.Put_Line ("** Missing closing character for command on line" & ARM_Input.Line_String (Input_Object));
	            ARM_Input.Replace_Char (Input_Object);
	        end if;
	    end Process_HTML_Nav_Bar;

	    procedure Process_HTML_Color is
	        --@HTMLColor{Text=[<Color]>,Background=[<Color>],
	        --  Link=[<Color>],VLink=[<Color>],ALink=[<Color>]}

	        procedure Get_Color (Param_Name : in ARM_Input.Command_Name_Type;
				     Is_First : in Boolean;
				     Result : out ARM_HTML.Color_String) is
		    -- Get a color value from a parameter named Param_Name.
		    Ch, Close_Ch : Character;
		    Color : ARM_HTML.Color_String := (others => ' ');
	        begin
		    ARM_Input.Check_Parameter_Name (Input_Object,
		        Param_Name => Param_Name,
		        Is_First => Is_First,
		        Param_Close_Bracket => Close_Ch);
		    if Close_Ch /= ' ' then
		        -- Get the color characters:
		        for I in ARM_HTML.Color_String'range loop
			    ARM_Input.Get_Char (Input_Object, Ch);
			    if Ch = Close_Ch then
			        Ada.Text_IO.Put_Line ("  ** HTML color too short for " &
				    Ada.Strings.Fixed.Trim (Param_Name, Ada.Strings.Right) &
				    " on line " & ARM_Input.Line_String (Input_Object));
				ARM_Input.Replace_Char (Input_Object);
				exit;
			    end if;
			    Color(I) := Ch;
			end loop;
--Ada.Text_IO.Put_Line("  Color=" & Color);
			Result := Color;
		        ARM_Input.Get_Char (Input_Object, Ch);
		        if Ch /= Close_Ch then
		            Ada.Text_IO.Put_Line ("  ** Bad close for color parameter " &
			        Ada.Strings.Fixed.Trim (Param_Name, Ada.Strings.Right) &
			        " on line " & ARM_Input.Line_String (Input_Object));
			    ARM_Input.Replace_Char (Input_Object);
		        end if;
		    -- else no parameter. Weird.
		    end if;
	        end Get_Color;

	    begin
		Get_Open_Char;
--Ada.Text_IO.Put_Line("Process HTML Color");
	        Get_Color ("Text" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			     Is_First => True, Result => HTML_Text_Color);
	        Ada.Text_IO.Put_Line("HTML text color is " & HTML_Text_Color);

	        Get_Color ("Background" & (11..ARM_Input.Command_Name_Type'Last => ' '),
			     Is_First => False, Result => HTML_Background_Color);
	        Ada.Text_IO.Put_Line("HTML background color is " & HTML_Background_Color);

	        Get_Color ("Link" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			     Is_First => False, Result => HTML_Link_Color);
	        Ada.Text_IO.Put_Line("HTML link color is " & HTML_Link_Color);

	        Get_Color ("VLink" & (6..ARM_Input.Command_Name_Type'Last => ' '),
			     Is_First => False, Result => HTML_VLink_Color);
	        Ada.Text_IO.Put_Line("HTML visited link color is " & HTML_VLink_Color);

	        Get_Color ("ALink" & (6..ARM_Input.Command_Name_Type'Last => ' '),
			     Is_First => False, Result => HTML_ALink_Color);
	        Ada.Text_IO.Put_Line("HTML active link color is " & HTML_ALink_Color);

	        ARM_Input.Get_Char (Input_Object, Ch);
	        if Ch = Close_Ch then
		    null;
	        else
		    Ada.Text_IO.Put_Line ("** Missing closing character for command on line" & ARM_Input.Line_String (Input_Object));
	            ARM_Input.Replace_Char (Input_Object);
	        end if;
	    end Process_HTML_Color;

	    procedure Process_Source_Command is
	        -- @Source{Name=<File Name>,SectionName=<Name>,
		-- SectionNumber=<AlphaNum>,NewSection=[T|F]}
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
			 Starts_New_Section => True);
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
				31 + Character'Pos(Item(1)) - Character'Pos('A');
			elsif ILen = 1 and then Item(1) in 'a'..'z' then
		            Source_Data(Source_Length).Section_Number :=
				31 + Character'Pos(Item(1)) - Character'Pos('a');
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
--Ada.Text_IO.Put_Line("Process command=" & Command_Type'Image(Command));
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
		    Ada.Text_IO.Put_Line("Show Index Entries");

		when Hide_Index_Entries =>
		    -- @HideIndexEntries
		    Display_Index_Entries := False;
		    Ada.Text_IO.Put_Line("Hide Index Entries");

		when Show_Annotations =>
		    -- @ShowAnnotations
                    if Groupings_Set_Up then
		        Ada.Text_IO.Put_Line ("** ShowAnnotations has to appear before " &
                           "Include or Exclude; on line" & ARM_Input.Line_String (Input_Object));                        
                    end if;
                    Include_Annotations := True;
		    Ada.Text_IO.Put_Line("Show Annotations");

		when Hide_Annotations =>
		    -- @HideAnnotations
                    if Groupings_Set_Up then
		        Ada.Text_IO.Put_Line ("** HideAnnotations has to appear before " &
                           "Include or Exclude; on line" & ARM_Input.Line_String (Input_Object));                        
                    end if;
		    Include_Annotations := False;
		    Ada.Text_IO.Put_Line("Hide Annotations");

		when Show_ISO =>
		    -- @ShowISO
                    if Groupings_Set_Up then
		        Ada.Text_IO.Put_Line ("** ShowISO has to appear before " &
                           "Include or Exclude; on line" & ARM_Input.Line_String (Input_Object));                        
                    end if;
		    Include_ISO_Text := True;
		    Ada.Text_IO.Put_Line("Show ISO Text");

		when Hide_ISO =>
		    -- @HideISO
                    if Groupings_Set_Up then
		        Ada.Text_IO.Put_Line ("** HideISO has to appear before " &
                           "Include or Exclude; on line" & ARM_Input.Line_String (Input_Object));                        
                    end if;
		    Include_ISO_Text := False;
		    Ada.Text_IO.Put_Line("Hide ISO Text");

		when Link_Non_Terminals =>
		    -- @LinkNonTerminals
		    Should_Link_Non_Terminals := True;
		    Ada.Text_IO.Put_Line("Link Non-Terminals");

		when Number_Paragraphs =>
		    -- @NumberParagraphs
		    Should_Number_Paragraphs := True;
		    Ada.Text_IO.Put_Line("Number Paragraphs");

		when Title =>
		    -- @Title{Version=[<version>],Text=[<title_text>]}
		    Process_Versioned_String (Document_Title);

		when File_Prefix =>
		    -- @FilePrefix{<File_Prefix>}
		    Output_File_Prefix := +Get_Single_String;
		    Ada.Text_IO.Put_Line("File Prefix is " &
			(+Output_File_Prefix));

		when Example_Font =>
		    -- @ExampleFont{Swiss|Fixed|Roman}
		    declare
			Font_Name : constant String :=
			    Ada.Characters.Handling.To_Lower (
				Get_Single_String);
		    begin
			if Font_Name = "swiss" then
			    Font_of_Examples := ARM_Output.Swiss;
			    Ada.Text_IO.Put_Line("Examples in swiss font");
			elsif Font_Name = "fixed" then
			    Font_of_Examples := ARM_Output.Fixed;
			    Ada.Text_IO.Put_Line("Examples in fixed-width font");
			elsif Font_Name = "roman" then
			    Font_of_Examples := ARM_Output.Roman;
			    Ada.Text_IO.Put_Line("Examples in roman font");
			else
		            Ada.Text_IO.Put_Line ("** Unknown example font name: " & Font_Name &
						  " on line" & ARM_Input.Line_String (Input_Object));
			end if;
		    end;

		when Example_Comment_Font =>
		    -- @ExampleCommentFont{Swiss|Fixed|Roman}
		    declare
			Font_Name : constant String :=
			    Ada.Characters.Handling.To_Lower (
				Get_Single_String);
		    begin
			if Font_Name = "swiss" then
			    Font_of_Example_Comments := ARM_Output.Swiss;
			    Ada.Text_IO.Put_Line("Example comments in swiss font");
			elsif Font_Name = "fixed" then
			    Font_of_Example_Comments := ARM_Output.Fixed;
			    Ada.Text_IO.Put_Line("Example comments in fixed-width font");
			elsif Font_Name = "roman" then
			    Font_of_Example_Comments := ARM_Output.Roman;
			    Ada.Text_IO.Put_Line("Example comments in roman font");
			else
		            Ada.Text_IO.Put_Line ("** Unknown example font name: " & Font_Name &
						  " on line" & ARM_Input.Line_String (Input_Object));
			end if;
		    end;

		when Body_Font =>
		    -- @BodyFont{Swiss|Roman}
		    declare
			Font_Name : constant String :=
			    Ada.Characters.Handling.To_Lower (
				Get_Single_String);
		    begin
			if Font_Name = "swiss" then
			    Font_of_Body := ARM_Output.Swiss;
			    Ada.Text_IO.Put_Line("Body in swiss font");
			elsif Font_Name = "roman" then
			    Font_of_Body := ARM_Output.Roman;
			    Ada.Text_IO.Put_Line("Body in roman font");
			else
		            Ada.Text_IO.Put_Line ("** Unknown body font name: " & Font_Name &
						  " on line" & ARM_Input.Line_String (Input_Object));
			end if;
		    end;

		when Note_Format =>
		    -- @NoteFormat{Ada95|ISO2004}
		    declare
			Format_Name : constant String :=
			    Ada.Characters.Handling.To_Lower (
				Get_Single_String);
		    begin
			if Format_Name = "ada95" then
			    Use_ISO_2004_Note_Format := False;
			    Ada.Text_IO.Put_Line("Notes in Ada 95 standard format");
			elsif Format_Name = "iso2004" then
			    Use_ISO_2004_Note_Format := True;
			    Ada.Text_IO.Put_Line("Notes in ISO 2004 standard format");
			else
		            Ada.Text_IO.Put_Line ("** Unknown note format name: " & Format_Name &
						  " on line" & ARM_Input.Line_String (Input_Object));
			end if;
		    end;

		when Contents_Format =>
		    -- @ContentsFormat{Ada95|ISO2004}
		    declare
			Format_Name : constant String :=
			    Ada.Characters.Handling.To_Lower (
				Get_Single_String);
		    begin
			if Format_Name = "ada95" then
			    Use_ISO_2004_Contents_Format := False;
			    Ada.Text_IO.Put_Line("Table of Contents in Ada 95 standard format");
			elsif Format_Name = "iso2004" then
			    Use_ISO_2004_Contents_Format := True;
			    Ada.Text_IO.Put_Line("Table of Contents in ISO 2004 standard format");
			else
		            Ada.Text_IO.Put_Line ("** Unknown contents format name: " & Format_Name &
						  " on line" & ARM_Input.Line_String (Input_Object));
			end if;
		    end;

		when List_Format =>
		    -- @ListFormat{Ada95|ISO2004}
		    declare
			Format_Name : constant String :=
			    Ada.Characters.Handling.To_Lower (
				Get_Single_String);
		    begin
			if Format_Name = "ada95" then
			    Use_ISO_2004_List_Format := False;
			    Ada.Text_IO.Put_Line("Lists in Ada 95 standard format");
			elsif Format_Name = "iso2004" then
			    Use_ISO_2004_List_Format := True;
			    Ada.Text_IO.Put_Line("Lists in ISO 2004 standard format");
			else
		            Ada.Text_IO.Put_Line ("** Unknown list format name: " & Format_Name &
						  " on line" & ARM_Input.Line_String (Input_Object));
			end if;
		    end;

		when Self_Ref_Format =>
		    -- @SelfRefFormat{RM|ISO1989|ISO2018}
		    declare
			Format_Name : constant String :=
			    Ada.Characters.Handling.To_Lower (
				Get_Single_String);
		    begin
			if Format_Name = "rm" then
			    Self_Reference_Format := ARM_Format.RM;
			    Ada.Text_IO.Put_Line("Self References in Reference Manual format");
			elsif Format_Name = "iso1989" then
			    Self_Reference_Format := ARM_Format.ISO_1989;
			    Ada.Text_IO.Put_Line("Self References in ISO 1989 format");
			elsif Format_Name = "iso2018" then
			    Self_Reference_Format := ARM_Format.ISO_2018;
			    Ada.Text_IO.Put_Line("Self References in ISO 2018 format");
			else
		            Ada.Text_IO.Put_Line ("** Unknown self reference format name: " & Format_Name &
						  " on line" & ARM_Input.Line_String (Input_Object));
			end if;
		    end;

		when Subdivision_Names =>
		    -- @SubdivisionNames{Chapter|Section|Clause}
		    declare
			SD_Name : constant String :=
			    Ada.Characters.Handling.To_Lower (
				Get_Single_String);
		    begin
			if SD_Name = "chapter" then
			    Subdivision_Name_Kind := ARM_Output.Chapter;
			    Ada.Text_IO.Put_Line("Top-level subdivisions known as Chapters");
			elsif SD_Name = "section" then
			    Subdivision_Name_Kind := ARM_Output.Section;
			    Ada.Text_IO.Put_Line("Top-level subdivisions known as Sections");
			elsif SD_Name = "clause" then
			    Subdivision_Name_Kind := ARM_Output.Clause;
			    Ada.Text_IO.Put_Line("Top-level subdivisions known as Clauses");
			else
		            Ada.Text_IO.Put_Line ("** Unknown subdivision name: " & SD_Name &
						  " on line" & ARM_Input.Line_String (Input_Object));
			end if;
		    end;
                    
                    
                when Include_Groups =>
                    if Groupings_Set_Up then
		        Ada.Text_IO.Put_Line ("** Include cannot appear with Exclude or another " &
                           "Include; on line" & ARM_Input.Line_String (Input_Object));                        
                    end if;
                    Groupings_Set_Up := True;
                    Included_Groupings := (others => False); -- Nothing included here.
                    
                    -- Read list of groups.
                    Get_Open_Char;
                    declare
                        Name : ARM_Input.Command_Name_Type;
                        Kind : ARM_Paragraph.Paragraph_Type;
                        Sep_Ch : Character;
                        use ARM_Paragraph; -- For enumeration literals; "use all type ARM_Paragraph.Paragraph_Type;"
                    begin
                        loop
                            -- Skip any leading blanks or line breaks (these are
                            -- common in long lists):
                            loop
                                ARM_Input.Get_Char (Input_Object, Sep_Ch);
                                if Sep_Ch = ' ' then
                                    null;
                                elsif Sep_Ch = Ascii.LF then
                                    null;
                                else
                                    ARM_Input.Replace_Char (Input_Object);
                                    exit;
                                end if;
                            end loop;                            
                            ARM_Input.Get_Name (Input_Object, Name);
                            Kind := ARM_Paragraph.Get_Paragraph_Kind (Name);
                            if Kind in ARM_Paragraph.Format_Kinds then
                                Ada.Text_IO.Put_Line ("** Formatting kind " & Kind'Image &
                                   " not allowed in Include; on line" & ARM_Input.Line_String (Input_Object));                        
                            elsif Kind = Unknown then
                                Ada.Text_IO.Put_Line ("** Unknown kind " & Name &
                                   " in Include; on line" & ARM_Input.Line_String (Input_Object));
                            else
                                Included_Groupings(Kind) := True; -- Include this group.
                            end if;
                            ARM_Input.Get_Char (Input_Object, Sep_Ch);
                            if Sep_Ch = ',' then
                                null; -- Skip a separator.
                            elsif Sep_Ch = Close_Ch then
                                -- Reached the end of the list.
                                exit;
                            elsif Sep_Ch = Ascii.SUB then
                                Ada.Text_IO.Put_Line ("** Unexpected end of file in group list " &
                                   " in Include; on line" & ARM_Input.Line_String (Input_Object));
                                exit;
                            else
                                ARM_Input.Replace_Char (Input_Object);
                                Ada.Text_IO.Put_Line ("** Unusual character '" & Sep_Ch & "' in group list " &
                                   " in Include; on line" & ARM_Input.Line_String (Input_Object));
                                exit;
                            end if; 
                        end loop;
                    end;
                    
                when Exclude_Groups =>
                    if Groupings_Set_Up then
		        Ada.Text_IO.Put_Line ("** Exclude cannot appear with Include or another " &
                           "Exclude; on line" & ARM_Input.Line_String (Input_Object));                        
                    end if;
                    Groupings_Set_Up := True;
                    Included_Groupings (ARM_Paragraph.RM_Groupings) :=
                        (ARM_Paragraph.RM_Groupings => True);
                    if Include_Annotations then
                        Included_Groupings (ARM_Paragraph.AARM_Groupings) :=
                             (ARM_Paragraph.AARM_Groupings => True);
                        Included_Groupings (ARM_Paragraph.AARM_Annotations) :=
                              (ARM_Paragraph.AARM_Annotations => True);
                        Included_Groupings (ARM_Paragraph.RM_Only) := False;
                        Included_Groupings (ARM_Paragraph.AARM_Only) := True;
                    else
                        Included_Groupings (ARM_Paragraph.AARM_Groupings) :=
                             (ARM_Paragraph.AARM_Groupings => False);
                        Included_Groupings (ARM_Paragraph.AARM_Annotations) :=
                             (ARM_Paragraph.AARM_Annotations => False);
                        Included_Groupings (ARM_Paragraph.AARM_Only) := False;
                        Included_Groupings (ARM_Paragraph.RM_Only) := True;           
                    end if;
                    if Include_ISO_Text then
                        Included_Groupings (ARM_Paragraph.Not_ISO) := False;
                        Included_Groupings (ARM_Paragraph.ISO_Only) := True;
                    else
                        Included_Groupings (ARM_Paragraph.Not_ISO) := True;
                        Included_Groupings (ARM_Paragraph.ISO_Only) := False;
                    end if;
                
                    -- Read list of groups.
                    Get_Open_Char;
                    declare
                        Name : ARM_Input.Command_Name_Type;
                        Kind : ARM_Paragraph.Paragraph_Type;
                        Sep_Ch : Character;
                        use ARM_Paragraph; -- For enumeration literals; "use all type ARM_Paragraph.Paragraph_Type;"
                    begin
                        loop
                            -- Skip any leading blanks or line breaks (these are
                            -- common in long lists):
                            loop
                                ARM_Input.Get_Char (Input_Object, Sep_Ch);
                                if Sep_Ch = ' ' then
                                    null;
                                elsif Sep_Ch = Ascii.LF then
                                    null;
                                else
                                    ARM_Input.Replace_Char (Input_Object);
                                    exit;
                                end if;
                            end loop;                            
                            ARM_Input.Get_Name (Input_Object, Name);
                            Kind := ARM_Paragraph.Get_Paragraph_Kind (Name);
                            if Kind in ARM_Paragraph.Format_Kinds then
                                Ada.Text_IO.Put_Line ("** Formatting kind " & Kind'Image &
                                   " not allowed in Exclude; on line" & ARM_Input.Line_String (Input_Object));                        
                            elsif Kind = Unknown then
                                Ada.Text_IO.Put_Line ("** Unknown kind " & Name &
                                   " in Exclude; on line" & ARM_Input.Line_String (Input_Object));
                            else
                                Included_Groupings(Kind) := False; -- Exclude this group.
                            end if;
                            ARM_Input.Get_Char (Input_Object, Sep_Ch);
                            if Sep_Ch = ',' then
                                null; -- Skip a separator.
                            elsif Sep_Ch = Close_Ch then
                                -- Reached the end of the list.
                                exit;
                            elsif Sep_Ch = Ascii.SUB then
                                Ada.Text_IO.Put_Line ("** Unexpected end of file in group list " &
                                   " in Exclude; on line" & ARM_Input.Line_String (Input_Object));
                                exit;
                            else
                                ARM_Input.Replace_Char (Input_Object);
                                Ada.Text_IO.Put_Line ("** Unusual character '" & Sep_Ch & "' in group list " &
                                   " in Exclude; on line" & ARM_Input.Line_String (Input_Object));
                                exit;
                            end if; 
                        end loop;
                    end;                   

		-- Text properties:

		when Single_Text_Output_File =>
		    -- @Single_Text_Output_File
		    Use_Large_Text_Files := True;
		    Ada.Text_IO.Put_Line("Single Text Output File");

		-- HTML properties:

		when Single_HTML_Output_File =>
		    -- @Single_HTML_Output_File
		    Use_Large_HTML_Files := True;
		    Ada.Text_IO.Put_Line("Single HTML Output File");

		when Use_MS_DOS_Names =>
		    -- @UseMSDOSFileNames
		    Use_MS_DOS_Filenames := True;
		    Ada.Text_IO.Put_Line("Use MS-DOS (8.3) file names for HTML output files");

		when HTML_Kind_Command =>
		    --@HTMLKind{Version=[3|4Comp|4],Unicode=[T|F]}
		    Process_HTML_Kind;

		when HTML_Nav_Bar =>
		    --@HTMLNavBar{RefName=[<URL>],SrchName=[<URL>],UseButtons=[T|F],OnTop=[T|F],OnBottom=[T|F]}
		    Process_HTML_Nav_Bar;

		when HTML_Tabs =>
		    -- @HTMLTabs{[SingleSpace|QuadSpace|EmulateFixedOnly|EmulateFixedOnlyQuad|EmulateAll]}
		    declare
			Tabs : constant String :=
			    Ada.Characters.Handling.To_Lower (Get_Single_String);
		    begin
			if Tabs = "singlespace" then
			    HTML_Tab_Emulation := ARM_HTML.Single_Space;
			elsif Tabs = "quadspace" then
			    HTML_Tab_Emulation := ARM_HTML.Quad_Space;
			elsif Tabs = "emulatefixedonly" then
			    HTML_Tab_Emulation := ARM_HTML.Emulate_Fixed_Only;
			elsif Tabs = "emulatefixedonlyquad" then
			    HTML_Tab_Emulation := ARM_HTML.Emulate_Fixed_Only_Quad;
			elsif Tabs = "emulateall" then
			    HTML_Tab_Emulation := ARM_HTML.Emulate_All;
			else
		            Ada.Text_IO.Put_Line ("** Unknown tab emulation name: " & Tabs &
						  " on line" & ARM_Input.Line_String (Input_Object));
			end if;
		        Ada.Text_IO.Put_Line("HTML Tab Emulation is " &
			    ARM_HTML.Tab_Emulation_Type'Image(HTML_Tab_Emulation));
		    end;

		when HTML_Script =>
		    --@HTMLHeader{<Script>}
		    HTML_Script_Text := +Get_Single_String;
		    if Ada.Strings.Unbounded.Length (HTML_Script_Text) /= 0 then
			Ada.Text_IO.Put_Line("Non-empty HTML Script seen");
		    end if;

		when HTML_Header =>
		    --@HTMLHeader{<HTML_for_Header>}
		    HTML_Header_Text := +Get_Single_String;
		    if Ada.Strings.Unbounded.Length (HTML_Header_Text) /= 0 then
			Ada.Text_IO.Put_Line("Non-empty HTML Header seen");
		    end if;

		when HTML_Footer =>
		    --@HTMLFooter{<HTML_for_Footer>}
		    HTML_Footer_Text := +Get_Single_String;
		    if Ada.Strings.Unbounded.Length (HTML_Footer_Text) /= 0 then
			Ada.Text_IO.Put_Line("Non-empty HTML Footer seen");
		    end if;

		when HTML_New_Revision_Colors =>
		    -- @HTMLNewRevisionColors
		    HTML_Use_New_Revision_Colors := True;
		    Ada.Text_IO.Put_Line("Unconditionally use new revision colors in HTML");

		when HTML_Color =>
		    --@HTMLColor{Text=[<Color]>,Background=[<Color>],
		    --  Link=[<Color>],VLink=[<Color>],ALink=[<Color>]}
		    Process_HTML_Color;

		-- RTF properties:

		when Single_RTF_Output_File =>
		    -- @Single_RTF_Output_File
		    Use_Large_RTF_Files := True;
		    Ada.Text_IO.Put_Line("Single RTF Output File");

		when RTF_Header_Prefix =>
		    -- @RTFHeaderPrefix{Version=[<version>],Text=[<title_text>]}
		    Process_Versioned_String (Header_Prefix);

		when RTF_Footer_Text =>
		    -- @RTFFooterPrefix{Version=[<version>],Text=[<title_text>]}
		    Process_Versioned_String (Footer_Text);

		when RTF_Footer =>
		    -- @RTFFooter{UseDate=[T|F],UseClauseName=[T|F],UseISOFormat=[T|F]}
		    begin
			Get_Open_Char;
--Ada.Text_IO.Put_Line("Process RTF Footer");
		        Get_Boolean ("UseDate" & (8..ARM_Input.Command_Name_Type'Last => ' '),
				     Footer_Use_Date, Is_First => True);
			if Footer_Use_Date then
			    Ada.Text_IO.Put_Line("RTF footer will include the date");
			else
			    Ada.Text_IO.Put_Line("RTF footer will omit the date");
			end if;

		        Get_Boolean ("UseClauseName" & (14..ARM_Input.Command_Name_Type'Last => ' '),
				     Footer_Use_Clause_Name);
			if Footer_Use_Clause_Name then
			    Ada.Text_IO.Put_Line("RTF footer will include the name of the clause that starts the page");
			else
			    Ada.Text_IO.Put_Line("RTF footer will include the fixed footer text");
			end if;

		        Get_Boolean ("UseISOFormat" & (13..ARM_Input.Command_Name_Type'Last => ' '),
				     Footer_Use_ISO_Format);
			if Footer_Use_ISO_Format then
			    Ada.Text_IO.Put_Line("RTF footer will use the ISO format (swiss font, multiple sizes)");
			else
			    Ada.Text_IO.Put_Line("RTF footer will use the normal format (body font, one size)");
			end if;

		        ARM_Input.Get_Char (Input_Object, Ch);
		        if Ch = Close_Ch then
			    null;
		        else
			    Ada.Text_IO.Put_Line ("** Missing closing character for command on line" & ARM_Input.Line_String (Input_Object));
		            ARM_Input.Replace_Char (Input_Object);
		        end if;
		    end;

		when RTF_Version_Name =>
		    -- @RTFVersionName{Version=[<version>],Text=[<title_text>]}
		    Process_Versioned_String (Version_Name);

		when RTF_Page_Size =>
		    -- @RTFPageSize{Letter|A4|HalfLetter|Ada95}
		    declare
			Size : constant String :=
			    Ada.Characters.Handling.To_Lower (Get_Single_String);
		    begin
			if Size = "letter" then
			    Page_Size := ARM_RTF.Letter;
			elsif Size = "a4" then
			    Page_Size := ARM_RTF.A4;
			elsif Size = "halfletter" then
			    Page_Size := ARM_RTF.Half_Letter;
			elsif Size = "ada95" then
			    Page_Size := ARM_RTF.Ada95;
			else
		            Ada.Text_IO.Put_Line ("** Unknown page size name: " & Size &
						  " on line" & ARM_Input.Line_String (Input_Object));
			end if;
		        Ada.Text_IO.Put_Line("RTF Page Size is " &
			    ARM_RTF.Page_Size'Image(Page_Size));
		    end;

		when RTF_Fonts =>
		    -- @RTFFonts{Serif=[Times|Souvenir]},SansSerif=[Arial|Helvetica]}
		    Process_RTF_Fonts;

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

	-- Check for bad combinations:
	if ARM_Output."="(Font_of_Examples, ARM_Output.Roman) and then
	   ARM_Output."="(Font_of_Body, ARM_Output.Swiss) then
	    -- This doesn't work, since "Roman" examples use the
	    -- body styles, and if they are Swiss...
	    Ada.Text_IO.Put_Line ("  ** Examples cannot be Roman if Body is Swiss!! (They will appear in the Swiss font)");
	end if;
    end Read_Master_File;


    procedure Create_Format (Format_Object : in out ARM_Format.Format_Type) is
	-- Create an appropriate format object.
    begin
        if not Groupings_Set_Up then
            Groupings_Set_Up := True;
            Included_Groupings (ARM_Paragraph.RM_Groupings) :=
                (ARM_Paragraph.RM_Groupings => True);
            if Include_Annotations then
                Included_Groupings (ARM_Paragraph.AARM_Groupings) :=
                     (ARM_Paragraph.AARM_Groupings => True);
                Included_Groupings (ARM_Paragraph.AARM_Annotations) :=
                     (ARM_Paragraph.AARM_Annotations => True);
                Included_Groupings (ARM_Paragraph.RM_Only) := False;
                Included_Groupings (ARM_Paragraph.AARM_Only) := True;
            else
                Included_Groupings (ARM_Paragraph.AARM_Groupings) :=
                     (ARM_Paragraph.AARM_Groupings => False);
                Included_Groupings (ARM_Paragraph.AARM_Annotations) :=
                     (ARM_Paragraph.AARM_Annotations => False);
                Included_Groupings (ARM_Paragraph.AARM_Only) := False;
                Included_Groupings (ARM_Paragraph.RM_Only) := True;           
            end if;
            if Include_ISO_Text then
                Included_Groupings (ARM_Paragraph.Not_ISO) := False;
                Included_Groupings (ARM_Paragraph.ISO_Only) := True;
            else
                Included_Groupings (ARM_Paragraph.Not_ISO) := True;
                Included_Groupings (ARM_Paragraph.ISO_Only) := False;
            end if;
        -- else already done.           
        end if;        
	ARM_Format.Create (Format_Object, Change_Kind, Change_Version,
                Base_Change_Version   => Base_Change_Version,
		Display_Index_Entries => Display_Index_Entries,
		Include_Annotations   => Include_Annotations,
		Include_ISO	      => Include_ISO_Text,
		Link_Non_Terminals    => Should_Link_Non_Terminals,
		Number_Paragraphs     => Should_Number_Paragraphs,
                Include_Group         => Included_Groupings,
		Examples_Font	      => Font_of_Examples,
		Example_Comment_Font  => Font_of_Example_Comments,
		Use_ISO_2004_Note_Format => Use_ISO_2004_Note_Format,
		Use_ISO_2004_Contents_Format => Use_ISO_2004_Contents_Format,
		Use_ISO_2004_List_Format => Use_ISO_2004_List_Format,
                Self_Ref_Format       => Self_Reference_Format,
		Top_Level_Subdivision_Name => Subdivision_Name_Kind);
    end Create_Format;


    procedure Scan_Sources is
	-- Run the scanning pass on the source files:
	Format_Object : ARM_Format.Format_Type;
    begin
	Create_Format (Format_Object);
	for Source_Index in Source_Data'First .. Source_Length loop
	    case Source_Data(Source_Index).Kind is
		when TOC => null;
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
		when A_File =>
		    ARM_Format.Process (Format_Object,
		        Source_Data(Source_Index).File_Name(1..Source_Data(Source_Index).File_Name_Len),
		        Output_Object,
		        Section_Name   => Source_Data(Source_Index).Section_Name(1..Source_Data(Source_Index).Section_Name_Len),
		        Section_Number => Source_Data(Source_Index).Section_Number,
		        Starts_New_Section => Source_Data(Source_Index).Starts_New_Section);
		when Empty => null;
	    end case;
	end loop;

	ARM_Format.Destroy (Format_Object);
    end Generate_Sources;


    procedure Read_and_Process_Master_File (
	File_Name : in String;
	The_Change_Kind : ARM_Format.Change_Kind; -- Changes to generate.
	The_Change_Version : ARM_Contents.Change_Version_Type; -- Change version.
	The_Base_Change_Version : ARM_Contents.Change_Version_Type; -- Base change version.
        Output_Format : in Output_Format_Type;
        Output_Path : in String) is
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

	Change_Kind := The_Change_Kind;
	Change_Version := The_Change_Version;
	Base_Change_Version := The_Base_Change_Version;

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
				     DOS_Filenames => Use_MS_DOS_Filenames,
				     Output_Path => Output_Path,
				     HTML_Kind => HTML_Kind,
				     Use_Unicode => HTML_Use_Unicode,
				     Number_Paragraphs => Should_Number_Paragraphs,
				     Ref_URL => +HTML_Ref_URL,
				     Srch_URL => +HTML_Srch_URL,
				     Index_URL => +HTML_Index_URL,
				     Use_Buttons => HTML_Use_Buttons,
			             Nav_On_Top => HTML_Nav_On_Top,
			             Nav_On_Bottom => HTML_Nav_On_Bottom,
				     Tab_Emulation => HTML_Tab_Emulation,
			             Script_HTML => +HTML_Script_Text,
			             Header_HTML => +HTML_Header_Text,
			             Footer_HTML => +HTML_Footer_Text,
				     Title => Get_Versioned_String(Document_Title,Change_Version),
				     Body_Font => Font_of_Body,
				     Force_New_Revision_Colors => HTML_Use_New_Revision_Colors and then Change_Version >= '5',
				     Text_Color => HTML_Text_Color,
				     Background_Color => HTML_Background_Color,
				     Link_Color => HTML_Link_Color,
				     VLink_Color => HTML_VLink_Color,
				     ALink_Color => HTML_ALink_Color);
		    Generate_Sources (Output);
		    ARM_HTML.Close (Output);
	        end;
	    when RTF =>
	        declare
		    Output : ARM_RTF.RTF_Output_Type;
	        begin
		    if ARM_Format."=" (Change_Kind, ARM_Format.Old_Only) or else
		       Change_Version = '0' then
		        ARM_RTF.Create (Output,
				        Page_Size => Page_Size,
				        Includes_Changes => False,
				        Big_Files => Use_Large_RTF_Files,
				        Output_Path => Output_Path,
				        Primary_Serif_Font => Serif_Font,
				        Primary_Sans_Serif_Font => Sans_Serif_Font,
				        File_Prefix => +Output_File_Prefix,
				        Title => Get_Versioned_String(Document_Title,Change_Version),
				        Header_Prefix => Get_Versioned_String(Header_Prefix,Change_Version),
					Footer_Use_Date => Footer_Use_Date,
					Footer_Use_Clause_Name => Footer_Use_Clause_Name,
					Footer_Use_ISO_Format=> Footer_Use_ISO_Format,
				        Footer_Text => Get_Versioned_String (Footer_Text, Change_Version),
				        Body_Font => Font_of_Body,
					Version_Names => Version_Name);
		    else
		        ARM_RTF.Create (Output,
				        Page_Size => Page_Size,
				        Includes_Changes => True,
				        Big_Files => Use_Large_RTF_Files,
				        Output_Path => Output_Path,
				        Primary_Serif_Font => Serif_Font,
				        Primary_Sans_Serif_Font => Sans_Serif_Font,
				        File_Prefix => +Output_File_Prefix,
				        Title => Get_Versioned_String (Document_Title, Change_Version),
				        Header_Prefix => Get_Versioned_String (Header_Prefix, Change_Version),
					Footer_Use_Date => Footer_Use_Date,
					Footer_Use_Clause_Name => Footer_Use_Clause_Name,
					Footer_Use_ISO_Format=> Footer_Use_ISO_Format,
				        Footer_Text => Get_Versioned_String (Footer_Text, Change_Version),
				        Body_Font => Font_of_Body,
					Version_Names => Version_Name);
		    end if;
		    Generate_Sources (Output);
		    ARM_RTF.Close (Output);
	        end;
	    when Text =>
	        declare
		    Output : ARM_Text.Text_Output_Type;
	        begin
		    ARM_Text.Create (Output,
				     File_Prefix => +Output_File_Prefix,
				     Output_Path => Output_Path,
                                     Big_Files => Use_Large_Text_Files,
				     Title => Get_Versioned_String (Document_Title, Change_Version));
		    Generate_Sources (Output);
		    ARM_Text.Close (Output);
	        end;
	    when Corr =>
	        declare
		    Output : ARM_Corr.Corr_Output_Type;
	        begin
		    ARM_Corr.Create (Output,
				     File_Prefix => +Output_File_Prefix,
				     Output_Path => Output_Path,
				     Title => Get_Versioned_String (Document_Title, Change_Version));
		    Generate_Sources (Output);
		    ARM_Corr.Close (Output);
	        end;
	    when Info =>
	        declare
	            Output : ARM_TexInfo.Texinfo_Output_Type;
	        begin
		    ARM_TexInfo.Create (Output,
				        File_Prefix => +Output_File_Prefix,
				        Output_Path => Output_Path,
				        Title => Get_Versioned_String (Document_Title, Change_Version));
	            Generate_Sources (Output);
	            ARM_TexInfo.Close (Output);
	        end;
            when ReST =>
                -- *** TBD.
		Ada.Text_IO.Put_Line ("** ReST output not yet implemented - sorry.");
	        --declare
	        --    Output : ARM_Rest.Rest_Output_Type;
	        --begin
		--    ARM_TexInfo.Create (Output,
		--		        File_Prefix => +Output_File_Prefix,
		--		        Output_Path => Output_Path,
		--		        Title => Get_Versioned_String (Document_Title, Change_Version));
	        --    Generate_Sources (Output);
	        --    ARM_Rest.Close (Output);
	        --end;
        end case;

    end Read_and_Process_Master_File;


end ARM_Master;
