with Ada.Text_IO,
     Ada.Strings.Fixed,
     Ada.Characters.Handling,
     Ada.Command_Line;
with ARM_Format,
     ARM_Output,
     ARM_Text,
     ARM_HTML,
     ARM_RTF,
     ARM_Contents;
procedure ARM_Formatter is

    --
    -- Main program: format the sources for the Ada reference manual
    -- (in a vaguely Scribe-like macro language) into the actual
    -- reference manual files (in various formats).
    --
    -- ---------------------------------------
    -- Copyright 2000, 2002, 2004  AXE Consultants.
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
    --  3/ 9/00 - RLB - Created base program.
    --  4/14/00 - RLB - Created from analysis program.
    --  4/18/00 - RLB - Added scanning pass.
    --  4/19/00 - RLB - Split 03 into two files. Added HTML output object.
    --  4/24/00 - RLB - Added Change_Kind and Display_Index_Entries.
    --  5/15/00 - RLB - Split formatter from input.
    --  5/16/00 - RLB - Added missing Destroy for formatting objects.
    --  5/18/00 - RLB - Added RTF output object.
    --  5/25/00 - RLB - Added the Big-Files option. Added the library separator.
    --  5/28/00 - RLB - Added index.
    --  8/31/00 - RLB - Added the New-Changes option.
    --  7/18/02 - RLB - Changed copyright date.
    --		- RLB - Changed Creates to include title and header.
    --		- RLB - Added Version parameter to command line and formatting
    --			commands.
    --  9/10/04 - RLB - Updated descriptions of standard commands.
    --  9/14/04 - RLB - Moved version to ARM_Contents.

    -- Standard commands:
    -- For Original RM:
    --	   Arm_Form RM <Format> No-Changes 0 Big_Files
    -- For Original AARM:
    --	   Arm_Form AARM <Format> No-Changes 0 Show-Index-Entries
    -- For RM with Corr:
    --	   Arm_Form RM <Format> New-Changes 1 Big_Files
    -- For AARM with Corr:
    --     [HTML; RTF for display]:
    --	      Arm_Form RM <Format> Show-Changes 1 Show-Index-Entries
    --     [TXT; RTF for printing]:
    --	      Arm_Form RM <Format> New-Only 1 Show-Index-Entries
    -- For RM with Corr and Amd:
    --	   Arm_Form RM <Format> New-Changes 2 Big_Files
    -- For AARM with Corr and Amd:
    --     [HTML; RTF for display]:
    --	      Arm_Form RM <Format> Changes-Only 2 Show-Index-Entries
    --        (for only Amd changes) or
    --	      Arm_Form RM <Format> Show-Changes 2 Show-Index-Entries
    --        (for all changes)
    --     [TXT; RTF for printing]:
    --	      Arm_Form RM <Format> New-Only 2 Show-Index-Entries


    type Output_Format_Type is (HTML, RTF, Text, All_Formats);
    No_Command_Error : exception;

    Format : Output_Format_Type; -- Format to generate.
    Document : ARM_Format.Document_Type; -- Document to generate.
    Change_Kind : ARM_Format.Change_Kind; -- Changes to generate.
    Change_Version : ARM_Contents.Change_Version_Type; -- Change version.
    Display_Index_Entries : Boolean; -- Should Index entries be displayed?
    Use_Large_Files : Boolean; -- Use large output files.

    procedure Get_Commands is
	-- Process the command line for this program.
    begin
	if Ada.Command_Line.Argument_Count not in 1 .. 6 then
	    Ada.Text_IO.Put_Line ("** Wrong number of arguments");
	    raise No_Command_Error;
	end if;
	if Ada.Command_Line.Argument_Count >= 5 then
	    Display_Index_Entries := False;
	    Use_Large_Files := False;
	    if Ada.Command_Line.Argument_Count >= 6 then
	        declare
		    Options_Arg : String :=
		         Ada.Characters.Handling.To_Lower (
			    Ada.Strings.Fixed.Trim (Ada.Command_Line.Argument(6),
			    Ada.Strings.Right));
	        begin
		    if Options_Arg = "show-index-entries" then
		        Display_Index_Entries := True;
		    elsif Options_Arg = "big-files" then
		        Use_Large_Files := True;
		    else
		        Ada.Text_IO.Put_Line ("** Unrecognized option: " & Options_Arg);
		        raise No_Command_Error;
		    end if;
	        end;
	    end if;
	    declare
		Options_Arg : String :=
		     Ada.Characters.Handling.To_Lower (
			Ada.Strings.Fixed.Trim (Ada.Command_Line.Argument(5),
			Ada.Strings.Right));
	    begin
		if Options_Arg = "show-index-entries" then
		    Display_Index_Entries := True;
		elsif Options_Arg = "big-files" then
		    Use_Large_Files := True;
		else
		    Ada.Text_IO.Put_Line ("** Unrecognized option: " & Options_Arg);
		    raise No_Command_Error;
		end if;
	    end;
	else
	    Display_Index_Entries := False;
	    Use_Large_Files := False;
	end if;
	if Ada.Command_Line.Argument_Count >= 4 then
	    declare
		Version_Arg : String :=
		     Ada.Characters.Handling.To_Lower (
			Ada.Strings.Fixed.Trim (Ada.Command_Line.Argument(4),
			Ada.Strings.Right));
	    begin
		if Version_Arg'Length = 1 and then
		   Version_Arg(Version_Arg'First) in ARM_Contents.Change_Version_Type then
		    Change_Version := Version_Arg(Version_Arg'First);
		else
		    Ada.Text_IO.Put_Line ("** Unrecognized change version: " & Version_Arg);
		    raise No_Command_Error;
		end if;
	    end;
	else
	    Change_Version := '0';
	end if;
	if Ada.Command_Line.Argument_Count >= 3 then
	    declare
		Changes_Arg : String :=
		     Ada.Characters.Handling.To_Lower (
			Ada.Strings.Fixed.Trim (Ada.Command_Line.Argument(3),
			Ada.Strings.Right));
	    begin
		if Changes_Arg = "no-changes" then
		    Change_Kind := ARM_Format.Old_Only;
		elsif Changes_Arg = "new-only" then
		    Change_Kind := ARM_Format.New_Only;
		elsif Changes_Arg = "changes-only" then
		    Change_Kind := ARM_Format.Changes_Only;
		elsif Changes_Arg = "show-changes" then
		    Change_Kind := ARM_Format.Show_Changes;
		elsif Changes_Arg = "new-changes" then
		    Change_Kind := ARM_Format.New_Changes;
		else
		    Ada.Text_IO.Put_Line ("** Unrecognized changes: " & Changes_Arg);
		    raise No_Command_Error;
		end if;
	    end;
	else
	    Change_Kind := ARM_Format.New_Only;
	end if;
	if Ada.Command_Line.Argument_Count >= 2 then
	    declare
		Format_Arg : String :=
		     Ada.Characters.Handling.To_Lower (
			Ada.Strings.Fixed.Trim (Ada.Command_Line.Argument(2),
			Ada.Strings.Right));
	    begin
		if Format_Arg = "all" then
		    Format := All_Formats;
		elsif Format_Arg = "rtf" then
		    Format := RTF;
		elsif Format_Arg = "html" then
		    Format := HTML;
		elsif Format_Arg = "text" then
		    Format := Text;
		else
		    Ada.Text_IO.Put_Line ("** Unrecognized format: " & Format_Arg);
		    raise No_Command_Error;
		end if;
	    end;
	else
	    Format := All_Formats;
	end if;
        declare
	    Document_Arg : String :=
		 Ada.Characters.Handling.To_Lower (
		    Ada.Strings.Fixed.Trim (Ada.Command_Line.Argument(1),
		    Ada.Strings.Right));
        begin
	    if Document_Arg = "aarm" then
	        Document := ARM_Format.AARM;
	    elsif Document_Arg = "rm" then
	        Document := ARM_Format.RM;
	    elsif Document_Arg = "iso-rm" then
	        Document := ARM_Format.RM_ISO;
	    else
	        Ada.Text_IO.Put_Line ("** Unrecognized document: " & Document_Arg);
	        raise No_Command_Error;
	    end if;
        end;
    exception
	when No_Command_Error =>
	    Ada.Text_IO.Put_Line ("  Usage: Arm_Form <Document> [<Format>, [<Changes>, [<Version>, [<Options>]]]}");
	    Ada.Text_IO.Put_Line ("     where: <Document> = 'RM' (reference manual),");
	    Ada.Text_IO.Put_Line ("                         'ISO-RM' (reference manual for ISO),");
	    Ada.Text_IO.Put_Line ("                         'AARM' (annotated reference manual);");
	    Ada.Text_IO.Put_Line ("     where: <Format> = 'Text' (text files),");
	    Ada.Text_IO.Put_Line ("                       'HTML' (HTML files),");
	    Ada.Text_IO.Put_Line ("                       'RTF' (RTF files for Word 97 or later),");
	    Ada.Text_IO.Put_Line ("                       'All' (Files of all formats);");
	    Ada.Text_IO.Put_Line ("     where: <Changes> = 'No-Changes' (RM text),");
	    Ada.Text_IO.Put_Line ("                        'New-Only' (Revised RM text only for <Version>),");
	    Ada.Text_IO.Put_Line ("                        'Changes-Only' (Text with changes marked for <Version> only),");
	    Ada.Text_IO.Put_Line ("                        'Show-Changes' (Text with changes marked for <Version> and earlier),");
	    Ada.Text_IO.Put_Line ("                        'New-Changes' (Text with insertions marked for <Version> and earlier);");
	    Ada.Text_IO.Put_Line ("     where: <Version> = a value in 0 .. 9 representing the change version");
	    Ada.Text_IO.Put_Line ("                        0-Original Ada 95 (equivalent to No-Changes)");
	    Ada.Text_IO.Put_Line ("                        1-Technical Corrigendum 1");
	    Ada.Text_IO.Put_Line ("                        2-Amendment 1");
	    Ada.Text_IO.Put_Line ("     where: <Options> = 'Show-Index-Entries' (print in document);");
	    Ada.Text_IO.Put_Line ("                        'Big-Files' (large output files).");
	    raise No_Command_Error;
    end Get_Commands;


    procedure Scan is
	-- Scan the document for section/clause headings, building the
	-- table of contents.
	Format : ARM_Format.Format_Type;
    begin
	ARM_Format.Create (Format, Document, Change_Kind, Change_Version,
		Display_Index_Entries);

	ARM_Contents.Initialize; -- Make sure that the contents DB is empty.

	-- Scan each of the files in order:

	ARM_Format.Scan (Format, "Title.MSS", Section_Number => 0,
	    Starts_New_Section => True);

	ARM_Format.Scan (Format, "Front_matter.MSS", Section_Number => 0,
	    Starts_New_Section => False);

	ARM_Format.Scan (Format, "01.MSS", Section_Number => 1,
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "02.MSS", Section_Number => 2,
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "03A.MSS", Section_Number => 3,
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "03B.MSS", Section_Number => 3,
	    Starts_New_Section => False);
	ARM_Format.Scan (Format, "04.MSS", Section_Number => 4,
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "05.MSS", Section_Number => 5,
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "06.MSS", Section_Number => 6,
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "07.MSS", Section_Number => 7,
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "08.MSS", Section_Number => 8,
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "09.MSS", Section_Number => 9,
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "10.MSS", Section_Number => 10,
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "11.MSS", Section_Number => 11,
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "12.MSS", Section_Number => 12,
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "13.MSS", Section_Number => 13,
	    Starts_New_Section => True);

	-- Library separator page:
	ARM_Format.Scan (Format, "LIBRARY.MSS", Section_Number => 20, -- Not a real section number.
	    Starts_New_Section => True);

	-- Normative annexes:
	ARM_Format.Scan (Format, "PRE.MSS", Section_Number => 21, -- represents A
	    Starts_New_Section => True);
	    --	  Note: All of the following named "Pre_" are sections of "Pre".
	    --	  These were include files, which I've eliminated.
	ARM_Format.Scan (Format, "PRE_Standard.MSS",
	    Section_Number => 21,
	    Starts_New_Section => False);
	ARM_Format.Scan (Format, "PRE_Ada.MSS",
	    Section_Number => 21,
	    Starts_New_Section => False);
	ARM_Format.Scan (Format, "PRE_Chars.MSS",
	    Section_Number => 21,
	    Starts_New_Section => False);
	ARM_Format.Scan (Format, "PRE_Strings.MSS",
	    Section_Number => 21,
 	    Starts_New_Section => False);
	ARM_Format.Scan (Format, "PRE_Math.MSS",
	    Section_Number => 21,
	    Starts_New_Section => False);
	ARM_Format.Scan (Format, "Real_Attribs.MSS",
	    Section_Number => 21,
	    Starts_New_Section => False);
	ARM_Format.Scan (Format, "PRE_IO.MSS",
	    Section_Number => 21,
	    Starts_New_Section => False);
	ARM_Format.Scan (Format, "PRE_Cmdln.MSS",
	    Section_Number => 21,
	    Starts_New_Section => False);

	-- Back to "normal" appendixes:
	ARM_Format.Scan (Format, "Interface.MSS",
	    Section_Number => 22, -- represents B
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "SP.MSS",
	    Section_Number => 23, -- represents C
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "RT.MSS",
	    Section_Number => 24, -- represents D
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "DS.MSS",
	    Section_Number => 25, -- represents E
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "InfoSys.MSS",
	    Section_Number => 26, -- represents F
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "Numerics.MSS",
	    Section_Number => 27, -- represents G
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "Safety.MSS",
	    Section_Number => 28, -- represents H
	    Starts_New_Section => True);
	ARM_Format.Scan (Format, "Obsolescent.MSS",
	    Section_Number => 30, -- represents J
	    Starts_New_Section => True);

	-- Informative annexes:
	ARM_Format.Scan (Format, "Attribs.MSS",
	    Section_Number => 31, -- represents K
	    Starts_New_Section => True);
		-- Mostly generated from the sections and annexes.
	ARM_Format.Scan (Format, "Pragmas.MSS",
	    Section_Number => 32, -- represents L
	    Starts_New_Section => True);
		-- Mostly generated from the sections and annexes.
	ARM_Format.Scan (Format, "Impldef.MSS",
	    Section_Number => 33, -- represents M
	    Starts_New_Section => True);
		-- Mostly generated from the sections and annexes.
	ARM_Format.Scan (Format, "Glossary.MSS",
	    Section_Number => 34, -- represents N
	    Starts_New_Section => True);
		-- Mostly generated from the sections and annexes.
	ARM_Format.Scan (Format, "Syntax.MSS",
	    Section_Number => 36, -- represents P
	    Starts_New_Section => True);
		-- Mostly generated from the sections and annexes.

	ARM_Format.Insert_Index (Format);

	ARM_Format.Destroy (Format);
    end Scan;


    procedure Generate (Output_Object : in out ARM_Output.Output_Type'Class) is
	-- Generate a document into Output_Object.
	Format : ARM_Format.Format_Type;
    begin
	ARM_Format.Create (Format, Document, Change_Kind, Change_Version,
		Display_Index_Entries);

	-- This subprogram stands in for Ada.MSS in the original Scribe
	-- sources (which we don't use).

	-- For the RTF output, we had better use the big-files option, so
	-- we get one large file.

	ARM_Format.Process (Format, "Title.MSS", Output_Object,
	    Section_Name => "Ttl", Section_Number => 0,
	    Starts_New_Section => True);

	-- The table of contents goes here in the collating order for the
	-- final document. However, for Word, we let Word generate
	-- it (this lets us use page number references in the ToC).
	ARM_Format.Write_Table_of_Contents (Format, Output_Object);

	ARM_Format.Process (Format, "Front_matter.MSS", Output_Object,
	    Section_Name => "00", Section_Number => 0,
	    Starts_New_Section => True);

	ARM_Format.Process (Format, "01.MSS", Output_Object,
	    Section_Name => "01", Section_Number => 1,
	    Starts_New_Section => True);
	ARM_Format.Process (Format, "02.MSS", Output_Object,
	    Section_Name => "02", Section_Number => 2,
	    Starts_New_Section => True);
	ARM_Format.Process (Format, "03A.MSS", Output_Object,
	    Section_Name => "03", Section_Number => 3,
	    Starts_New_Section => True);
	ARM_Format.Process (Format, "03B.MSS", Output_Object,
	    Section_Name => "03", Section_Number => 3,
	    Starts_New_Section => False);
	ARM_Format.Process (Format, "04.MSS", Output_Object,
	    Section_Name => "04", Section_Number => 4,
	    Starts_New_Section => True);
	ARM_Format.Process (Format, "05.MSS", Output_Object,
	    Section_Name => "05", Section_Number => 5,
	    Starts_New_Section => True);
	ARM_Format.Process (Format, "06.MSS", Output_Object,
	    Section_Name => "06", Section_Number => 6,
	    Starts_New_Section => True);
	ARM_Format.Process (Format, "07.MSS", Output_Object,
	    Section_Name => "07", Section_Number => 7,
	    Starts_New_Section => True);
	ARM_Format.Process (Format, "08.MSS", Output_Object,
	    Section_Name => "08", Section_Number => 8,
	    Starts_New_Section => True);
	ARM_Format.Process (Format, "09.MSS", Output_Object,
	    Section_Name => "09", Section_Number => 9,
	    Starts_New_Section => True);
	ARM_Format.Process (Format, "10.MSS", Output_Object,
	    Section_Name => "10", Section_Number => 10,
	    Starts_New_Section => True);
	ARM_Format.Process (Format, "11.MSS", Output_Object,
	    Section_Name => "11", Section_Number => 11,
	    Starts_New_Section => True);
	ARM_Format.Process (Format, "12.MSS", Output_Object,
	    Section_Name => "12", Section_Number => 12,
	    Starts_New_Section => True);
	ARM_Format.Process (Format, "13.MSS", Output_Object,
	    Section_Name => "13", Section_Number => 13,
	    Starts_New_Section => True);

	-- The "standard libraries" separator page:
	ARM_Format.Process (Format, "LIBRARY.MSS", Output_Object,
	    Section_Name => "Lib", Section_Number => 0, -- Not a real section number.
	    Starts_New_Section => True);

	-- Normative annexes:
	ARM_Format.Process (Format, "PRE.MSS", Output_Object,
	    Section_Name => "A", Section_Number => 21, -- represents A
	    Starts_New_Section => True);
	    --	  Note: All of the following named "Pre_" are sections of "Pre".
	    --	  These were include files, which I've eliminated.
	ARM_Format.Process (Format, "PRE_Standard.MSS", Output_Object,
	    Section_Name => "A", Section_Number => 21,
	    Starts_New_Section => False);
	ARM_Format.Process (Format, "PRE_Ada.MSS", Output_Object,
	    Section_Name => "A", Section_Number => 21,
	    Starts_New_Section => False);
	ARM_Format.Process (Format, "PRE_Chars.MSS", Output_Object,
	    Section_Name => "A", Section_Number => 21,
	    Starts_New_Section => False);
	ARM_Format.Process (Format, "PRE_Strings.MSS", Output_Object,
	    Section_Name => "A", Section_Number => 21,
 	    Starts_New_Section => False);
	ARM_Format.Process (Format, "PRE_Math.MSS", Output_Object,
	    Section_Name => "A", Section_Number => 21,
	    Starts_New_Section => False);
	ARM_Format.Process (Format, "Real_Attribs.MSS", Output_Object,
	    Section_Name => "A", Section_Number => 21,
	    Starts_New_Section => False);
	ARM_Format.Process (Format, "PRE_IO.MSS", Output_Object,
	    Section_Name => "A", Section_Number => 21,
	    Starts_New_Section => False);
	ARM_Format.Process (Format, "PRE_Cmdln.MSS", Output_Object,
	    Section_Name => "A", Section_Number => 21,
	    Starts_New_Section => False);

	-- Back to "normal" appendixes:
	ARM_Format.Process (Format, "Interface.MSS", Output_Object,
	    Section_Name => "B", Section_Number => 22, -- represents B
	    Starts_New_Section => True);
	ARM_Format.Process (Format, "SP.MSS", Output_Object,
	    Section_Name => "C", Section_Number => 23, -- represents C
	    Starts_New_Section => True);
	ARM_Format.Process (Format, "RT.MSS", Output_Object,
	    Section_Name => "D", Section_Number => 24, -- represents D
	    Starts_New_Section => True);
	ARM_Format.Process (Format, "DS.MSS", Output_Object,
	    Section_Name => "E", Section_Number => 25, -- represents E
	    Starts_New_Section => True);
	ARM_Format.Process (Format, "InfoSys.MSS", Output_Object,
	    Section_Name => "F", Section_Number => 26, -- represents F
	    Starts_New_Section => True);
	ARM_Format.Process (Format, "Numerics.MSS", Output_Object,
	    Section_Name => "G", Section_Number => 27, -- represents G
	    Starts_New_Section => True);
	ARM_Format.Process (Format, "Safety.MSS", Output_Object,
	    Section_Name => "H", Section_Number => 28, -- represents H
	    Starts_New_Section => True);

	-- Do something here so that the next Annex is Annex J;
	-- ISO requires skipping I and O.

	ARM_Format.Process (Format, "Obsolescent.MSS", Output_Object,
	    Section_Name => "J", Section_Number => 30, -- represents J
	    Starts_New_Section => True);

	-- Informative annexes:
	ARM_Format.Process (Format, "Attribs.MSS", Output_Object,
	    Section_Name => "K", Section_Number => 31, -- represents K
	    Starts_New_Section => True);
		-- Mostly generated from the sections and annexes.
	ARM_Format.Process (Format, "Pragmas.MSS", Output_Object,
	    Section_Name => "L", Section_Number => 32, -- represents L
	    Starts_New_Section => True);
		-- Mostly generated from the sections and annexes.
	ARM_Format.Process (Format, "Impldef.MSS", Output_Object,
	    Section_Name => "M", Section_Number => 33, -- represents M
	    Starts_New_Section => True);
		-- Mostly generated from the sections and annexes.
	ARM_Format.Process (Format, "Glossary.MSS", Output_Object,
	    Section_Name => "N", Section_Number => 34, -- represents N
	    Starts_New_Section => True);
		-- Mostly generated from the sections and annexes.

	-- Do something here so that the next Annex is Annex P;
	-- ISO requires skipping I and O.

	ARM_Format.Process (Format, "Syntax.MSS", Output_Object,
	    Section_Name => "P", Section_Number => 36, -- represents P
	    Starts_New_Section => True);
		-- Mostly generated from the sections and annexes.

	-- The index is generated here.
	ARM_Format.Write_Index (Format, Output_Object);

	ARM_Format.Destroy (Format);
    end Generate;


    procedure Create (Output_Object : in out ARM_Output.Output_Type'Class) is
	-- Create an Output_Object for the current parameters.
    begin
	case Document is
	    when ARM_Format.AARM =>
		if ARM_Format."=" (Change_Kind, ARM_Format.Old_Only) or else
		   Change_Version = '0' then
		    ARM_Output.Create (Output_Object,
				       Page_Size => ARM_Output.Letter,
				       Includes_Changes => False,
				       Big_Files => Use_Large_Files,
				       For_ISO => False,
				       File_Prefix => "AA",
				       Title => "Annotated Ada Reference Manual",
				       Header_Prefix => "ISO/IEC 8652:1995(E)");
		elsif Change_Version = '1' then
		    ARM_Output.Create (Output_Object,
				       Page_Size => ARM_Output.Letter,
				       Includes_Changes => True,
				       Big_Files => Use_Large_Files,
				       For_ISO => False,
				       File_Prefix => "AA",
				       Title => "Annotated Ada Reference Manual",
				       Header_Prefix => "ISO/IEC 8652:1995(E) with COR.1:2001");
		else
		    ARM_Output.Create (Output_Object,
				       Page_Size => ARM_Output.Letter,
				       Includes_Changes => True,
				       Big_Files => Use_Large_Files,
				       For_ISO => False,
				       File_Prefix => "AA",
				       Title => "Annotated Ada Reference Manual",
				       Header_Prefix => "ISO/IEC 8652:1995(E) with COR.1:2001 and AMD.1:Draft");
		end if;
	    when ARM_Format.RM =>
		if ARM_Format."=" (Change_Kind, ARM_Format.Old_Only) or else
		   Change_Version = '0' then
		    ARM_Output.Create (Output_Object,
				       Page_Size => ARM_Output.Ada95,
				       Includes_Changes => False,
				       Big_Files => Use_Large_Files,
				       For_ISO => False,
				       File_Prefix => "RM",
				       Title => "Ada Reference Manual",
				       Header_Prefix => "ISO/IEC 8652:1995(E)");
		elsif Change_Version = '1' then
		    ARM_Output.Create (Output_Object,
				       Page_Size => ARM_Output.Ada95,
				       Includes_Changes => True,
				       Big_Files => Use_Large_Files,
				       For_ISO => False,
				       File_Prefix => "RM",
				       Title => "Ada Reference Manual",
				       Header_Prefix => "ISO/IEC 8652:1995(E) with COR.1:2001");
		else
		    ARM_Output.Create (Output_Object,
				       Page_Size => ARM_Output.Ada95,
				       Includes_Changes => True,
				       Big_Files => Use_Large_Files,
				       For_ISO => False,
				       File_Prefix => "RM",
				       Title => "Ada Reference Manual",
				       Header_Prefix => "ISO/IEC 8652:1995(E) with COR.1:2001 and AMD.1:Draft");
		end if;
	    when ARM_Format.RM_ISO =>
		if ARM_Format."=" (Change_Kind, ARM_Format.Old_Only) or else
		   Change_Version = '0' then
		    ARM_Output.Create (Output_Object,
				       Page_Size => ARM_Output.A4,
				       Includes_Changes => False,
				       Big_Files => Use_Large_Files,
				       For_ISO => False,
				       File_Prefix => "RM",
				       Title => "Ada Reference Manual",
				       Header_Prefix => "ISO/IEC 8652:1995(E)");
		elsif Change_Version = '1' then
		    ARM_Output.Create (Output_Object,
				       Page_Size => ARM_Output.A4,
				       Includes_Changes => True,
				       Big_Files => Use_Large_Files,
				       For_ISO => False,
				       File_Prefix => "RM",
				       Title => "Ada Reference Manual",
				       Header_Prefix => "ISO/IEC 8652:1995(E) with COR.1:2001");
		else
		    ARM_Output.Create (Output_Object,
				       Page_Size => ARM_Output.A4,
				       Includes_Changes => True,
				       Big_Files => Use_Large_Files,
				       For_ISO => False,
				       File_Prefix => "RM",
				       Title => "Ada Reference Manual",
				       Header_Prefix => "ISO/IEC 8652:1995(E) with COR.1:2001 and AMD.1:Draft");
		end if;
	end case;
    end Create;


begin
    Ada.Text_IO.Put_Line ("ARM 95/0Y formatter");
    Ada.Text_IO.Put_Line ("  Copyright 2000, 2002  AXE Consultants");
    Ada.Text_IO.Put_Line ("  P.O. Box 1512, Madison WI  53701");

    Get_Commands;

Ada.Text_IO.Put_Line ("  Document = " & ARM_Format.Document_Type'Image(Document));
Ada.Text_IO.Put_Line ("  Format = " & Output_Format_Type'Image(Format));
Ada.Text_IO.Put_Line ("  Changes = " & ARM_Format.Change_Kind'Image(Change_Kind));
Ada.Text_IO.Put_Line ("  Version = " & Change_Version);
Ada.Text_IO.Put_Line ("  Display Index Entries = " & Boolean'Image(Display_Index_Entries));

    Scan;

    case Format is
	when HTML =>
	    declare
		Output : ARM_HTML.HTML_Output_Type;
	    begin
		Create (Output);
		Generate (Output);
		ARM_HTML.Close (Output);
	    end;
	when RTF =>
	    declare
		Output : ARM_RTF.RTF_Output_Type;
	    begin
		Create (Output);
		Generate (Output);
		ARM_RTF.Close (Output);
	    end;
	when Text =>
	    declare
		Output : ARM_Text.Text_Output_Type;
	    begin
		Create (Output);
		Generate (Output);
		ARM_Text.Close (Output);
	    end;
	when All_Formats =>
	    declare
		TOutput : ARM_Text.Text_Output_Type;
		ROutput : ARM_RTF.RTF_Output_Type;
		HOutput : ARM_HTML.HTML_Output_Type;
	    begin
		Create (TOutput);
		Generate (TOutput);
		ARM_Text.Close (TOutput);
		Create (ROutput);
		Generate (ROutput);
		ARM_RTF.Close (ROutput);
		Create (HOutput);
		Generate (HOutput);
		ARM_HTML.Close (HOutput);
	    end;

    end case;

    Ada.Text_IO.Put_Line ("ARM 95/0Y document created");
exception
    when No_Command_Error =>
	null; -- Error message displayed by command line processor.
end ARM_Formatter;

