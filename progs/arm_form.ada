with Ada.Text_IO,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded,
     Ada.Characters.Handling,
     Ada.Command_Line;
with ARM_Master,
     ARM_Contents,
     ARM_Format;
procedure ARM_Formatter is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This is the main subprogram: format the sources for the
    -- Ada reference manual and other documents
    -- (in a vaguely Scribe-like macro language) into the actual
    -- reference manual files (in various formats).
    --
    -- ---------------------------------------
    -- Copyright 2000, 2002, 2004, 2005, 2006, 2011, 2012
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
    -- 12/05/04 - RLB - Split/added various source files.
    --  6/ 2/05 - RLB - Added Corrigendum output module for comparisons to
    --			Amendment document.
    -- 10/12/05 - RLB - Changed the title to reflect what we learned from ISO.
    -- 10/28/05 - RLB - Added Annex Q.
    --  1/ 5/06 - RLB - Revised to use master files, rather than hard-coded
    --			properties.
    --  1/12/06 - RLB - Removed Document completely.
    -- 10/18/11 - RLB - Changed to GPLv3 license.
    -- 10/19/11 - RLB - Removed junk withs (now in master file handler).
    --  4/ 3/12 - RLB - Removed dead variable.

    -- Standard commands for Ada standards:
    -- For Original (Ada 95) RM:
    --	   Arm_Form RM <Format> No-Changes 0
    -- For Original AARM:
    --	   Arm_Form AARM <Format> No-Changes 0
    -- For RM with Corr:
    --     [With change bars for Word 97/2000:]
    --	   Arm_Form RM RTF New-Changes 1
    --     [Final versions with no changes:]
    --	   Arm_Form RM <Format> New-Only 1
    -- For AARM with Corr:
    --     [HTML; RTF for display]:
    --	      Arm_Form RM <Format> Show-Changes 1
    --     [TXT; RTF for printing]:
    --	      Arm_Form RM <Format> New-Only 1
    -- For RM with Corr and Amd:
    --     [With change bars for Word 97/2000:]
    --	   Arm_Form RM RTF New-Changes 2
    --     [With change ballons for Word XP/2003:]
    --	   Arm_Form RM RTF Show-Changes 2
    --     [Final versions with no changes:]
    --	   Arm_Form RM <Format> New-Only 2
    -- For AARM with Corr and Amd:
    --     [HTML; RTF for display]:
    --	      Arm_Form RM <Format> Changes-Only 2
    --        (for only Amd changes) or
    --	      Arm_Form RM <Format> Show-Changes 2
    --        (for all changes)
    --     [TXT; RTF for printing]:
    --	      Arm_Form RM <Format> New-Only 2


    No_Command_Error : exception;

    Format : ARM_Master.Output_Format_Type; -- Format to generate.
    Master_File : Ada.Strings.Unbounded.Unbounded_String; -- Master file for document to generate.
    Change_Kind : ARM_Format.Change_Kind; -- Changes to generate.
    Change_Version : ARM_Contents.Change_Version_Type; -- Change version.

    procedure Get_Commands is
	-- Process the command line for this program.
    begin
	if Ada.Command_Line.Argument_Count not in 1 .. 4 then
	    Ada.Text_IO.Put_Line ("** Wrong number of arguments");
	    raise No_Command_Error;
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
		if Format_Arg = "rtf" then
		    Format := ARM_Master.RTF;
		elsif Format_Arg = "html" then
		    Format := ARM_Master.HTML;
		elsif Format_Arg = "text" then
		    Format := ARM_Master.Text;
		elsif Format_Arg = "corr" then
		    Format := ARM_Master.Corr;
		elsif Format_Arg = "info" then
		    Format := ARM_Master.Info;
		else
		    Ada.Text_IO.Put_Line ("** Unrecognized format: " & Format_Arg);
		    raise No_Command_Error;
		end if;
	    end;
	else
	    Format := ARM_Master.HTML;
	end if;
	declare
	    use type Ada.Strings.Unbounded.Unbounded_String;
        begin
	    Master_File := Ada.Strings.Unbounded.To_Unbounded_String(
		    Ada.Strings.Fixed.Trim (Ada.Command_Line.Argument(1),
		       Ada.Strings.Right));
	    if Ada.Strings.Unbounded.Index (Master_File, ".") = 0 then
		-- Add extension if it is missing.
		Master_File := Master_File & ".MSM";
	    end if;
        end;
    exception
	when No_Command_Error =>
	    Ada.Text_IO.Put_Line ("  Usage: Arm_Form <Master_File> [<Format>[, <Changes>[, <Version>]]]}");
	    Ada.Text_IO.Put_Line ("     where: <Master_File> is the file name (and optional path) for the master file");
	    Ada.Text_IO.Put_Line ("                        for the document;");
	    Ada.Text_IO.Put_Line ("     where: <Format> = 'Text' (text files),");
	    Ada.Text_IO.Put_Line ("                       'HTML' (HTML files),");
	    Ada.Text_IO.Put_Line ("                       'RTF' (RTF files for Word 97 or later),");
	    Ada.Text_IO.Put_Line ("                       'Corr' (Corrigendum-style command files for comparisons);");
	    Ada.Text_IO.Put_Line ("     where: <Changes> = 'No-Changes' (RM text),");
	    Ada.Text_IO.Put_Line ("                        'New-Only' (Revised RM text only for <Version>),");
	    Ada.Text_IO.Put_Line ("                        'Changes-Only' (Text with changes marked for <Version> only),");
	    Ada.Text_IO.Put_Line ("                        'Show-Changes' (Text with changes marked for <Version> and earlier),");
	    Ada.Text_IO.Put_Line ("                        'New-Changes' (Text with insertions marked for <Version> and earlier);");
	    Ada.Text_IO.Put_Line ("     where: <Version> = a value in 0 .. 9 representing the change version");
	    Ada.Text_IO.Put_Line ("                        0-Original Ada 95 (equivalent to No-Changes)");
	    Ada.Text_IO.Put_Line ("                        1-Technical Corrigendum 1");
	    Ada.Text_IO.Put_Line ("                        2-Amendment 1");
	    raise No_Command_Error;
    end Get_Commands;


begin
    Ada.Text_IO.Put_Line ("Ada Manual formatter");
    Ada.Text_IO.Put_Line ("  Copyright 2000, 2002, 2004, 2005, 2006  AXE Consultants");
    Ada.Text_IO.Put_Line ("  P.O. Box 1512, Madison WI  53701");

    Get_Commands;

Ada.Text_IO.Put_Line ("  Master File = " & Ada.Strings.Unbounded.To_String(Master_File));
Ada.Text_IO.Put_Line ("  Format = " & ARM_Master.Output_Format_Type'Image(Format));
Ada.Text_IO.Put_Line ("  Changes = " & ARM_Format.Change_Kind'Image(Change_Kind));
Ada.Text_IO.Put_Line ("  Version = " & Change_Version);

    ARM_Master.Read_and_Process_Master_File (
		File_Name => Ada.Strings.Unbounded.To_String(Master_File),
		The_Change_Kind => Change_Kind,
		The_Change_Version => Change_Version,
		Output_Format => Format);

    Ada.Text_IO.Put_Line ("Ada document created");
exception
    when No_Command_Error =>
	null; -- Error message displayed by command line processor.
end ARM_Formatter;

