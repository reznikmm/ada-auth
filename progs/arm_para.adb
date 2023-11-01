with Ada.Characters.Handling,
     Ada.Strings.Fixed;
package body ARM_Paragraph is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package contains the types and subprograms to manage paragraph
    -- kinds.
    --
    -- ---------------------------------------
    -- Copyright 2022, 2023
    --   AXE Consultants. All rights reserved.
    -- 621 N. Sherman Ave., Suite B6, Madison WI  53704
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
    --  4/13/22 - RLB - Created package, moving Paragraph_Type from ARM_Frm,
    --                  reorganizing it, and adding subprograms to convert
    --                  between it and names. Also added Ada 2022 paragraphs.
    --  5/25/22 - RLB - Added Term_Marker.
    --  5/26/22 - RLB - Put Term_Group_Index here as it isn't big enough to
    --                  justify its own pacakge.
    --  9/11/23 - RLB - Added Usage category.
    
    function Get_Term_Group (Ch : in Character) return Term_Group_Index is
        -- Convert a character representing a term grouping into a Term_Group_Index.
        -- Raises Program_Error if Ch does not represent a term grouping.
    begin
        if Ch in '0' .. '9' then
            return Term_Group_Index (Character'Pos(Ch) - Character'Pos('0'));
        elsif Ch in 'A' .. 'Z' then
            return Term_Group_Index (Character'Pos(Ch) - Character'Pos('A') + 10);
        elsif Ch in 'a' .. 'z' then
            return Term_Group_Index (Character'Pos(Ch) - Character'Pos('a') + 10);
        else
            raise Program_Error with "Unknown term group - " & Ch;
        end if;
    end Get_Term_Group;            
   

    function Get_Change_Kind (Name : in String) return ARM_Database.Paragraph_Change_Kind_Type is
        -- Convert the name of a Change Kind into the appropriate enumeration.
        -- Raises Program_Error if Name does not represent a Change Kind.
        Clean_Name : constant String :=
            Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Name, Ada.Strings.Right));
    begin
        if Clean_Name = "revised" then
            return ARM_Database.Revised;
        elsif Clean_Name = "revisedadded" then
            return ARM_Database.Revised_Inserted_Number;
        elsif Clean_Name = "added" then
            return ARM_Database.Inserted;
        elsif Clean_Name = "addednormal" then
            return ARM_Database.Inserted_Normal_Number;
        elsif Clean_Name = "deleted" then
            return ARM_Database.Deleted;
        elsif Clean_Name = "deletedadded" then
            return ARM_Database.Deleted_Inserted_Number;
        elsif Clean_Name = "deletednodelmsg" then
            return ARM_Database.Deleted_No_Delete_Message;
        elsif Clean_Name = "deletedaddednodelmsg" then
            return ARM_Database.Deleted_Inserted_Number_No_Delete_Message;
        else
            raise Program_Error with "  ** Bad kind for change kind: " &
                    Ada.Strings.Fixed.Trim (Name, Ada.Strings.Right);
        end if;   
    end Get_Change_Kind;
   
   
    function Get_Paragraph_Kind (Name : in ARM_Input.Command_Name_Type)
        return Paragraph_Type is
        -- For the given name, return the Paragraph_Type. If Name matches
        -- no known paragraph type, return Unknown.
        Lower_Name : constant String :=
              Ada.Characters.Handling.To_Lower (
                  Ada.Strings.Fixed.Trim (Name, Ada.Strings.Right));
    begin
        if Lower_Name = "comment" then
            return Comment;
        elsif Lower_Name = "wideabove" then
	    return Wide_Above;
        elsif Lower_Name = "example" then
	    return Example_Text;
        elsif Lower_Name = "childexample" then
	    return Child_Example_Text;
        elsif Lower_Name = "descexample" then
            return Indented_Example_Text;
        elsif Lower_Name = "describecode" then
	    return Code_Indented;
        elsif Lower_Name = "indent" then
	    return Indent;
        elsif Lower_Name = "itemize" then
	    return Bulleted;
        elsif Lower_Name = "inneritemize" then
	    return Nested_Bulleted;
        elsif Lower_Name = "innerinneritemize" then
	    return Nested_X2_Bulleted;
        elsif Lower_Name = "display" then
	    return Display;
        elsif Lower_Name = "syntaxdisplay" then
            return Syntax_Display;
        elsif Lower_Name = "syntaxtext" then
	    return Syntax_Indented;
        elsif Lower_Name = "description" then
            return Hanging_Indented_3;
        elsif Lower_Name = "small" then
            return Small;
        elsif Lower_Name = "enumerate" then
            return Enumerated;
        elsif Lower_Name = "innerenumerate" then
            return Nested_Enumerated;
        elsif Lower_Name = "hang1list" then
            return Hanging_Indented_1;
        elsif Lower_Name = "hang2list" then
	    return Hanging_Indented_2;
        elsif Lower_Name = "hang3list" then
	    return Hanging_Indented_3;
        elsif Lower_Name = "hang4list" then
	    return Hanging_Indented_4;
        elsif Lower_Name = "title" then
	    return Title;

        -- RM Groupings:
        elsif Lower_Name = "intro" then
	    return Introduction;
        elsif Lower_Name = "syntax" then
	    return Syntax;
        elsif Lower_Name = "resolution" then
	    return Resolution;
        elsif Lower_Name = "legality" then
	    return Legality;
        elsif Lower_Name = "staticsem" then
	    return Static_Semantics;
        elsif Lower_Name = "linktime" then
	    return Link_Time;
        elsif Lower_Name = "runtime" then
	    return Run_Time;      
        elsif Lower_Name = "bounded" then
	    return Bounded_Errors;
        elsif Lower_Name = "erron" then
	    return Erroneous;
        elsif Lower_Name = "implreq" then
	    return Requirements;
        elsif Lower_Name = "docreq" then
	    return Documentation;
        elsif Lower_Name = "metrics" then
	    return Metrics;
        elsif Lower_Name = "implperm" then
	    return Permissions;
        elsif Lower_Name = "impladvice" then
	    return Advice;
        elsif Lower_Name = "examples" then
	    return Examples;
        elsif Lower_Name = "usage" then
	    return Usage;
        elsif Lower_Name = "notes" then
	    return Notes;
        elsif Lower_Name = "singlenote" then
	    return Single_Note;

        -- AARM groupings:
        elsif Lower_Name = "metarules" then
	    return Language_Design;
        elsif Lower_Name = "inconsistent83" then
	    return Ada83_Inconsistencies;
        elsif Lower_Name = "incompatible83" then
	    return Ada83_Incompatibilities;
        elsif Lower_Name = "extend83" then
	    return Ada83_Extensions;
        elsif Lower_Name = "diffword83" then
	    return Ada83_Wording;
        elsif Lower_Name = "inconsistent95" then
	    return Ada95_Inconsistencies;
        elsif Lower_Name = "incompatible95" then
	    return Ada95_Incompatibilities;
        elsif Lower_Name = "extend95" then
	    return Ada95_Extensions;
        elsif Lower_Name = "diffword95" then
	    return Ada95_Wording;
        elsif Lower_Name = "inconsistent2005" then
	    return Ada2005_Inconsistencies;
        elsif Lower_Name = "incompatible2005" then
	    return Ada2005_Incompatibilities;
        elsif Lower_Name = "extend2005" then
	    return Ada2005_Extensions;
        elsif Lower_Name = "diffword2005" then
	    return Ada2005_Wording;
        elsif Lower_Name = "inconsistent2012" then
	    return Ada2012_Inconsistencies;
        elsif Lower_Name = "incompatible2012" then
	    return Ada2012_Incompatibilities;
        elsif Lower_Name = "extend2012" then
	    return Ada2012_Extensions;
        elsif Lower_Name = "diffword2012" then
	    return Ada2012_Wording;
        elsif Lower_Name = "inconsistent2022" then
	    return Ada2022_Inconsistencies;
        elsif Lower_Name = "incompatible2022" then
	    return Ada2022_Incompatibilities;
        elsif Lower_Name = "extend2022" then
	    return Ada2022_Extensions;
        elsif Lower_Name = "diffword2022" then
	    return Ada2022_Wording;

	-- AARM annotations:
        elsif Lower_Name = "discussion" then
	    return Discussion;
        elsif Lower_Name = "reason" then
	    return Reason;
        elsif Lower_Name = "ramification" then
	    return Ramification;
        elsif Lower_Name = "theproof" then
	    return Proof;
        elsif Lower_Name = "implnote" then
	    return Imp_Note;
        elsif Lower_Name = "honest" then
	    return Honest;
        elsif Lower_Name = "glossarymarker" then
	    return Glossary_Marker;
        elsif Lower_Name = "termmarker" then
	    return Term_Marker;

        -- (A)ASIS groupings:
        elsif Lower_Name = "elementref" then
	    return Element_Ref;
        elsif Lower_Name = "childref" then
	    return Child_Ref;
        elsif Lower_Name = "usagenote" then
	    return Usage_Note;
        
        -- Classification kinds:
        elsif Lower_Name = "rmonly" then
	    return RM_Only;
        elsif Lower_Name = "aarmonly" then
	    return AARM_Only;
        elsif Lower_Name = "notiso" then
	    return Not_ISO;
        elsif Lower_Name = "isoonly" then
	    return ISO_Only; 

        else
            -- Dunno what this is, return "Unknown".
            return Unknown;
        end if;
    end Get_Paragraph_Kind;

end ARM_Paragraph;
