with ARM_Input, ARM_Database;
package ARM_Paragraph is

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
    --  5/27/22 - RLB - Added Get_Change_Kind here as it needs to be shared.
    --  9/11/23 - RLB - Added Usage category.
    
    type Term_Group_Index is range 0 .. 35;
    
    function Get_Term_Group (Ch : in Character) return Term_Group_Index;
        -- Convert a character representing a term grouping into a Term_Group_Index.
        -- Raises Program_Error if Ch does not represent a term grouping.
   
    function Get_Change_Kind (Name : in String) return ARM_Database.Paragraph_Change_Kind_Type;
        -- Convert the name of a Change Kind into the appropriate enumeration.
        -- Raises Program_Error if Name does not represent a Change Kind.
   
    type Paragraph_Type is (Unknown, -- Use to represent errors, uninitialized, and so on.
        Comment, -- Comments. Nothing should ever be formatted in this.
        Plain,
        Introduction,
	Syntax, Resolution, Legality,
	Static_Semantics, Link_Time, Run_Time, Bounded_Errors,
	Erroneous, Requirements, Documentation, Metrics, Permissions, Advice,
	Usage, Notes, Single_Note, Examples,
	Language_Design, -- AARM-only.
	Ada83_Inconsistencies, Ada83_Incompatibilities, -- AARM-only.
	Ada83_Extensions, Ada83_Wording, -- AARM-only.
	Ada95_Inconsistencies, Ada95_Incompatibilities, -- AARM-only.
	Ada95_Extensions, Ada95_Wording, -- AARM-only.
	Ada2005_Inconsistencies, Ada2005_Incompatibilities, -- AARM-only.
	Ada2005_Extensions, Ada2005_Wording, -- AARM-only.
	Ada2012_Inconsistencies, Ada2012_Incompatibilities, -- AARM-only.
	Ada2012_Extensions, Ada2012_Wording, -- AARM-only.
	Ada2022_Inconsistencies, Ada2022_Incompatibilities, -- AARM-only.
	Ada2022_Extensions, Ada2022_Wording, -- AARM-only.
	-- AARM annotations (no headers)
	Reason, Ramification, Proof, Imp_Note, Corr_Change, Discussion,
	Honest, Glossary_Marker, Term_Marker, Bare_Annotation,
	Element_Ref, Child_Ref, Usage_Note, -- For ASIS (AASIS-only).
        -- Classifications (no format implied):
        RM_Only, AARM_Only, ISO_Only, Not_ISO,
	-- Format only:
	Wide_Above, Example_Text, Child_Example_Text,
	Indented_Example_Text, Code_Indented, Indent, Bulleted, Nested_Bulleted,
        Nested_X2_Bulleted,
	Display, Syntax_Display, Syntax_Indented, Syntax_Production,
	Enumerated, Nested_Enumerated,
        Hanging_Indented_1, Hanging_Indented_2, Hanging_Indented_3,
        Hanging_Indented_4, Small, Title, In_Table);
        
    subtype Ada_Groupings is Paragraph_Type range Introduction .. Ada2022_Wording;
        -- The groupings used for Ada documents (but no annotations or
        -- classifications). Note that these kinds of
        -- paragraphs can never be nested (that is, no @begin for any of these
        -- can occur inside of a @begin for one of them).
    subtype RM_Groupings is Paragraph_Type range Introduction .. Examples;
        -- Grouping that occur in the RM (or similar documents).
    subtype AARM_Groupings is Paragraph_Type range Language_Design .. Ada2022_Wording;
        -- Groupings that occur only in the AARM (or similar documents); AARM
        -- annotations are not included.
    subtype AARM_Annotations is Paragraph_Type range Reason .. Usage_Note;
        -- Annotations that occur only in the AARM (or similar documents).
        -- Both AARM annotations and AASIS annotations are included here; these
        -- use prefixes rather than group headers.
    subtype Classification_Kinds is Paragraph_Type range RM_Only .. Not_ISO;
        -- Classifications, these do not imply formatting or nesting.
    subtype Format_Kinds is Paragraph_Type range Wide_Above .. In_Table;
        -- Paragraph kinds that are purely formatting. (Note that these
        -- cannot be excluded.) "Plain" is an honorary member of this group (test
        -- for it separately).
    
    subtype All_Groupings_and_Classifications is Paragraph_Type range Introduction .. Not_ISO;
        
    type Grouping_Array is array (All_Groupings_and_Classifications) of Boolean;
        -- A map of groupings and classifications; use it for optionality of
        -- groupings and classifications.
        
    function Get_Paragraph_Kind (Name : in ARM_Input.Command_Name_Type)
        return Paragraph_Type;
        -- For the given Name, return the Paragraph_Type. If Name matches
        -- no known paragraph type, return Unknown.     
    
    type LString is record
	Length : Natural;
	Str : String(1..40);
    end record;
    Paragraph_Kind_Name : constant array (Paragraph_Type) of LString :=
	(Unknown	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Comment	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Plain		 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Introduction	 => (Length => 17, Str => "Introductory Text                       "), -- IntroName
	 Language_Design => (Length => 25, Str => "Language Design Principle               "), -- MetaRulesName
	 Syntax		 => (Length => 11, Str => "Syntax Rule                             "), -- SyntaxName
	 Resolution	 => (Length => 20, Str => "Name Resolution Rule                    "), -- ResolutionName
	 Legality	 => (Length => 13, Str => "Legality Rule                           "), -- LegalityName
	 Static_Semantics=> (Length => 20, Str => "Static Semantic Item                    "), -- StaticSemName
	 Link_Time	 => (Length => 21, Str => "Post-Compilation Rule                   "), -- LinkTimeName
	 Run_Time	 => (Length => 21, Str => "Dynamic Semantic Item                   "), -- RunTimeName
	 Bounded_Errors  => (Length => 24, Str => "Bounded (Run-Time) Error                "), -- BoundedName
	 Erroneous	 => (Length => 19, Str => "Erroneous Execution                     "), -- ErronName
	 Requirements	 => (Length => 26, Str => "Implementation Requirement              "), -- ImplReqName
	 Documentation	 => (Length => 25, Str => "Documentation Requirement               "), -- DocReqName
	 Metrics	 => (Length =>  6, Str => "Metric                                  "), -- MetricsName
	 Permissions	 => (Length => 25, Str => "Implementation Permission               "), -- ImplPermName
	 Advice		 => (Length => 21, Str => "Implementation Advice                   "), -- ImplAdviceName
	 Usage 		 => (Length =>  5, Str => "Usage                                   "), -- UsageName
	 Notes		 => (Length =>  4, Str => "Note                                    "), -- NotesName
	 Single_Note	 => (Length =>  4, Str => "Note                                    "), -- SimpleNoteName
	 Examples	 => (Length =>  7, Str => "Example                                 "), -- ExamplesName
	 Ada83_Inconsistencies
			 => (Length => 25, Str => "Inconsistency with Ada 83               "), -- Inconsistent83Name
	 Ada83_Incompatibilities
			 => (Length => 27, Str => "Incompatibility with Ada 83             "), -- Incompatible83Name
	 Ada83_Extensions=> (Length => 19, Str => "Extension to Ada 83                     "), -- Extend83Name
	 Ada83_Wording	 => (Length => 26, Str => "Wording Change from Ada 83              "), -- DiffWord83Name
	 Ada95_Inconsistencies
			 => (Length => 25, Str => "Inconsistency with Ada 95               "), -- Inconsistent95Name
	 Ada95_Incompatibilities
			 => (Length => 27, Str => "Incompatibility with Ada 95             "), -- Incompatible95Name
	 Ada95_Extensions=> (Length => 19, Str => "Extension to Ada 95                     "), -- Extend95Name
	 Ada95_Wording	 => (Length => 26, Str => "Wording Change from Ada 95              "), -- DiffWord95Name
	 Ada2005_Inconsistencies
			 => (Length => 27, Str => "Inconsistency with Ada 2005             "), -- Inconsistent2005Name
	 Ada2005_Incompatibilities
			 => (Length => 29, Str => "Incompatibility with Ada 2005           "), -- Incompatible2005Name
	 Ada2005_Extensions
			 => (Length => 21, Str => "Extension to Ada 2005                   "), -- Extend2005Name
	 Ada2005_Wording => (Length => 28, Str => "Wording Change from Ada 2005            "), -- DiffWord2005Name
	 Ada2012_Inconsistencies
			 => (Length => 27, Str => "Inconsistency with Ada 2012             "), -- Inconsistent2012Name
	 Ada2012_Incompatibilities
			 => (Length => 29, Str => "Incompatibility with Ada 2012           "), -- Incompatible2012Name
	 Ada2012_Extensions
			 => (Length => 21, Str => "Extension to Ada 2012                   "), -- Extend2012Name
	 Ada2012_Wording => (Length => 28, Str => "Wording Change from Ada 2012            "), -- DiffWord2012Name
	 Ada2022_Inconsistencies
			 => (Length => 27, Str => "Inconsistency with Ada 2022             "), -- Inconsistent2022Name
	 Ada2022_Incompatibilities
			 => (Length => 29, Str => "Incompatibility with Ada 2022           "), -- Incompatible2022Name
	 Ada2022_Extensions
			 => (Length => 21, Str => "Extension to Ada 2022                   "), -- Extend2022Name
	 Ada2022_Wording => (Length => 28, Str => "Wording Change from Ada 2022            "), -- DiffWord2022Name
	 Element_Ref	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Child_Ref	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Usage_Note	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Reason		 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Ramification	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Proof		 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Imp_Note	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Corr_Change	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Discussion	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Honest		 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Glossary_Marker => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Term_Marker     => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Bare_Annotation => (Length =>  0, Str => (others => ' ')), -- Not used.
	 RM_Only	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 AARM_Only	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 ISO_Only	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Not_ISO	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Wide_Above	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Example_Text	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Child_Example_Text => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Indented_Example_Text=>(Length =>  0, Str => (others => ' ')), -- Not used.
	 Code_Indented	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Indent		 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Bulleted	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Nested_Bulleted => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Nested_X2_Bulleted=>(Length=>  0, Str => (others => ' ')), -- Not used.
	 Display	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Syntax_Display	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Syntax_Indented => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Syntax_Production=>(Length =>  0, Str => (others => ' ')), -- Not used.
	 Enumerated	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Nested_Enumerated=>(Length =>  0, Str => (others => ' ')), -- Not used.
	 Hanging_Indented_1=>(Length => 0, Str => (others => ' ')), -- Not used.
	 Hanging_Indented_2=>(Length => 0, Str => (others => ' ')), -- Not used.
	 Hanging_Indented_3=>(Length => 0, Str => (others => ' ')), -- Not used.
	 Hanging_Indented_4=>(Length => 0, Str => (others => ' ')), -- Not used.
	 Small		 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Title		 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 In_Table	 => (Length =>  0, Str => (others => ' '))); -- Not used.

    Paragraph_Kind_Title : constant array (Paragraph_Type) of LString :=
	(Unknown	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Comment	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Plain		 => (Length =>  0, Str => (others => ' ')),
	 Introduction	 => (Length =>  0, Str => (others => ' ')), -- IntroTitle (deleted).
	 Language_Design => (Length => 26, Str => "Language Design Principles              "), -- MetaRulesTitle
	 Syntax		 => (Length =>  6, Str => "Syntax                                  "), -- SyntaxTitle
	 Resolution	 => (Length => 21, Str => "Name Resolution Rules                   "), -- ResolutionTitle
	 Legality	 => (Length => 14, Str => "Legality Rules                          "), -- LegalityTitle
	 Static_Semantics=> (Length => 16, Str => "Static Semantics                        "), -- StaticSemTitle
	 Link_Time	 => (Length => 22, Str => "Post-Compilation Rules                  "), -- LinkTimeTitle
	 Run_Time	 => (Length => 17, Str => "Dynamic Semantics                       "), -- RunTimeTitle
	 Bounded_Errors  => (Length => 25, Str => "Bounded (Run-Time) Errors               "), -- BoundedTitle
	 Erroneous	 => (Length => 19, Str => "Erroneous Execution                     "), -- ErronTitle
	 Requirements	 => (Length => 27, Str => "Implementation Requirements             "), -- ImplReqTitle
	 Documentation	 => (Length => 26, Str => "Documentation Requirements              "), -- DocReqTitle
	 Metrics	 => (Length =>  7, Str => "Metrics                                 "), -- MetricsTitle
	 Permissions	 => (Length => 26, Str => "Implementation Permissions              "), -- ImplPermTitle
	 Advice		 => (Length => 21, Str => "Implementation Advice                   "), -- ImplAdviceTitle
	 Usage 		 => (Length =>  5, Str => "Usage                                   "), -- UsageTitle
	 Notes		 => (Length =>  5, Str => "NOTES                                   "), -- NotesTitle
	 Single_Note	 => (Length =>  5, Str => "NOTES                                   "), -- SimpleNoteTitle
	 Examples	 => (Length =>  8, Str => "Examples                                "), -- ExamplesTitle
	 Ada83_Inconsistencies
			 => (Length => 27, Str => "Inconsistencies With Ada 83             "), -- Inconsistent83Title
	 Ada83_Incompatibilities
			 => (Length => 29, Str => "Incompatibilities With Ada 83           "), -- Incompatible83Title
	 Ada83_Extensions=> (Length => 20, Str => "Extensions to Ada 83                    "), -- Extend83Title
	 Ada83_Wording	 => (Length => 27, Str => "Wording Changes from Ada 83             "), -- DiffWord83Title
	 Ada95_Inconsistencies
			 => (Length => 27, Str => "Inconsistencies With Ada 95             "), -- Inconsistent95Title
	 Ada95_Incompatibilities
			 => (Length => 29, Str => "Incompatibilities With Ada 95           "), -- Incompatible95Title
	 Ada95_Extensions=> (Length => 20, Str => "Extensions to Ada 95                    "), -- Extend95Title
	 Ada95_Wording	 => (Length => 27, Str => "Wording Changes from Ada 95             "), -- DiffWord95Title
	 Ada2005_Inconsistencies
			 => (Length => 29, Str => "Inconsistencies With Ada 2005           "), -- Inconsistent2005Title
	 Ada2005_Incompatibilities
			 => (Length => 31, Str => "Incompatibilities With Ada 2005         "), -- Incompatible2005Title
	 Ada2005_Extensions
			 => (Length => 22, Str => "Extensions to Ada 2005                  "), -- Extend2005Title
	 Ada2005_Wording => (Length => 29, Str => "Wording Changes from Ada 2005           "), -- DiffWord2005Title
	 Ada2012_Inconsistencies
			 => (Length => 29, Str => "Inconsistencies With Ada 2012           "), -- Inconsistent2012Title
	 Ada2012_Incompatibilities
			 => (Length => 31, Str => "Incompatibilities With Ada 2012         "), -- Incompatible2012Title
	 Ada2012_Extensions
			 => (Length => 22, Str => "Extensions to Ada 2012                  "), -- Extend2012Title
	 Ada2012_Wording => (Length => 29, Str => "Wording Changes from Ada 2012           "), -- DiffWord2012Title
	 Ada2022_Inconsistencies
			 => (Length => 29, Str => "Inconsistencies With Ada 2022           "), -- Inconsistent2022Title
	 Ada2022_Incompatibilities
			 => (Length => 31, Str => "Incompatibilities With Ada 2022         "), -- Incompatible2022Title
	 Ada2022_Extensions
			 => (Length => 22, Str => "Extensions to Ada 2022                  "), -- Extend2022Title
	 Ada2022_Wording => (Length => 29, Str => "Wording Changes from Ada 2022           "), -- DiffWord2022Title
	 Element_Ref	 => (Length => 19, Str => "Element Reference:                      "), -- Paragraph start.
	 Child_Ref	 => (Length => 28, Str => "Child Elements returned by:             "), -- Paragraph start.
	 Usage_Note	 => (Length => 12, Str => "Usage Note:                             "), -- Paragraph start.
	 Reason		 => (Length =>  8, Str => "Reason:                                 "), -- Paragraph start.
	 Ramification	 => (Length => 14, Str => "Ramification:                           "), -- Paragraph start.
	 Proof		 => (Length =>  7, Str => "Proof:                                  "), -- Paragraph start.
	 Imp_Note	 => (Length => 21, Str => "Implementation Note:                    "), -- Paragraph start.
	 Corr_Change	 => (Length =>  8, Str => "Change:                                 "), -- Paragraph start.
	 Discussion	 => (Length => 12, Str => "Discussion:                             "), -- Paragraph start.
	 Honest		 => (Length => 14, Str => "To be honest:                           "), -- Paragraph start.
	 Glossary_Marker => (Length => 16, Str => "Glossary entry:                         "), -- Paragraph start.
	 Term_Marker     => (Length => 12, Str => "Term entry:                             "), -- Paragraph start.
	 Bare_Annotation => (Length =>  0, Str => (others => ' ')), -- Not used.
	 RM_Only	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 AARM_Only	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 ISO_Only	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Not_ISO	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Wide_Above	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Example_Text	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Child_Example_Text => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Indented_Example_Text=>(Length =>  0, Str => (others => ' ')), -- Not used.
	 Code_Indented	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Indent		 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Bulleted	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Nested_Bulleted => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Nested_X2_Bulleted=>(Length=>  0, Str => (others => ' ')), -- Not used.
	 Display	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Syntax_Display	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Syntax_Indented => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Syntax_Production=>(Length =>  0, Str => (others => ' ')), -- Not used.
	 Enumerated	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Nested_Enumerated=>(Length =>  0, Str => (others => ' ')), -- Not used.
	 Hanging_Indented_1=>(Length => 0, Str => (others => ' ')), -- Not used.
	 Hanging_Indented_2=>(Length => 0, Str => (others => ' ')), -- Not used.
	 Hanging_Indented_3=>(Length => 0, Str => (others => ' ')), -- Not used.
	 Hanging_Indented_4=>(Length => 0, Str => (others => ' ')), -- Not used.
	 Small		 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Title		 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 In_Table	 => (Length =>  0, Str => (others => ' '))); -- Not used.

        
end ARM_Paragraph;


