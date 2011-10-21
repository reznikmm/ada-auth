private package ARM_Format.Data is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package contains various data used by the input file parser.
    --
    -- ---------------------------------------
    -- Copyright 2011  AXE Consultants. All rights reserved.
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
    --  8/ 8/11 - RLB - Split from base package, mainly to reduce the
    --			size of that package.
    --		- RLB - Added aspect index commands.
    -- 10/18/11 - RLB - Changed to GPLv3 license.
    -- 10/19/11 - RLB - Added AspectDefn command.
    -- 10/20/11 - RLB - Added DeletedPragmaSyn command.


    type LString is record
	Length : Natural;
	Str : String(1..40);
    end record;
    Paragraph_Kind_Name : constant array (Paragraph_Type) of LString :=
	(Plain		 => (Length =>  0, Str => (others => ' ')), -- Not used.
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
	 Bare_Annotation => (Length =>  0, Str => (others => ' ')), -- Not used.
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
	 Hanging_Indented=> (Length =>  0, Str => (others => ' ')), -- Not used.
	 Title		 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 In_Table	 => (Length =>  0, Str => (others => ' '))); -- Not used.

    Paragraph_Kind_Title : constant array (Paragraph_Type) of LString :=
	(Plain		 => (Length =>  0, Str => (others => ' ')),
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
	 Bare_Annotation => (Length =>  0, Str => (others => ' ')), -- Not used.
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
	 Hanging_Indented=> (Length =>  0, Str => (others => ' ')), -- Not used.
	 Title		 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 In_Table	 => (Length =>  0, Str => (others => ' '))); -- Not used.


    type Command_Type is (
	-- Paragraphs:
	Text_Begin, Text_End, Redundant, Comment, Part, New_Page, Soft_Page,
	New_Column, RM_New_Page,
	-- Basic text formatting:
	Bold, Italic, Roman, Swiss, Fixed, Roman_Italic, Shrink, Grow,
	Black, Red, Green, Blue,
	Keyword, Non_Terminal, Non_Terminal_Format,
	Example_Text, Example_Comment,
	No_Prefix, No_Para_Num, Keep_with_Next,
        Leading, Trailing, Up, Down, Thin_Line, Thick_Line, Tab_Clear, Tab_Set,
	-- Tables:
	Table, Table_Param_Caption, Table_Param_Header, Table_Param_Body, Table_Last,
	-- Pictures:
	Picture_Alone, Picture_Inline,
        -- Indexing:
	Index_List,
	Defn, RootDefn, PDefn, Defn2, RootDefn2, PDefn2, Index_See,
	Index_See_Also, See_Other, See_Also,
	Index_Root_Unit, Index_Child_Unit, Index_Subprogram_Child_Unit,
	Index_Type, Index_Subtype, Index_Subprogram,
	Index_Exception, Index_Object, Index_Package,
	Index_Other, Index_Check, Index_Attr, Index_Pragma, Index_Aspect,
	-- Clause labels:
	Labeled_Section, Labeled_Section_No_Break, Labeled_Clause,
	Labeled_Subclause, Labeled_Subsubclause,
	Labeled_Revised_Section, Labeled_Revised_Clause, Labeled_Revised_Subclause, Labeled_Revised_Subsubclause,
        Labeled_Added_Section, Labeled_Added_Clause, Labeled_Added_Subclause, Labeled_Added_Subsubclause,
        Labeled_Deleted_Clause, Labeled_Deleted_Subclause, Labeled_Deleted_Subsubclause,
	Preface_Section,
	Labeled_Annex, Labeled_Revised_Annex, Labeled_Added_Annex,
	Labeled_Informative_Annex, Labeled_Revised_Informative_Annex,
        Labeled_Added_Informative_Annex,
	Labeled_Normative_Annex, Labeled_Revised_Normative_Annex,
        Labeled_Added_Normative_Annex,
	Unnumbered_Section, Subheading, Heading, Center, Right,
        Added_Subheading,
	-- Clause references:
	Ref_Section, Ref_Section_Number, Ref_Section_by_Number,
	-- Links:
	Local_Target, Local_Link, URL_Link, AI_Link,
	-- Information:
	Syntax_Rule, Syntax_Rule_RHS, Syntax_Term, Syntax_Term_Undefined,
	Syntax_Prefix, Syntax_Summary, Syntax_Xref,
	Added_Syntax_Rule, Deleted_Syntax_Rule,
	Implementation_Defined,	Implementation_Defined_List,
	To_Glossary, To_Glossary_Also,
	Change_To_Glossary, Change_To_Glossary_Also,
	Glossary_Text_Param, -- This is a parameter of the last four.
	Glossary_List,
        Prefix_Type, Reset_Prefix_Type, Attribute, Attribute_Leading, Attribute_Text_Param, -- The last is a parameter of Attribute.
	Attribute_List,
	Pragma_Syntax, Pragma_List, Added_Pragma_Syntax, Deleted_Pragma_Syntax,
	Package_List, Type_List, Subprogram_List, Exception_List, Object_List,
	-- Corrigendum changes:
	Change, Change_Param_Old, Change_Param_New, -- The latter are the parameters of "Change".
	Change_Reference, Change_Note,
	Change_Added, Change_Added_Param, Change_Deleted, Change_Deleted_Param,
	Change_Implementation_Defined,
	Change_Impdef_Text_Param, -- This is a parameter of the previous.
	Change_Implementation_Advice,
	Change_Impladv_Text_Param, -- This is a parameter of the previous.
	Added_Implementation_Advice_List,
	Change_Documentation_Requirement,
	Change_DocReq_Text_Param, -- This is a parameter of the previous.
	Added_Documentation_Requirements_List,
	Change_Aspect_Description,
	Change_AspectDesc_Text_Param, -- This is a parameter of the previous.
	Added_Aspect_Description_List,
	Change_Attribute, Change_Prefix_Type,
	Change_Prefix_Text_Param, -- This is a parameter of the previous.
	-- Text macros:
	Intro_Name, Syntax_Name, Resolution_Name, Legality_Name, Static_Name,
	Link_Name, Run_Name, Bounded_Name, Erroneous_Name, Req_Name,
	Doc_Name, Metrics_Name, Permission_Name, Advice_Name, Notes_Name,
	Single_Note_Name, Examples_Name, Meta_Name, Inconsistent83_Name,
	Incompatible83_Name, Extend83_Name, Wording83_Name,
	Inconsistent95_Name, Incompatible95_Name, Extend95_Name, Wording95_Name,
	Inconsistent2005_Name, Incompatible2005_Name, Extend2005_Name, Wording2005_Name,
	Syntax_Title, Resolution_Title, Legality_Title, Static_Title,
	Link_Title, Run_Title, Bounded_Title, Erroneous_Title, Req_Title,
	Doc_Title, Metrics_Title, Permission_Title, Advice_Title, Notes_Title,
	Single_Note_Title,
	Examples_Title, Meta_Title, Inconsistent83_Title, Incompatible83_Title,
	Extend83_Title, Wording83_Title, Inconsistent95_Title, Incompatible95_Title,
	Extend95_Title, Wording95_Title, Inconsistent2005_Title, Incompatible2005_Title,
	Extend2005_Title, Wording2005_Title,
	-- Character macros:
	EM_Dash, EN_Dash, LE, LT, GE, GT, NE, PI, Times, PorM, Single_Quote,
	Latin_1, Unicode, Ceiling, Floor, Absolute, Log, Thin_Space,
	Left_Quote, Right_Quote, Left_Double_Quote, Right_Double_Quote,
	Left_Quote_Pair, Right_Quote_Pair, Small_Dotless_I, Capital_Dotted_I,
	Unknown);


    function Command (Name : in ARM_Input.Command_Name_Type)
        return Command_Type;
	-- Return the command value for a particular command name:

end ARM_Format.Data;