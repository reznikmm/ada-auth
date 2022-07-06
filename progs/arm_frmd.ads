private package ARM_Format.Data is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package contains various data used by the input file parser.
    --
    -- ---------------------------------------
    -- Copyright 2011, 2012, 2021, 2022  AXE Consultants. All rights reserved.
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
    -- 10/26/11 - RLB - Added versioned break commands.
    --  3/27/12 - RLB - Added more versioned break commands.
    -- 10/18/12 - RLB - Added more specific hanging_indent commands.
    -- 12/17/12 - RLB - Added Ada 2012 AARM headings.
    --  3/13/21 - RLB - Added Intl_Standard_Name command.
    --  4/ 8/22 - RLB - Added Virtual_Name command.
    --  4/13/22 - RLB - Moved the Paragraph_Kind_xxx arrays to ARM_Paragraph.
    --  4/18/22 - RLB - Added Ada 2022 AARM headings.
    --  5/ 8/22 - RLB - Added Deleted_Subheading.
    --  5/11/22 - RLB - Added LabeledRevisedSubClauseIsoClause.
    --  5/26/22 - RLB - Added ChgTermDef, AddedTermList, and Subnumber.

    type Command_Type is (
	-- Paragraphs:
	Text_Begin, Text_End, Redundant, Comment, Part, New_Page, Soft_Page,
	New_Column, RM_New_Page, New_Page_for_Version, New_Column_for_Version,
	RM_New_Page_for_Version, Not_Iso_RM_New_Page_for_Version,
	Iso_Only_RM_New_Page_for_Version,
	-- Basic text formatting:
	Bold, Italic, Roman, Swiss, Fixed, Roman_Italic, Shrink, Grow,
	Black, Red, Green, Blue,
	Keyword, Non_Terminal, Non_Terminal_Format,
	Example_Text, Example_Comment, Virtual_Name,
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
	Labeled_Revised_Section, Labeled_Revised_Clause, Labeled_Revised_Subclause, 
        Labeled_Revised_Subsubclause, Labeled_Revised_Subclause_ISO_Clause, 
        Labeled_Added_Section, Labeled_Added_Clause, Labeled_Added_Subclause, Labeled_Added_Subsubclause,
        Labeled_Deleted_Clause, Labeled_Deleted_Subclause, Labeled_Deleted_Subsubclause,
	Preface_Section,
	Labeled_Annex, Labeled_Revised_Annex, Labeled_Added_Annex,
	Labeled_Informative_Annex, Labeled_Revised_Informative_Annex,
        Labeled_Added_Informative_Annex,
	Labeled_Normative_Annex, Labeled_Revised_Normative_Annex,
        Labeled_Added_Normative_Annex,
	Unnumbered_Section, Subheading, Heading, Center, Right,
        Added_Subheading, Deleted_Subheading,
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
        Change_Term_Def, Added_Term_List,
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
	Inconsistent2012_Name, Incompatible2012_Name, Extend2012_Name, Wording2012_Name,
	Inconsistent2022_Name, Incompatible2022_Name, Extend2022_Name, Wording2022_Name,
	Syntax_Title, Resolution_Title, Legality_Title, Static_Title,
	Link_Title, Run_Title, Bounded_Title, Erroneous_Title, Req_Title,
	Doc_Title, Metrics_Title, Permission_Title, Advice_Title, Notes_Title,
	Single_Note_Title,
	Examples_Title, Meta_Title, Inconsistent83_Title, Incompatible83_Title,
	Extend83_Title, Wording83_Title, Inconsistent95_Title, Incompatible95_Title,
	Extend95_Title, Wording95_Title, Inconsistent2005_Title, Incompatible2005_Title,
	Extend2005_Title, Wording2005_Title, Inconsistent2012_Title, Incompatible2012_Title,
	Extend2012_Title, Wording2012_Title, Inconsistent2022_Title, Incompatible2022_Title,
	Extend2022_Title, Wording2022_Title,
        Intl_Standard_Name, Intl_Standard_Title, Standard_Title, Subnumber,
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