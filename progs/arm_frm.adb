with ARM_Output,
     ARM_Input,
     ARM_File,
     ARM_String,
     ARM_Contents,
     ARM_Database,
     ARM_Syntax,
     ARM_Index,
     Ada.Text_IO,
     Ada.Characters.Handling,
     Ada.Strings.Fixed;
package body ARM_Format is

    --
    -- Ada reference manual formatter.
    --
    -- This package contains the routines to parse the input files, and
    -- determine what to output.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2002, 2003, 2004  AXE Consultants.
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
    --  4/14/00 - RLB - Created base package.
    --  4/17/00 - RLB - Starting implementation of commands.
    --  4/19/00 - RLB - Implemented contents package and section references.
    --  4/21/00 - RLB - Added hard_space and line_break output routines.
    --  4/24/00 - RLB - Added Change_Kind and Display_Index_Entries.
    --		- RLB - Added Change and ChgRef.
    --  4/26/00 - RLB - Added paragraph commands and formats.
    --  4/29/00 - RLB - Loose ends: "Part", fixes for the example format,
    --			"DescribeCode" and "Itemize".
    --  5/10/00 - RLB - Added missing "MetricsName" and "MetricsTitle".
    --			Added additional paragraph format kinds.
    --  5/11/00 - RLB - Implemented numbers on enumerated paragraphs.
    --	5/12/00	- RLB - Added NoPrefix.
    --	        - RLB - Added attribute commands.
    --  5/13/00 - RLB - Added various character macros.
    --  5/15/00 - RLB - Split input from parsing/formatting.
    --  5/16/00 - RLB - Added database objects and Destroy.
    --		- RLB - Implemented pragma commands.
    --  5/17/00 - RLB - Implemented syntax commands.
    --  5/19/00 - RLB - Added hinge analysis.
    --  5/23/00 - RLB - Added column commands.
    --		- RLB - Added tab commands.
    --  5/24/00 - RLB - Implemented subscript/superscript commands.
    --  5/25/00 - RLB - Added more formatting commands and styles.
    --  5/26/00 - RLB - Removed hinge analysis, other junk.
    --  5/28/00 - RLB - Implemented index operations.
    --  6/ 2/00 - RLB - Implemented @|.
    --		- RLB - Added AdaDefn and AdaSubDefn commands, and unit saving.
    --  8/ 2/00 - RLB - Implemented @! (as @| doesn't work); implemented
    --			lquote, etc.
    --  8/ 4/00 - RLB - Added additional styles.
    --  8/ 8/00 - RLB - Added Attribute_Leading.
    --  8/11/00 - RLB - Fixed glossary report.
    --		- RLB - Added Hanging_in_Bulleted low-level style.
    --  8/15/00 - RLB - Replaced "LangDefType" with "AdaTypeDefn" (much smaller).
    --  8/16/00 - RLB - Added double nesting support for InnerItemize.
    --		- RLB - Added "noparanum" command; removed no paranum formats.
    --  8/17/00 - RLB - Changed Leading flag to Space_After, added Trailing command.
    --		- RLB - Added Nested_Enumerated styles.
    --  8/18/00 - RLB - Fixed a variety of errors in the AARM paragraph numbering.
    --		- RLB - Fixed Display_Index_Entry so it would work right when
    --			given in an insertion or deletion.
    --  8/21/00 - RLB - Fixed so send and later references in a ChgReg command
    --			don't accidentally include all preceding ones.
    --  8/22/00 - RLB - Added Labeled_Revised_Clause and
    --			Labeled_Revised_Subclause commands.
    --  8/23/00 - RLB - Fixed Syntax_Rules to allow @Chg commands in the
    --			LHS.
    --		- RLB - Fixed error in display of Defn2 index entries.
    --  8/28/00 - RLB - Added implementation-defined changes command.
    --  8/30/00 - RLB - Adjusted code in index entries to match old AARM.
    --		- RLB - Made the deleted paragraph text appear in all new
    --			versions.
    --		- RLB - Added AddedSubheading.
    --  8/31/00 - RLB - Added the New_Changes change kind.
    --		- RLB - Added RM_New_Page command.
    --  9/ 1/00 - RLB - Fixed bugs that prevented "deleted paragraph" messages
    --			from appearing and caused junk headers to appear for
    --			sections not appearing in the old document.
    --  9/ 8/00 - RLB - Added information about the language-defined
    --			subprograms to the index introduction.
    --  9/26/00 - RLB - Added Syntax_Display format.
    --  9/28/00 - RLB - Added RefSecbyNum command.
    -- 10/30/00 - RLB - Added ISOOnly paragraph grouping.
    --		- RLB - Fixed inserted paragraph numbers to support more than 9.
    --  6/17/02 - RLB - Added Ada95 changes sections.
    --  7/18/02 - RLB - Moved document type here.
    --		- RLB - Added ARef= parameter to ChgRef.
    --          - RLB - Added Changes_Only and versioning for individual changes.
    --  4/10/03 - RLB - Fixed Index_Pragma to include "<name> pragma".
    --  4/11/03 - RLB - Fixed order of removal for formatting for Heading and
    --			Subheading, so that the nesting is right (it needs to
    --			be exactly like the order of application).
    --		- RLB - Fixed code so that parameter version numbers aren't
    --			displayed higher than the item we're generating.
    --		- RLB - Fixed ChgRef and others not to generate anything if
    --			we're not generating the version that the reference is
    --			for. Similarly, avoid changing the paragraph kind if
    --			we're not going to use the changes.
    --		- RLB - Fixed font for changing non-terminals in @Syn.
    --  9/09/04 - RLB - Removed unused junk noticed by Stephen Leake.
    --  9/10/04 - RLB - Added Version to many Text_Format commands.
    --		- RLB - Fixed Get_NT to allow the Version parameter in @Chg.
    --		- RLB - Updated to allow @Chg nesting.
    --  9/14/04 - RLB - Moved Change_Version_Type to ARM_Contents.
    --		- RLB - Added version number parameters to revised header
    --			commands; added additional header commands.
    --		- RLB - Added code so that section references in Annex L and M
    --			are links.
    --  9/15/04 - RLB - Fixed incorrect name for LabeledAddedSubClause command.
    --		- RLB - Fixed to lift limit on number of inserted paragraphs.
    -- 10/28/04 - RLB - Replaced double single quotes with double quotes,
    --			as directed by the ARG.
    --		- RLB - Added "AddedNormal" ChgRef kind.

    type Command_Kind_Type is (Normal, Begin_Word, Parameter);

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
	 Examples	 => (Length =>  7, Str => "Example                                 "), -- ExamplesName
	 Ada83_Inconsistencies
			 => (Length => 25, Str => "Inconsistency With Ada 83               "), -- Inconsistent83Name
	 Ada83_Incompatibilities
			 => (Length => 27, Str => "Incompatibility With Ada 83             "), -- Incompatible83Name
	 Ada83_Extensions=> (Length => 19, Str => "Extension to Ada 83                     "), -- Extend83Name
	 Ada83_Wording	 => (Length => 26, Str => "Wording Change from Ada 83              "), -- DiffWord83Name
	 Ada95_Inconsistencies
			 => (Length => 25, Str => "Inconsistency With Ada 95               "), -- Inconsistent95Name
	 Ada95_Incompatibilities
			 => (Length => 27, Str => "Incompatibility With Ada 95             "), -- Incompatible95Name
	 Ada95_Extensions=> (Length => 19, Str => "Extension to Ada 95                     "), -- Extend95Name
	 Ada95_Wording	 => (Length => 26, Str => "Wording Change from Ada 95              "), -- DiffWord95Name
	 Reason		 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Ramification	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Proof		 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Imp_Note	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Corr_Change	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Discussion	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Honest		 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Glossary_Marker => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Bare_Annotation => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Wide		 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Example_Text	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Indented_Example_Text=>(Length =>  0, Str => (others => ' ')), -- Not used.
	 Code_Indented	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Bulleted	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Nested_Bulleted => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Display	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Syntax_Display	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Syntax_Indented => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Syntax_Production=>(Length =>  0, Str => (others => ' ')), -- Not used.
	 Enumerated	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Hanging_Indented=> (Length =>  0, Str => (others => ' ')), -- Not used.
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
	 Reason		 => (Length =>  8, Str => "Reason:                                 "), -- Paragraph start.
	 Ramification	 => (Length => 14, Str => "Ramification:                           "), -- Paragraph start.
	 Proof		 => (Length =>  7, Str => "Proof:                                  "), -- Paragraph start.
	 Imp_Note	 => (Length => 21, Str => "Implementation Note:                    "), -- Paragraph start.
	 Corr_Change	 => (Length =>  8, Str => "Change:                                 "), -- Paragraph start.
	 Discussion	 => (Length => 12, Str => "Discussion:                             "), -- Paragraph start.
	 Honest		 => (Length => 14, Str => "To be honest:                           "), -- Paragraph start.
	 Glossary_Marker => (Length => 16, Str => "Glossary entry:                         "), -- Paragraph start.
	 Bare_Annotation => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Wide		 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Example_Text	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Indented_Example_Text=>(Length =>  0, Str => (others => ' ')), -- Not used.
	 Code_Indented	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Bulleted	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Nested_Bulleted => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Display	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Syntax_Display	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Syntax_Indented => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Syntax_Production=>(Length =>  0, Str => (others => ' ')), -- Not used.
	 Enumerated	 => (Length =>  0, Str => (others => ' ')), -- Not used.
	 Hanging_Indented=> (Length =>  0, Str => (others => ' ')), -- Not used.
	 In_Table	 => (Length =>  0, Str => (others => ' '))); -- Not used.

    procedure Create (Format_Object : in out Format_Type;
		      Document : ARM_Format.Document_Type;
		      Changes : in ARM_Format.Change_Kind;
		      Change_Version : in ARM_Contents.Change_Version_Type;
		      Display_Index_Entries : in Boolean) is
	-- Initialize an input object. Document determines the type of
	-- document to create. Changes and Change_Version determine which
	-- changes should be displayed. If Display_Index_Entries is True,
	-- index entries will be printed in the document; otherwise, they
	-- will not generate any visible text (although they might generate
	-- a link anchor).
    begin
	Format_Object.Document := Document;
	Format_Object.Changes := Changes;
	Format_Object.Change_Version := Change_Version;
	Format_Object.Display_Index_Entries := Display_Index_Entries;
	Format_Object.Section := 0;
	Format_Object.Clause := 0;
	Format_Object.Subclause := 0;
        Format_Object.Next_Note := 1;
        Format_Object.Next_Paragraph := 1;
        Format_Object.Next_Insert_Para := 1;
        Format_Object.Next_AARM_Sub := 'a';
        Format_Object.Next_AARM_Insert_Para := 1;
        Format_Object.Next_Enumerated_Num := 1;
        Format_Object.Last_Paragraph_Subhead_Type := Plain;
        Format_Object.Next_Paragraph_Subhead_Type := Plain;
        Format_Object.Next_Paragraph_Format_Type := Plain;
	ARM_Database.Create (Format_Object.Attr_DB);
	ARM_Database.Create (Format_Object.Pragma_DB);
	ARM_Database.Create (Format_Object.Glossary_DB);
	ARM_Database.Create (Format_Object.Impdef_DB);
	ARM_Syntax.Create;
	ARM_Index.Create;
    end Create;


    procedure Destroy (Format_Object : in out Format_Type) is
	-- Destroy a format object, releasing any resources.
    begin
	ARM_Database.Destroy (Format_Object.Attr_DB);
	ARM_Database.Destroy (Format_Object.Pragma_DB);
	ARM_Database.Destroy (Format_Object.Glossary_DB);
	ARM_Database.Destroy (Format_Object.Impdef_DB);
	ARM_Syntax.Destroy;
	ARM_Index.Destroy;
    end Destroy;


    type Command_Type is (
	-- Paragraphs:
	Text_Begin, Text_End, Redundant, Comment, Part, New_Page, Soft_Page,
	New_Column, RM_New_Page,
	-- Basic text formatting:
	Bold, Italic, Roman, Swiss, Fixed, Roman_Italic, Shrink, Grow,
	Keyword, Non_Terminal, No_Prefix, No_Para_Num, Keep_with_Next,
        Leading, Trailing, Up, Down, Thin_Line, Thick_Line, Tab_Clear, Tab_Set,
	-- Tables:
	Table, Table_Param_Caption, Table_Param_Header, Table_Param_Body, Table_Last,
        -- Indexing:
	Defn, RootDefn, PDefn, Defn2, RootDefn2, PDefn2, Index_See,
	Index_See_Also, See_Other, See_Also,
	Index_Root_Unit, Index_Child_Unit, Index_Type, Index_Subprogram,
	Index_Other, Index_Check, Index_Attr, Index_Pragma,
	-- Clause labels:
	Labeled_Section, Labeled_Section_No_Break, Labeled_Clause,
	Labeled_Subclause, Labeled_Revised_Clause, Labeled_Revised_Subclause,
        Labeled_Added_Clause, Labeled_Added_Subclause,
	Preface_Section, Labeled_Informative_Annex, Labeled_Normative_Annex,
        Labeled_Revised_Normative_Annex,
	Unnumbered_Section, Subheading, Heading, Center, Right,
        Added_Subheading,
	-- Clause references:
	Ref_Section, Ref_Section_Number, Ref_Section_by_Number,
	-- Information:
	Syntax_Rule, Syntax_Rule_RHS, Syntax_Term, Syntax_Prefix,
	Syntax_Summary, Syntax_Xref,
	Implementation_Defined,	Implementation_Defined_List,
	To_Glossary, To_Glossary_Also, Glossary_Text_Param, -- The last is a parameter of the first two.
	Glossary_List,
        Prefix_Type, Reset_Prefix_Type, Attribute, Attribute_Leading, Attribute_Text_Param, -- The last is a parameter of Attribute.
	Attribute_List,
	Pragma_Syntax, Pragma_List, Added_Pragma_Syntax,
	-- Corrigendum changes:
	Change, Change_Param_Old, Change_Param_New, -- The latter are the parameters of "Change".
	Change_Reference, Change_Note,
	Change_Implementation_Defined,
	Change_Impdef_Text_Param, -- This is a parameter of the previous.
	Change_Attribute, Change_Prefix_Type,
	Change_Prefix_Text_Param, -- This is a parameter of the previous.
	-- Text macros:
	Intro_Name, Syntax_Name, Resolution_Name, Legality_Name, Static_Name,
	Link_Name, Run_Name, Bounded_Name, Erroneous_Name, Req_Name,
	Doc_Name, Metrics_Name, Permission_Name, Advice_Name, Notes_Name,
	Examples_Name, Meta_Name, Inconsistent83_Name, Incompatible83_Name,
	Extend83_Name, Wording83_Name,
	Inconsistent95_Name, Incompatible95_Name, Extend95_Name, Wording95_Name,
	Syntax_Title, Resolution_Title, Legality_Title, Static_Title,
	Link_Title, Run_Title, Bounded_Title, Erroneous_Title, Req_Title,
	Doc_Title, Metrics_Title, Permission_Title, Advice_Title, Notes_Title,
	Examples_Title, Meta_Title, Inconsistent83_Title, Incompatible83_Title,
	Extend83_Title, Wording83_Title, Inconsistent95_Title, Incompatible95_Title,
	Extend95_Title, Wording95_Title,
	-- Character macros:
	EM_Dash, EN_Dash, LE, LT, GE, GT, NE, PI, Times, PorM, Single_Quote,
	Latin_1, Ceiling, Floor, Absolute, Log, Thin_Space,
	Left_Quote, Right_Quote, Left_Double_Quote, Right_Double_Quote,
	Left_Quote_Pair, Right_Quote_Pair,
	Unknown);



    function Command (Name : in ARM_Input.Command_Name_Type) return Command_Type is
	-- Return the command value for a particular command name:
	Canonical_Name : constant String :=
	    Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Name, Ada.Strings.Right));
    begin
	if Canonical_Name = "begin" then
	    return Text_Begin;
	elsif Canonical_Name = "end" then
	    return Text_End;
	elsif Canonical_Name = "redundant" then
	    return Redundant;
	elsif Canonical_Name = "comment" then
	    return Comment;
	elsif Canonical_Name = "noprefix" then
	    return No_Prefix;
	elsif Canonical_Name = "noparanum" then
	    return No_Para_Num;
	elsif Canonical_Name = "keepnext" then
	    return Keep_with_Next;
	elsif Canonical_Name = "leading" then
	    return Leading;
	elsif Canonical_Name = "trailing" then
	    return Trailing;
	elsif Canonical_Name = "+" then -- Can't happen directly, but can happen through stacking.
	    return Up;
	elsif Canonical_Name = "-" then -- Can't happen directly, but can happen through stacking.
	    return Down;
	elsif Canonical_Name = "thinline" then
	    return Thin_Line;
	elsif Canonical_Name = "thickline" then
	    return Thick_Line;
	elsif Canonical_Name = "tabclear" then
	    return Tab_Clear;
	elsif Canonical_Name = "tabset" then
	    return Tab_Set;
	elsif Canonical_Name = "table" then
	    return Table;
	elsif Canonical_Name = "last" then
	    return Table_Last;
	elsif Canonical_Name = "part" then
	    return Part;
	elsif Canonical_Name = "newpage" then
	    return New_Page;
	elsif Canonical_Name = "rmnewpage" then
	    return RM_New_Page;
	elsif Canonical_Name = "softpage" then
	    return Soft_Page;
	elsif Canonical_Name = "newcolumn" then
	    return New_Column;
	elsif Canonical_Name = "b" or else Canonical_Name = "bold" then
	    return Bold;
	elsif Canonical_Name = "i" or else Canonical_Name = "italics" then
	    return Italic;
	elsif Canonical_Name = "r" or else Canonical_Name = "roman" then
	    return Roman;
	elsif Canonical_Name = "s" or else Canonical_Name = "swiss" then
	    return Swiss;
	elsif Canonical_Name = "f" or else Canonical_Name = "fixed" then
	    return Fixed;
	elsif Canonical_Name = "ri" then
	    return Roman_Italic;
	elsif Canonical_Name = "shrink" then
	    return Shrink;
	elsif Canonical_Name = "grow" then
	    return Grow;
	elsif Canonical_Name = "key" then
	    return Keyword;
	elsif Canonical_Name = "nt" then
	    return Non_Terminal;
	elsif Canonical_Name = "defn" then
	    return Defn;
	elsif Canonical_Name = "defn2" then
	    return Defn2;
	elsif Canonical_Name = "rootdefn" then
	    return RootDefn;
	elsif Canonical_Name = "rootdefn2" then
	    return RootDefn2;
	elsif Canonical_Name = "pdefn" then
	    return PDefn;
	elsif Canonical_Name = "pdefn2" then
	    return PDefn2;
	elsif Canonical_Name = "indexsee" then
	    return Index_See;
	elsif Canonical_Name = "indexseealso" then
	    return Index_See_Also;
	elsif Canonical_Name = "seeother" then
	    return See_Other;
	elsif Canonical_Name = "seealso" then
	    return See_Also;
	elsif Canonical_Name = "rootlibunit" then
	    return Index_Root_Unit;
	elsif Canonical_Name = "childunit" then
	    return Index_Child_Unit;
	elsif Canonical_Name = "adatypedefn" then
	    return Index_Type;
	elsif Canonical_Name = "adasubdefn" then
	    return Index_Subprogram;
	elsif Canonical_Name = "adadefn" then
	    return Index_Other;
	elsif Canonical_Name = "indexcheck" then
	    return Index_Check;
	elsif Canonical_Name = "attr" then
	    return Index_Attr;
	elsif Canonical_Name = "prag" then
	    return Index_Pragma;
	elsif Canonical_Name = "syn" then
	    return Syntax_Rule;
	elsif Canonical_Name = "syn2" then
	    return Syntax_Term;
	elsif Canonical_Name = "syni" then
	    return Syntax_Prefix;
	elsif Canonical_Name = "syntaxsummary" then
	    return Syntax_Summary;
	elsif Canonical_Name = "syntaxxref" then
	    return Syntax_Xref;
	elsif Canonical_Name = "toglossary" then
	    return To_Glossary;
	elsif Canonical_Name = "toglossaryalso" then
	    return To_Glossary_Also;
	elsif Canonical_Name = "glossarylist" then
	    return Glossary_List;
	elsif Canonical_Name = "prefixtype" then
	    return Prefix_Type;
	elsif Canonical_Name = "chgprefixtype" then
	    return Change_Prefix_Type;
	elsif Canonical_Name = "endprefixtype" then
	    return Reset_Prefix_Type;
	elsif Canonical_Name = "attribute" then
	    return Attribute;
	elsif Canonical_Name = "attributeleading" then
	    return Attribute_Leading;
	elsif Canonical_Name = "chgattribute" then
	    return Change_Attribute;
	elsif Canonical_Name = "attributelist" then
	    return Attribute_List;
	elsif Canonical_Name = "pragmasyn" then
	    return Pragma_Syntax;
	elsif Canonical_Name = "pragmalist" then
	    return Pragma_List;
	elsif Canonical_Name = "addedpragmasyn" then
	    return Added_Pragma_Syntax;
	elsif Canonical_Name = "impldef" then
	    return Implementation_Defined;
	elsif Canonical_Name = "chgimpldef" then
	    return Change_Implementation_Defined;
	elsif Canonical_Name = "impldeflist" then
	    return Implementation_Defined_List;
	elsif Canonical_Name = "labeledsection" then
	    return Labeled_Section;
	elsif Canonical_Name = "labeledsectionnobreak" then
	    return Labeled_Section_No_Break;
	elsif Canonical_Name = "labeledclause" then
	    return Labeled_Clause;
	elsif Canonical_Name = "labeledsubclause" then
	    return Labeled_Subclause;
	elsif Canonical_Name = "labeledinformativeannex" then
	    return Labeled_Informative_Annex;
	elsif Canonical_Name = "labelednormativeannex" then
	    return Labeled_Normative_Annex;
	elsif Canonical_Name = "unnumberedsection" then
	    return Unnumbered_Section;
	elsif Canonical_Name = "labeledrevisednormativeannex" then
	    return Labeled_Revised_Normative_Annex;
	elsif Canonical_Name = "labeledrevisedclause" then
	    return Labeled_Revised_Clause;
	elsif Canonical_Name = "labeledrevisedsubclause" then
	    return Labeled_Revised_Subclause;
	elsif Canonical_Name = "labeledaddedclause" then
	    return Labeled_Added_Clause;
	elsif Canonical_Name = "labeledaddedsubclause" then
	    return Labeled_Added_Subclause;
	elsif Canonical_Name = "subheading" then
	    return Subheading;
	elsif Canonical_Name = "addedsubheading" then
	    return Added_Subheading;
	elsif Canonical_Name = "heading" then
	    return Heading;
	elsif Canonical_Name = "center" then
	    return Center;
	elsif Canonical_Name = "right" then
	    return Right;
	elsif Canonical_Name = "prefacesection" then
	    return Preface_Section;
	elsif Canonical_Name = "refsec" then
	    return Ref_Section;
	elsif Canonical_Name = "refsecnum" then
	    return Ref_Section_Number;
	elsif Canonical_Name = "refsecbynum" then
	    return Ref_Section_By_Number;
	elsif Canonical_Name = "chg" then
	    return Change;
	elsif Canonical_Name = "chgref" then
	    return Change_Reference;
	elsif Canonical_Name = "chgnote" then
	    return Change_Note;
	elsif Canonical_Name = "introname" then
	    return Intro_Name;
	elsif Canonical_Name = "syntaxname" then
	    return Syntax_Name;
	elsif Canonical_Name = "resolutionname" then
	    return Resolution_Name;
	elsif Canonical_Name = "legalityname" then
	    return Legality_Name;
	elsif Canonical_Name = "staticsemname" then
	    return Static_Name;
	elsif Canonical_Name = "linktimename" then
	    return Link_Name;
	elsif Canonical_Name = "runtimename" then
	    return Run_Name;
	elsif Canonical_Name = "boundedname" then
	    return Bounded_Name;
	elsif Canonical_Name = "erronname" then
	    return Erroneous_Name;
	elsif Canonical_Name = "implreqname" then
	    return Req_Name;
	elsif Canonical_Name = "docreqname" then
	    return Doc_Name;
	elsif Canonical_Name = "metricsname" then
	    return Metrics_Name;
	elsif Canonical_Name = "implpermname" then
	    return Permission_Name;
	elsif Canonical_Name = "impladvicename" then
	    return Advice_Name;
	elsif Canonical_Name = "notesname" then
	    return Notes_Name;
	elsif Canonical_Name = "examplesname" then
	    return Examples_Name;
	elsif Canonical_Name = "metarulesname" then
	    return Meta_Name;
	elsif Canonical_Name = "inconsistent83name" then
	    return Inconsistent83_Name;
	elsif Canonical_Name = "incompatible83name" then
	    return Incompatible83_Name;
	elsif Canonical_Name = "extend83name" then
	    return Extend83_Name;
	elsif Canonical_Name = "diffword83name" then
	    return Wording83_Name;
	elsif Canonical_Name = "inconsistent95name" then
	    return Inconsistent95_Name;
	elsif Canonical_Name = "incompatible95name" then
	    return Incompatible95_Name;
	elsif Canonical_Name = "extend95name" then
	    return Extend95_Name;
	elsif Canonical_Name = "diffword95name" then
	    return Wording95_Name;
	elsif Canonical_Name = "syntaxtitle" then
	    return Syntax_Title;
	elsif Canonical_Name = "resolutiontitle" then
	    return Resolution_Title;
	elsif Canonical_Name = "legalitytitle" then
	    return Legality_Title;
	elsif Canonical_Name = "staticsemtitle" then
	    return Static_Title;
	elsif Canonical_Name = "linktimetitle" then
	    return Link_Title;
	elsif Canonical_Name = "runtimetitle" then
	    return Run_Title;
	elsif Canonical_Name = "boundedtitle" then
	    return Bounded_Title;
	elsif Canonical_Name = "errontitle" then
	    return Erroneous_Title;
	elsif Canonical_Name = "implreqtitle" then
	    return Req_Title;
	elsif Canonical_Name = "docreqtitle" then
	    return Doc_Title;
	elsif Canonical_Name = "metricstitle" then
	    return Metrics_Title;
	elsif Canonical_Name = "implpermtitle" then
	    return Permission_Title;
	elsif Canonical_Name = "impladvicetitle" then
	    return Advice_Title;
	elsif Canonical_Name = "notestitle" then
	    return Notes_Title;
	elsif Canonical_Name = "examplestitle" then
	    return Examples_Title;
	elsif Canonical_Name = "metarulestitle" then
	    return Meta_Title;
	elsif Canonical_Name = "inconsistent83title" then
	    return Inconsistent83_Title;
	elsif Canonical_Name = "incompatible83title" then
	    return Incompatible83_Title;
	elsif Canonical_Name = "extend83title" then
	    return Extend83_Title;
	elsif Canonical_Name = "diffword83title" then
	    return Wording83_Title;
	elsif Canonical_Name = "inconsistent95title" then
	    return Inconsistent95_Title;
	elsif Canonical_Name = "incompatible95title" then
	    return Incompatible95_Title;
	elsif Canonical_Name = "extend95title" then
	    return Extend95_Title;
	elsif Canonical_Name = "diffword95title" then
	    return Wording95_Title;
	elsif Canonical_Name = "em" then
	    return EM_Dash;
	elsif Canonical_Name = "en" then
	    return EN_Dash;
	elsif Canonical_Name = "lt" then
	    return LT;
	elsif Canonical_Name = "leq" then
	    return LE;
	elsif Canonical_Name = "gt" then
	    return GT;
	elsif Canonical_Name = "geq" then
	    return GE;
	elsif Canonical_Name = "neq" then
	    return NE;
	elsif Canonical_Name = "pi" then
	    return PI;
	elsif Canonical_Name = "times" then
	    return Times;
	elsif Canonical_Name = "porm" then
	    return PorM;
	elsif Canonical_Name = "singlequote" then
	    return Single_Quote;
	elsif Canonical_Name = "latin1" then
	    return LATIN_1;
	elsif Canonical_Name = "ceiling" then
	    return Ceiling;
	elsif Canonical_Name = "floor" then
	    return Floor;
	elsif Canonical_Name = "abs" then
	    return Absolute;
	elsif Canonical_Name = "log" then
	    return Log;
	elsif Canonical_Name = "thin" then
	    return Thin_Space;
	elsif Canonical_Name = "lquote" then
	    return Left_Quote;
	elsif Canonical_Name = "lquotes" then
	    return Left_Quote_Pair;
	elsif Canonical_Name = "ldquote" then
	    return Left_Double_Quote;
	elsif Canonical_Name = "rquote" then
	    return Right_Quote;
	elsif Canonical_Name = "rquotes" then
	    return Right_Quote_Pair;
	elsif Canonical_Name = "rdquote" then
	    return Right_Double_Quote;
	else
	    return Unknown;
	end if;
    end Command;


    procedure Scan (Format_Object : in out Format_Type;
		    File_Name : in String;
		    Section_Number : in ARM_Contents.Section_Number_Type;
		    Starts_New_Section : in Boolean) is
	-- Scans the contents for File_Name, determining the table of contents
	-- for the section. The results are written to the contents package.
	-- Starts_New_Section is True if the file starts a new section.
	-- Section_Number is the number (or letter) of the section.

	type Items is record
	    Command : Command_Type;
	    Close_Char : Character; -- Ought to be }, ], >, or ).
	end record;
	Nesting_Stack : array (1 .. 60) of Items;
	Nesting_Stack_Ptr : Natural := 0;
	Saw_a_Section_Header : Boolean := False;

	Input_Object : ARM_File.File_Input_Type;

	procedure Set_Nesting_for_Command (Command : in Command_Type;
					   Param_Ch : in Character) is
	    -- Push the command onto the nesting stack.
	begin
	    Nesting_Stack_Ptr := Nesting_Stack_Ptr + 1;
	    Nesting_Stack (Nesting_Stack_Ptr) :=
	        (Command => Command,
		 Close_Char => ARM_Input.Get_Close_Char (Param_Ch));
--Ada.Text_IO.Put_Line (" &Stack (" & Name & ")");
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

	begin
	    case Nesting_Stack(Nesting_Stack_Ptr).Command is
		when Labeled_Section | Labeled_Section_No_Break |
		     Labeled_Informative_Annex |
		     Labeled_Normative_Annex | Labeled_Clause |
		     Labeled_Subclause =>
		    -- Load the title into the Title string:
		    ARM_Input.Copy_to_String_until_Close_Char (
		        Input_Object,
		        Nesting_Stack(Nesting_Stack_Ptr).Close_Char,
		        Title, Title_Length);
		    Title(Title_Length+1 .. Title'Last) :=
			(others => ' ');
		    if Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Subclause then
			Format_Object.Subclause := Format_Object.Subclause + 1;
		    elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Clause then
			Format_Object.Clause := Format_Object.Clause + 1;
			Format_Object.Subclause := 0;
		    elsif Saw_a_Section_Header then
			Ada.Text_IO.Put_Line ("  ** Multiple section headers in a file, line " &
				ARM_File.Line_String (Input_Object));
		    else
			Saw_a_Section_Header := True;
			Format_Object.Clause := 0;
			Format_Object.Subclause := 0;
		    end if;

		    -- Load the title into the contents package:
		    if Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Subclause then
			ARM_Contents.Add (Title, ARM_Contents.Subclause,
					  Format_Object.Section,
					  Format_Object.Clause,
					  Format_Object.SubClause);
		    elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Clause then
			ARM_Contents.Add (Title, ARM_Contents.Clause,
					  Format_Object.Section,
					  Format_Object.Clause);
		    elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Section or else
		          Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Section_No_Break then
			ARM_Contents.Add (Title, ARM_Contents.Section,
					  Format_Object.Section);
		    elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Normative_Annex then
			ARM_Contents.Add (Title, ARM_Contents.Normative_Annex,
					  Format_Object.Section);
		    else
			ARM_Contents.Add (Title, ARM_Contents.Informative_Annex,
					  Format_Object.Section);
		    end if;

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
		    Format_Object.Clause := Format_Object.Clause + 1;
		    Format_Object.Subclause := 0;
		    if ARM_Contents."=" (Format_Object.Section, 20) then
			-- The library header:
		        -- Load the title into the contents package:
		        ARM_Contents.Add (Title, ARM_Contents.Unnumbered_Section,
				          0, -- Section is 0, so there is no number.
				          Format_Object.Clause + 27);
		    else
		        -- Load the title into the contents package:
		        ARM_Contents.Add (Title, ARM_Contents.Unnumbered_Section,
				          0, -- Section is 0, so there is no number.
				          Format_Object.Clause);
		    end if;

		    Nesting_Stack_Ptr := Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Header)");

		when Labeled_Revised_Normative_Annex |
		     Labeled_Revised_Clause |
		     Labeled_Revised_Subclause =>
		    declare
			Old_Title : ARM_Contents.Title_Type;
			Old_Title_Length : Natural;
			Ch : Character;
			Version : ARM_Contents.Change_Version_Type := '0';
		    begin
			Get_Change_Version (Is_First => True,
					    Version => Version);
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "New" & (4..ARM_Input.Command_Name_Type'Last => ' '),
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
			    ARM_Input.Check_Parameter_Name (Input_Object,
			        Param_Name => "Old" & (4..ARM_Input.Command_Name_Type'Last => ' '),
			        Is_First => False,
			        Param_Close_Bracket => Ch);
			    if Ch /= ' ' then
			        -- There is a parameter:
			        -- Load the new title into the Title string:
			        ARM_Input.Copy_to_String_until_Close_Char (
			            Input_Object,
			            Ch,
			            Old_Title, Old_Title_Length);
			        Old_Title(Old_Title_Length+1 .. Old_Title'Last) :=
				    (others => ' ');
			    end if;
			end if;
		        ARM_File.Get_Char (Input_Object, Ch);
		        if Ch /= Nesting_Stack(Nesting_Stack_Ptr).Close_Char then
			    Ada.Text_IO.Put_Line ("  ** Bad close for Labeled_Revised_(SubClause|Annex) on line " & ARM_File.Line_String (Input_Object));
			    ARM_File.Replace_Char (Input_Object);
		        end if;

		        if Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Revised_Subclause then
			    Format_Object.Subclause := Format_Object.Subclause + 1;
		        elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Revised_Clause then
			    Format_Object.Clause := Format_Object.Clause + 1;
			    Format_Object.Subclause := 0;
		        elsif Saw_a_Section_Header then
			    Ada.Text_IO.Put_Line ("  ** Multiple section headers in a file, line " &
				    ARM_File.Line_String (Input_Object));
		        else
			    Saw_a_Section_Header := True;
			    Format_Object.Clause := 0;
			    Format_Object.Subclause := 0;
		        end if;

		        -- Load the title into the contents package:
		        if Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Revised_Subclause then
			    ARM_Contents.Add (Title, ARM_Contents.Subclause,
					      Format_Object.Section,
					      Format_Object.Clause,
					      Format_Object.SubClause,
					      Version => Version);
			    ARM_Contents.Add_Old (Old_Title,
					      ARM_Contents.Subclause,
					      Format_Object.Section,
					      Format_Object.Clause,
					      Format_Object.SubClause);
		        elsif Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Revised_Clause then
			    ARM_Contents.Add (Title, ARM_Contents.Clause,
					      Format_Object.Section,
					      Format_Object.Clause,
					      Version => Version);
			    ARM_Contents.Add_Old (Old_Title,
					      ARM_Contents.Clause,
					      Format_Object.Section,
					      Format_Object.Clause);
		        else -- Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Normative_Annex then
			    ARM_Contents.Add (Title,
					      ARM_Contents.Normative_Annex,
					      Format_Object.Section,
					      Version => Version);
			    ARM_Contents.Add_Old (Old_Title,
					      ARM_Contents.Normative_Annex,
					      Format_Object.Section);
		        end if;

			Nesting_Stack_Ptr := Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Header)");
		    end;

		when Labeled_Added_Clause |
		     Labeled_Added_Subclause =>
		    declare
			Old_Title : ARM_Contents.Title_Type;
			Old_Title_Length : Natural;
			Ch : Character;
			Version : ARM_Contents.Change_Version_Type := '0';
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

		        if Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Added_Subclause then
			    Format_Object.Subclause := Format_Object.Subclause + 1;
		        else -- if Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Added_Clause then
			    Format_Object.Clause := Format_Object.Clause + 1;
			    Format_Object.Subclause := 0;
		        end if;

		        -- Load the title into the contents package:
		        if Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Added_Subclause then
			    ARM_Contents.Add (Title, ARM_Contents.Subclause,
					      Format_Object.Section,
					      Format_Object.Clause,
					      Format_Object.SubClause,
					      Version => Version);
			    ARM_Contents.Add_Old ((others => ' '),
					      ARM_Contents.Subclause,
					      Format_Object.Section,
					      Format_Object.Clause,
					      Format_Object.SubClause);
		        else --if Nesting_Stack(Nesting_Stack_Ptr).Command = Labeled_Added_Clause then
			    ARM_Contents.Add (Title, ARM_Contents.Clause,
					      Format_Object.Section,
					      Format_Object.Clause,
					      Version => Version);
			    ARM_Contents.Add_Old ((others => ' '),
					      ARM_Contents.Clause,
					      Format_Object.Section,
					      Format_Object.Clause);
		        end if;

			Nesting_Stack_Ptr := Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Header)");
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
	    Nesting_Stack_Ptr := Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Normal)");
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
		-- This represents a tab (or the end of centered text). We're
		-- done here.
		return;
	    elsif Ch = '=' then
		-- This marks the beginning of centered text.
		-- We're done here.
		return;
	    elsif Ch = '^' then
		-- This represented a tab stop (these should have been
		-- deleted from the input). We're done here.
		return;
	    elsif Ch = '@' then
		-- This represents @ in the text. We're done here.
		return;
	    elsif Ch = ' ' then
		-- This represents a hard space in the text. We're done here.
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
	    Format_Object.Section := Section_Number;
	    Format_Object.Clause := 0;
	    Format_Object.Subclause := 0;
	end if;
        loop
	    declare
	        Char : Character;
	    begin
	        ARM_File.Get_Char (Input_Object, Char);
	        case Char is
		    when '@' =>
		        Scan_Special;
		    when Ascii.SUB =>
		        exit; -- End of file.
		    when others =>
		        if Nesting_Stack_Ptr /= 0 and then
			   Nesting_Stack (Nesting_Stack_Ptr).Close_Char /= ' ' and then
			   Nesting_Stack (Nesting_Stack_Ptr).Close_Char = Char then
    			    -- Closing a command, remove it from the stack.
			    Handle_End_of_Command;
		        else
			    null; -- Ordinary characters, nothing to do.
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
        end if;
    end Scan;


    procedure Insert_Index (Format_Object : in out Format_Type) is
	-- Insert the header for the index into the table of contents.
    begin
	ARM_Contents.Add ("Index" & (6..ARM_Contents.Title_Type'Last => ' '),
			  ARM_Contents.Unnumbered_Section,
			  0, -- Section is 0, so there is no number.
			  29);
    end Insert_Index;


    procedure Write_Table_of_Contents (
		       Format_Object : in out Format_Type;
		       Output_Object : in out ARM_Output.Output_Type'Class) is
	-- Writes the table of contents for the document. (It will have
	-- a section name of "TOC"). This should be done after all calls to
	-- Scan and before any calls to Process.

	In_Paragraph : Boolean := False;

	procedure Write_It (Title : in ARM_Contents.Title_Type;
		   Level : in ARM_Contents.Level_Type;
		   Section_Number : in ARM_Contents.Section_Number_Type;
		   Clause_Number : in Natural;
		   Subclause_Number : in Natural;
                   Version : in ARM_Contents.Change_Version_Type;
		   Quit : out Boolean) is
	    Clause_Text : constant String :=
		ARM_Contents.Make_Clause_Number (Level, Section_Number,
						 Clause_Number, Subclause_Number);
	    Old_Title : ARM_Contents.Title_Type :=
		        ARM_Contents.Lookup_Old_Title (
			    Level, Section_Number,
			    Clause_Number, Subclause_Number);


	begin
	    Quit := False;
	    if Old_Title = ARM_Contents.Title_Type'(others => ' ') and then
	       (Format_Object.Change_Version < Version or else
		ARM_Format."=" (Format_Object.Changes, ARM_Format.Old_Only)) then
		-- This is an added item, and we're generating a version
		-- that does not include it. Skip it completely.
		return;
	    end if;
	    if ARM_Contents."=" (Level, ARM_Contents.Clause) then
	        ARM_Output.Line_Break (Output_Object);
	        ARM_Output.Ordinary_Text (Output_Object, "    ");
	        ARM_Output.Ordinary_Text (Output_Object, Clause_Text);
	        ARM_Output.Hard_Space (Output_Object);
	    elsif ARM_Contents."=" (Level, ARM_Contents.Subclause) then
		ARM_Output.Line_Break (Output_Object);
		ARM_Output.Ordinary_Text (Output_Object, "        ");
		ARM_Output.Ordinary_Text (Output_Object, Clause_Text);
		ARM_Output.Hard_Space (Output_Object);
	    elsif ARM_Contents."=" (Level, ARM_Contents.Unnumbered_Section) then
	        if In_Paragraph then
		    ARM_Output.End_Paragraph (Output_Object);
	        end if;
	        ARM_Output.Start_Paragraph (Output_Object,
				            ARM_Output.Normal,
				            "",
					    Justification => ARM_Output.Left);
	        In_Paragraph := True;
	    else
		if In_Paragraph then
		    ARM_Output.End_Paragraph (Output_Object);
		end if;
	        ARM_Output.Start_Paragraph (Output_Object,
				            ARM_Output.Normal,
				            "",
					    Justification => ARM_Output.Left);
		In_Paragraph := True;
	        if ARM_Contents."=" (Level, ARM_Contents.Section) then
		    ARM_Output.Ordinary_Text (Output_Object, Clause_Text);
		    ARM_Output.Ordinary_Text (Output_Object, ". ");
	        else -- Annexes.
	            ARM_Output.Ordinary_Character (Output_Object, Clause_Text(Clause_Text'Last)); -- We don't want the "Annex" part.
	            ARM_Output.Ordinary_Text (Output_Object, ". ");
	        end if;
	    end if;
	    if Format_Object.Change_Version < Version then
		-- Ignore the change:
	        ARM_Output.Clause_Reference (Output_Object,
		    Ada.Strings.Fixed.Trim (
		        ARM_Contents.Lookup_Old_Title (
			    Level, Section_Number,
			    Clause_Number, Subclause_Number), Ada.Strings.Right),
		    Clause_Text);
	    else
	        case Format_Object.Changes is
	            when ARM_Format.Old_Only =>
		        ARM_Output.Clause_Reference (Output_Object,
		            Ada.Strings.Fixed.Trim (
			        ARM_Contents.Lookup_Old_Title (
				    Level, Section_Number,
				    Clause_Number, Subclause_Number), Ada.Strings.Right),
		            Clause_Text);
	            when ARM_Format.New_Only |
		         ARM_Format.Changes_Only |
		         ARM_Format.Show_Changes |
		         ARM_Format.New_Changes =>
		        ARM_Output.Clause_Reference (Output_Object,
		            Ada.Strings.Fixed.Trim (Title, Ada.Strings.Right),
		            Clause_Text);
	        end case;
	    end if;
	end Write_It;

	procedure Write_Contents is new ARM_Contents.For_Each (Write_It);

    begin
	-- Note: For .RTF version, the result of this call will not be used,
	-- preferring to let Word make the TOC (it can include page numbers).
        ARM_Output.Section (Output_Object,
			    Section_Title => "Table of Contents",
			    Section_Name => "TOC");

	ARM_Output.Clause_Header (Output_Object,
				  Header_Text => "Table of Contents",
				  Level => ARM_Contents.Section,
				  Clause_Number => "");

	ARM_Output.TOC_Marker (Output_Object, For_Start => True);

	Write_Contents;

	if In_Paragraph then
	    ARM_Output.End_Paragraph (Output_Object);
	end if;

	ARM_Output.TOC_Marker (Output_Object, For_Start => False);

    end Write_Table_of_Contents;


    procedure Write_Index (
		        Format_Object : in out Format_Type;
		       Output_Object : in out ARM_Output.Output_Type'Class) is
	-- Writes the index for the document. (It will have
	-- a section name of "IDX").
    begin
        ARM_Output.Section (Output_Object,
			    Section_Title => "Index",
			    Section_Name => "IDX");

	ARM_Output.Clause_Header (Output_Object,
				  Header_Text => "Index",
				  Level => ARM_Contents.Unnumbered_Section,
				  Clause_Number => "0.29");

        ARM_Output.Start_Paragraph (Output_Object, ARM_Output.Normal, Number => "");
	if Format_Object.Document /= ARM_Format.RM_ISO then
	    ARM_Output.Ordinary_Text (Output_Object,
		"Index entries are given by paragraph number. A list of all " &
		"language-defined library units may be found under " &
		"Language-Defined Library Units. A list of all language-defined " &
		"types may be found under Language-Defined Types.");
	else -- ISO: No paragraph numbers!
	    ARM_Output.Ordinary_Text (Output_Object,
		"Index entries are given by subclause. A list of all " &
		"language-defined library units may be found under " &
		"Language-Defined Library Units. A list of all language-defined " &
		"types may be found under Language-Defined Types.");
	end if;
	if Format_Object.Change_Version < '1' then
	    null;
	else
            case Format_Object.Changes is
	        when ARM_Format.Old_Only =>
		    null;
	        when ARM_Format.New_Only =>
	            ARM_Output.Ordinary_Text (Output_Object,
		        " A list of all language-defined subprograms " &
		        "may be found under Language-Defined Subprograms.");
	        when ARM_Format.Show_Changes | ARM_Format.New_Changes | ARM_Format.Changes_Only =>
		    ARM_Output.Text_Format (Output_Object,
		           Bold => False, Italic => False,
		           Font => ARM_Output.Default,
		           Size => 0,
		           Change => ARM_Output.Insertion,
		           Version => '1',
		           Location => ARM_Output.Normal);
	            ARM_Output.Ordinary_Text (Output_Object,
		        " A list of all language-defined subprograms " &
		        "may be found under Language-Defined Subprograms.");
		    ARM_Output.Text_Format (Output_Object,
		           Bold => False, Italic => False,
		           Font => ARM_Output.Default,
		           Size => 0,
		           Change => ARM_Output.None,
		           Location => ARM_Output.Normal);
	    end case;
	end if;

        ARM_Output.End_Paragraph (Output_Object);
	-- Insert a blank paragraph:
        ARM_Output.Start_Paragraph (Output_Object, ARM_Output.Normal, Number => "");
	ARM_Output.Hard_Space (Output_Object);
        ARM_Output.End_Paragraph (Output_Object);

	ARM_Output.Set_Columns (Output_Object, Number_of_Columns => 3);

	ARM_Index.Generate_Index_Body (Output_Object,
		Use_Paragraphs => Format_Object.Document /= ARM_Format.RM_ISO);

	ARM_Output.Set_Columns (Output_Object, Number_of_Columns => 1);

    end Write_Index;


    type Items is record
        Kind : Command_Kind_Type;
        Name : ARM_Input.Command_Name_Type;
        Command : Command_Type;
        Close_Char : Character; -- Ought to be }, ], >, or ).
        -- The next four are only used if Kind=Begin_Word, or for
        -- Command=Implementation_Defined, Glossary_Text_Param, or
	--    Syntax_Rule_RHS.
        Old_Last_Subhead_Paragraph : Paragraph_Type;
        Old_Next_Subhead_Paragraph : Paragraph_Type;
        Old_Next_Paragraph_Format : Paragraph_Type;
	Old_Tab_Stops : ARM_Output.Tab_Info;
	Is_Formatting : Boolean; -- Only used if Kind=Begin_Word.
				 -- The command changes the PARAGRAPH format.
				 -- Otherwise, it should be ignored when
				 -- when determining the format.
	-- The following is only used if Command = Change, Added_Subheading,
	-- or Added_Pragma_Syntax.
	Change_Version : ARM_Contents.Change_Version_Type;
	-- The following are only used if Command = Change.
	Was_Text : Boolean; -- Did the current subcommand have text?
	Prev_Change : ARM_Output.Change_Type;
	Prev_Change_Version : ARM_Contents.Change_Version_Type;
	Prev_Old_Change_Version : ARM_Contents.Change_Version_Type;
    end record;
    type Nesting_Stack_Type is array (1 .. 40) of Items;
    type Format_State_Type is record
	Nesting_Stack : Nesting_Stack_Type;
	Nesting_Stack_Ptr : Natural := 0;
    end record;


    procedure Real_Process (Format_Object : in out Format_Type;
			    Format_State : in out Format_State_Type;
			    Input_Object : in out ARM_Input.Input_Type'Class;
			    Output_Object : in out ARM_Output.Output_Type'Class) is
	-- Process the contents of Input_Object, writing the results to
	-- Output_Object. (Output_Object uses dispatching calls to provide
	-- the correct formatting). Format_Object holds information about
	-- the state of the formatting.

	procedure Set_Nesting_for_Command (Name : in ARM_Input.Command_Name_Type;
					   Kind : in Command_Kind_Type;
					   Param_Ch : in Character) is
	    -- Push the command onto the nesting stack.
	begin
	    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr + 1;
	    Format_State.Nesting_Stack (Format_State.Nesting_Stack_Ptr) :=
	        (Name => Name,
		 Kind => Kind,
		 Command => Command (Name),
		 Close_Char => ' ', -- Set below.
		 Old_Last_Subhead_Paragraph => Plain, -- Not used.
		 Old_Next_Subhead_Paragraph => Plain, -- Not used.
		 Old_Next_Paragraph_Format => Plain, -- Not used.
		 Old_Tab_Stops => ARM_Output.NO_TABS, -- Not used.
		 Is_Formatting => False, -- Not used.
		 Change_Version => '0', -- Not used.
		 Was_Text => False, -- Not used.
		 Prev_Change => ARM_Output.None, -- Not used.
		 Prev_Change_Version => '0', -- Not used.
		 Prev_Old_Change_Version => '0'); -- Not used.
--Ada.Text_IO.Put_Line (" &Stack (" & Name & ")");
	    Format_State.Nesting_Stack (Format_State.Nesting_Stack_Ptr).Close_Char := ARM_Input.Get_Close_Char (Param_Ch);
	end Set_Nesting_for_Command;


	procedure Set_Nesting_for_Parameter (Command : in Command_Type;
					     Close_Ch : in Character) is
	    -- Push the parameter onto the nesting stack.
	begin
	    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr + 1;
	    Format_State.Nesting_Stack (Format_State.Nesting_Stack_Ptr) :=
	        (Name => (others => ' '),
		 Kind => Parameter,
		 Command => Command,
		 Close_Char => Close_Ch,
		 Old_Last_Subhead_Paragraph => Plain, -- Not used.
		 Old_Next_Subhead_Paragraph => Plain, -- Not used.
		 Old_Next_Paragraph_Format => Plain, -- Not used.
		 Old_Tab_Stops => ARM_Output.NO_TABS, -- Not used.
		 Is_Formatting => False, -- Not used.
		 Change_Version => '0', -- Not used.
		 Was_Text => False, -- Not used.
		 Prev_Change => ARM_Output.None, -- Not used.
		 Prev_Change_Version => '0', -- Not used.
		 Prev_Old_Change_Version => '0'); -- Not used.
--Ada.Text_IO.Put_Line (" &Stack (Parameter)");
	end Set_Nesting_for_Parameter;


        function Clause_String return String is
	    -- Returns a string for a clause reference.
	    use type ARM_Contents.Section_Number_Type;
        begin
	    if Format_Object.Subclause /= 0 then
	        return ARM_Contents.Make_Clause_Number (
		        ARM_Contents.SubClause,
		        Format_Object.Section,
		        Format_Object.Clause,
		        Format_Object.Subclause);
	    elsif Format_Object.Clause /= 0 then
	        return ARM_Contents.Make_Clause_Number (
		        ARM_Contents.Clause,
		        Format_Object.Section,
		        Format_Object.Clause);
	    else
		if Format_Object.Section = 0 then
	            return ARM_Contents.Make_Clause_Number (
		            ARM_Contents.Unnumbered_Section,
		            Format_Object.Section);
		elsif Format_Object.Section <= 20 then
	            return ARM_Contents.Make_Clause_Number (
		            ARM_Contents.Section,
		            Format_Object.Section);
		else
	            return ARM_Contents.Make_Clause_Number (
		            ARM_Contents.Normative_Annex,
		            Format_Object.Section);
		end if;
	    end if;
        end Clause_String;


        function Is_AARM_Paragraph (Kind : in Paragraph_Type) return Boolean is
        begin
	    case Kind is
	        when Plain | Introduction | Syntax | Resolution | Legality |
		     Static_Semantics | Link_Time |
		     Run_Time | Bounded_Errors |
		     Erroneous | Requirements | Documentation |
		     Metrics | Permissions | Advice | Notes | Examples =>
		    return False;
	        when Language_Design | Ada83_Inconsistencies |
		     Ada83_Incompatibilities | Ada83_Extensions |
		     Ada83_Wording | Ada95_Inconsistencies |
		     Ada95_Incompatibilities | Ada95_Extensions |
		     Ada95_Wording | Reason | Ramification | Proof |
		     Imp_Note | Corr_Change | Discussion |
		     Honest | Glossary_Marker | Bare_Annotation =>
		    return True;
	        when In_Table =>
		    return False; -- Tables are never considered part of the
			    -- AARM for formatting purposes, even when they are.
	        when Wide | Example_Text | Indented_Example_Text |
		     Bulleted | Code_Indented |
		     Nested_Bulleted | Display | Syntax_Display |
		     Syntax_Indented | Syntax_Production |
		     Enumerated | Hanging_Indented =>
		    -- This depends on the containing paragraph kind;
		    -- Last_Paragraph_Subhead_Type should contain that.
		    if Format_Object.Last_Paragraph_Subhead_Type = Wide or else
		       Format_Object.Last_Paragraph_Subhead_Type = Example_Text or else
		       Format_Object.Last_Paragraph_Subhead_Type = Indented_Example_Text or else
		       Format_Object.Last_Paragraph_Subhead_Type = Bulleted or else
		       Format_Object.Last_Paragraph_Subhead_Type = Code_Indented or else
		       Format_Object.Last_Paragraph_Subhead_Type = Nested_Bulleted or else
		       Format_Object.Last_Paragraph_Subhead_Type = Display or else
		       Format_Object.Last_Paragraph_Subhead_Type = Syntax_Display or else
		       Format_Object.Last_Paragraph_Subhead_Type = Syntax_Indented or else
		       Format_Object.Last_Paragraph_Subhead_Type = Syntax_Production or else
		       Format_Object.Last_Paragraph_Subhead_Type = Enumerated or else
		       Format_Object.Last_Paragraph_Subhead_Type = Hanging_Indented or else
		       Format_Object.Last_Paragraph_Subhead_Type = In_Table then
Ada.Text_IO.Put_Line ("%% Oops, can't find out if AARM paragraph, line " & ARM_Input.Line_String (Input_Object));
		        return False; -- Oops, can't tell (double nesting).
			    -- We make this check to avoid infinite recursion.
		    else
		        return Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type);
		    end if;
	    end case;
        end Is_AARM_Paragraph;


	procedure Paragraph_Number_String (Update_Numbers : in Boolean) is
	    -- Generate the current (next) paragraph number string into
	    -- Format_Object.Current_Paragraph_String. Update the paragraph
	    -- numbers if Update_Numbers is True.
            PNum : constant String := Integer'Image(Format_Object.Next_Paragraph);
            PNum_Pred : constant String := Integer'Image(Format_Object.Next_Paragraph-1);
	    use type Arm_Database.Paragraph_Change_Kind_Type;

	    procedure AARM_Sub_Num (Sub_Letter : in Character) is
		-- Adds a properly formatted AARM sub letter, with leading '.'.
		-- Sets the length appropriately.
	    begin
	        Format_Object.Current_Paragraph_String(PNum_Pred'Last) :=
		    '.';
	        if Sub_Letter <= 'z' then
		    Format_Object.Current_Paragraph_String(PNum_Pred'Last+1) :=
		        Sub_Letter;
		    Format_Object.Current_Paragraph_Len :=
			    PNum_Pred'Last + 1;
	        else
		    Format_Object.Current_Paragraph_String(PNum_Pred'Last+1) :=
		        Character'Val(Character'Pos(Sub_Letter) - 26);
		    Format_Object.Current_Paragraph_String(PNum_Pred'Last+2) :=
		        Character'Val(Character'Pos(Sub_Letter) - 26);
		    Format_Object.Current_Paragraph_Len :=
			    PNum_Pred'Last + 2;
	        end if;
            end AARM_Sub_Num;

	begin
	    -- The full paragraph number is:
	    -- Num.AARM.Ins/Vers
	    -- where Num is a paragraph number in the RM; AARM is the
	    -- paragraph subletter for AARM text; Ins is the number of
	    -- inserted paragraph; and Vers is the version number ('1'
	    -- for Technical Corrigendum 1; '0' for original RM.;
	    -- '2' for Amendment 1). Note that we don't include change
	    -- versions greater than the one we're currently processing.
	    -- Unused parts are omitted.

	    -- %%%% Buglet: If a paragraph was changed by multiple versions,
	    -- we only know the last one. So we guess. If there was a change
	    -- visible here, there must be a revision number, so we use the
	    -- one we're generating. That could be wrong if we have three
	    -- or more versions. I hope we don't need to regenerate old
	    -- versions that far back.

	    if Format_Object.Next_Paragraph_Change_Kind = ARM_Database.None or else
	       Format_Object.Changes = Old_Only then
	        -- Either there is no change, or we are only producing
	        -- the original document (which means we ignore all
	        -- marked changes).
	        if Is_AARM_Paragraph (Format_Object.Next_Paragraph_Subhead_Type) then
	            Format_Object.Current_Paragraph_String(1 .. PNum_Pred'Last-1) :=
		        PNum_Pred(2..PNum_Pred'Last);
		    AARM_Sub_Num (Format_Object.Next_AARM_Sub);
		    if Update_Numbers then
		        Format_Object.Next_AARM_Sub := Character'Succ(Format_Object.Next_AARM_Sub);
			Format_Object.Next_AARM_Insert_Para := 1;
		    end if;
	        else
	            Format_Object.Current_Paragraph_String(1 .. PNum'Last-1) :=
		        PNum(2..PNum'Last);
		    Format_Object.Current_Paragraph_Len := PNum'Last - 1;
		    if Update_Numbers then
		        Format_Object.Next_Paragraph := Format_Object.Next_Paragraph + 1;
		        Format_Object.Next_AARM_Sub := 'a';
			Format_Object.Next_AARM_Insert_Para := 1;
		    end if;
	        end if;
	        if Update_Numbers then
		    Format_Object.Next_Insert_Para := 1;
		end if;
	    elsif Format_Object.Next_Paragraph_Change_Kind = ARM_Database.Inserted then
	        -- We'll assume that there are no more than 99 inserted
		-- paragraphs in a row.
	        Format_Object.Current_Paragraph_String(1 .. PNum_Pred'Last-1) :=
		    PNum_Pred(2..PNum_Pred'Last);
	        if Is_AARM_Paragraph (Format_Object.Next_Paragraph_Subhead_Type) then
		    if Format_Object.Next_AARM_Sub = 'a' then
			AARM_Sub_Num ('a'); -- No paras before the insertion.
		    else
			AARM_Sub_Num (Character'Pred(Format_Object.Next_AARM_Sub));
			    -- Insertions use the preceeding paragraph letter.
		    end if;
	            Format_Object.Current_Paragraph_String(Format_Object.Current_Paragraph_Len + 1) :=
		        '.';
		    if Format_Object.Next_AARM_Insert_Para >= 10 then
			Format_Object.Current_Paragraph_String(Format_Object.Current_Paragraph_Len + 2) :=
			    Character'Val((Format_Object.Next_AARM_Insert_Para/10) + Character'Pos('0'));
	                Format_Object.Current_Paragraph_String(Format_Object.Current_Paragraph_Len + 3) :=
		            Character'Val((Format_Object.Next_AARM_Insert_Para mod 10) + Character'Pos('0'));
			Format_Object.Current_Paragraph_Len := Format_Object.Current_Paragraph_Len + 1;
			    -- Make this consistent with the other cases.
		    else
	                Format_Object.Current_Paragraph_String(Format_Object.Current_Paragraph_Len + 2) :=
		            Character'Val(Format_Object.Next_AARM_Insert_Para + Character'Pos('0'));
		    end if;
	            if Update_Numbers then
			Format_Object.Next_AARM_Insert_Para :=
			    Format_Object.Next_AARM_Insert_Para + 1;
		    end if;
		else
		    Format_Object.Current_Paragraph_Len := PNum_Pred'Last - 1;
	            Format_Object.Current_Paragraph_String(Format_Object.Current_Paragraph_Len + 1) :=
		        '.';
		    if Format_Object.Next_Insert_Para >= 10 then
	                Format_Object.Current_Paragraph_String(Format_Object.Current_Paragraph_Len + 2) :=
			    Character'Val((Format_Object.Next_Insert_Para/10) + Character'Pos('0'));
	                Format_Object.Current_Paragraph_String(Format_Object.Current_Paragraph_Len + 3) :=
		            Character'Val((Format_Object.Next_Insert_Para mod 10) + Character'Pos('0'));
			Format_Object.Current_Paragraph_Len := Format_Object.Current_Paragraph_Len + 1;
			    -- Make this consistent with the other cases.
		    else
	                Format_Object.Current_Paragraph_String(Format_Object.Current_Paragraph_Len + 2) :=
		            Character'Val(Format_Object.Next_Insert_Para + Character'Pos('0'));
		    end if;
	            if Update_Numbers then
			Format_Object.Next_Insert_Para := Format_Object.Next_Insert_Para + 1;
			-- Note: We don't update the AARM numbers for
			-- inserted paragraphs, as the insertion number is
			-- not included in them.
		    end if;
		end if;
		if Format_Object.Next_Paragraph_Version /= '0' then
	            Format_Object.Current_Paragraph_String(Format_Object.Current_Paragraph_Len + 3) :=
		        '/';
		    if Format_Object.Next_Paragraph_Version <= Format_Object.Change_Version then
	                Format_Object.Current_Paragraph_String(Format_Object.Current_Paragraph_Len + 4) :=
		            Format_Object.Next_Paragraph_Version;
		    else -- Use the number we're generating.
	                Format_Object.Current_Paragraph_String(Format_Object.Current_Paragraph_Len + 4) :=
		            Format_Object.Change_Version;
		    end if;
	            Format_Object.Current_Paragraph_Len := Format_Object.Current_Paragraph_Len + 4;
		else
	            Format_Object.Current_Paragraph_Len := Format_Object.Current_Paragraph_Len + 2;
                end if;
	    else --if Format_Object.Next_Paragraph_Change_Kind = ARM_Database.Revised or else
		 --   Format_Object.Next_Paragraph_Change_Kind = ARM_Database.Deleted or else
		 --   Format_Object.Next_Paragraph_Change_Kind = ARM_Database.Inserted_Normal_Number then
	        if Is_AARM_Paragraph (Format_Object.Next_Paragraph_Subhead_Type) then
	            Format_Object.Current_Paragraph_String(1 .. PNum_Pred'Last-1) :=
		        PNum_Pred(2..PNum_Pred'Last);
		    AARM_Sub_Num (Format_Object.Next_AARM_Sub);
	            Format_Object.Current_Paragraph_Len := Format_Object.Current_Paragraph_Len;
	            if Update_Numbers then
		        Format_Object.Next_AARM_Sub := Character'Succ(Format_Object.Next_AARM_Sub);
			Format_Object.Next_AARM_Insert_Para := 1;
		    end if;
	        else
	            Format_Object.Current_Paragraph_String(1 .. PNum'Last-1) :=
		        PNum(2..PNum'Last);
	            Format_Object.Current_Paragraph_Len := PNum'Last - 1;
		    if Update_Numbers then
		        Format_Object.Next_Paragraph := Format_Object.Next_Paragraph + 1;
		        Format_Object.Next_AARM_Sub := 'a';
			Format_Object.Next_AARM_Insert_Para := 1;
		    end if;
	        end if;
	        if Format_Object.Next_Paragraph_Version /= '0' then
	            Format_Object.Current_Paragraph_String(Format_Object.Current_Paragraph_Len + 1) :=
		        '/';
		    if Format_Object.Next_Paragraph_Version <= Format_Object.Change_Version then
	                Format_Object.Current_Paragraph_String(Format_Object.Current_Paragraph_Len + 2) :=
		            Format_Object.Next_Paragraph_Version;
		    else -- Use the number we're generating.
	                Format_Object.Current_Paragraph_String(Format_Object.Current_Paragraph_Len + 2) :=
		            Format_Object.Change_Version;
		    end if;
	            Format_Object.Current_Paragraph_Len := Format_Object.Current_Paragraph_Len + 2;
	        -- else no version to display.
		end if;
	        if Update_Numbers then
		    Format_Object.Next_Insert_Para := 1;
		end if;
	    end if;
	end Paragraph_Number_String;


        function Paragraph_String return String is
	    -- Returns a string for a paragraph reference, for use in an
	    -- index entry.
        begin
	    if Format_Object.In_Paragraph then
		null; -- It is already is stored.
	    else -- Generate the next paragraph number.
		Paragraph_Number_String (Update_Numbers => False);
	    end if;
	    return Format_Object.Current_Paragraph_String (
			1 .. Format_Object.Current_Paragraph_Len);
	end Paragraph_String;


	procedure Check_Paragraph is
	    -- Open a paragraph if needed before outputting any text that needs
	    -- one.

	    procedure Set_Format (For_Type : Paragraph_Type) is

		function Enclosing_Format return Paragraph_Type is
		begin
		    for I in reverse 1 .. Format_State.Nesting_Stack_Ptr loop
			if Format_State.Nesting_Stack(I).Command = Text_Begin and then
			   (Format_State.Nesting_Stack(I).Is_Formatting) then
			    return Format_State.Nesting_Stack(I).Old_Next_Paragraph_Format;
			end if;
		    end loop;
		    return Plain; -- The default format.
		end Enclosing_Format;

		function Outer_Enclosing_Format return Paragraph_Type is
		    -- Returns the second enclosing format:
		begin
		    for I in reverse 1 .. Format_State.Nesting_Stack_Ptr loop
			if Format_State.Nesting_Stack(I).Command = Text_Begin and then
			   (Format_State.Nesting_Stack(I).Is_Formatting) then
			    -- OK, this is the FIRST enclosing format, now
			    -- look for the next one:
			    for J in reverse 1 .. I - 1 loop
				if Format_State.Nesting_Stack(J).Command = Text_Begin and then
				   (Format_State.Nesting_Stack(J).Is_Formatting) then
				    return Format_State.Nesting_Stack(J).Old_Next_Paragraph_Format;
				end if;
			    end loop;
			end if;
		    end loop;
		    return Plain; -- The default format.
		end Outer_Enclosing_Format;

	    begin
		case For_Type is
        	    when Plain | Introduction |
		         Resolution |
		         Legality |
		         Static_Semantics |
		         Link_Time |
		         Run_Time |
		         Bounded_Errors |
		         Erroneous |
		         Requirements | -- ImplReq
		         Documentation | -- DocReq
		         Metrics |
		         Permissions | -- ImplPerm
		         Advice | -- ImplAdvice
		         Examples =>
			Format_Object.Format := ARM_Output.Normal;
			Format_Object.No_Breaks := False;
		    when Wide =>
			if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			   Format_Object.Format := ARM_Output.Wide_Annotations;
			else
			   Format_Object.Format := ARM_Output.Wide;
			end if;
			Format_Object.No_Breaks := False;
		    when Syntax =>
			Format_Object.Format := ARM_Output.Syntax_Indented;
			Format_Object.No_Breaks := True;
		    when Notes => -- Notes
			Format_Object.Format := ARM_Output.Notes;
			Format_Object.No_Breaks := False;
		    when Language_Design | -- "MetaRules"
		         Ada83_Inconsistencies | -- Inconsistent83
		         Ada83_Incompatibilities | -- Incompatible83
		         Ada83_Extensions | -- Extend83
		         Ada83_Wording | -- DiffWord83
		         Ada95_Inconsistencies | -- Inconsistent95
		         Ada95_Incompatibilities | -- Incompatible95
		         Ada95_Extensions | -- Extend95
		         Ada95_Wording => -- DiffWord95
			Format_Object.Format := ARM_Output.Annotations;
			Format_Object.No_Breaks := False;
        	    when Reason | Ramification | Proof |
			 Imp_Note | Corr_Change | Discussion |
			 Honest | Glossary_Marker | Bare_Annotation =>
			Format_Object.Format := ARM_Output.Annotations;
			Format_Object.No_Breaks := False;
        	    when Example_Text =>
			if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			   Format_Object.Format := ARM_Output.Small_Examples;
			else
			   Format_Object.Format := ARM_Output.Examples;
			end if;
			Format_Object.No_Breaks := True;
        	    when Indented_Example_Text =>
			if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			   Format_Object.Format := ARM_Output.Small_Indented_Examples;
			else
			   Format_Object.Format := ARM_Output.Indented_Examples;
			end if;
			Format_Object.No_Breaks := True;
        	    when Code_Indented =>
			if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			   Format_Object.Format := ARM_Output.Small_Code_Indented;
			else
			   Format_Object.Format := ARM_Output.Code_Indented;
			end if;
			Format_Object.No_Breaks := False;
        	    when Bulleted =>
        		if Enclosing_Format = Code_Indented or else
			   Enclosing_Format = Bulleted then
			   -- We also have the Nested_Bulleted style for
			   -- bullets nested inside of other bullets.
			   Format_Object.Format := ARM_Output.Code_Indented_Bulleted;
        		elsif Enclosing_Format = Hanging_Indented then
			   Format_Object.Format := ARM_Output.Indented_Bulleted;
                        elsif Enclosing_Format = Syntax_Indented or else
			      Enclosing_Format = Syntax then
			   Format_Object.Format := ARM_Output.Syntax_Indented_Bulleted;
                        elsif Enclosing_Format = Notes then
			   Format_Object.Format := ARM_Output.Notes_Bulleted;
                        elsif Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			   Format_Object.Format := ARM_Output.Small_Bulleted;
			else
			   Format_Object.Format := ARM_Output.Bulleted;
			end if;
		        Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;
			Format_Object.No_Breaks := False;
        	    when Nested_Bulleted =>
			if Enclosing_Format = Bulleted then
			    -- The normal case. The format depends on the
			    -- outer format:
        		    if Outer_Enclosing_Format = Code_Indented or else
			       Outer_Enclosing_Format = Bulleted then
			       Format_Object.Format := ARM_Output.Code_Indented_Nested_Bulleted;
        		    elsif Outer_Enclosing_Format = Hanging_Indented then
Ada.Text_IO.Put_Line ("%% Oops, Nested_Bulleted in Indented_Bulleted paragraph, line " & ARM_Input.Line_String (Input_Object));
			       Format_Object.Format := ARM_Output.Indented_Bulleted;
                            elsif Outer_Enclosing_Format = Syntax_Indented or else
			          Outer_Enclosing_Format = Syntax then
Ada.Text_IO.Put_Line ("%% Oops, Nested_Bulleted in Syntax_Bulleted paragraph, line " & ARM_Input.Line_String (Input_Object));
			       Format_Object.Format := ARM_Output.Syntax_Indented_Bulleted;
                            elsif Outer_Enclosing_Format = Notes then
			       Format_Object.Format := ARM_Output.Notes_Nested_Bulleted;
                            elsif Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			       Format_Object.Format := ARM_Output.Small_Nested_Bulleted;
			    else
			       Format_Object.Format := ARM_Output.Nested_Bulleted;
			    end if;
			elsif Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			   Format_Object.Format := ARM_Output.Small_Nested_Bulleted;
			else
			   Format_Object.Format := ARM_Output.Nested_Bulleted;
			end if;
		        Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;
			Format_Object.No_Breaks := False;
        	    when Display =>
			declare
			    EF : Paragraph_Type := Enclosing_Format;
			    use type ARM_Output.Paragraph_Type;
			begin
			    if EF = Display then
				null; -- The existing format ought to be correct.
				    -- But we'll go infinitely recursive in any
				    -- case, so forget it.
			    elsif EF = Bulleted or else EF = Nested_Bulleted then
				-- Formats that depend on Enclosing_Format can't
				-- be recursively call Set_Format. However,
				-- Display is never bulleted, so we can tell between
				-- the original format, and ourselves.
				if Format_Object.Format = ARM_Output.Indented_Bulleted then
				    Format_Object.Format := ARM_Output.Indented;
				        -- %%%% Not Indented enough!
Ada.Text_IO.Put_Line ("%% Oops, Display in Indented_Bulleted paragraph, line " & ARM_Input.Line_String (Input_Object));
				elsif Format_Object.Format = ARM_Output.Code_Indented_Bulleted then
				    Format_Object.Format := ARM_Output.Indented;
				        -- %%%% Not Indented further.
Ada.Text_IO.Put_Line ("%% No indentation for Display paragraph, line " & ARM_Input.Line_String (Input_Object));
				elsif Format_Object.Format = ARM_Output.Syntax_Indented_Bulleted then
				    Format_Object.Format := ARM_Output.Indented;
				elsif Format_Object.Format = ARM_Output.Bulleted then
				    Format_Object.Format := ARM_Output.Code_Indented;
				elsif Format_Object.Format = ARM_Output.Small_Bulleted then
				    Format_Object.Format := ARM_Output.Small_Code_Indented;
				elsif Format_Object.Format = ARM_Output.Nested_Bulleted then
				    Format_Object.Format := ARM_Output.Indented;
				elsif Format_Object.Format = ARM_Output.Code_Indented_Nested_Bulleted then
				    Format_Object.Format := ARM_Output.Indented;
				        -- %%%% Not Indented enough!
Ada.Text_IO.Put_Line ("%% Oops, Display in Code_Indented_Nested_Bulleted paragraph, line " & ARM_Input.Line_String (Input_Object));
				elsif Format_Object.Format = ARM_Output.Small_Nested_Bulleted then
				    Format_Object.Format := ARM_Output.Small_Indented;
				else
				    null; -- Probably ourselves.
				end if;
			    elsif EF = Hanging_Indented then
				-- Formats that depend on Enclosing_Format can't
				-- be recursively call Set_Format. However,
				-- Display is never hanging, so we can tell between
				-- the original format, and ourselves.
				if Format_Object.Format = ARM_Output.Indented_Hanging then
				    Format_Object.Format := ARM_Output.Indented;
				        -- %%%% Not Indented further.
Ada.Text_IO.Put_Line ("%% No indentation for Display paragraph, line " & ARM_Input.Line_String (Input_Object));
				elsif Format_Object.Format = ARM_Output.Hanging then
				    Format_Object.Format := ARM_Output.Indented;
				        -- %%%% Not Indented further.
Ada.Text_IO.Put_Line ("%% No indentation for Display paragraph, line " & ARM_Input.Line_String (Input_Object));
				elsif Format_Object.Format = ARM_Output.Hanging_in_Bulleted then
				    Format_Object.Format := ARM_Output.Indented;
				        -- %%%% Not Indented further.
Ada.Text_IO.Put_Line ("%% No indentation for Display paragraph, line " & ARM_Input.Line_String (Input_Object));
				elsif Format_Object.Format = ARM_Output.Small_Hanging then
				    Format_Object.Format := ARM_Output.Small_Indented;
				        -- %%%% Not Indented further.
Ada.Text_IO.Put_Line ("%% No indentation for Display paragraph, line " & ARM_Input.Line_String (Input_Object));
				elsif Format_Object.Format = ARM_Output.Small_Hanging_in_Bulleted then
				    Format_Object.Format := ARM_Output.Small_Indented;
				        -- %%%% Not Indented further.
Ada.Text_IO.Put_Line ("%% No indentation for Display paragraph, line " & ARM_Input.Line_String (Input_Object));
				else
				    null; -- Probably ourselves.
				end if;
			    elsif EF = Enumerated then
				-- Formats that depend on Enclosing_Format can't
				-- be recursively call Set_Format. However,
				-- Display is never hanging, so we can tell between
				-- the original format, and ourselves.
				if Format_Object.Format = ARM_Output.Enumerated then
				    Format_Object.Format := ARM_Output.Code_Indented;
				elsif Format_Object.Format = ARM_Output.Small_Enumerated then
				    Format_Object.Format := ARM_Output.Small_Code_Indented;
				elsif Format_Object.Format = ARM_Output.Nested_Enumerated then
				    Format_Object.Format := ARM_Output.Indented;
				elsif Format_Object.Format = ARM_Output.Small_Nested_Enumerated then
				    Format_Object.Format := ARM_Output.Small_Indented;
				else
				    null; -- Probably ourselves.
				end if;
			    else
			        Set_Format (EF); -- Get the enclosing format, if any.
				    -- We have to do this because we can't
				    -- read the current format: if this was
				    -- a previous Display paragraph (as might
				    -- happen if another format intervened),
				    -- we'll get the wrong answer.
				case Format_Object.Format is
				    when ARM_Output.Normal =>
				        Format_Object.Format := ARM_Output.Syntax_Indented;
				    when ARM_Output.Wide =>
				        Format_Object.Format := ARM_Output.Syntax_Indented;
Ada.Text_IO.Put_Line ("%% Oops, Display in Wide paragraph, line " & ARM_Input.Line_String (Input_Object));
				    when ARM_Output.Notes | ARM_Output.Notes_Header =>
				        Format_Object.Format := ARM_Output.Annotations;
				    when ARM_Output.Annotations =>
					Format_Object.Format := ARM_Output.Small_Code_Indented;
					    -- %% Really one level too deep for annotations.
				    when ARM_Output.Wide_Annotations =>
					Format_Object.Format := ARM_Output.Small_Code_Indented;
					    -- %% Really one level too deep for annotations.
Ada.Text_IO.Put_Line ("%% Oops, Display in Wide_Annotated paragraph, line " & ARM_Input.Line_String (Input_Object));
				    when ARM_Output.Index =>
					Format_Object.Format := ARM_Output.Index;
Ada.Text_IO.Put_Line ("%% No indentation for Display paragraph in Index, line " & ARM_Input.Line_String (Input_Object));
				    when ARM_Output.Syntax_Summary =>
					Format_Object.Format := ARM_Output.Syntax_Summary;
Ada.Text_IO.Put_Line ("%% No indentation for Display paragraph in Syntax Summary, line " & ARM_Input.Line_String (Input_Object));
				    when ARM_Output.Examples =>
					Format_Object.Format := ARM_Output.Code_Indented;
				    when ARM_Output.Small_Examples =>
					Format_Object.Format := ARM_Output.Small_Code_Indented;
				    when ARM_Output.Indented_Examples =>
				        Format_Object.Format := ARM_Output.Indented;
					    -- %%%% Not Indented enough!
Ada.Text_IO.Put_Line ("%% Oops, Display in Indented_Examples paragraph, line " & ARM_Input.Line_String (Input_Object));
				    when ARM_Output.Small_Indented_Examples =>
				        Format_Object.Format := ARM_Output.Small_Indented;
					    -- %%%% Not Indented enough!
Ada.Text_IO.Put_Line ("%% Oops, Display in Small_Indented_Examples paragraph, line " & ARM_Input.Line_String (Input_Object));
				    when ARM_Output.Syntax_Indented =>
				        Format_Object.Format := ARM_Output.Code_Indented;
				    when ARM_Output.Code_Indented =>
				        Format_Object.Format := ARM_Output.Indented;
				    when ARM_Output.Small_Code_Indented =>
				        Format_Object.Format := ARM_Output.Small_Indented;
				    when ARM_Output.Indented | ARM_Output.Small_Indented =>
				        null; -- %%%% Not Indented further.
Ada.Text_IO.Put_Line ("%% No indentation for Display paragraph, line " & ARM_Input.Line_String (Input_Object));
				    when ARM_Output.Bulleted =>
				        Format_Object.Format := ARM_Output.Code_Indented;
				    when ARM_Output.Nested_Bulleted =>
				        Format_Object.Format := ARM_Output.Indented;
				    when ARM_Output.Indented_Bulleted =>
				        Format_Object.Format := ARM_Output.Indented;
					    -- %%%% Not Indented enough!
Ada.Text_IO.Put_Line ("%% Oops, Display in Indented_Bulleted paragraph, line " & ARM_Input.Line_String (Input_Object));
				    when ARM_Output.Code_Indented_Bulleted =>
				        Format_Object.Format := ARM_Output.Indented;
					    -- %%%% Not Indented further.
Ada.Text_IO.Put_Line ("%% No indentation for Display paragraph, line " & ARM_Input.Line_String (Input_Object));
				    when ARM_Output.Code_Indented_Nested_Bulleted =>
				        Format_Object.Format := ARM_Output.Indented;
					    -- %%%% Not Indented enough!
Ada.Text_IO.Put_Line ("%% Oops, Display in Code_Indented_Nested_Bulleted paragraph, line " & ARM_Input.Line_String (Input_Object));
				    when ARM_Output.Syntax_Indented_Bulleted =>
				        Format_Object.Format := ARM_Output.Indented;
				    when ARM_Output.Notes_Bulleted =>
				        Format_Object.Format := ARM_Output.Small_Indented;
				    when ARM_Output.Notes_Nested_Bulleted =>
				        Format_Object.Format := ARM_Output.Small_Indented;
					    -- %%%% Not Indented further.
Ada.Text_IO.Put_Line ("%% No indentation for Display paragraph, line " & ARM_Input.Line_String (Input_Object));
				    when ARM_Output.Small_Bulleted =>
				        Format_Object.Format := ARM_Output.Small_Code_Indented;
				    when ARM_Output.Small_Nested_Bulleted =>
				        Format_Object.Format := ARM_Output.Small_Indented;
				    when ARM_Output.Hanging =>
				        Format_Object.Format := ARM_Output.Indented;
					    -- %%%% Not Indented further.
Ada.Text_IO.Put_Line ("%% No indentation for Display paragraph, line " & ARM_Input.Line_String (Input_Object));
				    when ARM_Output.Small_Hanging =>
				        Format_Object.Format := ARM_Output.Small_Indented;
					    -- %%%% Not Indented further.
Ada.Text_IO.Put_Line ("%% No indentation for Display paragraph, line " & ARM_Input.Line_String (Input_Object));
				    when ARM_Output.Indented_Hanging =>
				        Format_Object.Format := ARM_Output.Indented;
					    -- %%%% Not Indented further.
Ada.Text_IO.Put_Line ("%% No indentation for Display paragraph, line " & ARM_Input.Line_String (Input_Object));
				    when ARM_Output.Small_Indented_Hanging =>
				        Format_Object.Format := ARM_Output.Small_Indented;
					    -- %%%% Not Indented further.
Ada.Text_IO.Put_Line ("%% No indentation for Display paragraph, line " & ARM_Input.Line_String (Input_Object));
				    when ARM_Output.Hanging_in_Bulleted =>
				        Format_Object.Format := ARM_Output.Indented;
					    -- %%%% Not Indented further.
Ada.Text_IO.Put_Line ("%% No indentation for Display paragraph, line " & ARM_Input.Line_String (Input_Object));
				    when ARM_Output.Small_Hanging_in_Bulleted =>
				        Format_Object.Format := ARM_Output.Small_Indented;
					    -- %%%% Not Indented further.
Ada.Text_IO.Put_Line ("%% No indentation for Display paragraph, line " & ARM_Input.Line_String (Input_Object));
				    when ARM_Output.Enumerated =>
				        Format_Object.Format := ARM_Output.Code_Indented;
				    when ARM_Output.Small_Enumerated =>
				        Format_Object.Format := ARM_Output.Small_Code_Indented;
				    when ARM_Output.Nested_Enumerated =>
				        Format_Object.Format := ARM_Output.Indented;
				    when ARM_Output.Small_Nested_Enumerated =>
				        Format_Object.Format := ARM_Output.Small_Indented;
				end case;
			    end if;
			    Format_Object.No_Breaks := True;
			end;
        	    when Syntax_Display =>
			Format_Object.Format := ARM_Output.Syntax_Summary;
			Format_Object.No_Breaks := True;
        	    when Enumerated =>
        		if Enclosing_Format = Enumerated then
			    -- Nesting of enumerated lists should be discouraged,
			    -- so we assume this is ourselves.
			    null;
        		elsif Enclosing_Format = Code_Indented or else
			   Enclosing_Format = Bulleted then
			   if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			       Format_Object.Format := ARM_Output.Small_Nested_Enumerated;
			   else
			       Format_Object.Format := ARM_Output.Nested_Enumerated;
			   end if;
			else
                            if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			       Format_Object.Format := ARM_Output.Small_Enumerated;
			    else
			       Format_Object.Format := ARM_Output.Enumerated;
			    end if;
			end if;
		        Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;
			Format_Object.No_Breaks := False;
        	    when Syntax_Indented =>
			Format_Object.Format := ARM_Output.Syntax_Indented;
			Format_Object.No_Breaks := False;
        	    when Syntax_Production =>
			null; -- Leave format alone (but line-breaks are preserved).
			Format_Object.No_Breaks := True;
        	    when Hanging_Indented =>
                        if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
        		    if Enclosing_Format = Code_Indented or else
        		       Enclosing_Format = Hanging_Indented or else
                               Enclosing_Format = Syntax_Indented or else
			       Enclosing_Format = Syntax then
			       Format_Object.Format := ARM_Output.Small_Indented_Hanging;
        		    elsif Enclosing_Format = Bulleted then
			       Format_Object.Format := ARM_Output.Small_Hanging_in_Bulleted;
			    else
			       Format_Object.Format := ARM_Output.Small_Hanging;
			    end if;
			else -- Normal:
        		    if Enclosing_Format = Code_Indented or else
        		       Enclosing_Format = Hanging_Indented or else
                               Enclosing_Format = Syntax_Indented or else
			       Enclosing_Format = Syntax then
			       Format_Object.Format := ARM_Output.Indented_Hanging;
        		    elsif Enclosing_Format = Bulleted then
			       Format_Object.Format := ARM_Output.Hanging_in_Bulleted;
			    else
			       Format_Object.Format := ARM_Output.Hanging;
			    end if;
			end if;
		        Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;
			Format_Object.No_Breaks := False;
        	    when In_Table =>
                        -- Shouldn't get here.
			if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			    Format_Object.Format := ARM_Output.Annotations;
			else
			    Format_Object.Format := ARM_Output.Normal;
			end if;
			Format_Object.No_Breaks := False;
		end case;
	    end Set_Format;


	    procedure Make_Subhead (For_Type : Paragraph_Type) is
	    begin
		case For_Type is
		    when Syntax |
		         Resolution |
		         Legality |
		         Static_Semantics |
		         Link_Time |
		         Run_Time |
		         Bounded_Errors |
		         Erroneous |
		         Requirements | -- ImplReq
		         Documentation | -- DocReq
		         Metrics |
		         Permissions | -- ImplPerm
		         Advice | -- ImplAdvice
		         Examples =>
			ARM_Output.Category_Header (Output_Object, Paragraph_Kind_Title(For_Type).Str(1..Paragraph_Kind_Title(For_Type).Length));
			Format_Object.Last_Paragraph_Subhead_Type := For_Type;
		    when Notes => -- Notes
			-- The Notes header looks different from the others.
		        ARM_Output.Start_Paragraph (Output_Object,
					            Format => ARM_Output.Notes_Header,
					            Number => "",
						    No_Breaks => True,
						    Keep_with_Next => True);
			ARM_Output.Ordinary_Text (Output_Object, Paragraph_Kind_Title(For_Type).Str(1..Paragraph_Kind_Title(For_Type).Length));
			ARM_Output.End_Paragraph (Output_Object);
			Format_Object.Last_Paragraph_Subhead_Type := For_Type;
		    when Language_Design | -- "MetaRules"
		         Ada83_Inconsistencies | -- Inconsistent83
		         Ada83_Incompatibilities | -- Incompatible83
		         Ada83_Extensions | -- Extend83
		         Ada83_Wording | -- DiffWord83
		         Ada95_Inconsistencies | -- Inconsistent95
		         Ada95_Incompatibilities | -- Incompatible95
		         Ada95_Extensions | -- Extend95
		         Ada95_Wording => -- DiffWord95
			ARM_Output.Category_Header (Output_Object, Paragraph_Kind_Title(For_Type).Str(1..Paragraph_Kind_Title(For_Type).Length));
			Format_Object.Last_Paragraph_Subhead_Type := For_Type;
        	    when Plain | Introduction =>
			null; -- No subheader. We don't change the last
			    -- subheader generated, either.
        	    when Reason | Ramification | Proof |
			 Imp_Note | Corr_Change | Discussion |
			 Honest | Glossary_Marker | Bare_Annotation |
			 Wide | Example_Text |
			 Indented_Example_Text | Code_Indented | Bulleted |
			 Nested_Bulleted | Display | Syntax_Display |
			 Syntax_Indented | Syntax_Production |
			 Hanging_Indented | Enumerated | In_Table =>
			null; -- No subheader. We don't change the last
			    -- subheader generated, either.
		end case;
	    end Make_Subhead;


	    procedure Make_Annotation_Preface (For_Type : Paragraph_Type) is
	    begin
		case For_Type is
		    when Plain | Introduction |
			 Syntax |
		         Resolution |
		         Legality |
		         Static_Semantics |
		         Link_Time |
		         Run_Time |
		         Bounded_Errors |
		         Erroneous |
		         Requirements | -- ImplReq
		         Documentation | -- DocReq
		         Metrics |
		         Permissions | -- ImplPerm
		         Advice | -- ImplAdvice
		         Examples |
			 Notes |
		         Language_Design | -- "MetaRules"
		         Ada83_Inconsistencies | -- Inconsistent83
		         Ada83_Incompatibilities | -- Incompatible83
		         Ada83_Extensions | -- Extend83
		         Ada83_Wording | -- DiffWord83
		         Ada95_Inconsistencies | -- Inconsistent95
		         Ada95_Incompatibilities | -- Incompatible95
		         Ada95_Extensions | -- Extend95
		         Ada95_Wording => -- DiffWord95
			null; -- Not an annotation.
        	    when Reason | Ramification | Proof |
			 Imp_Note | Corr_Change | Discussion |
			 Honest | Glossary_Marker =>
		        ARM_Output.Text_Format (Output_Object,
				    Bold => True,
				    Italic => Format_Object.Is_Italic,
				    Font => Format_Object.Font,
				    Change => Format_Object.Change,
				    Version => Format_Object.Current_Change_Version,
				    Added_Version => Format_Object.Current_Old_Change_Version,
				    Size => Format_Object.Size,
				    Location => Format_Object.Location);
		        ARM_Output.Ordinary_Text (Output_Object,
			     Text => Paragraph_Kind_Title(For_Type).Str(
					1..Paragraph_Kind_Title(For_Type).Length));
		        ARM_Output.Text_Format (Output_Object,
				    Bold => Format_Object.Is_Bold,
				    Italic => Format_Object.Is_Italic,
				    Font => Format_Object.Font,
				    Change => Format_Object.Change,
				    Version => Format_Object.Current_Change_Version,
				    Added_Version => Format_Object.Current_Old_Change_Version,
				    Size => Format_Object.Size,
				    Location => Format_Object.Location);
			Format_Object.Last_Paragraph_Subhead_Type := For_Type;
		    when Bare_Annotation =>
			null; -- Header (if any) is generated elsewhere.
		    when Wide |
			 Example_Text | Indented_Example_Text |
			 Code_Indented | Bulleted | Nested_Bulleted |
			 Display | Syntax_Display |
			 Syntax_Indented | Syntax_Production |
			 Hanging_Indented | Enumerated | In_Table =>
			null; -- Just a format.
		end case;
	    end Make_Annotation_Preface;

	begin
	    if not Format_Object.In_Paragraph then
		-- Output subheader, if needed.
		if Format_Object.Next_Paragraph_Subhead_Type /=
		   Format_Object.Last_Paragraph_Subhead_Type then
		    Make_Subhead (Format_Object.Next_Paragraph_Subhead_Type);
		end if;

		-- Set the paragraph format:
		Set_Format (Format_Object.Next_Paragraph_Format_Type);

		if Format_Object.Document /= ARM_Format.RM_ISO and then
		   not Format_Object.No_Para_Num then

		    -- Format the paragraph number:
		    Paragraph_Number_String (Update_Numbers => True);

--Ada.Text_IO.Put_Line ("Check_Paragraph, make number " &
--Format_Object.Current_Paragraph_String (1 .. Format_Object.Current_Paragraph_Len) &
--": format= " & Paragraph_Type'Image(Format_Object.Next_Paragraph_Format_Type));
		    -- ...and start the paragraph:
		    ARM_Output.Start_Paragraph (Output_Object,
					        Format => Format_Object.Format,
					        Number => Format_Object.Current_Paragraph_String (1 .. Format_Object.Current_Paragraph_Len),
					        No_Prefix => Format_Object.No_Prefix,
					        Tab_Stops => Format_Object.Paragraph_Tab_Stops,
					        No_Breaks => Format_Object.No_Breaks or Format_Object.In_Bundle,
					        Keep_with_Next => Format_Object.Keep_with_Next or Format_Object.In_Bundle,
					        Space_After => Format_Object.Space_After);

		    if ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted) then
			-- If needed, make the "deleted text" message.
			if Format_Object.Current_Change_Version > Format_Object.Change_Version then
		            -- Beyond the current version, so its not deleted.
			    null; -- Not deleted.
			else
		            case Format_Object.Changes is
			        when ARM_Format.New_Only |
			             ARM_Format.Changes_Only |
			             ARM_Format.Show_Changes |
			             ARM_Format.New_Changes =>
				    -- Note that we include this in the
				    -- "Show_Changes" and "Changes_Only" versions,
				    -- so that complete paragraph deletions are obvious,
				    -- and also so that we can use revision bars rather than
				    -- displaying the changes in the RM version.
			            ARM_Output.Text_Format (Output_Object,
					        Bold => Format_Object.Is_Bold,
					        Italic => True,
					        Font => Format_Object.Font,
					        Change => Format_Object.Change,
					        Version => Format_Object.Current_Change_Version,
					        Added_Version => Format_Object.Current_Old_Change_Version,
					        Size => ARM_Output."-"(Format_Object.Size, 1),
					        Location => Format_Object.Location);
			            ARM_Output.Ordinary_Text (Output_Object,
				         Text => "This paragraph was deleted.");
			            ARM_Output.Text_Format (Output_Object,
					        Bold => Format_Object.Is_Bold,
					        Italic => Format_Object.Is_Italic,
					        Font => Format_Object.Font,
					        Change => Format_Object.Change,
					        Version => Format_Object.Current_Change_Version,
					        Added_Version => Format_Object.Current_Old_Change_Version,
					        Size => Format_Object.Size,
					        Location => Format_Object.Location);
			        when ARM_Format.Old_Only => null; -- Not deleted.
		            end case;
		        end if;
		    end if;
		    Format_Object.In_Paragraph := True;
		    Format_Object.Next_Paragraph_Change_Kind := ARM_Database.None;
		    Format_Object.Next_Paragraph_Version := '0';
		    Format_Object.Last_Non_Space := False;

		else -- No paragraph numbers in ISO version (or if the paragraph
		     -- number has been suppressed with @NoParaNum):
--Ada.Text_IO.Put_Line ("Check_Paragraph, no number: format= " & Paragraph_Type'Image(Format_Object.Next_Paragraph_Format_Type));
		    ARM_Output.Start_Paragraph (Output_Object,
				                Format => Format_Object.Format,
						Number => "",
						No_Prefix => Format_Object.No_Prefix,
						Tab_Stops => Format_Object.Paragraph_Tab_Stops,
						No_Breaks => Format_Object.No_Breaks or Format_Object.In_Bundle,
						Keep_with_Next => Format_Object.Keep_with_Next or Format_Object.In_Bundle,
						Space_After => Format_Object.Space_After);
		    Format_Object.In_Paragraph := True;
		    Format_Object.Next_Paragraph_Change_Kind := ARM_Database.None;
		    Format_Object.Next_Paragraph_Version := '0';
		    Format_Object.Current_Paragraph_Len := 0;
		    Format_Object.No_Para_Num := False;
		end if;

		if not Format_Object.No_Prefix then
		    if Format_Object.Next_Paragraph_Format_Type = Notes then
		        -- Output the note number.
		        declare
		            NNum : constant String := Integer'Image(Format_Object.Next_Note);
		        begin
		            ARM_Output.Ordinary_Text (Output_Object,
					              NNum(2..NNum'Last));
		            ARM_Output.Hard_Space (Output_Object);
		            ARM_Output.Hard_Space (Output_Object);
		            Format_Object.Next_Note := Format_Object.Next_Note + 1;
		        end;
		    elsif Format_Object.Next_Paragraph_Format_Type = Enumerated then
		        -- Output the item number.
		        declare
		            NNum : constant String := Integer'Image(Format_Object.Next_Enumerated_Num);
		        begin
		            ARM_Output.Ordinary_Text (Output_Object,
					              NNum(2..NNum'Last) & '.');
		            ARM_Output.End_Hang_Item (Output_Object);
		            Format_Object.Next_Enumerated_Num := Format_Object.Next_Enumerated_Num + 1;
		        end;
		    end if;
		else -- No prefix marked, meaning no number.
		    Format_Object.No_Prefix := False; -- Reset.
		end if;
		Format_Object.Last_Non_Space := False;

		Format_Object.Keep_with_Next := False; -- Reset for next paragraph.

		Format_Object.Space_After := ARM_Output.Normal; -- Reset for next paragraph.

		-- Output the annotation preface, if needed.
		if Format_Object.Next_Paragraph_Subhead_Type /=
		   Format_Object.Last_Paragraph_Subhead_Type then
		    Make_Annotation_Preface (Format_Object.Next_Paragraph_Subhead_Type);
		end if;

	    -- else already in a paragraph.
	    end if;
	end Check_Paragraph;


	procedure Check_End_Paragraph is
	    -- Check for the end of a paragraph; closing it if necessary.
	    -- We will never be in a paragraph after this routine.
	begin
	    if Format_Object.In_Paragraph then
		ARM_Output.End_Paragraph (Output_Object);
	        Format_Object.In_Paragraph := False;
	        Format_Object.No_Para_Num := False;
			-- Make sure any "leftover"
			-- NoParaNums are cleared; we don't want this lasting into
			-- the next paragraph.
		if Format_Object.In_Change then
		    Ada.Text_IO.Put_Line ("** Paragraph end while in change; line " & ARM_Input.Line_String (Input_Object));
		end if;
	    end if;
	end Check_End_Paragraph;


	type DIE_Kind is (None, Is_Root, Is_Partial);

	procedure Display_Index_Entry (Term_Text : in String;
				       Special : in DIE_Kind := None) is
	    -- If necessary, display the index entry for Term_Text.
	    Is_AARM : constant Boolean := Is_AARM_Paragraph (Format_Object.Next_Paragraph_Subhead_Type);
            use type ARM_Output.Size_Type;
	begin
	    if Format_Object.Display_Index_Entries then
	        Check_Paragraph;
		if not Is_AARM then
	            ARM_Output.Text_Format (Output_Object,
		       Bold => False, Italic => False,
		       Font => ARM_Output.Default, Size => -1,
		       Change => Format_Object.Change,
		       Version => Format_Object.Current_Change_Version,
		       Added_Version => Format_Object.Current_Old_Change_Version,
		       Location => ARM_Output.Normal);
		end if;
	        ARM_Output.Ordinary_Character (Output_Object, '{');
		if not Is_AARM then
	            ARM_Output.Text_Format (Output_Object,
		       Bold => False, Italic => True, Font => ARM_Output.Default,
		       Size => -1,
		       Change => Format_Object.Change,
		       Version => Format_Object.Current_Change_Version,
		       Added_Version => Format_Object.Current_Old_Change_Version,
		       Location => ARM_Output.Normal);
		else
	            ARM_Output.Text_Format (Output_Object,
		       Bold => False, Italic => True, Font => ARM_Output.Default,
		       Size => 0,
		       Change => Format_Object.Change,
		       Version => Format_Object.Current_Change_Version,
		       Added_Version => Format_Object.Current_Old_Change_Version,
		       Location => ARM_Output.Normal);
		end if;
	        ARM_Output.Ordinary_Text (Output_Object, ARM_Index.Clean(Term_Text));
		if not Is_AARM then
	            ARM_Output.Text_Format (Output_Object,
		       Bold => False, Italic => False, Font => ARM_Output.Default,
		       Size => -1,
		       Change => Format_Object.Change,
		       Version => Format_Object.Current_Change_Version,
		       Added_Version => Format_Object.Current_Old_Change_Version,
		       Location => ARM_Output.Normal);
		else
	            ARM_Output.Text_Format (Output_Object,
		       Bold => False, Italic => False, Font => ARM_Output.Default,
		       Size => 0,
		       Change => Format_Object.Change,
		       Version => Format_Object.Current_Change_Version,
		       Added_Version => Format_Object.Current_Old_Change_Version,
		       Location => ARM_Output.Normal);
		end if;
		case Special is
		    when None => null;
		    when Is_Root => ARM_Output.Ordinary_Text (Output_Object, " [distributed]");
		    when Is_Partial => ARM_Output.Ordinary_Text (Output_Object, " [partial]");
		end case;
	        ARM_Output.Ordinary_Character (Output_Object, '}');
		if not Is_AARM then
	            ARM_Output.Text_Format (Output_Object,
		       Bold => False, Italic => False, Font => ARM_Output.Default,
		       Size => 0,
		       Change => Format_Object.Change,
		       Version => Format_Object.Current_Change_Version,
		       Added_Version => Format_Object.Current_Old_Change_Version,
		       Location => ARM_Output.Normal);
		end if;
	        ARM_Output.Ordinary_Character (Output_Object, ' ');
		Format_Object.Last_Non_Space := False;
	    -- else not displayed.
	    end if;
	end Display_Index_Entry;


	procedure Parse_Tab_Stops (Stops : in String;
				   Tabs : in out ARM_Output.Tab_Info) is
	    -- Parse "Stops" as a tab string; store the result in Tabs.
	    Loc : Natural := Stops'First;
	begin
	    -- Parse tab stops:
	    -- <tab_stop> ::= <modifier><pica_count_literal>
	    -- <modifier> ::= L | P
	    -- <tab_stops> ::= <tab_stop> {, <tab_stop>}
	    while Loc <= Stops'Length loop
		Tabs.Number := Tabs.Number + 1;
		if Stops(Loc) = 'l' or Stops(Loc) = 'L' then
		    Tabs.Stops(Tabs.Number).Kind :=
			ARM_Output.Left_Fixed;
		    Loc := Loc + 1;
		elsif Stops(Loc) = 'p' or Stops(Loc) = 'P' then
		    Tabs.Stops(Tabs.Number).Kind :=
			ARM_Output.Left_Proportional;
		    Loc := Loc + 1;
		else -- Default:
		    Tabs.Stops(Tabs.Number).Kind :=
			ARM_Output.Left_Fixed;
		end if;
		while Loc <= Stops'Length loop
		    if Stops(Loc) in '0' .. '9' then
		        Tabs.Stops(Tabs.Number).Stop :=
		            Tabs.Stops(Tabs.Number).Stop * 10 +
			    Character'Pos(Stops(Loc)) - Character'Pos('0');
			Loc := Loc + 1;
		    else
			exit; -- Number ended.
		    end if;
		end loop;
		if Tabs.Stops(Tabs.Number).Stop = 0 then
		    Tabs.Number := Tabs.Number - 1;
		    Ada.Text_IO.Put_Line ("  ** Bad tab stop format, position" & Natural'Image(Loc) &
			" in [" & Stops(1..Stops'Length) & "] from line " &
		        ARM_Input.Line_String (Input_Object));
		    exit; -- Give up on this tabset.
		elsif Tabs.Number < 1 and then
			Tabs.Stops(Tabs.Number-1).Stop >=
			Tabs.Stops(Tabs.Number).Stop then
		    Tabs.Number := Tabs.Number - 1;
		    Ada.Text_IO.Put_Line ("  ** Bad tab stop, less than previous, at position" & Natural'Image(Loc) &
			" in [" & Stops(1..Stops'Length) & "] from line " &
		        ARM_Input.Line_String (Input_Object));
		    exit; -- Give up on this tabset.
		end if;
		if Loc > Stops'Length then
		    exit; -- Finished.
		elsif Stops(Loc) = ',' then
		    Loc := Loc + 1;
		    if Loc > Stops'Length then
		        Ada.Text_IO.Put_Line ("  ** Bad tab stop set format, ends with comma in [" &
			    Stops(1..Stops'Length) & "] from line " &
		            ARM_Input.Line_String (Input_Object));
		        exit; -- Give up on this tabset.
		    end if;
	        end if;
		-- Skip any blanks in between.
		while Loc <= Stops'Length and then Stops(Loc) = ' ' loop
		    Loc := Loc + 1;
		end loop;
	    end loop;
	end Parse_Tab_Stops;


	procedure Process_Begin is
	    -- Process a "begin". The "begin" has been stacked.

	    use type ARM_Format.Document_Type;

	    procedure Toss_for_RM (Paragraph_Kind_Name : in String) is
		-- Call this for AARM-only sections.
		-- It skips *everything* until the matching end. This includes
		-- index references, section references, and the like. Anything
		-- that ought to be in the RM should be moved outside of the
		-- AARM specific code. Thus, we can use a fairly simple text
		-- skip.
		Ch : Character;
		Close_Ch : Character;
		Command_Name : ARM_Input.Command_Name_Type;
	    begin
		-- Skip everything up to the next @end{<Paragraph_Kind_Name...
		-- then pop the stack and return.
		loop
		    ARM_Input.Get_Char (Input_Object, Ch);
		    while Ch /= '@' loop
			ARM_Input.Get_Char (Input_Object, Ch);
		    end loop;
	            Arm_Input.Get_Name (Input_Object, Command_Name, Null_Name_Allowed => True);
			-- Get the command name.
		    if Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
		    	Command_Name, Ada.Strings.Right)) /= "end" then
			-- Not an "end" command, keep going.
		        null;
		    else -- An End command, check if this is the one we want:
			ARM_Input.Get_Char (Input_Object, Ch);
			if ARM_Input.Is_Open_Char (Ch) then -- Start parameter:
			    Close_Ch := ARM_Input.Get_Close_Char (Ch);
			    Arm_Input.Get_Name (Input_Object, Command_Name); -- Get the end "type".
			    if Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
			    	Command_Name, Ada.Strings.Right)) /= Paragraph_Kind_Name then
				null; -- Wrong end, keep going.
			    else -- Right end!
				-- Skip to the close character:
			        while Ch /= Close_Ch loop
				    ARM_Input.Get_Char (Input_Object, Ch);
			        end loop;
				-- Unstack the "begin".
			        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (End AARM-Only)");
				-- And we're done with this "begin".
				return;
			    end if;
			else -- No parameter, forget it.
			    null;
		        end if;
		    end if;
		end loop;
	    end Toss_for_RM;

	begin
	    Check_End_Paragraph; -- End any paragraph that we're in.
	    if Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "comment" then
		Toss_for_RM ("comment");
	    -- Format only:
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "wide" then
		Format_Object.Next_Paragraph_Format_Type := Wide;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "example" then
		Format_Object.Next_Paragraph_Format_Type := Example_Text;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "descexample" then
		Format_Object.Next_Paragraph_Format_Type := Indented_Example_Text;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "describecode" then
		Format_Object.Next_Paragraph_Format_Type := Code_Indented;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "itemize" then
		Format_Object.Next_Paragraph_Format_Type := Bulleted;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "inneritemize" then
		Format_Object.Next_Paragraph_Format_Type := Nested_Bulleted;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "display" then
		Format_Object.Next_Paragraph_Format_Type := Display;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "syntaxdisplay" then
		Format_Object.Next_Paragraph_Format_Type := Syntax_Display;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "syntaxtext" then
	        Format_Object.Next_Paragraph_Format_Type := Syntax_Indented;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "description" then
		Format_Object.Next_Paragraph_Format_Type := Hanging_Indented;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "enumerate" then
		Format_Object.Next_Paragraph_Format_Type := Enumerated;
		Format_Object.Next_Enumerated_Num := 1;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "bundle" then
		-- Should prevent any page breaks until the "end". Not
		-- implemented currently.
	        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Is_Formatting
		    := False; -- Leave the format alone.
		Format_Object.In_Bundle := True; -- We don't need to stack this,
		    -- because once we're in it, we can't leave it until the @End.
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "twocol" then
		-- Two column; no affect on format.
		ARM_Output.Set_Columns (Output_Object, Number_of_Columns => 2);
	        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Is_Formatting
		    := False;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "fourcol" then
		-- Four column; no affect on format.
		ARM_Output.Set_Columns (Output_Object, Number_of_Columns => 4);
	        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Is_Formatting
		    := False;
	    -- RM groupings:
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "intro" then
		Format_Object.Next_Paragraph_Format_Type := Introduction;
		Format_Object.Next_Paragraph_Subhead_Type := Introduction;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "syntax" then
		Format_Object.Next_Paragraph_Format_Type := Syntax;
		Format_Object.Next_Paragraph_Subhead_Type := Syntax;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "resolution" then
		Format_Object.Next_Paragraph_Format_Type := Resolution;
		Format_Object.Next_Paragraph_Subhead_Type := Resolution;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "legality" then
		Format_Object.Next_Paragraph_Format_Type := Legality;
		Format_Object.Next_Paragraph_Subhead_Type := Legality;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "staticsem" then
		Format_Object.Next_Paragraph_Format_Type := Static_Semantics;
		Format_Object.Next_Paragraph_Subhead_Type := Static_Semantics;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "linktime" then
		Format_Object.Next_Paragraph_Format_Type := Link_Time;
		Format_Object.Next_Paragraph_Subhead_Type := Link_Time;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "runtime" then
		Format_Object.Next_Paragraph_Format_Type := Run_Time;
		Format_Object.Next_Paragraph_Subhead_Type := Run_Time;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "bounded" then
		Format_Object.Next_Paragraph_Format_Type := Bounded_Errors;
		Format_Object.Next_Paragraph_Subhead_Type := Bounded_Errors;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "erron" then
		Format_Object.Next_Paragraph_Format_Type := Erroneous;
		Format_Object.Next_Paragraph_Subhead_Type := Erroneous;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "implreq" then
		Format_Object.Next_Paragraph_Format_Type := Requirements;
		Format_Object.Next_Paragraph_Subhead_Type := Requirements;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "docreq" then
		Format_Object.Next_Paragraph_Format_Type := Documentation;
		Format_Object.Next_Paragraph_Subhead_Type := Documentation;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "metrics" then
		Format_Object.Next_Paragraph_Format_Type := Metrics;
		Format_Object.Next_Paragraph_Subhead_Type := Metrics;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "implperm" then
		Format_Object.Next_Paragraph_Format_Type := Permissions;
		Format_Object.Next_Paragraph_Subhead_Type := Permissions;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "impladvice" then
		Format_Object.Next_Paragraph_Format_Type := Advice;
		Format_Object.Next_Paragraph_Subhead_Type := Advice;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "examples" then
		Format_Object.Next_Paragraph_Format_Type := Examples;
		Format_Object.Next_Paragraph_Subhead_Type := Examples;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "notes" then
		Format_Object.Next_Paragraph_Format_Type := Notes;
		Format_Object.Next_Paragraph_Subhead_Type := Notes;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "rmonly" then
		if Format_Object.Document /= ARM_Format.AARM then
	            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Is_Formatting
		        := False; -- Leave the format alone.
		else -- AARM, but this is RM-only.
		    Toss_for_RM ("rmonly");
		end if;
	    -- NotISO text:
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "notiso" then
		if Format_Object.Document = ARM_Format.RM_ISO then
		    Toss_for_RM ("notiso"); -- This text does not appear in ISO documents.
		else -- RM.
	            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Is_Formatting
		        := False; -- Leave the format alone.
		end if;
	    -- ISOOnly text:
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "isoonly" then
		if Format_Object.Document /= ARM_Format.RM_ISO then
		    Toss_for_RM ("isoonly"); -- This text does not appear in non-ISO documents.
		else -- ISO RM.
	            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Is_Formatting
		        := False; -- Leave the format alone.
		end if;
	    -- AARM groupings:
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "metarules" then
		if Format_Object.Document = ARM_Format.AARM then
		    Format_Object.Next_Paragraph_Format_Type := Language_Design;
		    Format_Object.Next_Paragraph_Subhead_Type := Language_Design;
		else -- RM, but this is AARM-only.
		    Toss_for_RM ("metarules");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "inconsistent83" then
		if Format_Object.Document = ARM_Format.AARM then
		    Format_Object.Next_Paragraph_Format_Type := Ada83_Inconsistencies;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada83_Inconsistencies;
		else -- RM, but this is AARM-only.
		    Toss_for_RM ("inconsistent83");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "incompatible83" then
		if Format_Object.Document = ARM_Format.AARM then
		    Format_Object.Next_Paragraph_Format_Type := Ada83_Incompatibilities;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada83_Incompatibilities;
		else -- RM, but this is AARM-only.
		    Toss_for_RM ("incompatible83");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "extend83" then
		if Format_Object.Document = ARM_Format.AARM then
		    Format_Object.Next_Paragraph_Format_Type := Ada83_Extensions;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada83_Extensions;
		else -- RM, but this is AARM-only.
		    Toss_for_RM ("extend83");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "diffword83" then
		if Format_Object.Document = ARM_Format.AARM then
		    Format_Object.Next_Paragraph_Format_Type := Ada83_Wording;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada83_Wording;
		else -- RM, but this is AARM-only.
		    Toss_for_RM ("diffword83");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "inconsistent95" then
		if Format_Object.Document = ARM_Format.AARM then
		    Format_Object.Next_Paragraph_Format_Type := Ada95_Inconsistencies;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada95_Inconsistencies;
		else -- RM, but this is AARM-only.
		    Toss_for_RM ("inconsistent95");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "incompatible95" then
		if Format_Object.Document = ARM_Format.AARM then
		    Format_Object.Next_Paragraph_Format_Type := Ada95_Incompatibilities;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada95_Incompatibilities;
		else -- RM, but this is AARM-only.
		    Toss_for_RM ("incompatible95");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "extend95" then
		if Format_Object.Document = ARM_Format.AARM then
		    Format_Object.Next_Paragraph_Format_Type := Ada95_Extensions;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada95_Extensions;
		else -- RM, but this is AARM-only.
		    Toss_for_RM ("extend95");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "diffword95" then
		if Format_Object.Document = ARM_Format.AARM then
		    Format_Object.Next_Paragraph_Format_Type := Ada95_Wording;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada95_Wording;
		else -- RM, but this is AARM-only.
		    Toss_for_RM ("diffword95");
		end if;
	    -- AARM annotations:
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "discussion" then
		if Format_Object.Document = ARM_Format.AARM then
		    Format_Object.Next_Paragraph_Format_Type := Discussion;
		    Format_Object.Next_Paragraph_Subhead_Type := Discussion;
		else -- RM, but this is AARM-only.
		    Toss_for_RM ("discussion");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "reason" then
		if Format_Object.Document = ARM_Format.AARM then
		    Format_Object.Next_Paragraph_Format_Type := Reason;
		    Format_Object.Next_Paragraph_Subhead_Type := Reason;
		else -- RM, but this is AARM-only.
		    Toss_for_RM ("reason");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "ramification" then
		if Format_Object.Document = ARM_Format.AARM then
		    Format_Object.Next_Paragraph_Format_Type := Ramification;
		    Format_Object.Next_Paragraph_Subhead_Type := Ramification;
		else -- RM, but this is AARM-only.
		    Toss_for_RM ("ramification");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "theproof" then
		if Format_Object.Document = ARM_Format.AARM then
		    Format_Object.Next_Paragraph_Format_Type := Proof;
		    Format_Object.Next_Paragraph_Subhead_Type := Proof;
		else -- RM, but this is AARM-only.
		    Toss_for_RM ("theproof");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "implnote" then
		if Format_Object.Document = ARM_Format.AARM then
		    Format_Object.Next_Paragraph_Format_Type := Imp_Note;
		    Format_Object.Next_Paragraph_Subhead_Type := Imp_Note;
		else -- RM, but this is AARM-only.
		    Toss_for_RM ("implnote");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "honest" then
		if Format_Object.Document = ARM_Format.AARM then
		    Format_Object.Next_Paragraph_Format_Type := Honest;
		    Format_Object.Next_Paragraph_Subhead_Type := Honest;
		else -- RM, but this is AARM-only.
		    Toss_for_RM ("honest");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "glossarymarker" then
		if Format_Object.Document = ARM_Format.AARM then
		    Format_Object.Next_Paragraph_Format_Type := Glossary_Marker;
		    Format_Object.Next_Paragraph_Subhead_Type := Glossary_Marker;
		else -- RM, but this is AARM-only.
		    Toss_for_RM ("glossarymarker");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "aarmonly" then
		if Format_Object.Document = ARM_Format.AARM then
		    null; -- Leave the format alone.
		else -- RM, but this is AARM-only.
		    Toss_for_RM ("aarmonly");
		end if;
	    else
		Ada.Text_IO.Put_Line ("  -- Unknown 'begin' type - " &
		    Ada.Strings.Fixed.Trim (Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right) &
		    " on line " & ARM_Input.Line_String (Input_Object));
	    end if;
	end Process_Begin;


	procedure Process_Command_with_Parameter is
	    -- Process the start of a command with a parameter.
	    -- The parameter character has been processed, and
	    -- a stack item pushed.

	    function Get_NT return String is
	        -- Local routine:
	        -- Return the "current" non-terminal from
	        -- the Syntax_NT string. Handles @Chg.
		New_Pos : Natural;
	        Close_Ch : Character;
	        Open_Cnt : Natural;
	    begin
	        if Format_Object.Syntax_NT_Len < 11 or else
		   Format_Object.Syntax_NT (1) /= '@' or else
		   Ada.Characters.Handling.To_Lower (Format_Object.Syntax_NT (2 .. 4)) /= "chg" then
		    -- No @Chg command here.
		    return Format_Object.Syntax_NT (1 .. Format_Object.Syntax_NT_Len);
		end if;
		if Ada.Characters.Handling.To_Lower (Format_Object.Syntax_NT (6 .. 9)) = "new=" then
		    -- No version parameter:
		    New_Pos := 6;
		elsif Format_Object.Syntax_NT_Len > 22 and then
		    Ada.Characters.Handling.To_Lower (Format_Object.Syntax_NT (6 .. 14)) = "version=[" and then
		    Ada.Characters.Handling.To_Lower (Format_Object.Syntax_NT (16 .. 21)) = "],new=" then
		    New_Pos := 18;
		else
Ada.Text_IO.Put_Line ("%% Oops, can't either Version or New in NT chg command, line " & ARM_Input.Line_String (Input_Object));
		    return Format_Object.Syntax_NT (1 .. Format_Object.Syntax_NT_Len);
		end if;
	        if Format_Object.Changes = Old_Only then
		    -- Find the end of the "New" parameter, and
		    -- return it.
		    Close_Ch := ARM_Input.Get_Close_Char (
		        Format_Object.Syntax_NT(New_Pos+4));
		    Open_Cnt := 1;
		    for I in New_Pos+5 .. Format_Object.Syntax_NT_Len loop
		        if Format_Object.Syntax_NT(I) = Format_Object.Syntax_NT(New_Pos+4) then
			    Open_Cnt := Open_Cnt + 1;
		        elsif Format_Object.Syntax_NT(I) = Close_Ch then
			    if Open_Cnt <= 1 then
			        -- OK, the end of the "New" parameter is at 'I'.
			        if Format_Object.Syntax_NT_Len < I+7 or else
				   Format_Object.Syntax_NT (I+1) /= ',' or else
				   Ada.Characters.Handling.To_Lower (Format_Object.Syntax_NT (I+2 .. I+4)) /= "old" or else
				   Format_Object.Syntax_NT (I+5) /= '=' then
				    exit; -- Heck if I know.
			        end if;
			        Close_Ch := ARM_Input.Get_Close_Char (
				    Format_Object.Syntax_NT(I+6));
			        Open_Cnt := 1;
			        for J in I+7 .. Format_Object.Syntax_NT_Len loop
				    if Format_Object.Syntax_NT(J) = Format_Object.Syntax_NT(I+6) then
				        Open_Cnt := Open_Cnt + 1;
				    elsif Format_Object.Syntax_NT(J) = Close_Ch then
				        if Open_Cnt <= 1 then
					    return Format_Object.Syntax_NT (I + 7 .. J - 1);
				        else
					    Open_Cnt := Open_Cnt - 1;
				        end if;
				    -- else continue looking.
				    end if;
			        end loop;
Ada.Text_IO.Put_Line ("%% Oops, can't find end of NT chg old command, line " & ARM_Input.Line_String (Input_Object));
			        return Format_Object.Syntax_NT (I + 7 .. Format_Object.Syntax_NT_Len);
			    else
			        Open_Cnt := Open_Cnt - 1;
			    end if;
		        -- else continue looking.
		        end if;
		    end loop;
Ada.Text_IO.Put_Line ("%% Oops, can't find end of NT chg new command, line " & ARM_Input.Line_String (Input_Object));
		    return Format_Object.Syntax_NT (New_Pos+5 .. Format_Object.Syntax_NT_Len);
	        else -- Some new format, use the new name.
		    -- Find the end of the "New" parameter, and
		    -- return it.
		    Close_Ch := ARM_Input.Get_Close_Char (
		        Format_Object.Syntax_NT(New_Pos+4));
		    Open_Cnt := 1;
		    for I in New_Pos+5 .. Format_Object.Syntax_NT_Len loop
		        if Format_Object.Syntax_NT(I) = Format_Object.Syntax_NT(New_Pos+4) then
			    Open_Cnt := Open_Cnt + 1;
		        elsif Format_Object.Syntax_NT(I) = Close_Ch then
			    if Open_Cnt <= 1 then
			        return Format_Object.Syntax_NT (New_Pos+5 .. I - 1);
			    else
			        Open_Cnt := Open_Cnt - 1;
			    end if;
		        -- else continue looking.
		        end if;
		    end loop;
		    -- Weird if we get here, can't find end of parameter.
Ada.Text_IO.Put_Line ("%% Oops, can't find end of NT chg new command, line " & ARM_Input.Line_String (Input_Object));
		    return Format_Object.Syntax_NT (New_Pos+5 .. Format_Object.Syntax_NT_Len);
	        end if;
	    end Get_NT;


	    procedure Get_Change_Version (Is_First : in Boolean;
					  Version : out Character) is
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
		    ARM_Input.Get_Char (Input_Object, Version);
		    ARM_Input.Get_Char (Input_Object, Ch);
		    if Ch /= Close_Ch then
			Ada.Text_IO.Put_Line ("  ** Bad close for change version on line " & ARM_Input.Line_String (Input_Object));
			ARM_Input.Replace_Char (Input_Object);
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
		    ARM_Input.Get_Char (Input_Object, Ch);
		    if Ch /= Close_Ch then
			Ada.Text_IO.Put_Line ("  ** Bad close for change kind on line " & ARM_Input.Line_String (Input_Object));
			ARM_Input.Replace_Char (Input_Object);
		    end if;
		    if Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Kind_Name, Ada.Strings.Right)) =
			"revised" then
			Kind := ARM_Database.Revised;
		    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Kind_Name, Ada.Strings.Right)) =
			"added" then
			Kind := ARM_Database.Inserted;
		    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Kind_Name, Ada.Strings.Right)) =
			"addednormal" then
			Kind := ARM_Database.Inserted_Normal_Number;
		    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Kind_Name, Ada.Strings.Right)) =
			"deleted" then
			Kind := ARM_Database.Deleted;
		    else
			Ada.Text_IO.Put_Line ("  ** Bad kind for change kind: " &
				Ada.Strings.Fixed.Trim (Kind_Name, Ada.Strings.Right) &
				" on line " & ARM_Input.Line_String (Input_Object));
		    end if;
		-- else no parameter. Weird.
		end if;
	    end Get_Change_Kind;


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
		    -- Get the version character:
		    ARM_Input.Get_Char (Input_Object, Ch);
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


	    procedure Gen_Ref_or_ARef_Parameter (Display_It : Boolean) is
		-- Generate (and read) a "Ref" or "ARef" parameter, containing
		-- a DR or AI reference. Generate it into the document only
		-- if Display_It is True.
		Ch, Close_Ch : Character;
		Ref_Name : ARM_Input.Command_Name_Type;
		Len : Natural;
		Is_Ref : Boolean;
		AI_Number : String(1..5);
	    begin
	        ARM_Input.Check_One_of_Parameter_Names (Input_Object,
		    Param_Name_1 => "Ref" & (4..ARM_Input.Command_Name_Type'Last => ' '),
		    Param_Name_2 => "ARef" & (5..ARM_Input.Command_Name_Type'Last => ' '),
		    Is_First => False,
		    Is_Param_1 => Is_Ref,
		    Param_Close_Bracket => Close_Ch);
	        if Close_Ch /= ' ' then
		    -- Get the reference:
		    Len := 0;
		    loop
		        ARM_Input.Get_Char (Input_Object, Ch);
		        if Ch /= Close_Ch then
			    Len := Len + 1;
			    Ref_Name(Len) := Ch;
		        else -- End of the reference.
			    if Len = 0 then
			        Ada.Text_IO.Put_Line ("  ** Failed to find reference on line " & ARM_Input.Line_String (Input_Object));
			    end if;
			    exit;
		        end if;
		    end loop;
		    if Display_It then
		        if Is_Ref then
			    -- Output a DR reference.
			    Check_Paragraph;
			    ARM_Output.Ordinary_Character (Output_Object, '{');
			    ARM_Output.Text_Format (Output_Object,
						    Bold => Format_Object.Is_Bold,
						    Italic => True,
						    Font => Format_Object.Font,
						    Size => Format_Object.Size,
						    Change => Format_Object.Change,
					            Version => Format_Object.Current_Change_Version,
					            Added_Version => Format_Object.Current_Old_Change_Version,
						    Location => Format_Object.Location);
			    ARM_Output.DR_Reference (Output_Object,
						     Text => Ref_Name(1..Len),
						     DR_Number => Ref_Name(1..Len));
			    ARM_Output.Text_Format (Output_Object,
						    Bold => Format_Object.Is_Bold,
						    Italic => Format_Object.Is_Italic,
						    Font => Format_Object.Font,
						    Size => Format_Object.Size,
						    Change => Format_Object.Change,
					            Version => Format_Object.Current_Change_Version,
					            Added_Version => Format_Object.Current_Old_Change_Version,
						    Location => Format_Object.Location);
			    ARM_Output.Ordinary_Character (Output_Object, '}');
			    ARM_Output.Ordinary_Character (Output_Object, ' ');
			    Format_Object.Last_Non_Space := False;
		        else
			    -- Calculate the "folded" AI number.
			    declare
			        Hyphen_1 : Natural := Ada.Strings.Fixed.Index (Ref_Name(1..Len), "-");
			        -- Should be "AIzz-00xxx-yy", where -yy and zz are
			        -- optional.
			    begin
			        if Hyphen_1 = 0 or else Len < Hyphen_1+5 then
				    AI_Number := "00001";
				    Ada.Text_IO.Put_Line ("** Bad AI reference " & Ref_Name(1..Len) & ", line " & ARM_Input.Line_String (Input_Object));
			        elsif Len = Hyphen_1+5 then -- No alternative number.
				    AI_Number := Ref_Name(Hyphen_1+1 .. Hyphen_1+5);
			        elsif Len < Hyphen_1+8 or else Ref_Name(Hyphen_1+6) /= '-' then
				    AI_Number := Ref_Name(Hyphen_1+1 .. Hyphen_1+5);
				    Ada.Text_IO.Put_Line ("** Bad AI alternative reference " & Ref_Name(1..Len) & ", line " & ARM_Input.Line_String (Input_Object));
			        elsif Ref_Name(Hyphen_1+7) = '0' then
				    AI_Number := Ref_Name(Hyphen_1+1 .. Hyphen_1+5);
				    AI_Number(1) := Character'Pred(Ref_Name(Hyphen_1+8));
			        elsif Ref_Name(Hyphen_1+7) = '1' then
				    AI_Number := Ref_Name(Hyphen_1+1 .. Hyphen_1+5);
				    if Ref_Name(Hyphen_1+8) = '0' then
				        AI_Number(1) := '9';
				    else
				        AI_Number(1) := Character'Val(Character'Pos(Ref_Name(Hyphen_1+8)) - Character'Pos('1') + Character'Pos('A'));
				    end if;
			        elsif Ref_Name(Hyphen_1+7) = '2' then
				    AI_Number := Ref_Name(Hyphen_1+1 .. Hyphen_1+5);
				    AI_Number(1) := Character'Val(Character'Pos(Ref_Name(Hyphen_1+8)) - Character'Pos('1') + Character'Pos('A') + 10);
			        else
				    AI_Number := Ref_Name(Hyphen_1+1 .. Hyphen_1+5);
				    Ada.Text_IO.Put_Line ("** Bad AI alternative reference " & Ref_Name(1..Len) & ", line " & ARM_Input.Line_String (Input_Object));
			        end if;
			    end;

			    -- Output an AI reference.
			    Check_Paragraph;
			    ARM_Output.Ordinary_Character (Output_Object, '{');
			    ARM_Output.Text_Format (Output_Object,
						    Bold => Format_Object.Is_Bold,
						    Italic => True,
						    Font => Format_Object.Font,
						    Size => Format_Object.Size,
						    Change => Format_Object.Change,
					            Version => Format_Object.Current_Change_Version,
					            Added_Version => Format_Object.Current_Old_Change_Version,
						    Location => Format_Object.Location);
			    ARM_Output.AI_Reference (Output_Object,
						     Text => Ref_Name(1..Len),
						     AI_Number => AI_Number);
			    ARM_Output.Text_Format (Output_Object,
						    Bold => Format_Object.Is_Bold,
						    Italic => Format_Object.Is_Italic,
						    Font => Format_Object.Font,
						    Size => Format_Object.Size,
						    Change => Format_Object.Change,
					            Version => Format_Object.Current_Change_Version,
					            Added_Version => Format_Object.Current_Old_Change_Version,
						    Location => Format_Object.Location);
			    ARM_Output.Ordinary_Character (Output_Object, '}');
			    ARM_Output.Ordinary_Character (Output_Object, ' ');
			    Format_Object.Last_Non_Space := False;
		        end if;
	            -- else don't display it.
		    end if;
	        -- else no parameter. Weird.
	        end if;
	    end Gen_Ref_or_ARef_Parameter;

	begin
	    case Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command is

		-- Basic text commands:

		when Redundant =>
		    if Format_Object.Document = ARM_Format.AARM then
			Check_Paragraph;
		        ARM_Output.Ordinary_Character (Output_Object, '[');
			Format_Object.Last_Non_Space := True;
		    -- else ignored in the RM.
		    end if;

		when Comment | Part =>
		    -- Skip the contents of this command.
		    -- For Part, we don't use the information contained,
		    -- but it would help a human reader.
		    ARM_Input.Skip_until_Close_Char (Input_Object,
			Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char);
		    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		        -- Remove the "comment" or "part" record.

		when Bold =>
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Bold => True,
					    Italic => Format_Object.Is_Italic,
					    Font => Format_Object.Font,
					    Size => Format_Object.Size,
					    Change => Format_Object.Change,
				            Version => Format_Object.Current_Change_Version,
				            Added_Version => Format_Object.Current_Old_Change_Version,
					    Location => Format_Object.Location);
		    Format_Object.Is_Bold := True;

		when Italic =>
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Bold => Format_Object.Is_Bold,
					    Italic => True,
					    Font => Format_Object.Font,
					    Size => Format_Object.Size,
					    Change => Format_Object.Change,
				            Version => Format_Object.Current_Change_Version,
				            Added_Version => Format_Object.Current_Old_Change_Version,
					    Location => Format_Object.Location);
		    Format_Object.Is_Italic := True;

		when Roman =>
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Bold => Format_Object.Is_Bold,
					    Italic => Format_Object.Is_Italic,
					    Font => ARM_Output.Roman,
					    Size => Format_Object.Size,
					    Change => Format_Object.Change,
				            Version => Format_Object.Current_Change_Version,
				            Added_Version => Format_Object.Current_Old_Change_Version,
					    Location => Format_Object.Location);
		    Format_Object.Font := ARM_Output.Roman;

		when Swiss =>
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Bold => Format_Object.Is_Bold,
					    Italic => Format_Object.Is_Italic,
					    Font => ARM_Output.Swiss,
					    Size => Format_Object.Size,
					    Change => Format_Object.Change,
				            Version => Format_Object.Current_Change_Version,
				            Added_Version => Format_Object.Current_Old_Change_Version,
					    Location => Format_Object.Location);
		    Format_Object.Font := ARM_Output.Swiss;

		when Fixed =>
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Bold => Format_Object.Is_Bold,
					    Italic => Format_Object.Is_Italic,
					    Font => ARM_Output.Fixed,
					    Size => Format_Object.Size,
					    Change => Format_Object.Change,
				            Version => Format_Object.Current_Change_Version,
				            Added_Version => Format_Object.Current_Old_Change_Version,
					    Location => Format_Object.Location);
		    Format_Object.Font := ARM_Output.Fixed;

		when Roman_Italic =>
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Bold => Format_Object.Is_Bold,
					    Italic => True,
					    Font => ARM_Output.Roman,
					    Size => Format_Object.Size,
					    Change => Format_Object.Change,
				            Version => Format_Object.Current_Change_Version,
				            Added_Version => Format_Object.Current_Old_Change_Version,
					    Location => Format_Object.Location);
		    Format_Object.Font := ARM_Output.Roman;
		    Format_Object.Is_Italic := True;

		when Shrink =>
		    declare
			use type ARM_Output.Size_Type;
		    begin
		        Check_Paragraph;
		        ARM_Output.Text_Format (Output_Object,
					        Bold => Format_Object.Is_Bold,
					        Italic => Format_Object.Is_Italic,
					        Font => Format_Object.Font,
					        Size => Format_Object.Size-1,
					        Change => Format_Object.Change,
				                Version => Format_Object.Current_Change_Version,
				                Added_Version => Format_Object.Current_Old_Change_Version,
					        Location => Format_Object.Location);
		        Format_Object.Size := Format_Object.Size - 1;
		    end;

		when Grow =>
		    declare
			use type ARM_Output.Size_Type;
		    begin
		        Check_Paragraph;
		        ARM_Output.Text_Format (Output_Object,
					        Bold => Format_Object.Is_Bold,
					        Italic => Format_Object.Is_Italic,
					        Font => Format_Object.Font,
					        Size => Format_Object.Size+1,
					        Change => Format_Object.Change,
				                Version => Format_Object.Current_Change_Version,
				                Added_Version => Format_Object.Current_Old_Change_Version,
					        Location => Format_Object.Location);
		        Format_Object.Size := Format_Object.Size + 1;
		    end;

		when Keyword =>
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Bold => True,
					    Italic => Format_Object.Is_Italic,
					    Font => Format_Object.Font,
					    Size => Format_Object.Size,
					    Change => Format_Object.Change,
				            Version => Format_Object.Current_Change_Version,
				            Added_Version => Format_Object.Current_Old_Change_Version,
					    Location => Format_Object.Location);
		    Format_Object.Is_Bold := True;

		when Non_Terminal =>
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Bold => Format_Object.Is_Bold,
					    Italic => Format_Object.Is_Italic,
					    Font => ARM_Output.Swiss,
					    Size => Format_Object.Size,
					    Change => Format_Object.Change,
				            Version => Format_Object.Current_Change_Version,
				            Added_Version => Format_Object.Current_Old_Change_Version,
					    Location => Format_Object.Location);
		    Format_Object.Font := ARM_Output.Swiss;

		when Tab_Clear =>
		    Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;

		when Tab_Set =>
		    if Format_Object.Next_Paragraph_Format_Type = Bulleted or else
		       Format_Object.Next_Paragraph_Format_Type = Nested_Bulleted or else
		       Format_Object.Next_Paragraph_Format_Type = Enumerated or else
		       Format_Object.Next_Paragraph_Format_Type = Hanging_Indented then
		        Ada.Text_IO.Put_Line ("  ** Tab set in hang or bulleted format: " &
			    Paragraph_Type'Image(Format_Object.Next_Paragraph_Format_Type) &
			    ", line " & ARM_Input.Line_String (Input_Object));
		    elsif ARM_Output."/=" (Format_Object.Paragraph_Tab_Stops, ARM_Output.NO_TABS) then
		        Ada.Text_IO.Put_Line ("  ** Setting tabs when they are not clear on line "
				& ARM_Input.Line_String (Input_Object));
		    else
			declare
			    My_Tabs : ARM_Output.Tab_Info := ARM_Output.NO_TABS;
			    Stops : String(1..80);
			    Len : Natural;
			begin
		            ARM_Input.Copy_to_String_until_Close_Char (
			        Input_Object,
			        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char,
			        Stops, Len);
			    Parse_Tab_Stops (Stops(1..Len), My_Tabs);

			    Format_Object.Paragraph_Tab_Stops := My_Tabs;
			    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Tabstop)");
			end;
		    end if;

		-- Tables:
		when Table =>
			-- @table(Columns=<number>,
			--       Caption=<text>,
			--       Headers=<text>,
			--       Body=<row_text>)
			-- Columns must be a single digit (2-9).
			-- Caption defines the table caption.
			-- Headers defines the table headers.
			-- Body defines the table body.

		    Check_End_Paragraph; -- End any paragraph we're in.
		    declare
			Close_Ch, Ch : Character;
		    begin
		        ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Columns" & (8..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => True,
			    Param_Close_Bracket => Close_Ch);
		        if Close_Ch /= ' ' then
			    ARM_Input.Get_Char (Input_Object, Ch);
			    -- Set to the table format:
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Last_Subhead_Paragraph := Format_Object.Last_Paragraph_Subhead_Type;
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Subhead_Paragraph := Format_Object.Next_Paragraph_Subhead_Type;
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Paragraph_Format := Format_Object.Next_Paragraph_Format_Type;
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Tab_Stops := Format_Object.Paragraph_Tab_Stops;
			    Format_Object.Next_Paragraph_Format_Type := In_Table;
			    Format_Object.In_Paragraph := True; -- A fake, but we cannot have any format.
			    Arm_Output.Start_Table (Output_Object, Columns => Character'Pos(Ch) - Character'Pos('0'));
			    ARM_Input.Get_Char (Input_Object, Ch);
			    if Ch /= Close_Ch then
				Ada.Text_IO.Put_Line ("  ** Bad close for Table columns on line " & ARM_Input.Line_String (Input_Object));
				ARM_Input.Replace_Char (Input_Object);
			    end if;
			end if;
			-- OK, we've started the table. Now, get the caption:
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Caption" & (8..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Now, handle the parameter:
		            -- Stack it so we can process the end:
			    Set_Nesting_for_Parameter
			        (Command => Table_Param_Caption,
				 Close_Ch => Close_Ch);

			-- else no parameter, weird.
			end if;
		    end;

		-- Paragraph kind commands:

		when Text_Begin =>
		    declare
			Type_Name : ARM_Input.Command_Name_Type;
			Ch : Character;
		    begin
		        -- OK, now read the begin "type":
		        Arm_Input.Get_Name (Input_Object, Type_Name);
		        ARM_Input.Get_Char (Input_Object, Ch);
		        if Ch = ',' then
		            -- Multiple parameters. The remaining
		            -- parameters appear to be format instructions,
		            -- which we ought to replace or remove.
		            Ada.Text_IO.Put_Line ("  -- Multi-parameter begin, line " & ARM_Input.Line_String (Input_Object));

		            -- We ignore everything until the end of the
		            -- parameter.
			    while Ch /= Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char loop
			        -- Ignore everything until the end character
			        -- turns up.
			        ARM_Input.Get_Char (Input_Object, Ch);
			    end loop;
                        end if;

		        if Format_State.Nesting_Stack (Format_State.Nesting_Stack_Ptr).Close_Char = Ch then
		            -- Found the end of the parameter.
		            -- Replace the top of stack with the appropriate begin record:
		            Format_State.Nesting_Stack (Format_State.Nesting_Stack_Ptr) :=
			        (Name => Ada.Characters.Handling.To_Lower (Type_Name),
			         Kind => Begin_Word,
			         Command => Text_Begin,
			         Close_Char => ' ',-- No close character.
				 Old_Last_Subhead_Paragraph => Format_Object.Last_Paragraph_Subhead_Type,
				 Old_Next_Subhead_Paragraph => Format_Object.Next_Paragraph_Subhead_Type,
				 Old_Next_Paragraph_Format => Format_Object.Next_Paragraph_Format_Type,
				 Old_Tab_Stops => Format_Object.Paragraph_Tab_Stops,
				 Is_Formatting => True, -- Reset if needed later.
				 Change_Version => '0', -- Not used.
				 Was_Text => False, -- Not used.
				 Prev_Change => ARM_Output.None, -- Not used.
				 Prev_Change_Version => '0', -- Not used.
				 Prev_Old_Change_Version => '0'); -- Not used.

		            Process_Begin;

		        else
		            ARM_Input.Replace_Char (Input_Object);
		            Ada.Text_IO.Put_Line ("  ** Failed to find close for parameter to begin, line " & ARM_Input.Line_String (Input_Object));
		            --Bracket_Check := Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Bracket_State;
		            Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Bad Begin)");
		        end if;
		    end;

	        when Text_End =>
		    declare
			Type_Name : ARM_Input.Command_Name_Type;
			Ch : Character;
		    begin
	                Arm_Input.Get_Name (Input_Object, Type_Name); -- Get the end "type".
		        ARM_Input.Get_Char (Input_Object, Ch);
		        if Format_State.Nesting_Stack (Format_State.Nesting_Stack_Ptr).Close_Char = Ch then
			    -- Found end of parameter:
			    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
			        -- Remove the "End" record.
			    -- Check for the matching begin, and remove it.
			    if Format_State.Nesting_Stack_Ptr = 0 then
			        Ada.Text_IO.Put_Line ("  ** No begin for end, line " & ARM_Input.Line_String (Input_Object));
			    elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name /=
			        Ada.Characters.Handling.To_Lower (Type_Name) then
			        Ada.Text_IO.Put_Line ("  ** Names of begin and end mismatch, line " & ARM_Input.Line_String (Input_Object));
			        Ada.Text_IO.Put_Line ("     Begin name: " & Ada.Strings.Fixed.Trim(Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right) &
			                              "  End name: " & Ada.Strings.Fixed.Trim(Type_Name, Ada.Strings.Right));
--Ada.Text_IO.Put_Line (" &Stack name is " & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name);
			    else
			        if Format_Object.Next_Paragraph_Subhead_Type /= Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Subhead_Paragraph then
			            Format_Object.Last_Paragraph_Subhead_Type := Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Last_Subhead_Paragraph;
				-- else still in same subhead, leave alone. (If
				-- we didn't do this, we'd output the subhead
				-- multiple times).
				end if;
			        Format_Object.Next_Paragraph_Subhead_Type := Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Subhead_Paragraph;
			        Format_Object.Next_Paragraph_Format_Type := Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Paragraph_Format;
			        Format_Object.Paragraph_Tab_Stops := Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Tab_Stops;
			        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (End)");
			    end if;

			    Check_End_Paragraph; -- End any paragraph that we're in.

			    -- Check if number of columns is changing:
			    if Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
				Type_Name, Ada.Strings.Right)) = "twocol" then
				-- Leaving two column region, reset to one:
				ARM_Output.Set_Columns (Output_Object, Number_of_Columns => 1);
			    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
				Type_Name, Ada.Strings.Right)) = "fourcol" then
				-- Leaving four column region, reset to one:
				ARM_Output.Set_Columns (Output_Object, Number_of_Columns => 1);
			    end if;
			    -- Check if we're leaving a bundle:
			    if Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
				Type_Name, Ada.Strings.Right)) = "bundle" then
				Format_Object.In_Bundle := False;
			    end if;
		        else
			    ARM_Input.Replace_Char (Input_Object);
			    Ada.Text_IO.Put_Line ("  ** Failed to find close for parameter to end, line " & ARM_Input.Line_String (Input_Object));
		            Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Bad End)");
		        end if;
		    end;

		-- Indexing commands:

		when Defn | RootDefn =>
		    -- @Defn{term} or @RootDefn{term}. Index the term.
		    -- Note that there is no difference between these in terms
		    -- of the index, so we do them together.
		    declare
			Term : String(1..80);
			Len : Natural := 0;
			Key : ARM_Index.Index_Key;
		    begin
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
		            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char,
			    Term,
			    Len);
			ARM_Index.Add (Term => Term(1..Len),
				       Kind => ARM_Index.Primary_Term,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);

			Check_Paragraph;
			if Format_Object.Display_Index_Entries then
			    if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Defn then
			        Display_Index_Entry (Term(1..Len));
			    else -- RootDefn
			        Display_Index_Entry (Term(1..Len), Special => Is_Root);
			    end if;
			end if;
			ARM_Output.Index_Target (Output_Object, Key);
		        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		            -- Remove the "(Root)Defn" record.
		    end;

		when PDefn =>
		    -- @PDefn{term} ot @RootDefn{term}. Index the term as a partial
		    -- definition.
		    declare
			Term : String(1..80);
			Len : Natural := 0;
			Key : ARM_Index.Index_Key;
		    begin
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
		            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char,
			    Term,
			    Len);
			ARM_Index.Add (Term => Term(1..Len),
				       Kind => ARM_Index.Partial_Term,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);

			Check_Paragraph;
			if Format_Object.Display_Index_Entries then
			    Display_Index_Entry (Term(1..Len), Special => Is_Partial);
			end if;
			ARM_Output.Index_Target (Output_Object, Key);
		        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		            -- Remove the "PDefn" record.
		    end;

		when Defn2 | RootDefn2 =>
		    -- @Defn2[Term={term}, Sec={sec}]
 		    -- @RootDefn[Term={term}, Sec={sec}]. Index the term and subterm.
		    -- Note that there is no difference between these in terms
		    -- of the index, so we do them together.
		    declare
			Close_Ch : Character;
			Term, Subterm : String(1..90);
			TLen, SLen : Natural := 0;
			Key : ARM_Index.Index_Key;
		    begin
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Term" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => True,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Copy over the term:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
			        Close_Ch,
				Term,
				TLen);
			-- else no parameter. Weird.
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Sec" & (4..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Copy over the term:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
			        Close_Ch,
				Subterm,
				SLen);
			-- else no parameter. Weird.
			end if;

			ARM_Index.Add (Term => Term(1..TLen),
				       Subterm => Subterm(1..SLen),
				       Kind => ARM_Index.Primary_Term_and_Subterm,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);

			Check_Paragraph;
			if Format_Object.Display_Index_Entries then
			    if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Defn2 then
			        Display_Index_Entry (Term(1..TLen) & " (" & Subterm(1..SLen) & ')');
			    else -- RootDefn
			        Display_Index_Entry (Term(1..TLen) & " (" & Subterm(1..SLen) & ')', Special => Is_Root);
			    end if;
			end if;
			ARM_Output.Index_Target (Output_Object, Key);

			-- Leave the command end marker, let normal processing
			-- get rid of it.
		    end;

		when PDefn2 =>
		    -- @PDefn2[Term={term}, Sec={sec}]. Index the term and subterm.
		    -- Note that there is no difference between these in terms
		    -- of the index, so we do them together.
		    declare
			Close_Ch : Character;
			Term, Subterm : String(1..90);
			TLen, SLen : Natural := 0;
			Key : ARM_Index.Index_Key;
		    begin
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Term" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => True,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Copy over the term:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
			        Close_Ch,
				Term,
				TLen);
			-- else no parameter. Weird.
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Sec" & (4..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Copy over the term:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
			        Close_Ch,
				Subterm,
				SLen);
			-- else no parameter. Weird.
			end if;

			ARM_Index.Add (Term => Term(1..TLen),
				       Subterm => Subterm(1..SLen),
				       Kind => ARM_Index.Partial_Term_with_Subterm,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);

			Check_Paragraph;
			if Format_Object.Display_Index_Entries then
			    Display_Index_Entry (Term(1..TLen) & " (" & Subterm(1..SLen) & ')',
				Special => Is_Partial);
			end if;
			ARM_Output.Index_Target (Output_Object, Key);

			-- Leave the command end marker, let normal processing
			-- get rid of it.
		    end;

		when Index_See =>
		    -- @IndexSee[Term={term}, See={see}]. Index a See item.
		    declare
			Close_Ch : Character;
			Term, See : String(1..80);
			TLen, SLen : Natural := 0;
			Key : ARM_Index.Index_Key;
		    begin
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Term" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => True,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Copy over the term:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
			        Close_Ch,
				Term,
				TLen);
			-- else no parameter. Weird.
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "See" & (4..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Copy over the term:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
			        Close_Ch,
				See,
				SLen);
			-- else no parameter. Weird.
			end if;

			ARM_Index.Add (Term => Term(1..TLen),
				       Subterm => See(1..SLen),
				       Kind => ARM_Index.See_Term,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);

			Check_Paragraph;
			if Format_Object.Display_Index_Entries then
			    Display_Index_Entry (Term(1..TLen) & ": See " & See(1..SLen));
			end if;
			ARM_Output.Index_Target (Output_Object, Key);

			-- Leave the command end marker, let normal processing
			-- get rid of it.
		    end;

		when Index_See_Also =>
		    -- @IndexSeeAlso[Term={term}, See={see}]. Index a See Also item.
		    declare
			Close_Ch : Character;
			Term, See : String(1..80);
			TLen, SLen : Natural := 0;
			Key : ARM_Index.Index_Key;
		    begin
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Term" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => True,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Copy over the term:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
			        Close_Ch,
				Term,
				TLen);
			-- else no parameter. Weird.
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "See" & (4..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Copy over the term:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
			        Close_Ch,
				See,
				SLen);
			-- else no parameter. Weird.
			end if;

			ARM_Index.Add (Term => Term(1..TLen),
				       Subterm => See(1..SLen),
				       Kind => ARM_Index.See_Also_Term,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);

			Check_Paragraph;
			if Format_Object.Display_Index_Entries then
			    Display_Index_Entry (Term(1..TLen) & ": See also " & See(1..SLen));
			end if;
			ARM_Output.Index_Target (Output_Object, Key);

			-- Leave the command end marker, let normal processing
			-- get rid of it.
		    end;

		when See_Other =>
		    -- @SeeOther[Primary={term}, Other={see}]. Generate a
		    -- See {see} in the index, but no reference.
		    declare
			Close_Ch : Character;
			Term, See : String(1..80);
			TLen, SLen : Natural := 0;
			Key : ARM_Index.Index_Key;
		    begin
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Primary" & (8..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => True,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Copy over the term:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
			        Close_Ch,
				Term,
				TLen);
			-- else no parameter. Weird.
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Other" & (6..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Copy over the term:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
			        Close_Ch,
				See,
				SLen);
			-- else no parameter. Weird.
			end if;

			ARM_Index.Add (Term => Term(1..TLen),
				       Subterm => See(1..SLen),
				       Kind => ARM_Index.See_Other_Term,
				       Clause => "",
				       Paragraph => "",
				       Key => Key);

			Check_Paragraph;
			ARM_Output.Index_Target (Output_Object, Key);

			-- Leave the command end marker, let normal processing
			-- get rid of it.
		    end;

		when See_Also =>
		    -- @SeeAlso[Primary={term}, Other={see}]. Generate a
		    -- See also {see} in the index, but no reference.
		    declare
			Close_Ch : Character;
			Term, See : String(1..80);
			TLen, SLen : Natural := 0;
			Key : ARM_Index.Index_Key;
		    begin
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Primary" & (8..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => True,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Copy over the term:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
			        Close_Ch,
				Term,
				TLen);
			-- else no parameter. Weird.
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Other" & (6..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Copy over the term:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
			        Close_Ch,
				See,
				SLen);
			-- else no parameter. Weird.
			end if;

			ARM_Index.Add (Term => Term(1..TLen),
				       Subterm => See(1..SLen),
				       Kind => ARM_Index.See_Also_Other_Term,
				       Clause => "",
				       Paragraph => "",
				       Key => Key);

			Check_Paragraph;
			ARM_Output.Index_Target (Output_Object, Key);

			-- Leave the command end marker, let normal processing
			-- get rid of it.
		    end;

		when Index_Root_Unit =>
		    -- @RootLibUnit{<unit>}
		    -- Generates two index entries: An index entry for
		    -- "Language-Defined Library Units" with a secondary
		    -- entry of <unit>, and an index entry for <unit>.
		    declare
			Term : String(1..80);
			Len : Natural := 0;
			Key : ARM_Index.Index_Key;
		    begin
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
		            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char,
			    Term,
			    Len);

			-- Set the current unit for future use:
			Format_Object.Unit (1..Len) := Term(1..Len);
			Format_Object.Unit_Len := Len;

			ARM_Index.Add (Term => "Language-Defined Library Units",
				       Subterm => Term(1..Len),
				       Kind => ARM_Index.Primary_Term_and_Subterm,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);

			Check_Paragraph;
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Index.Add (Term => Term(1..Len),
				       Kind => ARM_Index.Primary_Term,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

		        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		            -- Remove the "RootLibUnit" record.
		    end;

		when Index_Child_Unit =>
		    -- @ChildUnit{Parent=[<parent>],Child=[<child>]}
		    -- Generates three index entries: An index entry for <child>, with a secondary
		    -- of "@i{child of} <parent>", an index entry for "Language-Defined
		    -- Library Units" with a secondary entry of <parent>.<child>,
		    -- and an index entry for <parent>.<child>.
		    declare
			Close_Ch : Character;
			Parent, Child : String(1..80);
			PLen, CLen : Natural := 0;
			Key : ARM_Index.Index_Key;
		    begin
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Parent" & (7..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => True,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Copy over the term:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
			        Close_Ch,
				Parent,
				PLen);
			-- else no parameter. Weird.
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Child" & (6..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Copy over the term:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
			        Close_Ch,
				Child,
				CLen);
			-- else no parameter. Weird.
			end if;

			-- Set the current unit for future use:
			Format_Object.Unit_Len := PLen + CLen + 1;
			Format_Object.Unit (1..Format_Object.Unit_Len) :=
			    Parent(1..PLen) & '.' & Child(1..CLen);

			ARM_Index.Add (Term => Child(1..CLen),
				       Subterm => Parent(1..PLen),
				       Kind => ARM_Index.Child_Unit_Parent,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);

			Check_Paragraph;
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Index.Add (Term => "Language-Defined Library Units",
				       Subterm => Parent(1..PLen) & '.' & Child(1..CLen),
				       Kind => ARM_Index.Primary_Term_and_Subterm,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Index.Add (Term => Parent(1..PLen) & '.' & Child(1..CLen),
				       Kind => ARM_Index.Primary_Term,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

			-- Leave the command end marker, let normal processing
			-- get rid of it.
		    end;

		when Index_Type =>
		    -- @AdaTypeDefn{<defn>}
		    -- Generates two index entries: one for <defn> with a
		    -- secondary entry of "@i{in} <Unit>" (where Unit is
		    -- the unit saved by a previous RootLibUnit or ChildUnit.),
		    -- and second for "Language-Defined Type" with a
		    -- secondary entry of "<defn> @i{in} <Unit>".
		    -- Also outputs the <defn> to the output file.
		    declare
			Type_Name : String(1..80);
			Len : Natural := 0;
			Key : ARM_Index.Index_Key;
		    begin
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
		            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char,
			    Type_Name,
			    Len);

			ARM_Index.Add (Term => "Language-Defined Type",
				       Subterm => Type_Name(1..Len) & " in " &
						  Format_Object.Unit(1..Format_Object.Unit_Len),
				       Kind => ARM_Index.SubDeclaration_in_Package,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);
			    -- Note that the Subdeclaration type changes the
			    -- "in" into italics.
			Check_Paragraph;
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Index.Add (Term => Type_Name(1..Len),
				       Subterm => Format_Object.Unit(1..Format_Object.Unit_Len),
				       Kind => ARM_Index.Declaration_in_Package,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Output.Ordinary_Text (Output_Object, Type_Name(1..Len));
			Format_Object.Last_Non_Space := True;

		        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		            -- Remove the "AdaSubDefn" record.
		    end;

		when Index_Subprogram =>
		    -- @AdaSubDefn{<defn>}
		    -- Generates two index entries: one for <defn> with a
		    -- secondary entry of "@i{in} <Unit>" (where Unit is
		    -- the unit saved by a previous RootLibUnit or ChildUnit.),
		    -- and second for "Language-Defined Subprogram" with a
		    -- secondary entry of "<defn> @i{in} <Unit>".
		    -- Also outputs the <defn> to the output file.
		    declare
			Subprogram : String(1..80);
			Len : Natural := 0;
			Key : ARM_Index.Index_Key;
		    begin
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
		            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char,
			    Subprogram,
			    Len);

			ARM_Index.Add (Term => "Language-Defined Subprogram",
				       Subterm => Subprogram(1..Len) & " in " &
						  Format_Object.Unit(1..Format_Object.Unit_Len),
				       Kind => ARM_Index.SubDeclaration_in_Package,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);
			    -- Note that the Subdeclaration type changes the
			    -- "in" into italics.
			Check_Paragraph;
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Index.Add (Term => Subprogram(1..Len),
				       Subterm => Format_Object.Unit(1..Format_Object.Unit_Len),
				       Kind => ARM_Index.Declaration_in_Package,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Output.Ordinary_Text (Output_Object, Subprogram(1..Len));
			Format_Object.Last_Non_Space := True;

		        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		            -- Remove the "AdaSubDefn" record.
		    end;

		when Index_Other =>
		    -- @AdaSubDefn{<defn>}
		    -- Generate an index entries for <defn> with a
		    -- secondary entry of "@i{in} <Unit>" (where Unit is
		    -- the unit saved by a previous RootLibUnit or ChildUnit.).
		    -- Also outputs the <defn> to the output file.
		    declare
			Item : String(1..80);
			Len : Natural := 0;
			Key : ARM_Index.Index_Key;
		    begin
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
		            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char,
			    Item,
			    Len);

			ARM_Index.Add (Term => Item(1..Len),
				       Subterm => Format_Object.Unit(1..Format_Object.Unit_Len),
				       Kind => ARM_Index.Declaration_in_Package,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);
			Check_Paragraph;
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Output.Ordinary_Text (Output_Object, Item(1..Len));
			Format_Object.Last_Non_Space := True;

		        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		            -- Remove the "AdaDefn" record.
		    end;

		when Index_Check =>
		    -- @Indexcheck{<check>}
		    -- Generates index items for
		    -- "check, language-defined", <check>,
		    -- and <check> [partial].
		    declare
			Term : String(1..80);
			Len : Natural := 0;
			Key : ARM_Index.Index_Key;
		    begin
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
		            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char,
			    Term,
			    Len);

			ARM_Index.Add (Term => Term(1..Len),
				       Kind => ARM_Index.Partial_Term,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);
			Check_Paragraph;
			if Format_Object.Display_Index_Entries then
			    Display_Index_Entry (Term(1..Len), Special => Is_Partial);
			end if;
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Index.Add (Term => "check, language-defined",
				       Subterm => Term(1..Len),
				       Kind => ARM_Index.Primary_Term_and_Subterm,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);
			if Format_Object.Display_Index_Entries then
			    Display_Index_Entry ("check, language-defined (" & Term(1..Len) & ")");
			end if;
			ARM_Output.Index_Target (Output_Object, Key);

		        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		            -- Remove the "Indexcheck" record.
		    end;

		when Index_Attr =>
		    -- This command indexes an attribute name.
		    -- This calls Defn2("attributes", <param>), and
		    -- also writes <param> to output.
		    declare
			Param : String(1..20);
			Len : Natural := 0;
			Key : ARM_Index.Index_Key;
		    begin
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
		            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char,
			    Param,
			    Len);

			Check_Paragraph;

			ARM_Index.Add (Term => "attributes",
				       Subterm => Param(1..Len),
				       Kind => ARM_Index.Primary_Term_and_Subterm,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Index.Add (Term => Param(1..Len) & " attribute",
				       Kind => ARM_Index.Primary_Term,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Output.Ordinary_Text (Output_Object,
			    Param(1..Len));
		        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		            -- Remove the "Index_Attr" record.
		    end;

		when Index_Pragma =>
		    -- This command indexes a pragma name.
		    -- This calls Defn2("pragmas", <param>), and
		    -- also writes <param> to output.
		    declare
			Param : String(1..30);
			Len : Natural := 0;
			Key : ARM_Index.Index_Key;
		    begin
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
		            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char,
			    Param,
			    Len);
			Check_Paragraph;
			ARM_Index.Add (Term => "pragmas",
				       Subterm => Param(1..Len),
				       Kind => ARM_Index.Primary_Term_and_Subterm,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Index.Add (Term => Param(1..Len) & " pragma",
				       Kind => ARM_Index.Primary_Term,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Output.Ordinary_Text (Output_Object,
			    Param(1..Len));
		        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		            -- Remove the "Index_Pragma" record.
		    end;

		when Syntax_Rule =>
		    -- @syn{[Tabs=<Tabset>, ]LHS=<Non-terminal>, RHS=<Production>}
		    -- Marks a syntax production of the form:
		    --    @nt<Non-Terminal> ::= <Production>
		    -- <Tabset> defines any tabs needed by the syntax production.
		    --
		    -- Also, the <Non-terminal> is indexed. The <Non-Terminal>
		    -- and <Production> (and the clause number) are sent to the
		    -- syntax manager. Also, saves <Non-terminal> for any
		    -- following Syntax_Term (@Syn2) to use.

		    declare
			Close_Ch, Ch : Character;
			Len : Natural := 0;
			Was_Tabs : Boolean := False;
			Org_Tabs : ARM_Output.Tab_Info;
			Key : ARM_Index.Index_Key;
		    begin
			-- Peek to see if Tabs parmeter is present:
			ARM_Input.Get_Char (Input_Object, Ch);
			ARM_Input.Replace_Char (Input_Object);
			if Ch = 'T' or else Ch = 't' then
			    Was_Tabs := True;
			    ARM_Input.Check_Parameter_Name (Input_Object,
			        Param_Name => "Tabs" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			        Is_First => True,
			        Param_Close_Bracket => Close_Ch);
			    if Close_Ch /= ' ' then
			        -- Grab the tab string:
			        ARM_Input.Copy_to_String_until_Close_Char (
				    Input_Object,
			            Close_Ch,
				    Format_Object.Syntax_Tab,
				    Format_Object.Syntax_Tab_Len);
			    -- else no parameter. Weird.
			    end if;
			else
			    Format_Object.Syntax_Tab_Len := 0;
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "LHS" & (4..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => not Was_Tabs,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Copy over the non-terminal:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
			        Close_Ch,
				Format_Object.Syntax_NT,
				Format_Object.Syntax_NT_Len);
			-- else no parameter. Weird.
			end if;

			-- Set up the tabs:
			Org_Tabs := Format_Object.Paragraph_Tab_Stops;
			Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;
			if Format_Object.Syntax_Tab_Len /= 0 then
			    Parse_Tab_Stops (Format_Object.Syntax_Tab(1..Format_Object.Syntax_Tab_Len),
				Format_Object.Paragraph_Tab_Stops);
			end if;

		        Check_Paragraph;
			ARM_Format.Format (Format_Object,
					   "@s{" & Format_Object.Syntax_NT(1..Format_Object.Syntax_NT_Len) & "}",
					   Output_Object,
					   Text_Name => "@Syn(LHS=",
					   No_AARM_Text => False);
			    -- We use Format here so we can support changes in
			    -- the non-terminal.

			-- Set the font for the "::=". Note that we use @s{}
			-- above, so that any font changes in the Non-Terminal
			-- (as in a @Chg command) are respected.

		        ARM_Output.Text_Format (Output_Object,
					        Bold => Format_Object.Is_Bold,
					        Italic => Format_Object.Is_Italic,
					        Font => ARM_Output.Swiss,
					        Size => Format_Object.Size,
					        Change => Format_Object.Change,
				                Version => Format_Object.Current_Change_Version,
				                Added_Version => Format_Object.Current_Old_Change_Version,
					        Location => Format_Object.Location);

			-- Index the non-terminal:
			ARM_Index.Add (Term => Get_NT,
				       Kind => ARM_Index.Primary_Term,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Output.Ordinary_Text (Output_Object, " ::= ");
		        ARM_Output.Text_Format (Output_Object,
					        Bold => Format_Object.Is_Bold,
					        Italic => Format_Object.Is_Italic,
					        Font => Format_Object.Font,
					        Size => Format_Object.Size,
					        Change => Format_Object.Change,
				                Version => Format_Object.Current_Change_Version,
				                Added_Version => Format_Object.Current_Old_Change_Version,
					        Location => Format_Object.Location);
			Format_Object.Last_Non_Space := False;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "RHS" & (4..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Now, handle the parameter:
		            -- Stack it so we can process the end:
			    Set_Nesting_for_Parameter
			        (Command => Syntax_Rule_RHS,
				 Close_Ch => Close_Ch);

			    ARM_Input.Start_Recording (Input_Object);

			    -- Set the format to preserve line breaks.
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Last_Subhead_Paragraph := Format_Object.Last_Paragraph_Subhead_Type;
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Subhead_Paragraph := Format_Object.Next_Paragraph_Subhead_Type;
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Paragraph_Format := Format_Object.Next_Paragraph_Format_Type;
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Tab_Stops := Org_Tabs;
			    Format_Object.Next_Paragraph_Format_Type := Syntax_Production;
			    -- Tab stops are already set.
			-- else no parameter, weird.
			end if;
		    end;

		when Syntax_Term =>
		    -- Marks a non-terminal name in the production of a syntax
		    -- rule. Generates the term in the same style as
		    -- @nt (Non_Terminal).
		    -- If the current LHS non-terminal is not null, generates
		    -- a syntax cross reference entry:
		    -- <Name> in <Non-Terminal> at <ClauseNum>. Also,
		    -- generate an index entry for the item:
		    -- @Defn2(Term=<Name>,Sec=@i{used}.
		    -- Note: We assume no internal formatting in <Name>.
		    declare
			Name : String(1..40);
			Len : Natural;
			Key : ARM_Index.Index_Key;
		    begin
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char,
			    Name, Len);
			if Format_Object.Syntax_NT_Len /= 0 then
			    -- Generate a syntax cross-reference entry.
			    ARM_Syntax.Add_Xref (
			         Name => Name(1..Len),
			         Used_In => Get_NT,
			         Clause => Clause_String);
			end if;

			-- Index the non-terminal:
			ARM_Index.Add (Term => Name(1..Len),
				       Kind => ARM_Index.Syntax_NT_Used,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

		        -- Set the appropriate style:
		        Check_Paragraph;
		        ARM_Output.Text_Format (Output_Object,
					        Bold => Format_Object.Is_Bold,
					        Italic => Format_Object.Is_Italic,
					        Font => ARM_Output.Swiss,
					        Size => Format_Object.Size,
					        Change => Format_Object.Change,
				                Version => Format_Object.Current_Change_Version,
				                Added_Version => Format_Object.Current_Old_Change_Version,
					        Location => Format_Object.Location);
			ARM_Output.Ordinary_Text (Output_Object, Name(1..Len));
		        ARM_Output.Text_Format (Output_Object,
					        Bold => Format_Object.Is_Bold,
					        Italic => Format_Object.Is_Italic,
					        Font => Format_Object.Font,
					        Size => Format_Object.Size,
					        Change => Format_Object.Change,
				                Version => Format_Object.Current_Change_Version,
				                Added_Version => Format_Object.Current_Old_Change_Version,
					        Location => Format_Object.Location);
			Format_Object.Last_Non_Space := True;
		    end;
		    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Syntax Term)");


		when Syntax_Prefix =>
		    -- Marks the prefix of a non-terminal. Writes italized
		    -- text in the current font.
		    -- Set the appropriate style:
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Bold => Format_Object.Is_Bold,
					    Italic => True,
					    Font => Format_Object.Font,
					    Size => Format_Object.Size,
					    Change => Format_Object.Change,
				            Version => Format_Object.Current_Change_Version,
				            Added_Version => Format_Object.Current_Old_Change_Version,
					    Location => Format_Object.Location);
		    Format_Object.Is_Italic := True;

		when To_Glossary | To_Glossary_Also =>
		    -- This is a glossary command.
		    -- It is of the form @ToGlossary(Term=[<term>], Text=[<text>])
		    -- We will store the term and definition in the glossary
		    -- database. We also have to pass through the Text
		    -- parameter, either to the regular text (for
		    -- ToGlossaryAlso) or the AARM (for ToGlossary).

		    declare
			Close_Ch : Character;
			Key : ARM_Index.Index_Key;
		    begin
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Term" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => True,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Copy the term:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
			        Close_Ch,
				Format_Object.Glossary_Term,
				Format_Object.Glossary_Term_Len);
			-- else no parameter. Weird.
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Text" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Now, handle the parameter:
		            -- Stack it so we can process the end:
			    Set_Nesting_for_Parameter
			        (Command => Glossary_Text_Param,
				 Close_Ch => Close_Ch);

			    ARM_Input.Start_Recording (Input_Object);

			    if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Command = To_Glossary_Also then
				-- The text just goes straight to the file.
				if Format_Object.Display_Index_Entries then
				    Display_Index_Entry (Format_Object.Glossary_Term (1..Format_Object.Glossary_Term_Len)); -- Includes Check_Paragraph.
				    ARM_Output.Ordinary_Text (Output_Object,
					"[Glossary Entry]");
				    Format_Object.Last_Non_Space := True;
				-- else no marker.
				end if;

				-- Index the term (because it does appear here):
				Check_Paragraph; -- We've got to be in a paragraph to write this.
				ARM_Index.Add (Term => Format_Object.Glossary_Term (1..Format_Object.Glossary_Term_Len),
					       Kind => ARM_Index.Primary_Term,
					       Clause => Clause_String,
					       Paragraph => Paragraph_String,
					       Key => Key);
				ARM_Output.Index_Target (Output_Object, Key);
			    else
			        case Format_Object.Document is
				    when ARM_Format.AARM =>
					Check_End_Paragraph; -- End any paragraph that we're in.
					Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Last_Subhead_Paragraph := Format_Object.Last_Paragraph_Subhead_Type;
					Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Subhead_Paragraph := Format_Object.Next_Paragraph_Subhead_Type;
					Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Paragraph_Format := Format_Object.Next_Paragraph_Format_Type;
					Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Tab_Stops := Format_Object.Paragraph_Tab_Stops;
					Format_Object.Next_Paragraph_Format_Type := Glossary_Marker;
					Format_Object.Next_Paragraph_Subhead_Type := Glossary_Marker;
					Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;
				        Display_Index_Entry (Format_Object.Glossary_Term (1..Format_Object.Glossary_Term_Len)); -- Includes Check_Paragraph.
				    when others =>
					if Format_Object.Display_Index_Entries then
				            Display_Index_Entry (Format_Object.Glossary_Term (1..Format_Object.Glossary_Term_Len)); -- Includes Check_Paragraph.
					    ARM_Output.Ordinary_Text (Output_Object,
						"[Glossary Entry]");
					    Format_Object.Last_Non_Space := True;
					-- else no marker.
					end if;
				        -- Skip the text:
				        ARM_Input.Skip_until_Close_Char (Input_Object,
					    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char);
				        ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
				end case;

				-- Note: The term is indexed in the glossary,
				-- but not here.
			    end if;
			end if;
		    end;

		when Implementation_Defined =>
		    -- Store an "implementation-defined" entry for the parameter;
		    -- also save the clause and paragraph reference.

		    ARM_Input.Start_Recording (Input_Object);

		    Format_Object.Impdef_Change_Kind := ARM_Database.None;
		    Format_Object.Impdef_Version := '0';

		    if Format_Object.In_Paragraph then
			-- Do this to preserve any inserted paragraph info.
			Format_Object.Impdef_Paragraph_String :=
			    Format_Object.Current_Paragraph_String;
			Format_Object.Impdef_Paragraph_Len :=
			    Format_Object.Current_Paragraph_Len;
		    else
			declare
			    PNum : constant String := Positive'Image (
				Format_Object.Next_Paragraph - 1);
			begin
			    Format_Object.Impdef_Paragraph_Len := PNum'Length - 1;
			    Format_Object.Impdef_Paragraph_String (1 .. PNum'Last-1) :=
				PNum (2 .. PNum'Last);
			end;
		    end if;

		    case Format_Object.Document is
		        when ARM_Format.AARM =>
			    Check_End_Paragraph; -- End any paragraph that we're in.
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Last_Subhead_Paragraph := Format_Object.Last_Paragraph_Subhead_Type;
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Subhead_Paragraph := Format_Object.Next_Paragraph_Subhead_Type;
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Paragraph_Format := Format_Object.Next_Paragraph_Format_Type;
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Tab_Stops := Format_Object.Paragraph_Tab_Stops;
			    Format_Object.Next_Paragraph_Format_Type := Bare_Annotation;
			    Format_Object.Next_Paragraph_Subhead_Type := Bare_Annotation;
			    Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;
			    Check_Paragraph;
			    ARM_Output.Text_Format (Output_Object,
				        Bold => True,
				        Italic => Format_Object.Is_Italic,
				        Font => Format_Object.Font,
				        Size => Format_Object.Size,
				        Change => Format_Object.Change,
			                Version => Format_Object.Current_Change_Version,
			                Added_Version => Format_Object.Current_Old_Change_Version,
				        Location => Format_Object.Location);
			    ARM_Output.Ordinary_Text (Output_Object,
				 Text => "Implementation defined: ");
			    ARM_Output.Text_Format (Output_Object,
				        Bold => Format_Object.Is_Bold,
				        Italic => Format_Object.Is_Italic,
				        Font => Format_Object.Font,
				        Size => Format_Object.Size,
				        Change => Format_Object.Change,
			                Version => Format_Object.Current_Change_Version,
			                Added_Version => Format_Object.Current_Old_Change_Version,
				        Location => Format_Object.Location);
			    Format_Object.Last_Paragraph_Subhead_Type := Bare_Annotation;
			    Format_Object.Last_Non_Space := False;
		        when others =>
			    -- Skip the text:
			    ARM_Input.Skip_until_Close_Char (Input_Object,
			        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char);
			    ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
		    end case;

		when Prefix_Type =>
		    -- Copy the text into the Format_Object.Prefix_Text string.
		    Check_Paragraph;
		    ARM_Input.Start_Recording (Input_Object);
		    -- No changes in this version (use ChgAttribute if you need that).
		    Format_Object.Attr_Prefix_Change_Kind := ARM_Database.None;
		    Format_Object.Attr_Prefix_Version := '0';

		when Reset_Prefix_Type =>
		    -- Set Format_Object.Prefix_Text string to the default.
		    Format_Object.Prefix_Text := "@b{NONE!}" & (10..160 => ' ');
		    Format_Object.Prefix_Text_Len := 9;

		when Attribute | Attribute_Leading =>
		     -- @Attribute{Prefix=<Prefix>,AttrName=<Name>,Text=<Text>}
		     -- Defines an attribute. Creates a hanging text item <Prefix>'<Name>,
		     --	with the specified text. The text can contain arbitrary commands;
		     --	it will be run through the full evaluation code.
		     --	The attribute and text is also sent to a database used to later create
		     --	Annex K. (This uses the current value of PrefixType.) Finally, the
		     --	attribute <Name> is indexed as by calling @Defn2{Term=[Attribute],
		     --	Sec=<Name>}, and as by calling @Defn{<Name> attribute}.
		    declare
			Close_Ch : Character;
			Key : ARM_Index.Index_Key;
		    begin
			Check_End_Paragraph; -- This is always a paragraph end.

			-- No changes in this version (use ChgAttribute if you need that).
		        Format_Object.Attr_Change_Kind := ARM_Database.None;
		        Format_Object.Attr_Version := '0';

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Prefix" & (7..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => True,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Save prefix:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
				Close_Ch,
				Format_Object.Attr_Prefix,
				Format_Object.Attr_Prefix_Len);
			-- else no parameter. Weird.
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "AttrName" & (9..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Save name:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
				Close_Ch,
				Format_Object.Attr_Name,
				Format_Object.Attr_Name_Len);
			-- else no parameter. Weird.
			end if;

			-- Output <Prefix>'<Name> as the hanging text.
			if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Attribute_Leading then
			    Format_Object.Space_After := ARM_Output.Narrow;
			    Format_Object.Attr_Leading := True;
			else
			    Format_Object.Space_After := ARM_Output.Normal;
			    Format_Object.Attr_Leading := False;
			end if;
			Check_Paragraph;
			ARM_Output.Ordinary_Text (Output_Object,
				Format_Object.Attr_Prefix (1 .. Format_Object.Attr_Prefix_Len));
		        ARM_Output.Ordinary_Character (Output_Object, ''');
		        ARM_Output.Ordinary_Text (Output_Object,
				Format_Object.Attr_Name (1 .. Format_Object.Attr_Name_Len));
		        ARM_Output.End_Hang_Item (Output_Object);
			Format_Object.Last_Non_Space := False; -- Treat like start of a line.

			ARM_Index.Add (Term => "attributes",
				       Subterm => Format_Object.Attr_Name (1 .. Format_Object.Attr_Name_Len),
				       Kind => ARM_Index.Primary_Term_and_Subterm,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Index.Add (Term => Format_Object.Attr_Name (1 .. Format_Object.Attr_Name_Len) & " attribute",
				       Kind => ARM_Index.Primary_Term,
				       Clause => Clause_String,
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Text" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Now, handle the parameter:
			    -- The text goes to the file *and* is recorded.
			    Arm_Input.Start_Recording (Input_Object);
			    -- Stack the parameter so we can process the end:
			    Set_Nesting_for_Parameter
			        (Command => Attribute_Text_Param,
				 Close_Ch => Close_Ch);
			end if;
		    end;

		when Pragma_Syntax =>
		     -- @PragmaSyntax{<Text>}
		     -- Defines a pragma. The text can contain arbitrary commands;
		     --	it will be run through the full evaluation code.
		     --	The text is also sent to a database used to later create
		     --	Annex L.
		     -- Note that these are indented slightly more than regular
		     -- syntax text. We handle that by adding a couple of
		     -- spaces before the text.

		     -- All we have to do here is output a couple of
		     -- hard spaces and then start recording.
		     Check_Paragraph;
		     ARM_Output.Hard_Space (Output_Object);
		     ARM_Output.Hard_Space (Output_Object);
		     ARM_Input.Start_Recording (Input_Object);
		     -- Just handle the text normally.

		when Added_Pragma_Syntax =>
		     -- @AddedPragmaSyntax{Version=[<Version>],<Text>}
		     -- Defines a pragma. The text can contain arbitrary commands;
		     --	it will be run through the full evaluation code.
		     --	The text is also sent to a database used to later create
		     --	Annex L.
		     -- Note that these are indented slightly more than regular
		     -- syntax text. We handle that by adding a couple of
		     -- spaces before the text.

		     declare
			Ch : Character;
			Version : ARM_Contents.Change_Version_Type := '0';
		     begin
			Get_Change_Version (Is_First => True,
					    Version => Version);
		        ARM_Input.Get_Char (Input_Object, Ch);
		        if Ch /= ',' then
			    Ada.Text_IO.Put_Line ("  ** Missing comma for AddedPragmaSyn on line " & ARM_Input.Line_String (Input_Object));
			    ARM_Input.Replace_Char (Input_Object);
		        end if;
			Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version := Version;
		        if Format_Object.Change_Version < Version then
			    -- Ignore the change, the version is too high.
			    -- Skip the text:
			    ARM_Input.Skip_until_Close_Char (Input_Object,
			        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char);
			    ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
		        else
			    case Format_Object.Changes is
			        when ARM_Format.Old_Only =>
				    -- Skip the text:
				    ARM_Input.Skip_until_Close_Char (Input_Object,
				        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char);
				    ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
			        when ARM_Format.New_Only | ARM_Format.Show_Changes |
				     ARM_Format.New_Changes | ARM_Format.Changes_Only =>
				     -- All we have to do here is output a couple of
				     -- hard spaces and then start recording. The outer
				     -- @Chg will handle the formatting for this.
				     Check_Paragraph;
				     ARM_Output.Hard_Space (Output_Object);
				     ARM_Output.Hard_Space (Output_Object);
				     ARM_Input.Start_Recording (Input_Object);
				     -- Just handle the text normally.
		            end case;
		        end if;
		    end;

		-- Clause title and reference commands:

		when Labeled_Section | Labeled_Section_No_Break |
		     Labeled_Informative_Annex |
		     Labeled_Normative_Annex | Labeled_Clause |
		     Labeled_Subclause | Unnumbered_Section =>
		    -- Load the title into the Title string:
		    declare
			Title : ARM_Contents.Title_Type;
			Title_Length : Natural;
		    begin
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char,
			    Title, Title_Length);
		        if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Subclause then
			    Format_Object.Subclause := Format_Object.Subclause + 1;
		        elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Clause or else
			      Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Unnumbered_Section then
			    Format_Object.Clause := Format_Object.Clause + 1;
			    Format_Object.Subclause := 0;
		        else
			    Format_Object.Clause := 0;
			    Format_Object.Subclause := 0;
			end if;
			Title(Title_Length+1 .. Title'Last) :=
			    (others => ' ');

			begin
			    declare
			        Clause_Number : constant String :=
				    ARM_Contents.Lookup_Clause_Number (Title);
			    begin
			        if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Subclause then
				    Check_End_Paragraph; -- End any paragraph that we're in.
				    ARM_Output.Clause_Header (Output_Object,
				        Title(1..Title_Length),
					Level => ARM_Contents.Subclause,
					Clause_Number => Clause_Number);
			            -- Check that the section numbers match the title:
			            if Ada.Characters.Handling.To_Lower (Title) /=
			               Ada.Characters.Handling.To_Lower (ARM_Contents.Lookup_Title (
				          ARM_Contents.Subclause, Format_Object.Section,
					  Format_Object.Clause, Format_Object.Subclause)) then
				        Ada.Text_IO.Put_Line ("** Unable to match title with section numbers, line " & ARM_Input.Line_String (Input_Object));
			            end if;
			        elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Clause then
				    Check_End_Paragraph; -- End any paragraph that we're in.
				    ARM_Output.Clause_Header (Output_Object,
				        Title(1..Title_Length),
					Level => ARM_Contents.Clause,
					Clause_Number => Clause_Number);
			            -- Check that the section numbers match the title:
			            if Ada.Characters.Handling.To_Lower (Title) /=
			               Ada.Characters.Handling.To_Lower (ARM_Contents.Lookup_Title (
				          ARM_Contents.Clause, Format_Object.Section, Format_Object.Clause)) then
				        Ada.Text_IO.Put_Line ("** Unable to match title with section numbers, line " & ARM_Input.Line_String (Input_Object));
			            end if;
			        elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Section or else
				      Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Section_No_Break then
				    Check_End_Paragraph; -- End any paragraph that we're in.
				    ARM_Output.Clause_Header (Output_Object,
				        Title(1..Title_Length),
					Level => ARM_Contents.Section,
					Clause_Number => Clause_Number,
					No_Page_Break => Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Section_No_Break);
			            -- Check that the section numbers match the title:
			            if Ada.Characters.Handling.To_Lower (Title) /=
			               Ada.Characters.Handling.To_Lower (ARM_Contents.Lookup_Title (
				          ARM_Contents.Section, Format_Object.Section)) then
				        Ada.Text_IO.Put_Line ("** Unable to match title with section numbers, line " & ARM_Input.Line_String (Input_Object));
			            end if;
			        elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Unnumbered_Section then
				    -- These are numbered like a clause, with a
				    -- section number of 0.
				    Check_End_Paragraph; -- End any paragraph that we're in.
				    ARM_Output.Clause_Header (Output_Object,
				        Title(1..Title_Length),
					Level => ARM_Contents.Unnumbered_Section,
					Clause_Number => Clause_Number);
				    -- Note that we don't check the clause numbers,
				    -- because we don't care about them.
				elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Normative_Annex then
				    Check_End_Paragraph; -- End any paragraph that we're in.
				    ARM_Output.Clause_Header (Output_Object,
				        Title(1..Title_Length),
					Level => ARM_Contents.Normative_Annex,
					Clause_Number => Clause_Number);
			            -- Check that the section numbers match the title:
			            if Ada.Characters.Handling.To_Lower (Title) /=
			               Ada.Characters.Handling.To_Lower (ARM_Contents.Lookup_Title (
				          ARM_Contents.Normative_Annex, Format_Object.Section)) then
				        Ada.Text_IO.Put_Line ("** Unable to match title with section numbers, line " & ARM_Input.Line_String (Input_Object));
			            end if;
				else --if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Informative_Annex then
				    Check_End_Paragraph; -- End any paragraph that we're in.
				    ARM_Output.Clause_Header (Output_Object,
				        Title(1..Title_Length),
					Level => ARM_Contents.Informative_Annex,
					Clause_Number => Clause_Number);
			            -- Check that the section numbers match the title:
			            if Ada.Characters.Handling.To_Lower (Title) /=
			               Ada.Characters.Handling.To_Lower (ARM_Contents.Lookup_Title (
				          ARM_Contents.Informative_Annex, Format_Object.Section)) then
				        Ada.Text_IO.Put_Line ("** Unable to match title with section numbers, line " & ARM_Input.Line_String (Input_Object));
			            end if;
			        end if;
			    end;
			exception
			    when ARM_Contents.Not_Found_Error =>
				Ada.Text_IO.Put_Line ("** Unable to find header reference, line " & ARM_Input.Line_String (Input_Object));
				Ada.Text_IO.Put_Line ("   Looking for " & Title(1..Title_Length));
			end;
		    end;
		    -- Reset the paragraph numbers:
		    Format_Object.Next_Paragraph := 1;
		    Format_Object.Next_Insert_Para := 1;
		    Format_Object.Next_AARM_Sub := 'a';
		    -- Reset the note number, only for sections:
		    if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Section or else
		       Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Section_No_Break or else
		       Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Informative_Annex or else
		       Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Normative_Annex then
		        Format_Object.Next_Note := 1;
		    end if;
		    -- Reset the subhead:
		    Format_Object.Last_Paragraph_Subhead_Type := Plain;
		    Format_Object.Next_Paragraph_Format_Type := Plain;

		    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Header)");

		when Labeled_Revised_Normative_Annex |
		     Labeled_Revised_Clause |
		     Labeled_Revised_Subclause =>
		    -- Load the title into the Title string:
		    declare
			New_Title : ARM_Contents.Title_Type;
			New_Title_Length : Natural;
			Old_Title : ARM_Contents.Title_Type;
			Old_Title_Length : Natural;
			Ch : Character;
			Version : ARM_Contents.Change_Version_Type := '0';
		    begin
			Get_Change_Version (Is_First => True,
					    Version => Version);
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "New" & (4..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Ch);
			if Ch /= ' ' then
			    -- There is a parameter:
			    -- Load the new title into the Title string:
			    ARM_Input.Copy_to_String_until_Close_Char (
			        Input_Object,
			        Ch,
			        New_Title, New_Title_Length);
			    New_Title(New_Title_Length+1 .. New_Title'Last) :=
				(others => ' ');
			    ARM_Input.Check_Parameter_Name (Input_Object,
			        Param_Name => "Old" & (4..ARM_Input.Command_Name_Type'Last => ' '),
			        Is_First => False,
			        Param_Close_Bracket => Ch);
			    if Ch /= ' ' then
			        -- There is a parameter:
			        -- Load the new title into the Title string:
			        ARM_Input.Copy_to_String_until_Close_Char (
			            Input_Object,
			            Ch,
			            Old_Title, Old_Title_Length);
			        Old_Title(Old_Title_Length+1 .. Old_Title'Last) :=
				    (others => ' ');
			    end if;
			end if;
		        ARM_Input.Get_Char (Input_Object, Ch);
		        if Ch /= Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char then
			    Ada.Text_IO.Put_Line ("  ** Bad close for Labeled_Revised_(SubClause|Annex) on line " & ARM_Input.Line_String (Input_Object));
			    ARM_Input.Replace_Char (Input_Object);
		        end if;

		        if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Revised_Subclause then
			    Format_Object.Subclause := Format_Object.Subclause + 1;
		        elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Revised_Clause then
			    Format_Object.Clause := Format_Object.Clause + 1;
			    Format_Object.Subclause := 0;
		        else
			    Format_Object.Clause := 0;
			    Format_Object.Subclause := 0;
			end if;

			begin
			    declare
			        Clause_Number : constant String :=
				    ARM_Contents.Lookup_Clause_Number (New_Title);
				Level : ARM_Contents.Level_Type;
			    begin
			        Check_End_Paragraph; -- End any paragraph that we're in.
			        if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Revised_Subclause then
				    Level := ARM_Contents.Subclause;
			            -- Check that the section numbers match the title:
			            if Ada.Characters.Handling.To_Lower (New_Title) /=
			               Ada.Characters.Handling.To_Lower (ARM_Contents.Lookup_Title (
				          ARM_Contents.Subclause, Format_Object.Section,
					  Format_Object.Clause, Format_Object.Subclause)) then
				        Ada.Text_IO.Put_Line ("** Unable to match title with section numbers, line " & ARM_Input.Line_String (Input_Object));
			            end if;
			        elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Revised_Clause then
				    Level := ARM_Contents.Clause;
			            -- Check that the section numbers match the title:
			            if Ada.Characters.Handling.To_Lower (New_Title) /=
			               Ada.Characters.Handling.To_Lower (ARM_Contents.Lookup_Title (
				          ARM_Contents.Clause, Format_Object.Section, Format_Object.Clause)) then
				        Ada.Text_IO.Put_Line ("** Unable to match title with section numbers, line " & ARM_Input.Line_String (Input_Object));
			            end if;
				else -- Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Revised_Normative_Annex then
				    Level := ARM_Contents.Normative_Annex;
			            -- Check that the section numbers match the title:
			            if Ada.Characters.Handling.To_Lower (New_Title) /=
			               Ada.Characters.Handling.To_Lower (ARM_Contents.Lookup_Title (
				          ARM_Contents.Normative_Annex, Format_Object.Section)) then
				        Ada.Text_IO.Put_Line ("** Unable to match title with section numbers, line " & ARM_Input.Line_String (Input_Object));
			            end if;
				end if;
			        if Format_Object.Change_Version < Version then
				    ARM_Output.Clause_Header (Output_Object,
				        Old_Title(1..Old_Title_Length),
				        Level => Level,
				        Clause_Number => Clause_Number);
			        else
				    case Format_Object.Changes is
				        when ARM_Format.Old_Only =>
					    ARM_Output.Clause_Header (Output_Object,
					        Old_Title(1..Old_Title_Length),
					        Level => Level,
					        Clause_Number => Clause_Number);
				        when ARM_Format.New_Only =>
					    ARM_Output.Clause_Header (Output_Object,
					        New_Title(1..New_Title_Length),
					        Level => Level,
					        Clause_Number => Clause_Number);
				        when ARM_Format.Show_Changes =>
					    ARM_Output.Revised_Clause_Header (Output_Object,
					        New_Header_Text => New_Title(1..New_Title_Length),
					        Old_Header_Text => Old_Title(1..Old_Title_Length),
					        Level => Level,
					        Version => Version,
					        Clause_Number => Clause_Number);
				        when ARM_Format.Changes_Only =>
					    if Format_Object.Change_Version = Version then
					        ARM_Output.Revised_Clause_Header (Output_Object,
					            New_Header_Text => New_Title(1..New_Title_Length),
					            Old_Header_Text => Old_Title(1..Old_Title_Length),
						    Level => Level,
						    Version => Version,
						    Clause_Number => Clause_Number);
					    else
					        ARM_Output.Clause_Header (Output_Object,
					            New_Title(1..New_Title_Length),
						    Level => Level,
						    Clause_Number => Clause_Number);
					    end if;
				        when ARM_Format.New_Changes =>
					    ARM_Output.Revised_Clause_Header (Output_Object,
					        New_Header_Text => New_Title(1..New_Title_Length),
					        Old_Header_Text => " ",
					        Level => Level,
					        Version => Version,
					        Clause_Number => Clause_Number);
				    end case;
			        end if;

			    end;
			exception
			    when ARM_Contents.Not_Found_Error =>
				Ada.Text_IO.Put_Line ("** Unable to find header reference, line " & ARM_Input.Line_String (Input_Object));
				Ada.Text_IO.Put_Line ("   Looking for " & New_Title(1..New_Title_Length));
			end;
		        -- Reset the paragraph numbers:
		        Format_Object.Next_Paragraph := 1;
		        Format_Object.Next_Insert_Para := 1;
		        Format_Object.Next_AARM_Sub := 'a';
		        -- Reset the subhead:
		        Format_Object.Last_Paragraph_Subhead_Type := Plain;
		        Format_Object.Next_Paragraph_Format_Type := Plain;

			Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Header)");
		    end;

		when Labeled_Added_Clause |
		     Labeled_Added_Subclause =>
		    -- Load the title into the Title string:
		    declare
			New_Title : ARM_Contents.Title_Type;
			New_Title_Length : Natural;
			Ch : Character;
			Version : ARM_Contents.Change_Version_Type := '0';
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
			        New_Title, New_Title_Length);
			    New_Title(New_Title_Length+1 .. New_Title'Last) :=
				(others => ' ');
			end if;
		        ARM_Input.Get_Char (Input_Object, Ch);
		        if Ch /= Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char then
			    Ada.Text_IO.Put_Line ("  ** Bad close for Labeled_Added_(Sub)Clause on line " & ARM_Input.Line_String (Input_Object));
			    ARM_Input.Replace_Char (Input_Object);
		        end if;

		        if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Added_Subclause then
			    Format_Object.Subclause := Format_Object.Subclause + 1;
		        else -- Labeled_Added_Clause
			    Format_Object.Clause := Format_Object.Clause + 1;
			    Format_Object.Subclause := 0;
			end if;

			begin
			    declare
			        Clause_Number : constant String :=
				    ARM_Contents.Lookup_Clause_Number (New_Title);
			    begin
			        if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Added_Subclause then
				    if Format_Object.Change_Version < Version then
					null; -- Ignore this.
				    else
				        case Format_Object.Changes is
					    when ARM_Format.Old_Only =>
						null; -- Ignore this.
					    when ARM_Format.New_Only =>
						Check_End_Paragraph; -- End any paragraph that we're in.
					        ARM_Output.Clause_Header (Output_Object,
					            New_Title(1..New_Title_Length),
						    Level => ARM_Contents.Subclause,
						    Clause_Number => Clause_Number);
					    when ARM_Format.Show_Changes | ARM_Format.New_Changes =>
						Check_End_Paragraph; -- End any paragraph that we're in.
					        ARM_Output.Revised_Clause_Header (Output_Object,
					            New_Header_Text => New_Title(1..New_Title_Length),
					            Old_Header_Text => "",
						    Level => ARM_Contents.Subclause,
						    Version => Version,
						    Clause_Number => Clause_Number);
					    when ARM_Format.Changes_Only =>
						Check_End_Paragraph; -- End any paragraph that we're in.
					        if Format_Object.Change_Version = Version then
					            ARM_Output.Revised_Clause_Header (Output_Object,
					                New_Header_Text => New_Title(1..New_Title_Length),
					                Old_Header_Text => "",
						        Level => ARM_Contents.Subclause,
						        Version => Version,
						        Clause_Number => Clause_Number);
						else
					            ARM_Output.Clause_Header (Output_Object,
					                New_Title(1..New_Title_Length),
						        Level => ARM_Contents.Subclause,
						        Clause_Number => Clause_Number);
						end if;
				        end case;
				    end if;
			            -- Check that the section numbers match the title:
			            if Ada.Characters.Handling.To_Lower (New_Title) /=
			               Ada.Characters.Handling.To_Lower (ARM_Contents.Lookup_Title (
				          ARM_Contents.Subclause, Format_Object.Section,
					  Format_Object.Clause, Format_Object.Subclause)) then
				        Ada.Text_IO.Put_Line ("** Unable to match title with section numbers, line " & ARM_Input.Line_String (Input_Object));
			            end if;
			        else -- Labeled_Added_Clause
				    if Format_Object.Change_Version < Version then
					-- Ignore the change, the version is too high.
					null;
				    else
				        case Format_Object.Changes is
					    when ARM_Format.Old_Only =>
						null; -- Nothing to do.
					    when ARM_Format.New_Only =>
					        Check_End_Paragraph; -- End any paragraph that we're in.
					        ARM_Output.Clause_Header (Output_Object,
					            New_Title(1..New_Title_Length),
						    Level => ARM_Contents.Clause,
						    Clause_Number => Clause_Number);
					    when ARM_Format.Show_Changes | ARM_Format.New_Changes =>
					        Check_End_Paragraph; -- End any paragraph that we're in.
					        ARM_Output.Revised_Clause_Header (Output_Object,
					            New_Header_Text => New_Title(1..New_Title_Length),
					            Old_Header_Text => "",
						    Level => ARM_Contents.Clause,
						    Version => Version,
						    Clause_Number => Clause_Number);
					    when ARM_Format.Changes_Only =>
					        Check_End_Paragraph; -- End any paragraph that we're in.
					        if Format_Object.Change_Version = Version then
					            ARM_Output.Revised_Clause_Header (Output_Object,
					                New_Header_Text => New_Title(1..New_Title_Length),
					                Old_Header_Text => "",
						        Level => ARM_Contents.Clause,
						        Version => Version,
						        Clause_Number => Clause_Number);
						else
					            ARM_Output.Clause_Header (Output_Object,
					                New_Title(1..New_Title_Length),
						        Level => ARM_Contents.Clause,
						        Clause_Number => Clause_Number);
						end if;
				        end case;
				    end if;
			            -- Check that the section numbers match the title:
			            if Ada.Characters.Handling.To_Lower (New_Title) /=
			               Ada.Characters.Handling.To_Lower (ARM_Contents.Lookup_Title (
				          ARM_Contents.Clause, Format_Object.Section, Format_Object.Clause)) then
				        Ada.Text_IO.Put_Line ("** Unable to match title with section numbers, line " & ARM_Input.Line_String (Input_Object));
			            end if;
				end if;
			    end;
			exception
			    when ARM_Contents.Not_Found_Error =>
				Ada.Text_IO.Put_Line ("** Unable to find header reference, line " & ARM_Input.Line_String (Input_Object));
				Ada.Text_IO.Put_Line ("   Looking for " & New_Title(1..New_Title_Length));
			end;
		        -- Reset the paragraph numbers:
		        Format_Object.Next_Paragraph := 1;
		        Format_Object.Next_Insert_Para := 1;
		        Format_Object.Next_AARM_Sub := 'a';
		        -- Reset the subhead:
		        Format_Object.Last_Paragraph_Subhead_Type := Plain;
		        Format_Object.Next_Paragraph_Format_Type := Plain;

			Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Header)");
		    end;

		when Preface_Section =>
		    Check_End_Paragraph; -- End any paragraph that we're in.
		    --ARM_Output.New_Page (Output_Object, ARM_Output.Odd_Page_Only);
		    ARM_Output.Clause_Header (Output_Object,
					      Header_Text => "",
					      Level => ARM_Contents.Unnumbered_Section,
					      Clause_Number => "0.9");

		when Subheading =>
		    -- This is used in preface sections where no numbers or
		    -- contents are desired.
		    Check_End_Paragraph; -- End any paragraph that we're in.
		    ARM_Output.Start_Paragraph (Output_Object,
			     Format => ARM_Output.Wide,
			     Number => "",
			     No_Breaks => True, Keep_with_Next => True);
		    Format_Object.In_Paragraph := True;
		    ARM_Output.Text_Format (Output_Object,
			   Bold => True, Italic => False,
			   Font => ARM_Output.Swiss,
			   Size => 0,
			   Change => ARM_Output.None,
			   Location => ARM_Output.Normal);
		    ARM_Output.Text_Format (Output_Object,
			   Bold => True, Italic => False,
			   Font => ARM_Output.Swiss,
			   Size => 2,
			   Change => ARM_Output.None,
			   Location => ARM_Output.Normal);
			-- Separate calls to Text_Format so we can use "Grow"
			-- in here.
		    Format_Object.Is_Bold := True;
		    Format_Object.Font := ARM_Output.Swiss;
		    Format_Object.Size := 2;

		when Added_Subheading =>
		    -- This is used in preface sections where no numbers or
		    -- contents are desired.
		    declare
			Ch : Character;
			Version : ARM_Contents.Change_Version_Type := '0';
		    begin
			Get_Change_Version (Is_First => True,
					    Version => Version);
		        ARM_Input.Get_Char (Input_Object, Ch);
		        if Ch /= ',' then
			    Ada.Text_IO.Put_Line ("  ** Missing comma for AddedSubheading on line " & ARM_Input.Line_String (Input_Object));
			    ARM_Input.Replace_Char (Input_Object);
		        end if;
			Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version := Version;
		        if Format_Object.Change_Version < Version then
			    -- Ignore the change, the version is too high.
			    -- Skip the text:
			    ARM_Input.Skip_until_Close_Char (Input_Object,
			        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char);
			    ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
		        else
			    case Format_Object.Changes is
			        when ARM_Format.Old_Only =>
				    -- Skip the text:
				    ARM_Input.Skip_until_Close_Char (Input_Object,
				        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char);
				    ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
			        when ARM_Format.New_Only =>
			            ARM_Output.Start_Paragraph (Output_Object,
			                 Format => ARM_Output.Wide,
			                 Number => "",
			                 No_Breaks => True, Keep_with_Next => True);
			            Format_Object.In_Paragraph := True;
			            ARM_Output.Text_Format (Output_Object,
			               Bold => True, Italic => False,
			               Font => ARM_Output.Swiss,
			               Size => 0,
			               Change => ARM_Output.None,
			               Location => ARM_Output.Normal);
			            ARM_Output.Text_Format (Output_Object,
			               Bold => True, Italic => False,
			               Font => ARM_Output.Swiss,
			               Size => 2,
			               Change => ARM_Output.None,
			               Location => ARM_Output.Normal);
				        -- Separate calls to Text_Format so we can use "Grow"
				        -- in here.
			            Format_Object.Is_Bold := True;
			            Format_Object.Font := ARM_Output.Swiss;
			            Format_Object.Size := 2;
			        when ARM_Format.Show_Changes | ARM_Format.New_Changes | ARM_Format.Changes_Only =>
			            ARM_Output.Start_Paragraph (Output_Object,
			                 Format => ARM_Output.Wide,
			                 Number => "",
			                 No_Breaks => True, Keep_with_Next => True);
			            Format_Object.In_Paragraph := True;
			            ARM_Output.Text_Format (Output_Object,
			               Bold => True, Italic => False,
			               Font => ARM_Output.Swiss,
			               Size => 0,
			               Change => ARM_Output.Insertion,
			               Version => Version,
			               Location => ARM_Output.Normal);
			            ARM_Output.Text_Format (Output_Object,
			               Bold => True, Italic => False,
			               Font => ARM_Output.Swiss,
			               Size => 2,
			               Change => ARM_Output.Insertion,
			               Version => Version,
			               Location => ARM_Output.Normal);
				        -- Separate calls to Text_Format so we can use "Grow"
				        -- in here.
			            Format_Object.Is_Bold := True;
			            Format_Object.Font := ARM_Output.Swiss;
			            Format_Object.Size := 2;
		                end case;
		            end if;
		        end;
		    when Heading =>
		        -- This is used in preface sections where no numbers or
		    -- contents are desired.
		    Check_End_Paragraph; -- End any paragraph that we're in.
		    ARM_Output.Start_Paragraph (Output_Object,
			     Format => ARM_Output.Wide,
			     Number => "",
			     No_Breaks => True, Keep_with_Next => True,
			     Justification => ARM_Output.Center);
		    Format_Object.In_Paragraph := True;
		    ARM_Output.Text_Format (Output_Object,
			   Bold => True, Italic => False,
			   Font => ARM_Output.Swiss,
			   Size => 0,
			   Change => ARM_Output.None,
			   Location => ARM_Output.Normal);
		    ARM_Output.Text_Format (Output_Object,
			   Bold => True, Italic => False,
			   Font => ARM_Output.Swiss,
			   Size => 3,
			   Change => ARM_Output.None,
			   Location => ARM_Output.Normal);
			-- Separate calls to Text_Format so we can use "Grow"
			-- in here.
		    Format_Object.Is_Bold := True;
		    Format_Object.Font := ARM_Output.Swiss;
		    Format_Object.Size := 3;

		when Center =>
		    Check_End_Paragraph; -- End any paragraph that we're in.
		    ARM_Output.Start_Paragraph (Output_Object,
			     Format => ARM_Output.Normal,
			     Number => "",
			     No_Breaks => True, Keep_with_Next => False,
			     Justification => ARM_Output.Center);
		    Format_Object.In_Paragraph := True;

		when Right =>
		    Check_End_Paragraph; -- End any paragraph that we're in.
		    ARM_Output.Start_Paragraph (Output_Object,
			     Format => ARM_Output.Normal,
			     Number => "",
			     No_Breaks => True, Keep_with_Next => False,
			     Justification => ARM_Output.Right);
		    Format_Object.In_Paragraph := True;

		when Ref_Section | Ref_Section_Number =>
		    -- Load the title into the Title string:
		    declare
			Ch : Character;
			Title : ARM_Contents.Title_Type;
			Title_Length : Natural;
		    begin
		        ARM_Input.Get_Char (Input_Object, Ch);
			Title_Length := 0;
			while Ch /= Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char loop
			    Title_Length := Title_Length + 1;
			    Title(Title_Length) := Ch;
			    ARM_Input.Get_Char (Input_Object, Ch);
			end loop;
			Title(Title_Length+1 .. Title'Last) :=
			    (others => ' ');

			begin
			    declare
			        Clause_Number_Text : constant String :=
				    ARM_Contents.Lookup_Clause_Number (Title);
			    begin
			        if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Ref_Section then
				    Check_Paragraph;
				    ARM_Output.Clause_Reference (Output_Object,
				        Text => Clause_Number_Text,
					Clause_Number => Clause_Number_Text);
				    ARM_Output.Ordinary_Text (Output_Object, ", ");
				    -- Was: (To match the Ada 95 Standard)
				    --ARM_Output.Special_Character (Output_Object, ARM_Output.Left_Quote);
				    --ARM_Output.Special_Character (Output_Object, ARM_Output.Left_Quote);
				    ARM_Output.Special_Character (Output_Object, ARM_Output.Left_Double_Quote);
				    if Format_Object.Change_Version < '1' or else
				       Format_Object.Changes = ARM_Format.Old_Only then
					-- Use original version:
				        declare
					    Section_Number : ARM_Contents.Section_Number_Type;
					    Clause_Number : Natural;
					    Subclause_Number : Natural;
				        begin
					    ARM_Contents.Make_Clause (Clause_Number_Text,
					        Section_Number, Clause_Number, Subclause_Number);
					    ARM_Output.Clause_Reference (Output_Object,
					        Text => Ada.Strings.Fixed.Trim (
						    ARM_Contents.Lookup_Old_Title (
							    ARM_Contents.Lookup_Level (Title),
							    Section_Number,
							    Clause_Number, Subclause_Number),
							    Ada.Strings.Right),
					        Clause_Number => Clause_Number_Text);
				        end;
				    else
					-- Use new version. We don't have version numbers for these yet.
				        ARM_Output.Clause_Reference (Output_Object,
					    Text => Title(1..Title_Length),
					    Clause_Number => Clause_Number_Text);
				    end if;
				    -- Was: (To match the Ada 95 Standard)
				    --ARM_Output.Special_Character (Output_Object, ARM_Output.Right_Quote);
				    --ARM_Output.Special_Character (Output_Object, ARM_Output.Right_Quote);
				    ARM_Output.Special_Character (Output_Object, ARM_Output.Right_Double_Quote);
			        else -- Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Ref_Section_Number then
				    Check_Paragraph;
				    ARM_Output.Clause_Reference (Output_Object,
				        Text => Clause_Number_Text,
					Clause_Number => Clause_Number_Text);
			        end if;
				Format_Object.Last_Non_Space := True;
			    end;
			exception
			    when ARM_Contents.Not_Found_Error =>
				Ada.Text_IO.Put_Line ("** Unable to find section reference, line " & ARM_Input.Line_String (Input_Object));
				Ada.Text_IO.Put_Line ("   Looking for " & Title(1..Title_Length));
			end;
		    end;

		    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Section Reference)");

		when Ref_Section_by_Number =>
		    -- Load the number into the Number string:
		    declare
			Ch : Character;
			Number : String(1..20);
			Number_Length : Natural;
		    begin
		        ARM_Input.Get_Char (Input_Object, Ch);
			Number_Length := 0;
			while Ch /= Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char loop
			    Number_Length := Number_Length + 1;
			    Number(Number_Length) := Ch;
			    ARM_Input.Get_Char (Input_Object, Ch);
			end loop;
			Number(Number_Length+1 .. Number'Last) :=
			    (others => ' ');

		        Check_Paragraph;
		        ARM_Output.Clause_Reference (Output_Object,
			    Text => Number(1..Number_Length),
			    Clause_Number => Number(1..Number_Length));
		        Format_Object.Last_Non_Space := True;
			-- No checking here.
		    end;

		    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Section Num Reference)");

		-- Change commands:

		when Change =>
		    -- This command is of the form:
		    -- @chg{[version=<Version>],new=[<new text>],old=[<old text>]}.
		    -- where the parameter names (other than version) are
		    -- optional (but highly recommended), and the curly and
		    -- square brackets can be any of the allowed bracketing characters.
		    -- We have to process this in parts, in order that the
		    -- text can be handled normally.

		    declare
			Ch : Character;
			Saw_Version : Boolean;
		    begin
		        -- Check for the optional "Version" parameter:
			ARM_Input.Get_Char (Input_Object, Ch);
			ARM_Input.Replace_Char (Input_Object);
			if Ch = 'V' or else Ch = 'v' then
			    -- There is a Version parameter, grab it.
			    Get_Change_Version (Is_First => True,
			        Version => Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version);
			        -- Read a parameter named "Version".
			    Saw_Version := True;
			else
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version := '1';
			    Saw_Version := False;
			end if;

		        -- Save the current state:
		        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Prev_Change :=
			    Format_Object.Change;
		        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Prev_Change_Version :=
			    Format_Object.Current_Change_Version;
		        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Prev_Old_Change_Version :=
			    Format_Object.Current_Old_Change_Version;

		        -- Check and handle the "New" parameter:
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "New" & (4..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => not Saw_Version,
			    Param_Close_Bracket => Ch);
			if Ch /= ' ' then
			    -- There is a parameter:

			    -- Stack the parameter so we can process the end:
			    Set_Nesting_for_Parameter
			        (Command => Change_Param_New,
				 Close_Ch => Ch);

			    Format_Object.In_Change := True;

			    -- Now, handle the parameter:
			    if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version >
			       Format_Object.Change_Version then
				-- Ignore any changes with version numbers higher than
				-- the current maximum.
			        -- Skip the text:
			        ARM_Input.Skip_until_Close_Char (Input_Object,
				    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char);
			        ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
			    else
			        case Format_Object.Changes is
				    when ARM_Format.Old_Only =>
				        -- Skip the text:
			                ARM_Input.Skip_until_Close_Char (Input_Object,
				            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char);
				        ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
				    when ARM_Format.New_Only =>
				        if ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind,
				           ARM_Database.Deleted) then
					    -- In a deleted paragraph, call Check_Paragraph
					    -- to trigger the "deleted paragraph" message.
					    -- (Otherwise, this never happens.)
				            Check_Paragraph;
				        -- else null; -- Nothing special to do.
				        end if;
				    when ARM_Format.Changes_Only |
					 ARM_Format.Show_Changes |
					 ARM_Format.New_Changes =>
					if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version <
					    Format_Object.Change_Version and then
					    Format_Object.Changes = ARM_Format.Changes_Only then
					    -- Just normal output text.
				            if ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind,
				               ARM_Database.Deleted) then
					        -- In a deleted paragraph, call Check_Paragraph
					        -- to trigger the "deleted paragraph" message.
					        -- (Otherwise, this never happens.)
				                Check_Paragraph;
				            -- else null; -- Nothing special to do.
				            end if;
					else
				            ARM_Input.Get_Char (Input_Object, Ch);
				            ARM_Input.Replace_Char (Input_Object);
					    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Was_Text :=
					        Ch /= Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char;
				            if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Was_Text then
						-- Non-empty text; Calculate new change state (current is insertion):
					        case Format_Object.Change is
					            when ARM_Output.Insertion | ARM_Output.None =>
						        Format_Object.Change := ARM_Output.Insertion;
						        Format_Object.Current_Change_Version :=
						           Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
						        Format_Object.Current_Old_Change_Version := '0';
					            when ARM_Output.Deletion =>
						        Format_Object.Change := ARM_Output.Both;
						        Format_Object.Current_Old_Change_Version := -- The insertion should be older.
						           Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
						        -- Current_Version is unchanged.
					            when ARM_Output.Both =>
						        Format_Object.Change := ARM_Output.Both;
						        Format_Object.Current_Old_Change_Version := -- The insertion should be older.
						           Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
						        -- Current_Version is unchanged.
					        end case;
				                Check_Paragraph;
				                ARM_Output.Text_Format (Output_Object,
							                Bold => Format_Object.Is_Bold,
							                Italic => Format_Object.Is_Italic,
							                Font => Format_Object.Font,
							                Size => Format_Object.Size,
							                Change => Format_Object.Change,
							                Version => Format_Object.Current_Change_Version,
							                Added_Version => Format_Object.Current_Old_Change_Version,
							                Location => Format_Object.Location);
				            -- else no text, so don't bother.
				            end if;
					end if;
			        end case;
			    end if;
			-- else no parameter. Weird.
			end if;
		    end;

		when Change_Param_Old | Change_Param_New =>
		    -- These can't get here; they represent the parameters of
		    -- "Change" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** Change parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Table_Param_Caption | Table_Param_Header | Table_Param_Body =>
		    -- These can't get here; they represent the parameters of
		    -- "Table" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** Table parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Syntax_Rule_RHS =>
		    -- This can't get here; it represents the second parameter of
		    -- "Syn" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** Syntax parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Glossary_Text_Param =>
		    -- This can't get here; it represents the second parameter of
		    -- "ToGlossary" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** Glossary parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Attribute_Text_Param =>
		    -- This can't get here; it represents the third parameter of
		    -- "Attribute" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** Attribute parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Change_Impdef_Text_Param =>
		    -- This can't get here; it represents the second parameter of
		    -- "ChgImpldef" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** Impdef parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Change_Prefix_Text_Param =>
		    -- This can't get here; it represents the second parameter of
		    -- "ChgPrefixType" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** ChgPrefix parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Change_Reference =>
		    -- This command is of the form:
		    -- @chgref{Version=[<version>], Kind=(<kind>)
		    --   {,Ref=(<DR_Number>)}{,ARef=(<AI_Number>)}}
		    -- where <version> is a single character, <Kind> is one
		    -- of Revised, Added, or Deleted, followed by zero or more
		    -- DR references, and finally zero or more AI references.
		    -- As usual, any of the allowed bracketing characters can
		    -- be used.

		    declare
			Ch : Character;
			Kind : ARM_Database.Paragraph_Change_Kind_Type;
			Version : ARM_Contents.Change_Version_Type;
			Display_It : Boolean;
        	    begin
			Get_Change_Version (Is_First => True,
			    Version => Version);
			    -- Read a parameter named "Version".

			Get_Change_Kind (Kind);
			    -- Read a parameter named "Kind".

			if Version <= Format_Object.Change_Version then
			    Format_Object.Next_Paragraph_Version := Version;
			    Format_Object.Next_Paragraph_Change_Kind := Kind;
			    case Format_Object.Document is
			        when ARM_Format.AARM =>
				    Display_It := True;
				when ARM_Format.RM | ARM_Format.RM_ISO =>
				    Display_It := False; -- No references in RM.
			    end case;
            		else --This reference is too new, ignore it.
			    Display_It := False;
			end if;

		        -- Handle zero or more "Ref" parameters.
		        loop
			    ARM_Input.Get_Char (Input_Object, Ch);
			    if Ch /= ',' then
			        exit; -- No more commands.
			    else
			        ARM_Input.Replace_Char (Input_Object);
			    end if;
			    Gen_Ref_or_ARef_Parameter (Display_It);
				-- Read (and possibly generate) a "Ref" or "ARef" parameter.
		        end loop;

			-- Now, close the command.
			-- Ch was read when checking for commas, above.
		        if Ch /= Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char then
			    Ada.Text_IO.Put_Line ("  ** Bad close for Chgref on line " & ARM_Input.Line_String (Input_Object));
			    ARM_Input.Replace_Char (Input_Object);
		        end if;
		        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		            -- Remove the "Change_Reference" record.
		    end;

		when Change_Note =>
		    -- Skip the contents of this command.
	            ARM_Input.Skip_until_Close_Char (Input_Object,
			Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char);
		    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		        -- Remove the "Change_Note" record.

		when Change_Implementation_Defined =>
		    -- This command is of the form:
		    -- @chgimpldef{Version=[<version>], Kind=(<kind>),
		    --   Text=(<text>)}}
		    -- where <version> is a single character, <Kind> is one
		    -- of Revised, Added, or Deleted, and this is followed
		    -- by the text. As usual, any of the
		    -- allowed bracketing characters can be used.
		    declare
			Close_Ch : Character;
			Kind : ARM_Database.Paragraph_Change_Kind_Type;
			Version : ARM_Contents.Change_Version_Type;
			Display_It : Boolean;
		    begin
			Get_Change_Version (Is_First => True,
			    Version => Version);
			    -- Read a parameter named "Version".

			Get_Change_Kind (Kind);
			    -- Read a parameter named "Kind".

			if Version <= Format_Object.Change_Version then
			    Format_Object.Impdef_Version := Version;
			    Format_Object.Impdef_Change_Kind := Kind;
			    case Format_Object.Document is
			        when ARM_Format.AARM =>
				    Display_It := True;
				when ARM_Format.RM | ARM_Format.RM_ISO =>
				    Display_It := False; -- No impdef note in RM.
			    end case;
			else --This reference is too new, ignore it.
			    Display_It := False;
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Text" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
		        if Close_Ch /= ' ' then
		            -- Stack it so we can process the end:
			    Set_Nesting_for_Parameter
			        (Command => Change_Impdef_Text_Param,
				 Close_Ch => Close_Ch);

			    ARM_Input.Start_Recording (Input_Object);

			    if Format_Object.In_Paragraph then
				-- Do this to preserve any inserted paragraph info.
				Format_Object.Impdef_Paragraph_String :=
				    Format_Object.Current_Paragraph_String;
				Format_Object.Impdef_Paragraph_Len :=
				    Format_Object.Current_Paragraph_Len;
			    else
				declare
				    PNum : constant String := Positive'Image (
					Format_Object.Next_Paragraph - 1);
				begin
				    Format_Object.Impdef_Paragraph_Len := PNum'Length - 1;
				    Format_Object.Impdef_Paragraph_String (1 .. PNum'Last-1) :=
					PNum (2 .. PNum'Last);
				end;
			    end if;

			    if Display_It then
			        Check_End_Paragraph; -- End any paragraph that we're in.
			        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Last_Subhead_Paragraph := Format_Object.Last_Paragraph_Subhead_Type;
			        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Subhead_Paragraph := Format_Object.Next_Paragraph_Subhead_Type;
			        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Paragraph_Format := Format_Object.Next_Paragraph_Format_Type;
			        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Tab_Stops := Format_Object.Paragraph_Tab_Stops;
			        Format_Object.Next_Paragraph_Format_Type := Bare_Annotation;
			        Format_Object.Next_Paragraph_Subhead_Type := Bare_Annotation;
			        Format_Object.Next_Paragraph_Version := Format_Object.Impdef_Version;
			        Format_Object.Next_Paragraph_Change_Kind := Format_Object.Impdef_Change_Kind;
			        Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;
			        Check_Paragraph;
			        ARM_Output.Text_Format (Output_Object,
				    Bold => True,
				    Italic => Format_Object.Is_Italic,
				    Font => Format_Object.Font,
				    Size => Format_Object.Size,
				    Change => Format_Object.Change,
				    Version => Format_Object.Impdef_Version,
				    Location => Format_Object.Location);
			        ARM_Output.Ordinary_Text (Output_Object,
				     Text => "Implementation defined: ");
			        ARM_Output.Text_Format (Output_Object,
				    Bold => Format_Object.Is_Bold,
				    Italic => Format_Object.Is_Italic,
				    Font => Format_Object.Font,
				    Size => Format_Object.Size,
				    Change => Format_Object.Change,
				    Version => Format_Object.Impdef_Version,
				    Location => Format_Object.Location);
			        Format_Object.Last_Paragraph_Subhead_Type := Bare_Annotation;
			        Format_Object.Last_Non_Space := False;
			    else -- Don't display, skip the text:
			        ARM_Input.Skip_until_Close_Char (Input_Object,
				    Close_Ch);
			        ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
			    end if;
		        -- else no parameter. Weird.
		        end if;
		    end;

		when Change_Attribute =>
		     -- @ChgAttribute{Version=[<version>], Kind=(<kind>),
		     --    {A}Ref=[<DR_Number],Chginannex=[T|F],Leading=[T|F],
		     --    Prefix=<Prefix>,AttrName=<Name>,Text=<Text>}
		     -- Defines a changed attribute.
		    declare
			Close_Ch : Character;
			Key : ARM_Index.Index_Key;
			Chg_in_Annex : Boolean;
			Is_Leading : Boolean;
			Kind : ARM_Database.Paragraph_Change_Kind_Type;
			Version : ARM_Contents.Change_Version_Type;
			Display_Ref : Boolean;
		    begin
			Check_End_Paragraph; -- This is always a paragraph end.

			Get_Change_Version (Is_First => True,
			    Version => Version);
			    -- Read a parameter named "Version".

			Get_Change_Kind (Kind);
			    -- Read a parameter named "Kind".

			if Version <= Format_Object.Change_Version then
			    Format_Object.Next_Paragraph_Version := Version;
			    Format_Object.Next_Paragraph_Change_Kind := Kind;
			    case Format_Object.Document is
			        when ARM_Format.AARM =>
				    Display_Ref := True;
				when ARM_Format.RM | ARM_Format.RM_ISO =>
				    Display_Ref := False; -- No impdef note in RM.
			    end case;
			else --This reference is too new, ignore it.
			    Display_Ref := False;
			end if;

			Get_Boolean ("ChginAnnex" & (11..ARM_Input.Command_Name_Type'Last => ' '),
				     Chg_in_Annex);
			    -- Read a boolean parameter.

			if Chg_in_Annex then
			    Format_Object.Attr_Change_Kind := Kind;
			    Format_Object.Attr_Version := Version;
			else -- don't save the change info.; it only applies here.
			    Format_Object.Attr_Change_Kind := ARM_Database.None;
			    Format_Object.Attr_Version := '0';
			end if;

			Get_Boolean ("Leading" & (8..ARM_Input.Command_Name_Type'Last => ' '),
				     Is_Leading);
			    -- Read a boolean parameter.

			if Is_Leading then
			    Format_Object.Space_After := ARM_Output.Narrow;
			    Format_Object.Attr_Leading := True;
			else
			    Format_Object.Space_After := ARM_Output.Normal;
			    Format_Object.Attr_Leading := False;
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Prefix" & (7..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Save prefix:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
				Close_Ch,
				Format_Object.Attr_Prefix,
				Format_Object.Attr_Prefix_Len);
			-- else no parameter. Weird.
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "AttrName" & (9..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Save name:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
				Close_Ch,
				Format_Object.Attr_Name,
				Format_Object.Attr_Name_Len);
			-- else no parameter. Weird.
			end if;

			case Format_Object.Attr_Change_Kind is
			    when ARM_Database.None | ARM_Database.Revised =>
				-- The prefix is unchanged.
				-- Output <Prefix>'<Name> as the hanging text.
				Check_Paragraph;
				ARM_Output.Ordinary_Text (Output_Object,
					Format_Object.Attr_Prefix (1 .. Format_Object.Attr_Prefix_Len));
			        ARM_Output.Ordinary_Character (Output_Object, ''');
			        ARM_Output.Ordinary_Text (Output_Object,
					Format_Object.Attr_Name (1 .. Format_Object.Attr_Name_Len));
			        ARM_Output.End_Hang_Item (Output_Object);
				Format_Object.Last_Non_Space := False; -- Treat like start of a line.

				ARM_Index.Add (Term => "attributes",
					       Subterm => Format_Object.Attr_Name (1 .. Format_Object.Attr_Name_Len),
					       Kind => ARM_Index.Primary_Term_and_Subterm,
					       Clause => Clause_String,
					       Paragraph => Paragraph_String,
					       Key => Key);
				ARM_Output.Index_Target (Output_Object, Key);

				ARM_Index.Add (Term => Format_Object.Attr_Name (1 .. Format_Object.Attr_Name_Len) & " attribute",
					       Kind => ARM_Index.Primary_Term,
					       Clause => Clause_String,
					       Paragraph => Paragraph_String,
					       Key => Key);
				ARM_Output.Index_Target (Output_Object, Key);

			        Gen_Ref_or_ARef_Parameter(Display_Ref);
				    -- Read (and possibly generate) a "Ref" or "ARef" parameter.
				    -- (Note: we put it here so it appears after the hang item.)

			    when ARM_Database.Inserted | ARM_Database.Inserted_Normal_Number =>
			        Gen_Ref_or_ARef_Parameter(Display_Ref);
				    -- Read (and possibly generate) a "Ref" or "ARef" parameter.
				    -- (Note: we put it here so it appears before the hang item.)

				Ada.Text_IO.Put_Line ("  ** Attribute adding not implemented on line " & ARM_Input.Line_String (Input_Object));
				-- If in new mode, do above; if in show_changes or new_changes,
				-- show insertion; otherwise do not generate or
				-- store attribute in DB. The last would
				-- require changes to Attribute_Text_Param.
				-- Careful: We need to ignore this completely
				-- if it is added and we're not adding
				-- it because of the version being generated.

			    when ARM_Database.Deleted =>
			        Gen_Ref_or_ARef_Parameter(Display_Ref);
				    -- Read (and possibly generate) a "Ref" or "ARef" parameter.
				    -- (Note: we put it here so it appears before the hang item.)

				Ada.Text_IO.Put_Line ("  ** Attribute deleting not implemented on line " & ARM_Input.Line_String (Input_Object));
				-- If in old mode, do above; if in show_changes,
				-- show deletion; if in new_changes, generate an empty deletion;
				-- otherwise do not generate or
				-- store attribute in DB. The last would
				-- require changes to Attribute_Text_Param.
			end case;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Text" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Now, handle the parameter:
			    -- The text goes to the file *and* is recorded.
			    Arm_Input.Start_Recording (Input_Object);
			    -- Stack the parameter so we can process the end:
			    Set_Nesting_for_Parameter
			        (Command => Attribute_Text_Param,
				 Close_Ch => Close_Ch);
			end if;
		    end;

		when Change_Prefix_Type =>
		    -- This command is of the form:
		    -- @chgprefixtype{Version=[<version>], Kind=(<kind>),
		    --   Text=(<text>)}}
		    -- where <version> is a single character, <Kind> is one
		    -- of Revised, Added, or Deleted, and this is followed
		    -- by the text. As usual, any of the
		    -- allowed bracketing characters can be used.
		    declare
			Close_Ch : Character;
		    begin
			Get_Change_Version (Is_First => True,
					    Version => Format_Object.Attr_Prefix_Version);
			    -- Read a parameter named "Version".

			Get_Change_Kind (Format_Object.Attr_Prefix_Change_Kind);
			    -- Read a parameter named "Kind".

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Text" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
		        if Close_Ch /= ' ' then
		            -- Stack it so we can process the end:
			    Set_Nesting_for_Parameter
			        (Command => Change_Prefix_Text_Param,
				 Close_Ch => Close_Ch);

			    ARM_Input.Start_Recording (Input_Object);
		        -- else no parameter. Weird.
		        end if;
		    end;

		when Latin_1 =>
		    -- The parameter is the decimal code for the Latin-1
		    -- character to generate.
		    declare
			Value : String (1..5);
			Len : Natural := 0;
			Ch : Character;
		    begin
			ARM_Input.Get_Char (Input_Object, Ch);
			while Ch /= Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char loop
			    Len := Len + 1;
			    Value(Len) := Ch;
			    ARM_Input.Get_Char (Input_Object, Ch);
			end loop;
		        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		            -- Remove the "Latin-1" record.
			Check_Paragraph;
			ARM_Output.Ordinary_Character (Output_Object,
			    Character'Val(Natural'Value(Value(1..Len))));
			Format_Object.Last_Non_Space := True;
		    exception
			when Constraint_Error =>
			    Ada.Text_IO.Put_Line ("  ** Bad Latin-1 value [" &
						  Value(1..Len) & "] on line " &
							ARM_Input.Line_String (Input_Object));
		    end;

		when Ceiling =>
		     Check_Paragraph;
		     ARM_Output.Special_Character (Output_Object, ARM_Output.Left_Ceiling);
		     Format_Object.Last_Non_Space := True;

		when Floor =>
		     Check_Paragraph;
		     ARM_Output.Special_Character (Output_Object, ARM_Output.Left_Floor);
		     Format_Object.Last_Non_Space := True;

		when Absolute =>
		     Check_Paragraph;
		     ARM_Output.Ordinary_Character (Output_Object, '|');
		     Format_Object.Last_Non_Space := True;

		when Log =>
		     Check_Paragraph;
		     ARM_Output.Ordinary_Text (Output_Object, "log(");
		     Format_Object.Last_Non_Space := True;

		when Down | Up =>
		    Ada.Text_IO.Put_Line ("  ** Su(b|per)script can't occur directly, line " &
		        ARM_Input.Line_String (Input_Object));

		when New_Column | New_Page | RM_New_Page | Soft_Page |
		     No_Prefix | No_Para_Num | Keep_with_Next | Leading | Trailing |
		     Thin_Line | Thick_Line | Table_Last |
		     Syntax_Summary | Syntax_XRef | Glossary_List |
		     Attribute_List | Pragma_List | Implementation_Defined_List |
		     Intro_Name | Syntax_Name | Resolution_Name |
		     Legality_Name | Static_Name | Link_Name | Run_Name |
		     Bounded_Name | Erroneous_Name | Req_Name |
		     Doc_Name | Metrics_Name | Permission_Name | Advice_Name |
		     Notes_Name | Examples_Name | Meta_Name | Inconsistent83_Name |
		     Incompatible83_Name | Extend83_Name | Wording83_Name |
		     Inconsistent95_Name |
		     Incompatible95_Name | Extend95_Name | Wording95_Name |
		     Syntax_Title | Resolution_Title | Legality_Title |
		     Static_Title | Link_Title | Run_Title | Bounded_Title |
		     Erroneous_Title | Req_Title | Doc_Title | Metrics_Title |
		     Permission_Title | Advice_Title | Notes_Title |
		     Examples_Title | Meta_Title | Inconsistent83_Title |
		     Incompatible83_Title | Extend83_Title | Wording83_Title |
		     Inconsistent95_Title |
		     Incompatible95_Title | Extend95_Title | Wording95_Title |
		     EM_Dash | EN_Dash | LT | LE | GT | GE | NE | PI |
		     Times | PorM | Single_Quote | Thin_Space | Left_Quote |
		     Right_Quote | Left_Double_Quote | Right_Double_Quote |
		     Left_Quote_Pair | Right_Quote_Pair =>

		    -- These commands must not have a parameter.
		    Ada.Text_IO.Put_Line ("  ** Parameter for " &
		        Ada.Strings.Fixed.Trim (Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right) &
		        ", line " & ARM_Input.Line_String (Input_Object));

		when Unknown =>
		    Ada.Text_IO.Put_Line ("  -- Unknown command (skipped) - " &
		        Ada.Strings.Fixed.Trim (Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right) &
		        " on line " & ARM_Input.Line_String (Input_Object));
	    end case;
	end Process_Command_with_Parameter;


	procedure Process_Command_without_Parameter (Name : in ARM_Input.Command_Name_Type) is
	    -- Process the start of a command without a parameter. The name
	    -- of the command is Name.
	    procedure Put_Name (Kind : Paragraph_Type) is
	    begin
	        Check_Paragraph;
	        ARM_Output.Ordinary_Text (Output_Object,
		    Paragraph_Kind_Name(Kind).Str(1..Paragraph_Kind_Name(Kind).Length));
		Format_Object.Last_Non_Space := True;
	    end Put_Name;

	    procedure Put_Title (Kind : Paragraph_Type) is
	    begin
	        Check_Paragraph;
	        ARM_Output.Ordinary_Text (Output_Object,
		    Paragraph_Kind_Title(Kind).Str(1..Paragraph_Kind_Title(Kind).Length));
		Format_Object.Last_Non_Space := True;
	    end Put_Title;

	    procedure Format_Text (Text : in String;
				   Text_Name : in String) is
		-- Note: We use the state of the surrounding call.
		Input_Object : Arm_String.String_Input_Type;
		Real_Document : ARM_Format.Document_Type := Format_Object.Document;
	    begin
		-- No AARM text in this document.
		if Real_Document = ARM_Format.AARM then
		     Format_Object.Document := ARM_Format.RM;
		end if;
		Arm_String.Open (Input_Object, Text, Text_Name);
		     -- Open the input object using a string for input.
		Real_Process (Format_Object, Format_State, Input_Object, Output_Object);
		Arm_String.Close (Input_Object);
		Format_Object.Document := Real_Document;
	    end Format_Text;

	    procedure DB_Report is new ARM_Database.Report (Format_Text);

	    procedure Syn_Report is new ARM_Syntax.Report (Format_Text);

	    procedure Syn_XRef is new ARM_Syntax.XRef (Format_Text);

	begin
	    case Command (Name) is
		when Comment =>
		    null; -- Harmless, but still junk.

		when No_Prefix =>
		    Format_Object.No_Prefix := True;

		when No_Para_Num =>
		    Format_Object.No_Para_Num := True;

		when Keep_with_Next =>
		    Format_Object.Keep_with_Next := True;

		when Leading =>
		    Format_Object.Space_After := ARM_Output.Narrow;

		when Trailing =>
		    Format_Object.Space_After := ARM_Output.Wide;

		when New_Page =>
		    Check_End_Paragraph; -- End any paragraph that we're in.
		    ARM_Output.New_Page (Output_Object, ARM_Output.Any_Page);

		when RM_New_Page =>
		    if Format_Object.Document = ARM_Format.RM or else
		       Format_Object.Document = ARM_Format.RM_ISO then
		        Check_End_Paragraph; -- End any paragraph that we're in.
		        ARM_Output.New_Page (Output_Object, ARM_Output.Any_Page);
		    -- else do nothing.
		    end if;

		when Soft_Page =>
		    Check_Paragraph;
		    ARM_Output.New_Page (Output_Object, ARM_Output.Soft_Page);

		when New_Column =>
		    Check_End_Paragraph; -- End any paragraph that we're in.
		    ARM_Output.New_Column (Output_Object);

		when Thin_Line =>
		    Check_End_Paragraph;
		    ARM_Output.Separator_Line (Output_Object, Is_Thin => True);

		when Thick_Line =>
		    Check_End_Paragraph;
		    ARM_Output.Separator_Line (Output_Object, Is_Thin => False);

		when Table_Last =>
		    if Format_Object.Next_Paragraph_Format_Type = In_Table then
			-- If in a table, ends the second last row.
		        ARM_Output.Table_Marker (Output_Object, ARM_Output.End_Row_Next_is_Last);
			-- Eat the following LF, if any, to avoid confusing
			-- row ends:
			declare
			    Ch : Character;
			begin
			    ARM_Input.Get_Char (Input_Object, Ch);
			    if Ch /= Ascii.LF then
				ARM_Input.Replace_Char (Input_Object);
			    end if;
			end;
		    else
		        Ada.Text_IO.Put_Line ("  ** @Last command not in table, line " & ARM_Input.Line_String (Input_Object));
		    end if;
		when Syntax_Summary =>
		    Syn_Report;

		when Syntax_XRef =>
		    Syn_XRef;

		when Glossary_List =>
		    DB_Report  (Format_Object.Glossary_DB,
				ARM_Database.Normal_Indexed_List,
				Sorted => True);

		when Attribute_List =>
		    DB_Report  (Format_Object.Attr_DB,
				ARM_Database.Hanging_List,
				Sorted => True);

		when Pragma_List =>
		    DB_Report  (Format_Object.Pragma_DB,
				ARM_Database.Normal_List,
				Sorted => True);

		when Implementation_Defined_List =>
		    DB_Report  (Format_Object.Impdef_DB,
				ARM_Database.Bullet_List,
				Sorted => True);

		when Text_Begin | Text_End | Redundant | Part | Bold | Italic |
		     Roman | Swiss | Fixed | Roman_Italic | Shrink | Grow |
		     Keyword | Non_Terminal | Up | Down | Tab_Clear | Tab_Set |
		     Table |
		     Defn | RootDefn | PDefn | Defn2 | RootDefn2 | PDefn2 |
		     Index_See | Index_See_Also | See_Other | See_Also |
		     Index_Root_Unit | Index_Child_Unit | Index_Type |
		     Index_Subprogram | Index_Other | Index_Check |
		     Index_Attr | Index_Pragma |
		     Syntax_Rule | Syntax_Term | Syntax_Prefix |
		     To_Glossary | To_Glossary_Also | Implementation_Defined |
		     Prefix_Type | Reset_Prefix_Type | Attribute | Attribute_Leading |
		     Pragma_Syntax | Added_Pragma_Syntax |
		     Labeled_Section | Labeled_Section_No_Break |
		     Labeled_Clause | Labeled_Subclause |
		     Labeled_Revised_Clause | Labeled_Revised_Subclause |
		     Labeled_Added_Clause | Labeled_Added_Subclause |
		     Labeled_Informative_Annex | Labeled_Normative_Annex |
		     Labeled_Revised_Normative_Annex |
		     Unnumbered_Section | Subheading | Added_Subheading | Heading |
		     Center | Right |
		     Preface_Section | Ref_Section | Ref_Section_Number | Ref_Section_by_Number |
		     Change | Change_Reference | Change_Note |
		     Change_Implementation_Defined | Change_Attribute |
		     Change_Prefix_Type |
		     Latin_1 | Ceiling | Floor | Absolute | Log =>
		    -- These commands must have a parameter.
		    Ada.Text_IO.Put_Line ("  ** Failed to find parameter for " &
		        Ada.Strings.Fixed.Trim (Name, Ada.Strings.Right) &
		        ", line " & ARM_Input.Line_String (Input_Object));

		when Change_Param_Old | Change_Param_New =>
		    -- These can't get here; they represent the parameters of
		    -- "Change" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** Change parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Table_Param_Caption | Table_Param_Header | Table_Param_Body =>
		    -- These can't get here; they represent the parameters of
		    -- "Table" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** Table parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Syntax_Rule_RHS =>
		    -- This can't get here; it represents the second parameter of
		    -- "Syn" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** Syntax parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Glossary_Text_Param =>
		    -- This can't get here; it represents a parameter of
		    -- "ToGlossary" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** Glossary parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Attribute_Text_Param =>
		    -- This can't get here; it represents a parameter of
		    -- "Attribute" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** Attribute parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Change_Impdef_Text_Param =>
		    -- This can't get here; it represents a parameter of
		    -- "ChgImpldef" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** Impdef parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Change_Prefix_Text_Param =>
		    -- This can't get here; it represents a parameter of
		    -- "ChgPrefixType" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** ChgPrefix parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Intro_Name =>
		    Put_Name(Introduction);
		when Syntax_Name =>
		    Put_Name(Syntax);
		when Resolution_Name =>
		    Put_Name(Resolution);
		when Legality_Name =>
		    Put_Name(Legality);
		when Static_Name =>
		    Put_Name(Static_Semantics);
		when Link_Name =>
		    Put_Name(Link_Time);
		when Run_Name =>
		    Put_Name(Run_Time);
		when Bounded_Name =>
		    Put_Name(Bounded_Errors);
		when Erroneous_Name =>
		    Put_Name(Erroneous);
		when Req_Name =>
		    Put_Name(Requirements);
		when Doc_Name =>
		    Put_Name(Documentation);
		when Metrics_Name =>
		    Put_Name(Metrics);
		when Permission_Name =>
		    Put_Name(Permissions);
		when Advice_Name =>
		    Put_Name(Advice);
		when Notes_Name =>
		    Put_Name(Notes);
		when Examples_Name =>
		    Put_Name(Examples);
		when Meta_Name =>
		    Put_Name(Language_Design);
		when Inconsistent83_Name =>
		    Put_Name(Ada83_Inconsistencies);
		when Incompatible83_Name =>
		    Put_Name(Ada83_Incompatibilities);
		when Extend83_Name =>
		    Put_Name(Ada83_Extensions);
		when Wording83_Name =>
		    Put_Name(Ada83_Wording);
		when Inconsistent95_Name =>
		    Put_Name(Ada95_Inconsistencies);
		when Incompatible95_Name =>
		    Put_Name(Ada95_Incompatibilities);
		when Extend95_Name =>
		    Put_Name(Ada95_Extensions);
		when Wording95_Name =>
		    Put_Name(Ada95_Wording);

		when Syntax_Title =>
		    Put_Title(Syntax);
		when Resolution_Title =>
		    Put_Title(Resolution);
		when Legality_Title =>
		    Put_Title(Legality);
		when Static_Title =>
		    Put_Title(Static_Semantics);
		when Link_Title =>
		    Put_Title(Link_Time);
		when Run_Title =>
		    Put_Title(Run_Time);
		when Bounded_Title =>
		    Put_Title(Bounded_Errors);
		when Erroneous_Title =>
		    Put_Title(Erroneous);
		when Req_Title =>
		    Put_Title(Requirements);
		when Doc_Title =>
		    Put_Title(Documentation);
		when Metrics_Title =>
		    Put_Title(Metrics);
		when Permission_Title =>
		    Put_Title(Permissions);
		when Advice_Title =>
		    Put_Title(Advice);
		when Notes_Title =>
		    Put_Title(Notes);
		when Examples_Title =>
		    Put_Title(Examples);
		when Meta_Title =>
		    Put_Title(Language_Design);
		when Inconsistent83_Title =>
		    Put_Title(Ada83_Inconsistencies);
		when Incompatible83_Title =>
		    Put_Title(Ada83_Incompatibilities);
		when Extend83_Title =>
		    Put_Title(Ada83_Extensions);
		when Wording83_Title =>
		    Put_Title(Ada83_Wording);
		when Inconsistent95_Title =>
		    Put_Title(Ada95_Inconsistencies);
		when Incompatible95_Title =>
		    Put_Title(Ada95_Incompatibilities);
		when Extend95_Title =>
		    Put_Title(Ada95_Extensions);
		when Wording95_Title =>
		    Put_Title(Ada95_Wording);

		when EM_Dash =>
		    Check_Paragraph;
		    ARM_Output.Special_Character (Output_Object, ARM_Output.EM_Dash);
		    Format_Object.Last_Non_Space := True;
		when EN_Dash =>
		    Check_Paragraph;
		    ARM_Output.Special_Character (Output_Object, ARM_Output.EN_Dash);
		    Format_Object.Last_Non_Space := True;
		when LE =>
		    Check_Paragraph;
		    ARM_Output.Special_Character (Output_Object, ARM_Output.LEQ);
		    Format_Object.Last_Non_Space := True;
		when LT =>
		    Check_Paragraph;
		    ARM_Output.Ordinary_Character (Output_Object, '<');
		    Format_Object.Last_Non_Space := True;
		when GE =>
		    Check_Paragraph;
		    ARM_Output.Special_Character (Output_Object, ARM_Output.GEQ);
		    Format_Object.Last_Non_Space := True;
		when GT =>
		    Check_Paragraph;
		    ARM_Output.Ordinary_Character (Output_Object, '>');
		    Format_Object.Last_Non_Space := True;
		when NE =>
		    Check_Paragraph;
		    ARM_Output.Special_Character (Output_Object, ARM_Output.NEQ);
		    Format_Object.Last_Non_Space := True;
		when PI =>
		    Check_Paragraph;
		    ARM_Output.Special_Character (Output_Object, ARM_Output.PI);
		    Format_Object.Last_Non_Space := True;
		when Times =>
		    Check_Paragraph;
		    ARM_Output.Ordinary_Character (Output_Object, Character'Val(183)); -- Middle Dot.
		    Format_Object.Last_Non_Space := True;
		when PorM =>
		    Check_Paragraph;
		    ARM_Output.Ordinary_Character (Output_Object, Character'Val(177)); -- Plus or Minus character.
		    Format_Object.Last_Non_Space := True;
		when Single_Quote =>
		    Check_Paragraph;
		    ARM_Output.Ordinary_Character (Output_Object, '''); -- Single quote.
		    Format_Object.Last_Non_Space := True;
		when Thin_Space =>
		    Check_Paragraph;
		    ARM_Output.Special_Character (Output_Object, ARM_Output.Thin_Space);
		    Format_Object.Last_Non_Space := True;
		when Left_Quote =>
		    Check_Paragraph;
		    ARM_Output.Special_Character (Output_Object, ARM_Output.Left_Quote);
		    Format_Object.Last_Non_Space := True;
		when Right_Quote =>
		    Check_Paragraph;
		    ARM_Output.Special_Character (Output_Object, ARM_Output.Right_Quote);
		    Format_Object.Last_Non_Space := True;
		when Left_Double_Quote =>
		    Check_Paragraph;
		    ARM_Output.Special_Character (Output_Object, ARM_Output.Left_Double_Quote);
		    Format_Object.Last_Non_Space := True;
		when Right_Double_Quote =>
		    Check_Paragraph;
		    ARM_Output.Special_Character (Output_Object, ARM_Output.Right_Double_Quote);
		    Format_Object.Last_Non_Space := True;
		when Left_Quote_Pair =>
		    Check_Paragraph;
		    -- Was: To match the Ada 95 standard:
		    --ARM_Output.Special_Character (Output_Object, ARM_Output.Left_Quote);
		    --ARM_Output.Special_Character (Output_Object, ARM_Output.Left_Quote);
		    ARM_Output.Special_Character (Output_Object, ARM_Output.Left_Double_Quote);
		    Format_Object.Last_Non_Space := True;
		when Right_Quote_Pair =>
		    Check_Paragraph;
		    -- Was: To match the Ada 95 standard:
		    --ARM_Output.Special_Character (Output_Object, ARM_Output.Right_Quote);
		    --ARM_Output.Special_Character (Output_Object, ARM_Output.Right_Quote);
		    ARM_Output.Special_Character (Output_Object, ARM_Output.Right_Double_Quote);
		    Format_Object.Last_Non_Space := True;

		when Unknown =>
		    Ada.Text_IO.Put_Line ("  -- Unknown command (skipped) - " &
		        Ada.Strings.Fixed.Trim (Name, Ada.Strings.Right) &
		        " on line " & ARM_Input.Line_String (Input_Object));
	    end case;
	end Process_Command_without_Parameter;


	procedure Handle_End_of_Command is
	    -- Unstack and handle the end of Commands.
	begin
	    case Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command is
		when Redundant =>
		    if Format_Object.Document = ARM_Format.AARM then
		        Check_Paragraph;
		        ARM_Output.Ordinary_Character (Output_Object, ']');
		    -- else ignored in the RM.
		    end if;
		when Bold =>
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Bold => False,
					    Italic => Format_Object.Is_Italic,
					    Font => Format_Object.Font,
					    Size => Format_Object.Size,
				            Change => Format_Object.Change,
			                    Version => Format_Object.Current_Change_Version,
			                    Added_Version => Format_Object.Current_Old_Change_Version,
					    Location => Format_Object.Location);
		    Format_Object.Is_Bold := False;

		when Italic =>
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Bold => Format_Object.Is_Bold,
					    Italic => False,
					    Font => Format_Object.Font,
					    Size => Format_Object.Size,
				            Change => Format_Object.Change,
			                    Version => Format_Object.Current_Change_Version,
			                    Added_Version => Format_Object.Current_Old_Change_Version,
					    Location => Format_Object.Location);
		    Format_Object.Is_Italic := False;

		when Roman | Swiss | Fixed =>
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Bold => Format_Object.Is_Bold,
					    Italic => Format_Object.Is_Italic,
					    Font => ARM_Output.Default,
					    Size => Format_Object.Size,
				            Change => Format_Object.Change,
			                    Version => Format_Object.Current_Change_Version,
			                    Added_Version => Format_Object.Current_Old_Change_Version,
					    Location => Format_Object.Location);
		    Format_Object.Font := ARM_Output.Default;

		when Roman_Italic =>
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Bold => Format_Object.Is_Bold,
					    Italic => False,
					    Font => ARM_Output.Default,
					    Size => Format_Object.Size,
				            Change => Format_Object.Change,
			                    Version => Format_Object.Current_Change_Version,
			                    Added_Version => Format_Object.Current_Old_Change_Version,
					    Location => Format_Object.Location);
		    Format_Object.Is_Italic := False;
		    Format_Object.Font := ARM_Output.Default;

		when Shrink =>
		    declare
			use type ARM_Output.Size_Type;
		    begin
		        Check_Paragraph;
		        ARM_Output.Text_Format (Output_Object,
					        Bold => Format_Object.Is_Bold,
					        Italic => Format_Object.Is_Italic,
					        Font => Format_Object.Font,
					        Size => Format_Object.Size+1,
				                Change => Format_Object.Change,
			                        Version => Format_Object.Current_Change_Version,
			                        Added_Version => Format_Object.Current_Old_Change_Version,
					        Location => Format_Object.Location);
		        Format_Object.Size := Format_Object.Size + 1;
		    end;

		when Grow =>
		    declare
			use type ARM_Output.Size_Type;
		    begin
		        Check_Paragraph;
		        ARM_Output.Text_Format (Output_Object,
					        Bold => Format_Object.Is_Bold,
					        Italic => Format_Object.Is_Italic,
					        Font => Format_Object.Font,
					        Size => Format_Object.Size-1,
				                Change => Format_Object.Change,
			                        Version => Format_Object.Current_Change_Version,
			                        Added_Version => Format_Object.Current_Old_Change_Version,
					        Location => Format_Object.Location);
		        Format_Object.Size := Format_Object.Size - 1;
		    end;

		when Up =>
		    declare
		        use type ARM_Output.Size_Type;
		    begin
		        Check_Paragraph;
		        ARM_Output.Text_Format (Output_Object,
					        Bold => Format_Object.Is_Bold,
					        Italic => Format_Object.Is_Italic,
					        Font => Format_Object.Font,
					        Size => Format_Object.Size+2,
				                Change => Format_Object.Change,
			                        Version => Format_Object.Current_Change_Version,
			                        Added_Version => Format_Object.Current_Old_Change_Version,
					        Location => ARM_Output.Normal);
		        Format_Object.Location := ARM_Output.Normal;
		        Format_Object.Size := Format_Object.Size+2;
		    end;

		when Down =>
		    declare
		        use type ARM_Output.Size_Type;
		    begin
		        Check_Paragraph;
		        ARM_Output.Text_Format (Output_Object,
					        Bold => Format_Object.Is_Bold,
					        Italic => Format_Object.Is_Italic,
					        Font => Format_Object.Font,
					        Size => Format_Object.Size+2,
				                Change => Format_Object.Change,
			                        Version => Format_Object.Current_Change_Version,
			                        Added_Version => Format_Object.Current_Old_Change_Version,
					        Location => ARM_Output.Normal);
		        Format_Object.Location := ARM_Output.Normal;
		        Format_Object.Size := Format_Object.Size+2;
		    end;

		when Keyword =>
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Bold => False,
					    Italic => Format_Object.Is_Italic,
					    Font => Format_Object.Font,
					    Size => Format_Object.Size,
				            Change => Format_Object.Change,
			                    Version => Format_Object.Current_Change_Version,
			                    Added_Version => Format_Object.Current_Old_Change_Version,
					    Location => Format_Object.Location);
		    Format_Object.Is_Bold := False;

		when Non_Terminal =>
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Bold => Format_Object.Is_Bold,
					    Italic => Format_Object.Is_Italic,
					    Font => ARM_Output.Default,
					    Size => Format_Object.Size,
				            Change => Format_Object.Change,
			                    Version => Format_Object.Current_Change_Version,
			                    Added_Version => Format_Object.Current_Old_Change_Version,
					    Location => Format_Object.Location);
		    Format_Object.Font := ARM_Output.Default;

		when Subheading | Heading =>
		    -- Restore the format.
		    Check_Paragraph;
		    -- Separate calls to exactly undo the original calls -
		    -- First the size:
		    ARM_Output.Text_Format (Output_Object,
			   Bold => True, Italic => False,
			   Font => ARM_Output.Swiss,
			   Size => 0,
			   Change => ARM_Output.None,
			   Location => ARM_Output.Normal);
		    -- Then the rest:
		    ARM_Output.Text_Format (Output_Object,
			   Bold => False, Italic => False,
			   Font => ARM_Output.Default,
			   Size => 0,
			   Change => ARM_Output.None,
			   Location => ARM_Output.Normal);
		    Format_Object.Is_Bold := False;
		    Format_Object.Font := ARM_Output.Default;
		    Format_Object.Size := 0;
		    Check_End_Paragraph;

		when Added_Subheading =>
		    -- Restore the format.
		    if Format_Object.Change_Version <
			Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version then
			null; -- Not in old versions.
		    else
			case Format_Object.Changes is
			    when ARM_Format.Old_Only =>
			        null; -- Not in old versions.
			    when ARM_Format.New_Only |
			         ARM_Format.Show_Changes |
			         ARM_Format.New_Changes |
			         ARM_Format.Changes_Only =>
			        Check_Paragraph;
			        -- Separate calls to exactly undo the original calls -
			        -- First the size:
			        if Format_Object.Changes = ARM_Format.New_Only then
				    -- Unmarked.
			            ARM_Output.Text_Format (Output_Object,
				           Bold => True, Italic => False,
				           Font => ARM_Output.Swiss,
				           Size => 0,
				           Change => ARM_Output.None,
				           Location => ARM_Output.Normal);
				else
				    -- Marked as an insertion.
			            ARM_Output.Text_Format (Output_Object,
				           Bold => True, Italic => False,
				           Font => ARM_Output.Swiss,
				           Size => 0,
				           Change => ARM_Output.Insertion,
				           Version => Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version,
				           Location => ARM_Output.Normal);
				end if;
			        -- Then the rest:
				ARM_Output.Text_Format (Output_Object,
				       Bold => False, Italic => False,
				       Font => ARM_Output.Default,
				       Size => 0,
				       Change => ARM_Output.None,
				       Location => ARM_Output.Normal);
			        Format_Object.Is_Bold := False;
			        Format_Object.Font := ARM_Output.Default;
			        Format_Object.Size := 0;
			        Check_End_Paragraph;
		        end case;
		    end if;

	        when Center | Right =>
		    -- Close the paragraph.
		    Check_End_Paragraph;

		when Table_Param_Caption =>
		    ARM_Output.Table_Marker (Output_Object,
					     ARM_Output.End_Caption);
		    Format_Object.Last_Non_Space := False;

		    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
			-- Unstack the "Caption" parameter.
--Ada.Text_IO.Put_Line (" &Unstack (Tab Cap)");

		    -- Check and handle the following "Headers" parameter:
		    declare
			Ch : Character;
		    begin
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Headers" & (8..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Ch);
			if Ch /= ' ' then -- There is a parameter.
			    -- Stack the parameter so we can process the end:
			    Set_Nesting_for_Parameter
			        (Command => Table_Param_Header,
				 Close_Ch => Ch);

			    -- No processing needed here.

			-- else no parameter. Weird.
			end if;
			return; -- We've already done the unstacking.
		    end;

		when Table_Param_Header =>
		    ARM_Output.Table_Marker (Output_Object,
					     ARM_Output.End_Header);
		    Format_Object.Last_Non_Space := False;

		    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
			-- Unstack the "Header" parameter.
--Ada.Text_IO.Put_Line (" &Unstack (Tab Head)");

		    -- Check and handle the following "Body" parameter:
		    declare
			Ch : Character;
		    begin
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Body" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Ch);
			if Ch /= ' ' then -- There is a parameter.
			    -- Stack the parameter so we can process the end:
			    Set_Nesting_for_Parameter
			        (Command => Table_Param_Body,
				 Close_Ch => Ch);

			    -- No processing needed here.

			-- else no parameter. Weird.
			end if;
			return; -- We've already done the unstacking.
		    end;

		when Table_Param_Body =>
		    -- Close the table:
		    ARM_Output.Table_Marker (Output_Object,
					     ARM_Output.End_Table);
		    Format_Object.Last_Non_Space := False;

		    -- Reset the paragraph format (and skip over the parameter records):
		    Format_Object.Last_Paragraph_Subhead_Type :=
 		        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Old_Last_Subhead_Paragraph;
		    Format_Object.Next_Paragraph_Subhead_Type :=
 		        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Old_Next_Subhead_Paragraph;
		    Format_Object.Next_Paragraph_Format_Type :=
		        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Old_Next_Paragraph_Format;
		    Format_Object.Paragraph_Tab_Stops :=
		        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Old_Tab_Stops;
		    Format_Object.In_Paragraph := False; -- End fake paragraph.

		when Syntax_Rule_RHS =>
		    -- Send the production to the syntax manager.
		    -- Other processing has already been handled.
		    declare
			Text_Buffer : String (1..ARM_Input.MAX_RECORDING_SIZE);
			Text_Buffer_Len : Natural;
		    begin
			Arm_Input.Stop_Recording_and_Read_Result
			    (Input_Object, Text_Buffer, Text_Buffer_Len);
			Text_Buffer_Len := Text_Buffer_Len - 1; -- Remove command close character.
			ARM_Syntax.Insert_Rule (For_Clause => Clause_String,
			    Rule => "@nt{" & Format_Object.Syntax_NT(1..Format_Object.Syntax_NT_Len) &
				"::=} " & Text_Buffer(1..Text_Buffer_Len),
			    Tabset => Format_Object.Syntax_Tab(1..Format_Object.Syntax_Tab_Len));

			Check_End_Paragraph; -- End the paragraph, so the
					     -- next rule gets its own.

			-- Reset the paragraph format:
		        Format_Object.Last_Paragraph_Subhead_Type :=
 			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Last_Subhead_Paragraph;
		        Format_Object.Next_Paragraph_Subhead_Type :=
 			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Subhead_Paragraph;
		        Format_Object.Next_Paragraph_Format_Type :=
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Paragraph_Format;
			Format_Object.Paragraph_Tab_Stops :=
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Tab_Stops;

			-- Set the Non-terminal to empty:
			Format_Object.Syntax_NT_Len := 0;
			Format_Object.Syntax_Tab_Len := 0;
		    end;

		when Syntax_Prefix =>
		    -- Reset the style:
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Bold => Format_Object.Is_Bold,
					    Italic => False,
					    Font => Format_Object.Font,
					    Size => Format_Object.Size,
				            Change => Format_Object.Change,
			                    Version => Format_Object.Current_Change_Version,
			                    Added_Version => Format_Object.Current_Old_Change_Version,
					    Location => Format_Object.Location);
		    Format_Object.Is_Italic := False;

		when Glossary_Text_Param =>
		    -- Save the glossary entry in the Glossary database.
		    declare
			Text_Buffer : String (1..ARM_Input.MAX_RECORDING_SIZE);
			Text_Buffer_Len : Natural;
		    begin
			Arm_Input.Stop_Recording_and_Read_Result
			    (Input_Object, Text_Buffer, Text_Buffer_Len);
			Text_Buffer_Len := Text_Buffer_Len - 1; -- Remove command close character.
			ARM_Database.Insert (Format_Object.Glossary_DB,
			    Sort_Key => Format_Object.Glossary_Term(1..Format_Object.Glossary_Term_Len),
			    Hang_Item => "",
			    Text => "@b{" & Format_Object.Glossary_Term(1..Format_Object.Glossary_Term_Len) &
				".} " & Text_Buffer(1..Text_Buffer_Len));
		    end;

		    -- Finish the text processing:
		    if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Command = To_Glossary_Also then
			null; -- Normal text, no special handling needed.
		    else
		        case Format_Object.Document is
			    when ARM_Format.AARM =>
				-- End the annotation.
			        Check_End_Paragraph;
			        Format_Object.Last_Paragraph_Subhead_Type :=
	 			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Last_Subhead_Paragraph;
			        Format_Object.Next_Paragraph_Subhead_Type :=
	 			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Subhead_Paragraph;
			        Format_Object.Next_Paragraph_Format_Type :=
				    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Paragraph_Format;
			        Format_Object.Paragraph_Tab_Stops :=
				    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Tab_Stops;
			    when others =>
				null; -- No text, no special handling needed.
			end case;
		    end if;

		when Attribute_Text_Param =>
		    declare
			Text_Buffer : String (1..ARM_Input.MAX_RECORDING_SIZE);
			Text_Buffer_Len : Natural;

			function Chg_Command (Kind : in ARM_Database.Paragraph_Change_Kind_Type;
					      Version : in Character) return String is
			begin
			    case Kind is
				when ARM_Database.None =>
				    return "";
				when ARM_Database.Inserted | ARM_Database.Inserted_Normal_Number =>
				    return "@Chgref{Version=[" & Version &
					"],Kind=[Added]}";
				when ARM_Database.Deleted =>
				    return "@Chgref{Version=[" & Version &
					"],Kind=[Deleted]}";
				when ARM_Database.Revised =>
				    return "@Chgref{Version=[" & Version &
					"],Kind=[Revised]}";
			    end case;
			end Chg_Command;

		    begin
			Arm_Input.Stop_Recording_and_Read_Result
			    (Input_Object, Text_Buffer, Text_Buffer_Len);
			Text_Buffer_Len := Text_Buffer_Len - 1; -- Remove command close character.
			-- Ordinary text processing is fine for the local text.
			if Format_Object.Attr_Leading then
			    ARM_Database.Insert (Format_Object.Attr_DB,
			        Sort_Key => Format_Object.Attr_Name(1..Format_Object.Attr_Name_Len),
			        Hang_Item =>
				    Format_Object.Attr_Prefix(1..Format_Object.Attr_Prefix_Len) &
				       ''' & Format_Object.Attr_Name(1..Format_Object.Attr_Name_Len),
			        Text => "For " & Format_Object.Prefix_Text(1..Format_Object.Prefix_Text_Len) &
				    ":" & Ascii.LF & Ascii.LF &
				    Chg_Command (Format_Object.Attr_Change_Kind, Format_Object.Attr_Version) &
				    "@leading@noprefix@;" & Text_Buffer(1..Text_Buffer_Len) &
				    " See " & Clause_String & '.',
				Change_Kind => Format_Object.Attr_Prefix_Change_Kind,
				Version => Format_Object.Attr_Prefix_Version);
			else -- not leading:
			    ARM_Database.Insert (Format_Object.Attr_DB,
			        Sort_Key => Format_Object.Attr_Name(1..Format_Object.Attr_Name_Len),
			        Hang_Item =>
				    Format_Object.Attr_Prefix(1..Format_Object.Attr_Prefix_Len) &
				       ''' & Format_Object.Attr_Name(1..Format_Object.Attr_Name_Len),
			        Text => "For " & Format_Object.Prefix_Text(1..Format_Object.Prefix_Text_Len) &
				    ":" & Ascii.LF & Ascii.LF &
				    Chg_Command (Format_Object.Attr_Change_Kind, Format_Object.Attr_Version) &
				    "@noprefix@;" & Text_Buffer(1..Text_Buffer_Len) &
				    " See " & Clause_String & '.',
				Change_Kind => Format_Object.Attr_Prefix_Change_Kind,
				Version => Format_Object.Attr_Prefix_Version);
			end if;
        	    end;

		when Pragma_Syntax | Added_Pragma_Syntax =>
		    -- Note: Pragma_Syntax is not recorded in the syntax summary.
		    declare
			Text_Buffer : String (1..ARM_Input.MAX_RECORDING_SIZE);
			Text_Buffer_Len : Natural;

			function My_Sort return String is
			    -- Find and return the @prag argument.
			begin
			    if Text_Buffer_Len > 20 and then
				Text_Buffer(14) = '@' and then
				(Text_Buffer(15) = 'p' or else Text_Buffer(15) = 'P') and then
				(Text_Buffer(16) = 'r' or else Text_Buffer(16) = 'R') and then
				(Text_Buffer(17) = 'a' or else Text_Buffer(17) = 'A') and then
				(Text_Buffer(18) = 'g' or else Text_Buffer(18) = 'G') and then
				ARM_Input.Is_Open_Char (Text_Buffer(19)) then
				-- Formatted as we expect.
				for I in 20 .. Text_Buffer_Len loop
				    if Text_Buffer(I) = ARM_Input.Get_Close_Char (Text_Buffer(19)) then
					return Text_Buffer(20 .. I-1);
				    end if;
				end loop;
				Ada.Text_IO.Put_Line ("** Can't find argument for @prag: " & Text_Buffer(1..Text_Buffer_Len) &
				    " on line " & ARM_Input.Line_String (Input_Object));
			        return ""; -- Never found the end of the argument.
			    else
				Ada.Text_IO.Put_Line ("** Funny pragma format: " & Text_Buffer(1..Text_Buffer_Len) &
				    " on line " & ARM_Input.Line_String (Input_Object));
				return ""; -- Gotta return something.
			    end if;
			end My_Sort;

		    begin
			Arm_Input.Stop_Recording_and_Read_Result
			    (Input_Object, Text_Buffer, Text_Buffer_Len);
			Text_Buffer_Len := Text_Buffer_Len - 1; -- Remove command close character.
			-- Ordinary text processing is fine for the local text.
			if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Pragma_Syntax then
			    ARM_Database.Insert (Format_Object.Pragma_DB,
			        Sort_Key => My_Sort,
			        Hang_Item => "",
			        Text => Text_Buffer(1..Text_Buffer_Len) &
				    " @em See @RefSecbyNum{" & Clause_String & "}.");
			else -- Added_Pragma_Syn
			    if Format_Object.Change_Version <
				Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version then
				null; -- Not in old versions, omit from Annex.
			    else
				case Format_Object.Changes is
				    when ARM_Format.Old_Only =>
				        null; -- Not in old versions, omit from Annex.
				    when ARM_Format.New_Only =>
				        ARM_Database.Insert (Format_Object.Pragma_DB,
				            Sort_Key => My_Sort,
				            Hang_Item => "",
				            Text => "@ChgRef{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version &
						"],Kind=[Added]}" &
						Text_Buffer(1..Text_Buffer_Len) &
					        " @em See @RefSecbyNum{" & Clause_String & "}.");
				    when ARM_Format.New_Changes | ARM_Format.Show_Changes =>
				        ARM_Database.Insert (Format_Object.Pragma_DB,
				            Sort_Key => My_Sort,
				            Hang_Item => "",
				            Text => "@ChgRef{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version &
						"],Kind=[Added]}" &
						"@Chg{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version &
						"],New={" & Text_Buffer(1..Text_Buffer_Len) &
					        " @em See @RefSecbyNum<" & Clause_String & ">.},Old=[]}");
			            when ARM_Format.Changes_Only =>
				        if Format_Object.Change_Version =
					    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version then
					    -- Show change in Annex:
				            ARM_Database.Insert (Format_Object.Pragma_DB,
				                Sort_Key => My_Sort,
				                Hang_Item => "",
				                Text =>
						    "@ChgRef{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version &
						    "],Kind=[Added]}" &
						    "@Chg{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version &
						    "],New={" & Text_Buffer(1..Text_Buffer_Len) &
					            " @em See @RefSecbyNum<" & Clause_String & ">.},Old=[]}");
					else
					    -- Don't show change, but use ChgRef to get paragraph numbers right.
				            ARM_Database.Insert (Format_Object.Pragma_DB,
				                Sort_Key => My_Sort,
				                Hang_Item => "",
				                Text =>
						    "@ChgRef{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version &
						    "],Kind=[Added]}" &
						    Text_Buffer(1..Text_Buffer_Len) &
					            " @em See @RefSecbyNum{" & Clause_String & "}.");
					end if;
			        end case;
			    end if;
			end if;
		    end;

		when Implementation_Defined | Change_Impdef_Text_Param =>
		    -- Save the implementation-defined entry in the database.
		    declare
			Text_Buffer : String (1..ARM_Input.MAX_RECORDING_SIZE);
			Text_Buffer_Len : Natural;

			function Clause_String return String is
			    use type ARM_Contents.Section_Number_Type;
			begin
			    if Format_Object.Clause = 0 then
				if Format_Object.Section in 0 .. 9 then
				    return
					Character'Val(Character'Pos('0') +
					   Format_Object.Section) & "";
				elsif Format_Object.Section in 10 .. 19 then
				    return "1" &
					Character'Val(Character'Pos('0') +
					   Format_Object.Section - 10);
				elsif Format_Object.Section = 20 then
				    return "20";
				else --if Format_Object.Section > 20 then
				    return Character'Val (Character'Pos('A') +
					 (Format_Object.Section - 21)) & "";
				end if;
			    elsif Format_Object.Subclause = 0 then
				return ARM_Contents.Make_Clause_Number (
				        ARM_Contents.Clause,
					Format_Object.Section,
					Format_Object.Clause);
			    else
				return ARM_Contents.Make_Clause_Number (
				        ARM_Contents.SubClause,
					Format_Object.Section,
					Format_Object.Clause,
					Format_Object.Subclause);
			    end if;
			end Clause_String;

			function Sort_Clause_String return String is
			    Res : String(1..9);
			    -- Always use the paragraph for sorting:
			begin
			    -- The funny encoding to insure proper sorting.
			    -- (Otherwise "10" sorts before "2".
			    Res(1) := Character'Val (Natural(Format_Object.Section) + 16#30#);
			    Res(2) := '.';
			    Res(3) := Character'Val (Format_Object.Clause + 16#30#);
			    Res(4) := '.';
			    Res(5) := Character'Val (Format_Object.Subclause + 16#30#);
			    Res(6) := '(';
			    Res(7) := Character'Val ((Format_Object.Next_Paragraph / 10) + 16#30#);
			    Res(8) := Character'Val ((Format_Object.Next_Paragraph mod 10) + 16#30#);
			    Res(9) := ')';
			    return Res;
			end Sort_Clause_String;

			function See_String return String is
			begin
			    case Format_Object.Impdef_Change_Kind is
				when ARM_Database.None | ARM_Database.Revised =>
				    if Format_Object.Document = ARM_Format.RM_ISO then
					return " See @RefSecbyNum{" & Clause_String & "}.";
				    else
					return " See @RefSecbyNum{" & Clause_String & "}(" &
					    Format_Object.Impdef_Paragraph_String (1..Format_Object.Impdef_Paragraph_Len) &
					    ").";
				    end if;
				when ARM_Database.Inserted | ARM_Database.Inserted_Normal_Number =>
				    if Format_Object.Document = ARM_Format.RM_ISO then
					return "@Chg{Version=[" & Format_Object.Impdef_Version &
					    "], New=[ See @RefSecbyNum{" & Clause_String & "}.],Old=[]}";
				    else
					return "@Chg{Version=[" & Format_Object.Impdef_Version &
				            "], New=[ See @RefSecbyNum{" & Clause_String & "}(" &
					    Format_Object.Impdef_Paragraph_String (1..Format_Object.Impdef_Paragraph_Len) &
					    ").],Old=[]}";
				    end if;
				when ARM_Database.Deleted =>
				    if Format_Object.Document = ARM_Format.RM_ISO then
					return "@Chg{Version=[" & Format_Object.Impdef_Version &
					    "], New=[],Old=[ See @RefSecbyNum{" & Clause_String & "}.]}";
				    else
					return "@Chg{Version=[" & Format_Object.Impdef_Version &
					    "], New=[],Old=[ See @RefSecbyNum{" & Clause_String & "}(" &
					    Format_Object.Impdef_Paragraph_String (1..Format_Object.Impdef_Paragraph_Len) &
					    ").]}";
				    end if;
			    end case;
			end See_String;

		    begin
			Arm_Input.Stop_Recording_and_Read_Result
			    (Input_Object, Text_Buffer, Text_Buffer_Len);
			Text_Buffer_Len := Text_Buffer_Len - 1; -- Remove command close character.
		        ARM_Database.Insert (Format_Object.Impdef_DB,
			    Sort_Key => Sort_Clause_String,
			    Hang_Item => "",
			    Text => Text_Buffer(1..Text_Buffer_Len) &
			       See_String,
			    Change_Kind => Format_Object.Impdef_Change_Kind,
			    Version => Format_Object.Impdef_Version);
		    end;
		    -- Finish the text processing:
		    case Format_Object.Document is
		        when ARM_Format.AARM =>
			    -- End the annotation:
			    Check_End_Paragraph;
			    if Format_Object.Next_Paragraph_Subhead_Type /=
				    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Subhead_Paragraph then
			        Format_Object.Last_Paragraph_Subhead_Type :=
				    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Last_Subhead_Paragraph;
			    -- else still in same subhead, leave alone. (If
			    -- we didn't do this, we'd output the subhead
			    -- multiple times).
			    end if;
			    Format_Object.Next_Paragraph_Subhead_Type :=
	 			Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Subhead_Paragraph;
			    Format_Object.Next_Paragraph_Format_Type :=
				Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Paragraph_Format;
			    Format_Object.Paragraph_Tab_Stops :=
				Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Tab_Stops;
		        when others =>
			    null; -- Nothing started, nothing to finish.
		    end case;

		when Prefix_Type | Change_Prefix_Text_Param =>
		    -- Copy the text into the Format_Object.Prefix_Text string.
		    ARM_Input.Stop_Recording_and_Read_Result (
			Input_Object,
			Format_Object.Prefix_Text,
		    	Format_Object.Prefix_Text_Len);
		    Format_Object.Prefix_Text_Len :=
			Format_Object.Prefix_Text_Len - 1; -- Remove command close character.

		when Change_Param_Old =>
		    if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version >
		        Format_Object.Change_Version then
			-- The new text was ignored, use the old only.
		        null; -- Nothing special to do.
		    else
		        case Format_Object.Changes is
			    when ARM_Format.Old_Only =>
			        null; -- Nothing special to do.
			    when ARM_Format.New_Only =>
			        null; -- Nothing to do (we nulled out the text before we got here).
			    when ARM_Format.Changes_Only |
				 ARM_Format.Show_Changes |
				 ARM_Format.New_Changes =>
				if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version <
				    Format_Object.Change_Version and then
				    Format_Object.Changes = ARM_Format.Changes_Only then
				    -- Old enough that only the new text is shown.
			            null; -- Nothing to do (we nulled out the text before we got here).
				else
				    if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Was_Text then
					-- Non-empty text. Restore the previous
					-- insertion state.
					Format_Object.Change :=
				            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Prev_Change;
					Format_Object.Current_Change_Version :=
				            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Prev_Change_Version;
					Format_Object.Current_Old_Change_Version :=
				            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Prev_Old_Change_Version;

			                Check_Paragraph; -- We have to be in a paragraph
				            -- in correct code, but this could happen
				            -- if the user ended the paragraph by mistake
				            -- (we've already generated an error in that case).
			                ARM_Output.Text_Format (Output_Object,
						                Bold => Format_Object.Is_Bold,
						                Italic => Format_Object.Is_Italic,
						                Font => Format_Object.Font,
						                Size => Format_Object.Size,
							        Change => Format_Object.Change,
							        Version => Format_Object.Current_Change_Version,
							        Added_Version => Format_Object.Current_Old_Change_Version,
							        Location => Format_Object.Location);
			                Format_Object.Change := ARM_Output.None;
			            -- else not in an deletion. That could happen if there
			            -- was no text (then we never entered an change).
			            end if;
				end if;
		        end case;
		    end if;
		    Format_Object.In_Change := False;

		when Change_Param_New =>
		    if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version >
		       Format_Object.Change_Version then
			-- The new text was ignored.
		        null; -- Nothing to do (we nulled out the text before we got here).
		    else
		        case Format_Object.Changes is
			    when ARM_Format.Old_Only =>
			        null; -- Nothing to do (we nulled out the text before we got here).
			    when ARM_Format.New_Only =>
			        null; -- Nothing special to do.
			    when ARM_Format.Changes_Only |
				 ARM_Format.Show_Changes |
				 ARM_Format.New_Changes =>
				if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version <
				    Format_Object.Change_Version and then
				    Format_Object.Changes = ARM_Format.Changes_Only then
				    -- Old enough that only the new text is shown.
			            null; -- Nothing special to do.
				else
				    if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Was_Text then
					-- Non-empty text. Restore the previous
					-- insertion state.
					Format_Object.Change :=
				            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Prev_Change;
					Format_Object.Current_Change_Version :=
				            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Prev_Change_Version;
					Format_Object.Current_Old_Change_Version :=
				            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Prev_Old_Change_Version;

			                Check_Paragraph; -- We have to be in a paragraph
				            -- in correct code, but this could happen
				            -- if the user ended the paragraph by mistake
				            -- (we've already generated an error in that case).
			                ARM_Output.Text_Format (Output_Object,
						                Bold => Format_Object.Is_Bold,
						                Italic => Format_Object.Is_Italic,
						                Font => Format_Object.Font,
						                Size => Format_Object.Size,
							        Change => Format_Object.Change,
							        Version => Format_Object.Current_Change_Version,
							        Added_Version => Format_Object.Current_Old_Change_Version,
						                Location => Format_Object.Location);
			                Format_Object.Change := ARM_Output.None;
			            -- else not in an insertion. That could happen if there
			            -- was no text (then we never entered an change).
			            end if;
				end if;
		        end case;
		    end if;

		    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		    -- Unstack the "New" parameter.
		    Format_Object.In_Change := False;
--Ada.Text_IO.Put_Line (" &Unstack (Chg New)");

		    -- Check and handle the following "Old" parameter:
		    declare
			Ch, Ch2 : Character;
		    begin
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Old" & (4..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Ch);
			if Ch /= ' ' then -- There is a parameter.
			    -- Stack the parameter so we can process the end:
			    Set_Nesting_for_Parameter
			        (Command => Change_Param_Old,
				 Close_Ch => Ch);

			    -- Now, handle the parameter:
			    if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version >
			        Format_Object.Change_Version then
				-- The new text was ignored, show the old only.
			        null; -- Nothing special to do.
			    else
			        case Format_Object.Changes is
				    when ARM_Format.Old_Only =>
				        null; -- Nothing special to do.
				    when ARM_Format.New_Only =>
				        -- Skip the text:
			                ARM_Input.Skip_until_Close_Char (Input_Object, Ch);
				        ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
				    when ARM_Format.Changes_Only |
					 ARM_Format.Show_Changes =>
				        if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version <
				            Format_Object.Change_Version and then
					    Format_Object.Changes = ARM_Format.Changes_Only then
					    -- Old enough that only the new text is shown.
				            -- Skip the text:
			                    ARM_Input.Skip_until_Close_Char (Input_Object, Ch);
				            ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
					else
				            ARM_Input.Get_Char (Input_Object, Ch2);
				            ARM_Input.Replace_Char (Input_Object);
					    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Was_Text :=
					        Ch /= Ch2;
				            if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Was_Text then
						-- Non-empty text; calculate new change state: (current is deletion)
					        case Format_Object.Change is
					            when ARM_Output.Deletion | ARM_Output.None =>
						        Format_Object.Change := ARM_Output.Deletion;
						        Format_Object.Current_Change_Version :=
						           Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
						        Format_Object.Current_Old_Change_Version := '0';
					            when ARM_Output.Insertion =>
						        Format_Object.Change := ARM_Output.Both;
						        Format_Object.Current_Old_Change_Version :=
						           Format_Object.Current_Change_Version; -- Old is the insertion.
						        Format_Object.Current_Change_Version :=
						           Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
					            when ARM_Output.Both =>
						        Format_Object.Change := ARM_Output.Both;
							-- Current_Old_Version is unchanged.
						        Format_Object.Current_Change_Version :=
						           Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
					        end case;
				                Check_Paragraph;
				                ARM_Output.Text_Format (Output_Object,
							                Bold => Format_Object.Is_Bold,
							                Italic => Format_Object.Is_Italic,
							                Font => Format_Object.Font,
							                Size => Format_Object.Size,
								        Change => Format_Object.Change,
								        Version => Format_Object.Current_Change_Version,
								        Added_Version => Format_Object.Current_Old_Change_Version,
							                Location => Format_Object.Location);
				            -- else no text, so don't emit a change area.
				            end if;
					end if;
				    when ARM_Format.New_Changes =>
				        ARM_Input.Get_Char (Input_Object, Ch2);
				        ARM_Input.Replace_Char (Input_Object);
				        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Was_Text :=
					    Ch /= Ch2;
				        if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Was_Text then
					    -- Non-empty text; calculate new change state: (current is deletion)
					    case Format_Object.Change is
					        when ARM_Output.Deletion | ARM_Output.None =>
						    Format_Object.Change := ARM_Output.Deletion;
						    Format_Object.Current_Change_Version :=
						       Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
						    Format_Object.Current_Old_Change_Version := '0';
					        when ARM_Output.Insertion =>
						    Format_Object.Change := ARM_Output.Both;
						    Format_Object.Current_Old_Change_Version :=
						       Format_Object.Current_Change_Version; -- Old is the insertion.
						    Format_Object.Current_Change_Version :=
						       Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
					        when ARM_Output.Both =>
						    Format_Object.Change := ARM_Output.Both;
						    -- Current_Old_Version is unchanged.
						    Format_Object.Current_Change_Version :=
						       Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
					    end case;
				            Check_Paragraph;
				            ARM_Output.Text_Format (Output_Object,
							            Bold => Format_Object.Is_Bold,
							            Italic => Format_Object.Is_Italic,
							            Font => Format_Object.Font,
							            Size => Format_Object.Size,
								    Change => Format_Object.Change,
								    Version => Format_Object.Current_Change_Version,
								    Added_Version => Format_Object.Current_Old_Change_Version,
							            Location => Format_Object.Location);
				            Format_Object.Change := ARM_Output.Deletion;
				            ARM_Output.Ordinary_Character (Output_Object, ' ');
				            -- Skip the text (we're not going to output it):
			                    ARM_Input.Skip_until_Close_Char (Input_Object, Ch);
				            ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
				        -- else no text, so don't emit a change area.
				        end if;
			        end case;
			    end if;
			    Format_Object.In_Change := True;

			-- else no parameter. Weird.
			end if;
			return; -- We've already done the unstacking.
		    end;

		when Ceiling =>
		     Check_Paragraph;
		     ARM_Output.Special_Character (Output_Object, ARM_Output.Right_Ceiling);
		     Format_Object.Last_Non_Space := True;

		when Floor =>
		     Check_Paragraph;
		     ARM_Output.Special_Character (Output_Object, ARM_Output.Right_Floor);
		     Format_Object.Last_Non_Space := True;

		when Absolute =>
		     Check_Paragraph;
		     ARM_Output.Ordinary_Character (Output_Object, '|');
		     Format_Object.Last_Non_Space := True;

		when Log =>
		     Check_Paragraph;
		     ARM_Output.Ordinary_Character (Output_Object, ')');
		     Format_Object.Last_Non_Space := True;

		when others =>
		    -- No special handling needed.
		    null;
	    end case;
--Ada.Text_IO.Put_Line (" &Unstack (Normal)");
	    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
	end Handle_End_of_Command;


	procedure Process_Special is
	    -- Process a special command/macro/tab.
	    -- These all start with '@'.
	    -- @xxxx is a command. It may have parameters delimited by
	    -- (), {}, [], or <>. There does not appear to be an escape, so
	    -- we don't have to worry about '}' being used in {} brackets,
	    -- for example. (Must be a pain to write, though.)
	    Command_Name : ARM_Input.Command_Name_Type;
	    Ch : Character;
	    use type ARM_Output.Size_Type;
	begin
	    ARM_Input.Get_Char (Input_Object, Ch);
	    if Ch = '\' then
		-- This represents a tab, or the end of centered text.
		-- (According to Bob Duff, from the Scribe manual).
		if Format_Object.Next_Paragraph_Format_Type = Hanging_Indented then
		    -- Instead of a tab, just use this to mark the end
		    -- of the hanging portion:
		    Check_Paragraph;
		    ARM_Output.End_Hang_Item (Output_Object);
	        elsif Format_Object.Next_Paragraph_Format_Type = In_Table then
		    -- If in a table, ends a item.
		    ARM_Output.Table_Marker (Output_Object, ARM_Output.End_Item);
		-- elsif centered text: TBD.
		elsif ARM_Output."="(Format_Object.Paragraph_Tab_Stops, ARM_Output.NO_TABS) then
	            Ada.Text_IO.Put_Line ("  ** Tab, but no tab stops set on line " &
		         ARM_Input.Line_String (Input_Object));
		else
		    Check_Paragraph;
		    ARM_Output.Tab (Output_Object);
		end if;
		return; -- We're done here.
	    elsif Ch = '=' then
		-- This marks the start of centered text.
		-- (According to Bob Duff, from the Scribe manual).
		-- We're not implementing this; we're just going to replace
		-- the handful of uses.
		-- We're done here.
	        Ada.Text_IO.Put_Line ("  ** Centered text unimplemented (skipped) on line " &
		     ARM_Input.Line_String (Input_Object));
		return;
	    elsif Ch = '^' then
		-- This represents setting at tab stop at the current location.
		-- Neither HTML nor RTF supports such a thing, so these should
		-- all have been replaced by conventional tab stops.
	        Ada.Text_IO.Put_Line ("  && Cursor tab stop unimplemented (skipped) on line " &
		     ARM_Input.Line_String (Input_Object));
		return;
	    elsif Ch = '@' then
		-- This represents @ in the text. We're done here.
		Check_Paragraph;
		ARM_Output.Ordinary_Character (Output_Object, '@');
		return;
	    elsif Ch = ' ' then
		-- This represents a hard space in the text. We're done here.
		Check_Paragraph;
		ARM_Output.Hard_Space (Output_Object);
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
	        ARM_Input.Get_Char (Input_Object, Ch);
		if ARM_Input.Is_Open_Char (Ch) then -- Start parameter:
		    Set_Nesting_for_Command
		        (Name => '-' & (2..ARM_Input.Command_Name_Type'Last => ' '),
			 Kind => Normal,
			 Param_Ch => Ch);
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Bold => Format_Object.Is_Bold,
					    Italic => Format_Object.Is_Italic,
					    Font => Format_Object.Font,
					    Size => Format_Object.Size-2,
				            Change => Format_Object.Change,
			                    Version => Format_Object.Current_Change_Version,
			                    Added_Version => Format_Object.Current_Old_Change_Version,
					    Location => ARM_Output.Subscript);
		    Format_Object.Size := Format_Object.Size-2;
		    Format_Object.Location := ARM_Output.Subscript;
		else -- No parameter. Weird.
		    ARM_Input.Replace_Char (Input_Object);
		    Ada.Text_IO.Put_Line ("  ** Failed to find parameter for subscript, line " & ARM_Input.Line_String (Input_Object));
		end if;
		return;
	    elsif Ch = '+' then
		-- This represents a superscript. It has an argument.
	        ARM_Input.Get_Char (Input_Object, Ch);
		if ARM_Input.Is_Open_Char (Ch) then -- Start parameter:
		    Set_Nesting_for_Command
		        (Name => '+' & (2..ARM_Input.Command_Name_Type'Last => ' '),
			 Kind => Normal,
			 Param_Ch => Ch);
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Bold => Format_Object.Is_Bold,
					    Italic => Format_Object.Is_Italic,
					    Font => Format_Object.Font,
					    Size => Format_Object.Size-2,
				            Change => Format_Object.Change,
			                    Version => Format_Object.Current_Change_Version,
			                    Added_Version => Format_Object.Current_Old_Change_Version,
					    Location => ARM_Output.Superscript);
		    Format_Object.Size := Format_Object.Size-2;
		    Format_Object.Location := ARM_Output.Superscript;
		else -- No parameter. Weird.
		    ARM_Input.Replace_Char (Input_Object);
		    Ada.Text_IO.Put_Line ("  ** Failed to find parameter for superscript, line " & ARM_Input.Line_String (Input_Object));
		end if;
		return;
	    elsif Ch = ':' then
		-- According to Tucker, the Scribe manual says:
		-- @:  After a ".", it forces the "." to be interpreted as a
		--     sentence-ending period rather than as an initial-ending
		--     period.  E.g.: You are better than I.@:  F. Stone is
		--     even better. Without the @:, the period after "I"
		--     would be interpreted as the period signifying an
		--     initial.
		--
		-- Besides not making much sense, this certainly does nothing
		-- for us.
		return;
	    elsif Ch = '*' then
		-- According to Tucker, the Scribe manual says:
		-- @*  This forces a line break, without starting a new
		--     paragraph.
		-- Tucker thinks this is "<BR>" in HTML.
		if Format_Object.In_Paragraph then
		    ARM_Output.Line_Break (Output_Object);
		    Format_Object.Last_Non_Space := False;
		-- else why start a paragraph with a line break??
		end if;
		return;
	    elsif Ch = '|' then
		-- According to Tucker, the Scribe manual says:
		-- @|  This marks a place within a word where a line break
		--     may be inserted, *without* inserting a hyphen.  It is
		--     effectively a zero-length "word".  You can use it to add
		--     spaces between words that disappear if the line gets
		--     broken there.  For example:
		--        This is @| a sentence with two spaces between "is" and "a".
		--     The extra space will disappear if the line is broken
		--     between "is" and "a".
		--
		-- However, this appears to be used mainly to insert potential
		-- line breaks into large words, and we use and implement it
		-- that way.
		if Format_Object.In_Paragraph then
		    ARM_Output.Soft_Line_Break (Output_Object);
		    Format_Object.Last_Non_Space := False;
		-- else don't care about non-required breaks between paragraphs.
		end if;
		return;
	    elsif Ch = '!' then
		-- This marks a place within a word where a line break
		-- may be inserted, inserting a hyphen.
		if Format_Object.In_Paragraph then
		    ARM_Output.Soft_Hyphen_Break (Output_Object);
		    Format_Object.Last_Non_Space := False;
		-- else don't care about non-required breaks between paragraphs.
		end if;
		return;
	    elsif Ch = Ascii.LF then
		-- Stand alone '@'.
		-- I now believe this is an error, perhaps a hard space where
		-- the trailing blank was dropped. It appears in
		-- Infosys.MSS.
		Ada.Text_IO.Put_Line("** Stand-alone '@' on line " & ARM_Input.Line_String (Input_Object));
		return;
	    end if;
	    ARM_Input.Replace_Char (Input_Object);
	    ARM_Input.Get_Name (Input_Object, Command_Name);

	    ARM_Input.Get_Char (Input_Object, Ch);
	    if ARM_Input.Is_Open_Char (Ch) then -- Start parameter:
	        Set_Nesting_for_Command
		    (Name => Ada.Characters.Handling.To_Lower (Command_Name),
		     Kind => Normal,
		     Param_Ch => Ch);
		Process_Command_with_Parameter;
	    else
		ARM_Input.Replace_Char (Input_Object);
		Process_Command_without_Parameter (Command_Name);
	    end if;
	end Process_Special;


	procedure Lookahead_for_End_of_Paragraph is
	    -- Look at the command following to see if it would
	    -- end the paragraph. If not, generate a Line_Break.
	    -- In any case, process the command (we don't allow more than
	    -- one call to Replace_Char).
	    -- We can assume that we are in a paragraph.
	    Command_Name : ARM_Input.Command_Name_Type;
	    Ch : Character;
	begin
	    ARM_Input.Get_Char (Input_Object, Ch);
	    if Ch not in 'A' .. 'Z' and then Ch not in 'a' .. 'z' then
		-- Not a named command, these never end a paragraph:
		ARM_Input.Replace_Char (Input_Object);
		ARM_Output.Line_Break (Output_Object);
	        Format_Object.Last_Non_Space := False;
		Process_Special;
	    else -- Named command.
		ARM_Input.Replace_Char (Input_Object);
	        ARM_Input.Get_Name (Input_Object, Command_Name);

		case Command (Command_Name) is
		    when Text_Begin | Text_End | New_Page | New_Column | RM_New_Page |
			Thin_Line | Thick_Line | Table |
			To_Glossary | Implementation_Defined | Labeled_Section |
			Labeled_Section_No_Break |
			Labeled_Clause | Labeled_Subclause |
			Labeled_Revised_Clause | Labeled_Revised_Subclause |
			Labeled_Added_Clause | Labeled_Added_Subclause |
			Preface_Section |
			Labeled_Informative_Annex | Labeled_Normative_Annex |
			Labeled_Revised_Normative_Annex |
			Unnumbered_Section | Subheading | Heading | Center | Right =>
			-- Ends a paragraph. No line break needed here (or
			-- we'd end up with two).
			null;
		    when others =>
			-- Does not end a paragraph. Put in the soft break.
			ARM_Output.Line_Break (Output_Object);
		        Format_Object.Last_Non_Space := False;
		end case;

		-- Now, process the command:
	        ARM_Input.Get_Char (Input_Object, Ch);
	        if ARM_Input.Is_Open_Char (Ch) then -- Start parameter:
	            Set_Nesting_for_Command
		        (Name => Ada.Characters.Handling.To_Lower (Command_Name),
		         Kind => Normal,
		         Param_Ch => Ch);
		    Process_Command_with_Parameter;
	        else
		    ARM_Input.Replace_Char (Input_Object);
		    Process_Command_without_Parameter (Command_Name);
	        end if;
	    end if;
	end Lookahead_for_End_of_Paragraph;

    begin
        Reading_Loop: loop
	    declare
	        Char : Character;
	    begin
	        ARM_Input.Get_Char (Input_Object, Char);
	        case Char is
		    when '@' =>
		        Process_Special;
		    when Ascii.LF =>
		        ARM_Input.Get_Char (Input_Object, Char);
		        if Char /= Ascii.LF then
			    -- Soft line break.
			    if Format_Object.Next_Paragraph_Format_Type = Example_Text or else
			       Format_Object.Next_Paragraph_Format_Type = Indented_Example_Text or else
			       Format_Object.Next_Paragraph_Format_Type = Display or else
			       Format_Object.Next_Paragraph_Format_Type = Syntax_Display or else
			       Format_Object.Next_Paragraph_Format_Type = Syntax_Production then
			        -- These formats preserves all line breaks, but a
			        -- "soft" break does not end a paragraph.
			        if Char /= '@' or else (not Format_Object.In_Paragraph) then
				    -- Not a paragraph end coming up:
				    if Format_Object.In_Paragraph then
				        ARM_Output.Line_Break (Output_Object);
				        Format_Object.Last_Non_Space := False;
				    -- else not in paragraph, we don't need to preserve
				    -- the break.
				    end if;
				    ARM_Input.Replace_Char (Input_Object);
			        else
				    -- A command following, and we're in a paragraph.
				    -- If that command ends the paragraph, then
				    -- we don't want a soft break here (else we'd
				    -- end up with an extra blank line at the end).
				    -- Otherwise, we do.
				    Lookahead_for_End_of_Paragraph;
			        end if;
			    elsif Format_Object.Next_Paragraph_Format_Type = In_Table then
			        -- If in a table, ends a row.
			        ARM_Output.Table_Marker (Output_Object, ARM_Output.End_Row);
			        ARM_Input.Replace_Char (Input_Object);
			        Format_Object.Last_Non_Space := False;
			    else -- Normal paragraph:
			        -- Output a space if the last character was
			        -- not a space and the next character is
			        -- not a space. Eliminate any leading blanks
			        -- added for formatting:
			        if Format_Object.In_Paragraph and then
				   Format_Object.Last_Non_Space then
				    ARM_Output.Ordinary_Character (Output_Object, ' ');
				    Format_Object.Last_Non_Space := False;
			        end if;
			        -- Skip any leading spaces for the next paragraph:
			        while Char = ' ' loop
				    ARM_Input.Get_Char (Input_Object, Char);
			        end loop;
			        ARM_Input.Replace_Char (Input_Object);
			    end if;
		        else -- Hard paragraph break. Only one, no matter
			    -- how many blank lines there are:
			    while Char = Ascii.LF loop
			        ARM_Input.Get_Char (Input_Object, Char);
			    end loop;
			    if Format_Object.Next_Paragraph_Format_Type = Example_Text or else
			       Format_Object.Next_Paragraph_Format_Type = Indented_Example_Text or else
			       Format_Object.Next_Paragraph_Format_Type = Display or else
			       Format_Object.Next_Paragraph_Format_Type = Syntax_Display or else
			       Format_Object.Next_Paragraph_Format_Type = Syntax_Production then
			        null; -- In these formats, blanks remain.
			    else
			        -- Also remove any leading blanks from the next
			        -- paragraph:
			        while Char = ' ' loop
				    ARM_Input.Get_Char (Input_Object, Char);
			        end loop;
			    end if;
			    ARM_Input.Replace_Char (Input_Object);
			    Check_End_Paragraph; -- End the paragraph.
		        end if;
		    when ' ' =>
		        if Format_Object.Next_Paragraph_Format_Type = Example_Text or else
			   Format_Object.Next_Paragraph_Format_Type = Indented_Example_Text or else
			   Format_Object.Next_Paragraph_Format_Type = Display or else
			   Format_Object.Next_Paragraph_Format_Type = Syntax_Display or else
			   Format_Object.Next_Paragraph_Format_Type = Syntax_Production then
			    -- Spaces are significant these formats.
			    Check_Paragraph;
			    ARM_Output.Hard_Space (Output_Object);
		        else
			    if Format_Object.In_Paragraph then
			        ARM_Output.Ordinary_Character (Output_Object, ' ');
			    -- else we never want to start a paragraph with a space.
			    end if;
		        end if;
		        Format_Object.Last_Non_Space := False;
		    when Ascii.SUB =>
		        -- End of file.
		        exit Reading_Loop;
		    when others =>
		        if Format_State.Nesting_Stack_Ptr /= 0 and then
			   Format_State.Nesting_Stack (Format_State.Nesting_Stack_Ptr).Close_Char /= ' ' and then
			   Format_State.Nesting_Stack (Format_State.Nesting_Stack_Ptr).Close_Char = Char then
    			    -- Closing a command, remove it from the stack.
			    Handle_End_of_Command;
		        else
			    Check_Paragraph;
			    ARM_Output.Ordinary_Character (Output_Object, Char);
			    null; -- Ordinary characters, output them.
			    Format_Object.Last_Non_Space := True;
		        end if;
	        end case;
	    end;
        end loop Reading_Loop;
    end Real_Process;


    procedure Process (Format_Object : in out Format_Type;
		       File_Name : in String;
		       Output_Object : in out ARM_Output.Output_Type'Class;
		       Section_Name : in String;
		       Section_Number : in ARM_Contents.Section_Number_Type;
		       Starts_New_Section : in Boolean) is
	-- Process the contents for File_Name, writing the results to
	-- Output_Object. (Output_Object uses dispatching calls to provide
	-- the correct formatting). Section_Name is the name of the section
	-- for this file. Starts_New_Section is True if the file starts
	-- a new section. Section_Number is the number (or letter) of the
	-- section. Values > 20 represent annex letters (21 => A, 22 => B, etc.)
	Input_Object : Arm_File.File_Input_Type;
	Format_State : Format_State_Type;
    begin
	Ada.Text_IO.Put_Line ("-- Processing " & File_Name);
	begin
	    Arm_File.Open (Input_Object, File_Name);
	exception
	    when others =>
		Ada.Text_IO.Put_Line ("** Unable to open file " & File_Name);
		return;
	end;
	if Starts_New_Section then
	    Format_Object.Section := Section_Number;
	    Format_Object.Clause := 0;
	    Format_Object.Subclause := 0;
	    declare
		use type ARM_Contents.Section_Number_Type;
	    begin
		if Section_Number = 0 then -- No title at all.
	            ARM_Output.Section (Output_Object,
				        Section_Title => "",
				        Section_Name => Section_Name);
		elsif Section_Number <= 20 then
	            ARM_Output.Section (Output_Object,
				        Section_Title => Ada.Strings.Fixed.Trim (
							 ARM_Contents.Lookup_Title (ARM_Contents.Section, Section_Number), Ada.Strings.Right),
				        Section_Name => Section_Name);
		elsif Section_Number <= 30 then -- && Kludge here: We don't
						-- have anyway to figure this
						-- out so we just assume Normative
						-- annexes go to 30.
	            ARM_Output.Section (Output_Object,
				        Section_Title => Ada.Strings.Fixed.Trim (
							 ARM_Contents.Lookup_Title (ARM_Contents.Normative_Annex, Section_Number), Ada.Strings.Right),
				        Section_Name => Section_Name);
		else
	            ARM_Output.Section (Output_Object,
				        Section_Title => Ada.Strings.Fixed.Trim (
							 ARM_Contents.Lookup_Title (ARM_Contents.Informative_Annex, Section_Number), Ada.Strings.Right),
				        Section_Name => Section_Name);
		end if;
	    exception
		when ARM_Contents.Not_Found_Error =>
		    Ada.Text_IO.Put_Line ("** Unable to find section title, line " & ARM_File.Line_String (Input_Object));
	    end;
	    Format_Object.Next_Note := 1;
	    Format_Object.Next_Paragraph := 1;
	    Format_Object.Next_Insert_Para := 1;
	    Format_Object.Next_AARM_Sub := 'a';
	    Format_Object.Next_Enumerated_Num := 1;

	    Format_Object.Is_Bold := False;
	    Format_Object.Is_Italic := False;
	    Format_Object.Font := ARM_Output.Default;
	    Format_Object.Size := 0;
	    Format_Object.Change := ARM_Output.None;
	    Format_Object.Location := ARM_Output.Normal;
	    Format_Object.No_Prefix := False;
	    Format_Object.No_Para_Num := False;
	    Format_Object.Keep_with_Next := False;
	    Format_Object.Space_After := ARM_Output.Normal;
	    Format_Object.No_Breaks := False;

	    Format_Object.Next_Paragraph_Change_Kind := ARM_Database.None;

	    Format_Object.Format := ARM_Output.Normal; -- The default.
	    Format_Object.In_Paragraph := False;
	end if;

	Real_Process (Format_Object, Format_State, Input_Object, Output_Object);

	-- Reached end of the file/input object.
	-- Kill any open paragraph:
	if Format_Object.In_Paragraph then
	    ARM_Output.End_Paragraph (Output_Object);
	    Format_Object.In_Paragraph := False;
        end if;
	Ada.Text_IO.Put_Line ("  Lines processed: " &
		ARM_File.Line_String (Input_Object));
	Arm_File.Close (Input_Object);
	if Format_State.Nesting_Stack_Ptr /= 0 then
	    Ada.Text_IO.Put_Line ("   ** Unfinished commands detected.");
	    for I in reverse 1 .. Format_State.Nesting_Stack_Ptr loop
	        Ada.Text_IO.Put_Line ("      Open command=" &
		    Format_State.Nesting_Stack(I).Name);
	    end loop;
	end if;
    end Process;


    procedure Format (Format_Object : in out Format_Type;
		      Text : in String;
		      Output_Object : in out ARM_Output.Output_Type'Class;
		      Text_Name : in String;
		      No_AARM_Text : in Boolean) is
	-- Format the contents of Text, writing the results to
	-- Output_Object. (Output_Object uses dispatching calls to provide
	-- the correct formatting). Text is assumed to be a component of
	-- a larger section. Text_Name is an identifying name for error messages.
	-- If No_AARM_Text is true, we don't want any AARM text even if we
	-- are generating the AARM.
	Input_Object : Arm_String.String_Input_Type;
	Format_State : Format_State_Type;
	Real_Document : ARM_Format.Document_Type := Format_Object.Document;
    begin
	if No_AARM_Text and then Real_Document = ARM_Format.AARM then
	     Format_Object.Document := ARM_Format.RM;
	end if;
	Arm_String.Open (Input_Object, Text, Text_Name);
	     -- Open the input object using a string for input.
	Real_Process (Format_Object, Format_State, Input_Object, Output_Object);
	Arm_String.Close (Input_Object);
	Format_Object.Document := Real_Document;
	if Format_State.Nesting_Stack_Ptr /= 0 then
	    Ada.Text_IO.Put_Line ("   ** Unfinished commands detected.");
	end if;
    end Format;

end ARM_Format;
