with ARM_Output,
     ARM_Input,
     ARM_File,
     ARM_String,
     ARM_Contents,
     ARM_Database,
     ARM_Syntax,
     ARM_Index,
     ARM_Subindex,
     ARM_Format.Data,
     Ada.Text_IO,
     Ada.Characters.Handling,
     Ada.Strings.Fixed;
package body ARM_Format is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package contains the routines to parse the input files, and
    -- determine what to output.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
    --           2010, 2011, 2012  AXE Consultants. All rights reserved.
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
    -- 10/29/04 - RLB - Added code so that section references in Annex K are
    --			links.
    -- 11/02/04 - RLB - Added "DeletedAdded" ChgRef kind.
    -- 11/03/04 - RLB - Fixed @Chg nesting glitch.
    --		- RLB - Added InnerInnerItemize == Nested_X2_Bulleted.
    -- 11/04/04 - RLB - Fixed a problem that reset the insertion number for
    --			paragraphs have a normal AARM para. was encountered.
    -- 11/15/04 - RLB - Added Indented_Nested_Bulleted style.
    -- 12/06/04 - RLB - Added "RevisedAdded" ChgRef kind.
    --		- RLB - Delayed generation of references until the start of
    --			the paragraph. That avoids "pinning" problems,
    --			especially for multiple changes in a single paragraph.
    --		- RLB - Allow multiple Ref and ARef params in ChgAttribute.
    --		- RLB - Added ChgAdded and ChgDeleted for entire paragraph
    --			operations.
    -- 12/11/04 - RLB - Fixed brackets in Added_Pragma_Syntax to allow {} in
    --			text.
    --		- RLB - Implemented attribute adding in Change_Attribute.
    -- 12/13/04 - RLB - Fixed problems in the new change commands.
    -- 12/15/04 - RLB - Fixed so a change is not left open across
    --			an End_Hang_Item.
    --		- RLB - Fixed glitches with deleted paragraphs.
    --  1/19/05 - RLB - Added LabeledRevisedInformativeAnnex.
    --		- RLB - Fixed AARM paragraph numbers to allow more than 52,
    --			and to put out an error message if we exceed the maximum.
    --		- RLB - Added ChgDocReq and ChgImplAdvice.
    --		- RLB - Added AddedDocReqList and AddedImplAdviceList.
    --  1/20/05 - RLB - Added debugging for stack overflows.
    --  1/24/05 - RLB - Added Inner_Indented.
    --	1/25/05 - RLB - Added AddedSyn and DeleteSyn commands.
    --  2/ 1/05 - RLB - Added Turkish Is.
    --  2/ 2/05 - RLB - Corrected so normal AARM numbers don't reset the
    --			RM insertion number.
    --  3/15/05 - RLB - Corrected spelling.
    --  5/27/05 - RLB - Added @Unicode command for examples.
    --  8/ 9/05 - RLB - Changed the capitalization of some AARM note headers.
    -- 10/17/05 - RLB - Added Glossary change commands.
    -- 10/28/05 - RLB - Made index changes for Ada 200Y.
    --		- RLB - Added added Annex headers.
    --		- RLB - Added Language-Defined Entity indexes.
    -- 10/31/05 - RLB - Fixed the "this paragraph was deleted" code to
    --			not change the version; it's not necessarily
    --			initialized, and the Kind isn't set anyway if the
    --			version is too new.
    --  1/ 5/06 - RLB - Corrected a comment.
    --  1/12/06 - RLB - Replaced "Document" with a number of new more general
    --			properties.
    --  1/13/06 - RLB - Added various link commands.
    --  1/16/06 - RLB - Added missing initializations.
    --		- RLB - Added IndexList command.
    --		- RLB - Added Unnumbered_Section counter to ensure that
    --			such sections are uniquely named.
    --  1/18/06 - RLB - Added "Example_Font".
    --		- RLB - Redid formatting command nesting so that closing
    --			restores to the initial state for the command, not the
    --			default state.
    --  1/20/06 - RLB - Added AILink command.
    --  2/ 8/06 - RLB - Added command checking at the end of each table row.
    --  2/ 9/06 - RLB - Implemented enhanced Table command.
    --  2/10/06 - RLB - Split scanning phase into a separate file.
    --		- RLB - Added additional features to the Table command.
    --		- RLB - Added the picture command.
    --	2/15/06 - RLB - Added code to prevent the generation of note numbers
    --			for deleted notes in final documents.
    --  2/17/06 - RLB - Tracked down issues with @ChgImplDef.
    --		- RLB - Added code so that index entries don't display soft
    --			hyphens.
    --		- RLB - Fixed glossary entries to not display insertions if
    --			the mode would prevent that.
    --  6/22/06 - RLB - Added non-terminal linking.
    --  8/ 4/06 - RLB - Added checking for bad unit indexing.
    --  9/22/06 - RLB - Added "Use_ISO_2004_Note_Format", and implemented that
    --			format.
    --		- RLB - Revised to use Clause_Number_Type, and to support
    --			Subsubclauses.
    --  9/25/06 - RLB - Added "Use_ISO_2004_Contents_Format".
    --		- RLB - Added LastColWidth to @Table.
    --		- RLB - Fixed Enumerated in Notes styles.
    --  9/29/06 - RLB - Added Element_Ref and Child_Ref for ASIS.
    -- 10/04/06 - RLB - Added and implemented "Use_ISO_2004_List_Format".
    --		- RLB - Added "InnerEnumerate" text block.
    -- 10/13/06 - RLB - Added the @ntf command to handle cases where the
    --			text needs to look like a non-terminal but it isn't
    --			a real non-terminal.
    --		- RLB - Added code to handle simple embedded commands in
    --			@nt{} to generate links.
    -- 10/16/06 - RLB - Added code to register deleted non-terminals (so
    --			that they can be linked).
    -- 10/18/06 - RLB - Fixed so that deleted glossary items still get
    --			deleted paragraph numbers.
    --  2/ 5/07 - RLB - Added a paragraph kind, and changed ones that
    --			appear in ASIS. Also renamed "Wide" to "Wide_Above"
    --			so the purpose is more obvious.
    --  2/ 9/07 - RLB - Moved AI interpretation and folding to the HTML
    --			driver, as constructing the link should be its
    --			responsibility. This also allows new kinds of AI here.
    --  2/13/07 - RLB - Redid output formating to use an explict indent;
    --                  added ChildExample.
    --  2/16/07 - RLB - Added Indent format.
    --  2/19/07 - RLB - Added Title format.
    -- 12/18/07 - RLB - Initialized Version in some cases.
    --		- RLB - Added check for open formatting commands
    --			in Check_End_Paragraph.
    --		- RLB - Added Plain_Annex and associated commands.
    -- 12/19/07 - RLB - Added color commands.
    --  6/12/08 - RLB - Corrected handling of the ChgGlossary command.
    --  3/ 4/09 - RLB - Added code to suppress bullets and the like when
    --			displaying a deleted paragraph in New-Only mode
    --			and no paragraph numbers are shown.
    --  5/ 6/09 - RLB - Added Labeled_Deleted_xxx.
    --  5/15/09 - RLB - Fixed missing code for note numbers in revised/added clauses.
    --  4/23/10 - RLB - Added Ada 2005 clause headers for Ada 2012 edition.
    --  8/ 8/11 - RLB - Split various data items to reduce the size of this
    --			package.
    -- 10/18/11 - RLB - Changed to GPLv3 license.
    -- 10/19/11 - RLB - Added AspectDefn command.
    -- 10/20/11 - RLB - Added optional initial version parameter to ChgImplDef
    --			and related commands.
    --		- RLB - Added DeletedPragmaSyn command.
    -- 10/25/11 - RLB - Added optional initial version parameter to
    --			LabeledRevisedSomething commands.
    -- 10/26/11 - RLB - Added versioned break commands.
    --  3/19/12 - RLB - Fixed bug that occurred only when paragraph numbers
    --			are off (ISO versions). Fixed sort order of attributes.

    type Command_Kind_Type is (Normal, Begin_Word, Parameter);

    use ARM_Format.Data; -- use all type ARM_Format.Data.Command_Type;
	-- Make the enumeration literals visible.

    Free_References : Reference_Ptr := null; -- Unused reference objects.
	-- We don't expect there ever to be many of these, so we don't try
	-- to deallocate them.
    Allocated_Reference_Count : Natural := 0;

    function Allocate_Reference return Reference_Ptr is
	-- Allocate a reference object from either the free list, or allocate
	-- it.
	T : Reference_Ptr;
    begin
	if Free_References /= null then
	    T := Free_References;
	    Free_References := Free_References.Next;
	    return T;
	else
	    Allocated_Reference_Count := Allocated_Reference_Count + 1;
	    if Allocated_Reference_Count > 20 then -- Never more than this on one paragraph.
                Ada.Text_IO.Put_Line ("  ** Too many references allocated");
	    end if;
	    return new Reference;
	end if;
    end Allocate_Reference;


    procedure Free_Reference (Reference : in out Reference_Ptr) is
	-- Put a reference object on the free list; setting Reference to null.
    begin
	Reference.Next := Free_References;
	Free_References := Reference;
	Reference.Ref_Len := 0; -- Clear length, so we don't reuse by accident.
	Reference := null;
    end Free_Reference;


    procedure Create (Format_Object : in out Format_Type;
		      Changes : in ARM_Format.Change_Kind;
		      Change_Version : in ARM_Contents.Change_Version_Type;
		      Display_Index_Entries : in Boolean;
		      Include_Annotations : in Boolean;
		      Include_ISO : in Boolean;
		      Link_Non_Terminals : in Boolean;
		      Number_Paragraphs : in Boolean;
		      Examples_Font : in ARM_Output.Font_Family_Type;
		      Use_ISO_2004_Note_Format : in Boolean;
		      Use_ISO_2004_Contents_Format : in Boolean;
		      Use_ISO_2004_List_Format : in Boolean) is
	-- Initialize an input object. Changes and Change_Version determine
	-- which changes should be displayed. If Display_Index_Entries is True,
	-- index entries will be printed in the document; otherwise, they
	-- will not generate any visible text (although they might generate
	-- a link anchor). If Include_Annotations is True, annotations (AARM
	-- text) will be included in the output; otherwise it will not be.
	-- If Include_ISO is True, ISOOnly text will be included in the output
	-- (and NotISO text will not); otherwise the reverse is true.
	-- If Link_Non_Terminals is True, links will be generated for
	-- each Non_Terminal, linking it to its definition.
	-- If Number_Paragraphs is true, paragraphs will be numbered (per
	-- subclause); otherwise they will not be.
	-- Example_Font specifies the font that examples will be set in.
	-- If Use_ISO_2004_Note_Format is true, that format will be used;
	-- else the Ada95 standard's format will be used for notes.
	-- If Use_ISO_2004_Contents_Format is true, that format will be used;
	-- else the Ada95 standard's format will be used for the table of contents.
	-- If Use_ISO_2004_List_Format is true, then lists will be lettered;
	-- else the Ada95 standard's numbering format will be used for
	-- enumerated lists.
    begin
	Format_Object.Changes := Changes;
	Format_Object.Change_Version := Change_Version;
	Format_Object.Display_Index_Entries := Display_Index_Entries;
	Format_Object.Include_Annotations := Include_Annotations;
	Format_Object.Include_ISO := Include_ISO;
	Format_Object.Link_Non_Terminals := Link_Non_Terminals;
	Format_Object.Number_Paragraphs := Number_Paragraphs;
	Format_Object.Examples_Font := Examples_Font;
	Format_Object.Use_ISO_2004_Note_Format := Use_ISO_2004_Note_Format;
	Format_Object.Use_ISO_2004_Contents_Format := Use_ISO_2004_Contents_Format;
	Format_Object.Use_ISO_2004_List_Format := Use_ISO_2004_List_Format;

	Format_Object.Clause_Number := (Section => 0, Clause => 0,
				        Subclause => 0, Subsubclause => 0);
	Format_Object.Unnumbered_Section := 0;
        Format_Object.Next_Note := 1;
        Format_Object.Next_Paragraph := 1;
        Format_Object.Next_Insert_Para := 1;
        Format_Object.Next_AARM_Sub := 'a';
        Format_Object.Next_AARM_Insert_Para := 1;
        Format_Object.Next_Enumerated_Num := 1;
        Format_Object.Enumerated_Level := 0;
        Format_Object.Last_Paragraph_Subhead_Type := Plain;
        Format_Object.Next_Paragraph_Subhead_Type := Plain;
        Format_Object.Next_Paragraph_Format_Type := Plain;
	ARM_Database.Create (Format_Object.Aspect_DB);
	ARM_Database.Create (Format_Object.Attr_DB);
	ARM_Database.Create (Format_Object.Pragma_DB);
	ARM_Database.Create (Format_Object.Glossary_DB);
	ARM_Database.Create (Format_Object.Impdef_DB);
	ARM_Database.Create (Format_Object.Impladv_DB);
	ARM_Database.Create (Format_Object.Docreq_DB);
	ARM_Syntax.Create;
	ARM_Index.Create;
	ARM_Subindex.Create (Format_Object.Package_Index);
	ARM_Subindex.Create (Format_Object.Type_Index);
	ARM_Subindex.Create (Format_Object.Subprogram_Index);
	ARM_Subindex.Create (Format_Object.Exception_Index);
	ARM_Subindex.Create (Format_Object.Object_Index);
    end Create;


    procedure Destroy (Format_Object : in out Format_Type) is
	-- Destroy a format object, releasing any resources.
    begin
	ARM_Database.Destroy (Format_Object.Aspect_DB);
	ARM_Database.Destroy (Format_Object.Attr_DB);
	ARM_Database.Destroy (Format_Object.Pragma_DB);
	ARM_Database.Destroy (Format_Object.Glossary_DB);
	ARM_Database.Destroy (Format_Object.Impdef_DB);
	ARM_Database.Destroy (Format_Object.Impladv_DB);
	ARM_Database.Destroy (Format_Object.Docreq_DB);
	ARM_Syntax.Destroy;
	ARM_Index.Destroy;
	ARM_Subindex.Destroy (Format_Object.Package_Index);
	ARM_Subindex.Destroy (Format_Object.Type_Index);
	ARM_Subindex.Destroy (Format_Object.Subprogram_Index);
	ARM_Subindex.Destroy (Format_Object.Exception_Index);
	ARM_Subindex.Destroy (Format_Object.Object_Index);
    end Destroy;


    function Clause_String (Format_Object : in Format_Type) return String is
        -- Returns a string for a clause reference.
        use type ARM_Contents.Section_Number_Type;
    begin
        if Format_Object.Clause_Number.Subsubclause /= 0 then
	    return ARM_Contents.Make_Clause_Number (
		    ARM_Contents.SubSubClause,
		    Format_Object.Clause_Number);
        elsif Format_Object.Clause_Number.Subclause /= 0 then
	    return ARM_Contents.Make_Clause_Number (
		    ARM_Contents.SubClause,
		    Format_Object.Clause_Number);
        elsif Format_Object.Clause_Number.Clause /= 0 then
	    return ARM_Contents.Make_Clause_Number (
		    ARM_Contents.Clause,
		    Format_Object.Clause_Number);
        else
	    if Format_Object.Clause_Number.Section = 0 then
	        return ARM_Contents.Make_Clause_Number (
		        ARM_Contents.Unnumbered_Section,
			Format_Object.Clause_Number);
	    elsif Format_Object.Clause_Number.Section < ARM_Contents.ANNEX_START then
	        return ARM_Contents.Make_Clause_Number (
		        ARM_Contents.Section,
		        Format_Object.Clause_Number);
	    else
	        return ARM_Contents.Make_Clause_Number (
		        ARM_Contents.Plain_Annex, -- Same for all kinds of annex.
		        Format_Object.Clause_Number);
	    end if;
        end if;
    end Clause_String;


    Do_Not_Display_Text : constant ARM_Output.Change_Type := ARM_Output.Both;
        -- Special meaning for Calc_Change_Disposition, below.
    procedure Calc_Change_Disposition
	        (Format_Object : in Format_Type;
		 Version       : in ARM_Contents.Change_Version_Type;
		 Operation     : in ARM_Output.Change_Type;
		 Text_Kind     : out ARM_Output.Change_Type) is
        -- Determine the appropriate disposition for text.
        -- The text is to be inserted if Operation is Insertion;
        -- and deleted if Operation is Deletion.
        -- The appropriate Change_Type to use is returned in Text_Kind.
        -- If Text_Kind is None, the text should be displayed normally.
        -- If Text_Kind is Insertion, the text should be displayed as inserted.
        -- If Text_Kind is Deletion, the text should be displayed as deletion.
        -- If Text_Kind is Do_Not_Display_Text (same as Both), the
        --   text should not be shown at all.
        -- Program_Error is raised if Operation is None or Both.
        -- This routine assumes that we are not nested
        -- in some other change item.
        use type ARM_Output.Change_Type;
    begin
        if Operation = ARM_Output.None or else
	   Operation = ARM_Output.Both then
	    raise Program_Error;
        end if;
        -- We can't check for nesting, because in some cases it happens
        -- harmlessly (i.e. Added_Pragma_Syn).

        case Format_Object.Changes is
	    when ARM_Format.Old_Only =>
	        -- Display only the original version ('0').
	        if Operation = ARM_Output.Insertion then
		    if Version > '0' then
		        Text_Kind := Do_Not_Display_Text; -- Newer than original.
		    else
		        Text_Kind := ARM_Output.None; -- Display normally.
		    end if;
	        else -- Deletion
		    if Version > '0' then
		        Text_Kind := ARM_Output.None; -- Display normally, not deleted in original code.
		    else
		        Text_Kind := Do_Not_Display_Text; -- Deleted in original.
		    end if;
	        end if;
	    when ARM_Format.New_Only =>
	        -- Display only the version
	        -- Format_Object.Change_Version, no insertions or deletions.
	        if Operation = ARM_Output.Insertion then
		    if Version > Format_Object.Change_Version then
		        -- Change version newer than we're displaying;
		        -- ignore the item.
		        Text_Kind := Do_Not_Display_Text;
		    else
		        -- Display the change normally.
		        Text_Kind := ARM_Output.None;
		    end if;
	        else -- Deletion
		    if Version > Format_Object.Change_Version then
		        -- Change version newer than we're displaying;
		        -- leave the item in and display normally.
		        Text_Kind := ARM_Output.None;
		    else
		        -- Delete the item.
		        Text_Kind := Do_Not_Display_Text;
		    end if;
	        end if;
	    when ARM_Format.Changes_Only =>
	        -- Display only the the changes for version
	        -- Format_Object.Change_Version, older changes
	        -- are applied and newer changes are ignored.
	        if Operation = ARM_Output.Insertion then
		    if Version > Format_Object.Change_Version then
		        -- Change version is newer than we're displaying;
		        -- ignore the item.
		        Text_Kind := Do_Not_Display_Text;
		    elsif Version < Format_Object.Change_Version then
		        -- Change version is older than we're displaying;
		        -- display the change normally.
		        Text_Kind := ARM_Output.None;
		    else
		        -- The correct version, display the change
		        -- as an insertion.
		        Text_Kind := ARM_Output.Insertion;
		    end if;
	        else -- Deletion.
		    if Version > Format_Object.Change_Version then
		        -- Change version is newer than we're displaying;
		        -- the item isn't deleted yet, display the change
		        -- normally.
		        Text_Kind := ARM_Output.None;
		    elsif Version < Format_Object.Change_Version then
		        -- Change version is older than we're displaying;
		        -- the item is deleted, so ignore the item.
		        Text_Kind := Do_Not_Display_Text;
		    else
		        -- The correct version, display the change
		        -- as a deletion.
		        Text_Kind := ARM_Output.Deletion;
		    end if;
	        end if;
	    when ARM_Format.Show_Changes |
		 ARM_Format.New_Changes =>
	        -- Display all of the changes up to version
	        -- Format_Object.Change_Version, newer changes are
	        -- ignored. (New_Changes shows deletions as a single
	        -- character for older versions of Word, but otherwise
	        -- is the same.)
	        if Operation = ARM_Output.Insertion then
		    if Version > Format_Object.Change_Version then
		        -- Change version is newer than we're displaying;
		        -- ignore the item.
		        Text_Kind := Do_Not_Display_Text;
		    else
		        -- This version or older, display the change
		        -- as an insertion.
		        Text_Kind := ARM_Output.Insertion;
		    end if;
	        else -- Deletion.
		    if Version > Format_Object.Change_Version then
		        -- Change version is newer than we're displaying;
		        -- the item isn't deleted yet, display the change
		        -- normally.
		        Text_Kind := ARM_Output.None;
		    else
		        -- The correct version, display the change
		        -- as a deletion.
		        Text_Kind := ARM_Output.Deletion;
		    end if;
	        end if;
        end case;
    end Calc_Change_Disposition;


    function Get_Current_Item (Format_Object : in Format_Type;
			       Input_Object : in ARM_Input.Input_Type'Class;
			       Item : in String) return String is
        -- Return the "current" item from Item. This is Item itself,
	-- unless Item includes an @Chg.
	New_Pos : Natural;
        Close_Ch : Character;
        Open_Cnt : Natural;
	My_Item : constant String (1 .. Item'Length) := Item;
		-- Just to slide the bounds.
	Version : ARM_Contents.Change_Version_Type := '0';
	Disposition : ARM_Output.Change_Type;
        use type ARM_Output.Change_Type;
    begin
        if My_Item'Length < 11 or else
	   My_Item (1) /= '@' or else
	   Ada.Characters.Handling.To_Lower (My_Item (2 .. 4)) /= "chg" then
	    -- No @Chg command here.
	    return My_Item;
	end if;
	if Ada.Characters.Handling.To_Lower (My_Item (6 .. 9)) = "new=" then
	    -- No version parameter:
	    New_Pos := 6;
	    Version := '1';
	elsif My_Item'Length > 22 and then
	    Ada.Characters.Handling.To_Lower (My_Item (6 .. 14)) = "version=[" and then
	    Ada.Characters.Handling.To_Lower (My_Item (16 .. 21)) = "],new=" then
	    New_Pos := 18;
	    Version := My_Item(15);
	else
Ada.Text_IO.Put_Line ("%% Oops, can't find either Version or New in item chg command, line " & ARM_Input.Line_String (Input_Object));
	    return My_Item;
	end if;

	Calc_Change_Disposition (Format_Object => Format_Object,
		 Version => Version,
		 Operation => ARM_Output.Insertion,
		 Text_Kind => Disposition);

	if Disposition = Do_Not_Display_Text then
	    -- Find the end of the "New" parameter, and return the "Old"
	    -- parameter.
	    Close_Ch := ARM_Input.Get_Close_Char (
	        My_Item(New_Pos+4));
	    Open_Cnt := 1;
	    for I in New_Pos+5 .. My_Item'Last loop
	        if My_Item(I) = My_Item(New_Pos+4) then
		    Open_Cnt := Open_Cnt + 1;
	        elsif My_Item(I) = Close_Ch then
		    if Open_Cnt <= 1 then
		        -- OK, the end of the "New" parameter is at 'I'.
		        if My_Item'Last < I+7 or else
			   My_Item (I+1) /= ',' or else
			   Ada.Characters.Handling.To_Lower (My_Item (I+2 .. I+4)) /= "old" or else
			   My_Item (I+5) /= '=' then
			    exit; -- Heck if I know.
		        end if;
		        Close_Ch := ARM_Input.Get_Close_Char (
			    My_Item(I+6));
		        Open_Cnt := 1;
		        for J in I+7 .. My_Item'Last loop
			    if My_Item(J) = My_Item(I+6) then
			        Open_Cnt := Open_Cnt + 1;
			    elsif My_Item(J) = Close_Ch then
			        if Open_Cnt <= 1 then
				    return My_Item (I + 7 .. J - 1);
			        else
				    Open_Cnt := Open_Cnt - 1;
			        end if;
			    -- else continue looking.
			    end if;
		        end loop;
Ada.Text_IO.Put_Line ("%% Oops, can't find end of item chg old command, line " & ARM_Input.Line_String (Input_Object));
		        return My_Item (I + 7 .. My_Item'Last);
		    else
		        Open_Cnt := Open_Cnt - 1;
		    end if;
	        -- else continue looking.
	        end if;
	    end loop;
Ada.Text_IO.Put_Line ("%% Oops, can't find end of item chg new command, line " & ARM_Input.Line_String (Input_Object));
	    return My_Item (New_Pos+5 .. My_Item'Length);
        else -- Some new format, use the new name.
	    -- Find the end of the "New" parameter, and
	    -- return it.
	    Close_Ch := ARM_Input.Get_Close_Char (My_Item(New_Pos+4));
	    Open_Cnt := 1;
	    for I in New_Pos+5 .. My_Item'Last loop
	        if My_Item(I) = My_Item(New_Pos+4) then
		    Open_Cnt := Open_Cnt + 1;
	        elsif My_Item(I) = Close_Ch then
		    if Open_Cnt <= 1 then
		        return My_Item (New_Pos+5 .. I - 1);
		    else
		        Open_Cnt := Open_Cnt - 1;
		    end if;
	        -- else continue looking.
	        end if;
	    end loop;
	    -- Weird if we get here, can't find end of parameter.
Ada.Text_IO.Put_Line ("%% Oops, can't find end of NT chg new command, line " & ARM_Input.Line_String (Input_Object));
	    return My_Item (New_Pos+5 .. My_Item'Last);
        end if;
    end Get_Current_Item;


    function Get_Old_Item (Format_Object : in Format_Type;
		           Input_Object : in ARM_Input.Input_Type'Class;
		           Item : in String) return String is
        -- Return the "old" item from Item, or nothing if there is no
	-- old item. This is nothing unless Item includes an @Chg,
	-- *and* the new item in the @Chg is displayed.
	New_Pos : Natural;
        Close_Ch : Character;
        Open_Cnt : Natural;
	My_Item : constant String (1 .. Item'Length) := Item;
		-- Just to slide the bounds.
	Version : ARM_Contents.Change_Version_Type := '0';
	Disposition : ARM_Output.Change_Type;
        use type ARM_Output.Change_Type;
    begin
        if My_Item'Length < 11 or else
	   My_Item (1) /= '@' or else
	   Ada.Characters.Handling.To_Lower (My_Item (2 .. 4)) /= "chg" then
	    -- No @Chg command here.
	    return "";
	end if;
	if Ada.Characters.Handling.To_Lower (My_Item (6 .. 9)) = "new=" then
	    -- No version parameter:
	    New_Pos := 6;
	    Version := '1';
	elsif My_Item'Length > 22 and then
	    Ada.Characters.Handling.To_Lower (My_Item (6 .. 14)) = "version=[" and then
	    Ada.Characters.Handling.To_Lower (My_Item (16 .. 21)) = "],new=" then
	    New_Pos := 18;
	    Version := My_Item(15);
	else
Ada.Text_IO.Put_Line ("%% Oops, can't find either Version or New in item chg command, line " & ARM_Input.Line_String (Input_Object));
	    return "";
	end if;

	Calc_Change_Disposition (Format_Object => Format_Object,
		 Version => Version,
		 Operation => ARM_Output.Insertion,
		 Text_Kind => Disposition);

	if Disposition /= Do_Not_Display_Text then
	    -- Some new item was shown.
	    -- Find the end of the "New" parameter, and return the "Old"
	    -- parameter.
	    Close_Ch := ARM_Input.Get_Close_Char (
	        My_Item(New_Pos+4));
	    Open_Cnt := 1;
	    for I in New_Pos+5 .. My_Item'Last loop
	        if My_Item(I) = My_Item(New_Pos+4) then
		    Open_Cnt := Open_Cnt + 1;
	        elsif My_Item(I) = Close_Ch then
		    if Open_Cnt <= 1 then
		        -- OK, the end of the "New" parameter is at 'I'.
		        if My_Item'Last < I+7 or else
			   My_Item (I+1) /= ',' or else
			   Ada.Characters.Handling.To_Lower (My_Item (I+2 .. I+4)) /= "old" or else
			   My_Item (I+5) /= '=' then
			    exit; -- Heck if I know.
		        end if;
		        Close_Ch := ARM_Input.Get_Close_Char (
			    My_Item(I+6));
		        Open_Cnt := 1;
		        for J in I+7 .. My_Item'Last loop
			    if My_Item(J) = My_Item(I+6) then
			        Open_Cnt := Open_Cnt + 1;
			    elsif My_Item(J) = Close_Ch then
			        if Open_Cnt <= 1 then
				    return My_Item (I + 7 .. J - 1);
			        else
				    Open_Cnt := Open_Cnt - 1;
			        end if;
			    -- else continue looking.
			    end if;
		        end loop;
Ada.Text_IO.Put_Line ("%% Oops, can't find end of item chg old command, line " & ARM_Input.Line_String (Input_Object));
		        return My_Item (I + 7 .. My_Item'Last);
		    else
		        Open_Cnt := Open_Cnt - 1;
		    end if;
	        -- else continue looking.
	        end if;
	    end loop;
Ada.Text_IO.Put_Line ("%% Oops, can't find end of item chg new command, line " & ARM_Input.Line_String (Input_Object));
	    return "";
        else -- The new item wasn't displayed, so we already have used the
	     -- old item.
	    return "";
        end if;
    end Get_Old_Item;


    procedure Scan (Format_Object : in out Format_Type;
		    File_Name : in String;
		    Section_Number : in ARM_Contents.Section_Number_Type;
		    Starts_New_Section : in Boolean) is separate;
	-- Scans the contents for File_Name, determining the table of contents
	-- for the section. The results are written to the contents package.
	-- Starts_New_Section is True if the file starts a new section.
	-- Section_Number is the number (or letter) of the section.
	-- See ARM_FRMS.ADB.


    procedure Write_Table_of_Contents (
		       Format_Object : in out Format_Type;
		       Output_Object : in out ARM_Output.Output_Type'Class) is
	-- Writes the table of contents for the document. (It will have
	-- a section name of "TOC"). This should be done after all calls to
	-- Scan and before any calls to Process.

	In_Paragraph : Boolean := False;

	procedure Write_It (Title : in ARM_Contents.Title_Type;
		   Level : in ARM_Contents.Level_Type;
		   Clause_Number : in ARM_Contents.Clause_Number_Type;
                   Version : in ARM_Contents.Change_Version_Type;
		   Quit : out Boolean) is
	    Clause_Text : constant String :=
		ARM_Contents.Make_Clause_Number (Level, Clause_Number);
	    Old_Title : ARM_Contents.Title_Type :=
		        ARM_Contents.Lookup_Old_Title (
			    Level, Clause_Number);
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
	    elsif ARM_Contents."=" (Level, ARM_Contents.Subsubclause) then
		ARM_Output.Line_Break (Output_Object);
		ARM_Output.Ordinary_Text (Output_Object, "            ");
		ARM_Output.Ordinary_Text (Output_Object, Clause_Text);
		ARM_Output.Hard_Space (Output_Object);
	    elsif ARM_Contents."=" (Level, ARM_Contents.Unnumbered_Section) then
	        if In_Paragraph then
		    ARM_Output.End_Paragraph (Output_Object);
	        end if;
	        ARM_Output.Start_Paragraph (Output_Object,
				            ARM_Output.Normal,
				            Indent => 0, Number => "",
					    Justification => ARM_Output.Left);
	        In_Paragraph := True;
	    else
		if In_Paragraph then
		    ARM_Output.End_Paragraph (Output_Object);
		end if;
	        ARM_Output.Start_Paragraph (Output_Object,
				            ARM_Output.Normal,
				            Indent => 0, Number => "",
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
			    Level, Clause_Number), Ada.Strings.Right),
		    Clause_Text);
	    else
	        case Format_Object.Changes is
	            when ARM_Format.Old_Only =>
		        ARM_Output.Clause_Reference (Output_Object,
		            Ada.Strings.Fixed.Trim (
			        ARM_Contents.Lookup_Old_Title (
				    Level, Clause_Number), Ada.Strings.Right),
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
	if Format_Object.Use_ISO_2004_Contents_Format then
            ARM_Output.Section (Output_Object,
			        Section_Title => "Contents",
			        Section_Name => "TOC");

	    ARM_Output.Clause_Header (Output_Object,
				      Header_Text => "Contents",
				      Level => ARM_Contents.Section,
				      Clause_Number => "");
	else
            ARM_Output.Section (Output_Object,
			        Section_Title => "Table of Contents",
			        Section_Name => "TOC");

	    ARM_Output.Clause_Header (Output_Object,
				      Header_Text => "Table of Contents",
				      Level => ARM_Contents.Section,
				      Clause_Number => "");
	end if;

	ARM_Output.TOC_Marker (Output_Object, For_Start => True);

	Write_Contents;

	if In_Paragraph then
	    ARM_Output.End_Paragraph (Output_Object);
	end if;

	ARM_Output.TOC_Marker (Output_Object, For_Start => False);

    end Write_Table_of_Contents;


    procedure Make_References (List : in out Reference_Ptr;
			       Format_Object : in out Format_Type;
			       Output_Object : in out ARM_Output.Output_Type'Class) is
	-- Write the references to the Output_Object, using the format
	-- of Format_Object.
	-- Deallocate the references on List; List will be null afterwards.
	Temp : Reference_Ptr;
	Our_Text_Format : ARM_Output.Format_Type;
    begin
	-- We assume these are only stored here if we want to see them
	-- on *this* paragraph. Thus, we just output them if they exist
	-- here.
	Our_Text_Format := Format_Object.Text_Format;
	Our_Text_Format.Change := ARM_Output.None; -- No changes should be reflected in references.

	while List /= null loop
	    -- Output a reference. These are *never* marked as
	    -- inserted or deleted, so set the style properly.
	    ARM_Output.Text_Format (Output_Object,
				    Format => Our_Text_Format);
	    ARM_Output.Ordinary_Character (Output_Object, '{');
	    Our_Text_Format.Italic := True;
	    ARM_Output.Text_Format (Output_Object,
				    Format => Our_Text_Format);
	    if List.Is_DR_Ref then
	        -- Output a DR reference.
	        ARM_Output.DR_Reference (Output_Object,
					 Text => List.Ref_Name(1..List.Ref_Len),
					 DR_Number => List.Ref_Name(1..List.Ref_Len));
	    else
	        -- Output an AI reference.
	        ARM_Output.AI_Reference (Output_Object,
					 Text => List.Ref_Name(1..List.Ref_Len),
					 AI_Number => List.Ref_Name(1..List.Ref_Len));
	    end if;
	    Our_Text_Format.Italic := Format_Object.Text_Format.Italic;
	    ARM_Output.Text_Format (Output_Object,
				    Format => Our_Text_Format);
	    ARM_Output.Ordinary_Character (Output_Object, '}');
	    ARM_Output.Ordinary_Character (Output_Object, ' ');
	    -- Reset to the current format.
	    ARM_Output.Text_Format (Output_Object,
				    Format => Format_Object.Text_Format);
	    Format_Object.Last_Non_Space := False;

	    Temp := List;
	    List := List.Next;
	    Free_Reference (Temp);
	end loop;
    end Make_References;


    procedure Dump_References (List : in out Reference_Ptr) is
	-- Destroy any references in List; List will be null afterwards.
	Temp : Reference_Ptr;
    begin
	while List /= null loop
	    Temp := List;
	    List := List.Next;
	    Free_Reference (Temp);
	end loop;
    end Dump_References;


    type Items is record
        Kind : Command_Kind_Type;
        Name : ARM_Input.Command_Name_Type;
        Command : Data.Command_Type;
        Close_Char : Character; -- Ought to be }, ], >, or ).
	Text_Format : ARM_Output.Format_Type;
	    -- Format at the start of the command.

        -- The next four are only used if Kind=Begin_Word, or for
        -- Command=Implementation_Defined, Glossary_Text_Param, or
	--    Syntax_Rule_RHS.
        Old_Last_Subhead_Paragraph : Paragraph_Type;
        Old_Next_Subhead_Paragraph : Paragraph_Type;
        Old_Next_Paragraph_Format : Paragraph_Type;
	Old_Tab_Stops : ARM_Output.Tab_Info;
	Old_Next_Enum_Num : Positive;
	Is_Formatting : Boolean; -- Only used if Kind=Begin_Word.
				 -- The command changes the PARAGRAPH format.
				 -- Otherwise, it should be ignored when
				 -- when determining the format.
	-- The following is only used if Command = Change, Change_Added,
	-- Change_Deleted, Added_Subheading,
	-- Added_Pragma_Syntax, Deleted_Pragma_Syntax,
	-- Added_Syntax, Deleted_Syntax,
	-- New_Page_for_Version, New_Column_for_Version, and
	-- RM_New_Page_for_Version.
	Change_Version : ARM_Contents.Change_Version_Type;
	-- The following are only used if Command = Change,
	-- Added_Pragma_Syntax, and Deleted_Pragma_Syntax.
	Prev_Change_Version : ARM_Contents.Change_Version_Type;
	-- The following are only used if Command = Change.
	Was_Text : Boolean; -- Did the current subcommand have text?
	Prev_Change : ARM_Output.Change_Type;
	Prev_Added_Change_Version : ARM_Contents.Change_Version_Type;
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
		 Command => Data.Command (Name),
		 Close_Char => ' ', -- Set below.
		 Text_Format => Format_Object.Text_Format, -- Save the current format.
		 -- Other things next necessarily used:
		 Old_Last_Subhead_Paragraph => Plain, -- Not used.
		 Old_Next_Subhead_Paragraph => Plain, -- Not used.
		 Old_Next_Paragraph_Format => Plain, -- Not used.
		 Old_Tab_Stops => ARM_Output.NO_TABS, -- Not used.
		 Old_Next_Enum_Num => 1, -- Not used.
		 Is_Formatting => False, -- Not used.
		 Change_Version => '0', -- Not used.
		 Was_Text => False, -- Not used.
		 Prev_Change => ARM_Output.None, -- Not used.
		 Prev_Change_Version => '0', -- Not used.
		 Prev_Added_Change_Version => '0'); -- Not used.
	    Format_State.Nesting_Stack (Format_State.Nesting_Stack_Ptr).Close_Char := ARM_Input.Get_Close_Char (Param_Ch);
--Ada.Text_IO.Put_Line (" &Stack (" & Name & "); Close-Char=" &
--  Format_State.Nesting_Stack (Format_State.Nesting_Stack_Ptr).Close_Char);
	end Set_Nesting_for_Command;


	procedure Set_Nesting_for_Parameter (Command  : in Data.Command_Type;
					     Close_Ch : in Character) is
	    -- Push the parameter onto the nesting stack.
	begin
	    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr + 1;
	    Format_State.Nesting_Stack (Format_State.Nesting_Stack_Ptr) :=
	        (Name => (others => ' '),
		 Kind => Parameter,
		 Command => Command,
		 Close_Char => Close_Ch,
		 Text_Format => Format_Object.Text_Format, -- Save the current
			-- format (not really used here).
		 Old_Last_Subhead_Paragraph => Plain, -- Not used.
		 Old_Next_Subhead_Paragraph => Plain, -- Not used.
		 Old_Next_Paragraph_Format => Plain, -- Not used.
		 Old_Tab_Stops => ARM_Output.NO_TABS, -- Not used.
		 Old_Next_Enum_Num => 1, -- Not used.
		 Is_Formatting => False, -- Not used.
		 Change_Version => '0', -- Not used.
		 Was_Text => False, -- Not used.
		 Prev_Change => ARM_Output.None, -- Not used.
		 Prev_Change_Version => '0', -- Not used.
		 Prev_Added_Change_Version => '0'); -- Not used.
--Ada.Text_IO.Put_Line (" &Stack (Parameter)");
	end Set_Nesting_for_Parameter;


        function Is_AARM_Paragraph (Kind : in Paragraph_Type) return Boolean is
        begin
	    case Kind is
	        when Plain | Introduction | Syntax | Resolution | Legality |
		     Static_Semantics | Link_Time |
		     Run_Time | Bounded_Errors |
		     Erroneous | Requirements | Documentation |
		     Metrics | Permissions | Advice | Notes | Single_Note |
		     Examples =>
		    return False;
	        when Language_Design | Ada83_Inconsistencies |
		     Ada83_Incompatibilities | Ada83_Extensions |
		     Ada83_Wording | Ada95_Inconsistencies |
		     Ada95_Incompatibilities | Ada95_Extensions |
		     Ada95_Wording | Ada2005_Inconsistencies |
		     Ada2005_Incompatibilities | Ada2005_Extensions |
		     Ada2005_Wording | Reason | Ramification | Proof |
		     Imp_Note | Corr_Change | Discussion |
		     Honest | Glossary_Marker | Bare_Annotation |
		     Element_Ref | Child_Ref | Usage_Note =>
		    return True;
	        when In_Table =>
		    return False; -- Tables are never considered part of the
			    -- AARM for formatting purposes, even when they are.
	        when Wide_Above | Example_Text | Indented_Example_Text |
		     Child_Example_Text | Code_Indented | Indent |
		     Bulleted |
		     Nested_Bulleted | Nested_X2_Bulleted |
		     Display | Syntax_Display |
		     Syntax_Indented | Syntax_Production |
		     Enumerated | Nested_Enumerated | Hanging_Indented |
		     Title =>
		    -- This depends on the containing paragraph kind;
		    -- Last_Paragraph_Subhead_Type should contain that.
		    if Format_Object.Last_Paragraph_Subhead_Type = Wide_Above or else
		       Format_Object.Last_Paragraph_Subhead_Type = Example_Text or else
		       Format_Object.Last_Paragraph_Subhead_Type = Child_Example_Text or else
		       Format_Object.Last_Paragraph_Subhead_Type = Indented_Example_Text or else
		       Format_Object.Last_Paragraph_Subhead_Type = Bulleted or else
		       Format_Object.Last_Paragraph_Subhead_Type = Code_Indented or else
		       Format_Object.Last_Paragraph_Subhead_Type = Indent or else
		       Format_Object.Last_Paragraph_Subhead_Type = Nested_Bulleted or else
		       Format_Object.Last_Paragraph_Subhead_Type = Nested_X2_Bulleted or else
		       Format_Object.Last_Paragraph_Subhead_Type = Display or else
		       Format_Object.Last_Paragraph_Subhead_Type = Syntax_Display or else
		       Format_Object.Last_Paragraph_Subhead_Type = Syntax_Indented or else
		       Format_Object.Last_Paragraph_Subhead_Type = Syntax_Production or else
		       Format_Object.Last_Paragraph_Subhead_Type = Enumerated or else
		       Format_Object.Last_Paragraph_Subhead_Type = Nested_Enumerated or else
		       Format_Object.Last_Paragraph_Subhead_Type = Hanging_Indented or else
		       Format_Object.Last_Paragraph_Subhead_Type = Title or else
		       Format_Object.Last_Paragraph_Subhead_Type = In_Table then
Ada.Text_IO.Put_Line ("%% Oops, can't find out if AARM paragraph, line " & ARM_Input.Line_String (Input_Object));
		        return False; -- Oops, can't tell (double nesting).
			    -- We make this check to avoid infinite recursion.
		    else
		        return Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type);
		    end if;
	    end case;
        end Is_AARM_Paragraph;


        function Is_Small_Format_Paragraph (Kind : in Paragraph_Type) return Boolean is
	    -- AARM annotations are in the small font, as are user notes.
        begin
	    case Kind is
	        when Plain | Introduction | Syntax | Resolution | Legality |
		     Static_Semantics | Link_Time |
		     Run_Time | Bounded_Errors |
		     Erroneous | Requirements | Documentation |
		     Metrics | Permissions | Advice | Examples =>
		    return False;
	        when Language_Design | Ada83_Inconsistencies |
		     Ada83_Incompatibilities | Ada83_Extensions |
		     Ada83_Wording | Ada95_Inconsistencies |
		     Ada95_Incompatibilities | Ada95_Extensions |
		     Ada95_Wording | Ada2005_Inconsistencies |
		     Ada2005_Incompatibilities | Ada2005_Extensions |
		     Ada2005_Wording | Reason | Ramification | Proof |
		     Imp_Note | Corr_Change | Discussion |
		     Honest | Glossary_Marker | Bare_Annotation |
		     Element_Ref | Child_Ref | Usage_Note |
		     Notes | Single_Note =>
		    return True;
	        when In_Table =>
		    return False; -- Tables are never considered part of the
			    -- AARM for formatting purposes, even when they are.
	        when Wide_Above | Example_Text | Indented_Example_Text |
		     Child_Example_Text | Code_Indented | Indent |
		     Bulleted |
		     Nested_Bulleted | Nested_X2_Bulleted |
		     Display | Syntax_Display |
		     Syntax_Indented | Syntax_Production |
		     Enumerated | Nested_Enumerated | Hanging_Indented |
		     Title =>
		    -- This depends on the containing paragraph kind;
		    -- Last_Paragraph_Subhead_Type should contain that.
		    if Format_Object.Last_Paragraph_Subhead_Type = Wide_Above or else
		       Format_Object.Last_Paragraph_Subhead_Type = Example_Text or else
		       Format_Object.Last_Paragraph_Subhead_Type = Child_Example_Text or else
		       Format_Object.Last_Paragraph_Subhead_Type = Indented_Example_Text or else
		       Format_Object.Last_Paragraph_Subhead_Type = Bulleted or else
		       Format_Object.Last_Paragraph_Subhead_Type = Code_Indented or else
		       Format_Object.Last_Paragraph_Subhead_Type = Indent or else
		       Format_Object.Last_Paragraph_Subhead_Type = Nested_Bulleted or else
		       Format_Object.Last_Paragraph_Subhead_Type = Nested_X2_Bulleted or else
		       Format_Object.Last_Paragraph_Subhead_Type = Display or else
		       Format_Object.Last_Paragraph_Subhead_Type = Syntax_Display or else
		       Format_Object.Last_Paragraph_Subhead_Type = Syntax_Indented or else
		       Format_Object.Last_Paragraph_Subhead_Type = Syntax_Production or else
		       Format_Object.Last_Paragraph_Subhead_Type = Enumerated or else
		       Format_Object.Last_Paragraph_Subhead_Type = Nested_Enumerated or else
		       Format_Object.Last_Paragraph_Subhead_Type = Hanging_Indented or else
		       Format_Object.Last_Paragraph_Subhead_Type = Title or else
		       Format_Object.Last_Paragraph_Subhead_Type = In_Table then
Ada.Text_IO.Put_Line ("%% Oops, can't find out if AARM paragraph, line " & ARM_Input.Line_String (Input_Object));
		        return False; -- Oops, can't tell (double nesting).
			    -- We make this check to avoid infinite recursion.
		    else
		        return Is_Small_Format_Paragraph (Format_Object.Last_Paragraph_Subhead_Type);
		    end if;
	    end case;
        end Is_Small_Format_Paragraph;


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
	        elsif Character'Val(Character'Pos(Sub_Letter) - 26) <= 'z' then
		    -- Double letter.
		    Format_Object.Current_Paragraph_String(PNum_Pred'Last+1) :=
		        Character'Val(Character'Pos(Sub_Letter) - 26);
		    Format_Object.Current_Paragraph_String(PNum_Pred'Last+2) :=
		        Character'Val(Character'Pos(Sub_Letter) - 26);
		    Format_Object.Current_Paragraph_Len :=
			    PNum_Pred'Last + 2;
	        elsif Character'Val(Character'Pos(Sub_Letter) - 52) <= 'z' then
		    -- Triple letter.
		    Format_Object.Current_Paragraph_String(PNum_Pred'Last+1) :=
		        Character'Val(Character'Pos(Sub_Letter) - 52);
		    Format_Object.Current_Paragraph_String(PNum_Pred'Last+2) :=
		        Character'Val(Character'Pos(Sub_Letter) - 52);
		    Format_Object.Current_Paragraph_String(PNum_Pred'Last+3) :=
		        Character'Val(Character'Pos(Sub_Letter) - 52);
		    Format_Object.Current_Paragraph_Len :=
			    PNum_Pred'Last + 3;
		else -- Doesn't fit!
		    Ada.Text_IO.Put_Line ("** AARM paragraph number out of range, line " & ARM_Input.Line_String (Input_Object));
		    Format_Object.Current_Paragraph_String(PNum_Pred'Last+1) := '$';
		    Format_Object.Current_Paragraph_String(PNum_Pred'Last+2) := '$';
		    Format_Object.Current_Paragraph_String(PNum_Pred'Last+3) := '$';
		    Format_Object.Current_Paragraph_Len :=
			    PNum_Pred'Last + 3;
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
		        Format_Object.Next_Insert_Para := 1;
		        Format_Object.Next_AARM_Sub := 'a';
			Format_Object.Next_AARM_Insert_Para := 1;
		    end if;
	        end if;
	    elsif Format_Object.Next_Paragraph_Change_Kind = ARM_Database.Inserted or else
	          Format_Object.Next_Paragraph_Change_Kind = ARM_Database.Revised_Inserted_Number or else
	          Format_Object.Next_Paragraph_Change_Kind = ARM_Database.Deleted_Inserted_Number or else
	          Format_Object.Next_Paragraph_Change_Kind = ARM_Database.Deleted_Inserted_Number_No_Delete_Message then
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
		else -- None inserted paragraphs.
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
		        Format_Object.Next_Insert_Para := 1;
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

		use type ARM_Output.Paragraph_Indent_Type;

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

		function Enclosing_Indent return ARM_Output.Paragraph_Indent_Type is
		    function Nested_Indent (Start_Nesting : in Natural) return ARM_Output.Paragraph_Indent_Type is
		    begin
		        for I in reverse 1 .. Start_Nesting loop
			    if Format_State.Nesting_Stack(I).Command = Text_Begin and then
			       (Format_State.Nesting_Stack(I).Is_Formatting) then
				case Format_State.Nesting_Stack(I).Old_Next_Paragraph_Format is
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
					return 0; -- No indent.
				    when Wide_Above =>
					if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
					    return 2; -- Normal indent for annotations.
					else
					    return 0; -- No indent.
					end if;
				    when Syntax =>
				        return 1; -- One unit.
				    when Notes | Single_Note => -- Notes (only the numbering varies)
				        return 1; -- One unit.
				    when Element_Ref | Child_Ref | Usage_Note => -- Similar to an AARM note.
				        return 2; -- Normal indent for annotations.
				    when Language_Design | -- "MetaRules"
				         Ada83_Inconsistencies | -- Inconsistent83
				         Ada83_Incompatibilities | -- Incompatible83
				         Ada83_Extensions | -- Extend83
				         Ada83_Wording | -- DiffWord83
				         Ada95_Inconsistencies | -- Inconsistent95
				         Ada95_Incompatibilities | -- Incompatible95
				         Ada95_Extensions | -- Extend95
				         Ada95_Wording | -- DiffWord95
				         Ada2005_Inconsistencies | -- Inconsistent2005
				         Ada2005_Incompatibilities | -- Incompatible2005
				         Ada2005_Extensions | -- Extend2005
				         Ada2005_Wording => -- DiffWord2005
				        return 2; -- Normal indent for annotations.
		        	    when Reason | Ramification | Proof |
					 Imp_Note | Corr_Change | Discussion |
					 Honest | Glossary_Marker | Bare_Annotation =>
				        return 2; -- Normal indent for annotations.
				    when Example_Text =>
					if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
					    return 3; -- Three units.
					else
					    return 1; -- One unit.
					end if;
				    when Child_Example_Text =>
					return 1 + Nested_Indent(I-1); -- Depends on enclosing.
		        	    when Indented_Example_Text =>
				        if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
				            return 6; -- Six units.
				        else
				            return 4; -- Four units.
				        end if;
		        	    when Code_Indented =>
					if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
				            return 4; -- Four units.
				        else
				            return 2; -- Two units.
				        end if;
		        	    when Indent =>
					return 1 + Nested_Indent(I-1); -- Depends on enclosing.
				    when Bulleted | Nested_Bulleted | Nested_X2_Bulleted =>
					return 1 + Nested_Indent(I-1); -- Depends on enclosing.
		        	    when Display =>
					return 1 + Nested_Indent(I-1); -- Depends on enclosing.
		        	    when Syntax_Display =>
					return 1; -- One unit.
		        	    when Enumerated | Nested_Enumerated =>
					return 1 + Nested_Indent(I-1); -- Depends on enclosing.
		        	    when Syntax_Indented =>
					return 1; -- One unit.
		        	    when Syntax_Production =>
					return Nested_Indent(I-1); -- Depends on enclosing.
		        	    when Hanging_Indented =>
		                        if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
					    return 5; -- Five units.
					else
					    return 3; -- Three units.
					end if;
		        	    when Title =>
					return 0; -- No indent.
		        	    when In_Table =>
		                        -- Shouldn't get here.
					return 0; -- No indent.
				end case;
			    end if;
		        end loop;
		        return 0; -- No indent.
		    end Nested_Indent;

		begin
		    return Nested_Indent (Format_State.Nesting_Stack_Ptr);
		end Enclosing_Indent;

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
			Format_Object.Style := ARM_Output.Normal;
			Format_Object.Indent := 0; -- No indent.
			Format_Object.No_Breaks := False;
		    when Wide_Above =>
			if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			    Format_Object.Style  := ARM_Output.Small_Wide_Above;
			    Format_Object.Indent := 2; -- Two units.
		        elsif Format_Object.Last_Paragraph_Subhead_Type = Notes or else
			      Format_Object.Last_Paragraph_Subhead_Type = Single_Note then
			    Format_Object.Style  := ARM_Output.Small_Wide_Above;
			    Format_Object.Indent := 1; -- One unit.
			else
			    Format_Object.Style  := ARM_Output.Wide_Above;
			    Format_Object.Indent := 0; -- No indent.
			end if;
			Format_Object.No_Breaks := False;
		    when Syntax =>
			Format_Object.Style  := ARM_Output.Normal;
			Format_Object.Indent := 1; -- One unit.
			Format_Object.No_Breaks := True;
		    when Notes | Single_Note => -- Notes (only the numbering varies)
			Format_Object.Style  := ARM_Output.Small;
			Format_Object.Indent := 1; -- One unit.
			Format_Object.No_Breaks := False;
		    when Element_Ref | Child_Ref | Usage_Note => -- Similar to an AARM note.
			Format_Object.Style  := ARM_Output.Small;
			Format_Object.Indent := 2; -- Two units.
			Format_Object.No_Breaks := False;
		    when Language_Design | -- "MetaRules"
		         Ada83_Inconsistencies | -- Inconsistent83
		         Ada83_Incompatibilities | -- Incompatible83
		         Ada83_Extensions | -- Extend83
		         Ada83_Wording | -- DiffWord83
		         Ada95_Inconsistencies | -- Inconsistent95
		         Ada95_Incompatibilities | -- Incompatible95
		         Ada95_Extensions | -- Extend95
		         Ada95_Wording | -- DiffWord95
		         Ada2005_Inconsistencies | -- Inconsistent2005
		         Ada2005_Incompatibilities | -- Incompatible2005
		         Ada2005_Extensions | -- Extend2005
		         Ada2005_Wording => -- DiffWord2005
			Format_Object.Style  := ARM_Output.Small;
			Format_Object.Indent := 2; -- Two units.
			Format_Object.No_Breaks := False;
        	    when Reason | Ramification | Proof |
			 Imp_Note | Corr_Change | Discussion |
			 Honest | Glossary_Marker | Bare_Annotation =>
			Format_Object.Style  := ARM_Output.Small;
			Format_Object.Indent := 2; -- Two units.
			Format_Object.No_Breaks := False;

        	    when Example_Text | Child_Example_Text |
			Indented_Example_Text =>
			case Format_Object.Examples_Font is
			    when ARM_Output.Fixed | ARM_Output.Default =>
			        if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			           Format_Object.Style := ARM_Output.Small_Examples;
			        else
			           Format_Object.Style := ARM_Output.Examples;
			        end if;
			    when ARM_Output.Roman =>
			        if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
				   Format_Object.Style := ARM_Output.Small;
			        else
				   Format_Object.Style := ARM_Output.Normal;
			        end if;
			    when ARM_Output.Swiss =>
			        if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
				   Format_Object.Style := ARM_Output.Small_Swiss_Examples;
			        else
				   Format_Object.Style := ARM_Output.Swiss_Examples;
			        end if;
			end case;
			Format_Object.No_Breaks := True;
			if For_Type = Child_Example_Text then
			    Format_Object.Indent := 1 + Enclosing_Indent;
--Ada.Text_IO.Put_Line ("&& Child example paragraph, line " & ARM_Input.Line_String (Input_Object) & " EF=" & Paragraph_Type'Image(Enclosing_Format) & " Indent=" &
--   ARM_Output.Paragraph_Indent_Type'Image(Format_Object.Indent));
			elsif For_Type = Indented_Example_Text then
			    if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			        Format_Object.Indent := 6; -- Fixed indent.
			    elsif Format_Object.Last_Paragraph_Subhead_Type = Notes or else
			          Format_Object.Last_Paragraph_Subhead_Type = Single_Note then
			        Format_Object.Indent := 5; -- Fixed indent.
			    else
			        Format_Object.Indent := 4; -- Fixed indent.
			    end if;
			else
			    if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			        Format_Object.Indent := 3; -- Fixed indent.
			    elsif Format_Object.Last_Paragraph_Subhead_Type = Notes or else
			          Format_Object.Last_Paragraph_Subhead_Type = Single_Note then
			        Format_Object.Indent := 2; -- Fixed indent.
			    else
			        Format_Object.Indent := 1; -- Fixed indent.
			    end if;
			end if;

        	    when Code_Indented =>
			if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			    Format_Object.Style  := ARM_Output.Small;
			    Format_Object.Indent := 4; -- Four units.
		        elsif Format_Object.Last_Paragraph_Subhead_Type = Notes or else
			      Format_Object.Last_Paragraph_Subhead_Type = Single_Note then
			    Format_Object.Style  := ARM_Output.Small;
			    Format_Object.Indent := 3; -- Three units.
			else
			    Format_Object.Style  := ARM_Output.Normal;
			    Format_Object.Indent := 2; -- Two indent.
			end if;
			Format_Object.No_Breaks := False;

        	    when Indent =>
			if Is_Small_Format_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			    Format_Object.Style  := ARM_Output.Small;
			else
			    Format_Object.Style  := ARM_Output.Normal;
			end if;
		        Format_Object.Indent := 1 + Enclosing_Indent;
--Ada.Text_IO.Put_Line ("&& Child Indented paragraph, line " & ARM_Input.Line_String (Input_Object) & " EF=" & Paragraph_Type'Image(Enclosing_Format) & " Indent=" &
--   ARM_Output.Paragraph_Indent_Type'Image(Format_Object.Indent));
			Format_Object.No_Breaks := False;

        	    when Bulleted =>
			if Is_Small_Format_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			    Format_Object.Style  := ARM_Output.Small_Bulleted;
			else
			    Format_Object.Style  := ARM_Output.Bulleted;
			end if;
		        Format_Object.Indent := 1 + Enclosing_Indent;
--Ada.Text_IO.Put_Line ("&& Regular bulleted paragraph, line " & ARM_Input.Line_String (Input_Object) & " EF=" & Paragraph_Type'Image(Enclosing_Format) & " Indent=" &
--   ARM_Output.Paragraph_Indent_Type'Image(Format_Object.Indent));
		        Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;
			Format_Object.No_Breaks := False;

        	    when Nested_Bulleted =>
			if Is_Small_Format_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			    Format_Object.Style  := ARM_Output.Small_Nested_Bulleted;
			else
			    Format_Object.Style  := ARM_Output.Nested_Bulleted;
			end if;
		        Format_Object.Indent := 1 + Enclosing_Indent;
--Ada.Text_IO.Put_Line ("&& Nested bulleted paragraph, line " & ARM_Input.Line_String (Input_Object) & " EF=" & Paragraph_Type'Image(Enclosing_Format) & " Indent=" &
--   ARM_Output.Paragraph_Indent_Type'Image(Format_Object.Indent));
		        Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;
			Format_Object.No_Breaks := False;

        	    when Nested_X2_Bulleted =>
			if Is_Small_Format_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			    Format_Object.Style  := ARM_Output.Small_Nested_Bulleted;
			else
			    Format_Object.Style  := ARM_Output.Nested_Bulleted;
			end if;
		        Format_Object.Indent := 1 + Enclosing_Indent;
--Ada.Text_IO.Put_Line ("&& Nested X2 bulleted paragraph, line " & ARM_Input.Line_String (Input_Object) & " EF=" & Paragraph_Type'Image(Enclosing_Format) & " Indent=" &
--   ARM_Output.Paragraph_Indent_Type'Image(Format_Object.Indent));
		        Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;
			Format_Object.No_Breaks := False;

        	    when Display =>
			if Is_Small_Format_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			    Format_Object.Style  := ARM_Output.Small;
			else
			    Format_Object.Style  := ARM_Output.Normal;
			end if;
		        Format_Object.Indent := 1 + Enclosing_Indent;
--Ada.Text_IO.Put_Line ("&& Display paragraph, line " & ARM_Input.Line_String (Input_Object) & " EF=" & Paragraph_Type'Image(Enclosing_Format) & " Indent=" &
--   ARM_Output.Paragraph_Indent_Type'Image(Format_Object.Indent));
			Format_Object.No_Breaks := True;

        	    when Syntax_Display =>
			Format_Object.Style := ARM_Output.Small;
		        Format_Object.Indent := 1;
			Format_Object.No_Breaks := True;

        	    when Enumerated =>
			if Is_Small_Format_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			    Format_Object.Style  := ARM_Output.Small_Enumerated;
			else
			    Format_Object.Style  := ARM_Output.Enumerated;
			end if;
		        Format_Object.Indent := 1 + Enclosing_Indent;
--Ada.Text_IO.Put_Line ("&& Regular enumerated paragraph, line " & ARM_Input.Line_String (Input_Object) & " EF=" & Paragraph_Type'Image(Enclosing_Format) & " Indent=" &
--   ARM_Output.Paragraph_Indent_Type'Image(Format_Object.Indent));
		        Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;
			Format_Object.No_Breaks := False;

        	    when Nested_Enumerated =>
			if Is_Small_Format_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			    Format_Object.Style  := ARM_Output.Small_Enumerated;
			else
			    Format_Object.Style  := ARM_Output.Enumerated;
			end if;
		        Format_Object.Indent := 1 + Enclosing_Indent;
--Ada.Text_IO.Put_Line ("&& Nested enumerated paragraph, line " & ARM_Input.Line_String (Input_Object) & " EF=" & Paragraph_Type'Image(Enclosing_Format) & " Indent=" &
--   ARM_Output.Paragraph_Indent_Type'Image(Format_Object.Indent));
			-- Note: The difference here is the numbering, not the
			-- layout.
		        Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;
			Format_Object.No_Breaks := False;

        	    when Syntax_Indented =>
			Format_Object.Style := ARM_Output.Normal;
			Format_Object.Indent := 1; -- One unit.
			Format_Object.No_Breaks := False;
        	    when Syntax_Production =>
			null; -- Leave format alone (but line-breaks are preserved).
			Format_Object.No_Breaks := True;

        	    when Hanging_Indented =>
                        if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
        		    if Enclosing_Format = Code_Indented or else
        		       Enclosing_Format = Indent or else
        		       Enclosing_Format = Hanging_Indented or else
                               Enclosing_Format = Syntax_Indented or else
			       Enclosing_Format = Syntax then
			        Format_Object.Style  := ARM_Output.Small_Narrow_Hanging;
        		    elsif Enclosing_Format = Bulleted or else
        		          Enclosing_Format = Enumerated then
			        Format_Object.Style  := ARM_Output.Small_Hanging_in_Bulleted;
			    else
			        Format_Object.Style  := ARM_Output.Small_Wide_Hanging;
			    end if;
			    Format_Object.Indent := 5; -- Five units.
		        elsif Format_Object.Last_Paragraph_Subhead_Type = Notes or else
			      Format_Object.Last_Paragraph_Subhead_Type = Single_Note then -- Notes:
        		    if Enclosing_Format = Code_Indented or else
        		       Enclosing_Format = Indent or else
        		       Enclosing_Format = Hanging_Indented or else
                               Enclosing_Format = Syntax_Indented or else
			       Enclosing_Format = Syntax then
			        Format_Object.Style  := ARM_Output.Small_Narrow_Hanging;
        		    elsif Enclosing_Format = Bulleted or else
        		          Enclosing_Format = Enumerated then
			        Format_Object.Style  := ARM_Output.Small_Hanging_in_Bulleted;
			    else
			        Format_Object.Style  := ARM_Output.Small_Wide_Hanging;
			    end if;
			    Format_Object.Indent := 4; -- Four units.
			else -- Normal:
        		    if Enclosing_Format = Code_Indented or else
        		       Enclosing_Format = Indent or else
        		       Enclosing_Format = Hanging_Indented or else
                               Enclosing_Format = Syntax_Indented or else
			       Enclosing_Format = Syntax then
			        Format_Object.Style  := ARM_Output.Narrow_Hanging;
        		    elsif Enclosing_Format = Bulleted or else
        		          Enclosing_Format = Enumerated then
			        Format_Object.Style  := ARM_Output.Hanging_in_Bulleted;
			    else
			        Format_Object.Style  := ARM_Output.Wide_Hanging;
			    end if;
			    Format_Object.Indent := 3; -- Three units.
			end if;
		        Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;
			Format_Object.No_Breaks := False;
        	    when Title =>
		        Format_Object.Style := ARM_Output.Title;
			Format_Object.Indent := 0; -- No indent.
			Format_Object.No_Breaks := False;
        	    when In_Table =>
                        -- Shouldn't get here.
			if Is_AARM_Paragraph (Format_Object.Last_Paragraph_Subhead_Type) then
			    Format_Object.Style := ARM_Output.Small;
			else
			    Format_Object.Style := ARM_Output.Normal;
			end if;
			Format_Object.Indent := 0; -- No indent.
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
			ARM_Output.Category_Header (Output_Object, Data.Paragraph_Kind_Title(For_Type).Str(1..Data.Paragraph_Kind_Title(For_Type).Length));
			Format_Object.Last_Paragraph_Subhead_Type := For_Type;
		    when Notes | Single_Note => -- Notes
			if not Format_Object.Use_ISO_2004_Note_Format then
			    -- The Notes header looks different from the others.
		            ARM_Output.Start_Paragraph (Output_Object,
					                Style  => ARM_Output.Small_Header,
					                Indent => 1,
							Number => "",
						        No_Breaks => True,
						        Keep_with_Next => True);
			    ARM_Output.Ordinary_Text (Output_Object, Data.Paragraph_Kind_Title(For_Type).Str(1..Data.Paragraph_Kind_Title(For_Type).Length));
			    ARM_Output.End_Paragraph (Output_Object);
			    Format_Object.Last_Paragraph_Subhead_Type := For_Type;
			else
			    null; -- No subheader. We don't change the last
			        -- subheader generated, either.
			end if;
		    when Language_Design | -- "MetaRules"
		         Ada83_Inconsistencies | -- Inconsistent83
		         Ada83_Incompatibilities | -- Incompatible83
		         Ada83_Extensions | -- Extend83
		         Ada83_Wording | -- DiffWord83
		         Ada95_Inconsistencies | -- Inconsistent95
		         Ada95_Incompatibilities | -- Incompatible95
		         Ada95_Extensions | -- Extend95
		         Ada95_Wording | -- DiffWord95
		         Ada2005_Inconsistencies | -- Inconsistent2005
		         Ada2005_Incompatibilities | -- Incompatible2005
		         Ada2005_Extensions | -- Extend2005
		         Ada2005_Wording => -- DiffWord2005
			ARM_Output.Category_Header (Output_Object, Paragraph_Kind_Title(For_Type).Str(1..Paragraph_Kind_Title(For_Type).Length));
			Format_Object.Last_Paragraph_Subhead_Type := For_Type;
        	    when Plain | Introduction | Element_Ref | Child_Ref | Usage_Note =>
			null; -- No subheader. We don't change the last
			    -- subheader generated, either.
        	    when Reason | Ramification | Proof |
			 Imp_Note | Corr_Change | Discussion |
			 Honest | Glossary_Marker | Bare_Annotation |
			 Wide_Above | Example_Text | Child_Example_Text |
			 Indented_Example_Text | Code_Indented | Indent |
			 Bulleted | Nested_Bulleted | Nested_X2_Bulleted |
			 Display |
			 Syntax_Display | Syntax_Indented | Syntax_Production |
			 Hanging_Indented | Title |
			 Enumerated | Nested_Enumerated |
			 In_Table =>
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
			 Notes | Single_Note |
		         Language_Design | -- "MetaRules"
		         Ada83_Inconsistencies | -- Inconsistent83
		         Ada83_Incompatibilities | -- Incompatible83
		         Ada83_Extensions | -- Extend83
		         Ada83_Wording | -- DiffWord83
		         Ada95_Inconsistencies | -- Inconsistent95
		         Ada95_Incompatibilities | -- Incompatible95
		         Ada95_Extensions | -- Extend95
		         Ada95_Wording | -- DiffWord95
		         Ada2005_Inconsistencies | -- Inconsistent2005
		         Ada2005_Incompatibilities | -- Incompatible2005
		         Ada2005_Extensions | -- Extend2005
		         Ada2005_Wording => -- DiffWord2005
			null; -- Not an annotation.
        	    when Reason | Ramification | Proof |
			 Imp_Note | Corr_Change | Discussion |
			 Honest | Glossary_Marker |
        	         Element_Ref | Child_Ref | Usage_Note =>
			declare
			    Format_Bold : ARM_Output.Format_Type :=
				Format_Object.Text_Format;
			begin
			    Format_Bold.Bold := True;
		            ARM_Output.Text_Format (Output_Object,
						    Format => Format_Bold);
			end;
		        ARM_Output.Ordinary_Text (Output_Object,
			     Text => Paragraph_Kind_Title(For_Type).Str(
					1..Paragraph_Kind_Title(For_Type).Length));
		        ARM_Output.Text_Format (Output_Object,
						Format => Format_Object.Text_Format);
			Format_Object.Last_Paragraph_Subhead_Type := For_Type;
		    when Bare_Annotation =>
			null; -- Header (if any) is generated elsewhere.
		    when Wide_Above |
			 Example_Text | Child_Example_Text | Indented_Example_Text |
			 Code_Indented | Indent |
			 Bulleted | Nested_Bulleted | Nested_X2_Bulleted |
			 Display | Syntax_Display |
			 Syntax_Indented | Syntax_Production |
			 Hanging_Indented | Title |
			 Enumerated | Nested_Enumerated |
			 In_Table =>
			null; -- Just a format.
		end case;
	    end Make_Annotation_Preface;


	    function Show_Leading_Text_for_Paragraph return Boolean is
		-- Returns True if the leading text (note number,
		-- annotation preface, etc.) should be shown for this paragraph.
		-- We assume that the current paragraph has a version less than
		-- or equal to the one that we're displaying.
		-- ** Note: This is not quite right. If this
		-- paragraph is deleted, but a following one needs the
		-- leading item, this will still lead the item out.
		-- We *do* have enough information for that, at least in the
		-- case of annotations: they can be marked as deleted, in
		-- which case we don't need them here. *But* that information
		-- doesn't get here in the case that we're not showing deletions.
		-- I can't think of a fix right now, and the note numbers need
		-- fixing ASAP.
	    begin
--Ada.Text_IO.Put_Line ("Show_Leading_Text, para kind: " &
--ARM_Database.Paragraph_Change_Kind_Type'Image(Format_Object.Next_Paragraph_Change_Kind) &
--"; version=" & Format_Object.Next_Paragraph_Version & "; on line " & Arm_Input.Line_String(Input_Object));
		if (ARM_Database."/=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted) and then
		    ARM_Database."/=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted_Inserted_Number) and then
		    ARM_Database."/=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted_No_Delete_Message) and then
		    ARM_Database."/=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted_Inserted_Number_No_Delete_Message)) then
		    -- Not a deleted paragraph.
--Ada.Text_IO.Put_Line ("%% True - Not deleted");
		    return True;
		end if;
	        case Format_Object.Changes is
		    when ARM_Format.Old_Only =>
		        -- Display only the original version ('0').
			if ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted_Inserted_Number) or else
			   ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted_Inserted_Number_No_Delete_Message) then
--Ada.Text_IO.Put_Line ("%% True - Not original");
			    return False; -- Not in the original document.
			else
--Ada.Text_IO.Put_Line ("%% True - Original");
			    return True; -- Probably in the original document.
				-- (If the paragraph numbers were "new" in a
				-- later version, we couldn't tell.)
			end if;
		    when ARM_Format.New_Only =>
		        -- Display only the version
		        -- Format_Object.Change_Version, no insertions or deletions.
--Ada.Text_IO.Put_Line ("%% False - New only");
			return False; -- This is always deleted.
		    when ARM_Format.Changes_Only =>
		        -- Display only the the changes for version
		        -- Format_Object.Change_Version, older changes
		        -- are applied and newer changes are ignored.
		        if Format_Object.Next_Paragraph_Version < Format_Object.Change_Version then
			    -- Change version is older than we're displaying;
			    -- no text will be shown.
--Ada.Text_IO.Put_Line ("%% False - changes only, old version");
			    return False; -- This is always deleted.
		        else
			    -- The correct version.
--Ada.Text_IO.Put_Line ("%% True - changes only, current version");
			    return True; -- Show the item, as the old text
					 -- will be shown as deleted.
		        end if;
		    when ARM_Format.Show_Changes |
		         ARM_Format.New_Changes =>
		        -- Display all of the changes up to version
		        -- Format_Object.Change_Version, newer changes are
		        -- ignored. (New_Changes shows deletions as a single
		        -- character for older versions of Word, but otherwise
		        -- is the same.)
--Ada.Text_IO.Put_Line ("%% True - show changes");
		        return True; -- Show the item, as the old text
				     -- will be shown as deleted.
	        end case;
	    end Show_Leading_Text_for_Paragraph;

	begin
	    if not Format_Object.In_Paragraph then
		-- Output subheader, if needed.
		if Format_Object.Next_Paragraph_Subhead_Type /=
		   Format_Object.Last_Paragraph_Subhead_Type then
		    if (ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted_No_Delete_Message) or else
		        ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted_Inserted_Number_No_Delete_Message)) and then
		       ARM_Format."=" (Format_Object.Changes, ARM_Format.New_Only) then
			-- Nothing at all should be showm.
			null;
if Format_Object.Next_Paragraph_Subhead_Type /= Plain or else
   Format_Object.Next_Paragraph_Subhead_Type /= Introduction then
   Ada.Text_IO.Put_Line("    -- No subhead (DelNoMsg); on line " & Arm_Input.Line_String(Input_Object));
end if;
		    elsif ((not Format_Object.Number_Paragraphs) or else
		          Format_Object.No_Para_Num) and then
		       (ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted) or else
		        ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted_Inserted_Number)) and then
		       ARM_Format."=" (Format_Object.Changes, ARM_Format.New_Only) then
			-- Nothing at all should be showm.
			null;
if Format_Object.Next_Paragraph_Subhead_Type /= Plain or else
   Format_Object.Next_Paragraph_Subhead_Type /= Introduction then
   Ada.Text_IO.Put_Line("    -- No subhead (Del-no paranum); on line " & Arm_Input.Line_String(Input_Object));
end if;
		    else
		        Make_Subhead (Format_Object.Next_Paragraph_Subhead_Type);
		    end if;
		end if;

		-- Set the paragraph format:
		Set_Format (Format_Object.Next_Paragraph_Format_Type);

		if Format_Object.Number_Paragraphs and then
		   not Format_Object.No_Para_Num then

		    -- Format the paragraph number:
		    Paragraph_Number_String (Update_Numbers => True);

--Ada.Text_IO.Put_Line ("Check_Paragraph, make number " &
--Format_Object.Current_Paragraph_String (1 .. Format_Object.Current_Paragraph_Len) &
--": format= " & Paragraph_Type'Image(Format_Object.Next_Paragraph_Format_Type));
		    -- ...and start the paragraph:
		    if (ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted_No_Delete_Message) or else
		        ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted_Inserted_Number_No_Delete_Message)) and then
		       ARM_Format."=" (Format_Object.Changes, ARM_Format.New_Only) then
			-- Nothing at all should be showm.
			-- ** Warning ** If we lie here, the program will crash!
		        Format_Object.No_Start_Paragraph := True;
Ada.Text_IO.Put_Line("    -- No Start Paragraph (DelNoMsg)");
		    else
		        ARM_Output.Start_Paragraph (Output_Object,
					            Style     => Format_Object.Style,
					            Indent    => Format_Object.Indent,
					            Number    => Format_Object.Current_Paragraph_String (1 .. Format_Object.Current_Paragraph_Len),
					            No_Prefix => Format_Object.No_Prefix,
					            Tab_Stops => Format_Object.Paragraph_Tab_Stops,
					            No_Breaks => Format_Object.No_Breaks or Format_Object.In_Bundle,
					            Keep_with_Next => Format_Object.Keep_with_Next or Format_Object.In_Bundle,
					            Space_After => Format_Object.Space_After);
		        Format_Object.No_Start_Paragraph := False;
		    end if;
		    if ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted) or else
		       ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted_Inserted_Number) then
			-- If needed, make the "deleted text" message.
			-- We trust that Next_Paragraph_Change_Kind is not
			-- set to Deleted if the version number of the change
			-- is beyond the document generation version.
			-- (Note that Current_Change_Version may not be set,
			-- so it is not safe to test here.)
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
				if ARM_Format."=" (Format_Object.Changes, ARM_Format.New_Only) and then
				    (Format_Object.Next_Paragraph_Format_Type = Enumerated or else
				     Format_Object.Next_Paragraph_Format_Type = Nested_Enumerated or else
				     Format_Object.Next_Paragraph_Format_Type = Hanging_Indented) then
				    -- We're in a hanging style, we need to end hanging first.
				    -- Nothing else will be displayed; if we didn't end the hang this
				    -- would end up on the same line as the next paragraph.
				    -- It's possible that we could have a problem with
				    -- hanging in one of the other cases if no text will be
				    -- displayed, but there is no way to know that here.
				    ARM_Output.End_Hang_Item (Output_Object);
				end if;
			        ARM_Output.Text_Format (Output_Object,
				    Format => (Bold => Format_Object.Text_Format.Bold,
					       Italic => True,
					       Font => Format_Object.Text_Format.Font,
					       Color => Format_Object.Text_Format.Color,
					       Change => ARM_Output.None, -- Never mark this as changed!!
					       Version => '0', Added_Version => '0', -- Not used.
					       Size => ARM_Output."-"(Format_Object.Text_Format.Size, 1),
					       Location => Format_Object.Text_Format.Location));
			        ARM_Output.Ordinary_Text (Output_Object,
				     Text => "This paragraph was deleted.");
			        ARM_Output.Text_Format (Output_Object, -- Restore the format.
					    Format => Format_Object.Text_Format);
			    when ARM_Format.Old_Only => null; -- Not deleted.
		        end case;
		    end if;
		    Format_Object.In_Paragraph := True;
		    Format_Object.Last_Non_Space := False;

		else -- No paragraph numbers (or if the paragraph
		     -- number has been suppressed with @NoParaNum):

--Ada.Text_IO.Put_Line ("Check_Paragraph, no number: format= " & Paragraph_Type'Image(Format_Object.Next_Paragraph_Format_Type) &
--   " output style= " & ARM_Output.Paragraph_Style_Type'Image(Format_Object.Style));
		    -- Start the paragraph:
		    if (ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted_No_Delete_Message) or else
		        ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted_Inserted_Number_No_Delete_Message) or else
			ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted) or else
		        ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind, ARM_Database.Deleted_Inserted_Number)) and then
		       ARM_Format."=" (Format_Object.Changes, ARM_Format.New_Only) then
			-- Nothing at all should be showm.
			-- ** Warning ** If we lie here, the program will crash!
		        Format_Object.No_Start_Paragraph := True;
Ada.Text_IO.Put_Line("    -- No Start Paragraph (Del-NewOnly)");
		    else
		        ARM_Output.Start_Paragraph (Output_Object,
				                    Style     => Format_Object.Style,
				                    Indent    => Format_Object.Indent,
						    Number    => "",
						    No_Prefix => Format_Object.No_Prefix,
						    Tab_Stops => Format_Object.Paragraph_Tab_Stops,
						    No_Breaks => Format_Object.No_Breaks or Format_Object.In_Bundle,
						    Keep_with_Next => Format_Object.Keep_with_Next or Format_Object.In_Bundle,
						    Space_After => Format_Object.Space_After);
		        Format_Object.In_Paragraph := True;
		        Format_Object.No_Start_Paragraph := False;
		        Format_Object.Current_Paragraph_Len := 0; -- Empty paragraph number.
		        Format_Object.No_Para_Num := False;
		    end if;
	        end if;

		if not Format_Object.No_Prefix then
		    if Format_Object.Next_Paragraph_Format_Type = Notes and then
		       Show_Leading_Text_for_Paragraph then
			if not Format_Object.Use_ISO_2004_Note_Format then
			    -- Output the note number (Ada95 format).
		            declare
		                NNum : constant String := Integer'Image(Format_Object.Next_Note);
		            begin
		                ARM_Output.Ordinary_Text (Output_Object,
					                  NNum(2..NNum'Last));
		                ARM_Output.Hard_Space (Output_Object);
		                ARM_Output.Hard_Space (Output_Object);
		                Format_Object.Next_Note := Format_Object.Next_Note + 1;
			    end;
			else
			    -- Output the note header (ISO 2004 format).
		            declare
		                NNum : constant String := Integer'Image(Format_Object.Next_Note);
		            begin
		                ARM_Output.Ordinary_Text (Output_Object,
					                  "NOTE " & NNum(2..NNum'Last));
		                ARM_Output.Hard_Space (Output_Object);
		                ARM_Output.Hard_Space (Output_Object);
		                ARM_Output.Hard_Space (Output_Object);
		                Format_Object.Next_Note := Format_Object.Next_Note + 1;
			    end;
			end if;
		    elsif Format_Object.Next_Paragraph_Format_Type = Single_Note and then
		       Show_Leading_Text_for_Paragraph then
			if not Format_Object.Use_ISO_2004_Note_Format then
			    -- No note number, and nothing else needed.
			    null;
			else
			    -- Output the note header (ISO 2004 format)
			    -- without a number.
		            ARM_Output.Ordinary_Text (Output_Object,
					              "NOTE");
		            ARM_Output.Hard_Space (Output_Object);
		            ARM_Output.Hard_Space (Output_Object);
		            ARM_Output.Hard_Space (Output_Object);
			end if;
		    elsif (Format_Object.Next_Paragraph_Format_Type = Enumerated or else
		        Format_Object.Next_Paragraph_Format_Type = Nested_Enumerated) and then
		       Show_Leading_Text_for_Paragraph then
		        -- Output the item number.
			if Format_Object.Use_ISO_2004_Note_Format then
			    if Format_Object.Enumerated_Level <= 1 then -- Outer list.
				-- Lower case letters for list:
		                ARM_Output.Ordinary_Text (Output_Object,
				   Character'Val ((Format_Object.Next_Enumerated_Num - 1) +
					Character'Pos ('a'))
					                   & ')');
		                ARM_Output.End_Hang_Item (Output_Object);
		                Format_Object.Next_Enumerated_Num := Format_Object.Next_Enumerated_Num + 1;
			    else -- numbered.
		                declare
		                    NNum : constant String := Integer'Image(Format_Object.Next_Enumerated_Num);
		                begin
		                    ARM_Output.Ordinary_Text (Output_Object,
					                      NNum(2..NNum'Last) & ')');
		                    ARM_Output.End_Hang_Item (Output_Object);
		                    Format_Object.Next_Enumerated_Num := Format_Object.Next_Enumerated_Num + 1;
		                end;
			    end if;
			else -- Ada 95 lists.
		            declare
		                NNum : constant String := Integer'Image(Format_Object.Next_Enumerated_Num);
		            begin
		                ARM_Output.Ordinary_Text (Output_Object,
					                  NNum(2..NNum'Last) & '.');
		                ARM_Output.End_Hang_Item (Output_Object);
		                Format_Object.Next_Enumerated_Num := Format_Object.Next_Enumerated_Num + 1;
		            end;
			end if;
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
		    if Show_Leading_Text_for_Paragraph then
		        Make_Annotation_Preface (Format_Object.Next_Paragraph_Subhead_Type);
		    end if;
		end if;

		if Format_Object.References /= null then
		    -- We assume these are only stored here if we want to see
		    -- them on *this* paragraph. Thus, we just output them if
		    -- they exist here. Note: This deallocates the references
		    -- after writing them.
		    if Format_Object.No_Start_Paragraph then
			-- Oh-oh! Can't generate references; there aren't
			-- supposed to be any at this point.
		        Ada.Text_IO.Put_Line ("** References generated for no display paragraph; line " & ARM_Input.Line_String (Input_Object));
		    else
			Make_References (Format_Object.References, Format_Object, Output_Object);
		    end if;
		end if;

		-- Reset the "next" paragraph kind and version (we have to
		-- wait, so that we can use this to determine whether
		-- note numbers and things are output):
	        Format_Object.Next_Paragraph_Change_Kind := ARM_Database.None;
	        Format_Object.Next_Paragraph_Version := '0';
	    -- else already in a paragraph.
	    end if;
	end Check_Paragraph;


	procedure Check_End_Paragraph is
	    -- Check for the end of a paragraph; closing it if necessary.
	    -- We will never be in a paragraph after this routine.
	begin
	    if Format_Object.In_Paragraph then
		if not Format_Object.No_Start_Paragraph then
		    ARM_Output.End_Paragraph (Output_Object);
--else Ada.Text_IO.Put_Line("No Start Paragraph, so no End Paragraph");
		end if;
	        Format_Object.In_Paragraph := False;
	        Format_Object.No_Start_Paragraph := False;
	        Format_Object.No_Para_Num := False;
			-- Make sure any "leftover"
			-- NoParaNums are cleared; we don't want this lasting into
			-- the next paragraph.
		if Format_Object.In_Change then
		    Ada.Text_IO.Put_Line ("** Paragraph end while in change; line " & ARM_Input.Line_String (Input_Object));
		    Format_Object.In_Change := False;
		end if;
		-- Check command stack for any open formatting commands,
		-- and complain (these won't be closed properly and chaos
		-- may result):
		for I in reverse 1 .. Format_State.Nesting_Stack_Ptr loop
		    if Format_State.Nesting_Stack (I).Command in Bold .. Tab_Set then
			-- There is a formatting command active.
			-- (Note: not all of these can be on the stack.)
		        Ada.Text_IO.Put_Line ("** Paragraph end while in formatting command " &
			    Data.Command_Type'Image(Format_State.Nesting_Stack (I).Command) &
			    "; line " & ARM_Input.Line_String (Input_Object));
			exit;
		    end if;
		end loop;
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
		       Format => (Bold => False, Italic => False,
		                  Font => ARM_Output.Default,
				  Color => ARM_Output.Default,
				  Size => -1,
			          Change => Format_Object.Text_Format.Change,
				  Version => Format_Object.Text_Format.Version,
				  Added_Version => Format_Object.Text_Format.Added_Version,
		       		  Location => ARM_Output.Normal));
		end if;
	        ARM_Output.Ordinary_Character (Output_Object, '{');
		if not Is_AARM then
	            ARM_Output.Text_Format (Output_Object,
		       Format => (Bold => False, Italic => True,
		                  Font => ARM_Output.Default,
				  Color => ARM_Output.Default,
				  Size => -1,
			          Change => Format_Object.Text_Format.Change,
				  Version => Format_Object.Text_Format.Version,
				  Added_Version => Format_Object.Text_Format.Added_Version,
		       		  Location => ARM_Output.Normal));
		else
	            ARM_Output.Text_Format (Output_Object,
		       Format => (Bold => False, Italic => True,
		                  Font => ARM_Output.Default,
				  Color => ARM_Output.Default,
				  Size => 0,
			          Change => Format_Object.Text_Format.Change,
				  Version => Format_Object.Text_Format.Version,
				  Added_Version => Format_Object.Text_Format.Added_Version,
		       		  Location => ARM_Output.Normal));
		end if;
	        ARM_Output.Ordinary_Text (Output_Object, ARM_Index.Clean(Term_Text, Remove_Soft_Hyphens => True));
		if not Is_AARM then
	            ARM_Output.Text_Format (Output_Object,
		       Format => (Bold => False, Italic => False,
		                  Font => ARM_Output.Default,
				  Color => ARM_Output.Default,
				  Size => -1,
			          Change => Format_Object.Text_Format.Change,
				  Version => Format_Object.Text_Format.Version,
				  Added_Version => Format_Object.Text_Format.Added_Version,
		       		  Location => ARM_Output.Normal));
		else
	            ARM_Output.Text_Format (Output_Object,
		       Format => (Bold => False, Italic => False,
		                  Font => ARM_Output.Default,
				  Color => ARM_Output.Default,
				  Size => 0,
			          Change => Format_Object.Text_Format.Change,
				  Version => Format_Object.Text_Format.Version,
				  Added_Version => Format_Object.Text_Format.Added_Version,
		       		  Location => ARM_Output.Normal));
		end if;
		case Special is
		    when None => null;
		    when Is_Root => ARM_Output.Ordinary_Text (Output_Object, " [distributed]");
		    when Is_Partial => ARM_Output.Ordinary_Text (Output_Object, " [partial]");
		end case;
	        ARM_Output.Ordinary_Character (Output_Object, '}');
		if not Is_AARM then
	            ARM_Output.Text_Format (Output_Object,
		       Format => (Bold => False, Italic => False,
		                  Font => ARM_Output.Default,
				  Color => ARM_Output.Default,
				  Size => 0,
			          Change => Format_Object.Text_Format.Change,
				  Version => Format_Object.Text_Format.Version,
				  Added_Version => Format_Object.Text_Format.Added_Version,
		       		  Location => ARM_Output.Normal));
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
			" in [" & Stops & "] from line " &
		        ARM_Input.Line_String (Input_Object));
		    exit; -- Give up on this tabset.
		elsif Tabs.Number < 1 and then
			Tabs.Stops(Tabs.Number-1).Stop >=
			Tabs.Stops(Tabs.Number).Stop then
		    Tabs.Number := Tabs.Number - 1;
		    Ada.Text_IO.Put_Line ("  ** Bad tab stop, less than previous, at position" & Natural'Image(Loc) &
			" in [" & Stops & "] from line " &
		        ARM_Input.Line_String (Input_Object));
		    exit; -- Give up on this tabset.
		end if;
		if Loc > Stops'Length then
		    exit; -- Finished.
		elsif Stops(Loc) = ',' then
		    Loc := Loc + 1;
		    if Loc > Stops'Length then
		        Ada.Text_IO.Put_Line ("  ** Bad tab stop set format, ends with comma in [" &
			    Stops & "] from line " &
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


        procedure Write_Subindex (
		            Subindex_Object : in out ARM_Subindex.Subindex_Type;
		            Format_Object : in out Format_Type;
		            Output_Object : in out ARM_Output.Output_Type'Class;
			    Minimize_Lines : in Boolean) is
	    -- Writes a subindex for the document.
        begin
	    Check_End_Paragraph;

	    -- Insert a blank paragraph:
            ARM_Output.Start_Paragraph (Output_Object, ARM_Output.Normal,
					Indent => 0, Number => "");
	    ARM_Output.Hard_Space (Output_Object);
            ARM_Output.End_Paragraph (Output_Object);

	    ARM_Output.Set_Columns (Output_Object, Number_of_Columns => 2);

	    ARM_Subindex.Write_Subindex (
		    Subindex_Object,
		    Output_Object,
		    Use_Paragraphs => Format_Object.Number_Paragraphs,
		    Minimize_Lines => Minimize_Lines);

	    ARM_Output.Set_Columns (Output_Object, Number_of_Columns => 1);

	    -- Not in a paragraph here, either.
        end Write_Subindex;


        procedure Simple_Subindex_Item (
	            Subindex_Object : in out ARM_Subindex.Subindex_Type;
	            Format_Object : in out Format_Type;
	            Output_Object : in out ARM_Output.Output_Type'Class;
		    Entity_Kind_Name : in String) is
	    -- Create a simple subindex item; the command has a single
	    -- parameter <defn>.
	    -- Create an "In_Unit" entry for the item;
	    -- Also create two regular index entries:
	    --    One for <defn> with a secondary entry of "@i{in} <Unit>"
	    --    (where Unit is the unit saved by a previous RootLibUnit or
	    --    ChildUnit.),
	    -- and a Second (only for version < 2 and if the entity name is
	    --    non-null) for
	    --    "Language-Defined <Entity>" with a
	    --     secondary entry of "<defn> @i{in} <Unit>".
	    -- Also outputs the <defn> parameter to the output file.
	    Entity : String(1..80);
	    Len : Natural := 0;
	    Key : ARM_Index.Index_Key := ARM_Index.Get_Key;
	    Disposition : ARM_Output.Change_Type;
	    use type ARM_Output.Change_Type;
        begin
	    ARM_Input.Copy_to_String_until_Close_Char (
	        Input_Object,
	        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char,
	        Entity,
	        Len);

	    -- Determine what to do with the "Language-Defined" entry:
	    Calc_Change_Disposition (
	        Format_Object => Format_Object,
		Version => '2',
	        Operation => ARM_Output.Deletion,
	        Text_Kind => Disposition);
	    if Entity_Kind_Name'Length = 0 or else
               Disposition = Do_Not_Display_Text then
	        null; -- Ignore this.
	    elsif Disposition = ARM_Output.None then
	        -- Normal reference:
	        ARM_Index.Add_Reusing_Key (
		    Term => "Language-Defined " & Entity_Kind_Name,
		    Subterm => Entity(1..Len) & " in " &
			      Format_Object.Unit(1..Format_Object.Unit_Len),
	            Kind => ARM_Index.SubDeclaration_in_Package,
		    Clause => Clause_String (Format_Object),
		    Paragraph => Paragraph_String,
		    Key => Key);
		    -- Note that the Subdeclaration type changes the
		    -- "in" into italics.
	    elsif Disposition = ARM_Output.Deletion then
	        null; -- Ignore this (no change info in the index).
	    else -- Insertion.
	        raise Program_Error; -- An insertion inside of a deletion command!
	    end if;

	    Check_Paragraph;
	    ARM_Output.Index_Target (Output_Object, Key);

	    if Format_Object.Unit_Len = 0 then
	        Ada.Text_IO.Put_Line ("** No unit defined for index entry expecting one on line " & ARM_Input.Line_String (Input_Object));

	        ARM_Index.Add_Reusing_Key (
	            Term => Entity(1..Len),
	            Subterm => "*unknown*",
	            Kind => ARM_Index.Declaration_in_Package,
	            Clause => Clause_String (Format_Object),
	            Paragraph => Paragraph_String,
	            Key => Key);

	        ARM_Subindex.Insert (
		    Subindex_Object => Subindex_Object,
		    Entity => Entity(1..Len),
	            From_Unit => "*unknown*",
		    Kind => ARM_Subindex.In_Unit,
		    Clause => Clause_String (Format_Object),
		    Paragraph => Paragraph_String,
		    Key => Key);
	    else
	        ARM_Index.Add_Reusing_Key (
	            Term => Entity(1..Len),
	            Subterm => Format_Object.Unit(1..Format_Object.Unit_Len),
	            Kind => ARM_Index.Declaration_in_Package,
	            Clause => Clause_String (Format_Object),
	            Paragraph => Paragraph_String,
	            Key => Key);

	        ARM_Subindex.Insert (
		    Subindex_Object => Subindex_Object,
		    Entity => Entity(1..Len),
		    From_Unit => Format_Object.Unit(1..Format_Object.Unit_Len),
		    Kind => ARM_Subindex.In_Unit,
		    Clause => Clause_String (Format_Object),
		    Paragraph => Paragraph_String,
		    Key => Key);
	    end if;

	    ARM_Output.Ordinary_Text (Output_Object, Entity(1..Len));
	    Format_Object.Last_Non_Space := True;
	end Simple_Subindex_Item;


        procedure Child_Unit (
	            Subindex_Object : in out ARM_Subindex.Subindex_Type;
	            Format_Object : in out Format_Type;
	            Output_Object : in out ARM_Output.Output_Type'Class) is
	    -- Generates three index entries: An index entry for <child>, with
	    -- a secondary of "@i{child of} <parent>", an index entry for
	    -- "Language-Defined Library Units" with a secondary entry of
	    -- <parent>.<child>, and an index entry for <parent>.<child>. The
	    -- Unit is set to <parent>.<child>. (For version 2 or later, the
	    -- Language-Defined entry is not generated.) The first entry is
	    -- added to the subindex list as well.
	    Close_Ch : Character;
	    Parent, Child : String(1..80);
	    PLen, CLen : Natural := 0;
	    Key : ARM_Index.Index_Key := ARM_Index.Get_Key;
            Disposition : ARM_Output.Change_Type;
            use type ARM_Output.Change_Type;
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

	    ARM_Index.Add_Reusing_Key (
		    Term => Child(1..CLen),
		    Subterm => Parent(1..PLen),
		    Kind => ARM_Index.Child_Unit_Parent,
		    Clause => Clause_String (Format_Object),
		    Paragraph => Paragraph_String,
		    Key => Key);

	    Check_Paragraph;
	    ARM_Output.Index_Target (Output_Object, Key);

	    -- Determine what to do with the "Language-Defined" entry:
	    Calc_Change_Disposition (
	        Format_Object => Format_Object,
	        Version => '2',
	        Operation => ARM_Output.Deletion,
	        Text_Kind => Disposition);
	    if Disposition = Do_Not_Display_Text then
	        null; -- Ignore this.
	    elsif Disposition = ARM_Output.None then
	        -- Make reference:
	        ARM_Index.Add_Reusing_Key (
		    Term => "Language-Defined Library Units",
		    Subterm => Parent(1..PLen) & '.' & Child(1..CLen),
		    Kind => ARM_Index.Primary_Term_and_Subterm,
		    Clause => Clause_String (Format_Object),
		    Paragraph => Paragraph_String,
		    Key => Key);
	    elsif Disposition = ARM_Output.Deletion then
	        null; -- Ignore this (no change info in the index).
	    else -- Insertion.
	        raise Program_Error; -- An insertion inside of a deletion command!
    	    end if;

	    ARM_Index.Add_Reusing_Key (
		    Term => Parent(1..PLen) & '.' & Child(1..CLen),
		    Kind => ARM_Index.Primary_Term,
		    Clause => Clause_String (Format_Object),
		    Paragraph => Paragraph_String,
		    Key => Key);

	    ARM_Subindex.Insert (
		    Subindex_Object => Subindex_Object,
		    Entity => Child(1..CLen),
		    From_Unit => Parent(1..PLen),
		    Kind => ARM_Subindex.Child_of_Parent,
		    Clause => Clause_String (Format_Object),
		    Paragraph => Paragraph_String,
		    Key => Key);

	    -- Leave the command end marker, let normal processing
	    -- get rid of it.
        end Child_Unit;


	procedure Process_Begin is
	    -- Process a "begin". The "begin" has been stacked.

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
	    	= "wideabove" then
		Format_Object.Next_Paragraph_Format_Type := Wide_Above;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "example" then
		Format_Object.Next_Paragraph_Format_Type := Example_Text;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "childexample" then
		Format_Object.Next_Paragraph_Format_Type := Child_Example_Text;
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
	    	= "indent" then
		Format_Object.Next_Paragraph_Format_Type := Indent;
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
	    	= "innerinneritemize" then
		Format_Object.Next_Paragraph_Format_Type := Nested_X2_Bulleted;
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
		Format_Object.Enumerated_Level := Format_Object.Enumerated_Level + 1;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "innerenumerate" then
		Format_Object.Next_Paragraph_Format_Type := Nested_Enumerated;
		Format_Object.Next_Enumerated_Num := 1;
		Format_Object.Enumerated_Level := Format_Object.Enumerated_Level + 1;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "title" then
		Format_Object.Next_Paragraph_Format_Type := Title;
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
	    	= "singlenote" then
		Format_Object.Next_Paragraph_Format_Type := Single_Note;
		Format_Object.Next_Paragraph_Subhead_Type := Single_Note;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "rmonly" then
		if Format_Object.Include_Annotations then -- AARM, but this is RM-only.
		    Toss_for_RM ("rmonly");
		else
		    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Is_Formatting
			:= False; -- Leave the format alone.
		end if;
	    -- NotISO text:
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "notiso" then
		if Format_Object.Include_ISO then
		    Toss_for_RM ("notiso"); -- This text does not appear in ISO documents.
		else -- not ISO
	            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Is_Formatting
		        := False; -- Leave the format alone.
		end if;
	    -- ISOOnly text:
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "isoonly" then
		if Format_Object.Include_ISO then
	            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Is_Formatting
		        := False; -- Leave the format alone.
		else -- Not ISO
		    Toss_for_RM ("isoonly"); -- This text does not appear in non-ISO documents.
		end if;
	    -- AARM groupings:
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "metarules" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Language_Design;
		    Format_Object.Next_Paragraph_Subhead_Type := Language_Design;
		else -- Don't show annotations.
		    Toss_for_RM ("metarules");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "inconsistent83" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Ada83_Inconsistencies;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada83_Inconsistencies;
		else -- Don't show annotations.
		    Toss_for_RM ("inconsistent83");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "incompatible83" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Ada83_Incompatibilities;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada83_Incompatibilities;
		else -- Don't show annotations.
		    Toss_for_RM ("incompatible83");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "extend83" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Ada83_Extensions;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada83_Extensions;
		else -- Don't show annotations.
		    Toss_for_RM ("extend83");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "diffword83" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Ada83_Wording;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada83_Wording;
		else -- Don't show annotations.
		    Toss_for_RM ("diffword83");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "inconsistent95" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Ada95_Inconsistencies;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada95_Inconsistencies;
		else -- Don't show annotations.
		    Toss_for_RM ("inconsistent95");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "incompatible95" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Ada95_Incompatibilities;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada95_Incompatibilities;
		else -- Don't show annotations.
		    Toss_for_RM ("incompatible95");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "extend95" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Ada95_Extensions;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada95_Extensions;
		else -- Don't show annotations.
		    Toss_for_RM ("extend95");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "diffword95" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Ada95_Wording;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada95_Wording;
		else -- Don't show annotations.
		    Toss_for_RM ("diffword95");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "inconsistent2005" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Ada2005_Inconsistencies;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada2005_Inconsistencies;
		else -- Don't show annotations.
		    Toss_for_RM ("inconsistent2005");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "incompatible2005" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Ada2005_Incompatibilities;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada2005_Incompatibilities;
		else -- Don't show annotations.
		    Toss_for_RM ("incompatible2005");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "extend2005" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Ada2005_Extensions;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada2005_Extensions;
		else -- Don't show annotations.
		    Toss_for_RM ("extend2005");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "diffword2005" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Ada2005_Wording;
		    Format_Object.Next_Paragraph_Subhead_Type := Ada2005_Wording;
		else -- Don't show annotations.
		    Toss_for_RM ("diffword2005");
		end if;
	    -- ASIS groupings:
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "elementref" then
		if Format_Object.Include_Annotations then
	            Format_Object.Next_Paragraph_Format_Type := Element_Ref;
	            Format_Object.Next_Paragraph_Subhead_Type := Element_Ref;
		else -- Don't show annotations.
		    Toss_for_RM ("elementref");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "childref" then
		if Format_Object.Include_Annotations then
	            Format_Object.Next_Paragraph_Format_Type := Child_Ref;
	            Format_Object.Next_Paragraph_Subhead_Type := Child_Ref;
		else -- Don't show annotations.
		    Toss_for_RM ("childref");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "usagenote" then
		if Format_Object.Include_Annotations then
	            Format_Object.Next_Paragraph_Format_Type := Usage_Note;
	            Format_Object.Next_Paragraph_Subhead_Type := Usage_Note;
		else -- Don't show annotations.
		    Toss_for_RM ("usagenote");
		end if;
	    -- AARM annotations:
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "discussion" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Discussion;
		    Format_Object.Next_Paragraph_Subhead_Type := Discussion;
		else -- Don't show annotations.
		    Toss_for_RM ("discussion");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "reason" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Reason;
		    Format_Object.Next_Paragraph_Subhead_Type := Reason;
		else -- Don't show annotations.
		    Toss_for_RM ("reason");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "ramification" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Ramification;
		    Format_Object.Next_Paragraph_Subhead_Type := Ramification;
		else -- Don't show annotations.
		    Toss_for_RM ("ramification");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "theproof" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Proof;
		    Format_Object.Next_Paragraph_Subhead_Type := Proof;
		else -- Don't show annotations.
		    Toss_for_RM ("theproof");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "implnote" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Imp_Note;
		    Format_Object.Next_Paragraph_Subhead_Type := Imp_Note;
		else -- Don't show annotations.
		    Toss_for_RM ("implnote");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "honest" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Honest;
		    Format_Object.Next_Paragraph_Subhead_Type := Honest;
		else -- Don't show annotations.
		    Toss_for_RM ("honest");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "glossarymarker" then
		if Format_Object.Include_Annotations then
		    Format_Object.Next_Paragraph_Format_Type := Glossary_Marker;
		    Format_Object.Next_Paragraph_Subhead_Type := Glossary_Marker;
		else -- Don't show annotations.
		    Toss_for_RM ("glossarymarker");
		end if;
	    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
	    	Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Name, Ada.Strings.Right))
	    	= "aarmonly" then
		if Format_Object.Include_Annotations then
		    null; -- Leave the format alone.
		else -- Don't show annotations.
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
	    begin
		return Get_Current_Item (Format_Object, Input_Object,
		    Format_Object.Syntax_NT (1 .. Format_Object.Syntax_NT_Len));
	    end Get_NT;


	    function Get_Old_NT return String is
	        -- Local routine:
	        -- Return the "current" non-terminal from
	        -- the Syntax_NT string. Handles @Chg.
	    begin
		return Get_Old_Item (Format_Object, Input_Object,
		    Format_Object.Syntax_NT (1 .. Format_Object.Syntax_NT_Len));
	    end Get_Old_NT;


	    procedure Get_Change_Version (Is_First : in Boolean;
					  Version : out Character) is
		-- Get a parameter named "Version",
		-- containing a character representing the version number.
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
			"revisedadded" then
			Kind := ARM_Database.Revised_Inserted_Number;
		    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Kind_Name, Ada.Strings.Right)) =
			"added" then
			Kind := ARM_Database.Inserted;
		    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Kind_Name, Ada.Strings.Right)) =
			"addednormal" then
			Kind := ARM_Database.Inserted_Normal_Number;
		    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Kind_Name, Ada.Strings.Right)) =
			"deleted" then
			Kind := ARM_Database.Deleted;
		    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Kind_Name, Ada.Strings.Right)) =
			"deletedadded" then
			Kind := ARM_Database.Deleted_Inserted_Number;
		    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Kind_Name, Ada.Strings.Right)) =
			"deletednodelmsg" then
			Kind := ARM_Database.Deleted_No_Delete_Message;
		    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Kind_Name, Ada.Strings.Right)) =
			"deletedaddednodelmsg" then
			Kind := ARM_Database.Deleted_Inserted_Number_No_Delete_Message;
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
		    -- Get the Boolean character:
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
		Which_Param : ARM_Input.Param_Num;
		New_Ref, Cursor : Reference_Ptr;
	    begin
	        ARM_Input.Check_One_of_Parameter_Names (Input_Object,
		    Param_Name_1 => "Ref" & (4..ARM_Input.Command_Name_Type'Last => ' '),
		    Param_Name_2 => "ARef" & (5..ARM_Input.Command_Name_Type'Last => ' '),
		    Is_First => False,
		    Param_Found => Which_Param,
		    Param_Close_Bracket => Close_Ch);
	        if Close_Ch /= ' ' then
		    -- Get the reference:
		    Len := 0;
		    loop
		        ARM_Input.Get_Char (Input_Object, Ch);
		        if Ch /= Close_Ch then
			    Len := Len + 1;
			    if Len > Ref_Name'Last then
			        Ada.Text_IO.Put_Line ("  ** Reference too long on line " & ARM_Input.Line_String (Input_Object));
			    else
				Ref_Name(Len) := Ch;
			    end if;
		        else -- End of the reference.
			    if Len = 0 then
			        Ada.Text_IO.Put_Line ("  ** Failed to find reference on line " & ARM_Input.Line_String (Input_Object));
			    end if;
			    exit;
		        end if;
		    end loop;

		    if Display_It then
		        -- Save a reference for outputting on the next
		        -- paragraph start.
		        New_Ref := Allocate_Reference;
		        New_Ref.all := (Ref_Name => Ref_Name,
					Ref_Len => Len,
					Is_DR_Ref => (Which_Param = 1),
					   -- DR reference if Param = 1;
					   -- AI reference otherwise.
					Next => null);
			-- Attach this to the *end* of the list.
			if Format_Object.References = null then
			    Format_Object.References := New_Ref;
			else
			    Cursor := Format_Object.References;
			    while Cursor.Next /= null loop
				Cursor := Cursor.next;
			    end loop;
			    Cursor.Next := New_Ref;
			end if;
	            -- else don't display it.
		    end if;
	        -- else no parameter. Weird.
	        end if;
	    end Gen_Ref_or_ARef_Parameter;


	    procedure Gen_Chg_xxxx (Param_Cmd   : in Data.Command_Type;
				    AARM_Prefix : in String;
				    For_Aspect  : in Boolean := False) is
		-- Implement chgimpdef, chgimpladv, chgdocreq, and
		-- chgaspectdesc commands.
		-- The AARM prefix (if needed) is AARM_Prefix, and
		-- the parameter command is Param_Cmd.

	        -- This command is of the form:
	        -- @chgxxxxx{Version=[<version>], Kind=(<kind>),
	        --   Text=(<text>)}}
	        -- where <version> is a single character, <Kind> is one
	        -- of Revised, Added, or Deleted, and this is followed
	        -- by the text. As usual, any of the
	        -- allowed bracketing characters can be used.
	        Close_Ch     : Character;
	        Kind         : ARM_Database.Paragraph_Change_Kind_Type;
	        Version	     : ARM_Contents.Change_Version_Type;
	        Display_It   : Boolean;
		use type ARM_Database.Paragraph_Change_Kind_Type;
		Local_Change : ARM_Output.Change_Type;
		Skip_Header  : Boolean := False;
		Key          : ARM_Index.Index_Key;
	    begin
	        Get_Change_Version (Is_First => True,
		    Version => Version);
		    -- Read a parameter named "Version".

	        Get_Change_Kind (Kind);
		    -- Read a parameter named "Kind".

	        -- Check for the optional "InitialVersion" parameter,
		-- and the not optional, but only used for some commands
		-- "Aspect" parameter, stopping when we reach Text:
		declare
		    Which_Param : ARM_Input.Param_Num;
		    Ch		: Character;
		    Saw_Aspect  : Boolean := False;
		begin
		    -- If there is no InitialVersion command, use the same
		    -- version of the rest of the command.
		    Format_Object.Impdef_Initial_Version := Version;
		    loop
	                ARM_Input.Check_One_of_Parameter_Names (Input_Object,
		            Param_Name_1 => "InitialVersion" & (15..ARM_Input.Command_Name_Type'Last => ' '),
		            Param_Name_2 => "Aspect" & (7..ARM_Input.Command_Name_Type'Last => ' '),
		            Param_Name_3 => "Text" & (5..ARM_Input.Command_Name_Type'Last => ' '),
		            Is_First => False,
		            Param_Found => Which_Param,
		            Param_Close_Bracket => Close_Ch);

		        if Which_Param = 1 and then Close_Ch /= ' ' then
			    -- Found InitialVersion
		            ARM_Input.Get_Char (Input_Object, Ch);
			    Format_Object.Impdef_Initial_Version := Ch;
		            ARM_Input.Get_Char (Input_Object, Ch);
		            if Ch /= Close_Ch then
			        Ada.Text_IO.Put_Line ("  ** Bad close for InitialVersion parameter on line " &
				    ARM_Input.Line_String (Input_Object));
			        ARM_Input.Replace_Char (Input_Object);
		            end if;
		        elsif Which_Param = 2 and then Close_Ch /= ' ' then
			    -- Found Aspect parameter.
			    Saw_Aspect := True;

		            -- Save name:
		            ARM_Input.Copy_to_String_until_Close_Char (
			        Input_Object,
			        Close_Ch,
			        Format_Object.Aspect_Name,
			        Format_Object.Aspect_Name_Len);

		        else -- We found "Text" (or an error)
			    exit; -- Handling of Text is below.
		        end if;
		    end loop;
		    if For_Aspect then
			if not Saw_Aspect then
			    Ada.Text_IO.Put_Line ("  ** Missing Aspect parameter on line " &
				ARM_Input.Line_String (Input_Object));
			-- else OK.
			end if;
		    else -- Not aspect.
			if Saw_Aspect then
			    Ada.Text_IO.Put_Line ("  ** Aspect parameter on non-aspect command on line " &
				ARM_Input.Line_String (Input_Object));
			-- else OK.
			end if;
		    end if;
		end;

--Ada.Text_IO.Put_Line ("Gen_Chg_xxxx, Kind=" &
--ARM_Database.Paragraph_Change_Kind_Type'Image(Kind) &
--"; version=" & Version & "; InitVer=" & Format_Object.Impdef_Initial_Version);

	        if (Kind = ARM_Database.Inserted or else
		    Kind = ARM_Database.Inserted_Normal_Number) then
		    Calc_Change_Disposition
		        (Format_Object => Format_Object,
		         Version => Version,
			 Operation => ARM_Output.Insertion,
			 Text_Kind => Local_Change);
--Ada.Text_IO.Put_Line ("  Insert, Local_Change=" &
--ARM_Output.Change_Type'Image(Local_Change));
		    case Local_Change is
		        when Do_Not_Display_Text =>
		            Display_It := False;
			    Local_Change := ARM_Output.None;
		        when ARM_Output.None|ARM_Output.Insertion =>
		            Format_Object.Impdef_Version := Version;
		            Format_Object.Impdef_Change_Kind := Kind;
		            Display_It := Format_Object.Include_Annotations;
			        -- Show impdef notes only if we're showing annotations.
		        when ARM_Output.Deletion =>
			    raise Program_Error;
		    end case;
		elsif Kind = ARM_Database.Deleted or else
		      Kind = ARM_Database.Deleted_Inserted_Number or else
		      Kind = ARM_Database.Deleted_No_Delete_Message or else
		      Kind = ARM_Database.Deleted_Inserted_Number_No_Delete_Message then
		    Calc_Change_Disposition
		        (Format_Object => Format_Object,
		         Version => Version,
			 Operation => ARM_Output.Deletion,
			 Text_Kind => Local_Change);
--Ada.Text_IO.Put_Line ("  Delete, Local_Change=" &
--ARM_Output.Change_Type'Image(Local_Change));
		    case Local_Change is
		        when Do_Not_Display_Text =>
		            --Display_It := False;
			    -- We need to show the paragraph, without its header,
			    -- so that we get a proper "this paragraph is deleted"
			    -- message (if one is needed). Nothing will actually
			    -- be output in this case.
			    Local_Change := ARM_Output.None;
			    Skip_Header := True;
		            Format_Object.Impdef_Version := Version;
		            Format_Object.Impdef_Change_Kind := Kind;
		            Display_It := Format_Object.Include_Annotations;
			        -- Show impdef notes only if we're showing annotations.
		        when ARM_Output.None | ARM_Output.Deletion =>
		            Format_Object.Impdef_Version := Version;
		            Format_Object.Impdef_Change_Kind := Kind;
		            Display_It := Format_Object.Include_Annotations;
			        -- Show impdef notes only if we're showing annotations.
			    Skip_Header := False;
		        when ARM_Output.Insertion =>
			    raise Program_Error;
		    end case;
	        else -- we always display it.
--Ada.Text_IO.Put_Line ("  Other");
		    Format_Object.Impdef_Version := Version;
		    Format_Object.Impdef_Change_Kind := Kind;
		    Display_It := Format_Object.Include_Annotations;
		        -- Show impdef notes only if we're showing annotations.
		    Local_Change := ARM_Output.None;
	        end if;
--Ada.Text_IO.Put_Line ("  Display_It=" & Boolean'Image(Display_It));

		-- "Text" parameter name consumed above.
	        if Close_Ch /= ' ' then
		    -- Stack it so we can process the end:
		    Set_Nesting_for_Parameter
		        (Command => Param_Cmd,
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

		        if not Skip_Header then
			    declare
			        Local_Format : ARM_Output.Format_Type :=
				    Format_Object.Text_Format;
			    begin
			        Local_Format.Bold := True;
			        Local_Format.Version := Format_Object.Impdef_Version;
		                if ARM_Output."/=" (Local_Change, ARM_Output.None) then
			            Local_Format.Change := Local_Change;
			            ARM_Output.Text_Format (Output_Object,
							    Local_Format);
		                else -- No change from us:
			            ARM_Output.Text_Format (Output_Object,
							    Local_Format);
		                end if;
		                ARM_Output.Ordinary_Text (Output_Object,
			             Text => AARM_Prefix);
			        Local_Format.Bold := Format_Object.Text_Format.Bold;
			        Local_Format.Change := Format_Object.Text_Format.Change;
		                ARM_Output.Text_Format (Output_Object,
							Local_Format);
			    end;
		        -- else skip the header, do nothing.
		        end if;
		        Format_Object.Last_Paragraph_Subhead_Type := Bare_Annotation;
		        Format_Object.Last_Non_Space := False;

			if For_Aspect then
			    -- Output the aspect name:
			    declare
			        Local_Format : ARM_Output.Format_Type :=
				    Format_Object.Text_Format;
			    begin
			        Local_Format.Bold := True;
			        Local_Format.Version := Format_Object.Impdef_Version;
		                if ARM_Output."/=" (Local_Change, ARM_Output.None) then
			            Local_Format.Change := Local_Change;
			            ARM_Output.Text_Format (Output_Object,
							    Local_Format);
		                else -- No change from us:
			            ARM_Output.Text_Format (Output_Object,
							    Local_Format);
		                end if;
		                ARM_Output.Ordinary_Text (Output_Object,
			             Text => Format_Object.Aspect_Name(1..Format_Object.Aspect_Name_Len));
		                ARM_Output.Ordinary_Text (Output_Object,
			             Text => ": ");
			        Local_Format.Bold := Format_Object.Text_Format.Bold;
			        Local_Format.Change := Format_Object.Text_Format.Change;
		                ARM_Output.Text_Format (Output_Object,
							Local_Format);
			    end;
			-- else no additional text.
			end if;
		    else -- Don't display, skip the text:
		        ARM_Input.Skip_until_Close_Char (Input_Object,
			    Close_Ch);
		        ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
		    end if;

	        -- else no parameter. Weird.
	        end if;
	    end Gen_Chg_xxxx;


	    procedure Get_Syntax_Parameters (Has_Version : in Boolean;
					     RHS_Close_Ch : out Character) is
		-- Get the parameters for a Syn, AddedSyn, or DeletedSyn
		-- command. The command has a version parameter (it's not @Syn)
		-- if Has_Version is True. The results are put into the usual
		-- places. The RHS parameter's name is evaluated, and its
		-- closing character is RHS_Close_Ch.
		-- @Syn{[Tabs=<Tabset>, ]LHS=<Non-terminal>, RHS=<Production>}
		-- @AddedSyn{Version=[Version],[Tabs=<Tabset>, ]LHS=<Non-terminal>, RHS=<Production>}
		-- @DeletedSyn{Version=[Version],[Tabs=<Tabset>, ]LHS=<Non-terminal>, RHS=<Production>}
		Close_Ch, Ch : Character;
		Was_Tabs : Boolean := False;
	    begin
		if Has_Version then
		    Get_Change_Version (Is_First => True,
				        Version =>
					   Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version);
		end if;

		-- Peek to see if Tabs parmeter is present:
		ARM_Input.Get_Char (Input_Object, Ch);
		ARM_Input.Replace_Char (Input_Object);
		if Ch = 'T' or else Ch = 't' then
		    Was_Tabs := True;
		    ARM_Input.Check_Parameter_Name (Input_Object,
		        Param_Name => "Tabs" & (5..ARM_Input.Command_Name_Type'Last => ' '),
		        Is_First => (not Has_Version),
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
		    Is_First => (not Was_Tabs) and (not Has_Version),
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

		ARM_Input.Check_Parameter_Name (Input_Object,
		    Param_Name => "RHS" & (4..ARM_Input.Command_Name_Type'Last => ' '),
		    Is_First => False,
		    Param_Close_Bracket => RHS_Close_Ch);
	    end Get_Syntax_Parameters;


	    procedure Gen_Syntax_Rule (Disposition : in ARM_Output.Change_Type;
				       RHS_Close_Ch : in Character) is
		-- Generate a syntax rule with the specified disposition
		-- for explicitly generated text. All of the parameters have been
		-- read in; the close character for the RHS parameter is
		-- RHS_Close_Ch.
		use type ARM_Output.Change_Type;
		Org_Tabs : ARM_Output.Tab_Info;
		Key : ARM_Index.Index_Key;
	    begin
		if Disposition = Do_Not_Display_Text then
		    if RHS_Close_Ch /= ' ' then
		        -- Skip the RHS and put nothing in the DB.
		        ARM_Input.Skip_until_Close_Char (Input_Object, RHS_Close_Ch);
		        ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
		        if ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind,
		           ARM_Database.Deleted) or else
		           ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind,
		           ARM_Database.Deleted_Inserted_Number) or else
		           ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind,
		           ARM_Database.Deleted_No_Delete_Message) or else
		           ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind,
		           ARM_Database.Deleted_Inserted_Number_No_Delete_Message) then
			    -- In a deleted paragraph, call Check_Paragraph
			    -- to trigger the "deleted paragraph" message.
			    -- (Otherwise, this may never happen.)
		            Check_Paragraph;
		        -- else null; -- Nothing special to do.
		        end if;
		    end if;
		    -- Stack it so we can process the end:
		    Set_Nesting_for_Parameter
		        (Command => Syntax_Rule_RHS,
			 Close_Ch => RHS_Close_Ch);
		    -- (We probably don't need to do the above, but consistency
		    -- is preferred.)
		else
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
				       No_Annotations => False);
		        -- We use Format here so we can support changes in
		        -- the non-terminal.

		    -- Index the non-terminal:
		    ARM_Index.Add (Term => Get_NT,
				   Kind => ARM_Index.Primary_Term,
				   Clause => Clause_String (Format_Object),
				   Paragraph => Paragraph_String,
				   Key => Key);
		    ARM_Output.Index_Target (Output_Object, Key);

		    -- Make an anchor for the non-terminal:
		    if Format_Object.Link_Non_Terminals then
		        declare
			    Lower_NT : constant String :=
				Ada.Characters.Handling.To_Lower (Get_NT);
			    Link_Target : ARM_Syntax.Target_Type :=
			        ARM_Syntax.Non_Terminal_Link_Target (Lower_NT);
			    Lower_Old_NT : constant String :=
				Ada.Characters.Handling.To_Lower (Get_Old_NT);
			    Old_Link_Target : ARM_Syntax.Target_Type :=
			        ARM_Syntax.Non_Terminal_Link_Target (Lower_Old_NT);
		        begin
			    if Lower_NT /= "" then
			        if Clause_String (Format_Object) /=
				    ARM_Syntax.Non_Terminal_Clause (Lower_NT) then
			            Ada.Text_IO.Put_Line ("  ** Clause mismatch for non-terminal: Is=" &
				        Clause_String (Format_Object) & "; Was=" & ARM_Syntax.Non_Terminal_Clause (Lower_NT) &
				        "; NT=" & Lower_NT & "; on line " & ARM_Input.Line_String (Input_Object));
			        end if;
		                ARM_Output.Local_Target (Output_Object,
			            Text => "",
			            Target => Link_Target);
			    -- else the Non-Terminal was deleted, no
			    -- anchor is needed.
			    end if;
			    if Lower_Old_NT /= "" then
			        if Clause_String (Format_Object) /=
				    ARM_Syntax.Non_Terminal_Clause (Lower_Old_NT) then
				    -- This can happen if an item is inserted
				    -- on one place and deleted in another.
				    -- We'll assume this isn't an error and just
				    -- do nothing here.
			            --Ada.Text_IO.Put_Line ("  %% Clause mismatch for old non-terminal: Is=" &
				    --    Clause_String (Format_Object) & "; Was=" & ARM_Syntax.Non_Terminal_Clause (Lower_Old_NT) &
				    --    "; NT=" & Lower_Old_NT & "; on line " & ARM_Input.Line_String (Input_Object));
				    null;
				else
		                    ARM_Output.Local_Target (Output_Object,
			                Text => "",
			                Target => Old_Link_Target);
			        end if;
			    -- else there was no old Non-Terminal.
			    end if;
			end;
		    end if;

		    -- Set the font for the "::=". Note that we use @s{}
		    -- above, so that any font changes in the Non-Terminal
		    -- (as in a @Chg command) are respected.
		    -- This command includes any needed insertion or deletion.

		    declare
			Swiss_Format : ARM_Output.Format_Type :=
			    Format_Object.Text_Format;
		    begin
		        Swiss_Format.Font := ARM_Output.Swiss;
			if Disposition = ARM_Output.None then
			    null;
		        else
		            Swiss_Format.Change := Disposition;
		            Swiss_Format.Version := Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version;
		        end if;
		        ARM_Output.Text_Format (Output_Object,
					        Swiss_Format);
		    end;
		    ARM_Output.Ordinary_Text (Output_Object, " ::= ");
		    ARM_Output.Text_Format (Output_Object,
					    Format_Object.Text_Format); -- Reset format.
		    Format_Object.Last_Non_Space := False;

		    if RHS_Close_Ch /= ' ' then
		        -- Now, handle the parameter:
		        -- Stack it so we can process the end:
		        Set_Nesting_for_Parameter
			    (Command => Syntax_Rule_RHS,
			     Close_Ch => RHS_Close_Ch);

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
		end if;
	    end Gen_Syntax_Rule;


	    procedure Format_Text (Text : in String;
				   Text_Name : in String) is
		-- Note: We use the state of the surrounding call.
		Input_Object : Arm_String.String_Input_Type;
		Real_Include_Annotations : Boolean := Format_Object.Include_Annotations;
	    begin
		-- Don't show annotations here:
                Format_Object.Include_Annotations := False;
		Arm_String.Open (Input_Object, Text, Text_Name);
		     -- Open the input object using a string for input.
		Real_Process (Format_Object, Format_State, Input_Object, Output_Object);
		Arm_String.Close (Input_Object);
		Format_Object.Include_Annotations := Real_Include_Annotations;
	    end Format_Text;

	    procedure DB_Report is new ARM_Database.Report (Format_Text);

	begin
	    case Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command is

		-- Basic text commands:

		when Redundant =>
		    if Format_Object.Include_Annotations then
			Check_Paragraph;
		        ARM_Output.Ordinary_Character (Output_Object, '[');
			Format_Object.Last_Non_Space := True;
		    -- else ignored.
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
		    Format_Object.Text_Format.Bold := True;
		    ARM_Output.Text_Format (Output_Object,
					    Format => Format_Object.Text_Format);

		when Italic =>
		    Check_Paragraph;
		    Format_Object.Text_Format.Italic := True;
		    ARM_Output.Text_Format (Output_Object,
					    Format => Format_Object.Text_Format);

		when Roman =>
		    Check_Paragraph;
		    Format_Object.Text_Format.Font := ARM_Output.Roman;
		    ARM_Output.Text_Format (Output_Object,
					    Format => Format_Object.Text_Format);

		when Swiss =>
		    Check_Paragraph;
		    Format_Object.Text_Format.Font := ARM_Output.Swiss;
		    ARM_Output.Text_Format (Output_Object,
					    Format => Format_Object.Text_Format);

		when Fixed =>
		    Check_Paragraph;
		    Format_Object.Text_Format.Font := ARM_Output.Fixed;
		    ARM_Output.Text_Format (Output_Object,
					    Format => Format_Object.Text_Format);

		when Roman_Italic =>
		    Check_Paragraph;
		    Format_Object.Text_Format.Italic := True;
		    Format_Object.Text_Format.Font := ARM_Output.Roman;
		    ARM_Output.Text_Format (Output_Object,
					    Format => Format_Object.Text_Format);
		when Shrink =>
		    declare
			use type ARM_Output.Size_Type;
		    begin
		        Check_Paragraph;
		        Format_Object.Text_Format.Size :=
			    Format_Object.Text_Format.Size - 1;
		        ARM_Output.Text_Format (Output_Object,
					        Format => Format_Object.Text_Format);
		    end;

		when Grow =>
		    declare
			use type ARM_Output.Size_Type;
		    begin
		        Check_Paragraph;
		        Format_Object.Text_Format.Size :=
			    Format_Object.Text_Format.Size + 1;
		        ARM_Output.Text_Format (Output_Object,
					        Format => Format_Object.Text_Format);
		    end;

		when Black =>
		    Check_Paragraph;
		    Format_Object.Text_Format.Color := ARM_Output.Black;
		    ARM_Output.Text_Format (Output_Object,
					    Format => Format_Object.Text_Format);

		when Red =>
		    Check_Paragraph;
		    Format_Object.Text_Format.Color := ARM_Output.Red;
		    ARM_Output.Text_Format (Output_Object,
					    Format => Format_Object.Text_Format);

		when Green =>
		    Check_Paragraph;
		    Format_Object.Text_Format.Color := ARM_Output.Green;
		    ARM_Output.Text_Format (Output_Object,
					    Format => Format_Object.Text_Format);

		when Blue =>
		    Check_Paragraph;
		    Format_Object.Text_Format.Color := ARM_Output.Blue;
		    ARM_Output.Text_Format (Output_Object,
					    Format => Format_Object.Text_Format);

		when Keyword =>
		    Check_Paragraph;
		    Format_Object.Text_Format.Bold := True;
		    ARM_Output.Text_Format (Output_Object,
					    Format => Format_Object.Text_Format);

		when Non_Terminal_Format =>
		    -- No linking here.
		    Check_Paragraph;
		    Format_Object.Text_Format.Font := ARM_Output.Swiss;
		    ARM_Output.Text_Format (Output_Object,
					    Format => Format_Object.Text_Format);

		when Example_Text =>
		    Check_Paragraph;
		    Format_Object.Text_Format.Font := Format_Object.Examples_Font;
		    ARM_Output.Text_Format (Output_Object,
					    Format => Format_Object.Text_Format);

		when Example_Comment =>
		    Check_Paragraph;
		    Format_Object.Text_Format.Font := ARM_Output.Roman;
		    Format_Object.Text_Format.Italic := True;
		    ARM_Output.Text_Format (Output_Object,
					    Format => Format_Object.Text_Format);

		when Tab_Clear =>
		    Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;

		when Tab_Set =>
		    if Format_Object.Next_Paragraph_Format_Type = Bulleted or else
		       Format_Object.Next_Paragraph_Format_Type = Nested_Bulleted or else
		       Format_Object.Next_Paragraph_Format_Type = Nested_X2_Bulleted or else
		       Format_Object.Next_Paragraph_Format_Type = Enumerated or else
		       Format_Object.Next_Paragraph_Format_Type = Nested_Enumerated or else
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

		when Non_Terminal =>
		    -- @nt{text}
		    -- This *was* simple, until we added linking.
		    declare
			Name : String(1..120);
			Len : Natural;
			Swiss_Format : ARM_Output.Format_Type :=
			    Format_Object.Text_Format;
		    begin
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char,
			    Name, Len);

		        -- Set the appropriate style:
		        Check_Paragraph;

		        Swiss_Format.Font := ARM_Output.Swiss;
		        ARM_Output.Text_Format (Output_Object,
					        Format => Swiss_Format);
			if Format_Object.Link_Non_Terminals then
			    if Ada.Strings.Fixed.Index (Name(1..Len), "@") /= 0 then
			        -- Embedded commands. We have to clean the
				-- string of the commands (if we can) before
				-- making a link.
				declare
				    Lower_NT : String :=
				        Ada.Characters.Handling.To_Lower (Name(1..Len));
				    Lower_NT_Len : Natural := Lower_NT'Length;
				    Loc : Natural := Lower_NT'First;
				begin
				    while Loc <= Lower_NT_Len loop
					-- Check for simple commands and remove them:
					if Lower_NT(Loc) = '@' then -- Start of a command.
					    if Loc < Lower_NT_Len and then
						(Lower_NT(Loc+1) = '!' or else
						 Lower_NT(Loc+1) = ';') then
						-- Soft hyphen or no-op, remove.
						Lower_NT(Loc .. Lower_NT_Len-2) :=
						    Lower_NT(Loc+2 .. Lower_NT_Len);
						Lower_NT_Len := Lower_NT_Len - 2;
					    else -- Unknown.
						exit;
					    end if;
					else -- nothing to do, move to next character
					    Loc := Loc + 1;
					end if;
				    end loop;

				    declare
				        Clause : constant String :=
				            ARM_Syntax.Non_Terminal_Clause (Lower_NT(1..Lower_NT_Len));
				        Target : constant ARM_Syntax.Target_Type :=
				            ARM_Syntax.Non_Terminal_Link_Target (Lower_NT(1..Lower_NT_Len));
				        Org_Font : ARM_Output.Font_Family_Type :=
				            Format_Object.Text_Format.Font;
				    begin
				        Format_Object.Text_Format.Font := ARM_Output.Swiss;
				        if Clause = "" then -- Not found. No link, but error message:
					    if Ada.Strings.Fixed.Index (Lower_NT(1..Lower_NT_Len), "@") /= 0 then
					        Ada.Text_IO.Put_Line ("  %% Non-terminal with complex embedded commands " &
					            Lower_NT(1..Lower_NT_Len) & " on line " & ARM_Input.Line_String (Input_Object));
					    else
					        Ada.Text_IO.Put_Line ("  ?? Unknown non-terminal " &
					            Lower_NT(1..Lower_NT_Len) & " on line " & ARM_Input.Line_String (Input_Object));
					    end if;
			                    ARM_Format.Format (Format_Object,
					                       Name(1..Len),
					                       Output_Object,
					                       Text_Name => "@nt{}",
					                       No_Annotations => False);
				        else
				            ARM_Output.Local_Link_Start (Output_Object,
					        Target => Target, Clause_Number => Clause);
			                    ARM_Format.Format (Format_Object,
					                       Name(1..Len),
					                       Output_Object,
					                       Text_Name => "@nt{}",
					                       No_Annotations => False);
				            ARM_Output.Local_Link_End (Output_Object,
					        Target => Target, Clause_Number => Clause);
				        end if;
				        Format_Object.Text_Format.Font := Org_Font;
				    end;
				end;
			    else -- Ordinary link.
			        declare
				    Lower_NT : constant String :=
				        Ada.Characters.Handling.To_Lower (Name(1..Len));
				    Clause : constant String :=
				        ARM_Syntax.Non_Terminal_Clause (Lower_NT);
				    Target : constant ARM_Syntax.Target_Type :=
				        ARM_Syntax.Non_Terminal_Link_Target (Lower_NT);
			        begin
				    if Clause = "" then -- Not found. No link, but error message:
				        Ada.Text_IO.Put_Line ("  ?? Unknown non-terminal " &
					    Name(1..Len) & " on line " & ARM_Input.Line_String (Input_Object));
				        ARM_Output.Ordinary_Text (Output_Object, Name(1..Len));
				    else
				        ARM_Output.Local_Link (Output_Object, Text => Name(1..Len),
					    Target => Target, Clause_Number => Clause);
				    end if;
			        end;
			    end if;
		        else
			    if Ada.Strings.Fixed.Index (Name(1..Len), "@") /= 0 then
			        -- Embedded commands, better execute them.
			        declare
				    Org_Font : ARM_Output.Font_Family_Type :=
				        Format_Object.Text_Format.Font;
			        begin
				    Format_Object.Text_Format.Font := ARM_Output.Swiss;
			            ARM_Format.Format (Format_Object,
					               Name(1..Len),
					               Output_Object,
					               Text_Name => "@nt{}",
					               No_Annotations => False);
				    Format_Object.Text_Format.Font := Org_Font;
			        end;
			    else
				ARM_Output.Ordinary_Text (Output_Object, Name(1..Len));
			    end if;
			end if;
		        ARM_Output.Text_Format (Output_Object,
					        Format => Format_Object.Text_Format);
			Format_Object.Last_Non_Space := True;
		    end;
		    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Nonterminal)");

		-- Versioned breaking:
		when New_Page_for_Version | RM_New_Page_for_Version |
		     New_Column_for_Version =>
		    declare
			Version : ARM_Contents.Change_Version_Type;
		    begin
		        Get_Change_Version (Is_First => True,
		            Version => Version);
		            -- Read a parameter named "Version".
			Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version := Version;
		    end;

		-- Tables:
		when Table =>
			-- @table(Columns=<number>,
			--       Alignment=<AllLeft|AllCenter|CenterExceptFirst>,
			--       FirstColWidth=<number>,
			--       LastColWidth=<number>,
			--       NoBreak=<T|F>,
			--       Border=<T|F>,
			--       SmallSize=<T|F>,
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
			Align_Name : ARM_Input.Command_Name_Type;
			Cols, FirstWidth, LastWidth : Character;
			No_Page_Break : Boolean;
			Has_Border : Boolean;
			Small_Text : Boolean;
			Alignment : ARM_Output.Column_Text_Alignment;
		    begin
		        ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Columns" & (8..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => True,
			    Param_Close_Bracket => Close_Ch);
		        if Close_Ch /= ' ' then
			    ARM_Input.Get_Char (Input_Object, Cols);
			    ARM_Input.Get_Char (Input_Object, Ch);
			    if Ch /= Close_Ch then
				Ada.Text_IO.Put_Line ("  ** Bad close for Table Columns on line " & ARM_Input.Line_String (Input_Object));
				ARM_Input.Replace_Char (Input_Object);
			    end if;
			    if Cols not in '2'..'9' then
				Ada.Text_IO.Put_Line ("  ** Bad table column count on line " & ARM_Input.Line_String (Input_Object));
			    end if;
			end if;

		        ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Alignment" & (10..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
		        if Close_Ch /= ' ' then
			    -- Get the alignment word:
			    Arm_Input.Get_Name (Input_Object, Align_Name);
			    ARM_Input.Get_Char (Input_Object, Ch);
			    if Ch /= Close_Ch then
				Ada.Text_IO.Put_Line ("  ** Bad close for Table Alignment on line " & ARM_Input.Line_String (Input_Object));
				ARM_Input.Replace_Char (Input_Object);
			    end if;

			    if Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Align_Name, Ada.Strings.Right)) =
				"allleft" then
				Alignment := ARM_Output.Left_All;
			    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Align_Name, Ada.Strings.Right)) =
				"allcenter" then
				Alignment := ARM_Output.Center_All;
			    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Align_Name, Ada.Strings.Right)) =
				"centerexceptfirst" then
				Alignment := ARM_Output.Center_Except_First;
			    else
				Ada.Text_IO.Put_Line ("  ** Bad column alignment: " &
					Ada.Strings.Fixed.Trim (Align_Name, Ada.Strings.Right) &
					" on line " & ARM_Input.Line_String (Input_Object));
			    end if;
			-- else no parameter. Weird.
			end if;

		        ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "FirstColWidth" & (14..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
		        if Close_Ch /= ' ' then
			    ARM_Input.Get_Char (Input_Object, FirstWidth);
			    ARM_Input.Get_Char (Input_Object, Ch);
			    if Ch /= Close_Ch then
				Ada.Text_IO.Put_Line ("  ** Bad close for Table FirstColWidth on line " & ARM_Input.Line_String (Input_Object));
				ARM_Input.Replace_Char (Input_Object);
			    end if;
			    if FirstWidth not in '1'..'9' then
				Ada.Text_IO.Put_Line ("  ** Bad table 1st column width on line " & ARM_Input.Line_String (Input_Object));
			    end if;
			end if;

		        ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "LastColWidth" & (13..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
		        if Close_Ch /= ' ' then
			    ARM_Input.Get_Char (Input_Object, LastWidth);
			    ARM_Input.Get_Char (Input_Object, Ch);
			    if Ch /= Close_Ch then
				Ada.Text_IO.Put_Line ("  ** Bad close for Table FirstColWidth on line " & ARM_Input.Line_String (Input_Object));
				ARM_Input.Replace_Char (Input_Object);
			    end if;
			    if FirstWidth not in '1'..'9' then
				Ada.Text_IO.Put_Line ("  ** Bad table last column width on line " & ARM_Input.Line_String (Input_Object));
			    end if;
			end if;

			Get_Boolean ("NoBreak" & (8..ARM_Input.Command_Name_Type'Last => ' '), No_Page_Break);
			Get_Boolean ("Border" & (7..ARM_Input.Command_Name_Type'Last => ' '), Has_Border);
			Get_Boolean ("SmallSize" & (10..ARM_Input.Command_Name_Type'Last => ' '), Small_Text);

		        -- Set to the table format:
		        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Last_Subhead_Paragraph := Format_Object.Last_Paragraph_Subhead_Type;
		        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Subhead_Paragraph := Format_Object.Next_Paragraph_Subhead_Type;
		        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Paragraph_Format := Format_Object.Next_Paragraph_Format_Type;
		        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Tab_Stops := Format_Object.Paragraph_Tab_Stops;
		        Format_Object.Next_Paragraph_Format_Type := In_Table;
		        Format_Object.In_Paragraph := True; -- A fake, but we cannot have any format.
		        Format_Object.No_Start_Paragraph := False; -- For most purposes, being in a table is like being in a paragraph.

			-- OK, we've started the table. Now, get the caption:
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Caption" & (8..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Check if the parameter is empty:
			    ARM_Input.Get_Char (Input_Object, Ch);
			    if Ch /= Close_Ch then
				-- There is a caption:
				ARM_Input.Replace_Char (Input_Object);

				-- Create the table.
			        Arm_Output.Start_Table (
					Output_Object,
					Columns => Character'Pos(Cols) - Character'Pos('0'),
				        First_Column_Width => Character'Pos(FirstWidth) - Character'Pos('0'),
				        Last_Column_Width => Character'Pos(LastWidth) - Character'Pos('0'),
					Alignment => Alignment,
					No_Page_Break => No_Page_Break,
					Has_Border => Has_Border,
					Small_Text_Size => Small_Text,
					Header_Kind => ARM_Output.Both_Caption_and_Header);

			        -- Now, handle the parameter:
		                -- Stack it so we can process the end:
			        Set_Nesting_for_Parameter
			            (Command => Table_Param_Caption,
				     Close_Ch => Close_Ch);

			    else -- Empty Caption. Move on to the Headers
				 -- command.
				ARM_Input.Check_Parameter_Name (Input_Object,
				    Param_Name => "Headers" & (8..ARM_Input.Command_Name_Type'Last => ' '),
				    Is_First => False,
				    Param_Close_Bracket => Close_Ch);

				if Close_Ch /= ' ' then
				    -- Check if the parameter is empty:
				    ARM_Input.Get_Char (Input_Object, Ch);
				    if Ch /= Close_Ch then
					-- There is a header:
					ARM_Input.Replace_Char (Input_Object);

					-- Create the table.
				        Arm_Output.Start_Table (
						Output_Object,
						Columns => Character'Pos(Cols) - Character'Pos('0'),
					        First_Column_Width => Character'Pos(FirstWidth) - Character'Pos('0'),
					        Last_Column_Width => Character'Pos(LastWidth) - Character'Pos('0'),
						Alignment => Alignment,
						No_Page_Break => No_Page_Break,
						Has_Border => Has_Border,
						Small_Text_Size => Small_Text,
						Header_Kind => ARM_Output.Header_Only);

				        -- Now, handle the parameter:
			                -- Stack it so we can process the end:
				        Set_Nesting_for_Parameter
				            (Command => Table_Param_Header,
					     Close_Ch => Close_Ch);

				    else -- Empty Headers, too. Move on to the
					 -- Body command.
					ARM_Input.Check_Parameter_Name (Input_Object,
					    Param_Name => "Body" & (5..ARM_Input.Command_Name_Type'Last => ' '),
					    Is_First => False,
					    Param_Close_Bracket => Close_Ch);

					if Close_Ch /= ' ' then
					    -- Create the table.
				            Arm_Output.Start_Table (
						    Output_Object,
						    Columns => Character'Pos(Cols) - Character'Pos('0'),
					            First_Column_Width => Character'Pos(FirstWidth) - Character'Pos('0'),
					            Last_Column_Width => Character'Pos(LastWidth) - Character'Pos('0'),
						    Alignment => Alignment,
						    No_Page_Break => No_Page_Break,
						    Has_Border => Has_Border,
						    Small_Text_Size => Small_Text,
						    Header_Kind => ARM_Output.No_Headers);

				            -- Now, handle the parameter:
			                    -- Stack it so we can process the end:
				            Set_Nesting_for_Parameter
				                (Command => Table_Param_Body,
					         Close_Ch => Close_Ch);

					-- else no parameter, weird.
					end if;

				    end if;

				-- else no parameter, weird.
				end if;

                            end if;

			-- else no parameter, weird.
			end if;
		    end;

		-- Pictures:
		when Picture_Alone | Picture_Inline =>
		    -- @PictureInline(Alignment=<Inline|FloatLeft|FloatRight>,
		    --		     Border=<None|Thin|Thick>,
		    --		     Height=<nnn>,
		    --		     Width=<nnn>,
		    --		     Name=<name>,
		    --		     Descr=<descr>)
		    -- @PictureAlone(Alignment=<Left|Right|Center>,
		    --		     Border=<None|Thin|Thick>,
		    --		     Height=<nnn>,
		    --		     Width=<nnn>,
		    --		     Name=<name>,
		    --		     Descr=<descr>)
		    declare
			Close_Ch, Ch : Character;
			Align_Name : ARM_Input.Command_Name_Type;
			Alignment : ARM_Output.Picture_Alignment;
			Border : ARM_Output.Border_Kind;
			Height : Natural := 0;
			Width : Natural := 0;
		        Name, Descr : String(1..120);
		        NLen, DLen : Natural := 0;
		    begin
		        if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command =
			    Picture_Alone then
			    Check_End_Paragraph; -- End any paragraph that we're in.

		            ARM_Input.Check_Parameter_Name (Input_Object,
			        Param_Name => "Alignment" & (10..ARM_Input.Command_Name_Type'Last => ' '),
			        Is_First => True,
			        Param_Close_Bracket => Close_Ch);
			    if Close_Ch /= ' ' then
			        -- Get the alignment word:
			        Arm_Input.Get_Name (Input_Object, Align_Name);
			        ARM_Input.Get_Char (Input_Object, Ch);
			        if Ch /= Close_Ch then
				    Ada.Text_IO.Put_Line ("  ** Bad close for Picture Alignment on line " & ARM_Input.Line_String (Input_Object));
				    ARM_Input.Replace_Char (Input_Object);
			        end if;

			        if Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Align_Name, Ada.Strings.Right)) =
				    "left" then
				    Alignment := ARM_Output.Alone_Left;
			        elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Align_Name, Ada.Strings.Right)) =
				    "right" then
				    Alignment := ARM_Output.Alone_Right;
			        elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Align_Name, Ada.Strings.Right)) =
				    "center" then
				    Alignment := ARM_Output.Alone_Center;
			        else
				    Ada.Text_IO.Put_Line ("  ** Bad stand-alone picture alignment: " &
					    Ada.Strings.Fixed.Trim (Align_Name, Ada.Strings.Right) &
					    " on line " & ARM_Input.Line_String (Input_Object));
			        end if;
			    -- else no parameter. Weird.
			    end if;

		        else -- Picture_Inline.
			    Check_Paragraph; -- Make sure we're in a paragraph.

		            ARM_Input.Check_Parameter_Name (Input_Object,
			        Param_Name => "Alignment" & (10..ARM_Input.Command_Name_Type'Last => ' '),
			        Is_First => True,
			        Param_Close_Bracket => Close_Ch);
			    if Close_Ch /= ' ' then
			        -- Get the alignment word:
			        Arm_Input.Get_Name (Input_Object, Align_Name);
			        ARM_Input.Get_Char (Input_Object, Ch);
			        if Ch /= Close_Ch then
				    Ada.Text_IO.Put_Line ("  ** Bad close for Picture Alignment on line " & ARM_Input.Line_String (Input_Object));
				    ARM_Input.Replace_Char (Input_Object);
			        end if;

			        if Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Align_Name, Ada.Strings.Right)) =
				    "inline" then
				    Alignment := ARM_Output.Inline;
			        elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Align_Name, Ada.Strings.Right)) =
				    "floatleft" then
				    Alignment := ARM_Output.Float_Left;
			        elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Align_Name, Ada.Strings.Right)) =
				    "floatright" then
				    Alignment := ARM_Output.Float_Right;
			        else
				    Ada.Text_IO.Put_Line ("  ** Bad inline picture alignment: " &
					    Ada.Strings.Fixed.Trim (Align_Name, Ada.Strings.Right) &
					    " on line " & ARM_Input.Line_String (Input_Object));
			        end if;
			    -- else no parameter. Weird.
			    end if;

		        end if;

		        ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Border" & (7..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
		        if Close_Ch /= ' ' then
			    -- Get the alignment word:
			    Arm_Input.Get_Name (Input_Object, Align_Name);
			    ARM_Input.Get_Char (Input_Object, Ch);
			    if Ch /= Close_Ch then
			        Ada.Text_IO.Put_Line ("  ** Bad close for Picture Border on line " & ARM_Input.Line_String (Input_Object));
			        ARM_Input.Replace_Char (Input_Object);
			    end if;

			    if Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Align_Name, Ada.Strings.Right)) =
			        "none" then
			        Border := ARM_Output.None;
			    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Align_Name, Ada.Strings.Right)) =
			        "thin" then
			        Border := ARM_Output.Thin;
			    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Align_Name, Ada.Strings.Right)) =
			        "thick" then
			        Border := ARM_Output.Thick;
			    else
			        Ada.Text_IO.Put_Line ("  ** Bad picture border: " &
				        Ada.Strings.Fixed.Trim (Align_Name, Ada.Strings.Right) &
				        " on line " & ARM_Input.Line_String (Input_Object));
			    end if;
		        -- else no parameter. Weird.
		        end if;

		        ARM_Input.Check_Parameter_Name (Input_Object,
			            Param_Name => "Height" & (7..ARM_Input.Command_Name_Type'Last => ' '),
			            Is_First => False,
			            Param_Close_Bracket => Close_Ch);
		        if Close_Ch /= ' ' then
		            -- Copy over the term:
		            ARM_Input.Copy_to_String_until_Close_Char (
			        Input_Object,
			        Close_Ch,
			        Name,
			        NLen);
			    begin
				Height := Natural'Value(Name(1..NLen));
			    exception
				when Constraint_Error =>
			            Ada.Text_IO.Put_Line ("  ** Bad picture height: " &
				            Name(1..NLen) & " on line " & ARM_Input.Line_String (Input_Object));
			    end;
		        -- else no parameter. Weird.
		        end if;

		        ARM_Input.Check_Parameter_Name (Input_Object,
			            Param_Name => "Width" & (6..ARM_Input.Command_Name_Type'Last => ' '),
			            Is_First => False,
			            Param_Close_Bracket => Close_Ch);
		        if Close_Ch /= ' ' then
		            -- Copy over the term:
		            ARM_Input.Copy_to_String_until_Close_Char (
			        Input_Object,
			        Close_Ch,
			        Name,
			        NLen);
			    begin
				Width := Natural'Value(Name(1..NLen));
			    exception
				when Constraint_Error =>
			            Ada.Text_IO.Put_Line ("  ** Bad picture width: " &
				            Name(1..NLen) & " on line " & ARM_Input.Line_String (Input_Object));
			    end;
		        -- else no parameter. Weird.
		        end if;

		        ARM_Input.Check_Parameter_Name (Input_Object,
			            Param_Name => "Name" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			            Is_First => False,
			            Param_Close_Bracket => Close_Ch);
		        if Close_Ch /= ' ' then
		            -- Copy over the term:
		            ARM_Input.Copy_to_String_until_Close_Char (
			        Input_Object,
			        Close_Ch,
			        Name,
			        NLen);
		        -- else no parameter. Weird.
		        end if;

		        ARM_Input.Check_Parameter_Name (Input_Object,
		            Param_Name => "Descr" & (6..ARM_Input.Command_Name_Type'Last => ' '),
		            Is_First => False,
		            Param_Close_Bracket => Close_Ch);
		        if Close_Ch /= ' ' then
		            -- Copy over the term:
		            ARM_Input.Copy_to_String_until_Close_Char (
			        Input_Object,
			        Close_Ch,
			        Descr,
			        DLen);
		        -- else no parameter. Weird.
		        end if;

			ARM_Output.Picture (
			    Output_Object,
			    Alignment => Alignment,
			    Border => Border,
			    Height => Height,
			    Width => Width,
			    Name => Name(1..NLen),
			    Descr => Descr(1..DLen));
		    end;
		    -- Normal processing should remove the command end marker.

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
				 Text_Format => Format_Object.Text_Format,
					-- Save the current format.
				 Old_Last_Subhead_Paragraph => Format_Object.Last_Paragraph_Subhead_Type,
				 Old_Next_Subhead_Paragraph => Format_Object.Next_Paragraph_Subhead_Type,
				 Old_Next_Paragraph_Format => Format_Object.Next_Paragraph_Format_Type,
				 Old_Tab_Stops => Format_Object.Paragraph_Tab_Stops,
				 Old_Next_Enum_Num => Format_Object.Next_Enumerated_Num,
				 Is_Formatting => True, -- Reset if needed later.
				 Change_Version => '0', -- Not used.
				 Was_Text => False, -- Not used.
				 Prev_Change => ARM_Output.None, -- Not used.
				 Prev_Change_Version => '0', -- Not used.
				 Prev_Added_Change_Version => '0'); -- Not used.

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
			        Format_Object.Next_Enumerated_Num := Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Enum_Num;
			        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (End)");
			    end if;

			    Check_End_Paragraph; -- End any paragraph that we're in.

			    declare
				Lower_Type_Name : constant String :=
				    Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (
					Type_Name, Ada.Strings.Right));
			    begin
			        -- Check if number of columns is changing:
			        if Lower_Type_Name = "twocol" then
				    -- Leaving two column region, reset to one:
				    ARM_Output.Set_Columns (Output_Object, Number_of_Columns => 1);
			        elsif Lower_Type_Name = "fourcol" then
				    -- Leaving four column region, reset to one:
				    ARM_Output.Set_Columns (Output_Object, Number_of_Columns => 1);
			        end if;
			        -- Check if we're leaving a bundle:
			        if Lower_Type_Name = "bundle" then
				    Format_Object.In_Bundle := False;
			        end if;
			        -- Check if we're leaving an enumerated list:
			        if Lower_Type_Name = "enumerate" or else
			           Lower_Type_Name = "innerenumerate" then
				    Format_Object.Enumerated_Level :=
				        Format_Object.Enumerated_Level - 1;
			        end if;
			    end;
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
				       Clause => Clause_String (Format_Object),
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
				       Clause => Clause_String (Format_Object),
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
				       Clause => Clause_String (Format_Object),
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
				       Clause => Clause_String (Format_Object),
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
				       Clause => Clause_String (Format_Object),
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
				       Clause => Clause_String (Format_Object),
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
			Key : ARM_Index.Index_Key := ARM_Index.Get_Key;
		        Disposition : ARM_Output.Change_Type;
		        use type ARM_Output.Change_Type;
		    begin
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
		            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char,
			    Term,
			    Len);

			-- Set the current unit for future use:
			Format_Object.Unit (1..Len) := Term(1..Len);
			Format_Object.Unit_Len := Len;

		        -- Determine what to do with the "Language-Defined" entry:
		        Calc_Change_Disposition (
		            Format_Object => Format_Object,
			    Version => '2',
			    Operation => ARM_Output.Deletion,
			    Text_Kind => Disposition);
			if Disposition = Do_Not_Display_Text then
			    null; -- Ignore this.
		        elsif Disposition = ARM_Output.None then
			    -- Make reference:
			    ARM_Index.Add_Reusing_Key (
				Term => "Language-Defined Library Units",
				Subterm => Term(1..Len),
			        Kind => ARM_Index.Primary_Term_and_Subterm,
			        Clause => Clause_String (Format_Object),
			        Paragraph => Paragraph_String,
			        Key => Key);
			elsif Disposition = ARM_Output.Deletion then
			    null; -- Ignore this (no change info in the index).
		        else -- Insertion.
			    raise Program_Error; -- An insertion inside of a deletion command!
	    		end if;

			Check_Paragraph;
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Index.Add_Reusing_Key (
				Term => Term(1..Len),
				Kind => ARM_Index.Primary_Term,
			        Clause => Clause_String (Format_Object),
			        Paragraph => Paragraph_String,
			        Key => Key);

		        ARM_Subindex.Insert (
				Subindex_Object => Format_Object.Package_Index,
				Entity => Term(1..Len),
				Kind => ARM_Subindex.Top_Level,
				Clause => Clause_String (Format_Object),
				Paragraph => Paragraph_String,
				Key => Key);

		        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		            -- Remove the "RootLibUnit" record.
		    end;

		when Index_Child_Unit =>
		    -- @ChildUnit{Parent=[<parent>],Child=[<child>]}
		    -- Generates three index entries: An index entry for <child>, with a secondary
		    -- of "@i{child of} <parent>", an index entry for "Language-Defined
		    -- Library Units" with a secondary entry of <parent>.<child>,
		    -- and an index entry for <parent>.<child>.
		    Child_Unit (Format_Object.Package_Index,
	                Format_Object,
	                Output_Object);


		when Index_Subprogram_Child_Unit =>
		    -- @ChildUnit{Parent=[<parent>],Child=[<child>]}
		    -- Generates three index entries: An index entry for <child>, with a secondary
		    -- of "@i{child of} <parent>", an index entry for "Language-Defined
		    -- Library Units" with a secondary entry of <parent>.<child>,
		    -- and an index entry for <parent>.<child>.
		    Child_Unit (Format_Object.Subprogram_Index,
	                Format_Object,
	                Output_Object);

		when Index_Type =>
		    -- @AdaTypeDefn{<defn>}
		    -- Generates two index entries: one for <defn> with a
		    -- secondary entry of "@i{in} <Unit>" (where Unit is
		    -- the unit saved by a previous RootLibUnit or ChildUnit.),
		    -- adds a similar entry to the exception list,
		    -- and second for "Language-Defined Type" with a
		    -- secondary entry of "<defn> @i{in} <Unit>".
		    -- Also outputs the <defn> to the output file.

		    Simple_Subindex_Item (
	                Format_Object.Type_Index,
	                Format_Object,
	                Output_Object,
		        Entity_Kind_Name => "Type");
		    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		        -- Remove the "AdaTypeDefn" record.

		when Index_Subtype =>
		    -- @AdaSubTypeDefn{Name=<defn>,Of=<type>}
		    -- Generates an index entry of "<defn> @i{subtype of}
		    -- <type>" with a secondary entry of "@i{in} <Unit>" (where
		    -- Unit is the unit saved by a previous RootLibUnit or
		    -- ChildUnit.) The entry is added to the type list as well.
		    -- Also outputs the <defn> to the output file.
		    declare
		        Subtype_Name, Type_Name : String(1..80);
		        SLen, TLen : Natural := 0;
	    		Key : ARM_Index.Index_Key := ARM_Index.Get_Key;
			Close_Ch : Character;
		    begin
		        ARM_Input.Check_Parameter_Name (Input_Object,
			            Param_Name => "Name" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			            Is_First => True,
			            Param_Close_Bracket => Close_Ch);
		        if Close_Ch /= ' ' then
		            -- Copy over the term:
		            ARM_Input.Copy_to_String_until_Close_Char (
			        Input_Object,
			        Close_Ch,
			        Subtype_Name,
			        SLen);
		        -- else no parameter. Weird.
		        end if;

		        ARM_Input.Check_Parameter_Name (Input_Object,
		            Param_Name => "Of" & (3..ARM_Input.Command_Name_Type'Last => ' '),
		            Is_First => False,
		            Param_Close_Bracket => Close_Ch);
		        if Close_Ch /= ' ' then
		            -- Copy over the term:
		            ARM_Input.Copy_to_String_until_Close_Char (
			        Input_Object,
			        Close_Ch,
			        Type_Name,
			        TLen);
		        -- else no parameter. Weird.
		        end if;

		        Check_Paragraph;
		        ARM_Output.Index_Target (Output_Object, Key);

			if Format_Object.Unit_Len = 0 then
			    Ada.Text_IO.Put_Line ("** No unit defined for index entry expecting one on line " & ARM_Input.Line_String (Input_Object));

			    ARM_Index.Add_Reusing_Key (
			        Term => Subtype_Name(1..SLen) & " subtype of " &
			            Type_Name(1..TLen),
			        Subterm => "*unknown*",
			        Kind => ARM_Index.Subtype_Declaration_in_Package,
			        Clause => Clause_String (Format_Object),
			        Paragraph => Paragraph_String,
			        Key => Key);

		            ARM_Subindex.Insert (
			        Subindex_Object => Format_Object.Type_Index,
			        Entity => Subtype_Name(1..SLen) & " subtype of " &
			            Type_Name(1..TLen),
			        From_Unit => "*unknown*",
			        Kind => ARM_Subindex.Subtype_In_Unit,
			        Clause => Clause_String (Format_Object),
			        Paragraph => Paragraph_String,
			        Key => Key);
			else
			    ARM_Index.Add_Reusing_Key (
			        Term => Subtype_Name(1..SLen) & " subtype of " &
			            Type_Name(1..TLen),
			        Subterm => Format_Object.Unit(1..Format_Object.Unit_Len),
			        Kind => ARM_Index.Subtype_Declaration_in_Package,
			        Clause => Clause_String (Format_Object),
			        Paragraph => Paragraph_String,
			        Key => Key);

		            ARM_Subindex.Insert (
			        Subindex_Object => Format_Object.Type_Index,
			        Entity => Subtype_Name(1..SLen) & " subtype of " &
			            Type_Name(1..TLen),
			        From_Unit => Format_Object.Unit(1..Format_Object.Unit_Len),
			        Kind => ARM_Subindex.Subtype_In_Unit,
			        Clause => Clause_String (Format_Object),
			        Paragraph => Paragraph_String,
			        Key => Key);
			end if;
		        ARM_Output.Ordinary_Text (Output_Object, Subtype_Name(1..SLen));
		        Format_Object.Last_Non_Space := True;
		        -- Leave the command end marker, let normal processing
		        -- get rid of it.
		    end;

		when Index_Subprogram =>
		    -- @AdaSubDefn{<defn>}
		    -- Generates two index entries: one for <defn> with a
		    -- secondary entry of "@i{in} <Unit>" (where Unit is
		    -- the unit saved by a previous RootLibUnit or ChildUnit.),
		    -- adds a similar entry to the exception list,
		    -- and second for "Language-Defined Subprogram" with a
		    -- secondary entry of "<defn> @i{in} <Unit>".
		    -- Also outputs the <defn> to the output file.
		    Simple_Subindex_Item (
	                Format_Object.Subprogram_Index,
	                Format_Object,
	                Output_Object,
		        Entity_Kind_Name => "Subprogram");
		    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		        -- Remove the "AdaSubDefn" record.

		when Index_Exception =>
		    -- @AdaExcDefn{<defn>}
		    -- Generates and index entries for <defn> with a
		    -- secondary entry of "@i{in} <Unit>" (where Unit is
		    -- the unit saved by a previous RootLibUnit or ChildUnit.),
		    -- and adds a similar entry to the exception list.
		    -- Also outputs the <defn> to the output file.
		    Simple_Subindex_Item (
	                Format_Object.Exception_Index,
	                Format_Object,
	                Output_Object,
		        Entity_Kind_Name => "");
		    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		        -- Remove the "AdaExcDefn" record.

		when Index_Object =>
		    -- @AdaObjDefn{<defn>}
		    -- Generates and index entries for <defn> with a
		    -- secondary entry of "@i{in} <Unit>" (where Unit is
		    -- the unit saved by a previous RootLibUnit or ChildUnit.),
		    -- and adds a similar entry to the exception list.
		    -- Also outputs the <defn> to the output file.
		    Simple_Subindex_Item (
	                Format_Object.Object_Index,
	                Format_Object,
	                Output_Object,
		        Entity_Kind_Name => "");
		    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		        -- Remove the "AdaObjDefn" record.

		when Index_Package =>
		    -- @AdaObjDefn{<defn>}
		    -- Generates and index entries for <defn> with a
		    -- secondary entry of "@i{in} <Unit>" (where Unit is
		    -- the unit saved by a previous RootLibUnit or ChildUnit.),
		    -- and adds a similar entry to the package list.
		    -- Also outputs the <defn> to the output file.
		    Simple_Subindex_Item (
	                Format_Object.Package_Index,
	                Format_Object,
	                Output_Object,
		        Entity_Kind_Name => "");
		    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		        -- Remove the "AdaPackDefn" record.

		when Index_Other =>
		    -- @AdaDefn{<defn>}
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
				       Clause => Clause_String (Format_Object),
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
				       Clause => Clause_String (Format_Object),
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
				       Clause => Clause_String (Format_Object),
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
				       Clause => Clause_String (Format_Object),
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Index.Add (Term => Param(1..Len) & " attribute",
				       Kind => ARM_Index.Primary_Term,
				       Clause => Clause_String (Format_Object),
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
				       Clause => Clause_String (Format_Object),
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Index.Add (Term => Param(1..Len) & " pragma",
				       Kind => ARM_Index.Primary_Term,
				       Clause => Clause_String (Format_Object),
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Output.Ordinary_Text (Output_Object,
			    Param(1..Len));
		        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		            -- Remove the "Index_Pragma" record.
		    end;

		when Index_Aspect =>
		    -- This command indexes an aspect name.
		    -- This calls Defn2("aspects", <param>), and
		    -- Defn(<param> "aspect")
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
			ARM_Index.Add (Term => "aspects",
				       Subterm => Param(1..Len),
				       Kind => ARM_Index.Primary_Term_and_Subterm,
				       Clause => Clause_String (Format_Object),
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Index.Add (Term => Param(1..Len) & " aspect",
				       Kind => ARM_Index.Primary_Term,
				       Clause => Clause_String (Format_Object),
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

		        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		            -- Remove the "Index_Aspect" record.
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
			RHS_Close_Ch : Character;
		    begin
			Get_Syntax_Parameters (Has_Version => False,
					       RHS_Close_Ch => RHS_Close_Ch);

			Gen_Syntax_Rule (ARM_Output.None, RHS_Close_Ch);
		    end;

		when Added_Syntax_Rule =>
		    -- @AddedSyn{Version=[Version],[Tabs=<Tabset>, ]LHS=<Non-terminal>, RHS=<Production>}
		    -- Marks an added syntax production of the form:
		    --    @nt<Non-Terminal> ::= <Production>
		    -- See Syntax_Rule for the details.
		    declare
			RHS_Close_Ch : Character;
			Disposition : ARM_Output.Change_Type;
		    begin
			Get_Syntax_Parameters (Has_Version => True,
					       RHS_Close_Ch => RHS_Close_Ch);
			if Format_Object.In_Change then
			    Ada.Text_IO.Put_Line ("  ** In change for AddedSyn on line " & ARM_Input.Line_String (Input_Object));
			    raise Program_Error;
			end if;

		        Calc_Change_Disposition (
		            Format_Object => Format_Object,
			    Version => Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version,
			    Operation => ARM_Output.Insertion,
			    Text_Kind => Disposition);

			Gen_Syntax_Rule (Disposition, RHS_Close_Ch);
		    end;

		when Deleted_Syntax_Rule =>
		    -- @DeletedSyn{Version=[Version],[Tabs=<Tabset>, ]LHS=<Non-terminal>, RHS=<Production>}
		    -- Marks a deleted syntax production of the form:
		    --    @nt<Non-Terminal> ::= <Production>
		    -- See Syntax_Rule for the details.
		    declare
			RHS_Close_Ch : Character;
			Disposition : ARM_Output.Change_Type;
		    begin
			Get_Syntax_Parameters (Has_Version => True,
					       RHS_Close_Ch => RHS_Close_Ch);
			if Format_Object.In_Change then
			    Ada.Text_IO.Put_Line ("  ** In change for DeletedSyn on line " & ARM_Input.Line_String (Input_Object));
			    raise Program_Error;
			end if;

		        Calc_Change_Disposition (
		            Format_Object => Format_Object,
			    Version => Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version,
			    Operation => ARM_Output.Deletion,
			    Text_Kind => Disposition);

			Gen_Syntax_Rule (Disposition, RHS_Close_Ch);
		    end;

		when Syntax_Term | Syntax_Term_Undefined =>
		    -- Marks a non-terminal name in the production of a syntax
		    -- rule. Generates the term in the same style as
		    -- @nt (Non_Terminal). "Undefined" means the term is
		    -- not formally defined (like the character class names in
		    -- the Ada standard).
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
			Defined : constant Boolean :=
			   Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command =
			       Syntax_Term;
		    begin
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char,
			    Name, Len);
			if Format_Object.Syntax_NT_Len /= 0 then
			    -- Generate a syntax cross-reference entry.
			    declare
				NT : constant String := Get_NT;
			    begin
				if NT /= "" then
			            ARM_Syntax.Add_Xref (
			                 Name => Name(1..Len),
			                 Used_In => Get_NT,
			                 Clause => Clause_String (Format_Object),
				         Defined => Defined);
				-- else this is a deleted production that is
				-- still displayed; forget the XRef.
				end if;
			    end;
			end if;

			-- Index the non-terminal:
			ARM_Index.Add (Term => Name(1..Len),
				       Kind => ARM_Index.Syntax_NT_Used,
				       Clause => Clause_String (Format_Object),
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

		        -- Set the appropriate style:
		        Check_Paragraph;
			declare
			    Swiss_Format : ARM_Output.Format_Type :=
				Format_Object.Text_Format;
			begin
			    Swiss_Format.Font := ARM_Output.Swiss;
		            ARM_Output.Text_Format (Output_Object,
						    Swiss_Format);
			end;
			if not Defined then
			    -- No linking to do.
			    ARM_Output.Ordinary_Text (Output_Object, Name(1..Len));
			elsif Format_Object.Link_Non_Terminals then
			    declare
				Lower_NT : constant String :=
				    Ada.Characters.Handling.To_Lower (Name(1..Len));
				Clause : constant String :=
				    ARM_Syntax.Non_Terminal_Clause (Lower_NT);
				Target : constant ARM_Syntax.Target_Type :=
				    ARM_Syntax.Non_Terminal_Link_Target (Lower_NT);
			    begin
				if Clause = "" then -- Not found. No link, but error message:
				    Ada.Text_IO.Put_Line ("  ** Unknown non-terminal in syntax production " &
					Name(1..Len) & " on line " & ARM_Input.Line_String (Input_Object));
				    ARM_Output.Ordinary_Text (Output_Object, Name(1..Len));
				else
				    ARM_Output.Local_Link (Output_Object, Text => Name(1..Len),
					Target => Target, Clause_Number => Clause);
				end if;
			    end;
			else
			    ARM_Output.Ordinary_Text (Output_Object, Name(1..Len));
			end if;
		        ARM_Output.Text_Format (Output_Object,
					        Format_Object.Text_Format); -- Reset the format.
			Format_Object.Last_Non_Space := True;
		    end;
		    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Syntax Term)");

		when Syntax_Prefix =>
		    -- Marks the prefix of a non-terminal. Writes italized
		    -- text in the current font.
		    -- Set the appropriate style:
		    Check_Paragraph;
		    Format_Object.Text_Format.Italic := True;
		    ARM_Output.Text_Format (Output_Object,
					    Format_Object.Text_Format);

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
			Format_Object.Glossary_Change_Kind := ARM_Database.None;
			    -- No change for this command.
			Format_Object.Add_to_Glossary := True;
			    -- Always add it.

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
					       Clause => Clause_String (Format_Object),
					       Paragraph => Paragraph_String,
					       Key => Key);
				ARM_Output.Index_Target (Output_Object, Key);
				Format_Object.Glossary_Displayed := True;
			    elsif Format_Object.Include_Annotations then
			        Check_End_Paragraph; -- End any paragraph that we're in.
			        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Last_Subhead_Paragraph := Format_Object.Last_Paragraph_Subhead_Type;
			        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Subhead_Paragraph := Format_Object.Next_Paragraph_Subhead_Type;
			        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Paragraph_Format := Format_Object.Next_Paragraph_Format_Type;
			        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Tab_Stops := Format_Object.Paragraph_Tab_Stops;
			        Format_Object.Next_Paragraph_Format_Type := Glossary_Marker;
			        Format_Object.Next_Paragraph_Subhead_Type := Glossary_Marker;
			        Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;
			        Display_Index_Entry (Format_Object.Glossary_Term (1..Format_Object.Glossary_Term_Len)); -- Includes Check_Paragraph.
			        Format_Object.Glossary_Displayed := True;
				-- Note: The term is indexed in the glossary,
				-- but not here.
			    else -- No annotations, "To_Glossary"
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
			        Format_Object.Glossary_Displayed := False;
				-- Note: The term is indexed in the glossary,
				-- but not here.
			    end if;
			end if;
		    end;

		when Change_To_Glossary | Change_To_Glossary_Also =>
		    -- This is a change glossary command.
		    -- It is of the form
		    -- @ChgToGlossary(Version=[<version>],Kind=(<kind>),Term=[<term>], Text=[<text>])
		    -- We will store the term and definition in the glossary
		    -- database. We also have to pass through the Text
		    -- parameter, either to the regular text (for
		    -- ChgToGlossaryAlso) or the AARM (for ChgToGlossary).

		    declare
			Close_Ch : Character;
			Key : ARM_Index.Index_Key;
		        Kind : ARM_Database.Paragraph_Change_Kind_Type;
			use type ARM_Database.Paragraph_Change_Kind_Type;
			Local_Change : ARM_Output.Change_Type;
		    begin
		        Get_Change_Version (Is_First => True,
			    Version => Format_Object.Glossary_Version);
			    -- Read a parameter named "Version".

		        Get_Change_Kind (Kind);
			    -- Read a parameter named "Kind".

			Format_Object.Glossary_Change_Kind := Kind;

			if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Change_To_Glossary_Also then
			    -- The text just goes straight to the file. It will
			    -- get formatted appropriately. So we only need to
			    -- figure out whether it will get indexed and displayed
			    -- in the Glossary.
			    Format_Object.Glossary_Displayed := True;
			    Local_Change := ARM_Output.None;
		            if Format_Object.Changes = ARM_Format.Old_Only and then
			        Format_Object.Glossary_Version > '0' then
			        -- Old only, don't display it (and it won't be
			        -- inserted, either).
			        Format_Object.Add_to_Glossary := False;
		            elsif (Format_Object.Glossary_Change_Kind = ARM_Database.Inserted or else
			           Format_Object.Glossary_Change_Kind = ARM_Database.Inserted_Normal_Number) then
			        if Format_Object.Glossary_Version <= Format_Object.Change_Version then
			            Format_Object.Add_to_Glossary := True;
			        else --This reference is too new, ignore it.
			            Format_Object.Glossary_Displayed := False;
			            Format_Object.Add_to_Glossary := False;
			        end if;
				Format_Object.Glossary_Change_Kind := ARM_Database.Inserted;
		            elsif (Format_Object.Glossary_Change_Kind = ARM_Database.Deleted or else
			           Format_Object.Glossary_Change_Kind = ARM_Database.Deleted_Inserted_Number or else
			           Format_Object.Glossary_Change_Kind = ARM_Database.Deleted_No_Delete_Message or else
			           Format_Object.Glossary_Change_Kind = ARM_Database.Deleted_Inserted_Number_No_Delete_Message) then
			        Format_Object.Add_to_Glossary := True;
				Format_Object.Glossary_Change_Kind := ARM_Database.Deleted;
		            else -- we always display it.
			        Format_Object.Add_to_Glossary := True;
		            end if;
			else
		            if (Format_Object.Glossary_Change_Kind = ARM_Database.Inserted or else
			        Format_Object.Glossary_Change_Kind = ARM_Database.Inserted_Normal_Number) then
				Format_Object.Glossary_Change_Kind := ARM_Database.Inserted;
				Calc_Change_Disposition
			            (Format_Object => Format_Object,
				     Version => Format_Object.Glossary_Version,
				     Operation => ARM_Output.Insertion,
				     Text_Kind => Local_Change);
				case Local_Change is
				    when Do_Not_Display_Text =>
				        Format_Object.Glossary_Displayed := False;
				        Format_Object.Add_to_Glossary := False;
				        Local_Change := ARM_Output.None;
				    when ARM_Output.None|ARM_Output.Insertion =>
			                Format_Object.Glossary_Displayed :=
					    Format_Object.Include_Annotations;
			                Format_Object.Add_to_Glossary := True;
				    when ARM_Output.Deletion =>
					raise Program_Error;
				end case;
		            elsif (Format_Object.Glossary_Change_Kind = ARM_Database.Deleted or else
			           Format_Object.Glossary_Change_Kind = ARM_Database.Deleted_Inserted_Number or else
			           Format_Object.Glossary_Change_Kind = ARM_Database.Deleted_No_Delete_Message or else
			           Format_Object.Glossary_Change_Kind = ARM_Database.Deleted_Inserted_Number_No_Delete_Message) then
				Format_Object.Glossary_Change_Kind := ARM_Database.Deleted;
				Calc_Change_Disposition
			            (Format_Object => Format_Object,
				     Version => Format_Object.Glossary_Version,
				     Operation => ARM_Output.Deletion,
				     Text_Kind => Local_Change);
				case Local_Change is
				    when Do_Not_Display_Text =>
				        Format_Object.Glossary_Displayed := False;
				        Format_Object.Add_to_Glossary := True;
					-- We still add this to the glossary so that
					-- the deleted paragraph message can be displayed for it.
				        Local_Change := ARM_Output.None;
				    when ARM_Output.None|ARM_Output.Deletion =>
			                Format_Object.Glossary_Displayed :=
					    Format_Object.Include_Annotations;
			                Format_Object.Add_to_Glossary := True;
				    when ARM_Output.Insertion =>
					raise Program_Error;
				end case;
		            else -- we always display it.
			        Format_Object.Glossary_Displayed :=
				    Format_Object.Include_Annotations;
			        Format_Object.Add_to_Glossary := True;
			        Local_Change := ARM_Output.None;
		            end if;
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Term" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
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

			    if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Command = Change_To_Glossary_Also then
				-- The text just goes straight to the file.
				if Format_Object.Add_to_Glossary then
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
					           Clause => Clause_String (Format_Object),
					           Paragraph => Paragraph_String,
					           Key => Key);
				    ARM_Output.Index_Target (Output_Object, Key);
				-- else no indexing.
				end if;
			    elsif Format_Object.Glossary_Displayed then -- Change_To_Glossary
				-- Create the AARM annotation:
				Check_End_Paragraph; -- End any paragraph that we're in.
				Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Last_Subhead_Paragraph := Format_Object.Last_Paragraph_Subhead_Type;
				Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Subhead_Paragraph := Format_Object.Next_Paragraph_Subhead_Type;
				Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Paragraph_Format := Format_Object.Next_Paragraph_Format_Type;
				Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Tab_Stops := Format_Object.Paragraph_Tab_Stops;
				Format_Object.Next_Paragraph_Format_Type := Glossary_Marker;
				Format_Object.Next_Paragraph_Subhead_Type := Glossary_Marker;
				Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;
			        Format_Object.Next_Paragraph_Version := Format_Object.Glossary_Version;
			        Format_Object.Next_Paragraph_Change_Kind := Kind;

                                -- We assume no outer changes;
			        -- set new change state:
			        Format_Object.Text_Format.Change := Local_Change;
			        Format_Object.Text_Format.Version :=
				   Format_Object.Glossary_Version;
			        Format_Object.Text_Format.Added_Version := '0';
			            -- Change the state *before* outputting the
				    -- paragraph header, so the AARM prefix is included.
			        Display_Index_Entry (Format_Object.Glossary_Term (1..Format_Object.Glossary_Term_Len)); -- Includes Check_Paragraph.

			        Format_Object.Text_Format.Change := ARM_Output.None; -- Undo (header) change.
			        Format_Object.Text_Format.Version := '0';

			    elsif Format_Object.Add_to_Glossary then -- Change_To_Glossary
				-- No AARM annotation:
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

				-- Note: The term is indexed in the glossary,
				-- but not here.
			    else
			        -- Skip the text (it won't be used at all):
			        ARM_Input.Skip_until_Close_Char (Input_Object,
				    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char);
			        ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
			    end if;
			end if;
		    end;

		when Implementation_Defined =>
		    -- Store an "implementation-defined" entry for the parameter;
		    -- also save the clause and paragraph reference.

		    ARM_Input.Start_Recording (Input_Object);

		    Format_Object.Impdef_Change_Kind := ARM_Database.None;
		    Format_Object.Impdef_Version := '0';
		    Format_Object.Impdef_Initial_Version := '0';

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

		    if Format_Object.Include_Annotations then
		        Check_End_Paragraph; -- End any paragraph that we're in.
		        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Last_Subhead_Paragraph := Format_Object.Last_Paragraph_Subhead_Type;
		        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Subhead_Paragraph := Format_Object.Next_Paragraph_Subhead_Type;
		        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Next_Paragraph_Format := Format_Object.Next_Paragraph_Format_Type;
		        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Old_Tab_Stops := Format_Object.Paragraph_Tab_Stops;
		        Format_Object.Next_Paragraph_Format_Type := Bare_Annotation;
		        Format_Object.Next_Paragraph_Subhead_Type := Bare_Annotation;
		        Format_Object.Paragraph_Tab_Stops := ARM_Output.NO_TABS;
		        Check_Paragraph;
		        declare
			    Bold_Format : ARM_Output.Format_Type :=
				Format_Object.Text_Format;
			begin
			    Bold_Format.Bold := True; -- Change only the boldface.
			    ARM_Output.Text_Format (Output_Object,
						    Bold_Format);
			end;
		        ARM_Output.Ordinary_Text (Output_Object,
			     Text => "Implementation defined: ");
		        ARM_Output.Text_Format (Output_Object,
				                Format_Object.Text_Format); -- Reset style.
		        Format_Object.Last_Paragraph_Subhead_Type := Bare_Annotation;
		        Format_Object.Last_Non_Space := False;
		    else -- No annotations
		        -- Skip the text:
		        ARM_Input.Skip_until_Close_Char (Input_Object,
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char);
		        ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
	            end if;

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
				       Clause => Clause_String (Format_Object),
				       Paragraph => Paragraph_String,
				       Key => Key);
			ARM_Output.Index_Target (Output_Object, Key);

			ARM_Index.Add (Term => Format_Object.Attr_Name (1 .. Format_Object.Attr_Name_Len) & " attribute",
				       Kind => ARM_Index.Primary_Term,
				       Clause => Clause_String (Format_Object),
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

--Ada.Text_IO.Put_Line ("%% Pragma - normal initialization.");
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

			Disposition : ARM_Output.Change_Type;
			use type ARM_Output.Change_Type;
		     begin
			Get_Change_Version (Is_First => True,
					    Version => Version);
		        ARM_Input.Get_Char (Input_Object, Ch);
		        if Ch /= ',' then
			    Ada.Text_IO.Put_Line ("  ** Missing comma for AddedPragmaSyn on line " & ARM_Input.Line_String (Input_Object));
			    ARM_Input.Replace_Char (Input_Object);
		        end if;
			Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version := Version;
			Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Prev_Change_Version := Version; -- Don't have or need an InitialVersion parameter.

		        Calc_Change_Disposition (
		            Format_Object => Format_Object,
			    Version => Version,
			    Operation => ARM_Output.Insertion,
			    Text_Kind => Disposition);

		        -- All we have to do here is output a couple of
		        -- hard spaces (if anything will be displayed) and then
			-- start recording. The inner @Chg will handle the
			-- formatting for this.
--Ada.Text_IO.Put_Line ("%% Added pragma.");
		        Check_Paragraph;
		        if Disposition /= Do_Not_Display_Text then
			    ARM_Output.Hard_Space (Output_Object);
			    ARM_Output.Hard_Space (Output_Object);
		        -- else nothing to display.
		        end if;
		        ARM_Input.Start_Recording (Input_Object);
		        -- Just handle the text normally.
		    end;

		when Deleted_Pragma_Syntax =>
		     -- @DeletedPragmaSyntax{Version=[<Version>],InitialVersion=[<InitialVersion>],<Text>}
		     -- Defines a pragma. The text can contain arbitrary commands;
		     --	it will be run through the full evaluation code.
		     --	The text is also sent to a database used to later create
		     --	Annex L.
		     -- Note that these are indented slightly more than regular
		     -- syntax text. We handle that by adding a couple of
		     -- spaces before the text.

		     declare
			Close_Ch, Ch : Character;
			Version : ARM_Contents.Change_Version_Type := '0';
			Initial_Version : ARM_Contents.Change_Version_Type := '0';

			Disposition : ARM_Output.Change_Type;
			use type ARM_Output.Change_Type;
		     begin
			Get_Change_Version (Is_First => True,
					    Version => Version);

			-- Now, get InitialVersion.
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "InitialVersion" & (15..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);

		        ARM_Input.Get_Char (Input_Object, Initial_Version);
		        ARM_Input.Get_Char (Input_Object, Ch);
		        if Ch /= Close_Ch then
			    Ada.Text_IO.Put_Line ("  ** Bad close for InitialVersion parameter on line " &
			        ARM_Input.Line_String (Input_Object));
			    ARM_Input.Replace_Char (Input_Object);
		        end if;

		        ARM_Input.Get_Char (Input_Object, Ch);
		        if Ch /= ',' then
			    Ada.Text_IO.Put_Line ("  ** Missing comma for AddedPragmaSyn on line " & ARM_Input.Line_String (Input_Object));
			    ARM_Input.Replace_Char (Input_Object);
		        end if;
			Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version := Version;
			Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Prev_Change_Version := Initial_Version;

		        Calc_Change_Disposition (
		            Format_Object => Format_Object,
			    Version => Version,
			    Operation => ARM_Output.Deletion,
			    Text_Kind => Disposition);

--Ada.Text_IO.Put_Line ("%% Deleted pragma.");
		        -- All we have to do here is output a couple of
		        -- hard spaces (if anything will be displayed) and then
			-- start recording. The inner @Chg will handle the
			-- formatting for this.
		        Check_Paragraph;
		        if Disposition /= Do_Not_Display_Text then
			    ARM_Output.Hard_Space (Output_Object);
			    ARM_Output.Hard_Space (Output_Object);
		        -- else nothing to display.
		        end if;
		        ARM_Input.Start_Recording (Input_Object);
		        -- Just handle the text normally.
		    end;

		-- Clause title and reference commands:

		when Labeled_Section | Labeled_Section_No_Break |
		     Labeled_Annex | Labeled_Informative_Annex |
		     Labeled_Normative_Annex | Labeled_Clause |
		     Labeled_Subclause | Labeled_Subsubclause |
		     Unnumbered_Section =>
		    -- Load the title into the Title string:
		    declare
			Title : ARM_Contents.Title_Type;
			Title_Length : Natural;
		        Level : ARM_Contents.Level_Type;
		    begin
		        ARM_Input.Copy_to_String_until_Close_Char (
			    Input_Object,
			    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char,
			    Title, Title_Length);
		        if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Subclause then
			    Format_Object.Clause_Number :=
				(Section   => Format_Object.Clause_Number.Section,
				 Clause    => Format_Object.Clause_Number.Clause,
				 Subclause => Format_Object.Clause_Number.Subclause + 1,
				 Subsubclause => 0);
			    Level := ARM_Contents.Subclause;
		        elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Subsubclause then
			    Format_Object.Clause_Number.Subsubclause :=
				Format_Object.Clause_Number.Subsubclause + 1;
			    Level := ARM_Contents.Subsubclause;
		        elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Clause then
			    Format_Object.Clause_Number :=
				(Section   => Format_Object.Clause_Number.Section,
				 Clause    => Format_Object.Clause_Number.Clause + 1,
				 Subclause => 0, Subsubclause => 0);
			    Level := ARM_Contents.Clause;
		        elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Unnumbered_Section then
			    Format_Object.Unnumbered_Section :=
				Format_Object.Unnumbered_Section + 1;
			    Format_Object.Clause_Number :=
			        (Section   => 0,
				 Clause    => Format_Object.Unnumbered_Section,
				 Subclause => 0, Subsubclause => 0);
			    Level := ARM_Contents.Unnumbered_Section;
		        elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Section or else
			      Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Section_No_Break then
			    Format_Object.Clause_Number :=
				(Section   => Format_Object.Clause_Number.Section, -- Will be set elsewhere.
				 Clause    => 0,
				 Subclause => 0, Subsubclause => 0);
			    Level := ARM_Contents.Section;
			elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Annex then
			    Format_Object.Clause_Number :=
				(Section   => Format_Object.Clause_Number.Section, -- Will be set elsewhere.
				 Clause    => 0,
				 Subclause => 0, Subsubclause => 0);
			    Level := ARM_Contents.Plain_Annex;
			elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Normative_Annex then
			    Format_Object.Clause_Number :=
				(Section   => Format_Object.Clause_Number.Section, -- Will be set elsewhere.
				 Clause    => 0,
				 Subclause => 0, Subsubclause => 0);
			    Level := ARM_Contents.Normative_Annex;
			else -- Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Informative_Annex then
			    Format_Object.Clause_Number :=
				(Section   => Format_Object.Clause_Number.Section, -- Will be set elsewhere.
				 Clause    => 0,
				 Subclause => 0, Subsubclause => 0);
			    Level := ARM_Contents.Informative_Annex;
			end if;
			Title(Title_Length+1 .. Title'Last) :=
			    (others => ' ');

			begin
			    declare
			        Clause_Number : constant String :=
				    ARM_Contents.Lookup_Clause_Number (Title);
			    begin
			        Check_End_Paragraph; -- End any paragraph that we're in.
			        if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Section_No_Break then
				    ARM_Output.Clause_Header (Output_Object,
				        Title(1..Title_Length),
					Level => ARM_Contents.Section,
					Clause_Number => Clause_Number,
					No_Page_Break => True);
				else -- Other cases:
				    ARM_Output.Clause_Header (Output_Object,
				        Title(1..Title_Length),
					Level => Level,
					Clause_Number => Clause_Number);
			        end if;
			        -- Check that the section numbers match the title:
			        if Ada.Characters.Handling.To_Lower (Title) /=
			           Ada.Characters.Handling.To_Lower (ARM_Contents.Lookup_Title (
				      Level, Format_Object.Clause_Number)) then
				    Ada.Text_IO.Put_Line ("** Unable to match title with section numbers, line " & ARM_Input.Line_String (Input_Object));
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
		    if Format_Object.Use_ISO_2004_Note_Format then
			-- Reset the note number:
		        Format_Object.Next_Note := 1;
		    elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Section or else
		          Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Section_No_Break or else
		          Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Annex or else
		          Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Informative_Annex or else
		          Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Normative_Annex then
		        -- Reset the note number, only for sections:
		        Format_Object.Next_Note := 1;
		    end if;
		    -- Reset the subhead:
		    Format_Object.Last_Paragraph_Subhead_Type := Plain;
		    Format_Object.Next_Paragraph_Format_Type := Plain;

		    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Header)");

		when Labeled_Revised_Annex |
		     Labeled_Revised_Informative_Annex |
		     Labeled_Revised_Normative_Annex |
		     Labeled_Revised_Section |
		     Labeled_Revised_Clause |
		     Labeled_Revised_Subclause |
		     Labeled_Revised_Subsubclause =>
		    -- Load the title into the Title string:
		    declare
			New_Title : ARM_Contents.Title_Type;
			New_Title_Length : Natural;
			Old_Title : ARM_Contents.Title_Type;
			Old_Title_Length : Natural;
			Close_Ch  : Character;
			Version   : ARM_Contents.Change_Version_Type := '0';
			Initial_Version : ARM_Contents.Change_Version_Type := '0';
			Level     : ARM_Contents.Level_Type;
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
			            ARM_Input.Get_Char (Input_Object, Ch);
				    Initial_Version := Ch;
			            ARM_Input.Get_Char (Input_Object, Ch);
			            if Ch /= Close_Ch then
				        Ada.Text_IO.Put_Line ("  ** Bad close for InitialVersion parameter on line " &
					    ARM_Input.Line_String (Input_Object));
				        ARM_Input.Replace_Char (Input_Object);
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
			        New_Title, New_Title_Length);
			    New_Title(New_Title_Length+1 .. New_Title'Last) :=
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
		        ARM_Input.Get_Char (Input_Object, Close_Ch);
		        if Close_Ch /= Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char then
			    Ada.Text_IO.Put_Line ("  ** Bad close for Labeled_Revised_(SubClause|Annex) on line " & ARM_Input.Line_String (Input_Object));
			    ARM_Input.Replace_Char (Input_Object);
		        end if;

		        if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Revised_Subclause then
			    Format_Object.Clause_Number :=
				(Section   => Format_Object.Clause_Number.Section,
				 Clause    => Format_Object.Clause_Number.Clause,
				 Subclause => Format_Object.Clause_Number.Subclause + 1,
				 Subsubclause => 0);
			    Level := ARM_Contents.Subclause;
		        elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Revised_Subsubclause then
			    Format_Object.Clause_Number.Subsubclause :=
				Format_Object.Clause_Number.Subsubclause + 1;
			    Level := ARM_Contents.Subsubclause;
		        elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Revised_Clause then
			    Format_Object.Clause_Number :=
				(Section   => Format_Object.Clause_Number.Section,
				 Clause    => Format_Object.Clause_Number.Clause + 1,
				 Subclause => 0, Subsubclause => 0);
			    Level := ARM_Contents.Clause;
		        elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Revised_Section then
			    Format_Object.Clause_Number :=
				(Section   => Format_Object.Clause_Number.Section, -- Will be set elsewhere.
				 Clause    => 0,
				 Subclause => 0, Subsubclause => 0);
			    Level := ARM_Contents.Section;
			elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Revised_Annex then
			    Format_Object.Clause_Number :=
				(Section   => Format_Object.Clause_Number.Section, -- Will be set elsewhere.
				 Clause    => 0,
				 Subclause => 0, Subsubclause => 0);
			    Level := ARM_Contents.Plain_Annex;
			elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Revised_Normative_Annex then
			    Format_Object.Clause_Number :=
				(Section   => Format_Object.Clause_Number.Section, -- Will be set elsewhere.
				 Clause    => 0,
				 Subclause => 0, Subsubclause => 0);
			    Level := ARM_Contents.Normative_Annex;
			else -- Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Revised_Informative_Annex then
			    Format_Object.Clause_Number :=
				(Section   => Format_Object.Clause_Number.Section, -- Will be set elsewhere.
				 Clause    => 0,
				 Subclause => 0, Subsubclause => 0);
			    Level := ARM_Contents.Informative_Annex;
			end if;

			begin
			    declare
			        Clause_Number : constant String :=
				    ARM_Contents.Lookup_Clause_Number (New_Title);

				New_Disposition : ARM_Output.Change_Type;
				Old_Disposition : ARM_Output.Change_Type;
				use type ARM_Output.Change_Type;
			    begin
			        Check_End_Paragraph; -- End any paragraph that we're in.
			        -- Check that the section numbers match the title:
			        if Ada.Characters.Handling.To_Lower (New_Title) /=
			           Ada.Characters.Handling.To_Lower (ARM_Contents.Lookup_Title (
				      Level, Format_Object.Clause_Number)) then
				    Ada.Text_IO.Put_Line ("** Unable to match title with section numbers, line " & ARM_Input.Line_String (Input_Object));
			        end if;

			        Calc_Change_Disposition (
		            	    Format_Object => Format_Object,
				    Version => Version,
				    Operation => ARM_Output.Insertion,
				    Text_Kind => New_Disposition);
				    -- Note: We use insertion here because
				    -- we want to decide what to do with
				    -- the New part.
			        Calc_Change_Disposition (
		            	    Format_Object => Format_Object,
				    Version => Initial_Version,
				    Operation => ARM_Output.Insertion,
				    Text_Kind => Old_Disposition);

			        if New_Disposition = Do_Not_Display_Text then
			            if Old_Disposition = Do_Not_Display_Text then
					null; -- Show nothing.
				    elsif Old_Disposition = ARM_Output.None then
			                -- Use the old only:
				        ARM_Output.Clause_Header (Output_Object,
				            Old_Title(1..Old_Title_Length),
				            Level => Level,
				            Clause_Number => Clause_Number);
				    elsif Old_Disposition = ARM_Output.Deletion then
			                raise Program_Error; -- A deletion inside of an insertion command!
				    else -- an insertion of the Old. Show this like an added item:
				        ARM_Output.Revised_Clause_Header (Output_Object,
				            New_Header_Text => Old_Title(1..Old_Title_Length),
				            Old_Header_Text => "",
				            Level => Level,
				            Version => Initial_Version,
				            Old_Version => '0',
				            Clause_Number => Clause_Number);
				    end if;
			        elsif New_Disposition = ARM_Output.None then
				    -- Use the new only:
				    ARM_Output.Clause_Header (Output_Object,
				        New_Title(1..New_Title_Length),
				        Level => Level,
				        Clause_Number => Clause_Number);
					-- In this case, we have no sane
					-- way to show the old, so we hope that
					-- isn't expected.
			        elsif New_Disposition = ARM_Output.Deletion then
			            raise Program_Error; -- A deletion inside of an insertion command!
			        else -- Insertion.
				    if Format_Object.Changes = ARM_Format.New_Changes or else
				        Old_Disposition = Do_Not_Display_Text then
				        ARM_Output.Revised_Clause_Header (Output_Object,
					    New_Header_Text => New_Title(1..New_Title_Length),
					    Old_Header_Text => " ",
					    Level => Level,
					    Version => Version,
					    Old_Version => '0',
					    Clause_Number => Clause_Number);
				    elsif Old_Disposition = ARM_Output.None then
					-- Show old without any insertion marks:
				        ARM_Output.Revised_Clause_Header (Output_Object,
					    New_Header_Text => New_Title(1..New_Title_Length),
					    Old_Header_Text => Old_Title(1..Old_Title_Length),
					    Level => Level,
					    Version => Version,
					    Old_Version => '0',
					    Clause_Number => Clause_Number);
				    elsif Old_Disposition = ARM_Output.Deletion then
			                raise Program_Error; -- A deletion inside of an insertion command!
				    else -- An insertion of the Old item:
				        ARM_Output.Revised_Clause_Header (Output_Object,
				            New_Header_Text => New_Title(1..New_Title_Length),
				            Old_Header_Text => Old_Title(1..Old_Title_Length),
					    Level => Level,
					    Version => Version,
					    Old_Version => Initial_Version,
					    Clause_Number => Clause_Number);
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
		        if Format_Object.Use_ISO_2004_Note_Format then
			    -- Reset the note number:
		            Format_Object.Next_Note := 1;
		        elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Revised_Section or else
		              Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Revised_Annex or else
		              Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Revised_Informative_Annex or else
		              Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Revised_Normative_Annex then
		            -- Reset the note number, only for sections:
		            Format_Object.Next_Note := 1;
		        end if;
		        -- Reset the subhead:
		        Format_Object.Last_Paragraph_Subhead_Type := Plain;
		        Format_Object.Next_Paragraph_Format_Type := Plain;

			Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Header)");
		    end;

		when Labeled_Added_Annex |
		     Labeled_Added_Informative_Annex |
		     Labeled_Added_Normative_Annex |
		     Labeled_Added_Section |
		     Labeled_Added_Clause |
		     Labeled_Added_Subclause |
		     Labeled_Added_Subsubclause =>
		    -- Load the title into the Title string:
		    declare
			New_Title : ARM_Contents.Title_Type;
			New_Title_Length : Natural;
			Ch : Character;
			Version : ARM_Contents.Change_Version_Type := '0';
		        Level : ARM_Contents.Level_Type;
			Disposition : ARM_Output.Change_Type;
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
			        New_Title, New_Title_Length);
			    New_Title(New_Title_Length+1 .. New_Title'Last) :=
				(others => ' ');
			end if;
		        ARM_Input.Get_Char (Input_Object, Ch);
		        if Ch /= Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char then
			    Ada.Text_IO.Put_Line ("  ** Bad close for Labeled_Added_(Sub)Clause on line " & ARM_Input.Line_String (Input_Object));
			    ARM_Input.Replace_Char (Input_Object);
		        end if;

		        Calc_Change_Disposition (
	            	    Format_Object => Format_Object,
			    Version => Version,
			    Operation => ARM_Output.Insertion,
			    Text_Kind => Disposition);

		        if Disposition = Do_Not_Display_Text then
			    null; -- Ignore this; it isn't numbered or anything.
		        elsif Disposition = ARM_Output.Deletion then
		            raise Program_Error; -- A deletion inside of an insertion command!
			else
		            if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Added_Subclause then
			        Format_Object.Clause_Number :=
				    (Section   => Format_Object.Clause_Number.Section,
				     Clause    => Format_Object.Clause_Number.Clause,
				     Subclause => Format_Object.Clause_Number.Subclause + 1,
				     Subsubclause => 0);
			        Level := ARM_Contents.Subclause;
		            elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Added_Subsubclause then
			        Format_Object.Clause_Number.Subsubclause :=
				    Format_Object.Clause_Number.Subsubclause + 1;
			        Level := ARM_Contents.Subsubclause;
		            elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Added_Clause then
			        Format_Object.Clause_Number :=
				    (Section   => Format_Object.Clause_Number.Section,
				     Clause    => Format_Object.Clause_Number.Clause + 1,
				     Subclause => 0, Subsubclause => 0);
			        Level := ARM_Contents.Clause;
		            elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Added_Section then
			        Format_Object.Clause_Number :=
				    (Section   => Format_Object.Clause_Number.Section, -- Will be set elsewhere.
				     Clause    => 0,
				     Subclause => 0, Subsubclause => 0);
			        Level := ARM_Contents.Section;
			    elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Added_Annex then
			        Format_Object.Clause_Number :=
				    (Section   => Format_Object.Clause_Number.Section, -- Will be set elsewhere.
				     Clause    => 0,
				     Subclause => 0, Subsubclause => 0);
			        Level := ARM_Contents.Plain_Annex;
			    elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Added_Normative_Annex then
			        Format_Object.Clause_Number :=
				    (Section   => Format_Object.Clause_Number.Section, -- Will be set elsewhere.
				     Clause    => 0,
				     Subclause => 0, Subsubclause => 0);
			        Level := ARM_Contents.Normative_Annex;
			    else -- Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Added_Informative_Annex then
			        Format_Object.Clause_Number :=
				    (Section   => Format_Object.Clause_Number.Section, -- Will be set elsewhere.
				     Clause    => 0,
				     Subclause => 0, Subsubclause => 0);
			        Level := ARM_Contents.Informative_Annex;
			    end if;

			    begin
			        declare
			            Clause_Number : constant String :=
				        ARM_Contents.Lookup_Clause_Number (New_Title);

			        begin
			            if Disposition = ARM_Output.None then
				        -- Normal reference:
				        Check_End_Paragraph; -- End any paragraph that we're in.
				        ARM_Output.Clause_Header (Output_Object,
				            New_Title(1..New_Title_Length),
				            Level => Level,
				            Clause_Number => Clause_Number);
			            else -- Insertion.
				        Check_End_Paragraph; -- End any paragraph that we're in.
				        ARM_Output.Revised_Clause_Header (Output_Object,
				            New_Header_Text => New_Title(1..New_Title_Length),
				            Old_Header_Text => "",
				            Level => Level,
				            Version => Version,
				            Old_Version => '0',
				            Clause_Number => Clause_Number);
				    end if;

			            -- Check that the section numbers match the title:
			            if Ada.Characters.Handling.To_Lower (New_Title) /=
			               Ada.Characters.Handling.To_Lower (ARM_Contents.Lookup_Title (
				          Level, Format_Object.Clause_Number)) then
				        Ada.Text_IO.Put_Line ("** Unable to match title with section numbers, line " & ARM_Input.Line_String (Input_Object));
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
		            if Format_Object.Use_ISO_2004_Note_Format then
			        -- Reset the note number:
		                Format_Object.Next_Note := 1;
		            elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Added_Section or else
		                  Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Added_Annex or else
		                  Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Added_Informative_Annex or else
		                  Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Added_Normative_Annex then
		                -- Reset the note number, only for sections:
		                Format_Object.Next_Note := 1;
		            end if;
		            -- Reset the subhead:
		            Format_Object.Last_Paragraph_Subhead_Type := Plain;
		            Format_Object.Next_Paragraph_Format_Type := Plain;
			end if;

		        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Header)");
		    end;

	        when Labeled_Deleted_Clause |
		     Labeled_Deleted_Subclause |
		     Labeled_Deleted_Subsubclause =>
		    -- Load the title into the Title string:
		    declare
		        Old_Title : ARM_Contents.Title_Type;
		        Old_Title_Length : Natural;
		        Ch : Character;
		        Version : ARM_Contents.Change_Version_Type := '0';
		        Level : ARM_Contents.Level_Type;
			Disposition : ARM_Output.Change_Type;
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
			        Old_Title, Old_Title_Length);
			    Old_Title(Old_Title_Length+1 .. Old_Title'Last) :=
				(others => ' ');
			end if;
		        ARM_Input.Get_Char (Input_Object, Ch);
		        if Ch /= Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char then
			    Ada.Text_IO.Put_Line ("  ** Bad close for Labeled_Deleted_(Sub)Clause on line " & ARM_Input.Line_String (Input_Object));
			    ARM_Input.Replace_Char (Input_Object);
		        end if;

		        Calc_Change_Disposition (
	            	    Format_Object => Format_Object,
			    Version => Version,
			    Operation => ARM_Output.Deletion,
			    Text_Kind => Disposition);

--Ada.Text_IO.Put_Line ("Labeled_Deleted disp: " & ARM_Output.Change_Type'Image(Disposition));
--Ada.Text_IO.Put_Line ("  Version:" & Version);
		        if Disposition = Do_Not_Display_Text then
			    null; -- Ignore this; it isn't numbered or anything.
		        elsif Disposition = ARM_Output.Insertion then
		            raise Program_Error; -- An insertion inside of a deletion command!
		        elsif Disposition = ARM_Output.Deletion then
			    -- Format the text as a deletion, but not as a header.
			    Check_End_Paragraph; -- End any paragraph that we're in.

			    ARM_Output.Start_Paragraph (Output_Object,
				Style => ARM_Output.Title,
				Indent => 0,
				Number => "");
			    ARM_Output.Text_Format (Output_Object,
				Format => (Bold => True,
					   Italic => False,
					   Font => ARM_Output.Default,
					   Size => ARM_Output.Size_Type(-2),
					   Color => ARM_Output.Default,
					   Change => ARM_Output.Deletion,
					   Version => Version,
					   Added_Version => '0',
					   Location => ARM_Output.Normal));
			    ARM_Output.Ordinary_Text (Output_Object,
				Old_Title(1..Old_Title_Length));
			    ARM_Output.Text_Format (Output_Object,
				ARM_Output.NORMAL_FORMAT);
			    ARM_Output.End_Paragraph (Output_Object);

		            -- Reset the paragraph numbers: (for the following deleted text, presumably also shown).
		            Format_Object.Next_Paragraph := 1;
		            Format_Object.Next_Insert_Para := 1;
		            Format_Object.Next_AARM_Sub := 'a';
		            if Format_Object.Use_ISO_2004_Note_Format then
			        -- Reset the note number:
		                Format_Object.Next_Note := 1;
		            --elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Deleted_Section or else
		            --      Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Deleted_Annex or else
		            --      Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Deleted_Informative_Annex or else
		            --      Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Deleted_Normative_Annex then
		            --    -- Reset the note number, only for sections: (no sections yet)
		            --    Format_Object.Next_Note := 1;
		            end if;
		            -- Reset the subhead:
		            Format_Object.Last_Paragraph_Subhead_Type := Plain;
		            Format_Object.Next_Paragraph_Format_Type := Plain;
			else -- Disposition = ARM_Output.None then
		            if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Deleted_Subclause then
			        Format_Object.Clause_Number :=
				    (Section   => Format_Object.Clause_Number.Section,
				     Clause    => Format_Object.Clause_Number.Clause,
				     Subclause => Format_Object.Clause_Number.Subclause + 1,
				     Subsubclause => 0);
			        Level := ARM_Contents.Subclause;
		            elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Deleted_Subsubclause then
			        Format_Object.Clause_Number.Subsubclause :=
				    Format_Object.Clause_Number.Subsubclause + 1;
			        Level := ARM_Contents.Subsubclause;
		            else --Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Labeled_Deleted_Clause then
			        Format_Object.Clause_Number :=
				    (Section   => Format_Object.Clause_Number.Section,
				     Clause    => Format_Object.Clause_Number.Clause + 1,
				     Subclause => 0, Subsubclause => 0);
			        Level := ARM_Contents.Clause;
			    end if;

			    begin
			        declare
			            Clause_Number : constant String :=
				        ARM_Contents.Lookup_Clause_Number (Old_Title);
			        begin
				    -- Normal reference:
				    Check_End_Paragraph; -- End any paragraph that we're in.
				    ARM_Output.Clause_Header (Output_Object,
				        Old_Title(1..Old_Title_Length),
				        Level => Level,
				        Clause_Number => Clause_Number);
				end;

			        -- Check that the section numbers match the title:
			        if Ada.Characters.Handling.To_Lower (Old_Title) /=
			           Ada.Characters.Handling.To_Lower (ARM_Contents.Lookup_Title (
				      Level, Format_Object.Clause_Number)) then
				    Ada.Text_IO.Put_Line ("** Unable to match title with section numbers, line " & ARM_Input.Line_String (Input_Object));
			        end if;
			    exception
			        when ARM_Contents.Not_Found_Error =>
				    Ada.Text_IO.Put_Line ("** Unable to find header reference, line " & ARM_Input.Line_String (Input_Object));
				    Ada.Text_IO.Put_Line ("   Looking for " & Old_Title(1..Old_Title_Length));
			    end;
		            -- Reset the paragraph numbers:
		            Format_Object.Next_Paragraph := 1;
		            Format_Object.Next_Insert_Para := 1;
		            Format_Object.Next_AARM_Sub := 'a';
		            -- Reset the subhead:
		            Format_Object.Last_Paragraph_Subhead_Type := Plain;
		            Format_Object.Next_Paragraph_Format_Type := Plain;
			end if;

			Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
--Ada.Text_IO.Put_Line (" &Unstack (Header)");
		    end;

		when Preface_Section =>
		    Check_End_Paragraph; -- End any paragraph that we're in.
		    --ARM_Output.New_Page (Output_Object, ARM_Output.Odd_Page_Only);
		    ARM_Output.Clause_Header (Output_Object,
					      Header_Text => "",
					      Level => ARM_Contents.Unnumbered_Section,
					      Clause_Number => "0.99");

		when Subheading =>
		    -- This is used in preface sections where no numbers or
		    -- contents are desired.
		    Check_End_Paragraph; -- End any paragraph that we're in.
		    ARM_Output.Start_Paragraph (Output_Object,
			     Style     => ARM_Output.Wide_Above,
			     Indent    => 0,
			     Number    => "",
			     No_Breaks => True, Keep_with_Next => True);
		    Format_Object.In_Paragraph := True;
		    Format_Object.No_Start_Paragraph := False;

		    Format_Object.Text_Format := ARM_Output.NORMAL_FORMAT;
		    Format_Object.Text_Format.Bold := True;
		    Format_Object.Text_Format.Font := ARM_Output.Swiss;
		    Format_Object.Text_Format.Size := 2;
		    ARM_Output.Text_Format (Output_Object,
					    Format_Object.Text_Format);

		when Added_Subheading =>
		    -- This is used in preface sections where no numbers or
		    -- contents are desired.
		    declare
			Ch : Character;
			Version : ARM_Contents.Change_Version_Type := '0';

			Disposition : ARM_Output.Change_Type;
			use type ARM_Output.Change_Type;
		    begin
			Get_Change_Version (Is_First => True,
					    Version => Version);
		        ARM_Input.Get_Char (Input_Object, Ch);
		        if Ch /= ',' then
			    Ada.Text_IO.Put_Line ("  ** Missing comma for AddedSubheading on line " & ARM_Input.Line_String (Input_Object));
			    ARM_Input.Replace_Char (Input_Object);
		        end if;
			Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version := Version;

		        Calc_Change_Disposition (
	            	    Format_Object => Format_Object,
			    Version => Version,
			    Operation => ARM_Output.Insertion,
			    Text_Kind => Disposition);

		        if Disposition = Do_Not_Display_Text then
			    -- Skip the text:
			    ARM_Input.Skip_until_Close_Char (Input_Object,
			        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char);
			    ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
		        elsif Disposition = ARM_Output.None then
			    -- Normal text:
		            ARM_Output.Start_Paragraph (Output_Object,
		                 Style  => ARM_Output.Wide_Above,
				 Indent => 0,
		                 Number => "",
		                 No_Breaks => True, Keep_with_Next => True);
		            Format_Object.In_Paragraph := True;
			    Format_Object.No_Start_Paragraph := False;

			    Format_Object.Text_Format := ARM_Output.NORMAL_FORMAT;
			    Format_Object.Text_Format.Bold := True;
			    Format_Object.Text_Format.Font := ARM_Output.Swiss;
			    Format_Object.Text_Format.Size := 2;
			    ARM_Output.Text_Format (Output_Object,
						    Format_Object.Text_Format);
		        elsif Disposition = ARM_Output.Deletion then
		            raise Program_Error; -- A deletion inside of an insertion command!
		        else -- Insertion.
		            ARM_Output.Start_Paragraph (Output_Object,
		                 Style  => ARM_Output.Wide_Above,
				 Indent => 0,
		                 Number => "",
		                 No_Breaks => True, Keep_with_Next => True);
		            Format_Object.In_Paragraph := True;
			    Format_Object.No_Start_Paragraph := False;

			    Format_Object.Text_Format := ARM_Output.NORMAL_FORMAT;
			    Format_Object.Text_Format.Bold := True;
			    Format_Object.Text_Format.Font := ARM_Output.Swiss;
			    Format_Object.Text_Format.Size := 2;
			    Format_Object.Text_Format.Change := ARM_Output.Insertion;
			    Format_Object.Text_Format.Version := Version;
			    ARM_Output.Text_Format (Output_Object,
					    Format_Object.Text_Format);
			end if;
		    end;

	        when Heading =>
		    -- This is used in preface sections where no numbers or
		    -- contents are desired.
		    Check_End_Paragraph; -- End any paragraph that we're in.
		    ARM_Output.Start_Paragraph (Output_Object,
			     Style  => ARM_Output.Title,
			     Indent => 0,
			     Number => "",
			     No_Breaks => True, Keep_with_Next => True,
			     Justification => ARM_Output.Center);
		    Format_Object.In_Paragraph := True;
		    Format_Object.No_Start_Paragraph := False;

		    Format_Object.Text_Format := ARM_Output.NORMAL_FORMAT;
		    Format_Object.Text_Format.Bold := True;
		    Format_Object.Text_Format.Font := ARM_Output.Swiss;
		    Format_Object.Text_Format.Size := 0;
			-- Note that the size is +3 from the Title format.
		    ARM_Output.Text_Format (Output_Object,
					    Format_Object.Text_Format);

		when Center =>
		    Check_End_Paragraph; -- End any paragraph that we're in.
		    ARM_Output.Start_Paragraph (Output_Object,
			     Style  => ARM_Output.Normal,
			     Indent => 0,
			     Number => "",
			     No_Breaks => True, Keep_with_Next => False,
			     Justification => ARM_Output.Center);
		    Format_Object.In_Paragraph := True;
		    Format_Object.No_Start_Paragraph := False;

		when Right =>
		    Check_End_Paragraph; -- End any paragraph that we're in.
		    ARM_Output.Start_Paragraph (Output_Object,
			     Style  => ARM_Output.Normal,
			     Indent => 0,
			     Number => "",
			     No_Breaks => True, Keep_with_Next => False,
			     Justification => ARM_Output.Right);
		    Format_Object.In_Paragraph := True;
		    Format_Object.No_Start_Paragraph := False;

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
					    Clause_Number : ARM_Contents.Clause_Number_Type;
				        begin
					    ARM_Contents.Make_Clause (Clause_Number_Text,
					        Clause_Number);
					    ARM_Output.Clause_Reference (Output_Object,
					        Text => Ada.Strings.Fixed.Trim (
						    ARM_Contents.Lookup_Old_Title (
							    ARM_Contents.Lookup_Level (Title),
							    Clause_Number),
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

		-- Link commands:
		when Local_Target =>
		    -- @LocalTarget{Target=[<target-text>],Text=[<text>]}
		    declare
			Close_Ch : Character;
			Target : String(1..40);
			Target_Len : Natural;
			Text : String(1..100);
			Text_Len : Natural;
		    begin
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Target" & (7..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => True,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Save URL:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
				Close_Ch,
				Target,
				Target_Len);
			-- else no parameter. Weird.
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Text" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Save name:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
				Close_Ch,
				Text,
				Text_Len);
			-- else no parameter. Weird.
			end if;

		        Check_Paragraph;
		        ARM_Output.Local_Target (Output_Object,
			    Text => Text(1..Text_Len),
			    Target => Target(1..Target_Len));
			if Text_Len /= 0 and then Text(Text_Len) /= ' ' then
		            Format_Object.Last_Non_Space := True;
			end if;
		    end;
		    -- Leave the command end marker, let normal processing
		    -- get rid of it.

		when Local_Link =>
		    -- @LocalLink{Target=[<target-text>],Sec=[<title>],Text=[<text>]}
		    declare
			Close_Ch : Character;
			Target : String(1..40);
			Target_Len : Natural;
			Text : String(1..100);
			Text_Len : Natural;
			Title : ARM_Contents.Title_Type;
			Title_Length : Natural;
		    begin
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Target" & (7..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => True,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Save URL:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
				Close_Ch,
				Target,
				Target_Len);
			-- else no parameter. Weird.
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Sec" & (4..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Save URL:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
				Close_Ch,
				Title,
				Title_Length);
			    Title(Title_Length+1 .. Title'Last) :=
			        (others => ' ');
			-- else no parameter. Weird.
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Text" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Save name:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
				Close_Ch,
				Text,
				Text_Len);
			-- else no parameter. Weird.
			end if;

			begin
			    declare
			        Clause_Number_Text : constant String :=
				    ARM_Contents.Lookup_Clause_Number (Title);
			    begin
			        Check_Paragraph;
			        ARM_Output.Local_Link (Output_Object,
				    Text => Text(1..Text_Len),
				    Target => Target(1..Target_Len),
				    Clause_Number => Clause_Number_Text);
				if Text_Len /= 0 and then Text(Text_Len) /= ' ' then
			            Format_Object.Last_Non_Space := True;
				end if;
			    end;
			exception
			    when ARM_Contents.Not_Found_Error =>
				Ada.Text_IO.Put_Line ("** Unable to find section in local link, line " & ARM_Input.Line_String (Input_Object));
				Ada.Text_IO.Put_Line ("   Looking for " & Title(1..Title_Length));
			end;
		    end;
		    -- Leave the command end marker, let normal processing
		    -- get rid of it.

		when URL_Link =>
		    -- @URLLink{URL=[<URL>],Text=[<text>]}
		    declare
			Close_Ch : Character;
			URL : String(1..80);
			URL_Len : Natural;
			Text : String(1..100);
			Text_Len : Natural;
		    begin
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "URL" & (4..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => True,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Save URL:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
				Close_Ch,
				URL,
				URL_Len);
			-- else no parameter. Weird.
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Text" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Save name:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
				Close_Ch,
				Text,
				Text_Len);
			-- else no parameter. Weird.
			end if;

		        Check_Paragraph;
		        ARM_Output.URL_Link (Output_Object,
			    Text => Text(1..Text_Len),
			    URL => URL(1..URL_Len));
			if Text_Len /= 0 and then Text(Text_Len) /= ' ' then
		            Format_Object.Last_Non_Space := True;
			end if;
		    end;
		    -- Leave the command end marker, let normal processing
		    -- get rid of it.

		when AI_Link =>
		    -- @AILink{AI=[<AI>],Text=[<text>]}
		    declare
			Close_Ch : Character;
			AI : String(1..30);
			AI_Len : Natural;
			Text : String(1..100);
			Text_Len : Natural;
		    begin
			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "AI" & (3..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => True,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Save AI:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
				Close_Ch,
				AI,
				AI_Len);
			-- else no parameter. Weird.
			end if;

			ARM_Input.Check_Parameter_Name (Input_Object,
			    Param_Name => "Text" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			    Is_First => False,
			    Param_Close_Bracket => Close_Ch);
			if Close_Ch /= ' ' then
			    -- Save name:
			    ARM_Input.Copy_to_String_until_Close_Char (
				Input_Object,
				Close_Ch,
				Text,
				Text_Len);
			-- else no parameter. Weird.
			end if;

		        Check_Paragraph;
		        ARM_Output.AI_Reference (Output_Object,
			    Text => Text(1..Text_Len),
			    AI_Number => AI(1..AI_Len));
			if Text_Len /= 0 and then Text(Text_Len) /= ' ' then
		            Format_Object.Last_Non_Space := True;
			end if;
		    end;
		    -- Leave the command end marker, let normal processing
		    -- get rid of it.

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
			    Format_Object.Text_Format.Change;
		        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Prev_Change_Version :=
			    Format_Object.Text_Format.Version;
		        Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Prev_Added_Change_Version :=
			    Format_Object.Text_Format.Added_Version;

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

			    -- Note: We can't use Calc_Change_Disposition here,
			    -- because it isn't intended to work on possibly
			    -- nested calls like these.

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
				           ARM_Database.Deleted) or else
				           ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind,
				           ARM_Database.Deleted_Inserted_Number) or else
				           ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind,
				           ARM_Database.Deleted_No_Delete_Message) or else
				           ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind,
				           ARM_Database.Deleted_Inserted_Number_No_Delete_Message) then
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
				               ARM_Database.Deleted) or else
				               ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind,
				               ARM_Database.Deleted_Inserted_Number) or else
				               ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind,
				               ARM_Database.Deleted_No_Delete_Message) or else
				               ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind,
				               ARM_Database.Deleted_Inserted_Number_No_Delete_Message) then
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
				                Check_Paragraph; -- Change the state *after* outputting the paragraph header.
					        case Format_Object.Text_Format.Change is
					            when ARM_Output.Insertion | ARM_Output.None =>
						        Format_Object.Text_Format.Change := ARM_Output.Insertion;
						        Format_Object.Text_Format.Version :=
						           Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
						        Format_Object.Text_Format.Added_Version := '0';
					            when ARM_Output.Deletion =>
						        Format_Object.Text_Format.Change := ARM_Output.Both;
						        Format_Object.Text_Format.Added_Version := -- The insertion should be older.
						           Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
						        -- .Text_Format.Version is unchanged.
					            when ARM_Output.Both =>
						        Format_Object.Text_Format.Change := ARM_Output.Both;
						        Format_Object.Text_Format.Added_Version := -- The insertion should be older.
						           Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
						        -- .Text_Format.Version is unchanged.
					        end case;
				                ARM_Output.Text_Format (Output_Object,
							                Format_Object.Text_Format); -- Set style.
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

		when Change_Added | Change_Deleted =>
		    -- @ChgAdded{Version=[<Version>],[NoPrefix=[T|F],][NoParanum=[T|F],]
		    --     [Type=[Leading|Trailing|Normal],][Keepnext=[T|F],]Text=[text]}
		    -- @ChgDeleted{Version=[<Version>],[NoPrefix=[T|F],][NoParanum=[T|F],]
		    --     [Type=[Leading|Trailing|Normal],][Keepnext=[T|F],]Text=[text]}
		    -- Whole paragraph change. These let us modify the AARM prefix
		    -- (like "Reason:" or "Discussion:", and also let us
		    -- conditionally handle paragraph formatting (which
		    -- otherwise would come too late).
		    declare
		        Which_Param : ARM_Input.Param_Num;
		        Ch, Close_Ch : Character;

			NoPrefix, Noparanum, Keepnext : Boolean := False;
			Space_After : ARM_Output.Space_After_Type := ARM_Output.Normal;

			Disposition : ARM_Output.Change_Type;
			use type ARM_Output.Change_Type;

			function Read_Boolean (Close_Ch : in Character;
					       Name : in String) return Boolean is
			    -- Read a Boolean parameter for a parameter already
			    -- opened and ending with Close_Ch.
			    -- Name is the name of the parameter; for error messages only.
			    Ch : Character;
			    Result : Boolean;
			begin
			    ARM_Input.Get_Char (Input_Object, Ch);
			    case Ch is
				when 'F' | 'f' | 'N' | 'n' =>
				    Result := False;
				when 'T' | 't' | 'Y' | 'y' =>
				    Result := True;
				when others =>
				    Ada.Text_IO.Put_Line ("  ** Bad value for boolean parameter " &
					Name & " on line " & ARM_Input.Line_String (Input_Object));
			    end case;
			    ARM_Input.Get_Char (Input_Object, Ch);
			    if Ch /= Close_Ch then
			        Ada.Text_IO.Put_Line ("  ** Bad close for boolean parameter " &
				    Name & " on line " & ARM_Input.Line_String (Input_Object));
				ARM_Input.Replace_Char (Input_Object);
			    end if;
			    return Result;
			end Read_Boolean;

			function Read_Type (Close_Ch : in Character) return ARM_Output.Space_After_Type is
			    -- Read the Type parameter.
			    Type_Name : ARM_Input.Command_Name_Type;
			    Ch : Character;
			    Result : ARM_Output.Space_After_Type := ARM_Output.Normal;
		        begin
			    -- Get the type word:
			    Arm_Input.Get_Name (Input_Object, Type_Name);
			    ARM_Input.Get_Char (Input_Object, Ch);
			    if Ch /= Close_Ch then
				Ada.Text_IO.Put_Line ("  ** Bad close for Type on line " & ARM_Input.Line_String (Input_Object));
				ARM_Input.Replace_Char (Input_Object);
			    end if;
			    if Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Type_Name, Ada.Strings.Right)) =
				"leading" then
				Result := ARM_Output.Narrow;
			    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Type_Name, Ada.Strings.Right)) =
				"trailing" then
				Result := ARM_Output.Wide;
			    elsif Ada.Characters.Handling.To_Lower (Ada.Strings.Fixed.Trim (Type_Name, Ada.Strings.Right)) =
				"normal" then
				Result := ARM_Output.Normal;
			    else
				Ada.Text_IO.Put_Line ("  ** Bad type for paragraph type: " &
				    Ada.Strings.Fixed.Trim (Type_Name, Ada.Strings.Right) &
				    " on line " & ARM_Input.Line_String (Input_Object));
			    end if;
			    return Result;
		        end Read_Type;

		    begin
		        -- These are not allowed in other @Chg commands.
		        if Format_Object.In_Change then
			    Ada.Text_IO.Put_Line ("** ChgAdded/ChgDeleted nested in other Chg, line " & ARM_Input.Line_String (Input_Object));
		        end if;

		        Get_Change_Version (Is_First => True,
		            Version => Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version);
		            -- Read a parameter named "Version".

		        loop
			    -- Handle the optional parameters; stop on Text.
		            ARM_Input.Check_One_of_Parameter_Names (Input_Object,
			        Param_Name_1 => "NoPrefix" & (9..ARM_Input.Command_Name_Type'Last => ' '),
			        Param_Name_2 => "NoParanum" & (10..ARM_Input.Command_Name_Type'Last => ' '),
			        Param_Name_3 => "Type" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			        Param_Name_4 => "Keepnext" & (9..ARM_Input.Command_Name_Type'Last => ' '),
			        Param_Name_5 => "Text" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			        Is_First => False,
			        Param_Found => Which_Param,
			        Param_Close_Bracket => Close_Ch);

			    if Which_Param = 1 and then Close_Ch /= ' ' then
				NoPrefix := Read_Boolean (Close_Ch, "NoPrefix");
			    elsif Which_Param = 2 and then Close_Ch /= ' ' then
				NoParanum := Read_Boolean (Close_Ch, "NoParanum");
			    elsif Which_Param = 3 and then Close_Ch /= ' ' then
				Space_After := Read_Type (Close_Ch);
			    elsif Which_Param = 4 and then Close_Ch /= ' ' then
				Keepnext := Read_Boolean (Close_Ch, "KeepNext");
			    else
				exit; -- We found "Text" (or an error)
			    end if;
		        end loop;

		        if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Change_Added and then
			   Close_Ch /= ' ' then
			    -- Generate an insertion, there is a Text parameter.

			    Calc_Change_Disposition (
				Format_Object => Format_Object,
				Version => Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version,
				Operation => ARM_Output.Insertion,
				Text_Kind => Disposition);

			    -- Stack the parameter so we can process the end:
			    Set_Nesting_for_Parameter
			        (Command => Change_Added_Param,
				 Close_Ch => Close_Ch);

			    Format_Object.In_Change := True;

			    if Disposition = Do_Not_Display_Text then
			        -- Skip the text:
			        ARM_Input.Skip_until_Close_Char (Input_Object,
				    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char);
			        ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
			    elsif Disposition = ARM_Output.None then
				-- Display the text normally.
				Format_Object.No_Prefix := NoPrefix;
				Format_Object.No_Para_Num := NoParanum;
				Format_Object.Keep_with_Next := KeepNext;
			        Format_Object.Space_After := Space_After;
			    elsif Disposition = ARM_Output.Deletion then
			        raise Program_Error; -- A deletion inside of an insertion command!
			    else -- Insertion.
				Format_Object.No_Prefix := NoPrefix;
				Format_Object.No_Para_Num := NoParanum;
				Format_Object.Keep_with_Next := KeepNext;
			        Format_Object.Space_After := Space_After;
			        -- We assume non-empty text and no outer changes;
			        -- set new change state:
			        Format_Object.Text_Format.Change := ARM_Output.Insertion;
			        Format_Object.Text_Format.Version :=
				   Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
			        Format_Object.Text_Format.Added_Version := '0';
			        Check_Paragraph; -- Change the state *before* outputting the
						 -- paragraph header, so the AARM prefix is included.
		                ARM_Output.Text_Format (Output_Object,
					                Format_Object.Text_Format); -- Reset style.
			    end if;

		        elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Change_Deleted and then
			   Close_Ch /= ' ' then
			    -- Generate a deletion.

			    Calc_Change_Disposition (
				Format_Object => Format_Object,
				Version => Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version,
				Operation => ARM_Output.Deletion,
				Text_Kind => Disposition);

			    -- Stack the parameter so we can process the end:
			    Set_Nesting_for_Parameter
			        (Command => Change_Deleted_Param,
				 Close_Ch => Close_Ch);

			    Format_Object.In_Change := True;

			    if Disposition = Do_Not_Display_Text then
			        -- Skip the text:
			        ARM_Input.Skip_until_Close_Char (Input_Object,
				    Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char);
			        ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.
			        if ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind,
			           ARM_Database.Deleted) or else
			           ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind,
			           ARM_Database.Deleted_Inserted_Number) or else
			           ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind,
			           ARM_Database.Deleted_No_Delete_Message) or else
			           ARM_Database."=" (Format_Object.Next_Paragraph_Change_Kind,
			           ARM_Database.Deleted_Inserted_Number_No_Delete_Message) then
			            -- In a deleted paragraph, call Check_Paragraph
				    -- to trigger the "deleted paragraph" message and
				    -- increment the paragraph number.
				    -- (Otherwise, this may never happen.)
				    Format_Object.No_Para_Num := NoParanum;
			            Check_Paragraph;
			        -- else null; -- Nothing special to do.
			        end if;
			    elsif Disposition = ARM_Output.None then
				-- Display the text normally.
				Format_Object.No_Prefix := NoPrefix;
				Format_Object.No_Para_Num := NoParanum;
				Format_Object.Keep_with_Next := KeepNext;
			        Format_Object.Space_After := Space_After;
			    elsif Disposition = ARM_Output.Insertion then
			        raise Program_Error; -- An insertion inside of a deletion command!
			    else -- Deletion.
			        if Format_Object.Changes = ARM_Format.New_Changes then
				    -- Special case: we ignore the deleted text, but mark its presence.
				    -- We assume that the text is non-empty;
				    -- set the new change state.
				    -- Note: We ignore the formatting here!
			            Format_Object.No_Prefix := True;
			            Format_Object.No_Para_Num := NoParanum;
				    Format_Object.Text_Format.Change := ARM_Output.Deletion;
				    Format_Object.Text_Format.Version :=
				       Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
				    Format_Object.Text_Format.Added_Version := '0';
				    Check_Paragraph; -- Output the paragraph headers *after* changing the state,
				        -- so that AARM headers are marked.
		                    ARM_Output.Text_Format (Output_Object,
					                    Format_Object.Text_Format); -- Reset style.

				    ARM_Output.Ordinary_Character (Output_Object, ' ');
				    -- Skip the text (we're not going to output it):
			            ARM_Input.Skip_until_Close_Char (Input_Object, Close_Ch);
				    ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.

			        else -- Normal.
			            -- We assume that the text is non-empty;
			            -- set the new change state and formatting.
				    Format_Object.No_Prefix := NoPrefix;
				    Format_Object.No_Para_Num := NoParanum;
			            Format_Object.Keep_with_Next := KeepNext;
			            Format_Object.Space_After := Space_After;

			            Format_Object.Text_Format.Change := ARM_Output.Deletion;
			            Format_Object.Text_Format.Version :=
				       Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
			            Format_Object.Text_Format.Added_Version := '0';
			            Check_Paragraph; -- Output the paragraph headers *after* changing the state,
				        -- so that AARM headers are marked.
		                    ARM_Output.Text_Format (Output_Object,
					                    Format_Object.Text_Format); -- Reset style.
				end if;
			    end if;
		        -- else no parameter; previous error.
			end if;
		    end;

		when Change_Added_Param | Change_Deleted_Param =>
		    -- These can't get here; they represent the parameter of
		    -- "Change_Added" and "Change_Deleted" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** Change_xxx parameter command?? on line " & ARM_Input.Line_String (Input_Object));

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
		    -- This can't get here; it represents the third parameter of
		    -- "ChgImpldef" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** Impdef parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Change_Impladv_Text_Param =>
		    -- This can't get here; it represents the third parameter of
		    -- "ChgImpladvice" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** Impladv parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Change_Docreq_Text_Param =>
		    -- This can't get here; it represents the third parameter of
		    -- "ChgDocReq" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** Docreq parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Change_AspectDesc_Text_Param =>
		    -- This can't get here; it represents the fourth parameter of
		    -- "ChgAspectDesc" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** AspectDesc parameter command?? on line " & ARM_Input.Line_String (Input_Object));

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
			    if (ARM_Database."=" (Kind, ARM_Database.Deleted_No_Delete_Message) or else
			        ARM_Database."=" (Kind, ARM_Database.Deleted_Inserted_Number_No_Delete_Message)) and then
			       ARM_Format."=" (Format_Object.Changes, ARM_Format.New_Only) then
			        -- In this case, display nothing, period.
				Display_It := False;
				-- Check if any Ref's already exist; remove them
				-- if they do. %% Ugly: This is needed if
				-- a paragraph is first revised, then deleted,
				-- as in 4.6. There ought to be a better way
				-- of handling this, not sure what it is.
				Dump_References (Format_Object.References);
			    elsif (ARM_Database."=" (Kind, ARM_Database.Deleted) or else
			        ARM_Database."=" (Kind, ARM_Database.Deleted_Inserted_Number)) and then
			       ARM_Format."=" (Format_Object.Changes, ARM_Format.New_Only) and then
				(not Format_Object.Number_Paragraphs) then
			        -- No delete messages ever in this case, so
				-- display nothing, period.
				Display_It := False;
				-- Check if any Ref's already exist; see above.
				Dump_References (Format_Object.References);
			    else
			        Display_It := Format_Object.Include_Annotations;
			    end if;
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
		    Gen_Chg_xxxx (Param_Cmd => Change_Impdef_Text_Param,
				  AARM_Prefix => "Implementation defined: ");

		when Change_Implementation_Advice =>
		    -- This command is of the form:
		    -- @chgimpladvice{Version=[<version>], Kind=(<kind>),
		    --   Text=(<text>)}}
		    -- where <version> is a single character, <Kind> is one
		    -- of Revised, Added, or Deleted, and this is followed
		    -- by the text. As usual, any of the
		    -- allowed bracketing characters can be used.
		    Gen_Chg_xxxx (Param_Cmd => Change_Impladv_Text_Param,
				  AARM_Prefix => "Implementation Advice: ");

		when Change_Documentation_Requirement =>
		    -- This command is of the form:
		    -- @chgdocreq{Version=[<version>], Kind=(<kind>),
		    --   Text=(<text>)}}
		    -- where <version> is a single character, <Kind> is one
		    -- of Revised, Added, or Deleted, and this is followed
		    -- by the text. As usual, any of the
		    -- allowed bracketing characters can be used.
		    Gen_Chg_xxxx (Param_Cmd => Change_Docreq_Text_Param,
				  AARM_Prefix => "Documentation Requirement: ");

		when Change_Aspect_Description =>
		    -- This command is of the form:
		    -- @chgdocreq{Version=[<version>], Kind=(<kind>),
		    --   Aspect=[<name>],Text=(<text>)}}
		    -- where <version> is a single character, <Kind> is one
		    -- of Revised, Added, or Deleted, <Name> is the aspect
		    -- name, and this is followed
		    -- by the text. As usual, any of the
		    -- allowed bracketing characters can be used.
		    Gen_Chg_xxxx (Param_Cmd => Change_AspectDesc_Text_Param,
				  AARM_Prefix => "Aspect Description for ",
				  For_Aspect => True);

		when Change_Attribute =>
		     -- @ChgAttribute{Version=[<version>], Kind=(<kind>),
		     --    Chginannex=[T|F],Leading=[T|F],
		     --    Prefix=<Prefix>,AttrName=<Name>,
		     --    {[A]Ref=[<DR_Number>]},Text=<Text>}
		     -- Defines a changed attribute.
		    declare
			Close_Ch : Character;
			Key : ARM_Index.Index_Key;
			Chg_in_Annex : Boolean;
			Is_Leading : Boolean;
			Kind : ARM_Database.Paragraph_Change_Kind_Type;
			Version : ARM_Contents.Change_Version_Type;
			Display_Ref : Boolean;
			Which_Param : ARM_Input.Param_Num;
			References : Reference_Ptr := null;


			procedure Make_Attribute_Text is
			    -- Generate the attribute text.
			    -- Output <Prefix>'<Name> as the hanging text.
			    -- Generate the needed index entries.
			begin
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
					   Clause => Clause_String (Format_Object),
					   Paragraph => Paragraph_String,
					   Key => Key);
			    ARM_Output.Index_Target (Output_Object, Key);

			    ARM_Index.Add (Term => Format_Object.Attr_Name (1 .. Format_Object.Attr_Name_Len) & " attribute",
					   Kind => ARM_Index.Primary_Term,
					   Clause => Clause_String (Format_Object),
					   Paragraph => Paragraph_String,
					   Key => Key);
			    ARM_Output.Index_Target (Output_Object, Key);

			    Make_References (References, Format_Object, Output_Object);

			end Make_Attribute_Text;

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
			    if (ARM_Database."=" (Kind, ARM_Database.Deleted_No_Delete_Message) or else
			        ARM_Database."=" (Kind, ARM_Database.Deleted_Inserted_Number_No_Delete_Message)) and then
			       ARM_Format."=" (Format_Object.Changes, ARM_Format.New_Only) then
			        -- In this case, display nothing, period.
				Display_Ref := False;
			    else
			        Display_Ref := Format_Object.Include_Annotations;
			    end if;
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

			-- Handle the Ref and ARef parameters, until
			-- the Text parameter shows up.
			loop
		            ARM_Input.Check_One_of_Parameter_Names (Input_Object,
			        Param_Name_1 => "Ref" & (4..ARM_Input.Command_Name_Type'Last => ' '),
			        Param_Name_2 => "ARef" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			        Param_Name_3 => "Text" & (5..ARM_Input.Command_Name_Type'Last => ' '),
			        Is_First => False,
			        Param_Found => Which_Param,
			        Param_Close_Bracket => Close_Ch);
		            if (Which_Param = 1 or else Which_Param = 2) and then
				Close_Ch /= ' ' then
				declare
				    Ref_Name : ARM_Input.Command_Name_Type;
				    Len : Natural := 0;
				    New_Ref, Cursor : Reference_Ptr;
				    Ch : Character;
				begin
				    -- Get the reference:
				    loop
				        ARM_Input.Get_Char (Input_Object, Ch);
				        if Ch /= Close_Ch then
					    Len := Len + 1;
					    if Len > Ref_Name'Last then
					        Ada.Text_IO.Put_Line ("  ** Reference too long on line " & ARM_Input.Line_String (Input_Object));
					    else
						Ref_Name(Len) := Ch;
					    end if;
				        else -- End of the reference.
					    if Len = 0 then
					        Ada.Text_IO.Put_Line ("  ** Failed to find reference on line " & ARM_Input.Line_String (Input_Object));
					    end if;
					    exit;
				        end if;
				    end loop;

				    if Display_Ref then
				        -- Save a reference for outputting
					-- later.
				        New_Ref := Allocate_Reference;
				        New_Ref.all := (Ref_Name => Ref_Name,
							Ref_Len => Len,
							Is_DR_Ref => (Which_Param = 1),
							   -- DR reference if Param = 1;
							   -- AI reference otherwise.
							Next => null);
					-- Attach this to the *end* of the list.
					if References = null then
					    References := New_Ref;
					else
					    Cursor := References;
					    while Cursor.Next /= null loop
						Cursor := Cursor.next;
					    end loop;
					    Cursor.Next := New_Ref;
					end if;
			            -- else don't display it.
				    end if;
				end;
			    else
				exit; -- We found "Text" (or an error)
			    end if;
		        end loop;

			case Format_Object.Attr_Change_Kind is
			    when ARM_Database.None | ARM_Database.Revised |
				 ARM_Database.Revised_Inserted_Number =>
				-- The prefix is unchanged.
				Make_Attribute_Text;
			        if Close_Ch /= ' ' then
			            -- Now, handle the parameter:
			            -- The text goes to the file *and* is recorded.
			            Arm_Input.Start_Recording (Input_Object);
			            -- Stack the parameter so we can process the end:
			            Set_Nesting_for_Parameter
			                (Command => Attribute_Text_Param,
				         Close_Ch => Close_Ch);
			        end if;

			    when ARM_Database.Inserted | ARM_Database.Inserted_Normal_Number =>
				-- The insertion has to be both here and in the
				-- Annex.
				if not Chg_in_Annex then
				    Ada.Text_IO.Put_Line ("  ** Attribute adding in text, but not in Annex??? on line " & ARM_Input.Line_String (Input_Object));
				end if;

			        if Close_Ch /= ' ' then

				    -- Stack the parameter so we can process the end:
				    Set_Nesting_for_Parameter
				        (Command => Attribute_Text_Param,
					 Close_Ch => Close_Ch);

				    declare
					Disposition : ARM_Output.Change_Type;
					use type ARM_Output.Change_Type;
				    begin

				        Calc_Change_Disposition (
					    Format_Object => Format_Object,
					    Version => Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version,
					    Operation => ARM_Output.Insertion,
					    Text_Kind => Disposition);

				        if Disposition = Do_Not_Display_Text then
				            -- Skip the text:
				            ARM_Input.Skip_until_Close_Char (Input_Object, Close_Ch);
				            ARM_Input.Replace_Char (Input_Object); -- Let the normal termination clean this up.

				        elsif Disposition = ARM_Output.None then
					    -- Display the text normally.
					    Make_Attribute_Text;
					    -- Nothing special to do (normal text).

				        elsif Disposition = ARM_Output.Deletion then
				            raise Program_Error; -- A deletion inside of an insertion command!
				        else -- Insertion.
					    -- We assume non-empty text and no outer changes;
					    -- set new change state:
					    Format_Object.Text_Format.Change := ARM_Output.Insertion;
					    Format_Object.Text_Format.Version := Version;
					    Format_Object.Text_Format.Added_Version := '0';
				            Check_Paragraph; -- Change the state *before* outputting the
							     -- paragraph header, so the AARM prefix is included.
			                    ARM_Output.Text_Format (Output_Object,
						                    Format_Object.Text_Format);
					    Make_Attribute_Text;

					    -- Reset the state to normal:
					    Format_Object.Text_Format.Change := ARM_Output.None;
					    Format_Object.Text_Format.Version := '0';
					    Format_Object.Text_Format.Added_Version := '0';
			                    ARM_Output.Text_Format (Output_Object,
						                    Format_Object.Text_Format);
					end if;
				    end;

			            -- The text goes to the file *and* is recorded.
			            Arm_Input.Start_Recording (Input_Object);
			        -- else no parameter. Do nothing.
				end if;

			    when ARM_Database.Deleted | ARM_Database.Deleted_Inserted_Number |
			         ARM_Database.Deleted_No_Delete_Message |
				 ARM_Database.Deleted_Inserted_Number_No_Delete_Message =>

				Ada.Text_IO.Put_Line ("  ** Attribute deleting not implemented on line " & ARM_Input.Line_String (Input_Object));
				-- This should work very similarly to the above.
				-- If disposition is do not display text,
				-- do not generate or store attribute in DB.
				-- That would require changes to Attribute_Text_Param.

			        Make_Attribute_Text;
			        if Close_Ch /= ' ' then
			            -- Now, handle the parameter:
			            -- The text goes to the file *and* is recorded.
			            Arm_Input.Start_Recording (Input_Object);
			            -- Stack the parameter so we can process the end:
			            Set_Nesting_for_Parameter
			                (Command => Attribute_Text_Param,
				         Close_Ch => Close_Ch);
			        end if;
			end case;
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

		when Added_Implementation_Advice_List =>
		    -- This command is of the form:
		    -- @AddedImplAdvice{Version=[v]}
		    declare
			Version : ARM_Contents.Change_Version_Type;
		    begin
		        Get_Change_Version (Is_First => True,
		            Version => Version);
		            -- Read a parameter named "Version".
		        DB_Report  (Format_Object.ImplAdv_DB,
				    ARM_Database.Bullet_List,
				    Sorted => True,
				    Added_Version => Version);
		    end;

		when Added_Documentation_Requirements_List =>
		    -- This command is of the form:
		    -- @AddedDocReq{Version=[v]}
		    declare
			Version : ARM_Contents.Change_Version_Type;
		    begin
		        Get_Change_Version (Is_First => True,
		            Version => Version);
		            -- Read a parameter named "Version".
		        DB_Report  (Format_Object.Docreq_DB,
				    ARM_Database.Bullet_List,
				    Sorted => True,
				    Added_Version => Version);
		    end;

		when Added_Aspect_Description_List =>
		    -- This command is of the form:
		    -- @AddedAspect{Version=[v]}
		    declare
			Version : ARM_Contents.Change_Version_Type;
		    begin
		        Get_Change_Version (Is_First => True,
		            Version => Version);
		            -- Read a parameter named "Version".
		        DB_Report  (Format_Object.Aspect_DB,
				    ARM_Database.Hanging_List,
				    Sorted => True,
				    Added_Version => Version);
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
			    if Len > Value'Last then
			        Ada.Text_IO.Put_Line ("  ** Latin-1 value too long on line " &
							    ARM_Input.Line_String (Input_Object));
				exit;
			    end if;
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

        	when Unicode =>
		    -- The parameter is the decimal code for the Uncode
		    -- character to generate.
		    declare
			Value : String (1..11);
			Len : Natural := 0;
			Ch : Character;
		    begin
			ARM_Input.Get_Char (Input_Object, Ch);
			while Ch /= Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Close_Char loop
			    Len := Len + 1;
			    if Len > Value'Last then
			        Ada.Text_IO.Put_Line ("  ** Unicode value too long on line " &
							    ARM_Input.Line_String (Input_Object));
				exit;
			    end if;
			    Value(Len) := Ch;
			    ARM_Input.Get_Char (Input_Object, Ch);
			end loop;
		        Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		            -- Remove the "Unicode" record.
			Check_Paragraph;
			ARM_Output.Unicode_Character (Output_Object,
			    ARM_Output.Unicode_Type'Value(Value(1..Len)));
			Format_Object.Last_Non_Space := True;
		    exception
			when Constraint_Error =>
			    Ada.Text_IO.Put_Line ("  ** Bad Unicode value [" &
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
		     Index_List |
		     Syntax_Summary | Syntax_XRef | Glossary_List |
		     Attribute_List | Pragma_List | Implementation_Defined_List |
		     Package_List | Type_List | Subprogram_List |
		     Exception_List | Object_List |
		     Intro_Name | Syntax_Name | Resolution_Name |
		     Legality_Name | Static_Name | Link_Name | Run_Name |
		     Bounded_Name | Erroneous_Name | Req_Name |
		     Doc_Name | Metrics_Name | Permission_Name | Advice_Name |
		     Notes_Name | Single_Note_Name | Examples_Name |
		     Meta_Name | Inconsistent83_Name |
		     Incompatible83_Name | Extend83_Name | Wording83_Name |
		     Inconsistent95_Name |
		     Incompatible95_Name | Extend95_Name | Wording95_Name |
		     Inconsistent2005_Name |
		     Incompatible2005_Name | Extend2005_Name | Wording2005_Name |
		     Syntax_Title | Resolution_Title | Legality_Title |
		     Static_Title | Link_Title | Run_Title | Bounded_Title |
		     Erroneous_Title | Req_Title | Doc_Title | Metrics_Title |
		     Permission_Title | Advice_Title | Notes_Title |
		     Single_Note_Title |
		     Examples_Title | Meta_Title | Inconsistent83_Title |
		     Incompatible83_Title | Extend83_Title | Wording83_Title |
		     Inconsistent95_Title |
		     Incompatible95_Title | Extend95_Title | Wording95_Title |
		     Inconsistent2005_Title |
		     Incompatible2005_Title | Extend2005_Title | Wording2005_Title |
		     EM_Dash | EN_Dash | LT | LE | GT | GE | NE | PI |
		     Times | PorM | Single_Quote | Thin_Space | Left_Quote |
		     Right_Quote | Left_Double_Quote | Right_Double_Quote |
		     Left_Quote_Pair | Right_Quote_Pair | Small_Dotless_I |
		     Capital_Dotted_I =>

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
		    Data.Paragraph_Kind_Name(Kind).Str(1..Data.Paragraph_Kind_Name(Kind).Length));
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
		Real_Include_Annotations : Boolean := Format_Object.Include_Annotations;
	    begin
		-- Don't show annotations here:
                Format_Object.Include_Annotations := False;
		Arm_String.Open (Input_Object, Text, Text_Name);
		     -- Open the input object using a string for input.
		Real_Process (Format_Object, Format_State, Input_Object, Output_Object);
		Arm_String.Close (Input_Object);
		Format_Object.Include_Annotations := Real_Include_Annotations;
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
		    if not Format_Object.Include_Annotations then
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

		when Index_List =>
		    -- Generate the index body.
		    Check_End_Paragraph;

		    ARM_Output.Set_Columns (Output_Object, Number_of_Columns => 3);

		    ARM_Index.Generate_Index_Body (Output_Object,
			Use_Paragraphs => Format_Object.Number_Paragraphs);

		    ARM_Output.Set_Columns (Output_Object, Number_of_Columns => 1);

		when Syntax_Summary =>
--Ada.Text_IO.Put_Line ("%% Generate Syntax summary");
		    Syn_Report;

		when Syntax_XRef =>
--Ada.Text_IO.Put_Line ("%% Generate Syntax xref");
		    Syn_XRef;

		when Glossary_List =>
		    DB_Report  (Format_Object.Glossary_DB,
				ARM_Database.Normal_Indexed_List,
				Sorted => True);

		when Attribute_List =>
		    DB_Report  (Format_Object.Attr_DB,
				ARM_Database.Hanging_List,
				Sorted => True,
				No_Deleted_Paragraph_Messages => True);

		when Pragma_List =>
		    DB_Report  (Format_Object.Pragma_DB,
				ARM_Database.Normal_List,
				Sorted => True);

		when Implementation_Defined_List =>
		    DB_Report  (Format_Object.Impdef_DB,
				ARM_Database.Bullet_List,
				Sorted => True);

		when Package_List =>
		    Write_Subindex (Format_Object.Package_Index,
				    Format_Object,
				    Output_Object,
				    Minimize_Lines => False);

		when Type_List =>
		    Write_Subindex (Format_Object.Type_Index,
				    Format_Object,
				    Output_Object,
				    Minimize_Lines => False);

		when Subprogram_List =>
		    Write_Subindex (Format_Object.Subprogram_Index,
				    Format_Object,
				    Output_Object,
				    Minimize_Lines => True);

		when Exception_List =>
		    Write_Subindex (Format_Object.Exception_Index,
				    Format_Object,
				    Output_Object,
				    Minimize_Lines => False);

		when Object_List =>
		    Write_Subindex (Format_Object.Object_Index,
				    Format_Object,
				    Output_Object,
				    Minimize_Lines => True);

		when Text_Begin | Text_End | Redundant | Part | Bold | Italic |
		     Roman | Swiss | Fixed | Roman_Italic | Shrink | Grow |
		     Black | Red | Green | Blue |
		     Keyword | Non_Terminal | Non_Terminal_Format |
		     Example_Text | Example_Comment |
		     Up | Down | Tab_Clear | Tab_Set |
		     New_Page_for_Version | RM_New_Page_for_Version | New_Column_for_Version |
		     Table | Picture_Alone | Picture_Inline |
		     Defn | RootDefn | PDefn | Defn2 | RootDefn2 | PDefn2 |
		     Index_See | Index_See_Also | See_Other | See_Also |
		     Index_Root_Unit | Index_Child_Unit | Index_Subprogram_Child_Unit |
		     Index_Type | Index_Subtype |
		     Index_Subprogram | Index_Exception | Index_Object |
		     Index_Package | Index_Other | Index_Check |
		     Index_Attr | Index_Pragma | Index_Aspect |
		     Syntax_Rule | Syntax_Term | Syntax_Term_Undefined | Syntax_Prefix |
		     Added_Syntax_Rule | Deleted_Syntax_Rule |
		     To_Glossary | To_Glossary_Also |
		     Change_To_Glossary | Change_To_Glossary_Also |
		     Implementation_Defined |
		     Prefix_Type | Reset_Prefix_Type | Attribute | Attribute_Leading |
		     Pragma_Syntax | Added_Pragma_Syntax | Deleted_Pragma_Syntax |
		     Labeled_Section | Labeled_Section_No_Break |
		     Labeled_Clause | Labeled_Subclause | Labeled_Subsubclause |
		     Labeled_Revised_Section | Labeled_Revised_Clause |
		     Labeled_Revised_Subclause | Labeled_Revised_Subsubclause |
		     Labeled_Added_Section | Labeled_Added_Clause |
		     Labeled_Added_Subclause | Labeled_Added_Subsubclause |
		     Labeled_Deleted_Clause |
		     Labeled_Deleted_Subclause | Labeled_Deleted_Subsubclause |
		     Labeled_Annex | Labeled_Revised_Annex | Labeled_Added_Annex |
		     Labeled_Informative_Annex | Labeled_Revised_Informative_Annex |
		     Labeled_Added_Informative_Annex |
		     Labeled_Normative_Annex | Labeled_Revised_Normative_Annex |
		     Labeled_Added_Normative_Annex |
		     Unnumbered_Section | Subheading | Added_Subheading | Heading |
		     Center | Right |
		     Preface_Section | Ref_Section | Ref_Section_Number | Ref_Section_by_Number |
		     Local_Target | Local_Link | URL_Link | AI_Link |
		     Change | Change_Reference | Change_Note |
		     Change_Added | Change_Deleted |
		     Change_Implementation_Defined |
		     Change_Implementation_Advice |
		     Change_Documentation_Requirement |
		     Change_Aspect_Description |
		     Added_Implementation_Advice_List |
		     Added_Documentation_Requirements_List |
		     Added_Aspect_Description_List |
		     Change_Attribute |
		     Change_Prefix_Type |
		     Latin_1 | Unicode | Ceiling | Floor | Absolute | Log =>
		    -- These commands must have a parameter.
		    Ada.Text_IO.Put_Line ("  ** Failed to find parameter for " &
		        Ada.Strings.Fixed.Trim (Name, Ada.Strings.Right) &
		        ", line " & ARM_Input.Line_String (Input_Object));

		when Change_Param_Old | Change_Param_New | Change_Added_Param | Change_Deleted_Param =>
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

		when Change_Impladv_Text_Param =>
		    -- This can't get here; it represents a parameter of
		    -- "ChgImpladv" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** Impladv parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Change_Docreq_Text_Param =>
		    -- This can't get here; it represents a parameter of
		    -- "ChgDocreq" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** DocReq parameter command?? on line " & ARM_Input.Line_String (Input_Object));

		when Change_AspectDesc_Text_Param =>
		    -- This can't get here; it represents a parameter of
		    -- "ChgAspectDesc" and can't be generated explicitly.
		    Ada.Text_IO.Put_Line ("  ** AspectDesc parameter command?? on line " & ARM_Input.Line_String (Input_Object));

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
		when Single_Note_Name =>
		    Put_Name(Single_Note);
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
		when Inconsistent2005_Name =>
		    Put_Name(Ada2005_Inconsistencies);
		when Incompatible2005_Name =>
		    Put_Name(Ada2005_Incompatibilities);
		when Extend2005_Name =>
		    Put_Name(Ada2005_Extensions);
		when Wording2005_Name =>
		    Put_Name(Ada2005_Wording);

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
		when Single_Note_Title =>
		    Put_Title(Single_Note);
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
		when Inconsistent2005_Title =>
		    Put_Title(Ada2005_Inconsistencies);
		when Incompatible2005_Title =>
		    Put_Title(Ada2005_Incompatibilities);
		when Extend2005_Title =>
		    Put_Title(Ada2005_Extensions);
		when Wording2005_Title =>
		    Put_Title(Ada2005_Wording);

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
		when Small_Dotless_I =>
		    Check_Paragraph;
		    ARM_Output.Special_Character (Output_Object, ARM_Output.Small_Dotless_I);
		    Format_Object.Last_Non_Space := True;
		when Capital_Dotted_I =>
		    Check_Paragraph;
		    ARM_Output.Special_Character (Output_Object, ARM_Output.Capital_Dotted_I);
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

	    procedure Finish_and_DB_Entry (DB : in out ARM_Database.Database_Type;
					   For_Aspect : in Boolean := False) is
		-- Close the text parameter for a number of commands
		-- (impdef, chgimpdef, chgimpladv, chgdocreg)
		-- and insert the resulting string into the appropriate DB.
		Text_Buffer : String (1..ARM_Input.MAX_RECORDING_SIZE);
		Text_Buffer_Len : Natural;

		function DB_Clause_String return String is
		    use type ARM_Contents.Section_Number_Type;
		begin
		    if Format_Object.Clause_Number.Clause = 0 then
			if Format_Object.Clause_Number.Section in 0 .. 9 then
			    return
				Character'Val(Character'Pos('0') +
				   Format_Object.Clause_Number.Section) & "";
			elsif Format_Object.Clause_Number.Section in 10 .. 19 then
			    return "1" &
				Character'Val(Character'Pos('0') +
				   Format_Object.Clause_Number.Section - 10);
			elsif Format_Object.Clause_Number.Section in 20 .. 29 then
			    return "2" &
				Character'Val(Character'Pos('0') +
				   Format_Object.Clause_Number.Section - 20);
			elsif Format_Object.Clause_Number.Section = 30 then
			    return "30";
			else --if Format_Object.Clause_Number.Section >= ARM_Contents.ANNEX_START then
			    return Character'Val (Character'Pos('A') +
				 (Format_Object.Clause_Number.Section -
				    ARM_Contents.ANNEX_START)) & "";
			end if;
		    elsif Format_Object.Clause_Number.Subclause = 0 then
			return ARM_Contents.Make_Clause_Number (
			        ARM_Contents.Clause,
				Format_Object.Clause_Number);
		    elsif Format_Object.Clause_Number.Subsubclause = 0 then
			return ARM_Contents.Make_Clause_Number (
			        ARM_Contents.SubClause,
				Format_Object.Clause_Number);
		    else
			return ARM_Contents.Make_Clause_Number (
			        ARM_Contents.SubsubClause,
				Format_Object.Clause_Number);
		    end if;
		end DB_Clause_String;

		function Sort_Clause_String return String is
		    Res : String(1..13);
		    -- Always use the paragraph for sorting:
		begin
		    -- The funny encoding to insure proper sorting.
		    -- (Otherwise "10" sorts before "2".
		    Res(1) := Character'Val (Natural(Format_Object.Clause_Number.Section) + 16#30#);
		    Res(2) := '.';
		    Res(3) := Character'Val (Format_Object.Clause_Number.Clause + 16#30#);
		    Res(4) := '.';
		    Res(5) := Character'Val (Format_Object.Clause_Number.Subclause + 16#30#);
		    Res(6) := '.';
		    Res(7) := Character'Val (Format_Object.Clause_Number.Subsubclause + 16#30#);
		    Res(8) := '(';
		    Res(9) := Character'Val ((Format_Object.Next_Paragraph / 10) + 16#30#);
		    Res(10) := Character'Val ((Format_Object.Next_Paragraph mod 10) + 16#30#);
		    Res(11) := '.';
		    Res(12) := Character'Val ((Format_Object.Next_Insert_Para mod 10) + 16#30#);
		    Res(13) := ')';
		    return Res;
		end Sort_Clause_String;

		function See_String return String is
		begin
		    case Format_Object.Impdef_Change_Kind is
			when ARM_Database.None | ARM_Database.Revised |
			     ARM_Database.Revised_Inserted_Number =>
			    if Format_Object.Number_Paragraphs and (not For_Aspect) then
				return " See @RefSecbyNum{" & DB_Clause_String & "}(" &
				    Format_Object.Impdef_Paragraph_String (1..Format_Object.Impdef_Paragraph_Len) &
				    ").";
			    else -- No paragraph numbers.
				return " See @RefSecbyNum{" & DB_Clause_String & "}.";
			    end if;
			when ARM_Database.Inserted | ARM_Database.Inserted_Normal_Number =>
			    if Format_Object.Number_Paragraphs and (not For_Aspect) then
				return "@Chg{Version=[" & Format_Object.Impdef_Version &
			            "], New=[ See @RefSecbyNum{" & DB_Clause_String & "}(" &
				    Format_Object.Impdef_Paragraph_String (1..Format_Object.Impdef_Paragraph_Len) &
				    ").],Old=[]}";
			    else -- No paragraph numbers.
				return "@Chg{Version=[" & Format_Object.Impdef_Version &
				    "], New=[ See @RefSecbyNum{" & DB_Clause_String & "}.],Old=[]}";
			    end if;
			when ARM_Database.Deleted |
			     ARM_Database.Deleted_Inserted_Number |
			     ARM_Database.Deleted_No_Delete_Message |
			     ARM_Database.Deleted_Inserted_Number_No_Delete_Message =>
			    if Format_Object.Number_Paragraphs and (not For_Aspect) then
				return "@Chg{Version=[" & Format_Object.Impdef_Version &
				    "], New=[],Old=[ See @RefSecbyNum{" & DB_Clause_String & "}(" &
				    Format_Object.Impdef_Paragraph_String (1..Format_Object.Impdef_Paragraph_Len) &
				    ").]}";
			    else -- No paragraph numbers.
				return "@Chg{Version=[" & Format_Object.Impdef_Version &
				    "], New=[],Old=[ See @RefSecbyNum{" & DB_Clause_String & "}.]}";
			    end if;
		    end case;
		end See_String;

	    begin
		Arm_Input.Stop_Recording_and_Read_Result
		    (Input_Object, Text_Buffer, Text_Buffer_Len);
		Text_Buffer_Len := Text_Buffer_Len - 1; -- Remove command close character.
		if For_Aspect then
	            ARM_Database.Insert (DB,
		        Sort_Key => Format_Object.Aspect_Name(1..Format_Object.Aspect_Name_Len),
		        Hang_Item => Format_Object.Aspect_Name(1..Format_Object.Aspect_Name_Len),
		        Text => Text_Buffer(1..Text_Buffer_Len) &
		           See_String,
		        Change_Kind => Format_Object.Impdef_Change_Kind,
		        Version => Format_Object.Impdef_Version,
			Initial_Version => Format_Object.Impdef_Initial_Version);
		else
	            ARM_Database.Insert (DB,
		        Sort_Key => Sort_Clause_String,
		        Hang_Item => "",
		        Text => Text_Buffer(1..Text_Buffer_Len) &
		           See_String,
		        Change_Kind => Format_Object.Impdef_Change_Kind,
		        Version => Format_Object.Impdef_Version,
			Initial_Version => Format_Object.Impdef_Initial_Version);
		end if;
	        -- Finish the text processing:
	        if Format_Object.Include_Annotations then
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
	        -- else nothing started, nothing to finish.
	        end if;
	    end Finish_and_DB_Entry;

	begin
	    case Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command is
		when Redundant =>
		    if Format_Object.Include_Annotations then
		        Check_Paragraph;
		        ARM_Output.Ordinary_Character (Output_Object, ']');
		    -- else ignored.
		    end if;

		when Bold | Italic | Roman | Swiss | Fixed | Roman_Italic |
		     Shrink | Grow | Up | Down |
		     Black | Red | Green | Blue |
		     Keyword | Non_Terminal | Non_Terminal_Format |
		     Example_Text | Example_Comment =>
		    -- Formatting commands; revert to the previous (saved)
		    -- version:
		    Check_Paragraph;
		    Format_Object.Text_Format :=
			Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Text_Format;
		    ARM_Output.Text_Format (Output_Object,
					    Format => Format_Object.Text_Format);

		when Subheading | Heading =>
		    -- Restore the format.
		    Check_Paragraph;
		    ARM_Output.Text_Format (Output_Object,
					    Format => ARM_Output.NORMAL_FORMAT);
		    Format_Object.Text_Format := ARM_Output.NORMAL_FORMAT;
		    Check_End_Paragraph;

		when Added_Subheading =>
		    -- Restore the format.
		    declare
			Disposition : ARM_Output.Change_Type;
			use type ARM_Output.Change_Type;
		    begin
		        Calc_Change_Disposition (
			    Format_Object => Format_Object,
			    Version => Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version,
			    Operation => ARM_Output.Insertion,
			    Text_Kind => Disposition);

		        if Disposition = Do_Not_Display_Text then
			    -- The new text was ignored.
		            null; -- Nothing to do (we nulled out the text before we got here).
		        elsif Disposition = ARM_Output.None then
			    -- Display the text normally.
			    Check_Paragraph;
			    ARM_Output.Text_Format (Output_Object,
						    Format => ARM_Output.NORMAL_FORMAT);
			    Format_Object.Text_Format := ARM_Output.NORMAL_FORMAT;
			    Check_End_Paragraph;
		        elsif Disposition = ARM_Output.Deletion then
			    raise Program_Error; -- A deletion inside of an insertion command!
		        else -- Insertion.
			    Check_Paragraph;
			    ARM_Output.Text_Format (Output_Object,
						    Format => ARM_Output.NORMAL_FORMAT);
			    Format_Object.Text_Format := ARM_Output.NORMAL_FORMAT;
			    Check_End_Paragraph;
			end if;
		    end;

	        when Center | Right =>
		    -- Close the paragraph.
		    Check_End_Paragraph;

		when New_Page_for_Version =>
		    -- The version parameter is stored in Change_Version on
		    -- the stack.
		    if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version
			= Format_Object.Change_Version then
		        Check_End_Paragraph; -- End any paragraph that we're in.
		        ARM_Output.New_Page (Output_Object, ARM_Output.Any_Page);
		    -- else do nothing.
		    end if;

		when RM_New_Page_for_Version =>
		    -- The version parameter is stored in Change_Version on
		    -- the stack.
		    if not Format_Object.Include_Annotations and then
		       Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version
			= Format_Object.Change_Version then
		        Check_End_Paragraph; -- End any paragraph that we're in.
		        ARM_Output.New_Page (Output_Object, ARM_Output.Any_Page);
		    -- else do nothing.
		    end if;

		when New_Column_for_Version =>
		    -- The version parameter is stored in Change_Version on
		    -- the stack.
		    if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version
			= Format_Object.Change_Version then
		        Check_End_Paragraph; -- End any paragraph that we're in.
		        ARM_Output.New_Column (Output_Object);
		    -- else do nothing.
		    end if;

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
		    Format_Object.No_Start_Paragraph := False;

		when Syntax_Rule_RHS =>
		    -- Send the production to the syntax manager.
		    -- Other processing has already been handled.
		    declare
			Text_Buffer : String (1..ARM_Input.MAX_RECORDING_SIZE);
			Text_Buffer_Len : Natural;
			Disposition : ARM_Output.Change_Type;
			use type ARM_Output.Change_Type;
		    begin
			-- Calculate Disposition:
			if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Command = Syntax_Rule then
			    Disposition := ARM_Output.None; -- Normal text.
			elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Command = Added_Syntax_Rule then
		            Calc_Change_Disposition (
				Format_Object => Format_Object,
			        Version => Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version,
			        Operation => ARM_Output.Insertion,
			        Text_Kind => Disposition);
			else -- Deleted_Syntax_Rule
		            Calc_Change_Disposition (
				Format_Object => Format_Object,
			        Version => Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version,
			        Operation => ARM_Output.Deletion,
			        Text_Kind => Disposition);
			end if;

		        if Disposition = Do_Not_Display_Text then
			    -- The text was ignored and skipped.
			    -- Set the Non-terminal to empty:
			    Format_Object.Syntax_NT_Len := 0;
			    Format_Object.Syntax_Tab_Len := 0;
		        else
			    Arm_Input.Stop_Recording_and_Read_Result
			        (Input_Object, Text_Buffer, Text_Buffer_Len);
			    Text_Buffer_Len := Text_Buffer_Len - 1; -- Remove command close character.
			    if Disposition = ARM_Output.None then
			        -- Display the text normally.
			        ARM_Syntax.Insert_Rule (For_Clause => Clause_String (Format_Object),
			            Rule => "@ntf{" & Format_Object.Syntax_NT(1..Format_Object.Syntax_NT_Len) &
				        " ::=} " & Text_Buffer(1..Text_Buffer_Len),
			            Tabset => Format_Object.Syntax_Tab(1..Format_Object.Syntax_Tab_Len));
		            elsif Disposition = ARM_Output.Deletion then
			        ARM_Syntax.Insert_Rule (For_Clause => Clause_String (Format_Object),
			            Rule => "@ntf{" & Format_Object.Syntax_NT(1..Format_Object.Syntax_NT_Len) &
				        "@Chg{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version &
					"],New=[],Old=[ ::=]}} " & Text_Buffer(1..Text_Buffer_Len),
			            Tabset => Format_Object.Syntax_Tab(1..Format_Object.Syntax_Tab_Len));
			    else -- Insertion.
			        ARM_Syntax.Insert_Rule (For_Clause => Clause_String (Format_Object),
			            Rule => "@ntf{" & Format_Object.Syntax_NT(1..Format_Object.Syntax_NT_Len) &
				        "@Chg{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version &
					"],New=[ ::=],Old=[]}} " & Text_Buffer(1..Text_Buffer_Len),
			            Tabset => Format_Object.Syntax_Tab(1..Format_Object.Syntax_Tab_Len));
			    end if;
			    -- Note: The LHS non-terminals aren't linked as usual.

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
			end if;
		    end;

		when Syntax_Prefix =>
		    -- Reset the style:
		    Check_Paragraph;
		    Format_Object.Text_Format.Italic := False;
		    ARM_Output.Text_Format (Output_Object,
					    Format_Object.Text_Format);

		when Glossary_Text_Param =>
		    -- Save the glossary entry in the Glossary database.
		    declare
			use type ARM_Database.Paragraph_Change_Kind_Type;
			Text_Buffer : String (1..ARM_Input.MAX_RECORDING_SIZE);
			Text_Buffer_Len : Natural;
		    begin
			Arm_Input.Stop_Recording_and_Read_Result
			    (Input_Object, Text_Buffer, Text_Buffer_Len);
			Text_Buffer_Len := Text_Buffer_Len - 1; -- Remove command close character.
			if Format_Object.Add_to_Glossary then
			    if Format_Object.Glossary_Change_Kind = ARM_Database.Inserted then
			        ARM_Database.Insert (Format_Object.Glossary_DB,
			            Sort_Key => Format_Object.Glossary_Term(1..Format_Object.Glossary_Term_Len),
			            Hang_Item => "",
			            Text => "@chg{Version=[" & Format_Object.Glossary_Version & "],New=[@b{" &
					Format_Object.Glossary_Term(1..Format_Object.Glossary_Term_Len) &
				        ".}],Old=[]} " & Text_Buffer(1..Text_Buffer_Len),
			            Change_Kind => Format_Object.Glossary_Change_Kind,
			            Version => Format_Object.Glossary_Version,
			            Initial_Version => Format_Object.Glossary_Version);

			    elsif Format_Object.Glossary_Change_Kind = ARM_Database.Deleted then
			        ARM_Database.Insert (Format_Object.Glossary_DB,
			            Sort_Key => Format_Object.Glossary_Term(1..Format_Object.Glossary_Term_Len),
			            Hang_Item => "",
			            Text => "@chg{Version=[" & Format_Object.Glossary_Version & "],New=[],Old=[@b{" &
					Format_Object.Glossary_Term(1..Format_Object.Glossary_Term_Len) &
				        ".}]} " & Text_Buffer(1..Text_Buffer_Len),
			            Change_Kind => Format_Object.Glossary_Change_Kind,
			            Version => Format_Object.Glossary_Version);

			    else
			        ARM_Database.Insert (Format_Object.Glossary_DB,
			            Sort_Key => Format_Object.Glossary_Term(1..Format_Object.Glossary_Term_Len),
			            Hang_Item => "",
			            Text => "@b{" & Format_Object.Glossary_Term(1..Format_Object.Glossary_Term_Len) &
				        ".} " & Text_Buffer(1..Text_Buffer_Len),
			            Change_Kind => Format_Object.Glossary_Change_Kind,
			            Version => Format_Object.Glossary_Version);
			    end if;
			end if;
		    end;

		    -- Finish the text processing:
		    if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Command = To_Glossary_Also or else
		       Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Command = Change_To_Glossary_Also then
			null; -- Normal text, no special handling needed.
		    else
			if Format_Object.Glossary_Displayed then
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
		        else
			    null; -- No text, no special handling needed.
			end if;
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
				when ARM_Database.Deleted |
				     ARM_Database.Deleted_Inserted_Number |
				     ARM_Database.Deleted_No_Delete_Message |
				     ARM_Database.Deleted_Inserted_Number_No_Delete_Message =>
				    return "@Chgref{Version=[" & Version &
					"],Kind=[Deleted]}";
				when ARM_Database.Revised | ARM_Database.Revised_Inserted_Number =>
				    return "@Chgref{Version=[" & Version &
					"],Kind=[Revised]}";
			    end case;
			end Chg_Command;

			procedure Write_to_DB (Prefix_Kind, Text_Kind :
				in ARM_Database.Paragraph_Change_Kind_Type;
				Prefix_Version, Text_Version : in Character) is
			    -- Write the item to the DB; use Prefix_Kind and
			    -- Text_Kind for the change kind.
			    Init_Version : Character;
			    function Sort_Key return String is
				-- Generate a Sort Key so that these sort
				-- as they did in Ada 95.
			    begin
				if Format_Object.Attr_Prefix_Len > 2 and then
					Format_Object.Attr_Prefix(2) = ''' then
				    -- Class-wide prefix. For some reason, this
				    -- sorts before the main item in the Ada 95 RM.
				    -- (And this continues in later RMs.)
				    return Format_Object.Attr_Name(1..Format_Object.Attr_Name_Len) &
					''' & Character'Pred(Format_Object.Attr_Prefix(1)) &
					"'Class";
					    -- Sort by name, then by prefix.
				else
				    return Format_Object.Attr_Name(1..Format_Object.Attr_Name_Len) &
					''' & Format_Object.Attr_Prefix(1..Format_Object.Attr_Prefix_Len);
					    -- Sort by name, then by prefix.
				end if;
				-- Note: Ada 2005 sorted E'Identity and T'Identity
				-- backwards from Ada 95. This will fix that.
			    end Sort_Key;

			begin
			    -- Guess the Initial_Version (eventually, we'll
			    -- add this as an optional parameter):
			    case Prefix_Kind is
				when ARM_Database.Revised_Inserted_Number |
				     ARM_Database.Inserted                |
				     ARM_Database.Deleted_Inserted_Number |
				     ARM_Database.Deleted_Inserted_Number_No_Delete_Message =>
				    Init_Version := Prefix_Version;
--Ada.Text_IO.Put_Line ("-- Inserted.");
			        when ARM_Database.None | ARM_Database.Revised |
			             ARM_Database.Inserted_Normal_Number |
			             ARM_Database.Deleted |
				     ARM_Database.Deleted_No_Delete_Message =>
--Ada.Text_IO.Put_Line ("-- Normal.");
				    Init_Version := '0';
			    end case;
			    if Format_Object.Attr_Leading then
			        ARM_Database.Insert (Format_Object.Attr_DB,
				    Sort_Key => Sort_Key,
				    Hang_Item =>
				        Format_Object.Attr_Prefix(1..Format_Object.Attr_Prefix_Len) &
					   ''' & Format_Object.Attr_Name(1..Format_Object.Attr_Name_Len),
				    Text => "For " & Format_Object.Prefix_Text(1..Format_Object.Prefix_Text_Len) &
				        ":" & Ascii.LF & Ascii.LF &
				        Chg_Command (Text_Kind, Text_Version) &
				        "@leading@noprefix@;" & Text_Buffer(1..Text_Buffer_Len) &
				        " See @RefSecbyNum{" & Clause_String(Format_Object) & "}.",
				    Change_Kind => Prefix_Kind,
				    Version => Prefix_Version,
				    Initial_Version => Init_Version);
			    else -- not leading:
			        ARM_Database.Insert (Format_Object.Attr_DB,
				    Sort_Key => Sort_Key,
				    Hang_Item =>
				        Format_Object.Attr_Prefix(1..Format_Object.Attr_Prefix_Len) &
					   ''' & Format_Object.Attr_Name(1..Format_Object.Attr_Name_Len),
				    Text => "For " & Format_Object.Prefix_Text(1..Format_Object.Prefix_Text_Len) &
				        ":" & Ascii.LF & Ascii.LF &
				        Chg_Command (Text_Kind, Text_Version) &
				        "@noprefix@;" & Text_Buffer(1..Text_Buffer_Len) &
				        " See @RefSecbyNum{" & Clause_String(Format_Object) & "}.",
				    Change_Kind => Prefix_Kind,
				    Version => Prefix_Version,
				    Initial_Version => Init_Version);
			    end if;
			end Write_to_DB;

			use type ARM_Database.Paragraph_Change_Kind_Type;

		    begin
			Arm_Input.Stop_Recording_and_Read_Result
			    (Input_Object, Text_Buffer, Text_Buffer_Len);
			Text_Buffer_Len := Text_Buffer_Len - 1; -- Remove command close character.
--Ada.Text_IO.Put_Line ("&& Attr: " & Format_Object.Attr_Name(1..Format_Object.Attr_Name_Len) &
--   " Prefix kind=" & ARM_Database.Paragraph_Change_Kind_Type'Image(Format_Object.Attr_Prefix_Change_Kind) &
--   " Attr chg kind=" & ARM_Database.Paragraph_Change_Kind_Type'Image(Format_Object.Attr_Change_Kind));
			case Format_Object.Attr_Change_Kind is
			    when ARM_Database.None | ARM_Database.Revised |
				 ARM_Database.Revised_Inserted_Number =>
				-- Ordinary text processing is fine for the local text.
				Write_to_DB (Prefix_Kind => Format_Object.Attr_Prefix_Change_Kind,
 					     Text_Kind => Format_Object.Attr_Change_Kind,
					     Prefix_Version => Format_Object.Attr_Prefix_Version,
					     Text_Version => Format_Object.Attr_Version);
			    when ARM_Database.Inserted | ARM_Database.Inserted_Normal_Number =>
				declare
				    Disposition : ARM_Output.Change_Type;
				    use type ARM_Output.Change_Type;
			        begin
			            Calc_Change_Disposition (
					Format_Object => Format_Object,
				        Version => Format_Object.Attr_Version,
				        Operation => ARM_Output.Insertion,
				        Text_Kind => Disposition);

			            if Disposition = Do_Not_Display_Text then
				        null; -- Do *not* put this into the DB.
--Ada.Text_IO.Put_Line ("   Inserted: Ignore");
			            elsif Disposition = ARM_Output.None then
--Ada.Text_IO.Put_Line ("   Inserted: Normal");
					-- Normal reference:
				        Write_to_DB (Prefix_Kind => ARM_Database.Inserted,
		 				     Text_Kind => ARM_Database.Inserted,
						     Prefix_Version => Format_Object.Attr_Version,
						     Text_Version => Format_Object.Attr_Version);
				        -- We could get away without any
					-- insert, except that then the paragraph
					-- numbers would be wrong. Note (as below),
					-- the whole thing is an insertion, so
					-- we ignore the prefix kind and version and force this
					-- to have an inserted kind.
			            elsif Disposition = ARM_Output.Deletion then
			                raise Program_Error; -- A deletion inside of an insertion command!
			            else -- Insertion.
					-- Write inserted text:
					-- We need to mark everything with
					-- the kind and version of the *entire* insertion,
					-- because the entire thing is an
					-- insertion. (So we ignore the prefix kind and version).
--Ada.Text_IO.Put_Line ("   Inserted: Inserted version:" & Format_Object.Attr_Version);
				        if Format_Object.Attr_Leading then
				            ARM_Database.Insert (Format_Object.Attr_DB,
					        Sort_Key => Format_Object.Attr_Name(1..Format_Object.Attr_Name_Len),
					        Hang_Item =>
						    "@ChgAdded{Version=[" & Format_Object.Attr_Version & "],Text=[" &
					            Format_Object.Attr_Prefix(1..Format_Object.Attr_Prefix_Len) &
						       ''' & Format_Object.Attr_Name(1..Format_Object.Attr_Name_Len) & "]}",
					        Text =>
						    "@ChgAdded{Version=[" & Format_Object.Attr_Version & "],Text=[" &
						    "For " & Format_Object.Prefix_Text(1..Format_Object.Prefix_Text_Len) &
					            ":]}" & Ascii.LF & Ascii.LF &
					            Chg_Command (Format_Object.Attr_Change_Kind, Format_Object.Attr_Version) &
						    "@leading@noprefix@;" & Text_Buffer(1..Text_Buffer_Len) &
					            " See @RefSecbyNum{" & Clause_String(Format_Object) & "}.",
					        Change_Kind => Format_Object.Attr_Change_Kind,
					        Version => Format_Object.Attr_Version,
					        Initial_Version => Format_Object.Attr_Version);
				        else -- not leading:
				            ARM_Database.Insert (Format_Object.Attr_DB,
					        Sort_Key => Format_Object.Attr_Name(1..Format_Object.Attr_Name_Len),
					        Hang_Item =>
						    "@ChgAdded{Version=[" & Format_Object.Attr_Version & "],Text=[" &
					            Format_Object.Attr_Prefix(1..Format_Object.Attr_Prefix_Len) &
						       ''' & Format_Object.Attr_Name(1..Format_Object.Attr_Name_Len) & "]}",
					        Text =>
						    "@ChgAdded{Version=[" & Format_Object.Attr_Version & "],Text=[" &
						    "For " & Format_Object.Prefix_Text(1..Format_Object.Prefix_Text_Len) &
					            ":]}" & Ascii.LF & Ascii.LF &
					            Chg_Command (Format_Object.Attr_Change_Kind, Format_Object.Attr_Version) &
					            "@noprefix@;" & Text_Buffer(1..Text_Buffer_Len) &
					            " See @RefSecbyNum{" & Clause_String(Format_Object) & "}.",
					        Change_Kind => Format_Object.Attr_Change_Kind,
					        Version => Format_Object.Attr_Version,
					        Initial_Version => Format_Object.Attr_Version);
				        end if;
				    end if;
				end;

			    when ARM_Database.Deleted |
				 ARM_Database.Deleted_Inserted_Number |
				 ARM_Database.Deleted_No_Delete_Message |
				 ARM_Database.Deleted_Inserted_Number_No_Delete_Message =>
				-- *** We don't support this yet. (It doesn't make much sense;
				-- *** it would be unlikely that we'd stop defining
				-- *** an attribute).
				Write_to_DB (Prefix_Kind => Format_Object.Attr_Prefix_Change_Kind,
 					     Text_Kind => Format_Object.Attr_Change_Kind,
					     Prefix_Version => Format_Object.Attr_Prefix_Version,
					     Text_Version => Format_Object.Attr_Version);
			end case;
        	    end;

		when Pragma_Syntax | Added_Pragma_Syntax | Deleted_Pragma_Syntax =>
		    -- Note: Pragma_Syntax is not recorded in the syntax summary.
		    declare
			Text_Buffer : String (1..ARM_Input.MAX_RECORDING_SIZE);
			Text_Buffer_Len : Natural;

			function My_Sort return String is
			    -- Find and return the @prag argument.
			begin
			    for I in 1 .. Text_Buffer_Len - 7 loop
				if Text_Buffer(I) = '@' and then
				(Text_Buffer(I+1) = 'p' or else Text_Buffer(I+1) = 'P') and then
				(Text_Buffer(I+2) = 'r' or else Text_Buffer(I+2) = 'R') and then
				(Text_Buffer(I+3) = 'a' or else Text_Buffer(I+3) = 'A') and then
				(Text_Buffer(I+4) = 'g' or else Text_Buffer(I+4) = 'G') and then
				ARM_Input.Is_Open_Char (Text_Buffer(I+5)) then
				    -- Found @prag.
				    for J in I+6 .. Text_Buffer_Len loop
				        if Text_Buffer(J) = ARM_Input.Get_Close_Char (Text_Buffer(I+5)) then
					    return Text_Buffer(I+6 .. J-1);
				        end if;
				    end loop;
				    Ada.Text_IO.Put_Line ("** Can't find argument for @prag: " & Text_Buffer(1..Text_Buffer_Len) &
				        " on line " & ARM_Input.Line_String (Input_Object));
			            return ""; -- Never found the end of the argument.
				-- else not @prag, keep looking.
				end if;
			    end loop;
			    -- If we get here, we never found "@prag"
			    Ada.Text_IO.Put_Line ("** Funny pragma format: " & Text_Buffer(1..Text_Buffer_Len) &
			        " on line " & ARM_Input.Line_String (Input_Object));
			    return ""; -- Gotta return something.
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
				    " @em See @RefSecbyNum{" & Clause_String(Format_Object) & "}.");
			elsif Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Added_Pragma_Syntax then
			    declare
			        Disposition : ARM_Output.Change_Type;
			        use type ARM_Output.Change_Type;
			    begin
			        Calc_Change_Disposition (
				    Format_Object => Format_Object,
				    Version => Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version,
				    Operation => ARM_Output.Insertion,
				    Text_Kind => Disposition);

			        if Disposition = Do_Not_Display_Text then
				    null; -- Not in old versions, omit from Annex.
			        elsif Disposition = ARM_Output.None then
				    -- Normal reference:
				    ARM_Database.Insert (Format_Object.Pragma_DB,
				        Sort_Key => My_Sort,
				        Hang_Item => "",
				        Text => "@ChgRef{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version &
					    "],Kind=[Added]}" &
					    Text_Buffer(1..Text_Buffer_Len) &
					    " @em See @RefSecbyNum{" & Clause_String(Format_Object) & "}.");
				    -- Note: We still need the @ChgRef in order
				    -- to get the paragraph numbers right.
			        elsif Disposition = ARM_Output.Deletion then
			            raise Program_Error; -- A deletion inside of an insertion command!
			        else -- Insertion.
				    ARM_Database.Insert (Format_Object.Pragma_DB,
				        Sort_Key => My_Sort,
				        Hang_Item => "",
				        Text => "@ChgRef{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version &
					    "],Kind=[Added]}" &
					    Text_Buffer(1..Text_Buffer_Len) &
					    "@Chg{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version &
					    "],New=[" &
					    " @em See @RefSecbyNum<" & Clause_String(Format_Object) & ">.],Old=[]}");
				    -- Note: Text includes any needed @Chg commands.
			        end if;
		            end;
			else --if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Command = Deleted_Pragma_Syntax then
			    declare
			        Add_Disposition : ARM_Output.Change_Type;
			        Del_Disposition : ARM_Output.Change_Type;
			        use type ARM_Output.Change_Type;

				function Para_Kind return String is
				begin
				    if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Prev_Change_Version = '0' and then
					Add_Disposition /= ARM_Output.Insertion then
					return "Deleted";
				    else
					return "DeletedAdded";
				    end if;
				end Para_Kind;

			    begin
			        Calc_Change_Disposition (
				    Format_Object => Format_Object,
				    Version => Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version,
				    Operation => ARM_Output.Deletion,
				    Text_Kind => Del_Disposition);
				if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Prev_Change_Version = '0' then
				    Add_Disposition := ARM_Output.None;
				else
			            Calc_Change_Disposition (
				        Format_Object => Format_Object,
				        Version => Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Prev_Change_Version,
				        Operation => ARM_Output.Insertion,
				        Text_Kind => Add_Disposition);
				end if;

			        if Del_Disposition = Do_Not_Display_Text then
--Ada.Text_IO.Put_Line ("%% Deleted pragma completely omitted");
				    if Add_Disposition /= Do_Not_Display_Text
					and then Format_Object.Number_Paragraphs then
					-- If this was in older editions, then
					-- we need a deletion message (and also
					-- to get the paragraph numbers right).
					-- But don't need this if there are no
					-- paragraph numbers (then there is no
					-- deleted message).
--Ada.Text_IO.Put_Line ("   ... but need a deletion message");
				        ARM_Database.Insert (Format_Object.Pragma_DB,
					    Sort_Key => My_Sort,
					    Hang_Item => "",
				            Text => "@ChgRef{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version &
					        "],Kind=[" & Para_Kind & "]}@ ");
                                    else
					null; -- Not at all in this version, omit from Annex.
				    end if;
			        elsif Del_Disposition = ARM_Output.None then
--Ada.Text_IO.Put_Line ("%% Deleted pragma normal format");
				    -- Is the initial item inserted or normal?
				    if Add_Disposition = ARM_Output.Insertion then
--Ada.Text_IO.Put_Line ("... but inserted");
				        -- Inserted reference:
					ARM_Database.Insert (Format_Object.Pragma_DB,
				            Sort_Key => My_Sort,
				            Hang_Item => "",
				            Text => "@ChgRef{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Prev_Change_Version &
					        "],Kind=[Added]}" &
						Text_Buffer(1..Text_Buffer_Len) &
					        "@Chg{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Prev_Change_Version &
					        "],New=[" &
					        " @em See @RefSecbyNum<" & Clause_String(Format_Object) & ">.],Old=[]}");
				    else -- Anything else.
				        -- Normal reference:
					if Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Prev_Change_Version = '0' then
					    ARM_Database.Insert (Format_Object.Pragma_DB,
					        Sort_Key => My_Sort,
					        Hang_Item => "",
					        Text => Text_Buffer(1..Text_Buffer_Len) &
						    " @em See @RefSecbyNum{" & Clause_String(Format_Object) & "}.");
					else
				            ARM_Database.Insert (Format_Object.Pragma_DB,
				                Sort_Key => My_Sort,
				                Hang_Item => "",
				                Text => "@ChgRef{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Prev_Change_Version &
					            "],Kind=[Added]}" &
					            Text_Buffer(1..Text_Buffer_Len) &
					            " @em See @RefSecbyNum{" & Clause_String(Format_Object) & "}.");
				            -- Note: We still need the @ChgRef in order
				            -- to get the paragraph numbers right.
					end if;
				    end if;
			        elsif Del_Disposition = ARM_Output.Insertion then
			            raise Program_Error; -- An insertion inside of a deletion command!
			        else -- Deletion.
--Ada.Text_IO.Put_Line ("%% Deleted pragma deleted text");
				    -- Is the initial item inserted or normal?
				    if Add_Disposition = ARM_Output.Insertion then
				        ARM_Database.Insert (Format_Object.Pragma_DB,
				            Sort_Key => My_Sort,
				            Hang_Item => "",
				            Text => "@ChgRef{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version &
					        "],Kind=[DeletedAdded]}" &
						Text_Buffer(1..Text_Buffer_Len) &
					        "@Chg{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Prev_Change_Version &
					        "],New=[" &
					        "@Chg{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version &
					        "],New=[],Old=[" &
					        " @em See @RefSecbyNum<" & Clause_String(Format_Object) & ">.]}],Old=[]}");
				    else -- Anything else.
				        -- Just a deleted reference:
				        ARM_Database.Insert (Format_Object.Pragma_DB,
					    Sort_Key => My_Sort,
					    Hang_Item => "",
				            Text => "@ChgRef{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version &
					        "],Kind=[" & Para_Kind & "]}" &
						Text_Buffer(1..Text_Buffer_Len) &
					        "@Chg{Version=[" & Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr).Change_Version &
					        "],New=[],Old=[" &
					        " @em See @RefSecbyNum<" & Clause_String(Format_Object) & ">.]}");
				        -- Note: We still need the @ChgRef in order
				        -- to get the paragraph numbers right and for the deleted paragraph message.
				    end if;
			        end if;
		            end;

		        end if;
	            end;

		when Implementation_Defined | Change_Impdef_Text_Param =>
		    -- Save the implementation-defined entry in the database.
		    Finish_and_DB_Entry (Format_Object.Impdef_DB);

		when Change_Impladv_Text_Param =>
		    -- Save the implementation advice entry in the database.
		    Finish_and_DB_Entry (Format_Object.Impladv_DB);

		when Change_Docreq_Text_Param =>
		    -- Save the documentation requirement entry in the database.
		    Finish_and_DB_Entry (Format_Object.Docreq_DB);

		when Change_AspectDesc_Text_Param =>
		    -- Save the documentation requirement entry in the database.
		    Finish_and_DB_Entry (Format_Object.Aspect_DB, For_Aspect => True);

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
					Format_Object.Text_Format.Change :=
				            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Prev_Change;
					Format_Object.Text_Format.Version :=
				            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Prev_Change_Version;
					Format_Object.Text_Format.Added_Version :=
				            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Prev_Added_Change_Version;

			                Check_Paragraph; -- We have to be in a paragraph
				            -- in correct code, but this could happen
				            -- if the user ended the paragraph by mistake
				            -- (we've already generated an error in that case).
			                ARM_Output.Text_Format (Output_Object,
								Format_Object.Text_Format);
			            -- else no text.
			            end if;
				end if;
		        end case;
		    end if;
		    Format_Object.In_Change :=
			Arm_Output."/=" (Format_Object.Text_Format.Change, ARM_Output.None);

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
					Format_Object.Text_Format.Change :=
				            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Prev_Change;
					Format_Object.Text_Format.Version :=
				            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Prev_Change_Version;
					Format_Object.Text_Format.Added_Version :=
				            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Prev_Added_Change_Version;

			                Check_Paragraph; -- We have to be in a paragraph
				            -- in correct code, but this could happen
				            -- if the user ended the paragraph by mistake
				            -- (we've already generated an error in that case).
			                ARM_Output.Text_Format (Output_Object,
								Format_Object.Text_Format);
			            -- else no text.
			            end if;
				end if;
		        end case;
		    end if;

		    Format_State.Nesting_Stack_Ptr := Format_State.Nesting_Stack_Ptr - 1;
		    -- Unstack the "New" parameter.
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
				                Check_Paragraph; -- Output the paragraph headers before changing the state.
						    -- This can only matter for a deletion without
						    -- an insertion; otherwise, we're already in a paragraph.
					        case Format_Object.Text_Format.Change is
					            when ARM_Output.Deletion | ARM_Output.None =>
						        Format_Object.Text_Format.Change := ARM_Output.Deletion;
						        Format_Object.Text_Format.Version :=
						            Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
						        Format_Object.Text_Format.Added_Version := '0';
					            when ARM_Output.Insertion =>
						        Format_Object.Text_Format.Change := ARM_Output.Both;
						        Format_Object.Text_Format.Added_Version :=
							    Format_Object.Text_Format.Version;
						        Format_Object.Text_Format.Version :=
						           Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
					            when ARM_Output.Both =>
						        Format_Object.Text_Format.Change := ARM_Output.Both;
							-- Added_Version is unchanged.
						        Format_Object.Text_Format.Version :=
						           Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
					        end case;
				                ARM_Output.Text_Format (Output_Object,
									Format_Object.Text_Format);
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
				            Check_Paragraph;
					    case Format_Object.Text_Format.Change is
					        when ARM_Output.Deletion | ARM_Output.None =>
						    Format_Object.Text_Format.Change := ARM_Output.Deletion;
						    Format_Object.Text_Format.Version :=
						       Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
						    Format_Object.Text_Format.Added_Version := '0';
					        when ARM_Output.Insertion =>
						    Format_Object.Text_Format.Change := ARM_Output.Both;
						    Format_Object.Text_Format.Added_Version :=
						       Format_Object.Text_Format.Version;
						    Format_Object.Text_Format.Version :=
						       Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
					        when ARM_Output.Both =>
						    Format_Object.Text_Format.Change := ARM_Output.Both;
						    -- Added_Version is unchanged.
						    Format_Object.Text_Format.Version :=
						       Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version;
					    end case;
				            ARM_Output.Text_Format (Output_Object,
								    Format_Object.Text_Format);
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

		when Change_Added_Param =>
		    declare
			Disposition : ARM_Output.Change_Type;
			use type ARM_Output.Change_Type;
		    begin
		        Format_Object.In_Change := False;

		        Calc_Change_Disposition (
			    Format_Object => Format_Object,
			    Version => Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version,
			    Operation => ARM_Output.Insertion,
			    Text_Kind => Disposition);

		        if Disposition = Do_Not_Display_Text then
			    -- The new text was ignored.
		            null; -- Nothing to do (we nulled out the text before we got here).
		        elsif Disposition = ARM_Output.None then
			    -- Display the text normally.
			    null; -- Nothing special to do.
		        elsif Disposition = ARM_Output.Deletion then
			    raise Program_Error; -- A deletion inside of an insertion command!
		        else -- Insertion.
			    -- Reset the state to normal:
			    Format_Object.Text_Format.Change := ARM_Output.None;
			    Format_Object.Text_Format.Version := '0';
			    Format_Object.Text_Format.Added_Version := '0';

			    Check_Paragraph; -- We have to be in a paragraph
			        -- in correct code, but this could happen
			        -- if the user ended the paragraph by mistake
			        -- (we've already generated an error in that case).
		            ARM_Output.Text_Format (Output_Object,
						    Format_Object.Text_Format);
		        end if;
		    end;

		when Change_Deleted_Param =>
		    declare
			Disposition : ARM_Output.Change_Type;
			use type ARM_Output.Change_Type;
		    begin
		        Format_Object.In_Change := False;

		        Calc_Change_Disposition (
			    Format_Object => Format_Object,
			    Version => Format_State.Nesting_Stack(Format_State.Nesting_Stack_Ptr-1).Change_Version,
			    Operation => ARM_Output.Deletion,
			    Text_Kind => Disposition);

		        if Disposition = Do_Not_Display_Text then
			    -- The old text was ignored.
		            null; -- Nothing to do (we nulled out the text before we got here).
		        elsif Disposition = ARM_Output.None then
			    -- Display the text normally.
			    null; -- Nothing special to do.
		        elsif Disposition = ARM_Output.Insertion then
			    raise Program_Error; -- An insertion inside of a deletion command!
		        else -- Insertion.
			    -- Reset the state to normal:
			    Format_Object.Text_Format.Change := ARM_Output.None;
			    Format_Object.Text_Format.Version := '0';
			    Format_Object.Text_Format.Added_Version := '0';

		            Check_Paragraph; -- We have to be in a paragraph
			        -- in correct code, but this could happen
			        -- if the user ended the paragraph by mistake
			        -- (we've already generated an error in that case).
		            ARM_Output.Text_Format (Output_Object,
						    Format_Object.Text_Format);
			end if;
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
		    if Format_Object.In_Change then
			-- Close all formatting:
			declare
			    Closed_Formatting : ARM_Output.Format_Type :=
				ARM_Output.NORMAL_FORMAT;
			begin
			    Closed_Formatting.Font := Format_Object.Text_Format.Font; -- No clear default for these.
			    Closed_Formatting.Size := Format_Object.Text_Format.Size;

		            ARM_Output.Text_Format (Output_Object,
						    Closed_Formatting);
			end;
		    end if;
		    ARM_Output.End_Hang_Item (Output_Object);
		    if Format_Object.In_Change then
			-- Reset to the normal case:
		        ARM_Output.Text_Format (Output_Object,
					        Format_Object.Text_Format);
		    end if;
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
		    Format_Object.Text_Format.Size :=
			Format_Object.Text_Format.Size - 2;
		    Format_Object.Text_Format.Location := ARM_Output.Subscript;
		    ARM_Output.Text_Format (Output_Object,
					    Format_Object.Text_Format);
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
		    Format_Object.Text_Format.Size :=
			Format_Object.Text_Format.Size - 2;
		    Format_Object.Text_Format.Location := ARM_Output.Superscript;
		    ARM_Output.Text_Format (Output_Object,
					    Format_Object.Text_Format);
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
		-- the trailing blank was dropped. It originally appeared in
		-- Infosys.MSS.
		Ada.Text_IO.Put_Line("** Stand-alone '@' on line " & ARM_Input.Line_String (Input_Object));
		return;
	    end if;
	    ARM_Input.Replace_Char (Input_Object);
	    ARM_Input.Get_Name (Input_Object, Command_Name);
--Ada.Text_IO.Put_Line("!!Command=" & Ada.Strings.Fixed.Trim(Command_Name, Ada.Strings.Both));

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
			Thin_Line | Thick_Line | Table | Picture_Alone |
			To_Glossary | Change_To_Glossary |
			Implementation_Defined |
			Change_Implementation_Defined |
			Change_Implementation_Advice |
			Change_Documentation_Requirement |
			Labeled_Section |
			Labeled_Section_No_Break |
			Labeled_Clause | Labeled_Subclause | Labeled_Subsubclause |
			Labeled_Revised_Section | Labeled_Revised_Clause |
			Labeled_Revised_Subclause | Labeled_Revised_Subsubclause |
			Labeled_Added_Section | Labeled_Added_Clause |
			Labeled_Added_Subclause | Labeled_Added_Subsubclause |
		        Labeled_Deleted_Clause |
			Labeled_Deleted_Subclause | Labeled_Deleted_Subsubclause |
			Preface_Section |
			Labeled_Annex | Labeled_Revised_Annex | Labeled_Added_Annex |
			Labeled_Informative_Annex |
			Labeled_Revised_Informative_Annex | Labeled_Added_Informative_Annex |
			Labeled_Normative_Annex |
		        Labeled_Revised_Normative_Annex | Labeled_Added_Normative_Annex |
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
--Ada.Text_IO.Put_Line("!!Char=" & Char & " Nesting=" & Natural'Image(Format_State.Nesting_Stack_Ptr));
	        case Char is
		    when '@' =>
		        Process_Special;
		    when Ascii.LF =>
		        ARM_Input.Get_Char (Input_Object, Char);
		        if Char /= Ascii.LF then
			    -- Soft line break.
			    if Format_Object.Next_Paragraph_Format_Type = Example_Text or else
			       Format_Object.Next_Paragraph_Format_Type = Child_Example_Text or else
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
				-- There should be nothing above the table at
				-- this point. Complain about other commands
				-- (this is a signficant aid to building tables):
				declare
				    Start_Depth : Natural := 1;
				begin
				    --Find the table:
				    for I in reverse 1 .. Format_State.Nesting_Stack_Ptr loop
				        if Format_State.Nesting_Stack(I).Command = Table then
					    Start_Depth := I;
					    exit;
				        end if;
				    end loop;
				    if Format_State.Nesting_Stack(Start_Depth+1).Command /= Table_Param_Body then
				        Ada.Text_IO.Put_Line ("   ** Wrong command on top of table, line " & ARM_Input.Line_String (Input_Object));
				        Ada.Text_IO.Put_Line ("      Command=" & Format_State.Nesting_Stack(Start_Depth+1).Name & " Class=" &
					    Data.Command_Type'Image(Format_State.Nesting_Stack(Start_Depth+1).Command));
				    elsif Format_State.Nesting_Stack_Ptr /= Start_Depth+1 then
				        Ada.Text_IO.Put_Line ("   ** Unfinished commands detected at end of row, line " & ARM_Input.Line_String (Input_Object));
				    end if;
				    for I in reverse Start_Depth+2 .. Format_State.Nesting_Stack_Ptr loop
				        Ada.Text_IO.Put_Line ("      Open command=" &
					    Format_State.Nesting_Stack(I).Name & " Class=" &
					    Data.Command_Type'Image(Format_State.Nesting_Stack(I).Command));
				    end loop;
				end;

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
			       Format_Object.Next_Paragraph_Format_Type = Child_Example_Text or else
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
			   Format_Object.Next_Paragraph_Format_Type = Child_Example_Text or else
			   Format_Object.Next_Paragraph_Format_Type = Indented_Example_Text or else
			   Format_Object.Next_Paragraph_Format_Type = Display or else
			   Format_Object.Next_Paragraph_Format_Type = Syntax_Display or else
			   Format_Object.Next_Paragraph_Format_Type = Syntax_Production then
			    -- Spaces are significant these formats.
			    Check_Paragraph;
			    ARM_Output.Hard_Space (Output_Object);
		        else
			    if Format_Object.In_Paragraph then
				if Format_Object.No_Start_Paragraph then
				    -- Not really in a paragraph.
Ada.Text_IO.Put_Line ("Attempt to write into a deleted paragraph, on line " & ARM_Input.Line_String (Input_Object));
				    -- We'll probably crash soon.
				    null;
				else
			            ARM_Output.Ordinary_Character (Output_Object, ' ');
				end if;
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
    exception
	when ARM_Output.Not_Valid_Error =>
	    Ada.Text_IO.Put_Line ("** Output validity error processing line " & ARM_Input.Line_String (Input_Object));
	    raise;
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
	    Format_Object.Clause_Number := (Section => Section_Number,
		Clause => 0, Subclause => 0, Subsubclause => 0);
	    declare
		use type ARM_Contents.Section_Number_Type;
	    begin
		if Section_Number = 0 then -- No title at all.
	            ARM_Output.Section (Output_Object,
				        Section_Title => "",
				        Section_Name => Section_Name);
		elsif Section_Number < ARM_Contents.ANNEX_START then
	            ARM_Output.Section (Output_Object,
				        Section_Title => Ada.Strings.Fixed.Trim (
							 ARM_Contents.Lookup_Title (ARM_Contents.Section,
								(Section => Section_Number, others => 0)), Ada.Strings.Right),
				        Section_Name => Section_Name);
		else
		    -- We don't have a way to tell between the three kinds of annexes, so we try them all:
		    begin
	                ARM_Output.Section (Output_Object,
				            Section_Title => Ada.Strings.Fixed.Trim (
							     ARM_Contents.Lookup_Title (ARM_Contents.Normative_Annex,
								    (Section => Section_Number, others => 0)), Ada.Strings.Right),
				            Section_Name => Section_Name);
		    exception
			when ARM_Contents.Not_Found_Error =>
			    begin
                                ARM_Output.Section (Output_Object,
				                    Section_Title => Ada.Strings.Fixed.Trim (
							             ARM_Contents.Lookup_Title (ARM_Contents.Informative_Annex,
									    (Section => Section_Number, others => 0)), Ada.Strings.Right),
				                    Section_Name => Section_Name);
			    exception
				when ARM_Contents.Not_Found_Error =>
	                            ARM_Output.Section (Output_Object,
				                        Section_Title => Ada.Strings.Fixed.Trim (
							                 ARM_Contents.Lookup_Title (ARM_Contents.Plain_Annex,
									        (Section => Section_Number, others => 0)), Ada.Strings.Right),
				                        Section_Name => Section_Name);
			            -- If this fails, too, we just propagate to the outer handler.
			    end;
		    end;
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
	    Format_Object.Enumerated_Level := 0;

	    Format_Object.Text_Format := ARM_Output.NORMAL_FORMAT;

	    Format_Object.No_Prefix := False;
	    Format_Object.No_Para_Num := False;
	    Format_Object.Keep_with_Next := False;
	    Format_Object.Space_After := ARM_Output.Normal;
	    Format_Object.No_Breaks := False;
	    Format_Object.In_Change := False;
	    Format_Object.Last_Non_Space := False;

	    Format_Object.Next_Paragraph_Change_Kind := ARM_Database.None;

	    Format_Object.Style := ARM_Output.Normal; -- The default.
	    Format_Object.Indent := 0; -- No indent to start.
	    Format_Object.In_Paragraph := False;
	    Format_Object.No_Start_Paragraph := False;
	end if;

	Real_Process (Format_Object, Format_State, Input_Object, Output_Object);

	-- Reached end of the file/input object.
	-- Kill any open paragraph:
	if Format_Object.In_Paragraph and then (not Format_Object.No_Start_Paragraph) then
	    ARM_Output.End_Paragraph (Output_Object);
	    Format_Object.In_Paragraph := False;
	    Format_Object.No_Start_Paragraph := False;
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
		      No_Annotations : in Boolean) is
	-- Format the contents of Text, writing the results to
	-- Output_Object. (Output_Object uses dispatching calls to provide
	-- the correct formatting). Text is assumed to be a component of
	-- a larger section. Text_Name is an identifying name for error messages.
	-- If No_Annotations is true, we don't want any annotations even if we
	-- are generating a document with annotations.
	Input_Object : Arm_String.String_Input_Type;
	Format_State : Format_State_Type;
	Real_Include_Annotations : Boolean := Format_Object.Include_Annotations;
    begin
	if No_Annotations then
            Format_Object.Include_Annotations := False;
	end if;
	Arm_String.Open (Input_Object, Text, Text_Name);
	     -- Open the input object using a string for input.
	Real_Process (Format_Object, Format_State, Input_Object, Output_Object);
	Arm_String.Close (Input_Object);
	Format_Object.Include_Annotations := Real_Include_Annotations;
	if Format_State.Nesting_Stack_Ptr /= 0 then
	    Ada.Text_IO.Put_Line ("   ** Unfinished commands detected.");
	end if;
    end Format;

end ARM_Format;
