with Ada.Characters.Handling,
     Ada.Strings.Fixed;
package body ARM_Format.Data is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package contains various data used by the input file parser.
    --
    -- ---------------------------------------
    -- Copyright 2011  AXE Consultants.
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
	elsif Canonical_Name = "picturealone" then
	    return Picture_Alone;
	elsif Canonical_Name = "pictureinline" then
	    return Picture_Inline;
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
	elsif Canonical_Name = "black" then
	    return Black;
	elsif Canonical_Name = "red" then
	    return Red;
	elsif Canonical_Name = "green" then
	    return Green;
	elsif Canonical_Name = "blue" then
	    return Blue;
	elsif Canonical_Name = "key" then
	    return Keyword;
	elsif Canonical_Name = "nt" then
	    return Non_Terminal;
	elsif Canonical_Name = "ntf" then
	    return Non_Terminal_Format;
	elsif Canonical_Name = "exam" then
	    return Example_Text;
	elsif Canonical_Name = "examcom" then
	    return Example_Comment;
	elsif Canonical_Name = "indexlist" then
	    return Index_List;
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
	elsif Canonical_Name = "subchildunit" then
	    return Index_Subprogram_Child_Unit;
	elsif Canonical_Name = "adatypedefn" then
	    return Index_Type;
	elsif Canonical_Name = "adasubtypedefn" then
	    return Index_Subtype;
	elsif Canonical_Name = "adasubdefn" then
	    return Index_Subprogram;
	elsif Canonical_Name = "adaexcdefn" then
	    return Index_Exception;
	elsif Canonical_Name = "adaobjdefn" then
	    return Index_Object;
	elsif Canonical_Name = "adapackdefn" then
	    return Index_Package;
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
	elsif Canonical_Name = "synf" then
	    return Syntax_Term_Undefined;
	elsif Canonical_Name = "syni" then
	    return Syntax_Prefix;
	elsif Canonical_Name = "syntaxsummary" then
	    return Syntax_Summary;
	elsif Canonical_Name = "syntaxxref" then
	    return Syntax_Xref;
	elsif Canonical_Name = "addedsyn" then
	    return Added_Syntax_Rule;
	elsif Canonical_Name = "deletedsyn" then
	    return Deleted_Syntax_Rule;
	elsif Canonical_Name = "toglossary" then
	    return To_Glossary;
	elsif Canonical_Name = "toglossaryalso" then
	    return To_Glossary_Also;
	elsif Canonical_Name = "chgtoglossary" then
	    return Change_To_Glossary;
	elsif Canonical_Name = "chgtoglossaryalso" then
	    return Change_To_Glossary_Also;
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
	elsif Canonical_Name = "chgimpladvice" then
	    return Change_Implementation_Advice;
	elsif Canonical_Name = "addedimpladvicelist" then
	    return Added_Implementation_Advice_List;
	elsif Canonical_Name = "chgdocreq" then
	    return Change_Documentation_Requirement;
	elsif Canonical_Name = "addeddocreqlist" then
	    return Added_Documentation_Requirements_List;
	elsif Canonical_Name = "chgaspectdesc" then
	    return Change_Aspect_Description;
	elsif Canonical_Name = "addedaspectlist" then
	    return Added_Aspect_Description_List;
	elsif Canonical_Name = "packagelist" then
	    return Package_List;
	elsif Canonical_Name = "typelist" then
	    return Type_List;
	elsif Canonical_Name = "subprogramlist" then
	    return Subprogram_List;
	elsif Canonical_Name = "exceptionlist" then
	    return Exception_List;
	elsif Canonical_Name = "objectlist" then
	    return Object_List;
	elsif Canonical_Name = "labeledsection" then
	    return Labeled_Section;
	elsif Canonical_Name = "labeledsectionnobreak" then
	    return Labeled_Section_No_Break;
	elsif Canonical_Name = "labeledclause" then
	    return Labeled_Clause;
	elsif Canonical_Name = "labeledsubclause" then
	    return Labeled_Subclause;
	elsif Canonical_Name = "labeledsubsubclause" then
	    return Labeled_Subsubclause;
	elsif Canonical_Name = "labeledannex" then
	    return Labeled_Annex;
	elsif Canonical_Name = "labeledinformativeannex" then
	    return Labeled_Informative_Annex;
	elsif Canonical_Name = "labelednormativeannex" then
	    return Labeled_Normative_Annex;
	elsif Canonical_Name = "unnumberedsection" then
	    return Unnumbered_Section;
	elsif Canonical_Name = "labeledrevisedannex" then
	    return Labeled_Revised_Annex;
	elsif Canonical_Name = "labeledrevisedinformativeannex" then
	    return Labeled_Revised_Informative_Annex;
	elsif Canonical_Name = "labeledrevisednormativeannex" then
	    return Labeled_Revised_Normative_Annex;
	elsif Canonical_Name = "labeledaddedannex" then
	    return Labeled_Added_Annex;
	elsif Canonical_Name = "labeledaddedinformativeannex" then
	    return Labeled_Added_Informative_Annex;
	elsif Canonical_Name = "labeledaddednormativeannex" then
	    return Labeled_Added_Normative_Annex;
	elsif Canonical_Name = "labeledrevisedsection" then
	    return Labeled_Revised_Section;
	elsif Canonical_Name = "labeledrevisedclause" then
	    return Labeled_Revised_Clause;
	elsif Canonical_Name = "labeledrevisedsubclause" then
	    return Labeled_Revised_Subclause;
	elsif Canonical_Name = "labeledrevisedsubsubclause" then
	    return Labeled_Revised_Subsubclause;
	elsif Canonical_Name = "labeledaddedsection" then
	    return Labeled_Added_Section;
	elsif Canonical_Name = "labeledaddedclause" then
	    return Labeled_Added_Clause;
	elsif Canonical_Name = "labeledaddedsubclause" then
	    return Labeled_Added_Subclause;
	elsif Canonical_Name = "labeledaddedsubsubclause" then
	    return Labeled_Added_Subsubclause;
	elsif Canonical_Name = "labeleddeletedclause" then
	    return Labeled_Deleted_Clause;
	elsif Canonical_Name = "labeleddeletedsubclause" then
	    return Labeled_Deleted_Subclause;
	elsif Canonical_Name = "labeleddeletedsubsubclause" then
	    return Labeled_Deleted_Subsubclause;
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
	elsif Canonical_Name = "locallink" then
	    return Local_Link;
	elsif Canonical_Name = "localtarget" then
	    return Local_Target;
	elsif Canonical_Name = "urllink" then
	    return URL_Link;
	elsif Canonical_Name = "ailink" then
	    return AI_Link;
	elsif Canonical_Name = "chg" then
	    return Change;
	elsif Canonical_Name = "chgadded" then
	    return Change_Added;
	elsif Canonical_Name = "chgdeleted" then
	    return Change_Deleted;
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
	elsif Canonical_Name = "singlenotename" then
	    return Single_Note_Name;
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
	elsif Canonical_Name = "inconsistent2005name" then
	    return Inconsistent2005_Name;
	elsif Canonical_Name = "incompatible2005name" then
	    return Incompatible2005_Name;
	elsif Canonical_Name = "extend2005name" then
	    return Extend2005_Name;
	elsif Canonical_Name = "diffword2005name" then
	    return Wording2005_Name;
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
	elsif Canonical_Name = "singlenotetitle" then
	    return Single_Note_Title;
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
	elsif Canonical_Name = "inconsistent2005title" then
	    return Inconsistent2005_Title;
	elsif Canonical_Name = "incompatible2005title" then
	    return Incompatible2005_Title;
	elsif Canonical_Name = "extend2005title" then
	    return Extend2005_Title;
	elsif Canonical_Name = "diffword2005title" then
	    return Wording2005_Title;
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
	elsif Canonical_Name = "unicode" then
	    return Unicode;
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
	elsif Canonical_Name = "smldotlessi" then
	    return Small_Dotless_I;
	elsif Canonical_Name = "capdottedi" then
	    return Capital_Dotted_I;
	else
	    return Unknown;
	end if;
    end Command;

end ARM_Format.Data;
