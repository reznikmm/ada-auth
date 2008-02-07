@Part(glossary, Root="asis.msm")

@Comment{$Date: 2008/02/06 06:23:47 $}
@LabeledAddedNormativeAnnex{Version=[2],Name=[Obsolescent Features]}

@comment{$Source: e:\\cvsroot/ARM/ASIS/obsolescent.mss,v $}
@comment{$Revision: 1.5 $}

@LabeledAddedClause{Version=[2],Name=[Annex Contents]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Text=[@Defn{obsolescent feature}
This Annex contains descriptions of features of ASIS whose functionality is
largely redundant with other features
defined by this International Standard.
Use of these features is not recommended in newly written programs.]}
@end{Intro}

@ChgNote{SI99-0025-1 add chapter on newly obsolete subprograms}
@LabeledAddedClause{Version=[2],Name=[Obsolescent Features in package Asis.Implementation.Permissions]}

@LabeledAddedSubclause{Version=[2],Name=[Introduction for Obsolescent Features in package Asis.Implementation.Permissions]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2], Text=[In addition to the interfaces defined in section
@RefSecNum{package Asis.Implementation.Permissions}, the library package
Asis.Implementation.Permissions also shall provide interfaces equivalent to
the obsolescent ones described in the following subclauses.]}


@ChgNote{SI99-0025-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Is_Formal_Parameter_Named_Notation_Supported]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2], Text=[This query always returns True. Use of the query
Is_Formal_Parameter_Named_Notation_Supported is not recommended in new
programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[@key[function] Is_Formal_Parameter_Named_Notation_Supported @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[It shall be possible to detect usage
of named notation.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Default_In_Mode_Supported]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2], Text=[This query always returns True. Use of the query
Default_In_Mode_Supported is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[@key[function] Default_In_Mode_Supported @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[The A_Default_In_Mode kind shall be
supported by the implementation.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Generic_Actual_Part_Normalized]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2], Text=[This query always returns False. Use of the query
Generic_Actual_Part_Normalized is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[@key[function] Generic_Actual_Part_Normalized @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[The query Generic_Actual_Part shall be able to
return both normalized and unnormalized associations.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Record_Component_Associations_Normalized]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2], Text=[This query always returns False. Use of the query
Record_Component_Associations_Normalized is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[@key[function] Record_Component_Associations_Normalized @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[The query Record_Component_Associations shall be
able to return both normalized and unnormalized associations.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Is_Prefix_Call_Supported]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2], Text=[This query always returns True. Use of the query
Is_Prefix_Call_Supported is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[@key[function] Is_Prefix_Call_Supported @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[The ASIS implementation shall have the ability to
determine whether calls are in prefix form.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 add section on newly obsolete subprogram}

@LabeledAddedSubclause{Version=[2],Name=[function Function_Call_Parameters_Normalized]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2], Text=[This query always returns False. Use of the query
Function_Call_Parameters_Normalized is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[@key[function] Function_Call_Parameters_Normalized @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[The query
Function_Call_Parameters shall be able to return both normalized and
unnormalized associations.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Call_Statement_Parameters_Normalized]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2], Text=[This query always returns False. Use of the query
Call_Statement_Parameters_Normalized is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[@key[function] Call_Statement_Parameters_Normalized @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[The query Call_Statement_Parameters shall be able
to return both normalized and unnormalized associations.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Discriminant_Associations_Normalized]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2], Text=[This query always returns False. Use of the query
Discriminant_Association_Normalized is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[@key[function] Discriminant_Associations_Normalized @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[The query Discriminant_Associations shall be able
to return both normalized and unnormalized associations.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Is_Line_Number_Supported]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2], Text=[This query always returns True. Use of the query
Is_Line_Number_Supported is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[@key[function] Is_Line_Number_Supported @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[The implementation shall be able to return valid
line numbers for Elements.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Is_Span_Column_Position_Supported]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2], Text=[This query always returns True. Use of the query
Is_Span_Column_Position_Supported is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[@key[function] Is_Span_Column_Position_Supported @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[The implementation shall be able to return valid
character positions for elements.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Is_Commentary_Supported]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2], Text=[This query always returns True. Use of the query
Is_Commentary_Supported is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[@key[function] Is_Commentary_Supported @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[The implementation shall be able to return comments.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Object_Declarations_Normalized]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2], Text=[This query always returns False. Use of the query
Object_Declarations_Normalized is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[@key[function] Object_Declarations_Normalized @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[The implementation shall not normalize multiple
object declarations to an equivalent sequence of single declarations.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Inherited_Declarations_Supported]}

@ChgAdded{Version=[2], Text=[This query always returns True. Use of the query
Inherited_Declarations_Supported is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgAdded{Version=[2],Text=[@key[function] Inherited_Declarations_Supported @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[The implementation shall support queries of
inherited declarations.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Inherited_Subprograms_Supported]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2], Text=[This query always returns True. Use of the query
Inherited_Subprograms_Supported is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[@key[function] Inherited_Subprograms_Supported @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[The implementation shall support queries of
inherited subprograms.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Generic_Macro_Expansion_Supported]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2], Text=[This query always returns True. Use of the query
Generic_Macro_Expansion_Supported is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[@key[function] Generic_Macro_Expansion_Supported @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2],Text=[The implementation shall expand generics using
macros to support queries.]}
@end{DescribeCode}

@begin{Ramification}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0025-1]}
@ChgAdded{Version=[2], Text=[This only requires an implementation to expand
generics for ASIS purposes; it makes no requirement on how an Ada compiler
implements generics.]}
@end{Ramification}


@ChgNote{SI99-0027-1 add chapter on newly obsolete subprograms}
@LabeledAddedClause{Version=[2],Name=[Obsolescent Features in package Asis.Declarations]}

@LabeledAddedSubclause{Version=[2],Name=[Introduction for Obsolescent Features in package Asis.Declarations]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Text=[In addition to the interfaces defined in section
@RefSecNum{package Asis.Declarations}, the library package
Asis.Declarations also shall provide interfaces equivalent to the obsolescent
ones described in the following subclauses.]}

@ChgNote{SI99-0027-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Body_Block_Statement]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Text=[This function is redundant with the queries
Body_Declarative_Items, Body_Statements, and Body_Exception_Handlers.
Use of the query Body_Block_Statement is not recommended in new
programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Body_Block_Statement} (Declaration : @key[in] Asis.Declaration)
                              @key[return] Asis.Statement;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Text=[Declaration specifies the program unit body to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Text=[Returns a block statement that is the structural
equivalent of the body. The block statement is not Is_Part_Of_Implicit. The
block includes the declarative part, the sequence of statements, and any
exception handlers.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Declaration expects an
element of one of the following Declaration_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[A_Function_Body_Declaration
A_Procedure_Body_Declaration
A_Package_Body_Declaration
A_Task_Body_Declaration
An_Entry_Body_Declaration]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Returns an element that
has one of the following Statement_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[A_Block_Statement]}
@end{Display}
@end{DescribeCode}

@begin{SingleNote}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Text=[This function is never called by Traverse_Element.]}
@end{SingleNote}

@begin{UsageNote}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Text=[This function is intended for compatibility with
ASIS 83.]}
@end{UsageNote}


@ChgNote{SI99-0004-1 add section on newly obsolete subprogram}
@LabeledAddedSubClause{Version=[2],Name=[function Object_Declaration_View]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[This function has been replaced by
Object_Declaration_Subtype. Use of the function Object_Declaration_View is not
recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Object_Declaration_View} (Declaration : @key[in] Asis.Declaration)
              @key[return] Asis.Definition;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[Declaration specifies the
declaration element to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[Returns the definition characteristics that form
the view of the object_declaration. The view is the subtype_indication or full
type definition of the object_declaration. An initial value, if any, is not
part of this view.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[For a single_task_declaration or
single_protected_declaration, returns the task_definition or
protected_definition following the reserved word @key[is].]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[Returns a Nil_Element for a single_task_declaration
that has no explicit task_definition.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[For a Component_Declaration, returns the
Component_Definition following the colon.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[For all other object_declaration variables or
constants, returns the subtype_indication or array_type_definition following
the colon.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1],ARef=[SI99-0010-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Declaration expects an
element that has one of the following Declaration_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[A_Variable_Declaration
A_Constant_Declaration
A_Deferred_Constant_Declaration
A_Single_Protected_Declaration
A_Single_Task_Declaration
A_Component_Declaration
A_Return_Object_Specification]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Returns an element
that has one of the following Definition_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Not_A_Definition
A_Type_Definition that has one of the following Type_Kinds:
    A_Constrained_Array_Definition
A_Subtype_Indication
A_Task_Definition
A_Protected_Definition
A_Component_Definition]}
@end{Display}
@end{DescribeCode}


@ChgNote{SI99-0004-1 add section on newly obsolete subprogram}
@LabeledAddedSubClause{Version=[2],Name=[function Declaration_Subtype_Mark]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[This function has been replaced by
Object_Declaration_Subtype. Use of the function Declaration_Subtype_Mark is not
recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Declaration_Subtype_Mark} (Declaration : @key[in] Asis.Declaration)
                                  @key[return] Asis.Expression;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[Declaration specifies the
declaration element to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[Returns the expression element that names the
subtype_mark of the declaration.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Declaration expects
an element that has one of the following Declaration_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[A_Discriminant_Specification
A_Parameter_Specification
A_Formal_Object_Declaration
An_Object_Renaming_Declaration]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Returns an element
that has one of the following Expression_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[An_Identifier
A_Selected_Component
An_Attribute_Reference]}
@end{Display}
@end{DescribeCode}



@ChgNote{SI99-0004-1 add section on newly obsolete subprogram}
@LabeledAddedSubClause{Version=[2],Name=[function Result_Profile]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[This function has been replaced by
function Result_Subtype. Use of the function Result_Profile is not
recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Result_Profile} (Declaration : @key[in] Asis.Declaration)
                     @key[return] Asis.Expression;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[Declaration specifies the function declaration
to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[Returns the subtype mark expression for the return
type for any function declaration.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Declaration expects an
element that has one of the following Declaration_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[A_Function_Declaration
A_Function_Body_Declaration
A_Function_Body_Stub
A_Function_Renaming_Declaration
A_Generic_Function_Declaration
A_Formal_Function_Declaration]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Returns an element
with one of Expression_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[An_Identifier
A_Selected_Component
An_Attribute_Reference]}
@end{Display}
@end{DescribeCode}


@ChgNote{SI99-0004-1 add chapter on newly obsolete functions}
@LabeledAddedClause{Version=[2],Name=[Obsolescent Features in package Asis.Definitions]}

@LabeledAddedSubclause{Version=[2],Name=[Introduction for Obsolescent Features in package Asis.Definitions]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[In addition to the interfaces defined in section
@RefSecNum{package Asis.Definitions}, the library package
Asis.Definitions also shall provide interfaces equivalent to the obsolescent
ones described in the following subclauses.]}

@ChgNote{SI99-0004-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Access_To_Function_Result_Profile]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[This function has been replaced by
Access_To_Function_Result_Subtype. Use of the function
Access_To_Function_Result_Profile is not recommended in new
programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Access_To_Function_Result_Profile}
               (Type_Definition : @key[in] Asis.Type_Definition)
                      @key[return] Asis.Expression;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[Type_Definition specifies the
Access_Type_Definition to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[Returns the subtype_mark expression for the return
type for the access function.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Type_Definition expects
an element of Type_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[An_Access_Type_Definition
A_Formal_Access_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[and Type_Definition
expects an element of Access_Type_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[An_Access_To_Function
An_Access_To_Protected_Function]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Returns an element that
has one of the following Expression_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[An_Identifier
A_Selected_Component]}
@end{Display}
@end{DescribeCode}

