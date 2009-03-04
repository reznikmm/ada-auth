@Part(implperm, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/implperm.mss,v $}
@comment{$Revision: 1.7 $ $Date: 2009/02/15 08:00:55 $}


@LabeledSection{package Asis.Implementation.Permissions}

@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis.Implementation],Child=[Permissions]}Asis.Implementation.Permissions
shall exist. The package
shall provide interfaces equivalent to those described in the
following subclauses.],
Old=[@f{@key[package] @ChildUnit{Parent=[Asis.Implementation],Child=[Permissions]}Asis.Implementation.Permissions @key[is]}]}


@ChgNote{SI99-0025-1 remove subprogram}
@LabeledRevisedClause{Version=[2],New=[obsolete function IFPNNS],
Old=[function Is_Formal_Parameter_Named_Notation_Supported]}
@ChgAdded{Version=[2],Text=[@b{@i{This clause header is left for now;
removing it now would change all of the clause numbers,
and that would make a mess for editing and reference purposes. Ultimately,
when the final standard is produced, it will be removed. - GG}}]}
@begin{DescribeCode}
@begin{Example}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Formal_Parameter_Named_Notation_Supported} @key[return] Boolean;]}
@end{Example}
@ChgDeleted{Version=[2],Text=[Returns True if it is possible to detect usage of
named notation.]}

l@ChgDeleted{Version=[2],Text=[Returns False if this implementation will always change parameter lists
using named notation to positional lists in function, subprogram, and
entry calls. In that case, the Formal_Parameter query will always return
a Nil_Element unless the parameter list is obtained with Normalized = True.]}

@ChgDeleted{Version=[2],Text=[This function affects association lists for
aggregates, instantiations, discriminant lists, entry calls, and subprogram
calls.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 remove subprogram}
@LabeledRevisedClause{Version=[2],New=[obsolete function D_In_Mode_S],
Old=[function Default_In_Mode_Supported]}
@ChgAdded{Version=[2],Text=[@b{@i{This clause header is left for now;
removing it now would change all of the clause numbers,
and that would make a mess for editing and reference purposes. Ultimately,
when the final standard is produced, it will be removed. - GG}}]}
@begin{DescribeCode}
@begin{Example}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Default_In_Mode_Supported} @key[return] Boolean;]}
@end{Example}
@ChgDeleted{Version=[2],Text=[Returns True if the A_Default_In_Mode kind is
supported by this implementation.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 remove subprogram}
@LabeledRevisedClause{Version=[2],New=[obsolete function G_Act_Part_Norm],
Old=[function Generic_Actual_Part_Normalized]}
@ChgAdded{Version=[2],Text=[@b{@i{This clause header is left for now;
removing it now would change all of the clause numbers,
and that would make a mess for editing and reference purposes. Ultimately,
when the final standard is produced, it will be removed. - GG}}]}
@begin{DescribeCode}
@begin{Example}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Generic_Actual_Part_Normalized} @key[return] Boolean;]}
@end{Example}
@ChgDeleted{Version=[2],Text=[Returns True if the query Generic_Actual_Part
will always return artificial Is_Normalized associations using the
defining_identifier instead of the generic_formal_parameter_selector_name, and
using default_expression or default_name.]}

@ChgDeleted{Version=[2],Text=[If Generic_Actual_Part_Normalized then the query Generic_Actual_Part will
always behave as if called with Normalized => True.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 remove subprogram}
@LabeledRevisedClause{Version=[2],New=[obsolete function Rec_Comp_Assoc_Norm],
Old=[function Record_Component_Associations_Normalized]}
@ChgAdded{Version=[2],Text=[@b{@i{This clause header is left for now;
removing it now would change all of the clause numbers,
and that would make a mess for editing and reference purposes. Ultimately,
when the final standard is produced, it will be removed. - GG}}]}
@begin{DescribeCode}
@begin{Example}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Record_Component_Associations_Normalized} @key[return] Boolean;]}
@end{Example}
@ChgDeleted{Version=[2],Text=[Returns True if the query
Record_Component_Associations will always return artificial Is_Normalized
associations using the defining_identifier instead of the
component_selector_name.]}

@ChgDeleted{Version=[2],Text=[If Record_Component_Associations_Normalized then
the query Record_Component_Associations will always behave as if called with
Normalized => True.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 remove subprogram}
@LabeledRevisedClause{Version=[2],New=[obsolete function IPrefix_Call_S],
Old=[function Is_Prefix_Call_Supported]}
@ChgAdded{Version=[2],Text=[@b{@i{This clause header is left for now;
removing it now would change all of the clause numbers,
and that would make a mess for editing and reference purposes. Ultimately,
when the final standard is produced, it will be removed. - GG}}]}
@begin{DescribeCode}
@begin{Example}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Prefix_Call_Supported} @key[return] Boolean;]}
@end{Example}
@ChgDeleted{Version=[2],Text=[Returns True if the ASIS implementation has the
ability to determine whether calls are in prefix form.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 remove subprogram}
@LabeledRevisedClause{Version=[2],New=[obsolete function FuncC_Param_Norm],
Old=[function Function_Call_Parameters_Normalized]}
@ChgAdded{Version=[2],Text=[@b{@i{This clause header is left for now;
removing it now would change all of the clause numbers,
and that would make a mess for editing and reference purposes. Ultimately,
when the final standard is produced, it will be removed. - GG}}]}
@begin{DescribeCode}
@begin{Example}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Function_Call_Parameters_Normalized} @key[return] Boolean;]}
@end{Example}
@ChgDeleted{Version=[2],Text=[Returns True if the query
Function_Call_Parameters will always return artificial Is_Normalized
associations using the defining_identifier instead of the
formal_parameter_selector_name, and using the default_expression.]}

@ChgDeleted{Version=[2],Text=[If Function_Call_Parameters_Normalized then the
query Function_Call_Parameters will always behave as if called with Normalized
=> True.]}
@end{DescribeCode}

@ChgNote{SI99-0025-1 remove subprogram}
@LabeledRevisedClause{Version=[2],New=[obsolete function Call_St_Param_Norm],
Old=[function Call_Statement_Parameters_Normalized]}
@ChgAdded{Version=[2],Text=[@b{@i{This clause header is left for now;
removing it now would change all of the clause numbers,
and that would make a mess for editing and reference purposes. Ultimately,
when the final standard is produced, it will be removed. - GG}}]}
@begin{DescribeCode}
@begin{Example}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Call_Statement_Parameters_Normalized} @key[return] Boolean;]}
@end{Example}
@ChgDeleted{Version=[2],Text=[Returns True if the query
Call_Statement_Parameters will always return artificial Is_Normalized
associations using the defining_identifier instead of the
formal_parameter_selector_name, and using the default_expression.]}

@ChgDeleted{Version=[2],Text=[If Call_Statement_Parameters_Normalized then the
query Call_Statement_Parameters will always behave as if called with Normalized
=> True.]}

@begin{SingleNote}
@ChgDeleted{Version=[2],Text=[It is not possible to obtain either a normalized
or unnormalized Discriminant_Association list for an unconstrained record or
derived subtype_indication where the discriminant_association is supplied by
default; there is no constraint to query, and a Nil_Element is returned from
the query Subtype_Constraint.]}
@end{SingleNote}
@end{DescribeCode}


@ChgNote{SI99-0025-1 remove subprogram}
@LabeledRevisedClause{Version=[2],New=[obsolete function Disc_Asc_Norm],
Old=[function Discriminant_Associations_Normalized]}
@ChgAdded{Version=[2],Text=[@b{@i{This clause header is left for now;
removing it now would change all of the clause numbers,
and that would make a mess for editing and reference purposes. Ultimately,
when the final standard is produced, it will be removed. - GG}}]}
@begin{DescribeCode}
@begin{Example}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Discriminant_Associations_Normalized} @key[return] Boolean;]}
@end{Example}
@ChgDeleted{Version=[2],Text=[Returns True if the query
Discriminant_Associations will always return artificial Is_Normalized
associations using the defining_identifier instead of the
discriminant_selector_name.]}

@ChgDeleted{Version=[2],Text=[If Discriminant_Associations_Normalized then the
query Discriminant_Associations will always behave as if called with Normalized
=> True.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 remove subprogram}
@LabeledRevisedClause{Version=[2],New=[obsolete function ILine_NumS],
Old=[function Is_Line_Number_Supported]}
@ChgAdded{Version=[2],Text=[@b{@i{This clause header is left for now;
removing it now would change all of the clause numbers,
and that would make a mess for editing and reference purposes. Ultimately,
when the final standard is produced, it will be removed. - GG}}]}
@begin{DescribeCode}
@begin{Example}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Line_Number_Supported} @key[return] Boolean;]}
@end{Example}
@ChgDeleted{Version=[2],Text=[Returns True if the implementation can return
valid line numbers for Elements.]}

@ChgDeleted{Version=[2],Text=[An implementation may choose to ignore line
number values in which case this function returns False.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 remove subprogram}
@LabeledRevisedClause{Version=[2],New=[obsolete function ISCol_PosS],
Old=[function Is_Span_Column_Position_Supported]}
@ChgAdded{Version=[2],Text=[@b{@i{This clause header is left for now;
removing it now would change all of the clause numbers,
and that would make a mess for editing and reference purposes. Ultimately,
when the final standard is produced, it will be removed. - GG}}]}
@begin{DescribeCode}
@begin{Example}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Span_Column_Position_Supported} @key[return] Boolean;]}
@end{Example}
@ChgDeleted{Version=[2],Text=[Returns True if the implementation can return
valid character positions for elements.]}

@ChgDeleted{Version=[2],Text=[An implementation may choose to ignore column
character position values within spans in which case this function returns
False. This function will be False if Is_Line_Number_Supported = False.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 remove subprogram}
@LabeledRevisedClause{Version=[2],New=[obsolete function ICommS],
Old=[function Is_Commentary_Supported]}
@ChgAdded{Version=[2],Text=[@b{@i{This clause header is left for now;
removing it now would change all of the clause numbers,
and that would make a mess for editing and reference purposes. Ultimately,
when the final standard is produced, it will be removed. - GG}}]}
@begin{DescribeCode}
@begin{Example}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Commentary_Supported} @key[return] Boolean;]}
@end{Example}
@ChgDeleted{Version=[2],Text=[Returns True if the implementation can return
comments.]}

@ChgDeleted{Version=[2],Text=[An implementation may choose to ignore comments
in the text in which case the function Is_Commentary_Supported returns False.]}
@end{DescribeCode}


@LabeledClause{function Attributes_Are_Supported}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Attributes_Are_Supported} @key[return] Boolean;
@end{Example}

Returns True if an implementation supports compilation unit attributes.
Returns False if all attributes will return Has_Attribute() = False.
@end{DescribeCode}


@LabeledClause{function Implicit_Components_Supported}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Implicit_Components_Supported} @key[return] Boolean;
@end{Example}

Returns True if the implementation provides elements representing
implicit implementation-defined record components.
@end{DescribeCode}


@ChgNote{SI99-0025-1 remove subprogram}
@LabeledRevisedClause{Version=[2],New=[obsolete function Obj_Dec_Norm],
Old=[function Object_Declarations_Normalized]}

@begin{DescribeCode}
@begin{Example}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Object_Declarations_Normalized} @key[return] Boolean;]}
@end{Example}
@ChgDeleted{Version=[2],Text=[Returns True if the implementation normalizes
multiple object declarations to an equivalent sequence of single declarations.]}
@end{DescribeCode}


@LabeledClause{function Predefined_Operations_Supported}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Predefined_Operations_Supported} @key[return] Boolean;
@end{Example}

Returns True if the implementation supports queries of predefined
operations.
@end{DescribeCode}


@ChgNote{SI99-0025-1 remove subprogram}
@Chg{Version=[2],New=[],Old=[obsolete function Inherited_Declarations_Supported]}
@ChgNote{This clause header was removed as there are no remaining clauses
after it. - RLB}

@begin{DescribeCode}
@begin{Example}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Inherited_Declarations_Supported} @key[return] Boolean;]}
@end{Example}
@ChgDeleted{Version=[2],Text=[Returns True if the implementation supports
queries of inherited declarations.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 remove subprogram}
@Chg{Version=[2],New=[],Old=[obsolete function Inherited_Subprograms_Supported]}
@ChgNote{This clause header was removed as there are no remaining clauses
after it. - RLB}

@ChgAdded{Version=[2],Text=[@b{@i{This clause header is left for now;
removing it now would change all of the clause numbers,
and that would make a mess for editing and reference purposes. Ultimately,
when the final standard is produced, it will be removed. - GG}}]}
@begin{DescribeCode}
@begin{Example}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Inherited_Subprograms_Supported} @key[return] Boolean;]}
@end{Example}
@ChgDeleted{Version=[2],Text=[Returns True if the implementation supports
queries of inherited subprograms.]}
@end{DescribeCode}


@ChgNote{SI99-0025-1 remove subprogram}
@Chg{Version=[2],New=[],Old=[obsolete function Generic_Macro_Expansion_Supported]}
@ChgNote{This clause header was removed as there are no remaining clauses
after it. - RLB}
@begin{DescribeCode}
@begin{Example}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Generic_Macro_Expansion_Supported} @key[return] Boolean;]}
@end{Example}
@ChgDeleted{Version=[2],Text=[Returns True if the implementation expands
generics using macros to @Chg{Version=[1],New=[support],Old=[supports]}
queries.]}
@end{DescribeCode}

@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[end] Asis.Implementation.Permissions;]}
@end{Example}

