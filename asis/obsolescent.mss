@Part(glossary, Root="asis.msm")

@Comment{$Date: 2009/02/10 06:51:27 $}
@LabeledAddedNormativeAnnex{Version=[2],Name=[Obsolescent Features]}

@comment{$Source: e:\\cvsroot/ARM/ASIS/obsolescent.mss,v $}
@comment{$Revision: 1.7 $}

@LabeledAddedClause{Version=[2],Name=[Annex Contents]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Text=[@Defn{obsolescent feature}
This Annex contains descriptions of features of ASIS whose functionality is
largely redundant with other features
defined by this International Standard.
Use of these features is not recommended in newly written programs.]}
@end{Intro}

@ChgNote{SI99-0022-1 add chapter on newly obsolete subprograms}
@LabeledAddedClause{Version=[2],Name=[Obsolescent Features in package Asis]}

@LabeledAddedSubclause{Version=[2],Name=[Introduction for Obsolescent Features in package Asis]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[In addition to the interfaces defined in section
@RefSecNum{package Asis}, the library package
Asis also shall provide interfaces equivalent to the obsolescent
ones described in the following subclauses.]}

@ChgNote{SI99-0022-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[type Trait_Kinds]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[This type has been replaced by
functions Has_Abstract, Has_Aliased, Has_Limited, Has_Private, Has_Protected,
Has_Reverse, Has_Synchronized, Has_Tagged, and Has_Task. Use of the type
Trait_Kinds is not recommended in new programs.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0017-1],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[Trait_Kinds provide a means of further classifying the syntactic structure
or @i{trait} of certain A_Declaration and A_Definition elements.@Defn{Trait}
Trait_Kinds are determined only by the presence (or absence) of certain
syntactic constructs. The semantics of an element are not considered.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0017-1],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[The syntax of interest
here are the reserved words @key[abstract], @key[aliased],
@key[limited], @key[private], @key[reverse], whereever they appear, and the reserved
word @key[access] when it qualifies a definition defining an anonymous type
(an access_definition).
Trait_Kinds enumerates all combinations useful in this classification.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The subordinate Trait_Kinds allow Declaration_Kinds and
Definition_Kinds to enumerate fewer higher level elements, and be less
cluttered by all possible permutations of syntactic possibilities. For example,
in the case of a record_type_definition, Definition_Kinds can provide just two
literals that differentiate between ordinary record types and tagged record
types:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   A_Record_Type_Definition,              -- @examcom{3.8(2)    -> Trait_Kinds}
   A_Tagged_Record_Type_Definition,       -- @examcom{3.8(2)    -> Trait_Kinds}]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[The remaining classification can be accomplished, if desired, using
Trait_Kinds to determine if the definition is abstract, or limited, or both.
Without Trait_Kinds, Definition_Kinds needs six literals to identify
all the syntactic combinations for a record_type_definition.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[Elements expected by the Trait_Kind query are any Declaration_Kinds or
Definition_Kinds for which Trait_Kinds is a subordinate kind: the literal
definition has "-> Trait_Kinds" following it. For example, the
definitions of:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   A_Discriminant_Specification,              -- @examcom{3.7(5)   -> Trait_Kinds}
   A_Component_Declaration,                   -- @examcom{3.8(6)}]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[indicate A_Discriminant_Specification is an expected kind while
A_Component_Declaration is unexpected.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[All Declaration_Kinds and Definition_Kinds for which Trait_Kinds is not a
subordinate kind, and all other Element_Kinds, are unexpected and are
Not_A_Trait.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[An_Ordinary_Trait is any expected element whose syntax does not explicitly
contain any of the reserved words listed above.]}

@Chg{Version=[1],New=[],Old=[Trait_Kinds
Literals]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[@key[type] @AdaTypeDefn{Trait_Kinds} @key[is] (]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0017-1],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{Not_A_Trait},                         -- @examcom{An unexpected element}
   @AdaObjDefn{An_Ordinary_Trait},                   -- @examcom{The declaration or definition does}
                                        -- @examcom{have any of the following traits}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0017-1],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{An_Aliased_Trait},                    -- @examcom{@key[aliased] is present}
   @AdaObjDefn{An_Access_Definition_Trait},          -- @examcom{The definition defines an anonymous access type}
   @AdaObjDefn{A_Reverse_Trait},                     -- @examcom{@key[reverse] is present}
   @AdaObjDefn{A_Private_Trait},                     -- @examcom{Only @key[private] is present}
   @AdaObjDefn{A_Limited_Trait},                     -- @examcom{Only @key[limited] is present}
   @AdaObjDefn{A_Limited_Private_Trait},             -- @examcom{@key[limited] and @key[private] are present}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{An_Abstract_Trait},                   -- @examcom{Only @key[abstract] is present}
   @AdaObjDefn{An_Abstract_Private_Trait},           -- @examcom{@key[abstract] and @key[private] are present}
   @AdaObjDefn{An_Abstract_Limited_Trait},           -- @examcom{@key[abstract] and @key[limited] are present}
   @AdaObjDefn{An_Abstract_Limited_Private_Trait});  -- @examcom{@key[abstract], @key[limited], and @key[private] are}
                                        -- @examcom{present}]}
@end{Example}
@end{DescribeCode}


@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
@LabeledAddedSubclause{Version=[2],Name=[subtypes Representation_Clause and Representation_Clause_List]}
@begin{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
@ChgAdded{Version=[2],Text=[Subtypes Representation_Clause and Representation_Clause_List are
provided for compatibility with previous editions of this standard. Use of these
subtypes is not recommended in new programs.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
   @ChgAdded{Version=[2],Text=[@key[subtype] @AdaTypeDefn{Representation_Clause}           @key[is] Aspect_Clause;]}
   @ChgAdded{Version=[2],Text=[@key[subtype] @AdaTypeDefn{Representation_Clause_List}      @key[is] Aspect_Clause_List;]}
@end{Example}
@end{DescribeCode}


@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
@LabeledAddedSubclause{Version=[2],Name=[function A_Representation_Clause]}
@begin{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
@ChgAdded{Version=[2],Text=[Function A_Representation_Clause is provided for
compatibility with previous editions of this standard. Use of this function is
not recommended in new programs.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
@ChgAdded{Version=[2],Text=[@key[function] A_Representation_Clause @key[return] Clause_Kinds @key[renames] An_Aspect_Clause;]}
@end{Example}
@end{DescribeCode}


@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
@LabeledAddedSubclause{Version=[2],Name=[type Representation_Clause_Kinds]}
@begin{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
@ChgAdded{Version=[2],Text=[Subtype Representation_Clause_Kinds and function
Not_A_Representation_Clause are provided for compatibility with
previous editions of this standard. Use of these entities is not recommended in new programs.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
   @ChgAdded{Version=[2],Text=[@key[subtype] @AdaTypeDefn{Representation_Clause_Kinds} @key[is] Aspect_Clause_Kinds;]}
   @ChgAdded{Version=[2],Text=[@key[function] Not_A_Representation_Clause @key[return] Aspect_Clause_Kinds @key[renames] Not_An_Aspect_Clause;]}
@end{Example}
@end{DescribeCode}



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


@ChgNote{SI99-0022-1 add chapter on newly obsolete subprograms}
@LabeledAddedClause{Version=[2],Name=[Obsolescent Features in package Asis.Elements]}

@LabeledAddedSubclause{Version=[2],Name=[Introduction for Obsolescent Features in package Asis.Elements]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[In addition to the interfaces defined in section
@RefSecNum{package Asis.Elements}, the library package
Asis.Elements also shall provide interfaces equivalent to the obsolescent
ones described in the following subclauses.]}

@ChgNote{SI99-0022-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Trait_Kind]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[This function has been replaced by
functions Has_Abstract, Has_Aliased, Has_Limited, Has_Private, Has_Protected,
Has_Reverse, Has_Synchronized, Has_Tagged, and Has_Task. Use of the function
Trait_Kind is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Trait_Kind} (Element : @key[in] Asis.Element)
                      @key[return] Asis.Trait_Kinds;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[Element specifies the Element to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[Returns the Trait_Kinds value of the Element.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[Returns Not_A_Trait for any unexpected element such as a
Nil_Element, A_Statement, or An_Expression.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Element
expects an element that has one of the following Declaration_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[A_Private_Type_Declaration
A_Private_Extension_Declaration
A_Variable_Declaration
A_Constant_Declaration
A_Deferred_Constant_Declaration
A_Discriminant_Specification
A_Loop_Parameter_Specification
A_Procedure_Declaration
A_Function_Declaration
A_Parameter_Specification]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[or Element expects an element
that has one of the following Definition_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[A_Component_Definition
A_Private_Type_Definition
A_Tagged_Private_Type_Definition
A_Private_Extension_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[or Element expects an element
that has one of the following Type_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[A_Derived_Type_Definition
A_Derived_Record_Extension_Definition
A_Record_Type_Definition
A_Tagged_Record_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[or Element expects an element
that has one of the following Formal_Type_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[A_Formal_Private_Type_Definition
A_Formal_Tagged_Private_Type_Definition
A_Formal_Derived_Type_Definition]}
@end{Display}
@end{DescribeCode}


@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@LabeledAddedSubclause{Version=[2],Name=[function Corresponding_Pragmas]}

@begin{DescribeCode}
@begin{Example}@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Corresponding_Pragmas} (Element : @key[in] Asis.Element)
                                 @key[return] Asis.Pragma_Element_List;]}
@end{Example}

@ChgAdded{Version=[2],Text=[Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element to
query.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@ChgAdded{Version=[2],Text=[Returns the list of pragmas semantically associated with the given element,
in their order of appearance, or, in any order that does not affect their
relative interpretations. These are pragmas that directly affect the
given element. For example, a pragma Pack affects the type it names.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@ChgAdded{Version=[2],Text=[Returns a Nil_Element_List if there are no semantically associated pragmas.]}

@begin{UsageNote}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@ChgAdded{Version=[2],Text=[If the argument is a inherited entry declaration from a derived task
type, all pragmas returned are elements taken from the original task
type's declarative item list. Their Enclosing_Element is the original
type definition and not the derived type definition.]}
@end{UsageNote}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@ChgAdded{Version=[2],Text=[If Element is a declaration that includes several defining_names,
the result of this query is implementation defined.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@ChgAdded{Version=[2],Text=[@leading@keepnext@;@Chg{Version=[2],New=[Element expects an element
that has one of the following],Old=[Appropriate]} Element_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Declaration
A_Statement]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}


@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@ChgAdded{Version=[2],Text=[@leading@keepnext@;Returns @Chg{Version=[2],New=[an element that
has the following],Old=[]} Element_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Pragma]}
@end{Display}

@end{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
@LabeledAddedSubclause{Version=[2],Name=[function Representation_Clause_Kind]}
@begin{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
@ChgAdded{Version=[2],Text=[Function Representation_Clause_Kind is provided for
compatibility with previous editions of this standard. Use of this function is
not recommended in new programs.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
   @ChgAdded{Version=[2],Text=[@key[function] Representation_Clause_Kind
                  (Clause : @key[in] Asis.Aspect_Clause)
                   @key[return] Asis.Aspect_Clause_Kinds @key[renames] Aspect_Clause_Kind;]}

@end{Example}
@end{DescribeCode}




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
has the following Statement_Kinds:]}
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
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1],ARef=[SI99-0023-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Declaration_Subtype_Mark} (Declaration : @key[in] Asis.Declaration)
                                  @key[return] Asis.Name;]}
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
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1],ARef=[SI99-0023-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Result_Profile} (Declaration : @key[in] Asis.Declaration)
                     @key[return] Asis.@Chg{Version=[2],New=[Name],Old=[Expression]};]}
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


@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@LabeledAddedSubClause{Version=[2],Name=[function Corresponding_Representation_Clauses]}


@begin{DescribeCode}
@begin{Example}@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@Chg{Version=[2],New=[@key[function] @AdaSubDefn{Corresponding_Representation_Clauses}
            (Declaration : @key[in] Asis.Declaration)
            @key[return] Asis.Representation_Clause_List;],Old=[]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@Chg{Version=[2],New=[Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
declaration to query.],Old=[]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@Chg{Version=[2],New=[Returns all aspect_clause elements that apply to the
declaration, including the aspect clauses for
stream-oriented attributes whose prefix is the class-wide type
associated with the named entity.],Old=[Returns
all representation_clause elements that apply to the declaration.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@Chg{Version=[2],New=[Returns a Nil_Element_List if no clauses apply to the declaration.],Old=[]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@Chg{Version=[2],New=[If Declaration is a declaration that includes several defining_names,
the result of this query is implementation defined.],Old=[]}


@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@Chg{Version=[2],New=[The clauses returned may be the clauses applying to a parent type if the
type is a derived type with no explicit representation. These clauses
are not Is_Part_Of_Implicit, they are the representation_clause elements
specified in conjunction with the declaration of the parent type.],Old=[]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@Chg{Version=[2],New=[@Chg{Version=[2],New=[Declaration expects an element that has any],Old=[All]}
Declaration_Kinds @Chg{Version=[2],New=[except],Old=[are appropriate
except]} Not_A_Declaration.],Old=[]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@Chg{Version=[2],New=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.],Old=[]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@Chg{Version=[2],New=[@leading@keepnext@;Returns @Chg{Version=[2],New=[a list of elements that each
have the following],Old=[]} Clause_Kinds:],Old=[]}
@begin{Display}
@Chg{Version=[2],New=[A_Representation_Clause],Old=[]}
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
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1],ARef=[SI99-0023-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Access_To_Function_Result_Profile}
               (Type_Definition : @key[in] Asis.Type_Definition)
                      @key[return] Asis.@Chg{Version=[2],New=[Name],Old=[Expression]};]}
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


@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
@LabeledAddedClause{Version=[2],Name=[Obsolescent Features in package Asis.Clauses]}

@LabeledAddedSubclause{Version=[2],Name=[Introduction for Obsolescent Features in package Asis.Clauses]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[In addition to the interfaces defined in section
@RefSecNum{package Asis.Clauses}, the library package
Asis.Clauses also shall provide interfaces equivalent to the obsolescent
ones described in the following subclauses.]}


@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
@LabeledAddedSubclause{Version=[2],Name=[function Representation_Clause_Name]}
@begin{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
@ChgAdded{Version=[2],Text=[Function Representation_Clause_Name is provided for
compatibility with previous editions of this standard. Use of this function is
not recommended in new programs.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
   @ChgAdded{Version=[2],Text=[@key[function] Representation_Clause_Name (Clause : @key[in] Asis.Clause)
                                        @key[return] Asis.Name @key[renames] Aspect_Clause_Name;]}
   @end{Example}
@end{DescribeCode}


@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
@LabeledAddedSubclause{Version=[2],Name=[function Representation_Clause_Expression]}
@begin{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
@ChgAdded{Version=[2],Text=[Function Representation_Clause_Expression is
provided for compatibility with previous editions of this standard. Use of this
function is not recommended in new programs.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0039-1]}
   @ChgAdded{Version=[2],Text=[@key[function] Representation_Clause_Expression (Clause : @key[in] Asis.Aspect_Clause)
                                        @key[return] Asis.Expression @key[renames] Aspect_Clause_Expression;]}
   @end{Example}
@end{DescribeCode}



@ChgNote{SI99-0035-1 add chapter on newly obsolete functions}
@LabeledAddedClause{Version=[2],Name=[Obsolescent Features in package Asis.Data_Decomposition]}

@LabeledAddedSubclause{Version=[2],Name=[Introduction for Obsolescent Features in package Asis.Data_Decomposition]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[In addition to the interfaces defined in
@RefSecNum{package Asis.Data_Decomposition (optional)}, the library package
Asis.Data_Decomposition (if provided) also shall provide interfaces equivalent
to the obsolescent ones described in the following subclauses.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[Operations to decompose data values using the ASIS
type information and a Portable_Data stream, representing a data value of that
type, are provided by this package. These facilities are largely superseded by
the Ada streams operations.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[An application can write data, using the
Asis.Data_Decomposition.Portable_Transfer package to an external medium for
later retrieval by another application. The second application reads that data
and then uses this package to convert that data into useful information. Simple
discrete scalar types can be converted directly into useful information.
Composite types, such as records and arrays, shall first be broken into their
various discriminants and components.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[A data stream representing a record value can be
decomposed into a group of discriminant and component data streams by extracting
those streams from the record's data stream. This extraction is performed by
applying any of the Record_Components which describe the discriminants and
components of the record type. Each discriminant and each component of a record
type is described by a Record_Component value. Each value encapsulates the
information needed, by the implementation, to efficiently extract the associated
field from a data stream representing a record value of the correct type.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[A data stream representing an array value can be
decomposed into a group of component data streams by extracting those streams
from the array's data stream. This extraction is performed by applying the
single Array_Component which describes the components of the array type. One
Array_Component value is used to describe all array components. The value
encapsulates the information needed, by the implementation, to efficiently
extract any of the array components.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2], Text=[This interface assumes that the data stream is
appropriate for the ASIS host machine. For example, the implementation of this
interface will not need to worry about byte flipping or reordering of bits
caused by movement of data between machine architectures.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[All operations in this package will attempt to detect the use of invalid
data streams. A data stream is "invalid" if an operation determines that
the stream could not possibly represent a value of the expected variety.
Possible errors are: stream is of incorrect length, stream contains bit
patterns which are illegal, etc. The exception ASIS_Inappropriate_Element
is raised in these cases. The Status value is Data_Error. The
Diagnosis string will indicate the kind of error detected.]}

@ChgNote{SI99-0035-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[type Portable_Data]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[Portable_Data represents an ordered "stream" of data values.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[The portable representation for application data is an array of data
values. This portable data representation is guaranteed to be valid when
written, and later read, on the same machine architecture, using the same
implementor's compiler and runtime system. Portability of the data
values, across implementations and architectures, is not guaranteed.
Some implementors may be able to provide data values which are portable
across a larger subset of their supported machine architectures.]}

@ChgAdded{Version=[2],Text=[Some of the problems encountered when changing architectures are: bit
order, byte order, floating point representation, and alignment
constraints. Some of the problems encountered when changing runtime
systems or implementations are: type representation, optimization,
record padding, and other I/O subsystem implementation variations.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The nature of these data values is deliberately unspecified. An
implementor will choose a data value type that is suitable for the
expected uses of these arrays and data values. Arrays and data
values have these uses:]}

@begin{Enumerate}
@ChgAdded{Version=[2],Text=[Array values are used in conjunction with the
   Asis.Data_Decomposition interface. The data value type should be
   readily decomposable, by that package, so that array and record
   components can be efficiently extracted from a data stream represented
   by this array type. The efficiency of that interface is a priority.]}

@ChgAdded{Version=[2],Type=[Leading],Text=[The data value type is read and
   written by applications. It should have a size that makes efficient I/O
   possible. Applications can be expected to perform I/O in any or all of these
   ways:]}

@begin{InnerEnumerate}
@ChgAdded{Version=[2],Text=[Ada.Sequential_Io or Ada.Direct_Io could be used to read or write
      these values.]}

@ChgAdded{Version=[2],Text=[Individual values may be placed inside other types and those types
      may be read or written.]}

@ChgAdded{Version=[2],Text=[The 'Address of a data value, plus the 'Size of the data value
      type, may be used to perform low level system I/O. Note: This
      requires the 'Size of the type and the 'Size of a variable of that
      type to be the same for some implementations.]}

@ChgAdded{Version=[2],Text=[Individual values may be passed through Unchecked_Conversion in
      order to obtain a different value type, of the same 'Size, suitable
      for use with some user I/O facility. This usage is non-portable
      across implementations.]}
@end{InnerEnumerate}

@ChgAdded{Version=[2],Type=[Leading],Text=[Array values are read and written by applications. The data value
   type should have a size that makes efficient I/O possible.
   Applications can be expected to perform I/O in any or all of these ways:]}

@begin{InnerEnumerate}
@ChgAdded{Version=[2],Text=[Ada.Sequential_Io or Ada.Direct_Io could be used to read or write a
      constrained array subtype.]}

@ChgAdded{Version=[2],Text=[Array values may be placed inside other types and those types may
      be read and written.]}

@ChgAdded{Version=[2],Text=[The 'Address of the first array value, plus the 'Length of the
      array times the 'Size of the values, may be used to perform low
      level system I/O. Note: This implies that the array type is
      unpacked, or, that the packed array type has no "padding" (e.g.,
      groups of five 6-bit values packed into 32-bit words with 2 bits
      of padding every 5 elements).]}

@ChgAdded{Version=[2],Text=[Array values may be passed through Unchecked_Conversion in order to
      obtain an array value, with a different value type, suitable for
      use with some user I/O facility. This usage is non-portable across
      implementations.]}
@end{InnerEnumerate}
@end{Enumerate}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[The data value type should be chosen so that the 'Address of the first
array data value is also the 'Address of the first storage unit containing
array data. This is especially necessary for target architectures where
the "bit" instructions address bits in the opposite direction as that used
by normal machine memory (or array component) indexing. A recommended
'Size is System.Storage_Unit (or a multiple of that size).]}

@ChgAdded{Version=[2],Text=[Implementations that do not support Unchecked_Conversion of array values,
or which do not guarantee that Unchecked_Conversion of array values will
always "do the right thing" (convert only the data, and not the dope vector
information), should provide warnings in their ASIS documentation that
detail possible consequences and work-arounds.]}

@ChgAdded{Version=[2],Text=[The index range for the Portable_Data type shall be a numeric type whose
range is large enough to encompass the Portable_Data representation for all
possible runtime data values.]}

@ChgAdded{Version=[2],Text=[All conversion interfaces always return Portable_Data array values with a
'First of one (1).]}

@ChgAdded{Version=[2],Text=[The Portable_Value type may be implemented in any way
whatsoever. It need not be a numeric type.]}

@begin{DescribeCode}
@begin{Example}
@ChgAdded{Version=[2],Text=[@key[type] @AdaTypeDefn{Portable_Value} @key[is] @i{(Implementation_Defined)};]}

@ChgAdded{Version=[2],Text=[@key[subtype] @AdaSubTypeDefn{Name=[Portable_Positive],Of=[Asis.ASIS_Positive]} @key[is] Asis.ASIS_Positive
   @key[range] 1 .. @i{Implementation_Defined_Integer_Constant};]}

@ChgAdded{Version=[2],Text=[@key[type] @AdaTypeDefn{Portable_Data} @key[is array] (Portable_Positive @key[range] <>) @key[of] Portable_Value;]}

@ChgAdded{Version=[2],Text=[@AdaObjDefn{Nil_Portable_Data} : Portable_Data (1 .. 0);]}
@end{Example}
@end{DescribeCode}


@ChgNote{SI99-0035-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Record_Components (stream)]}


@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Record_Components}
            (Type_Definition : @key[in] Asis.Type_Definition;
             Data_Stream     : @key[in] Portable_Data)
            @key[return] Record_Component_List;]}

@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Record_Components}
            (Component   : @key[in] Record_Component;
             Data_Stream : @key[in] Portable_Data)
            @key[return] Record_Component_List;]}

@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Record_Components}
            (Component   : @key[in] Array_Component;
             Data_Stream : @key[in] Portable_Data)
            @key[return] Record_Component_List;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[Type_Definition specifies the record type definition to query.
Component specifies a component which has a record subtype,
                  Is_Record(Component) = True.
Data_Stream specifies a data stream containing, at least, the
                  complete set of discriminant or index constraints for the type.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[These functions are superseded by the Ada Stream
operations. The use of any of the functions Record_Components is not recommended
in new programs.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[Returns a list of the discriminants and components for the indicated record
type, using the data stream argument as a guide. The record type shall be
either a simple static, or a simple dynamic, record
type.]}

@ChgAdded{Version=[2],Text=[The result describes the locations of the record type's discriminants and
all components of the appropriate variant parts. The contents of the list
are determined by the discriminant values present in the data stream.]}

@ChgAdded{Version=[2],Text=[A simple static type will always return the same component list (Is_Equal
parts) regardless of the Data_Stream, because the layout of a simple static
type does not change with changes in discriminant values. A simple dynamic
type returns different component lists (non-Is_Equal parts) depending on
the contents of the Data_Stream, because the contents and layout of a
simple dynamic type changes with changes in discriminant values. All
return components are intended for use with a data stream representing a
value of the indicate record type.]}

@ChgAdded{Version=[2],Text=[The Data_Stream shall represent a fully discriminated value of the indicated
record type. The stream may have been read from a file, it may have been
extracted from some enclosing data stream, or it may be an artificial value
created by the Construct_Artificial_Data_Stream operation. Only the
discriminant portion of the Data_Stream is checked for validity, and, only
some discriminant fields may need to be checked, depending on the
complexity of the record type. The best approach, for any application that
is constructing artificial data streams, is to always provide appropriate
values for all discriminant fields. It is not necessary to provide values
for non-discriminant fields.]}

@ChgAdded{Version=[2],Text=[All Is_Record(Component) = True values are appropriate. All return values
are valid parameters for all query operations.]}

@begin{SingleNote}
@ChgAdded{Version=[2],Text=[If an Ada implementation uses implementation-dependent record
components (Ada Standard 13.5.1 (15)), then each such component of the
record type is included in the result.]}
@end{SingleNote}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[
Type_Definition expects an element
that has the following Element_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Type_Definition that has one of the following Type_Kinds:
   A_Derived_Type_Definition       (derived from a record type)
   A_Record_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[
Component expects a component that has one of the following
Asis.Data_Decomposition.Type_Model_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Simple_Static_Model
A_Simple_Dynamic_Model]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element or component that does not have one of these
expected kinds.]}
@end{DescribeCode}


@ChgNote{SI99-0035-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Component_Data_Stream]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Component_Data_Stream}
            (Component   : @key[in] Record_Component;
             Data_Stream : @key[in] Portable_Data)
            @key[return] Portable_Data;]}

@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Component_Data_Stream}
            (Component   : @key[in] Array_Component;
             Index       : @key[in] Asis.ASIS_Positive;
             Data_Stream : @key[in] Portable_Data)
            @key[return] Portable_Data;]}

@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Component_Data_Stream}
            (Component   : @key[in] Array_Component;
             Indexes     : @key[in] Dimension_Indexes;
             Data_Stream : @key[in] Portable_Data)
            @key[return] Portable_Data;]}

@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Component_Data_Stream}
            (Iterator    : @key[in] Array_Component_Iterator;
             Data_Stream : @key[in] Portable_Data)
            @key[return] Portable_Data;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[These functions are superseded by the Ada Stream
operations. The use of any of the functions Component_Data_Stream is not recommended
in new programs.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[Component specifies the component or discriminant to be extracted.
Index specifies an index, 1..Array_Length, within an array.
Indexes specifies a list of index values, there is one value for
              each dimension of the array type, each index N is in the
              range 1..Array_Length (Component, N);.
Iterator specifies the array component to extract.
Data_Stream specifies the data stream from which to extract the result.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[Returns a data stream representing just the value of the chosen Component.
The return value is sliced from the data stream. The Data_Stream shall
represent a value of the appropriate type. It may have been obtained from
a file, extracted from another data stream, or artificially constructed
using the Construct_Artificial_Data_Stream operation.]}

@ChgAdded{Version=[2],Text=[An artificial data stream may raise ASIS_Inappropriate_Element (the Status
is Value_Error). Only the constraint values are valid, once they
have been properly initialized, and can be safely extracted from an
artificial data stream.]}

@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element if given a Nil_Array_Component_Iterator
or one where Done(Iterator) = True. The Status value is Data_Error.
The Diagnosis string will indicate the kind of error detected.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Component expects any kind of non-Nil component.]}
@end{DescribeCode}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Keepnext=[T],Type=[Leading],Text=[An example:]}
@begin{Example}
@ChgAdded{Version=[2],Text=[@key[declare]
   Component        : Array_Component := ...;
   Iter             : Array_Component_Iterator;
   Array_Stream     : Portable_Data (...) := ...;
   Component_Stream : Portable_Data (...);
@key[begin]
   Iter := Array_Iterator (Component);
   @key[while not] Done (Iter) @key[loop]
      Component_Stream := Component_Data_Stream (Iter, Array_Stream);
      Next (Iter);
   @key[end loop];
@key[end];]}
@end{Example}


@ChgNote{SI99-0035-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Size (stream)]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Size} (Type_Definition : @key[in] Asis.Type_Definition;
               Data_Stream     : @key[in] Portable_Data)
              @key[return] Asis.ASIS_Natural;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[This function is superseded by the Ada Stream
operations. The use of this function Size is not recommended
in new programs.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[Type_Definition specifies the type definition to query.
Data_Stream specifies a data stream containing, at least, the complete
set of discriminant or index constraints for the type.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[Returns the 'Size of a value of this type, with these constraints. This is
the minimum number of bits that is needed to hold any possible value of the
given fully constrained subtype. Only the constraint portion of the
Data_Stream is checked.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[The Data_Stream may be a data stream or it may be an artificial
data stream created by the Construct_Artificial_Data_Stream operation.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Keepnext=[T],Type=[Leading],Text=[Type_Definition
expects an element
that has the following Element_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Keepnext=[T],Type=[Leading],Text=[and
Type_Definition expects an element
that has the following Asis.Data_Decomposition.Type_Model_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Simple_Static_Model
A_Simple_Dynamic_Model]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}
@end{DescribeCode}


@ChgNote{SI99-0035-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Portable_Constrained_Subtype]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[@key[generic]
    @key[type] Constrained_Subtype @key[is private];
@key[function] @AdaSubDefn{Portable_Constrained_Subtype}
            (Data_Stream : @key[in] Portable_Data)
            @key[return] Constrained_Subtype;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[This generic function is superseded by the Ada Stream
operations. The use of the generic function Portable_Constrained_Subtype
is not recommended in new programs.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[Data_Stream specifies an extracted
component of a record.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[Instantiated with an appropriate scalar type, (e.g.,
Standard.Integer, can be used to convert a data stream to a value that can be
directly examined).]}

@ChgAdded{Version=[2],Text=[Instantiated with a record type, can be used to convert a data stream to a
value that can be directly examined.]}

@ChgAdded{Version=[2],Text=[Instantiations with constrained array subtypes may not convert array values
if they were created using the Portable_Array_Type_1,
Portable_Array_Type_2, or Portable_Array_Type_3 interfaces.]}

@ChgAdded{Version=[2],Text=[May raise Constraint_Error if the subtype is a scalar and the converted
value is not in the subtype's range.]}
@end{DescribeCode}


@ChgNote{SI99-0035-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Construct_Artifical_Data_Stream]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Construct_Artificial_Data_Stream}
            (Type_Definition : @key[in] Asis.Type_Definition;
             Data_Stream     : @key[in] Portable_Data;
             Discriminant    : @key[in] Record_Component;
             Value           : @key[in] Portable_Data)
            @key[return] Portable_Data;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[This function is superseded by the Ada Stream
operations. The use of the function Construct_Artificial_Data_Stream
is not recommended in new programs.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[Type_Definition specifies the record type definition for the record
valued data stream being constructed.
Data_Stream specifies the data stream constructed so far; initially
specified as the Nil_Portable_Data value.
Discriminant specifies the discriminant of the record type that is
being set or changed.
Value specifies a data stream representing a single
discriminant value of the appropriate type.]}

@ChgAdded{Version=[2],Text=[Used to artificially construct a data stream which represents the
discriminant portion of a fully constrained value of the indicated record
type. This operation is called once with a value for each discriminant of
the record type (the order in which the discriminants are specified is not
important). The return value of each call is used as the input Data_Stream
for the next.]}

@ChgAdded{Version=[2],Text=[The resulting artificial data stream may be used solely for the purpose of
creating Record_Component values. The values of any non-discriminant
fields are arbitrary and quite possibly invalid. The resulting
component values may then be used for any purpose. In particular, they may
be used to determine First_Bit, Last_Bit, and Size values for all record
discriminants and components.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Keepnext=[T],Type=[Leading],Text=[Type_Definition expects an element
that has the following Element_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Type_Definition that has one of the following Type_Kinds:
   A_Derived_Type_Definition       (derived from a record type)
   A_Record_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element, with a Status of Data_Error, if the
discriminant Value is inappropriate for the specified Discriminant.]}
@end{DescribeCode}



@ChgNote{SI99-0035-1 add chapter on newly obsolete subprograms}
@LabeledAddedClause{Version=[2],Name=[Obsolescent package Asis.Data_Decomposition.Portable_Transfer]}

@LabeledAddedSubclause{Version=[2],Name=[Introduction for Obsolescent package Asis.Data_Decomposition.Portable_Transfer]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[The library package @ChildUnit{Parent=[Asis.Data_Decomposition],Child=[Portable_Transfer]}Asis.Data_Decomposition.Portable_Transfer
may exist. If it exists, the package
shall provide interfaces equivalent to the obsolescent ones described in the
following subclauses.]}

@ChgAdded{Version=[2],Text=[This package is part of the optional Asis.Data_Decomposition package.
It may or may not be present in all ASIS implementations.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[It provides support for marshalling and unmarshalling
of application data using ASIS.]}

@ChgAdded{Version=[2],Text=[These interfaces do not make use of the normal ASIS implementation.
This is a standalone package. It is not necessary to initialize ASIS
before using this interface.]}

@ChgAdded{Version=[2],Text=[This interface is intended to be separate from the normal ASIS
implementation. That is, linking with this interface does not cause the
full ASIS implementation to be linked with an application. This keeps the
memory cost of this facility to a minimum. However, it also means that the
normal ASIS exceptions, Status, and Diagnosis facilities are not available.
This interface may propagate exceptions other than those defined in
Asis.Errors. In particular, the I/O facilities may raise any of the
normal Ada I/O exceptions from package Ada.Io_Exceptions.]}


@ChgNote{SI99-0035-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[generic package Portable_Constrained_Subtype]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[@key[generic]
   @key[type] Constrained_Subtype @key[is private];
@key[package] @AdaPackDefn{Portable_Constrained_Subtype} @key[is]
   @key[function] @AdaSubDefn{Convert} (Value : @key[in] Constrained_Subtype)
     @key[return] Asis.Data_Decomposition.Portable_Data;
@key[end] Portable_Constrained_Subtype;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[This generic package is superseded by the Ada Stream
operations. The use of generic package Portable_Constrained_Subtype is not
recommended in new programs.]}

@ChgAdded{Version=[2],Text=[Value specifies a data value
which is to be converted to a portable data stream.]}

@ChgAdded{Version=[2],Text=[Instantiated with a scalar subtype, or a fully constrained composite
subtype, this function will correctly convert values of that subtype to a
portable data value. "Correctly" means that no information is lost. No
guarantee is made that the result will not be larger (larger 'Size) than
the argument.]}

@ChgAdded{Version=[2],Text=[Instantiation of this generic, with arguments other than scalar types or
fully constrained subtypes, will fail to compile or will fail to execute
and raise Constraint_Error.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2], Text=[The marshalling and unmarshalling of a non-null access value (either
as a top-level object or as a component of an enclosing structure) by
means of the Portable_Constrained_Subtype generics is subject to the
same restrictions as the predefined Streaming-Oriented attributes
for an access type. For most implementations, the bits representing
the access value are simply copied and it is the user's responsibility to
ensure the validity (or cope with the non-validity) of those bits
when the access value is reconstituted.]}

@end{DescribeCode}


@ChgNote{SI99-0035-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[generic package Portable_Unconstrained_Record_Type]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[@key[generic]
   @key[type] Unconstrained_Record_Type (<>) @key[is private];
@key[package] @AdaPackDefn{Portable_Unconstrained_Record_Type} @key[is]
   @key[function] @AdaSubDefn{Convert} (Value : @key[in] Unconstrained_Record_Type)
                   @key[return] Asis.Data_Decomposition.Portable_Data;
@key[end] Portable_Unconstrained_Record_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[This generic package is superseded by the Ada Stream
operations. The use of generic package Portable_Unconstrained_Subtype is not
recommended in new programs.]}

@ChgAdded{Version=[2],Text=[Value specifies a record value
which is to be converted to a portable data stream.]}

@ChgAdded{Version=[2],Text=[Instantiated with a record type, this function will correctly convert
values of that type to a portable data value. "Correctly" means that no
information is lost. No guarantee is made that the result will not be
larger (larger 'Size) than the argument.]}

@ChgAdded{Version=[2],Text=[This generic may also be instantiated with fully constrained subtypes.
However, the Portable_Constrained_Subtype generic is likely to be a more
efficient mechanism for these subtypes.]}

@ChgAdded{Version=[2],Text=[Instantiation of this generic, with unconstrained array types, will not
fail at compile time or at runtime. However, no guarantee is made that
such instantiations will operate correctly; data could be lost.]}
@end{DescribeCode}


@ChgNote{SI99-0035-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[generic packages Portable_Array_Type_n]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[@key[generic]
   @key[type] Element_Type @key[is private];
   @key[type] Index_Type @key[is] (<>);
   @key[type] Array_Type @key[is array] (Index_Type @key[range] <>) @key[of] Element_Type;
@key[package] @AdaPackDefn{Portable_Array_Type_1} @key[is]
   @key[function] @AdaSubDefn{Convert} (Value : @key[in] Array_Type)
       @key[return] Asis.Data_Decomposition.Portable_Data;
@key[end] Portable_Array_Type_1;]}
@end{Example}

@begin{Example}
@ChgAdded{Version=[2],Text=[@key[generic]
   @key[type] Element_Type @key[is private];
   @key[type] Index_Type_1 @key[is] (<>);
   @key[type] Index_Type_2 @key[is] (<>);
   @key[type] Array_Type @key[is array] (Index_Type_1 @key[range] <>,
                              Index_Type_2 @key[range] <>) @key[of] Element_Type;
@key[package] @AdaPackDefn{Portable_Array_Type_2} @key[is]
   @key[function] @AdaSubDefn{Convert} (Value : @key[in] Array_Type)
      @key[return] Asis.Data_Decomposition.Portable_Data;
@key[end] Portable_Array_Type_2;]}
@end{Example}

@begin{Example}
@ChgAdded{Version=[2],Text=[@key[generic]
   @key[type] Element_Type @key[is private];
   @key[type] Index_Type_1 @key[is] (<>);
   @key[type] Index_Type_2 @key[is] (<>);
   @key[type] Index_Type_3 @key[is] (<>);
   @key[type] Array_Type @key[is array] (Index_Type_1 @key[range] <>,
                             Index_Type_2 @key[range] <>,
                             Index_Type_3 @key[range] <>) @key[of] Element_Type;
@key[package] @AdaPackDefn{Portable_Array_Type_3} @key[is]
   @key[function] @AdaSubDefn{Convert} (Value : @key[in] Array_Type)
       @key[return] Asis.Data_Decomposition.Portable_Data;
@key[end] Portable_Array_Type_3;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2],Text=[These generic packages are superseded by the Ada Stream
operations. The use of generic packages Portable_Array_Type_1
Portable_Array_Type_2, and Portable_Array_Type_3 is not
recommended in new programs.]}

@ChgAdded{Version=[2],Text=[Value specifies an array value
which is to be converted to a portable data stream.]}

@ChgAdded{Version=[2],Text=[Instantiated with an unconstrained array subtype, this function will
correctly convert values of that subtype to a portable data value.
"Correctly" means that no information is lost. No guarantee is made that
the result will not be larger (larger 'Size) than the argument.
This interface will correctly capture the array index (dope vector)
information associated with the array value.]}

@ChgAdded{Version=[2],Text=[Use the Portable_Constrained_Subtype generic for statically constrained
array subtypes.]}

@ChgAdded{Version=[2],Text=[Array types with more than 3 dimensions can be converted to portable data
streams by placing them inside a discriminated record type (where the
discriminants are the dynamic array index constraints) and then converting
the record value. The record will then contain the necessary array index
(dope vector) information.]}
@end{DescribeCode}


