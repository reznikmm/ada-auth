@Part(glossary, Root="asis.msm")

@Comment{$Date: 2010/03/27 07:31:18 $}
@LabeledAddedNormativeAnnex{Version=[2],Name=[Obsolescent Features]}

@comment{$Source: e:\\cvsroot/ARM/ASIS/obsolescent.mss,v $}
@comment{$Revision: 1.16 $}

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

@ChgNote{SI99-0022-1 add section on newly obsolete type}
@LabeledAddedSubclause{Version=[2],Name=[type Trait_Kinds]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[This type has been replaced by
functions Has_Abstract, Has_Aliased, Has_Limited, Has_Private, Has_Protected,
Has_Reverse, Has_Synchronized, Has_Tagged, and Has_Task
(see @RefSecNum{function Has_Abstract} through @RefSecNum{function Has_Task}).
Use of the type Trait_Kinds is not recommended in new programs.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0017-1],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[Trait_Kinds provide a means of further classifying the syntactic structure
or @i{trait} of certain A_Declaration and A_Definition elements.@Defn{Trait}
Trait_Kinds are determined only by the presence (or absence) of certain
syntactic constructs. The semantics of an element are not considered.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0017-1],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[The syntax of interest
here are the reserved words @key[abstract], @key[aliased],
@key[limited], @key[private], @key[reverse], wherever they appear, and the reserved
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


@ChgNote{SI99-0058-1 add chapter on newly obsolete subprograms}
@LabeledAddedClause{Version=[2],Name=[Obsolescent Features in package Asis.Exceptions]}

@LabeledAddedSubclause{Version=[2],Name=[Introduction for Obsolescent Features in package Asis.Exceptions]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[In addition to the interfaces defined in section
@RefSecNum{package Asis.Exceptions}, the library package
Asis.Exceptions also shall provide interfaces equivalent to the
obsolescent one described in the following subclause.]}

@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[exception ASIS_Inappropriate_Container]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Containers provide no implementation-independent
functionality; use of containers is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@AdaExcDefn{ASIS_Inappropriate_Container} : @key[exception];]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Raised when ASIS is passed a Container value that is not appropriate for
the operation. This exception will typically indicate that a user error
has occurred within the application.]}
@end{DescribeCode}



@ChgNote{SI99-0058-1 add chapter on newly obsolete package}
@LabeledAddedClause{Version=[2],Name=[Obsolescent package Asis.Ada_Environments.Containers]}

@LabeledAddedSubclause{Version=[2],Name=[Introduction for Obsolescent package Asis.Ada_Environments.Containers]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[The library package Asis.Ada_Environments.Containers
shall exist. The package shall provide interfaces equivalent to the obsolescent
ones described in the following subclauses.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[If an Ada implementation supports the notion of a
program library@Defn{Program library} or "library" as suggested in Subclause
10(2) of the Ada Standard, then an ASIS Context value can be mapped onto one or
more implementor libraries represented by Containers.]}


@LabeledAddedSubclause{Version=[2],Name=[type Container]}


@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[The @i{Container}@Defn{Container} abstraction is a
logical collection of compilation units. For example, one container might hold
compilation units which include Ada predefined library units, another container
might hold implementation-defined packages. Alternatively, there might be 26
containers, each holding compilation units that begin with their respective
letter of the alphabet. The point is that no implementation-independent
semantics are associated with a container; it is simply a logical collection.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Since containers provide no
implementation-independent functionality it is recommended to avoid their
use; other ASIS queries provide equivalent functionality. Use of type Container
is not recommended in new programs.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[ASIS implementations shall minimally map the
Asis.Context to a list of one ASIS Container whose Name is that of the
Asis.Context Name.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[type] @AdaTypeDefn{Container} @key[is private];
@AdaObjDefn{Nil_Container} : @key[constant] Container;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] "=" (Left  : @key[in] Container;
              Right : @key[in] Container)
              @key[return] Boolean @key[is abstract];]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0047-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Nil_Container is the value of a Container that
represents no container.]}
@end{DescribeCode}


@LabeledAddedSubclause{Version=[2],Name=[type Container_List]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[type] @AdaTypeDefn{Container_List} @key[is]
   @key[array] (List_Index @key[range] <>) @key[of] Container;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0047-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Type Container_List represents a list of containers.]}
@end{DescribeCode}


@LabeledAddedSubclause{Version=[2],Name=[function Defining_Containers]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Containers provide no implementation-independent
functionality; use operations directly on The_Context rather than using
containers. Use of function Defining_Containers is not recommended in new
programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Defining_Containers} (The_Context : @key[in] Asis.Context)
    @key[return] Container_List;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[The_Context specifies the Context to define.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns a Container_List value that defines the
single environment Context. Each Container will have an Enclosing_Context that
Is_Identical to the argument The_Context. The order of Container values in the
list is not defined.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns a minimal list of length one if the ASIS Ada
implementation does not support the concept of a program library. In this case,
the Container will have the same name as the given Context.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Context if The_Context is not open.]}
@end{DescribeCode}


@LabeledAddedSubclause{Version=[2],Name=[function Enclosing_Context (container)]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Containers provide no implementation-independent
functionality; get the context directly from a compilation unit using
Enclosing_Context (see @RefSecNum{function Enclosing_Context (unit)}). Use of
function Enclosing_Context for a container is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Enclosing_Context} (The_Container : @key[in] Container)
    @key[return] Asis.Context;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[The_Container specifies the Container to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns the Context value associated with the Container.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns the Context for which the Container value
was originally obtained. Container values obtained through the
Defining_Containers query will always remember the Context from which they were
defined.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Because Context is limited private, this function is
only intended to be used to supply a Context parameter for other queries.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Container if the Container is Nil_Container.]}
@end{DescribeCode}


@LabeledAddedSubclause{Version=[2],Name=[function Library_Unit_Declarations (container)]}


@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Containers provide no implementation-independent
functionality; use Library_Unit_Declarations on the context (see
@RefSecNum{function Library_Unit_Declarations (context)}) instead. Use of
function Library_Unit_Declarations for a container is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Library_Unit_Declarations} (The_Container : @key[in] Container)
                                   @key[return] Asis.Compilation_Unit_List;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[The_Container specifies the Container to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns a list of all library_unit_declaration and
library_unit_renaming_declaration elements contained in The_Container.
Individual units will appear only once in an order that is not defined.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Nil_Compilation_Unit_List is returned if there are no declarations of
library units within The_Container.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0030-1],ARef=[SI99-0053-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[This query will never return a unit with
kinds A_Configuration_Compilation or A_Nonexistent_Declaration.
It will never return a unit with A_Procedure_Body or A_Function_Body unit kind
even though the unit is interpreted as both the declaration and body of a
library procedure or library function. Ada Standard 10.1.4(4).]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[All units in the result will have an
Enclosing_Container value that Is_Identical to The_Container.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Context if the Enclosing_Context(The_Container)
is not open.]}
@end{DescribeCode}


@LabeledAddedSubclause{Version=[2],Name=[function Compilation_Unit_Bodies (container)]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Containers provide no implementation-independent
functionality; use Compilation_Unit_Bodies on the context (see
@RefSecNum{function Compilation_Unit_Bodies (context)}) instead. Use of
function Compilation_Unit_Bodies for a container is not recommended in new
programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Compilation_Unit_Bodies} (The_Container : @key[in] Container)
                                 @key[return] Asis.Compilation_Unit_List;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[The_Container specifies the Container to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns a list of all library_unit_body and subunit
elements contained in The_Container. Individual units will appear only once in
an order that is not defined.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Nil_Compilation_Unit_List is returned if there are
no bodies within The_Container.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0053-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[This query will never return a unit with kinds
A_Configuration_Compilation or A_Nonexistent_Body.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[All units in the result will have an
Enclosing_Container value that Is_Identical to The_Container.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Context if the
Enclosing_Context(The_Container) is not open.]}
@end{DescribeCode}


@LabeledAddedSubclause{Version=[2],Name=[function Compilation_Units (container)]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Containers provide no implementation-independent
functionality; use Compilation_Units on the context (see
@RefSecNum{function Compilation_Units (context)}) instead. Use of
function Compilation_Units for a container is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Compilation_Units} (The_Container : @key[in] Container)
                           @key[return] Asis.Compilation_Unit_List;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[The_Container specifies the Container to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns a list of all compilation units contained in
The_Container. Individual units will appear only once in an order that is not
defined.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Nil_Compilation_Unit_List is returned if there are
no units within the Container.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0053-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[This query will never return a unit with kinds
A_Configuration_Compilation, A_Nonexistent_Declaration,
or A_Nonexistent_Body.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[All units in the result will have an
Enclosing_Container value that Is_Identical to The_Container.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Context if the Enclosing_Context(The_Container)
is not open.]}
@end{DescribeCode}


@LabeledAddedSubclause{Version=[2],Name=[function Is_Equal (containers)]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Containers provide no implementation-independent
functionality; use Is_Equal on the context (see
@RefSecNum{function Is_Equal (context)}) instead. Use of
function Is_Equal for a container is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Equal} (Left  : @key[in] Container;
                   Right : @key[in] Container) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Left specifies first Container.
Right specifies the second Container.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0047-1],ARef=[SI99-0053-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns True if Left and Right designate Container
values that contain the same set of compilation units,
where two compilation units are the same when Is_Equal on the corresponding
Compilation_Units returns True, and returns False otherwise. The
Container values may have been defined from different Context values.]}
@end{DescribeCode}


@LabeledAddedSubclause{Version=[2],Name=[function Is_Identical (containers)]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Containers provide no implementation-independent
functionality; use Is_Identical on the context (see
@RefSecNum{function Is_Identical (context)}) instead. Use of
function Is_Identical for a container is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Identical} (Left  : @key[in] Container;
                       Right : @key[in] Container) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Left specifies the first Container.
Right specifies the second Container.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0047-1],ARef=[SI99-0053-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns True if Is_Equal(Left, Right) and the
Container values have been defined from Is_Identical
Context values, and returns False otherwise.]}
@end{DescribeCode}


@LabeledAddedSubclause{Version=[2],Name=[function Name (container)]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Containers provide no implementation-independent
functionality; use Name on the context (see
@RefSecNum{function Name (context)}) instead. Use of function Name for a
container is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Name} (The_Container : @key[in] Container) @key[return] Wide_String;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[The_Container specifies the Container to name.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns the Name value associated with the
Container.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns a null string if the Container is
Nil_Container.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add chapter on newly obsolete subprograms}
@LabeledAddedClause{Version=[2],Name=[Obsolescent Features in package Asis.Compilation_Units]}

@LabeledAddedSubclause{Version=[2],Name=[Introduction for Obsolescent Features in package Asis.Compilation_Units]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[In addition to the interfaces defined in section
@RefSecNum{package Asis.Compilation_Units}, the library package
Asis.Compilation_Units also shall provide interfaces equivalent to the
obsolescent one described in the following subclause.]}

@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Enclosing_Container]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Containers provide no implementation-independent
functionality; get the context directly using Enclosing_Context (see
@RefSecNum{function Enclosing_Context (unit)}). Use
of function Enclosing_Container is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Enclosing_Container} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
           @key[return] Asis.Ada_Environments.Containers.Container;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Compilation_Unit specifies the unit whose Container is required.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns the Container of the Context containing the
compilation unit. Compilation units always remember the ASIS Context and
Container from which they were obtained.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0028-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Compilation_Unit with a
Status of Value_Error if the unit is Nil_Compilation_Unit.]}
@end{DescribeCode}



@ChgNote{SI99-0022-1 add chapter on newly obsolete subprograms}
@LabeledAddedClause{Version=[2],Name=[Obsolescent Features in package Asis.Elements]}

@LabeledAddedSubclause{Version=[2],Name=[Introduction for Obsolescent Features in package Asis.Elements]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[In addition to the interfaces defined in section
@RefSecNum{package Asis.Elements}, the library package
Asis.Elements also shall provide interfaces equivalent to the obsolescent
ones described in the following subclauses.]}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Configuration_Pragmas]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[This function has been replaced by
Inherited_Configuration_Pragmas (see @RefSecNum{function Inherited_Configuration_Pragmas}).
Use of the function
Configuration_Pragmas is not recommended in new programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Configuration_Pragmas} (The_Context : @key[in] Asis.Context)
                             @key[return] Asis.Pragma_Element_List;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[The_Context specifies the Context to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns a list of pragmas that apply to all future
compilation_unit elements compiled into The_Context. Pragmas returned by this
query should have appeared in a compilation that had no compilation_unit
elements. To the extent that order is meaningful, the pragmas should be in their
order of appearance in the compilation. (The order is implementation dependent,
many pragmas have the same effect regardless of order.)]}

@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[This query is obsolescent as most Ada
implementations have multiple ways to provide configuration pragmas; they often
depend on the location or project where a unit is compiled. This query does not
provide that information, so no useful answer can be provided on those
implementations.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns Nil_Element_List if there are no such
configuration pragmas.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Returns a list of
elements that each have the following Element_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Pragma]}
@end{Display}
@end{DescribeCode}



@ChgNote{SI99-0022-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Trait_Kind]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2],Text=[This function has been replaced by
functions Has_Abstract, Has_Aliased, Has_Limited, Has_Private, Has_Protected,
Has_Reverse, Has_Synchronized, Has_Tagged, and Has_Task
(see @RefSecNum{function Has_Abstract} through @RefSecNum{function Has_Task}).
Use of the function
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
@ChgAdded{Version=[2],Text=[Returns Not_A_Trait for any unexpected element such as
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


@LabeledAddedSubclause{Version=[2],Name=[function Corresponding_Pragmas]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@ChgAdded{Version=[2],Text=[This function has been replaced by
Corresponding_Aspect_Pragmas (see @RefSecNum{function Corresponding_Aspect_Pragmas}).
Use of the function Corresponding_Pragmas is not recommended in new programs.]}

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
@ChgAdded{Version=[2],Text=[Returns Nil_Element_List if there are no semantically associated pragmas.]}

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

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0021-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Element expects an element
that has one of the following Element_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Declaration
A_Statement]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}


@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Returns an element that
has the following Element_Kinds:]}
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


@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0057-1]}
@LabeledAddedSubclause{Version=[2],Name=[function Enclosing_Element (with expected parameter)]}
@begin{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0057-1]}
@ChgAdded{Version=[2],Text=[The behavior of this function is not portable; use
the one parameter version of Enclosing_Element (see
@RefSecNum{function Enclosing_Element}) instead. Use of the two parameter
version of function Enclosing_Element is not recommended in new programs.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0057-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Enclosing_Element} (Element                    : @key[in] Asis.Element;
                            Expected_Enclosing_Element : @key[in] Asis.Element)
                        @key[return] Asis.Element;]}

@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0057-1]}
@ChgAdded{Version=[2],Text=[Element specifies the element to query.
Expected_Enclosing_Element specifies an enclosing element expected to contain
the element; the interpretation of this parameter is implementation-defined.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0057-1]}
@ChgAdded{Version=[2],Text=[Returns the Element that immediately encloses the
given element. This query is intended to exactly reverse any single
parent-to-child element traversal. For any structural query that returns a
subcomponent of an element (or that returns a list of subcomponent elements),
the original element can be determined by passing the subcomponent element to
this query.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0057-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Returns Nil_Element if:]}
@begin{Itemize}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[the element is the declaration part of a
  compilation unit (Unit_Declaration).]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[the element is with clause or use clause of a
  context clause (Context_Clause_Elements).]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[the element is a pragma for a compilation unit
  (Compilation_Pragmas and Context_Clause_Elements).]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0057-1]}
@ChgAdded{Version=[2],Text=[Use Enclosing_Compilation_Unit to get the enclosing
compilation unit for any element value other than Nil_Element.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0057-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element if the Element is
Nil_Element.]}

@end{DescribeCode}
@begin{UsageNote}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0057-1]}
@ChgAdded{Version=[2],Text=[The original intent of the second parameter was
described as follows (we do not try to normatively describe this intent):]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The Expected_Enclosing_Element parameter is used
only to optimize this query. This speed up is only present for ASIS
implementations where the underlying implementor's environment does not have
"parent pointers". For these implementations, this query is implemented as a
"search". The Enclosing_Compilation_Unit is searched for the argument Element.
The Expected_Enclosing_Element parameter provides a means of shortening the
search. Note: If the argument Element is not a sub-element of the
Expected_Enclosing_Element parameter, or if the Expected_Enclosing_Element is
Nil_Element, the result of the call is Nil_Element.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Implementations that do not require the
Expected_Enclosing_Element parameter may ignore it. They are encouraged, but not
required, to test the Expected_Enclosing_Element parameter and to determine if
it is an invalid Element value (its associated Environment Context may be
closed).]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Portable applications should not use the
Expected_Enclosing_Element parameter since it can lead to unexpected differences
when porting an application between ASIS implementations where one
implementation uses the parameter and the other implementation does not. Passing
a "wrong" Expected_Enclosing_Element to an implementation that ignores it, is
harmless. Passing a "wrong" Expected_Enclosing_Element to an implementation that
may utilize it, can lead to an unexpected Nil_Element result.]}
@end{UsageNote}


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
Body_Declarative_Items, Body_Statements, and Body_Exception_Handlers
(see @RefSecNum{function Body_Declarative_Items},
@RefSecNum{function Body_Statements}, and
@RefSecNum{function Body_Exception_Handlers}).
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

@begin{UsageNote}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Text=[This function is intended for compatibility with
ASIS 83.]}
@end{UsageNote}


@ChgNote{SI99-0004-1 add section on newly obsolete subprogram}
@LabeledAddedSubClause{Version=[2],Name=[function Object_Declaration_View]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[This function has been replaced by
Object_Declaration_Subtype (see @RefSecNum{function Object_Declaration_Subtype}).
Use of the function Object_Declaration_View is not
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
@ChgAdded{Version=[2],Text=[Returns Nil_Element for a single_task_declaration
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
A_Subtype_Indication
A_Task_Definition
A_Protected_Definition
A_Component_Definition
A_Type_Definition @em the returned element also has the following Type_Kinds:
 @ @ @ @  A_Constrained_Array_Definition]}
@end{Display}
@end{DescribeCode}


@ChgNote{SI99-0004-1 add section on newly obsolete subprogram}
@LabeledAddedSubClause{Version=[2],Name=[function Declaration_Subtype_Mark]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[This function has been replaced by
Object_Declaration_Subtype (see @RefSecNum{function Object_Declaration_Subtype}).
Use of the function Declaration_Subtype_Mark is not recommended in new programs.]}

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
function Result_Subtype (see @RefSecNum{function Result_Subtype}).
Use of the function Result_Profile is not recommended in new programs.]}

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
that has one of the following Expression_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[An_Identifier
A_Selected_Component
An_Attribute_Reference]}
@end{Display}
@end{DescribeCode}

@ChgRef{Version=[2],Kind=[Added]}
@LabeledAddedSubClause{Version=[2],Name=[function Corresponding_Representation_Clauses]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@ChgAdded{Version=[2],Text=[This function has been replaced by
Corresponding_Aspect_Clauses (see @RefSecNum{function Corresponding_Aspect_Clauses}).
Use of the function
Corresponding_Representation_Clauses is not recommended in new programs.]}


@begin{DescribeCode}
@begin{Example}@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@Chg{Version=[2],New=[@key[function] @AdaSubDefn{Corresponding_Representation_Clauses}
            (Declaration : @key[in] Asis.Declaration)
            @key[return] Asis.Representation_Clause_List;],Old=[]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@Chg{Version=[2],New=[Declaration specifies the
declaration to query.],Old=[]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@ChgAdded{Version=[2],Text=[Returns all aspect_clause elements that apply to the
declaration, including the aspect clauses for
stream-oriented attributes whose prefix is the class-wide type
associated with the named entity.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@Chg{Version=[2],New=[Returns Nil_Element_List if no clauses apply to the declaration.],Old=[]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@Chg{Version=[2],New=[If Declaration is a declaration that includes several defining_names,
the result of this query is implementation defined.],Old=[]}


@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@Chg{Version=[2],New=[The clauses returned may be the clauses applying to a parent type if the
type is a derived type with no explicit representation. These clauses
are not Is_Part_Of_Implicit, they are the aspect_clause elements
specified in conjunction with the declaration of the parent type.],Old=[]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Declaration expects an element that has any
Declaration_Kinds except Not_A_Declaration.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.],Old=[]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Returns a list of elements that each
have the following Clause_Kinds:]}
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
Access_To_Function_Result_Subtype (see @RefSecNum{function Access_To_Function_Result_Subtype}).
Use of the function Access_To_Function_Result_Profile is not recommended in new
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
an element of one of the following Type_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[An_Access_Type_Definition
A_Formal_Access_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[that also has one of
the following Access_Type_Kinds:]}
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


@ChgNote{SI99-0044-1 add clause on newly obsolete function}
@LabeledAddedSubClause{Version=[2],Name=[function Corresponding_Parent_Subtype]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0044-1]}
@ChgAdded{Version=[2],Text=[This function has been replaced by
Parent_Subtype_Indication (see @RefSecNum{function Parent_Subtype_Indication}).
Use of the function Corresponding_Parent_Subtype is not recommended in new
programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0044-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Corresponding_Parent_Subtype}
             (Type_Definition : @key[in] Asis.Type_Definition)
                 @key[return] Asis.Declaration;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0044-1]}
@ChgAdded{Version=[2],Text=[Type_Definition specifies the
derived_type_definition to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0044-1]}
@ChgAdded{Version=[2],Text=[Returns the parent subtype declaration of the
derived_type_definition. The parent subtype is defined by the
parent_subtype_indication. If the parent subtype have no explicit
declaration, the value returns is implementation defined.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0044-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Type_Definition expects an element
that has one of the following Type_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Derived_Type_Definition
A_Derived_Record_Extension_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0044-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0044-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Returns an element
that has one of the following Declaration_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[An_Ordinary_Type_Declaration
A_Task_Type_Declaration
A_Protected_Type_Declaration
A_Subtype_Declaration
A_Formal_Type_Declaration
An_Incomplete_Type_Declaration
A_Private_Type_Declaration
A_Private_Extension_Declaration]}
@end{Display}
@end{DescribeCode}


@ChgNote{SI99-0056-1 add clause on newly obsolete function}
@LabeledAddedSubClause{Version=[2],Name=[function Component_Subtype_Indication]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0056-1]}
@ChgAdded{Version=[2],Text=[This function has been replaced by
Component_Definition_Subtype (see @RefSecNum{function Component_Definition_Subtype}).
Use of the function Component_Subtype_Indication is not recommended in new
programs.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0056-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Component_Subtype_Indication}
        (Component_Definition : @key[in] Asis.Component_Definition)
            @key[return] Asis.Subtype_Indication;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0056-1]}
@ChgAdded{Version=[2],Text=[Component_Definition specifies the
Component_Definition to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0056-1]}
@ChgAdded{Version=[2],Text=[Returns the subtype_indication of the
Component_Definition.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0056-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Component_Definition
expects an element that has the following Definition_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Component_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0056-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have this expected kind.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0056-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Returns an element
that has the following Definition_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Subtype_Indication]}
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



@ChgNote{SI99-0058-1 add chapter on newly obsolete package}
@LabeledAddedClause{Version=[2],Name=[Obsolescent package Asis.Data_Decomposition]}

@LabeledAddedSubclause{Version=[2],Name=[Introduction for Obsolescent package Asis.Data_Decomposition]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0037-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Support for data decomposition is optional. The
library package Asis.Data_Decomposition shall exist for an implementation that
supports data decomposition. If it exists, the package shall provide interfaces
equivalent to the obsolescent ones described in the following subclauses.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Operations to decompose data values using the ASIS
type information and a Portable_Data stream, representing a data value of that
type, are provided by this package. These facilities are largely superseded by
the Ada streams operations.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[An application can write data, using the
Asis.Data_Decomposition.Portable_Transfer package to an external medium for
later retrieval by another application. The second application reads that data
and then uses this package to convert that data into useful information. Simple
discrete scalar types can be converted directly into useful information.
Composite types, such as records and arrays, shall first be broken into their
various discriminants and components.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[A data stream representing a record value can be
decomposed into a group of discriminant and component data streams by extracting
those streams from the record's data stream. This extraction is performed by
applying any of the Record_Components which describe the discriminants and
components of the record type. Each discriminant and each component of a record
type is described by a Record_Component value. Each value encapsulates the
information needed, by the implementation, to efficiently extract the associated
field from a data stream representing a record value of the correct type.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[A data stream representing an array value can be
decomposed into a group of component data streams by extracting those streams
from the array's data stream. This extraction is performed by applying the
single Array_Component which describes the components of the array type. One
Array_Component value is used to describe all array components. The value
encapsulates the information needed, by the implementation, to efficiently
extract any of the array components.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[Assumptions and Limitations of this Interface:]}
@begin{Enumerate}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2], Text=[The data stream is appropriate for the ASIS host machine. For example,
the implementation of this interface will not need to worry about
byte flipping or reordering of bits caused by movement of data between
machine architectures.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Records, arrays, and their components may be packed.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0037-1],ARef=[SI99-0039-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Records, array components, enumerations, and scalar
types may have aspect clauses applied to them. This includes scalar types used
as record discriminants and array indices.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[This specification supports two of the three type
models discussed below. Models 1 and 2 are supported. Model 3 is not
supported.]}

@begin{InnerEnumerate}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[Simple "static" types contain no variants, have a single fixed 'Size,
      and all components and attributes are themselves static and/or fully
      constrained. The size and position for any component of the type can be
      determined without regard to constraints. For example:]}

@begin{ChildExample}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[type] Static_Record @key[is]
    @key[record]
        F1, F2 : Natural;
        C1     : Wide_Character;
        A1     : Wide_String (1..5);
    @key[end record];]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[type] Static_Discriminated (X : Boolean) @key[is]
    @key[record]
        F1, F2 : Natural;
        C1     : Wide_Character;
    @key[end record];]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[type] Static_Array   @key[is array] (Integer @key[range] 1 .. 100) @key[of] Boolean;
@key[type] Static_Enum    @key[is] (One, Two, Three);
@key[type] Static_Integer @key[is range] 1 .. 512;
@key[type] Static_Float   @key[is digits] 15 @key[range] -100.0 .. 100.0;
@key[type] Static_Fixed   @key[is delta] 0.1 @key[range] -100.0 .. 100.0;]}
@end{ChildExample}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Comment{Start item 2}@ChgAdded{Version=[2],Text=[Simple "dynamic" types contain one or more components or
attributes whose size, position, or value depends on the value of one or more
constraints computed at execution time. This means that the size, position, or
number of components within the data type cannot be determined without
reference to constraint values.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],NoPrefix=[T],Text=[Records containing components, whose size depends on discriminants
      of the record, can be handled because the discriminants for a record
      value are fully specified by the data stream form of the record value.
      For example:]}

@begin{ChildExample}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[type] Dynamic_Length (Length : Natural) @key[is]
    @key[record]
        S1 : Wide_String (1 .. Length);
    @key[end record];]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[type] Dynamic_Variant (When : Boolean) @key[is]
    @key[record]
        @key[case] When @key[is]
            @key[when] True =>
                C1 : Wide_Character;
            @key[when] False =>
                @key[null];
        @key[end case];
    @key[end record];]}
@end{ChildExample}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],NoPrefix=[T],Text=[Arrays with an
unconstrained subtype, whose 'Length, 'First, and 'Last depend on dynamic index
constraints, can be handled because these attributes can be queried and stored
when the data stream is written. For example:]}

@begin{ChildExample}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[I : Integer := Some_Function;
@key[type] Dynamic_Array @key[is]
    @key[array] (Integer @key[range] I .. I + 10) @key[of] Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[type] Heap_Array   @key[is array] (Integer @key[range] <>) @key[of] Boolean;
@key[type] Access_Array @key[is access] Heap_Array;
X : Access_Array := @key[new] Heap_Array (1 .. 100);]}
@end{ChildExample}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Comment{Start item 3}@ChgAdded{Version=[2],Type=[Leading],Text=[Complex,
externally "discriminated" records,
contain one or more components whose size or position depends on the value of
one or more non-static external values (values not stored within instances of
the type) at execution time. The size for a value of the type cannot be
determined without reference to these external values, whose runtime values are
not known to the ASIS Context and cannot be automatically recorded by the
Asis.Data_Decomposition.Portable_Transfer generics. A class-wide type also
falls in this category, as does an array type with a dynamic component size.
For example:]}

@begin{ChildExample}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[N : Natural := Function_Call();
....
@key[declare]
    @key[type] Complex @key[is]
        @key[record]
            S1 : Wide_String (1 .. N);
        @key[end record];
@key[begin]
    ....
@key[end];]}
@end{ChildExample}
@end{InnerEnumerate}
@end{Enumerate}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[General Usage Rules:]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[All operations in this package will attempt to detect the use of invalid
data streams. A data stream is "invalid" if an operation determines that
the stream could not possibly represent a value of the expected variety.
Possible errors are: stream is of incorrect length, stream contains bit
patterns which are illegal, etc. The exception ASIS_Inappropriate_Element
is raised in these cases. The Status value is Data_Error. The
Diagnosis string will indicate the kind of error detected.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[All implementations will handle arrays with a
minimum of 16 dimensions, or the number of dimensions allowed by their compiler,
whichever is smaller.]}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[type Record_Component]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Type Record_Component describes one discriminant or
component of a record type.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0047-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[The "=" operator is not
meaningful between Record_Component values unless one of them is the
Nil_Record_Component value.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[A record type describes composite values which
contain zero or more discriminant and component fields. A_Record_Type_Definition
can be queried to obtain a list of Record_Components. Each Record_Component
contains the information necessary to extract one discriminant or component
field of the record.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Record_Components are intended for use with data
stream extraction operations. An extraction operation is performed using a
Record_Component, in conjunction with a data stream representing a value of the
record type. The record data stream contains data for all fields of the record.
The result is an extracted data stream representing just the value of the one
field. Record_Components are implemented so as to allow for efficient extraction
of field values.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[An extracted field data stream is suitable for all
uses. If the field is a scalar type, it can be converted directly into useful
information. If the field is, in turn, another composite value, it can be
further decomposed into its own component values.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[There are two ways to obtain the Record_Components
or the Array_Component needed to further decompose an embedded composite field.
First, if the type of the field is known, the type definition can be directly
queried to obtain the Record_Components or the Array_Component that describe its
internal structure. Second, the Record_Component used to extract the field can
be queried to obtain the same Record_Components or the same Array_Component.
Both methods return identical information.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[This kind of nested decomposition can be carried to
any required level.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Record_Components become invalid when the Context,
from which they originate, is closed. All Record_Components are obtained by
referencing a) an Element, which has an associated Context, b) another
Record_Component, or c) an Array_Component. Ultimately, all component values
originate from a A_Type_Definition Element; that Element determines their
Context of origin.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[type] @AdaTypeDefn{Record_Component} @key[is private];
@AdaObjDefn{Nil_Record_Component} : @key[constant] Record_Component;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] "=" (Left  : @key[in] Record_Component;
              Right : @key[in] Record_Component)
              @key[return] Boolean @key[is abstract];]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Nil_Record_Component is the value of a
Record_Component that represents no component.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[type Record_Component_List]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[type] @AdaTypeDefn{Record_Component_List} @key[is]
   @key[array] (Asis.List_Index @key[range] <>) @key[of] Record_Component;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Type Record_Component_List represents a list of
record components.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[type Array_Component]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Type Array_Component describes the components of an array valued field for a
record type.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0047-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[The "=" operator is not
meaningful between Array_Component values unless one of them is the
Nil_Array_Component value.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[An array type describes composite values which
contain zero or more indexed components. Both An_Unconstrained_Array_Definition
or A_Constrained_Array_Definition can be queried to obtain a single
Array_Component. The Array_Component contains the information necessary to
extract any arbitrary component of the array.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Array_Components are intended for use with data stream extraction
operations. An extraction operation is performed using an Array_Component,
in conjunction with a data stream representing a value of the array type.
The array data stream contains data for all components of the array. The
result is an extracted data stream representing just the value of the one
component. Array_Components are implemented so as to allow for efficient
extraction of array component values.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[An extracted component data stream is suitable for
all uses. If the component is a scalar type, it can be converted directly into
useful information. If the component is, in turn, another composite value, it
can be further decomposed into its own component values.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[There are two ways to obtain the Record_Components
or the Array_Component needed to further decompose an embedded composite
component. First, if the type of the component is known, the type definition can
be directly queried to obtain the Record_Components or the Array_Component that
describe its internal structure. Second, the Array_Component used to extract the
component can be queried to obtain the same Record_Components or the same
Array_Component. Both methods return identical information.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[This kind of nested decomposition can be carried to any required level.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Array_Components become invalid when the Context, from which they
originate, is closed. All Record_Components are obtained by referencing a)
an Element, which has an associated Context, b) a Record_Component, or c)
another Array_Component. Ultimately, all component values originate from a
A_Type_Definition Element; that Element determines their Context of origin.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[type] @AdaTypeDefn{Array_Component} @key[is private];
@AdaObjDefn{Nil_Array_Component} : @key[constant] Array_Component;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] "=" (Left  : @key[in] Array_Component;
              Right : @key[in] Array_Component)
              @key[return] Boolean @key[is abstract];]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Nil_Array_Component is the value of an
Array_Component that represents no component.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[type Array_Component_List]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[type] @AdaTypeDefn{Array_Component_List} @key[is]
   @key[array] (Asis.List_Index @key[range] <>) @key[of] Array_Component;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Type Array_Component_List represents a list of
array components.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[type Dimension_Indexes]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Type Dimension_Indexes
is an array of index values used to access an array stream.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[type] @AdaTypeDefn{Dimension_Indexes} @key[is]
   @key[array] (Asis.ASIS_Positive @key[range] <>) @key[of] Asis.ASIS_Positive;]}
@end{Example}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[type Array_Component_Iterator]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0047-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Type Array_Component_Iterator is used to iterate
over successive components of an array. This can be more efficient than using
individual index values when extracting array components from a data stream
because it substitutes two subroutine calls (Next and Done) for the
multiplications and divisions implicit in indexing an N dimensional array with a
single index value.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Iterators can be copied. The copies operate
independently (have separate state).]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Keepnext=[T],Type=[Leading],Text=[An example:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
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


@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[type] @AdaTypeDefn{Array_Component_Iterator} @key[is] private;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@AdaObjDefn{Nil_Array_Component_Iterator} : @key[constant] Array_Component_Iterator;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Nil_Array_Component_Iterator is the value of an
Array_Component_Iterator that represents no iterator.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete type}
@LabeledAddedSubclause{Version=[2],Name=[type Portable_Data]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Portable_Data represents an ordered "stream" of data values.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
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

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
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

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
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



@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[type Type_Model_Kinds]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Each Type_Definition fits into one of three type models.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[type] @AdaTypeDefn{Type_Model_Kinds} @key[is] (A_Simple_Static_Model,
                          A_Simple_Dynamic_Model,
                          A_Complex_Dynamic_Model,
                          Not_A_Type_Model);           -- @examcom{Nil arguments}]}
@end{Example}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Type_Model_Kind]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Type_Model_Kind} (Type_Definition : @key[in] Asis.Type_Definition)
                         @key[return] Type_Model_Kinds;]}

@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Type_Model_Kind} (Component : @key[in] Record_Component)
                         @key[return] Type_Model_Kinds;]}

@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Type_Model_Kind} (Component : @key[in] Array_Component)
                         @key[return] Type_Model_Kinds;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Type_Definition specifies the type definition to query.
Component specifies a record field with a record or array type.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns the model that best describes the type
indicated by the argument. Returns Not_A_Type_Model for any unexpected argument
such as a Nil value.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0028-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Type_Definition expects
an element that has the following Element_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Is_Nil (component)]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Nil} (Right : @key[in] Record_Component) @key[return] Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Nil} (Right : @key[in] Array_Component)  @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Right specifies the component to check.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Returns True if Right is a Nil (or uninitialized) component value.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Returns False for all other values.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Right expects any kind of component.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Is_Equal (component)]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Equal} (Left  : @key[in] Record_Component;
                   Right : @key[in] Record_Component) @key[return] Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Equal} (Left  : @key[in] Array_Component;
                   Right : @key[in] Array_Component)  @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Left specifies the left component to compare.
Right specifies the right component to compare.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0047-1],ARef=[SI99-0055-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns True if Left and Right represent the same physical component of the
same record or array type from the same physical compilation unit, and
returns False otherwise. The two components may or may not be from the same open
ASIS Context.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Implies:]}
@begin{ChildExample}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Is_Equal (Enclosing_Compilation_Unit (Component_Declaration (Left)),
          Enclosing_Compilation_Unit (Component_Declaration (Right))) = True]}
@end{ChildExample}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Right and Left expect any kind of component.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Is_Identical (component)]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Identical} (Left  : @key[in] Record_Component;
                       Right : @key[in] Record_Component) @key[return] Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Identical} (Left  : @key[in] Array_Component;
                       Right : @key[in] Array_Component)  @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Left specifies the left component to compare.
Right specifies the right component to compare.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0047-1],ARef=[SI99-0055-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns True if Left and Right represent the same
physical component of the same record or array type from the same physical
compilation unit and the same open ASIS Context, and returns False otherwise.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Implies:]}
@begin{ChildExample}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Is_Identical (Enclosing_Compilation_Unit (Component_Declaration (Left)),
              Enclosing_Compilation_Unit (Component_Declaration (Right))) = True]}
@end{ChildExample}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Right and Left expect any kind of component.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Is_Array]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Array} (Component : @key[in] Record_Component) @key[return] Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Array} (Component : @key[in] Array_Component)  @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Component specifies any component.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Returns True if the component has an array subtype
(contains an array value).]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Returns False for Nil components and any component
that is not an embedded array.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Is_Record]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Record} (Component : @key[in] Record_Component) @key[return] Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Record} (Component : @key[in] Array_Component)  @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Component specifies any component.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0047-1],ARef=[SI99-0055-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns True if the component has a record,
task, or protected subtype.
Returns True for a task or protected component because such a
component may have discriminants. Returns False otherwise.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Done]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Done} (Iterator : @key[in] Array_Component_Iterator) @key[return] Boolean;]}
@end{Example}

**** Redo TBD ****

Iterator @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the iterator
to query.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
Returns True if the iterator has been advanced past the last array
component. Returns True for Nil_Array_Component_Iterator.@Chg{Version=[2],New=[
Returns False otherwise.],Old=[]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[procedure Next]}

@begin{DescribeCode}
@begin{Example}
@key[procedure] @AdaSubDefn{Next} (Iterator : @key[in out] Array_Component_Iterator);
@end{Example}

Iterator @Chg{Version=[1],New=[specifies],Old=[    @en Specifies]} the iterator to advance.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
Advances the iterator to the next array component. Use Done to test the
iterator to see if it has passed the last component. Does nothing if the
iterator is @Chg{Version=[2],New=[Nil_Array_Component_Iterator
or is ],Old=[]}already past the last component.
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[procedure Reset]}

@begin{DescribeCode}
@begin{Example}
@key[procedure] @AdaSubDefn{Reset} (Iterator : @key[in out] Array_Component_Iterator);
@end{Example}

Iterator @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the iterator to reset.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
Resets the iterator to the first array component.@Chg{Version=[2],New=[
Has no effect if Iterator is Nil_Array_Component_Iterator.],Old=[]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Array_Index]}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Array_Index} (Iterator : @key[in] Array_Component_Iterator)
                     @key[return] Asis.ASIS_Natural;
@end{Example}

Iterator @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the iterator to query.

Returns the Index value which, when used in conjunction with the
Array_Component value used to create the Iterator, indexes the same array
component as that presently addressed by the Iterator.

Raises ASIS_Inappropriate_Element if given a Nil_Array_Component_Iterator
or one where Done(Iterator) = True. The Status value is Data_Error.
The Diagnosis string will indicate the kind of error detected.
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Array_Indexes]}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Array_Indexes} (Iterator : @key[in] Array_Component_Iterator)
                        @key[return] Dimension_Indexes;
@end{Example}

Iterator @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the iterator to query.

Returns the index values which, when used in conjunction with the
Array_Component value used to create the Iterator, indexes the same array
component as that presently addressed by the Iterator.

Raises ASIS_Inappropriate_Element if given a Nil_Array_Component_Iterator
or one where Done(Iterator) = True. The Status value is Data_Error.
The Diagnosis string will indicate the kind of error detected.
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Discriminant_Components]}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Discriminant_Components} (Type_Definition : @key[in] Asis.Type_Definition)
                                 @key[return] Record_Component_List;

@key[function] @AdaSubDefn{Discriminant_Components} (Component : @key[in] Record_Component)
                                 @key[return] Record_Component_List;

@key[function] @AdaSubDefn{Discriminant_Components} (Component : @key[in] Array_Component)
                                 @key[return] Record_Component_List;
@end{Example}

Type_Definition @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the record type definition to query.
Component @Chg{Version=[1],New=[specifies],Old=[      @en Specifies]} a component which has a record subtype,
                  Is_Record(Component) = True.

Returns a list of the discriminant components for records of the indicated
record type.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
The result describes the locations of the record type's discriminants,
regardless of the static or dynamic nature of the record
type.@Chg{Version=[2],New=[],Old=[All return components are intended for use
with a data stream representing a value of the indicated record type.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[],Old=[All Is_Record(Component) = True arguments are
appropriate. ]}All return
values are valid parameters for all query
operations@Chg{Version=[2],New=[ that accept Record_Components],Old=[]}.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Type_Definition expects an element
that has the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Type_Definition@Chg{Version=[2],New=[ that has one of the following Type_Kinds:
   A_Derived_Type_Definition       (derived from a record type)
   A_Record_Type_Definition],Old=[]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Appropriate Type_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_Derived_Type_Definition       (derived from a record type)
A_Record_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Component expects a component
that has Is_Record(Component) = True and has one of the
following],Old=[Appropriate]} Asis.Data_Decomposition.Type_Model_Kinds:
@begin{Display}
A_Simple_Static_Model
A_Simple_Dynamic_Model
A_Complex_Dynamic_Model
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element or component that does not have one of these
expected kinds or types.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Record_Components (data decomposition)]}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Record_Components} (Type_Definition : @key[in] Asis.Type_Definition)
                           @key[return] Record_Component_List;

@key[function] @AdaSubDefn{Record_Components} (Component : @key[in] Record_Component)
                           @key[return] Record_Component_List;

@key[function] @AdaSubDefn{Record_Components} (Component : @key[in] Array_Component)
                           @key[return] Record_Component_List;
@end{Example}

Type_Definition @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the record type definition to query.
Component @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} a component
which has a record subtype, Is_Record(Component) = True.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0035-1]}
Returns a list of the discriminants and components for the indicated simple
static record type.@Chg{Version=[2],New=[],Old=[ (See rule 6.A above.)]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
The result describes the locations of the record type's discriminants and
components.@Chg{Version=[2],New=[],Old=[ All return components are intended for
use with a data stream representing a value of the indicated record type.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[],Old=[All Is_Record(Component) = True arguments are
appropriate. ]}All return
values are valid parameters for all query
operations@Chg{Version=[2],New=[ that accept Record_Components],Old=[]}.


@begin{SingleNote}
@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
If an Ada implementation uses implementation-dependent record
components (@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} 13.5.1 (15)), then each such component of
the record type is included in the result.
@end{SingleNote}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Type_Definition expects an element
that has the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Type_Definition@Chg{Version=[2],New=[ that has one of Type_Kinds:
   A_Derived_Type_Definition       (derived from a record type)
   A_Record_Type_Definition],Old=[]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Appropriate Type_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_Derived_Type_Definition       (derived from a record type)
A_Record_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Component expects a component
that has Is_Record(Component) = True and has the
following],Old=[Appropriate]} Asis.Data_Decomposition.Type_Model_Kinds:
@begin{Display}
A_Simple_Static_Model
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element or component that does not have one of these
expected kinds or types.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Record_Components (stream)]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
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

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Type_Definition specifies the record type definition to query.
Component specifies a component which has a record subtype,
                  Is_Record(Component) = True.
Data_Stream specifies a data stream containing, at least, the
                  complete set of discriminant or index constraints for the type.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[These functions are superseded by the Ada Stream
operations. The use of any of the functions Record_Components is not recommended
in new programs.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
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

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Type_Definition
expects an element that has the following Element_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Type_Definition that has one of the following Type_Kinds:
   A_Derived_Type_Definition       (derived from a record type)
   A_Record_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Component
expects a component that has one of the following
Asis.Data_Decomposition.Type_Model_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Simple_Static_Model
A_Simple_Dynamic_Model]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element or component that does not have one of these
expected kinds.]}

@begin{SingleNote}
@ChgAdded{Version=[2],Text=[If an Ada implementation uses implementation-dependent record
components (Ada Standard 13.5.1 (15)), then each such component of the
record type is included in the result.]}
@end{SingleNote}

@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Array_Components]}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Array_Components} (Type_Definition : @key[in] Asis.Type_Definition)
                          @key[return] Array_Component;

@key[function] @AdaSubDefn{Array_Components} (Component : @key[in] Record_Component)
                          @key[return] Array_Component;

@key[function] @AdaSubDefn{Array_Components} (Component : @key[in] Array_Component)
                          @key[return] Array_Component;
@end{Example}

Type_Definition @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the array type definition to query.
Component @Chg{Version=[1],New=[specifies],Old=[      @en Specifies]} a
component which has an array subtype, Is_Array(Component) = True.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0035-1]}
Returns a single component, describing all components of the indicated
array type. The array type shall be a simple static, or a simple dynamic
array type.@Chg{Version=[2],New=[],Old=[ (See rules 6.A and 6.B above.)]}

The result contains all information necessary to index and extract any
component of a data stream representing a value of the indicated array
type.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[],Old=[All Is_Array(Component) = True values are
appropriate. ]}All return values
are valid parameters for all query operations@Chg{Version=[2],New=[ that
accept Array_Components],Old=[]}.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Type_Definition expects an element
that has the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Type_Definition@Chg{Version=[2],New=[ that has one of the following Type_Kinds:
   A_Derived_Type_Definition       (derived from an array type)
   An_Unconstrained_Array_Definition
   A_Constrained_Array_Definition],Old=[]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Appropriate Type_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_Derived_Type_Definition       (derived from an array type)
An_Unconstrained_Array_Definition
A_Constrained_Array_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Component expects a component
that has Is_Array(Component) = True and has
one of the following],Old=[Appropriate]} Asis.Data_Decomposition.Type_Model_Kinds:
@begin{Display}
A_Simple_Static_Model
A_Simple_Dynamic_Model
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element or component that does not have one of these
expected kinds or types.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Array_Iterator]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2], Text=[@key[function] @AdaSubDefn{Array_Iterator} (Type_Definition : @key[in] Asis.Type_Definition)
                        @key[return] Array_Component_Iterator;]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2], Text=[@key[function] @AdaSubDefn{Array_Iterator} (Component : @key[in] Record_Component)
                        @key[return] Array_Component_Iterator;]}

@key[function] @AdaSubDefn{Array_Iterator} (Component : @key[in] Array_Component)
                        @key[return] Array_Component_Iterator;
@end{Example}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0035-1]}
@Chg{Version=[2], New=[Type_Definition specifies the array type definition to
query. ],Old=[]}Component @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]}
@Chg{Version=[2],New=[a component which has an array subtype,
Is_Array(Component) = True.],Old=[an array component to be used for iteration]}

Returns an iterator poised to fetch the 1st component of an array.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Type_Definition expects
an element that has the following Element_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Type_Definition that has one of the following Type_Kinds:
   A_Derived_Type_Definition       (derived from an array type)
   An_Unconstrained_Array_Definition
   A_Constrained_Array_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[
Component expects a component
that has Is_Array(Component) = True and has
one of the following Asis.Data_Decomposition.Type_Model_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Simple_Static_Model
A_Simple_Dynamic_Model]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element or component that does not have one of these
expected kinds or types.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Component_Data_Stream]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
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

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[These functions are superseded by the Ada Stream
operations. The use of any of the functions Component_Data_Stream is not recommended
in new programs.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Component specifies the component or discriminant to be extracted.
Index specifies an index, 1..Array_Length, within an array.
Indexes specifies a list of index values, there is one value for
              each dimension of the array type, each index N is in the
              range 1..Array_Length (Component, N);.
Iterator specifies the array component to extract.
Data_Stream specifies the data stream from which to extract the result.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns a data stream representing just the value of the chosen Component.
The return value is sliced from the data stream. The Data_Stream shall
represent a value of the appropriate type. It may have been obtained from
a file, extracted from another data stream, or artificially constructed
using the Construct_Artificial_Data_Stream operation.]}

@ChgAdded{Version=[2],Text=[An artificial data stream may raise ASIS_Inappropriate_Element (the Status
is Value_Error). Only the constraint values are valid, once they
have been properly initialized, and can be safely extracted from an
artificial data stream.]}

@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element if given Nil_Array_Component_Iterator
or one where Done(Iterator) = True. The Status value is Data_Error.
The Diagnosis string will indicate the kind of error detected.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Component expects any kind of non-Nil component.
Raises ASIS_Inappropriate_Element with a Status of Value_Error for any Nil
component.]}
@end{DescribeCode}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
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


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Component_Declaration]}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Component_Declaration} (Component : @key[in] Record_Component)
                               @key[return] Asis.Declaration;
@end{Example}

Component @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the component to be queried

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
Returns an Asis.Declaration, which is either A_Component_Declaration
or A_Discriminant_Specification. These values can be used to determine the
subtype, type, and base type of the record component. The result may be an
explicit declaration made by the user, or, it may be an implicit
component declaration for an implementation-defined component (@Chg{Version=[2],
New=[Ada Standard],Old=[Reference Manual]} 13.5.1(15)).

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2], New=[Component expects any kind of non-Nil component.],
Old=[All non-Nil component values are appropriate.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status of
Value_Error for any element or component that does not have one of these
expected kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element that has
the following ],Old=[]}Element_Kinds:
@begin{Display}
A_Declaration@Chg{Version=[2],New=[ that has one of the following Declaration_Kinds:
    A_Component_Declaration
    A_Discriminant_Specification],Old=[]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Returns Declaration_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_Component_Declaration
A_Discriminant_Specification]}
@end{Display}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Component_Indication]}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Component_Indication} (Component : @key[in] Array_Component)
                              @key[return] Asis.Subtype_Indication;
@end{Example}

Component @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the component
to be queried.

Returns an Asis.Subtype_Indication. These values can be used to determine
the subtype, type, and base type of the array components.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2], New=[Component expects any kind of non-Nil component.],
Old=[All non-Nil component values are appropriate.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status of
Value_Error for any element or component that does not have one of these
expected kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element that has
the following ],Old=[]}Element_Kinds:
@begin{Display}
A_Subtype_Indication
@end{Display}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function All_Named_Components]}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{All_Named_Components} (Type_Definition : @key[in] Asis.Type_Definition)
                              @key[return] Asis.Defining_Name_List;
@end{Example}

Type_Definition @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the record type definition to query.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
Returns a list of all discriminant and component entity names defined by
the record type. All record type definitions are appropriate for this
operation. This query provides a means for determining whether a field,
with a particular name, exists for some possible instance of the record
type. This list does not include the names of implementation-defined
components (@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} 13.5.1 (15)); those name have the form of
An_Attribute_Reference expression.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Type_Definition expects an element
that has the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Type_Definition@Chg{Version=[2],New=[ that has one of the following Type_Kinds:
   A_Derived_Type_Definition       (derived from a record type)
   A_Record_Type_Definition],Old=[]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Appropriate Type_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_Derived_Type_Definition       (derived from a record type)
A_Record_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[and Type_Definition expects an element
that has one of the following],Old=[Appropriate]} Asis.Data_Decomposition.Type_Model_Kinds:
@begin{Display}
A_Simple_Static_Model
A_Simple_Dynamic_Model
A_Complex_Dynamic_Model
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Array_Length]}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Array_Length} (Component : @key[in] Record_Component)
                      @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Array_Length} (Component : @key[in] Array_Component)
                      @key[return] Asis.ASIS_Natural;
@end{Example}

Component @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the component
to query.

Returns the number of components within an array valued component. The
array subtype may be multidimensional. The result treats the array as if
it were unidimensional. It is the product of the 'Lengths of the
individual array dimensions.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Component expects a component that has],Old=[All]}
Is_Array(Component) = True@Chg{Version=[2],New=[],Old=[ values
are appropriate]}.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any component that is not an array component.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Array_Length (with dimension)]}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Array_Length} (Component : @key[in] Record_Component;
                       Dimension : @key[in] Asis.ASIS_Natural)
                      @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Array_Length} (Component : @key[in] Array_Component;
                       Dimension : @key[in] Asis.ASIS_Natural)
                       @key[return] Asis.ASIS_Natural;
@end{Example}

Component @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the component
to query. Dimension @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
array dimension to query.

Returns the number of components within an array valued component. The
array subtype may be unidimensional. The result is the 'Length(Dimension)
of the array.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Component expects a component that has],Old=[All]}
Is_Array(Component) = True@Chg{Version=[2],New=[],Old=[ values
are appropriate]}.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any component that is not an array component.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Size]}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Size} (Type_Definition : @key[in] Asis.Type_Definition)
              @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Size} (Component : @key[in] Record_Component) @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Size} (Component : @key[in] Array_Component)  @key[return] Asis.ASIS_Natural;
@end{Example}

Type_Definition @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} a type definition, whose 'Size is desired.
Component @Chg{Version=[1],New=[specifies],Old=[      @en Specifies]} a component, whose 'Size is desired.

Returns the minimum number of bits required to hold a simple static type,
the number of bits allocated to hold a record field, or the number of bits
allocated to hold each array component.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Type_Definition expects an element
of],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Type_Definition
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Component expects a component
that has the following],Old=[Appropriate]} Asis.Data_Decomposition.Type_Model_Kinds:
@begin{Display}
A_Simple_Static_Model
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element or component that does not have one of these
expected kinds.]}
@end{DescribeCode}

@begin{UsageNote}
For components, this is the number of bits allocated
within the composite value. It may be greater than the number
of bits occupied by data values of this component type.
Also, the data value, when occupying more space than is
minimally required, may be preceded, followed, or surrounded by
padding bits which are necessary to fully occupy the space allotted.
@end{UsageNote}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Size (stream)]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Size} (Type_Definition : @key[in] Asis.Type_Definition;
               Data_Stream     : @key[in] Portable_Data)
              @key[return] Asis.ASIS_Natural;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[This function is superseded by the Ada Stream
operations. The use of this function Size is not recommended
in new programs.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Type_Definition specifies the type definition to query.
Data_Stream specifies a data stream containing, at least, the complete
set of discriminant or index constraints for the type.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Returns the 'Size of a value of this type, with these constraints. This is
the minimum number of bits that is needed to hold any possible value of the
given fully constrained subtype. Only the constraint portion of the
Data_Stream is checked.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[The Data_Stream may be a data stream or it may be an artificial
data stream created by the Construct_Artificial_Data_Stream operation.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Keepnext=[T],Type=[Leading],Text=[Type_Definition
expects an element
that has the following Element_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Keepnext=[T],Type=[Leading],Text=[and
Type_Definition expects an element
that has one of the following Asis.Data_Decomposition.Type_Model_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Simple_Static_Model
A_Simple_Dynamic_Model]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Position]}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Position} (Component : @key[in] Record_Component)
                  @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Position} (Component : @key[in] Array_Component;
                   Index     : @key[in] Asis.ASIS_Positive)
                  @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Position} (Component : @key[in] Array_Component;
                   Indexes   : @key[in] Dimension_Indexes)
                  @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Position} (Iterator : @key[in] Array_Component_Iterator)
                  @key[return] Asis.ASIS_Natural;
@end{Example}

Component @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the component to query.
Index @Chg{Version=[1],New=[specifies],Old=[      @en Specifies]} a value in the range 1..Array_Length (Component),
the index of the component to query.
Indexes @Chg{Version=[1],New=[specifies],Old=[    @en Specifies]} a list of index values, there is one value for
each dimension of the array type, each index N is in the
range 1..Array_Length (Component, N);.
Iterator @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} a particular
array component to query.

Returns the System.Storage_Unit offset, from the start of the first storage
unit occupied by the enclosing composite type, of the first of the storage
units occupied by the Component. The offset is measured in storage units.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@Chg{Version=[2], New=[Component expects any kind of non-Nil component. Raises
ASIS_Inappropriate_Element with a Status of Value_Error for any
Nil component.],
Old=[All non-Nil component values are appropriate.]} Raises
ASIS_Inappropriate_Element with a Status of Data_Error if any index is not
in the expected range or if Done (Iterator) = True. The Status value will
be Data_Error. The Diagnosis string will indicate the kind of error
detected.
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function First_Bit]}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{First_Bit} (Component : @key[in] Record_Component)
                   @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{First_Bit} (Component : @key[in] Array_Component;
                    Index     : @key[in] Asis.ASIS_Positive)
                   @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{First_Bit} (Component : @key[in] Array_Component;
                    Indexes   : @key[in] Dimension_Indexes)
                   @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{First_Bit} (Iterator : @key[in] Array_Component_Iterator)
                   @key[return] Asis.ASIS_Natural;
@end{Example}

Component @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the component to query.
Index @Chg{Version=[1],New=[specifies],Old=[      @en Specifies]} a value in the range
1..Array_Length (Component), the index of the component to query.
Indexes @Chg{Version=[1],New=[specifies],Old=[    @en Specifies]} a list of index values, there is one value for
each dimension of the array type, each index N is in the
range 1..Array_Length (Component, N);.
Iterator @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} a particular
array component to query.

Returns the bit offset, from the start of the first of the storage units
occupied by the Component, of the first bit occupied by the Component. The
offset is measured in bits.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@Chg{Version=[2], New=[Component expects any kind of non-Nil component. Raises
ASIS_Inappropriate_Element with a Status of Value_Error for any
Nil component.],
Old=[All non-Nil component values are appropriate.]} Raises
ASIS_Inappropriate_Element with a Status of Data_Error if any index is not
in the expected range or if Done (Iterator) = True. The Status value will
be Data_Error. The Diagnosis string will indicate the kind of error
detected.
@end{DescribeCode}


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Last_Bit]}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Last_Bit} (Component : @key[in] Record_Component)
                  @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Last_Bit} (Component : @key[in] Array_Component;
                   Index     : @key[in] Asis.ASIS_Positive)
                  @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Last_Bit} (Component : @key[in] Array_Component;
                   Indexes   : @key[in] Dimension_Indexes)
                  @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Last_Bit} (Iterator : @key[in] Array_Component_Iterator)
                  @key[return] Asis.ASIS_Natural;
@end{Example}

Component @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the component to query.
Index @Chg{Version=[1],New=[specifies],Old=[      @en Specifies]} a value in the range 1..Array_Length (Component),
the index of the component to query.
Indexes @Chg{Version=[1],New=[specifies],Old=[    @en Specifies]} a list of index values, there is one value for
each dimension of the array type, each index N is in the
range 1..Array_Length (Component, N);.
Iterator @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} a particular array component to query.

Returns the bit offset, from the start of the first of the storage units
occupied by the Index'th Element, of the last bit occupied by the Element.
The offset is measured in bits.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@Chg{Version=[2], New=[Component expects any kind of non-Nil component. Raises
ASIS_Inappropriate_Element with a Status of Value_Error for any
Nil component.],
Old=[All non-Nil component values are appropriate.]} Raises
ASIS_Inappropriate_Element with a Status of Data_Error if any index is not
in the expected range or if Done (Iterator) = True. The Status value will
be Data_Error. The Diagnosis string will indicate the kind of error detected.
@end{DescribeCode}

*** End TBD ***


@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Portable_Constrained_Subtype]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[generic]
    @key[type] Constrained_Subtype @key[is private];
@key[function] @AdaSubDefn{Portable_Constrained_Subtype}
            (Data_Stream : @key[in] Portable_Data)
            @key[return] Constrained_Subtype;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[This generic function is superseded by the Ada Stream
operations. The use of the generic function Portable_Constrained_Subtype
is not recommended in new programs.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[Data_Stream specifies an extracted
component of a record.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
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



@ChgNote{SI99-0058-1 add section on newly obsolete subprogram}
@LabeledAddedSubclause{Version=[2],Name=[function Construct_Artificial_Data_Stream]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Construct_Artificial_Data_Stream}
            (Type_Definition : @key[in] Asis.Type_Definition;
             Data_Stream     : @key[in] Portable_Data;
             Discriminant    : @key[in] Record_Component;
             Value           : @key[in] Portable_Data)
            @key[return] Portable_Data;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
@ChgAdded{Version=[2],Text=[This function is superseded by the Ada Stream
operations. The use of the function Construct_Artificial_Data_Stream
is not recommended in new programs.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0058-1]}
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

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1],ARef=[SI99-0058-1]}
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

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
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
@ChgAdded{Version=[2],Text=[These generic packages are superseded by the Ada
Stream operations. The use of generic packages Portable_Array_Type_1
Portable_Array_Type_2, and Portable_Array_Type_3 is not recommended in new
programs.]}

@ChgAdded{Version=[2],Text=[Value specifies an array value
which is to be converted to a portable data stream.]}

@ChgAdded{Version=[2],Text=[Instantiated with an unconstrained array subtype, this function will
correctly convert values of that subtype to a portable data value.
"Correctly" means that no information is lost. No guarantee is made that
the result will not be larger (larger 'Size) than the argument.
This interface will correctly capture the array index (dope vector)
information associated with the array value.]}

@ChgAdded{Version=[2],Text=[Use the Portable_Constrained_Subtype generic for
statically constrained array subtypes.]}

@ChgAdded{Version=[2],Text=[Array types with more than 3 dimensions can be converted to portable data
streams by placing them inside a discriminated record type (where the
discriminants are the dynamic array index constraints) and then converting
the record value. The record will then contain the necessary array index
(dope vector) information.]}
@end{DescribeCode}


