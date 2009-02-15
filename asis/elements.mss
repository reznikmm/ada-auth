@Part(elements, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/elements.mss,v $}
@comment{$Revision: 1.13 $ $Date: 2009/02/10 06:51:27 $}


@LabeledSection{package Asis.Elements}

@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis],Child=[Elements]}Asis.Elements
shall exist. The package
shall provide interfaces equivalent to those described in the
following subclauses.],
Old=[@f{@key[package] @ChildUnit{Parent=[Asis],Child=[Elements]}Asis.Elements @key[is]}]}

Asis.Elements encapsulates a set of queries that operate on all elements
and some queries specific to A_Pragma elements.


@LabeledClause{function Unit_Declaration}

Gateway queries between Compilation_Units and Elements.

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Unit_Declaration} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
                          @key[return] Asis.Declaration;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the unit to query.

Returns the element representing the declaration of the compilation_unit.

Returns a Nil_Element if the unit is A_Nonexistent_Declaration,
A_Nonexistent_Body, A_Configuration_Compilation, or An_Unknown_Unit.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects a unit of
any],Old=[All]} Unit_Kinds@Chg{Version=[2],New=[],Old=[ are appropriate]} except Not_A_Unit.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Compilation_Unit with a
Status of Value_Error for any unit that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element that has
one of the following],Old=[]} Declaration_Kinds:
@begin{Display}
Not_A_Declaration
A_Function_Body_Declaration
A_Function_Declaration
A_Function_Instantiation
A_Generic_Function_Declaration
A_Generic_Package_Declaration
A_Generic_Procedure_Declaration
A_Package_Body_Declaration
A_Package_Declaration
A_Package_Instantiation
A_Procedure_Body_Declaration
A_Procedure_Declaration
A_Procedure_Instantiation
A_Task_Body_Declaration
A_Package_Renaming_Declaration
A_Procedure_Renaming_Declaration
A_Function_Renaming_Declaration
A_Generic_Package_Renaming_Declaration
A_Generic_Procedure_Renaming_Declaration
A_Generic_Function_Renaming_Declaration
A_Protected_Body_Declaration
@end{Display}
@end{DescribeCode}


@LabeledClause{function Enclosing_Compilation_Unit}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Enclosing_Compilation_Unit} (Element : @key[in] Asis.Element)
                                    @key[return] Asis.Compilation_Unit;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} an Element whose Compilation_Unit is desired.

Returns the Compilation_Unit that contains the given Element.

Raises ASIS_Inappropriate_Element if the Element is a Nil_Element.
@end{DescribeCode}


@LabeledClause{function Context_Clause_Elements}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Context_Clause_Elements}
            (Compilation_Unit : @key[in] Asis.Compilation_Unit;
             Include_Pragmas  : @key[in] Boolean := False)
            @key[return] Asis.Context_Clause_List;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the unit to query.
Include_Pragmas @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} whether pragmas are to be returned.

Returns a list of with clauses, use clauses, and pragmas that explicitly
appear in the context clause of the compilation unit, in their order of
appearance.

Returns a Nil_Element_List if the unit has A_Nonexistent_Declaration,
A_Nonexistent_Body, or An_Unknown_Unit Unit_Kind.

@begin{ImplReq}
@begin{Itemize}
@noprefix@;All pragma Elaborate elements for this unit will appear in this
list. Other pragmas will appear in this list, or in the Compilation_Pragmas
list, or both.
@end{Itemize}
@end{ImplReq}

@begin{ImplPerm}
@begin{Itemize}
@noprefix@;Implementors are encouraged to use this list to return all pragmas whose
full effect is determined by their exact textual position. Pragmas that
do not have placement dependencies may be returned in either list. Only
pragmas that appear in the unit's context clause are returned
by this query. All other pragmas, affecting the compilation of this
unit, are available from the Compilation_Pragmas query.
@end{Itemize}
@end{ImplPerm}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0037-1]}
@Chg{Version=[2],New=[],Old=[Ada predefined packages, such as package Standard,
may or may not have context-clause elements available for processing by
applications. The physical existence of a package Standard is implementation
specific. The same is true for other Ada predefined packages, such as
Ada.Text_Io and Ada.Direct_Io. The Origin query can be used to determine whether
or not a particular unit is an Ada Predefined unit.]}

@begin{ImplPerm}
@begin{Itemize}
@noprefix@;Results of this query may vary across ASIS implementations. Some
implementations normalize all multi-name with clauses and use clauses
into an equivalent sequence of single-name with clause and use clauses.
Similarly, an implementation may retain only a single reference to a name
that appeared more than once in the original context clause.
Some implementors will return only pragma
Elaborate elements in this list and return all other pragmas via the
Compilation_Pragmas query.
@end{Itemize}
@end{ImplPerm}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects a unit of
any],Old=[All]} Unit_Kinds@Chg{Version=[2],New=[],Old=[ are appropriate]} except Not_A_Unit.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Compilation_Unit with a
Status of Value_Error for any unit that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element that has
one of the following],Old=[]} Element_Kinds:
@begin{Display}
A_Pragma
A_Clause@Chg{Version=[2],New=[ that has one of the following Clause_Kinds:
  A_With_Clause
  A_Use_Package_Clause],Old=[]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Type=[Leading],Keepnext=[T],Text=[Returns Clause_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_With_Clause
A_Use_Package_Clause]}
@end{Display}
@end{DescribeCode}


@LabeledClause{function Configuration_Pragmas}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Configuration_Pragmas} (The_Context : @key[in] Asis.Context)
                             @key[return] Asis.Pragma_Element_List;
@end{Example}

The_Context @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the Context to query.

Returns a list of pragmas that apply to all future compilation_unit elements
compiled into The_Context. Pragmas returned by this query should
have appeared in a compilation that had no compilation_unit elements.
To the extent that order is meaningful, the pragmas should be in
their order of appearance in the compilation. (The order is implementation
dependent, many pragmas have the same effect regardless of order.)

Returns a Nil_Element_List if there are no such configuration pragmas.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element that
has the following],Old=[]} Element_Kinds:
@begin{Display}
A_Pragma
@end{Display}
@end{DescribeCode}


@LabeledClause{function Compilation_Pragmas}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Compilation_Pragmas} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
                             @key[return] Asis.Pragma_Element_List;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the unit to query.

Returns a list of pragmas that apply to the compilation of the unit.
To the extent that order is meaningful, the pragmas should be in
their order of appearance in the compilation. (The order is implementation
dependent, many pragmas have the same effect regardless of order.)

There are two sources for the pragmas that appear in this list:

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
Program unit pragmas appearing at the place of a compilation_unit.
See @Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} 10.1.5(4).

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
Configuration pragmas appearing before the first
compilation_unit of a compilation. See @Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} 10.1.5(8).
@end{Itemize}

This query does not return Elaborate pragmas from the unit context
clause of the compilation unit; they do not apply to the compilation,
only to the unit.

Use the Context_Clause_Elements query to obtain a list of all pragmas
(including Elaborate pragmas) from the context clause of a compilation unit.

Pragmas from this query may be duplicates of some or all of the
non-Elaborate pragmas available from the Context_Clause_Elements query.
Such duplication is simply the result of the textual position of the
pragma--globally effective pragmas may appear textually within the context
clause of a particular unit, and be returned as part of the Context_Clause
for that unit.

Ada predefined packages, such as package Standard, may or may not have
pragmas available for processing by applications. The physical
existence of a package Standard is implementation specific. The same
is true for other Ada predefined packages, such as Ada.Text_Io and
Ada.Direct_Io. The Origin query can be used to determine whether or
not a particular unit is an Ada Predefined unit.

@leading@keepnext@;Returns a Nil_Element_List if the compilation unit:

@begin{Itemize}
has no such applicable pragmas; or

is an An_Unknown_Unit, A_Nonexistent_Declaration, or A_Nonexistent_Body.
@end{Itemize}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects a unit of
any],Old=[All]} Unit_Kinds@Chg{Version=[2],New=[],Old=[ are appropriate]} except Not_A_Unit.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Compilation_Unit with a
Status of Value_Error for any unit that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element that
has the following],Old=[]} Element_Kinds:
@begin{Display}
A_Pragma
@end{Display}
@end{DescribeCode}

@Comment{The following does not belong here at all.}

@begin{Example}
@ChgDeleted{Version=[1],Text=[Element_Kinds Hierarchy]}

@ChgDeleted{Version=[1],Text=[   Element_Kinds Value      Subordinate Kinds
Key: Read "->" as "can be further categorized by its"]}

@ChgDeleted{Version=[1],Text=[   A_Pragma            -> Pragma_Kinds]}

@ChgDeleted{Version=[1],Text=[   A_Defining_Name     -> Defining_Name_Kinds
                                 -> Operator_Kinds]}

@ChgDeleted{Version=[1],Text=[   A_Declaration       -> Declaration_Kinds
                                 -> Declaration_Origin
                                 -> Mode_Kinds
                                 -> Subprogram_Default_Kinds]}

@ChgDeleted{Version=[1],Text=[   A_Definition        -> Definition_Kinds
                                 -> Trait_Kinds
                                 -> Type_Kinds
                                 -> Formal_Type_Kinds
                                 -> Access_Type_Kinds
                                 -> Root_Type_Kinds
                                 -> Constraint_Kinds
                                 -> Discrete_Range_Kinds]}

@ChgDeleted{Version=[1],Text=[   An_Expression       -> Expression_Kinds
                                 -> Operator_Kinds
                                 -> Attribute_Kinds]}

@ChgDeleted{Version=[1],Text=[   An_Association      -> Association_Kinds]}

@ChgDeleted{Version=[1],Text=[   A_Statement         -> Statement_Kinds]}

@ChgDeleted{Version=[1],Text=[   A_Path              -> Path_Kinds]}

@ChgDeleted{Version=[1],Text=[   A_Clause            -> Clause_Kinds
                                 -> Representation_Clause_Kinds]}

@ChgDeleted{Version=[1],Text=[   An_Exception_Handler]}
@end{Example}


@LabeledClause{function Element_Kind}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Element_Kind} (Element : @key[in] Asis.Element)
                       @key[return] Asis.Element_Kinds;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element to query.

@ChgAdded{Version=[1],Text=[The hierarchy of Element_Kinds is shown in
@RefSecNum{type Element_Kinds}.]}

Returns the Element_Kinds value of Element.
Returns Not_An_Element for a Nil_Element.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1],ARef=[SI99-0037-1]}
@Chg{Version=[2],New=[Element expects any kind of element],
Old=[All element kinds are expected]}.
@end{DescribeCode}


@LabeledClause{function Pragma_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Pragma_Kind} (Pragma_Element : @key[in] Asis.Pragma_Element)
                     @key[return] Asis.Pragma_Kinds;
@end{Example}

Pragma_Element @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the element to query.

Returns the Pragma_Kinds value of Pragma_Element.
Returns Not_A_Pragma for any unexpected element such as a
Nil_Element, A_Statement, or A_Declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Pragma_Element expects an element
that has the following],Old=[Expected]} Element_Kinds:
@begin{Display}
A_Pragma
@end{Display}
@end{DescribeCode}


@LabeledClause{function Defining_Name_Kind}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Defining_Name_Kind} (Defining_Name : @key[in] Asis.Defining_Name)
                            @key[return] Asis.Defining_Name_Kinds;
@end{Example}

Defining_Name @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the element to query.

Returns the Defining_Name_Kinds value of the Defining_Name.

Returns Not_A_Defining_Name for any unexpected element such as a
Nil_Element, A_Clause, or A_Statement.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Defining_Name expects an element
that has the following],Old=[Expected]} Element_Kinds:
@begin{Display}
A_Defining_Name
@end{Display}
@end{DescribeCode}


@LabeledClause{function Declaration_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Declaration_Kind} (Declaration : @key[in] Asis.Declaration)
                            @key[return] Asis.Declaration_Kinds;
@end{Example}

Declaration @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the element
to query.

Returns the Declaration_Kinds value of the Declaration.

Returns Not_A_Declaration for any unexpected element such as a
Nil_Element, A_Definition, or A_Statement.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
that has the following],Old=[Expected]} Element_Kinds:
@begin{Display}
A_Declaration
@end{Display}
@end{DescribeCode}


@ChgNote{SI99-0022-1 replace subprogram}
@LabeledRevisedClause{Version=[2],New=[function Has_Abstract],
Old=[function Trait_Kind]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2], Text=[@key[function] @AdaSubDefn{Has_Abstract} ( Element : @key[in] Asis.Element ) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2], Text=[Element specifies the element to query.]}

@ChgAdded{Version=[2], Text=[Returns True if the reserved word @key[abstract] appears in Element, and
False otherwise.]}

@ChgAdded{Version=[2], Text=[Returns False for any unexpected element, including a Nil_Element.]}

@ChgAdded{Version=[2], Text=[Element expects an element that has one of the following Declaration_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  A_Formal_Procedure_Declaration
  A_Formal_Function_Declaration
  A_Function_Declaration
  A_Private_Type_Declaration
  A_Private_Extension_Declaration
  A_Procedure_Declaration
  A_Type_Declaration]}
@end{Display}

@ChgAdded{Version=[2], Text=[or an element that has one of the following Definition_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  A_Private_Extension_Definition
  A_Private_Type_Definition
  A_Tagged_Private_Type_Definition
  A_Type_Definition]}
@end{Display}

@ChgAdded{Version=[2], Text=[or an element that has one of the following Formal_Type_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  A_Formal_Private_Type_Definition
  A_Formal_Tagged_Private_Type_Definition
  A_Formal_Derived_Type_Definition]}
@end{Display}
@end{DescribeCode}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0022-1]}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Trait_Kind} (Element : @key[in] Asis.Element)
                      @key[return] Asis.Trait_Kinds;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0022-1]}
@ChgDeleted{Version=[2],Text=[Element @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the Element to
query.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0022-1]}
@ChgDeleted{Version=[2],Text=[Returns the Trait_Kinds value of the Element.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0022-1]}
@ChgDeleted{Version=[2],Text=[Returns Not_A_Trait for any unexpected element such as a
Nil_Element, A_Statement, or An_Expression.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0022-1]}
@ChgDeleted{Version=[2],Type=[Leading],Keepnext=[T],Text=[Expected Declaration_Kinds:]}
@begin{Display}
@ChgDeleted{Version=[2],Text=[A_Private_Type_Declaration
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

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0022-1]}
@ChgDeleted{Version=[2],Type=[Leading],Keepnext=[T],Text=[Expected Definition_Kinds:]}
@begin{Display}
@ChgDeleted{Version=[2],Text=[A_Component_Definition
A_Private_Type_Definition
A_Tagged_Private_Type_Definition
A_Private_Extension_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0022-1]}
@ChgDeleted{Version=[2],Type=[Leading],Keepnext=[T],Text=[Expected Type_Kinds:]}
@begin{Display}
@ChgDeleted{Version=[2],Text=[A_Derived_Type_Definition
A_Derived_Record_Extension_Definition
A_Record_Type_Definition
A_Tagged_Record_Type_Definition]}
@end{Display}

@ChgDeleted{Version=[2],Type=[Leading],Keepnext=[T],Text=[Expected Formal_Type_Kinds:]}
@begin{Display}
@ChgDeleted{Version=[2],Text=[A_Formal_Private_Type_Definition
A_Formal_Tagged_Private_Type_Definition
A_Formal_Derived_Type_Definition]}
@end{Display}
@end{DescribeCode}


@comment{Remainder of SI99-0022-1 function inserts}

@Comment{@LabeledAddedClause{Version=[2],Name=[function Has_Aliased]}}
@LabeledAddedSubClause{Version=[2],Name=[function Has_Aliased]}
@begin{Discussion}
@ChgAdded{Version=[2],Text=[Can't use a real clause for now, as that would
change all of the following clause numbers, so this is a subclause temporarily.]}
@end{Discussion}

@begin{DescribeCode}
@begin{Example}
@ChgAdded{Version=[2], Text=[@key[function] @AdaSubDefn{Has_Aliased} ( Element : @key[in] Asis.Element ) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2], Text=[Element specifies the Element to query.]}

@ChgAdded{Version=[2], Text=[Returns True if the reserved word @key[aliased]
appears in Element, and False otherwise.]}

@ChgAdded{Version=[2], Text=[Returns False for any unexpected element, including
a Nil_Element.]}

@ChgAdded{Version=[2], Text=[Element expects an element that has one of the following Declaration_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  A_Constant_Declaration
  A_Deferred_Constant_Declaration
  A_Return_Object_Specification
  A_Variable_Declaration]}
@end{Display}

@ChgAdded{Version=[2], Text=[or an element that has the following Definition_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  A_Component_Definition]}
@end{Display}
@end{DescribeCode}



@Comment{@LabeledAddedClause{Version=[2],Name=[function Has_Limited]}}
@LabeledAddedSubClause{Version=[2],Name=[function Has_Limited]}
@begin{Discussion}
@ChgAdded{Version=[2],Text=[Can't use a real clause for now, as that would
change all of the following clause numbers, so this is a subclause temporarily.]}
@end{Discussion}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2], Text=[@key[function] @AdaSubDefn{Has_Limited} ( Element : @key[in] Asis.Element ) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2], Text=[Element specifies the Element to query.]}

@ChgAdded{Version=[2], Text=[Returns True if the reserved word @key[limited] appears in Element, and
False otherwise.]}

@ChgAdded{Version=[2], Text=[Returns False for any unexpected element, including a Nil_Element.]}

@ChgAdded{Version=[2], Text=[Element expects an element that has the following Clause_Kinds:]}
  A_With_Clause

@ChgAdded{Version=[2], Text=[or an element that has one of the following Declaration_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  A_Type_Declaration
  A_Private_Type_Declaration
  A_Private_Extension_Declaration]}
@end{Display}

@ChgAdded{Version=[2], Text=[or an element that has one of the following Definition_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  A_Type_Definition
  A_Private_Type_Definition
  A_Tagged_Private_Type_Definition
  A_Private_Extension_Definition
  An_Interface_Type_Definition]}
@end{Display}

@ChgAdded{Version=[2], Text=[or an element that has one of the following Formal_Type_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  A_Formal_Private_Type_Definition
  A_Formal_Tagged_Private_Type_Definition
  A_Formal_Derived_Type_Definition]}
@end{Display}
@end{DescribeCode}


@Comment{@LabeledAddedClause{Version=[2],Name=[function Has_Private]}}
@LabeledAddedSubClause{Version=[2],Name=[function Has_Private]}
@begin{Discussion}
@ChgAdded{Version=[2],Text=[Can't use a real clause for now, as that would
change all of the following clause numbers, so this is a subclause temporarily.]}
@end{Discussion}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2], Text=[@key[function] @AdaSubDefn{Has_Private} ( Element : @key[in] Asis.Element ) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2], Text=[Element specifies the element to query.]}

@ChgAdded{Version=[2], Text=[Returns True if the reserved word @key[private] appears in Element, and
False otherwise.]}

@ChgAdded{Version=[2], Text=[Returns False for any unexpected element, including a Nil_Element.]}

@ChgAdded{Version=[2], Text=[Element expects an element that has one of the following Declaration_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  A_Type_Declaration
  A_Private_Type_Declaration]}
@end{Display}

@ChgAdded{Version=[2], Text=[or an element that has one of the following Definition_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  A_Private_Extension_Definition
  A_Private_Type_Definition
  A_Tagged_Private_Type_Definition
  A_Type_Definition]}
@end{Display}

@ChgAdded{Version=[2], Text=[or an element that has one of the following Formal_Type_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  A_Formal_Private_Type_Definition
  A_Formal_Tagged_Private_Type_Definition]}
@end{Display}

@ChgAdded{Version=[2], Text=[or an element that has the following Clause_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  A_With_Clause]}
@end{Display}
@end{DescribeCode}


@Comment{@LabeledAddedClause{Version=[2],Name=[function Has_Protected]}}
@LabeledAddedSubClause{Version=[2],Name=[function Has_Protected]}
@begin{Discussion}
@ChgAdded{Version=[2],Text=[Can't use a real clause for now, as that would
change all of the following clause numbers, so this is a subclause temporarily.]}
@end{Discussion}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2], Text=[@key[function] @AdaSubDefn{Has_Protected} ( Element : @key[in] Asis.Element ) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2], Text=[Element specifies the Element to query.]}

@ChgAdded{Version=[2], Text=[Returns True if the reserved word @key[protected] appears in Element, and
False otherwise.]}

@ChgAdded{Version=[2], Text=[Returns False for any unexpected element, including a Nil_Element.]}

@ChgAdded{Version=[2], Text=[Element expects an element that has one of the following Definition_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  An_Interface_Type_Definition
  A_Protected_Definition]}
@end{Display}

@ChgAdded{Version=[2], Text=[or an element that has one of the following Declaration_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  A_Protected_Body_Declaration
  A_Protected_Type_Declaration
  A_Single_Protected_Declaration]}
@end{Display}
@end{DescribeCode}


@Comment{@LabeledAddedClause{Version=[2],Name=[function Has_Reverse]}}
@LabeledAddedSubClause{Version=[2],Name=[function Has_Reverse]}
@begin{Discussion}
@ChgAdded{Version=[2],Text=[Can't use a real clause for now, as that would
change all of the following clause numbers, so this is a subclause temporarily.]}
@end{Discussion}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2], Text=[@key[function] @AdaSubDefn{Has_Reverse} ( Element : @key[in] Asis.Element ) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2], Text=[Element specifies the Element to query.]}

@ChgAdded{Version=[2], Text=[Returns True if the reserved word @key[reverse] appears in Element, and
False otherwise.]}

@ChgAdded{Version=[2], Text=[Returns False for any unexpected element, including a Nil_Element.]}

@ChgAdded{Version=[2], Text=[Element expects an element that has the following Declaration_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  A_Loop_Parameter_Specification]}
@end{Display}
@end{DescribeCode}


@Comment{@LabeledAddedClause{Version=[2],Name=[function Has_Synchronized]}}
@LabeledAddedSubClause{Version=[2],Name=[function Has_Synchronized]}
@begin{Discussion}
@ChgAdded{Version=[2],Text=[Can't use a real clause for now, as that would
change all of the following clause numbers, so this is a subclause temporarily.]}
@end{Discussion}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2], Text=[@key[function] @AdaSubDefn{Has_Synchronized} ( Element : @key[in] Asis.Element ) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2], Text=[Element specifies the Element to query.]}

@ChgAdded{Version=[2], Text=[Returns True if the reserved word @key[synchronized]
appears in Element, and False otherwise.]}

@ChgAdded{Version=[2], Text=[Returns False for any unexpected element, including
a Nil_Element.]}

@ChgAdded{Version=[2], Text=[Element expects an element that has one of the following Definition_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  An_Interface_Type_Definition
  A_Private_Extension_Definition]}
@end{Display}
@end{DescribeCode}


@Comment{@LabeledAddedClause{Version=[2],Name=[function Has_Tagged]}}
@LabeledAddedSubClause{Version=[2],Name=[function Has_Tagged]}
@begin{Discussion}
@ChgAdded{Version=[2],Text=[Can't use a real clause for now, as that would
change all of the following clause numbers, so this is a subclause temporarily.]}
@end{Discussion}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2], Text=[@key[function] @AdaSubDefn{Has_Tagged} ( Element : @key[in] Asis.Element ) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2], Text=[Element specifies the Element to query.]}

@ChgAdded{Version=[2], Text=[Returns True if the reserved word @key[tagged] appears in Element, and
False otherwise.]}

@ChgAdded{Version=[2], Text=[Returns False for any unexpected element, including a Nil_Element.]}

@ChgAdded{Version=[2], Text=[Element expects an element that has one of the following Definition_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  A_Tagged_Incomplete_Type_Definition
  A_Tagged_Private_Type_Definition
  A_Tagged_Record_Type_Definition]}
@end{Display}

@ChgAdded{Version=[2], Text=[or an element that has the following Type_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  A_Tagged_Record_Type_Definition]}
@end{Display}

@ChgAdded{Version=[2], Text=[or an element that has the following Formal_Type_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  A_Formal_Tagged_Private_Type_Definition]}
@end{Display}
@end{DescribeCode}



@Comment{@LabeledAddedClause{Version=[2],Name=[function Has_Task]}}
@LabeledAddedSubClause{Version=[2],Name=[function Has_Task]}
@begin{Discussion}
@ChgAdded{Version=[2],Text=[Can't use a real clause for now, as that would
change all of the following clause numbers, so this is a subclause temporarily.]}
@end{Discussion}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2], Text=[@key[function] @AdaSubDefn{Has_Task} ( Element : @key[in] Asis.Element ) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0022-1]}
@ChgAdded{Version=[2], Text=[Element specifies the Element to query.]}

@ChgAdded{Version=[2], Text=[Returns True if the reserved word @key[task] appears in Element, and
False otherwise.]}

@ChgAdded{Version=[2], Text=[Returns False for any unexpected element, including a Nil_Element.]}

@ChgAdded{Version=[2], Text=[Element expects an element that has one of the following Definition_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  An_Interface_Type_Definition
  A_Task_Definition]}
@end{Display}

@ChgAdded{Version=[2], Text=[or an element that has one of the following Declaration_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[  A_Task_Type_Declaration
  A_Single_task_Declaration
  A_Task_Body_Declaration]}
@end{Display}
@end{DescribeCode}

@comment{End of SI99-0022-1 function inserts}


@LabeledClause{function Declaration_Origin}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Declaration_Origin} (Declaration : @key[in] Asis.Declaration)
                              @key[return] Asis.Declaration_Origins;
@end{Example}

Declaration @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
Declaration to query.

Returns the Declaration_Origins value of the Declaration.

Returns Not_A_Declaration_Origin for any unexpected element such as a
Nil_Element, A_Definition, or A_Clause.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
that has the following],Old=[Expected]} Element_Kinds:
@begin{Display}
A_Declaration
@end{Display}
@end{DescribeCode}


@LabeledClause{function Mode_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Mode_Kind} (Declaration : @key[in] Asis.Declaration)
                               @key[return] Asis.Mode_Kinds;
@end{Example}

Declaration @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the element
to query.

Returns the Mode_Kinds value of the Declaration.

Returns A_Default_In_Mode for an access parameter.

Returns Not_A_Mode for any unexpected element such as a
Nil_Element, A_Definition, or A_Statement.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
that has one of the following],Old=[Expected]} Declaration_Kinds:
@begin{Display}
A_Parameter_Specification
A_Formal_Object_Declaration
@end{Display}
@end{DescribeCode}


@LabeledClause{function Default_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Default_Kind} (Declaration : @key[in] Asis.Generic_Formal_Parameter)
                        @key[return] Asis.Subprogram_Default_Kinds;
@end{Example}

Declaration @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the element
to query.

Returns the Subprogram_Default_Kinds value of the Declaration.

Returns Not_A_Declaration for any unexpected element such as a
Nil_Element, A_Definition, or A_Statement.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
that has one of the following],Old=[Expected]} Declaration_Kinds:
@begin{Display}
A_Formal_Function_Declaration
A_Formal_Procedure_Declaration
@end{Display}
@end{DescribeCode}


@LabeledClause{function Definition_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Definition_Kind} (Definition : @key[in] Asis.Definition)
                           @key[return] Asis.Definition_Kinds;
@end{Example}

Definition @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
Definition to query.

Returns the Definition_Kinds value of the Definition.

Returns Not_A_Definition for any unexpected element such as a
Nil_Element, A_Statement, or A_Declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Definition expects an element
that has the following],Old=[Expected]} Element_Kinds:
@begin{Display}
A_Definition
@end{Display}
@end{DescribeCode}


@LabeledClause{function Type_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Type_Kind} (Definition : @key[in] Asis.Type_Definition)
                                @key[return] Asis.Type_Kinds;
@end{Example}

Definition @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the
Type_Definition to query.

Returns the Type_Kinds value of the Definition.

Returns Not_A_Type_Definition for any unexpected element such as a
Nil_Element, A_Statement, or A_Declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Definition expects an element
that has the following],Old=[Expected]} Definition_Kinds:
@begin{Display}
A_Type_Definition
@end{Display}
@end{DescribeCode}


@LabeledClause{function Formal_Type_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Formal_Type_Kind}
               (Definition : @key[in] Asis.Formal_Type_Definition)
                @key[return] Asis.Formal_Type_Kinds;
@end{Example}

Definition @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
Formal_Type_Definition to query.

Returns the Formal_Type_Kinds value of the Definition.

Returns Not_A_Formal_Type_Definition for any unexpected element such as a
Nil_Element, A_Statement, or A_Declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Definition expects an element
that has the following],Old=[Expected]} Definition_Kinds:
@begin{Display}
A_Formal_Type_Definition
@end{Display}
@end{DescribeCode}


@LabeledClause{function Access_Type_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Access_Type_Kind}
               (Definition : @key[in] Asis.Access_Type_Definition)
                @key[return] Asis.Access_Type_Kinds;
@end{Example}

Definition @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
Access_Type_Definition to query.

Returns the Access_Type_Kinds value of the Definition.

Returns Not_An_Access_Type_Definition for any unexpected element such as a
Nil_Element, A_Statement, or A_Declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Definition expects an element
that has the following],Old=[Expected]} Type_Kinds:
@begin{Display}
An_Access_Type_Definition
@end{Display}
@end{DescribeCode}


@ChgNote{ SI99-0004-1 }
@Comment{@LabeledAddedClause{Version=[2],Name=[function Has_Null_Exclusion]}}
@LabeledAddedSubClause{Version=[2],Name=[function Has_Null_Exclusion]}
@begin{Discussion}
@ChgAdded{Version=[2],Text=[Can't use a real clause for now, as that would
change all of the following clause numbers, so this is a subclause temporarily.]}
@end{Discussion}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Has_Null_Exclusion} -- @examcom{3.2.2(3/2), 3.7(5/2), 3.10 (2/2,6/2),}
                            -- @examcom{6.1(13/2,15/2), 8.5.1(2/2), 12.4(2/2)}
     (Element : @key[in] Asis.Element)
      @key[return] Boolean;]}
@end{Example}


@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[Element specifies the element to check.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Checks if the argument element has a null_exclusion
specifier.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Returns False for any unexpected element such as a Nil_Element,
A_Statement or A_Clause.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Element expects an
element that has one of the following Definition_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[A_Type_Definition
An_Access_Definition
A_Subtype_Indication]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[or Element expects an
that has the following Type_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[An_Access_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[or Element expects an
that has one of the following Declaration_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[A_Discriminant_Specification
A_Parameter_Specification
A_Formal_Object_Declaration
An_Object_Renaming_Declaration]}
@end{Display}
@end{DescribeCode}



@ChgNote{ SI99-0004-1 }
@Comment{@LabeledAddedClause{Version=[2],Name=[function Access_Definition_Kind]}}
@LabeledAddedSubClause{Version=[2],Name=[function Access_Definition_Kind]}
@begin{Discussion}
@ChgAdded{Version=[2],Text=[Can't use a real clause for now, as that would
change all of the following clause numbers, so this is a subclause temporarily.]}
@end{Discussion}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Access_Definition_Kind} -- 3.3.1(2) / 3.10(6)
     (Definition : @key[in] Asis.Definition)
      @key[return] Asis.Access_Definition_Kinds;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1]}
@ChgAdded{Version=[2],Text=[Definition specifies the Definition to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Returns the Access_Definition_Kinds value of the
Definition.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Returns Not_An_Access_Definition for any unexpected element
such as a Nil_Element, A_Statement, or A_Declaration.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0004-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Definition expects an
element that has the following Element_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[An_Access_Definition]}
@end{Display}
@end{DescribeCode}


@ChgNote{ SI99-0013-1 }
@Comment{@LabeledAddedClause{Version=[2],Name=[function Interface_Kind]}}
@LabeledAddedSubClause{Version=[2],Name=[function Interface_Kind]}
@begin{Discussion}
@ChgAdded{Version=[2],Text=[Can't use a real clause for now, as that would
change all of the following clause numbers, so this is a subclause temporarily.]}
@end{Discussion}


@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0013-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Interface_Kind}
     (Definition : @key[in] Asis.Definition)
      @key[return] Asis.Interface_Kinds;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0013-1]}
@ChgAdded{Version=[2],Text=[Definition specifies the Definition to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Returns the Interface_Kinds value of the
Definition.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Returns Not_An_Interface for any unexpected element
such as a Nil_Element, A_Statement, or A_Declaration.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0013-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Definition expects
an element that has the following Definition_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[A_Type_Definition that has one of the following Type_Kinds:
   An_Interface_Type_Definition
A_Formal_Type_Definition that has the following Formal_Type_Kinds:
   A_Formal_Interface_Type_Definition]}
@end{Display}
@end{DescribeCode}


@LabeledClause{function Root_Type_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Root_Type_Kind}
               (Definition : @key[in] Asis.Root_Type_Definition)
                @key[return] Asis.Root_Type_Kinds;
@end{Example}

Definition @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
Root_Type_Definition to query.

Returns the Root_Type_Kinds value of the Definition.

Returns Not_A_Root_Type_Definition for any unexpected element such as a
Nil_Element, A_Statement, or A_Declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Definition expects an element
that has the following],Old=[Expected]} Type_Kinds:
@begin{Display}
A_Root_Type_Definition
@end{Display}
@end{DescribeCode}


@LabeledClause{function Constraint_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Constraint_Kind}
               (Definition : @key[in] Asis.Constraint)
                @key[return] Asis.Constraint_Kinds;
@end{Example}

Definition @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
constraint to query.

Returns the Constraint_Kinds value of the Definition.

Returns Not_A_Constraint for any unexpected element such as a
Nil_Element, A_Statement, or A_Declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Definition expects an element
that has the following],Old=[Expected]} Definition_Kinds:
@begin{Display}
A_Constraint
@end{Display}
@end{DescribeCode}


@LabeledClause{function Discrete_Range_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Discrete_Range_Kind}
               (Definition : @key[in] Asis.Discrete_Range)
                @key[return] Asis.Discrete_Range_Kinds;
@end{Example}

Definition @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
discrete_range to query.

Returns the Discrete_Range_Kinds value of the Definition.

Returns Not_A_Discrete_Range for any unexpected element such as a
Nil_Element, A_Statement, or A_Declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Definition expects an element
that has one of the following],Old=[Appropriate]} Definition_Kinds:
@begin{Display}
A_Discrete_Subtype_Definition
A_Discrete_Range
@end{Display}
@end{DescribeCode}


@LabeledClause{function Expression_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Expression_Kind} (Expression : @key[in] Asis.Expression)
                           @key[return] Asis.Expression_Kinds;
@end{Example}

Expression @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
Expression to query.

Returns the Expression_Kinds value of the Expression.

Returns Not_An_Expression for any unexpected element such as a
Nil_Element, A_Statement, or A_Declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Expression expects an element
that has the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
An_Expression
@end{Display}
@end{DescribeCode}


@LabeledClause{function Operator_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Operator_Kind} (Element : @key[in] Asis.Element)
                         @key[return] Asis.Operator_Kinds;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the Element to
query.

Returns the Operator_Kinds value of the A_Defining_Name or An_Expression
element.

Returns Not_An_Operator for any unexpected element such as a
Nil_Element, A_Statement, or A_Declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Element expects an element
that has the following],Old=[Appropriate]} Defining_Name_Kinds:
@begin{Display}
A_Defining_Operator_Symbol
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[or that has the following],
Old=[Expected]} Expression_Kinds:
@begin{Display}
An_Operator_Symbol
@end{Display}
@end{DescribeCode}


@LabeledClause{function Attribute_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Attribute_Kind} (Expression : @key[in] Asis.Expression)
                          @key[return] Asis.Attribute_Kinds;
@end{Example}

Expression @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the
Expression to query.

Returns the Attribute_Kinds value of the Expression.

Returns Not_An_Attribute for any unexpected element such as a
Nil_Element, A_Statement, or A_Declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Expression expects an element
that has the following],Old=[Appropriate]} Expression_Kinds:
@begin{Display}
An_Attribute_Reference
@end{Display}
@end{DescribeCode}


@LabeledClause{function Association_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Association_Kind} (Association : @key[in] Asis.Association)
                            @key[return] Asis.Association_Kinds;
@end{Example}

Association @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
Association to query.

Returns the Association_Kinds value of the Association.

Returns Not_An_Association for any unexpected element such as a
Nil_Element, A_Statement, or A_Declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Association expects an element
that has the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
An_Association
@end{Display}
@end{DescribeCode}


@LabeledClause{function Statement_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Statement_Kind} (Statement : @key[in] Asis.Statement)
                          @key[return] Asis.Statement_Kinds;
@end{Example}

Statement @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the element
to query.

Returns the Statement_Kinds value of the statement.

Returns Not_A_Statement for any unexpected element such as a
Nil_Element, A_Definition, or A_Declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Statement expects an element
that has the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Statement
@end{Display}
@end{DescribeCode}


@LabeledClause{function Path_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Path_Kind} (Path : @key[in] Asis.Path) @key[return] Asis.Path_Kinds;
@end{Example}

Path @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the Path to query.

Returns the Path_Kinds value of the Path.

Returns Not_A_Path for any unexpected element such as a
Nil_Element, A_Statement, or A_Declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Path expects an element
that has the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Path
@end{Display}
@end{DescribeCode}


@LabeledClause{function Clause_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Clause_Kind} (Clause : @key[in] Asis.Clause) @key[return] Asis.Clause_Kinds;
@end{Example}

Clause @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the element to
query.

Returns the Clause_Kinds value of the Clause.

Returns Not_A_Clause for any unexpected element such as a
Nil_Element, A_Definition, or A_Declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Clause expects an element
that has the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Clause
@end{Display}
@end{DescribeCode}


@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0039-1]}
@LabeledRevisedClause{Version=[2],New=[function Aspect_Clause_Kind],Old=[function Representation_Clause_Kind]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0039-1]}
@key[function] @Chg{Version=[2],New=[@AdaSubDefn{Aspect_Clause_Kind}],Old=[@AdaSubDefn{Representation_Clause_Kind}]}
               (Clause : @key[in] @Chg{Version=[2],New=[Asis.Aspect_Clause],Old=[Asis.Representation_Clause]})
                @key[return] @Chg{Version=[2],New=[Asis.Aspect_Clause_Kinds],Old=[Asis.Representation_Clause_Kinds]};
@end{Example}

Clause @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the element to
query.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0039-1]}
Returns the @Chg{Version=[2],New=[Aspect_Clause_Kinds],Old=[Representation_Clause_Kinds]} value of the Clause.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0039-1]}
Returns
@Chg{Version=[2],New=[Not_An_Aspect_Clause],Old=[Not_A_Representation_Clause]}
for any unexpected element such as a Nil_Element, A_Definition, or
A_Declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Clause expects an element
that has the following],Old=[Appropriate]} Clause_Kinds:
@begin{Display}
@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0039-1]}
@Chg{Version=[2],New=[An_Aspect_Clause],Old=[A_Representation_Clause]}
@end{Display}
@end{DescribeCode}


@LabeledClause{function Is_Nil (element)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Nil} (Right : @key[in] Asis.Element) @key[return] Boolean;
@end{Example}

Right @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the element to
check.

Returns True if the program element is the Nil_Element.
@end{DescribeCode}


@LabeledClause{function Is_Nil (element list)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Nil} (Right : @key[in] Asis.Element_List) @key[return] Boolean;
@end{Example}

Right @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the element list
to check.

Returns True if the element list has a length of zero.
@end{DescribeCode}


@LabeledClause{function Is_Equal (element)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Equal} (Left  : @key[in] Asis.Element;
                     Right : @key[in] Asis.Element) @key[return] Boolean;
@end{Example}

Left @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the left element
to compare. Right @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
right element to compare.

Returns True if Left and Right represent the same physical element,
from the same physical compilation unit. The two elements may or
may not be from the same open ASIS Context variable.

@leading@keepnext@Chg{Version=[1],New=[A True result implies],Old=[Implies]}:
@begin{ChildExample}
Is_Equal (Enclosing_Compilation_Unit (Left),
          Enclosing_Compilation_Unit (Right)) = True
@end{ChildExample}
@end{DescribeCode}


@LabeledClause{function Is_Identical (element)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Identical} (Left  : @key[in] Asis.Element;
                         Right : @key[in] Asis.Element) @key[return] Boolean;
@end{Example}

Left @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the left element.
Right @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the right
element.

Returns True if Left and Right represent the same physical element,
from the same physical compilation unit, from the same open ASIS
Context variable.

@leading@keepnext@Chg{Version=[1],New=[A True result implies],Old=[Implies]}:
@begin{ChildExample}
Is_Identical (Enclosing_Compilation_Unit (Left),
              Enclosing_Compilation_Unit (Right)) = True
@end{ChildExample}
@end{DescribeCode}


@LabeledClause{function Is_Part_Of_Implicit}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Part_Of_Implicit} (Element : @key[in] Asis.Element) @key[return] Boolean;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element to
query.

Returns True for any Element that is, or that forms part of, any
implicitly declared or specified program Element structure.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
Returns True for any implicit generic child unit specifications or their
subcomponents. @Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} 10.1.1(19).

Returns False for a Nil_Element, or any Element that correspond to text
which was specified explicitly (typed, entered, written).

Generic instance specifications and bodies, while implicit, are treated
as a special case. These elements will not normally test as
Is_Part_Of_Implicit. Rather, they are Is_Part_Of_Instance. They only test
as Is_Part_Of_Implicit if one of the following rules applies. This is
done so that it is possible to determine whether a declaration, which
happens to occur within an instance, is an implicit result of
another declaration which occurs explicitly within the generic template.

@leading@;Implicit Elements are those that represent these portions of the Ada
language:

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} 4.5.(9)

@begin{InnerItemize}
All predefined operator declarations and their component elements are
Is_Part_Of_Implicit
@end{InnerItemize}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} 3.4(16)

@begin{InnerItemize}
Implicit predefined operators of the derived type.
@end{InnerItemize}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} 3.4(17-22)

@begin{InnerItemize}
Implicit inherited subprogram declarations and their component elements are
 Is_Part_Of_Implicit
@end{InnerItemize}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} 6.4(9) and 12.3(7)

@begin{InnerItemize}
Implicit actual parameter expressions (defaults).

The A_Parameter_Association that includes a defaulted parameter value
Is_Normalized and also Is_Part_Of_Implicit. The Formal_Parameter and the
Actual_Parameter values from such Associations are not Is_Part_Of_Implicit
unless they are from default initializations for an inherited subprogram
declaration and have an Enclosing_Element that is the parameter specification
of the subprogram declaration. (Those elements are shared with (were created
by) the original subprogram declaration, or they are naming expressions
representing the actual generic subprogram selected at the place of an
instantiation for A_Box_Default.)

All A_Parameter_Association Kinds from a Normalized list are
Is_Part_Of_Implicit.
@end{InnerItemize}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} 6.6 (6)

@begin{InnerItemize}
Inequality operator declarations for limited private types are
Is_Part_Of_Implicit.

Depending on the ASIS implementation, a "/=" appearing in the compilation may
result in a @key[not] and an "=" in the internal representation. These two elements
test as Is_Part_Of_Implicit because they do not represent text from the
original compilation text.
@end{InnerItemize}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} 12.3 (16)

@begin{InnerItemize}
implicit generic instance specifications and bodies are not
Is_Part_Of_Implicit; they are Is_Part_Of_Instance and are only implicit if some
other rule makes them so.
@end{InnerItemize}
@end{Itemize}
@end{DescribeCode}


@LabeledClause{function Is_Part_Of_Inherited}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Part_Of_Inherited} (Element : @key[in] Asis.Element) @key[return] Boolean;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element to query.

Returns True for any Element that is, or that forms part of, an
inherited primitive subprogram declaration.

Returns False for any other Element including a Nil_Element.
@end{DescribeCode}


@LabeledClause{function Is_Part_Of_Instance}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Part_Of_Instance} (Element : @key[in] Asis.Element) @key[return] Boolean;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element to
test.

Returns True if the Element is part of an implicit generic specification
instance or an implicit generic body instance.

Returns False for explicit, inherited, and predefined Elements that are
not the result of a generic expansion.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
Returns False for any implicit generic child unit specifications or
their subcomponents. @Chg{Version=[2],New=[Ada Standard],Old=[Reference
Manual]} 10.1.1(19).

Returns False for a Nil_Element.

Instantiations are not themselves Is_Part_Of_Instance unless they are
encountered while traversing a generic instance.
@end{DescribeCode}


@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0007-1]}
@Comment{@LabeledAddedClause{Version=[2],Name=[function Is_Prefix_Notation]}}
@LabeledAddedSubClause{Version=[2],Name=[function Is_Prefix_Notation]}}
@begin{Discussion}
@ChgAdded{Version=[2],Text=[Can't use a real clause for now, as that would
change all of the following clause numbers, so this is a subclause temporarily.]}
@end{Discussion}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0012-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Prefix_Notation} (Call : @key[in] Asis.Element) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0007-1]}
@ChgAdded{Version=[2],Text=[Call specifies the call to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0007-1]}
@ChgAdded{Version=[2],Text=[Returns True if the prefix of the call is a prefixed view of the
subprogram (see Ada Standard 4.1.3(9.2/2)).]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0007-1]}
@ChgAdded{Version=[2],Text=[Call expects an element that has the following Statement_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[A_Procedure_Call_Statement]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0007-1]}
@ChgAdded{Version=[2],Text=[or an element that has the following Expression_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[A_Function_Call]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0007-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status of Value_Error for any
element that does not have one of these expected kinds.]}
@end{DescribeCode}



@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0012-1]}
@Comment{@LabeledAddedClause{Version=[2],Name=[function Is_Null_Procedure]}}
@LabeledAddedSubClause{Version=[2],Name=[function Is_Null_Procedure]}
@begin{Discussion}
@ChgAdded{Version=[2],Text=[Can't use a real clause for now, as that would
change all of the following clause numbers, so this is a subclause temporarily.]}
@end{Discussion}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0012-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Null_Procedure}
     (Element : @key[in] Asis.Element)
      @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0012-1]}
@ChgAdded{Version=[2],Text=[Element specifies the element to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Returns True for a declaration of a procedure or
formal procedure that is declared as null.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Returns False for any other Element including a
Nil_Element.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0012-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Element expects an
element that has the following Element_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[A_Declaration]}
@end{Display}
@ChgAdded{Version=[2],Text=[that has one of the following Declaration_Kinds:]}
@begin{Display}@ChgAdded{Version=[2], Text=[A_Procedure_Declaration
A_Formal_Procedure_Declaration]}
@end{Display}
@end{DescribeCode}

@begin{UsageNote}
@ChgAdded{Version=[2],Text=[This routine tests for the syntactic element
null_procedure_declaration; calling Is_Null_Procedure on a renaming of a null
procedure will return False. Use the routine ASIS.Callable_Views.Is_Null to
determine if a declaration is semantically a null procedure (including
renames).]}
@end{UsageNote}

@begin{Discussion}
@ChgAdded{Version=[2],Text=[A generic procedure cannot be null, while generic
formal procedures can be null.]}
@end{Discussion}



@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0012-1]}
@Comment{@LabeledAddedClause{Version=[2],Name=[function Is_Abstract_Subprogram]}}
@LabeledAddedSubClause{Version=[2],Name=[function Is_Abstract_Subprogram]}
@begin{Discussion}
@ChgAdded{Version=[2],Text=[Can't use a real clause for now, as that would
change all of the following clause numbers, so this is a subclause temporarily.]}
@end{Discussion}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0012-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Is_Abstract_Subprogram}
     (Element : @key[in] Asis.Element)
      @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0012-1]}
@ChgAdded{Version=[2],Text=[Element specifies the element to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Returns True for a declaration of a subprogram or
formal subprogram that is declared as abstract.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Returns False for any other Element including a
Nil_Element.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0012-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Element expects an
element that has the following Element_Kinds:]}
@begin{Display}@ChgAdded{Version=[2], Text=[A_Declaration]}
@end{Display}
@ChgAdded{Version=[2],Text=[that has one of the following Declaration_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2], Text=[A_Procedure_Declaration
A_Function_Declaration
A_Formal_Procedure_Declaration
A_Formal_Function_Declaration]}
@end{Display}
@end{DescribeCode}

@begin{UsageNote}
@ChgAdded{Version=[2],Text=[This routine tests for the syntactic element
abstract_subprogram_declaration; calling Is_Abstract_Subprogram on a renaming
of an abstract subprogram will return False. Use the routine
ASIS.Callable_Views.Is_Abstract to determine if a declaration is semantically
an abstract subprogram (including renames).]}
@end{UsageNote}


@LabeledClause{function Enclosing_Element}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Enclosing_Element} (Element : @key[in] Asis.Element) @key[return] Asis.Element;

@key[function] @AdaSubDefn{Enclosing_Element} (Element                    : @key[in] Asis.Element;
                            Expected_Enclosing_Element : @key[in] Asis.Element)
                        @key[return] Asis.Element;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element to query.
Expected_Enclosing_Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} an enclosing element expected to
contain the element.

Returns the Element that immediately encloses the given element. This
query is intended to exactly reverse any single parent-to-child element
traversal. For any structural query that returns a subcomponent of an
element (or that returns a list of subcomponent elements), the original element
can be determined by passing the subcomponent element to this query.

@begin{UsageNote}
Semantic queries (queries that test the meaning of a program rather
than its structure) return Elements that usually do not have the original
argument Element as their parent.
@end{UsageNote}

@leading@keepnext@;Returns a Nil_Element if:
@begin{Itemize}
the element is the declaration part of a compilation unit
(Unit_Declaration).

the element is with clause or use clause of a context clause
(Context_Clause_Elements).

the element is a pragma for a compilation unit
(Compilation_Pragmas and Context_Clause_Elements).
@end{Itemize}

Use Enclosing_Compilation_Unit to get the enclosing compilation unit for
any element value other than Nil_Element.

Raises ASIS_Inappropriate_Element if the Element is a Nil_Element.
@end{DescribeCode}

@begin{Examples}
@begin{Itemize}
Given a A_Declaration/A_Full_Type_Declaration in the declarative region
of a block statement, returns the A_Statement/A_Block_Statement Element
that encloses the type declaration.

Given A_Statement, from the sequence of statements within a loop
statement, returns the enclosing A_Statement/A_Loop_Statement.

Given the An_Expression/An_Identifier selector from an expanded name,
returns the An_Expression/A_Selected_Component that represents the
combination of the prefix, the dot, and the selector.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
Given the A_Declaration corresponding to the implicit redeclaration of
a child generic for an instantiated parent generic, returns the expanded
generic specific template from the parent generic instantiation
corresponding to any implicit generic child unit specification given as
an argument. @Chg{Version=[2],
New=[Ada Standard],Old=[Reference Manual]} 10.1.1(19).
@end{Itemize}
@end{Examples}

@begin{UsageNote}
The optional Expected_Enclosing_Element parameter is used only to optimize
this query. This speed up is only present for ASIS implementations
where the underlying implementor's environment does not have "parent
pointers". For these implementations, this query is implemented as a
"search". The Enclosing_Compilation_Unit is searched for the argument
Element. The Expected_Enclosing_Element parameter provides a means of
shortening the search.
Note: If the argument Element is not a sub-element of the
Expected_Enclosing_Element parameter, or if the
Expected_Enclosing_Element is a Nil_Element, the result of the
call is a Nil_Element.

Implementations that do not require the Expected_Enclosing_Element
parameter may ignore it. They are encouraged, but not required, to test
the Expected_Enclosing_Element parameter and to determine if it is an
invalid Element value (its associated Environment Context may be closed)

Portable applications should not use the Expected_Enclosing_Element
parameter since it can lead to unexpected differences when porting an
application between ASIS implementations where one implementation uses
the parameter and the other implementation does not. Passing a "wrong"
Expected_Enclosing_Element to an implementation that ignores it, is
harmless. Passing a "wrong" Expected_Enclosing_Element to an
implementation that may utilize it, can lead to an unexpected
Nil_Element result.
@end{UsageNote}


@LabeledClause{function Pragmas}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Pragmas} (The_Element : @key[in] Asis.Element)
                   @key[return] Asis.Pragma_Element_List;
@end{Example}

The_Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element
to query.

Returns the list of pragmas, in their order of appearance, that appear
directly within the given The_Element. Returns only those pragmas that are
immediate component elements of the given The_Element. Pragmas embedded
within other component elements are not returned. For example, returns
the pragmas in a package specification, in the statement list of a loop,
or in a record component list.

This query returns exactly those pragmas that are returned by the
various queries, that accept these same argument kinds, and that
return Declaration_List and Statement_List, where the inclusion of
Pragmas is controlled by an Include_Pragmas parameter.

Returns a Nil_Element_List if there are no pragmas.

@b{@i{We ought to get rid of the two column format here, because it
formats horribly, especially in HTML. Any ideas?? (I tried a table, but
that was worse; tabs don't appear in HTML) - RLB}}@*

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[The_Element expects an element
that has one of the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
@tabset{P34}
A_Path@\(pragmas from the statement list + pragmas immediately preceding the reserved word @key[when] of the first alternative)
An_Exception_Handler@\(pragmas from the statement list + pragmas immediately preceding the reserved word @key[when] of the first exception handler)
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[The_Element expects an element
that has one of the following],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
@tabset{P34}
A_Procedure_Body_Declaration@\(pragmas from declarative region + statements)
A_Function_Body_Declaration@\(pragmas from declarative region + statements)
A_Package_Declaration@\(pragmas from visible + private declarative regions)
A_Package_Body_Declaration@\(pragmas from declarative region + statements)
A_Task_Body_Declaration@\(pragmas from declarative region + statements)
A_Protected_Body_Declaration@\(pragmas from declarative region)
An_Entry_Body_Declaration@\(pragmas from declarative region + statements)
A_Generic_Procedure_Declaration@\(pragmas from formal declarative region)
A_Generic_Function_Declaration@\(pragmas from formal declarative region)
A_Generic_Package_Declaration@\(pragmas from formal + visible + private declarative regions)
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[or an element
that has one of the following],Old=[Appropriate]} Definition_Kinds:
@begin{Display}
@tabset{P34}
A_Record_Definition@\(pragmas from the component list)
A_Variant_Part@\(pragmas immediately preceding the first reserved word @key[when] + between variants)
A_Variant@\(pragmas from the component list)
A_Task_Definition@\(pragmas from visible + private declarative regions)
A_Protected_Definition@\(pragmas from visible + private declarative regions)
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[or an element
that has one of the following],Old=[Appropriate]} Statement_Kinds:
@begin{Display}
@tabset{P34}
A_Loop_Statement@\(pragmas from statement list)
A_While_Loop_Statement@\(pragmas from statement list)
A_For_Loop_Statement@\(pragmas from statement list)
A_Block_Statement@\(pragmas from declarative region + statements)
An_Accept_Statement@\(pragmas from statement list)
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1],ARef=[SI99-0039-1]}
@leading@keepnext@;@Chg{Version=[2],New=[or an element
that has the following],Old=[Appropriate]} @Chg{Version=[2],New=[Aspect_Clause_Kinds],Old=[Representation_Clause_Kinds]}:
@comment{@table{Columns=[2],Alignment=[Allleft],FirstColWidth=[1],LastColWidth=[2],
NoBreak=[F],Border=[F],SmallSize=[F],Caption=[],Headers=[],
Body=[]}}
@begin{Display}
@tabset{P34}
A_Record_Representation_Clause@\(pragmas from component specifications)
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}


@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element that
has the following],Old=[]} Element_Kinds:
@begin{Display}
A_Pragma
@end{Display}
@end{DescribeCode}


@LabeledRevisedClause{Version=[2],New=[function Corresponding_Aspect_Pragmas],Old=[function Corresponding_Pragmas]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0021-1]}
@key[function] @Chg{Version=[2],New=[@AdaSubDefn{Corresponding_Aspect_Pragmas}],Old=[@AdaSubDefn{Corresponding_Pragmas}]} (@Chg{Version=[2],New=[Defining_Name],Old=[Element]} : @key[in] New=[Asis.Defining_Name],Old=[Asis.Element]})
                                 @key[return] Asis.Pragma_Element_List;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0021-1]}
@Chg{Version=[2],New=[Defining_Name specifies the defining name],Old=[Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element]}
to query.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0021-1]}
Returns @Chg{Version=[2],New=[all aspect pragma elements that apply to the
named entity],Old=[Returns the list of pragmas semantically associated with the given element,
in their order of appearance, or, in any order that does not affect their
relative interpretations. These are pragmas that directly affect the
given element. For example, a pragma Pack affects the type it names]}.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0021-1]}
@ChgAdded{Version=[2],Text=[The pragmas returned may be the pragmas applying to
a parent type if the name refers to type which is a derived type with no
explicit aspect pragma. These pragmas are not Is_Part_Of_Implicit, they are the
A_Pragma elements specified in conjunction with the declaration of the parent
type.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0021-1]}
Returns a Nil_Element_List if there are no @Chg{Version=[2],New=[aspect
pragmas that apply to the named entity],Old=[semantically associated pragmas]}.

@begin{UsageNote}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0021-1]}
@ChgDeleted{Version=[2],Text=[If the argument is a inherited entry declaration
from a derived task type, all pragmas returned are elements taken from the
original task type's declarative item list. Their Enclosing_Element is the
original type definition and not the derived type definition.]}
@end{UsageNote}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0021-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Defining_Name expects an element that
has the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
@Chg{Version=[2],New=[A_Defining_Name],Old=[A_Declaration
A_Statement]}
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0021-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[a list of elements that
each have],Old=[]} Element_Kinds:
@begin{Display}
A_Pragma]
@end{Display}
@end{DescribeCode}


@LabeledClause{function Pragma_Name_Image}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Pragma_Name_Image}
          (Pragma_Element : @key[in] Asis.Pragma_Element) @key[return] Program_Text;
@end{Example}

Pragma_Element @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the
element to query.

Returns the program text image of the simple name of the pragma.

The case of names returned by this query may vary between implementors.
Implementors are encouraged, but not required, to return names in the
same case as was used in the original compilation text.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Pragma_Element expects an element
that has the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Pragma
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@end{DescribeCode}


@LabeledClause{function Pragma_Argument_Associations}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Pragma_Argument_Associations}
          (Pragma_Element : @key[in] Asis.Pragma_Element)
              @key[return] Asis.Association_List;
@end{Example}

Pragma_Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
element to query.

Returns a list of the Pragma_Argument_Associations of the pragma, in their
order of appearance.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Pragma_Element expects an element
that has the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Pragma
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}


@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element that
has the following],Old=[]} Element_Kinds:
@begin{Display}
A_Pragma_Argument_Association
@end{Display}
@end{DescribeCode}


@LabeledClause{function Debug_Image (element)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Debug_Image} (Element : @key[in] Asis.Element) @key[return] Wide_String;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the program
element to convert.

Returns a string value containing implementation-defined debug
information associated with the element.

The return value uses Asis.Text.Delimiter_Image to separate the lines
of multi-line results. The return value does not end with
Asis.Text.Delimiter_Image.

These values are intended for two purposes. They are suitable for
inclusion in problem reports sent to the ASIS implementor. They can
be presumed to contain information useful when debugging the implementation
itself. They are also suitable for use by the ASIS application when
printing simple application debugging messages during application
development. They are intended to be, to some worthwhile degree,
intelligible to the user.
@end{DescribeCode}


@LabeledClause{function Hash}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Hash} (Element : @key[in] Asis.Element) @key[return] Asis.ASIS_Integer;
@end{Example}

The purpose of the hash function is to provide a convenient name for an
object of type Asis.Element in order to facilitate application defined I/O
and/or other application defined processing.

The hash function maps Asis.Element objects into N discrete classes
("buckets") of objects. A good hash function is uniform across its range.
It is important to note that the distribution of objects in the
application's domain will affect the distribution of the hash function.
A good hash measured against one domain will not necessarily be good when
fed objects from a different set.

A continuous uniform hash can be divided by any N and provide a uniform
distribution of objects to each of the N discrete classes. A hash value is
not unique for each hashed Asis.Element. The application is responsible for
handling name collisions of the hashed value.

The hash function returns a hashed value of type ASIS_Integer. If desired,
a user could easily map ASIS_Integer'Range to any smaller range for the
hash based on application constraints (i.e., the application implementor
can tune the time-space tradeoffs by choosing a small table, implying
slower lookups within each "bucket", or a large table, implying faster
lookups within each "bucket").
@end{DescribeCode}

@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[end] Asis.Elements;]}
@end{Example}



