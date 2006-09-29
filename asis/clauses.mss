@Part(clauses, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/clauses.mss,v $}
@comment{$Revision: 1.1 $ $Date: 2006/09/28 05:11:58 $}

@LabeledSection{package Asis.Clauses}

@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis],Child=[Clauses]}Asis.Clauses
shall exist. The package
shall provide interfaces equivalent to those described in the
following subclauses.],
Old=[@f{@key[package] @ChildUnit{Parent=[Asis],Child=[Clauses]}Asis.Clauses @key[is]}]}

@ChgDeleted{Version=[1],Text=[Asis.Clauses]}

This package encapsulates a set of queries that operate on A_Clause
elements.
Element Reference -A_Use_Package_Clause - 8.4
Element Reference -A_Use_Type_Clause    - 8.4
Element Reference -A_With_Clause        - 10.1.2

Child Elements returned by
   function Clause_Names


@LabeledClause{function Clause_Names}


    @key[function] @AdaSubDefn{Clause_Names} (Clause : @key[in] Asis.Element)
                           @key[return] Asis.Name_List;

Clause @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the with_clause or use_clause to query.

Returns a list of the names that appear in the given clause.
The names in the list should be in their order of appearance in the
original clauses from the compilation text.

Results of this query may vary across ASIS implementations. Some
implementations normalize all clauses containing multiple names
into an equivalent sequence of corresponding single clauses.
Similarly, an implementation may keep a name only once even though that
name can appear more than once in a clause.

Appropriate Element_Kinds:
     A_Use_Package_Clause
     A_Use_Type_Clause
     A_With_Clause

Returns Expression_Kinds:
     An_Identifier
     A_Selected_Component
     An_Attribute_Reference
Element Reference -A_Representation_Clause - 13.1

Element Reference -An_Attribute_Definition_Clause - 13.3
Element Reference -An_Enumeration_Representation_Clause - 13.4
Element Reference -An_At_Clause - J.7

Child Elements returned by
   function Representation_Clause_Name
   function Representation_Clause_Expression

@LabeledClause{function Representation_Clause_Name}


    @key[function] @AdaSubDefn{Representation_Clause_Name} (Clause : @key[in] Asis.Clause)
                                         @key[return] Asis.Name;

Clause  @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the representation_clause to query.

Returns the direct_name expression following the reserved word "for".

Appropriate Clause_Kinds:
     A_Representation_Clause
     A_Component_Clause

Returns Expression_Kinds:
     An_Identifier
     An_Attribute_Reference

@LabeledClause{function Representation_Clause_Expression}


    @key[function] @AdaSubDefn{Representation_Clause_Expression}
                (Clause : @key[in] Asis.Representation_Clause)
                 @key[return] Asis.Expression;

Clause  @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the representation_clause to query.

Returns the expression following the reserved word “use” or the reserved
words “use at”.

Appropriate Representation_Clause_Kinds:
     An_Attribute_Definition_Clause
     An_Enumeration_Representation_Clause
     An_At_Clause

Returns Element_Kinds:
     An_Expression
Element Reference -A_Record_Representation_Clause - 13.5.1

Child Elements returned by
   function Representation_Clause_Name
   function Mod_Clause_Expression
   function Component_Clauses

@LabeledClause{function Mod_Clause_Expression}


    @key[function] @AdaSubDefn{Mod_Clause_Expression} (Clause : @key[in] Asis.Representation_Clause)
                                 @key[return] Asis.Expression;

Clause  @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the record representation clause to query.

Returns the static_expression appearing after the reserved words "at mod".

Returns a Nil_Element if a mod_clause is not present.

Appropriate Representation_Clause_Kinds:
     A_Record_Representation_Clause

Returns Element_Kinds:
     Not_An_Element
     An_Expression

@LabeledClause{function Component_Clauses}


    @key[function] @AdaSubDefn{Component_Clauses} (Clause : @key[in] Asis.Representation_Clause;
                                Include_Pragmas : @key[in] Boolean := False)
                                @key[return] Asis.Component_Clause_List;

Clause          @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the record representation clause to query.
Include_Pragmas @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether pragmas are to be returned.

Returns the component_clause and pragma elements from the
record_representation_clause, in their order of appearance.

Returns a Nil_Element_List if the record_representation_clause has no
component_clause or pragma elements.

Appropriate Representation_Clause_Kinds:
     A_Record_Representation_Clause

Returns Element_Kinds:
     A_Clause
     A_Pragma

Returns Clause_Kinds:
     A_Component_Clause
Element Reference -A_Component_Clause - 13.5.1

Child Elements returned by
   function Representation_Clause_Name
   function Component_Clause_Position
   function Component_Clause_Range

@LabeledClause{function Component_Clause_Position}


    @key[function] @AdaSubDefn{Component_Clause_Position} (Clause : @key[in] Asis.Component_Clause)
                                        @key[return] Asis.Expression;

Clause  @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the component_clause to query.

Returns the position expression for the component_clause.

Appropriate Clause_Kinds:
     A_Component_Clause

Returns Element_Kinds:
     An_Expression

@LabeledClause{function Component_Clause_Range}


    @key[function] @AdaSubDefn{Component_Clause_Range} (Clause : @key[in] Asis.Component_Clause)
                                     @key[return] Asis.Discrete_Range;

Clause  @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the component_clause to query.

Returns the first_bit .. last_bit range for the component_clause.

Appropriate Clause_Kinds:
     A_Component_Clause

Returns Discrete_Range_Kinds:
     A_Discrete_Simple_Expression_Range

@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[end] Asis.Clauses;]}
@end{Example}

