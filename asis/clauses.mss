@Part(clauses, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/clauses.mss,v $}
@comment{$Revision: 1.6 $ $Date: 2007/10/13 05:33:32 $}

@LabeledSection{package Asis.Clauses}

@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis],Child=[Clauses]}Asis.Clauses
shall exist. The package
shall provide interfaces equivalent to those described in the
following subclauses.],
Old=[@f{@key[package] @ChildUnit{Parent=[Asis],Child=[Clauses]}Asis.Clauses @key[is]}]}

@ChgDeleted{Version=[1],Text=[Asis.Clauses]}

This package encapsulates a set of queries that operate on A_Clause
elements.


@LabeledClause{function Clause_Names}

@begin{ElementRef}
A_Use_Package_Clause @em 8.4@*
A_Use_Type_Clause @em 8.4@*
A_With_Clause @em 10.1.2
@end{ElementRef}
@begin{ChildRef}@ @;
@begin{Display}
function Clause_Names
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Clause_Names} (Clause : @key[in] Asis.Element)
                       @key[return] Asis.Name_List;
@end{Example}

Clause @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the with_clause or use_clause to query.

Returns a list of the names that appear in the given clause.
The names in the list should be in their order of appearance in the
original clauses from the compilation text.

Results of this query may vary across ASIS implementations. Some
implementations normalize all clauses containing multiple names
into an equivalent sequence of corresponding single clauses.
Similarly, an implementation may keep a name only once even though that
name can appear more than once in a clause.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Clause expects an element
of],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Use_Package_Clause
A_Use_Type_Clause
A_With_Clause
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[a list of elements that each have
one of ],Old=[]}Expression_Kinds:
@begin{Display}
An_Identifier
A_Selected_Component
An_Attribute_Reference
@end{Display}
@end{DescribeCode}


@LabeledClause{function Representation_Clause_Name}

@begin{ElementRef}
A_Representation_Clause @em 13.1@*
@*
An_Attribute_Definition_Clause @em 13.3@*
An_Enumeration_Representation_Clause @em 13.4@*
An_At_Clause @em J.7
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Representation_Clause_Name
function Representation_Clause_Expression
@end{Display}
@end{ChildRef}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Representation_Clause_Name} (Clause : @key[in] Asis.Clause)
                                     @key[return] Asis.Name;
@end{Example}

Clause @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the
representation_clause to query.

Returns the direct_name expression following the reserved word @key[for].

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Clause expects an element
of],Old=[Appropriate]} Clause_Kinds:
@begin{Display}
A_Representation_Clause
A_Component_Clause
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Expression_Kinds:
@begin{Display}
An_Identifier
An_Attribute_Reference
@end{Display}
@end{DescribeCode}


@LabeledClause{function Representation_Clause_Expression}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Representation_Clause_Expression}
            (Clause : @key[in] Asis.Representation_Clause)
             @key[return] Asis.Expression;
@end{Example}

Clause @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the representation_clause to query.

Returns the expression following the reserved word @key[use] or the reserved
words @key[use at].

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Clause expects an element
of],Old=[Appropriate]} Representation_Clause_Kinds:
@begin{Display}
An_Attribute_Definition_Clause
An_Enumeration_Representation_Clause
An_At_Clause
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Element_Kinds:
@begin{Display}
An_Expression
@end{Display}
@end{DescribeCode}


@LabeledClause{function Mod_Clause_Expression}


@begin{ElementRef}
A_Record_Representation_Clause @em 13.5.1
@end{ElementRef}
@begin{ChildRef}@ @;
@begin{Display}
function Representation_Clause_Name
function Mod_Clause_Expression
function Component_Clauses
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Mod_Clause_Expression} (Clause : @key[in] Asis.Representation_Clause)
                             @key[return] Asis.Expression;
@end{Example}

Clause @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the record representation clause to query.

Returns the static_expression appearing after the reserved words @key[at mod].

Returns a Nil_Element if a mod_clause is not present.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Clause expects an element
of],Old=[Appropriate]} Representation_Clause_Kinds:
@begin{Display}
A_Record_Representation_Clause
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Element_Kinds:
@begin{Display}
Not_An_Element
An_Expression
@end{Display}
@end{DescribeCode}


@LabeledClause{function Component_Clauses}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Component_Clauses} (Clause : @key[in] Asis.Representation_Clause;
                            Include_Pragmas : @key[in] Boolean := False)
                            @key[return] Asis.Component_Clause_List;
@end{Example}

Clause @Chg{Version=[1],New=[specifies],Old=[         @en Specifies]} the
record representation clause to query.
Include_Pragmas @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether
pragmas are to be returned.

Returns the component_clause and pragma elements from the
record_representation_clause, in their order of appearance.

Returns a Nil_Element_List if the record_representation_clause has no
component_clause or pragma elements.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Clause expects an element
of],Old=[Appropriate]} Representation_Clause_Kinds:
@begin{Display}
A_Record_Representation_Clause
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[a list of elements that each have
one of ],Old=[]}Element_Kinds:
@begin{Display}
A_Clause@Chg{Version=[2],New=[ @em the returned element also has one of Clause_Kinds:],Old=[]}
@Chg{Version=[2],New=[   A_Component_Clause],Old=[]}
A_Pragma
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Type=[Leading],Keepnext=[T],Text=[Returns Clause_Kinds:]}
@begin{Display}
@ChgDeleted{Version=[2],Text=[A_Component_Clause]}
@end{Display}
@end{DescribeCode}


@LabeledClause{function Component_Clause_Position}

@begin{ElementRef}
A_Component_Clause @em 13.5.1
@end{ElementRef}
@begin{ChildRef}@ @;
@begin{Display}
function Representation_Clause_Name
function Component_Clause_Position
function Component_Clause_Range
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Component_Clause_Position} (Clause : @key[in] Asis.Component_Clause)
                                    @key[return] Asis.Expression;
@end{Example}

Clause @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the component_clause to query.

Returns the position expression for the component_clause.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Clause expects an element
of],Old=[Appropriate]} Clause_Kinds:
@begin{Display}
A_Component_Clause
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Element_Kinds:
@begin{Display}
An_Expression
@end{Display}
@end{DescribeCode}


@LabeledClause{function Component_Clause_Range}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Component_Clause_Range} (Clause : @key[in] Asis.Component_Clause)
                                 @key[return] Asis.Discrete_Range;
@end{Example}

Clause @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the component_clause to query.

Returns the first_bit .. last_bit range for the component_clause.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Clause expects an element
of],Old=[Appropriate]} Clause_Kinds:
@begin{Display}
A_Component_Clause
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Discrete_Range_Kinds:
@begin{Display}
A_Discrete_Simple_Expression_Range
@end{Display}
@end{DescribeCode}

@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[end] Asis.Clauses;]}
@end{Example}

