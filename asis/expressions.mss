@Part(expressions, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/expressions.mss,v $}
@comment{$Revision: 1.6 $ $Date: 2007/08/05 01:46:38 $}


@LabeledSection{package Asis.Expressions}


@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis],Child=[Expressions]}Asis.Expressions
shall exist. The package
shall provide interfaces equivalent to those described in the
following subclauses.],
Old=[@f{@key[package] @ChildUnit{Parent=[Asis],Child=[Expressions]}Asis.Expressions @key[is]}]}

Asis.Expressions encapsulates a set of queries that operate on
An_Expression and An_Association elements.


@LabeledClause{function Corresponding_Expression_Type}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Expression_Type} (Expression : @key[in] Asis.Expression)
                         @key[return] Asis.Declaration;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the
expression to query.

@leading@;Returns the type declaration for the type or subtype of the expression.
This query does not "unwind" subtypes or derived types to get to the
Corresponding_First_Subtype or Corresponding_Parent_Subtype declarations.
For example, for the following program text:

@begin{ChildExample}
@key[type] Int @key[is range] -5_000 .. 5_000;
@key[type] My_Int @key[is new] Int;
@key[type] Good_Int @key[is new] My_Int;
Var: Good_Int;
@end{ChildExample}

The type declaration for Good_Int should be returned. The "unwinding" should
not occur. The type declaration for either My_Int or Int should not be returned.

@leading@;Returns a Nil_Element if the argument Expression does not represent
an Ada expression having an Ada type, including the following classes:

@begin{Itemize}
Naming expressions that name packages, subprograms, tasks, etc. These
expressions do have a Corresponding_Name_Definition and a
Corresponding_Name_Declaration. Although task objects do have
a type, this query is limited, on purpose. Thus, when a naming
expression is given to this query (for packages, subprograms,
tasks, etc.), this query will return Nil_Element. @Chg{Version=[1],
New=[If],Old=[As the @b{APPLICATION NOTE} below indicates, if]}
any further information is needed, the element should be queried by
Corresponding_Name_Definition or Corresponding_Name_Declaration,
which should eventually return an A_Task_Type_Declaration element.

When An_Identifier Element representing an attribute designator is passed
as the actual to this query.

The Actual_Parameter Expression from A_Pragma_Argument_Association for a
Pragma may or may not have a Corresponding_Expression_Type.

An_Attribute_Reference Element also may or may not have a
Corresponding_Expression_Type;

An enumeration_aggregate which is a part of enumeration_representation_clause.
@end{Itemize}

Returns a Nil_Element, if the statically determinable type of Expression is a
class-wide type, or the Expression corresponds to an inner sub-aggregate in
multi-dimensional array aggregates.

@begin{UsageNote}
If the returned declaration is Nil, an application should make its own
analysis based on Corresponding_Name_Definition or
Corresponding_Name_Declaration to get more information about the argument,
including the static type resolution for class-wide expressions, if needed.
Use Enclosing_Element to determine if Expression is from pragma argument
association. If for such an expression, Corresponding_Name_Definition
raises ASIS_Failed (with a Status of Value_Error), this An_Expression
element does not represent a normal Ada expression at all and does not
follow normal Ada semantic rules.
For example, @f{@key[pragma] Private_Part (Open => Yes);}, the @f{Yes}
expression may simply be a keyword that is specially recognized by the
implementor's compilation system and may not refer to any
declared object.
@end{UsageNote}

@leading@keepnext@;Appropriate Element_Kinds:
@begin{Display}
An_Expression
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
Not_An_Element
A_Declaration
@end{Display}
@end{DescribeCode}


@LabeledClause{function Value_Image}

@begin{ElementRef}
An_Integer_Literal @em 2.4 @em No child elements@*
A_Real_Literal @em 2.4 @em No child elements@*
A_String_Literal @em 2.6 @em No child elements
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
@ChgAdded{Version=[1],Text=[No child elements]}
@end{Display}
@leading@;A string image returned by:
@begin{Display}
function Value_Image
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Value_Image} (Expression : @key[in] Asis.Expression) @key[return] Wide_String;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the
expression to query.

Returns the string image of the value of the string, integer, or real
literal.

For string literals, Value will return the quotes around the string
literal, these quotes are doubled, just as any quote appearing embedded in
the string literal in the program text.

The form of numbers returned by this query may vary between implementors.
Implementors are encouraged, but not required, to return numeric literals
using the same based or exponent form used in the original compilation text.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
An_Integer_Literal
A_Real_Literal
A_String_Literal
@end{Display}
@end{DescribeCode}


@LabeledClause{function Name_Image}

@begin{ElementRef}
An_Identifier @em 4.1 @em No child elements@*
An_Operator_Symbol @em 4.1 @em No child elements@*
A_Character_Literal @em 4.1 @em No child elements@*
An_Enumeration_Literal @em 4.1 @em No child elements
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
@ChgAdded{Version=[1],Text=[No child elements]}
@end{Display}
@leading@;A string image returned by:
@begin{Display}
function Name_Image
@end{Display}
@leading@;Semantic elements returned by:
@begin{Display}
function Corresponding_Name_Definition
function Corresponding_Name_Definition_List
function Corresponding_Name_Declaration
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Name_Image} (Expression : @key[in] Asis.Expression) @key[return] Program_Text;
@end{Example}

Name @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the name to query.

Returns the program text image of the name.

An_Operator_Symbol elements have names with embedded quotes ""abs""
(function "@key[abs]").

A_Character_Literal elements have names with embedded apostrophes "'x'"
(literal 'x').

An_Enumeration_Literal and An_Identifier elements have identifier names
"Blue" (literal Blue) "Abc" (identifier Abc).

The case of names returned by this query may vary between implementors.
Implementors are encouraged, but not required, to return names in the
same case as was used in the original compilation text.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
An_Identifier
An_Operator_Symbol
A_Character_Literal
An_Enumeration_Literal
@end{Display}
@end{DescribeCode}

@begin{SingleNote}
Implicit subtypes that can be encountered while traversing the
semantic information embedded in implicit inherited subprogram declarations
(Reference Manual 3.4 (17-22)) could have names that are unique in a
particular scope. This is because these subtypes are Is_Part_Of_Implicit
declarations that do not form part of the physical text of the original
compilation units. Some applications may wish to carefully separate the names
of declarations from the names of Is_Part_Of_Implicit declaration when
creating symbol tables and other name-specific lookup mechanisms.
@end{SingleNote}


@LabeledClause{function References}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{References} (Name           : @key[in] Asis.Element;
                     Within_Element : @key[in] Asis.Element;
                     Implicitly     : @key[in] Boolean := False)
                    @key[return] Asis.Name_List;
@end{Example}

Name @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the entity to query.
Within_Element @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the limits
for the query which is limited to the Element and its children.

@leading@;If the Implicitly argument is True:
@begin{Description}
@noprefix@;Returns all usage references of the given entity made by both
explicit and implicit elements within the given limits.
@end{Description}

@leading@;If the Implicitly argument is False:
@begin{Description}
@noprefix@;Returns all usage references of the given entity made only by
explicit elements within the given limits.
@end{Description}

Returned references are in their order of appearance.

@leading@keepnext@;Appropriate Element_Kinds:
@begin{Display}
A_Defining_Name
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
An_Expression
@end{Display}

May raise ASIS_Failed with a Status of Obsolete_Reference_Error if the
argument is part of an inconsistent compilation unit.
@end{DescribeCode}


@LabeledClause{function Is_Referenced}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Referenced} (Name           : @key[in] Asis.Element;
                        Within_Element : @key[in] Asis.Element;
                        Implicitly     : @key[in] Boolean := False)
                       @key[return] Boolean;
@end{Example}

Name @chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the entity to query.
Within_Element @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the limits
for the query which is limited to the Element and its children.

@leading@;If the Implicitly argument is True:
@begin{Description}
@noprefix@;Returns True if the Name is referenced by either implicit or
explicit elements within the given limits.
@end{Description}

@leading@;If the Implicitly argument is False:
@begin{Description}
@noprefix@;Returns True only if the Name is referenced by explicit elements.
@end{Description}

Returns False for any unexpected Element.

@leading@keepnext@;Expected Element_Kinds:
@begin{Display}
A_Defining_Name
@end{Display}

May raise ASIS_Failed with a Status of Obsolete_Reference_Error if the
argument is part of an inconsistent compilation unit.
@end{DescribeCode}


@LabeledClause{function Corresponding_Name_Definition}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Name_Definition} (Reference : @key[in] Asis.Expression)
                         @key[return] Asis.Defining_Name;
@end{Example}

Reference @chg{Version=[1],New=[specifies],Old=[  @en Specifies]} an expression
to query.

Returns the defining_identifier, defining_character_literal,
defining_operator_symbol, or defining_program_unit_name from the
declaration of the referenced entity.

@begin{Itemize}
Record component references return the defining name of the
  record discriminant or component_declaration. For references to inherited
  declarations of derived types, the Corresponding_Name_Definition returns
  the defining name of the implicit inherited declaration.

References to implicit operators and inherited subprograms will return
  an Is_Part_Of_Implicit defining name for the operation. The
  Enclosing_Element of the name is an implicit declaration for the
  operation. The Enclosing_Element of the declaration is the associated
  derived_type_definition.

References to formal parameters given in calls to inherited subprograms
  will return an Is_Part_Of_Implicit defining name for the
  Parameter_Specification from the inherited subprogram specification.

References to visible components of instantiated generic packages will
  return a name from the expanded generic specification instance.

References, within expanded generic instances, that refer to other
  components of the same, or an enclosing, expanded generic instance,
  return a name from the appropriate expanded specification or body
  instance.
@end{Itemize}

In case of renaming, the function returns the new name for the entity.

Returns a Nil_Element if the reference is to an implicitly declared
element for which the implementation does not provide declarations and
defining name elements.

Returns a Nil_Element if the argument is a dispatching call.

The Enclosing_Element of a non-Nil result is either a Declaration or a
Statement.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
An_Identifier
An_Operator_Symbol
A_Character_Literal
An_Enumeration_Literal
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
Not_An_Element
A_Defining_Name
@end{Display}
@end{DescribeCode}

@begin{ImplPerm}

@leading@;An implementation may choose to return any part of multi-part
declarations and definitions. Multi-part declaration/definitions
can occur for:

@begin{Itemize}
Subprogram specification in package specification, package body,
  and subunits (@key[is separate]);

Entries in package specification, package body, and subunits (@key[is separate]);

Private type and full type declarations;

Incomplete type and full type declarations; and

Deferred constant and full constant declarations.
@end{Itemize}

No guarantee is made that the element will be the first part or
that the determination will be made due to any visibility rules.
An application should make its own analysis for each case based
on which part is returned.

Some implementations do not represent all forms of implicit
declarations such that elements representing them can be easily
provided. An implementation can choose whether or not to construct
and provide artificial declarations for implicitly declared elements.
@end{ImplPerm}

@begin{ImplReq}

@leading@;Raises ASIS_Inappropriate_Element, with a Status of Value_Error, if
passed a reference that does not have a declaration:

@begin{Itemize}
a reference to an attribute_designator. Attributes are defined, but
have no implicit or explicit declarations;

an identifier which syntactically is placed before "=>" in a
pragma_argument_association which has the form of a named association;
such an identifier can never have a declaration;

an identifier specific to a pragma (Reference Manual, 2.8(10)).
@end{Itemize}

@ChgAdded{Version=[1],Type=[Leading],Text=[For example:]}

@begin{ChildExample}
@key[pragma] Should_I_Check (Really => Yes);
@end{ChildExample}

In this example, both the names Really and Yes have no declaration.

@leading@;Raises ASIS_Inappropriate_Element, with a Status of Value_Error, if
passed a portion of a pragma that was "ignored" by the compiler and which does
not have (sufficient) semantic information for a proper return result to be
computed. For example:

@begin{ChildExample}
@key[pragma] I_Am_Ignored (Foof);
@end{ChildExample}

The "Foof" expression is An_Identifier but raises this exception
if passed to Corresponding_Name_Definition if the pragma was ignored
or unprocessed.

@leading@;Raises ASIS_Inappropriate_Element, with a Status of Value_Error, if
passed a portion of a pragma that is an ambiguous reference to more than one
entity. For example:

@begin{ChildExample}
@key[pragma] Inline ("+");  -- @examcom{Inlines all "+" operators}
@end{ChildExample}

The "+" expression is An_Operator_Symbol but raises this
exception if it referenced more than one "+" operator. In this
case, the Corresponding_Name_Definition_List query can be used to obtain a
list of referenced entities.
@end{ImplReq}


@LabeledClause{function Corresponding_Name_Definition_List}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Name_Definition_List} (Reference : @key[in] Asis.Element)
                               @key[return] Asis.Defining_Name_List;
@end{Example}

Reference @chg{Version=[1],New=[specifies],Old=[  @en Specifies]} an entity
reference to query.

@leading@;Exactly like Corresponding_Name_Definition except it returns a list.
The list will almost always have a length of one. The exception to this
is the case where an expression in a pragma is ambiguous and reference
more than one entity. For example:

@begin{ChildExample}
@key[pragma] Inline ("+");        -- @examcom{Inlines all "+" operators}
@end{ChildExample}

The "+" expression is An_Operator_Symbol but could reference more than one "+"
operator. In this case, the resulting list includes all referenced entities.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
An_Identifier
An_Operator_Symbol
A_Character_Literal
An_Enumeration_Literal
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
A_Defining_Name
@end{Display}
@end{DescribeCode}


@LabeledClause{function Corresponding_Name_Declaration}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Name_Declaration} (Reference : @key[in] Asis.Expression)
                          @key[return] Asis.Element;
@end{Example}

Reference @chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the entity
reference to query.

@leading@;Returns the declaration that declared the entity named by the given
reference. The result is exactly the same as:

@begin{ChildExample}
Result := Corresponding_Name_Definition (Reference);
@key[if not] Is_Nil (Result) @key[then]
   Result := Enclosing_Element (Result);
@key[end if];
@key[return] Result;
@end{ChildExample}

See the comments for Corresponding_Name_Definition for details.
The result is either a Declaration or a Statement. Statements result
from references to statement labels, loop identifiers, and block identifiers.

@leading@keepnext@;Appropriate Element_Kinds:
@begin{Display}
An_Expression
@end{Display}

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
An_Identifier
An_Operator_Symbol
A_Character_Literal
An_Enumeration_Literal
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
A_Declaration
A_Statement
@end{Display}
@end{DescribeCode}

Predefined types, exceptions, operators in package Standard can be
checked by testing that the enclosing Compilation_Unit is standard.


@LabeledClause{function Prefix}

@begin{ElementRef}
An_Explicit_Dereference @em 4.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Prefix
@end{Display}
@end{ChildRef}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Prefix} (Expression : @key[in] Asis.Expression) @key[return] Asis.Expression;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the name
expression to query.

Returns the prefix (the construct to the left of: the rightmost unnested left
parenthesis in function_call elements and indexed_component elements or slice
elements, the rightmost 'dot' for selected_component elements, or the rightmost
tick for attribute_reference elements).

Returns the operator_symbol for infix operator function calls. The infix
form A + B is equivalent to the prefix form "+"(A, B).

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
An_Explicit_Dereference @em P.ALL
An_Attribute_Reference @em Priv'Base'First
A_Function_Call @em Abc(...) or Integer'Image(...)
An_Indexed_Component @em An_Array(3)
A_Selected_Component @em A.B.C
A_Slice @em An_Array(3 .. 5)
@end{Display}

@leading@keepnext@;Returns Expression_Kinds:
@begin{Display}
An_Expression
@end{Display}
@end{DescribeCode}


@LabeledClause{function Index_Expressions}

@begin{ElementRef}
An_Indexed_Component @em 4.1.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Prefix
function Index_Expressions
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Index_Expressions} (Expression : @key[in] Asis.Expression)
                            @key[return] Asis.Expression_List;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} an
indexed_component to query.

Returns the list of expressions (possibly only one) within the parenthesis,
in their order of appearance.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
An_Indexed_Component
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
An_Expression
@end{Display}
@end{DescribeCode}


@LabeledClause{function Slice_Range}

@begin{ElementRef}
A_Slice @em 4.1.2
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Prefix
function Slice_Range
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Slice_Range} (Expression : @key[in] Asis.Expression)
                      @key[return] Asis.Discrete_Range;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the slice to
query.

Returns the discrete range of the slice.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
A_Slice
@end{Display}

@leading@keepnext@;Returns Definition_Kinds:
@begin{Display}
A_Discrete_Range
@end{Display}
@end{DescribeCode}


@LabeledClause{function Selector}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Selector} (Expression : @key[in] Asis.Expression)
                   @key[return] Asis.Expression;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the
selected_component to query.

Returns the selector (the construct to the right of the rightmost 'dot' in
the selected_component).

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
A_Selected_Component
@end{Display}

@leading@keepnext@;Returns Expression_Kinds:
@begin{Display}
An_Identifier
An_Operator_Symbol
A_Character_Literal
An_Enumeration_Literal
@end{Display}
@end{DescribeCode}


@LabeledClause{function Attribute_Designator_Identifier}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Attribute_Designator_Identifier}
        (Expression : @key[in] Asis.Expression)
            @key[return] Asis.Expression;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} an
attribute_reference expression to query.

Returns the identifier of the attribute_designator (the construct to the
right of the rightmost tick of the attribute_reference). The Prefix of
the attribute_reference can itself be an attribute_reference as in
T'Base'First where the prefix is T'Base and the attribute_designator name
is First.

Attribute_designator reserved words @key[access], @key[delta], and @key[digits]
are treated as An_Identifier.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
An_Attribute_Reference
@end{Display}

@leading@keepnext@;Returns Expression_Kinds:
@begin{Display}
An_Identifier
@end{Display}
@end{DescribeCode}


@LabeledClause{function Attribute_Designator_Expressions}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Attribute_Designator_Expressions}
        (Expression : @key[in] Asis.Expression)
            @key[return] Asis.Expression_List;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} an attribute
expression to query.

Returns the static expressions associated with the optional argument of the
attribute_designator. Expected predefined attributes are A'First(N),
A'Last(N), A'Length(N), and A'Range(N).

Returns a Nil_Element_List if there are no arguments.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
An_Attribute_Reference
  Appropriate Attribute_Kinds:
    A_First_Attribute
    A_Last_Attribute
    A_Length_Attribute
    A_Range_Attribute
    An_Implementation_Defined_Attribute
    An_Unknown_Attribute
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
An_Expression
@end{Display}
@end{DescribeCode}

@b{Implementation Permissions}

This query returns a list to support implementation-defined attributes
that may have more than one static_expression.


@LabeledClause{function Record_Component_Associations}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Record_Component_Associations}
              (Expression : @key[in] Asis.Expression;
               Normalized : @key[in] Boolean := False)
                    @key[return] Asis.Association_List;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} an aggregate
expression to query.
Normalized @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} whether the
normalized form is desired.

Returns a list of the record_component_association elements of a
record_aggregate or an extension_aggregate.

Returns a Nil_Element_List if the aggregate is of the form (null record).

An unnormalized list contains all needed associations ordered as they
appear in the program text. Each unnormalized association has an optional
list of discriminant_selector_name elements, and an explicit expression.

A normalized list contains artificial associations representing all
needed components in an order matching the declaration order of the
needed components.

Each normalized association represents a one on one mapping of a
component to the explicit expression. A normalized association has one
A_Defining_Name component that denotes the discriminant_specification or
component_declaration, and one An_Expression component that is the
expression.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
A_Record_Aggregate
An_Extension_Aggregate
@end{Display}

@leading@keepnext@;Appropriate Association_Kinds:
@begin{Display}
A_Record_Component_Association
@end{Display}
@end{DescribeCode}

@begin{ImplReq}
Normalized associations are Is_Normalized and Is_Part_Of_Implicit.
Normalized associations are never Is_Equal to unnormalized associations.
@end{ImplReq}

@begin{ImplPerm}
An implementation may choose to normalize its internal representation
to use the defining_identifier element instead of the component_selector_name
element.

If so, this query will return Is_Normalized associations even if
Normalized is False, and the query
Record_Component_Associations_Normalized will return True.
@end{ImplPerm}


@LabeledClause{function Extension_Aggregate_Expression}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Extension_Aggregate_Expression}
                          (Expression : @key[in] Asis.Expression)
                                   @key[return] Asis.Expression;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} an
extension_aggregate expression to query.

Returns the ancestor_part expression preceding the reserved word @key[with] in
the extension_aggregate.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
An_Extension_Aggregate
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
An_Expression
@end{Display}
@end{DescribeCode}


@LabeledClause{function Array_Component_Associations}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Array_Component_Associations}
              (Expression : @key[in] Asis.Expression)
                    @key[return] Asis.Association_List;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} an array
aggregate expression to query.

Returns a list of the Array_Component_Associations in an array aggregate.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
A_Positional_Array_Aggregate
A_Named_Array_Aggregate
@end{Display}

@leading@keepnext@;Returns Association_Kinds:
@begin{Display}
An_Array_Component_Association
@end{Display}
@end{DescribeCode}

@begin{SingleNote}
@b{@i{Shouldn't this be normative?? - RLB}}
While positional_array_aggregate elements do not have
array_component_association elements defined by Ada syntax, ASIS treats
A_Positional_Array_Aggregate as if it were A_Named_Array_Aggregate.
The An_Array_Component_Association elements returned will have
Array_Component_Choices that are a Nil_Element_List for all positional
expressions except an others choice.
@end{SingleNote}


@LabeledClause{function Array_Component_Choices}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Array_Component_Choices}
        (Association : @key[in] Asis.Association)
            @key[return] Asis.Expression_List;
@end{Example}

Association @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the component
association to query.

@leading@;If the Association is from a named_array_aggregate:
@begin{Itemize}
Returns the discrete_choice_list order of appearance. The choices are
  either An_Expression or A_Discrete_Range elements, or a single
  An_Others_Choice element.
@end{Itemize}

@leading@;If the Association is from a positional_array_aggregate:
@begin{Itemize}
Returns a single An_Others_Choice if the association is an others
choice (@key[others] => expression).

Returns a Nil_Element_List otherwise.
@end{Itemize}

@leading@keepnext@;Appropriate Association_Kinds:
@begin{Display}
An_Array_Component_Association
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
A_Definition
An_Expression
@end{Display}

@leading@keepnext@;Returns Definition_Kinds:
@begin{Display}
A_Discrete_Range
An_Others_Choice
@end{Display}
@end{DescribeCode}


@LabeledClause{function Record_Component_Choices}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Record_Component_Choices}
        (Association : @key[in] Asis.Association)
            @key[return] Asis.Expression_List;
@end{Example}

Association @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the component
association to query.

@leading@;If the Association argument is from an unnormalized list:

@begin{Itemize}
@leading@;If the Association is a named component association:

@begin{InnerItemize}
Returns the component_choice_list order of appearance. The choices are
    either An_Identifier elements representing component_selector_name elements, or
    a single An_Others_Choice element.

The Enclosing_Element of the choices is the Association argument.
@end{InnerItemize}

@leading@;If the Association is a positional component association:

@begin{InnerItemize}
Returns a Nil_Element_List.
@end{InnerItemize}
@end{Itemize}

@leading@;If the Association argument is from a Normalized list:
@begin{Itemize}
@leading@;Returns a list containing a single choice:

@begin{InnerItemize}
A_Defining_Name element representing the defining_identifier of
the component_declaration.

The Enclosing_Element of the A_Defining_Name is the component_declaration.
@end{InnerItemize}
@end{Itemize}

Normalized lists contain artificial ASIS An_Association elements that
provide one formal A_Defining_Name => An_Expression pair per
association. These artificial associations are Is_Normalized. Their
component A_Defining_Name is not Is_Normalized.

@leading@keepnext@;Appropriate Association_Kinds:
@begin{Display}
A_Record_Component_Association
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
A_Defining_Name @em Is_Normalized(Association)
An_Expression @em @key[not] Is_Normalized(Association)
  Returns Expression_Kinds:
    An_Identifier
A_Definition
  Returns Definition_Kinds:
    An_Others_Choice
@end{Display}
@end{DescribeCode}


@LabeledClause{function Component_Expression}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Component_Expression} (Association : @key[in] Asis.Association)
                              @key[return] Asis.Expression;
@end{Example}

Association @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the component
association to query.

Returns the expression of the record_component_association or
array_component_association.

The Enclosing_Element of the expression is the Association argument.

Normalized lists contain artificial ASIS An_Association elements that
provide one formal A_Defining_Name => An_Expression pair per
association. These artificial associations are Is_Normalized. Their
component An_Expression elements are not Is_Normalized.

@leading@keepnext@;Appropriate Association_Kinds:
@begin{Display}
A_Record_Component_Association
An_Array_Component_Association
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
An_Expression
@end{Display}
@end{DescribeCode}


@LabeledClause{function Formal_Parameter}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Formal_Parameter} (Association : @key[in] Asis.Association)
                          @key[return] Asis.Element;
@end{Example}

Association @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
association to query.

@leading@;If the Association argument is from an unnormalized list:

@begin{Itemize}
@leading@;If the Association is given in named notation:

@begin{Indent}
Returns An_Identifier representing the formal_parameter_selector_name,
generic_formal_parameter_selector_name, or pragma_argument_identifier.

The Enclosing_Element of the An_Identifier element is the Association
argument.
@end{Indent}

@leading@;If the Association is given in positional notation:

@begin{Indent}
Returns a Nil_Element.
@end{Indent}
@end{Itemize}

@leading@;If the Association argument is from a Normalized list:

@begin{Itemize}
Returns A_Defining_Name representing the defining_identifier of the
parameter_specification or generic_formal_parameter_declaration.
Pragma_argument_association elements are not available in normalized form.

The Enclosing_Element of the A_Defining_Name is the
parameter_specification or generic_formal_parameter_declaration element.
@end{Itemize}

Normalized lists contain artificial ASIS An_Association elements that
provide one formal A_Defining_Name => An_Expression pair per
association. These artificial associations are Is_Normalized. Their
component A_Defining_Name elements are not Is_Normalized.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0014-1]}
@ChgAdded{Version=[2],Text=[Asis.Expressions.Formal_Parameter
may return An_Others_Choice for a
non-normalized A_Generic_Association argument;]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0014-1]}
@ChgAdded{Version=[2],Text=[
If a formal_package_association contains a box, then the corresponding
non-normalized A_Generic_Association element contains an Expression_Element
with expression kind A_Box_Expression as its
Actual_Parameter part. The normalized A_Generic_Association contains either
a default parameter or an Expression_Element with expression kind
A_Box_Expression;]}

@leading@keepnext@;Appropriate Association_Kinds:
@begin{Display}
A_Parameter_Association
A_Generic_Association
A_Pragma_Argument_Association
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
Not_An_Element
An_Operator_Symbol
A_Defining_Name @em Is_Normalized(Association)
An_Expression @em @key[not] Is_Normalized(Association)
@end{Display}
@leading@keepnext@;Returns Expression_Kinds:
@begin{Display}
An_Identifier
@end{Display}
@end{DescribeCode}


@LabeledClause{function Actual_Parameter}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Actual_Parameter} (Association : @key[in] Asis.Association)
                          @key[return] Asis.Expression;
@end{Example}

Association   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the association to query

@leading@;If the Association argument is from an unnormalized list:

@begin{Itemize}
@leading@;Returns An_Expression representing:

@begin{InnerItemize}
the explicit_actual_parameter of a parameter_association.

the explicit_generic_actual_parameter of a generic_association.

the name or expression of a pragma_argument_association.
@end{InnerItemize}

@noprefix@;The Enclosing_Element of An_Expression is the Association argument.
@end{Itemize}

@leading@;If the Association argument is from a Normalized list:

@begin{Itemize}
@leading@;If the Association is given explicitly:

@begin{InnerItemize}
Returns An_Expression representing:

@begin{InnerInnerItemize}
the explicit_actual_parameter of a parameter_association.

the explicit_generic_actual_parameter of a generic_association.
@end{InnerInnerItemize}

@noprefix@;The Enclosing_Element of An_Expression is the Association argument.
@end{InnerItemize}

@leading@;If the Association is given by default:

@begin{InnerItemize}
Returns An_Expression representing:

@begin{InnerInnerItemize}
the corresponding default_expression of the Is_Normalized
A_Parameter_Association.

the corresponding default_expression or default_name of the
Is_Normalized A_Generic_Association.
@end{InnerInnerItemize}

The Enclosing_Element of the An_Expression element is the
    parameter_specification or generic_formal_parameter_declaration that
    contains the default_expression or default_name, except for the case when
    this An_Expression element is an implicit naming expression
    representing the actual subprogram selected at the place of the
    instantiation for A_Box_Default. In the latter case, the Enclosing_Element
    for such An_Expression is the instantiation.

Normalized lists contain artificial ASIS An_Association elements that
    provide one formal A_Defining_Name => An_Expression pair per
    association. These artificial associations are Is_Normalized.
    Artificial associations of default associations are
    Is_Defaulted_Association. Their component An_Expression elements are
    not Is_Normalized and are not Is_Defaulted_Association.
@end{InnerItemize}
@end{Itemize}

If the argument is A_Pragma_Argument_Association, then this function may
return any expression to support implementation-defined pragmas.

@leading@keepnext@;Appropriate Association_Kinds:
@begin{Display}
A_Parameter_Association
A_Generic_Association
A_Pragma_Argument_Association
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
An_Expression
@end{Display}
@end{DescribeCode}


@LabeledClause{function Discriminant_Selector_Names}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Discriminant_Selector_Names}
            (Association : @key[in] Asis.Discriminant_Association)
            @key[return] Asis.Expression_List;
@end{Example}

Association @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
discriminant association to query.

@leading@;If the Association argument is from an unnormalized list:

@begin{Itemize}
@leading@;If the Association is a named discriminant_association:

@begin{InnerItemize}
Returns a list of the An_Identifier discriminant_selector_name elements in order
    of appearance.

The Enclosing_Element of the names is the Association argument.
@end{InnerItemize}

@leading@;If the Association is a positional discriminant_association:

@begin{InnerItemize}
Returns a Nil_Element_List.
@end{InnerItemize}

@end{Itemize}

@leading@;If the Association argument is from a Normalized list:

@begin{Itemize}
Returns a list containing a single A_Defining_Name element representing
  the defining_identifier of the discriminant_specification.

The Enclosing_Element of the A_Defining_Name is the
  discriminant_specification.

Normalized lists contain artificial ASIS An_Association elements that
  provide one formal A_Defining_Name => An_Expression pair per
  association. These artificial associations are Is_Normalized. Their
  component A_Defining_Name elements are not Is_Normalized.
@end{Itemize}

@leading@keepnext@;Appropriate Association_Kinds:
@begin{Display}
A_Discriminant_Association
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
A_Defining_Name @em Is_Normalized (Association)
An_Expression @em @key[not] Is_Normalized (Association)
  Returns Expression_Kinds:
    An_Identifier
@end{Display}
@end{DescribeCode}


@LabeledClause{function Discriminant_Expression}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Discriminant_Expression}
            (Association : @key[in] Asis.Discriminant_Association)
            @key[return] Asis.Expression;
@end{Example}

Association @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
discriminant_association to query.

@leading@;If the Association argument is from an unnormalized list:

@begin{Itemize}
Returns An_Expression representing the expression of the
  discriminant_association.

@noprefix@;The Enclosing_Element of An_Expression is the Association argument.
@end{Itemize}

@leading@;If the Association argument is from a Normalized list:

@begin{Itemize}
@leading@;If the Association is given explicitly:

@begin{InnerItemize}
Returns An_Expression representing the expression of the
    discriminant_association.

The Enclosing_Element of An_Expression is the Association argument.
@end{InnerItemize}

@leading@;If the Association is given by default:

@begin{InnerItemize}
@leading@;Returns An_Expression representing:

@begin{InnerInnerItemize}
the corresponding default_expression of the Is_Normalized
A_Discriminant_Association.
@end{InnerInnerItemize}

@noprefix@;The Enclosing_Element of the An_Expression element is the
    discriminant_specification that contains the default_expression.
@end{InnerItemize}

Normalized lists contain artificial ASIS An_Association elements that
  provide one formal A_Defining_Name => An_Expression pair per
  association. These artificial associations are Is_Normalized.
  Artificial associations of default associations are
  Is_Defaulted_Association. Their component An_Expression elements are
  not Is_Normalized and are not Is_Defaulted_Association.
@end{Itemize}

@leading@keepnext@;Appropriate Association_Kinds:
@begin{Display}
A_Discriminant_Association
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
An_Expression
@end{Display}
@end{DescribeCode}


@LabeledClause{function Is_Normalized}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Normalized} (Association : @key[in] Asis.Association) @key[return] Boolean;
@end{Example}

Association @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
association to query.

Returns True if the association is a normalized, artificially created
association returned by the queries Discriminant_Associations,
Generic_Actual_Part, Call_Statement_Parameters,
Record_Component_Associations, or Function_Call_Parameters where
Normalized => True (or the operation returns Is_Normalized associations
even if Normalized => False). See the @b{Implementation Permissions} for
these queries.

Returns False for any unexpected Element.

@leading@keepnext@;Expected Association_Kinds:
@begin{Display}
A_Discriminant_Association
A_Record_Component_Association
A_Parameter_Association
A_Generic_Association
@end{Display}
@end{DescribeCode}


@LabeledClause{function Is_Defaulted_Association}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Defaulted_Association}
    (Association : @key[in] Asis.Association) @key[return] Boolean;
@end{Example}

Association @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
association to query.

Returns True if the association is a normalized, artificially created
association returned by the queries Discriminant_Associations,
Generic_Actual_Part, Record_Component_Associations,
Call_Statement_Parameters, or Function_Call_Parameters where
Normalized => True (or the operation returns default associations even if
Normalized => False) and the association contains a default expression.
A default expression is one that is implicitly supplied by the language
semantics and that was not explicitly supplied (typed) by the user.

Returns False for any unexpected Element.

@leading@keepnext@;Expected Association_Kinds:
@begin{Display}
A_Parameter_Association
A_Generic_Association
@end{Display}
@end{DescribeCode}

@begin{UsageNote}
Always returns False for discriminant associations. Defaulted
discriminant associations occur only when the discriminant constraint is
completely missing from a subtype indication. Consequently, it is not
possible to obtain a (normalized) discriminant constraint list for such
subtype indications. Always returns False for component associations.
Aggregates cannot have defaulted components.
@end{UsageNote}


@LabeledClause{function Expression_Parenthesized}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Expression_Parenthesized} (Expression : @key[in] Asis.Expression)
                                  @key[return] Asis.Expression;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the
parenthesized expression to query.

Returns the expression within the parenthesis. This operation unwinds only
one set of parenthesis at a time, so the result may itself be
A_Parenthesized_Expression.

A_Parenthesized_Expression kind corresponds only to the (expression)
alternative in the syntax notion of primary in Reference Manual 4.4. For
example, an expression of a type_conversion is A_Parenthesized_Expression only
if it is similar to the form subtype_mark((expression)) where it has at least
one set of its own parenthesis.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
A_Parenthesized_Expression
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
An_Expression
@end{Display}
@end{DescribeCode}


@LabeledClause{function Is_Prefix_Call}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Prefix_Call} (Expression : @key[in] Asis.Expression) @key[return] Boolean;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the function
call expression to query.

Returns True if the function call is in prefix form.

Returns False for any unexpected Element.

@leading@;For example:

@begin{ChildExample}
Foo (A, B);   -- @examcom{Returns True}
"<" (A, B);   -- @examcom{Returns True}
... A < B ... -- @examcom{Returns False}
@end{ChildExample}

@leading@keepnext@;Expected Expression_Kinds:
@begin{Display}
A_Function_Call
@end{Display}
@end{DescribeCode}


@LabeledClause{function Corresponding_Called_Function}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Called_Function}
        (Expression : @key[in] Asis.Expression)
            @key[return] Asis.Declaration;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the
function_call to query.

Returns the declaration of the called function.

@leading@;Returns a Nil_Element if the:

@begin{Itemize}
function_prefix denotes a predefined operator for which the implementation
  does not provide an artificial function declaration,

prefix of the call denotes an access to a function implicit or explicit
  dereference,

argument is a dispatching call.
@end{Itemize}

If function_prefix denotes an attribute_reference, and if the corresponding
attribute is (re)defined by an attribute definition clause, an implementation
is encouraged, but not required, to return the definition of the corresponding
subprogram whose name is used after @key[use] in this attribute definition
clause. If an implementation cannot return such a subprogram definition, a
Nil_Element should be returned. For an attribute reference which is not
(re)defined by an attribute definition clause, a Nil_Element should be
returned.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
A_Function_Call
@end{Display}

@leading@keepnext@;Returns Declaration_Kinds:
@begin{Display}
Not_A_Declaration
A_Function_Declaration
A_Function_Body_Declaration
A_Function_Body_Stub
A_Function_Renaming_Declaration
A_Function_Instantiation
A_Formal_Function_Declaration
A_Generic_Function_Declaration
@end{Display}
@end{DescribeCode}

@begin{ImplPerm}
@leading@;An implementation may choose to return any part of multi-part
declarations and definitions. Multi-part declaration/definitions can occur for:
@begin{Itemize}
Subprogram specification in package specification, package body,
and subunits (@key[is separate]);

Entries in package specification, package body, and subunits
(@key[is separate]);

Private type and full type declarations;

Incomplete type and full type declarations; and

Deferred constant and full constant declarations.
@end{Itemize}

No guarantee is made that the element will be the first part or
that the determination will be made due to any visibility rules.
An application should make its own analysis for each case based
on which part is returned.

An implementation can choose whether or not to construct and provide
artificial implicit declarations for predefined operators.
@end{ImplPerm}


@LabeledClause{function Function_Call_Parameters}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Function_Call_Parameters} (Expression : @key[in] Asis.Expression;
                                   Normalized : @key[in] Boolean := False)
                                  @key[return] Asis.Association_List;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the function
call expression to query.
Normalized @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} whether the
normalized form is desired.

Returns a list of parameter_association elements of the call.

Returns a Nil_Element_List if there are no parameter_association elements.

An unnormalized list contains only explicit associations ordered as they
appear in the program text. Each unnormalized association has an optional
formal_parameter_selector_name and an explicit_actual_parameter component.

A normalized list contains artificial associations representing all
explicit and default associations. It has a length equal to the number of
parameter_specification elements of the formal_part of the
parameter_and_result_profile. The order of normalized associations matches
the order of parameter_specification elements.

Each normalized association represents a one on one mapping of a
parameter_specification elements to the explicit or default expression.
A normalized association has one A_Defining_Name component that denotes the
parameter_specification, and one An_Expression component that is either the
explicit_actual_parameter or a default_expression.

If the prefix of the call denotes an access to a function implicit or
explicit deference, normalized associations are constructed on the basis
of the formal_part of the parameter_and_result_profile from the
corresponding access_to_subprogram definition.

Returns Nil_Element for normalized associations in the case where
the called function can be determined only dynamically (dispatching
calls). ASIS cannot produce any meaningful result in this case.

The exception ASIS_Inappropriate_Element is raised when the function
call is an attribute reference and Is_Normalized is True.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
A_Function_Call
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
A_Parameter_Association
@end{Display}
@end{DescribeCode}

@begin{ImplReq}
Normalized associations are Is_Normalized and Is_Part_Of_Implicit.
Normalized associations provided by default are Is_Defaulted_Association.
Normalized associations are never Is_Equal to unnormalized associations.
@end{ImplReq}

@begin{ImplPerm}

An implementation may choose to always include default parameters in its
internal representation.

An implementation may also choose to normalize its representation
to use defining_identifier elements rather than formal_parameter_selector_name
elements.

In either case, this query will return Is_Normalized associations even if
Normalized is False, and the query Function_Call_Parameters_Normalized
will return True.
@end{ImplPerm}


@LabeledClause{function Short_Circuit_Operation_Left_Expression}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Short_Circuit_Operation_Left_Expression}
            (Expression : @key[in] Asis.Expression)
            @key[return] Asis.Expression;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the short
circuit operation to query.

Returns the expression preceding the reserved words @key[and then] or
@key[or else] in the short circuit expression.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
An_And_Then_Short_Circuit
An_Or_Else_Short_Circuit
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
An_Expression
@end{Display}
@end{DescribeCode}


@LabeledClause{function Short_Circuit_Operation_Right_Expression}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Short_Circuit_Operation_Right_Expression}
            (Expression : @key[in] Asis.Expression)
            @key[return] Asis.Expression;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the short
circuit operation to query.

Returns the expression following the reserved words @key[or else] or
@key[and then] in the short circuit expression.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
An_And_Then_Short_Circuit
An_Or_Else_Short_Circuit
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
An_Expression
@end{Display}
@end{DescribeCode}


@LabeledClause{function Membership_Test_Expression}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Membership_Test_Expression} (Expression : @key[in] Asis.Expression)
                                    @key[return] Asis.Expression;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the
membership test operation to query.

Returns the expression on the left hand side of the membership test.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
An_In_Range_Membership_Test
A_Not_In_Range_Membership_Test
An_In_Type_Membership_Test
A_Not_In_Type_Membership_Test
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
An_Expression
@end{Display}
@end{DescribeCode}


@LabeledClause{function Membership_Test_Range}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Membership_Test_Range}
            (Expression : @key[in] Asis.Expression)
            @key[return] Asis.Range_Constraint;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the
membership test operation to query.

Returns the range following the reserved words @key[in] or @key[not in] from
the membership test.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
An_In_Range_Membership_Test
A_Not_In_Range_Membership_Test
@end{Display}

@leading@keepnext@;Returns Constraint_Kinds:
@begin{Display}
A_Range_Attribute_Reference
A_Simple_Expression_Range
@end{Display}
@end{DescribeCode}


@LabeledClause{function Membership_Test_Subtype_Mark}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Membership_Test_Subtype_Mark}
            (Expression : @key[in] Asis.Expression)
            @key[return] Asis.Expression;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the
membership test operation to query.

Returns the subtype_mark expression following the reserved words @key[in] or
@key[not in] from the membership test.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
An_In_Type_Membership_Test
A_Not_In_Type_Membership_Test
@end{Display}

@leading@keepnext@;Returns Expression_Kinds:
@begin{Display}
An_Identifier
A_Selected_Component
An_Attribute_Reference
@end{Display}
@end{DescribeCode}


@LabeledClause{function Converted_Or_Qualified_Subtype_Mark}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Converted_Or_Qualified_Subtype_Mark}
               (Expression : @key[in] Asis.Expression)
                   @key[return] Asis.Expression;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the type
conversion or qualified expression to query.

Returns the subtype_mark expression that converts or qualifies the
expression.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
A_Type_Conversion
A_Qualified_Expression
@end{Display}

@leading@keepnext@;Returns Expression_Kinds:
@begin{Display}
An_Identifier
A_Selected_Component
An_Attribute_Reference
@end{Display}
@end{DescribeCode}


@LabeledClause{function Converted_Or_Qualified_Expression}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Converted_Or_Qualified_Expression}
               (Expression : @key[in] Asis.Expression)
                   @key[return] Asis.Expression;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the type
conversion or qualified expression to query.

Returns the expression being converted or qualified.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
A_Type_Conversion
A_Qualified_Expression
@end{Display}

@leading@keepnext@;Returns Element_Kinds:
@begin{Display}
An_Expression
@end{Display}
@end{DescribeCode}


@LabeledClause{function Allocator_Subtype_Indication}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Allocator_Subtype_Indication} (Expression : @key[in] Asis.Expression)
                                       @key[return] Asis.Subtype_Indication;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the allocator
expression to query.

Returns the subtype indication for the object being allocated.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
An_Allocation_From_Subtype
@end{Display}

@leading@keepnext@;Returns Definition_Kinds:
@begin{Display}
A_Subtype_Indication
@end{Display}
@end{DescribeCode}


@LabeledClause{function Allocator_Qualified_Expression}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Allocator_Qualified_Expression} (Expression : @key[in] Asis.Expression)
                                         @key[return] Asis.Expression;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the allocator
expression to query.

Returns the qualified expression for the object being allocated.

@leading@keepnext@;Appropriate Expression_Kinds:
@begin{Display}
An_Allocation_From_Qualified_Expression
@end{Display}

@leading@keepnext@;Returns Expression_Kinds:
@begin{Display}
A_Qualified_Expression
@end{Display}
@end{DescribeCode}

@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[end] Asis.Expressions;]}
@end{Example}


