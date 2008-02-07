@Part(declarations, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/declarations.mss,v $}
@comment{$Revision: 1.11 $ $Date: 2008/02/06 06:23:46 $}


@LabeledSection{package Asis.Declarations}


@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis],Child=[Declarations]}Asis.Declarations
shall exist. The package
shall provide interfaces equivalent to those described in the
following subclauses.],
Old=[@f{@key[package] @ChildUnit{Parent=[Asis],Child=[Declarations]}Asis.Declarations @key[is]}]}

Asis.Declarations encapsulates a set of queries that operate on
A_Defining_Name and A_Declaration elements.


@LabeledClause{function Names}

@begin{ElementRef}
A_Declaration @em 3.1
@end{ElementRef}
@begin{ChildRef}@ @;
@begin{Display}
function Names
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Names} (Declaration : @key[in] Asis.Declaration)
               @key[return] Asis.Defining_Name_List;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element
to query.

Returns a list of names defined by the declaration, in their order of
appearance. Declarations that define a single name will return a list of
length one.

Returns Nil_Element_List for A_Declaration Elements representing the (implicit)
declarations of universal and root numeric type (that is, if Type_Kind
(Type_Declaration_View (Declaration) = A_Root_Type_Definition.

Function designators that define operators are A_Defining_Operator_Symbol.

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0025-1]}
@ChgDeleted{Version=[2],Text=[Results of this query may vary across ASIS
implementations. Some implementations may normalize all multi-name declarations
into an equivalent series of corresponding single name declarations. For those
implementations, this query will always return a list containing a single name.
See Reference Manual 3.3.1(7).]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
that has one of the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[a list of elements that
have one of the following],Old=[]} Element_Kinds:
@begin{Display}
A_Defining_Name
@end{Display}
@end{DescribeCode}

@begin{Examples}
@begin{Example}
@key[type] Foo @key[is] (Pooh, Baah);
   -- @examcom{Returns a list containing one A_Defining_Name: Foo.}

One, Uno : @key[constant] Integer := 1;
   -- @examcom{Returns a list of two A_Defining_Name elements: One and Uno.}
@end{Example}
@end{Examples}


@LabeledClause{function Defining_Name_Image}

@begin{ElementRef}
A_Defining_Name @em 3.1@*
@*
A_Defining_Identifier @em 3.1 @em no child elements@*
A_Defining_Operator_Symbol @em 6.1 @em no child elements@*
@*
A string image returned by:@*
   function Defining_Name_Image
@end{ElementRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Defining_Name_Image}
        (Defining_Name : @key[in] Asis.Defining_Name) @key[return] Program_Text;
@end{Example}

Defining_Name @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the
element to query.

Returns the program text image of the name. Embedded quotes (for operator
designator strings) are doubled.

A_Defining_Identifier elements are simple identifier names "Abc" (name Abc).

A_Defining_Operator_Symbol elements have names with embedded quotes
"""abs""" (function "@key[abs]").

A_Defining_Character_Literal elements have names with embedded apostrophes
"'x'" (literal 'x').

A_Defining_Enumeration_Literal elements have simple identifier names
"Blue" (literal Blue). If A_Defining_Enumeration_Literal element is of type
Character or Wide_Character but does not have a graphical presentation, then
the result is implementation-dependent.

A_Defining_Expanded_Name elements are prefix.selector names "A.B.C"
(name A.B.C).

The case of names returned by this query may vary between implementors.
Implementors are encouraged, but not required, to return names in the
same case as was used in the original compilation text.

The Defining_Name_Image of a label_statement_identifier does not include
the enclosing "<<" and ">>" that form the label syntax. Similarly, the
Defining_Name_Image of an identifier for a loop_statement or block_statement
does not include the trailing colon that forms the loop name syntax.
Use Asis.Text.Element_Image or Asis.Text.Lines queries to obtain these
syntactic constructs and any comments associated with them.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Defining_Name expects an element
that has one of the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Defining_Name
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}
@end{DescribeCode}


@LabeledClause{function Position_Number_Image}

@begin{ElementRef}
A_Defining_Character_Literal @em 3.5.1 @em no child elements@*
A_Defining_Enumeration_Literal @em 3.5.1 @em no child elements@*
@*
A program text image returned by:@*
   function Defining_Name_Image@*
@*
A program text image of the enumeration literal value returned by:@*
   function Position_Number_Image@*
   function Representation_Value_Image
@end{ElementRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Position_Number_Image}
        (Defining_Name : @key[in] Asis.Defining_Name) @key[return] Wide_String;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the literal
expression to query.

Returns the program text image of the position number of the value of the
enumeration literal.

The program text returned is the image of the universal_integer value that is
returned by the attribute 'Pos if it were applied to the value.
For example: Integer'Image(Color'Pos(Blue)).

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Defining_Name expects an element
that is one of the following],Old=[Appropriate]} Defining_Name_Kinds:
@begin{Display}
A_Defining_Character_Literal
A_Defining_Enumeration_Literal
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}
@end{DescribeCode}


@LabeledClause{function Representation_Value_Image}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Representation_Value_Image}
        (Defining_Name : @key[in] Asis.Defining_Name) @key[return] Wide_String;
@end{Example}

Expression @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the literal
expression to query.

Returns the string image of the internal code for the enumeration literal.

If a representation_clause is defined for the enumeration type then the
string returned is the Integer'Wide_Image of the corresponding value given in
the enumeration_aggregate. Otherwise, the string returned is the same as
the Position_Number_Image.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Defining_Name expects an element
that is one of the following],Old=[Appropriate]} Defining_Name_Kinds:
@begin{Display}
A_Defining_Character_Literal
A_Defining_Enumeration_Literal
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}
@end{DescribeCode}


@LabeledClause{function Defining_Prefix}

@begin{ElementRef}
A_Defining_Expanded_Name @em 6.1@*
@*
A string image returned by:@*
   function Defining_Name_Image
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Defining_Prefix
function Defining_Selector
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Defining_Prefix} (Defining_Name : @key[in] Asis.Defining_Name)
                   @key[return] Asis.Name;
@end{Example}

Defining_Name @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the element
to query.

Returns the element that forms the prefix of the name. The prefix is the
name to the left of the rightmost 'dot' in the expanded name.
The Defining_Prefix of A.B is A, and of A.B.C is A.B.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Defining_Name expects an element
that has one of the following],Old=[Appropriate]} Defining_Name_Kinds:
@begin{Display}
A_Defining_Expanded_Name
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element that has
one of the following],Old=[]} Expression_Kinds:
@begin{Display}
An_Identifier
A_Selected_Component
@end{Display}
@end{DescribeCode}


@LabeledClause{function Defining_Selector}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Defining_Selector} (Defining_Name : @key[in] Asis.Defining_Name)
                     @key[return] Asis.Defining_Name;
@end{Example}

Defining_Name @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the
element to query.

Returns the element that forms the selector of the name. The selector is
the name to the right of the rightmost 'dot' in the expanded name.
The Defining_Selector of A.B is B, and of A.B.C is C.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Defining_Name expects an element
that has one of the following],Old=[Appropriate]} Defining_Name_Kinds:
@begin{Display}
A_Defining_Expanded_Name
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element that
has one of the following],Old=[]} Defining_Name_Kinds:
@begin{Display}
A_Defining_Identifier
@end{Display}
@end{DescribeCode}


@LabeledClause{function Discriminant_Part}

@begin{ElementRef}
An_Ordinary_Type_Declaration @em 3.2.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Discriminant_Part
function Type_Declaration_View
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Discriminant_Part} (Declaration : @key[in] Asis.Declaration)
                       @key[return] Asis.Definition;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the type
declaration to query.

Returns the discriminant_part, if any, from the type_declaration or
formal_type_declaration.

Returns a Nil_Element if the Declaration has no explicit discriminant_part.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
that has one of the following],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
An_Ordinary_Type_Declaration
A_Task_Type_Declaration
A_Protected_Type_Declaration
An_Incomplete_Type_Declaration
A_Private_Type_Declaration
A_Private_Extension_Declaration
A_Formal_Type_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element that has
one of the following],Old=[]} Definition_Kinds:
@begin{Display}
Not_A_Definition
An_Unknown_Discriminant_Part
A_Known_Discriminant_Part
@end{Display}
@end{DescribeCode}


@LabeledClause{function Type_Declaration_View}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Type_Declaration_View} (Declaration : @key[in] Asis.Declaration)
              @key[return] Asis.Definition;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
declaration element to query.

Returns the definition characteristics that form the view of the
type_declaration. The view is the remainder of the declaration following
the reserved word @key[is].

For a full_type_declaration, returns the type_definition, task_definition,
or protected_definition following the reserved word @key[is] in the declaration.

Returns a Nil_Element for a task_type_declaration that has no explicit
task_definition.

For a private_type_declaration or private_extension_declaration, returns
the definition element representing the private declaration view.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0011-1]}
@ChgAdded{Version=[2],Text=[
For an incomplete_type_declaration, returns the definition element
representing the incomplete declaration view.
]}

For a subtype_declaration, returns the subtype_indication.

For a formal_type_declaration, returns the formal_type_definition.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0011-1]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
that has one of the following],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
An_Ordinary_Type_Declaration
A_Task_Type_Declaration
A_Protected_Type_Declaration
A_Private_Type_Declaration
A_Private_Extension_Declaration
A_Subtype_Declaration
A_Formal_Type_Declaration@ChgAdded{Version=[2],Text=[
An_Incomplete_Type_Declaration]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0011-1],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element that has
one of the following],Old=[]} Definition_Kinds:
@begin{Display}
Not_A_Definition
A_Type_Definition
A_Subtype_Indication@ChgAdded{Version=[2],Text=[
An_Incomplete_Type_Definition
A_Tagged_Incomplete_Type_Definition]}
A_Private_Type_Definition
A_Tagged_Private_Type_Definition
A_Private_Extension_Definition
A_Task_Definition
A_Protected_Definition
A_Formal_Type_Definition
@end{Display}
@end{DescribeCode}


*** This should be replaced by a new routine.
@LabeledRevisedClause{Version=[2],New=[function Object_Declaration_Subtype],Old=[function Object_Declaration_View]}

@begin{ElementRef}
A_Subtype_Declaration @em 3.2.2
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Type_Declaration_View
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Variable_Declaration @em 3.3.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Object_Declaration_View
function Initialization_Expression
@end{Display}
@end{ChildRef}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Object_Declaration_View} (Declaration : @key[in] Asis.Declaration)
              @key[return] Asis.Definition;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
declaration element to query.

Returns the definition characteristics that form the view of the
object_declaration. The view is the subtype_indication or full type
definition of the object_declaration. An initial value, if any, is not
part of this view.

For a single_task_declaration or single_protected_declaration, returns the
task_definition or protected_definition following the reserved word @key[is].

Returns a Nil_Element for a single_task_declaration that has no explicit
task_definition.

For a Component_Declaration, returns the Component_Definition following
the colon.

For all other object_declaration variables or constants, returns the
subtype_indication or array_type_definition following the colon.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0010-1],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
A_Variable_Declaration
A_Constant_Declaration
A_Deferred_Constant_Declaration
A_Single_Protected_Declaration
A_Single_Task_Declaration
A_Component_Declaration@ChgAdded{Version=[2],Text=[
A_Return_Object_Specification]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Definition_Kinds:
@begin{Display}
@ChgRef{Version=[2],Kind=[Revised]}
Not_A_Definition
A_Type_Definition@Chg{Version=[2],New=[ that has],Old=[
   Returns]} Type_Kinds:
@Chg{Version=[2],New=[],Old=[    ]}    A_Constrained_Array_Definition
A_Subtype_Indication
A_Task_Definition
A_Protected_Definition
A_Component_Definition
@end{Display}
@end{DescribeCode}
*** End Obsolete code ***

@LabeledClause{function Initialization_Expression}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Initialization_Expression} (Declaration : @key[in] Asis.Declaration)
                       @key[return] Asis.Expression;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the object
declaration to query.

Returns the initialization expression [:= expression] of the declaration.

Returns a Nil_Element if the declaration does not include an explicit
initialization.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0010-1],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
A_Variable_Declaration
A_Constant_Declaration
An_Integer_Number_Declaration
A_Real_Number_Declaration
A_Discriminant_Specification
A_Component_Declaration
A_Parameter_Specification
A_Formal_Object_Declaration@ChgAdded{Version=[2],Text=[
A_Return_Object_Specification]}
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


@LabeledClause{function Corresponding_Constant_Declaration}

@begin{ElementRef}
A_Constant_Declaration @em 3.3.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Object_Declaration_View
function Initialization_Expression
@end{Display}
@leading@keepnext@;Element queries that provide semantically related elements:
@begin{Display}
function Corresponding_Constant_Declaration
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Constant_Declaration}
            (Name : @key[in] Asis.Defining_Name)
            @key[return] Asis.Declaration;
@end{Example}

Name @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the name of a
constant declaration to query.

Returns the corresponding full constant declaration when given the name
from a deferred constant declaration.

Returns the corresponding deferred constant declaration when given the name
from a full constant declaration.

Returns A_Pragma if the deferred constant declaration is completed
by pragma Import.

Returns a Nil_Element if the full constant declaration has no corresponding
deferred constant declaration.

Raises ASIS_Inappropriate_Element with a Status of Value_Error if the
argument is not the name of a constant or a deferred constant.

The name of a constant declaration is available from both the Names and the
Corresponding_Name_Definition queries.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Name expects an element
of],Old=[Appropriate]} Element_Kinds:
@begin{Display}
@ChgRef{Version=[2],Kind=[Revised]}
A_Defining_Name@Chg{Version=[2],New=[ that has Declaration_Kinds:
    Not_A_Declaration
    A_Constant_Declaration
    A_Deferred_Constant_Declaration],Old=[]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Appropriate Declaration_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[Not_A_Declaration
A_Constant_Declaration
A_Deferred_Constant_Declaration]}
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
A_Declaration
A_Pragma
@end{Display}
@end{DescribeCode}


*** Start obsolete code ***

@LabeledRevisedClause{Version=[2],New=[obsolete function DSM],Old=[function Declaration_Subtype_Mark]}

@begin{ElementRef}
A_Deferred_Constant_Declaration @em 3.3.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Object_Declaration_View
@end{Display}
@end{ChildRef}

@begin{ElementRef}
An_Integer_Number_Declaration @em 3.3.2@*
A_Real_Number_Declaration @em 3.3.2
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Initialization_Expression
@end{Display}
@end{ChildRef}

@begin{ElementRef}
An_Enumeration_Literal_Specification @em 3.5.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Discriminant_Specification @em 3.7
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Declaration_Subtype_Mark
function Initialization_Expression
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Declaration_Subtype_Mark} (Declaration : @key[in] Asis.Declaration)
                                  @key[return] Asis.Expression;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
declaration element to query.

Returns the expression element that names the subtype_mark of the
declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
A_Discriminant_Specification
A_Parameter_Specification
A_Formal_Object_Declaration
An_Object_Renaming_Declaration
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
A_Selected_Component
An_Attribute_Reference
@end{Display}
@end{DescribeCode}
*** end obsolete code ***

@LabeledClause{function Corresponding_Type_Declaration}

@begin{ElementRef}
A_Component_Declaration @em 3.8
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Object_Declaration_View
function Initialization_Expression
@end{Display}
@end{ChildRef}

@begin{ElementRef}
An_Incomplete_Type_Declaration @em 3.10.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Discriminant_Part
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Type_Declaration}
            (Declaration : @key[in] Asis.Declaration) @key[return] Asis.Declaration;

@key[function] @AdaSubDefn{Corresponding_Type_Declaration}
            (Declaration : @key[in] Asis.Declaration;
             The_Context : @key[in] Asis.Context) @key[return] Asis.Declaration;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the type
declaration to query. The_Context @chg{Version=[1],New=[specifies],Old=[@en
Specifies]} the program Context to use for obtaining package body information.

Returns the corresponding full type declaration when given a private or
incomplete type declaration. Returns the corresponding private or
incomplete type declaration when given a full type declaration.

@leading@;These two function calls will always produce identical results:

@begin{ChildExample}
Decl2 := Corresponding_Type_Declaration (Decl1);
Decl2 := Corresponding_Type_Declaration
         (Decl1,
          Enclosing_Context (Enclosing_Compilation_Unit (Decl1)));
@end{ChildExample}

Returns a Nil_Element when a full type declaration is given that has no
corresponding private or incomplete type declaration, or when a
corresponding type declaration does not exist within The_Context.

The parameter The_Context is used whenever the corresponding full type of
an incomplete type is in a corresponding package body. See
Reference Manual 3.10.1(3). Any non-Nil result will always have the given
Context as its Enclosing_Context.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
An_Ordinary_Type_Declaration
A_Task_Type_Declaration
A_Protected_Type_Declaration
An_Incomplete_Type_Declaration
A_Private_Type_Declaration
A_Private_Extension_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Declaration_Kinds:
@begin{Display}
Not_A_Declaration
An_Ordinary_Type_Declaration
A_Task_Type_Declaration
A_Protected_Type_Declaration
An_Incomplete_Type_Declaration
A_Private_Type_Declaration
A_Private_Extension_Declaration
@end{Display}
@end{DescribeCode}


@LabeledClause{function Corresponding_First_Subtype}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_First_Subtype}
              (Declaration : @key[in] Asis.Declaration)
                  @key[return] Asis.Declaration;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
subtype_declaration to query.

This function recursively unwinds subtyping to return at a type_declaration
that defines the first subtype of the argument.

Returns a declaration that Is_Identical to the argument if the argument is
already the first subtype.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
An_Ordinary_Type_Declaration
A_Task_Type_Declaration
A_Protected_Type_Declaration
A_Private_Type_Declaration
A_Private_Extension_Declaration
A_Subtype_Declaration
A_Formal_Type_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Declaration_Kinds:
@begin{Display}
An_Ordinary_Type_Declaration
A_Task_Type_Declaration
A_Protected_Type_Declaration
A_Private_Type_Declaration
A_Private_Extension_Declaration
A_Formal_Type_Declaration
@end{Display}
@end{DescribeCode}


@LabeledClause{function Corresponding_Last_Constraint}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Last_Constraint}
              (Declaration : @key[in] Asis.Declaration)
                  @key[return] Asis.Declaration;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
subtype_declaration or type_declaration to query.

This function recursively unwinds subtyping to return at a declaration
that is either a type_declaration or subtype_declaration that imposes
an explicit constraint on the argument.

Unwinds a minimum of one level of subtyping even if an argument declaration
itself has a constraint.

Returns a declaration that Is_Identical to the argument if the argument is
a type_declaration, i.e. the first subtype.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
An_Ordinary_Type_Declaration
A_Task_Type_Declaration
A_Protected_Type_Declaration
A_Private_Type_Declaration
A_Private_Extension_Declaration
A_Subtype_Declaration
A_Formal_Type_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Declaration_Kinds:
@begin{Display}
An_Ordinary_Type_Declaration
A_Task_Type_Declaration
A_Protected_Type_Declaration
A_Private_Type_Declaration
A_Private_Extension_Declaration
A_Subtype_Declaration
A_Formal_Type_Declaration
@end{Display}
@end{DescribeCode}


@LabeledClause{function Corresponding_Last_Subtype}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Last_Subtype}
              (Declaration : @key[in] Asis.Declaration)
                  @key[return] Asis.Declaration;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
subtype_declaration or type_declaration to query.

This function unwinds subtyping a single level to arrive at a declaration
that is either a type_declaration or subtype_declaration.

Returns a declaration that Is_Identical to the argument if the argument is
a type_declaration (i.e., the first subtype).

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
An_Ordinary_Type_Declaration
A_Task_Type_Declaration
A_Protected_Type_Declaration
A_Private_Type_Declaration
A_Private_Extension_Declaration
A_Subtype_Declaration
A_Formal_Type_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Declaration_Kinds:
@begin{Display}
An_Ordinary_Type_Declaration
A_Task_Type_Declaration
A_Protected_Type_Declaration
A_Private_Type_Declaration
A_Private_Extension_Declaration
A_Subtype_Declaration
A_Formal_Type_Declaration
@end{Display}
@end{DescribeCode}


@LabeledClause{function Corresponding_Representation_Clauses}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Representation_Clauses}
            (Declaration : @key[in] Asis.Declaration)
            @key[return] Asis.Representation_Clause_List;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
declaration to query.

Returns all representation_clause elements that apply to the declaration.

Returns a Nil_Element_List if no clauses apply to the declaration.

The clauses returned may be the clauses applying to a parent type if the
type is a derived type with no explicit representation. These clauses
are not Is_Part_Of_Implicit, they are the representation_clause elements
specified in conjunction with the declaration of the parent type.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Declaration expects an element of any],Old=[All]}
Declaration_Kinds @Chg{Version=[2],New=[other than],Old=[are appropriate
except]} Not_A_Declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[a list of elements that each have
one of ],Old=[]}Clause_Kinds:
@begin{Display}
A_Representation_Clause
@end{Display}
@end{DescribeCode}


@LabeledClause{function Specification_Subtype_Definition}

@begin{ElementRef}
A_Loop_Parameter_Specification @em 5.5
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Specification_Subtype_Definition
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Specification_Subtype_Definition}
          (Specification : @key[in] Asis.Declaration)
              @key[return] Asis.Discrete_Subtype_Definition;
@end{Example}

Specification @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
loop_parameter_specification or Entry_Index_Specification to query.

Returns the Discrete_Subtype_Definition of the specification.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Specification expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
A_Loop_Parameter_Specification
An_Entry_Index_Specification
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Definition_Kinds:
@begin{Display}
A_Discrete_Subtype_Definition
@end{Display}
@end{DescribeCode}


@LabeledClause{function Parameter_Profile}

@begin{ElementRef}
A_Procedure_Declaration @em 6.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Parameter_Profile
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Parameter_Profile} (Declaration : @key[in] Asis.Declaration)
                           @key[return] Asis.Parameter_Specification_List;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
subprogram or entry declaration to query.

Returns a list of parameter specifications in the formal part of the
subprogram or entry declaration, in their order of appearance.

Returns a Nil_Element_List if the subprogram or entry has no
parameters.

Results of this query may vary across ASIS implementations. Some
implementations normalize all multiple name parameter specifications into
an equivalent sequence of corresponding single name parameter
specifications. See Reference Manual 3.3.1(7).

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
A_Procedure_Declaration
A_Function_Declaration
A_Procedure_Body_Declaration
A_Function_Body_Declaration
A_Procedure_Renaming_Declaration
A_Function_Renaming_Declaration
An_Entry_Declaration
An_Entry_Body_Declaration
A_Procedure_Body_Stub
A_Function_Body_Stub
A_Generic_Function_Declaration
A_Generic_Procedure_Declaration
A_Formal_Function_Declaration
A_Formal_Procedure_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[a list of elements that
have one of ],Old=[]}Declaration_Kinds:
@begin{Display}
A_Parameter_Specification
@end{Display}
@end{DescribeCode}


*** Start obsolete code, should be replaced ***
@LabeledRevisedClause{Version=[2],New=[function Result_Subtype],Old=[function Result_Profile]}

@begin{ElementRef}
A_Function_Declaration @em 6.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Parameter_Profile
function Result_Profile
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Result_Profile} (Declaration : @key[in] Asis.Declaration)
                     @key[return] Asis.Expression;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the function
declaration to query.

Returns the subtype mark expression for the return type for any function
declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
A_Function_Declaration
A_Function_Body_Declaration
A_Function_Body_Stub
A_Function_Renaming_Declaration
A_Generic_Function_Declaration
A_Formal_Function_Declaration
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
A_Selected_Component
An_Attribute_Reference
@end{Display}
@end{DescribeCode}
*** End Obsolete code ***

@LabeledClause{function Body_Declarative_Items}

@begin{ElementRef}
A_Parameter_Specification @em 6.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Declaration_Subtype_Mark
function Initialization_Expression
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Procedure_Body_Declaration @em 6.3
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Parameter_Profile
function Body_Declarative_Items
function Body_Statements
function Body_Exception_Handlers
function Body_Block_Statement @em obsolescent, not recommended
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Body_Declarative_Items} (Declaration : @key[in] Asis.Declaration;
                                 Include_Pragmas : @key[in] Boolean := False)
                                @key[return] Asis.Element_List;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[    @en Specifies]} the body
declaration to query. Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en
Specifies]} whether pragmas are to be returned.

Returns a list of all basic declarations, representation specifications,
use clauses, and pragmas in the declarative part of the body, in their
order of appearance.

Returns a Nil_Element_List if there are no declarative_item or pragma elements.

Results of this query may vary across ASIS implementations. Some
implementations normalize all multi-name declarations into an
equivalent sequence of corresponding single name object declarations.
See Reference Manual 3.3.1(7).

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
A_Function_Body_Declaration
A_Procedure_Body_Declaration
A_Package_Body_Declaration
A_Task_Body_Declaration
An_Entry_Body_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[a list of elements that
have one of ],Old=[]}Element_Kinds:
@begin{Display}
A_Pragma
A_Declaration
A_Clause
@end{Display}
@end{DescribeCode}


@LabeledClause{function Body_Statements}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Body_Statements} (Declaration : @key[in] Asis.Declaration;
                          Include_Pragmas : @key[in] Boolean := False)
                         @key[return] Asis.Statement_List;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[    @en Specifies]} the body
declaration to query. Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en
Specifies]} whether pragmas are to be returned.

Returns a list of the statements and pragmas for the body, in
their order of appearance.

Returns a Nil_Element_List if there are no statements or pragmas.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
A_Function_Body_Declaration
A_Procedure_Body_Declaration
A_Package_Body_Declaration
A_Task_Body_Declaration
An_Entry_Body_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[a list of elements that each have
one of ],Old=[]}Element_Kinds:
@begin{Display}
A_Pragma
A_Statement
@end{Display}
@end{DescribeCode}


@LabeledClause{function Body_Exception_Handlers}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Body_Exception_Handlers} (Declaration : @key[in] Asis.Declaration;
                                  Include_Pragmas : @key[in] Boolean := False)
                                 @key[return] Asis.Exception_Handler_List;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[    @en Specifies]} the body
declaration to query. Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en
Specifies]} whether pragmas are to be returned.

Returns a list of the exception_handler elements of the body, in their order of
appearance.

The only pragmas returned are those following the reserved word @key[exception]
and preceding the reserved word @key[when] of first exception handler.

Returns a Nil_Element_List if there are no exception_handler or pragma elements.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
A_Function_Body_Declaration
A_Procedure_Body_Declaration
A_Package_Body_Declaration
A_Task_Body_Declaration
An_Entry_Body_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[a list of elements that each have
one of ],Old=[]}Element_Kinds:
@begin{Display}
An_Exception_Handler
A_Pragma
@end{Display}
@end{DescribeCode}


@LabeledRevisedClause{Version=[2],New=[obsolesent function 1],Old=[function Body_Block_Statement]}

@begin{ElementRef}
A_Function_Body_Declaration @em 6.3
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Parameter_Profile
function Result_Profile
function Body_Declarative_Items
function Body_Statements
function Body_Exception_Handlers
function Body_Block_Statement @em obsolescent, not recommended
@end{Display}
@end{ChildRef}

@ChgAdded{Version=[2],Text=[@b{@i{This clause header is left for now;
removing it now would change all of the clause numbers,
and that would make a mess for editing and reference purposes. Ultimately,
when the final standard is produced, it will be removed. - RLB}}]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[SI99-0027-1]}
@ChgDeleted{Version=[2],Text=[Function Body_Block_Statement is a new query
that supplies the equivalent combined functionality of the replaced queries:
Subprogram_Body_Block, Package_Body_Block, and Task_Body_Block.
Use of the query Body_Block_Statement is not recommended in new programs.
This functionality is redundant with the queries Body_Declarative_Items,
Body_Statements, and Body_Exception_Handlers.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[SI99-0027-1]}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Body_Block_Statement} (Declaration : @key[in] Asis.Declaration)
                              @key[return] Asis.Statement;]}
@end{Example}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[SI99-0027-1]}
@ChgDeleted{Version=[2],Text=[Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the program unit body to query.]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[SI99-0027-1]}
@ChgDeleted{Version=[2],Text=[Returns a block statement that is the structural
equivalent of the body. The block statement is not Is_Part_Of_Implicit. The
block includes the declarative part, the sequence of statements, and any
exception handlers.]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[SI99-0027-1]}
@ChgDeleted{Version=[2],Type=[Leading],Keepnext=[T],Text=[Appropriate Declaration_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[A_Function_Body_Declaration
A_Procedure_Body_Declaration
A_Package_Body_Declaration
A_Task_Body_Declaration
An_Entry_Body_Declaration]}
@end{Display}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[SI99-0027-1]}
@ChgDeleted{Version=[2],Type=[Leading],Keepnext=[T],Text=[Returns Statement_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[A_Block_Statement]}
@end{Display}
@end{DescribeCode}

@begin{SingleNote}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[SI99-0027-1]}
@ChgDeleted{Version=[2],Text=[This function is an obsolescent feature retained for compatibility with
ASIS 83. It is never called by Traverse_Element. Use of this query is
not recommended in new programs.]}
@end{SingleNote}


@LabeledClause{function Is_Name_Repeated (declaration)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Name_Repeated}
            (Declaration : @key[in] Asis.Declaration) @key[return] Boolean;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
declaration to query.

Returns True if the name of the declaration is repeated after the "end"
which terminates the declaration.

Returns False for any unexpected Element.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Expected]} Declaration_Kinds:
@begin{Display}
A_Package_Declaration
A_Package_Body_Declaration
A_Procedure_Body_Declaration
A_Function_Body_Declaration
A_Generic_Package_Declaration
A_Task_Type_Declaration
A_Single_Task_Declaration
A_Task_Body_Declaration
A_Protected_Type_Declaration
A_Single_Protected_Declaration
A_Protected_Body_Declaration
An_Entry_Body_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}
@end{DescribeCode}


@LabeledClause{function Corresponding_Declaration (declaration)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Declaration}
            (Declaration : @key[in] Asis.Declaration)
            @key[return] Asis.Declaration;

@key[function] @AdaSubDefn{Corresponding_Declaration}
            (Declaration : @key[in] Asis.Declaration;
             The_Context : @key[in] Asis.Context)
            @key[return] Asis.Declaration;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the
specification to query. The_Context @chg{Version=[1],New=[specifies],
Old=[   @en Specifies]} a Context to use.

Returns the corresponding specification of a subprogram, package, or task
body declaration. Returns the expanded generic specification template for
generic instantiations. The argument can be a Unit_Declaration from a
Compilation_Unit, or, it can be any appropriate body declaration from any
declarative context.

@leading@keepnext@;These two function calls will always produce identical
results:
@begin{ChildExample}
Decl2 := Corresponding_Declaration (Decl1);
Decl2 := Corresponding_Declaration
         (Decl1, Enclosing_Context (Enclosing_Compilation_Unit (Decl1)));
@end{ChildExample}

If a specification declaration is given, the same element is returned,
unless it is a generic instantiation or an inherited subprogram declaration
(see below).

@leading@keepnext@;If a subprogram renaming declaration is given:

@begin{enumerate}
in case of renaming-as-declaration, the same element is returned;

in case of renaming-as-body, the subprogram declaration completed
by this subprogram renaming declaration is returned.
(Reference Manual, 8.5.4(1))
@end{enumerate}

Returns a Nil_Element if no explicit specification exists, or the
declaration is the proper body of a subunit.

The parameter The_Context is used to locate the corresponding specification
within a particular Context. The_Context need not be the Enclosing_Context
of the Declaration. Any non-Nil result will always have The_Context
as its Enclosing_Context. This implies that while a non-Nil result may be
Is_Equal with the argument, it will only be Is_Identical if the
Enclosing_Context of the Declaration is the same as the parameter
The_Context.

If a generic instantiation is given, the expanded generic specification
template representing the instance is returned and Is_Part_Of_Instance.
For example, an argument that is A_Package_Instantiation, results in a
value that is A_Package_Declaration that can be analyzed with all
appropriate queries.

Retuns the declaration of the generic child unit corresponding to an
implicit generic child unit specification. Reference Manual 10.1.1(19).

The Enclosing_Element of the expanded specification is the generic
instantiation. The Enclosing_Compilation_Unit of the expanded template is
that of the instantiation.

If an inherited subprogram declaration is given, the specification
returned is the one for the user-defined subprogram from which the
argument was ultimately inherited.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds for returning a specification:
@begin{Display}
A_Function_Body_Declaration
A_Function_Renaming_Declaration (renaming-as-body)
A_Function_Body_Stub
A_Function_Instantiation
A_Package_Body_Declaration
A_Package_Body_Stub
A_Package_Instantiation
A_Procedure_Body_Declaration
A_Procedure_Renaming_Declaration (renaming-as-body)
A_Procedure_Body_Stub
A_Procedure_Instantiation
A_Task_Body_Declaration
A_Task_Body_Stub
A_Protected_Body_Declaration
A_Protected_Body_Stub
A_Formal_Package_Declaration
A_Formal_Package_Declaration_With_Box
An_Entry_Body_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds for returning the argument Declaration:
@begin{Display}
A_Function_Declaration
A_Function_Renaming_Declaration (renaming-as-declaration)
A_Generic_Function_Declaration
A_Generic_Package_Declaration
A_Generic_Procedure_Declaration
A_Package_Declaration
A_Package_Renaming_Declaration
A_Procedure_Declaration
A_Procedure_Renaming_Declaration (renaming-as-declaration)
A_Single_Task_Declaration
A_Task_Type_Declaration
A_Protected_Type_Declaration
A_Single_Protected_Declaration
A_Generic_Package_Renaming_Declaration
A_Generic_Procedure_Renaming_Declaration
A_Generic_Function_Renaming_Declaration
An_Entry_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Declaration_Kinds:
@begin{Display}
Not_A_Declaration
A_Function_Declaration
A_Function_Renaming_Declaration
A_Generic_Function_Declaration
A_Generic_Package_Declaration
A_Generic_Procedure_Declaration
A_Package_Declaration
A_Package_Renaming_Declaration
A_Procedure_Declaration
A_Procedure_Renaming_Declaration
A_Single_Task_Declaration
A_Task_Type_Declaration
A_Protected_Type_Declaration
A_Single_Protected_Declaration
An_Entry_Declaration
@end{Display}
@end{DescribeCode}


@LabeledClause{function Corresponding_Body (declaration)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Body} (Declaration : @key[in] Asis.Declaration)
                            @key[return] Asis.Declaration;

@key[function] @AdaSubDefn{Corresponding_Body} (Declaration : @key[in] Asis.Declaration;
                             The_Context : @key[in] Asis.Context)
                             @key[return] Asis.Declaration;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the specification to query.
The_Context @chg{Version=[1],New=[specifies],Old=[@en Specifies]} a Context to use.

Returns the corresponding body for a given subprogram, package, or task
specification declaration. Returns the expanded generic body template for
generic instantiations. The argument can be a Unit_Declaration from a
Compilation_Unit, or, it can be any appropriate specification declaration
from any declarative context.

@leading@keepnext@;These two function calls will always produce identical
results:
@begin{ChildExample}
Decl2 := Corresponding_Body (Decl1);
Decl2 := Corresponding_Body
         (Decl1, Enclosing_Context (Enclosing_Compilation_Unit (Decl1)));
@end{ChildExample}

If a body declaration is given, the same element is returned.

Returns a Nil_Element if no body exists in The_Context.

The parameter The_Context is used to locate the corresponding specification
within a particular Context. The_Context need not be the Enclosing_Context
of the Declaration. Any non-Nil result will always have The_Context
as its Enclosing_Context. This implies that while a non-Nil result may be
Is_Equal with the argument, it will only be Is_Identical if the
Enclosing_Context of the Declaration is the same as the parameter
The_Context.

Implicit predefined operations (e.g., "+", "=", etc.) will not typically
have unit bodies. (Corresponding_Body returns a Nil_Element.)
User-defined overloads of the predefined operations will have
Corresponding_Body values once the bodies have inserted into the
environment. The Corresponding_Body of an inherited subprogram is that
of the original user-defined subprogram.

If a generic instantiation is given, the body representing the expanded
generic body template is returned. (i.e., an argument that is
A_Package_Instantiation, results in a value that is
A_Package_Body_Declaration that can be analyzed with all appropriate ASIS queries).

Returns a Nil_Element if the body of the generic has not yet been compiled
or inserted into the Ada Environment Context.

The Enclosing_Element of the expanded body is the generic instantiation. The
Enclosing_Compilation_Unit of the expanded template is that of the instantiation.

Returns Nil_Element for an implicit generic child unit specification.
Reference Manual 10.1.1(19).

Returns A_Pragma if the Declaration is completed by pragma Import.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds returning a body:
@begin{Display}
A_Function_Declaration
A_Function_Instantiation
A_Generic_Package_Declaration
A_Generic_Procedure_Declaration
A_Generic_Function_Declaration
A_Package_Declaration
A_Package_Instantiation
A_Procedure_Declaration
A_Procedure_Instantiation
A_Single_Task_Declaration
A_Task_Type_Declaration
A_Protected_Type_Declaration
A_Single_Protected_Declaration
A_Formal_Package_Declaration
A_Formal_Package_Declaration_With_Box
An_Entry_Declaration (restricted to protected entry)
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds for returning the argument Declaration:
@begin{Display}
A_Function_Body_Declaration
A_Function_Body_Stub
A_Function_Renaming_Declaration
A_Package_Body_Declaration
A_Package_Body_Stub
A_Package_Renaming_Declaration
A_Procedure_Body_Declaration
A_Procedure_Renaming_Declaration
A_Procedure_Body_Stub
A_Task_Body_Declaration
A_Task_Body_Stub
A_Protected_Body_Declaration
A_Protected_Body_Stub
A_Generic_Package_Renaming_Declaration
A_Generic_Procedure_Renaming_Declaration
A_Generic_Function_Renaming_Declaration
An_Entry_Body_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Returns Declaration_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[Not_A_Declaration
A_Function_Body_Declaration
A_Function_Body_Stub
A_Function_Renaming_Declaration
A_Package_Body_Declaration
A_Package_Body_Stub
A_Procedure_Body_Declaration
A_Procedure_Renaming_Declaration
A_Procedure_Body_Stub
A_Task_Body_Declaration
A_Task_Body_Stub
A_Protected_Body_Declaration
A_Protected_Body_Stub
An_Entry_Body_Declaration]}
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Element_Kinds:
@begin{Display}
Not_An_Element
A_Declaration@Chg{Version=[2],New=[ that has one of Declaration_Kinds:
    Not_A_Declaration
    A_Function_Body_Declaration
    A_Function_Body_Stub
    A_Function_Renaming_Declaration
    A_Package_Body_Declaration
    A_Package_Body_Stub
    A_Procedure_Body_Declaration
    A_Procedure_Renaming_Declaration
    A_Procedure_Body_Stub
    A_Task_Body_Declaration
    A_Task_Body_Stub
    A_Protected_Body_Declaration
    A_Protected_Body_Stub
    An_Entry_Body_Declaration],Old=[]}
A_Pragma
@end{Display}
@end{DescribeCode}


@LabeledClause{function Corresponding_Subprogram_Derivation}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Subprogram_Derivation}
          (Declaration : @key[in] Asis.Declaration)
              @key[return] Asis.Declaration;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} an implicit
inherited subprogram declaration.

Returns the subprogram declaration from which the given implicit inherited
subprogram argument was inherited. The result can itself be an implicitly
inherited subprogram.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Element_Kinds:
@begin{Display}
@ChgRef{Version=[2],Kind=[Revised]}
A_Declaration@Chg{Version=[2],New=[ that has one of Declaration_Kinds:
    A_Function_Declaration
    A_Procedure_Declaration],Old=[]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Appropriate Declaration_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_Function_Declaration
A_Procedure_Declaration]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Element_Kinds:
@begin{Display}
@ChgRef{Version=[2],Kind=[Revised]}
A_Declaration@Chg{Version=[2],New=[ that has one of Declaration_Kinds:
    A_Function_Body_Declaration
    A_Function_Declaration
    A_Function_Renaming_Declaration
    A_Procedure_Body_Declaration
    A_Procedure_Declaration
    A_Procedure_Renaming_Declaration],Old=[]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Returns Declaration_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_Function_Body_Declaration
A_Function_Declaration
A_Function_Renaming_Declaration
A_Procedure_Body_Declaration
A_Procedure_Declaration
A_Procedure_Renaming_Declaration]}
@end{Display}

Raises ASIS_Inappropriate_Element for a subprogram declaration that is not
Is_Part_Of_Inherited.
@end{DescribeCode}


@LabeledClause{function Corresponding_Type}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Type} (Declaration : @key[in] Asis.Declaration)
                            @key[return] Asis.Type_Definition;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
subprogram_declaration to query.

Returns the type definition for which this entity is an implicit
declaration. The result will often be a derived type. However, this query
also works for declarations of predefined operators such as "+" and "=".
Raises ASIS_Inappropriate_Element if the argument is not an implicit
declaration resulting from the declaration of a type.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Declaration@Chg{Version=[2],New=[ that has one of Declaration_Kinds:
    A_Function_Declaration
    A_Procedure_Declaration],Old=[]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Appropriate Declaration_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_Function_Declaration
A_Procedure_Declaration]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Definition_Kinds:
@begin{Display}
A_Type_Definition
A_Formal_Type_Definition
@end{Display}
@end{DescribeCode}


@LabeledClause{function Corresponding_Equality_Operator}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Equality_Operator}
         (Declaration : @key[in] Asis.Declaration) @key[return] Asis.Declaration;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} an equality
or an inequality operator declaration.

@leading@;If given an explicit Declaration of "=" whose result type is Boolean:

@begin{Itemize}
Returns the complimentary implicit "/=" operator declaration.

Returns a Nil_Element if the Ada implementation has not defined an
implicit "/=" for the "=". Implementations of this sort will transform
a A/=B expression into a @key[not](A=B) expression. The function call
representing the @key[not] operation is Is_Part_Of_Implicit in this case.
@end{Itemize}

@leading@;If given an implicit declaration of "/=" whose result type is Boolean:

@begin{Itemize}
Returns the complimentary explicit "=" operator declaration.
@end{Itemize}

Returns a Nil_Element for any other function declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
A_Function_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Declaration_Kinds:
@begin{Display}
A_Function_Declaration
@end{Display}
@end{DescribeCode}


@LabeledClause{function Visible_Part_Declarative_Items}

@begin{ElementRef}
A_Package_Declaration @em 7.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Visible_Part_Declarative_Items
function Private_Part_Declarative_Items
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Visible_Part_Declarative_Items}
            (Declaration     : @key[in] Asis.Declaration;
             Include_Pragmas : @key[in] Boolean := False)
            @key[return] Asis.Declarative_Item_List;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[    @en Specifies]} the
package to query.
Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether
pragmas are to be returned.

Returns a list of all basic declarations, representation specifications,
use clauses, and pragmas in the visible part of a package, in their order
of appearance.

Results of this query may vary across ASIS implementations. Some
implementations normalize all multi-name object declarations into an
equivalent sequence of corresponding single name object declarations.
See Reference Manual 3.3.1(7).

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[]} Declaration_Kinds:
@begin{Display}
A_Generic_Package_Declaration
A_Package_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[a list of elements that each have
one of ],Old=[]}Element_Kinds:
@begin{Display}
A_Declaration
A_Pragma
A_Clause
@end{Display}
@end{DescribeCode}


@LabeledClause{function Is_Private_Present (declaration)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Private_Present}
             (Declaration : @key[in] Asis.Declaration) @key[return] Boolean;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
declaration to query.

Returns True if the argument is a package specification which has a reserved
word @key[private] which marks the beginning of a (possibly empty) private part.

Returns False for any package specification without a private part.
Returns False for any unexpected Element.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Expected]} Element_Kinds:
@begin{Display}
A_Declaration@Chg{Version=[2],New=[ that has one of Declaration_Kinds:
    A_Generic_Package_Declaration
    A_Package_Declaration],Old=[]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Appropriate Declaration_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_Generic_Package_Declaration
A_Package_Declaration]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}
@end{DescribeCode}


@LabeledClause{function Private_Part_Declarative_Items}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Private_Part_Declarative_Items}
            (Declaration     : @key[in] Asis.Declaration;
             Include_Pragmas : @key[in] Boolean := False)
            @key[return] Asis.Declarative_Item_List;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[    @en Specifies]} the
package to query.
Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether
pragmas are to be returned.

Returns a list of all basic declarations, representation specifications,
use clauses, and pragmas in the private part of a package in their order of
appearance.

Results of this query may vary across ASIS implementations. Some
implementations normalize all multi-name object declarations into an
equivalent sequence of corresponding single name object declarations.
See Reference Manual 3.3.1(7).

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
A_Generic_Package_Declaration
A_Package_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[a list of elements that each have
one of ],Old=[]}Element_Kinds:
@begin{Display}
A_Declaration
A_Pragma
A_Clause
@end{Display}
@end{DescribeCode}


@LabeledClause{function Renamed_Entity}

@begin{ElementRef}
A_Package_Body_Declaration @em 7.2
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Body_Declarative_Items
function Body_Statements
function Body_Exception_Handlers
function Body_Block_Statement @em obsolescent, not recommended
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Private_Type_Declaration @em 7.3@*
A_Private_Extension_Declaration @em 7.3
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Discriminant_Part
function Type_Declaration_View
@end{Display}
@end{ChildRef}

@begin{ElementRef}
An_Object_Renaming_Declaration @em 8.5.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Declaration_Subtype_Mark
function Renamed_Entity
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Renamed_Entity} (Declaration : @key[in] Asis.Declaration)
                        @key[return] Asis.Expression;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the rename
declaration to query.

Returns the name expression that follows the reserved word @key[renames] in the
renaming declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
An_Exception_Renaming_Declaration
A_Function_Renaming_Declaration
An_Object_Renaming_Declaration
A_Package_Renaming_Declaration
A_Procedure_Renaming_Declaration
A_Generic_Package_Renaming_Declaration
A_Generic_Procedure_Renaming_Declaration
A_Generic_Function_Renaming_Declaration
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


@LabeledClause{function Corresponding_Base_Entity}

@begin{ElementRef}
An_Exception_Renaming_Declaration @em 8.5.2@*
A_Package_Renaming_Declaration @em 8.5.3
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Renamed_Entity
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Procedure_Renaming_Declaration @em 8.5.4
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Parameter_Profile
function Renamed_Entity
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Function_Renaming_Declaration @em 8.5.4
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Parameter_Profile
function Result_Profile
function Renamed_Entity
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Generic_Package_Renaming_Declaration @em 8.5.5@*
A_Generic_Procedure_Renaming_Declaration @em 8.5.5@*
A_Generic_Function_Renaming_Declaration @em 8.5.5
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Renamed_Entity
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Base_Entity} (Declaration : @key[in] Asis.Declaration)
                             @key[return] Asis.Expression;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the rename
declaration to query.

The base entity is defined to be the renamed entity that is not itself
defined by another renaming declaration.

If the name following the reserved word @key[renames] is itself declared
by a previous renaming_declaration, then this query unwinds the renamings
by recursively operating on the previous renaming_declaration.

Otherwise, the name following the reserved word @key[renames] is returned.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
An_Object_Renaming_Declaration
An_Exception_Renaming_Declaration
A_Procedure_Renaming_Declaration
A_Function_Renaming_Declaration
A_Package_Renaming_Declaration
A_Generic_Package_Renaming_Declaration
A_Generic_Procedure_Renaming_Declaration
A_Generic_Function_Renaming_Declaration
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


@LabeledClause{function Protected_Operation_Items}

@begin{ElementRef}
A_Task_Type_Declaration @em 9.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Discriminant_Part
function Type_Declaration_View
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Single_Task_Declaration @em 9.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Object_Declaration_View
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Task_Body_Declaration @em 9.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Body_Declarative_Items
function Body_Statements
function Body_Exception_Handlers
function Body_Block_Statement @em obsolescent, not recommended
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Protected_Type_Declaration @em 9.4
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Discriminant_Part
function Type_Declaration_View
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Single_Protected_Declaration @em 9.4
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Object_Declaration_View
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Protected_Body_Declaration @em 9.4
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Protected_Operation_Items
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Protected_Operation_Items}
              (Declaration     : @key[in] Asis.Declaration;
               Include_Pragmas : @key[in] Boolean := False)
              @key[return] Asis.Declaration_List;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[    @en Specifies]} the
protected_body declaration to query.
Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether
pragmas are to be returned.

Returns a list of protected_operation_item and pragma elements of the
protected_body, in order of appearance.

Returns a Nil_Element_List if there are no items or pragmas.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
A_Protected_Body_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[a list of elements that each have
one of ],Old=[]}Element_Kinds:
@begin{Display}
A_Pragma
A_Declaration@Chg{Version=[2],New=[ that has one of Declaration_Kinds:
    A_Procedure_Declaration
    A_Function_Declaration
    A_Procedure_Body_Declaration
    A_Function_Body_Declaration
    An_Entry_Body_Declaration],Old=[]}
A_Clause@Chg{Version=[2],New=[ that has one of Clause_Kinds:
    A_Representation_Clause],Old=[]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Returns Declaration_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_Procedure_Declaration
A_Function_Declaration
A_Procedure_Body_Declaration
A_Function_Body_Declaration
An_Entry_Body_Declaration]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Returns Clause_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_Representation_Clause]}
@end{Display}
@end{DescribeCode}


@LabeledClause{function Entry_Family_Definition}

@begin{ElementRef}
An_Entry_Declaration @em 9.5.2
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Entry_Family_Definition
function Parameter_Profile
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Entry_Family_Definition} (Declaration : @key[in] Asis.Declaration)
                                 @key[return] Asis.Discrete_Subtype_Definition;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the entry
declaration to query.

Returns the Discrete_Subtype_Definition element for the entry family of
an entry_declaration.

Returns a Nil_Element if the entry_declaration does not define a family
of entries.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
An_Entry_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Definition_Kinds:
@begin{Display}
Not_A_Definition
A_Discrete_Subtype_Definition
@end{Display}
@end{DescribeCode}


@LabeledClause{function Entry_Index_Specification}

@begin{ElementRef}
An_Entry_Body_Declaration @em 9.5.2
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Entry_Index_Specification
function Parameter_Profile
function Entry_Barrier
function Body_Declarative_Items
function Body_Statements
function Body_Exception_Handlers
function Body_Block_Statement @em obsolescent, not recommended
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Entry_Index_Specification} (Declaration : @key[in] Asis.Declaration)
                                   @key[return] Asis.Declaration;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the entry
body declaration to query.

Returns the An_Entry_Index_Specification element of an entry body
declaration.

Returns a Nil_Element if the entry does not declare any
An_Entry_Index_Specification element.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
An_Entry_Body_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Declaration_Kinds:
@begin{Display}
Not_A_Declaration
An_Entry_Index_Specification
@end{Display}
@end{DescribeCode}


@LabeledClause{function Entry_Barrier}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Entry_Barrier} (Declaration : @key[in] Asis.Declaration)
                       @key[return] Asis.Expression;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the entry
body declaration to query.

Returns the expression following the reserved word @key[when] in an entry body
declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
An_Entry_Body_Declaration
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


@LabeledClause{function Corresponding_Subunit}

@begin{ElementRef}
A_Procedure_Body_Stub @em 10.1.3
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Parameter_Profile
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Function_Body_Stub @em 10.1.3
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Parameter_Profile
function Result_Profile
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Package_Body_Stub @em 10.1.3@*
A_Task_Body_Stub @em 10.1.3@*
A_Protected_Body_Stub @em 10.1.3
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Subunit} (Body_Stub : @key[in] Asis.Declaration)
                 @key[return] Asis.Declaration;

@key[function] @AdaSubDefn{Corresponding_Subunit} (Body_Stub   : @key[in] Asis.Declaration;
                                The_Context : @key[in] Asis.Context)
                 @key[return] Asis.Declaration;
@end{Example}

Body_Stub @chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the stub to
query. The_Context @chg{Version=[1],New=[specifies],Old=[@en Specifies]} a
Context to use to locate the subunit.

Returns the Unit_Declaration of the subunit compilation unit corresponding
to the body stub.

Returns a Nil_Element if the subunit does not exist in The_Context.

@leading@;These two function calls will always produce identical results:
@begin{ChildExample}
Decl2 := Corresponding_Subunit (Decl1);
Decl2 := Corresponding_Subunit
         (Decl1, Enclosing_Context (Enclosing_Compilation_Unit (Decl1));
@end{ChildExample}

The parameter The_Context is used to locate the corresponding subunit body.
Any non-Nil result will always have The_Context as its Enclosing_Context.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Body_Stub expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
A_Function_Body_Stub
A_Package_Body_Stub
A_Procedure_Body_Stub
A_Task_Body_Stub
A_Protected_Body_Stub
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Declaration_Kinds:
@begin{Display}
Not_A_Declaration
A_Function_Body_Declaration
A_Package_Body_Declaration
A_Procedure_Body_Declaration
A_Task_Body_Declaration
A_Protected_Body_Declaration
@end{Display}
@end{DescribeCode}


@LabeledClause{function Is_Subunit}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Subunit} (Declaration : @key[in] Asis.Declaration) @key[return] Boolean;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
declaration to query.

Returns True if the declaration is the proper_body of a subunit.

Returns False for any unexpected Element.

@leading@keepnext@;Equivalent to:
@begin{ChildExample}
Declaration = Unit_Declaration (Enclosing_Compilation_Unit (Declaration))
@key[and] Unit_Kind (Enclosing_Compilation_Unit (Declaration)) @key[in] A_Subunit
@end{ChildExample}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Expected]} Declaration_Kinds:
@begin{Display}
A_Procedure_Body_Declaration
A_Function_Body_Declaration
A_Package_Body_Declaration
A_Task_Body_Declaration
A_Protected_Body_Declaration
@end{Display}
@end{DescribeCode}


@LabeledClause{function Corresponding_Body_Stub}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Body_Stub} (Subunit : @key[in] Asis.Declaration)
                   @key[return] Asis.Declaration;

@key[function] @AdaSubDefn{Corresponding_Body_Stub} (Subunit     : @key[in] Asis.Declaration;
                                  The_Context : @key[in] Asis.Context)
                   @key[return] Asis.Declaration;
@end{Example}

Subunit @chg{Version=[1],New=[specifies],Old=[    @en Specifies]} the
Is_Subunit declaration to query. The_Context
@chg{Version=[1],New=[specifies],Old=[@en Specifies]} a Context to use to
locate the parent unit.

Returns the body stub declaration located in the subunit's parent unit.

Returns a Nil_Element if the parent unit does not exist in The_Context.

@leading@;These two function calls will always produce identical results:
@begin{ChildExample}
Decl2 := Corresponding_Body_Stub (Decl1);
Decl2 := Corresponding_Body_Stub
         (Decl1, Enclosing_Context (Enclosing_Compilation_Unit (Decl1)));
@end{ChildExample}

The parameter The_Context is used to locate the corresponding parent body.
Any non-Nil result will always have The_Context as its Enclosing_Context.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Subunit expects an element
of],Old=[Appropriate]} Declaration_Kinds
@begin{Display}
(Is_Subunit(Declaration) shall also be True)
A_Function_Body_Declaration
A_Package_Body_Declaration
A_Procedure_Body_Declaration
A_Task_Body_Declaration
A_Protected_Body_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Declaration_Kinds:
@begin{Display}
Not_A_Declaration
A_Function_Body_Stub
A_Package_Body_Stub
A_Procedure_Body_Stub
A_Task_Body_Stub
A_Protected_Body_Stub
@end{Display}
@end{DescribeCode}


@LabeledClause{function Generic_Formal_Part}

@begin{ElementRef}
An_Exception_Declaration @em 11.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Choice_Parameter_Specification @em 11.2
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Generic_Procedure_Declaration @em 12.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Generic_Formal_Part
function Names
function Parameter_Profile
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Generic_Formal_Part}
            (Declaration     : @key[in] Asis.Declaration;
             Include_Pragmas : @key[in] Boolean := False)
            @key[return] Asis.Element_List;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the generic
declaration to query. Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en
Specifies]} whether pragmas are to be returned.

Returns a list of generic formal parameter declarations, use clauses,
and pragmas, in their order of appearance.

Results of this query may vary across ASIS implementations. Some
implementations normalize all multi-name object declarations into an
equivalent sequence of corresponding single name object declarations.
See Reference Manual 3.3.1(7).

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
A_Generic_Package_Declaration
A_Generic_Procedure_Declaration
A_Generic_Function_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[a a list of elements that each have
one of ],Old=[]}Element_Kinds:
@begin{Display}
A_Pragma
A_Declaration
A_Clause
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Declaration_Kinds is A_Declaration:
@begin{Display}
A_Formal_Object_Declaration
A_Formal_Type_Declaration
A_Formal_Procedure_Declaration
A_Formal_Function_Declaration
A_Formal_Package_Declaration
A_Formal_Package_Declaration_With_Box
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Clause_Kinds if A_Clause:
@begin{Display}
A_Use_Package_Clause
A_Use_Type_Clause
@end{Display}
@end{DescribeCode}


@LabeledClause{function Generic_Unit_Name}

@begin{ElementRef}
A_Generic_Function_Declaration @em 12.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Generic_Formal_Part
function Names
function Parameter_Profile
function Result_Profile
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Generic_Package_Declaration @em 12.1
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Generic_Formal_Part
function Names
function Visible_Part_Declarative_Items
function Private_Part_Declarative_Items
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Package_Instantiation @em 12.3@*
A_Procedure_Instantiation @em 12.3@*
A_Function_Instantiation  @em 12.3
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Generic_Unit_Name
function Generic_Actual_Part
@end{Display}
@end{ChildRef}

@begin{Intro}
@b{@i{I don't know where this introductory text goes; it doesn't seem to
relate very closely to any of the routines near here. It surely does not
belong here, however. - RLB@*}}
Instantiations can always be analyzed in terms of the generic actual
parameters supplied with the instantiation. A generic instance is a copy
of the generic unit, and while there is no explicit (textual) specification
in the program text, an implicit specification and body, if there is one,
with the generic actual parameters is implied.

@leading@;To analyze the implicit instance specification or body of a generic
instantiation:
@begin{Itemize}
Use Corresponding_Declaration to return the implicit expanded
specification of an instantiation.

Use Corresponding_Body to return the implicit body of an instantiation.

Then analyze the specification or body with any appropriate queries.
@end{Itemize}

@leading@;To analyze the explicit generic specification or body referenced by a
generic instantiation:
@begin{Itemize}
Use Generic_Unit_Name to obtain the name of the generic unit.

Then use Corresponding_Name_Declaration to get to the generic declaration.

Then use Corresponding_Body to get to the body of the generic declaration.
@end{Itemize}
@end{Intro}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Generic_Unit_Name} (Declaration : @key[in] Asis.Declaration)
                           @key[return] Asis.Expression;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the generic
instantiation to query.

Returns the name following the reserved word @key[new] in the generic
instantiation. The name denotes the generic package, generic procedure, or
generic function that is the template for this generic instance.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
A_Function_Instantiation
A_Package_Instantiation
A_Procedure_Instantiation
A_Formal_Package_Declaration
A_Formal_Package_Declaration_With_Box
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
An_Operator_Symbol
A_Selected_Component
@end{Display}
@end{DescribeCode}


@LabeledClause{function Generic_Actual_Part}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Generic_Actual_Part} (Declaration : @key[in] Asis.Declaration;
                              Normalized  : @key[in] Boolean := False)
                             @key[return] Asis.Association_List;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
generic_instantiation to query.
Normalized @chg{Version=[1],New=[specifies],Old=[ @en Specifies]} whether the
normalized form is desired.

Returns a list of the generic_association elements of the instantiation.

Returns a Nil_Element_List if there are no generic_association elements.

An unnormalized list contains only explicit associations ordered as they
appear in the program text. Each unnormalized association has an optional
generic_formal_parameter_selector_name and an
explicit_generic_actual_parameter component.

A normalized list contains artificial associations representing all
explicit and default associations. It has a length equal to the number of
generic_formal_parameter_declaration elements of the generic_formal_part of the
template. The order of normalized associations matches the order of the
generic_formal_parameter_declaration elements.

@leading@;Each normalized association represents a one-on-one mapping of a
generic_formal_parameter_declaration to the explicit or default expression
or name. A normalized association has:
@begin{Itemize}
one A_Defining_Name component that denotes the
generic_formal_parameter_declaration, and

@leading@;one An_Expression component that is either:
@begin{InnerItemize}
the explicit_generic_actual_parameter,

a default_expression, or

a default_name from the generic_formal_parameter_declaration or an implicit
naming expression which denotes the actual subprogram selected at the place of
instantiation for a formal subprogram having A_Box_Default.
@end{InnerItemize}
@end{Itemize}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
A_Function_Instantiation
A_Package_Instantiation
A_Procedure_Instantiation
A_Formal_Package_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[a list of elements that each have
one of ],Old=[]}Association_Kinds:
@begin{Display}
A_Generic_Association
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
to use the defining_identifier element rather than the
generic_formal_parameter_selector_name elements.

In either case, this query will return Is_Normalized associations even if
Normalized is False, and the query Generic_Actual_Part_Normalized will
return True.
@end{ImplPerm}


@LabeledClause{function Formal_Subprogram_Default}

@begin{ElementRef}
A_Formal_Object_Declaration @em 12.4
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Declaration_Subtype_Mark
function Initialization_Expression
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Formal_Type_Declaration @em 12.5
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Discriminant_Part
function Type_Declaration_View
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Formal_Procedure_Declaration @em 12.6
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
functions Names
function Parameter_Profile
function Formal_Subprogram_Default
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Formal_Subprogram_Default}
            (Declaration : @key[in] Asis.Generic_Formal_Parameter)
           @key[return] Asis.Expression;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the generic
formal subprogram declaration to query.

Returns the name appearing after the reserved word @key[is] in the given
generic formal subprogram declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Appropriate]} Declaration_Kinds:
@begin{Display}
A_Formal_Function_Declaration
A_Formal_Procedure_Declaration
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[or Declaration expects an element
of],Old=[Appropriate]} Subprogram_Default_Kinds:
@begin{Display}
A_Name_Default
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


@LabeledClause{function Corresponding_Generic_Element}

@begin{ElementRef}
A_Formal_Function_Declaration @em 12.6
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Parameter_Profile
function Result_Profile
function Formal_Subprogram_Default
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Formal_Package_Declaration @em 12.7
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Generic_Unit_Name
function Generic_Actual_Part
@end{Display}
@end{ChildRef}

@begin{ElementRef}
A_Formal_Package_Declaration_With_Box @em 12.7
@end{ElementRef}

@begin{ChildRef}@ @;
@begin{Display}
function Names
function Generic_Unit_Name
@end{Display}
@end{ChildRef}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Generic_Element} (Reference : @key[in] Asis.Element)
                                       @key[return] Asis.Defining_Name;
@end{Example}

Reference @chg{Version=[1],New=[specifies],Old=[  @en Specifies]} an expression
that references an entity declared within the implicit specification of a
generic instantiation, or, specifies the defining name of such an entity.

Given a reference to some implicit entity, whose declaration occurs within
an implicit generic instance, returns the corresponding entity name
definition from the generic template used to create the generic instance.
(Reference Manual 12.3 (16))

Returns the first A_Defining_Name, from the generic template, that
corresponds to the entity referenced.

Returns a Nil_Element if the argument does not refer to an entity declared
as a component of a generic package instantiation. The entity name can
refer to an ordinary declaration, an inherited subprogram declaration, or a
predefined operator declaration.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Reference expects an element
of],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Defining_Name
An_Expression@Chg{Version=[2],New=[ that has one of Expression_Kinds:
    An_Identifier
    An_Operator_Symbol
    A_Character_Literal
    An_Enumeration_Literal],Old=[]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Appropriate Expression_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[An_Identifier
An_Operator_Symbol
A_Character_Literal
An_Enumeration_Literal]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element with
one of ],Old=[]}Expression_Kinds:
@begin{Display}
Not_An_Element
A_Defining_Name
@end{Display}
@end{DescribeCode}


@LabeledClause{function Is_Dispatching_Operation}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Dispatching_Operation} (Declaration : @key[in] Asis.Element)
                                      @key[return] Boolean;
@end{Example}

Declaration @chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
declaration to query.

Returns True if the declaration is a primitive subprogram of a tagged type.

Returns False for any unexpected argument.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Declaration expects an element
of],Old=[Expected]} Element_Kinds:
Element_Kinds:
@begin{Display}
A_Procedure_Declaration
A_Function_Declaration
A_Procedure_Renaming_Declaration
A_Function_Renaming_Declaration
@end{Display}
@end{DescribeCode}

@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[end] Asis.Declarations;]}
@end{Example}


@ChgNote{ SI99-006-1 }
@LabeledAddedClause{Version=[2],Name=[function Progenitor_List (declaration)]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0006-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Progenitor_List}
     (Declaration : @key[in] Asis.Definition)
      @key[return] Asis.Name_List;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0006-1]}
@ChgAdded{Version=[2],Text=[Declaration specifies the declaration to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0006-1]}
@ChgAdded{Version=[2],Text=[Returns a list of subtype marks making up the
interface_list in the argument declaration, in their order of appearance.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0006-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Keepnext=[T],Type=[Leading],Text=[
Declaration expects an element of Declaration_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Private_Extension_Declaration
A_Private_Type_Declaration
A_Task_Type_Declaration
A_Protected_Type_Declaration
A_Single_Task_Declaration
A_Single_Protected_Declaration]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0006-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Keepnext=[T],Type=[Leading],Text=[Returns a list
of elements that each have one of Expression_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[An_Identifier
A_Selected_Component]}
@end{Display}
@end{DescribeCode}


@ChgNote{ SI99-0003-1 }
@LabeledAddedClause{Version=[2],Name=[function Overriding_Indicator_Kind]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0003-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Overriding_Indicator_Kind} -- @examcom{8.3.1 (2)}
	(Declaration : @key[in] Asis.Declaration)
        @key[return] Overriding_Indicator_Kinds;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Declaration specifies the subprogram declaration
to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Returns the kind of Overriding_Indicator for the
subprogram declaration.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Returns Not_An_Overriding_Indicator for any
unexpected Element.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0003-1],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Keepnext=[T],Type=[Leading],Text=[
Declaration expects an element of Declaration_Kinds:]}
@begin{Display}
@ChgAdded{Version=[2],Text=[A_Procedure_Declaration
A_Function_Declaration
A_Procedure_Body_Declaration
A_Function_Body_Declaration
A_Null_Procedure_Declaration
A_Procedure_Renaming_Declaration
A_Function_Renaming_Declaration
An_Entry_Declaration
A_Procedure_Body_Stub
A_Function_Body_Stub
A_Procedure_Instantiation
A_Function_Instantiation ]}
@end{Display}
@end{DescribeCode}


