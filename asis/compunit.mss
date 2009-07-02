@Part(compunit, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/compunit.mss,v $}
@comment{$Revision: 1.14 $ $Date: 2009/05/16 03:55:40 $}


@LabeledSection{package Asis.Compilation_Units}


@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis],Child=[Compilation_Units]}Asis.Compilation_Units
shall exist. The package
shall provide interfaces equivalent to those described in the
following subclauses.],
Old=[@f{@key[with] Asis.Ada_Environments.Containers;@*
@key[package] @ChildUnit{Parent=[Asis],Child=[Compilation_Units]}Asis.Compilation_Units @key[is]}]}

Asis.Compilation_Units encapsulates a set of queries that implement the
ASIS Compilation_Unit abstraction.

More than one compilation unit may be manipulated at one time. (The exact
number is subject to implementation specific limitations.)

A specific Compilation_Unit value is valid (usable) for as long as the ASIS
Context variable used to create it remains open. Once an ASIS Context is
closed, all associated Compilation_Unit values become invalid. It is
erroneous to use an invalid Compilation_Unit value.


@LabeledClause{function Unit_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Unit_Kind} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
           @key[return] Asis.Unit_Kinds;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
compilation unit to query.

Returns the Unit_Kinds value of the compilation unit.
Returns Not_A_Unit for a Nil_Compilation_Unit.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects any kind of unit],
Old=[All Unit_Kinds are expected]}.

Returns An_Unknown_Unit for any compilation unit that exists, but that
does not have semantic element information available through ASIS.

@leading@;Returns a nonexistent kind for units that have name-only entries in
the environment Context. Such entries may exist for names because:

@begin{Itemize}
They represent an illegal compilation unit added to the environment.

They are referenced by some existing unit, but the program text for the
referenced unit has never been supplied, compiled, or otherwise
inserted into the environment.

They represent a separate subunit that has never been supplied,
compiled, or otherwise inserted into the environment.

The unit may have existed at one time but the semantic information is no
longer available. It may be inconsistent, have been removed by some
user or Ada environment operations, or simply have been lost as the
result of some sort of failure.
@end{Itemize}
@end{DescribeCode}


@LabeledClause{function Unit_Class}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Unit_Class} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
           @key[return] Asis.Unit_Classes;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the compilation unit to query.

Returns the Unit_Classes value of the compilation unit.
Returns Not_A_Class for a Nil_Compilation_Unit.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects any kind of unit],
Old=[All Unit_Kinds are expected]}.
@end{DescribeCode}


@LabeledClause{function Unit_Origin}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Unit_Origin} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
           @key[return] Asis.Unit_Origins;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the compilation unit to query.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0037-1]}
Returns the Unit_Origins value of the unit.
Returns Not_An_Origin for a compilation_unit whose Unit_Kind is
Not_A_Unit,
@Chg{Version=[2],New=[],Old=[An_Unknown_Unit, ]}A_Nonexistent_Declaration,
@Chg{Version=[2],New=[],Old=[or ]}A_Nonexistent_Body@Chg{Version=[2],New=[, or
possibly An_Unknown_Unit],Old=[]}.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects any kind of unit],
Old=[All Unit_Kinds are expected]}.
@end{DescribeCode}

@begin{ImplPerm}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0037-1]}
@ChgAdded{Version=[2],Text=[ASIS queries on the private part and bodies of
Compilation_Units whose Unit_Origin is other than An_Application_Unit may have
limitations that are implementation-defined. Operations of package Asis.Text on
Compilation_Units whose Unit_Origin is other than An_Application_Unit may have
limitations that are implementation-defined.]}
@end{ImplPerm}


@LabeledClause{function Enclosing_Context (unit)}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Enclosing_Context} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
           @key[return] Asis.Context;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the unit whose Context is required.

Returns the Context containing the compilation unit.

Compilation units always remember the ASIS Context and Container from
which they were obtained.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[],Old=[Because Context is limited private, this function
is only intended to be used to supply a Context parameter for other
queries. ]}This @Chg{Version=[2],New=[function can be used to
eliminate],Old=[conveniently eliminates]} the need to make the original Context
visible at the place of each call where a Context parameter is required.

Two Compilation_Unit values, that represent the same physical compilation
units (same Ada implementor Context implementation unit value) will test as
Is_Equal, but not Is_Identical, if they were obtained from different open
ASIS Context variables.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
Raises ASIS_Inappropriate_Compilation_Unit @ChgAdded{Version=[2],Text=[with
a Status of Value_Error]} if the unit is a Nil_Compilation_Unit.
@end{DescribeCode}


@LabeledClause{function Enclosing_Container}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Enclosing_Container} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
           @key[return] Asis.Ada_Environments.Containers.Container;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the unit whose Container is required.

Returns the Container of the Context containing the compilation unit.
Compilation units always remember the ASIS Context and Container from
which they were obtained.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
Raises ASIS_Inappropriate_Compilation_Unit @ChgAdded{Version=[2],Text=[with
a Status of Value_Error]} if the unit is a Nil_Compilation_Unit.
@end{DescribeCode}


@LabeledClause{function Library_Unit_Declaration}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Library_Unit_Declaration} (Name        : @key[in] Wide_String;
                                   The_Context : @key[in] Asis.Context)
                                    @key[return] Asis.Compilation_Unit;
@end{Example}

Name @Chg{Version=[1],New=[specifies],Old=[       @en Specifies]} the defining program unit name.
The_Context @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} a program Context environment.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
Returns the library_unit_declaration or library_unit_renaming_declaration
with the @Chg{Version=[2],New=[specified Name],Old=[name]}, contained in
The_Context.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
This query will never return a unit with A_Configuration_Compilation or
a nonexistent unit kind. It will never return a unit with A_Procedure_Body or
A_Function_Body unit kind even though the unit is interpreted as both the
declaration and body of a library procedure or library function.
(@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} 10.1.4(4).)

A Nil_Compilation_Unit is returned if no such declaration exists.

Any non-Nil result will have an Enclosing_Context value that Is_Identical
to the Context. Never returns a unit with a nonexistent unit kind.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Context with a Status of
Value_Error if The_Context is not open.]}
@end{DescribeCode}


@LabeledClause{function Compilation_Unit_Body}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Compilation_Unit_Body} (Name        : @key[in] Wide_String;
                                The_Context : @key[in] Asis.Context)
                                @key[return] Asis.Compilation_Unit;
@end{Example}

Name @Chg{Version=[1],New=[specifies],Old=[       @en Specifies]} the defining_program_unit_name.
The_Context @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} a program Context environment.

Returns the library_unit_body or subunit with the name, contained
in the library.

A Nil_Compilation_Unit is returned if no such body exists.

Any non-Nil result will have an Enclosing_Context value that Is_Identical
to The_Context. Never returns a unit with a nonexistent unit kind.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Context with a Status of
Value_Error if The_Context is not open.]}
@end{DescribeCode}


@LabeledClause{function Library_Unit_Declarations (context)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Library_Unit_Declarations} (The_Context : @key[in] Asis.Context)
                                   @key[return] Asis.Compilation_Unit_List;
@end{Example}

The_Context @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} a program Context environment.

Returns a list of all library_unit_declaration and
library_unit_renaming_declaration elements contained in The_Context. Individual
units will appear only once in an order that is not defined.

A Nil_Compilation_Unit_List is returned if there are no declarations of
library units within The_Context.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
This query will never return a unit with A_Configuration_Compilation or
a nonexistent unit kind. It will never return a unit with A_Procedure_Body or
A_Function_Body unit kind even though the unit is interpreted as both the
declaration and body of a library procedure or library function.
(@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} 10.1.4(4).)

All units in the result will have an Enclosing_Context value that
Is_Identical to The_Context.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Context with a Status of
Value_Error if The_Context is not open.]}
@end{DescribeCode}


@LabeledClause{function Compilation_Unit_Bodies (context)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Compilation_Unit_Bodies} (The_Context : @key[in] Asis.Context)
                                 @key[return] Asis.Compilation_Unit_List;
@end{Example}

The_Context @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} a program Context environment.

Returns a list of all library_unit_body and subunit elements contained in
The_Context. Individual units will appear only once in an order that is not
defined.

A Nil_Compilation_Unit_List is returned if there are no bodies within
The_Context.

This query will never return a unit with A_Configuration_Compilation or
a nonexistent unit kind.

All units in the result will have an Enclosing_Context value that
Is_Identical to The_Context.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Context with a Status of
Value_Error if The_Context is not open.]}
@end{DescribeCode}


@LabeledClause{function Compilation_Units (context)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Compilation_Units} (The_Context : @key[in] Asis.Context)
                           @key[return] Asis.Compilation_Unit_List;
@end{Example}

The_Context @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} a program Context environment.

Returns a list of all compilation units contained in The_Context.
Individual units will appear only once in an order that is not defined.

A Nil_Compilation_Unit_List is returned if there are no units within
The_Context.

This query will never return a unit with A_Configuration_Compilation or
a nonexistent unit kind.

All units in the result will have an Enclosing_Context value that
Is_Identical to The_Context.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Context with a Status of
Value_Error if The_Context is not open.]}
@end{DescribeCode}


@LabeledClause{function Corresponding_Children}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Children} (Library_Unit : @key[in] Asis.Compilation_Unit)
                  @key[return] Asis.Compilation_Unit_List;

@key[function] @AdaSubDefn{Corresponding_Children} (Library_Unit : @key[in] Asis.Compilation_Unit;
                                 The_Context  : @key[in] Asis.Context)
                  @key[return] Asis.Compilation_Unit_List;
@end{Example}

Library_Unit @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the library unit whose children are desired.
The_Context @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} a program Context environment.

Returns a list of the child units for the given parent library unit.

Both the declaration and body (if any) of each child unit are returned.
Descendants beyond immediate children (i.e., children of children) are not
returned by this query.

Use the compilation unit relationship queries
with a Relation_Kinds of Descendants to create a list of children, children
of children, and so on.

Returns a Nil_Compilation_Unit_List for all library unit arguments that
do not have any child units contained in The_Context.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0046-1]}
@ChgAdded{Version=[2],Text=[If The_Context specifies a context that is different
from the Enclosing_Context(Library_Unit), returns a list of the child units for
the parent library unit that Is_Equal to Library_Unit in The_Context. If no such
parent library unit exists in The_Context, returns a
Nil_Compilation_Unit_List.]}

@leading@;These two function calls will always produce identical results:

@begin{ChildExample}
Units := Corresponding_Children (Unit);
Units := Corresponding_Children (Unit, Enclosing_Context (Unit));
@end{ChildExample}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0046-1]}
@ChgDeleted{Version=[2],Text=[Any non-Nil result will have an Enclosing_Context
value that Is_Identical to The_Context.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0046-1]}
The Enclosing_Context for any non-Nil result will always be
@Chg{Version=[2],New=[Is_Identical to ],Old=[]}The_Context,
regardless of the Enclosing_Context value for the Library_Unit argument.
This query is one means of obtaining (Is_Equal) child units
from separate ASIS Context values whose underlying implementations
overlap.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Library_Unit expects a unit
that has one of the following],Old=[Appropriate]} Unit_Kinds:
@begin{Display}
A_Package
A_Generic_Package
A_Package_Instance
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Compilation_Unit with a Status
of Value_Error for any unit that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Context with a Status of
Value_Error if The_Context is provided and is not open.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[a list of units that
each have one of the following],Old=[]} Unit_Kinds:
@begin{Display}
A_Procedure
A_Function
A_Package
A_Generic_Procedure
A_Generic_Function
A_Generic_Package
A_Procedure_Instance
A_Function_Instance
A_Package_Instance
A_Procedure_Renaming
A_Function_Renaming
A_Package_Renaming
A_Generic_Procedure_Renaming
A_Generic_Function_Renaming
A_Generic_Package_Renaming
A_Procedure_Body
A_Function_Body
A_Package_Body
An_Unknown_Unit
@end{Display}

If the declaration of a child is inconsistent with the argument of the
query, neither the declaration nor the body is returned. If the
declaration of a child is consistent with the argument, but the body
is not, the declaration is returned, and for the body, the result of
the Corresponding_Body query applied to the declaration is returned.
@end{DescribeCode}


@LabeledClause{function Corresponding_Parent_Declaration}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Parent_Declaration}
            (Library_Unit : @key[in] Asis.Compilation_Unit)
            @key[return] Asis.Compilation_Unit;

@key[function] @AdaSubDefn{Corresponding_Parent_Declaration}
            (Library_Unit : @key[in] Asis.Compilation_Unit;
             The_Context  : @key[in] Asis.Context)
            @key[return] Asis.Compilation_Unit;
@end{Example}

Library_Unit @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the unit whose parent is desired.
The_Context @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} a program Context environment.

Returns the parent unit of the given library unit.

Returns a Nil_Compilation_Unit if the Library_Unit argument represents
package Standard. Root Library_Unit arguments return the package Standard.

Returns A_Nonexistent_Declaration when the Library_Unit has a
parent_unit_name denoted in the defining_program_unit_name but the parent
unit is not contained in The_Context.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0046-1]}
@ChgAdded{Version=[2],Text=[If The_Context specifies a context that is different
from the Enclosing_Context(Library_Unit), returns the parent library unit for
the child unit in The_Context that Is_Equal to Library_Item. If no such child
unit exists in The_Context, returns a Nil_Compilation_Unit.]}

@leading@;These two function calls will always produce identical results:

@begin{ChildExample}
Unit := Corresponding_Parent_Declaration (Unit);
Unit := Corresponding_Parent_Declaration (Unit, Enclosing_Context (Unit));
@end{ChildExample}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0046-1]}
@ChgDeleted{Version=[2],Text=[Any non-Nil result will have an Enclosing_Context
value that Is_Identical to The_Context.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0046-1]}
The Enclosing_Context for any non-Nil result will always be
@Chg{Version=[2],New=[Is_Identical to ],Old=[]}The_Context,
regardless of the Enclosing_Context value for the Library_Unit
argument. This query is one means of obtaining (Is_Equal) parent units
from separate ASIS Context values whose underlying implementations
overlap.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Library_Unit expects a unit
that has one of the following],Old=[Appropriate]} Unit_Kinds:
@begin{Display}
A_Procedure
A_Function
A_Package
A_Generic_Procedure
A_Generic_Function
A_Generic_Package
A_Procedure_Instance
A_Function_Instance
A_Package_Instance
A_Procedure_Renaming
A_Function_Renaming
A_Package_Renaming
A_Generic_Procedure_Renaming
A_Generic_Function_Renaming
A_Generic_Package_Renaming
A_Procedure_Body
A_Function_Body
A_Package_Body
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Compilation_Unit with a Status
of Value_Error for any unit that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Context with a Status of
Value_Error if The_Context is provided and is not open.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[a unit with
one of the following],Old=[]} Unit_Kinds:
@begin{Display}
Not_A_Unit
A_Package
A_Generic_Package
A_Package_Instance
A_Nonexistent_Declaration
An_Unknown_Unit
@end{Display}

If a parent is inconsistent with a child passed as the argument,
A_Nonexistent_Declaration shall be returned.
@end{DescribeCode}


@LabeledClause{function Corresponding_Declaration (unit)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Declaration}
            (Library_Item : @key[in] Asis.Compilation_Unit)
            @key[return] Asis.Compilation_Unit;

@key[function] @AdaSubDefn{Corresponding_Declaration}
            (Library_Item : @key[in] Asis.Compilation_Unit;
             The_Context  : @key[in] Asis.Context)
            @key[return] Asis.Compilation_Unit;
@end{Example}

Library_Item @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the library_item whose declaration is desired.
The_Context @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} a program Context environment.

Returns the corresponding library_unit_declaration, if any, for the
library_unit_body. The corresponding library unit is the unit upon which
the library_unit_body depends semantically.

@leading@;Returns a unit that Is_Equal to the argument if:
@begin{Itemize}
the argument is a library_unit_declaration,
a library_unit_renaming_declaration, or a subunit.

the argument is A_Nonexistent_Declaration or A_Nonexistent_Body.
@end{Itemize}

Returns a Nil_Compilation_Unit for library_unit_body arguments that do
not have a corresponding library unit contained in The_Context.

@ChgRef{Version=[1],Kind=[Deleted]}
@ChgNote{The following is either redundant or conflicting with the later definition.}
@ChgDeleted{Version=[1],Text=[All Unit_Kinds are appropriate except Not_A_Unit.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0046-1]}
@ChgAdded{Version=[2],Text=[If The_Context specifies a context that is different
from the Enclosing_Context(Library_Unit), returns the corresponding library unit
declaration for the library unit body in The_Context that Is_Equal to
Library_Item. If no such library unit body exists in The_Context, returns a
Nil_Compilation_Unit.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[These two function calls will always
produce identical results:]}

@begin{ChildExample}
@ChgAdded{Version=[2],Text=[Unit := Corresponding_Declaration (Unit);
Unit := Corresponding_Declaration (Unit, Enclosing_Context (Unit));]}
@end{ChildExample}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0046-1],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[The Enclosing_Context for any non-Nil result
will always be Is_Identical to The_Context,
regardless of the Enclosing_Context value for the Library_Item
argument. This query is one means of obtaining corresponding
(Is_Equal) units from separate ASIS Context values whose underlying
implementations overlap.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Type=[Leading],Keepnext=[T],Text=[Appropriate Unit_Kinds:]}
@begin{Display}
@ChgDeleted{Version=[2],Text=[A_Procedure_Body
A_Function_Body
A_Package_Body
An_Unknown_Unit            -- @examcom{See @ImplPermTitle}]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Library_Item expects a unit of any Unit_Kinds
except Not_A_Unit.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Compilation_Unit with a Status
of Value_Error if Library_Item has Unit_Kinds Not_A_Unit.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Context with a Status of
Value_Error if The_Context is provided and is not open.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Returns a unit
that has one of the following],Old=[Appropriate]} Unit_Kinds:@Chg{Version=[2],New=[],
Old=[ returning the argument Library_Item]}:
@begin{Display}
A_Procedure
A_Function
A_Package
A_Generic_Procedure
A_Generic_Function
A_Generic_Package
A_Procedure_Instance
A_Function_Instance
A_Package_Instance
A_Procedure_Renaming
A_Function_Renaming
A_Package_Renaming
A_Generic_Procedure_Renaming
A_Generic_Function_Renaming
A_Generic_Package_Renaming
A_Procedure_Body_Subunit
A_Function_Body_Subunit
A_Package_Body_Subunit
A_Task_Body_Subunit
A_Protected_Body_Subunit
A_Nonexistent_Declaration
A_Nonexistent_Body
@end{Display}

@ChgRef{Version=[1],Kind=[Deleted]}
@ChgNote{The following is either redundant or conflicting with the previous definition.}
@ChgDeleted{Version=[1],Text=[Returns all Unit Kinds.]}

If the declaration of an argument Element is inconsistent with the
argument, A_Nonexistent_Declaration shall be returned. (For a unit
A_Procedure_Body or A_Function_Body kind, the solution may be in any
case, to return Nil_Compilation_Unit if the unit is of
A_Public_Declaration_And_Body kind.)
@end{DescribeCode}

@begin{ImplReq}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0046-1]}
@ChgDeleted{Version=[2],Text=[Any non-Nil result will have an Enclosing_Context
value that Is_Identical to The_Context.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Type=[Leading],Text=[These two function calls will always
produce identical results:]}

@begin{ChildExample}
@ChgDeleted{Version=[2],Text=[Unit := Corresponding_Declaration (Unit);
Unit := Corresponding_Declaration (Unit, Enclosing_Context (Unit));]}
@end{ChildExample}


@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[The Enclosing_Context for any non-Nil result will
always be The_Context, regardless of the Enclosing_Context value for the
Library_Item argument. This query is one means of obtaining corresponding
(Is_Equal) units from separate ASIS Context values whose underlying
implementations overlap.]}
@end{ImplReq}

@begin{ImplPerm}
The handling of An_Unknown_Unit is implementation specific. The
expected use for An_Unknown_Unit is to hide proprietary implementation
details contained within unit bodies. In these cases, it should be
possible to obtain an appropriate library_unit_declaration when
starting with An_Unknown_Unit. Some implementors may choose to simply
return the An_Unknown_Unit argument in all cases.
@end{ImplPerm}


@LabeledClause{function Corresponding_Body (unit)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Body}
            (Library_Item : @key[in] Asis.Compilation_Unit)
            @key[return] Asis.Compilation_Unit;

@key[function] @AdaSubDefn{Corresponding_Body}
            (Library_Item : @key[in] Asis.Compilation_Unit;
             The_Context  : @key[in] Asis.Context)
            @key[return] Asis.Compilation_Unit;
@end{Example}

Library_Item @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the library_item whose body is desired.
The_Context @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} a program Context environment.

Returns the corresponding library_unit_body, if any, for the
library_unit_declaration. The corresponding library_unit_body is the unit
that depends semantically on the library_unit_declaration.

@leading@;Returns a unit that Is_Equal to the argument if:

@begin{Itemize}
the argument is an instance of a library_unit_declaration,
a library_unit_body, a library_unit_renaming_declaration, or a subunit.

the argument is A_Nonexistent_Declaration or A_Nonexistent_Body.
@end{Itemize}

Returns a Nil_Compilation_Unit for library_unit_declaration arguments that
do not have a corresponding library_unit_body contained in The_Context.

@ChgRef{Version=[1],Kind=[Deleted]}
@ChgNote{The following is either redundant or conflicting with the later definition.}
@ChgDeleted{Version=[1],Text=[All Unit_Kinds are appropriate except Not_A_Unit.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0046-1]}
@ChgAdded{Version=[2],Text=[If The_Context specifies a context that is different
from the Enclosing_Context(Library_Unit), returns the corresponding library unit
body for the library unit in The_Context that Is_Equal to
Library_Item. If no such library unit exists in The_Context, returns a
Nil_Compilation_Unit.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[These two function calls will always
produce identical results:]}

@begin{ChildExample}
@ChgAdded{Version=[2],Text=[Unit := Corresponding_Body (Unit);
Unit := Corresponding_Body (Unit, Enclosing_Context (Unit));]}
@end{ChildExample}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0046-1],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[The Enclosing_Context for any non-Nil result will
always be Is_Identical to The_Context, regardless of the Enclosing_Context value
for the Library_Item argument. This query is one means of obtaining
corresponding (Is_Equal) units from separate ASIS Context values whose
underlying implementations overlap.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Type=[Leading],Keepnext=[T],Text=[Appropriate Unit_Kinds:]}
@begin{Display}
@ChgDeleted{Version=[2],Text=[A_Procedure
A_Function
A_Package
A_Generic_Procedure
A_Generic_Function
A_Generic_Package
An_Unknown_Unit            -- @examcom{See @ImplPermTitle}]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Library_Item expects a unit of any Unit_Kinds
except Not_A_Unit.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Compilation_Unit with a Status
of Value_Error if Library_Item has Unit_Kinds Not_A_Unit.]}


@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Context with a Status of
Value_Error if The_Context is provided and is not open.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Returns a unit
that has one of the following],Old=[Appropriate]} Unit_Kinds@Chg{Version=[2],New=[],
Old=[ returning the argument Library_Item]}:
@begin{Display}
A_Procedure_Body
A_Function_Body
A_Package_Body
A_Procedure_Instance
A_Function_Instance
A_Package_Instance
A_Procedure_Renaming
A_Function_Renaming
A_Package_Renaming
A_Generic_Procedure_Renaming
A_Generic_Function_Renaming
A_Generic_Package_Renaming
A_Procedure_Body_Subunit
A_Function_Body_Subunit
A_Package_Body_Subunit
A_Task_Body_Subunit
A_Protected_Body_Subunit
A_Nonexistent_Declaration
A_Nonexistent_Body
@end{Display}

@ChgRef{Version=[1],Kind=[Deleted]}
@ChgNote{The following is either redundant or conflicting with the previous definition.}
@ChgDeleted{Version=[1],Text=[Returns all Unit Kinds.]}

If the argument Element requires a body to be presented to make up a
complete partition containing this Element, but The_Context  does not
contain the corresponding body, or the body contained in The_Context
is inconsistent with the argument Element, A_Nonexistent_Body shall
be returned.
@end{DescribeCode}

@begin{ImplReq}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0046-1]}
@ChgDeleted{Version=[2],Text=[Any non-Nil result will have an Enclosing_Context
value that Is_Identical to The_Context.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Type=[Leading],Text=[These two function calls will always
produce identical results:]}

@begin{ChildExample}
@ChgDeleted{Version=[2],Text=[Unit := Corresponding_Body (Unit);
Unit := Corresponding_Body (Unit, Enclosing_Context (Unit));]}
@end{ChildExample}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[The Enclosing_Context for any non-Nil result will
always be The_Context, regardless of the Enclosing_Context value for the
Library_Item argument. This query is one means of obtaining corresponding
(Is_Equal) units from separate ASIS Context values whose underlying
implementations overlap.]}
@end{ImplReq}

@begin{ImplPerm}
The handling of An_Unknown_Unit is implementation specific. The
expected use for An_Unknown_Unit is to hide proprietary implementation
details contained within unit bodies. In some cases, it could be possible
to obtain an appropriate library_unit_body when starting with
An_Unknown_Unit. Some implementors may choose to simply return the
An_Unknown_Unit argument in all cases.
@end{ImplPerm}


@LabeledClause{function Is_Nil (unit)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Nil} (Right : @key[in] Asis.Compilation_Unit)
                 @key[return] Boolean;
@end{Example}

Right @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the unit to test.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
Returns True if the compilation_unit is a
Nil_Compilation_Unit@Chg{Version=[2],New=[, and returns False
otherwise],Old=[]}.

@end{DescribeCode}


@LabeledClause{function Is_Nil (unit list)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Nil} (Right : @key[in] Asis.Compilation_Unit_List)
                @key[return] Boolean;
@end{Example}

Right @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the unit list to test.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
Returns True if the compilation_unit list has a length of
zero@Chg{Version=[2],New=[, and returns False otherwise],Old=[]}.
@end{DescribeCode}


@LabeledClause{function Is_Equal (unit)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Equal} (Left  : @key[in] Asis.Compilation_Unit;
                   Right : @key[in] Asis.Compilation_Unit) @key[return] Boolean;
@end{Example}

Left @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the first unit to compare.
Right @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the second unit to compare.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1],ARef=[SI99-0037-1],ARef=[SI99-0047-1]}
Returns True if Left and Right represent the same physical compilation unit
or if both are Nil_Compilation_Unit values@Chg{Version=[2],New=[, and returns
False otherwise],Old=[]}. The two units may or may not
be from the same ASIS Context variable. (@Chg{Version=[2],New=[],Old=[@ldquote]}The
@Chg{Version=[2],New=[@i<same physical compilation unit> has],Old=[same
physical compilation unit have]} the same version, as defined
by @Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} E.3(5)
and the same program text.)@Defn{Same physical compilation unit}@Defn2{Term=[physical compilation unit],Sec=[same]}@Defn2{Term=[compilation unit],Sec=[same physical]}

Two nonexistent units are Is_Equal if they have the same Name and Unit_Kind.
@end{DescribeCode}


@LabeledClause{function Is_Identical}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Identical} (Left  : @key[in] Asis.Compilation_Unit;
                       Right : @key[in] Asis.Compilation_Unit) @key[return] Boolean;
@end{Example}

Left @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the first unit to compare.
Right @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the second unit to compare.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1],ARef=[SI99-0037-1],ARef=[SI99-0047-1]}
Returns True if Left and Right represent the same physical compilation
unit from the same open ASIS Context variable or if both are
Nil_Compilation_Unit values@Chg{Version=[2],New=[, and returns False otherwise],Old=[]}.@Chg{Version=[2],New=[],Old=[ (@ldquote@;The same physical compilation
unit@rdquote@; have the same version, as defined
by @Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} E.3(5)
and the same program text.)]}

Two nonexistent units are Is_Identical if they have the same
Unique_Name and the same Enclosing_Context.
@end{DescribeCode}


@LabeledClause{function Unit_Full_Name}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Unit_Full_Name} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
                         @key[return] Wide_String;

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the unit whose name is desired.
@end{Example}

Returns the string image of the fully expanded Ada name of the given
compilation unit. This may be a simple name ("A") of a root library
unit, or an expanded name ("A.B") of a subunit or non-root child unit.
An expanded name shall contain the full parent_unit_name as its prefix.

Returns a null string only if A_Configuration_Compilation or a
Nil_Compilation_Unit is given.

The case of names returned by this query may vary between implementations.
Implementors are encouraged, but not required, to return names in the
same case as was used in the original compilation text.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects any kind of unit],
Old=[All Unit_Kinds are expected]}.
@end{DescribeCode}


@LabeledClause{function Unique_Name}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Unique_Name}
     (Compilation_Unit : @key[in] Asis.Compilation_Unit) @key[return] Wide_String;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the unit.
whose name is desired.

Returns a string that uniquely identifies the given compilation unit
within the underlying Ada Context implementation. The result may vary
depending on the ASIS implementation. The unique name may include the name
and parameters of the Context, file system paths, library files, version
numbers, kind, or any other information that an implementation may need
to uniquely identify the compilation unit.

Returns a null string only if a Nil_Compilation_Unit is given.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects any kind of unit],
Old=[All Unit_Kinds are expected]}.
@end{DescribeCode}


@LabeledClause{function Exists (unit)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Exists} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
        @key[return] Boolean;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
unit to test.

Returns False for any unit with Not_A_Unit or nonexistent kind.
Returns True for all other unit kinds.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects any kind of unit],
Old=[All Unit_Kinds are expected]}.
@end{DescribeCode}


@LabeledClause{function Can_Be_Main_Program}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Can_Be_Main_Program} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
                             @key[return] Boolean;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the unit to test.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
Returns True if the Compilation_Unit exists and is a subprogram
library_unit_declaration, library_unit_renaming_declaration, or
library_unit_body that can be used as a main subprogram.
See @Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} 10.2(7).

Returns False otherwise.

Results of this function may vary according to the requirements an Ada
implementation may impose on a main subprogram.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects any kind of unit],
Old=[All Unit_Kinds are expected]}.
@end{DescribeCode}


@LabeledClause{function Is_Body_Required}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Body_Required} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
                             @key[return] Boolean;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
unit to test.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1],ARef=[SI99-0038-1]}
Returns True if the Compilation_Unit exists and @ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0038-1]}
@Chg{Version=[2],New=[requires a body to be present to make up a complete
partition; otherwise returns False.], Old=[is a library
package_declaration that requires a body. See
@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} 7.2(4).]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects any kind of unit],
Old=[All Unit_Kinds are expected]}.
@end{DescribeCode}



@LabeledClause{function Text_Name}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Text_Name} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
         @key[return] Wide_String;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
unit whose text name is desired.

Returns the name of the text, or other structure, that was the source
of the compilation that resulted in this Compilation_Unit. Returns a
null string if the unit has a Nil or nonexistent kind, or if the text
name is not available for any reason.

Ada has no concept of source or text file.
Text_Name availability is a required feature of ASIS.
Results of this function may vary among implementations.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects any kind of unit],
Old=[All Unit_Kinds are expected]}.
@end{DescribeCode}


@LabeledClause{function Text_Form}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Text_Form} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
         @key[return] Wide_String;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
unit whose text form is desired.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0037-1]}
Returns the Form parameter (as for @ChgAdded{Version=[2],Text=[Ada.]}Text_Io.Open) for the text, or
other structure, that was the source of the compilation that resulted in
this Compilation_Unit. Returns a null string if the unit has a Nil or
nonexistent kind, if the text was created with an empty Form parameter,
or if the text Form parameter value is not available for any reason.

Ada has no concept of source or text file.
Text_Form availability is a required feature of ASIS.
Results of this function may vary among implementations.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects any kind of unit],
Old=[All Unit_Kinds are expected]}.
@end{DescribeCode}


@LabeledClause{function Object_Name}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Object_Name} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
         @key[return] Wide_String;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
unit whose object name is desired.

Returns the name of the object, or other structure, that contains the
binary result of the compilation for this Compilation_Unit. Returns
a null string if the unit has a Nil or nonexistent kind, or if the
object name is not available for any reason.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects any kind of unit],
Old=[All Unit_Kinds are expected]}.
@end{DescribeCode}


@LabeledClause{function Object_Form}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Object_Form} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
         @key[return] Wide_String;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
unit whose object form is desired.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0037-1]}
Returns the Form parameter (as for @Chg{Version=[2],New=[Ada.],Old=[]}Text_Io.Open)
for the object, or other structure, that was the machine-code result of the
compilation of this Compilation_Unit. Returns a null string if the unit has a
Nil or nonexistent kind, if the object was created with an empty Form parameter,
or if the object Form parameter value is not available for any reason.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects any kind of unit],
Old=[All Unit_Kinds are expected]}.
@end{DescribeCode}


@LabeledClause{function Compilation_Command_Line_Options}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Compilation_Command_Line_Options}
         (Compilation_Unit : @key[in] Asis.Compilation_Unit)
          @key[return] Wide_String;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
unit to query.

Returns the command line options used to compile the Compilation_Unit.
Returns null string if the unit has a Nil or nonexistent unit kind, or
if the command line options are not available for any reason.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects any kind of unit],
Old=[All Unit_Kinds are expected]}.
@end{DescribeCode}


@LabeledClause{function Has_Attribute}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Has_Attribute} (Compilation_Unit : @key[in] Asis.Compilation_Unit;
                        Attribute        : @key[in] Wide_String) @key[return] Boolean;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
unit to query. Attribute @Chg{Version=[1],New=[specifies],Old=[         @en
Specifies]} the name of the attribute to query.

Returns True if the compilation unit has the given attribute.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
Returns False if the unit is a Nil_Compilation_Unit argument, the
@Chg{Version=[2],New=[unit does not have the
given ],Old=[]}Attribute@Chg{Version=[2],New=[],Old=[ does not exist]}, or the
implementation does not support attributes.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects any kind of unit],
Old=[All Unit_Kinds are expected]}.

Results of this query may vary across ASIS implementations.
@end{DescribeCode}


@LabeledClause{function Attribute_Value_Delimiter}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Attribute_Value_Delimiter} @key[return] Wide_String;
@end{Example}

Returns the string used as a delimiter separating individual values
within the string Attribute_Values of a compilation unit.

Results of this query may vary across ASIS implementations. The result
can be a null string for implementations that do not support attributes,
or that do not support more than one attribute.
@end{DescribeCode}


@LabeledClause{function Attribute_Values}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Attribute_Values}
         (Compilation_Unit : @key[in] Asis.Compilation_Unit;
          Attribute        : @key[in] Wide_String)
          @key[return] Wide_String;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
unit to query. Attribute @Chg{Version=[1],New=[specifies],Old=[         @en
Specifies]} the name of the attribute to query.

Returns a string containing zero or more images of values that are
associated with the given attribute. When more than one value is returned,
the Attribute_Value_Delimiter string is used to separate the individual
values. Returns a null string if the unit is a Nil_Compilation_Unit
argument, the unit has no values for this Attribute, or the implementation
does not support attributes.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects any kind of unit],
Old=[All Unit_Kinds are expected]}.

Results of this query may vary across ASIS implementations.
@end{DescribeCode}


@LabeledClause{function Subunits}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Subunits} (Parent_Body : @key[in] Asis.Compilation_Unit)
         @key[return] Asis.Compilation_Unit_List;

@key[function] @AdaSubDefn{Subunits} (Parent_Body : @key[in] Asis.Compilation_Unit;
                   The_Context : @key[in] Asis.Context)
         @key[return] Asis.Compilation_Unit_List;
@end{Example}

Parent_Body @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the parent
unit to query. The_Context @Chg{Version=[1],New=[specifies],Old=[@en
Specifies]} the program Context to use for context.

Returns a complete list of subunit values, with one value for each body
stub that appears in the given Parent_Body. Returns a
Nil_Compilation_Unit_List if the parent unit does not contain any body
stubs. Every unit in the result will have an Enclosing_Context that
Is_Identical to The_Context.

@leading@;These two function calls will always produce identical results:

@begin{ChildExample}
SUnits := Subunits (PUnit);
SUnits := Subunits (PUnit, Enclosing_Context (PUnit));
@end{ChildExample}

The result may include unit values with a nonexistent unit kind. It
includes values for subunits that exist in The_Context as
well as values for subunits that do not exist, but whose name can be
deduced from the body stub and the name of the parent unit. These
nonexistent units are known to be library_unit_body elements so their unit
kind is A_Nonexistent_Body.

Subunit lists are also available through the Semantic_Dependence_Order
query using the Family relation.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
Raises ASIS_Inappropriate_Compilation_Unit @ChgAdded{Version=[2],Text=[with
a Status of Value_Error]} if the unit
@Chg{Version=[2],New=[Parent_Body ],Old=[]}is a Nil_Compilation_Unit.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Context with a Status of
Value_Error if The_Context is provided and is not open.]}

If a subunit is absent or if it is inconsistent with the argument Element,
A_Nonexistent_Body shall be returned for it.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[a list of units that each have
one of the following],Old=[]}Unit_Kinds:
@begin{Display}
A_Nonexistent_Body
A_Procedure_Body_Subunit
A_Function_Body_Subunit
A_Package_Body_Subunit
A_Task_Body_Subunit
A_Protected_Body_Subunit
@end{Display}
@end{DescribeCode}


@LabeledClause{function Corresponding_Subunit_Parent_Body}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Corresponding_Subunit_Parent_Body}
           (Subunit : @key[in] Asis.Compilation_Unit)
            @key[return] Asis.Compilation_Unit;

@key[function] @AdaSubDefn{Corresponding_Subunit_Parent_Body}
           (Subunit     : @key[in] Asis.Compilation_Unit;
            The_Context : @key[in] Asis.Context)
            @key[return] Asis.Compilation_Unit;
@end{Example}

Subunit @Chg{Version=[1],New=[specifies],Old=[    @en Specifies]} the subunit
to query. The_Context @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
program Context to use for context.


Returns the Compilation_Unit containing the body stub of the given Subunit.
Returns a Nil_Compilation_Unit if the subunit parent is not contained in
The_Context. Any non-Nil result will have an Enclosing_Context value that
Is_Identical to The_Context.

@leading@;These two function calls will always produce identical results:

@begin{ChildExample}
PUnit := Corresponding_Subunit_Parent_Body (SUnit);
PUnit := Corresponding_Subunit_Parent_Body (SUnit,
                                   Enclosing_Context (SUnit));
@end{ChildExample}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Subunit expects a unit
that has one of the following],Old=[Appropriate]} Unit_Kinds:
@begin{Display}
A_Procedure_Body_Subunit
A_Function_Body_Subunit
A_Package_Body_Subunit
A_Task_Body_Subunit
A_Protected_Body_Subunit
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Compilation_Unit with a Status
of Value_Error for any unit that does not have one of these expected
kinds.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Context with a Status of
Value_Error if The_Context is provided and is not open.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[a unit that has
one of the following],Old=[]} Unit_Kinds:
@begin{Display}
A_Procedure_Body
A_Function_Body
A_Package_Body
A_Procedure_Body_Subunit
A_Function_Body_Subunit
A_Package_Body_Subunit
A_Task_Body_Subunit
A_Protected_Body_Subunit
@end{Display}

If the corresponding body does not exist in The_Context, or if it exists,
but is inconsistent with the argument Element, then A_Nonexistent_Body
shall be returned.
To locate the parent of a subunit that is not itself a subunit,
repeatedly call Corresponding_Subunit_Parent_Body until a unit that
is not a subunit is returned.
@end{DescribeCode}



@LabeledClause{function Debug_Image (unit)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Debug_Image} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
         @key[return] Wide_String;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} a unit
to convert.

Returns a string value containing implementation-defined debug
information associated with the compilation unit.

The return value uses Asis.Text.Delimiter_Image to separate the lines
of multi-line results. The return value does not end with
Asis.Text.Delimiter_Image.

These values are intended for two purposes. They are suitable for
inclusion in problem reports sent to the ASIS implementor. They can be
presumed to contain information useful when debugging the implementation
itself. They are also suitable for use by the ASIS application when
printing simple application debugging messages during application
development. They are intended to be, to some worthwhile degree,
intelligible to the user.
@end{DescribeCode}

@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[end] Asis.Compilation_Units;]}
@end{Example}
