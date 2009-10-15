@Part(env-cont, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/env-cont.mss,v $}
@comment{$Revision: 1.4 $ $Date: 2009/09/15 04:48:14 $}


@LabeledSection{package Asis.Ada_Environments.Containers}

@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis.Ada_Environments],Child=[Containers]}Asis.Ada_Environments.Containers
shall exist. The package
shall provide interfaces equivalent to those described in the
following subclauses.],
Old=[@f{@key[package] @ChildUnit{Parent=[Asis.Ada_Environments],Child=[Containers]}Asis.Ada_Environments.Containers @key[is]}]}

@ChgDeleted{Version=[1],Text=[Asis.Ada_Environments.Containers]}

If an Ada implementation supports the notion of a program
library@Defn{Program library} or
@ldquote@;library@rdquote as specified in Subclause 10(2) of the Ada Reference
Manual, then an ASIS Context value can be mapped onto one or more implementor
libraries represented by Containers.


@LabeledClause{type Container}


The @i{Container}@Defn{Container} abstraction is a logical collection of
compilation units. For example, one container might hold compilation units
which include Ada predefined library units, another container might hold
implementation-defined packages. Alternatively, there might be 26 containers,
each holding compilation units that begin with their respective letter of the
alphabet. The point is that no implementation-independent semantics are
associated with a container; it is simply a logical collection.

ASIS implementations shall minimally map the Asis.Context to a list of
one ASIS Container whose Name is that of the Asis.Context Name.

@begin{DescribeCode}
@begin{Example}
@key[type] @AdaTypeDefn{Container} @key[is private];
@AdaObjDefn{Nil_Container} : @key[constant] Container;

@key[function] "=" (Left  : @key[in] Container;
              Right : @key[in] Container)
              @key[return] Boolean @key[is abstract];
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Nil_Container is the value of a Container that
represents no container.]}
@end{DescribeCode}


@LabeledClause{type Container_List}

@begin{DescribeCode}
@begin{Example}
@key[type] @AdaTypeDefn{Container_List} @key[is]
   @key[array] (List_Index @key[range] <>) @key[of] Container;
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Type Container_List represents a list of containers.]}
@end{DescribeCode}


@LabeledClause{function Defining_Containers}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Defining_Containers} (The_Context : @key[in] Asis.Context)
    @key[return] Container_List;
@end{Example}

The_Context @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the Context to define.

Returns a Container_List value that defines the single environment Context.
Each Container will have an Enclosing_Context that Is_Identical to the
argument The_Context. The order of Container values in the list is not
defined.

Returns a minimal list of length one if the ASIS Ada implementation does
not support the concept of a program library. In this case, the Container
will have the same name as the given Context.

Raises ASIS_Inappropriate_Context if The_Context is not open.
@end{DescribeCode}


@LabeledClause{function Enclosing_Context (container)}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Enclosing_Context} (The_Container : @key[in] Container)
    @key[return] Asis.Context;
@end{Example}

The_Container @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the Container to query.

Returns the Context value associated with the Container.

Returns the Context for which the Container value was originally obtained.
Container values obtained through the Defining_Containers query will always
remember the Context from which they were defined.

Because Context is limited private, this function is only intended to be
used to supply a Context parameter for other queries.

Raises ASIS_Inappropriate_Container if the Container is a Nil_Container.
@end{DescribeCode}


@LabeledClause{function Library_Unit_Declarations (container)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Library_Unit_Declarations} (The_Container : @key[in] Container)
                                   @key[return] Asis.Compilation_Unit_List;
@end{Example}

The_Container @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the Container to query.

Returns a list of all library_unit_declaration and
library_unit_renaming_declaration  elements contained in the Container. Individual
units will appear only once in an order that is not defined.

A Nil_Compilation_Unit_List is returned if there are no declarations of
library units within the Container.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
This query will never return a unit with A_Configuration_Compilation or
a Nonexistent unit kind. It will never return a unit with A_Procedure_Body or
A_Function_Body unit kind even though the unit is interpreted as both the
declaration and body of a library procedure or library function.
@Chg{Version=[2],New=[Ada Standard],Old=[(Reference Manual]} 10.1.4(4).

All units in the result will have an Enclosing_Container value that
Is_Identical to the Container.

Raises ASIS_Inappropriate_Context if the Enclosing_Context(Container)
is not open.
@end{DescribeCode}


@LabeledClause{function Compilation_Unit_Bodies (container)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Compilation_Unit_Bodies} (The_Container : @key[in] Container)
                                 @key[return] Asis.Compilation_Unit_List;
@end{Example}

The_Container @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the Container to query.

Returns a list of all library_unit_body and subunit elements contained in the
Container. Individual units will appear only once in an order that is not
defined.

A Nil_Compilation_Unit_List is returned if there are no bodies within the
Container.

This query will never return a unit with A_Configuration_Compilation or
a nonexistent unit kind.

All units in the result will have an Enclosing_Container value that
Is_Identical to the Container.

Raises ASIS_Inappropriate_Context if the Enclosing_Context(Container)
is not open.
@end{DescribeCode}


@LabeledClause{function Compilation_Units (container)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Compilation_Units} (The_Container : @key[in] Container)
                           @key[return] Asis.Compilation_Unit_List;
@end{Example}

The_Container @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the Container to query.

Returns a list of all compilation units contained in the Container.
Individual units will appear only once in an order that is not defined.

A Nil_Compilation_Unit_List is returned if there are no units within the
Container.

This query will never return a unit with A_Configuration_Compilation or
a nonexistent unit kind.

All units in the result will have an Enclosing_Container value that
Is_Identical to the Container.

Raises ASIS_Inappropriate_Context if the Enclosing_Context(Container)
is not open.
@end{DescribeCode}


@LabeledClause{function Is_Equal (containers)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Equal} (Left  : @key[in] Container;
                   Right : @key[in] Container) @key[return] Boolean;
@end{Example}

Left @chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the first Container.
Right @chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the second Container.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
Returns True if Left and Right designate Container values that contain the
same set of compilation units@Chg{Version=[2],New=[, and returns False
otherwise],Old=[]}. The Container values may have been defined from different
Context values.
@end{DescribeCode}


@LabeledClause{function Is_Identical (containers)}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Identical} (Left  : @key[in] Container;
                       Right : @key[in] Container) @key[return] Boolean;
@end{Example}

Left @chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the first Container.
Right @chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the second Container.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
Returns True if Is_Equal(Left, Right) and the Container values have been
defined from Is_Equal Context values@Chg{Version=[2],New=[, and returns False otherwise],Old=[]}.
@end{DescribeCode}


@LabeledClause{function Name (container)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Name} (The_Container : @key[in] Container) @key[return] Wide_String;
@end{Example}

The_Container @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the Container to name.

Returns the Name value associated with the Container.

Returns a null string if the Container is a Nil_Container.
@end{DescribeCode}

@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[private]]}

@ChgDeleted{Version=[1],Text=[   @key[type] Container @key[is] @i{(Implementation_Defined)};
   Nil_Container : @key[constant] Container := @i{Implementation_Defined};]}

@ChgDeleted{Version=[1],Text=[end Asis.Ada_Environments.Containers;]}
@end{Example}
