@Part(04, Root="ada.mss")

@Comment{$Date: 2005/03/01 06:05:02 $}
@LabeledSection{Names and Expressions}

@Comment{$Source: e:\\cvsroot/ARM/Source/04a.mss,v $}
@Comment{$Revision: 1.49 $}

@begin{Intro}
@Redundant[The rules applicable to the different forms of @nt<name> and
expression, and to their evaluation, are given in this section.]
@end{Intro}

@LabeledClause{Names}

@begin{Intro}
@redundant[@nt<Name>s can denote declared entities, whether declared explicitly
or implicitly (see @RefSecNum(Declarations)). @nt<Name>s can also
denote objects or subprograms designated by access values; the
results of @nt<type_conversion>s or @nt<function_call>s; subcomponents
and slices of objects and values; protected subprograms,
single entries, entry families,
and entries in families of entries.
Finally, @nt<name>s can denote attributes of any of the foregoing.]
@end{Intro}

@begin{Syntax}
@Syn{tabs=[P22], lhs=<name>,rhs="
     @Syn2{direct_name}@\| @Syn2{explicit_dereference}
   | @Syn2{indexed_component}@\| @Syn2{slice}
   | @Syn2{selected_component}@\| @Syn2{attribute_reference}
   | @Syn2{type_conversion}@\| @Syn2{function_call}
   | @Syn2{character_literal}"}


@Syn{lhs=<direct_name>,
rhs="@Syn2{identifier} | @Syn2{operator_symbol}"}
@begin{Discussion}
  @nt<character_literal> is no longer a @nt<direct_name>.
  @nt<character_literal>s are usable even when the corresponding
  @nt<enumeration_type_declaration> is not visible. See @RefSecNum(Literals).
@end{Discussion}

@Syn{lhs=<prefix>,rhs="@Syn2{name} | @Syn2{implicit_dereference}"}

@Syn{lhs=<explicit_dereference>,rhs="@Syn2{name}.@key{all}"}

@Syn{lhs=<implicit_dereference>,rhs="@Syn2{name}"}

@end{Syntax}

@begin{Intro}
@Redundant[Certain forms of @nt<name> (@nt<indexed_component>s,
@nt<selected_component>s, @nt<slice>s, and @nt<attribute>s)
include a @nt<prefix> that is either itself a @nt<name> that denotes
some related entity, or an @nt<implicit_dereference> of an access
value that designates some related entity.]
@end{Intro}

@begin{Resolution}
@Defn{dereference}
@PDefn2{Term=[expected type],
  Sec=(dereference @nt{name})}
The @nt{name} in a @i(dereference) (either an
@nt<implicit_dereference> or an @nt<explicit_dereference>)
is expected to be of any access type.
@end{Resolution}

@begin{StaticSem}
@PDefn2{Term=[nominal subtype], Sec=(associated with a dereference)}
If the type of the @nt{name} in a dereference is some access-to-object
type @i(T), then the dereference denotes a view of an object, the
@i(nominal subtype) of the view being the designated subtype of @i(T).
@begin{Ramification}
If the
value of the @nt<name> is the result of an access type conversion, the
dereference denotes a view created as part of the conversion.
The nominal subtype of the view is not necessarily
the same as that used to create the designated object.
See @RefSecNum{Type Conversions}.
@end{Ramification}
@begin{Honest}
  @PDefn2{Term=[nominal subtype], Sec=(of a @nt<name>)}
  We sometimes refer to the nominal subtype of a particular kind
  of @nt<name> rather than the nominal subtype of the view denoted by
  the @nt<name> (presuming the @nt<name> denotes a view of an object).
  These two uses of nominal subtype are intended to mean the same
  thing.
@end{Honest}

@PDefn2{Term=[profile], Sec=(associated with a dereference)}
If the type of the @nt<name> in a dereference is some access-to-subprogram
type @i(S), then the dereference denotes a view of a subprogram,
the @i(profile) of the view being the designated profile of @i(S).
@begin{Ramification}
This means
  that the formal parameter names and default expressions to be used
  in a call whose @nt<name> or @nt<prefix> is a dereference
  are those of the designated profile, which need not be the same as those
  of the subprogram designated by the access value, since 'Access
  requires only subtype conformance, not full conformance.
@end{Ramification}
@end{StaticSem}

@begin{RunTime}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00415-01]}
@PDefn2{Term=[evaluation], Sec=(name)}
The evaluation of a @nt<name> determines the entity denoted by the
@Chg{Version=[2],New=[@nt<name>],Old=[name]}. This evaluation has no other
effect for a @nt<name> that
is a @nt<direct_name> or a @nt<character_literal>.

@PDefn2{Term=[evaluation], Sec=(name that has a prefix)}
@Redundant[The evaluation of a @nt<name> that has a @nt<prefix> includes
the evaluation of the @nt<prefix>.]
@PDefn2{Term=[evaluation], Sec=(prefix)}
The evaluation of a @nt{prefix} consists of the evaluation of
the @nt{name} or the @nt{implicit_dereference}.
The @nt{prefix} denotes the entity denoted by the @nt{name} or the
@nt{implicit_dereference}.

@PDefn2{Term=[evaluation], Sec=(dereference)}
The evaluation of a dereference
consists of the evaluation of the @nt{name}
and the determination of the object or subprogram that is designated
by the value of the @nt{name}.
@IndexCheck{Access_Check}
A check is made that the value of the @nt{name} is not the null access
value.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
Constraint_Error is raised if this check fails.
The dereference
denotes the object or subprogram designated by the value of the @nt{name}.
@end{RunTime}

@begin{Examples}
@Leading@keepnext@i(Examples of direct names:)
@begin(Example)
@tabclear()@tabset(P9, P47)
Pi @\@RI(-- the direct name of a number) @\(see @RefSecNum(Number Declarations))
Limit @\@RI(-- the direct name of a constant) @\(see @RefSecNum(Object Declarations))
Count @\@RI(-- the direct name of a scalar variable) @\(see @RefSecNum(Object Declarations))
Board @\@RI(-- the direct name of an array variable) @\(see @RefSecNum(Index Constraints and Discrete Ranges))
Matrix @\@RI(-- the direct name of a type) @\(see @RefSecNum(Array Types))
Random @\@RI(-- the direct name of a function) @\(see @RefSecNum(Subprogram Declarations))
Error @\@RI(-- the direct name of an exception) @\(see @RefSecNum(Exception Declarations))
@end(Example)

@begin{Wide}
@leading@keepnext@i{Examples of dereferences:}
@end{Wide}
@begin{Example}@tabclear()@tabset(P19)
Next_Car.@key[all]@\@RI[--  explicit dereference denoting the object designated by]
               @\@RI[--  the access variable Next_Car (see @RefSecNum{Incomplete Type Declarations})]
Next_Car.Owner @\@RI[--  selected component with implicit dereference;]
               @\@RI[--  same as Next_Car.@key[all].Owner]
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
Type conversions and function calls are now considered names
that denote the result of the operation.
In the case of a type conversion used as an actual
parameter or that is of a tagged type, the type conversion is considered
a variable if the operand is a variable.
This simplifies the description of "parameters of the
form of a type conversion" as well as better supporting an
important OOP paradigm that requires the combination of a
conversion from a class-wide type to some specific
type followed immediately by component selection.
Function calls are considered names so that a type conversion
of a function call and the function call itself are treated
equivalently in the grammar.
A function call is considered the name of a constant,
and can be used anywhere such a name is permitted.
See @RefSecNum(Return Statements).

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{To be consistent with 8652/0006}
Type conversions of a tagged type are permitted anywhere
their operand is permitted. That is, if the operand
is a variable, then the type conversion can appear on the
left-hand side of an @nt{assignment_statement}.
If the operand is an object,
then the type conversion can appear in an object renaming
or as a @Chg{New=[@nt{prefix}],Old=[prefix]}.
See @RefSecNum(Type Conversions).
@end{Extend83}

@begin{DiffWord83}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
Everything of the general syntactic form @nt{name}(...) is
now syntactically a @nt{name}. In any realistic parser,
this would be a necessity since distinguishing among the various
@nt{name}(...) constructs inevitably requires name resolution.
In cases where the construct yields a value rather than an object,
the name denotes @Chg{Version=[2],New=[a],Old=[the]} value rather than an
object. Names already denote values in Ada 83 with named numbers, components of
the result of a function call, etc. This is partly just a wording change, and
partly an extension of functionality (see Extensions heading above).

The syntax rule for @nt{direct_name} is new. It is used in places where
direct visibility is required.
It's kind of like Ada 83's @nt{simple_name}, but @nt{simple_name} applied
to both direct visibility and visibility by selection,
and furthermore, it didn't work right for @nt{operator_symbol}s.
The syntax rule for @nt{simple_name} is removed,
since its use is covered by a combination of @nt{direct_name} and
@nt{selector_name}.
The syntactic categories @nt{direct_name} and @nt{selector_name} are similar;
it's mainly the visibility rules that distinguish the two.
The introduction of @nt{direct_name} requires the insertion of one new
explicit textual rule: to forbid @nt<statement_identifier>s from being
@nt<operator_symbol>s.
This is the only case where the explicit rule is needed,
because this is the only case where the declaration of the entity is
implicit.
For example, there is no need to syntactically forbid (say) @lquotes@;X: "Rem";@rquotes@;,
because it is impossible to declare a type whose name is an
@nt{operator_symbol} in the first place.

The syntax rules for @nt{explicit_dereference}
and @nt{implicit_dereference} are new;
this makes other rules simpler, since dereferencing an access value has
substantially different semantics from @nt{selected_component}s.
We also use @nt{name} instead of @nt{prefix} in the
@nt{explicit_dereference} rule
since that seems clearer. Note that these rules rely on the
fact that function calls are now names, so we don't need to
use prefix to allow functions calls in front of .@key{all}.
@begin{Discussion}
Actually, it would be reasonable to allow any @nt{primary} in front
of .@key{all}, since only the value is needed, but that would be a bit
radical.
@end{Discussion}

We no longer use the term @i(appropriate for a type)
since we now describe the semantics of a prefix in terms
of implicit dereference.
@end{DiffWord83}

@LabeledSubClause{Indexed Components}

@begin{Intro}
@Redundant[An @nt<indexed_component> denotes either
a component of an array or an entry
in a family of entries.
@IndexSee{Term=[array indexing],See=(indexed_component)}]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<indexed_component>,rhs="@Syn2{prefix}(@Syn2{expression} {, @Syn2{expression}})"}
@end{Syntax}

@begin{Resolution}
The @nt{prefix} of an @nt{indexed_component} with a given
number of @nt<expression>s
shall resolve to denote an array (after any implicit dereference)
with the corresponding number of index positions,
or shall resolve to denote an entry family of a task or protected object
(in which case there shall be only one @nt<expression>).

@PDefn2{Term=[expected type], Sec=(indexed_component expression)}
The expected type for each @nt{expression} is the corresponding index type.

@end{Resolution}

@begin{StaticSem}
When the @nt<prefix> denotes an array,
the @nt<indexed_component> denotes the component of the
array with the specified index value(s).
@PDefn2{Term=[nominal subtype],
  Sec=(associated with an @nt<indexed_component>)}
The nominal subtype of the @nt<indexed_component> is the
component subtype of the array type.
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00363-01]}
@ChgDeleted{Version=[2],Text=[In the case of an array whose components are
aliased, and
of an unconstrained discriminated subtype, the components
are constrained even though their nominal subtype is unconstrained.
(This is because all aliased discriminated objects are constrained.
See @RefSecNum(Operations of Access Types).)
In all other cases, an array component is constrained if and only
if its nominal subtype is constrained.]}
@end{Ramification}

When the @nt<prefix> denotes an entry family,
the @nt<indexed_component> denotes
the individual entry of the entry family with the specified index value.

@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[evaluation], Sec=(indexed_component)}
For the evaluation of an @nt<indexed_component>, the @nt{prefix} and the
@nt{expression}s are evaluated in an arbitrary order. The value of
each @nt<expression> is converted to the corresponding index type.
@PDefn2{Term=[implicit subtype conversion],Sec=(array index)}
@IndexCheck{Index_Check}
A check is made that each index value
belongs to the corresponding index range of the array or entry family
denoted by the @nt<prefix>.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
Constraint_Error is raised if this check fails.

@end{RunTime}

@begin{Examples}
@Leading@keepnext@i(Examples of indexed components:)
@begin{Example}
@tabclear()@tabset(P64)
 My_Schedule(Sat)     @RI[--  a component of a one-dimensional array @\(see @RefSecNum{Index Constraints and Discrete Ranges})]
 Page(10)             @RI[--  a component of a one-dimensional array @\(see @RefSecNum{Array Types})]
 Board(M, J + 1)      @RI[--  a component of a two-dimensional array @\(see @RefSecNum{Index Constraints and Discrete Ranges})]
 Page(10)(20)         @RI[--  a component of a component @\(see @RefSecNum{Array Types})]
 Request(Medium)      @RI[--  an entry in a family of entries @\(see @RefSecNum{Task Units and Task Objects})]
 Next_Frame(L)(M, N)  @RI[--  a component of a function call @\(see @RefSecNum{Subprogram Declarations})]
@end{Example}
@end{Examples}

@begin{Notes}
@i(Notes on the examples:)
Distinct notations are used for components of multidimensional arrays (such
as Board) and arrays of arrays (such as Page). The components of an array
of arrays are arrays and can therefore be indexed. Thus Page(10)(20)
denotes the 20th component of Page(10). In the last example Next_Frame(L)
is a function call returning an access value that designates a
two-dimensional array.

@end{Notes}

@LabeledSubClause{Slices}

@begin{Intro}
@redundant[@Defn{array slice}
A @nt<slice> denotes a one-dimensional array formed by a sequence of
consecutive components of a one-dimensional array. A @nt<slice> of
a variable is a variable; a @nt<slice> of a constant is a constant;]
a @nt<slice> of a value is a value.
@end{Intro}

@begin{Syntax}
@Syn{lhs=<slice>,rhs="@Syn2{prefix}(@Syn2{discrete_range})"}
@end{Syntax}

@begin{Resolution}
The @nt{prefix} of a @nt{slice}
shall resolve to denote a one-dimensional array
(after any implicit dereference).

@PDefn2{Term=[expected type], Sec=(slice discrete_range)}
The expected type for the @nt{discrete_range} of a @nt<slice>
is the index type of the array type.
@end{Resolution}

@begin{StaticSem}
A @nt<slice> denotes a one-dimensional array formed by the sequence of
consecutive components of the array denoted by the @nt<prefix>,
corresponding to the range of values
of the index given by the @nt<discrete_range>.

The type of the @nt<slice> is that of the @nt<prefix>.
Its bounds are those defined by the @nt{discrete_range}.

@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[evaluation], Sec=(slice)}
For the evaluation of a @nt{slice},
the @nt{prefix} and the @nt{discrete_range}
are evaluated in an arbitrary order.
@IndexCheck{Index_Check}
@Defn{null slice}
If the @nt{slice} is not a @i(null slice)
(a @nt<slice> where the @nt<discrete_range> is a null range),
then a check is made that the bounds of the @nt{discrete_range}
belong to the index range of the array denoted by the @nt{prefix}.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
Constraint_Error is raised if this check fails.

@end{RunTime}

@begin{Notes}
A @nt<slice> is not permitted as the @nt<prefix> of an
Access @nt<attribute_reference>,
even if the components or the array as a whole are aliased.
See @RefSecNum(Operations of Access Types).
@begin{TheProof}
  Slices are not aliased, by @RefSec{Access Types}.
@end{TheProof}
@begin(Reason)
  This is to ease implementation of general-access-to-array.
  If slices were aliased, implementations would need to store
  array dope with the access values, which is not always desirable
  given access-to-incomplete types completed in a package body.
@end(Reason)

For a one-dimensional array A, the @nt<slice> A(N .. N) denotes
an array that has only one component;
its type is the type of A. On the other hand, A(N) denotes a
component of the array A and has the corresponding component type.
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of slices:)
@begin{Example}
@tabclear()@tabset(P58)
  Stars(1 .. 15)        @RI[--  a slice of 15 characters @\(see @RefSecNum{String Types})]
  Page(10 .. 10 + Size) @RI[--  a slice of 1 + Size components @\(see @RefSecNum{Array Types})]
  Page(L)(A .. B)       @RI[--  a slice of the array Page(L) @\(see @RefSecNum{Array Types})]
  Stars(1 .. 0)         @RI[--  a null slice @\(see @RefSecNum{String Types})]
  My_Schedule(Weekday)  @RI[--  bounds given by subtype @\(see @RefSecNum{Index Constraints and Discrete Ranges} and @RefSecNum{Enumeration Types})]
  Stars(5 .. 15)(K)     @RI[--  same as Stars(K) @\(see @RefSecNum{String Types})]
                        @RI[--  provided that K is in 5 .. 15]
@end{Example}
@end{Examples}

@LabeledSubClause{Selected Components}

@begin{Intro}
@redundant[@nt{Selected_component}s are used to denote components (including
discriminants),
entries, entry families, and protected subprograms; they are
also used as expanded names as described below.
@IndexSee{Term=[dot selection],See=(selected_component)}]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<selected_component>,rhs="@Syn2{prefix} . @Syn2{selector_name}"}


@Syn{lhs=<selector_name>,rhs="@Syn2{identifier} | @Syn2{character_literal} | @Syn2{operator_symbol}"}
@end{Syntax}

@begin{Resolution}
@Defn{expanded name}
A @nt<selected_component> is called an @i(expanded name)
if, according to the visibility rules, at least one possible
interpretation of its @nt<prefix> denotes
a package or an enclosing named construct (directly, not through
a @nt<subprogram_renaming_declaration>
or @nt<generic_renaming_declaration>).
@begin{Discussion}
See AI83-00187.
@end{Discussion}

@Leading@;A @nt{selected_component} that is not an expanded name
shall resolve to denote one of the following:
@begin(Ramification)
  If the @nt<prefix> of a @nt<selected_component> denotes
  an enclosing named construct, then the @nt<selected_component> is interpreted
  only as an expanded name, even if the named construct
  is a function that could be called without parameters.
@end(Ramification)
@begin{Itemize}
A component @Redundant[(including a discriminant)]:

@NoPrefix@;The @nt{prefix} shall resolve to denote an object or value of some
non-array composite type
(after any implicit dereference).
The @nt{selector_name} shall resolve to denote a
@nt{discriminant_specification} of the type, or, unless the type is
a protected type, a @nt<component_declaration>
of the type. The @nt{selected_component} denotes the
corresponding component of the object or value.
@begin{Reason}
  @ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-00015}
  The components of a protected object cannot be named except
  by an expanded name, even from within the corresponding protected body.
  The protected body may not reference @Chg{New=[],Old=[the ]}the private
  components of some arbitrary object of the protected
  type; the protected body may reference components of the current
  instance only (by an expanded name or a @nt<direct_name>).
@end{Reason}
@begin{Ramification}
  Only the discriminants and components visible at the place of the
  @nt<selected_component> can be selected, since a @nt<selector_name>
  can only denote declarations that are visible (see @RefSecNum{Visibility}).
@end{Ramification}

A single entry, an entry family, or a protected subprogram:

@NoPrefix@;The @nt{prefix} shall resolve to denote an object or value of some
task or protected type (after any implicit dereference).
The @nt{selector_name} shall resolve to denote an @nt{entry_declaration}
or @nt{subprogram_declaration} occurring (implicitly or explicitly)
within the visible part of that type.
The @nt{selected_component} denotes the
corresponding entry, entry family, or protected subprogram.
@begin{Reason}
  This explicitly says @lquotes@;visible part@rquotes@; because even though the body
  has visibility on the private part, it cannot call the
  private operations of some arbitrary object of the task or protected
  type, only those of the current instance (and expanded name notation
  has to be used for that).
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00252-01]}
@ChgAdded{Version=[2],Text=[A view of a subprogram whose first formal parameter is of
a tagged type or is an access parameter whose designated type is tagged:]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],NoPrefix=[T],Text=[The @nt<prefix> (after any implicit
dereference) shall resolve to denote an object or value of a specific tagged
type @i<T> or class-wide type @i<T>'Class. The @nt<selector_name> shall resolve
to denote a view of a subprogram declared immediately within the declarative
region in which an ancestor of the type @i<T> is declared. The first formal
parameter of the subprogram shall be of type @i<T>, or a class-wide type that
covers @i<T>, or an access parameter designating one of these types. The
designator of the subprogram shall not be the same as that of a component of
the tagged type visible at the point of the @nt<selected_component>. The
@nt<selected_component> denotes a view of this subprogram that omits the first
formal parameter.]}

@end{Itemize}

@Leading@;An expanded name shall resolve to denote a declaration that
occurs immediately within a named declarative region, as follows:
@begin(itemize)
The @nt<prefix> shall resolve to denote either a package @Redundant[(including
the current instance of a generic package, or a rename of a package)], or
an enclosing named construct.

The @nt{selector_name}
shall resolve to denote a declaration that occurs
immediately within the declarative region of the
package or enclosing construct @Redundant[(the declaration shall be visible
at the place of the expanded name @em see @RefSecNum(Visibility))].
The expanded name denotes that declaration.
@begin{Ramification}
  Hence, a library unit or subunit can use an expanded
  name to refer to the declarations within the private part of its
  parent unit, as well as to other children that have been mentioned in
  @nt<with_clause>s.
@end{Ramification}

If the @nt<prefix> does not denote a package, then it
shall be a @nt<direct_name> or an expanded name,
and it shall resolve to denote a program unit (other than a package),
the current instance of a type, a @nt{block_statement}, a @nt{loop_statement},
or an @nt{accept_@!statement}
(in the case of an @nt<accept_@!statement> or @nt<entry_@!body>,
no family index is allowed);
the expanded name shall occur within the
declarative region of this construct.
Further, if this construct is a callable construct
and the @nt<prefix> denotes more than one such enclosing callable construct,
then the expanded name is ambiguous, independently of the @nt<selector_name>.

@end(itemize)

@end{Resolution}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00252-01]}
@ChgAdded{Version=[2],Text=[If a @nt<selected_component> other than an expanded
name resolves to denote a view of a subprogram whose first parameter is an
access parameter, the @nt<prefix> shall denote an aliased view of an object.]}

@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[We want calls in both this new notation and the
traditional notation to have the same legality. Thus, the implicit 'Access in
this new notation needs the same legality check that an explicit 'Access
would have.]}
@end{Reason}
@end{Legality}

@begin{RunTime}
@PDefn2{Term=[evaluation], Sec=(selected_component)}
The evaluation of a @nt{selected_component} includes the
evaluation of the @nt{prefix}.

@IndexCheck{Discriminant_Check}
For a @nt{selected_component} that denotes a component of a @nt{variant},
a check is made that the values of the discriminants are such that
the value or object denoted by the @nt<prefix> has this component.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
The exception Constraint_Error is raised if this check fails.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00252-01]}
@ChgAdded{Version=[2],Text=[ For a @nt<selected_component> with a tagged @nt<prefix>
and a @nt<selector_name> that denotes a view of a subprogram, a call on the view
denoted by the @nt<selected_component> is equivalent to a call on the
subprogram with the first actual parameter being provided by the
object or value denoted by the @nt<prefix> (or the Access attribute of this
object or value if the first formal parmeter is an access parameter), and the
remaining actual parameters given by the @nt<actual_parameter_part>, if any.]}
@end{RunTime}

@begin{Examples}
@Leading@keepnext@i(Examples of selected components:)
@begin{Example}
@tabclear()@tabset(P60)
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00252-01]}
  Tomorrow.Month     @RI[--  a record component @\(see @RefSecNum{Record Types})]
  Next_Car.Owner     @RI[--  a record component @\(see @RefSecNum{Incomplete Type Declarations})]
  Next_Car.Owner.Age @RI[--  a record component @\(see @RefSecNum{Incomplete Type Declarations})]
                     @RI[--  the previous two lines involve implicit dereferences]
  Writer.Unit        @RI[--  a record component (a discriminant) @\(see @RefSecNum{Variant Parts and Discrete Choices})]
  Min_Cell(H).Value  @RI[--  a record component of the result @\(see @RefSecNum{Subprogram Declarations})]
                     @RI[--  of the function call Min_Cell(H)]
@Chg{Version=[2],New=<  X.Activate         @RI[--  a procedure call assuming X has @\(see @RefSecNum{Subprogram Calls})]
                     @RI[--  a tagged type]
>,Old=<>}  Control.Seize      @RI[--  an entry of a protected object @\(see @RefSecNum{Protected Units and Protected Objects})]
  Pool(K).Write      @RI[--  an entry of the task Pool(K) @\(see @RefSecNum{Protected Units and Protected Objects})]
@end{Example}

@begin{Wide}
@leading@keepnext@i(Examples of expanded names:)
@end{Wide}
@begin{Example}
@tabclear()@tabset(P67)
  Key_Manager."<"      @RI[--  an operator of the visible part of a package @\(see @RefSecNum{Private Operations})]
  Dot_Product.Sum      @RI[--  a variable declared in a function body @\(see @RefSecNum{Subprogram Declarations})]
  Buffer.Pool          @RI[--  a variable declared in a protected unit @\(see @RefSecNum{Example of Tasking and Synchronization})]
  Buffer.Read          @RI[--  an entry of a protected unit @\(see @RefSecNum{Example of Tasking and Synchronization})]
  Swap.Temp            @RI[--  a variable declared in a block statement @\(see @RefSecNum{Block Statements})]
  Standard.Boolean     @RI[--  the name of a predefined type @\(see @RefSecNum{The Package Standard})]
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
We now allow an expanded name to use a prefix
that denotes a rename of a package, even if the
selector is for an entity local to the body or private
part of the package, so long as the entity is visible
at the place of the reference. This eliminates
a preexisting anomaly where references in a package
body may refer to declarations of its visible part
but not those of its private part or body when the
prefix is a rename of the package.
@end{Extend83}

@begin{DiffWord83}
The syntax rule for @nt{selector_name} is new. It is used in places where
visibility, but not necessarily direct visibility, is required.
See @RefSec{Names} for more information.

The description of dereferencing an access type has been moved
to @RefSec{Names}; @nt<name>.@key(all) is no longer considered
a @nt<selected_component>.

The rules have been restated to be consistent with our
new terminology, to accommodate class-wide types, etc.
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00252-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}The prefix call notation for
  tagged objects is new. This provides a similar notation to that used in other
  popular languages, and also reduces the need for @nt{use_clause}s. This
  is sometimes known as @lquotes@;prefix call notation@rquotes or @lquotes@;distinguished
  receiver notation@rquotes@;.],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[Given the following
definitions for a tagged type T:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[@key{procedure} Do_Something (Obj : in out T; Count : in Natural);
My_Object : T;],Old=[]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[the following calls are equivalent:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Do_Something (My_Object, Count => 10);
My_Object.Do_Something (Count => 10);]}
@end{Example}
@end{Extend95}


@LabeledSubClause{Attributes}

@begin{Intro}
@Defn{attribute}
@Redundant[An @i(attribute) is a characteristic of an entity that can be
queried via an @nt{attribute_@!reference}
or a @nt<range_@!attribute_@!reference>.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<attribute_reference>,
  rhs="@Syn2{prefix}@SingleQuote@Syn2{attribute_designator}"}

@Syn{lhs=<attribute_designator>,rhs="
    @Syn2{identifier}[(@SynI{static_}@Syn2{expression})]
  | Access | Delta | Digits"}


@Syn{lhs=<range_attribute_reference>,
  rhs="@Syn2{prefix}@SingleQuote@Syn2{range_attribute_designator}"}

@Syn{lhs=<range_attribute_designator>,
  rhs="Range[(@SynI{static_}@Syn2{expression})]"}
@end{Syntax}

@begin{Resolution}
In an @nt<attribute_reference>,
if the @nt<attribute_designator> is for an attribute defined
for (at least some) objects of an access type, then the @nt<prefix> is never
interpreted as an @nt<implicit_dereference>;
otherwise (and for all @nt<range_attribute_reference>s),
if the type of the @nt<name> within the @nt<prefix>
is of an access type, the @nt<prefix> is interpreted as an
@nt<implicit_dereference>.
Similarly, if the @nt{attribute_designator} is for an attribute defined for (at
least some) functions, then the @nt<prefix> is never interpreted as a
parameterless @nt{function_call}; otherwise
(and for all @nt<range_attribute_reference>s), if the @nt<prefix>
consists of a @nt<name> that denotes a function, it is interpreted
as a parameterless @nt<function_call>.
@begin{Discussion}
  The first part of this rule is essentially a "preference"
  against implicit dereference, so that it is possible
  to ask for, say, 'Size of an access object,
  without automatically getting the size of the object designated
  by the access object.
  This rule applies to 'Access, 'Unchecked_Access, 'Size, and 'Address,
  and any other attributes that are defined for at least some
  access objects.

  The second part of this rule implies that, for a parameterless function F,
  F'Address is the address of F, whereas
  F'Size is the size of the anonymous constant returned by F.

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{To be consistent with 8652/0006}
  We normally talk in terms of expected type or profile for
  name resolution rules, but we don't do this for attributes
  because certain attributes are legal independent of the type
  or the profile of the @Chg{New=[@nt{prefix}],Old=[prefix]}.
@end{Discussion}

@PDefn2{Term=[expected type],
  Sec=(attribute_designator expression)}
@PDefn2{Term=[expected type],
  Sec=(range_attribute_designator expression)}
The @nt{expression}, if any, in an
@nt{attribute_designator} or @nt{range_attribute_designator}
is expected to be of any integer type.
@end{Resolution}

@begin{Legality}
The @nt{expression}, if any, in an @nt{attribute_designator}
or @nt{range_attribute_designator} shall be static.
@end{Legality}

@begin{StaticSem}
An @nt{attribute_reference} denotes a
value, an object, a subprogram, or some
other kind of program entity.
@begin{Ramification}
The attributes defined by the language are summarized in
@RefSecNum{Language-Defined Attributes}.
Implementations can define additional attributes.
@end{Ramification}

@Redundant[A @nt{range_attribute_reference}
X'Range(N) is equivalent to the @nt<range> X'First(N) ..
X'Last(N), except that the @nt{prefix} is only evaluated once.
Similarly,
X'Range is equivalent to X'First .. X'Last, except that the @nt{prefix}
is only evaluated once.]

@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[evaluation], Sec=(attribute_reference)}
@PDefn2{Term=[evaluation], Sec=(range_attribute_reference)}
The evaluation of an @nt{attribute_reference}
(or @nt{range_attribute_reference}) consists
of the evaluation of the @nt{prefix}.
@end{RunTime}

@begin{ImplPerm}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0015],ARef=[AI95-00093-01]}
An implementation may provide implementation-defined attributes;
the @nt{identifier} for an implementation-defined
attribute shall differ from those of the language-defined
attributes@Chg{New=[ unless supplied for compatibility with a previous edition of
this International Standard],Old=[]}.
@ImplDef{Implementation-defined attributes.}
@begin{Ramification}
They cannot be reserved words because reserved words are not legal
identifiers.

The semantics of implementation-defined attributes,
and any associated rules, are, of course, implementation defined.
For example, the implementation defines whether a given
implementation-defined attribute can be used in a static expression.

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0015],ARef=[AI95-00093-01]}
@Chg{New=[Implementations are allowed to support the Small attribute for
floating types, as this was defined in Ada 83, even though the name would
conflict with a language-defined attribute.],Old=[]}
@end{Ramification}
@end{ImplPerm}

@begin{Notes}
Attributes are defined throughout this International Standard,
and are summarized in
@RefSecNum{Language-Defined Attributes}.

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{To be consistent with 8652/0006}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00235]}
In general, the @nt<name> in a @nt<prefix> of an @nt<attribute_reference>
(or a @nt<range_attribute_reference>) has to be resolved
without using any context.
However, in the case of the Access attribute,
the expected type for the @Chg{Version=[2],New=[@nt{attribute_reference}],
Old=[@Chg{New=[@nt{prefix}],Old=[prefix]}]} has to be a
single access type, and@Chg{Version=[2],New=[],Old=[ if it is an
access-to-subprogram type (see @RefSecNum(Operations of Access Types)) then]}
the resolution of the @nt<name> can use the fact that
the@Chg{Version=[2],New=[ type of the object or the],Old=[]} profile of the
callable entity denoted by the @nt<prefix>
has to @Chg{Version=[2],New=[match the designated type or ],Old=[]}be type
conformant with the designated profile of the access type.
@Defn2{Term=[type conformance],Sec=(required)}
@begin(TheProof)
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00235]}
  In the general case, there is no @lquotes@;expected type@rquotes@; for
  the @nt<prefix> of an @nt<attribute_reference>.
  In the special case of 'Access,
  there is an @Chg{Version=[2],New=[@lquotes@;expected type@rquotes@; or ],
  Old=[]}@lquotes@;expected profile@rquotes@; for the @nt<prefix>.
@end(TheProof)
@begin(Reason)
  'Access is a special case, because without it,
  it would be very difficult to take 'Access of an overloaded
  subprogram.
@end(Reason)
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of attributes:)
@begin{Example}
@tabclear()@tabset(P64)
Color'First        @RI[-- minimum value of the enumeration type Color @\(see @RefSecNum{Enumeration Types})]
Rainbow'Base'First @RI[-- same as Color'First @\(see @RefSecNum{Enumeration Types})]
Real'Digits        @RI[-- precision of the type Real @\(see @RefSecNum{Floating Point Types})]
Board'Last(2)      @RI[-- upper bound of the second dimension of Board @\(see @RefSecNum{Index Constraints and Discrete Ranges})]
Board'Range(1)     @RI[-- index range of the first dimension of Board @\(see @RefSecNum{Index Constraints and Discrete Ranges})]
Pool(K)'Terminated @RI[-- True if task Pool(K) is terminated @\(see @RefSecNum{Task Units and Task Objects})]
Date'Size          @RI[-- number of bits for records of type Date @\(see @RefSecNum{Record Types})]
Message'Address    @RI[-- address of the record variable Message @\(see @RefSecNum{Discriminant Constraints})]
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
We now uniformly treat X'Range as X'First..X'Last,
allowing its use with scalar subtypes.

We allow any integer type in the @SynI{static_}@nt{expression}
of an attribute designator, not
just a value of @i(universal_integer). The preference rules
ensure upward compatibility.
@end{Extend83}

@begin{DiffWord83}
We use the syntactic category @nt{attribute_reference} rather
than simply "attribute" to avoid confusing the name of something with
the thing itself.

The syntax rule for @nt{attribute_reference}
now uses @nt{identifier} instead of
@nt{simple_name}, because attribute @nt{identifier}s are not required to
follow the normal visibility rules.

We now separate @nt{attribute_reference}
from @nt{range_attribute_reference},
and enumerate the reserved words that are legal attribute or range attribute
designators.
We do this because @nt{identifier} no longer includes reserved
words.

The Ada 95 name resolution rules are a bit more explicit than in Ada 83.
The Ada 83 rule said that the
  "meaning of the prefix of an attribute must be determinable
  independently of the attribute designator and independently
  of the fact that it is the prefix of an attribute."  That isn't
  quite right since the meaning even in Ada 83 embodies whether or not
  the prefix is interpreted as a parameterless function call,
  and in Ada 95, it also embodies whether or not the prefix is interpreted
  as an implicit_dereference. So the attribute designator does
  make a difference @em just not much.

  Note however that if the attribute designator is Access,
  it makes a big difference in the interpretation of the
  prefix (see @RefSecNum(Operations of Access Types)).
@end{DiffWord83}

@begin{DiffWord95}
@ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0015],ARef=[AI95-00093-01]}
@Chg{Version=[2],New=[@b<Corrigendum:> The wording
was changed to allow implementations to continue to implement the Ada 83 Small
attribute. This was always intended to be allowed (and never was
disallowed).],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00235-01]}
@Chg{Version=[2],New=[The note about resolving prefixes of attributes was
updated to reflect that the prefix of an Access attribute now has an expected
type (see @RefSecNum{Operations of Access Types}).],Old=[]}
@end{DiffWord95}


@LabeledClause{Literals}

@begin{Intro}
@Redundant[@Defn{literal}
A @i(literal) represents a value literally, that is, by means
of notation suited to its kind.]
A literal is either a @nt<numeric_literal>, a @nt<character_literal>,
the literal @key(null), or a @nt<string_literal>.
@IndexSeeAlso{Term=[constant],See=(literal)}
@begin(Discussion)
  An enumeration literal that is an @nt<identifier>
  rather than a @nt<character_literal> is not considered a @i(literal)
  in the above sense, because it involves no special notation
  @lquotes@;suited to its kind.@rquotes@;
  It might more properly be called an @nt<enumeration_identifier>,
  except for historical reasons.
@end(Discussion)
@end{Intro}

@begin{Resolution}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00230-01]}
@ChgDeleted{Version=[2],Text=[@PDefn2{Term=[expected type],Sec=(null literal)}
The expected type for a literal @key(null) shall be a single
access type.]}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[This new wording ("expected type ... shall be a
single ... type") replaces the old "shall be determinable" stuff. It reflects
an attempt to simplify and unify the description of the rules for resolving
aggregates, literals, type conversions, etc. See
@RefSec{The Context of Overload Resolution} for the details.]}
@end{Discussion}

@PDefn2{Term=[expected type],Sec=(character_literal)}
@PDefn2{Term=[expected profile],Sec=(character_literal)}
For a @nt<name> that consists of a @nt<character_literal>,
either its expected type shall be a single character type, in which case
it is interpreted as a parameterless @nt<function_call> that yields
the corresponding value of the character type,
or its expected profile shall correspond to a parameterless
function with a character result type, in which case it
is interpreted as the name of the corresponding parameterless
function declared as part of the character type's definition
(see @RefSecNum(Enumeration Types)).
In either case, the @nt{character_literal} denotes the
@nt{enumeration_literal_specification}.
@begin{Discussion}
  See @RefSecNum(Selected Components) for the resolution rules for a
  @nt<selector_name> that is a @nt<character_literal>.
@end{Discussion}

@PDefn2{Term=[expected type],Sec=(string_literal)}
The expected type for a @nt{primary} that is a @nt<string_literal>
shall be a single string type.
@end{Resolution}

@begin{Legality}
A @nt{character_literal} that is a @nt<name> shall correspond to a
@nt<defining_character_literal> of the expected type, or
of the result type of the expected profile.

For each character of a @nt{string_literal} with a given
expected string type, there shall be
a corresponding @nt<defining_character_literal> of
the component type of the expected string type.

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00230-01],ARef=[AI95-00231-01]}
@ChgDeleted{Version=[2],Text=[A literal @nt<null> shall not be of an anonymous
access type@Redundant[, since such types do not have a null value
(see @RefSecNum{Access Types})].]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[This is a legality rule rather than an overloading
rule, to simplify implementations.]}
@end{Reason}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
An integer literal is of type @i{universal_integer}.
A real literal is of type @i{universal_real}.@Chg{Version=[2],New=[ The literal
@key<null> is of type @i<universal_access>.],Old=[]}
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[evaluation], Sec=(numeric literal)}
@PDefn2{Term=[evaluation], Sec=(null literal)}
@Defn{null access value}
@IndexSee{Term=[null pointer],See=(null access value)}
The evaluation of a numeric literal, or the literal @key(null),
yields the represented value.

@PDefn2{Term=[evaluation], Sec=(string_literal)}
The evaluation of a @nt{string_literal} that is a @nt<primary>
yields an array value containing the value of each character of the
sequence of characters of the @nt<string_literal>,
as defined in @RefSecNum{String Literals}.
The bounds of this array value are determined according to the rules for
@nt<positional_array_aggregate>s (see @RefSecNum{Array Aggregates}),
except that for a null string literal, the upper bound is the predecessor
of the lower bound.

@IndexCheck{Range_Check}
For the evaluation of a @nt<string_literal> of type @i(T),
a check is made that the value of each
character of the @nt<string_literal> belongs to the component
subtype of @i(T).
For the evaluation of a null string literal, a check is made that its
lower bound is greater than the lower bound of the base range
of the index type.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
The exception Constraint_Error is raised if either of these checks fails.
@begin{Ramification}
The checks on the characters need not involve more than two
checks altogether, since one need only check the characters
of the string with the
lowest and highest position numbers against the range of the
component subtype.
@end{Ramification}
@end{RunTime}

@begin{Notes}
Enumeration literals that are @nt<identifier>s rather than
@nt<character_literal>s follow the normal rules for @nt<identifier>s
when used in a @nt<name>
(see @RefSecNum{Names} and @RefSecNum{Selected Components}).
@nt<Character_literal>s used as @nt<selector_name>s follow the normal
rules for expanded names (see @RefSecNum{Selected Components}).
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of literals:)
@begin{Example}
@tabclear()@tabset(P16)
3.14159_26536 @\@RI[--  a real literal]
1_345 @\@RI[--  an integer literal]
'A' @\@RI[--  a character literal]
"Some Text" @\@RI[--  a string literal ]
@end{Example}
@end{Examples}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
Because @nt<character_literal>s are now treated like
other literals, in that they are resolved using context
rather than depending on direct visibility, additional
qualification might be necessary when passing a @nt<character_literal>
to an overloaded subprogram.
@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
@nt<Character_literal>s are now treated
analogously to @key(null) and @nt<string_literal>s, in that
they are resolved using context, rather than their content;
the declaration of the corresponding @nt<defining_character_literal>
need not be directly visible.
@end{Extend83}

@begin{DiffWord83}
Name Resolution rules for enumeration literals that are not
@nt<character_literal>s are not included anymore, since
they are neither syntactically
nor semantically "literals" but are rather names of parameterless
functions.
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01],ARef=[AI95-00231-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}@key{Null} now has
  type @i<universal_access>, which is similar to other literals. @key{Null}
  can be used with anonymous access types.]}
@end{Extend95}


@LabeledClause{Aggregates}

@begin{Intro}
@Redundant[@Defn{aggregate}
An @i(aggregate) combines component values
into a composite value of an array type, record type, or record extension.]
@IndexSeeAlso{Term={literal},See=(aggregate)}
@end{Intro}

@begin{Syntax}
@Syn{lhs=<aggregate>,rhs="@Syn2{record_aggregate} | @Syn2{extension_aggregate} | @Syn2{array_aggregate}"}
@end{Syntax}

@begin{Resolution}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01],ARef=[AI95-00389]}
@PDefn2{Term=[expected type],Sec=(aggregate)}
The expected type for an @nt{aggregate} shall be a
single @Chg{Version=[2],New=[composite],Old=[nonlimited array]}
type@Chg{Version=[2],New=[],Old=[, record type, or record extension]}.
@begin{Discussion}
See @RefSec{The Context of Overload Resolution}
for the meaning of @lquotes@;shall be a single ... type.@rquotes@;
@end{Discussion}
@end{Resolution}

@begin{Legality}
An @nt{aggregate} shall not be of a class-wide type.
@begin{Ramification}
When the
expected type in some context is class-wide, an aggregate has to
be explicitly qualified by the specific type of value to be created,
so that the expected type for the aggregate itself is specific.
@end{Ramification}
@begin{Discussion}
  We used to disallow @nt<aggregate>s of a type with unknown
  discriminants. However, that was unnecessarily restrictive
  in the case of an extension aggregate, and irrelevant to
  a record aggregate (since a type that is legal for a record
  aggregate could not possibly have unknown discriminants) and
  to an array aggregate (the only specific types that can
  have unknown discriminants are private types, private extensions,
  and types derived from them).
@end{Discussion}

@end{Legality}

@begin{RunTime}
@PDefn2{Term=[evaluation], Sec=(aggregate)}
For the evaluation of an @nt<aggregate>, an anonymous object
is created and values for the components or ancestor part
are obtained (as described in the subsequent subclause for each
kind of the @nt<aggregate>) and assigned into the corresponding
components or ancestor part of the anonymous object.
@Defn2{Term=[assignment operation],
  Sec=(during evaluation of an @nt{aggregate})}
Obtaining the values and the assignments occur in an
arbitrary order.
The value of the @nt{aggregate} is the value of this object.
@begin{Discussion}
  The ancestor part is the set of components inherited from the
  ancestor type. The syntactic category @nt<ancestor_part> is
  the @nt<expression> or @nt<subtype_mark> that specifies
  how the ancestor part of the anonymous object should be initialized.
@end{Discussion}
@begin{Ramification}
  The assignment operations do the necessary
  value adjustment, as described in
  @RefSecNum{User-Defined Assignment and Finalization}.
  Note that the value as a whole is not adjusted
  @em just the subcomponents (and ancestor part, if any).
  @RefSecNum{User-Defined Assignment and Finalization} also describes
  when this anonymous object is finalized.

  If the @nt<ancestor_part> is a @nt<subtype_mark>
  the Initialize procedure for the ancestor type is applied
  to the ancestor part after default-initializing it,
  unless the procedure is abstract, as described
  in @RefSecNum{User-Defined Assignment and Finalization}.
  The Adjust procedure for the ancestor type is not called
  in this case, since there is no assignment to the ancestor
  part as a whole.
@end{Ramification}

@IndexCheck{Discriminant_Check}
If an @nt{aggregate} is of a tagged type, a check is made that
its value belongs to the first subtype of the type.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
Constraint_Error is raised if this check fails.
@begin{Ramification}
This check ensures that no values of a tagged type are
ever outside the first subtype, as required
for inherited dispatching operations to work properly
(see @RefSecNum(Derived Types and Classes)). This check will always
succeed if the first subtype is unconstrained.
This check is not extended to untagged types
to preserve upward compatibility.
@end{Ramification}
@end{RunTime}

@begin{Extend83}
@Defn{extensions to Ada 83}
We now allow @nt{extension_aggregate}s.
@end{Extend83}

@begin{DiffWord83}
We have adopted new wording
for expressing the
rule that the type of an aggregate shall be determinable
from the outside, though using the fact that
it is nonlimited record (extension) or array.

An @nt{aggregate} now creates an anonymous object.
This is necessary so that controlled types
will work (see @RefSecNum{User-Defined Assignment and Finalization}).
@end{DiffWord83}

@begin{Incompatible95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Defn{incompatibilities with Ada 95}
In Ada 95, a limited type is not considered when resolving an @nt{aggregate}.
Since Ada 2005 now allows limited @nt{aggregate}s, we can have
incompatibilities. For example:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[@key{type} Lim @key{is} @key{limited}
   @key{record}
      Comp: Integer;
   @key{end} @key{record};],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[@key{type} Not_Lim @key{is}
   @key{record}
      Comp: Integer;
   @key{end} @key{record};],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[@key{procedure} P(X: Lim);
@key{procedure} P(X: Not_Lim);],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[P((Comp => 123)); -- @RI[Illegal in Ada 2005, legal in Ada 95]]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[The call to P is ambiguous in Ada 2005, while it would
not be ambiguous in Ada 95 as the @nt{aggregate} could not have a limited type.
Qualifying the @nt{aggregate} will eliminate any ambiguity. This construction
would be rather confusing to a maintenance programmer, so it should be
avoided, and thus we expect it to be rare.],Old=[]}
@end{Incompatible95}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01],ARef=[AI95-00389-01]}
@Chg{Version=[2],New=[@Defn{extensions to Ada 95}@nt{Aggregate}s can be of a
limited type and of any composite type.],Old=[]}
@end{Extend95}


@LabeledSubClause{Record Aggregates}

@begin{Intro}
@Redundant[In a @nt<record_aggregate>, a value is specified for
each component of the record or record extension value,
using either a named or a positional association.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<record_aggregate>,rhs="(@Syn2{record_component_association_list})"}

@Syn{lhs=<record_component_association_list>,rhs="
    @Syn2{record_component_association} {, @Syn2{record_component_association}}
  | @key<null record>"}


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
@Syn{lhs=<record_component_association>,rhs="
    [@Syn2{component_choice_list} =>] @Syn2{expression}@Chg{Version=[2],New=[
   | @Syn2{component_choice_list} => <>],Old=[]}"}

@Syn{lhs=<component_choice_list>,rhs="
     @SynI{component_}@Syn2{selector_name} {| @SynI{component_}@Syn2{selector_name}}
   | @key{others}"}

@begin(SyntaxText)
@Defn{named component association}
A @nt<record_@!component_@!association> is a @i(named component association)
if it has a @nt<component_choice_list>;
@Defn{positional component association}
otherwise, it is a @i(positional component association).
Any positional component associations shall precede any
named component associations.
If there is a named association with a @nt<component_choice_list>
of @key(others), it shall come last.
@begin{Discussion}
These rules were
  implied by the BNF in an early version of the RM9X, but it
  made the grammar harder to read, and was inconsistent
  with how we handle discriminant constraints.
  Note that for array aggregates we still express
  some of the rules in the grammar, but array aggregates
  are significantly different because an array aggregate
  is either all positional (with a possible @key(others) at the
  end), or all named.
@end{Discussion}

In the @nt<record_@!component_@!association_@!list> for a @nt<record_@!aggregate>,
if there is only one association, it shall be a named association.
@begin{Reason}
  Otherwise the construct would be interpreted as a parenthesized
  expression.
  This is considered a syntax rule, since it is relevant to
  overload resolution. We choose not to express it with BNF so we
  can share the definition of @nt<record_component_association_list>
  in both @nt<record_aggregate> and @nt<extension_aggregate>.
@end{Reason}
@begin{Ramification}
  The @nt<record_component_association_list> of an @nt<extension_aggregate>
  does not have such a restriction.
@end{Ramification}
@end(SyntaxText)
@end{Syntax}

@begin{Resolution}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01],ARef=[AI95-00389-01]}
@PDefn2{Term=[expected type],Sec=(record_aggregate)}
The expected type for a @nt{record_aggregate} shall be
a single @Chg{Version=[2],New=[non-array composite],Old=[nonlimited record]}
type@Chg{Version=[2],New=[],Old=[ or record extension]}.
@begin{Ramification}
This rule is used to resolve whether an @nt{aggregate} is
an @nt{array_aggregate} or a @nt{record_aggregate}.
The presence of a @key(with) is used to resolve between
a @nt{record_aggregate} and an @nt{extension_aggregate}.
@end{Ramification}

@Defn2{Term=[needed component],
  Sec=(@nt<record_aggregate> @nt<record_component_association_list>)}
For the @nt<record_@!component_@!association_@!list>
of a @nt<record_@!aggregate>,
all components of the composite value defined by
the aggregate are @i(needed)@Redundant[; for the association list of
an @nt<extension_aggregate>,
only those components not determined by the ancestor expression or
subtype are needed
(see @RefSecNum{Extension Aggregates}).]
Each @nt{selector_@!name} in a @nt{record_@!component_@!association} shall denote
a needed component @Redundant[(including possibly a discriminant)].
@begin{Ramification}
For the association list of a @nt{record_aggregate},
@lquotes@;needed components@rquotes@; includes every component of the composite value, but
does not include those in unchosen @nt{variant}s (see AI83-309).
If there are @nt<variant>s, then
the value specified for
the discriminant that governs them
determines which @nt<variant> is chosen, and hence which components
are needed.

If an extension defines a new @nt{known_discriminant_part}, then all of
its discriminants are needed in the component association list of
an extension
aggregate for that type, even if the discriminants have the same
names and types as discriminants of the type of the ancestor
expression.
This is necessary to ensure that the positions in
the @nt<record_@!component_@!association_@!list>
are well defined, and that discriminants that govern @nt{variant_part}s
can be given by static expressions.

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00389-01]}
@ChgAdded{Version=[2],Text=[For a partial view, all of the components of the full
type are needed, even though not all of them can be named in the
@nt{aggregate}. For a generic formal type, all of the components of the actual
type are needed, even though not all of them can be named in the
@nt{aggregate}. The @nt{aggregate} would include @key{others} => <>
in the case of needed but unnameable components.]}
@end{Ramification}

@Leading@Keepnext@PDefn2{Term=[expected type],
  Sec=(record_component_association expression)}
The expected type for the @nt<expression> of a
@nt<record_@!component_@!association> is the type
of the @i(associated) component(s);
@Defn2{Term=[associated components],
  Sec=(of a @nt<record_component_association>)}
the associated component(s) are as follows:
@begin(itemize)
  For a positional association,
  the component @Redundant[(including possibly a discriminant)]
  in the corresponding relative position (in the declarative region of
  the type), counting only the
  needed components;
@begin{Ramification}
    This means that for
    an association list of an @nt<extension_aggregate>,
    only noninherited components are counted to determine
    the position.
@end{Ramification}

  For a named association with one or more
  @i(component_)@nt<selector_name>s,
  the named component(s);

  For a named association with the reserved word @key(others),
  all needed components
  that are not associated with some previous association.
@end(itemize)
@end{Resolution}

@begin{Legality}

If the type of a @nt{record_aggregate} is a record extension,
then it shall be a descendant of a record type, through one
or more record extensions (and no private extensions).

If there are no components needed in a given
@nt<record_@!component_@!association_@!list>,
then the reserved words @key(null record) shall appear rather
than a list of @nt<record_@!component_@!association>s.
@begin{Ramification}
  For example, "(@key(null record))" is a @nt<record_aggregate>
  for a null record type. Similarly, "(T'(A) @key(with null record))" is
  an @nt<extension_aggregate> for a type defined as a null
  record extension of T.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01],ARef=[AI95-00389-01]}
Each @nt<record_component_association>@Chg{Version=[2],New=[ other than an
@key{others} choice with a <>],Old=[]} shall have at least
one associated component, and each needed component
shall be associated with exactly
one @nt<record_@!component_@!association>.
If a @nt<record_@!component_@!association> @Chg{Version=[2],New=[with an @nt{expression} ],Old=[]}has
two or more associated components, all of them shall be of the same type.
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00389-01]}
  These rules apply to an association with an @key(others)
  choice@Chg{Version=[2],New=[ with an expression. An @key(others) choice with
  a <> can match zero components or several components with different
  types],Old=[]}.
@end{Ramification}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
  Without these rules, there would be no way to know what
  was the expected type for the @nt<expression> of the association.
  @Chg{Version=[2],New=[Note that the second rule does not apply to <>
  associations, as we do not need to resolve anything. That means that
  (@key{others} => <>) always represents a default-initialized composite
  value.],Old=[]}
@end{Reason}
@begin{Discussion}
  AI83-00244 also requires that the @nt{expression} shall
  be legal for each associated component. This is because
  even though two components have the same type, they might have
  different subtypes. Therefore, the legality of the
  @nt<expression>, particularly if it is an array aggregate,
  might differ depending on the associated component's subtype.
  However, we have relaxed the rules on array aggregates slightly for Ada 95,
  so the staticness of an applicable index constraint has no
  effect on the legality of the array aggregate to which it applies.
  See @RefSecNum{Array Aggregates}. This was the only case (that we know of)
  where a subtype provided by context affected the legality
  of an @nt{expression}.
@end{Discussion}
@begin{Ramification}
  The rule that requires at least one associated component for
  each @nt<record_component_association>
  implies that there can be no extra associations for
  components that don't exist in the composite value, or that
  are already determined by the ancestor expression or subtype of
  an @nt<extension_aggregate>.

  The second part of the first sentence ensures that no
  needed components are left out,
  nor specified twice.
@end{Ramification}

If the components of a @nt{variant_part} are needed, then the value
of a discriminant that governs the @nt{variant_part} shall be given
by a static expression.
@begin{Ramification}
This expression might either be given within the aggregate itself,
or in a constraint on the parent subtype in a @nt<derived_type_definition>
for some ancestor of the type of the aggregate.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00287-01]}
@ChgAdded{Version=[2],Text=[A @nt<record_component_association> for a discriminant
without a @nt<default_expression> shall have an @nt<expression> rather
than <>.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[A discriminant must always have a defined value,
but <> means uninitialized for a discrete type unless the component has a
default value.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00389-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[If the type of the
@nt{record_aggregate} is a partial view of a type, a task type, a protected
type, a formal private type, or a formal derived type:]}
@begin{Itemize}
  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00389-01]}
  @ChgAdded{Version=[2],Text=[The @nt{record_component_association_list} shall
  include @key{others} => <>; and]}
  @begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[For partial views and generic formals, this is needed
   to avoid "leaking" information about the full type to the @nt{aggregate}.
   Without this rule, if the full type was @key{null record}, @key{null record}
   could be used; and if the full type had only one component, @key{others} =>
   @i<<some-expression>> could be used. That would clearly break privacy.
   Similar issues apply to formal types; if information about the actual type
   could be used in the aggregate, we'd have a generic contract issue. Since
   each component can only occur in a single association, this rule prevents
   those issues.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[For tasks and protected types, this recognizes
   that there may be components (such as the lock for a protected type) that
   are known only to the implementation; these components necessarily have
   to be given default initialization. For portability, we don't want
   aggregates to be written that depend on whether the implementation has
   such components.]}
  @end{Reason}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00389-01]}
  @ChgAdded{Version=[2],Text=[The type of the @nt{record_aggregate} shall have known
  discriminants or be a tagged, task, or protected type that does not have
  unknown discriminants; and]}

  @begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We only want to allow @nt{aggregate}s of partial
   views when the full type is certain to be composite. Otherwise, we would
   have anomalies where additional visibility would cause the ability to have
   @nt{aggregate}s to disappear (if the full type is elementary).
   The full type is certain to be composite if the partial view has known
   discriminants or is tagged. Task and protected types are also allowed, as
   they are always composite.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We also disallow types with unknown
   discriminants. Unknown discriminants are often used to prevent uncontrolled
   initialization of objects, and we don't want to provide an uncontrolled
   initialization mechanism for such types. Moreover, we would have break
   privacy to make them work. For instance:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package} P @key{is}
   @key{type} T (<>) @key{is tagged private};
@key{private}
   @key{type} T (D : Natural) @key{is tagged record}
      C : String (1..D);
   @key{end record};
@key{end} P;]}
@end{Example}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We couldn't allow giving an aggregate for T
  outside of P, because we don't know what value to give the discriminant D.
  But that would require looking in the private part to see if D had a default.
  It's better to disallow unknown discriminants generally.]}
  @end{Reason}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00389-01]}
  @ChgAdded{Version=[2],Text=[The @nt{record_component_association_list} shall not
  include a positional component association.]}

  @begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Otherwise, positional notation could be used to give
   associations for components of the full view that are not visible to the
   @nt{aggregate}. That would be bad. We disallow positional notation
   completely to avoid confusion and complexity; this shouldn't be a hardship
   since typically there will be no more than a couple discriminants along with
   @key{others} => <> in such an @nt{aggregate}.]}
  @end{Reason}
@end{Itemize}

@end{Legality}

@begin{RunTime}
@PDefn2{Term=[evaluation], Sec=(record_aggregate)}
The evaluation of a @nt<record_aggregate> consists of the
evaluation of the @nt<record_@!component_@!association_@!list>.

@PDefn2{Term=[evaluation], Sec=(record_component_association_list)}
For the evaluation of a @nt{record_@!component_@!association_@!list},
any per-object constraints (see @RefSecNum(Record Types))
for components specified in the association list are elaborated and
any @nt<expression>s are evaluated and converted to the subtype of the
associated component.
@PDefn2{Term=[implicit subtype conversion],Sec=(expressions in aggregate)}
Any constraint elaborations and @nt{expression} evaluations (and conversions)
occur in an arbitrary order, except that the @nt<expression>
for a discriminant is evaluated (and converted) prior to the
elaboration of any per-object constraint that depends on it, which in
turn occurs prior to the evaluation and conversion of the @nt{expression} for
the component with the per-object constraint.
@begin{Ramification}
The conversion in the first rule might raise Constraint_Error.
@end{Ramification}
@begin{Discussion}
This check in the first rule presumably happened as part of the dependent
compatibility check in Ada 83.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00287-01]}
@ChgAdded{Version=[2],Text=[For a @nt<record_component_association> with an
@nt<expression>, the @nt<expression> defines the value for the associated
component(s). For a @nt<record_component_association> with <>, if the
@nt<component_declaration> has a @nt<default_expression>, that
@nt<default_expression> defines the value for the associated component(s);
otherwise, the associated component(s) are initialized by default as for a
stand-alone object of the component subtype
(see @RefSecNum{Object Declarations}).]}

The @nt<expression> of a @nt{record_component_association}
is evaluated (and converted) once for each associated component.

@end{RunTime}

@begin{Notes}
For a @nt<record_aggregate> with positional associations, expressions
specifying discriminant
values appear first since the @nt<known_discriminant_part>
is given first in the declaration of the type; they have to
be in the same order as in the @nt<known_discriminant_part>.
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Example of a record aggregate with positional associations:)
@begin{Example}
(4, July, 1776)                                       @RI[--  see @RefSecNum{Record Types} ]
@end{Example}

@begin{Wide}
@leading@keepnext@i(Examples of record aggregates with named associations:)
@end{Wide}
@begin{Example}
(Day => 4, Month => July, Year => 1776)
(Month => July, Day => 4, Year => 1776)

(Disk, Closed, Track => 5, Cylinder => 12)            @RI[--  see @RefSecNum{Variant Parts and Discrete Choices}]
(Unit => Disk, Status => Closed, Cylinder => 9, Track => 1)
@end{Example}

@begin{Wide}
@leading@keepnext@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
@i(Example@Chg{Version=[2],New=[s],Old=[]} of component association with
several choices:)
@end{Wide}
@begin{Example}
@tabclear()@tabset(P50)
(Value => 0, Succ|Pred => @key(new) Cell'(0, @key(null), @key(null))) @\@RI[--  see @RefSecNum{Incomplete Type Declarations}]

 @RI[--  The allocator is evaluated twice: Succ and Pred designate different cells]

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[(Value => 0, Succ|Pred => <>) @\@RI[--  see @RefSecNum{Incomplete Type Declarations}]],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[ @RI[--  Succ and Pred will be set to @key{null}]],Old=[]}

@end{Example}

@begin{Wide}
@leading@keepnext@i{Examples of record aggregates for tagged types
(see @RefSecNum(Tagged Types and Type Extensions)
and @RefSecNum{Type Extensions}):}
@end{Wide}
@begin{Example}
Expression'(@key{null record})
Literal'(Value => 0.0)
Painted_Point'(0.0, Pi/2.0, Paint => Red)
@end{Example}

@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
Null record aggregates may now be specified, via "(@key(null record))".
However, this syntax is more useful for null record extensions in
extension aggregates.
@end{Extend83}

@begin{DiffWord83}
Various AIs have been incorporated (AI83-00189, AI83-00244, and AI83-00309).
In particular, Ada 83 did not explicitly disallow extra values in
a record aggregate. Now we do.
@end{DiffWord83}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
@Chg{Version=[2],New=[@Defn{extensions to Ada 95}<> can be used in place of
an @nt{expression} in a @nt{record_aggregate}, default initializing the
component.],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00389-01]}
@Chg{Version=[2],New=[A @nt{record_aggregate} can be of any non-array
composite type, other than partial views whose full type @i{might} be an
elementary type. For partial views, task types, protected types, and generic
formal types, @key{others} => <> is required, as all of the components are
not (necessarily) visible to the @nt{record_aggregate}.],Old=[]}
@end{Extend95}

@begin{DiffWord95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
@Chg{Version=[2],New=[Limited @nt{record_aggregate}s are allowed (since
all kinds of aggregates can now be limited, see @RefSecNum{Aggregates}).],Old=[]}
@end{DiffWord95}


@LabeledSubClause{Extension Aggregates}

@begin{Intro}
@Redundant[An @nt<extension_aggregate> specifies a value
for a type that is a record extension by specifying a value
or subtype
for an ancestor of the type,
followed by associations for
any components not determined by the @nt<ancestor_part>.]
@end{Intro}

@begin{MetaRules}
The model underlying this syntax is that a record extension
can also be viewed as a regular record type with an ancestor "prefix."
The @nt<record_@!component_@!association_@!list> corresponds to
exactly what would be needed
if there were no ancestor/prefix type.
The @nt{ancestor_part}
determines the value of the ancestor/prefix.
@end{MetaRules}

@begin{Syntax}
@Syn{lhs=<extension_aggregate>,rhs="
    (@Syn2{ancestor_part} @key(with) @Syn2{record_component_association_list})"}

@Syn{lhs=<ancestor_part>,
  rhs="@Syn2{expression} | @Syn2{subtype_mark}"}
@end{Syntax}

@begin{Resolution}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01],ARef=[AI95-00389-01]}
@PDefn2{Term=[expected type], Sec=(extension_aggregate)}
The expected type for an @nt{extension_aggregate} shall be
a single @Chg{Version=[2],New=[],Old=[nonlimited ]}type that is a
@Chg{Version=[2],New=[type],Old=[record]} extension.
@PDefn2{Term=[expected type],
  Sec=(extension_aggregate ancestor expression)}
If the @nt<ancestor_part> is an @nt<expression>,
it is expected to be of any @Chg{Version=[2],New=[],Old=[nonlimited ]}tagged
type.
@begin{Reason}
We could have made
the expected type @i(T')Class where @i(T) is the ultimate ancestor of
the type of the aggregate, or we could have made it even more
specific than that. However, if the overload resolution rules
get too complicated, the implementation gets more difficult and
it becomes harder to produce good error messages.
@end{Reason}
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00306-01],ARef=[AI95-00389-01]}
If the @nt<ancestor_part> is a @nt<subtype_mark>, it shall
denote a specific tagged subtype.
The type of the @nt{extension_aggregate} shall be @Chg{Version=[2],New=[a
descendant of],Old=[derived from]} the type of the
@nt<ancestor_part>@Chg{Version=[2],New=[],Old=[, through one
or more record extensions (and no private extensions)]}.
@Chg{Version=[2],New=[If the @nt{ancestor_part} is an @nt{expression}, it
shall not be dynamically tagged. If the type of the
@nt{extension_aggregate} is derived from the type of the @nt{ancestor_part}
through one or more private extensions, then the
@nt{record_component_association_list} shall include @key{others} => <>, and
shall not include a positional component association.],Old=[]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00306-01]}
  @ChgAdded{Version=[2],Text=[The expression cannot be dynamically tagged to
  prevent implicit "truncation" of a dynamically-tagged value to the specific
  ancestor type. This is similar to the
  rules in @RefSecNum{Dispatching Operations of Tagged Types}.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00389-01]}
  @ChgAdded{Version=[2],Text=[We allow the type of the @nt{ancestor_part} to be
  the type itself, so that these @nt{aggregate}s are allowed for generic
  formal derived types, where the actual type might be the same as the
  ancestor type.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00389-01]}
  @ChgAdded{Version=[2],Text=[If the type involves any private extensions, we
  require the use of @key{others} => <> and disallow positional associations
  in order to avoid breaking privacy of the extensions.
  See @RefSecNum{Record Aggregates} for a more complete discussion.]}
@end{Reason}
@end{Legality}

@begin{StaticSem}
@Defn2{Term=[needed component],
  Sec=(@nt<extension_aggregate> @nt<record_component_association_list>)}
For the @nt{record_@!component_@!association_@!list}
of an @nt{extension_@!aggregate},
the only components @i(needed) are those of the composite value defined
by the aggregate that are not inherited from the type of
the @nt<ancestor_@!part>, plus any inherited discriminants
if the @nt<ancestor_@!part> is a @nt<subtype_@!mark> that
denotes an unconstrained subtype.
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[evaluation], Sec=(extension_aggregate)}
For the evaluation of an @nt{extension_aggregate},
the @nt{record_@!component_@!association_@!list} is evaluated.
If the @nt<ancestor_part> is an @nt<expression>, it is also evaluated;
if the @nt<ancestor_part> is a @nt<subtype_mark>,
the components of the value of the aggregate not given by the
@nt<record_@!component_@!association_@!list> are initialized by default
as for an object of the ancestor type.
Any implicit initializations or evaluations are performed
in an arbitrary order, except that the @nt<expression>
for a discriminant is evaluated prior to any other evaluation
or initialization that depends on it.

@IndexCheck{Discriminant_Check}
If the type of the @nt<ancestor_part> has
discriminants that are not inherited by the
type of the @nt{extension_aggregate},
then, unless the @nt<ancestor_part> is a @nt<subtype_mark> that
denotes an unconstrained subtype,
a check is made that each discriminant of the ancestor
has the value specified for a corresponding discriminant,
either in the @nt{record_@!component_@!association_@!list}, or in
the @nt<derived_type_definition> for some ancestor of the type of
the @nt{extension_aggregate}.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
Constraint_Error is raised if this check fails.
@begin{Ramification}
Corresponding and specified
discriminants are defined in @RefSecNum{Discriminants}.
The rules requiring static compatibility between
new discriminants of a derived type
and the parent discriminant(s) they constrain
ensure that at most one check is required per discriminant
of the ancestor expression.
@end{Ramification}
@end{RunTime}

@begin{Notes}
If all components of the value of the @nt<extension_aggregate>
are determined by the @nt<ancestor_part>, then
the @nt<record_@!component_@!association_@!list> is required to be
simply @key(null record).

If the @nt<ancestor_part> is a @nt<subtype_mark>,
then its type can be abstract. If its type is controlled,
then as the last step of evaluating the aggregate,
the Initialize procedure of the ancestor type is called,
unless the Initialize procedure is abstract
(see @RefSecNum{User-Defined Assignment and Finalization}).
@end{Notes}

@begin{Examples}
@Leading@keepnext@i{Examples of extension aggregates (for types defined in @RefSecNum{Type Extensions}):}
@begin(example)
Painted_Point'(Point @key{with} Red)
(Point'(P) @key{with} Paint => Black)

(Expression @key{with} Left => 1.2, Right => 3.4)
Addition'(Binop @key{with null record})
             @RI[-- presuming Binop is of type Binary_Operation]
@end(example)

@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
The extension aggregate syntax is new.
@end{Extend83}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00389-01]}
@Chg{Version=[2],New=[@Defn{extensions to Ada 95}An
@nt{extension_aggregate} can be of any type extension, including partial
views. For partial views (including generic formal derived types),
@key{others} => <> is required, as all of the extension components are
not (necessarily) visible to the @nt{extension_aggregate}.],Old=[]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
  @ChgAdded{Version=[2],Text=[Limited @nt{extension_aggregate}s are allowed (since
  all kinds of aggregates can now be limited, see @RefSecNum{Aggregates}).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00306-01]}
  @ChgAdded{Version=[2],Text=[Eliminated implicit @lquotes@;truncation@rquotes
  of a dynamically-tagged value when it is used as an ancestor @nt{expression}.]}
@end{DiffWord95}


@LabeledSubClause{Array Aggregates}

@begin{Intro}

@Redundant[In an @nt<array_aggregate>, a value is specified
for each component of an array, either positionally or by
its index.]
For a @nt{positional_array_aggregate},
the components are given in increasing-index order,
with a final @key[others], if any,
representing any remaining components.
For a @nt{named_array_aggregate},
the components are identified by the values covered by the
@nt{discrete_choice}s.

@end{Intro}

@begin{MetaRules}
@ChgRef{Version=[1],Kind=[Revised]}
The rules in this subclause are based on
terms and rules for @nt{discrete_choice_list}s
defined in @RefSec{Variant Parts and Discrete Choices}.
@Chg{New=[For example, the requirements that @key(others) come last and stand
alone are found there.],Old=[]}@Comment{This question is asked periodically, so
we answer it explicitly.}
@end{MetaRules}

@begin{Syntax}
@Syn{lhs=<array_aggregate>,rhs="
  @Syn2{positional_array_aggregate} | @Syn2{named_array_aggregate}"}


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
@Syn{lhs=<positional_array_aggregate>,rhs="
    (@Syn2{expression}, @Syn2{expression} {, @Syn2{expression}})
  | (@Syn2{expression} {, @Syn2{expression}}, @key(others) => @Syn2{expression})@Chg{Version=[2],New=[
  | (@Syn2{expression} {, @Syn2{expression}}, @key(others) => <>)],Old=[]}"}


@Syn{lhs=<named_array_aggregate>,rhs="
    (@Syn2{array_component_association} {, @Syn2{array_component_association}})"}


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
@Syn{lhs=<array_component_association>,rhs="
    @Syn2{discrete_choice_list} => @Syn2{expression}@Chg{Version=[2],New=[
  | @Syn2{discrete_choice_list} => <>],Old=[]}"}

@end{Syntax}

@begin{Intro}
@Defn{n-dimensional @nt<array_aggregate>}
An @i(n-dimensional) @nt<array_aggregate> is one that is written as
n levels of nested @nt{array_aggregate}s (or at the bottom level,
equivalent @nt{string_literal}s).
@Defn2{Term=[subaggregate], Sec=(of an @nt{array_aggregate})}
For the multidimensional case (n >= 2) the @nt<array_aggregate>s
(or equivalent @nt<string_literal>s)
at the n@en@;1 lower levels are called @i(subaggregate)s of the
enclosing n-dimensional @nt<array_aggregate>.
@Defn{array component expression}
The @nt<expression>s of the bottom level subaggregates (or of the
@nt<array_aggregate> itself if one-dimensional)
are called the @i(array component expressions) of the enclosing
n-dimensional @nt<array_aggregate>.
@begin(Ramification)
  Subaggregates do not have a type. They correspond to part of
  an array. For example, with a matrix, a subaggregate would correspond to
  a single row of the matrix.
  The definition of "n-dimensional" @nt<array_aggregate>
  applies to subaggregates as well
  as @nt<aggregate>s that have a type.
@end(Ramification)
@begin(Honest)
@Defn{others choice}
An @i(@key(others) choice) is
the reserved word @key(others) as it appears in
a @nt{positional_array_aggregate} or as the
@nt{discrete_choice} of the @nt{discrete_choice_list}
in an @nt{array_component_association}.
@end(Honest)

@end{Intro}

@begin{Resolution}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
@PDefn2{Term=[expected type], Sec=(array_aggregate)}
The expected type for an @nt{array_aggregate} (that is not
a subaggregate) shall be a
single @Chg{Version=[2],New=[],Old=[nonlimited ]}array type.
@PDefn2{Term=[expected type],
  Sec=(array_aggregate component expression)}
The component type of this array type is the
expected type for each array component expression of
the @nt<array_aggregate>.
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
We already require a single array or record type or
record extension for an @nt{aggregate}.
The above rule requiring a single @Chg{Version=[2],New=[],
Old=[nonlimited ]}array type
(and similar ones for record and extension aggregates)
resolves which kind of aggregate you have.
@end{Ramification}

@PDefn2{Term=[expected type],
  Sec=(array_aggregate discrete_choice)}
The expected type for each
@nt{discrete_choice} in any @nt{discrete_choice_list} of
a @nt{named_array_aggregate} is the type of the @i(corresponding index);
@Defn2{Term=[corresponding index], Sec=(for an @nt{array_aggregate})}
the corresponding index for an @nt<array_aggregate> that is not
a subaggregate is the first index of its type; for an (n@en@;m)-dimensional
subaggregate within an @nt<array_aggregate> of an n-dimensional type,
the corresponding index is the index in position m+1.
@end{Resolution}

@begin{Legality}
An @nt<array_aggregate> of an n-dimensional array type shall be
written as an n-dimensional @nt<array_aggregate>.
@begin(Ramification)
In an m-dimensional @nt<array_aggregate> @Redundant[(including a subaggregate)],
where m >= 2, each of the @nt<expression>s
has to be an (m@en@;1)-dimensional subaggregate.
@end(Ramification)

@Leading@;An @key(others) choice is allowed for an @nt<array_aggregate>
only if an @i(applicable index
constraint) applies to the @nt{array_aggregate}.
@Defn{applicable index constraint}
@Redundant[An applicable index constraint is
a constraint provided by certain contexts where an @nt{array_aggregate}
is permitted that can be used
to determine the bounds of the array value specified by the aggregate.]
Each of the following contexts (and none other)
defines an applicable index constraint:
@begin(itemize)
  For an @nt{explicit_actual_parameter},
  an @nt{explicit_generic_actual_parameter},
  the @nt{expression} of a @nt{return_statement}, the
  initialization expression
  in an @nt{object_@!declaration}, or a @nt{default_@!expression}
  @Redundant[(for a parameter or a component)],
  when the nominal subtype
  of the corresponding formal parameter, generic formal parameter,
  function result, object, or component is a constrained array subtype, the
  applicable index constraint is the constraint of the subtype;

  For the @nt{expression} of an @nt{assignment_statement} where
  the @nt{name} denotes an array variable, the
  applicable index constraint is the constraint of the array variable;
@begin{Reason}
  This case is broken out because the constraint comes from the actual
  subtype of the variable (which is always constrained)
  rather than its nominal subtype (which might be unconstrained).
@end{Reason}

  For the operand of a @nt{qualified_expression}
  whose @nt{subtype_mark}
  denotes a constrained array subtype, the applicable index constraint
  is the constraint of the subtype;

  For a component @nt{expression} in an @nt{aggregate},
  if the component's nominal subtype is a constrained
  array subtype, the applicable index constraint is the constraint
  of the subtype;
  @begin{Discussion}
  Here, the @nt{array_aggregate} with @key[others]
  is being used within a larger aggregate.
  @end{Discussion}

  For a parenthesized @nt{expression}, the
  applicable index constraint is that, if any, defined for the
  @nt{expression}.
@begin{Discussion}
RM83 omitted this
  case, presumably as an oversight. We want to minimize situations
  where an @nt{expression} becomes illegal if parenthesized.
@end{Discussion}
@end(itemize)

The applicable index constraint @i(applies) to an @nt{array_aggregate}
that appears in such a context, as well as to any subaggregates thereof.
In the case of an @nt<explicit_actual_parameter> (or @nt<default_expression>)
for a call on a generic formal subprogram,
no applicable index constraint is defined.
@begin(Reason)
  This avoids generic contract model problems,
  because only mode conformance is required when matching
  actual subprograms with generic formal subprograms.
@end(Reason)

The @nt{discrete_choice_list} of an
@nt{array_component_association} is allowed to
have a @nt{discrete_choice} that is a nonstatic @nt<expression>
or that is a @nt{discrete_range} that defines a nonstatic or
null range, only if it is the single @nt{discrete_choice} of
its @nt{discrete_choice_list}, and there is only one
@nt{array_component_association} in the @nt<array_aggregate>.
@begin{Discussion}
We now
allow a nonstatic @key(others) choice even if there are
other array component expressions as well.
@end{Discussion}

In a @nt<named_array_aggregate> with more than one @nt<discrete_choice>,
no two @nt<discrete_choice>s are allowed to
cover the same value (see @RefSecNum{Variant Parts and Discrete Choices});
if there is no @key[others] choice, the @nt<discrete_choice>s taken
together shall
exactly cover a contiguous sequence of values of the corresponding index type.
@begin{Ramification}
  This implies that each component must be
  specified exactly once. See AI83-309.
@end{Ramification}

A bottom level subaggregate of a multidimensional @nt<array_aggregate>
of a given array type
is allowed to be a @nt<string_literal> only if the component type of the
array type is a character type;
each character of such a @nt{string_literal} shall correspond to
a @nt<defining_character_literal> of the component type.
@end{Legality}

@begin{StaticSem}
A subaggregate that is a @nt<string_literal> is equivalent
to one that is a @nt<positional_array_aggregate> of the same length,
with each @nt<expression> being the @nt<character_literal>
for the corresponding character of the @nt<string_literal>.
@end{StaticSem}

@begin{RunTime}
@Leading@PDefn2{Term=[evaluation], Sec=(array_aggregate)}
The evaluation of an @nt{array_aggregate} of a given array type
proceeds in two steps:
@begin(enumerate)
  Any @nt{discrete_choice}s of this aggregate and of its subaggregates
  are evaluated in an arbitrary order, and converted to the corresponding
  index type;
  @PDefn2{Term=[implicit subtype conversion],Sec=(choices of aggregate)}

  The array component expressions of the aggregate
  are evaluated in an arbitrary order and
  their values are converted to the component subtype of
  the array type; an array component expression
  is evaluated once for each associated component.
  @PDefn2{Term=[implicit subtype conversion],Sec=(expressions of aggregate)}
@end(enumerate)
@begin(Ramification)
  Subaggregates are not separately evaluated.
  The conversion of the value of the component expressions
  to the component subtype might raise Constraint_Error.
@end(Ramification)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00287-01]}
@ChgAdded{Version=[2],Text=[Each @nt<expression> in an
@nt<array_component_association> defines the value for the associated
component(s). For an @nt<array_component_association> with <>, the associated
component(s) are initialized by default as for a stand-alone object of the
component subtype (see @RefSecNum{Object Declarations}).]}

@Leading@Defn2{Term=[bounds],
  Sec=(of the index range of an @nt{array_aggregate})}
The bounds of the index range of an @nt{array_aggregate} @Redundant[(including
a subaggregate)]
are determined as follows:
@begin(itemize)
  For an @nt{array_aggregate} with an @key(others) choice, the bounds are
  those of the corresponding index range from the applicable
  index constraint;

  For a @nt{positional_array_aggregate} @Redundant[(or equivalent
  @nt<string_literal>)]
  without an @key(others)
  choice, the lower bound is that of the corresponding index range in the
  applicable index constraint, if defined, or that of the corresponding
  index subtype, if not; in either case, the upper bound is
  determined from the lower bound and the number of @nt<expression>s
  @Redundant[(or the length of the @nt<string_literal>)];

  For a @nt{named_array_aggregate} without an @key(others) choice,
  the bounds are determined by the smallest and largest index values
  covered by any @nt{discrete_choice_list}.
@begin{Reason}
  We don't need to say that each index value has to be covered exactly
  once, since that is a ramification of the general rule
  on @nt{aggregate}s that each component's value has to be specified
  exactly once.
@end{Reason}
@end(itemize)

@IndexCheck{Range_Check}
For an @nt<array_aggregate>, a check is made
that the index range defined by its bounds
is compatible with the corresponding index subtype.
@begin{Discussion}
In RM83, this was
phrased more explicitly, but once we define "compatibility"
between a range and a subtype, it seems to make sense to
take advantage of that definition.
@end{Discussion}
@begin(Ramification)
  The definition of compatibility handles the special case
  of a null range, which is always compatible with
  a subtype. See AI83-00313.
@end(Ramification)

@IndexCheck{Index_Check}
For an @nt{array_aggregate} with an @key(others) choice,
a check is made that no @nt<expression> is specified
for an index value outside the bounds determined by the
applicable index constraint.
@begin{Discussion}
RM83 omitted this case,
apparently through an oversight. AI83-00309 defines this
as a dynamic check, even though other Ada 83 rules ensured
that this check could be performed statically. We now allow
an @key(others) choice to be dynamic, even if
it is not the only choice, so this check now needs to be
dynamic, in some cases. Also, within a generic unit,
this would be a nonstatic check in some cases.
@end{Discussion}

@IndexCheck{Index_Check}
For a multidimensional @nt{array_aggregate}, a check is made
that all subaggregates that correspond to the same index have the same bounds.
@begin{Ramification}
  No array bounds @lquotes@;sliding@rquotes@; is performed on subaggregates.
@end{Ramification}
@begin{Reason}
  If sliding were performed, it would not be obvious which
  subaggregate would determine the bounds of the corresponding index.
@end{Reason}

@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
The exception Constraint_Error is raised if any of the above
checks fail.
@end{RunTime}

@begin{Notes}
In an @nt<array_aggregate>, positional notation may only be used
with two or more @nt<expression>s; a single @nt<expression>
in parentheses is interpreted as a @nt<parenthesized_expression>.
A @nt<named_array_aggregate>, such as (1 => X), may be used to specify
an array with a single component.
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of array aggregates with positional associations:)
@begin{Example}
(7, 9, 5, 1, 3, 2, 4, 8, 6, 0)
Table'(5, 8, 4, 1, @key(others) => 0)  @RI[--  see @RefSecNum{Array Types} ]
@end{Example}

@begin{Wide}
@leading@keepnext@i(Examples of array aggregates with named associations:)
@end{Wide}
@begin{Example}
(1 .. 5 => (1 .. 8 => 0.0))      @RI[--  two-dimensional]
(1 .. N => @key(new) Cell)             @RI[--  N new cells, in particular for N = 0]

Table'(2 | 4 | 10 => 1, @key(others) => 0)
Schedule'(Mon .. Fri => True,  @key(others) => False)  @RI[--  see @RefSecNum{Array Types}]
Schedule'(Wed | Sun  => False, @key(others) => True)
Vector'(1 => 2.5)                                @RI[--  single-component vector]
@end{Example}

@begin{Wide}
@leading@keepnext@i(Examples of two-dimensional array aggregates:)
@end{Wide}
@begin{Example}
@RI[-- Three aggregates for the same value of subtype Matrix(1..2,1..3) (see @RefSecNum{Array Types}):]

((1.1, 1.2, 1.3), (2.1, 2.2, 2.3))
(1 => (1.1, 1.2, 1.3), 2 => (2.1, 2.2, 2.3))
(1 => (1 => 1.1, 2 => 1.2, 3 => 1.3), 2 => (1 => 2.1, 2 => 2.2, 3 => 2.3))
@end{Example}

@begin{Wide}
@leading@keepnext@i(Examples of aggregates as initial values:)
@end{Wide}
@begin{Example}
A : Table := (7, 9, 5, 1, 3, 2, 4, 8, 6, 0);        @RI[-- A(1)=7, A(10)=0]
B : Table := (2 | 4 | 10 => 1, @key(others) => 0);        @RI[-- B(1)=0, B(10)=1]
C : @key(constant) Matrix := (1 .. 5 => (1 .. 8 => 0.0)); @RI[-- C'Last(1)=5, C'Last(2)=8]

D : Bit_Vector(M .. N) := (M .. N => True);         @RI[-- see @RefSecNum{Array Types}]
E : Bit_Vector(M .. N) := (@key(others) => True);
F : String(1 .. 1) := (1 => 'F');  @RI[-- a one component aggregate: same as "F"]
@end{Example}
@end{Examples}

@begin{Incompatible83}
@ChgRef{Version=[1],Kind=[Added]}@ChgNote{Presentation AI-00016}
@ChgAdded{Version=[1],Type=[Leading],Text=[@Defn{incompatibilities with Ada 83}
In Ada 95, no applicable index constraint is defined for a parameter
in a call to a generic formal subprogram; thus, some aggregates that are
legal in Ada 83 are illegal in Ada 95. For example:]}
@begin{Example}
@ChgRef{Version=[1],Kind=[Added]}@ChgNote{Presentation AI-00016}
@Chg{New=[@key[subtype] S3 @key[is] String (1 .. 3);
...
@key[generic]
   @key[with function] F (The_S3 : @key[in] S3) @key[return] Integer;
@key[package] Gp @key[is]
   I : constant Integer := F ((1 => '!', others => '?'));
       -- @RI{The aggregate is legal in Ada 83, illegal in Ada 95.}
@key[end] Gp;],Old=[]}
@end{Example}
@ChgRef{Version=[1],Kind=[Added]}@ChgNote{Presentation AI-00016}
@Chg{New=[This change eliminates generic contract model problems.],Old=[]}
@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
We now allow "named with others" aggregates in all contexts
where there is an applicable index constraint, effectively
eliminating what was RM83-4.3.2(6). Sliding never occurs
on an aggregate with others, because its bounds come from
the applicable index constraint, and therefore already match
the bounds of the target.

The legality of an @key(others) choice is no longer affected
by the staticness of the applicable index constraint.
This substantially simplifies several rules, while being slightly
more flexible for the user. It obviates the rulings
of AI83-00244 and AI83-00310, while taking advantage of the dynamic nature
of the "extra values" check required by AI83-00309.

Named array aggregates are permitted even if the
index type is descended from a formal scalar type.
See @RefSecNum(Static Expressions and Static Subtypes) and AI83-00190.
@end{Extend83}

@begin{DiffWord83}
We now separate named and positional array aggregate syntax,
since, unlike other aggregates, named and positional
associations cannot be mixed in array aggregates (except
that an @key(others) choice is allowed in a positional array aggregate).

We have also reorganized the presentation to handle
multidimensional and one-dimensional aggregates more uniformly,
and to incorporate the rulings of AI83-00019, AI83-00309, etc.
@end{DiffWord83}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
@Chg{Version=[2],New=[@Defn{extensions to Ada 95}<> can be used in place of
an @nt{expression} in an @nt{array_aggregate}, default-initializing the
component.],Old=[]}
@end{Extend95}

@begin{DiffWord95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
@Chg{Version=[2],New=[Limited @nt{array_aggregate}s are allowed (since
all kinds of aggregates can now be limited, see @RefSecNum{Aggregates}).],Old=[]}
@end{DiffWord95}


@LabeledClause{Expressions}

@begin{Intro}
@Defn{expression}
An @i(expression) is a formula that defines the computation or retrieval
of a value.
In this International Standard, the term @lquotes@;expression@rquotes@; refers to a construct
of the syntactic category @nt<expression> or
of any of the other five syntactic categories defined below.
@Defn{and operator}@Defn2{Term=[operator],Sec=(and)}
@Defn{or operator}@Defn2{Term=[operator],Sec=(or)}
@Defn{xor operator}@Defn2{Term=[operator],Sec=(xor)}
@Defn{and then (short-circuit control form)}
@Defn{or else (short-circuit control form)}
@Defn{= operator}@Defn2{Term=[operator],Sec=(=)}
@Defn{equal operator}@Defn2{Term=[operator],Sec=(equal)}
@Defn{/= operator}@Defn2{Term=[operator],Sec=(/=)}
@Defn{not equal operator}@Defn2{Term=[operator],Sec=(not equal)}
@Defn{< operator}@Defn2{Term=[operator],Sec=(<)}
@Defn{less than operator}@Defn2{Term=[operator],Sec=(less than)}
@Defn{<= operator}@Defn2{Term=[operator],Sec=(<=)}
@Defn{less than or equal operator}@Defn2{Term=[operator],Sec=(less than or equal)}
@Defn{> operator}@Defn2{Term=[operator],Sec=(>)}
@Defn{greater than operator}@Defn2{Term=[operator],Sec=(greater than)}
@Defn{>= operator}@Defn2{Term=[operator],Sec=(>=)}
@Defn{greater than or equal operator}@Defn2{Term=[operator],Sec=(greater than or equal)}
@Defn{in (membership test)}
@Defn{not in (membership test)}
@Defn{+ operator}@Defn2{Term=[operator],Sec=(+)}
@Defn{plus operator}@Defn2{Term=[operator],Sec=(plus)}
@Defn{- operator}@Defn2{Term=[operator],Sec=(-)}
@Defn{minus operator}@Defn2{Term=[operator],Sec=(minus)}
@Defn{& operator}@Defn2{Term=[operator],Sec=(&)}
@Defn{ampersand operator}@Defn2{Term=[operator],Sec=(ampersand)}
@Defn{concatenation operator}@Defn2{Term=[operator],Sec=(concatenation)}
@IndexSee{Term=[catenation operator],See=(concatenation operator)}
@Defn{* operator}@Defn2{Term=[operator],Sec=(*)}
@Defn{multiply operator}@Defn2{Term=[operator],Sec=(multiply)}
@Defn{times operator}@Defn2{Term=[operator],Sec=(times)}
@Defn{/ operator}@Defn2{Term=[operator],Sec=(/)}
@Defn{divide operator}@Defn2{Term=[operator],Sec=(divide)}
@Defn{mod operator}@Defn2{Term=[operator],Sec=(mod)}
@Defn{rem operator}@Defn2{Term=[operator],Sec=(rem)}
@Defn{** operator}@Defn2{Term=[operator],Sec=(**)}
@Defn{exponentiation operator}@Defn2{Term=[operator],Sec=(exponentiation)}
@Defn{abs operator}@Defn2{Term=[operator],Sec=(abs)}
@Defn{absolute value}
@Defn{not operator}@Defn2{Term=[operator],Sec=(not)}
@end{Intro}

@begin{Syntax}
@Syn{tabs=[P23], lhs=<expression>,rhs="
     @Syn2{relation} {@key{and} @Syn2{relation}} @\| @Syn2{relation} {@key{and} @key{then} @Syn2{relation}}
   | @Syn2{relation} {@key{or} @Syn2{relation}} @\| @Syn2{relation} {@key{or} @key{else} @Syn2{relation}}
   | @Syn2{relation} {@key{xor} @Syn2{relation}}"}


@Syn{lhs=<relation>,rhs="
     @Syn2{simple_expression} [@Syn2{relational_operator} @Syn2{simple_expression}]
   | @Syn2{simple_expression} [@key{not}] @key{in} @Syn2{range}
   | @Syn2{simple_expression} [@key{not}] @key{in} @Syn2{subtype_mark}"}


@Syn{lhs=<simple_expression>,rhs="[@Syn2{unary_adding_operator}] @Syn2{term} {@Syn2{binary_adding_operator} @Syn2{term}}"}


@Syn{lhs=<term>,rhs="@Syn2{factor} {@Syn2{multiplying_operator} @Syn2{factor}}"}


@Syn{lhs=<factor>,rhs="@Syn2{primary} [** @Syn2{primary}] | @key{abs} @Syn2{primary} | @key{not} @Syn2{primary}"}

@Syn{lhs=<primary>,rhs="
   @Syn2{numeric_literal} | @key{null} | @Syn2{string_literal} | @Syn2{aggregate}
 | @Syn2{name} | @Syn2{qualified_expression} | @Syn2{allocator} | (@Syn2{expression})"}
@end{Syntax}

@begin{Resolution}
A @nt<name> used as a @nt<primary> shall resolve to denote an object
or a value.
@begin{Discussion}
This replaces RM83-4.4(3). We don't need to mention named numbers
explicitly, because the name of a named number denotes a value.
We don't need to mention attributes explicitly, because
attributes now denote (rather than yield) values in general.
Also, the new wording allows attributes that denote objects,
which should always have been allowed (in case the implementation chose to
have such a thing).
@end{Discussion}
@begin{Reason}
  It might seem odd that this is an overload resolution rule,
  but it is relevant during overload resolution. For example,
  it helps ensure that a @nt<primary> that consists of only
  the identifier of a parameterless function is interpreted as a
  @nt<function_call> rather than directly as a @nt<direct_name>.
@end{Reason}
@end{Resolution}

@begin{StaticSem}
Each expression has a type; it specifies the
computation or retrieval of a value of that type.
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[evaluation], Sec=(primary that is a name)}
The value of a @nt<primary> that is a @nt{name} denoting an object
is the value of the object.
@end{RunTime}

@begin{ImplPerm}
@IndexCheck{Overflow_Check}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
For the evaluation of
a @nt<primary> that is a @nt<name> denoting an object of an
unconstrained numeric subtype,
if the value of the object is outside the base range of its type,
the implementation may either raise Constraint_Error
or return the value of the object.
@begin{Ramification}
  This means that if extra-range intermediates are used to
  hold the value of an object of an unconstrained numeric subtype,
  a Constraint_Error can be raised on a read of the object, rather than
  only on an assignment to it. Similarly, it means that
  computing the value of an object of such a subtype
  can be deferred until the first read of the object
  (presuming no side-effects other than failing an Overflow_Check
  are possible). This permission is over and above that provided
  by clause @RefSecNum(Exceptions and Optimization), since
  this allows the Constraint_Error to move to a different handler.
@end{Ramification}
@begin{Reason}
  This permission is intended to allow extra-range registers
  to be used efficiently to hold parameters and local variables,
  even if they might need to be transferred into smaller registers
  for performing certain predefined operations.
@end{Reason}
@begin{Discussion}
  There is no need to mention other kinds of @nt<primary>s, since any
  Constraint_Error to be raised can be @lquotes@;charged@rquotes@; to the evaluation
  of the particular kind of @nt<primary>.
@end{Discussion}
@end{ImplPerm}

@begin{Examples}
@Leading@keepnext@i(Examples of primaries:)
@begin{Example}
@Trailing@;4.0                @RI[--  real literal]
Pi                 @RI[--  named number]
(1 .. 10 => 0)     @RI[--  array aggregate]
Sum                @RI[--  variable]
Integer'Last       @RI[--  attribute]
Sine(X)            @RI[--  function call]
Color'(Blue)       @RI[--  qualified expression]
Real(M*N)          @RI[--  conversion]
(Line_Count + 10)  @RI[--  parenthesized expression ]
@end{Example}

@leading@keepnext@i(Examples of expressions:)
@begin{Example}
Volume                      @RI[-- primary]
@key(not) Destroyed               @RI[-- factor]
2*Line_Count                @RI[-- term  ]
-4.0                        @RI[-- simple expression]
-4.0 + A                    @RI[-- simple expression]
B**2 - 4.0*A*C              @RI[-- simple expression]
Password(1 .. 3) = "Bwv"    @RI[-- relation]
Count @key(in) Small_Int          @RI[-- relation]
Count @key(not) @key(in) Small_Int      @RI[-- relation]
Index = 0 @key(or) Item_Hit       @RI[-- expression]
(Cold @key(and) Sunny) @key(or) Warm    @RI[-- expression (parentheses are required)]
A**(B**C)                   @RI[-- expression (parentheses are required)]
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
In Ada 83, @key{out} parameters and their nondiscriminant
subcomponents are not allowed as @nt{primaries}.
These restrictions are eliminated in Ada 95.

In various contexts throughout the language where Ada 83 syntax rules
had @nt<simple_expression>, the corresponding Ada 95 syntax
rule has @nt<expression> instead. This reflects the inclusion
of modular integer types, which makes the logical operators
"@key[and]", "@key[or]", and "@key[xor]" more useful in expressions of
an integer type.
Requiring parentheses to use these operators in such contexts
seemed unnecessary and potentially confusing.
Note that the bounds of a @nt<range> still have to be
specified by @nt<simple_expression>s, since otherwise @nt<expression>s
involving membership tests might be ambiguous.
Essentially, the operation ".." is of higher precedence than the
logical operators, and hence uses of logical operators
still have to be parenthesized when used in a bound of a range.
@end{Extend83}


@LabeledClause{Operators and Expression Evaluation}

@begin{Intro}
@Redundant[@Defn{precedence of operators}
@Defn{operator precedence}
The language defines the following six categories
of operators (given in order of increasing
precedence). The corresponding @nt<operator_symbol>s,
and only those, can be used as @nt<designator>s in declarations
of functions for user-defined operators. See @RefSec(Overloading of Operators).]
@end{Intro}

@begin{Syntax}
@Syn{tabs=[P36], lhs=<logical_operator>,
    rhs="@\ @key{and} | @key{or}  | @key{xor}"}
@Syn{tabs=[P36], lhs=<relational_operator>,rhs="@\ =   | /=  | <   | <= | > | >="}
@Syn{tabs=[P36], lhs=<binary_adding_operator>,rhs="@\ +   | @en   | &"}
@Syn{tabs=[P36], lhs=<unary_adding_operator>,rhs="@\ +   | @en"}
@Syn{tabs=[P36], lhs=<multiplying_operator>,rhs="@\ *   | /   | @key{mod} | @key{rem}"}
@Syn{tabs=[P36], lhs=<highest_precedence_operator>,rhs="@\ **  | @key{abs} | @key{not}"}
@begin(Discussion)
  Some of the above syntactic categories are not used in other
  syntax rules. They are just used for classification.
  The others are used for both classification and parsing.
@end(Discussion)
@end{Syntax}

@begin{StaticSem}
For a sequence of operators of the same precedence level, the
operators are associated with their operands
in textual order from left to right.
Parentheses can be used to impose specific associations.
@begin{Discussion}
  The left-associativity is not directly inherent in the grammar of
  @RefSecNum{Expressions},
  though in @RefSecNum{Method of Description and Syntax Notation}
  the definition of the metasymbols @b({}) implies left
  associativity. So this could be seen as redundant, depending on
  how literally one interprets the definition of the @b({}) metasymbols.

  See the Implementation Permissions below regarding flexibility
  in reassociating operators of the same precedence.
@end{Discussion}

@Defn{predefined operator}@Defn2{Term=[operator],Sec=(predefined)}
For each form of type definition, certain of the above operators are
@i(predefined);
that is, they are implicitly declared immediately after the type definition.
@Defn{binary operator}@Defn2{Term=[operator],Sec=(binary)}
@Defn{unary operator}@Defn2{Term=[operator],Sec=(unary)}
For each such implicit operator declaration, the
parameters are called Left and Right for @i(binary) operators;
the single parameter is called Right for @i(unary) operators.
@redundant[An expression of the form X op Y,
where op is a binary operator,
is equivalent to a @nt<function_call> of the form "op"(X, Y).
An expression of the form op Y,
where op is a unary operator,
is equivalent to a @nt<function_call> of the form "op"(Y).
The predefined operators and their effects are described
in subclauses @RefSecNum(Logical Operators and Short-Circuit Control Forms)
through @RefSecNum(Highest Precedence Operators).]
@end{StaticSem}

@begin{RunTime}
@redundant[@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
The predefined operations on integer types either yield the mathematically
correct result or raise the exception Constraint_Error.
For implementations that support the Numerics Annex,
the predefined operations on real types yield results whose
accuracy is defined in @RefSecNum(Numerics), or
raise the exception Constraint_Error.
]
@begin{Honest}
  Predefined operations on real types can @lquotes@;silently@rquotes@; give wrong results
  when the Machine_Overflows attribute is false, and the
  computation overflows.
@end{Honest}

@end{RunTime}

@begin{ImplReq}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
The implementation of a predefined operator
that delivers a result of an integer or fixed point type may
raise Constraint_Error only if the result is outside
the base range of the result type.

@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
The implementation of a predefined operator
that delivers a result of a floating point type may raise Constraint_Error
only if the result is outside the safe range of the
result type.
@begin{Honest}

An exception is made for exponentiation by a negative exponent in
@RefSecNum{Highest Precedence Operators}.

@end{Honest}
@end{ImplReq}

@begin{ImplPerm}
For a sequence of predefined operators of the same precedence
level (and in the absence of parentheses imposing a specific association),
an implementation may impose any association of the operators with
operands so long as the result produced is
an allowed result for the left-to-right association, but ignoring
the potential for failure of language-defined checks in either the
left-to-right or chosen order of association.
@begin{Discussion}
  Note that the permission to reassociate the operands in
  any way subject to producing a result allowed for
  the left-to-right association is not much help
  for most floating point operators, since reassociation may
  introduce significantly different round-off errors, delivering
  a result that is outside the model interval for the left-to-right
  association. Similar problems arise for division with
  integer or fixed point operands.

  Note that this permission does not apply to user-defined
  operators.
@end{Discussion}
@end{ImplPerm}

@begin{Notes}
The two operands of an expression of the form X op Y, where
op is a binary operator, are evaluated in an arbitrary order,
as for any @nt<function_call> (see @RefSecNum(Subprogram Calls)).
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of precedence:)
@begin{Example}
@key(not) Sunny @key(or) Warm    @RI[--  same as (not Sunny) or Warm]
X > 4.0 @key(and) Y > 0.0  @RI[--  same as (X > 4.0) and (Y > 0.0)]

-4.0*A**2            @RI[--  same as @en@;(4.0 * (A**2))]
@key(abs)(1 + A) + B       @RI[--  same as (abs (1 + A)) + B]
Y**(-3)              @RI[--  parentheses are necessary]
A / B * C            @RI[--  same as (A/B)*C]
A + (B + C)          @RI[--  evaluate B + C before adding it to A ]
@end{Example}
@end{Examples}

@begin{DiffWord83}
We don't give a detailed definition of precedence, since
it is all implicit in the syntax rules anyway.

The permission to reassociate is moved here from RM83-11.6(5), so
it is closer to the rules defining operator association.
@end{DiffWord83}


@LabeledSubClause{Logical Operators and Short-circuit Control Forms}

@begin{Resolution}
@Defn{short-circuit control form}
@Defn{and then (short-circuit control form)}
@Defn{or else (short-circuit control form)}
An @nt<expression> consisting of two @nt<relation>s
connected by @key(and then) or @key(or else)
(a @i(short-circuit control form))
shall resolve to be of some boolean type;
@PDefn2{Term=[expected type],Sec=(short-circuit control form relation)}
the expected type for both @nt<relation>s
is that same boolean type.
@begin(Reason)
  This rule is written this way so that overload resolution treats
  the two operands symmetrically; the resolution of overloading
  present in either one can benefit from the resolution of the other.
  Furthermore, the type expected by context can help.
@end(Reason)
@end{Resolution}

@begin{StaticSem}
@Leading@Defn{logical operator}@Defn2{Term=[operator],Sec=(logical)}
@Defn{and operator}@Defn2{Term=[operator],Sec=(and)}
@Defn{or operator}@Defn2{Term=[operator],Sec=(or)}
@Defn{xor operator}@Defn2{Term=[operator],Sec=(xor)}
The following logical operators are predefined for every
boolean type @i(T),
for every modular type @i(T), and
for every one-dimensional array type @i(T) whose
component type is a boolean type:
@IndexSee{Term=[bit string],See=(logical operators on boolean arrays)}
@begin{example}
@tabclear()
@key[function] "@key(and)"(Left, Right : @RI(T)) @key[return] @RI(T)
@key[function] "@key(or)" (Left, Right : @RI(T)) @key[return] @RI(T)
@key[function] "@key(xor)"(Left, Right : @RI(T)) @key[return] @RI(T)
@end{example}
@begin{Honest}
  For predefined operators, the parameter and result subtypes
  shown as @i(T) are actually the unconstrained subtype of the type.
@end{Honest}

For boolean types, the predefined logical operators
@key{and}, @key{or}, and @key{xor}
perform the conventional operations of conjunction, inclusive
disjunction, and exclusive disjunction, respectively.

For modular types, the predefined logical operators
are defined on a bit-by-bit basis, using the binary
representation of the value of the operands to
yield a binary representation for the result,
where zero represents False and one represents True.
If this result is outside the base range of the type,
a final subtraction by the modulus is performed to bring the
result into the base range of the type.

The logical operators on arrays are performed on a
component-by-component basis on
matching components (as for equality @em
see @RefSecNum{Relational Operators and Membership Tests}),
using the predefined logical operator for the component type. The bounds of
the resulting array are those of the left operand.

@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[evaluation], Sec=(short-circuit control form)}
The short-circuit control forms @key{and then} and @key{or else}
deliver the same result as the corresponding predefined @key{and} and @key{or}
operators for boolean types, except that the left operand is always
evaluated first, and the right operand is not evaluated if the
value of the left operand determines the result.

@IndexCheck{Length_Check}
For the logical operators on arrays,
a check is made that
for each component of the left operand there is a matching component of the
right operand, and vice versa.
@IndexCheck{Range_Check}
Also, a check is made that each component
of the result belongs to the component subtype.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
The exception Constraint_Error is raised if
either of the above checks fails.
@begin{Discussion}
  The check against the component subtype is per AI83-00535.
@end{Discussion}

@end{RunTime}

@begin{Notes}
@Leading@;The conventional meaning of the logical operators is given by the
following truth table:
@begin(Display)
@TabClear()
@TabSet(P4, P20, P36, P52, P68)
@\@ @ A@\@ @ B@\(A @key(and) B)@\(A @key(or) B)@\(A @key(xor) B)@*
@\True  @\True  @\True  @\True  @\False
@\True  @\False @\False @\True  @\True
@\False @\True  @\False @\True  @\True
@\False @\False @\False @\False @\False
@end(Display)
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of logical operators:)
@begin{Example}
Sunny @key(or) Warm
Filter(1 .. 10) @key(and) Filter(15 .. 24)   @RI[--   see @RefSecNum{Index Constraints and Discrete Ranges} ]
@end{Example}

@begin{Wide}
@leading@keepnext@i(Examples of short-circuit control forms:)
@end{Wide}
@begin{Example}
Next_Car.Owner /= @key(null) @key(and) @key(then) Next_Car.Owner.Age > 25   @RI[--   see @RefSecNum{Incomplete Type Declarations}]
N = 0 @key(or) @key(else) A(N) = Hit_Value
@end{Example}
@end{Examples}


@LabeledSubClause{Relational Operators and Membership Tests}

@begin{Intro}
@redundant[@Defn{relational operator}@Defn2{Term=[operator],Sec=(relational)}
@IndexSee{Term=[comparison operator],See=(relational operator)}
@Defn{equality operator}@Defn2{Term=[operator],Sec=(equality)}
The @i(equality operators)
= (equals) and /= (not equals) are predefined for nonlimited types.
@Defn{ordering operator}@Defn2{Term=[operator],Sec=(ordering)}
The other @nt<relational_operator>s are the @i(ordering operators)
< (less than), <= (less than or
equal), > (greater than), and >= (greater than or equal).
@Defn{= operator}@Defn2{Term=[operator],Sec=(=)}
@Defn{equal operator}@Defn2{Term=[operator],Sec=(equal)}
@Defn{/= operator}@Defn2{Term=[operator],Sec=(/=)}
@Defn{not equal operator}@Defn2{Term=[operator],Sec=(not equal)}
@Defn{< operator}@Defn2{Term=[operator],Sec=(<)}
@Defn{less than operator}@Defn2{Term=[operator],Sec=(less than)}
@Defn{<= operator}@Defn2{Term=[operator],Sec=(<=)}
@Defn{less than or equal operator}@Defn2{Term=[operator],Sec=(less than or equal)}
@Defn{> operator}@Defn2{Term=[operator],Sec=(>)}
@Defn{greater than operator}@Defn2{Term=[operator],Sec=(greater than)}
@Defn{>= operator}@Defn2{Term=[operator],Sec=(>=)}
@Defn{greater than or equal operator}@Defn2{Term=[operator],Sec=(greater than or equal)}
@Defn{discrete array type}
The ordering operators are predefined for scalar
types, and for @i(discrete array types), that is,
one-dimensional array types whose components are of
a discrete type.
@begin{Ramification}
The equality operators are not defined for @i{every} nonlimited
type @em see below for the exact rule.
@end{Ramification}

@Defn{membership test}
@Defn{in (membership test)}
@Defn{not in (membership test)}
A @i(membership test), using @key(in) or @key(not in),
determines whether or not a value
belongs to a given subtype or range, or has a tag that identifies
a type that is covered by a given type.
Membership tests are allowed for all types.]

@end{Intro}

@begin{Resolution}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01]}
@PDefn2{Term=[expected type],
  Sec=(membership test simple_expression)}
@Defn2{Term=[tested type], Sec=(of a membership test)}
The @i(tested type) of a membership test
is the type of the @nt<range> or the type
determined by the @nt<subtype_mark>.
If the tested type is tagged, then the @nt<simple_expression> shall resolve to
be of a type that @Chg{Version=[2],New=[is convertible (see
@RefSecNum{Type Conversions}) to],Old=[covers or is covered by]} the tested
type; if untagged, the expected type for the @nt<simple_expression> is
the tested type.

@begin{Reason}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
  The part of the rule for untagged types is stated in a way
  that ensures that operands
  like @Chg{Version=[2],New=[a string literal],Old=[@key(null)]} are still
  legal as operands of a membership test.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01]}
  The significance of @lquotes@;@Chg{Version=[2],New=[is convertible to],
  Old=[covers or is covered by]}@rquotes@; is that we allow the
  @nt<simple_expression> to be of any class-wide type that @Chg{Version=[2],
  New=[could be converted to],Old=[covers]} the tested type, not just the
  one rooted at the tested type.@Chg{Version=[2],New=[ This includes any
  class-wide type that covers the tested type, along with class-wide interfaces
  in some cases.],Old=[]}
@end{Reason}

@end{Resolution}

@begin{Legality}
For a membership test,
if the @nt<simple_expression> is of a tagged class-wide type,
then the tested type shall be (visibly) tagged.
@begin{Ramification}
Untagged types covered by the tagged class-wide type
  are not permitted. Such types can exist if they are
  descendants of a private type whose full type is tagged.
  This rule is intended to avoid confusion since such derivatives
  don't have their @lquotes@;own@rquotes@; tag, and hence are indistinguishable
  from one another at run time once converted to a covering
  class-wide type.
@end{Ramification}
@end{Legality}

@begin{StaticSem}
The result type of a membership test is the predefined type Boolean.

@Leading@;The equality operators are predefined for every specific
type @i(T) that is not limited,
and not an anonymous access type,
with the following specifications:
@begin(example)
@key(function) "=" (Left, Right : @RI(T)) @key(return) Boolean
@key(function) "/="(Left, Right : @RI(T)) @key(return) Boolean
@end(example)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00230-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The following additional equality
operators for the
@i<universal_access> type are declared in package Standard for use with
anonymous access types:]}
@begin(example)
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[@key<function> "=" (Left, Right : @i<universal_access>) @key<return> Boolean
@key<function> "/="(Left, Right : @i<universal_access>) @key<return> Boolean]}
@end(example)

@Leading@;The ordering operators are predefined for every specific
scalar type @i(T), and for every discrete array type
@i(T), with the following specifications:
@begin(example)
@key(function) "<" (Left, Right : @RI(T)) @key(return) Boolean
@key(function) "<="(Left, Right : @RI(T)) @key(return) Boolean
@key(function) ">" (Left, Right : @RI(T)) @key(return) Boolean
@key(function) ">="(Left, Right : @RI(T)) @key(return) Boolean
@end(example)
@end{StaticSem}

@begin{Resolution}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00230-01]}
@ChgAdded{Version=[2],Text=[At least one of the operands of the equality
operators for @i<universal_access> shall be of a specific anonymous access type.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This prevents compatibility problems by insuring that
these operators are not used for named access types. Also, universal access
types do not count for the purposes of this rule. Otherwise, equality
expressions like (X = @key{null}) would be ambiguous for normal access types.]}
@end{Reason}
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00230-01]}
@ChgAdded{Version=[2],Text=[The operands of the equality operators for
@i<universal_access> shall be convertible to one another (see
@RefSecNum{Type Conversions}).]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This ensures that the designated type is the same,
or one of the operands is @key{null}.]}
@end{Reason}
@end{Legality}


@begin{RunTime}
For discrete types, the predefined relational operators are
defined in terms of corresponding mathematical operations on
the position numbers of the values of the operands.

For real types, the predefined relational operators are
defined in terms of the corresponding mathematical operations
on the values of the operands, subject to the accuracy of the
type.
@begin{Ramification}
  For floating point types, the results of comparing
  @i(nearly) equal values depends on the accuracy of
  the implementation
  (see @RefSec{Model of Floating Point Arithmetic}
  for implementations that support the Numerics Annex).
@end{Ramification}
@begin{ImplNote}
  On a machine with signed zeros,
  if the generated code generates both plus zero and minus zero,
  plus and minus zero must be equal
  by the predefined equality operators.
@end{ImplNote}

Two access-to-object values are equal if they designate the same
object, or if both are equal to the null value of the access type.

Two access-to-subprogram values are equal if they are the
result of the same evaluation of an Access @nt<attribute_reference>,
or if both
are equal to the null value of the access type. Two
access-to-subprogram values are unequal if they designate
different subprograms.
@PDefn{unspecified}
@Redundant[It is unspecified whether
two access values that designate the same subprogram but are
the result of distinct evaluations of
Access @nt<attribute_reference>s are equal
or unequal.]
@begin{Reason}
This allows each Access @nt<attribute_reference>
  for a subprogram to designate a distinct @lquotes@;wrapper@rquotes@; subprogram
  if necessary to support an indirect call.
@end{Reason}

@Defn2{Term=[equality operator],Sec=(special inheritance rule for tagged types)}
For a type extension, predefined equality
is defined in terms of the primitive @Redundant[(possibly
user-defined)] equals operator
of the parent type and of any tagged components of the
extension part, and predefined equality
for any other components not inherited from the parent type.
@begin{Ramification}
  Two values of a type extension are not equal if there is
  a @nt<variant_part> in the extension part and the two
  values have different @nt<variant>s present.
  This is a ramification of the requirement that a
  discriminant governing such a @nt<variant_part> has to be a @lquotes@;new@rquotes@;
  discriminant, and so has to be equal in the two values for
  the values to be equal. Note that @nt<variant_part>s in
  the parent part need not match if the primitive equals operator
  for the parent type considers them equal.

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00349-01]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[The full type extension's operation
  is used for a private extension. This follows as only full types have parent types;
  the type specified in a private extension is an ancestor, but not necessarily
  the parent type. For instance, in:]}
  @begin(Example)
@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[@key{with} Pak1;
@key{package} Pak2 @key{is}
   @key{type} Typ3 @key{is} @key{new} Pak1.Typ1 @key{with} @key{private};
@key{private}
   @key{type} Typ3 @key{is} @key{new} Pak1.Typ2 @key{with} @key{null} @key{record};
@key{end} Pak2;],Old=[]}
  @end(Example)
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @Chg{Version=[2],New=[the parent type is Pak1.Typ2, not Pak1.Typ1, and the
  equality operator of Pak1.Typ2 is used to create predefined equality for
  Typ3.],Old=[]}
@end{Ramification}

For a private type, if its full type is tagged, predefined
equality is defined in terms of the primitive equals operator of the
full type; if the full type is untagged, predefined equality
for the private type is that of its full type.

@Leading@Defn{matching components}
For other composite types, the predefined equality operators
@Redundant[(and
certain other predefined operations on composite types @em
see @RefSecNum(Logical Operators and Short-circuit Control Forms)
and @RefSecNum(Type Conversions))] are defined
in terms of the corresponding operation on
@i(matching components), defined as follows:
@begin(itemize)
  For two composite objects or values of the same non-array type,
  matching components are those that correspond to the
  same @nt<component_declaration> or @nt<discriminant_specification>;

  For two one-dimensional arrays of the same type, matching components are
  those (if any) whose index values match in the following sense: the
  lower bounds of the index ranges are defined to match, and the successors
  of matching indices are defined to match;

  For two multidimensional arrays of the same type, matching components
  are those whose index values match in successive index positions.
@end(itemize)

The analogous definitions apply if the types of the two objects or values
are convertible, rather than being the same.
@begin{Discussion}
  Ada 83 seems to
  omit this part of the definition, though it is used in array type
  conversions. See @RefSecNum{Type Conversions}.
@end{Discussion}

@Leading@;Given the above definition of matching components,
the result of the predefined equals operator for composite types (other than
for those composite types covered earlier) is defined as follows:
@begin(Itemize)
  If there are no components, the result is defined to be True;

  If there are unmatched components, the result is defined to be False;

  Otherwise, the result is defined in terms of
  the primitive equals operator for any
  matching tagged components, and the predefined equals for any
  matching untagged components.
  @begin{Reason}
    This asymmetry between tagged and untagged components is
    necessary to preserve upward compatibility and corresponds
    with the corresponding situation with generics, where the
    predefined operations @lquotes@;reemerge@rquotes@; in a generic for
    untagged types, but do not for tagged types. Also, only
    tagged types support user-defined assignment
    (see @RefSecNum{User-Defined Assignment and Finalization}),
    so only tagged types
    can fully handle levels of indirection in the implementation
    of the type. For untagged types, one reason for
    a user-defined equals operator might be to allow values with different
    bounds or discriminants to compare equal in certain cases.
    When such values are matching components, the bounds or discriminants
    will necessarily match anyway if the
    discriminants of the enclosing values match.
  @end{Reason}
@end(Itemize)
@begin{Ramification}
  Two null arrays of the same type are always equal;
  two null records of the same type are always equal.

  Note that if a composite object has a component
  of a floating point type, and the floating point type
  has both a plus and minus zero, which are considered
  equal by the predefined equality, then a block compare
  cannot be used for the predefined composite equality.
  Of course, with user-defined equals operators for tagged components,
  a block compare breaks down anyway, so this is not the only
  special case that requires component-by-component comparisons.
  On a one's complement machine, a similar situation might
  occur for integer types, since one's complement machines
  typically have both a plus and minus (integer) zero.
@end{Ramification}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0016],ARef=[AI95-00123-01]}
@Chg{New=[For any composite type, the order in which "=" is called for
components is unspecified. Furthermore, if the result can be determined
before calling "=" on some components, it is unspecified whether "=" is called
on those components.@PDefn{Unspecified}],Old=[]}

The predefined "/=" operator gives the complementary result
to the predefined "=" operator.
@begin{Ramification}
Furthermore,
  if the user defines an "=" operator that returns Boolean,
  then a "/=" operator is implicitly declared in terms of
  the user-defined "=" operator so as to give the complementary
  result. See @RefSecNum(Overloading of Operators).
@end{Ramification}

@Defn{lexicographic order}
For a discrete array type, the predefined ordering operators
correspond to @i(lexicographic order) using the predefined order
relation of the component type: A null array is lexicographically
less than any array having at least one component.
In the case of nonnull arrays, the left operand is lexicographically
less than the right operand if the first component of
the left operand is less than that of the right; otherwise
the left operand is lexicographically less than the right operand
only if their first components are equal and the tail of the
left operand is lexicographically less than that of the right (the
@i(tail) consists of the remaining components beyond the first and
can be null).

@PDefn2{Term=[evaluation], Sec=(membership test)}
For the evaluation of a membership test,
the @nt<simple_expression> and the @nt<range> (if any) are evaluated
in an arbitrary order.

@Leading@;A membership test using
@key(in) yields the result True if:
@begin(itemize)
  The tested type is scalar, and the value of
  the @nt<simple_expression> belongs to the given @nt<range>, or
  the range of the named subtype; or
@begin{Ramification}
    The scalar membership test only does a range check.
    It does not perform any other check, such as whether
    a value falls in a @lquotes@;hole@rquotes@; of a @lquotes@;holey@rquotes@; enumeration type.
    The Pos attribute function can be used for that purpose.

    Even though Standard.Float is an unconstrained subtype,
    the test @lquotes@;X in Float@rquotes@; will still return False
    (presuming the evaluation of X does not raise Constraint_Error)
    when X is outside Float'Range.
@end{Ramification}

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00231-01]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[]}@ChgNote{To get conditional Leading}.
  The tested type is not scalar, and
  the value of the @nt<simple_expression> satisfies any constraints
  of the named subtype, and@Chg{Version=[2],New=[:],Old=[, if the type of
  the @nt{simple_expression}
  is class-wide, the value has a tag that identifies a type covered by
  the tested type.]}
  @begin{Inneritemize}
    @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00231-01]}
    @ChgAdded{Version=[2],Text=[if the type of the @nt{simple_expression} is
    class-wide, the value has a tag that identifies a type covered by the
    tested type;]}
    @begin{Ramification}
      Note that the tag is not checked if the @nt{simple_expression} is of a
      specific type.
    @end{Ramification}
    @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00231-01]}
    @ChgAdded{Version=[2],Text=[if the tested type is an access type and the
    named subtype excludes null, the value of the @nt{simple_expression} is
    not null.]}
  @end{Inneritemize}
@end(itemize)

Otherwise the test yields the result False.

A membership test using @key(not in) gives the complementary result to
the corresponding membership test using @key(in).
@end{RunTime}

@begin{ImplReq}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0016],ARef=[AI95-00123-01]}
@ChgAdded{Version=[1],Text=[For all nonlimited types declared in
language-defined packages, the "=" and "/=" operators of the type shall behave
as if they were the predefined equality operators for the purposes of the
equality of composite types and generic formal types.]}
@begin{Ramification}
@ChgRef{Version=[1],Kind=[Added]}
@ChgAdded{Version=[1],Text=[If any language-defined types are implemented with
a user-defined "=" operator, then either the full type must be tagged, or
the compiler must
use @lquotes@;magic@rquotes@; to implement equality for this type. A normal
user-defined "=" operator for an untagged type does @i{not} meet this
requirement.]}
@end{Ramification}
@end{ImplReq}

@begin{Notes}
No exception is ever raised by a membership test, by a predefined
ordering operator,
or by a predefined equality operator for an elementary type,
but an exception can be raised by the evaluation of
the operands. A predefined equality operator for a composite
type can only raise an exception if the type has a tagged part
whose primitive equals operator propagates an exception.

If a composite type has components that depend on discriminants, two values
of this type have matching components if and only if their
discriminants are equal. Two nonnull arrays have matching components
if and only if the length of each dimension is the same for both.
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of expressions involving relational operators and
membership tests:)
@begin{Example}
X /= Y

"" < "A" @key(and) "A" < "Aa"     @RI[--  True]
"Aa" < "B" @key(and) "A" < "A  "  @RI[--  True]

My_Car = @key(null)               @RI[-- true if My_Car has been set to null (see @RefSecNum{Incomplete Type Declarations})]
My_Car = Your_Car           @RI[-- true if we both share the same car]
My_Car.@key[all] = Your_Car.@key[all]   @RI[-- true if the two cars are identical]

N @key(not) @key(in) 1 .. 10            @RI[-- range membership test]
Today @key(in) Mon .. Fri         @RI[-- range membership test]
Today @key(in) Weekday            @RI[-- subtype membership test (see @RefSecNum{Enumeration Types})]
Archive @key(in) Disk_Unit        @RI[-- subtype membership test (see @RefSecNum{Variant Parts and Discrete Choices})]
Tree.@key(all) @key(in) Addition'Class  @RI[-- class membership test (see @RefSecNum{Type Extensions})]
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
Membership tests can be used to test the tag of a class-wide value.

Predefined equality for a composite type
is defined in terms of the primitive equals operator
for tagged components or the parent part.
@end{Extend83}

@begin{DiffWord83}
The term @lquotes@;membership test@rquotes@; refers to the @nt<relation> "X in S" rather
to simply the reserved word @key(in) or @key(not in).

We use the term @lquotes@;equality operator@rquotes@; to refer to both
the = (equals) and /= (not equals) operators.
Ada 83 referred to = as @i(the) equality operator, and
/= as the inequality operator. The new wording is more
consistent with the ISO 10646 name for "=" (equals sign) and provides a
category similar to @lquotes@;ordering operator@rquotes@; to refer to both
= and /=.

We have changed the term @lquotes@;catenate@rquotes@; to @lquotes@;concatenate@rquotes@;.
@end{DiffWord83}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01]}
@Chg{Version=[2],New=[@Defn{extensions to Ada 95}The @i{universal_access}
equality operators are new. They provide equality operations (most importantly,
testing against @key{null}) for anonymous access types.],Old=[]}
@end{Extend95}

@begin{DiffWord95}
@ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0016],ARef=[AI95-00123-01]}
@Chg{Version=[2],New=[@b<Corrigendum:> Wording was added to clarify that
the order of calls (and whether the calls are made at all) on "=" for
components is unspecified. Also clarified that "=" must compose properly for
language-defined types.],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
@Chg{Version=[2],New=[Memberships were adjusted to allow interfaces which don't
cover the tested type, in order to be consistent with type
conversions.],Old=[]}
@end{DiffWord95}


@LabeledSubClause{Binary Adding Operators}

@begin{StaticSem}
@Leading@Defn{binary adding operator}@Defn2{Term=[operator],Sec=(binary adding)}
@Defn{+ operator}@Defn2{Term=[operator],Sec=(+)}
@Defn{plus operator}@Defn2{Term=[operator],Sec=(plus)}
@Defn{- operator}@Defn2{Term=[operator],Sec=(-)}
@Defn{minus operator}@Defn2{Term=[operator],Sec=(minus)}
The binary adding operators + (addition) and @en (subtraction) are predefined
for every specific numeric type @i(T) with their
conventional meaning.
They have the following specifications:
@begin(example)
@key(function) "+"(Left, Right : @RI(T)) @key(return) @RI(T)
@key(function) "-"(Left, Right : @RI(T)) @key(return) @RI(T)
@end(example)

@Leading@Defn{& operator}@Defn2{Term=[operator],Sec=(&)}
@Defn{ampersand operator}@Defn2{Term=[operator],Sec=(ampersand)}
@Defn{concatenation operator}@Defn2{Term=[operator],Sec=(concatenation)}
@IndexSee{Term=[catenation operator],See=(concatenation operator)}
The concatenation operators & are predefined for
every nonlimited,
one-dimensional array type @i(T) with component type @i(C).
They have the following specifications:
@begin(example)
@key(function) "&"(Left : @RI(T); Right : @RI(T)) @key(return) @RI(T)
@key(function) "&"(Left : @RI(T); Right : @RI(C)) @key(return) @RI(T)
@key(function) "&"(Left : @RI(C); Right : @RI(T)) @key(return) @RI(T)
@key(function) "&"(Left : @RI(C); Right : @RI(C)) @key(return) @RI(T)
@end(example)
@end{StaticSem}

@begin{RunTime}

@Leading@PDefn2{Term=[evaluation], Sec=(concatenation)}
For the evaluation of a concatenation with result type @i(T),
if both operands are of type @i(T), the result of the concatenation
is a one-dimensional array whose length is the sum of the lengths
of its operands, and whose components comprise the components of
the left operand followed by the components of the right operand.
If the left operand is a null array, the result of the
concatenation is the right operand.
Otherwise, the lower bound of the result is determined as
follows:
@begin(Itemize)
  If the ultimate ancestor of the array type was defined
  by a @nt<constrained_array_definition>, then
  the lower bound of the result is that of the index subtype;
  @begin(Reason)
    This rule avoids Constraint_Error when using concatenation
    on an array type whose first subtype is constrained.
  @end(Reason)

  If the ultimate ancestor of the array type was defined
  by an @nt<unconstrained_array_definition>, then
  the lower bound of the result is that of the left operand.
@end(Itemize)

@Redundant[The upper bound is determined by the lower bound and the length.]
@IndexCheck{Index_Check}
A check is made that the upper bound of the result of the
concatenation belongs to the range of the index subtype, unless the
result is a null array.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
Constraint_Error is raised if this check fails.

If either operand is of the component type @i(C), the result of the
concatenation is given by the above rules, using in place of such an operand
an array having this operand as its only component (converted
to the component subtype)
and having the lower bound
of the index subtype of the array type as its lower bound.
@PDefn2{Term=[implicit subtype conversion],Sec=(operand of concatenation)}
@begin{Ramification}
  The conversion might raise Constraint_Error.
  The conversion provides @lquotes@;sliding@rquotes@;
  for the component in the case of an array-of-arrays, consistent with
  the normal Ada 95 rules that allow sliding during parameter passing.
@end{Ramification}

@Defn2{Term=[assignment operation], Sec=(during evaluation of concatenation)}
The result of a concatenation is defined in terms of an
assignment to an anonymous object,
as for any function call (see @RefSecNum{Return Statements}).
@begin{Ramification}
This implies that value adjustment is performed as appropriate
@em see @RefSecNum{User-Defined Assignment and Finalization}.
We don't bother saying this for other predefined operators,
even though they are all function calls,
because this is the only one where it matters.
It is the only one that can return a value having controlled parts.
@end{Ramification}
@end{RunTime}

@begin{Notes}
As for all predefined operators on modular types, the binary adding
operators + and @en on modular types include a final
reduction modulo the modulus if the result is outside
the base range of the type.
@begin{ImplNote}
A full "modulus" operation need not be performed after
addition or subtraction of modular types. For binary moduli,
a simple mask is sufficient. For nonbinary moduli, a check after
addition to see if the value is greater than the high bound of
the base range can be followed by a conditional subtraction of the modulus.
Conversely, a check after subtraction to see if a "borrow" was
performed can be followed by a conditional addition of the modulus.
@end{ImplNote}
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of expressions involving binary adding operators:)
@begin{Example}
Z + 0.1      @RI[--  Z has to be of a real type ]

"A" & "BCD"  @RI[--  concatenation of two string literals]
'A' & "BCD"  @RI[--  concatenation of a character literal and a string literal]
'A' & 'A'    @RI[--  concatenation of two character literals ]
@end{Example}
@end{Examples}

@begin{Inconsistent83}
@Defn{inconsistencies with Ada 83}
The lower bound of the result of concatenation,
for a type whose first subtype is constrained, is
now that of the index subtype. This is inconsistent with Ada 83,
but generally only for Ada 83 programs that raise Constraint_Error.
For example, the concatenation operator in
@begin(Example)
X : @key(array)(1..10) @key(of) Integer;
@key(begin)
X := X(6..10) & X(1..5);
@end(Example)

would raise Constraint_Error in Ada 83 (because
the bounds of the result of the concatenation would be 6..15, which is outside
of 1..10),
but would succeed and swap the halves of X (as expected) in Ada 95.
@end{Inconsistent83}

@begin{Extend83}
@Defn{extensions to Ada 83}
Concatenation is now useful for array types whose
first subtype is constrained.
When the result type of a concatenation
is such an array type,
Constraint_Error is avoided by effectively
first sliding the left operand (if nonnull) so that
its lower bound is that of the index subtype.
@end{Extend83}

@LabeledSubClause{Unary Adding Operators}

@begin{StaticSem}
@Leading@Defn{unary adding operator}@Defn2{Term=[operator],Sec=(unary adding)}
@Defn{+ operator}@Defn2{Term=[operator],Sec=(+)}
@Defn{plus operator}@Defn2{Term=[operator],Sec=(plus)}
@Defn{- operator}@Defn2{Term=[operator],Sec=(-)}
@Defn{minus operator}@Defn2{Term=[operator],Sec=(minus)}
The unary adding operators + (identity) and @en (negation) are predefined
for every specific numeric type @i(T) with their
conventional meaning.
They have the following specifications:
@begin(example)
@key(function) "+"(Right : @RI(T)) @key(return) @RI(T)
@key(function) "-"(Right : @RI(T)) @key(return) @RI(T)
@end(example)
@end{StaticSem}

@begin{Notes}
For modular integer types, the unary adding operator @en, when
given a nonzero operand, returns the result of subtracting
the value of the operand from the modulus;
for a zero operand, the result is zero.
@end{Notes}

@LabeledSubClause{Multiplying Operators}

@begin{StaticSem}
@Leading@Defn{multiplying operator}@Defn2{Term=[operator],Sec=(multiplying)}
@Defn{* operator}@Defn2{Term=[operator],Sec=(*)}
@Defn{multiply operator}@Defn2{Term=[operator],Sec=(multiply)}
@Defn{times operator}@Defn2{Term=[operator],Sec=(times)}
@Defn{/ operator}@Defn2{Term=[operator],Sec=(/)}
@Defn{divide operator}@Defn2{Term=[operator],Sec=(divide)}
@Defn{mod operator}@Defn2{Term=[operator],Sec=(mod)}
@Defn{rem operator}@Defn2{Term=[operator],Sec=(rem)}
The multiplying operators * (multiplication), / (division),
@key(mod) (modulus), and @key(rem) (remainder)
are predefined for every specific integer type @i(T):
@begin(example)
@key(function) "*"  (Left, Right : @RI(T)) @key(return) @RI(T)
@key(function) "/"  (Left, Right : @RI(T)) @key(return) @RI(T)
@key(function) "@key(mod)"(Left, Right : @RI(T)) @key(return) @RI(T)
@key(function) "@key(rem)"(Left, Right : @RI(T)) @key(return) @RI(T)
@end(example)

Signed integer multiplication has its conventional meaning.

@Leading@keepnext@;Signed integer division and remainder are defined by the relation:
@begin(example)
A = (A/B)*B + (A @key(rem) B)
@end(example)

@Leading@;where (A @key(rem) B) has the sign of A and an absolute value less than
the absolute value of B. Signed integer division satisfies the identity:
@begin(example)
(-A)/B = -(A/B) = A/(-B)
@end(example)

@begin{Wide}
@Leading@;The signed integer modulus operator is defined such
that the result of A @key(mod) B has
the sign of B and an absolute value less than the absolute value
of B; in addition, for some signed integer value N, this result
satisfies the relation:
@begin(example)
A = B*N + (A @key(mod) B)
@end(example)

The multiplying operators on modular types are defined in terms
of the corresponding signed integer operators@Redundant[, followed by a reduction
modulo the modulus if the result is outside
the base range of the type] @Redundant[(which is only possible for the "*"
operator)].
@begin{Ramification}
The above identity satisfied by signed integer
division is not satisfied by modular division
because of the difference in effect of negation.
@end{Ramification}
@end{Wide}

@Leading@;Multiplication and division operators are predefined for
every specific floating point type @i(T):
@begin(example)
@key(function) "*"(Left, Right : @RI(T)) @key(return) @RI(T)
@key(function) "/"(Left, Right : @RI(T)) @key(return) @RI(T)
@end(example)

@Leading@;The following multiplication and division operators, with
an operand of the predefined type Integer, are predefined
for every specific fixed point type @i(T):
@begin(example)
@key(function) "*"(Left : @RI(T); Right : Integer) @key(return) @RI(T)
@key(function) "*"(Left : Integer; Right : @RI(T)) @key(return) @RI(T)
@key(function) "/"(Left : @RI(T); Right : Integer) @key(return) @RI(T)
@end(example)

@Leading@Redundant[All of the above multiplying operators
are usable with an operand
of an appropriate universal numeric type.]  The following additional
multiplying operators for @i(root_real) are predefined@Redundant[,
and are usable when both operands are of an appropriate universal or
root numeric type, and the result is allowed to be of
type @i(root_real), as in a @nt<number_declaration>]:
@begin{Ramification}
These operators
are analogous to the multiplying operators
involving fixed or floating point types
where @i(root_real) substitutes for the
fixed or floating point type,
and @i(root_integer) substitutes for Integer.
Only values of the corresponding universal numeric types are
implicitly convertible to these root numeric types,
so these operators are really restricted to use with
operands of a universal type, or the specified
root numeric types.
@end{Ramification}
@begin(example)
@key(function) "*"(Left, Right : @RI(root_real)) @key(return) @RI(root_real)
@key(function) "/"(Left, Right : @RI(root_real)) @key(return) @RI(root_real)

@key(function) "*"(Left : @RI(root_real); Right : @RI(root_integer)) @key(return) @RI(root_real)
@key(function) "*"(Left : @RI(root_integer); Right : @RI(root_real)) @key(return) @RI(root_real)
@key(function) "/"(Left : @RI(root_real); Right : @RI(root_integer)) @key(return) @RI(root_real)
@end(example)

@Leading@;Multiplication and division between any two fixed point types are
provided by the following two predefined operators:
@begin{Ramification}
@i(Universal_fixed) is the universal type for the class of
fixed point types, meaning that these operators take operands
of any fixed point types (not necessarily the same)
and return a result that is implicitly (or explicitly) convertible to
any fixed point type.
@end{Ramification}
@begin(example)
@key(function) "*"(Left, Right : @RI(universal_fixed)) @key(return) @RI(universal_fixed)
@key(function) "/"(Left, Right : @RI(universal_fixed)) @key(return) @RI(universal_fixed)
@end(example)
@end{StaticSem}

@begin{Resolution}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00364-01]}
@ChgAdded{Version=[2],Text=[The above two fixed-fixed multiplying operators shall
not be used in a context where the expected type for the result is itself
@i(universal_fixed) @Redundant[@em the context has to identify some other
numeric type to which the result is to be converted, either explicitly or
implicitly]. An explicit conversion is required on the result when using the
above fixed-fixed multiplication operator when either operand is of a type
having a user-defined primitive multiplication operator declared immediately
within the same list of declarations as the type and with both formal
parameters of a fixed-point type. A corresponding requirement applies to
the universal fixed-fixed division operator.]}

@begin(Discussion)
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The @i(small) of @i(universal_fixed) is
infinitesimal; no loss of precision is permitted.
However, fixed-fixed division is impractical to implement when
an exact result is required,
and multiplication will sometimes result in unanticipated overflows
in such circumstances,
so we require an explicit conversion to be inserted in
expressions like A * B * C if A, B, and C are each of some fixed point
type.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[On the other hand, X := A * B; is permitted by this rule, even if X, A, and B
are all of different fixed point types, since the expected type
for the result of the multiplication is the type of X, which is necessarily
not @i(universal_fixed).]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00364-01]}
@ChgAdded{Version=[2],Text=[We have made these into Name Resolution rules to
ensure that user-defined primitive fixed-fixed operators are not made unusable
due to the presence of these universal fixed-fixed operators.]}
@end(Discussion)
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00364-01]}
@ChgDeleted{Version=[2],Text=[The above two fixed-fixed multiplying operators
shall not be used in a context where the expected type for the result
is itself @i(universal_fixed) @em @Redundant[the context has to
identify some other numeric type to which the result is to be converted,
either explicitly or implicitly].]}
@begin(Discussion)
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[The @i(small) of @i(universal_fixed) is infinitesimal; no loss
of precision is permitted.
However, fixed-fixed division is impractical to implement when
an exact result is required,
and multiplication will sometimes result in unanticipated overflows
in such circumstances,
so we require an explicit conversion to be inserted in
expressions like A * B * C if A, B, and C are each of some fixed point
type.]}

@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[On the other hand, X := A * B; is permitted by this rule, even if X, A, and B
are all of different fixed point types, since the expected type
for the result of the multiplication is the type of X, which is necessarily
not @i(universal_fixed).]}
@end(Discussion)
@end{Legality}

@begin{RunTime}
The multiplication and division operators for real types have
their conventional meaning.
@redundant[For floating point types, the accuracy of the result is
determined by the precision of the result type.
For decimal fixed point types, the result is truncated toward zero
if the mathematical result is between two multiples of the @i(small)
of the specific result type (possibly determined by context);
for ordinary fixed point types, if the mathematical result is
between two multiples of the @i(small), it is unspecified
which of the two is the result.
@PDefn{unspecified}]

@IndexCheck{Division_Check}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
The exception Constraint_Error is raised by
integer division, @key(rem),
and @key(mod) if the right operand is zero.
@Redundant[Similarly, for a real type @i(T) with @i(T')Machine_Overflows
True, division by zero raises Constraint_Error.]
@end{RunTime}

@begin{Notes}
@Leading@;For positive A and B, A/B is the quotient and A @key(rem) B is
the remainder when A is divided by B. The following relations are satisfied
by the rem operator:
@begin{Example}
     A  @key(rem) (-B) =   A @key(rem) B
   (-A) @key(rem)   B  = -(A @key(rem) B)
@end{Example}

@Leading@keepnext@;For any signed integer K, the following identity holds:
@begin{Example}
   A @key(mod) B   =   (A + K*B) @key(mod) B
@end{Example}
@begin{Bundle}
@NoPrefix@Leading@;The relations between signed integer
division, remainder, and modulus are
illustrated by the following table:
@begin{Example}
   A      B   A/B   A @key(rem) B  A @key(mod) B     A     B    A/B   A @key(rem) B   A @key(mod) B

   10     5    2       0        0       -10    5    -2       0         0
   11     5    2       1        1       -11    5    -2      -1         4
   12     5    2       2        2       -12    5    -2      -2         3
   13     5    2       3        3       -13    5    -2      -3         2
   14     5    2       4        4       -14    5    -2      -4         1

   A      B   A/B   A @key(rem) B  A @key(mod) B     A     B    A/B   A @key(rem) B   A @key(mod) B@*
   10    -5   -2       0        0       -10   -5     2       0         0
   11    -5   -2       1       -4       -11   -5     2      -1        -1
   12    -5   -2       2       -3       -12   -5     2      -2        -2
   13    -5   -2       3       -2       -13   -5     2      -3        -3
   14    -5   -2       4       -1       -14   -5     2      -4        -4
@end{Example}
@end{Bundle}
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of expressions involving multiplying operators:)
@begin{Example}
I : Integer := 1;
J : Integer := 2;
K : Integer := 3;

X : Real := 1.0;                      @RI[--     see @RefSecNum{Floating Point Types}]
Y : Real := 2.0;

F : Fraction := 0.25;                 @RI[--     see @RefSecNum{Fixed Point Types}]
G : Fraction := 0.5;
@end{Example}
@begin{Example}
@tabclear()@tabset(P19, P31)
@RI(Expression)  @\@RI(Value)  @\@RI(Result Type)@*
@R{I*J}            @\@R{2}      @\@RI(same as I and J, that is, Integer)
@R{K/J}            @\@R{1}      @\@RI(same as K and J, that is, Integer)
@R{K @key(mod) J}  @\@R{1}      @\@RI(same as K and J, that is, Integer)@*
@R{X/Y}            @\@R{0.5}    @\@RI(same as X and Y, that is, Real)
@R{F/2}            @\@R{0.125}  @\@RI(same as F, that is, Fraction)@*
@R{3*F}            @\@R{0.75}   @\@RI(same as F, that is, Fraction)
@R{0.75*G}         @\@R{0.375}  @\@RI(universal_fixed, implicitly convertible)
               @\       @\@RI(to any fixed point type)
@R{Fraction(F*G)}  @\@R{0.125}  @\@RI(Fraction, as stated by the conversion)
@R{Real(J)*Y}      @\@R{4.0}    @\@RI(Real, the type of both operands after)
               @\       @\@RI(conversion of J)
@end{Example}
@end{Examples}

@begin{Incompatible83}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00364-01]}
@Chg{Version=[2],New=[@Defn{incompatibilities with Ada 83}The universal
fixed-fixed multiplying operators are now directly available (see below).
Any attempt to use user-defined fixed-fixed multiplying operators
will be ambiguous with the universal ones. The only way to use the user-defined
operators is to fully qualify them in a prefix call. This problem was not
documented during the design of Ada 95, and has been mitigated by
Ada 2005.],Old=[]}
@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
Explicit conversion of the result of multiplying
or dividing two fixed point numbers is no longer required,
provided the context uniquely determines some specific
fixed point result type.
This is to improve support for decimal fixed point, where
requiring explicit conversion on every fixed-fixed multiply
or divide was felt to be inappropriate.

The type @i(universal_fixed) is covered by @i(universal_real),
so real literals and fixed point operands may be multiplied
or divided directly, without any explicit conversions required.
@end{Extend83}

@begin{DiffWord83}
We have used the normal syntax for function definition
rather than a tabular format.
@end{DiffWord83}


@begin{Incompatible95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00364-01]}
@Chg{Version=[2],New=[@Defn{incompatibilities with Ada 95}We have changed the
resolution rules for the universal fixed-fixed multiplying operators to remove
the incompatibility with Ada 83 discussed above. The solution is to hide
the universal operators in some circumstances. As a result, some legal Ada 95
programs will require the insertion of an explicit conversion around a
fixed-fixed multiply operator. This change is likely to catch as many bugs as
it causes, since it is unlikely that the user wanted to use predefined
operators when they had defined user-defined versions.],Old=[]}
@end{Incompatible95}


@LabeledSubClause{Highest Precedence Operators}

@begin{StaticSem}
@Leading@Defn{highest precedence operator}@Defn2{Term=[operator],Sec=(highest precedence)}
@Defn{abs operator}@Defn2{Term=[operator],Sec=(abs)}
@Defn{absolute value}
The highest precedence unary operator @key(abs) (absolute value)
is predefined for every specific numeric type @i(T),
with the following specification:
@begin(example)
@key(function) "@key(abs)"(Right : @RI(T)) @key(return) @RI(T)
@end(example)

@Leading@Defn{not operator}@Defn2{Term=[operator],Sec=(not)}
@IndexSeeAlso{Term=[logical operator],See=(not operator)}
The highest precedence unary operator @key(not) (logical negation) is
predefined for every boolean type @i(T),
every modular type @i(T),
and for every one-dimensional array type @i(T) whose
components are of a boolean type,
with the following specification:
@begin(example)
@key(function) "@key(not)"(Right : @RI(T)) @key(return) @RI(T)
@end(example)

The result of the operator @key(not) for a modular type is
defined as the difference between the high bound of the base range
of the type and the value of the operand. @Redundant[For
a binary modulus, this corresponds to a bit-wise complement
of the binary
representation of the value
of the operand.]

The operator @key(not) that applies
to a one-dimensional array of boolean
components yields a one-dimensional boolean array with the same bounds;
each component of the result is obtained by logical negation of the
corresponding component of the operand (that is, the component that
has the same index value).
@IndexCheck{Range_Check}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
A check is made that each component of the result belongs to the
component subtype; the exception Constraint_Error is raised if this
check fails.
@begin{Discussion}
  The check against the component subtype is per AI83-00535.
@end{Discussion}

@Leading@Defn{exponentiation operator}@Defn2{Term=[operator],Sec=(exponentiation)}
@Defn{** operator}@Defn2{Term=[operator],Sec=(**)}
The highest precedence @i(exponentiation) operator ** is predefined
for every specific integer type @i(T)
with the following specification:
@begin(example)
@key(function) "**"(Left : @RI(T); Right : Natural) @key(return) @RI(T)
@end(example)

@Leading@;Exponentiation is also predefined for
every specific floating point type
as well as @i{root_real},
with the following specification (where @i(T) is @i{root_real}
or the floating point type):
@begin(example)
@key(function) "**"(Left : @RI(T); Right : Integer'Base) @key(return) @RI(T)
@end(example)

@Defn{exponent}
The right operand of an exponentiation is the @i(exponent).
The expression X**N with the value of the exponent
N positive is equivalent to the
expression X*X*...X (with N@en@;1 multiplications) except that the multiplications
are associated in an arbitrary order. With N equal to zero, the result is one.
With the value of N negative
@Redundant[(only defined for a floating point operand)],
the result is the reciprocal of the result using the absolute value of
N as the exponent.
@begin{Ramification}
  The language does not specify the order of association of the multiplications
  inherent in an exponentiation. For a floating point type,
  the accuracy of the result might depend on the particular
  association order chosen.
@end{Ramification}

@end{StaticSem}

@begin{ImplPerm}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
The implementation of
exponentiation for the case of a negative exponent
is allowed to raise Constraint_Error
if the intermediate result of the repeated multiplications
is outside the safe range of the type, even though the final
result (after taking the reciprocal)
would not be.
(The best machine approximation to the
final result in this case would generally be 0.0.)
@end{ImplPerm}

@begin{Notes}
@IndexCheck{Range_Check}
As implied by the specification given above
for exponentiation of an integer type, a check is made that
the exponent is not negative.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
Constraint_Error is raised if this check fails.
@end{Notes}

@begin{Inconsistent83}
  @ChgRef{Version=[1],Kind=[Added],Ref=[8652/0100],ARef=[AI95-00018-01]}
  @ChgAdded{Version=[1],Text=[@Defn{inconsistencies with Ada 83}
  The definition of "**" allows arbitrary association of the
  multiplications which make up the result. Ada 83 required left-to-right
  associations (confirmed by AI83-00137). Thus it is possible that "**"
  would provide a slightly different answer in Ada 95 than in the same Ada 83
  program.]}
@end{Inconsistent83}

@begin{DiffWord83}
We now show the specification for "**" for integer types
with a parameter subtype of Natural rather than Integer for the exponent.
This reflects the fact that Constraint_Error is raised if
a negative value is provided for the exponent.
@end{DiffWord83}

