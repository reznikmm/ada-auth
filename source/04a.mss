@Part(04, Root="ada.mss")

@Comment{$Date: 2016/02/09 04:55:40 $}
@LabeledSection{Names and Expressions}

@Comment{$Source: e:\\cvsroot/ARM/Source/04a.mss,v $}
@Comment{$Revision: 1.140 $}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@Redundant[The rules applicable to the different forms of @nt<name> and
expression, and to their evaluation, are given in this
@Chg{Version=[3],New=[clause],Old=[section]}.]
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
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0003-1],ARef=[AI05-0139-2]}
@Syn{tabs=[P22], lhs=<name>,rhs="
     @Syn2{direct_name}@\| @Syn2{explicit_dereference}
   | @Syn2{indexed_component}@\| @Syn2{slice}
   | @Syn2{selected_component}@\| @Syn2{attribute_reference}
   | @Syn2{type_conversion}@\| @Syn2{function_call}
   | @Syn2{character_literal}@Chg{Version=[3],New=[@\| @Syn2{qualified_expression}
   | @Syn2{generalized_reference}@\| @Syn2{generalized_indexing}],Old=[]}"}


@Syn{lhs=<direct_name>,
rhs="@Syn2{identifier} | @Syn2{operator_symbol}"}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
  @nt<character_literal> is no longer a @nt<direct_name>.
  @nt<character_literal>s are usable even when the corresponding
  @Chg{Version=[2],New=[enumeration type
  declaration],Old=[@ntf<enumeration_type_declaration>]} is not visible. See
  @RefSecNum(Literals).
@end{Discussion}

@Syn{lhs=<prefix>,rhs="@Syn2{name} | @Syn2{implicit_dereference}"}

@Syn{lhs=<explicit_dereference>,rhs="@Syn2{name}.@key{all}"}

@Syn{lhs=<implicit_dereference>,rhs="@Syn2{name}"}

@end{Syntax}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0004-1]}
@Redundant[Certain forms of @nt<name> (@nt<indexed_component>s,
@nt<selected_component>s, @nt<slice>s, and
@Chg{Version=[3],New=[@nt<attribute_reference>s],Old=[@ntf<attribute>s]})
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
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0008-1]}
@PDefn2{Term=[nominal subtype], Sec=(associated with a dereference)}
If the type of the @nt{name} in a dereference is some access-to-object
type @i(T), then the dereference denotes a view of an object, the
@i(nominal subtype) of the view being the designated subtype
of @i(T).@Chg{Version=[3],New=[ If the designated subtype has
unconstrained discriminants, the (actual) subtype of the view is constrained
by the values of the discriminants of the designated object, except when
there is a partial view of the type of the designated subtype that does not
have discriminants, in which case the dereference is not constrained by its
discriminant values.],Old=[]}
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

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0008-1]}
  @ChgAdded{Version=[3],Text=[The last sentence was not present in Ada 95;
    it is necessary in Ada 2005 because general access types can designate
    unconstrained objects, which was not possible in Ada 95. Thus, the rules
    that had this effect in Ada 95 (the object being constrained by its initial
    value) don't work in Ada 2005 and we have to say this explicitly.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0008-1]}
  @ChgAdded{Version=[3],Text=[The @ldquote@;except@rdquote@; part of the
   last sentence prevents privacy
    @ldquote@;breaking@rdquote@;, so that
    if a private type has discriminants only in the full view, they don't
    interfere with freely interassigning values between objects of the type,
    even when the objects live in the heap.]}
@end{Reason}
@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0008-1]}
  @ChgAdded{Version=[3],Text=[Since we don't depend on whether the designated
  object is constrained, it is not necessary to include a constrained
  bit in every object that could be designated by a general access type.]}
@end{ImplNote}

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

@begin{WideAbove}
@leading@keepnext@i{Examples of dereferences:}
@end{WideAbove}
@begin{Example}@tabclear()@tabset(P19)
Next_Car.@key[all]@\--@RI[  explicit dereference denoting the object designated by]
               @\--@RI[  the access variable Next_Car (see @RefSecNum{Incomplete Type Declarations})]
Next_Car.Owner @\--@RI[  selected component with implicit dereference;]
               @\--@RI[  same as Next_Car.@key[all].Owner]
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
It's kind of like Ada 83's @ntf{simple_name}, but @ntf{simple_name} applied
to both direct visibility and visibility by selection,
and furthermore, it didn't work right for @nt{operator_symbol}s.
The syntax rule for @ntf{simple_name} is removed,
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

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0003-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}A
  @nt{qualified_expression} is now a @nt{name} denoting a constant view;
  this allows them to be used as a prefix and to be renamed as an object.
  They are often used to remove ambiguity from function calls, and there
  may be no other way to do that. Interestingly, a @nt{type_conversion} of
  a @nt{qualified_expression} is already legal in these contexts, so this
  change mainly reduces clutter by eliminating an otherwise unneeded
  @nt{type_conversion} from some expressions.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0008-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a missing rule so
  that most dereferences are assumed constrained (without determining whether
  the designated object is). This is just confirming the Ada 95 rules;
  Ada 2005 failed to ensure that this property was unchanged.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0299-1]}
  @ChgAdded{Version=[3],Text=[Added @nt{generalized_reference} and
  @nt{generalized_indexing} as types of @nt{name}; these are documented
  as extensions in the appropriate subclauses.]}
@end{DiffWord2005}


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
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00363-01]}
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
@nt{expression}s are evaluated in an arbitrary order.@PDefn2{Term=[arbitrary order],Sec=[allowed]}
The value of
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
 My_Schedule(Sat)     --@RI[  a component of a one-dimensional array @\(see @RefSecNum{Index Constraints and Discrete Ranges})]
 Page(10)             --@RI[  a component of a one-dimensional array @\(see @RefSecNum{Array Types})]
 Board(M, J + 1)      --@RI[  a component of a two-dimensional array @\(see @RefSecNum{Index Constraints and Discrete Ranges})]
 Page(10)(20)         --@RI[  a component of a component @\(see @RefSecNum{Array Types})]
 Request(Medium)      --@RI[  an entry in a family of entries @\(see @RefSecNum{Task Units and Task Objects})]
 Next_Frame(L)(M, N)  --@RI[  a component of a function call @\(see @RefSecNum{Subprogram Declarations})]
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
are evaluated in an arbitrary order.@PDefn2{Term=[arbitrary order],Sec=[allowed]}
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
  Stars(1 .. 15)        --@RI[  a slice of 15 characters @\(see @RefSecNum{String Types})]
  Page(10 .. 10 + Size) --@RI[  a slice of 1 + Size components @\(see @RefSecNum{Array Types})]
  Page(L)(A .. B)       --@RI[  a slice of the array Page(L) @\(see @RefSecNum{Array Types})]
  Stars(1 .. 0)         --@RI[  a null slice @\(see @RefSecNum{String Types})]
  My_Schedule(Weekday)  --@RI[  bounds given by subtype @\(see @RefSecNum{Index Constraints and Discrete Ranges} and @RefSecNum{Enumeration Types})]
  Stars(5 .. 15)(K)     --@RI[  same as Stars(K) @\(see @RefSecNum{String Types})]
                        --@RI[  provided that K is in 5 .. 15]
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
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  The components of a protected object cannot be named except
  by an expanded name, even from within the corresponding protected body.
  The protected body @Chg{Version=[3],New=[cannot],Old=[may not]} reference
  @Chg{New=[],Old=[the ]}the private
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

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00252-01],ARef=[AI95-00407-01]}
@ChgAdded{Version=[2],Text=[A view of a subprogram whose first formal parameter is of
a tagged type or is an access parameter whose designated type is tagged:]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00252-01],ARef=[AI95-00407-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0090-1]}
@ChgAdded{Version=[2],NoPrefix=[T],Text=[The @nt<prefix> (after any implicit
dereference) shall resolve to denote an object or value of a specific tagged
type @i<T> or class-wide type @i<T>'Class. The @nt<selector_name> shall resolve
to denote a view of a subprogram declared immediately within the declarative
region in which an ancestor of the type @i<T> is declared. The first formal
parameter of the subprogram shall be of type @i<T>, or a class-wide type that
covers @i<T>, or an access parameter designating one of these types. The
designator of the subprogram shall not be the same as that of a component of
the tagged type visible at the point of the @nt<selected_component>.
@Chg{Version=[3],New=[The subprogram shall not
be an implicitly declared primitive operation of type @i<T> that overrides
an inherited subprogram implemented by an entry or protected subprogram
visible at the point of the @nt{selected_component}. ],Old=[]}The
@nt<selected_component> denotes a view of this subprogram that omits the first
formal parameter. This view is called a @i{prefixed view} of the subprogram,
and the @nt{prefix} of the @nt<selected_component> (after any implicit
dereference) is called the @i<prefix> of the prefixed view.
@Defn{prefixed view}@Defn2{Term=[prefix],Sec=[of a prefixed view]}]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0090-1]}
  @ChgAdded{Version=[3],Text=[The part of the rule that excludes a primitive
  overriding subprogram as a selector applies only to the wrapper subprogram
  that is implicitly declared to override a subprogram inherited from a
  synchronized interface that is implemented by an operation of a task or
  protected type (see @RefSecNum{Task Units and Task Objects} and
  @RefSecNum{Protected Units and Protected Objects}). We don't want
  calls that use a prefixed view to be ambiguous between the wrapper
  subprogram and the implementing entry or protected operation. Note that
  it is illegal to declare an explicit primitive that has a prefixed view
  that is homographic with one of the type's operations, so
  in normal cases it isn't possible to have an ambiguity in a prefix call.
  However, a class-wide operation of an ancestor type that is declared in the
  same declaration list with the ancestor type is also considered, and that
  can still make a call ambiguous.]}
@end{Discussion}
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
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00252-01],ARef=[AI95-00407-01]}
@ChgAdded{Version=[2],Text=[For a subprogram whose first parameter is an
access parameter, the prefix of any prefixed view shall denote an aliased
view of an object.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00407-01]}
@ChgAdded{Version=[2],Text=[For a subprogram whose first parameter is of mode
@b<in out> or @b<out>, or of an anonymous access-to-variable type, the prefix
of any prefixed view shall denote a variable.]}

@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[We want calls through a prefixed view and through
a normal view to have the same legality. Thus, the implicit 'Access in
this new notation needs the same legality check that an explicit 'Access
would have. Similarly, we need to prohibit the object from being constant
if the first parameter of the subprogram is @key{in out}, because that is
(obviously) prohibited for passing a normal parameter.]}
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

@end{RunTime}

@begin{Examples}
@Leading@keepnext@i(Examples of selected components:)
@begin{Example}
@tabclear()@tabset(P60)
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00252-01],ARef=[AI95-00407-01]}
  Tomorrow.Month     --@RI[  a record component @\(see @RefSecNum{Record Types})]
  Next_Car.Owner     --@RI[  a record component @\(see @RefSecNum{Incomplete Type Declarations})]
  Next_Car.Owner.Age --@RI[  a record component @\(see @RefSecNum{Incomplete Type Declarations})]
                     --@RI[  the previous two lines involve implicit dereferences]
  Writer.Unit        --@RI[  a record component (a discriminant) @\(see @RefSecNum{Variant Parts and Discrete Choices})]
  Min_Cell(H).Value  --@RI[  a record component of the result @\(see @RefSecNum{Subprogram Declarations})]
                     --@RI[  of the function call Min_Cell(H)]
@Chg{Version=[2],New=<  Cashier.Append     --@RI[  a prefixed view of a procedure @\(see @RefSecNum{Interface Types})]
>,Old=<>}  Control.Seize      --@RI[  an entry of a protected object @\(see @RefSecNum{Protected Units and Protected Objects})]
  Pool(K).Write      --@RI[  an entry of the task Pool(K) @\(see @RefSecNum{Protected Units and Protected Objects})]
@end{Example}

@begin{WideAbove}
@leading@keepnext@i(Examples of expanded names:)
@end{WideAbove}
@begin{Example}
@tabclear()@tabset(P67)
  Key_Manager."<"      --@RI[  an operator of the visible part of a package @\(see @RefSecNum{Private Operations})]
  Dot_Product.Sum      --@RI[  a variable declared in a function body @\(see @RefSecNum{Subprogram Declarations})]
  Buffer.Pool          --@RI[  a variable declared in a protected unit @\(see @RefSecNum{Example of Tasking and Synchronization})]
  Buffer.Read          --@RI[  an entry of a protected unit @\(see @RefSecNum{Example of Tasking and Synchronization})]
  Swap.Temp            --@RI[  a variable declared in a block statement @\(see @RefSecNum{Block Statements})]
  Standard.Boolean     --@RI[  the name of a predefined type @\(see @RefSecNum{The Package Standard})]
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
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}The prefixed view notation
  for tagged objects is new. This provides a similar notation to that used in other
  popular languages, and also reduces the need for @nt{use_clause}s. This
  is sometimes known as @lquotes@;distinguished receiver notation@rquotes@;.
  @Defn{distinguished receiver notation}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[Given the following
definitions for a tagged type T:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{procedure} Do_Something (Obj : @key{in out} T; Count : @key{in} Natural);
@key{procedure} Do_Something_Else (Obj : @key{access} T; Flag : @key{in} Boolean);
My_Object : @key{aliased} T;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[the following calls are equivalent:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Do_Something (My_Object, Count => 10);
My_Object.Do_Something (Count => 10);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[as are the following calls:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Do_Something_Else (My_Object'Access, Flag => True);
My_Object.Do_Something_Else (Flag => True);]}
@end{Example}
@end{Extend95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0090-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected the definition of
  a prefixed view to ignore the implicit subprograms declared for
  @ldquote@;implemented by@rdquote entries and protected subprograms.]}
@end{DiffWord2005}


@RMNewPageVer{Version=[2]}@Comment{For printed version of Ada 2005 RM}
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

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI05-0004-1]}
@Syn{lhs=<attribute_designator>,rhs="
    @Syn2{identifier}[(@SynI{static_}@Syn2{expression})]
  | Access | Delta | Digits@Chg{Version=[3],New=[ | Mod],Old=[]}"}


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

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00114-01]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[Other than the rules given above,
  the @ResolutionName@;s for the @nt{prefix} of each attribute are defined as
  @ResolutionTitle for that attribute. If no such rules are defined, then no
  context at all should be used when resolving
  the @nt{prefix}. In particular, any knowledge about the kind of entities
  required must not be used for resolution unless that is required by
  @ResolutionTitle. This matters in obscure cases;
  for instance, given the following declarations:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[  @key[function] Get_It @key[return] Integer @key[is] ... -- @RI[(1)]
  @key[function] Get_It @key[return] Some_Record_Type @key[is] ... -- @RI[(2)]]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[the following @nt{attribute_reference} cannot be
resolved and is illegal:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[  @key[if] Get_It'Valid @key[then]]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[even though the Valid attribute is only defined
  for objects of scalar types, and thus cannot be applied to the result of
  function (2). That information cannot be used to resolve the @nt{prefix}.
  The same would be true if (2) @Chg{Version=[3],New=[had],Old=[was]} been
  a procedure; even though the procedure does not denote an object, the
  @nt{attribute_reference} is still illegal.]}
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
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0006-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0032-1],ARef=[AI12-0159-1]}
An @nt{attribute_reference} denotes a
value, an object, a subprogram, or some
other kind of program entity.@Chg{Version=[3],New=[
@Chg{Version=[4],New=[Unless explicitly specified otherwise, for],Old=[For]}
an @nt{attribute_reference} that denotes a value or an object, if
its type is scalar, then its nominal subtype is the base subtype of
the type; if its type is tagged, its nominal subtype is the first
subtype of the type; otherwise, its nominal subtype is a subtype of the type
without any constraint@Chg{Version=[4],New=[,],Old=[ or]}
@nt{null_exclusion}@Chg{Version=[4],New=[, or predicate],Old=[]}.
Similarly, unless explicitly specified otherwise, for an
@nt{attribute_reference} that denotes a function, when its result
type is scalar, its result subtype is the base subtype of the type,
when its result type is tagged, the result subtype is the first
subtype of the type, and when the result type is some other type,
the result subtype is a subtype of the type without any
constraint@Chg{Version=[4],New=[,],Old=[ or]}
@nt{null_exclusion}@Chg{Version=[4],New=[, or predicate],Old=[]}.],Old=[]}
@begin{Ramification}
  The attributes defined by the language are summarized in
  @RefSecNum{Language-Defined Attributes}.
  Implementations can define additional attributes.
@end{Ramification}
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0006-1]}
  @ChgAdded{Version=[3],Text=[The nominal subtype is primarily
  a concern when an @nt{attribute_reference}, or
  a call on an @nt{attribute_reference}, is used as the @nt{expression}
  of a case statement, due to the full coverage requirement based on
  the nominal subtype. For nondiscrete cases, we define the
  nominal subtype mainly for completeness. Implementations may
  specify otherwise for implementation-defined attribute functions.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The rule is written to match the meaning
  of the italicized @i{T} in the definition of attributes such as Input;
  see @RefSecNum{Logical Operators and Short-circuit Control Forms}.]}
@end{Discussion}
@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0006-1]}
  @ChgAdded{Version=[3],Text=[We don't worry about the fact that
  @ldquote@;base subtype@rdquote is not explicitly defined for the
  universal types. Since it is
  not possible to constrain a universal numeric type, all subtypes
  are unconstrained, and hence can be considered base subtypes.
  The wording above could be altered to bypass this issue, but it
  doesn't seem necessary, since universal integer is handled
  specially in the rules for case expression full coverage, and
  we don't allow user-defined functions for attribute functions
  whose result type is universal.]}
@end{Honest}

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
Color'First        --@RI[ minimum value of the enumeration type Color @\(see @RefSecNum{Enumeration Types})]
Rainbow'Base'First --@RI[ same as Color'First @\(see @RefSecNum{Enumeration Types})]
Real'Digits        --@RI[ precision of the type Real @\(see @RefSecNum{Floating Point Types})]
Board'Last(2)      --@RI[ upper bound of the second dimension of Board @\(see @RefSecNum{Index Constraints and Discrete Ranges})]
Board'Range(1)     --@RI[ index range of the first dimension of Board @\(see @RefSecNum{Index Constraints and Discrete Ranges})]
Pool(K)'Terminated --@RI[ True if task Pool(K) is terminated @\(see @RefSecNum{Task Units and Task Objects})]
Date'Size          --@RI[ number of bits for records of type Date @\(see @RefSecNum{Record Types})]
Message'Address    --@RI[ address of the record variable Message @\(see @RefSecNum{Discriminant Constraints})]
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
@ntf{simple_name}, because attribute @nt{identifier}s are not required to
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
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> The wording
  was changed to allow implementations to continue to implement the Ada 83
  Small attribute. This was always intended to be allowed.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00235-01]}
  @ChgAdded{Version=[2],Text=[The note about resolving prefixes of attributes
  was updated to reflect that the prefix of an Access attribute now has an
  expected type (see @RefSecNum{Operations of Access Types}).]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0006-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Defined the nominal
  subtype of an @nt{attribute_reference} to close a minor language hole.]}
@end{DiffWord2005}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0032-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Allowed overriding the
  nominal subtype of an @nt{attribute_reference} for an object; that is used
  elsewhere in this standard.]}

  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0159-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Added wording so it is clear that
  predicates don't apply to the result of an attribute.]}
@end{DiffWord2012}


@LabeledAddedSubClause{Version=[3],Name=[User-Defined References]}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Type=[Leading],Text=[Given a discriminated type @i<T>,
the following type-related operational aspect may be specified:]}

@begin{Description}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Implicit_Dereference@\This aspect is specified by a
@nt{name} that denotes an access discriminant declared for the
type @i<T>.@AspectDefn{Implicit_Dereference}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Implicit_Dereference],
    Text=[@ChgAdded{Version=[3],Text=[Mechanism for user-defined implicit .@key[all].]}]}

@end{Description}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Text=[A (view of a) type with a specified
Implicit_Dereference aspect is a @i{reference type}.@Defn{reference type}
A @i{reference object} is an object of a reference type.@Defn{reference object}
The discriminant named by the Implicit_Dereference aspect is the @i{reference
discriminant} of the reference type or reference object.@Defn{reference discriminant}
@Redundant[A @nt{generalized_reference} is a @nt{name} that identifies a
reference object, and denotes the object or subprogram designated by the
reference discriminant of the reference object.]]}

@ChgToGlossary{Version=[3],Kind=[Added],Term=<Reference type>,
Text=<@ChgAdded{Version=[3],Text=[A reference type is one that has user-defined
behavior for @ldquote.@key[all]@rdquote, defined by the
Implicit_Dereference aspect.]}>}
@end{StaticSem}

@begin{Syntax}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<generalized_reference>,Old=<>}>,
rhs="@Chg{Version=[3],New=<@SynI{reference_object_}@Syn2{name}>,Old=<>}"}

@end{Syntax}

@begin{Resolution}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[The expected type for the @SynI{reference_object_}@nt{name}
in a @nt{generalized_reference} is any reference type.@PDefn2{Term=[expected type],
  Sec=(@SynI{reference_object_}@nt{name})}]}
@end{Resolution}

@begin{StaticSem}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0138-1]}
@ChgAdded{Version=[4],Text=[The Implicit_Dereference aspect
is nonoverridable (see @RefSecNum{Aspect Specifications}).]}

@begin{Reason}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[This ensures that all descendants of a
  reference type have the same reference discriminant. This prevents
  generic contract problems with formal derived types.]}
@end{Reason}


@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Text=[A @nt{generalized_reference} denotes a view
equivalent to that of a dereference of the reference discriminant of the
reference object.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Text=[Given a reference type @i<T>, the
Implicit_Dereference aspect is inherited by descendants of type @i<T> if not
overridden. If a descendant type constrains the value of the reference
discriminant of @i<T> by a new discriminant, that new discriminant is the
reference discriminant of the descendant. @Redundant[If the descendant type
constrains the value of the reference discriminant of @i<T> by an
@nt{expression} other than the @nt{name} of a new discriminant, a
@nt{generalized_reference} that identifies an object of the descendant type
denotes the object or subprogram designated by the value of this constraining
expression.]]}

@end{StaticSem}

@begin{RunTime}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=[evaluation], Sec=(generalized_reference)}
The evaluation of a @nt{generalized_reference}
consists of the evaluation of the @SynI{reference_object_}@nt{name} and
a determination of the object or subprogram designated by the
reference discriminant of the named reference object.
@IndexCheck{Access_Check}
A check is made that the value of the reference
discriminant is not the null access value.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
Constraint_Error is raised if this check fails. The
@nt{generalized_reference} denotes the object or
subprogram designated by the value of the reference discriminant of the
named reference object.]}

@end{RunTime}

@begin{Examples}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0268-1]}
@ChgAdded{Version=[3],Text=[@key[type] Barrel @key[is tagged] ...  -- @Examcom{holds objects of type Element}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0299-1]}
@ChgAdded{Version=[3],Text=[@key[type] Ref_Element(Data : @key[access] Element) @key[is limited private]
   @key[with] Implicit_Dereference => Data;
      -- @Examcom{This Ref_Element type is a "reference" type.}
      -- @ExamCom{"Data" is its reference discriminant.}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0268-1]}
@ChgAdded{Version=[3],Text=[@key[function] Find (B : @key[aliased in out] Barrel; Key : String) @key[return] Ref_Element;
   -- @Examcom{Return a reference to an element of a barrel.}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0268-1],ARef=[AI05-0299-1]}
@ChgAdded{Version=[3],Text=[B: @key[aliased] Barrel;]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Text=[...]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0268-1]}
@ChgAdded{Version=[3],Text=[Find (B, "grape") := Element'(...);  -- @Examcom{Assign through a reference.}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0268-1]}
@ChgAdded{Version=[3],Text=[-- @Examcom{This is equivalent to:}
Find (B, "grape").Data.@key[all] := Element'(...);]}

@end{Example}
@end{Examples}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}The aspect
  Implicit_Dereference and the @nt{generalized_reference} are new.]}
@end{Extend2005}

@begin{Incompatible2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0138-1]}
  @ChgAdded{Version=[4],Text=[@Defn{incompatibilities with Ada 2012}@b<Corrigendum:>
  Defined Implicit_Dereference to be nonoveridable, which makes redefinitions
  and hiding of the aspect illegal. It's possible that some program could
  violate one of these new restrictions, but this is not very likely as
  reference types are not likely to be used in a hierarchy.]}
@end{Incompatible2012}


@LabeledAddedSubClause{Version=[3],Name=[User-Defined Indexing]}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Type=[Leading],Text=[Given a tagged type @i<T>,
the following type-related, operational aspects
may be specified:]}

@begin{Description}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Constant_Indexing@\This aspect shall be specified by
a @nt{name} that denotes one or more functions declared immediately within the
same declaration list in which @i<T> is declared. All such functions shall
have at least two parameters, the first of which is of type @i<T> or
@i<T>'Class, or is an access-to-constant parameter with designated type @i<T> or
@i<T>'Class.@AspectDefn{Constant_Indexing}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Constant_Indexing],
    Text=[@ChgAdded{Version=[3],Text=[Defines function(s) to implement
      user-defined @nt{indexed_component}s.]}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Variable_Indexing@\This aspect shall be specified
by a @nt{name} that denotes one or more functions declared immediately within
the same declaration list in which @i<T> is declared. All such functions
shall have at least two parameters, the first of which is of type @i<T> or
@i<T>'Class, or is an access parameter with designated type @i<T> or @i<T>'Class.
All such functions shall have a return type that is a reference
type (see @RefSecNum{User-Defined References}), whose reference discriminant
is of an access-to-variable type.@AspectDefn{Variable_Indexing}]}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[We require these functions to return a reference
  type so that the object returned from the function can act like a variable.
  We need no similar rule for Constant_Indexing, since all functions return
  constant objects.]}
@end{Reason}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Variable_Indexing],
    Text=[@ChgAdded{Version=[3],Text=[Defines function(s) to implement
      user-defined @nt{indexed_component}s.]}]}

@end{Description}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0104-1]}
@ChgAdded{Version=[3],Text=[These aspects are inherited by descendants of @i<T>
(including the class-wide type @i<T>'Class).@Chg{Version=[4],New=[],Old=[
@Redundant[The aspects shall not be overridden, but the functions they denote
may be.]]}]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Indexing can be provided for multiple index types
  by overloading routines with different parameter profiles. For instance, the
  map containers provide indexing on both cursors and keys by providing
  pairs of overloaded routines to the Constant_Indexing and Variable_Indexing
  aspects.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@ChgAdded{Version=[3],Text=[An @i<indexable container type> is (a view of) a
tagged type with at least one of the aspects Constant_Indexing or
Variable_Indexing specified.@Defn{indexable container type}
An @i<indexable container object> is an object of an indexable container
type.@Defn{indexable container object} @Redundant[A @nt{generalized_indexing} is
a @nt{name} that denotes the result of calling a function named by a
Constant_Indexing or Variable_Indexing aspect.]]}

@ChgToGlossary{Version=[3],Kind=[Added],Term=<Indexable container type>,
Text=<@ChgAdded{Version=[3],Text=[An indexable container type is one that has
user-defined behavior for indexing, via the Constant_Indexing or
Variable_Indexing aspects.]}>}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0138-1]}
@ChgAdded{Version=[4],Text=[The Constant_Indexing and Variable_Indexing aspects
are nonoverridable (see @RefSecNum{Aspect Specifications}).]}

@begin{Reason}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[This ensures that all descendants of an
  indexable container type have aspects with the same properties. This prevents
  generic contract problems with formal derived types.]}

  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0104-1],ARef=[AI12-0138-1]}
  @ChgAdded{Version=[4],Text=[A nonoverridable aspect allows the replacement
    of the implementation of an indexing function and the addition of a
    new indexing function for a derived type, but not the removal of an
    indexing function. This is necessary so that indexing can be used on
    objects of T'Class. So long as the tag of O is that of its nominal
    subtype, we do not want T'Class(O)(I) to mean something different
    than O(I). Thus we cannot allow a change in the function identified.
    As T'Class(O)(I) expands into a dispatching call, we need to ensure that
    there is a body for each such function -- but it is OK for that body to be
    changed from the original body (that's just normal dispatching).]}
@end{Reason}

@begin{NotIso}
@ChgAdded{Version=[4],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 6 through 9
were deleted.>}]}@Comment{This message should be
deleted if the paragraphs are ever renumbered. Note that this is under the
wrong heading, so that the heading can also be eliminated.}
@end{NotIso}

@end{StaticSem}

@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@begin{Legality}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgRef{Version=[4],Kind=[DeletedNoDelMsg],ARef=[AI12-0138-1]}
@ChgDeleted{Version=[4],Type=[Leading],Text=[@Chg{Version=[3],New=[The
Constant_Indexing or Variable_Indexing aspect shall not be specified:],Old=[]}]}
@begin{Itemize}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgRef{Version=[4],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[4],Text=[@Chg{Version=[3],New=[on a derived type
if the parent type has the
corresponding aspect specified or inherited; or],Old=[]}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgRef{Version=[4],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[4],Text=[@Chg{Version=[3],New=[on a
@nt{full_type_declaration} if the type has
a tagged partial view.],Old=[]}]}
@end{Itemize}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgRef{Version=[4],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[4],Text=[@Chg{Version=[3],New=[@PDefn{generic contract issue}
In addition to the places where @LegalityTitle normally apply
(see @RefSecNum{Generic Instantiation}),
these rules apply also in the private part of an instance of a
generic unit.],Old=[]}]}

@begin{Ramification}
  @ChgNote{The following notes were moved to @RefSecNum{Aspect Specifications}
  along with the rules (now part of the definition of "nonoverridable aspect").}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgRef{Version=[4],Kind=[DeletedNoDelMsg]}
  @ChgDeleted{Version=[4],Text=[@Chg{Version=[3],New=[In order to enforce
  these rules without breaking privacy, we cannot allow a tagged private type
  to have hidden indexing aspects. There is no problem if the private type
  is not tagged (as the indexing aspects cannot be specified on
  descendants in that case).],Old=[]}]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgRef{Version=[4],Kind=[DeletedNoDelMsg]}
  @ChgDeleted{Version=[4],Text=[@Chg{Version=[3],New=[We don't need
  an assume-the-worst rule as deriving from formal tagged types is not
  allowed in generic bodies.],Old=[]}]}
@end{Ramification}

@end{Legality}

@begin{Syntax}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<generalized_indexing>,Old=<>}>,
rhs="@Chg{Version=[3],New=<@SynI{indexable_container_object_}@Syn2{prefix} @Syn2{actual_parameter_part}>,Old=<>}"}

@end{Syntax}

@begin{Resolution}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@ChgAdded{Version=[3],Text=[The expected type for the
@Syni{indexable_container_object_}@nt{prefix} of a @nt{generalized_indexing}
is any indexable container type.@PDefn2{Term=[expected type],
  Sec=(@SynI{indexable_container_object_}@nt{prefix})}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[If the Constant_Indexing aspect is
specified for the type of the @SynI{indexable_container_object_}@nt{prefix} of a
@nt{generalized_indexing}, then the @nt{generalized_indexing} is interpreted as
a @i<constant indexing> under the following
circumstances:@Defn{constant indexing}@Defn2{Term=[indexing],Sec=[constant]}]}

@begin{Itemize}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[when the Variable_Indexing aspect is not specified
     for the type of the @SynI{indexable_container_object_}@nt{prefix};]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[when the @SynI{indexable_container_object_}@nt{prefix}
    denotes a constant;]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[when the @nt{generalized_indexing} is used within
    a @nt{primary} where a @nt{name} denoting a constant is permitted.]}

    @begin{Ramification}
      @ChgRef{Version=[3],Kind=[AddedNormal]}
      @ChgAdded{Version=[3],Text=[This means it is not interpreted as a
        constant indexing for the @SynI{variable_}@nt{name} in the LHS of an
        assignment (not inside a @nt{primary}), nor for the @nt{name} used
        for an @key[out] or @key[in out] parameter (not allowed to be
        a constant), nor for the @nt{name} in an object renaming
        (not inside a primary), unless
        there is no Variable_Indexing aspect defined.]}
    @end{Ramification}

@end{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Otherwise, the @nt{generalized_indexing} is
interpreted as a
@i{variable indexing}.@Defn{variable indexing}@Defn2{Term=[indexing],Sec=[variable]}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[When a @nt{generalized_indexing} is interpreted as a
constant (or variable) indexing, it is equivalent to a call on a prefixed view
of one of the functions named by the Constant_Indexing (or Variable_Indexing)
aspect of the type of the @Syni{indexable_container_object_}@nt{prefix} with the given
@nt{actual_parameter_part}, and with the @Syni{indexable_container_object_}@nt{prefix} as
the @nt{prefix} of the prefixed view.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[In other words, the
    @nt{generalized_indexing} is equivalent to:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@SynI{indexable_container_object_}@nt{prefix}.Indexing @nt{actual_parameter_part}]}
@end{Example}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0005-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[where Indexing is the @nt{name}
    specified for the Constant_Indexing or Variable_Indexing
    aspect.@Chg{Version=[4],New=[ This equivalence is then resolved in the
    normal way; the aspect specifies a @nt{name}, it does not denote
    declarations.],Old=[]}]}
@end{Ramification}

@end{Resolution}

@begin{Notes}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0104-1]}
  @ChgAdded{Version=[4],Text=[The Constant_Indexing and Variable_Indexing
    aspects cannot be redefined when inherited for a derived type, but the
    functions that they denote can be modified by overriding or overloading.]}
@end{Notes}

@begin{Examples}
@begin{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0268-1],ARef=[AI05-0292-1]}
@ChgAdded{Version=[3],Text=[@key[type] Indexed_Barrel @key[is tagged] ...
  @key[with] Variable_Indexing => Find;
  -- @Examcom{Indexed_Barrel is an indexable container type,}
  -- @Examcom{Find is the generalized indexing operation.}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0268-1]}
@ChgAdded{Version=[3],Text=[@key[function] Find (B : @key[aliased in out] Indexed_Barrel; Key : String) @key[return] Ref_Element;
   -- @Examcom{Return a reference to an element of a barrel (see @RefSecNum{User-Defined References}).}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0268-1]}
@ChgAdded{Version=[3],Text=[IB: @key[aliased] Indexed_Barrel;]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0268-1]}
@ChgAdded{Version=[3],Text=[-- @Examcom{All of the following calls are then equivalent:}
Find (IB,"pear").Data.@key[all] := Element'(...); -- @Examcom{Traditional call}
IB.Find ("pear").Data.@key[all] := Element'(...); -- @Examcom{Call of prefixed view}
IB.Find ("pear")          := Element'(...); -- @Examcom{Implicit dereference (see @RefSecNum{User-Defined References})}
IB      ("pear")          := Element'(...); -- @Examcom{Implicit indexing and dereference}
IB      ("pear").Data.@key[all] := Element'(...); -- @Examcom{Implicit indexing only}]}

@end{Example}
@end{Examples}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}Aspects
  Constant_Indexing and Variable_Indexing, and the @nt{generalized_indexing}
  syntax are new.]}
@end{Extend2005}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0104-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Converted confusing and
    unnecessary normative wording about "overriding an aspect" into a note.]}

  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0138-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:>
  Defined Constant_Indexing and Variable_Indexing to be nonoveridable.
  This is merely a new description for @LegalityTitle which already applied
  to these aspects.]}
@end{DiffWord2012}


@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
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
  It might more properly be called an @ntf<enumeration_identifier>,
  except for historical reasons.
@end(Discussion)
@end{Intro}

@begin{Resolution}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00230-01]}
@ChgDeleted{Version=[2],Text=[@PDefn2{Term=[expected type],Sec=(null literal)}
The expected type for a literal @key(null) shall be a single
access type.]}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
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
@ChgDeleted{Version=[2],Text=[A literal @s<null>@ChgNote{We use @S since this
isn't a nonterminal, and since it is deleted we don't want to fix it.} shall
not be of an anonymous
access type@Redundant[, since such types do not have a null value
(see @RefSecNum{Access Types})].]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[This is a legality rule rather than an overloading
rule, to simplify implementations.]}
@end{Reason}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
An integer literal is of type @i{universal_integer}.
A real literal is of type @i{universal_real}.@Chg{Version=[2],New=[ The literal
@key<null> is of type @i<universal_access>.@Chg{Version=[3],
New=[@PDefn{universal_integer}@PDefn{universal_real}@PDefn{universal_access}],Old=[]}],Old=[]}
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
3.14159_26536 @\--@RI[  a real literal]
1_345 @\--@RI[  an integer literal]
'A' @\--@RI[  a character literal]
"Some Text" @\--@RI[  a string literal ]
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
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
@PDefn2{Term=[expected type],Sec=(aggregate)}
The expected type for an @nt{aggregate} shall be a
single @Chg{Version=[2],New=[],Old=[nonlimited ]}array
type, record type, or record extension.
@begin{Discussion}
See @RefSec{The Context of Overload Resolution}
for the meaning of @lquotes@;shall be a single ... type.@rquotes@;
@end{Discussion}
@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
@ChgAdded{Version=[3],Text=[There are additional rules for each kind of
aggregate. These aggregate rules are additive; a legal expression needs to
satisfy all of the applicable rules. That means the rule given here must be
satisfied even when it is syntactically possible to tell which specific kind of
aggregate is being used.]}
@end{Ramification}
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
arbitrary order.@PDefn2{Term=[arbitrary order],Sec=[allowed]}
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
  @RefSecNum{Assignment and Finalization}.
  Note that the value as a whole is not adjusted
  @em just the subcomponents (and ancestor part, if any).
  @RefSecNum{Assignment and Finalization} also describes
  when this anonymous object is finalized.

  If the @nt<ancestor_part> is a @nt<subtype_mark>
  the Initialize procedure for the ancestor type is applied
  to the ancestor part after default-initializing it,
  unless the procedure is abstract, as described
  in @RefSecNum{Assignment and Finalization}.
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
will work (see @RefSecNum{Assignment and Finalization}).
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

  @ChgRef{Version=[2],Kind=[AddedNormal]} @ChgAdded{Version=[2],Text=[The call
  to P is ambiguous in Ada 2005, while it would not be ambiguous in Ada 95 as
  the @nt{aggregate} could not have a limited type. Qualifying the
  @nt{aggregate} will eliminate any ambiguity. This construction would be
  rather confusing to a maintenance programmer, so it should be avoided, and
  thus we expect it to be rare.]}
@end{Incompatible95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}@nt{Aggregate}s can
  be of a limited type.]}
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
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
  Otherwise@Chg{Version=[3],New=[,],Old=[]} the construct would be interpreted
  as a parenthesized expression.
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
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
@PDefn2{Term=[expected type],Sec=(record_aggregate)}
The expected type for a @nt{record_aggregate} shall be
a single @Chg{Version=[2],New=[],Old=[nonlimited ]}record
type or record extension.
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

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[3],Text=[For a derived type (including type extensions),
  the order of declaration is defined in @RefSec{Derived Types and Classes}.
  In particular, all discriminants come first, regardless of whether they are
  defined for the parent type or are newly added to the derived type.]}
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

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0016-1]}
@Chg{Version=[3],New=[The reserved words @key[null record] may appear only if],
Old=[If]} there are no components needed in a given
@nt<record_@!component_@!association_@!list>@Chg{Version=[3],New=[],Old=[,
then the reserved words @key(null record) shall appear rather
than a list of @nt<record_@!component_@!association>s]}.
@begin{Ramification}
  For example, "(@key(null record))" is a @nt<record_aggregate>
  for a null record type. Similarly, "(T'(A) @key(with null record))" is
  an @nt<extension_aggregate> for a type defined as a null
  record extension of T.

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0016-1]}
@ChgAdded{Version=[3],Text=[If no components are needed and @key[null record]
  is not used, the @nt{record_@!component_@!association} must necessarily be
  @key[others] => <>, as that is the only
  @nt{record_@!component_@!association} that does not require an associated
  component.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0199-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0046-1]}
Each @nt<record_component_association>@Chg{Version=[2],New=[ other than an
@key{others} choice with a <>],Old=[]} shall have at least
one associated component, and each needed component
shall be associated with exactly
one @nt<record_@!component_@!association>.
If a @nt<record_@!component_@!association> @Chg{Version=[2],New=[with an
@nt{expression} ],Old=[]}has two or more associated components, all of them
shall be of the same type@Chg{Version=[3],New=[, or all of them shall be of
anonymous access types whose subtypes
statically match],Old=[]}.@Chg{Version=[4],New=[
In addition, @LegalityTitle are enforced separately for each associated
component.],Old=[]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
  These rules apply to an association with an @key(others)
  choice@Chg{Version=[2],New=[ with an expression. An @key(others) choice with
  a <> can match zero components or several components with different
  types],Old=[]}.
@end{Ramification}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
  Without these rules, there would be no way to know what
  was the expected type for the @nt<expression> of the association.
  @Chg{Version=[2],New=[Note that some of the rules do not apply to <>
  associations, as we do not need to resolve anything. We allow @key{others}
  => <> to match no components as this is similar to array aggregates.
  That means that (@key{others} => <>) always represents a default-initialized
  record or array value.],Old=[]}
@end{Reason}
@begin{Discussion}
  @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0046-1]}
  @ChgAdded{Version=[4],Type=[Leading],Text=[]}@ChgNote{Fake to get conditional "leading"}
  AI83-00244 also requires that the @nt{expression} shall
  be legal for each associated component. @Chg{Version=[4],New=[Ada 95
  omitted this wording, as it was thought that all cases of
  difference had been eliminated. That probably was true, but Ada 2005
  reintroduced cases where the types match but the legality differs.
  For example:],Old=[This is
  because even though two components have the same type, they might have
  different subtypes. Therefore, the legality of the
  @nt<expression>, particularly if it is an array aggregate,
  might differ depending on the associated component's subtype.
  However, we have relaxed the rules on array aggregates slightly for Ada 95,
  so the staticness of an applicable index constraint has no
  effect on the legality of the array aggregate to which it applies.
  See @RefSecNum{Array Aggregates}. This was the only case (that we know of)
  where a subtype provided by context affected the legality
  of an @nt{expression}.]}

@begin{Example}
@ChgRef{Version=[4],Kind=[Added]}
@ChgAdded{Version=[4],Text=[@key[type] Rec (D : @key[access] Integer) @key[is record]
          F : @key[access] Integer;
@key[end record];]}

@ChgRef{Version=[4],Kind=[Added]}
@ChgAdded{Version=[4],Text=[...
X : @key[aliased] Integer;
R : Rec := (D | F => X'Access); -- @examcom<Legal for D, illegal for F>]}
@end{Example}

  @ChgRef{Version=[4],Kind=[Added]}
  @ChgAdded{Version=[4],Text=[There are additional ways for this to happen;
  because of cases like the above we require that the @LegalityTitle are checked
  individually for each associated component.]}
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

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0220-1]}
@Chg{Version=[3],New=[The],Old=[If the components of a @nt{variant_part}
are needed, then the]} value of a discriminant that governs
@Chg{Version=[3],New=[a],Old=[the]} @nt{variant_part}
@Chg{Version=[3],New=[@i<P> ],Old=[]}shall be given
by a static expression@Chg{Version=[3],New=[, unless @i<P> is nested within
a @nt{variant} @i<V> that is not selected by the discriminant value
governing the @nt{variant_part} enclosing @i<V>],Old=[]}.
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
the component with the per-object constraint.@PDefn2{Term=[arbitrary order],Sec=[allowed]}
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

@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
@ChgAdded{Version=[3],Text=[We don't need similar language for <>,
as we're considering the value of <> for each individual component.
Each component has its own default expression or its own
default initialization (they can be different for each component;
the components even could have different types), and each one
has to be evaluated. So there is no need to repeat that.]}
@end{Ramification}

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
(4, July, 1776)                                       --@RI[  see @RefSecNum{Record Types} ]
@end{Example}

@begin{WideAbove}
@leading@keepnext@i(Examples of record aggregates with named associations:)
@end{WideAbove}
@begin{Example}
(Day => 4, Month => July, Year => 1776)
(Month => July, Day => 4, Year => 1776)

(Disk, Closed, Track => 5, Cylinder => 12)            --@RI[  see @RefSecNum{Variant Parts and Discrete Choices}]
(Unit => Disk, Status => Closed, Cylinder => 9, Track => 1)
@end{Example}

@begin{WideAbove}
@leading@keepnext@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
@i(@Chg{Version=[2],New=[Examples],Old=[Example]} of component
@Chg{Version=[2],New=[associations],Old=[association]} with
several choices:)
@end{WideAbove}
@begin{Example}
@tabclear()@tabset(P50)
(Value => 0, Succ|Pred => @key(new) Cell'(0, @key(null), @key(null))) @\--@RI[  see @RefSecNum{Incomplete Type Declarations}]

 --@RI[  The allocator is evaluated twice: Succ and Pred designate different cells]

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[(Value => 0, Succ|Pred => <>) @\--@RI[  see @RefSecNum{Incomplete Type Declarations}]],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[ --@RI[  Succ and Pred will be set to @key{null}]],Old=[]}

@end{Example}

@begin{WideAbove}
@leading@keepnext@i{Examples of record aggregates for tagged types
(see @RefSecNum(Tagged Types and Type Extensions)
and @RefSecNum{Type Extensions}):}
@end{WideAbove}
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
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}<> can be used in
  place of an @nt{expression} in a @nt{record_aggregate}, default
  initializing the component.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
  @ChgAdded{Version=[2],Text=[Limited @nt{record_aggregate}s are allowed (since
  all kinds of aggregates can now be limited, see @RefSecNum{Aggregates}).]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0220-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b<Correction:>
  Corrected wording so that the rule for discriminants governing @nt{variant_part}s
  was not effectively circular. The change makes a few @nt{aggregate}s where a
  nonstatic discriminant governs an empty @nt{variant_part} illegal. However,
  most Ada implementations already enforce some version of the new rule and
  already reject these @nt{aggregate}s. So it is unlikely that any
  incompatibility will be noticed in practice.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0016-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}@b<Correction:> Fixed the wording so that
  @key[others] => <> can be used in place of @key[null record].
  This is needed to avoid a generic contract issue for generic bodies:
  we do not want to have to assume the worst to disallow @key[others] => <>
  if the record type @i{might} be a null record.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0199-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> We now allow multiple components
  with anonymous access types to be specified with a single component
  association. This is to be consistent with the capabilities of a named
  access type.]}
@end{Extend2005}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0046-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> We explicitly say that the
  @LegalityTitle have to be rechecked for each component individually.
  This @i<seems> obvious, but as the AARM note @RefSecNum{Record Aggregates}
  (16.c) appeared to say that this was not necessary, and since we explicitly
  state this sort of thing for generic instances, it seemed better to be
  explicit.]}
@end{DiffWord2012}


@RMNewPageVer{Version=[2]}@Comment{For printed version of Ada 2005 RM}
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
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
@PDefn2{Term=[expected type], Sec=(extension_aggregate)}
The expected type for an @nt{extension_aggregate} shall be
a single @Chg{Version=[2],New=[],Old=[nonlimited ]}type that is a
record extension.
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
@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
@ChgAdded{Version=[3],Text=[This rule is additive with the rule given in
@RefSecNum{Aggregates}. That means the @RefSecNum{Aggregates} rule must be
satisfied even though it is always syntactically possible to tell that
something is an extension aggregate rather than another kind of aggregate.
Specifically, that means that an extension aggregate is ambiguous if the
context is overloaded on array and/or untagged record types, even though
those are never legal contexts for an extension aggregate. Thus, this rule
acts more like a @LegalityName than a @ResolutionName.]}
@end{Ramification}
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00306-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0115-1]}
If the @nt<ancestor_part> is a @nt<subtype_mark>, it shall
denote a specific tagged subtype.
@Chg{Version=[2],New=[If the @nt{ancestor_part} is an @nt{expression}, it
shall not be dynamically tagged. ],Old=[]}
The type of the @nt{extension_aggregate} shall be
@Chg{Version=[3],New=[a descendant of],Old=[derived from]} the type of the
@nt<ancestor_part>@Chg{Version=[3],New=[ (the @i<ancestor> type)@Defn2{Term=[ancestor type],
Sec=<of an @nt<extension_aggregate>>}],Old=[]},
through one or more record extensions (and no private
extensions).@Chg{Version=[3],New=[ If the @nt{ancestor_part} is a
@nt{subtype_mark}, the view of the ancestor type from which the type
is descended (see @RefSecNum{Private Operations})
shall not have unknown discriminants.],Old=[]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00306-01]}
  @ChgAdded{Version=[2],Text=[The expression cannot be dynamically tagged to
  prevent implicit "truncation" of a dynamically-tagged value to the specific
  ancestor type. This is similar to the
  rules in @RefSecNum{Dispatching Operations of Tagged Types}.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0067-1],ARef=[AI05-0244-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[If the type of the @nt{ancestor_part}
is limited and at least one component is needed
in the @nt{record_component_association_list}, then the @nt{ancestor_part}
shall not be:]}
@begin{Itemize}
@ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[a call to a function with an unconstrained result
  subtype; nor]}

@ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[a parenthesized or qualified expression whose
  operand would violate this rule; nor]}

@ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[a @nt{conditional_expression} having at least one
  @SynI{dependent_}@nt{expression} that would violate this rule.]}
@end{Itemize}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0067-1],ARef=[AI05-0244-1]}
  @ChgAdded{Version=[3],Text=[This restriction simplifies implementation,
  because it ensures that either the caller or the callee knows the size to
  allocate for the aggregate. Without this restriction, information from both
  caller and callee would have to be combined to determine the appropriate
  size.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0067-1]}
  @ChgAdded{Version=[3],Text=[The (F(...) with null record) case is exempt from
  this rule, because such extension
  aggregates are created internally for inherited functions returning
  null-extension types @em we can't very well make those illegal. Moreover, we
  don't need the rule for null extensions, as the result can simply use
  the space returned by the function call.]}
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
or initialization that depends on it.@PDefn2{Term=[arbitrary order],Sec=[allowed]}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0282-1]}
@IndexCheck{Discriminant_Check}
If the type of the @nt<ancestor_part> has
discriminants @Chg{Version=[3],New=[and],Old=[that are not inherited by the
type of the @nt{extension_aggregate},
then, unless]} the @nt<ancestor_part> is@Chg{Version=[3],New=[ not],Old=[]}
a @nt<subtype_mark> that
denotes an unconstrained subtype,@Chg{Version=[3],New=[ then],Old=[]}
a check is made that each discriminant @Chg{Version=[3],New=[determined by the
@nt{ancestor_part}],Old=[of the ancestor]}
has the value specified for a corresponding discriminant,@Chg{Version=[3],New=[ if any,],Old=[]}
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

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0282-1]}
  @ChgAdded{Version=[3],Text=[The check needs to be made any time that the
  ancestor is constrained; the source of the discriminants or the constraints
  is irrelevant.]}
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
(see @RefSecNum{Assignment and Finalization}).
@end{Notes}

@begin{Examples}
@Leading@keepnext@i{Examples of extension aggregates (for types defined in @RefSecNum{Type Extensions}):}
@begin(example)
Painted_Point'(Point @key{with} Red)
(Point'(P) @key{with} Paint => Black)

(Expression @key{with} Left => 1.2, Right => 3.4)
Addition'(Binop @key{with null record})
             --@RI[ presuming Binop is of type Binary_Operation]
@end(example)

@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
The extension aggregate syntax is new.
@end{Extend83}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00306-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  @b[Amendment Correction:] Eliminated implicit @lquotes@;truncation@rquotes
  of a dynamically tagged value when it is used as an ancestor
  @nt{expression}. If an @nt{aggregate} includes such an @nt{expression},
  it is illegal in Ada 2005. Such @nt{aggregate}s are thought to be rare;
  the problem can be fixed with a type conversion to the appropriate
  specific type if it occurs.]}
@end{Incompatible95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
  @ChgAdded{Version=[2],Text=[Limited @nt{extension_aggregate}s are allowed (since
  all kinds of aggregates can now be limited, see @RefSecNum{Aggregates}).]}
@end{DiffWord95}

@begin{Inconsistent2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0282-1]}
  @ChgAdded{Version=[3],Text=[@Defn{inconsistencies with Ada 2005}@b[Correction:]
  An @nt{extension_aggregate} with an @nt{ancestor_part} whose discriminants
  are constrained and inherited might now raise Constraint_Error if the
  @nt{aggregate}'s type is constrained, while it was OK in Ada 2005. In almost
  all cases, this will make no difference as the constraint will be checked
  by the immediately following use of the @nt{aggregate}, but it is possible to
  compare such an aggregate for equality;
  in this case, no exception would be raised by Ada 2005, while Ada 2012 will
  raise Constraint_Error. This should be very rare, and having the possibility
  means that the representation of the aggregate type has to be able to support
  unconstrained values of the type, even if the first subtype is constrained
  and no such objects can be created any other way.]}
@end{Inconsistent2005}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0067-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b[Correction:]
  A limited unconstrained ancestor expression that is a function call is now
  illegal unless the extension part is null.
  Such @nt{aggregate}s were first introduced in Ada 2005 and are very complex
  to implement as they must be built-in-place with an unknown size; as such,
  it is unlikely that they are implemented correctly in existing compilers and
  thus not often used in existing code.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0115-1]}
  @ChgAdded{Version=[3],Text=[@b[Correction:] An @nt{ancestor_part} that is a
  subtype with unknown discriminants is now explicitly illegal. Such a
  subtype should not be used to declare an object, and the @nt{ancestor_part}
  acts like an object. The Ada 95 rules did not disallow such cases, so it
  is possible that code exists that uses such an ancestor, but this should
  be rare.]}
@end{Incompatible2005}


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
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02]}
  @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0157-1]}
  For an @nt{explicit_actual_parameter},
  an @nt{explicit_generic_actual_parameter},
  the @nt{expression} of a
  @Chg{Version=[2],New=[return statement],Old=[@nt{return_statement}]},
  @Chg{Version=[4],New=[the return expression of an expression
  function, ],Old=[]}the initialization expression
  in an @nt{object_@!declaration}, or a @nt{default_@!expression}
  @Redundant[(for a parameter or a component)],
  when the nominal subtype
  of the corresponding formal parameter, generic formal parameter,
  function @Chg{Version=[2],New=[return object],Old=[result]},
  @Chg{Version=[4],New=[expression function return object, ],Old=[]}object, or
  component is a constrained array subtype, the
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

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0147-1]}
  For a parenthesized @nt{expression}, the
  applicable index constraint is that, if any, defined for the
  @nt{expression}@Chg{Version=[3],New=[;],Old=[.]}
@begin{Discussion}
RM83 omitted this
  case, presumably as an oversight. We want to minimize situations
  where an @nt{expression} becomes illegal if parenthesized.
@end{Discussion}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0147-1]}
@ChgAdded{Version=[3],Text=[For a @nt{conditional_expression}, the
applicable index constraint for each @SynI<dependent_>@nt{expression} is that,
if any, defined for the @nt{conditional_expression}.]}
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

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0153-3]}
The @nt{discrete_choice_list} of an
@nt{array_component_association} is allowed to
have a @nt{discrete_choice} that is a nonstatic
@Chg{Version=[3],New=[@nt<choice_expression>],Old=[@nt<expression>]}
or that is a @Chg{Version=[3],New=[@nt{subtype_indication} or @nt{range}],
Old=[@nt{discrete_range}]} that defines a nonstatic or
null range, only if it is the single @nt{discrete_choice} of
its @nt{discrete_choice_list}, and there is only one
@nt{array_component_association} in the @nt<array_aggregate>.
@begin{Discussion}
We now allow a nonstatic @key(others) choice even if there are
other array component expressions as well.
@end{Discussion}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
In a @nt<named_array_aggregate>
@Chg{Version=[3],New=[where all @nt{discrete_choice}s are
static],Old=[with more than one @nt<discrete_choice>]},
no two @nt<discrete_choice>s are allowed to
cover the same value (see @RefSecNum{Variant Parts and Discrete Choices});
if there is no @key[others] choice, the @nt<discrete_choice>s taken
together shall
exactly cover a contiguous sequence of values of the corresponding index type.
@begin{Ramification}
  This implies that each component must be
  specified exactly once. See AI83-309.
@end{Ramification}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Text=[This has to apply even if there is only one
  static @nt{discrete_choice}; a single choice has to represent a contiguous
  range (a @nt{subtype_mark} with a static predicate might represent a
  discontiguous set of values). If the (single) choice is a dynamic subtype, we
  don't need to make this check as no predicates are allowed (see
  @RefSecNum{Subtype Predicates}) and
  thus the range has to be contiguous.]}
@end{Reason}

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
  index type;@PDefn2{Term=[arbitrary order],Sec=[allowed]}
  @PDefn2{Term=[implicit subtype conversion],Sec=(choices of aggregate)}

  The array component expressions of the aggregate
  are evaluated in an arbitrary order and
  their values are converted to the component subtype of
  the array type; an array component expression
  is evaluated once for each associated component.
  @PDefn2{Term=[implicit subtype conversion],Sec=(expressions of aggregate)}
  @PDefn2{Term=[arbitrary order],Sec=[allowed]}
@end(enumerate)
@begin(Ramification)
  Subaggregates are not separately evaluated.
  The conversion of the value of the component expressions
  to the component subtype might raise Constraint_Error.

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
@ChgAdded{Version=[3],Text=[We don't need to say that <> is
  evaluated once for each component, as <> means that each component
  is @i{initialized by default}. That means that the actions defined
  for default initialization are applied to each component individually.
  Initializing one component by default and copying
  that to the others would be an incorrect implementation in general
  (although it might be OK if the default initialization is known
  to be constant).]}
@end(Ramification)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00287-01]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0084-1]}
@ChgAdded{Version=[2],Text=[Each @nt<expression> in an
@nt<array_component_association> defines the value for the associated
component(s). For an @nt<array_component_association> with <>, the associated
component(s) are initialized @Chg{Version=[4],New=[to the Default_Component_Value
of the array type if this aspect has been specified for the array type;
otherwise, they are initialized ],Old=[]}by default
as for a stand-alone object of the
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

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0037-1]}
@IndexCheck{Index_Check}
For an @nt{array_aggregate} with an @key(others) choice,
a check is made that no @nt<expression>@Chg{Version=[3],New=[ or <>],Old=[]}
is specified for an index value outside the bounds determined by the
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
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0004-1]}
In an @nt<array_aggregate>, positional notation may only be used
with two or more @nt<expression>s; a single @nt<expression>
in parentheses is interpreted as a
@Chg{Version=[3],New=[parenthesized expression],Old=[@ntf{parenthesized_expression}]}.
A @nt<named_array_aggregate>, such as (1 => X), may be used to specify
an array with a single component.
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of array aggregates with positional associations:)
@begin{Example}
(7, 9, 5, 1, 3, 2, 4, 8, 6, 0)
Table'(5, 8, 4, 1, @key(others) => 0)  --@RI[  see @RefSecNum{Array Types} ]
@end{Example}

@begin{WideAbove}
@leading@keepnext@i(Examples of array aggregates with named associations:)
@end{WideAbove}
@begin{Example}
(1 .. 5 => (1 .. 8 => 0.0))      --@RI[  two-dimensional]
(1 .. N => @key(new) Cell)             --@RI[  N new cells, in particular for N = 0]

Table'(2 | 4 | 10 => 1, @key(others) => 0)
Schedule'(Mon .. Fri => True,  @key(others) => False)  --@RI[  see @RefSecNum{Array Types}]
Schedule'(Wed | Sun  => False, @key(others) => True)
Vector'(1 => 2.5)                                --@RI[  single-component vector]
@end{Example}

@begin{WideAbove}
@leading@keepnext@i(Examples of two-dimensional array aggregates:)
@end{WideAbove}
@begin{Example}
--@RI[ Three aggregates for the same value of subtype Matrix(1..2,1..3) (see @RefSecNum{Array Types}):]

((1.1, 1.2, 1.3), (2.1, 2.2, 2.3))
(1 => (1.1, 1.2, 1.3), 2 => (2.1, 2.2, 2.3))
(1 => (1 => 1.1, 2 => 1.2, 3 => 1.3), 2 => (1 => 2.1, 2 => 2.2, 3 => 2.3))
@end{Example}

@begin{WideAbove}
@leading@keepnext@i(Examples of aggregates as initial values:)
@end{WideAbove}
@begin{Example}
A : Table := (7, 9, 5, 1, 3, 2, 4, 8, 6, 0);        --@RI[ A(1)=7, A(10)=0]
B : Table := (2 | 4 | 10 => 1, @key(others) => 0);        --@RI[ B(1)=0, B(10)=1]
C : @key(constant) Matrix := (1 .. 5 => (1 .. 8 => 0.0)); --@RI[ C'Last(1)=5, C'Last(2)=8]

D : Bit_Vector(M .. N) := (M .. N => True);         --@RI[ see @RefSecNum{Array Types}]
E : Bit_Vector(M .. N) := (@key(others) => True);
F : String(1 .. 1) := (1 => 'F');  --@RI[ a one component aggregate: same as "F"]
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Type=[Leading],KeepNext=[T],Text=[@i{Example of an array
aggregate with defaulted others choice and with an applicable index constraint
provided by an enclosing record aggregate:}]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Buffer'(Size => 50, Pos => 1, Value => String'('x', @key(others) => <>))  --@RI[ see @RefSecNum{Discriminants}]]}
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
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}<> can be used in
  place of an @nt{expression} in an @nt{array_aggregate}, default-initializing
  the component.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
  @ChgAdded{Version=[2],Text=[Limited @nt{array_aggregate}s are allowed (since
  all kinds of aggregates can now be limited, see @RefSecNum{Aggregates}).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00318-02]}
  @ChgAdded{Version=[2],Text=[Fixed @nt{aggregate}s to use the subtype of
  the return object of a function, rather than the result subtype, because
  they can be different for an @nt{extended_return_statement}, and we want
  to use the subtype that's explicitly in the code at the point of the
  @nt{expression}.]}
@end{DiffWord95}

@begin{Inconsistent2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0037-1]}
  @ChgAdded{Version=[3],Text=[@Defn{inconsistencies with Ada 2005}@b<Correction:>
  Fixed so the check
  for components outside of the array applies to both @nt<expression>s and
  <>s. As <> was a new feature in Ada 2005, there should be little existing
  code that depends on a <> component that is specified outside of the array
  (and that is nonsense anyway, that a compiler is likely to detect even
  without an explicit language rule disallowing it).]}
@end{Inconsistent2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1]}
  @ChgAdded{Version=[3],Text=[Added a definition of the applicable index
  constraint for @nt{conditional_expression}s (which are new).]}
@end{DiffWord2005}

@begin{Inconsistent2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI05-0084-1]}
  @ChgAdded{Version=[4],Text=[@Defn{inconsistencies with Ada 2012}@b<Corrigendum:>
  Fixed so that the Default_Component_Value (if any) is used to initialize
  components specified with <>. This is what users would expect, and all
  Ada 2012 implementation known at the time of this writing initialize with
  the Default_Component_Value, so it is unlikely that anyone will be affected
  by this inconsistency.]}
@end{Inconsistent2012}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI05-0157-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:>
  Added expression functions to the contexts that provide an applicable
  index constraint, because expression functions are handled separately in
  static semantics and legality rules.]}
@end{DiffWord2012}


@LabeledClause{Expressions}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0147-1],ARef=[AI05-0158-1],ARef=[AI05-0176-1]}
@Defn{expression}
An @i(expression) is a formula that defines the computation or retrieval
of a value.
In this International Standard, the term @lquotes@;expression@rquotes@; refers to a construct
of the syntactic category @nt<expression> or of any of the
@Chg{Version=[3],New=[following categories: @nt{choice_expression}, @nt{choice_relation},
@nt{relation}, @nt{simple_expression}, @nt{term}, @nt{factor}, @nt{primary},
@nt{conditional_expression}, @nt{quantified_expression}],Old=[other five syntactic categories defined
below]}.
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

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0158-1]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<choice_expression>,Old=<>}>,
rhs="@Chg{Version=[3],New=<
     @Syn2[choice_relation] {@key[and] @Syn2[choice_relation]}
   | @Syn2[choice_relation] {@key[or] @Syn2[choice_relation]}
   | @Syn2[choice_relation] {@key[xor] @Syn2[choice_relation]}
   | @Syn2[choice_relation] {@key[and then] @Syn2[choice_relation]}
   | @Syn2[choice_relation] {@key[or else] @Syn2[choice_relation]}>,Old=<>}"}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0158-1]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<choice_relation>,Old=<>}>,
rhs="@Chg{Version=[3],New=<
     @Syn2{simple_expression} [@Syn2{relational_operator} @Syn2{simple_expression}]>,Old=<>}"}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0158-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0022-1],ARef=[AI12-0039-1]}
@Syn{lhs=<relation>,rhs="
     @Syn2{simple_expression} [@Syn2{relational_operator} @Syn2{simple_expression}]
   | @Chg{Version=[4],New=[@SynI{tested_}@Syn2{simple_expression}],Old=[@Syn2{simple_expression}]} [@key{not}] @key{in} @Chg{Version=[3],New=[@Syn2{membership_choice_list}@Chg{Version=[4],New=[
   | @Syn2{raise_expression}],Old=[]}],Old="@Syn2{range}
   | @Syn2{simple_expression} [@key{not}] @key{in} @Syn2{subtype_mark}"}"}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0158-1]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<membership_choice_list>,Old=<>}>,
rhs="@Chg{Version=[3],New=<@Syn2{membership_choice} {| @Syn2{membership_choice}}>,Old=<>}"}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0158-1]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0039-1]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<membership_choice>,Old=<>}>,
rhs="@Chg{Version=[3],New=<@Chg{Version=[4],New=<@SynI{choice_}@Syn2{simple_expression}>,Old=<@Syn2{choice_expression}>} | @Syn2{range} | @Syn2{subtype_mark}>,Old=<>}"}

@Syn{lhs=<simple_expression>,rhs="[@Syn2{unary_adding_operator}] @Syn2{term} {@Syn2{binary_adding_operator} @Syn2{term}}"}


@Syn{lhs=<term>,rhs="@Syn2{factor} {@Syn2{multiplying_operator} @Syn2{factor}}"}


@Syn{lhs=<factor>,rhs="@Syn2{primary} [** @Syn2{primary}] | @key{abs} @Syn2{primary} | @key{not} @Syn2{primary}"}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0003-1],ARef=[AI05-0147-1],ARef=[AI05-0176-1]}
@Syn{lhs=<primary>,rhs="
   @Syn2{numeric_literal} | @key{null} | @Syn2{string_literal} | @Syn2{aggregate}
 | @Syn2{name} | @Chg{Version=[3],New=[],Old=[@Syn2{qualified_expression} | ]}@Syn2{allocator} | (@Syn2{expression})@Chg{Version=[3],New=[
 | (@Syn2{conditional_expression}) | (@Syn2{quantified_expression})],Old=[]}"}
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
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
  This means that if extra-range intermediates are used to
  hold the value of an object of an unconstrained numeric subtype,
  a Constraint_Error can be raised on a read of the object, rather than
  only on an assignment to it. Similarly, it means that
  computing the value of an object of such a subtype
  can be deferred until the first read of the object
  (presuming no side effects other than failing an Overflow_Check
  are possible). This permission is over and above that provided
  by @Chg{Version=[3],New=[subclause],Old=[clause]}
  @RefSecNum(Exceptions and Optimization), since
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
@Trailing@;4.0                --@RI[  real literal]
Pi                 --@RI[  named number]
(1 .. 10 => 0)     --@RI[  array aggregate]
Sum                --@RI[  variable]
Integer'Last       --@RI[  attribute]
Sine(X)            --@RI[  function call]
Color'(Blue)       --@RI[  qualified expression]
Real(M*N)          --@RI[  conversion]
(Line_Count + 10)  --@RI[  parenthesized expression ]
@end{Example}

@leading@keepnext@i(Examples of expressions:)
@begin{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
Volume                      --@RI[ primary]
@key(not) Destroyed               --@RI[ factor]
2*Line_Count                --@RI[ term]
-4.0                        --@RI[ simple expression]
-4.0 + A                    --@RI[ simple expression]
B**2 - 4.0*A*C              --@RI[ simple expression]@Chg{Version=[2],New=[
R*Sin(@unicode<952>)*Cos(@unicode<966>)             --@RI[ simple expression]],Old=[]}
Password(1 .. 3) = "Bwv"    --@RI[ relation]
Count @key(in) Small_Int          --@RI[ relation]
Count @key(not) @key(in) Small_Int      --@RI[ relation]
Index = 0 @key(or) Item_Hit       --@RI[ expression]
(Cold @key(and) Sunny) @key(or) Warm    --@RI[ expression (parentheses are required)]
A**(B**C)                   --@RI[ expression (parentheses are required)]
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
In Ada 83, @key{out} parameters and their nondiscriminant
subcomponents are not allowed as @ntf{primaries}.
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

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0003-1]}
  @ChgAdded{Version=[3],Text=[Moved @nt{qualified_expression}
  from @nt{primary} to @nt{name} (see @RefSecNum{Names}). This allows the
  use of @nt{qualified_expression}s in more places.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1],ARef=[AI05-0176-1]}
  @ChgAdded{Version=[3],Text=[Added @nt{conditional_expression} and
  @nt{quantified_expression} to @nt{primary}.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0158-1]}
  @ChgAdded{Version=[3],Text=[Expanded membership test syntax (see
  @RefSecNum{Relational Operators and Membership Tests}).]}
@end{DiffWord2005}

@begin{Inconsistent2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0039-1]}
  @ChgAdded{Version=[4],Text=[@Defn{inconsistencies with Ada 2012}@b<Corrigendum:>
  Revised membership syntax to eliminate ambiguities. In some cases,
  previously ambiguous membership expressions will now have an unambiguous
  meaning. If an Ada 2012 implementation chose the "wrong" meaning, the
  expression could silently change meaning. Virtually all such expressions
  will become illegal because of type mismatches (and thus be incompatible,
  not inconsistent). However, if the choices
  are all of a Boolean type, resolution might succeed. For instance, @exam{A
  @key[in] B | C @key[and] D} now always means @exam{(A @key[in] B | C)
  @key[and] D}, but the original Ada 2012 syntax would have allowed it to mean
  @exam{A @key[in] B | (C @key[and] D)}. If a compiler allowed the expression
  and interpreted it as the latter, the meaning of the expression would silently
  change. We expect this to be extremely rare as membership operations on
  Boolean types are unlikely (and this can happen only in code written for Ada
  2012).]}
@end{Inconsistent2012}

@begin{Incompatible2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0039-1]}
  @ChgAdded{Version=[4],Text=[@Defn{incompatibilities with Ada 2012}@b<Corrigendum:>
  The revised membership syntax will require
  parentheses in @nt{membership_choice_list}s in some cases where the
  Ada 2012 grammar did not require them. For instance,
  @exam{A @key[in] B @key[in] C | D}
  is now illegal. However, such expressions can be interpreted in multiple ways
  (either @exam{A @key[in] (B @key[in] C) | D} or
  @exam{A @key[in] (B @key[in] C | D)} for this example), so using such
  expressions is likely to be dangerous (another compiler might interpret the
  expression differently). In addition, all such expressions occur only in
  Ada 2012 syntax; so they should be rare.]}
@end{Incompatible2012}



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
@key(not) Sunny @key(or) Warm    --@RI[  same as (not Sunny) or Warm]
X > 4.0 @key(and) Y > 0.0  --@RI[  same as (X > 4.0) and (Y > 0.0)]

-4.0*A**2            --@RI[  same as @en@;(4.0 * (A**2))]
@key(abs)(1 + A) + B       --@RI[  same as (abs (1 + A)) + B]
Y**(-3)              --@RI[  parentheses are necessary]
A / B * C            --@RI[  same as (A/B)*C]
A + (B + C)          --@RI[  evaluate B + C before adding it to A ]
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
  @ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00145-01]}
  @ChgDeleted{Version=[2],Text=[For predefined operators, the parameter
  and result subtypes shown as @i(T) are actually the unconstrained
  subtype of the type.]}
  @ChgNote{Sorry, Bob, but there is no "honesty" issue here. And
  "unconstrained" is wrong.}
@end{Honest}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00145-01]}
  @ChgAdded{Version=[2],Text=[For these operators, we are talking about
  the type without any (interesting) subtype, and not some subtype with a
  constraint or exclusion. Since it's possible that there is no name for
  the @lquotes@;uninteresting@rquotes subtype, we denote the type
  with an italicized @i(T).
  This applies to the italicized @i(T) in many other predefined operators and
  attributes as well.@Defn2{Term=[T],Sec=[italicized]}]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00145-01]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[In many cases, there is a subtype
  with the correct properties available. The italicized @i(T) means:]}
@begin{Itemize}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[@i(T)'Base, for scalars;]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[the first subtype of @i(T), for tagged types;]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[a subtype of the type @i(T) without any
  constraint or null exclusion, in other cases.]}
@end{Itemize}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Note that @lquotes@;without a constraint@rquotes
  is not the same as unconstrained. For instance, a record type with no
  discriminant part is considered constrained; no subtype of it has a
  constraint, but the subtype is still constrained.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Thus, the last case often is the same as
  the first subtype of @i(T), but that isn't the case for constrained array
  types (where the correct subtype is unconstrained) and for access types
  with a @nt{null_exclusion} (where the correct subtype does not
  exclude null).]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This italicized @i(T) is used for defining
  operators and attributes of the language. The meaning is intended to be
  as described here.]}
@end{Ramification}

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
Filter(1 .. 10) @key(and) Filter(15 .. 24)   --@RI[   see @RefSecNum{Index Constraints and Discrete Ranges} ]
@end{Example}

@begin{WideAbove}
@leading@keepnext@i(Examples of short-circuit control forms:)
@end{WideAbove}
@begin{Example}
Next_Car.Owner /= @key(null) @key(and) @key(then) Next_Car.Owner.Age > 25   --@RI[   see @RefSecNum{Incomplete Type Declarations}]
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

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1],ARef=[AI05-0269-1]}
@Defn{membership test}
@Defn{in (membership test)}
@Defn{not in (membership test)}
A @i(membership test), using @key(in) or @key(not in),
determines whether or not a value
belongs to @chg{Version=[3],New=[any],Old=[a]} given subtype or range,
@chg{Version=[3],New=[is equal to any given value,],Old=[or]} has a tag
that identifies a type that is covered by a given type@chg{Version=[3],New=[,
or is convertible to and has an accessibility level appropriate for a given
access type],Old=[]}. Membership tests are allowed for all types.]

@end{Intro}

@begin{Resolution}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0158-1]}
@PDefn2{Term=[expected type],
  Sec=(membership test simple_expression)}
@Defn2{Term=[tested type], Sec=(of a membership test)}
The @i(tested type) of a membership test
is@Chg{Version=[3],New=[],Old=[ the type of the @nt<range> or the type]}
determined by the @Chg{Version=[3],New=[@nt<membership_choice>s of the
@nt<membership_choice_list>. Either all @nt{membership_choice}s of the
@nt{membership_choice_list} shall resolve to the same type, which is the tested
type; or each @nt{membership_choice} shall be of an elementary type, and the
tested type shall be covered by each of these elementary
types.],Old=[@nt<subtype_mark>. If the tested type is tagged, then the
@nt<simple_expression> shall resolve to be of a type that
@Chg{Version=[2],New=[is convertible (see @RefSecNum{Type Conversions})
to],Old=[covers or is covered by]} the tested type; if untagged, the expected
type for the @nt<simple_expression> is the tested type.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0158-1]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0039-1]}
@ChgAdded{Version=[3],Text=[If the tested type is tagged, then the
@Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
shall resolve to be of a type that is
convertible (see @RefSecNum{Type Conversions}) to the tested
type; if untagged, the expected type for the
@Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
is the tested type. The expected type of a
@Chg{Version=[4],New=[@SynI{choice_}@nt{simple_expression}],Old=[@nt{choice_expression}]}
in a @nt{membership_choice}, and of a @nt{simple_expression} of a @nt{range} in
a @nt{membership_choice}, is the tested type of the membership operation.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
  The part of the rule for untagged types is stated in a way
  that ensures that operands
  like @Chg{Version=[2],New=[a string literal],Old=[@key(null)]} are still
  legal as operands of a membership test.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0039-1]}
  The significance of @lquotes@;@Chg{Version=[2],New=[is convertible to],
  Old=[covers or is covered by]}@rquotes@; is that we allow the
  @Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
  to be of any class-wide type that @Chg{Version=[2],
  New=[could be converted to],Old=[covers]} the tested type, not just the
  one rooted at the tested type.@Chg{Version=[2],New=[ This includes any
  class-wide type that covers the tested type, along with class-wide interfaces
  in some cases.],Old=[]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0158-1]}
@ChgAdded{Version=[3],Text=[The special rule for determining the tested type for
  elementary types is to allow numeric literals in @nt{membership_choice_list}s.
  Without the rule, @exam{A @key[in] B | 1} would be illegal as B and 1 would
  have different types (the literal having type @i<universal integer>).]}
@end{Reason}

@end{Resolution}

@begin{Legality}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0039-1]}
For a membership test, if the
@Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
is of a tagged class-wide type,
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

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0158-1]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0039-1]}
@ChgAdded{Version=[3],Text=[If a membership test includes one or more
@Chg{Version=[4],New=[@SynI{choice_}@nt{simple_expression}s],Old=[@nt{choice_expression}s]}
and the tested type of the membership test is limited, then the
tested type of the membership test shall have a visible primitive equality
operator.]}
@begin{Reason}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0158-1]}
@ChgAdded{Version=[3],Text=[A visible equality operator is required in order
to avoid breaking privacy; that is, we don't want to depend on a hidden
equality operator.]}
@end{Reason}
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
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00230-01],ARef=[AI95-00420-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[At least one of the operands of an equality
operator for @i<universal_access> shall be of a specific anonymous access type.
Unless the predefined equality operator is identified using an expanded name
with @nt{prefix} denoting the package Standard, neither operand shall be of an
access-to-object type whose designated type is @i<D> or @i<D>'Class, where
@i<D> has a user-defined primitive equality operator such that:]}
@begin{Itemize}
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[its result type is Boolean;]}

  @ChgRef{Version=[2],Kind=[Added]}
  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0020-1]}
  @ChgAdded{Version=[2],Text=[it is declared immediately within the same
  declaration list as @i<D>@Chg{Version=[3],New=[ or any partial or incomplete
  view of @i<D>],Old=[]}; and]}

  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[at least one of its operands is an
  access parameter with designated type @i<D>.]}
@end{Itemize}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The first sentence prevents compatibility
  problems by ensuring that these operators are not used for named access
  types. Also, universal access types do not count for the purposes of this
  rule. Otherwise, equality expressions like (X = @key{null}) would be
  ambiguous for normal access types.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The rest of the rule makes it possible to
  call (including a dispatching call) user-defined "=" operators for anonymous
  access-to-object types (they'd be hidden
  otherwise), and to write user-defined "=" operations for anonymous access
  types (by making it possible to see the universal operator using the
  Standard prefix).]}
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We don't need a similar rule for anonymous
  access-to-subprogram types because they can't be primitive for any type.
  Note that any nonprimitive user-defined equality operators still are hidden
  by the universal operators; they'll have to be called with a package
  prefix, but they are likely to be very uncommon.]}
@end{Ramification}
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00230-01]}
@ChgAdded{Version=[2],Text=[At least one of the operands of the equality
operators for @i<universal_access> shall be of type @i<universal_access>, or
both shall be of access-to-object types, or both shall be of
access-to-subprogram types. Further:]}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[When both are of access-to-object types, the
designated types shall be the same or one shall cover the
other, and if the designated types are elementary or array types,
then the designated subtypes shall statically match;]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[When both are of access-to-subprogram types,
the designated profiles shall be subtype conformant.]}
@end{Itemize}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We don't want to allow completely arbitrary
  comparisons, as we don't want to insist that all access types are represented
  in ways that are convertible to one another. For instance, a compiler could
  use completely separate address spaces or incompatible representations.
  Instead, we allow compares if there exists an access parameter to which both
  operands could be converted. Since the user could write such an subprogram,
  and any reasonable meaning for "=" would allow using it in such a subprogram,
  this doesn't impose any further restrictions on Ada implementations.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0123-1]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0101-1]}
@ChgAdded{Version=[3],Text=[If the profile of an explicitly declared primitive
equality operator of an untagged record type is type conformant with that of the
corresponding predefined equality operator, the declaration shall occur before
the type is frozen.@Chg{Version=[4],New=[],Old=[ In addition, if the untagged
record type has a nonlimited partial view, then the declaration shall occur
in the visible part of the enclosing package.]}
@PDefn{generic contract issue}
In addition to the places where @LegalityTitle normally apply
(see @RefSecNum{Generic Instantiation}),
this rule applies also in the private part of an instance of a generic unit.]}
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

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0123-1]}
@Defn2{Term=[equality operator],Sec=(special inheritance rule for tagged types)}
For a type extension, predefined equality
is defined in terms of the primitive @Redundant[(possibly
user-defined)] equals operator
@Chg{Version=[3],New=[for],Old=[of]} the parent type and
@Chg{Version=[3],New=[for],Old=[of]} any @Chg{Version=[3],New=[],Old=[tagged ]}components
@Chg{Version=[3],New=[that have a record type in],Old=[of]} the extension part, and
predefined equality
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

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0123-1]}
@ChgAdded{Version=[3],Text=[For a derived type whose parent is an untagged
record type, predefined equality is defined in terms of the primitive (possibly
user-defined) equals operator of the parent type.]}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This prevents predefined equality from
  reemerging in generic units for untagged record types. For other uses
  the primitive equality is inherited and the inherited routine is primitive.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0123-1]}
For a private type, if its full type is
@Chg{Version=[3],New=[a record type],Old=[tagged]}, predefined
equality is defined in terms of the primitive equals operator of the
full type; @Chg{Version=[3],New=[otherwise],Old=[if the full type is untagged]},
predefined equality for the private type is that of its full type.

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

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0123-1]}
  Otherwise, the result is defined in terms of
  the primitive equals operator for any
  matching @Chg{Version=[3],New=[],Old=[tagged ]}components@Chg{Version=[3],New=[ that
  are records],Old=[]}, and
  the predefined equals for any @Chg{Version=[3],New=[other ],Old=[]}matching
  @Chg{Version=[3],New=[],Old=[untagged ]}components.
  @begin{Reason}
    @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0123-1]}
    This asymmetry between @Chg{Version=[3],New=[],Old=[tagged and
    untagged ]}components @Chg{Version=[3],New=[with and without a record type ],Old=[]}is
    necessary to preserve @Chg{Version=[3],New=[most ],Old=[]}upward compatibility and corresponds
    with the corresponding situation with generics, where the
    predefined operations @lquotes@;reemerge@rquotes@; in a generic for
    @Chg{Version=[3],New=[non-record],Old=[untagged]} types, but do not
    for @Chg{Version=[3],New=[record],Old=[tagged]} types. Also, only
    tagged types support user-defined assignment
    (see @RefSecNum{Assignment and Finalization}),
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

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0123-1]}
  Note that if a composite object has a component
  of a floating point type, and the floating point type
  has both a plus and minus zero, which are considered
  equal by the predefined equality, then a block compare
  cannot be used for the predefined composite equality.
  Of course, with user-defined equals operators for
  @Chg{Version=[3],New=[],Old=[tagged ]}components@Chg{Version=[3],New=[ that
  are records],Old=[]},
  a block compare breaks down anyway, so this is not the only
  special case that requires component-by-component comparisons.
  On a one's complement machine, a similar situation might
  occur for integer types, since one's complement machines
  typically have both a plus and minus (integer) zero.
@end{Ramification}
@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01]}
  @ChgAdded{Version=[2],Text=[For a component with an anonymous access type,
  @lquotes@;predefined equality@rquotes@; is that defined for the
  @i<universal_access> type (anonymous access types have no equality operators
  of their own).]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0123-1]}
  @ChgAdded{Version=[2],Text=[For a component with a @Chg{Version=[3],New=[record],
  Old=[tagged]} type @i{T},
  @lquotes@;the primitive equals operator@rquotes@; is the one with two
  parameters of @i(T) which returns Boolean. We're not talking about some
  random other primitive function named "=".]}
@end{Honest}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0123-1]}
@ChgAdded{Version=[3],Text=[If the primitive equals operator for an untagged
record type is abstract, then Program_Error is raised at the point of any
(implicit) call to that abstract subprogram.]}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[An explicit call to an abstract subprogram is
  illegal. This rule is needed in order to define the effect of an implicit call
  such as a call that is part of the predefined equality operation for an
  enclosing composite type that has a component of an untagged record type that
  has an abstract primitive equals operator. For tagged types, an abstract
  primitive equals operator is only allowed for an abstract type, and abstract
  types cannot be components, so this case does not occur.]}
@end{Reason}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0016],ARef=[AI95-00123-01]}
@ChgAdded{Version=[1],Text=[For any composite type, the order in which "="
is called for components is unspecified. Furthermore, if the result can be
determined before calling "=" on some components, it is unspecified whether
"=" is called on those components.@PDefn{Unspecified}]}

The predefined "/=" operator gives the complementary result
to the predefined "=" operator.
@begin{Ramification}
Furthermore,
  if the user defines an "=" operator that returns Boolean,
  then a "/=" operator is implicitly declared in terms of
  the user-defined "=" operator so as to give the complementary
  result. See @RefSecNum(Overloading of Operators).
@end{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@Defn{lexicographic order}
For a discrete array type, the predefined ordering operators
correspond to @i(lexicographic order) using the predefined order
relation of the component type: A null array is lexicographically
less than any array having at least one component.
In the case of nonnull arrays, the left operand is lexicographically
less than the right operand if the first component of
the left operand is less than that of the right; otherwise@Chg{Version=[3],New=[,],Old=[]}
the left operand is lexicographically less than the right operand
only if their first components are equal and the tail of the
left operand is lexicographically less than that of the right (the
@i(tail) consists of the remaining components beyond the first and
can be null).

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[An @i<individual membership test> is the
membership test of a single @nt{membership_choice}.@Defn{individual membership test}]}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0158-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0039-1]}
@PDefn2{Term=[evaluation], Sec=(membership test)}
For the evaluation of a membership test@Chg{Version=[3],New=[ using @key[in]
whose @nt{membership_choice_list} has a single @nt{membership_choice}],Old=[]},
the
@Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
and the @Chg{Version=[3],New=[@nt{membership_choice}],Old=[@nt<range> (if any)]}
are evaluated in an arbitrary order@Chg{Version=[3],New=[; the result is the
result of the individual membership test for
the @nt{membership_choice}],Old=[]}.@PDefn2{Term=[arbitrary order],Sec=[allowed]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0158-1]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0039-1]}
@ChgAdded{Version=[3],Text=[For the evaluation of a membership test using
@key[in] whose @nt{membership_choice_list} has more than one
@nt{membership_choice}, the
@Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
of the membership test is evaluated first and the
result of the operation is equivalent to that of a sequence consisting
of an individual membership test on each @nt{membership_choice}
combined with the short-circuit control form @b[or else].]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0158-1]}
  @ChgAdded{Version=[3],Text=[This equivalence includes the evaluation of the
  @nt{membership_choice}s; evaluation stops as soon as an individual choice
  evaluates to True.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0158-1],ARef=[AI05-0269-1]}
@Leading@;@Chg{Version=[3],New=[An individual membership test],Old=[A
membership test using @key(in)]} yields the result True if:
@begin(itemize)
  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0158-1],ARef=[AI05-0264-1]}
  @ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0039-1]}
  @ChgAdded{Version=[3],Text=[The @nt{membership_choice} is a
  @Chg{Version=[4],New=[@SynI{choice_}@nt{simple_expression}],Old=[@nt{choice_expression}]},
  and the
  @Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
  is equal to the value of the @nt{membership_choice}.
  If the tested type is a record type or a limited type, the test uses the
  primitive equality for the type; otherwise, the test uses predefined equality.]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0153-3],ARef=[AI05-0158-1]}
  @ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0039-1]}
  @ChgAdded{Version=[3],Text=[The @nt{membership_choice} is a @nt{range}
  and the value of the
  @Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
  belongs to the given @nt<range>.]}

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0153-3],ARef=[AI05-0158-1]}
  @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0039-1],ARef=[AI12-0071-1]}
  @Chg{Version=[3],New=[The @nt{membership_choice} is a @nt{subtype_mark},
  the],Old=[The]} tested type is scalar, @Chg{Version=[3],New=[],Old=[and ]}the
  value of the
  @Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
  belongs to the
  @Chg{Version=[3],New=[],Old=[given @nt<range>, or the ]}range of the
  named subtype@Chg{Version=[3],New=[, and the @Chg{Version=[4],New=[value
  satisfies the predicates],Old=[predicate]} of the named
  subtype@Chg{Version=[4],New=[],Old=[ evaluates to True]}.@Chg{Version=[4],
  New=[@Defn2{Term=[predicates satisfied required],Sec=[membership]}],Old=[@Defn2{Term=[predicate evaluated],Sec=[membership]}]}],Old=[; or]}
@begin{Ramification}
    @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0153-3]}
    The scalar membership test only does a range check@Chg{Version=[3],New=[
    and a predicate check],Old=[]}.
    It does not perform any other check, such as whether
    a value falls in a @lquotes@;hole@rquotes@; of a @lquotes@;holey@rquotes@; enumeration type.
    The Pos attribute function can be used for that purpose.

    Even though Standard.Float is an unconstrained subtype,
    the test @lquotes@;X in Float@rquotes@; will still return False
    (presuming the evaluation of X does not raise Constraint_Error)
    when X is outside Float'Range.
@end{Ramification}

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00231-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0153-3],ARef=[AI05-0158-1]}
  @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0039-1],ARef=[AI12-0071-1]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[]}@ChgNote{To get conditional Leading}
  @Chg{Version=[3],New=[The @nt<membership_choice> is a
  @nt<subtype_mark>, the],Old=[The]} tested type is not scalar, @Chg{Version=[3],New=[],Old=[and ]}
  the value of the
  @Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
  satisfies any constraints
  of the named subtype, @Chg{Version=[3],New=[the @Chg{Version=[4],New=[value
  satisfies the predicates],Old=[predicate]} of the named
  subtype@Chg{Version=[4],New=[],Old=[ evaluates to
  True]}, ],Old=[]}and@Chg{Version=[2],New=[:],Old=[, if the type of
  the @nt{simple_expression} is class-wide, the value has a tag that
  identifies a type covered by the tested type.]}
  @begin{Inneritemize}
    @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00231-01]}
    @ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0039-1]}
    @ChgAdded{Version=[2],Text=[if the type of the
    @Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
    is class-wide, the value has a tag that identifies a type covered by the
    tested type;]}
    @begin{Ramification}
      @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0039-1]}
      Note that the tag is not checked if the
      @Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
      is of a specific type.
    @end{Ramification}
    @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00231-01]}
    @ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0149-1]}
    @ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0039-1]}
    @ChgAdded{Version=[2],Text=[if the tested type is an access type and the
    named subtype excludes null, the value of the
    @Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
    is not null@Chg{Version=[3],New=[;],Old=[.]}]}

    @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0149-1]}
    @ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0039-1]}
    @ChgAdded{Version=[3],Text=[if the tested type is a general access-to-object
    type, the type of the
    @Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
    is convertible to the tested
    type and its accessibility level is no deeper than that of the tested type;
    further, if the designated type of the tested type is tagged and the
    @Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
    is nonnull, the tag of the object designated by the
    value of the
    @Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
    is covered by the designated type of the tested type.]}
  @end{Inneritemize}
@end(itemize)

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
Otherwise@Chg{Version=[3],New=[,],Old=[]} the test yields the result False.

A membership test using @key(not in) gives the complementary result to
the corresponding membership test using @key(in).

@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0158-1]}
  @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0039-1]}
  @ChgAdded{Version=[3],Text=[@exam{@i<X> @key[not in] @i<A> | @i<B> | @i<C>}
  is intended to be exactly equivalent to @exam{@key[not] (@i<X> @key[in] @i<A> | @i<B> | @i<C>)},
  including the order of evaluation of the
  @Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
  and @nt{membership_choice}s.]}
@end{Honest}
@end{RunTime}

@begin{ImplReq}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0016],ARef=[AI95-00123-01]}
@ChgAdded{Version=[1],Text=[For all nonlimited types declared in
language-defined packages, the "=" and "/=" operators of the type shall behave
as if they were the predefined equality operators for the purposes of the
equality of composite types and generic formal types.]}
@begin{Ramification}
@ChgRef{Version=[1],Kind=[Added],ARef=[AI95-00123-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0123-1]}
@ChgAdded{Version=[1],Text=[If any language-defined types are implemented with
a user-defined "=" operator, then either the full type must be @Chg{Version=[3],
New=[a record type],Old=[tagged]}, or the compiler must
use @lquotes@;magic@rquotes@; to implement equality for this type. A normal
user-defined "=" operator for @Chg{Version=[3],New=[a non-record],Old=[an untagged]}
type does @i{not} meet this requirement.]}
@end{Ramification}
@end{ImplReq}

@begin{Notes}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00230-01]}
@ChgDeleted{Version=[2],Text=[No exception is ever raised by a membership test,
by a predefined ordering operator, or by a predefined equality operator for an
elementary type, but an exception can be raised by the evaluation of the
operands. A predefined equality operator for a composite type can only raise an
exception if the type has a tagged part whose primitive equals operator
propagates an exception.]}

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

"" < "A" @key(and) "A" < "Aa"     --@RI[  True]
"Aa" < "B" @key(and) "A" < "A  "  --@RI[  True]

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
My_Car = @key(null)               --@RI[ @Chg{Version=[3],New=[True],Old=[true]} if My_Car has been set to null (see @RefSecNum{Incomplete Type Declarations})]
My_Car = Your_Car           --@RI[ @Chg{Version=[3],New=[True],Old=[true]} if we both share the same car]
My_Car.@key[all] = Your_Car.@key[all]   --@RI[ @Chg{Version=[3],New=[True],Old=[true]} if the two cars are identical]

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0158-1]}
N @key(not) @key(in) 1 .. 10            --@RI[ range membership test]
Today @key(in) Mon .. Fri         --@RI[ range membership test]
Today @key(in) Weekday            --@RI[ subtype membership test (see @RefSecNum{Enumeration Types})]@Chg{Version=[3],New=[
Card @key(in) Clubs | Spades      --@RI[ list membership test (see @RefSecNum{Enumeration Types})]],Old=[]}
Archive @key(in) Disk_Unit        --@RI[ subtype membership test (see @RefSecNum{Variant Parts and Discrete Choices})]
Tree.@key(all) @key(in) Addition'Class  --@RI[ class membership test (see @RefSecNum{Type Extensions})]
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
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01],ARef=[AI95-00420-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}The @i{universal_access}
  equality operators are new. They provide equality operations (most
  importantly, testing against @key{null}) for anonymous access types.]}
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

@begin{Inconsistent2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0123-1]}
  @ChgAdded{Version=[3],Text=[@Defn{inconsistencies with Ada 2005}
  User-defined untagged record equality is now defined to compose and be used in
  generics. Any code which assumes that the predefined equality reemerges
  in generics and in predefined equals for composite types could fail.
  However, it is much more likely that this change will fix bugs, as the
  behavior that would be expected (the user-defined "=" is used) will be
  true in more cases.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0123-1]}
  @ChgAdded{Version=[3],Text=[If a composite type contains
  a component of an untagged record type with an abstract equality operation,
  calling "=" on the composite type will raise Program_Error, while in the
  past a result will be returned using the predefined equality. This is
  quite possible in ASIS programs; it will detect a bug in such programs but
  of course the programs will need to be fixed before they will work.]}
@end{Inconsistent2005}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0123-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}
  Late and hidden overriding of equality for untagged record types is now
  prohibited. This is necessary to make composition of equality predictable.
  It should always be possible to move the overriding to an earlier spot
  where it will be legal.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0149-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}Membership tests
  for valid accessibility levels and tag coverage by the designated type
  for general access types are new.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3]}
  @ChgAdded{Version=[3],Text=[Membership tests now include a predicate check.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0158-1]}
  @ChgAdded{Version=[3],Text=[Membership tests now allow multiple choices.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0020-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Wording was added to clarify
  that @i{universal_access} "=" does not apply if an appropriate operator is
  declared for a partial or incomplete view of the designated type.
  Otherwise, adding a partial or incomplete view could make some "=" operators
  ambiguous.]}
@end{DiffWord2005}

@begin{Inconsistent2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0101-1]}
  @ChgAdded{Version=[4],Text=[@Defn{inconsistencies with Ada 2012}@b{Corrigendum:}
  Removed the incompatible rule preventing the declaration of "=" in the
  private part of a package specification for an untagged record type that
  completes a private type. Any code that calls the
  predefined "=" on the private type will now execute the body for the
  redefined "=" instead for the predefined "=". Eliminating the rule eliminates
  an unnecessary incompatibility (especially for programs that never call the
  predefined "="). Moreover, (like the composition of untagged record "=" in
  Ada 2012) this is more likely to fix bugs than cause
  them (who defines an "=" with a presumably different result and does not want
  clients to us it?).]}
@end{Inconsistent2012}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0039-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Reworded membership tests to use
  the syntax items @SynI{tested_}@nt{simple_expression} and
  @SynI{choice_}@nt{simple_expression}. This was necessary to eliminate
  wording ambiguities introduced when the grammar was corrected to eliminate
  syntax ambiguities. (Both of the above are now @nt{simple_expression}s, so
  merely talking about a @nt{simple_expression} is insufficient.)]}

  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0071-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Updated wording of the
  membership tests to use the new term "satisfies the predicates"
  (see @RefSecNum{Subtype Predicates}).]}
@end{DiffWord2012}



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
@em see @RefSecNum{Assignment and Finalization}.
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
Z + 0.1      --@RI[  Z has to be of a real type ]

"A" & "BCD"  --@RI[  concatenation of two string literals]
'A' & "BCD"  --@RI[  concatenation of a character literal and a string literal]
'A' & 'A'    --@RI[  concatenation of two character literals ]
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

@begin{WideAbove}
@Leading@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0260-1]}
The signed integer modulus operator is defined such
that the result of A @key(mod) B @Chg{Version=[3],New=[is either zero,
or ],Old=[]}has the sign of B and an absolute value less than the
absolute value of B; in addition, for some signed integer value N, this result
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
@end{WideAbove}

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
of an appropriate universal numeric type.] The following additional
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
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00364-01],ARef=[AI95-00420-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The above two fixed-fixed
multiplying operators shall
not be used in a context where the expected type for the result is itself
@i(universal_fixed) @Redundant[@em the context has to identify some other
numeric type to which the result is to be converted, either explicitly or
implicitly]. Unless the predefined universal operator is identified using an
expanded name with @nt{prefix} denoting the package Standard, an explicit
conversion is required on the result when using the above fixed-fixed
multiplication operator if either operand is of a type having a user-defined
primitive multiplication operator such that:]}
@begin{Itemize}
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0020-1],ARef=[AI05-0209-1]}
  @ChgAdded{Version=[2],Text=[it is declared immediately within the same
  declaration list as the type@Chg{Version=[3],New=[ or any partial
  or incomplete view thereof],Old=[]}; and]}

  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[both of its formal parameters are of a
  fixed-point type.]}
@end{Itemize}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00364-01],ARef=[AI95-00420-01]}
@ChgAdded{Version=[2],Text=[A corresponding requirement applies to the
universal fixed-fixed division operator.]}

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
@ChgAdded{Version=[2],Text=[On the other hand, X := A * B; is permitted by
this rule, even if X, A, and B
are all of different fixed point types, since the expected type
for the result of the multiplication is the type of X, which is necessarily
not @i(universal_fixed).]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00364-01],ARef=[AI95-00420-01]}
@ChgAdded{Version=[2],Text=[We have made these into Name Resolution rules to
ensure that user-defined primitive fixed-fixed operators are not made unusable
due to the presence of these universal fixed-fixed operators. But we do allow
these operators to be used if prefixed by package Standard, so that they can be
used in the definitions of user-defined operators.]}
@end(Discussion)
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00364-01]}
@ChgDeleted{Version=[2],Text=[The above two fixed-fixed multiplying operators
shall not be used in a context where the expected type for the result
is itself @i(universal_fixed) @em @Redundant[the context has to
identify some other numeric type to which the result is to be converted,
either explicitly or implicitly].]}
@begin(Discussion)
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[The @i(small) of @i(universal_fixed) is infinitesimal; no loss
of precision is permitted.
However, fixed-fixed division is impractical to implement when
an exact result is required,
and multiplication will sometimes result in unanticipated overflows
in such circumstances,
so we require an explicit conversion to be inserted in
expressions like A * B * C if A, B, and C are each of some fixed point
type.]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[On the other hand, X := A * B; is permitted by this rule, even if X, A, and B
are all of different fixed point types, since the expected type
for the result of the multiplication is the type of X, which is necessarily
not @i(universal_fixed).]}
@end(Discussion)
@end{Legality}
@begin{NotIso}
@ChgAdded{Version=[2],Noparanum=[T],Text=[@Shrink{@i<Paragraph 20 was
deleted.>}]}@Comment{This message should be deleted if the paragraphs
are ever renumbered.}
@end{NotIso}

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
@Leading@Keepnext@;For positive A and B, A/B is the quotient and A @key(rem) B is
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

X : Real := 1.0;                      --@RI[     see @RefSecNum{Floating Point Types}]
Y : Real := 2.0;

F : Fraction := 0.25;                 --@RI[     see @RefSecNum{Fixed Point Types}]
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
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00364-01],ARef=[AI95-00420-01]}
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

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0020-1],ARef=[AI05-0209-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Wording was added to clarify
  that @i{universal_fixed} "*" and "/" does not apply if an appropriate
  operator is declared for a partial (or incomplete) view of the designated type.
  Otherwise, adding a partial (or incomplete) view could make some
  "*" and "/" operators ambiguous.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0260-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> The wording for the @key[mod]
  operator was corrected so that a result of 0 does not have to have
  "the sign of B" (which is impossible if B is negative).]}
@end{DiffWord2005}


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

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0088-1]}
@Defn{exponent}
The right operand of an exponentiation is the @i(exponent).
The @Chg{Version=[3],New=[value of],Old=[expression]} X**N with the value of
the exponent N positive is @Chg{Version=[3],New=[the same as the value of],
Old=[equivalent to the expression]} X*X*...X (with N@en@;1 multiplications)
except that the multiplications
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
  would provide a slightly different (and more potentially accurate) answer in
  Ada 95 than in the same Ada 83 program.]}
@end{Inconsistent83}

@begin{DiffWord83}
We now show the specification for "**" for integer types
with a parameter subtype of Natural rather than Integer for the exponent.
This reflects the fact that Constraint_Error is raised if
a negative value is provided for the exponent.
@end{DiffWord83}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0088-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> The equivalence definition
  for "**" was corrected so that it does not imply that the operands
  are evaluated multiple times.]}
@end{DiffWord2005}


@LabeledAddedSubclause{Version=[3],Name=[Conditional Expressions]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1],ARef=[AI05-0188-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[A @nt{conditional_expression} selects for evaluation
at most one of the enclosed @SynI<dependent_>@nt{expression}s, depending on
a decision among the alternatives. One
kind of @nt{conditional_expression} is the @nt{if_expression}, which selects for
evaluation a @SynI<dependent_>@nt{expression} depending on the value of one or more
corresponding conditions. The other kind of @nt{conditional_expression} is the
@nt{case_expression}, which selects for evaluation one of a number of alternative
@SynI<dependent_>@nt{expression}s; the chosen alternative is determined by the
value of a @SynI<selecting_>@nt{expression}.]}
@end{Intro}

@begin{MetaRules}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0188-1]}
  @ChgAdded{Version=[3],Text=[As previously noted, there are two kinds of
  @nt{conditional_expression}, @nt{if_expression}s and @nt{case_expression}s.
  Whenever possible, we have written the rules in terms of
  @nt{conditional_expression}s to avoid duplication.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1]}
  @ChgAdded{Version=[3],Text=[The rules for @nt{conditional_expression}s
  have been designed as much as possible to work similarly to a parenthesized
  expression. The intent is that as much as possible, wherever a parenthesized
  expression would be allowed, a @nt{conditional_expression} would be allowed,
  and it should work the same way.]}
@end{MetaRules}

@begin{Syntax}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0188-1]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<conditional_expression>,Old=<>}>,
rhs="@Chg{Version=[3],New=<@Syn2{if_expression} | @Syn2{case_expression}>,Old=<>}"}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1],ARef=[AI05-0188-1]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<if_expression>,Old=<>}>,
rhs="
   @Chg{Version=[3],New=<@key[if] @Syn2{condition} @key[then] @SynI{dependent_}@Syn2{expression}
   {@key[elsif] @Syn2{condition} @key[then] @SynI{dependent_}@Syn2{expression}}
   [@key[else] @SynI{dependent_}@Syn2{expression}]>,Old=<>}"}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<condition>,Old=<>}>,
rhs="@Chg{Version=[3],New=<@SynI{boolean_}@Syn2{expression}>,Old=<>}"}
@Comment{Moved from "If Statements"}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0188-1]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<case_expression>,Old=<>}>,
rhs="@Chg{Version=[3],New=<
    @key[case] @SynI{selecting_}@Syn2{expression} @key[is]
    @Syn2[case_expression_alternative] {,
    @Syn2[case_expression_alternative]}>,Old=<>}"}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0188-1]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<case_expression_alternative>,Old=<>}>,
rhs="@Chg{Version=[3],New=[
    @key[when] @Syn2{discrete_choice_list} =>
        @SynI{dependent_}@Syn2{expression}],Old=<>}"}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1]}
@ChgAdded{Version=[3],Text=[Wherever the Syntax Rules allow an @nt{expression},
a @nt{conditional_expression} may be used in place of the @nt{expression}, so
long as it is immediately surrounded by parentheses.]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1]}
  @ChgAdded{Version=[3],Text=[The syntactic category @nt{conditional_expression}
  appears only as a primary that is parenthesized. The above rule allows it to
  additionally be used in other contexts where it would be directly surrounded
  by parentheses.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The grammar makes the following directly legal:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[A := (@key[if] X @key[then] Y @key[else] Z); --@examcom{ parentheses required}
A := B + (@key[if] X @key[then] Y @key[else] Z) + C; --@examcom{ parentheses required}]}
@end{Example}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The following procedure calls are
  syntactically legal; the first uses the above rule to eliminate the redundant
  parentheses found in the second:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[P(@key[if] X @key[then] Y @key[else] Z);
P((@key[if] X @key[then] Y @key[else] Z)); --@Examcom{ redundant parentheses}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[P((@key[if] X @key[then] Y @key[else] Z), Some_Other_Param);
P(Some_Other_Param, (@key[if] X @key[then] Y @key[else] Z));
P(Formal => (@key[if] X @key[then] Y @key[else] Z));]}
@end{Example}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[whereas the following are illegal:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[P(@key[if] X @key[then] Y @key[else] Z, Some_Other_Param);
P(Some_Other_Param, @key[if] X @key[then] Y @key[else] Z);
P(Formal => @key[if] X @key[then] Y @key[else] Z);]}
@end{Example}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[because in these latter cases, the
  @nt{conditional_expression} is not immediately surrounded by parentheses (which
  means on both sides!).]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The English-language rule applies in all places
  that could surround an expression with parentheses, including pragma
  arguments, type conversion and qualified expression operands, and array index
  expressions.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This English-language rule could have been
  implemented instead by adding a nonterminal
  @ntf{expression_within_parentheses}, which would consist of @nt{expression}s
  and @nt{conditional_expression}s. Then, that could be used in all of the
  syntax which could consist of parens directly around an @nt{expression}. We
  did not do that because of the large amount of change required. A complete
  grammar is given in @AILink{AI=[AI05-0147-1],Text=[AI05-0147-1]}.]}
@end{Discussion}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1]}
  @ChgAdded{Version=[3],Text=[Implementers are cautioned to consider error
  detection when implementing the syntax for @nt{conditional_expression}s. An
  @nt{if_expression} and an @nt{if_statement} are very similar syntactically, (as
  are a @nt{case_expression} and a @nt{case_statement}) and simple mistakes can
  appear to change one into the other, potentially causing errors to be moved
  far away from their actual location. The absence of @key[end if] to terminate
  an @nt{if_expression} (and @key[end case] for a @nt{case_expression}) also
  may make error handling harder.]}
@end{ImplNote}

@end{Syntax}

@begin{Resolution}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[If a @nt{conditional_expression} is
expected to be of a type @i<T>, then each @SynI{dependent_}@nt{expression} of
the @nt{conditional_expression} is expected to be of type @i<T>. Similarly, if a
@nt{conditional_expression} is expected to be of some class of types, then each
@SynI{dependent_}@nt{expression} of the @nt{conditional_expression} is subject
to the same expectation. If a @nt{conditional_expression} shall resolve to be of
a type @i<T>, then each @SynI{dependent_}@nt{expression} shall resolve to be of
type @i<T>.@PDefn2{Term=[expected type],Sec=[dependent_expression]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The possible types of a
@nt{conditional_expression} are further determined as follows:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[If the @nt{conditional_expression} is the operand
  of a type conversion, the type of the @nt{conditional_expression} is the
  target type of the conversion; otherwise,]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[This rule distributes an enclosing
  type conversion to the @SynI{dependent_}@nt{expression}s. This means that]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[T(@key[if] C @key[then] A @key[else] B)]}
@end{Example}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[has the same semantics as]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[(@key[if] C @key[then] T(A) @key[else] T(B))]}
@end{Example}
@end{Reason}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[If all of the @SynI{dependent_}@nt{expression}s
  are of the same type, the type of the @nt{conditional_expression} is that
  type; otherwise,]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[If a @SynI{dependent_}@nt{expression} is of an
  elementary type, the type of the @nt{conditional_expression} shall be covered
  by that type; otherwise,]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This rule supports the use of
  numeric literals and universal expressions within a
  @nt{conditional_expression}.]}
@end{Reason}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[If the @nt{conditional_expression} is expected to
  be of type @i<T> or shall resolve to type @i<T>, then the
  @nt{conditional_expression} is of type @i<T>.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[If the type of the @nt{conditional_expression}
  cannot be determined by one of these rules, then Name Resolution has failed for
  that expression, even if the @SynI{dependent_}@nt{expression}s would
  resolve individually.]}
@end{Ramification}
@end{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=[expected type], Sec=(condition)}
A @nt{condition} is expected to be of any boolean type.]}
@Comment{Moved from "If Statements"}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0188-1]}
@ChgAdded{Version=[3],Text=[The expected type for the
@SynI<selecting_>@nt{expression} and the @nt{discrete_choice}s are as for case
statements
(see @RefSecNum{Case Statements}).@PDefn2{Term=[expected type],Sec=[case_expression selecting_expression]}
@PDefn2{Term=[expected type],Sec=[case_expression_alternative discrete_choice]}
@PDefn2{Term=[expected type],Sec=[selecting_expression case_expression]}]}

@end{Resolution}

@begin{Legality}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1],ARef=[AI05-0188-1]}
@ChgAdded{Version=[3],Text=[All of the @SynI{dependent_}@nt{expression}s
shall be convertible (see @RefSecNum{Type Conversions}) to the type of the
@nt{conditional_expression}.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1],ARef=[AI05-0188-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[If the expected type of a
@nt{conditional_expression} is a specific tagged type, all of the
@SynI{dependent_}@nt{expression}s of the @nt{conditional_expression} shall be
dynamically tagged, or none shall be dynamically tagged. In this case, the
@nt{conditional_expression} is dynamically tagged if all of the
@SynI{dependent_}@nt{expression}s are dynamically tagged, is tag-indeterminate
if all of the @SynI{dependent_}@nt{expression}s are tag-indeterminate, and is
statically tagged otherwise.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[If there is no @key[else]
@SynI{dependent_}@nt{expression}, the @nt{if_expression} shall be of
a boolean type.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0188-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[All @LegalityTitle that apply to the
@nt{discrete_choice}s of a @nt{case_statement} (see @RefSecNum{Case Statements})
also apply to the @nt{discrete_choice}s of a @nt{case_expression} except
within an instance of a generic unit.]}
@begin{Reason}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The exemption for a case expression
that occurs in an instance allows the following example:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[generic]
   @key[with function] Int_Func @key[return] Integer;
@key[package] G @key[is]
   X : Float := (@key[case] Int_Func @key[is]
                  @key[when] Integer'First .. -1 => -1.0,
                  @key[when] 0 => 0.0,
                  @key[when] Positive => 1.0);
@key[end] G;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] Nat_Func @key[return] Natural @key[is] (123);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[package] I @key[is new] G (Int_Func => Nat_Func); -- @Examcom{Legal}]}
@end{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Note that the @LegalityTitle still apply in the
generic unit itself; they are just not enforced in an instance of the unit.]}
@end{Reason}
@end{Legality}

@begin{Runtime}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1],ARef=[AI05-0188-1]}
@ChgAdded{Version=[3],Text=[For the evaluation of an @nt{if_expression}, the
@nt{condition} specified after @key[if], and any @nt{condition}s specified
after @key[elsif], are evaluated in succession (treating a final @key[else]
as @key[elsif] True @key[then]), until one evaluates to True or
all @nt{condition}s are evaluated and yield False. If a @nt{condition}
evaluates to True, the associated @synI{dependent_}@nt{expression} is evaluated,
converted to the type of the @nt{if_expression}, and the resulting value is the
value of the @nt{if_expression}. Otherwise (when there is no @key[else] clause),
the value of the @nt{if_expression} is True.@PDefn2{Term=[evaluation],Sec=[if_expression]}]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[@key[Else] is required unless the
  @nt{if_expression} has a boolean type, so the last sentence can only apply to
  @nt{if_expression}s with a boolean type.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0188-1]}
@ChgAdded{Version=[3],Text=[For the evaluation of a @nt{case_expression}, the
@SynI<selecting_>@nt{expression} is first evaluated.
If the value of the @SynI<selecting_>@nt{expression}
is covered by the @nt{discrete_choice_list} of some
@nt{case_expression_alternative}, then the @SynI<dependent_>@nt{expression} of
the @nt{case_expression_alternative} is evaluated, converted to the type of the
@nt{case_expression}, and the resulting value is the value of the
@nt{case_expression}.@IndexCheck{Overflow_Check}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
Otherwise (the value is not covered by any
@nt{discrete_choice_list}, perhaps due to being outside the base range),
Constraint_Error is raised.@PDefn2{Term=[evaluation],Sec=[case_expression]}]}

@end{Runtime}


@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}If expressions
  and case expressions are new.]}
@end{Extend2005}


@LabeledAddedSubclause{Version=[3],Name=[Quantified Expressions]}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0158-1]}
@ChgAdded{Version=[4],Text=[Quantified expressions provide a way to write
universally and existentially quantified predicates over containers and
arrays.]}

@begin{Syntax}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0176-1]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<quantified_expression>,Old=<>}>,
rhs="@Chg{Version=[3],New=[@key[for] @Syn2{quantifier} @Syn2{loop_parameter_specification} => @Syn2{predicate}
  | @key[for] @Syn2{quantifier} @Syn2{iterator_specification} => @Syn2{predicate}],Old=<>}"}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<quantifier>,Old=<>}>,
rhs="@Chg{Version=[3],New=<@key[all] | @key[some]>,Old=<>}"}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<predicate>,Old=<>}>,
rhs="@Chg{Version=[3],New=<@SynI<boolean_>@Syn2{expression}>,Old=<>}"}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0176-1]}
@ChgAdded{Version=[3],Text=[Wherever the Syntax Rules allow an @nt{expression}, a
@nt{quantified_expression} may be used in place of the @nt{expression}, so
long as it is immediately surrounded by parentheses.]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The syntactic category @nt{quantified_expression}
  appears only as a @nt{primary} that is parenthesized. The above rule allows it to additionally
  be used in other contexts where it would be directly surrounded by
  parentheses. This is the same rule that is used for @nt{conditional_expression}s;
  see @RefSecNum{Conditional Expressions} for a detailed discussion of the
  meaning and effects of this rule.]}
@end{Discussion}
@end{Syntax}

@begin{Resolution}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0176-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=[expected type],Sec=[quantified_expression]}
The expected type of a @nt{quantified_expression} is
any Boolean type. The @nt{predicate} in a @nt{quantified_expression} is
expected to be of the same type.@Defn{quantified expressions}]}
@end{Resolution}

@begin{Runtime}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0176-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0158-1]}
@ChgAdded{Version=[3],Text=[For the evaluation of a @nt{quantified_expression},
the @nt{loop_parameter_specification} or @nt{iterator_specification} is first elaborated. The evaluation of a
@nt{quantified_expression} then evaluates the @nt{predicate} for
@Chg{Version=[4],New=[the values],Old=[each value]} of the loop
parameter@Chg{Version=[4],New=[],Old=[. These values are examined]} in the
order specified by the
@nt{loop_parameter_specification} (see @RefSecNum{Loop Statements}) or
@nt{iterator_specification} (see @RefSecNum{Generalized Loop Iteration}).@PDefn2{Term=[evaluation],Sec=[quantified_expression]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0176-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The value of the
@nt{quantified_expression} is determined as follows:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0158-1]}
  @ChgAdded{Version=[3],Text=[If the @nt{quantifier} is @key[all], the
  expression is @Chg{Version=[4],New=[False],Old=[True]} if the evaluation
  of @Chg{Version=[4],New=[any],Old=[the]} @nt{predicate} yields
  @Chg{Version=[4],New=[False; evaluation of the
  @nt{quantified_expression} stops at that point. Otherwise (every
  predicate has been evaluated and yielded True), the expression is
  True],Old=[True for
  each value of the loop parameter. It is False otherwise. Evaluation of
  the @nt{quantified_expression} stops when all values of the domain have been
  examined, or when the @nt{predicate} yields False for a given value]}. Any
  exception raised by evaluation of the @nt{predicate} is propagated.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The @nt{expression} is True if the domain
  contains no values.]}
@end{Ramification}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0158-1]}
  @ChgAdded{Version=[3],Text=[If the @nt{quantifier} is @key[some], the
  expression is True if the evaluation of
  @Chg{Version=[4],New=[any],Old=[the]} @nt{predicate} yields
  True@Chg{Version=[4],New=[; evaluation of the
  @nt{quantified_expression} stops at that point. Otherwise (every
  predicate has been evaluated and yielded False), the expression is
  False],Old=[ for
  some value of the loop parameter. It is False otherwise. Evaluation of
  the @nt{quantified_expression} stops when all values of the domain have been
  examined, or when the @nt{predicate} yields True for a given value]}. Any
  exception raised by evaluation of the @nt{predicate} is propagated.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The @nt{expression} is False if the domain
  contains no values.]}
@end{Ramification}
@end{Itemize}
@end{Runtime}

@begin{Examples}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0176-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The postcondition for a sorting
routine on an array A with an index subtype T can be written:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Post => (A'Length < 2 @key[or else]
   (@key[for all] I @key[in] A'First .. T'Pred(A'Last) => A (I) <= A (T'Succ (I))))]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0176-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The assertion that a positive number
is composite (as opposed to prime) can be written:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[pragma] Assert (@key[for some] X @key[in] 2 .. N / 2 => N @key[mod] X = 0);]}
@end{Example}

@end{Examples}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0176-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}Quantified
  expressions are new.]}
@end{Extend2005}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0158-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Revised the wording to make it
  clear that the semantics is short-circuited, and what the result is when
  there are no values for the loop parameter.]}
@end{DiffWord2012}
