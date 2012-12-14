@Part(03, Root="ada.mss")

@Comment{$Date: 2012/11/28 23:53:02 $}

@Comment{$Source: e:\\cvsroot/ARM/Source/03b.mss,v $}
@Comment{$Revision: 1.101 $}

@LabeledClause{Array Types}

@begin{Intro}
@Defn{array}
@Defn{array type}
An @i(array) object is a composite object consisting of components
which all have the same subtype.
The name for a component of an array uses one or more
index values belonging to specified discrete types.
The value of an array object is a composite
value consisting of the values of the components.
@end{Intro}

@begin{Syntax}
@Syn{lhs=<array_type_definition>,rhs="
   @Syn2{unconstrained_array_definition} | @Syn2{constrained_array_definition}"}


@Syn{lhs=<unconstrained_array_definition>,rhs="
   @key{array}(@Syn2{index_subtype_definition} {, @Syn2{index_subtype_definition}}) @key{of} @Syn2{component_definition}"}

@Syn{lhs=<index_subtype_definition>,rhs="@Syn2{subtype_mark} @key{range} <>"}

@Syn{lhs=<constrained_array_definition>,rhs="
   @key{array} (@Syn2{discrete_subtype_definition} {, @Syn2{discrete_subtype_definition}}) @key{of} @Syn2{component_definition}"}

@Syn{lhs=<discrete_subtype_definition>,rhs="@SynI{discrete_}@Syn2{subtype_indication} | @Syn2{range}"}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01],ARef=[AI95-00406-01]}
@Syn{lhs=<component_definition>,rhs="@Chg{Version=[2],New=<
   >,Old=<>}[@key{aliased}] @Syn2{subtype_indication}@Chg{Version=[2],New=<
 | [@key{aliased}] @Syn2{access_definition}>,Old=<>}"}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected type], Sec=(discrete_subtype_definition range)}
For a @nt<discrete_subtype_definition> that is a @nt<range>, the
@nt<range> shall resolve to be of some specific discrete
type@Redundant[; which
  discrete type shall be determined without using any context other than
  the bounds of the @nt<range> itself (plus the preference
  for @i(root_integer) @em
  see @RefSecNum(The Context of Overload Resolution)).]
@end{Resolution}

@begin{Legality}
@Defn{index subtype}
Each @nt{index_subtype_definition}
or @nt{discrete_subtype_definition} in an @nt{array_type_definition}
defines an @i(index subtype);
@Defn{index type}
its type (the @i(index type)) shall be discrete.
@begin{Discussion}
@Defn2{Term=[index], Sec=(of an array)}
An @i(index) is a discrete quantity used to select along a given
dimension of an array. A component is selected by specifying corresponding
values for each of the indices.
@end{Discussion}

@Defn{component subtype}
The subtype defined by the @nt<subtype_indication> of a
@nt<component_definition> (the @i(component subtype)) shall be
a definite subtype.
@begin{Ramification}

  This applies to all uses of @nt<component_definition>,
  including in @nt<record_type_definition>s and @nt<protected_definition>s.@end{ramification}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00363-01]}
@ChgDeleted{Version=[2],Text=[Within the definition of a
nonlimited composite type
(or a limited composite type that later in its immediate
scope becomes nonlimited @em see @RefSecNum{Private Operations}
and @RefSecNum{Limited Types}),
if a @nt{component_definition} contains the reserved word
@key[aliased] and the type of the component is discriminated,
then the nominal subtype of the component shall be constrained.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgNote{The rule's gone, we might as well clobber all the notes.}
@ChgDeleted{Version=[2],Text=[If we allowed the subtype to be unconstrained,
then the discriminants might change because of
an assignment to the containing (nonlimited) object,
thus causing a potential violation of an access subtype constraint
of an access value designating the aliased component.]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[Note that the rule elsewhere defining all aliased
discriminated objects to be constrained does not help @em that rule prevents
assignments to the component itself from doing any harm, but not assignments to
the containing object.]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Type=[Leading],Text=[We allow this
for components within limited types since assignment to
the enclosing object is not a problem. Furthermore, it is
important to be able to use a default expression for a discriminant
in arrays of limited components, since that is the only way
to give the components different values for their discriminants.
For example:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[@key[protected] @key[type] Counter_Type(Initial_Value : Integer := 1) @key[is]
   @key[procedure] Get_Next(Next_Value : @key[out] Integer);
     --@RI{ Returns the next value on each call, bumping Count}
     --@RI{ before returning.}
@key[private]
   Count : Integer := Initial_Value;
@key[end] Counter_Type;
@key[protected] @key[body] Counter_Type @key[is] ...]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[@key[function] Next_Id(Counter : @key[access] Counter_Type) @key[return] Integer @key[is]
    Result : Integer;
@key[begin]
    Counter.Get_Next(Result);
    @key[return] Result;
@key[end] Next_Id;]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[C : @key[aliased] Counter_Type;
@key[task] @key[type] T(Who_Am_I : Integer := Next_Id(C'Access));
@key[task] @key[body] T @key[is] ...]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[Task_Array : @key[array](1..100) @key[of] @key[aliased] T;
  --@RI{ Array of task elements, each with its own unique ID.}
  --@RI{ We specify "aliased" so we can use Task_Array(I)'Access.}
  --@RI{ This is safe because Task_Array is of a limited type,}
  --@RI{ so there is no way an assignment to it could change}
  --@RI{ the discriminants of one of its components.}]}
@end{Example}
@end{Reason}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[Note that this rule applies to array components
and record components, but not to protected type components (since
they are always limited).]}
@end{Ramification}
@end{Legality}

@begin{StaticSem}
@Defn2{Term=[dimensionality], Sec=(of an array)}
@Defn{one-dimensional array}
@Defn{multi-dimensional array}
An array is characterized by the number of indices
(the @i(dimensionality) of the array), the type and position
of each index, the lower and upper bounds for each index, and
the subtype of the components.
The order of the indices is significant.

A one-dimensional array has a distinct component for each possible index
value. A multidimensional array has a distinct component for each possible
sequence of index values that can be formed by selecting one value for
each index position (in the given order).
The possible values for a given index are all the values between the
lower and upper bounds, inclusive;
@Defn{index range}
this range of values is called the @i(index range).
@Defn2{Term=[bounds], Sec=(of an array)}
The @i(bounds) of an array are the bounds of its index ranges.
@Defn2{Term=[length], Sec=(of a dimension of an array)}
The @i(length) of a dimension of an array is
the number of values of the index range of the dimension (zero for
a null range).
@Defn2{Term=[length], Sec=(of a one-dimensional array)}
The @i(length) of a one-dimensional array is
the length of its only dimension.

An @nt<array_type_definition> defines an array type and its first subtype.
For each object of this array type, the number of indices, the type
and position of each index, and the subtype of the components
are as in the type definition@Redundant[; the values of the lower
and upper bounds for each index belong to
the corresponding index subtype of its type, except for null arrays
(see @RefSecNum(Index Constraints and Discrete Ranges))].

@Defn2{Term=[constrained], Sec=(subtype)}
@Defn2{Term=[unconstrained], Sec=(subtype)}
An @nt{unconstrained_array_definition} defines an array type with
an unconstrained first subtype.
Each @nt{index_@!subtype_@!definition} defines the
corresponding index subtype to be the subtype denoted
by the @nt{subtype_@!mark}.
@redundant[@PDefn2{Term=[box], Sec=(compound delimiter)}
The compound delimiter <> (called a @i(box)) of an
@nt<index_subtype_definition> stands for an undefined range
(different objects of the type need not have the same bounds).]

@Defn2{Term=[constrained], Sec=(subtype)}
@Defn2{Term=[unconstrained], Sec=(subtype)}
A @nt{constrained_array_definition} defines an array type with
a constrained first subtype.
Each @nt{discrete_@!subtype_@!definition}
defines the corresponding index subtype,
as well as the corresponding index range for the
constrained first subtype.
@PDefn2{Term=[constraint], Sec=(of a first array subtype)}
The @i(constraint) of the first subtype consists of the bounds
of the index ranges.
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  Although there is no @Chg{Version=[3],New=[nameable],Old=[namable]}
  unconstrained array subtype in this case, the predefined slicing
  and concatenation operations can operate on and yield
  values that do not necessarily belong to the first array subtype.
  This is also true for Ada 83.
@end{Discussion}

@Leading@;The discrete subtype defined by a @nt{discrete_@!subtype_@!definition} is
either that defined by the @nt{subtype_@!indication}, or a subtype
determined by the @nt{range} as follows:
@begin(itemize)
  If the type of the @nt{range} resolves to @i(root_integer), then
  the @nt{discrete_subtype_definition} defines a subtype of the
  predefined type Integer with bounds given by a conversion to Integer
  of the bounds of the @nt<range>;
  @PDefn2{Term=[implicit subtype conversion],Sec=(bounds of a range)}
  @begin{Reason}
    This ensures that
    indexing over the discrete subtype can be performed with
    regular Integers, rather than only @i(universal_integer)s.
  @end{Reason}
  @begin{Discussion}
    We considered doing this by simply creating
    a @lquotes@;preference@rquotes@; for Integer when resolving the @nt<range>.
    @PDefn{Beaujolais effect}
    However, this can introduce @i(Beaujolais) effects when the
    @nt<simple_expression>s involve calls on functions visible
    due to @key(use) clauses.
  @end{Discussion}

  Otherwise, the @nt{discrete_subtype_definition} defines
  a subtype of the type of the @nt{range}, with the bounds given
  by the @nt<range>.
@end(itemize)

@PDefn2{Term=[nominal subtype], Sec=(of a component)}
The @nt{component_definition} of an @nt<array_type_definition>
defines the nominal subtype of the components.
If the reserved word @key(aliased) appears in the @nt{component_definition},
then each component of the array is aliased
(see @RefSecNum{Access Types}).
@begin(Ramification)
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00363-01]}
  @ChgDeleted{Version=[2],Text=[In this case, the nominal subtype cannot be an
  unconstrained discriminated subtype. See @RefSecNum{Record Types}.]}
@end(Ramification)

@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(array_type_definition)}
The elaboration of an @nt{array_type_definition}
creates the array type and its first subtype,
and consists of the elaboration of any @nt{discrete_@!subtype_@!definition}s
and the @nt{component_@!definition}.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0002],ARef=[AI95-00171-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
@PDefn2{Term=[elaboration], Sec=(discrete_subtype_definition)}
The elaboration of a @nt{discrete_subtype_definition}
@Chg{New=[that does not contain any per-object expressions],Old=[]}
creates the discrete subtype, and consists
of the elaboration of the @nt{subtype_@!indication} or the
evaluation of the @nt{range}.
@Chg{New=[The elaboration of a @nt{discrete_subtype_definition} that contains
one or more per-object expressions is defined in @RefSecNum{Record Types}.],Old=[]}
@PDefn2{Term=[elaboration], Sec=(component_definition)}
The elaboration of a @nt{component_@!definition} in an
@nt{array_@!type_@!definition} consists of the elaboration
of the @nt{subtype_@!indication}@Chg{Version=[2],New=[ or @nt{access_definition}],Old=[]}.
The elaboration of any @nt{discrete_@!subtype_@!definition}s
and the elaboration of the
@nt{component_@!definition} are performed in an arbitrary order.@PDefn2{Term=[arbitrary order],Sec=[allowed]}
@end{RunTime}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0228-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For an array type with a
scalar component type, the following language-defined representation aspect
may be specified with an @nt{aspect_specification} (see
@RefSecNum{Aspect Specifications}):]}
@begin{Description}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Default_Component_Value@\This aspect
shall be specified by a static expression, and that
expression shall be explicit, even if the aspect has a boolean type.
Default_Component_Value shall be specified only on
a @nt{full_type_declaration}.@AspectDefn{Default_Component_Value}]}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The part about requiring an explicit expression is
  to disallow omitting the value for this aspect, which would otherwise be
  allowed by the rules of @RefSecNum{Aspect Specifications}.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is a representation attribute in order to
  disallow specifying it on a derived type that has inherited primitive
  subprograms; that is necessary as the sizes of @key[out] parameters could be
  different whether or not a Default_Value is specified (see
  @RefSecNum{Parameter Associations}).]}
@end{Reason}

@ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Default_Component_Value],
  Text=[@ChgAdded{Version=[3],Text=[Default value for the components of an
    array-of-scalar subtype.]}]}
@end{Description}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0228-1]}
@ChgAdded{Version=[3],Text=[If a derived type with no primitive subprograms
inherits a boolean Default_Component_Value aspect, the aspect may be specified to have any
value for the derived type.]}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This overrides the
  @RefSecNum{Aspect Specifications} rule that says that a boolean aspect
with a value True cannot be changed.]}
@end{Reason}
@end{StaticSem}

@begin{Resolution}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0228-1]}
@ChgAdded{Version=[3],Text=[The expected type for the @nt{expression}
specified for the Default_Component_Value aspect is the component type of the
array type defined by the
@nt{full_type_declaration} on which it
appears.@PDefn2{Term=[expected type],Sec=[@nt{expression} of a Default_Component_Value aspect]}]}
@end{Resolution}

@begin{Notes}
All components of an array have the same subtype. In particular, for an array
of components that are one-dimensional arrays, this means that all components
have the same bounds and hence the same length.

Each elaboration of an @nt<array_type_definition> creates
a distinct array type. A consequence of this is that each
object whose @nt<object_declaration> contains an @nt<array_type_definition>
is of its own unique type.
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of type declarations with unconstrained array definitions: )
@begin(Example)
@key(type) Vector     @key(is) @key(array)(Integer  @key(range) <>) @key(of) Real;
@key(type) Matrix     @key(is) @key(array)(Integer  @key(range) <>, Integer @key(range) <>) @key(of) Real;
@key(type) Bit_Vector @key(is) @key(array)(Integer  @key(range) <>) @key(of) Boolean;
@key(type) Roman      @key(is) @key(array)(Positive @key(range) <>) @key(of) Roman_Digit; --@RI[ see @RefSecNum(Character Types)]
@end(Example)

@begin{WideAbove}
@leading@keepnext@i(Examples of type declarations with constrained array definitions: )
@end{WideAbove}
@begin(Example)
@key(type) Table    @key(is) @key(array)(1 .. 10) @key(of) Integer;
@key(type) Schedule @key(is) @key(array)(Day) @key(of) Boolean;
@key(type) Line     @key(is) @key(array)(1 .. Max_Line_Size) @key(of) Character;
@end(Example)

@begin{WideAbove}
@leading@keepnext@i(Examples of object declarations with array type definitions: )
@end{WideAbove}
@begin(Example)
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
Grid @Chg{Version=[2],New=[     ],Old=[]}: @key(array)(1 .. 80, 1 .. 100) @key(of) Boolean;
Mix  @Chg{Version=[2],New=[     ],Old=[]}: @key(array)(Color @key(range) Red .. Green) @key(of) Boolean;@Chg{Version=[2],New=[
Msg_Table : @key(constant array)(Error_Code) @key(of access constant) String :=
      (Too_Big => @key(new) String'("Result too big"), Too_Small => ...);],Old=[]}
Page @Chg{Version=[2],New=[     ],Old=[]}: @key(array)(Positive @key(range) <>) @key(of) Line :=  --@RI[  an array of arrays]
  (1 | 50  => Line'(1 | Line'Last => '+', @key(others) => '-'),  --@RI[ see @RefSecNum(Array Aggregates)]
   2 .. 49 => Line'(1 | Line'Last => '|', @key(others) => ' '));
    --@RI[ Page is constrained by its initial value to (1..50)]
@end(Example)
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
The syntax rule for @nt{component_definition} is modified to allow
the reserved word @key{aliased}.

The syntax rules for @nt{unconstrained_array_definition} and
@nt{constrained_array_definition} are modified to use
@nt{component_definition} (instead of
@i(component_)@nt{subtype_indication}). The effect of this change is to allow
the reserved word @key{aliased} before the
component @nt{subtype_indication}.

A @nt{range} in a @nt{discrete_subtype_definition}
may use arbitrary
universal expressions for each bound (e.g. @en@;1 .. 3+5), rather
than strictly "implicitly convertible" operands. The subtype
defined will still be a subtype of Integer.
@end{Extend83}

@begin{DiffWord83}
We introduce a new syntactic category, @nt{discrete_subtype_definition},
as distinct from @nt{discrete_range}. These two constructs have
the same syntax, but their semantics
are quite different (one defines a subtype, with
a preference for Integer subtypes, while the
other just selects a subrange of an existing subtype).
We use this new syntactic category in @key(for) loops
and entry families.

The syntax for @nt{index_constraint} and @nt{discrete_range}
have been moved to their own subclause, since they are no
longer used here.

The syntax rule for @nt{component_definition} (formerly
@ntf<component_subtype_definition>) is moved here from
RM83-3.7.
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01],ARef=[AI95-00406-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Array components can have an anonymous access type.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00363-01]}
  @ChgAdded{Version=[2],Text=[The prohibition against unconstrained
  discriminated aliased components has been lifted. It has been replaced
  by a prohibition against the actual troublemakers: general access
  discriminant constraints (see @RefSecNum{Discriminant Constraints}).]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0002],ARef=[AI95-00171-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Added wording to allow
  the elaboration of per-object constraints for constrained arrays.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0228-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  The new aspect Default_Component_Value allows defining implicit initial
  values (see @RefSecNum{Object Declarations}) for arrays of scalar types.]}
@end{Extend2005}


@LabeledSubClause{Index Constraints and Discrete Ranges}

@begin{Intro}
An @nt<index_constraint> determines the range of possible values for every
index of an array subtype, and thereby the corresponding array bounds.
@end{Intro}

@begin{Syntax}
@Syn{lhs=<index_constraint>,rhs=" (@Syn2{discrete_range} {, @Syn2{discrete_range}})"}

@Syn{lhs=<discrete_range>,rhs="@SynI{discrete_}@Syn2{subtype_indication} | @Syn2{range}"}
@end{Syntax}

@begin{Resolution}
@Defn{type of a @nt{discrete_range}}
The type of a @nt<discrete_range> is the
type of the subtype defined by the @nt<subtype_indication>,
or the type of the @nt<range>.
@PDefn2{Term=[expected type], Sec=(index_constraint discrete_range)}
For an @nt{index_constraint},
each @nt{discrete_range} shall resolve to be of the type of the
corresponding index.
@begin(Discussion)
  In Ada 95, @nt{index_constraint}s only appear in
  a @nt{subtype_indication}; they no longer appear in
  @nt<constrained_array_definition>s.
@end(Discussion)

@end{Resolution}

@begin{Legality}
An @nt{index_constraint} shall appear only in a
@nt{subtype_indication} whose @nt{subtype_mark} denotes
either an unconstrained array subtype, or an
unconstrained access subtype whose designated subtype is
an unconstrained array subtype; in either case,
the @nt{index_constraint} shall provide a @nt{discrete_range}
for each index of the array type.
@end{Legality}

@begin{StaticSem}
@Defn2{Term=[bounds], Sec=(of a @nt<discrete_range>)}
A @nt{discrete_range} defines a range whose bounds are given
by the @nt{range}, or by the range of the subtype defined by the
@nt{subtype_indication}.
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[compatibility], Sec=(index constraint with a subtype)}
An @nt{index_constraint} is @i(compatible) with an unconstrained array subtype
if and only if the index range defined by each @nt{discrete_range}
is compatible (see @RefSecNum(Scalar Types)) with the corresponding index subtype.
@Defn{null array}
If any of the @nt<discrete_range>s defines a null range, any array thus
constrained is a @i(null array), having no components.
@PDefn2{Term=[satisfies], Sec=(an index constraint)}
An array value @i(satisfies) an @nt{index_constraint} if at each index position
the array value and the @nt{index_constraint} have the same index
bounds.
@begin{Ramification}

  There is no need to define compatibility with a constrained
  array subtype, because one is not allowed to constrain it again.@end{ramification}

@PDefn2{Term=[elaboration], Sec=(index_constraint)}
The elaboration of an @nt{index_constraint} consists
of the evaluation of the @nt{discrete_range}(s), in an arbitrary order.
@PDefn2{Term=[evaluation], Sec=(discrete_range)}
The evaluation of a @nt{discrete_range}
consists of the elaboration of the @nt{subtype_indication}
or the evaluation of the @nt{range}.
@end{RunTime}

@begin{Notes}
The elaboration of a @nt<subtype_indication> consisting
of a @nt<subtype_mark> followed
by an @nt<index_constraint> checks the compatibility of the
@nt<index_constraint> with the @nt<subtype_mark>
(see @RefSecNum(Subtype Declarations)).

Even if an array value does not satisfy the index constraint
of an array subtype, Constraint_Error is not
raised on conversion to the array subtype, so long as
the length of each dimension of the array value and the
array subtype match. See @RefSecNum(Type Conversions).
@end{Notes}

@begin{Examples}
@leading@keepnext@i(Examples of array declarations including an index constraint: )
@begin(Example)
Board     : Matrix(1 .. 8,  1 .. 8);  --@RI[  see @RefSecNum(Array Types)]
Rectangle : Matrix(1 .. 20, 1 .. 30);
Inverse   : Matrix(1 .. N,  1 .. N);  --@RI[  N need not be static ]

Filter    : Bit_Vector(0 .. 31);
@end(Example)

@begin{WideAbove}
@leading@keepnext@i(Example of array declaration with a constrained array subtype: )
@end{WideAbove}
@begin(Example)
My_Schedule : Schedule;  --@RI[  all arrays of type Schedule have the same bounds]
@end(Example)

@begin{WideAbove}
@leading@keepnext@i(Example of record type with a component that is an array: )
@end{WideAbove}
@begin(Example)
@key(type) Var_Line(Length : Natural) @key(is)
   @key(record)
      Image : String(1 .. Length);
   @key(end) @key(record);

Null_Line : Var_Line(0);  --@RI[  Null_Line.Image is a null array]
@end(Example)
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
We allow the declaration of a variable with
a nominally unconstrained array subtype, so long as
it has an initialization expression to determine
its bounds.
@end{Extend83}

@begin{DiffWord83}
We have moved the syntax for @nt{index_constraint} and
@nt{discrete_range} here since they are no longer
used in @nt{constrained_array_definition}s.
We therefore also no longer have to describe the
(special) semantics of @nt{index_constraint}s and @nt{discrete_range}s
that appear in @nt{constrained_array_definition}s.

The rules given in RM83-3.6.1(5,7-10),
which define the bounds of an array object, are redundant
with rules given elsewhere, and so are not repeated here.
RM83-3.6.1(6), which requires that the (nominal) subtype of
an array variable be constrained, no longer applies,
so long as the variable is explicitly initialized.
@end{DiffWord83}

@LabeledSubClause{Operations of Array Types}

@begin{Legality}
@Redundant[The argument N used in the @nt<attribute_designator>s for
the N-th dimension of an array shall be a static @nt<expression> of
some integer type.] The value of N shall be positive (nonzero)
and no greater than the dimensionality of the array.
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0006],ARef=[AI95-00030-01]}
The following attributes are defined for
@ChgPrefixType{Version=[1],Kind=[Revised],Text=[a @Chg{New=[@nt{prefix}],
Old=[prefix]} A that is of an array type
@Redundant[(after any implicit dereference)], or denotes
a constrained array subtype]}:
@begin{Ramification}
These attributes are not defined if A is a subtype-mark
  for an access-to-array subtype. They are defined (by implicit
  dereference) for access-to-array values.@end{ramification}
@begin(description)
@Attribute{Prefix=<A>, AttrName=<First>,
  Text=[A'First denotes the lower bound of the first index range; its
  type is the corresponding index type.]}

@Attribute{Prefix=<A>, AttrName=<First(N)>,
  Text=[A'First(N) denotes the lower bound of the N-th index range; its
  type is the corresponding index type.]}

@Attribute{Prefix=<A>, AttrName=<Last>,
  Text=[A'Last denotes the upper bound of the first index range; its
  type is the corresponding index type.]}

@Attribute{Prefix=<A>, AttrName=<Last(N)>,
  Text=[A'Last(N) denotes the upper bound of the N-th index range; its
  type is the corresponding index type.]}

@Attribute{Prefix=<A>, AttrName=<Range>,
  Text=[A'Range is equivalent to the range A'First .. A'Last,
  except that the @nt<prefix> A is only evaluated once.]}

@Attribute{Prefix=<A>, AttrName=<Range(N)>,
  Text=[A'Range(N) is equivalent to the range A'First(N) .. A'Last(N),
  except that the @nt<prefix> A is only evaluated once.]}

@Attribute{Prefix=<A>, AttrName=<Length>,
  Text=[A'Length denotes the number of values of the first index
  range (zero for a null range); its type is @i(universal_integer).]}

@Attribute{Prefix=<A>, AttrName=<Length(N)>,
  Text=[A'Length(N) denotes the number of values of the N-th index
  range (zero for a null range); its type is @i(universal_integer).]}
@end(description)
@EndPrefixType{}
@end{StaticSem}

@begin{ImplAdvice}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
An implementation should normally represent
multidimensional arrays in row-major order, consistent with the notation used
for multidimensional array aggregates (see @RefSecNum(Array Aggregates)).
However, if @Chg{Version=[3],New=[convention ],Old=[a @key<pragma>
Convention(]}Fortran@Chg{Version=[3],New=[ is specified
for],Old=[, ...) applies to]} a
multidimensional array type, then column-major order should be used
instead (see @RefSec{Interfacing with Fortran}).
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Multidimensional arrays should be represented in row-major order,
unless the array has convention Fortran.]}]}
@end{ImplAdvice}

@begin{Notes}
@Leading@;The @nt<attribute_reference>s A'First and A'First(1) denote the same value.
A similar relation exists for the @nt<attribute_reference>s A'Last,
A'Range, and A'Length. The following relation is satisfied (except
for a null array) by the above attributes if the index type is an
integer type:
@begin(example)
   A'Length(N) = A'Last(N) - A'First(N) + 1
@end(example)

An array type is limited if its component type is limited
(see @RefSecNum(Limited Types)).

@PDefn2{Term=[predefined operations],Sec=(of an array type)}
The predefined operations of an array type include the
membership tests, qualification, and explicit conversion.
If the array type is not limited, they also include assignment
and the predefined
equality operators. For a one-dimensional array type,
they include the predefined concatenation operators (if nonlimited) and,
if the component type is discrete, the predefined relational operators;
if the component type is boolean, the predefined
logical operators are also included.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
A component of an array can be named with an @nt<indexed_component>.
A value of an array type can be specified with an
@nt<array_aggregate>@Chg{Version=[2],New=[],Old=[, unless the array type
is limited]}.
For a one-dimensional array type, a slice of the array can be named;
also, string literals are defined if the component type is
a character type.
@end{Notes}

@begin{Examples}
@Leading@keepnext@i{Examples (using arrays declared in the examples of subclause @RefSecNum(Index Constraints and Discrete Ranges)):}
@begin(Example)
--  Filter'First      =   0   Filter'Last       =  31   Filter'Length =  32
--  Rectangle'Last(1) =  20   Rectangle'Last(2) =  30
@end(Example)

@end{Examples}

@LabeledSubClause{String Types}

@begin{StaticSem}
@Defn{string type}
A one-dimensional array type whose component type is a character type
is called a @i(string) type.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@Leading@redundant[There are @Chg{Version=[2],New=[three],Old=[two]} predefined
string types, String@Chg{Version=[2],New=[,],Old=[ and]}
Wide_String@Chg{Version=[2],New=[, and Wide_Wide_String],Old=[]},
each indexed by values of the predefined subtype Positive;
these are declared in the visible part of package Standard:]
@begin(example)
@redundant[@key(subtype) Positive @key(is) Integer @key(range) 1 .. Integer'Last;

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@key(type) String @key(is) @key(array)(Positive @key(range) <>) @key(of) Character;
@key(type) Wide_String @key(is) @key(array)(Positive @key(range) <>) @key(of) Wide_Character;
@Chg{Version=[2],New=[@key(type) Wide_Wide_String @key(is) @key(array)(Positive @key(range) <>) @key(of) Wide_Wide_Character;],Old=[]}
]@end(example)
@end{StaticSem}

@begin{Notes}
String literals (see @RefSecNum(String Literals) and
@RefSecNum(Literals)) are defined for all string types.
The concatenation operator & is predefined
for string types, as for all nonlimited
one-dimensional array types.
The ordering operators <, <=, >, and >= are predefined
for string types, as for all one-dimensional discrete array types;
these ordering operators correspond to lexicographic order
(see @RefSecNum(Relational Operators and Membership Tests)).

@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of string objects:)
@begin(Example)
@TabClear()@TabSet(P50)
Stars      : String(1 .. 120) := (1 .. 120 => '*' );
Question   : @key(constant) String  := "How many characters?";
@\--@RI[ Question'First = 1, Question'Last = 20]
@\--@RI[ Question'Length = 20 (the number of characters)]

Ask_Twice  : String  := Question & Question;@\--@RI[ constrained to (1..40)]
Ninety_Six : @key(constant) Roman   := "XCVI";@\--@RI[ see @RefSecNum(Character Types) and @RefSecNum(Array Types)]
@end(Example)
@end{Examples}

@begin{Inconsistent83}
@Defn{inconsistencies with Ada 83}
The declaration of Wide_String in Standard hides a use-visible
declaration with the same @nt<defining_identifier>.
In rare cases, this might result in an inconsistency between
Ada 83 and Ada 95.
@end{Inconsistent83}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
Because both String and Wide_String are always directly visible,
an expression like
@begin(Example)
"a" < "bc"
@end(Example)

is now ambiguous, whereas in Ada 83 both string literals could
be resolved to type String.
@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
The type Wide_String is new (though it was approved by
ARG for Ada 83 compilers as well).
@end{Extend83}

@begin{DiffWord83}
We define the term @i(string type) as a natural analogy
to the term @i(character type).
@end{DiffWord83}

@begin{Inconsistent95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[@Defn{inconsistencies with Ada 95}
The declaration of Wide_Wide_String in Standard hides a use-visible
declaration with the same @nt<defining_identifier>.
In the (very) unlikely event that an Ada 95 program had
depended on such a use-visible declaration, and the program remains
legal after the substitution of Standard.Wide_Wide_String,
the meaning of the program will be different.]}
@end{Inconsistent95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The type Wide_Wide_String is new.]}
@end{Extend95}

@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}

@LabeledClause{Discriminants}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01],ARef=[AI95-00326-01]}
@redundant[@Defn{discriminant}
@IndexSee{Term=[type parameter],See=(discriminant)}
@IndexSeeAlso{Term=[parameter],See=(discriminant)}
A composite type (other than an array@Chg{Version=[2],New=[ or
interface],Old=[]} type) can have discriminants,
which parameterize the type.
A @nt<known_discriminant_part> specifies the discriminants
of a composite type.
A discriminant of an object is a component of the object,
and is either of a discrete type or an access type.
An @nt<unknown_discriminant_part> in the declaration of
a@Chg{Version=[2],New=[],Old=[ partial]} view of a type
specifies that the discriminants of the type are unknown
for the given view;
all subtypes of such a@Chg{Version=[2],New=[],Old=[ partial]} view
are indefinite subtypes.]
@ChgRef{Version=[2],Kind=[Revised]}
@ChgToGlossary{Version=[2],Kind=[Revised],Term=<Discriminant>,
  Text=<A discriminant is a parameter @Chg{Version=[2],New=[for],Old=[of]} a composite type.
  It can control, for example, the bounds of a component
  of the type if @Chg{Version=[2],New=[the component is],Old=[that type is]}
  an array@Chg{Version=[2],New=[],Old=[ type]}.
  A discriminant @Chg{Version=[2],New=[for],Old=[of]} a task type can be
  used to pass data to a task of the type upon creation.>}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
  @PDefn{unknown discriminants}
  @PDefn2{Term=[discriminants], Sec=(unknown)}
  A @Chg{Version=[2],New=[view of a ],Old=[]}type, and all @Chg{Version=[2],New=[],
  Old=[of its ]}subtypes@Chg{Version=[2],New=[ of the view],Old=[]}, have
  @i(unknown discriminants)
  when the number or names of the discriminants, if any, are unknown at
  the point of the type declaration@Chg{Version=[2],New=[ for the view],Old=[]}.
  A @nt<discriminant_part> of (<>) is used to indicate unknown discriminants.
@end{Discussion}
@end{Intro}

@begin{Metarules}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00402-01]}
  @ChgAdded{Version=[2],Text=[When an access discriminant
  is initialized at the time of object creation with an allocator
  of an anonymous type, the allocated object and the object
  with the discriminant are tied together for their lifetime.
  They should be allocated out of the same storage pool,
  and then at the end of the lifetime of the enclosing object, finalized
  and reclaimed together. In this case, the allocated object is called
  a coextension (see @RefSecNum{Operations of Access Types}).]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The above principle when applied to a
  nonlimited type implies that such an object may be copied only to a
  shorter-lived object, because attempting to assign it to a longer-lived
  object would fail because the access discriminants would not match.
  In a copy, the lifetime connection between the enclosing object
  and the allocated object does not exist. The allocated object is
  tied in the above sense only to the original object. Other copies
  have only secondary references to it.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Note that when an @nt{allocator} appears as a
  constraint on an access discriminant in a @nt{subtype_indication}
  that is elaborated independently from object creation,
  no such connection exists. For example, if a named constrained
  subtype is declared via
  "@key{subtype} Constr @key{is} Rec(Acc_Discrim => @key{new} T);"
  or if such an @nt{allocator} appears in the @nt{subtype_indication} for
  a component, the allocator is evaluated when the @nt{subtype_indication}
  is elaborated, and hence its lifetime is typically longer than
  the objects or components that will later be subject to the
  constraint. In these cases, the allocated object should not
  be reclaimed until the @nt{subtype_indication} goes out of scope.]}
@end{Discussion}
@end{Metarules}

@begin{Syntax}
@Syn{lhs=<discriminant_part>,rhs="@Syn2{unknown_discriminant_part} | @Syn2{known_discriminant_part}"}

@Syn{lhs=<unknown_discriminant_part>,rhs="(<>)"}

@Syn{lhs=<known_discriminant_part>,rhs="
   (@Syn2{discriminant_specification} {; @Syn2{discriminant_specification}})"}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00231-01]}
@Syn{lhs=<discriminant_specification>,rhs="
   @Syn2{defining_identifier_list} : @Chg{Version=[2],New=<[@Syn2{null_exclusion}] >,Old=<>}@Syn2{subtype_mark} [:= @Syn2{default_expression}]
 | @Syn2{defining_identifier_list} : @Syn2{access_definition} [:= @Syn2{default_expression}]"}

@Syn{lhs=<default_expression>,rhs="@Syn2{expression}"}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected type], Sec=(discriminant default_expression)}
The expected type for the @nt{default_expression}
of a @nt{discriminant_specification} is that of the corresponding
discriminant.
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0007],ARef=[AI95-00098-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01]}
A @Chg{New=[@nt{discriminant_part}],Old=[@nt{known_discriminant_part}]} is only
permitted in a declaration for a composite type that is not an
array@Chg{Version=[2],New=[ or interface],Old=[]} type
@Redundant[(this includes generic formal types)]@Chg{New=[. A],Old=[; a]}
type declared with a @nt<known_discriminant_part> is called
a @i(discriminated) type,@Defn{discriminated type} as is a type that inherits
(known) discriminants.
@begin{ImplNote}
  Discriminants on array types were considered,
  but were omitted to ease (existing) implementations.
@end{implnote}
@begin(Discussion)
  Note that the above definition for @lquotes@;discriminated type@rquotes@; does
  not include types declared with an @nt<unknown_discriminant_part>.
  This seems consistent with Ada 83, where such types (in a generic
  formal part) would not be considered discriminated types.
  Furthermore, the full type for a type with unknown discriminants
  need not even be composite, much less have any discriminants.

  @ChgRef{Version=[1],Kind=[Added],Ref=[8652/0007],ARef=[AI95-00098-01]}
  @Chg{New=[On the other hand, @nt<unknown_discriminant_part>s cannot be
  applied to type declarations that cannot have a @nt<known_discriminant_part>.
  There is no point in having unknown discriminants on a type that can never
  have discriminants (for instance, a formal modular type), even when these
  are allowed syntactically.],Old=[]}
@end(Discussion)

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00231-01],ARef=[AI95-00254-01]}
The subtype of a discriminant may be defined
by@Chg{Version=[2],New=[ an optional @nt{null_exclusion} and],Old=[]} a
@nt<subtype_mark>, in which case the @nt<subtype_mark> shall denote
a discrete or access subtype, or it may be defined by an
@nt<access_definition>@Chg{Version=[2],New=[],Old=[ @Redundant[(in which case
the @nt<subtype_mark> of the @nt<access_definition> may denote
any kind of subtype)]]}.
@Defn{access discriminant}
A discriminant that is defined by an @nt<access_definition>
is called an @i(access discriminant)
and is of an anonymous @Chg{Version=[2],New=[access],
Old=[general access-to-variable]} type@Chg{Version=[2],New=[],Old=[ whose
designated subtype is
denoted by the @nt<subtype_mark> of the @nt<access_definition>]}.
@begin(Reason)
  @ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00230-01]}
  @ChgNote{It still does cause complexities. :-)}@ChgNote{We don't
  want to delete "Reason:" here, thus we use @Chg and not @ChgDeleted}
  @Chg{Version=[2],New=[],Old=[In an early version of Ada 9X,
  we allowed access discriminants on nonlimited types,
  but this created unpleasant complexities.
  It turned out to be simpler and more uniform to allow discriminants
  of a named access type on any discriminated type, and keep access
  discriminants just for limited types.]}

  Note that discriminants of a named access type are not
  considered @lquotes@;access discriminants.@rquotes@;
  Similarly, @lquotes@;access parameter@rquotes@;
  only refers to a formal parameter defined by an @nt<access_definition>.
@end(Reason)

@ChgNote{This paragraph is just moved up}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00402-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0214-1]}
@ChgAdded{Version=[2],Text=[@nt{Default_expression}s shall be provided either
for all or for none
of the discriminants of a @nt{known_@!discriminant_@!part}.
No @nt<default_@!expression>s are permitted in a
@nt<known_@!discriminant_@!part> in a declaration of a
@Chg{Version=[3],New=[nonlimited ],Old=[]}tagged
type @Redundant[or a generic formal type].]}

@begin(Reason)
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The all-or-none rule
  is related to the rule that a discriminant constraint shall specify
  values for all discriminants. One could imagine a different rule
  that allowed a constraint to specify only some of the discriminants,
  with the others provided by default. Having defaults for discriminants
  has a special significance @em it allows objects of the type to
  be unconstrained, with the discriminants alterable as part of
  assigning to the object.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0214-1]}
  @ChgAdded{Version=[2],Text=[Defaults for discriminants of tagged types
  are disallowed so that every object of a
  @Chg{Version=[3],New=[nonlimited ],Old=[]}tagged type is constrained,
  either by an explicit constraint,
  or by its initial discriminant values.
  This substantially simplifies the semantic rules
  and the implementation of inherited
  dispatching operations. @Chg{Version=[3],New=[We don't need this rule for
  limited tagged types, as the discriminants of such objects cannot be changed
  after the object is created in any case @em no full-object assignment is
  supported, and that is required to change discriminant values. ],Old=[]}For
  generic formal types, the restriction simplifies the type matching rules.
  If one simply wants a "default" value for the discriminants,
  a constrained subtype can be declared for future use.]}
@end(Reason)

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01],ARef=[AI95-00402-01],ARef=[AI95-00419-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0063-1]}
A @nt<discriminant_specification> for
an access discriminant
@Chg{Version=[2],New=[may have a @nt{default_expression}],Old=[shall appear]}
only in the declaration for @Chg{Version=[3],New=[an immutably limited type
(see @RefSecNum{Limited Types})],Old=[a task or protected type,
or for a type @Chg{Version=[2],New=[that is a descendant of an explicitly
limited record type],Old=[with the reserved word @key[limited] in its
@Redundant[(full)] definition
or in that of one of its ancestors]}]}.
In addition to the places where @LegalityTitle normally apply
(see @RefSecNum{Generic Instantiation}),
this rule applies also in the private part of an
instance of a generic unit.@PDefn{generic contract issue}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[Revised]}
  @ChgRef{Version=[3],Kind=[Revised]}
  This rule implies that a type can have @Chg{Version=[2],
  New=[a default for ],Old=[]}an access discriminant
  if the type is limited,
  but not if the only reason it's limited is because of a limited component.
  Compare @Chg{Version=[3],New=[],Old=[with ]}the definition of limited type
  @Chg{Version=[3],New=[and immutably limited type ],Old=[]}in
  @RefSecNum{Limited Types}.@Chg{Version=[3],New=[],Old=[@Chg{Version=[2],New=[ Also,
  recall that a @ldquote@;descendant@rdquote includes the type itself, so
  an explicitly limited record type can have defaults.],Old=[]}]}
@end{Discussion}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[Revised]}
  @ChgRef{Version=[3],Kind=[Revised]}
  @ChgDeleted{Version=[2],Text=[It is a consequence
  of this rule that only a return-by-reference
  type can have an access discriminant (see @RefSecNum{Return Statements}).
  This is important to avoid dangling references to local variables.]}
  @ChgAdded{Version=[3],Text=[A (nonformal) limited private type can always
  have a default for an access discriminant, because having the default itself
  makes the type immutably limited. Such a private type must necessarily
  have a full type with the same access discriminant with a default, and
  thus the full type will always be immutably limited (if legal).]}
@end{Ramification}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
  @Leading@Keepnext@;We @Chg{Version=[2],New=[],Old=[also ]}considered the
  following rules@Chg{Version=[2],New=[ for access discriminants],Old=[]}:
  @begin{Itemize}
    If a type has an access discriminant,
    this automatically makes it limited,
    just like having a limited component automatically
    makes a type limited.
    This was rejected because it decreases program readability,
    and because it seemed error prone (two bugs in a previous
    version of the RM9X were attributable to this rule).

    @ChgRef{Version=[2],Kind=[Revised]}
    A type with an access discriminant shall be limited.
    This is equivalent to the rule we actually chose@Chg{Version=[2],New=[ for Ada 95],Old=[]},
    except that it allows a type to have an access discriminant
    if it is limited just because of a limited component.
    For example, any record containing a task would be allowed to have
    an access discriminant, whereas the actual rule requires
    @lquotes@key[limited] @key[record]@rquotes@;.
    This rule was also rejected due to readability concerns,
    and because would interact badly with the rules for
    limited types that @lquotes@;become nonlimited@rquotes@;.

    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0063-1]}
    @ChgAdded{Version=[2],Text=[A type may have an access discriminant if
    it is @Chg{Version=[3],New=[an immutably limited],Old=[a limited partial
    view, or a task, protected, or explicitly limited record]} type. This
    was the rule chosen for Ada 95.]}

    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[Any type may have an access discriminant.
    For nonlimited type, there is no special accessibility for access
    discriminants; they're the same as any other anonymous access component.
    For a limited type, they have the special accessibility of Ada 95. However,
    this doesn't work because a limited partial view can have a nonlimited
    full view -- giving the two views different accessibility.]}

    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0063-1]}
    @ChgAdded{Version=[2],Text=[Any type may have an access discriminant,
    as above. However, special accessibility rules only apply to types
    that are @Chg{Version=[3],New=[immutably],Old=[@lquotes@;really@rquotes@;]}
    limited (task, protected, and
    explicitly limited records). However, this breaks privacy; worse,
    @LegalityTitle depend on the definition of accessibility.]}

    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0063-1]}
    @ChgAdded{Version=[2],Text=[Any type may have an access discriminant,
    as above. Limited types have special accessibility, while nonlimited
    types have normal accessibility. However, a limited partial view with an
    access discriminant can only be completed by @Chg{Version=[3],New=[an
    immutably limited],Old=[a task, protected, or
    explicitly limited record]} type. That prevents accessibility from changing.
    A runtime accessibility check is required on generic formal types with
    access discriminants. However, changing between limited and nonlimited
    types would have far-reaching consequences for access discriminants @em
    which is uncomfortable.]}

    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[Any type may have an access discriminant.
    All types have special accessibility. This was considered early during
    the Ada 9X process, but was dropped for @lquotes@;unpleasant
    complexities@rquotes@;, which unfortunately aren't recorded. It does
    seem that an accessibility check would be needed on assignment of such
    a type, to avoid copying an object with a discriminant pointing to a
    local object into a more global object (and thus creating a dangling
    pointer).]}

    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[Any type may have an access discriminant,
    but access discriminants cannot have defaults. All types have special
    accessibility. This gets rid of the problems on assignment (you couldn't
    change such a discriminant), but it would be horribly incompatible with
    Ada 95.]}

    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0063-1]}
    @ChgAdded{Version=[2],Text=[Any type may have an access discriminant,
    but access discriminants may have defaults only if they are
    @Chg{Version=[3],New=[of an immutably], Old=[a
    @lquotes@;really@rquotes@;]} limited type. This is the rule chosen for
    Ada 2005, as it is not incompatible, and it doesn't require weird
    accessibility checks.]}
  @end{Itemize}
@end{Reason}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00402-01]}
@ChgDeleted{Version=[2],Text=[@nt{Default_expression}s shall be provided either for all or for none
of the discriminants of a @nt{known_@!discriminant_@!part}.
No @nt<default_@!expression>s are permitted in a
@nt<known_@!discriminant_@!part> in a declaration of a tagged
type @Redundant[or a generic formal type].]}
@begin(Reason)
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
  @ChgDeleted{Version=[2],Text=[The all-or-none rule
  is related to the rule that a discriminant constraint shall specify
  values for all discriminants. One could imagine a different rule
  that allowed a constraint to specify only some of the discriminants,
  with the others provided by default. Having defaults for discriminants
  has a special significance @em it allows objects of the type to
  be unconstrained, with the discriminants alterable as part of
  assigning to the object.]}

  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
  @ChgDeleted{Version=[2],Text=[Defaults for discriminants of tagged types
  are disallowed so that every object of a
  tagged type is constrained,
  either by an explicit constraint,
  or by its initial discriminant values.
  This substantially simplifies the semantic rules
  and the implementation of inherited
  dispatching operations. For generic formal types,
  the restriction simplifies the type matching rules.
  If one simply wants a "default" value for the discriminants,
  a constrained subtype can be declared for future use.]}
@end(Reason)

@Leading@;For a type defined by a @nt<derived_type_definition>,
if a @nt<known_discriminant_part> is provided in its declaration, then:
@begin{Itemize}
The parent subtype shall be constrained;

If the parent type is not a tagged type, then each discriminant
of the derived type shall be used in the constraint defining
the parent subtype;@begin{ImplNote}

  This ensures that the new discriminant can share storage with
  an existing discriminant.@end{implnote}

If a discriminant is used in the constraint defining the parent subtype,
the subtype of the discriminant shall be statically compatible
(see @RefSecNum{Statically Matching Constraints and Subtypes}) with the
subtype of the corresponding parent discriminant.
@begin(Reason)
  This ensures that on conversion
  (or extension via an extension aggregate) to a distantly related type,
  if the discriminants satisfy the target type's requirements they satisfy
  all the intermediate types' requirements as well.
@end(Reason)
@begin(Ramification)
  There is no requirement that the new discriminant have the
  same (or any) @nt<default_expression> as the parent's discriminant.
@end(Ramification)
@end{Itemize}

@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0102-1]}
@ChgDeleted{Version=[3],Text=[The type of the @nt<default_expression>, if any,
for an access discriminant shall be convertible to the anonymous access type of
the discriminant (see @RefSecNum{Type Conversions}).
@PDefn2{Term=[convertible],Sec=(required)}]}
@begin{Ramification}
@ChgRef{Version=[3],Kind=[Deleted]}
@ChgDeleted{Version=[3],Text=[This requires convertibility
of the designated subtypes.]}
@end{ramification}
@end{Legality}

@begin{StaticSem}
A @nt{discriminant_specification} declares a discriminant;
the @nt{subtype_mark}
denotes its subtype unless it is an access discriminant, in which case
the discriminant's subtype is the anonymous access-to-variable subtype
defined by the @nt{access_definition}.

@Redundant[For a type defined by a @nt<derived_type_definition>,
each discriminant of the parent type
is either inherited,
constrained to equal some new discriminant of the derived type,
or constrained to the value of an expression.]
@Defn{corresponding discriminants}
When inherited or constrained to equal some new discriminant,
the parent discriminant and the discriminant of the derived type are said
to @i(correspond).
Two discriminants also correspond
if there is some common discriminant to which they
both correspond. A discriminant corresponds to itself
as well.
@Defn{specified discriminant}
If a discriminant of a parent type is constrained to a specific value
by a @nt<derived_type_definition>,
then that discriminant is said to be @i(specified)
by that @nt<derived_type_definition>.
@begin{Ramification}

  The correspondence relationship is transitive, symmetric,
  and reflexive. That is,
  if A corresponds to B, and B corresponds to C, then
  A, B, and C each corresponds to A, B, and C in all combinations.@end{ramification}

@Defn2{Term=[depend on a discriminant],
  Sec=(for a @nt<constraint> or @nt<component_definition>)}
A @nt{constraint} that appears
within the definition of a discriminated
type @i(depends on a discriminant) of the type if it names the
discriminant as a bound or discriminant value.
A @nt{component_definition} depends on a discriminant if its
@nt{constraint} depends on the discriminant, or on a discriminant
that corresponds to it.
@begin(Ramification)
  A @nt{constraint} in a @nt{task_body} is not considered to
  @i(depend) on a discriminant of the task type, even if it
  names it. It is only the @nt<constraint>s in the type definition
  itself that are considered dependents. Similarly for protected types.
@end(Ramification)

@Leading@keepnext@Defn2{Term=[depend on a discriminant], Sec=(for a component)}
A component @i(depends on a discriminant) if:
@begin(itemize)
  Its @nt{component_definition} depends on the discriminant; or
  @begin{Ramification}
A component does @i(not) depend on a discriminant just because
  its @nt<default_expression> refers to the discriminant.@end{ramification}

  It is declared in a @nt{variant_part} that is governed by the
  discriminant; or

  It is a component inherited as part of a @nt<derived_type_definition>,
  and the @nt<constraint> of the @i(parent_)@nt<subtype_indication>
  depends on the discriminant; or
  @begin{Reason}
    When the parent subtype depends on a discriminant, the parent
    part of the derived type is treated like a discriminant-dependent
    component.
  @end{Reason}
  @begin{Ramification}
    Because of this rule, we don't really need to worry about @lquotes@;corresponding@rquotes@;
    discriminants, since all the inherited components will be
    discriminant-dependent if there is a new @nt<known_discriminant_part>
    whose discriminants are used to constrain the old discriminants.
  @end{Ramification}

  It is a subcomponent of a component that depends on the discriminant.
@end(itemize)
@begin{Reason}
  The concept of discriminant-dependent (sub)components is primarily used
  in various rules that disallow renaming or 'Access, or specify that
  certain discriminant-changing assignments are erroneous.
  The goal is to allow implementations to move around or change the size
  of discriminant-dependent subcomponents upon a discriminant-changing
  assignment to an enclosing object. The above definition specifies that
  all subcomponents of a discriminant-dependent component or parent part
  are themselves discriminant-dependent, even though their presence
  or size does not in fact depend on a discriminant. This is because
  it is likely that they will move in a discriminant-changing assignment
  if they are a component of one of several discriminant-dependent
  parts of the same record.
@end{Reason}

Each value of a discriminated type includes a value for
each component
of the type that does not depend
on a discriminant@Redundant[; this includes the discriminants
themselves]. The values
of discriminants determine which other component values are present
in the value of the discriminated type.
@begin{Honest}
Which values are present might depend on discriminants of
  some ancestor type that are constrained in an intervening
  @nt<derived_type_definition>. That's why we say "values of discriminants"
  instead of "values of @i(the) discriminants" @em a subtle point.@end{honest}

@Defn{known discriminants}
@Defn2{Term=[discriminants], Sec=(known)}
@Defn2{Term=[constrained], Sec=(subtype)}
@Defn2{Term=[unconstrained], Sec=(subtype)}
A type declared with a @nt<known_discriminant_part> is said to have
@i(known discriminants); its first subtype is unconstrained.
@Defn{unknown discriminants}
@Defn2{Term=[discriminants], Sec=(unknown)}
A type declared with an
@nt<unknown_discriminant_part> is said to have @i(unknown discriminants).
A type declared without a @nt<discriminant_part> has
no discriminants, unless it is a derived type; if derived,
such a type has the same sort of discriminants (known, unknown, or none)
as its parent (or ancestor) type.
A tagged class-wide type also has unknown discriminants.
@Defn{class-wide type}
@Defn{indefinite subtype}
@Redundant[Any subtype of a type with unknown discriminants
is an unconstrained and indefinite
subtype (see @RefSecNum{Types and Subtypes}
and @RefSecNum{Objects and Named Numbers}).]
@begin(Discussion)
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
  An @nt<unknown_discriminant_part> @lquotes@;(<>)@rquotes@; is only permitted in
  the declaration of a (generic or nongeneric) private type,
  private extension, @Chg{Version=[2],New=[incomplete type, ],Old=[]}or
  formal derived type.@Comment{That was always intended,
  but 8652/0007 was needed to make it true.}
  Hence, only such types, descendants thereof, and class-wide
  types can have unknown discriminants.
  An @nt<unknown_discriminant_part> is used to indicate that the corresponding
  actual or full type might have discriminants without defaults,
  or be an unconstrained array subtype. Tagged class-wide types
  are also considered to have unknown discriminants because discriminants
  can be added by type extensions, so the total number of
  discriminants of any given value of a tagged class-wide type
  is not known at compile time.

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
  A subtype with unknown discriminants
  is indefinite, and hence an object of such a subtype needs
  explicit initialization.@Chg{Version=[2],New=[],Old=[ If the subtype is
  limited, no (stand-alone) objects
  can be declared since initialization is not permitted (though
  formal parameters are permitted, and objects of the actual/full type
  will generally be declarable).]} A limited private type with
  unknown discriminants is @lquotes@;extremely@rquotes@;
  limited;@Chg{Version=[2],New=[ objects of],Old=[]} such a type
  @Chg{Version=[2],New=[ can be initialized only by subprograms (either
  procedures with a parameter of the type, or a function returning the
  type) declared in the package. Subprograms declared elsewhere can operate on
  and even return the type, but they can only initialize the object by calling
  (ultimately) a subprogram in the package declaring the type. Such a type],Old=[]} is useful for
  keeping complete control over object creation within the package declaring
  the type.

  A partial view of a type might have unknown discriminants, while
  the full view of the same type might have known, unknown, or no
  discriminants@Chg{New=[.],Old=[,]}
@end(Discussion)
@end{StaticSem}

@begin{RunTime}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01],ARef=[AI95-00416-01]}
@Chg{Version=[2],New=[For an access discriminant, its],
Old=[An]} @nt<access_definition> is
elaborated when the value of @Chg{Version=[2],New=[the],Old=[a corresponding]}
access discriminant is defined@Chg{Version=[2],New=[:],Old=[, either]}
by evaluation of its @nt<default_expression>@Chg{Version=[2],New=[,],Old=[ or]}
by elaboration of a @nt<discriminant_constraint>@Chg{Version=[2],New=[, or
by an assignment that initializes the enclosing object.],Old=[.
@Redundant[The elaboration of an @nt<access_definition> creates the
anonymous access type. When the expression defining the
access discriminant is evaluated, it is converted to this
anonymous access type (see @RefSecNum{Type Conversions}).]]}
@PDefn2{Term=[implicit subtype conversion],Sec=(access discriminant)}
@begin(Ramification)
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00231-01],ARef=[AI95-00416-01]}
  @Chg{Version=[2],New=[The],Old=[This]} conversion@Chg{Version=[2],New=[
  of the @nt{expression} defining the access discriminant to the anonymous
  access type],Old=[]} raises @Chg{Version=[2],New=[Program_Error],
  Old=[Constraint_Error if the initial value is @key(null), or,]} for an
  object created by an allocator of an access type T, if the
  initial value is an access parameter that designates a view whose
  accessibility level is deeper than that of T.
@end(Ramification)
@end{RunTime}

@begin{Notes}
If a discriminated type has @nt<default_expression>s for its
discriminants, then unconstrained variables of the type are permitted,
and the values of the discriminants can be changed by an assignment
to such a variable.
If defaults are not provided for the discriminants, then all
variables of the type are constrained, either by explicit
constraint or by their initial value; the values of the discriminants
of such a variable cannot be changed after initialization.
@begin(Discussion)
  This connection between discriminant defaults and unconstrained variables
  can be a source of confusion. For Ada 95, we considered various
  ways to break the connection between defaults and unconstrainedness,
  but ultimately gave up for lack of a sufficiently simple and intuitive
  alternative.


  @Defn{mutable}
  An unconstrained discriminated subtype with defaults is called
  a @i{mutable} subtype, and a variable of such a subtype is called
  a mutable variable,
  because the discriminants of such a variable can change.
  There are no mutable arrays (that is, the bounds of an array
  object can never change), because there is no way in the language
  to define default values for the bounds.
  Similarly, there are no mutable class-wide subtypes,
  because there is no way to define the default tag,
  and defaults for discriminants are not allowed in the
  tagged case.
  Mutable tags would also require a way for the
  maximum possible size of such a class-wide subtype to be known.
  (In some implementations, all mutable variables are allocated
  with the maximum possible size. This approach is appropriate
  for real-time applications where implicit use of the heap
  is inappropriate.)

@end(Discussion)

The @nt{default_expression} for a discriminant of a type is evaluated
when an object of an unconstrained subtype of the type is created.

Assignment to a discriminant of an object (after its
initialization) is not allowed,
since the name of a discriminant is a constant; neither
@nt{assignment_statement}s
nor assignments inherent in passing as an @key(in out)
or @key(out) parameter
are allowed.
Note however that the value of a discriminant
can be changed by assigning to the enclosing object, presuming it
is an unconstrained variable.
@begin(Discussion)
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
An @nt{unknown_discriminant_part} is permitted only in the
declaration of a private type (including generic formal private),
private extension, @Chg{Version=[2],New=[incomplete type, ],Old=[]}or
generic formal derived type.
These are the things that will have a corresponding completion or
generic actual, which will either define the discriminants,
or say there are none.
The (<>) indicates that the actual/full subtype might be an indefinite subtype.
An @nt{unknown_discriminant_part} is not permitted in a normal
untagged derived type declaration, because there is no separate
full type declaration for such a type.
Note that (<>) allows unconstrained array bounds;
those are somewhat like undefaulted discriminants.

For a derived type, either the discriminants are inherited as is,
or completely respecified in a new @nt<discriminant_part>. In this
latter case, each discriminant of the parent type shall be constrained,
either to a specific value, or to equal one of the new discriminants.
Constraining a parent type's discriminant to equal one of the new
discriminants is like a renaming of the discriminant, except that
the subtype of the new discriminant can be more restrictive than
that of the parent's one.
In any case, the new discriminant can share storage with the
parent's discriminant.
@end(Discussion)

A discriminant that is of a named access type is not called
an access discriminant; that term is
used only for discriminants defined by an @nt<access_definition>.
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of discriminated types:)
@begin(Example)
@key(type) Buffer(Size : Buffer_Size := 100)  @key(is)        --@RI[ see @RefSecNum(Integer Types)]
   @key(record)
      Pos   : Buffer_Size := 0;
      Value : String(1 .. Size);
   @key(end) @key(record);

@key(type) Matrix_Rec(Rows, Columns : Integer) @key(is)
   @key(record)
      Mat : Matrix(1 .. Rows, 1 .. Columns);       --@RI[ see @RefSecNum(Array Types)]
   @key(end) @key(record);

@key(type) Square(Side : Integer) @key(is) @key(new)
   Matrix_Rec(Rows => Side, Columns => Side);

@key(type) Double_Square(Number : Integer) @key(is)
   @key(record)
      Left  : Square(Number);
      Right : Square(Number);
   @key(end) @key(record);

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Chg{Version=[2],New=[@key(task type) Worker(Prio : System.Priority; Buf : @key(access) Buffer)@Chg{Version=[3],New=[
   @key(with) Priority => Prio @key(is) --@RI[ see @RefSecNum{Task Priorities}]],Old=[@key(is)]}
   --@RI[ discriminants used to parameterize the task type (see @RefSecNum{Task Units and Task Objects})]@Chg{Version=[3],New=[],Old=[
   @key(pragma) Priority(Prio);  --@RI[ see @RefSecNum{Task Priorities}]]}
   @key(entry) Fill;
   @key(entry) Drain;
@key(end) Worker;],Old=[@key(type) Item(Number : Positive) @key(is)
   @key(record)
      Content : Integer;
      --@RI[  no component depends on the discriminant]
   @key(end) @key(record);]}
@end(Example)
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
The syntax for a @nt{discriminant_specification}
is modified to allow an @i{access discriminant},
with a type specified by an @nt{access_definition}
(see @RefSecNum{Access Types}).

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01]}
Discriminants are allowed on all composite types other than array@Chg{Version=[2],New=[ and
interface],Old=[]} types.

Discriminants may be of an access type.
@end{Extend83}

@begin{DiffWord83}
@nt{Discriminant_part}s are not elaborated,
though an @nt<access_definition> is elaborated
when the discriminant is initialized.

@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01],ARef=[AI95-00402-01],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[Access discriminants (anonymous access types
  used as a discriminant) can be used on any type allowing discriminants.
  Defaults aren't allowed on discriminants of nonlimited types, however, so
  that accessibility problems don't happen on assignment.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00231-01]}
  @ChgAdded{Version=[2],Text=[
  @nt{null_exclusion} can be used in the declaration of a discriminant.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0007],ARef=[AI95-00098-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> The wording was clarified so
  that types that cannot have discriminants cannot have an
  @nt{unknown_discriminant_part}.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
  @ChgAdded{Version=[2],Text=[Added wording to prevent interfaces from
  having discriminants. We don't want interfaces to have any components.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00254-01]}
  @ChgAdded{Version=[2],Text=[Removed wording which implied or required an
  access discriminant to have an access-to-object type (anonymous access
  types can now be access-to-subprogram types as well).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00326-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
  @ChgAdded{Version=[2],Text=[Fixed the wording of the introduction to this
  @Chg{Version=[3],New=[subclause],Old=[clause]} to reflect that both incomplete
  and partial views can have unknown discriminants. That was always true,
  but for some reason this wording specified partial views.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00419-01]}
  @ChgAdded{Version=[2],Text=[Changed the wording to use the new term
  @lquotes@;explicitly limited record@rquotes, which makes the intent
  much clearer (and eliminates confusion with derived types that happen to
  contain the reserved word @key[limited]).]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0063-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b<Correction:>
  Changed the rules for when access discriminants can have defaults to depend
  on the new definition for immutably limited types; this will help ensure that
  unusual corner cases are properly handled. Note that the Ada 2005 rule was
  unintentionally incompatible with the Ada 95 rule (as enforced by the ACATS);
  this change brings it back into alignment with actual practice. So there
  should be no practical incompatibility.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0214-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  A limited tagged type may now have defaults for its discriminants.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0102-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Moved implicit conversion
  @LegalityName to @RefSecNum{The Context of Overload Resolution}.]}
@end{DiffWord2005}


@ISOOnlyRMNewPageVer{Version=[3]}@Comment{For ISO version of Ada 2012 Standard}
@LabeledSubClause{Discriminant Constraints}

@begin{Intro}
A @nt<discriminant_constraint> specifies the values of the discriminants
for a given discriminated type.
@end{Intro}

@begin{MetaRules}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
The rules in this @Chg{Version=[3],New=[subclause],Old=[clause]} are
intentionally parallel to those given in @RefSec{Record Aggregates}.
@end{MetaRules}

@begin{Syntax}
@Syn{lhs=<discriminant_constraint>,rhs="
   (@Syn2{discriminant_association} {, @Syn2{discriminant_association}})"}


@Syn{lhs=<discriminant_association>,rhs="
   [@SynI{discriminant_}@Syn2{selector_name} {| @SynI{discriminant_}@Syn2{selector_name}} =>] @Syn2{expression}"}

@begin{SyntaxText}
@Defn{named discriminant association}
A @nt<discriminant_association>
is said to be @i(named) if it has one or more
@i(discriminant_)@nt<selector_name>s;
@Defn{positional discriminant association}
it is otherwise said to be @i(positional).
In a @nt<discriminant_constraint>,
any positional associations shall precede any named
associations.
@end{SyntaxText}

@end{Syntax}

@begin{Resolution}
Each @nt<selector_name> of a named @nt<discriminant_@!association> shall
resolve to denote a discriminant of the subtype being constrained;
@Defn2{Term=[associated discriminants],
  Sec=(of a named @nt<discriminant_association>)}
the discriminants so named are the @i(associated discriminants) of
the named association.
@Defn2{Term=[associated discriminants],
  Sec=(of a positional @nt<discriminant_association>)}
For a positional association, the @i(associated discriminant)
is the one whose @nt<discriminant_@!specification> occurred in
the corresponding position in the @nt<known_@!discriminant_@!part>
that defined the discriminants of the subtype being constrained.

@PDefn2{Term=[expected type], Sec=(discriminant_association expression)}
The expected type for the @nt{expression} in
a @nt{discriminant_association}
is that of the associated discriminant(s).
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0008],ARef=[AI95-00168-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00363-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0041-1]}
A @nt{discriminant_constraint} is only allowed in a
@nt{subtype_indication} whose @nt{subtype_mark} denotes
either an unconstrained discriminated subtype, or an
unconstrained access subtype whose designated subtype is
an unconstrained discriminated subtype.
@Chg{New=[However, in the case of @Chg{Version=[2],New=[an],Old=[a general]}
access subtype, a @nt{discriminant_@!constraint} is
@Chg{Version=[3],New=[legal only if any dereference of a value of the access
type is known to be constrained (see @RefSecNum{Objects and Named Numbers})],
Old=[illegal if @Chg{Version=[2],New=[the
designated type has a partial view that is constrained or, for a general
access subtype, has @nt{default_expression}s for its discriminants],Old=[]}]}],
Old=[there is a place within the
immediate scope of the designated subtype where the designated subtype's view
is constrained]}.
@Chg{Version=[2],New=[In addition to the places where @LegalityTitle@;
normally apply (see @RefSecNum{Generic Instantiation}),
these rules apply also in the private part of an instance
of a generic unit.@PDefn{generic contract issue}],Old=[]}@Chg{Version=[3],
New=[],Old=[@Chg{Version=[2],
New=[ In a generic body, this rule is checked presuming all
formal access types of the generic might be general access types, and all
untagged discriminated formal types of the generic might have
@nt{default_expression}s for their discriminants.],Old=[]}]}

@begin{Reason}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0008],ARef=[AI95-00168-01]}
@ChgRef{Version=[2],Kind=[DeletedAdded],ARef=[AI95-00363-01]}
@ChgDeleted{Version=[2],Text=[@Chg{New=[The second rule is necessary to
prevent assignments that change the discriminant of a constrained object.
See the defect report for examples.],Old=[]}]}
@end{Reason}

@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00363-01]}
@ChgAdded{Version=[2],Text=[The second rule is necessary to prevent objects
from changing so that they no longer match their constraint. In Ada 95, we
attempted to prevent this by banning every case where an aliased object
could be unconstrained or be changed by an enclosing assignment. New ways
to cause this problem were being discovered frequently, meaning that new rules
had to be dreamed up to cover them. Meanwhile, aliased objects and components
were getting more and more limited. In Ada 2005, we sweep away all of that
cruft and replace it by a simple rule @lquotes@;thou shalt not create an
access subtype that can point to an item whose discriminants can be changed by
assignment@rquotes@;.]}
@end{Reason}

@begin{Discussion}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0041-1]}
@ChgAdded{Version=[3],Text=[The second rule will only use the indefinite or
dereference bullets in the definition of @ldquote@;known to be constrained@rdquote.
The rule is worded in terms of @ldquote@;known to be constrained@rdquote in
order to capture the special rules that apply in generic bodies (rather than
repeating them and getting them subtly wrong).]}
@end{Discussion}

A named @nt<discriminant_association> with more than one
@nt<selector_name> is allowed only if the named discriminants
are all of the same type.
A @nt<discriminant_constraint> shall provide exactly one value for each
discriminant of the subtype being constrained.

@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0102-1]}
@ChgDeleted{Version=[3],Text=[The @nt<expression> associated with an access
discriminant shall be of a type convertible to the anonymous access type.
@PDefn2{Term=[convertible],Sec=(required)}]}

@begin(Ramification)

  @ChgRef{Version=[3],Kind=[Revised]}
  @Chg{Version=[3],New=[In addition, @RefSecNum{The Context of Overload Resolution}
  requires that the @nt{expression} associated with an access discriminant
  is convertible (see @RefSecNum{Type Conversions}) to the anonymous
  access type. ],Old=[]}This implies both convertibility of designated
  types, and static accessibility.
  This implies that if an object of type T with an
  access discriminant is created
  by an allocator for an access type A, then it requires that the
  type of the @nt<expression> associated
  with the access discriminant have an accessibility level
  that is not statically deeper than that of A.
  This is to avoid dangling references.

@end(Ramification)
@end{Legality}

@begin{RunTime}
@PDefn2{Term=[compatibility], Sec=(discriminant constraint with a
subtype)}
A @nt{discriminant_constraint} is @i(compatible) with an unconstrained
discriminated subtype if each discriminant value belongs to the
subtype of the corresponding discriminant.
@begin{Ramification}
The "dependent
  compatibility check" has been eliminated in Ada 95. Any checking
  on subcomponents is performed when (and if) an object is created.@end{ramification}
@begin{Discussion}
  There is no need to define compatibility with a constrained
  discriminated subtype, because one is not allowed to constrain it again.@end{discussion}

@PDefn2{Term=[satisfies], Sec=(a discriminant constraint)}
A composite value @i(satisfies) a discriminant constraint if and only
if each discriminant of the composite value has the value imposed
by the discriminant constraint.

@PDefn2{Term=[elaboration], Sec=(discriminant_constraint)}
For the elaboration of a @nt{discriminant_constraint}, the @nt{expression}s
in the @nt{discriminant_association}s are evaluated in an arbitrary
order and converted to the type of the associated
discriminant (which might raise Constraint_Error @em
see @RefSecNum{Type Conversions});
the @nt{expression} of a named association is evaluated
(and converted) once for each associated discriminant.
@PDefn2{Term=[implicit subtype conversion],Sec=(discriminant values)}
The result of each evaluation and conversion is the
value imposed by the constraint for the associated discriminant.
@begin{Reason}

  We convert to the type, not the subtype, so that the definition
  of compatibility of discriminant constraints is not vacuous.@end{reason}

@end{RunTime}

@begin{Notes}
The rules of the language ensure that
a discriminant of an object always has a value, either
from explicit or implicit initialization.
@begin(Discussion)
Although it is illegal to constrain a class-wide tagged subtype, it
is possible to have a partially constrained class-wide
subtype: If the subtype S is defined by T(A => B),
then S'Class is partially constrained in the sense that objects of
subtype S'Class have to have discriminants corresponding
to A equal to B,
but there can be other discriminants defined
in extensions that are not constrained to any particular value.
@end(Discussion)
@end{Notes}

@begin{Examples}
@Leading@keepnext@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@i{Examples (using types declared above in
@Chg{Version=[3],New=[subclause],Old=[clause]} @RefSecNum(Discriminants)):}
@begin(Example)
Large   : Buffer(200);  --@RI[  constrained, always 200 characters]
                        --@RI[   (explicit discriminant value)]
Message : Buffer;       --@RI[  unconstrained, initially 100 characters]
                        --@RI[   (default discriminant value)]
Basis   : Square(5);    --@RI[  constrained, always 5 by 5]
Illegal : Square;       --@RI[  illegal, a Square has to be constrained]
@end(Example)
@end{Examples}

@begin{Inconsistent83}
@Defn{inconsistencies with Ada 83}
Dependent compatibility checks are no longer performed on
subtype declaration. Instead they are deferred until
object creation (see @RefSecNum(Object Declarations)).
This is upward compatible for a program
that does not raise Constraint_Error.
@end{Inconsistent83}

@begin{DiffWord83}
Everything in RM83-3.7.2(7-12), which specifies the
initial values for discriminants, is now redundant
with 3.3.1, 6.4.1, 8.5.1, and 12.4. Therefore, we don't
repeat it here. Since the material is largely intuitive,
but nevertheless complicated to state formally, it doesn't
seem worth putting it in a "NOTE."
@end{DiffWord83}

@begin{Incompatible95}
@ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0008],ARef=[AI95-00168-01],ARef=[AI95-00363-01]}
@ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
The Corrigendum added a restriction on @nt{discriminant_constraint}s for
general access subtypes. Such constraints are prohibited
if the designated type can be treated as constrained somewhere in the program.
Ada 2005 goes further and prohibits such @nt{discriminant_constraint}s if
the designated type has (or might have, in the case of a formal type)
defaults for its discriminants. The use of general access subtypes is rare,
and this eliminates a boatload of problems that required many restrictions
on the use of aliased objects and components (now lifted). Similarly,
Ada 2005 prohibits @nt{discriminant_constraint}s on any access type whose
designated type has a partial view that is constrained. Such a type will
not be constrained in the heap to avoid privacy problems. Again, the use
of such subtypes is rare (they can only happen within the package and its
child units).]}
@end{Incompatible95}

@begin{Diffword2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0041-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Revised the rules on
  access subtypes having discriminant constraints to depend on the
  @ldquote@;known to be constrained@rdquote rules. This centralizes
  the rules so that future fixes need to be made in only one place,
  as well as fixing bugs in obscure cases.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0102-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Moved implicit conversion
  @LegalityName to @RefSecNum{The Context of Overload Resolution}.]}
@end{Diffword2005}


@LabeledSubClause{Operations of Discriminated Types}

@begin{Intro}
@Redundant[If a discriminated type has @nt<default_expression>s for its
discriminants, then unconstrained variables of the type are permitted,
and the discriminants of such a variable can be changed by assignment to
the variable. For a formal parameter of such a type, an attribute
is provided to determine whether the corresponding actual parameter
is constrained or unconstrained.]
@end{Intro}

@begin{StaticSem}
@Leading@Keepnext@;For @PrefixType{a @nt<prefix> A that is of a
discriminated type @Redundant[(after any implicit dereference)]},
the following attribute is defined:
@begin(description)
@ChgAttribute{Version=[3], Kind=[Revised], ChginAnnex=[T],
  Leading=[F], Prefix=<A>, AttrName=<Constrained>, ARef=[AI05-0214-1],
  Text=[Yields the value True if A denotes a constant, a value,
  @Chg{Version=[3],New=[a tagged object, ],Old=[]}or a constrained variable,
  and False otherwise.]}
@begin(ImplNote)
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0214-1]}
 This attribute is primarily used on parameters, to determine whether
 the discriminants can be changed as part of an assignment.
 The Constrained attribute is statically True for @key(in) parameters.
 For @key(in out) and @key(out) parameters of a discriminated type,
 the value of this attribute needs to be passed as an implicit
 parameter, in general. However, if the type @Chg{Version=[3],New=[is
 tagged or ],Old=[]}does not have
 defaults for its discriminants, the attribute is statically True,
 so no implicit parameter is needed.
 Parameters of a limited @Chg{Version=[3],New=[untagged ],Old=[]}type
 with defaulted discriminants need this implicit parameter,
 unless there are no nonlimited views,
 because they might be passed to a subprogram whose body has
 visibility on a nonlimited view of the type, and hence might be
 able to assign to the object and change its discriminants.
@end(ImplNote)

@begin{Reason}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0214-1]}
@ChgAdded{Version=[3],Text=[All tagged objects are known to be constrained (as
 nonlimited tagged types cannot have discriminant defaults, and limited tagged
 objects are immutably limited), and are always considered constrained by this
 attribute to avoid distributed overhead for parameters of limited classwide
 types, as limited tagged objects may technically be unconstrained if they use
 defaulted discriminants. Such objects still cannot have their discriminants
 changed, as assignment is not supported for them, so there is no use for this
 attribute that would justify the overhead of passing it with all classwide
 parameters.]}
@end{Reason}

@begin{Discussion}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1],ARef=[AI05-0214-1]}
@ChgAdded{Version=[3],Text=[If the type of A is a type derived from an untagged
 partial view of a tagged type such that it is not a tagged type, then A is not
 considered a tagged object, and A'Constrained can return either True or
 False depending on the nature of the object.]}
@end{Discussion}
@end(description)
@EndPrefixType{}
@end{StaticSem}

@begin{Erron}
@PDefn2{Term=(erroneous execution),Sec=(cause)}
The execution of a construct is erroneous if the construct
has a constituent that is a @nt<name> denoting
a subcomponent that depends on discriminants, and the
value of any of these discriminants is changed by this execution
between evaluating the @nt<name> and the last use (within this
execution) of the subcomponent denoted by the @nt<name>.
@begin{Ramification}
  This rule applies to @nt<assignment_statement>s,
  calls (except when the discriminant-dependent subcomponent is
  an @key(in) parameter passed by copy),
  @nt<indexed_component>s, and @nt<slice>s.
  Ada 83 only covered the first two cases. AI83-00585 pointed out
  the situation with the last two cases.
  The cases of @nt<object_renaming_declaration>s and
  generic formal @key(in out) objects are handled differently,
  by disallowing the situation at compile time.
@end{Ramification}
@end{Erron}

@begin{Extend83}
@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{To be consistent with 8652/0006}
@Defn{extensions to Ada 83}
For consistency with other attributes, we are allowing
the @Chg{New=[@nt{prefix}],Old=[prefix]} of Constrained to be a value as well
as an object of a discriminated type, and also an implicit
dereference. These extensions are
not important capabilities, but there seems no
reason to make this attribute different from
other similar attributes. We are curious what most
Ada 83 compilers do with F(1).X'Constrained.

We now handle in a general way
the cases of erroneousness identified by AI83-00585, where
the @nt<prefix> of an @nt<indexed_component> or @nt<slice>
is discriminant-dependent, and the evaluation of the index or
discrete range changes the value of a discriminant.
@end{Extend83}

@begin{DiffWord83}
We have moved all discussion of erroneous use of @nt<name>s
that denote discriminant-dependent
subcomponents to this subclause. In Ada 83, it used to
appear separately under @nt{assignment_statement}s and
subprogram calls.
@end{DiffWord83}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0214-1]}
  @ChgAdded{Version=[3],Text=[A'Constrained is now defined to return
  True for any A that is a tagged object. This doesn't change the result
  for any A allowed by previous versions of Ada; the change is necessary to
  avoid unnecessary overhead for limited tagged parameters.]}
@end{DiffWord2005}


@LabeledClause{Record Types}

@begin{Intro}
@Defn{record}
@Defn{record type}
A record object is a composite object consisting of named components.
The value of a record object is a composite value consisting of the
values of the components.
@IndexSee{Term=[structure],See=(record type)}
@end{Intro}

@begin{Syntax}
@Syn{lhs=<record_type_definition>,rhs="[[@key{abstract}] @key{tagged}] [@key{limited}] @Syn2{record_definition}"}
@Syn{lhs=<record_definition>,rhs="
    @key{record}
       @Syn2{component_list}
    @key{end} @key{record}
  | @key{null record}"}


@Syn{lhs=<component_list>,rhs="
      @Syn2{component_item} {@Syn2{component_item}}
   | {@Syn2{component_item}} @Syn2{variant_part}
   |  @key{null};"}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@Syn{lhs=<component_item>,rhs="@Syn2{component_declaration} | @Chg{New=[@Syn2{aspect_clause}],Old=[@Syn2{representation_clause}]}"}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@Syn{lhs=<component_declaration>,rhs="
   @Syn2{defining_identifier_list} : @Syn2{component_definition} [:= @Syn2{default_expression}]@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]};"}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected type], Sec=(component_declaration default_expression)}
The expected type for the @nt<default_expression>, if any, in
a @nt<component_declaration> is the type of the
component.
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00287-01]}
@ChgDeleted{Version=[2],Text=[A @nt<default_expression> is not permitted if
the component is of a limited type.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00366-01]}@ChgNote{This is not THE definition of component}
@PDefn2{Term=[components], Sec=(of a record type)}
Each @nt<component_declaration> declares a
@Chg{Version=[2],New=[component],Old=[@i(component)]} of the record type.
Besides components declared by @nt<component_declaration>s, the components of a
record type include any components declared by @nt<discriminant_specification>s
of the record type declaration. @Redundant[The identifiers of all components of
a record type shall be distinct.]
@begin{TheProof}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
  The identifiers of all
  components of a record type have to
  be distinct because they are all declared immediately
  within the same declarative region. See @Chg{Version=[3],New=[Clause],Old=[Section]}
  @RefSecNum{Visibility Rules}.
@end{theproof}


Within a @nt{type_declaration},
a @nt{name} that denotes a component, protected subprogram,
or entry of the type is allowed only in the following cases:

@begin{Itemize}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0004-1],ARef=[AI05-0295-1]}
A @nt{name} that denotes any component, protected subprogram,
or entry is allowed within @Chg{Version=[3],New=[an @nt{aspect_specification},
an operational item, or ],Old=[]}a
representation item that occurs within the declaration of the composite type.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
A @nt{name} that denotes a noninherited discriminant
is allowed within the declaration of the type,
but not within the @nt{discriminant_part}.
If the discriminant is used to define the constraint of
a component, the bounds of an entry family, or
the constraint of the parent subtype in a
@nt<derived_type_definition>@Chg{Version=[3],New=[,],Old=[]}
then its name shall appear alone as a @nt<direct_name> (not as
part of a larger expression or expanded name).
A discriminant shall not be used to define the constraint of
a scalar component.@Defn2{Term=[discriminant],Sec=[use in a record definition]}@ChgNote{New index entry}
@begin{Reason}
The penultimate restriction simplifies implementation,
  and allows the outer discriminant and the inner discriminant
  or bound to possibly share storage.
@end{reason}
@begin{Ramification}
  Other rules prevent such a discriminant from being an inherited one.
@end{ramification}
@begin{Reason}
The last restriction is inherited from Ada 83.
The restriction is not really necessary from a language design point of
view, but we did not remove it,
in order to avoid unnecessary changes to existing compilers.
@end{Reason}
@begin{Discussion}
  Note that a discriminant can be used to define the constraint
  for a component that is of an access-to-composite type.
@end{Discussion}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00373-01]}
  The above rules, and a similar one in @RefSecNum{Subprogram Declarations}
  for formal parameters, are intended to allow initializations of
  components or parameters to occur in @Chg{Version=[2],
  New=[a (nearly)],Old=[an]} arbitrary order @em whatever
  order is most efficient@Chg{Version=[2],New=[ (subject to the restrictions
  of @RefSecNum{Object Declarations})],Old=[]},
  since one @nt{default_expression} cannot depend on the value of
  another one.
  @Chg{Version=[2],New=[They],Old=[It]} also prevent circularities.
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
  Inherited discriminants are not allowed to be denoted,
  except within @Chg{Version=[3],New=[@nt{aspect_specification}s
  and ],Old=[]}representation items.
  However, the @SynI{discriminant_}@nt{selector_name} of
  the parent @nt{subtype_indication} is allowed to denote
  a discriminant of the parent.
@end{Ramification}
@end{Itemize}

If the name of the current instance of a type
(see @RefSecNum{The Context of Overload Resolution})
is used to define the constraint of a component,
then it shall appear as a @nt{direct_name}
that is the @nt<prefix> of an @nt{attribute_reference}
whose result is of an access type,
and the @nt{attribute_reference} shall appear alone.
@begin{Reason}
  This rule allows T'Access
  or T'Unchecked_Access, but disallows, for example,
  a range constraint (1..T'Size).
  Allowing things like (1..T'Size) would mean that a per-object
  constraint could affect the size of the object,
  which would be bad.
@end{Reason}

@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00318-02]}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0004-1]}
@ChgAdded{Version=[2],Text=[@Defn{explicitly limited record}
@Defn2{Term=[record],Sec=(explicitly limited)}
If a @Chg{Version=[3],New=[@nt{record_type_definition}],Old=[@ntf{record_type_declaration}]}
includes the reserved word @key{limited}, the type is called an
@i<explicitly limited record> type.]}

@PDefn2{Term=[nominal subtype], Sec=(of a record component)}
The @nt{component_definition} of a @nt<component_declaration>
defines the (nominal) subtype of the component.
If the reserved word @key(aliased) appears in the @nt{component_definition},
then the component is aliased (see @RefSecNum{Access Types}).
@begin(Ramification)
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00363-01]}
  @ChgDeleted{Version=[2],Text=[In this case, the nominal subtype cannot be
  an unconstrained discriminated subtype.
  See @RefSecNum{Array Types}.]}
@end(Ramification)

@Defn{null record}
If the @nt<component_list> of a record type is defined by the reserved
word @key(null) and there are no discriminants, then the record type has
no components and all records of the type are @i(null records).
A @nt<record_definition> of @key{null record} is equivalent to
@key{record null; end record}.
@begin{Ramification}

  This short-hand is available both for declaring a record type
  and a record extension @em see @RefSecNum(Type Extensions).
@end{ramification}

@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(record_type_definition)}
The elaboration of a @nt{record_type_definition} creates
the record type and its first subtype,
and consists of the elaboration of the @nt<record_definition>.
@PDefn2{Term=[elaboration], Sec=(record_definition)}
The elaboration of a @nt<record_definition> consists of the
elaboration of its @nt{component_list}, if any.

@PDefn2{Term=[elaboration], Sec=(component_list)}
The elaboration of a @nt{component_list} consists of the elaboration
of the @nt{component_item}s and
@nt{variant_part}, if any, in the order in which they appear.
@PDefn2{Term=[elaboration], Sec=(component_declaration)}
The elaboration of a @nt{component_declaration} consists of the
elaboration of the @nt{component_definition}.
@begin(Discussion)
  If the @nt<defining_identifier_list> has more than one
  @nt<defining_identifier>, we presume here that the transformation
  explained in @RefSecNum(Object Declarations) has already
  taken place. Alternatively, we could say that
  the @nt<component_definition> is elaborated once for
  each @nt<defining_identifier> in the list.
@end(Discussion)

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0002],ARef=[AI95-00171-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
@Defn{per-object expression}
@Defn{per-object constraint}
@Defn{entry index subtype}
Within the definition of a composite type,
if a @nt<component_definition> or @nt<discrete_subtype_definition>
(see @RefSecNum{Entries and Accept Statements})
includes a @nt<name> that denotes a discriminant of the type, or
that is an @nt<attribute_reference> whose @nt<prefix> denotes the current
instance of the type,
the expression containing the @nt<name> is called a @i(per-object expression),
and the @Chg{New=[@nt{constraint} or @nt{range}],Old=[constraint]} being defined
is called a @i(per-object constraint).
@PDefn2{Term=[elaboration], Sec=(component_definition)}
For the elaboration of a @nt{component_definition} of
a @nt<component_declaration>@Chg{New=[ or the @nt{discrete_@!subtype_@!definition}
of an @nt{entry_@!declaration} for an entry family (see
@RefSecNum{Entries and Accept Statements})],Old=[]},@Chg{Version=[2],
New=[ if the component subtype is defined by an @nt{access_definition} or],
Old=[]} if the @nt{constraint}
@Chg{New=[or @nt{range}],Old=[]} of the @nt{subtype_indication}
@Chg{New=[or @nt{discrete_@!subtype_@!definition}],Old=[]} is not a per-object
constraint, then the@Chg{Version=[2],New=[ @nt{access_definition},],Old=[]}
@nt{subtype_indication}@Chg{Version=[2],New=[,],Old=[]}
@Chg{New=[or @nt{discrete_@!subtype_@!definition}],Old=[]}
is elaborated. On the other hand, if the @nt{constraint}
@Chg{New=[or @nt{range}],Old=[]} is a per-object constraint,
then the elaboration consists of the evaluation of any included
expression that is not part of a per-object expression.
@Chg{New=[Each such expression is evaluated once unless it is part of a named
association in a discriminant constraint, in which case it is evaluated once
for each associated discriminant.],Old=[]}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0002],ARef=[AI95-00171-01]}
@Chg{New=[@PDefn2{Term=[Elaboration],Sec=(per-object constraint)}When a
per-object constraint is elaborated @Redundant[(as part of creating an
object)], each per-object expression of the constraint is evaluated. For other
expressions, the values determined during the elaboration of the
@nt{component_@!definition} or @nt{entry_@!declaration} are used. Any checks
associated with the enclosing @nt{subtype_indication} or
@nt{discrete_subtype_definition} are performed@Redundant[, including the subtype
compatibility check (see @RefSecNum{Subtype Declarations}),] and the associated
subtype is created.],
Old=[]}
@begin(Discussion)
  The evaluation of other expressions that appear in
  @nt<component_definition>s and @nt<discrete_subtype_definition>s
  is performed when the type definition is elaborated.
  The evaluation of expressions that appear as
  @nt<default_expression>s is postponed until an object is created.
  Expressions in representation items
  that appear within a composite type definition are evaluated
  according to the rules of the particular representation item.
@end(Discussion)
@end{RunTime}

@begin{Notes}
A @nt<component_declaration> with several identifiers is equivalent
to a sequence of single @nt<component_declaration>s, as explained
in @RefSecNum{Object Declarations}.

The @nt<default_expression> of a record component is only
evaluated upon the creation of
a default-initialized object of the record type (presuming
the object has the component, if it is in a @nt<variant_part> @em
see @RefSecNum{Object Declarations}).

The subtype defined by a @nt<component_definition> (see @RefSecNum(Array Types))
has to be a definite subtype.

If a record type does not have a @nt<variant_part>, then the same components
are present in all values of the type.

A record type is limited if it has the reserved word @key[limited]
in its definition, or if any of its components are
limited (see @RefSecNum{Limited Types}).

@PDefn2{Term=[predefined operations],Sec=(of a record type)}
The predefined operations of a record type include membership
tests, qualification, and explicit conversion. If the
record type is nonlimited, they also include
assignment and the predefined equality operators.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
A component of a record can be named with a @nt<selected_component>.
A value of a record can be specified with a @nt<record_aggregate>@Chg{Version=[2],
New=[],Old=[, unless the record type is limited]}.

@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of record type declarations: )
@begin(Example)
@key(type) Date @key(is)
   @key(record)
      Day   : Integer @key(range) 1 .. 31;
      Month : Month_Name;
      Year  : Integer @key(range) 0 .. 4000;
   @key(end) @key(record);

@key(type) Complex @key(is)
   @key(record)
      Re : Real := 0.0;
      Im : Real := 0.0;
   @key(end) @key(record);
@end(Example)

@begin{WideAbove}
@leading@keepnext@i(Examples of record variables: )
@end{WideAbove}
@begin(Example)
Tomorrow, Yesterday : Date;
A, B, C : Complex;

--@RI[ both components of A, B, and C are implicitly initialized to zero ]
@end(Example)
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
The syntax rule for @nt<component_declaration> is modified
to use @nt<component_definition> (instead of
@ntf<component_subtype_definition>). The effect of this change
is to allow the reserved word @key(aliased) before the
@ntf<component_subtype_definition>.

A short-hand is provided for defining a null record type
(and a null record extension), as these will be more common
for abstract root types (and derived types without additional components).

The syntax rule for @nt{record_type_definition} is modified to allow
the reserved words @key{tagged} and @key{limited}.
Tagging is new.
Limitedness is now orthogonal to privateness.
In Ada 83 the syntax implied that limited private was sort of more
private than private.
However, limitedness really has nothing to do with privateness;
limitedness simply indicates the lack of assignment capabilities,
and makes perfect sense for nonprivate types such as record types.
@end{Extend83}

@begin{DiffWord83}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
The syntax rules now allow @Chg{New=[@nt{aspect_clause}s],
Old=[@nt{representation_clause}s]} to appear in a @nt{record_definition}.
This is not a language extension, because @LegalityName@;s prevent all
language-defined representation clauses from appearing there.
However, an implementation-defined @nt{attribute_definition_clause}
could appear there.
The reason for this change is to allow the rules for
@Chg{New=[@nt{aspect_clause}s],Old=[@nt{representation_clause}s]} and
representation pragmas to be as similar as possible.
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Record components can have an anonymous access type.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Limited components can be initialized, so long as the expression is
  one that allows building the object in place (such as an @nt{aggregate} or
  @nt{function_call}).]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0002],ARef=[AI95-00171-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Improved the description of the
  elaboration of per-object constraints.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0009],ARef=[AI95-00137-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Changed representation clauses to
  aspect clauses to reflect that they are used for more than just
  representation.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00318-02]}
  @ChgAdded{Version=[2],Text=[Defined @i{explicitly limited record} type to
  use in other rules.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  An optional @nt{aspect_specification} can be used in a @nt{component_declaration}.
  This is described in @RefSecNum{Aspect Specifications}.]}
@end{Extend2005}



@LabeledSubClause{Variant Parts and Discrete Choices}

@begin{Intro}
A record type with a @nt<variant_part> specifies alternative
lists of components. Each @nt<variant> defines the components
for the value or values of the discriminant
covered by its @nt<discrete_choice_list>.
@begin{Discussion}
@RootDefn{cover a value}
@nt{Discrete_choice_list}s and @nt{discrete_choice}s are
said to @i(cover) values as defined below; which
@nt{discrete_choice_list} covers a value determines
which of various alternatives is chosen. These are used
in @nt{variant_part}s, @nt{array_aggregate}s, and
@nt{case_statement}s.
@end{Discussion}
@end{Intro}

@begin{MetaRules}
The definition of @lquotes@;cover@rquotes@; in this subclause
and the rules about discrete choices
are designed so that they
are also appropriate for array aggregates and case statements.

The rules of this subclause intentionally
parallel those for case statements.
@end{MetaRules}

@begin{Syntax}
@Syn{lhs=<variant_part>,rhs="
   @key{case} @SynI{discriminant_}@Syn2{direct_name} @key{is}
       @Syn2{variant}
      {@Syn2{variant}}
   @key{end} @key{case};"}


@Syn{lhs=<variant>,rhs="
   @key{when} @Syn2{discrete_choice_list} =>
      @Syn2{component_list}"}


@Syn{lhs=<discrete_choice_list>,rhs="@Syn2{discrete_choice} {| @Syn2{discrete_choice}}"}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0153-3],ARef=[AI05-0158-1]}
@Syn{lhs=<discrete_choice>,rhs="@Chg{Version=[3],New=[@Syn2{choice_expression}],Old=[@Syn2{expression}]} | @Chg{Version=[3],New=[@SynI{discrete_}@Syn2{subtype_indication} | @Syn2{range}],Old=[@Syn2{discrete_range}]} | @key{others}"}
@end{Syntax}

@begin{Resolution}
@Defn2{Term=[discriminant], Sec=(of a @nt<variant_part>)}
The @i(discriminant_)@nt{direct_name} shall resolve to denote
a discriminant (called
the @i(discriminant of the @nt<variant_part>)) specified in
the @nt{known_discriminant_part} of the @nt{full_type_declaration}
that contains the @nt{variant_part}.
@PDefn2{Term=[expected type], Sec=(variant_part discrete_choice)}
The expected type for each @nt{discrete_choice} in a @nt<variant> is the type
of the discriminant of the @nt{variant_part}.
@begin{Ramification}
  A @nt<full_type_declaration> with a @nt<variant_part>
  has to have a (new) @nt<known_discriminant_part>;
  the discriminant of the @nt<variant_part> cannot be an
  inherited discriminant.
@end{Ramification}
@end{Resolution}

@begin{Legality}
The discriminant of the @nt{variant_part} shall
be of a discrete type.
  @begin{Ramification}
It shall not be of an access type,
  named or anonymous.@end{ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0153-3]}
The @Chg{Version=[3],New=[@nt{choice_expression}s, @nt{subtype_indication}s,
and @nt{range}s],Old=[@nt{expression}s and @nt{discrete_range}s]} given as
@nt{discrete_choice}s in a @nt{variant_part} shall be static.
The @nt{discrete_choice} @key(others) shall appear alone
in a @nt{discrete_choice_list}, and such a @nt{discrete_choice_list},
if it appears, shall be the last one in the enclosing construct.

@Leading@PDefn2{Term=[cover a value], Sec=(by a @nt{discrete_choice})}
A @nt<discrete_choice> is defined to @i(cover a value) in the
following cases:
@begin(itemize)
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
  A @nt{discrete_choice} that is
  @Chg{Version=[3],New=[a @nt{choice_expression}],Old=[an @nt{expression}]}
  covers a value if
  the value equals the value of
  the @Chg{Version=[3],New=[@nt{choice_expression}],Old=[@nt{expression}]}
  converted to the expected type.

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0153-3],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Text=[A @nt{discrete_choice} that is a
  @nt{subtype_indication} covers all values (possibly none) that belong to the
  subtype and that satisfy the static predicate of the subtype
  (see @RefSecnum{Subtype Predicates}).]}

  @begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0262-1]}
    @ChgAdded{Version=[3],Text=[A dynamic predicate is never allowed in this case
    (for @nt{variant}s, @nt{case_statement}s, and @nt{case_expression}s,
    a subtype with a dynamic
    predicate isn't static and thus isn't allowed in a @nt{discrete_choice},
    and for a choice in an @nt{array_aggregate}, a dynamic predicate
    is explicitly disallowed @em see @RefSecnum{Subtype Predicates}).]}
  @end{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0153-3]}
  A @nt{discrete_choice} that is a
  @Chg{Version=[3],New=[@nt{range}],Old=[@nt{discrete_range}]} covers all values
  (possibly none) that belong to the range.

  The @nt{discrete_choice} @key{others} covers all values of its
  expected type that are not covered by
  previous @nt{discrete_choice_list}s of the same construct.
  @begin(Ramification)
    For @nt{case_statement}s,
    this includes values outside the range of the static subtype (if any)
    to be covered by the choices.
    It even includes values outside the base
    range of the case expression's type,
    since values of numeric types (and undefined values of any scalar type?)
    can be outside their base range.
  @end(Ramification)
@end(itemize)

@PDefn2{Term=[cover a value], Sec=(by a @nt{discrete_choice_list})}
A @nt{discrete_choice_list} covers a value if one of
its @nt{discrete_choice}s covers the value.

@Leading@keepnext@;The possible values of the discriminant of a @nt{variant_part}
shall be covered as follows:
@begin{itemize}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0153-3],ARef=[AI05-0188-1],ARef=[AI05-0262-1]}
  If the discriminant is of a static constrained scalar
  subtype@Chg{Version=[3],New=[],Old=[,]}
  then@Chg{Version=[3],New=[, except within an instance of a generic
  unit,],Old=[]} each non-@key{others} @nt{discrete_@!choice} shall cover
  only values in that subtype@Chg{Version=[3],New=[ that satisfy its
  predicate],Old=[]}, and each value
  of that subtype @Chg{Version=[3],New=[that
  satisfies its predicate ],Old=[]}shall be covered
  by some @nt{discrete_@!choice} @Redundant[(either explicitly or
  by @key<others>)];

@begin{Reason}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0188-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The exemption for a discriminated type declared in an instance
  allows the following example:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[generic]
   @key[type] T @key[is new] Integer;
@key[package] G @key[is]
   @key[type] Rec (Discrim : T) @key[is record]
      @key[case] Discrim @key[is]
         @key[when] -10 .. -1 =>
            Foo : Float;
         @key[when others] =>
            @key[null];
      @key[end case];
   @key[end record];
@key[end] G;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{package} I @key{is new} G (Natural); -- @Examcom{Legal}]}
@end{Example}
@end{Reason}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
  If the type of the discriminant is a
  descendant of a generic formal scalar type@Chg{Version=[3],New=[,],Old=[]}
  then the @nt{variant_part} shall have an @key{others}
  @nt{discrete_choice};
@begin{Reason}
  The base range is not known statically in this case.
@end{Reason}

  Otherwise,
  each value of the base range of the type of the discriminant shall
  be covered @Redundant[(either explicitly or by @key<others>)].
@end{itemize}

Two distinct @nt{discrete_choice}s of a @nt{variant_part} shall not cover
the same value.

@end{Legality}

@begin{StaticSem}
If the @nt{component_list} of a @nt{variant} is specified by @key(null),
the variant has no components.

@Defn{govern a @nt{variant_part}}
@Defn{govern a @nt{variant}}
The discriminant of a @nt<variant_part> is said to @i(govern) the
@nt<variant_part> and its @nt<variant>s. In addition,
the discriminant of a derived
type governs a @nt<variant_part> and its @nt<variant>s if it
corresponds (see @RefSecNum(Discriminants)) to the
discriminant of the @nt<variant_part>.

@end{StaticSem}

@begin{RunTime}
A record value contains the values of the components of a particular
@nt{variant} only if the
value of the discriminant governing the @nt<variant> is covered
by the @nt{discrete_choice_list} of the @nt{variant}.
This rule applies in turn to any further @nt{variant} that is, itself,
included in the @nt{component_list} of the given @nt{variant}.

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0290-1]}
@ChgAdded{Version=[3],Text=[When an object of a discriminated type @i<T> is
initialized by default, Constraint_Error is raised if no
@nt{discrete_choice_list} of any @nt{variant} of a @nt{variant_part} of @i<T>
covers the value of the discriminant that governs the @nt{variant_part}. When a
@nt{variant_part} appears in the @nt{component_list} of another @nt{variant}
@i<V>, this test is only applied if the value of the discriminant governing
@i<V> is covered by the @nt{discrete_choice_list} of @i<V>.]}
@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is not a @ldquote@;check@rdquote; it cannot
  be suppressed. However, in most cases it is not necessary to generate any code
  to raise this exception. A test is needed (and can fail) in the case where the
  discriminant subtype has a Static_Predicate specified, it also has predicate
  checking disabled, and the discriminant governs a @nt{variant_part} which
  lacks a @key[when others] choice.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The test also could fail for a static discriminant
  subtype with range checking suppressed and the discriminant governs a
  @nt{variant_part} which lacks a @key[when others] choice. But execution is erroneous if a range
  check that would have failed is suppressed (see @RefSecNum{Suppressing Checks}),
  so an implementation does not have to generate code to check this case. (An
  unchecked failed predicate does not cause erroneous execution, so the test is
  required in that case.)]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Like the checks associated with a per-object
  constraint, this test is not made during the elaboration of a
  @nt{subtype_indication}.]}
@end{ImplNote}

@PDefn2{Term=[elaboration], Sec=(variant_part)}
The elaboration of a @nt{variant_part} consists
of the elaboration of the @nt{component_list} of each
@nt{variant} in the order in which they appear.
@end{RunTime}

@begin{Examples}
@Leading@keepnext@i(Example of record type with a variant part: )
@begin(Example)
@key(type) Device @key(is) (Printer, Disk, Drum);
@key(type) State  @key(is) (Open, Closed);

@key(type) Peripheral(Unit : Device := Disk) @key(is)
   @key(record)
      Status : State;
      @key(case) Unit @key(is)
         @key(when) Printer =>
            Line_Count : Integer @key(range) 1 .. Page_Size;
         @key(when) @key(others) =>
            Cylinder   : Cylinder_Index;
            Track      : Track_Number;
         @key(end) @key(case);
      @key(end) @key(record);
@end(Example)

@begin{WideAbove}
@leading@keepnext@i(Examples of record subtypes:)
@end{WideAbove}
@begin(Example)
@key(subtype) Drum_Unit @key(is) Peripheral(Drum);
@key(subtype) Disk_Unit @key(is) Peripheral(Disk);
@end(Example)

@begin{WideAbove}
@leading@keepnext@i(Examples of constrained record variables:)
@end{WideAbove}
@begin(Example)
Writer   : Peripheral(Unit  => Printer);
Archive  : Disk_Unit;
@end(Example)
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
In Ada 83, the discriminant of a @nt{variant_part} is not allowed to
be of a generic formal type.
This restriction is removed in Ada 95; an @key{others} @nt{discrete_choice}
is required in this case.
@end{Extend83}

@begin{DiffWord83}
The syntactic category @ntf{choice} is removed.
The syntax rules for @nt{variant}, @nt{array_aggregate}, and
@nt{case_statement} now use @nt{discrete_choice_list}
or @nt{discrete_choice} instead.
The syntax rule for @nt{record_aggregate} now defines its own syntax
for named associations.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
We have added the term Discrete Choice to the title
since this is where they are talked about.
This is analogous to the name of the subclause
"Index Constraints and Discrete Ranges" in
the @Chg{Version=[3],New=[subclause],Old=[clause]} on Array Types.

The rule requiring that the discriminant denote
a discriminant of the type being defined seems to have been
left implicit in RM83.
@end{DiffWord83}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0158-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}
  Membership tests are no longer allowed as a @nt{discrete_choice}, in
  order that those tests can be expanded to allow multiple tests in a
  single expression without ambiguity. Since a membership test has a
  boolean type, they are very unlikely to be used as a @nt{discrete_choice}.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Subtypes with static predicates can be used in @nt{discrete_choice}s,
  and the coverage rules are modified to respect the predicates.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0188-1]}
  @ChgAdded{Version=[3],Text=[Variants in generic specifications are no
  longer rejected if the subtype
  of the actual type does not include all of the case choices. This probably
  isn't useful, but it is consistent with the treatment of @nt{case_expression}s.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0290-1]}
  @ChgAdded{Version=[3],Text=[Added a test that some @nt{variant} covers the
  value of a discriminant that governs a @nt{variant_part}. This is similar
  to the test that some case limb covers the value of the
  @SynI{Selecting_}@nt{expression} of a @nt{case_statement}. This test cannot
  change the behavior of any nonerroneous Ada 2005 program, so it is not
  an inconsistency.]}
@end{DiffWord2005}
