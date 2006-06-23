@Part(xxx, Root="rat.msm")

@comment($Source: e:\\cvsroot/ARM/Rationale/access.mss,v $)
@comment($Revision: 1.8 $ $Date: 2006/04/04 05:48:59 $)

@LabeledSection{Access types}

@i{This chapter describes various improvements concerning access types for Ada
2005.}

@i{Ada 2005 permits all access types to be access to constant types and to
indicate that null is not an allowed value in all contexts. Anonymous access
types are permitted in more contexts than just as access parameters and
discriminants; they can also be used for variables and all components of
composite types. This further use of access types is of considerable value in
object oriented programming by reducing the need for (unnecessary) explicit
type conversions.}

@i{A further major improvement concerns access to subprogram types which are
now allowed to be anonymous in line with access to object types. This permits
so-called "downward closures" and allows the flexible use of procedures as
parameters of subprograms and thereby avoids excessive use of generic units.}


@LabeledClause{Ada Issues: Access types}

The WG9 guidance document @LocalLink{Target=[R1],Sec=[References],Text={[1]}}
does not specifically mention access
types as an area needing attention. Access types are, of course, more
of a tactical detail than a strategic issue and so this is not surprising.

However, the guidance document strongly emphasizes improvements to
object oriented programming and the use of access types figures highly
in that area. Indeed one of the motivations for changes was to reduce
the number of explicit access type conversions required for OOP.

The guidance document also asks for "improvements that will remedy
shortcomings in Ada". The introduction of anonymous access-to-subprogram
types comes into that category in the minds of many users.

@leading@;The following Ada Issues cover the relevant changes and are described
in detail in this chapter:

@begin[Description]
@begin[Description]@Comment{Second one to indent this}
@AILink{AI=[AI95-00230-01],Text=[230]}@\Generalized use of anonymous access types

@AILink{AI=[AI95-00231-01],Text=[231]}@\Access to constant parameters, null-excluding types

@AILink{AI=[AI95-00254-01],Text=[254]}@\Anonymous access to subprogram types

@AILink{AI=[AI95-00318-02],Text=[318]}@\Limited and anonymous access return types

@AILink{AI=[AI95-00363-01],Text=[363]}@\Eliminating access subtype problems

@AILink{AI=[AI95-00382-01],Text=[382]}@\Current instance rule and anonymous access types

@AILink{AI=[AI95-00384-01],Text=[384]}@\Discriminated type conversion rules

@AILink{AI=[AI95-00385-01],Text=[385]}@\Stand-alone objects of anonymous access types

@AILink{AI=[AI95-00392-01],Text=[392]}@\Prohibit unsafe array conversions

@AILink{AI=[AI95-00402-01],Text=[402]}@\Access discriminants of nonlimited types

@AILink{AI=[AI95-00404-01],Text=[404]}@\Not null and all in access parameters and types

@AILink{AI=[AI95-00406-01],Text=[406]}@\Aliased permitted with anonymous access types

@AILink{AI=[AI95-00409-01],Text=[409]}@\Conformance with access to subprogram types

@AILink{AI=[AI95-00416-01],Text=[416]}@\Access results, accessibility and return statements

@AILink{AI=[AI95-00420-01],Text=[420]}@\Resolution of universal operations in @exam[Standard]

@AILink{AI=[AI95-00423-01],Text=[423]}@\Renaming, null exclusion and formal objects

@end[Description]
@end[Description]

These changes can be grouped as follows.

First, there is a general orthogonalization of the rules regarding
whether the designated type is constant and whether the access subtype
includes null (@AILink{AI=[AI95-00231-01],Text=[231]},
part of @AILink{AI=[AI95-00404-01],Text=[404]},
part of @AILink{AI=[AI95-00423-01],Text=[423]}).

A major change is the ability to use anonymous access types more widely
(@AILink{AI=[AI95-00230-01],Text=[230]},
part of @AILink{AI=[AI95-00318-02],Text=[318]},
@AILink{AI=[AI95-00385-01],Text=[385]},
@AILink{AI=[AI95-00392-01],Text=[392]},
part of @AILink{AI=[AI95-00404-01],Text=[404]},
@AILink{AI=[AI95-00406-01],Text=[406]},
part of @AILink{AI=[AI95-00416-01],Text=[416]},
part of @AILink{AI=[AI95-00420-01],Text=[420]}).
This was found to require some redefinition of the rules regarding
the use of a type name within its own definition
(@AILink{AI=[AI95-00382-01],Text=[382]}). Access discriminants
are now also permitted with nonlimited
types (@AILink{AI=[AI95-00402-01],Text=[402]}).

The introduction of anonymous access-to-subprogram types enables local
subprograms to be passed as parameters to other subprograms
(@AILink{AI=[AI95-00254-01],Text=[254]},
@AILink{AI=[AI95-00409-01],Text=[409]}).
This has been a feature of many other programming languages
for over 40 years and its omission from Ada has always been both surprising
and irritating and forced the excessive use of generics.

Finally there are some corrections to the rules regarding changing
discriminants which prevent attempting to access components of variants
that do not exist (@AILink{AI=[AI95-00363-01],Text=[363]}). There is also a
change to the rules concerning
type conversions and discriminants to make them
symmetric (@AILink{AI=[AI95-00384-01],Text=[384]}).


@LabeledClause{Null exclusion and constant}

In Ada 95, anonymous access types and named access types have unnecessarily
different properties. Furthermore anonymous access types only occur
as access parameters and access discriminants.

@leading@;Anonymous access types in Ada 95 never have null as a value whereas
named access types always have null as a value. Suppose we have the
following declarations
@begin[Example]
@key[type] T @key[is]
   @key[record]
      Component: Integer;
   @key[end record];

@key[type] Ref_T @key[is access] T;
T_Ptr: Ref_T;
@end[Example]

@leading@;Note that @exam[T_Ptr] by default will have the value @key[null].
Now suppose we have a procedure with an access parameter thus
@begin[Example]
@tabset[P42]
@key[procedure] P(A: @key[access] T) is
   X: Integer;
@key[begin]
   X := A.Component;@\-- @examcom[read a component of A]
@\-- @examcom[no check for null in Ada 95]
   ...
@key[end] P;
@end[Example]

In Ada 95 an access parameter such as @exam[A] can never have the
value null and so there is no need to check for null when doing a
dereference such as reading the component @exam[A.Component]. This
is assured by always performing a check when @exam[P] is called. So
calling @exam[P] with an actual parameter whose value is null such
as @exam[P(T_Ptr)] causes @exam[Constraint_Error] to be raised at
the point of call. The idea was that within @exam[P] we would have
more efficient code for dereferencing and dispatching at the cost
of just one check when the procedure is called. Such an access parameter
we now refer to as being of a subtype that excludes null.

@leading@;Ada 2005 extends this idea of access types that exclude null to named
access types as well. Thus we can write@Defn{exclude null}@Defn{null exclusion}
@begin[Example]
@key[type] Ref_NNT @key[is not null access] T;
@end[Example]

In this case an object of the type @exam[Ref_NNT] cannot have the
value null. An immediate consequence is that all such objects should
be explicitly initialized @en they will otherwise be initialized to
null by default and this will raise @exam[Constraint_Error].

@leading@;Since the property of excluding null can now be given explicitly for
named types, it was decided that for uniformity, anonymous access
types should follow the same rule whenever possible. So, if we want
an access parameter such as @exam[A] to exclude null in Ada 2005 then
we have to indicate this in the same way
@begin[Example]
@tabset[P42]
@key[procedure] PNN(A: @key[not null access] T) is
   X: Integer;
@key[begin]
   X := A.Component;@\-- @examcom[read a component of A]
@\-- @examcom[no check for null in 2005]
   ...
@key[end] PNN;
@end[Example]

@leading@keepnext@;This means of course that the original procedure
@begin[Example]
@tabset[P42]
@key[procedure] P(A: @key[access] T) is
   X: Integer;
@key[begin]
   X := A.Component;@\-- @examcom[read a component of A]
@\-- @examcom[check for null in 2005]
   ...
@key[end] P;
@end[Example]

behaves slightly differently in Ada 2005 since @exam[A] is no longer
of a type that excludes null. There now has to be a check when accessing
the component of the record because null is now an allowed value of
@exam[A]. So in Ada 2005, calling @exam[P] with a null parameter results
in @exam[Constraint_Error] being raised within @exam[P] only when
we attempt to do the dereference, whereas in Ada 95 it is always raised
at the point of call.

This is of course technically an incompatibility of an unfortunate
kind. Here we have a program that is legal in both Ada 95 and Ada
2005 but it behaves differently at execution time in that
@exam[Constraint_Error]
is raised at a different place. But of course, in practice if such
a program does raise @exam[Constraint_Error] in this way then it clearly
has a bug and so the difference does not really matter.

Various alternative approaches were considered in order to eliminate
this incompatibility but they all seemed to be ugly and it was felt
that it was best to do the proper thing rather than have a permanent
wart.

@leading@;However the situation regarding controlling access parameters
is somewhat different. Remember that a controlling parameter is a parameter of
a tagged type where the operation is primitive @en that is declared
alongside the tagged type in a package specification (or inherited
of course). Thus consider
@begin[Example]
@tabset[P42]
@key[package] PTT @key[is]
   @key[type] TT @key[is tagged]
      @key[record]
         Component: Integer;
      @key[end record];

   @key[procedure] Op(X: @key[access] TT);@\-- @examcom[primitive operation]
   ...
@key[end] PTT;
@end[Example]

The type @exam[TT] is tagged and the procedure @exam[Op] is a primitive
operation and so the access parameter @exam[X] is a controlling parameter.

In this case the anonymous access (sub)type still excludes null as
in Ada 95 and so null is not permitted as a parameter. The reason
is that controlling parameters provide the tag for dispatching and
null has no tag value. Remember that all controlling parameters have
to have the same tag. We can add @key[not null] to the parameter specification
if we wish but to require it explicitly for all controlling parameters
was considered to be too much of an incompatibility. But in newly
written programs, we should be encouraged to write @key[not null]
explicitly in order to avoid confusion during maintenance.

@leading@;Another rule regarding null exclusion is that a type derived from
a type that excludes null also excludes null. Thus given
@begin[Example]
@key[type] Ref_NNT @key[is not null access] T;
@key[type] Another_Ref_NNT @key[is new] Ref_NNT;
@end[Example]

@leading@;then@exam[ Another_Ref_NNT] also excludes null. On the other hand
if we start with an access type that does not exclude null then a
derived type can exclude null or not thus
@begin[Example]
@key[type] Ref_T @key[is access] T;
@key[type] Another_Ref_T @key[is new] Ref_T;
@key[type] ANN_Ref_T @key[is new not null] Ref_T;
@end[Example]

then@exam[ Another_Ref_T] does not exclude null but @exam[ANN_Ref_T]
does exclude null.

A technical point is that all access types including anonymous access
types in Ada 2005 have null as a value whereas in Ada 95 the anonymous
access types did not. It is only subtypes in Ada 2005 that do not
always have null as a value. Remember that @exam[Ref_NNT] is actually
a first-named subtype.

An important advantage of all access types having null as a value
is that it makes interfacing to C much easier. If a parameter in C
has type @exam[*t] then the corresponding parameter in Ada can have
type @key[access] @exam[T] and if the C routine needs null passed
sometimes then all is well @en this was a real pain in Ada 95.

@leading@;An explicit null exclusion can also be used in object declarations
much like a constraint. Thus we can have
@begin[Example]
@key[type] Ref_Int @key[is access all] Integer;
X: @key[not null] Ref_Int := Some_Integer'Access;
@end[Example]

Note that we must initialize @exam[X] otherwise the default initialization
with @key[null] will raise @exam[Constraint_Error].

@leading@;In some ways null exclusions have much in common with constraints.
We should compare the above with
@begin[Example]
Y: Integer @key[range] 1 .. 10;
...
Y := 0;
@end[Example]

Again @exam[Constraint_Error] is raised because the value is not permitted
for the subtype of @exam[Y]. A difference however is that in the case
of @exam[X] the check is @exam[Access_Check] whereas in the case of
@exam[Y] it is @exam[Range_Check].

@leading@;The fact that a null exclusion is not actually classified as a
constraint is seen by the syntax for subtype_indication which in Ada 2005 is
@begin[Example]
subtype_indication ::= [null_exclusion] subtype_mark [constraint]
@end[Example]

@leading@;An explicit null exclusion can also be used in subprogram
declarations thus
@begin[Example]
@key[function] F(X:@key[ not null] Ref_Int) @key[return not null] Ref_Int;
@key[procedure] P(X: @key[in not null] Ref_Int);
@key[procedure] Q(X: @key[in out not null] Ref_Int);
@end[Example]

@leading@;But a difference between null exclusions and constraints is that
although we can use a null exclusion in a parameter specification we cannot use
a constraint in a parameter specification. Thus
@begin[Example]
@tabset(P49)
@key[procedure] P(X:@key[ in not null ]Ref_Int);@\-- @examcom[legal]
@key[procedure] Q(X: @key[in] Integer @key[range] 1 .. N);@\-- @examcom[illegal]
@end[Example]

But null exclusions are like constraints in that they are both used
in defining subtype conformance and static matching.

@leading@;We can also use a null exclusion with access-to-subprogram types
including protected subprograms.
@begin[Example]
@key[type] F @key[is access function] (X: Float) @key[return] Float;
Fn: @key[not null] F := Sqrt'Access;
@end[Example]

and so on.

A null exclusion can also be used in object and subprogram renamings.
We will consider subprogram renamings here and object renamings in
the next section when we discuss anonymous access types. This is an
area where there is a significant difference between null exclusions
and constraints.

@leading@;Remember that if an entity is renamed then any constraints are
unchanged. We might have
@begin[Example]
@tabset[P49]
@key[procedure] P(X: Positive);
...
@key[procedure] Q(Y: Natural) @key[renames] P;
...
Q(0);@\-- @examcom[raises Constraint_Error]
@end[Example]

The call of @exam[Q] raises @exam[Constraint_Error] because zero is
not an allowed value of @exam[Positive]. The constraint @exam[Natural]
on the renaming is completely ignored (Ada has been like that since
time immemorial).

We would have preferred that this sort of peculiar behaviour did not
extend to null exclusions. However, we already have the problem that
a controlling parameter always excludes null even if it does not say
so. So the rule adopted generally with null exclusions is that "null
exclusions never lie". In other words, if we give a null exclusion
then the entity must exclude null; however, if no null exclusion is
given then the entity might nevertheless exclude null for other reasons
(as in the case of a controlling parameter).

@leading@keepnext@;So consider
@begin[Example]
@tabset[P49]
@key[procedure] P(X: @key[not null access] T);
...
@key[procedure] Q(Y: @key[access] T) @key[renames] P;@\-- @examcom[OK]
...
Q(@key[null]);@\-- @examcom[raises Constraint_Error]
@end[Example]

@leading@;The call of @exam[Q] raises @exam[Constraint_Error] because the
parameter excludes null even though there is no explicit null exclusion in the
renaming. On the other hand (we assume that @exam[X] is not a controlling
parameter)
@begin[Example]
@tabset[P49]
@key[procedure] P(X: @key[access] T);
...
@key[procedure] Q(Y: @key[not null] @key[access] T) @key[renames] P;@\-- @examcom[NO]
@end[Example]

is illegal because the null exclusion in the renaming is a lie.

However, if @exam[P] had been a primitive operation of @exam[T] so
that @exam[X] was a controlling parameter then the renaming with the
null exclusion would be permitted.

@leading@;Care needs to be taken when a renaming itself is used as a primitive
operation. Consider
@begin[Example]
@tabset[P49]
@key[package] P @key[is]
   @key[type] T @key[is tagged] ...
   @key[procedure] One(X: @key[access] T);@\-- @examcom[excludes null]

   @key[package] Inner @key[is]
      @key[procedure] Deux(X: @key[access] T);@\-- @examcom[includes null]
      @key[procedure] Trois(X: @key[not null access] T);@\-- @examcom[excludes null]
   @key[end] Inner;

   @key[use] Inner;

   @key[procedure] Two(X: @key[access] T) @key[renames] Deux;@\-- @examcom[NO]
   @key[procedure] Three(X: @key[access] T) @key[renames] Trois;@\-- @examcom[OK]
   ...
@end[Example]

The procedure @exam[One] is a primitive operation of @exam[T] and
its parameter @exam[X] is therefore a controlling parameter and so
excludes null even though this is not explicitly stated. However,
the declaration of @exam[Two] is illegal. It is trying to be a dispatching
operation of @exam[T] and therefore its controlling parameter @exam[X]
has to exclude null. But @exam[Two] is a renaming of @exam[Deux] whose
corresponding parameter does not exclude null and so the renaming
is illegal. On the other hand the declaration of @exam[Three] is permitted
because the parameter of @exam[Trois] does exclude null.

@leading@;The other area that needed unification concerned @key[constant]. In
Ada 95 a named access type can be an access to constant type rather
than an access to variable type thus
@begin[Example]
@key[type] Ref_CT @key[is access constant] T;
@end[Example]

Remember that this means that we cannot change the value of an object
of type @exam[T] via the access type.

@leading@;Remember also that Ada 95 introduced more general access types
whereas in Ada 83 all access types were pool specific and could only access
values created by an allocator. An access type in Ada 95 can also
refer to any object marked @key[aliased] provided that the access
type is declared with @key[all] thus
@begin[Example]
@key[type] Ref_VT @key[is access all] T;
X: @key[aliased] T;
R: Ref_VT := X'Access;
@end[Example]

@leading@;So in summary, Ada 95 has three kinds of named access types
@begin[Example]
@tabset(P42)
@key[access] T;@\-- @examcom[pool specific only, read & write]
@key[access all] T@\-- @examcom[general, read & write]
@key[access constant] T@\-- @examcom[general, read only]
@end[Example]

@leading@;But in Ada 95, the distinction between variable and constant access
parameters is not permitted. Ada 2005 rectifies this by permitting
@key[constant] with access parameters. So we can write
@begin[Example]
@tabset[P42]
@key[procedure] P(X: @key[access constant] T);@\-- @examcom[legal 2005]
@key[procedure] P(X: @key[access ]T);
@end[Example]

@leading@;Observe however, that @key[all] is not permitted with access
parameters. Ordinary objects can be constant or variable thus
@begin[Example]
C: @key[constant] Integer := 99;
V: Integer;
@end[Example]

and access parameters follow this pattern. It is named access types
that are anomalous because of the need to distinguish pool specific
types for compatibility with Ada 83 and the subsequent need to introduce
@key[all].

@leading@;In summary, Ada 2005 access parameters can take the following four
forms
@begin[Example]
@key[procedure] P1(X: @key[access] T);
@key[procedure] P2(X: @key[access] @key[constant] T);
@key[procedure] P3(X: @key[not null] @key[access] T);
@key[procedure] P4(X: @key[not null] @key[access] @key[constant] T);
@end[Example]

Moreover, as mentioned above, controlling parameters always exclude
null even if this is not stated and so in that case @exam[P1] and
@exam[P3] are equivalent. Controlling parameters can also be constant
in which case @exam[P2] and @exam[P4] are equivalent.

Similar rules apply to access discriminants; thus they can exclude
null and/or be access to constant.


@LabeledClause{Anonymous access types}

As just mentioned, Ada 95 permits anonymous access types only as access
parameters and access discriminants. And in the latter case only for
limited types. Ada 2005 sweeps away these restrictions and permits
anonymous access types quite freely.@Defn{anonymous access type}

@leading@;The main motivation for this change concerns type conversion.
It often happens that we have a type @exam[T] somewhere in a program and later
discover that we need an access type referring to @exam[T] in some
other part of the program. So we introduce
@begin[Example]
@key[type] Ref_T @key[is access] @key[all] T;
@end[Example]

@leading@;And then we find that we also need a similar access type somewhere
else and so declare another access type
@begin[Example]
@key[type] T_Ptr@key[ is access all ]T;
@end[Example]

If the uses of these two access types overlap then we will find that
we have explicit type conversions all over the place despite the fact
that they are really the same type. Of course one might argue that
planning ahead would help a lot but, as we know, programs often evolve
in an unplanned way.

@leading@;A more important example of the curse of explicit type conversion
concerns object oriented programming. Access types feature quite widely
in many styles of OO programming. We might have a hierarchy of geometrical
object types starting with a root abstract type @exam[Object] thus
@begin[Example]
@key[type] Object @key[is abstract];
@key[type] Circle @key[is new] Object @key[with] ...

@key[type] Polygon @key[is new] Object @key[with] ...
@key[type] Pentagon @key[is new] Polygon @key[with] ...

@key[type] Triangle @key[is new] Polygon @key[with] ...
@key[type] Equilateral_Triangle @key[is new] Triangle @key[with] ...
@end[Example]

@leading@;then we might well find ourselves declaring named access types such
as
@begin[Example]
@key[type] Ref_Object @key[is access all] Object'Class;
@key[type] Ref_Circle@key[ is access all] Circle;
@key[type] Ref_Triangle @key[is access all] Triangle'Class;
@key[type] Ref_Equ_Triangle @key[is access all] Equilateral_Triangle;
@end[Example]

@leading@;Conversion between these clearly ought to be permitted in many cases.
In some cases it can never go wrong and in others a run time check is required.
Thus a conversion between a @exam[Ref_Circle] and a @exam[Ref_Object] is always
possible because every value of @exam[Ref_Circle] is also a value of
@exam[Ref_Object] but the reverse is not the case. So we might have
@begin[Example]
@tabset[P42]
RC: Ref_Circle := A_Circle'Access;
RO: Ref_Object;
...
RO := Ref_Object(RC);@\-- @examcom[explicit conversion, no check]
...
RC := Ref_Circle(RO);@\-- @examcom[needs a check]
@end[Example]

However, it is a rule of Ada 95 that type conversions between these
named access types have to be explicit and give the type name. This
is considered to be a nuisance by many programmers because such conversions
are allowed without naming the type in other OO languages. It would
not be quite so bad if the explicit conversion were only required
in those cases where a run time check was necessary.

Moreover, these are trivial (view) conversions since they are all
just pointers and no actual change of value takes place anyway; all
that has to be done is to check that the value is a legal reference
for the target type and in many cases this is clear at compilation.
So requiring the type name is very annoying.

In fact the only conversions between named tagged types (and named
access types) that are allowed implicitly in Ada are conversions to
a class wide type when it is initialized or when it is a parameter
(which is really the same thing).

It would have been nice to have been able to relax the rules in Ada
2005 perhaps by saying that a named conversion is only required when
a run time check is required. However, such a change would have caused
lots of existing programs to become ambiguous.

@leading@;So, rather than meddle with the conversion rules, it was instead
decided to permit the use of anonymous access types in more contexts in Ada
2005. Anonymous access types have the interesting property that they
are anonymous and so necessarily do not have a name that could be
used in a conversion. Thus we can have
@begin[Example]
@tabset(P42)
RC: @key[access] Circle := A_Circle'Access;
RO: @key[access] Object'Class;@\-- @examcom[default null]
...
RO := RC;@\-- @examcom[implicit conversion, no check]
@end[Example]

@leading@keepnext@;On the other hand we cannot write
@begin[Example]
@tabset(P42)
RC := RO;@\-- @examcom[illegal, would need a check]
@end[Example]

because the general rule is that if a tag check is required then the
conversion must be explicit. So typically we will still need to introduce
named access types for some conversions. But checks relating to accessibility
and null exclusions do not require an explicit conversion and so anonymous
access types cause no problems in those areas.

@leading@;The use of null exclusions with anonymous access types is illustrated
by
@begin[Example]
@tabset(P42)
RC: @key[not null access] Circle := A_Circle'Access;
RO: @key[not null access] Object'Class;@\--@examcom[careful]
@end[Example]

The declaration of @exam[RO] is unfortunate because no initial value is given
and the default of null is not permitted and so it will raise
@exam[Constraint_Error]; a worthy compiler will detect this during compilation
and give us a friendly warning.

Note that we never never write @key[all] with anonymous access types.

@leading@;We can of course also use @key[constant]
with anonymous access types. Note carefully the difference between
the following
@begin[Example]
ACT: @key[access constant] T := T1'Access;
CAT: @key[constant access] T := T1'Access;
@end[Example]

@leading@;In the first case @exam[ACT] is a variable and can be used to access
different objects @exam[T1] and @exam[T2] of type @exam[T]. But it cannot be
used to change the value of those objects. In the second case @exam[CAT] is a
constant and can only refer to the object given in its initialization. But we
can change the value of the object that @exam[CAT] refers to. So we have

@begin[Example]
@tabset(P42)
ACT := T2'Access;@\-- @examcom[legal, can assign]
ACT.@key[all] := T2;@\-- @examcom[illegal, constant view]
CAT := T2'Access;@\-- @examcom[illegal, cannot assign]

CAT.@key[all] := T2;@\-- @examcom[legal, variable view]
@end[Example]

@leading@;At first sight this may seem confusing
and consideration was given to disallowing the use of constants such
as @exam[CAT] (but permitting @exam[ACT]
which is probably more useful since it protects the accessed value).
But the lack of orthogonality was considered very undesirable. Moreover
Ada is a left to right language and we are familiar with equivalent
constructions such as
@begin[Example]
@key[type] CT @key[is access constant] T;
ACT: CT;
@end[Example]

@leading@keepnext@;and
@begin[Example]
@key[type] AT @key[ is access] T;
CAT: @key[constant] AT;
@end[Example]

(although the alert reader will note
that the latter is illegal because I have foolishly used the reserved
word @key[at] as an identifier).

@leading@;We can of course also write
@begin[Example]
CACT: @key[constant access constant] T := T1'Access;
@end[Example]

The object @exam[CACT] is then a constant and provides
read-only access to the object @exam[T1]
it refers to. It cannot be changed to refer to another object such
as @exam[T2] nor can the value of
@exam[T1] be changed via @exam[CACT].

@leading@;An object of an anonymous access
type, like other objects, can also be declared as aliased thus
@begin[Example]
X: @key[aliased access] T;
@end[Example]

although such constructions are likely to be used rarely.

@leading@;Anonymous access types can also be used as the components of arrays
and records. In the Introduction we saw that rather than having to
write
@begin[Example]
@key[type] Cell;
@key[type] Cell_Ptr @key[is access] Cell;

@key[type] Cell @key[is]
   @key[record]
      Next: Cell_Ptr;
      Value: Integer;
   @key[end record];
@end[Example]

@leading@keepnext@;we can simply write
@begin[Example]
@key[type] Cell @key[is]
   @key[record]
      Next: @key[access] Cell;
      Value: Integer;
   @key[end record];
@end[Example]

and this not only avoids having to declare the named access type
@exam[Cell_Ptr] but it also avoids the need for the incomplete type declaration
of @exam[Cell].

Permitting this required some changes to a rule regarding the use
of a type name within its own declaration @en the so-called current
instance rule.

The original current instance rule was that within a type declaration
the type name did not refer to the type itself but to the current
object of the type. The following task type declaration illustrates
both a legal and illegal use of the task type name within its own
declaration. It is essentially an extract from a program in Section
18.10 of @LocalLink{Target=[R6],Sec=[References],Text={[6]}} which
finds prime numbers by a multitasking implementation
of the Sieve of Eratosthenes. Each task of the type is associated
with a prime number and is responsible for removing multiples of that
number and for creating the next task when a new prime number is discovered.
It is thus quite natural that the task should need to make a clone
of itself.

@begin[Example]
@tabset[P42]
@key[task type] TT (P: Integer) @key[is]
   ...
@key[end];

@key[type] ATT@key[ is access] TT;

@key[task body] TT @key[is]
   @key[function] Make_Clone(N: Integer) @key[return] ATT @key[is]
   @key[begin]
      @key[return new] TT(N);@\-- @examcom[illegal]
   @key[end] Make_Clone;

   Ref_Clone: ATT;
   ...
@key[begin]
   ...
   Ref_Clone := Make_Clone(N);
   ...
   @key[abort] TT;@\-- @examcom[legal]
   ...
@key[end] TT;
@end[Example]

The attempt to make a slave clone of the task in the function @exam[Make_Clone]
is illegal because within the task type its name refers to the current
instance and not to the type. However, the abort statement is permitted
and will abort the current instance of the task. In this example the
solution is simply to move the function @exam[Make_Clone] outside
the task body.

However, this rule would have prevented the use of the type name @exam[Cell]
to declare the component @exam[Next] within the type @exam[Cell] and
this would have been infuriating since the linked list paradigm is
very common.

In order to permit this the current instance rule has been changed
in Ada 2005 to allow the type name to denote the type itself within
an anonymous access type declaration (but not a named access type
declaration). So the type @exam[Cell] is permitted.

@leading@;Note however that in Ada 2005, the task @exam[TT] still cannot
contain the declaration of the function @exam[Make_Clone]. Although we no
longer need to declare the named type @exam[ATT] since we can now
declare @exam[Ref_Clone] as
@begin[Example]
Ref_Clone: @key[access] TT;
@end[Example]

@leading@keepnext@;and we can declare the function as
@begin[Example]
   @key[function] Make_Clone(N: Integer) @key[return] @key[access] TT @key[is]
   @key[begin]
      @key[return new] TT(N);
   @key[end] Make_Clone;
@end[Example]

where we have an anonymous result type, nevertheless the allocator
@key[new]@exam[ TT] inside@exam[ Make_Clone] remains illegal if
@exam[Make_Clone] is declared within the task body @exam[TT]. But such a use is
unusual and declaring a distinct external function is hardly a burden.

@leading@;To be honest we can simply declare a subtype of a different name
outside the task
@begin[Example]
@key[subtype] XTT @key[is] TT;
@end[Example]

@leading@;and then we can write @key[new] @exam[XTT(N);] in the function and
keep the function hidden inside the task. Indeed we don't need the
function anyway because we can just write
@begin[Example]
Ref_Clone := @key[new] XTT(N);
@end[Example]
in the task body.

@leading@;The introduction of the wider use
of anonymous access types requires some revision to the rules concerning
type comparisons and conversions. This is achieved by the introduction
of a type @examcom{universal_access} by analogy with the types
@examcom{universal_integer} and @examcom{universal_real}. Two new equality
operators are defined in the package @exam[Standard] thus
@begin[Example]
@key[function] "=" (Left, Right: @examcom[universal_access]) @key[return] Boolean;

@key[function] "/=" (Left, Right: @examcom[universal_access]) @key[return] Boolean;
@end[Example]

The literal @key[null] is now deemed to be of type @i{universal_access}
and appropriate conversions are defined as well. These new operations
are only applied when at least one of the arguments is of an anonymous
access type (not counting @key[null]).

@leading@;Interesting problems arise if we define our own equality operation.
For example, suppose we wish to do a deep comparison on two lists
defined by the type @exam[Cell]. We might decide to write a recursive
function with specification
@begin[Example]
@key[function] "=" (L, R: @key[access] Cell) @key[return] Boolean;
@end[Example]

@leading@;Note that it is easier to use access parameters rather than
parameters of type @exam[Cell] itself because it then caters naturally for
cases where null is used to represent an empty list. We might attempt to write
the body as
@begin[Example]
@tabset[P42]
@key[function] "=" (L, R: @key[access] Cell) @key[return] Boolean @key[is]
@key[begin]
   @key[if] L = @key[null or] R = @key[null then]@\-- @examcom[wrong =]
      @key[return] L = R;@\-- @examcom[wrong =]
   @key[elsif] L.Value = R.Value @key[then]
      @key[return] L.Next = R.Next;@\-- @examcom[recurses OK]
   @key[else]
      @key[return] False;
   @key[end if];
@key[end] "=" ;
@end[Example]

But this doesn't work because the calls of @exam["="] in the first
two lines recursively call the function being declared whereas we
want to call the predefined @exam["="] in these cases.

@leading@;The difficulty is overcome by writing@exam[ Standard."="] thus
@begin[Example]
   @key[if] Standard."=" (L, @key[null]) @key[or] Standard."=" (R, @key[null]) @key[then]
      @key[return] Standard."=" (L, R);
@end[Example]

The full rules regarding the use of the predefined equality are that
it cannot be used if there is a user-defined primitive equality operation
for either operand type unless we use the prefix @exam[Standard].
A similar rule applies to fixed point types as we shall see in a later
chapter (see @RefSecNum{Numerics}).

@leading@;Another example of the use of the type @exam[Cell] occurred in the
previous chapter (see @RefSecNum{Nested type extension}) when we were discussing
type extension at nested levels.
That example also illustrated that access types have to be named in
some circumstances such as when they provide the full type for a private
type. We had
@begin[Example]
@tabset[P42]
@key[package] Lists @key[is]
   @key[type] List @key[is limited private];@\-- @examcom[private type]
   ...
@key[private]
   @key[type] Cell @key[is]
      @key[record]
         Next: @key[access] Cell;@\-- @examcom[anonymous type]
         C: Colour;
      @key[end record];

   @key[type] List @key[is access] Cell;@\-- @examcom[full type]
@key[end];

@key[package body] Lists @key[is]
   @key[procedure] Iterate(IC: @key[in] Iterator'Class; L: @key[in] List) @key[is]
      This: @key[access] Cell := L;@\-- @examcom[anonymous type]
   @key[begin]
      @key[while] This /= @key[null loop]
         IC.Action(This.C);@\-- @examcom[dispatches]
         This := This.Next;
      @key[end loop];
   @key[end] Iterate;
@key[end] Lists;
@end[Example]

In this case we have to name the type @exam[List] because it is a
private type. Nevertheless it is convenient to use an anonymous access
type to avoid an incomplete declaration of @exam[Cell].

@leading@;In the procedure @exam[Iterate] the local variable @exam[This] is
also of an anonymous type. It is interesting to observe that if @exam[This]
had been declared to be of the named type @exam[List] then we would
have needed an explicit conversion in
@begin[Example]
@tabset[P42]
         This := List(This.Next); @\-- @examcom[explicit conversion]
@end[Example]

Remember that we @i[always] need an explicit conversion when converting
to a named access type. There is clearly an art in using anonymous
types to best advantage.

The Introduction showed a number of other uses of anonymous access
types in arrays and records and as function results when discussing
Noah's Ark and other animal situations. We will now turn to more weighty
matters.

An important matter in the case of access types is accessibility.
The accessibility rules are designed to prevent dangling references.
The basic rule is that we cannot create an access value if the object
referred to has a lesser lifetime than the access type.

@leading@;However there are circumstances where the rule is unnecessarily
severe and that was one reason for the introduction of access parameters.
Perhaps some recapitulation of the problems would be helpful. Consider
@begin[Example]
@tabset[P42]
@key[type] T @key[is] ...
Global: T;
@key[type] Ref_T @key[is access all] T;
Dodgy: Ref_T;

@key[procedure] P(Ptr: @key[access] T) is
@key[begin]
   ...
   Dodgy := Ref_T(Ptr);@\-- @examcom[dynamic check]
@key[end] P;

@key[procedure] Q(Ptr: Ref_T) @key[is]
@key[begin]
   ...
   Dodgy := Ptr;@\-- @examcom[legal]
@key[end] Q;
...
@key[declare]
   X: @key[aliased] T;
@key[begin]
   P(X'Access);@\-- @examcom[legal]
   Q(X'Access);@\-- @examcom[illegal]
@key[end];
@end[Example]

Here we have an object @exam[X] with a short lifetime and we must
not squirrel away an access referring to @exam[X] in an object with
a longer lifetime such as @exam[Dodgy]. Nevertheless we want to manipulate
@exam[X] indirectly using a procedure such as @exam[P].

If the parameter were of a named type such as @exam[Ref_T] as in the
case of the procedure @exam[Q] then the call would be illegal since
within @exam[Q] we could then assign to a variable such as @exam[Dodgy]
which would then retain the "address" of @exam[X] after @exam[X] had
ceased to exist.

However, the procedure @exam[P] which uses an access parameter permits
the call. The reason is that access parameters carry dynamic accessibility
information regarding the actual parameter. This extra information
enables checks to be performed only if we attempt to do something
foolish within the procedure such as make an assignment to @exam[Dodgy].
The conversion to the type @exam[Ref_T] in this assignment fails dynamically
and disaster is avoided.

@leading@keepnext@;But note that if we had called @exam[P] with
@begin[Example]
P(Global'Access);
@end[Example]

where @exam[Global] is declared at the same level as @exam[Ref_T]
then the assignment to @exam[Dodgy] would be permitted.

The accessibility rules for the new uses of anonymous access types
are very simple. The accessibility level is simply the level of the
enclosing declaration and no dynamic information is involved. (The
possibility of preserving dynamic information was considered but this
would have led to inefficiencies at the points of use.)

@leading@keepnext@;In the case of a stand-alone variable such as
@begin[Example]
V: @key[access] Integer;
@end[Example]

@leading@keepnext@;then this is essentially equivalent to
@begin[Example]
@key[type] @examcom[anon] @key[is access all] Integer;
V: @examcom[anon];
@end[Example]

@leading@;A similar situation applies in the case of a component of a record
or array type. Thus if we have
@begin[Example]
@key[type] R @key[is]
   @key[record]
      C: @key[access] Integer;
      ...
   @key[end record];
@end[Example]

@leading@keepnext@;then this is essentially equivalent to
@begin[Example]
@key[type] @examcom[anon] @key[is access all] Integer;
@key[type] R @key[is]
   @key[record]
      C: @examcom[anon];
      ...
   @key[end record];
@end[Example]

@leading@;Further if we now declare a derived type then there is no new
physical access definition, and the accessibility level is that of the original
declaration. Thus consider
@begin[Example]
@tabset[P42]
@key[procedure] Proc @key[is]
   Local: @key[aliased] Integer;
   @key[type] D @key[is new] R;
   X: D := D'(C => Local'Access, ... ); @\-- @examcom[illegal]
@key[begin]
   ...
@key[end] Proc;
@end[Example]

In this example the accessibility level of the component @exam[C]
of the derived type is the same as that of the parent type @exam[R
]and so the aggregate is illegal. This somewhat surprising rule is
necessary to prevent some very strange problems which we will not
explore here.

@leading@;One consequence of which users should be aware is that if we assign
the value in an access parameter to a local variable of an anonymous
access type then the dynamic accessibility of the actual parameter
will not be held in the local variable. Thus consider again the example
of the procedure @exam[P] containing the assignment to @exam[Dodgy]
@begin[Example]
@tabset[P42]
@key[procedure] P(Ptr: @key[access] T) @key[is]
@key[begin]
   ...
   Dodgy := Ref_T(Ptr);@\-- @examcom[dynamic check]
@key[end] P;
@end[Example]

@leading@;and this variation in which we have introduced a local variable of
an anonymous access type
@begin[Example]
@tabset[P42]
@key[procedure] P1(Ptr: @key[access] T) @key[is]
   Local_Ptr: @key[access] T;
@key[begin]
   ...
   Local_Ptr := Ptr;@\-- @examcom[implicit conversion]
   Dodgy := Ref_T(Local_Ptr);@\-- @examcom[static check, illegal]
@key[end] P1;
@end[Example]

Here we have copied the value in the parameter to a local variable
before attempting the assignment to @exam[Dodgy]. (Actually it won't
compile but let us analyze it in detail anyway.)

The conversion in @exam[P] using the access parameter @exam[Ptr] is
dynamic and will only fail if the actual parameter has an accessibility
level greater than that of the type @exam[Ref_T]. So it will fail
if the actual parameter is @exam[X] and so raise @exam[Program_Error]
but will pass if it has the same level as the type @exam[Ref_T] such
as the variable @exam[Global].

In the case of @exam[P1], the assignment from @exam[Ptr] to @exam[Local_Ptr]
involves an implicit conversion and static check which always passes.
(Remember that implicit conversions are never allowed if they involve
a dynamic check.) However, the conversion in the assignment to @exam[Dodgy]
in @exam[P1] is also static and will always fail no matter whether
@exam[X] or @exam[Global] is passed as actual parameter.

So the effective behaviours of @exam[P] and@exam[ P1] are the same
if the actual parameter is @exam[X] (they both fail, although one
dynamically and the other statically) but will be different
if the actual parameter has the same level as the type @exam[Ref_T]
such as the variable @exam[Global]. The assignment to @exam[Dodgy]
in @exam[P] will work in the case of @exam[Global] but the assignment
to @exam[Dodgy] in @exam[P1] never works.

This is perhaps surprising, an apparently innocuous intermediate assignment
has a significant effect because of the implicit conversion and the
consequent loss of the accessibility information. In practice this
is very unlikely to be a problem. In any event programmers are aware
that access parameters are special and carry dynamic information.

@leading@;In this particular example the loss of the accessibility information
through the use of the intermediate stand-alone variable is detected
at compile time. More elaborate examples can be constructed whereby
the problem only shows up at execution time. Thus suppose we introduce
a third procedure @exam[Agent] and modify @exam[P] and @exam[P1] so
that we have
@begin[Example]
@tabset[P42]
@key[procedure] Agent(A: @key[access] T) @key[is]
@key[begin]
   Dodgy := Ref_T(A);@\-- @examcom[dynamic check]
@key[end] Agent;

@key[procedure] P(Ptr: @key[access] T) @key[is]
@key[begin]
   Agent(Ptr);@\-- @examcom[may be OK]
@key[end] P;

@key[procedure] P1(Ptr: @key[access] T) @key[is]
   Local_Ptr: @key[access] T;
@key[begin]
   Local_Ptr := Ptr;@\-- @examcom[implicit conversion ]
   Agent(Local_Ptr);@\-- @examcom[never OK]
@key[end] P1;
@end[Example]

Now we find that @exam[P] works much as before. The accessibility
level passed into @exam[P] is passed to @exam[Agent] which then carries
out the assignment to @exam[Dodgy]. If the parameter passed to @exam[P]
is the local @exam[X] then @exam[Program_Error] is raised in @exam[Agent]
and propagated to @exam[P]. If the parameter passed is @exam[Global]
then all is well.

The procedure @exam[P1] now compiles whereas it did not before. However,
because the accessibility of the original parameter is lost by the
assignment to @exam[Local_Ptr], it is the accessibility level of @exam[Local_Ptr]
that is passed to @exam[Agent] and this means that the assignment
to @exam[Dodgy] always fails and raises @exam[Program_Error] irrespective
of whether @exam[P1] was called with @exam[X] or @exam[Global].

@leading@;If we just want to use another name for some reason then we can avoid
the loss of the accessibility level by using renaming. Thus we could have
@begin[Example]
@tabset[P42]
@key[procedure] P2(Ptr: @key[access] T) @key[is]
   Local_Ptr: @key[access] T @key[renames] Ptr;
@key[begin]
   ...
   Dodgy := Ref_T(Local_Ptr);@\-- @examcom[dynamic check]
@key[end] P2;
@end[Example]

and this will behave exactly as the original procedure @exam[P].

As usual a renaming just provides another view of the same entity
and thus preserves the accessibility information.

@leading@keepnext@;A renaming can also include @key[not null] thus
@begin[Example]
Local_Ptr: @key[not null access] T @key[renames] Ptr;
@end[Example]

Remember that not null must never lie so this is only legal if @exam[Ptr]
is indeed of a type that excludes null (which it will be if @exam[Ptr]
is a controlling access parameter of the procedure @exam[P2]).

@leading@;A renaming might be useful when the accessed type @exam[T] has
components that we wish to refer to many times in the procedure. For example
the accessed type might be the type @exam[Cell] declared earlier in which case
we might usefully have
@begin[Example]
Next: @key[access] Cell @key[renames] Ptr.Next;
@end[Example]

and this will preserve the accessibility information.

@leading@;Anonymous access types can also be used as the result of a function.
In the Introduction we had
@begin[Example]
@key[function] Mate_Of(A: @key[access] Animal'Class) @key[return] @key[access] Animal'Class;
@end[Example]

The accessibility level of the result in this case is the same as
that of the declaration of the function itself.

@leading@;We can also dispatch on the result of a function if the result is
an access to a tagged type. Consider
@begin[Example]
@key[function] Unit @key[return access] T;
@end[Example]

We can suppose that @exam[T] is a tagged type representing some category
of objects such as our geometrical objects and that @exam[Unit] is
a function returning a unit object such as a circle of unit radius
or a triangle with unit side.

@leading@keepnext@;We might also have a function
@begin[Example]
@key[function] Is_Bigger(X, Y: @key[access] T) @key[return] Boolean;
@end[Example]

@leading@keepnext@;and then
@begin[Example]
Thing: @key[access] T'Class := ... ;
...
Test: Boolean := Is_Bigger(Thing, Unit);
@end[Example]

This will dispatch to the function @exam[Unit] according to the tag
of @exam[Thing] and then of course dispatch to the appropriate function@exam[
Is_Bigger].

@leading@;The function @exam[Unit] could also be used as a default value for
a parameter thus
@begin[Example]
@key[function] Is_Bigger(X: @key[access] T; Y: @key[access] T := Unit)
@key[return] Boolean;
@end[Example]

Remember that a default used in such a construction has to be tag
indeterminate.

Permitting anonymous access types as result types eliminates the need
to define the concept of a "return by reference" type. This was a
strange concept in Ada 95 and primarily concerned limited types (including
task and protected types) which of course could not be copied. Enabling
us to write @key[access] explicitly and thereby tell the truth removes
much confusion. Limited types will be discussed in detail in a later
chapter (see @RefSecNum{Limited types and return statements}).

@leading@;Access return types can be a convenient way of getting a constant
view of an object such as a table. We might have an array in a package
body (or private part) and a function in the specification thus
@begin[Example]
@key[package] P @key[is]
   @key[type] Vector @key[is array] (Integer @key[range] <>) of Float;

   @key[function] Read_Vec @key[return access constant] Vector;
   ...
@key[private]

@key[end];

@key[package body] P @key[is]

   The_Vector: @key[aliased] Vector :=   ;

   @key[function] Read_Vec @key[return access constant] Vector @key[is]
   @key[begin]
      @key[return] The_Vector'Access;
   @key[end];
   ...
@key[end] P;
@end[Example]

@leading@keepnext@;We can now write
@begin[Example]
@tabset(P42)
X := Read_Vec(7);@\-- @examcom[read element of array]
@end[Example]

@leading@keepnext@;This is strictly short for
@begin[Example]
X := Read_Vec.@key[all](7);
@end[Example]

@leading@keepnext@;Note that we cannot write
@begin[Example]
@tabset(P42)
Read_Vec(7) := Y;@\-- @examcom[illegal]
@end[Example]

although we could do so if we removed @key[constant] from the return
type (in which case we should use a different name for the function).

@leading@;The last new use of anonymous access types concerns discriminants.
Remember that a discriminant can be of a named access type or an anonymous
access type (as well as oher things). Discriminants of an anonymous
access type are known as access discriminants. In Ada 95, access discriminants
are only allowed with limited types. Discriminants of a named access
type are just additional components with no special properties. But
access discriminants of limited types are special. Since the type
is limited, the object cannot be changed by a whole record assignment
and so the discriminant cannot be changed even if it has defaults.
Thus
@begin[Example]
@key[type] Minor @key[is] ...

@key[type] Major(M: @key[access] Minor) @key[is limited]
   @key[record]
      ...
   @key[end record];

Small: @key[aliased] Minor;
Large: Major(Small'Access);
@end[Example]

The objects @exam[Small] and @exam[Large] are now bound permanently
together.

@leading@;In Ada 2005, access discriminants are also allowed for nonlimited
types. However, defaults are not permitted so that the discriminant
cannot be changed so again the objects are bound permanently together.
An interesting case arises when the discriminant is provided by an
allocator thus
@begin[Example]
Larger: Major(@key[new] Minor( ... ));
@end[Example]

In this case we say that the allocated object is a coextension of
@exam[Larger]. Coextensions have the same lifetime as the major object
and so are finalized when it is finalized. There are various accessibility
and other rules concerning objects which have coextensions which prevent
difficulty when returning such objects from functions.@Defn{coextension}


@LabeledClause{Downward closures}

This section is really about access to subprogram types in general
but the title downward closures has come to epitomize the topic.

The requirements for Ada 83, (Strawman .. Steelman) were strangely
silent about whether parameters of subprograms could themselves be
subprograms as was the case in Algol 60 and Pascal. Remember that
Pascal was one of the languages on which the designs for the DoD language
were to be based.

The predictability aspects of the requirements were interpreted as
implying that all subprogram calls should be identified at compilation
time on the grounds that if you didn't know what was being called
than you couldn't know what the program was going to do. This was
a particularly stupid attitude to take. The question of predictability
(presumably in some safety or security context) really concerns the
behaviour of particular programs rather than the universe of all programs
that can be constructed in a language.

In any event the totality of subprograms that might be called in a
program is finite and closed. It simply consists of the subprograms
in the program. Languages such as Ada are not able to construct totally
new subprograms out of lesser components in the way that they can
create say floating point values.

So the world had to use generics for many applications that were natural
for subprograms as parameters of other subprograms. Thankfully many
implementers avoided the explosion that might occur with generics
by clever code sharing which in a sense hid the parameterization behind
the scenes.

The types of applications for which subprograms are natural as parameters
are any where one subroutine is parameterized by another. They include
many mathematical applications such as integration and maximization
and more logical applications such as sorting and searching and iterating.

As outlined in the Introduction, the matter was partly improved in
Ada 95 by the introduction of named access-to-subprogram types. This
was essentially done to allow program call back to be implemented.

Program call back is when one program passes the "address" of a subprogram
within it to another program so that this other program can later
respond by calling back to the first program using the subprogram
address supplied. This is often used for communication between an
Ada application program and some other software such as an operating
system which might even be written in another language such as C.

@leading@;Named access to subprogram types certainly work for call back
(especially
with languages such as C that do not have nested subprograms) but
the accessibility rules which followed those for general access to
object types were restrictive. For example, suppose we have a general
library level function for integration using a named access to subprogram
type to pass the function to be integrated thus
@begin[Example]
@key[type] Integrand @key[is access function] (X: Float) @key[return] Float;

@key[function] Integrate(Fn: Integrand; Lo, Hi: Float) @key[return] Float;
@end[Example]

@leading@;then we cannot even do the simplest integration of our own function
in a natural way. For example, suppose we wish to integrate a function
such as @exam[Exp(X**2)]. We can try
@begin[Example]
@tabset[P42]
@key[with] Integrate;
@key[procedure] Main @key[is]
   @key[function] F(X: Float) @key[return] Float @key[is]
   @key[begin]
      @key[return] Exp(X**2);
   @key[end] F;

   Result, L, H: Float;
@key[begin]
   ...        -- @examcom[set bounds in L and H say]
   Result := Integrate(F'Access, L, H);@\-- @examcom[illegal in 95]
   ...
@key[end] Main;
@end[Example]

@leading@;But this is illegal because of the accessibility check necessary to
prevent us from writing something like
@begin[Example]
@tabset[P42]
Evil: Integrand;
X: Float;
...
@key[declare]
   Y: Float;
   @key[function] F(X: Float) @key[return] Float @key[is]
      ...
      Y := X;@\--@examcom[assign to Y in local block]
      ...
   @key[end] F;
@key[begin]
   Evil := F'Access:@\-- @examcom[illegal]
@key[end];
   X := Evil(X);@\-- @examcom[call function out of context]
@end[Example]

Here we have attempted to assign an access to the local function @exam[F]
in the global variable @exam[Evil]. If this assignment had been permitted
then the call of @exam[Evil] would indirectly have called the function
@exam[F] when the context in which @exam[F] was declared no longer
existed;@exam[ F] would then have attempted to assign to the variable
@exam[Y] which no longer existed and whose storage space might now
be used for something else. We can summarise this perhaps by saying
that we are attempting to call @exam[F] when it no longer exists.

Ada 2005 overcomes the problem by introducing anonymous access to
subprogram types. This was actually considered during the design of
Ada 95 but it was not done at the time for two main reasons. Firstly,
the implementation problems for those who were using display vectors
rather than static links were considered a hurdle. And secondly, a
crafty technique was available using the newly introduced tagged types.
And of course one could continue to use generics. But further thought
showed that the implementation burden was not so great after all and
nobody understood the tagged type technique which was really incredibly
contorted. Moreover, the continued use of generics when other languages
forty years ago had included a more natural mechanism was tiresome.
So at long last Ada 2005 includes anonymous access to subprogram types.

@leading@keepnext@;We rewrite the integration function much as follows
@begin[Example]
@tabset[P42]
@key[function] Integrate(
         Fn: @key[access function] (X: Float) @key[return] Float;
         Lo, Hi: Float) @key[return] Float @key[is]
   Total: Float;
   N: @key[constant] Integer := ... ;@\-- @examcom[no of subdivisions]
   Step: Float := (Hi @en Lo) / Float(N);
   X: Float := Lo; @\-- @examcom[current point]
@key[begin]
   Total := 0.5 * Fn(Lo);@\-- @examcom[value at low bound]
   @key[for] I @key[in] 1 .. N@en@;1 @key[loop]
      X := X + Step;@\-- @examcom[add values at]
      Total := Total + Fn(X);@\-- @examcom[intermediate points]
   @key[end loop];
   Total := Total + 0.5 * Fn(Hi);@\-- @examcom[add final value]
   @key[return] Total * Step;@\-- @examcom[normalize]
@key[end] Integrate;
@end[Example]

The important thing to notice is the profile of @exam[Integrate] in
which the parameter @exam[Fn] is of an anonymous access to subprogram
type. We have also shown a simple body which uses the trapezium/trapezoid
method and so calls the actual function corresponding to @exam[Fn]
at the two end points of the range and at a number of equally spaced
intermediate points.

(NB It is time for a linguistic interlude. Roughly speaking English
English trapezium equals US English trapezoid. They both originate
from the Greek @Comment{Hex: 03C4; 03C1; 03B1; 03C0; 03B5; 03B6;
03B1}@Unicode(964)@Unicode(961)@Unicode(945)@Unicode(960)@Unicode(949)@Unicode(950)@Unicode(945)
meaning a table
(literally with four feet). Both originally meant a quadrilateral
with no pairs of sides parallel. In the late 17th century, trapezium
came to mean having one pair of sides parallel. In the 18th century
trapezoid came to mean the same as trapezium but promptly faded out
of use in England whereas in the US it continues in use. Meanwhile
in the US, trapezium reverted to its original meaning of totally irregular.
Trapezoid is rarely used in the UK but if used has reverted to its
original meaning of totally irregular. A standard language would be
useful. Anyway, the integration is using quadrilateral strips with
one pair of sides parallel.)

With this new declaration of @exam[Integrate], the accessibility problems
are overcome and we are allowed to write @exam[Integrate(F'Access,
... )] just as we could write@exam[ P(X'Access)] in the example in
the previous section where we discussed anonymous access to object
types.

We still have to consider how a type conversion which would permit
an assignment to a global variable is prevented. The following text
illustrates both access to object and access to subprogram parameters.

@begin[Example]
@tabset[P42]
@key[type] AOT @key[is access all] Integer;
@key[type] APT @key[is access procedure] (X: in out Float);

Evil_Obj: AOT;
Evil_Proc: APT;

@key[procedure] P(
              Objptr: @key[access] Integer;
              Procptr: @key[access procedure] (X: @key[in out] Float)) @key[is]
@key[begin]
   Evil_Obj := AOT(Objptr);@\-- @examcom[fails at run time]
   Evil_Proc := APT(Procptr);@\-- @examcom[fails at compile time]
@key[end] P;

@key[declare]
   An_Obj: @key[aliased] Integer;
   @key[procedure] A_Proc(X:@key[ in out] Float) @key[is]
   @key[begin] ... @key[end] A_Proc;
@key[begin]
   P(An_Obj'Access, A_Proc'Access);@\-- @examcom[legal]
@key[end];

Evil_Obj.@key[all] := 0;@\-- @examcom[assign to nowhere]
Evil_Proc.@key[all]( ... );@\-- @examcom[call nowhere]
@end[Example]

This repeats some of the structure of the previous section. The procedure
@exam[P] has an access to object parameter @exam[Objptr] and an access
to subprogram parameter @exam[Procptr]; they are both of anonymous
type. The call of @exam[P] in the local block passes the addresses
of a local object @exam[An_Obj] and a local procedure @exam[A_Proc]
to @exam[P]. This is permitted. We now attempt to assign the parameter
values from within @exam[P] to global objects @exam[Evil_Obj ]and
@exam[Evil_Proc] with the intent of assigning indirectly via @exam[Evil_Obj]
and calling indirectly via @exam[Evil_Proc] after the object and procedure
referred to no longer exist.

Both of these wicked deeds are prevented by the accessibility rules.

In the case of the object parameter @exam[Objptr] it knows the accessibility
level of the actual @exam[An_Obj] and this is seen to be greater than
that of the type @exam[AOT] and so the conversion is prevented at
run time and in fact @exam[Program_Error] is raised. But if @exam[An_Obj]
had been declared at the same level as @exam[AOT] and not within an
inner block then the conversion would have been permitted.

However, somewhat different rules apply to anonymous access to subprogram
parameters. They do not carry an indication of the accessibility level
of the actual parameter but simply treat it as if it were infinite
(strictly @en deeper than anything else). This of course prevents
the conversion to the type @exam[APT] and all is well; this is detected
at compile time. But note that if the procedure @exam[A_Proc] had
been declared at the same level as @exam[APT] then the conversion
would still have failed because the accessibility level is treated
as infinite.

There are a number of reasons for the different treatment of anonymous
access to subprogram types. A big problem is that named access to
subprogram types are implemented in the same way as C *func in almost
all compilers. Permitting the conversion from anonymous access to
subprogram types to named ones would thus have caused problems because
that model does not work especially for display based implementations.
Carrying the accessibility level around would not have prevented these
conversions. The key goal was simply to provide a facility corresponding
to that in Pascal and not to encourage too much fooling about with
access to subprogram types. Recall that the attribute @exam[Unchecked_Access]
is permitted for access to object types but was considered far too
dangerous for access to subprogram types for similar reasons.

@leading@;The reader may be feeling both tired and that there are other ways
around the problems of accessibility anyway. Thus the double integration
presented in the Introduction can easily be circumvented in many cases.
We computed
@Comment{This is better displayed as a graphic; a lot of systems won't have these characters.
@begin{Example}@Roman{@grow{@grow{@Unicode(8992)}@+{1}@grow{@Unicode(8992)}@+{1}
@grow{@Unicode(9474)} @grow{@Unicode(9474)}  xy @i{dy dx}
@grow{@Unicode(8993)}@-{0}@grow{@Unicode(8993)}@-{0}}}
@end{Example}}
@PictureAlone{Alignment=[Left],Border=[None],Height=[65],Width=[130],
Name=[form-2.png],Descr=[Integrate xy]}

@leading@;using the following program@Comment{No keepnext here, Word keeps the graphic with this, which can be too big.}
@begin[Example]
@key[with] Integrate;
@key[procedure] Main @key[is]
   @key[function] G(X: Float) @key[return] Float @key[is]
      @key[function] F(Y: Float) @key[return] Float @key[is]
      @key[begin]
         @key[return] X*Y;
      @key[end] F;
   @key[begin]
      @key[return] Integrate(F'Access, 0.0, 1.0);
   @key[end] G;

   Result: Float;
@key[begin]
   Result:= Integrate(G'Access, 0.0, 1.0);
   ...
@key[end] Main;
@end[Example]

@leading@;The essence of the problem was that @exam[F] had to be declared
inside @exam[G] because it needed access to the parameter @exam[X] of @exam[G].
But the astute reader will note that this example is not very convincing
because the integrals can be separated and the functions both declared
at library level thus
@begin[Example]
@key[function] F(Y: Float) @key[return] Float @key[is]
@key[begin]
   @key[return] Y;
@key[end] F;

@key[function] G(X: Float) @key[return] Float @key[is]
@key[begin]
   @key[return] X;
@key[end] G;

Result:= Integrate(F'Access, 0.0, 1.0) * Integrate(G'Access, 0.0, 1.0);
@end[Example]

and so it all works using the Ada 95 version of @exam[Integrate] anyway.

@leading@;However, if the two integrals had been more convoluted or perhaps
the region had not been square but triangular so that the bound of
the inner integral depended on the outer variable as in

@Comment{This is better displayed as a graphic; a lot of systems won't have these characters.
@begin{Example}
@Roman{@grow{@grow{@Unicode(8992)}@+{1}@grow{@Unicode(8992)}@+{x}
@grow{@Unicode(9474)} @grow{@Unicode(9474)}  xy @i{dy dx}
@grow{@Unicode(8993)}@-{0}@grow{@Unicode(8993)}@-{0}}}
@end{Example}}
@PictureAlone{Alignment=[Left],Border=[None],Height=[65],Width=[130],
Name=[form-3.png],Descr=[Integrate xy]}

then nested functions would be vital.

We will now consider a more elegant example which illustrates how
we might integrate an arbitrary function of two variables @i[F](@i[x],
@i[y]) over a rectangular region.

@leading@;Assume that we have the function @exam[Integrate] for one dimension
as before
@begin[Example]
@key[function] Integrate(
          Fn: @key[access function](X: Float) @key[return] Float;
          Lo, Hi: Float) @key[return] Float;
@end[Example]

@leading@keepnext@;Now consider
@begin[Example]
@key[function] Integrate(
          Fn: @key[access function](X: Float) @key[return] Float;
          LoX, HiX: Float;
          LoY, HiY: Float) @key[return] Float @key[is]
   @key[function] FnX(X: Float) @key[return] Float @key[is]
      @key[function] FnY(Y: Float) @key[return] Float @key[is]
      @key[begin]
         @key[return] Fn(X, Y);
      @key[end] FnY;
   @key[begin]
      @key[return] Integrate(FnY'Access, LoY, HiY);
   @key[end] FnX;
@key[begin]
   @key[return] Integrate(FnX'Access, LoX, HiX);
@key[end] integrate;
@end[Example]

The new function @exam[Integrate] for two dimensions overloads and
uses the function @exam[Integrate] for one dimension (a good example
of overloading). With this generality it is again impossible to arrange
the structure in a manner which is legal in Ada 95.

@leading@;We might use the two-dimensional integration routine to solve the
original trivial problem as follows
@begin[Example]
@key[function] F(X, Y: Float) @key[return] Float @key[is]
@key[begin]
   @key[return] X*Y;
@key[end] F;
...

Result := Integrate(F'Access, 0.0, 1.0, 0.0, 1.0);
@end[Example]

@leading@;As an exercise the reader might like to rewrite the two dimensional
function to work on a non-rectangular domain. The trick is to pass
the bounds of the inner integral also as functions. The profile then
becomes
@begin[Example]
@key[function] Integrate(
        Fn: @key[access function] (X, Y: Float) @key[return] Float;
        LoX, HiX: Float;
        LoY, HiY: @key[access function](X: Float) @key[return] Float)
                                        @key[return] Float;
@end[Example]

In case the reader should think that this topic is all too mathematical
it should be pointed out that anonymous access to subprogram parameters
are widely used in the new container library thereby saving the unnecessary
use of generics.

@leading@;For example the package @exam[Ada.Containers.Vectors] declares
procedures such as
@begin[Example]
@key[procedure] Update_Element
   (Container: @key[in] Vector; Index: @key[in] Index_Type;
    Process: @key[not null access] @key[procedure ](Element: @key[in out] Element_Type));
@end[Example]

@leading@;This updates the element of the vector @exam[Container] whose index
is @exam[Index] by calling the procedure @exam[Process] with that
element as parameter. Thus if we have a vector of integers @exam[V]
and we need to double the value of those with index in the range 5
to 10, then we would first declare a procedure such as
@begin[Example]
@key[procedure] Double(E: @key[in out] Integer) @key[is]
@key[begin]
   E := 2 * E;
@key[end] Double;
@end[Example]

@leading@keepnext@;and then write
@begin[Example]
@key[for] I @key[in] 5 .. 10 @key[loop]
   Update_Element(V, I, Double'Access);
@key[end] @key[loop];
@end[Example]

Further details of the use of access to subprogram types with containers
will be found in a later chapter (see @RefSecNum{Lists and vectors}).

Finally it should be noted that anonymous access to subprogram types
can also be used in all those places where anonymous access to object
types are allowed. That is as stand-alone objects, as components of
arrays and records, as function results, in renamings, and in access
discriminants.

@leading@;The reader who likes long sequences of reserved words should realise
by now that there is no limit in Ada 2005. This is because a function
without parameters can return an access to function as its result
and this in turn could be of a similar kind. So we would have
@begin[Example]
@key[type] FF@key[ is access function return access function return access function] ...
@end[Example]

Attempts to compile such an access to function type will inevitably
lead to madness.


@LabeledClause{Access types and discriminants}


This final topic concerns two matters. The first is about accessing
components of discriminated types that might vanish or change mysteriously
and the second is about type conversions.

@leading@keepnext@;Recall that we can have a mutable variant record such as
@begin[Example]
@key[type] Gender @key[is] (Male, Female, Neuter);

@key[type] Mutant(Sex: Gender := Neuter) @key[is]
   @key[record]
      @key[case] Sex @key[is]
         @key[when] Male =>
            Bearded: Boolean;
         @key[when] Female =>
            Children: Integer;
         @key[when] Neuter =>
            @key[null];
      @key[end case];
   @key[end record];
@end[Example]

@leading@;This represents a world in which there are three sexes, males which
can have beards, females which can bear children, and neuters which
are fairly useless. Note the default value for the discriminant. This
means that if we declare an unconstrained object thus
@begin[Example]
The_Thing: Mutant;
@end[Example]

@leading@;then @exam[The_Thing] is neuter by default but could have its sex
changed by a whole record assignment thus
@begin[Example]
The_Thing := (Sex => Male, Bearded => True);
@end[Example]

It now is @exam[Male] and has a beard.

The problem with this sort of object is that components can disappear.
If it were changed to be @exam[Female] then the beard would vanish
and be replaced by children. Because of this ghostly behaviour certain
operations on mutable objects are forbidden.

@leading@;One obvious rule is that it is not permissible to rename components
which might vanish. So
@begin[Example]
@tabset(P49)
Hairy: Boolean @key[renames] The_Thing.Bearded;  @\-- @examcom[illegal]
@end[Example]

is not permitted. This was an Ada 83 rule. It was probably the case
that the rules were watertight in Ada 83. However, Ada 95 introduced
many more possibilities. Objects and components could be marked as
@key[aliased] and the @exam[Access] attribute could be applied. Additional
rules were then added to prevent creating references to things that
could vanish.

However, it was then discovered that the rules in Ada 95 regarding
access types were not watertight. Accordingly various attempts were
made to fix them in a somewhat piecemeal fashion. The problems are
subtle and do not seem worth describing in their entirety in this
general presentation. We will content ourselves with just a couple
of examples.

@leading@keepnext@;In Ada 95 we can declare types such as
@begin[Example]
@key[type] Mutant_Name @key[is access all] Mutant;
@key[type] Things_Name @key[is access all] Mutant(Neuter);
@end[Example]

Naturally enough an object of type @exam[Things_Name] can only be
permitted to reference a @exam[Mutant] whose @exam[Sex] is @exam[Neuter].

@begin[Example]
Some_Thing: @key[aliased] Mutant;
Thing_Ptr: Things_Name := Some_Thing'Access;
@end[Example]

Things would now go wrong if we allowed @exam[Some_Thing] to have
a sex change. Accordingly there is a rule in Ada 95 that says that
an aliased object such as @exam[Some_Thing] is considered to be constrained.
So that is quite safe.

@leading@;However, matters get more difficult when a type such as @exam[Mutant]
is used for a component of another type such as
@begin[Example]
@key[type] Monster @key[is]
   @key[record]
      Head: Mutant(Female);
      Tail: @key[aliased] Mutant;
   @key[end record];
@end[Example]

@leading@;Here we are attempting to declare a nightmare monster whose head is
a female but whose tail is deceivingly mutable. Those with a decent
education might find that this reminds them of the Sirens who tempted
Odysseus by their beautiful voices on his trip past the monster Scylla
and the whirlpool Charybdis. Those with an indecent education can
compare it to a pantomime theatre horse (or mare, maybe indeed a nightmare).
We could then write
@begin[Example]
M: Monster;
Thing_Ptr := Monster.Tail'Access;
@end[Example]

However, there is an Ada 95 rule that says that the @exam[Tail] has
to be constrained since it is aliased so the type @exam[Monster] is
not allowed. So far so good.

@leading@keepnext@;But now consider the following very nasty example
@begin[Example]
@key[generic]
   @key[type] T @key[is private];
   Before, After: T;
   @key[type] Name @key[is access all] T;
   A_Name: @key[in out] Name;
@key[package] Sex_Change @key[is end];

@key[package body] Sex_Change is
   @key[type] Single @key[is array ](1..1) @key[of aliased] T;
   X: Single := (1 => Before);
@key[begin]
   A_Name := X(1)'Access;
   X := (1 => After);
@key[end] Sex_Change;
@end[Example]

@leading@keepnext@;and then
@begin[Example]
@tabset{P42}
A_Neuter: Mutant_Name(Neuter);@\-- @examcom[fixed neuter]

@key[package] Surgery @key[is new] Sex_Change(
           T => Mutant,
           Before => (Sex => Neuter),
           After => (Sex => Male, Bearded => True),
           Name => Mutant_Name,
           A_Name => A_Neuter);

@\-- @examcom[instantiation of Surgery makes A_Neuter hairy]
@end[Example]

The problem here is that there are loopholes in the checks
when the package @exam[Sex_Change] is elaborated. The object @exam[A_Name]
is assigned
an access to the single component of the array @exam[X] whose value
is @exam[Before]. When this is done there is a check that the component
of the array has the correct subtype. However the subsequent assignment
to the whole array changes the value of the component to @exam[After]
and this can change the subtype of @exam[X(1)] surreptitiously and
there is no check concerning @exam[A_Name]. The key point is that
the generic doesn't know that the type @exam[T] is mutable; this information
is not part of the generic contract.

So when we instantiate @exam[Surgery] (at the same level as the type
@exam[Mutant_Name] so that accessibility succeeds), the object @exam[A_Neuter]
suddenly finds that it has grown a beard!

@leading@;A similar difficulty occurs when private types are involved because
the partial view and full view might disagree about whether the type
is constrained or not. Consider
@begin[Example]
@tabset[P42]
@key[package] Beings @key[is]
   @key[type] Mutant @key[is private];
   @key[type] Mutant_Name @key[is access] Mutant;
   F, M: @key[constant] Mutant;
@key[private]
   @key[type] Mutant(Sex: Gender := Neuter) @key[is]
      @key[record]
         ... @\-- @examcom[as above]
      @key[end record];

   F: @key[constant] Mutant := (Female, ... );
   M: @key[constant] Mutant := (Male, ... );
@key[end] Beings;
@end[Example]

@leading@;Now suppose some innocent user (who
has not peeked at the private part) writes
@begin[Example]
@tabset[P42]
Chris: Mutant_Name := @key[new] Mutant'(F);@\-- @examcom[OK]
...
Chris.@key[all] := M;@\--@examcom[raises Constraint_Error]
@end[Example]

This is very surprising. The user cannot see that the type @exam[Mutant] is
mutable and in particular cannot see that @exam[M] and @exam[F] are different
in some way. From the outside they just look like constants of the same type.
The big trouble is that there is a rule in Ada 95 that says that an object
created by an allocator is constrained. So the new object referred to by
@exam[Chris] is permanently @exam[Female] and therefore the attempt to assign
the value of @exam[M] with its @exam[Bearded] component to her is doomed.

Attempting to fix these and related problems with a number of minimal
rules seemed fated not to succeed.
So a different approach has been taken. Rather than saying that aliased and
allocated objects are always treated as constrained so that accessed components
do not disappear, Ada 2005 takes the approach of preventing the @exam[Access]
attribute from being applied in certain circumstances by disallowing
certain access subtypes at all. In particular, general access subtypes which
refer to types with defaults for their discriminants are forbidden.

The net outcome is that the declaration of @exam[A_Neuter] is illegal because
we cannot write @exam[Mutant_Name(Neuter)] and so the @exam[Surgery] cannot be
applied to constrained mutants. On the other hand, Chris is allowed to change
sex because the allocated objects are no longer automatically constrained in
the case of private types whose partial view does not have discriminants.

These changes introduce some minor incompatibilities which are explained with
further examples in the Epilogue.

@leading@;The other change in this area concerns type conversions. A variation
on the gender theme is illustrated by the following
@begin[Example]
@key[type] Gender @key[is] (Male, Female);

@key[type] Person(Sex: Gender) @key[is]
   @key[record]
      @key[case] Sex @key[is]
         @key[when] Male =>
            Bearded: Boolean;
         @key[when] Female =>
            Children: Integer;
      @key[end case];
   @key[end record];
@end[Example]

Note that this type is not mutable so all persons are stuck with their
sex from birth.

@leading@keepnext@;We might now declare some access types
@begin[Example]
@key[type] Person_Name@key[ is access all] Person;
@key[type] Mans_Name @key[is access all] Person(Male);
@key[type] Womans_Name@key[ is access all] Person(Female);
@end[Example]

@leading@;so that we can manipulate various names of people. We would naturally
use @exam[Person_Name] if we did not know the sex of the person and
otherwise use @exam[Mans_Name] or @exam[Womans_Name] as appropriate.
We might have
@begin[Example]
It: Person_Name := Chris'Access;
Him: Mans_Name := Jack'Access;
Her: Womans_Name := Jill'Access;
@end[Example]

@leading@;If we later discover that Chris is actually Christine then we might
like to assign the value in @exam[It] to a more appropriate variable
such as @exam[Her]. So we would like to write
@begin[Example]
Her := Womans_Name(It);
@end[Example]

@leading@;But curiously enough this is not permitted in Ada 95 although the
reverse conversion
@begin[Example]
It := Person_Name(Her);
@end[Example]

is permitted. The Ada 95 rule is that any constraints have to statically
match or the conversion has to be to an unconstrained type. Presumably
the reason was to avoid checks at run time. But this lack of symmetry
is unpleasant and the rule has been changed in Ada 2005 to allow conversion
in both directions with a run time check as necessary.

The above example is actually Exercise 19.8(1) in the textbook
@LocalLink{Target=[R6],Sec=[References],Text={[6]}}. The poor student was
invited to solve an impossible problem. But they will be successful in Ada
2005.

