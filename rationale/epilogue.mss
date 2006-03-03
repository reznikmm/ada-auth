@Part(Epilogue, Root="rat.msm")

@comment($Source: e:\\cvsroot/ARM/Rationale/epilogue.mss,v $)
@comment($Revision: 1.2 $ $Date: 2006/02/25 06:39:56 $)

@LabeledSection{Epilogue}

@Subheading{Abstract}

@i{This is
the last of a number of papers describing the rationale for Ada 2005.
In due course it is anticipated that the papers will be combined (after
appropriate reformatting and editing) into a single volume for formal
publication.}

@i{This last paper summarizes a small number of general issues of importance
to the user such as compatibility between Ada 2005 and Ada 95. It
also briefly considers a few potential changes that were considered
for Ada 2005 but rejected for various reasons.}

@LabeledClause{Compatibility}

There are two main sorts of problems regarding compatibility. These
are termed Incompatibilities and Inconsistencies.

An incompatibility is a situation where a legal Ada 95 program is
illegal in Ada 2005. These can be annoying but not a disaster since
the compiler automatically detects such situations.@Defn{incompatibility}

An inconsistency is where a legal Ada 95 program is also a legal Ada
2005 program but might have a different effect at execution time.
These can in principle be really nasty but typically the program is
actually wrong anyway (in the sense that it does not do what the programmer
intended) or its behaviour depends upon the raising of a predefined
exception (which is generally considered poor style) or the situation
is extremely unlikely to occur.@Defn{inconsistency}

As mentioned below in Section @RefSecNum{Retrospective changes to Ada 95},
during the development of Ada 2005
a number of corrections were made to Ada 95 and these resulted in
some incompatibilities and inconsistencies with the original Ada 95
standard. These are not considered to be incompatibilities or inconsistencies
between Ada 95 and Ada 2005 and so are not covered in this section.


@LabeledSubClause{Incompatibilities with Ada 95}


Each incompatibility listed below gives the AI concerned and the paragraph
in the AARM which in some cases will give more information. Where
relevant, the section in this rationale where the topic is discussed
is also given. Where appropriate the incompatibilities are grouped
together.

1 @em The words @key[interface], @key[overriding]
and @key[synchronized] are now reserved. Programs using them as identifiers
will need to be changed. (@AILink{AI=[AI95-00284-02],Text=[AI-284]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-2-9.html],
Text=[2.9(3.c)]})

This is perhaps the most important incompatibility in terms of visibility
to the average programmer. It is discussed in @RefSecNum{Reserved Words}.

2 @em If a @LocalTarget{Target=[Item2],Text={predefined}} package has
additional entities then incompatibilities can arise. Thus suppose the
predefined package @exam[Ada.Stuff] has an additional entity @exam[More] added
to it. Then if an Ada 95 program has a package @exam[P] containing an entity
@exam[More] then a program with a use clause for both @exam[Ada.Stuff] and
@exam[P] will become illegal in Ada 2005 because the reference to @exam[More]
will become ambiguous. This also applies if further overloadings of an existing
entity are added.

Because of this there has been reluctance to extend existing packages
but a preference to add child packages. Nevertheless in some cases
extending a package seemed more appropriate especially if the identifiers
concerned are unlikely to have been used by programmers.

The following packages have been extended with additional entities
as listed.


@exam[Ada.Exceptions] @em @exam[Wide_Exception_Name],
@exam[Wide_Wide_Exception_Name]. (@AILink{AI=[AI95-00400-01],Text=[AI-400]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-11-4-1.html],
Text=[11.4.1(19.bb)]})

@exam[Ada.Real_Time] @em @exam[Seconds], @exam[Minutes].
(@AILink{AI=[AI95-00386-01],Text=[AI-386]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-D-8.html],
Text=[D.8(51.a)]})

@exam[Ada.Strings] @em @exam[Wide_Wide_Space].
(@AILink{AI=[AI95-00285-01],Text=[AI-285]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-A-4-1.html],
Text=[A.4.1(6.a)]})

@exam[Ada.Strings.Fixed] @em @exam[Index], @exam[Index_Non_Blank].
(@AILink{AI=[AI95-00301-01],Text=[AI-301]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-A-4-3.html],
Text=[A.4.3(109.a)]})

@exam[Ada.Strings.Bounded] @em @exam[Set_Bounded_String],
@exam[Bounded_Slice], @exam[Index], @exam[Index_Non_Blank].
(@AILink{AI=[AI95-00301-01],Text=[AI-301]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-A-4-4.html],
Text=[A.4.4(106.f)]})

@exam[Ada.Strings.Unbounded] @em @exam[Set_Unbounded_String],
@exam[Unbounded_Slice], @exam[Index], @exam[Index_Non_Blank].
(@AILink{AI=[AI95-00301-01],Text=[AI-301]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-A-4-5.html],
Text=[A.4.5(88.c)]})

There are similar additions to @exam[Ada.Strings.Wide_Fixed], @exam[Ada.Strings.Wide_Bounded]
and @exam[Ada.Strings.Wide_Unbounded].
(@AILink{AI=[AI95-00301-01],Text=[AI-301]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-A-4-7.html],
Text=[A.4.7(48.a)]})

@exam[Ada.Tags] @em @exam[No_Tag],
@exam[Parent_Tag], @exam[Interface_Ancestor_Tags], @exam[Descendant_Tag],
@exam[Is_Descendant_
At_Same_Level], @exam[Wide_Expanded_Name], @exam[Wide_Wide_Expanded_Name].
(@AILink{AI=[AI95-00260-02],Text=[AI-260]},
@AILink{AI=[AI95-00344-01],Text=[AI-344]},
@AILink{AI=[AI95-00400-01],Text=[AI-400]},
@AILink{AI=[AI95-00405-01],Text=[AI-405]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-3-9.html],
Text=[3.9(33.d)]})

@exam[Ada.Text_IO] @em @exam[Get_Line].
(@AILink{AI=[AI95-00301-01],Text=[AI-301]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-A-10-7.html],
Text=[A.10.7(26.a)]})

@exam[Interfaces.C] @em @exam[char16_t], @exam[char32_t]
and related types and operations. (@AILink{AI=[AI95-00285-01],Text=[AI-285]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-B-3.html],
Text=[B.3(84.a)]})

It seems unlikely that existing programs will be affected by these
potential incompatibilities.

3 @em If a subprogram has an access parameter (without
a null exclusion) and is not a dispatching operation then it cannot
be renamed as a dispatching operation in Ada 2005 although it can
be so renamed in Ada 95. See @RefSecNum{Null exclusion and constant} for an
example.
(@AILink{AI=[AI95-00404-01],Text=[AI-404]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-3-9-2.html],
Text=[3.9.2(24.b)]})

4 @em As discussed in @RefSecNum{Access types and discriminants}, there are
many awkward situations in Ada 95 regarding access types, discriminants
and constraints. One problem is that some components can change shape
or disappear. The rules in Ada generally aim to prevent such components
from being accessed or renamed. However, in Ada 95, some entities
don't look constrained but actually are constrained. The consequence
is that it is difficult to prevent some constrained objects from having
their constraints changed and this can cause components to change
or disappear even though they might be accessed or renamed.

A key rule in Ada 95 was that aliased variables were always constrained
with the intent that that would solve the problems. But loopholes
remained and so the rules have been changed considerably. Aliased
variables are not necessarily constrained in Ada 2005 and other rules
now disallow certain constructions that were permitted in Ada 95 and
this gives rise to a number of minor incompatibilities.

@leading@;If a general access subtype refers to a type with default
discriminants then that access subtype cannot have constraints in Ada 2005.
Consider
@begin[Example]
@key[type] T(Disc: Boolean := False) @key[is]
   @key[record]
     ...
   @key[end record];
@end[Example]

@leading@;The discriminated type @exam[T] has a default and so unconstrained
objects of type @exam[T] are mutable. Suppose we now have
@begin[Example]
@tabset[P49]
@key[type] T_Ptr @key[is access all] T;
@key[subtype] Sub_True_T_Ptr @key[is] T_Ptr(Disc => True);@\-- @examcom[subtype illegal in Ada 2005]
@end[Example]

@leading@;The type @exam[T_Ptr] is legal in both Ada 95 and Ada 2005 of course,
but the subtype @exam[Sub_True_T_Ptr] is only legal in Ada 95 and
not in Ada 2005. The reason why the subtype cannot be permitted is
illustrated by the following
@begin[Example]
Some_T: @key[aliased] T := (Disc => True, ...);
A_True_T: Sub_True_T_Ptr := Some_T'Access;
...
Some_T := (Disc => False, ...);
@end[Example]

When @exam[Some_T'Access] is evaluated there is a check that the discriminant
has the correct value so that @exam[A_True_T] is assigned a valid
value. But the second assignment to @exam[Some_T] means that the discriminant
changes and so @exam[A_True_T] would no longer have a valid value.

In Ada 95, all aliased variables were considered constrained and so
the second assignment would not have been permitted anyway. But, as
mentioned above, aliased variables are not considered to be constrained
in Ada 2005 just because they are aliased.

@leading@;Note that there is no similar restriction on types; thus we can still
write
@begin[Example]
@key[type] True_T_Ptr @key[is access all] T(Disc => True);
@end[Example]

because any conversion which might cause difficulties is forbidden
as explained in one of the examples below.

The restriction on subtypes does not apply if the discriminants do
not have defaults, nor to pool-specific types.
(@AILink{AI=[AI95-00363-01],Text=[AI-363]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-3-7-1.html],
Text=[3.7.1(15.c)]})

@leading@;Since aliased variables are not necessarily constrained in Ada 2005
there are situations where components might change shape or disappear
in Ada 2005 that could not happen in Ada 95. Applying the @exam[Access]
attribute to such components is thus illegal in Ada 2005. Suppose
the example above has components as follows
@begin[Example]
@key[type] T(Disc: Boolean := False) @key[is]
   @key[record]
      @key[case] Disc @key[is]
         @key[when] False =>
            Comp: @key[aliased] Integer;
         @key[when] True =>
            @key[null];
      @key[end case];
   @key[end record];
@end[Example]

@LocalTarget{Target=[Item4],Text={Since}} objects of
type @exam[T] might be mutable, the component @exam[Comp]
might disappear.

@begin[Example]
@tabset[P49]
@key[type] Int_Ptr @key[is access all] Integer;
Obj: @key[aliased] T;@\-- @examcom[mutable object]
Dodgy: Int_Ptr := Obj.Comp'Access;@\-- @examcom[take care]
...
Obj:= (Disc => True);@\-- @examcom[Comp gone]
@end[Example]

In Ada 95, the assignment to @exam[Dodgy] is permitted but then the
assignment to @exam[Obj] raises @exam[Constraint_Error] because there
might be dodgy pointers.

In Ada 2005, the assignment statement to @exam[Dodgy] is illegal since
we cannot write @exam[Obj.Comp'Access]. The assignment to @exam[Obj]
is itself permitted because we now know that there cannot be any dodgy
pointers.

See (@AILink{AI=[AI95-00363-01],Text=[AI-363]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-3-10-2.html],
Text=[3.10.2(41.b)]}). Similarly, renaming an aliased component
such as @exam[Comp] is also illegal.
(@AILink{AI=[AI95-00363-01],Text=[AI-363]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-8-5-1.html],
Text=[8.5.1(8.b)]})

@leading@;There are related situations regarding discriminated private types
where type conversions and the @exam[Access] attribute are forbidden.
Suppose we have a private type and an access type and that the full
type is in fact the discriminated type above thus
@begin[Example]
@tabset[P49]
@key[package] P @key[is]
   @key[type] T @key[is private];
   @key[type] T_Ptr @key[is access all ]T;
   @key[function] Evil @key[return] T_Ptr;
   @key[function] Flip(Obj: T) @key[return] T;
@key[private]
   @key[type] T(Disc: Boolean := False) @key[is]
      @key[record]
         ...
      @key[end record];
   ...
@key[end] P;

@key[package body] P @key[is]

   @key[type] True_T_Ptr @key[is access all] T(Disc => True);
   @key[subtype] Sub_True_T_Ptr @key[is] T_Ptr(Disc => True);  --@examcom[legal in Ada 95, illegal in Ada 2005]
   True_Obj: @key[aliased] T(Disc => True);
   TTP: True_T_Ptr := True_Obj'Access;
   STTP: Sub_True_T_Ptr := True_Obj'Access;

   @key[function] Evil @key[return] T_Ptr @key[is]
   @key[begin]
      @key[if] ... @key[then]
         @key[return] T_Ptr(TTP);@\-- @examcom[OK in 95, not in 2005]
      @key[elsif] ... @key[then]
         @key[return] True_Obj'Access;@\-- @examcom[OK in 95, not in 2005]
      @key[else]
        @key[return] STTP;
      @key[end if];
   @key[end] Evil;

   @key[function] Flip(Obj: T) @key[return] T is
   @key[begin]
      @key[case] Obj.Disc @key[is]
         @key[when] True => @key[return] (Disc => False, ...);
         @key[when] False => @key[return] (Disc => True, ...);
      @key[end case];
   @key[end] Flip;

@key[end] P;
@end[Example]

@leading@;The function @exam[Evil] has three branches illustrating
various possible
ways of returning a value of the type @exam[T]. The function @exam[Flip]
just returns a value of the type @exam[T] with opposite discriminants
to the parameter. Now consider
@begin[Example]
@key[with] P;  @key[use] P;
@key[procedure] Do_It @key[is]
   A: T;
   B: T_Ptr := @key[new] T;
   C: T_Ptr := Evil;
@key[begin]
   A := Flip(A);
   B.@key[all] := Flip(B.@key[all]);
   C.@key[all] := Flip(C.@key[all]);
@key[end] Do_It;
@end[Example]

This declares an object @exam[A] of type @exam[T] and then two objects
@exam[B] and @exam[C] of the access type @exam[T_Ptr] and initializes
them in different ways. Finally it attempts to change the discriminant
of the three objects by calling the function @exam[Flip].

In Ada 95 all objects on the heap are constrained. This means that
clients cannot change the discriminants even if they do not know that
they exist. So the assignment to @exam[B.]@key[all] raises
@exam[Constraint_Error] since @exam[B.]@key[all] is on the heap and
thus constrained whereas the assignment to @exam[A] is fine since
@exam[A] is not constrained. However, from the client's point of view
they both really do the same thing and so the behaviour is very curious.
Remember that the client doesn't know about the discriminants and
so both operations look the same in the abstract. This is unfortunate
and breaks privacy which is sinful. There is a similar example in
@RefSecNum{Access types and discriminants} where we try to change Chris
but do not know that
the new value has a beard and this fails because Chris is female.

To prevent such privacy breaking the rules are changed in Ada 2005
so that objects on the heap are unconstrained in this one case. So
the assignments to @exam[B.]@key[all] and @exam[C.]@key[all] do not
have checks on the discriminant. As a consequence @exam[Evil] must
not return an object which is constrained otherwise the assignment
to @exam[C] would result in @exam[True_Obj] having its discriminant
turned to @exam[False].

All three possible branches in @exam[Evil] are prevented in Ada 2005.
The conversion in the first branch is forbidden and the @exam[Access]
attribute in the second branch is forbidden. In the case of the third
branch the return itself is acceptable in principle because @exam[STTP]
is of the correct type. However, this is prevented by the rule mentioned
above since the subtype @exam[Sub_True_T_Ptr] is itself forbidden
and so the object @exam[STTP] could not be declared in the first place.

See (@AILink{AI=[AI95-00363-01],Text=[AI-363]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-3-10-2.html],
Text=[3.10.2(41.e)]} and
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-4-6.html],
Text=[4.6(71.k)]}).

@leading@;5 @em Aggregates of limited types are permitted
in Ada 2005 as discussed in @RefSecNum{Limited types and return statements}.
This means that in
obscure situations an aggregate might be ambiguous in Ada 2005 and
thus illegal. Consider
@begin[Example]
@tabset[P49]
@key[type] Lim @key[is limited]
   @key[record]
      Comp: Integer;
   @key[end record];

@key[type] Not_Lim @key[is]
   @key[record]
      Comp: Integer;
   @key[end record];

@key[procedure] P(X: LIm);
@key[procedure] P(X: Not_Lim);

P((Comp => 123));@\-- @examcom[illegal in Ada 2005]
@end[Example]

In Ada 95, the aggregate cannot be of a limited type and so the type
@exam[Lim] is not considered for resolution. But Ada 2005 permits
aggregates of limited types and so the aggregate is ambiguous.
See (@AILink{AI=[AI95-00287-01],Text=[AI-287]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-4-3.html],
Text=[4.3(6.e)]})

@leading@;Another similar situation with limited types and nonlimited types
concerns assignment. Again this relates to the fact that limitedness
is no longer considered for name resolution. Consider
@begin[Example]
@tabset[P49]
@key[type] Acc_Not_Lim @key[is access] Not_Lim;
@key[function] F(X: Integer) @key[return] Acc_Not_Lim;
@key[type] Acc_Lim @key[is access] Lim;
@key[function] F(X: Integer) @key[return] Acc_Lim;
F(1).@key[all] := F(2).@key[all];@\-- @examcom[illegal in Ada 2005]
@end[Example]

In Ada 95, only the first @exam[F] is considered for name resolution
and the program is valid. In Ada 2005, there is an ambiguity because
both functions are considered. Note of course that the assignment
for the limited function is still illegal anyway but the compiler
meets the ambiguity first. Clearly this is an obscure situation.
(@AILink{AI=[AI95-00287-01],Text=[AI-287]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-5-2.html],
Text=[5.2(28.d)]})

@leading@;6 @em Because of the changes to the fixed-fixed
multiplication and division rules there are situations where a legal
program in Ada 95 becomes illegal in Ada 2005. Consider
@begin[Example]
@tabset[P49]
@key[package] P @key[is]
   @key[type] My_Fixed @key[is delta] ... ;
   @key[function] "*" (L, R: My_Fixed) @key[return] My_Fixed;
@key[end] P;

@key[use] P;
A, B: My_Fixed;
D: Duration := A * B;@\-- @examcom[illegal in Ada 2005]
@end[Example]

Although this is legal in Ada 95, the new rule in Ada 2005 says that
if there is a user-defined operation involving the type concerned
then the predefined operation cannot be used unless there is a type
conversion or we write @exam[Standard."*"( ... )].

@leading@;So in Ada 2005 a conversion can be used thus
@begin[Example]
D: Duration := Duration(A * B);
@end[Example]
See @RefSecNum{Numerics}.
(@AILink{AI=[AI95-00364-01],Text=[AI-364]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-4-5-5.html],
Text=[4.5.5(35.d)]})

7 @em The concept of return by reference types has
gone. Instead the user has to explicitly declare a function with an
anonymous access type as the return type. This only affects functions
that return an existing limited object such as choosing a task from
among a pool of tasks. See @RefSecNum{Limited types and return statements}
for an example.
(@AILink{AI=[AI95-00318-02],Text=[AI-318]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-6-5.html],
Text=[6.5(27.g)]})

8 @em There is a very curious situation regarding
exporting multiple homographs from an instantiation that is now illegal.
This is a side effect of adding interfaces to the language.
(@AILink{AI=[AI95-00251-01],Text=[AI-251]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-8-3.html],
Text=[8.3(29.s)]})

@leading@;9 @em The introduction of more forms of access types
has changed the rules regarding name resolution. Consider the following
contrived example
@begin[Example]
@tabset[P49]
@key[type] Cacc@key[ is access constant] Integer;
@key[procedure] Proc(Acc: @key[access] Integer);
@key[procedure] Proc(Acc: Cacc);
List: Cacc := ... ;
...
Proc(List);@\-- @examcom[illegal in Ada 2005]
@end[Example]

In Ada 95 the call of @exam[Proc] is resolved because the parameters
@exam[Acc] are anonymous access to variable in one case and access
to constant in the other. In Ada 2005, the name resolution rules do
not take this into account so it becomes ambiguous and thus illegal
which is a good thing because it is likely that the Ada 95 programmer
made a mistake anyway.
(@AILink{AI=[AI95-00409-01],Text=[AI-409]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-8-6.html],
Text=[8.6(34.n)]})

10 @em In Ada 2005, a procedure call that might
be an entry is permitted in timed and conditional entry calls. See
@RefSecNum{Synchronized interfaces}. In Ada 95, a procedure could not be so
used and this fact is used in name resolution in Ada 95 but does not apply
in Ada 2005. Hence if a procedure and an entry have the same profile
then an ambiguity can exist in Ada 2005.
(@AILink{AI=[AI95-00345-01],Text=[AI-345]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-9-7-2.html],
Text=[9.7.2(7.b)]})

11 @em It is now illegal to have an allocator for
an access type with @exam[Storage_Size] equal to zero whereas in Ada
95 it raised @exam[Storage_Error] on execution. It is always better
to detect errors at compile time wherever possible. The reason for
the change is to allow @exam[Pure] units to use access types provided
they do not use allocators. If the storage size is zero then this
is now known at compile time.
(@AILink{AI=[AI95-00366-01],Text=[AI-366]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-4-8.html],
Text=[4.8(20.g)]})

12 @em The requirement that a partial view with
available stream attributes be externally streamable can cause an
incompatibility in extremely rare cases. This also relates to pragma
@exam[Pure].
(@AILink{AI=[AI95-00366-01],Text=[AI-366]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-10-2-1.html],
Text=[10.2.1(28.e)]})

13 @em It is now illegal to use an incomplete view
as a parameter or result of an access to subprogram type or as an
access parameter of a primitive operation if the completion is deferred
to the package body. See @RefSecNum{Mutually dependent types} for examples.
(@AILink{AI=[AI95-00326-01],Text=[AI-326]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-3-10-1.html],
Text=[3.10.1(23.h, i)]})

14 @em The specification of @exam[System.RPC] can
now be tailored for an implementation by adding further operations
or by changing the profile of existing operations. If it is tailored
in this way then an existing program might not compile in Ada 2005.
See @RefSecNum{Categorization of library units}.
(@AILink{AI=[AI95-00273-01],Text=[AI-273]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-E-5.html],
Text=[E.5(30.a)]})


@LabeledSubClause{Inconsistencies with Ada 95}


1 @em The awkward situations regarding access types,
discriminants and constraints discussed in
@RefSecNum{Access types and discriminants}, can also give rise to obscure
inconsistencies.

Unconstrained aliased objects of types with discriminants with defaults
are no longer constrained by their initial values. This means that
a program that raised @exam[Constraint_Error] in Ada 95 because of
attempting to change the discriminants will no longer do so.

@leading@;Thus consider @LocalLink{Target=[Item4],Sec=[Epilogue],Text=[item 4]}
in the previous section. We had
@begin[Example]
@tabset[P42]
@key[type] Int_Ptr @key[is access all] Integer;
Obj: @key[aliased] T;@\-- @examcom[mutable object]
Dodgy: Int_Ptr := Obj.Comp'Access;@\-- @examcom[take care]
...
Obj:= (Disc => True);@\-- @examcom[Comp gone]
@end[Example]

We noted that in Ada 2005, the assignment statement to @exam[Dodgy]
is illegal because we cannot write @exam[Obj.Comp'Access]. The assignment
to @exam[Obj] is itself permitted because we now know that there cannot
be any dodgy pointers. Suppose that the assignment to @exam[Dodgy]
is removed. Then in Ada 95, the assignment to @exam[Obj] will raise
@exam[Constraint_Error] but it will not in Ada 2005. It is extremely
unlikely that any correct program relied upon this behaviour.
(@AILink{AI=[AI95-00363-01],Text=[AI-363]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-3-3-1.html],
Text=[3.3.1(33.f)]} and
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-3-10.html],
Text=[3.10(26.d)]})

@leading@;A related situation applies with allocators where the allocated type
is a private type with hidden discriminants. This is also illustrated
by an earlier example where we had
@begin[Example]
@tabset[P42]
@key[with] P;  @key[use] P;
@key[procedure] Do_It @key[is]
   A: T;
   B: T_Ptr := @key[new] T;
   C: T_Ptr := Evil;
@key[begin]
   A := Flip(A);
   B.@key[all] := Flip(B.@key[all]);@\-- @examcom[Constraint_Error in Ada 95, not in 2005]
   C.@key[all] := Flip(C.@key[all]);
@key[end] Do_It;
@end[Example]

The assignment to @exam[B.]@key[all] raises @exam[Constraint_Error]
in Ada 95 but not in Ada 2005 as explained above. Again it is extremely
unlikely that any correct program relied upon this behaviour.
(@AILink{AI=[AI95-00363-01],Text=[AI-363]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-4-8.html],
Text=[4.8(20.f)]})

2 @em In Ada 2005 the categorization of certain
wide characters is changed. As a consequence
@exam[Wide_Character'Wide_Value] and @exam[Wide_Character'Wide_Image]
will change
in some rare situations. A further consequence is that for some subtypes
@exam[S] of @exam[Wide_Character] the value of @exam[S'Wide_Width]
is different. But the value of @exam[Wide_Character'Wide_Width] itself
is not changed.
(@AILink{AI=[AI95-00285-01],Text=[AI-285]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-3-5-2.html],
Text=[3.5.2(9.h)]} and @AILink{AI=[AI95-00395-01],Text=[AI-395]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-3-5-2.html],
Text=[3.5.2(9.i, j)]})

3 @em There is an interesting analogy to incompatibility
@LocalLink{Target=[Item2],Sec=[Epilogue],Text=[number 2]} which
concerns adding further entities to existing predefined
packages. If we add further entries to @exam[Standard] itself then
an inconsistency is possible. Thus if an additional entity @exam[More]
is added to the package @exam[Standard] and an existing program has
a package @exam[P] with an existing entity @exam[More] and a use clause
for @exam[P] then, in Ada 2005, references to @exam[More] will now
be to that in @exam[Standard] and not that in @exam[P]. In the most
unlikely event that the program remains legal, it will behave differently.
The only such identifiers added to @exam[Standard] are @exam[Wide_Wide_Character]
and @exam[Wide_Wide_String] so this is extremely unlikely.
(@AILink{AI=[AI95-00285-01],Text=[AI-285]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-3-5-2.html],
Text=[3.5.2(9.k)]} and
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-3-6-3.html],
Text=[3.6.3(8.g)]})

4 @em Access discriminants and non-controlling access
parameters no longer exclude null in Ada 2005. A program that passed
null to these will behave differently.

The usual situation is that @exam[Constraint_Error] will be raised
within the subprogram when an attempt to dereference is made rather
than at the point of call. If the subprogram has no handler for @exam[Constraint_Error]
then the final effect will be much the same.

But clearly it is possible for the behaviour to be quite different.
For example, the access value might not be dereferenced or the subprogram
might have a handler for @exam[Constraint_Error] which does something
unusual. And there might even be a pragma @exam[Suppress] for the
check in which case the program will become erroneous.

See @RefSecNum{Null exclusion and constant} for an example.
(@AILink{AI=[AI95-00231-01],Text=[AI-231]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-3-10.html],
Text=[3.10(26.c)]})

5 @em The lower bound of strings returned by functions
@exam[Expanded_Name] and @exam[External_Name] (and wide versions)
in @exam[Ada.Tags] are defined to be @exam[1] in Ada 2005. Ada 95
did not actually define the value and so if an implementation has
chosen to return some other lower bound such as @exam[77] then the
program might behave differently.
(@AILink{AI=[AI95-00417-01],Text=[AI-417]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-3-9.html],
Text=[3.9(33.c)]}) See also
@RefSecNum{Inconsistencies with original Ada 95}
@LocalLink{Target=[Item2-4],Sec=[Epilogue],Text=[item 4]} below.

6 @em The upper bound of the range of @exam[Year_Number]
in Ada 2005 is @exam[2399] whereas it was @exam[2099] in Ada 95. See
@RefSecNum{Times and dates}. (@AILink{AI=[AI95-00351-01],Text=[AI-351]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-9-6.html],
Text=[9.6(40.e)]})


@LabeledClause{Retrospective changes to Ada 95}


In the course of the development of Ada 2005, a number of small changes
were deemed to apply also to Ada 95 and thus were classified as binding
interpretations rather than amendments. Accordingly they are not (generally)
covered by the changes discussed in the previous papers. Note however,
that @AILink{AI=[AI95-00241-01],Text=[AI-241]} on exceptions was discussed in
paper @RefSecNum{Exceptions, generics etc} even
though it was eventually classified as a binding interpretation. Moreover,
@AILink{AI=[AI95-00329-01],Text=[AI-329]} was
split and the part stating that Raise_Exception never returns (also applying
to Ada 95) was formed into @AILink{AI=[AI95-00446-01],Text=[AI-446]}.

@leading@AILink{AI=[AI95-00438-01],Text=[AI-438]} adds subprograms
@exam[Read_Exception_Occurrence] and @exam[Write_Exception_Occurence]
plus corresponding attribute definition clauses for streams to the
package @exam[Ada.Exceptions] thus
@begin[Example]
@key[procedure] Read_Exception_Occurrence
     (Stream: @key[not null access] Root_Stream_Type'Class; Item: @key[out] Exception_Occurrence);

@key[procedure] Write_Exception_Occurrence
     (Stream: @key[not null access] Root_Stream_Type'Class; Item: @key[in] Exception_Occurrence);

@key[for] Exception_Occurrence'Read @key[use] Read_Exception_Occurrence;

@key[for] Exception_Occurrence'Write @key[use] Write_Exception_Occurrence;
@end[Example]

These attributes enable the type @exam[Exception_Occurrence] to be
streamed. Note that this is a limited type and so streaming is only
possible if predefined. A survey of other existing and new predefined
limited types showed that no others needed to be treated in this way.

@leading@;No other retrospective AIs actually affect the specification of any
units but typically add or correct a number of rules. Of these some
are of special interest because they introduce minor incompatibilities
or inconsistencies. They are

@begin[Description]
@begin[Description]@Comment{Second one to indent this}
@AILink{AI=[AI95-00108-01],Text=[108]}@\Inheritance of stream attributes for type extensions

@\ (108 was actually in the 2001 Corrigendum)

@AILink{AI=[AI95-00133-01],Text=[133]}@\Controlling bit ordering

@AILink{AI=[AI95-00195-01],Text=[195]}@\Streams (this covers many issues regarding streams)

@AILink{AI=[AI95-00220-01],Text=[220]}@\Subprograms withing private compilation units

@AILink{AI=[AI95-00225-01],Text=[225]}@\Aliased current instance for limited types

@AILink{AI=[AI95-00229-01],Text=[229]}@\Accessibility rules and generics

@AILink{AI=[AI95-00238-01],Text=[238]}@\Lower bound of Ada.Strings.Bounded_Slice

@AILink{AI=[AI95-00240-01],Text=[240]}@\Stream attributes for limited types in Annex E

@AILink{AI=[AI95-00242-01],Text=[242]}@\Surprise behavior of Update

@AILink{AI=[AI95-00246-01],Text=[246]}@\Conversions between arrays of a by-reference type

@AILink{AI=[AI95-00253-01],Text=[253]}@\Pragmas Attach_Handler and Interrupt_Handler

@AILink{AI=[AI95-00268-01],Text=[268]}@\Rounding of real static expressions

@AILink{AI=[AI95-00279-01],Text=[279]}@\Tag read by T'Class'Input

@AILink{AI=[AI95-00283-01],Text=[283]}@\Truncation of stream files by Close and Reset

@AILink{AI=[AI95-00306-01],Text=[306]}@\Class-wide extension aggregate expressions

@AILink{AI=[AI95-00341-01],Text=[341]}@\Primitive subprograms are frozen with a tagged type

@AILink{AI=[AI95-00360-01],Text=[360]}@\Types that need finalization

@AILink{AI=[AI95-00377-01],Text=[377]}@\Naming of generic child packages

@AILink{AI=[AI95-00378-01],Text=[378]}@\The bounds of Ada.Exceptions.Exception_Name

@AILink{AI=[AI95-00403-01],Text=[403]}@\Preelaboration checks and formal objects

@AILink{AI=[AI95-00435-01],Text=[435]}@\Storage pools for access-to-subprogram types

@AILink{AI=[AI95-00446-01],Text=[446]}@\Raise_Exception for Null_Id
@end{Description}
@end{Description}

These are briefly discussed in the following subsections.


@LabeledSubClause{Incompatibilities with original Ada 95}


There are a small number of incompatibilities between the original
Ada 95 and that resulting from various corrections.

1 @em A limited type can become nonlimited. Applying
the @exam[Access] or @exam[Unchecked_Access] attribute to the current
instance of such a type is now illegal.
(@AILink{AI=[AI95-00225-01],Text=[AI-225]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-3-10.html],
Text=[3.10(26.e)]})

@leading@;This is fairly obscure. Remember that the current instance rule is
about referring to a type within its own declaration such as
@begin[Example]
@key[type] Strange @key[is limited]
   @key[record]
      Me: @key[access] Strange := Strange'Unchecked_Access;
      ...
   @key[end record];
@end[Example]

@leading@;This is fine. It only makes sense to permit the attribute if the type
is limited. But a type can be limited by virtue of having a limited component.
for example
@begin[Example]
@key[type] Limp @key[is limited private];

@key[type] Strange @key[is ]
   @key[record]
      Me: @key[access] Strange := Strange'Unchecked_Access;
      C: Limp;
   @key[end record];
@end[Example]

If the component is limited private and it turns out that the full
type of the component is not limited after all then the enclosing
type becomes nonlimited. In such a case the attribute is now not allowed.
The cure is to make the enclosing type explicitly limited.

2 @em Conversions between unrelated array types
that are limited or (for view conversions) might be by-reference types
are now illegal. This is because they might not have the same representation
and they cannot be copied in order to change the representation.
(@AILink{AI=[AI95-00246-01],Text=[AI-246]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-4-6.html],
Text=[4.6(71.j)]})

3 @em The meaning of a record representation clause
and the storage place attributes for the non-default bit order is
now clarified. One consequence is that the equivalence of bit 1 in
word 1 to bit 9 in word 0 for a machine with @exam[Storage_Unit] =
8 no longer applies for the non-default order.
(@AILink{AI=[AI95-00133-01],Text=[AI-133]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-13-5-1.html],
Text=[13.5.1 (31.d)]} and
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-13-5-2.html],
Text=[13.5.2 (5.c)]})

4 @em Various new freezing rules were added in order
to fix a number of holes in the original rules for Ada 95.
(@AILink{AI=[AI95-00341-01],Text=[AI-341]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-13-14.html],
Text=[13.14(20.p)]})

5 @em The type @exam[Unbounded_String] is defined
to need finalization. If the partition has @exam[No_Nested_Finalization]
and moreover the implementation of @exam[Unbounded_String] does not
have a controlled part then it will not be allowed in local objects
now although it was in original Ada 95. Clearly this is extremely
unlikely. (@AILink{AI=[AI95-00360-01],Text=[AI-360]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-A-4-5.html],
Text=[A.4.5(88.b)]}). The same applies to the type @exam[Generator]
in @exam[Numerics.Float_Random] and @exam[Discrete_Random] (@AILink{AI=[AI95-00360-01],Text=[AI-360]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-A-5-2.html],
Text=[A.5.2(61.a)]}) and to @exam[File_Type] in @exam[Sequential_IO]
(@AILink{AI=[AI95-00360-01],Text=[AI-360]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-A-8-1.html],
Text=[A.8.1(17.b)]}), @exam[Direct_IO] (@AILink{AI=[AI95-00360-01],Text=[AI-360]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-A-4-8.html],
Text=[A.8.4(20.a)]}), @exam[Text_IO]
(@AILink{AI=[AI95-00360-01],Text=[AI-360]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-A-10-1.html],
Text=[A.10.1(86.c)]}) and @exam[Stream_IO]
(@AILink{AI=[AI95-00360-01],Text=[AI-360]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-A-12-1.html],
Text=[A.12.1(36.b)]}).
See also @URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-D-7.html],
Text=[D.7(22.a)]}.

This problem is unlikely with types such as @exam[Unbounded_String]
which were introduced into Ada 95 at the same time as controlled types
and thus are almost inevitably implemented in terms of controlled
types. It is more likely with the file types that existed in Ada 83
since some implementations might not have changed them to use controlled
types.

6 @em It is now illegal to apply the @exam[Access]
attribute to a subprogram declared in the specification of a generic
unit in the body of that unit. The usual workaround applies which
is to move the use of the attribute to the private part.
(@AILink{AI=[AI95-00229-01],Text=[AI-229]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-3-10-2.html],
Text=[3.10.2((41.f)]})

@leading@;7 @em It is now illegal for the ancestor expression
in an extended aggregate to be of a class wide type or to be dispatching
call (probably most readers would never dream of doing that anyway).
Thus if we have tagged type @exam[T] and a type @exam[NT] extended
from it and we declare

@begin[Example]
X: T'Class := ... ;
@end[Example]

@leading@keepnext@;then the aggregate

@begin[Example]
@tabset[P42]
NT'(X @key[with] ... )@\-- @examcom[illegal]
@end[Example]

@leading@keepnext@;is illegal. We have to use a type conversion and write

@begin[Example]
@tabset[P42]
NT'(T(X) @key[with] ... )@\-- @examcom[legal]
@end[Example]

@leading@;Similarly the ancestor part cannot be a dispatching call such as
@exam[F(X)] where the function @exam[F] is
@begin[Example]
@tabset[P42]
@key[function] F(Y: T) @key[return] T @key[is]
@key[begin]
   @key[return] Y;
@key[end] F;
...
NT'(F(X) @key[with] ... )@\-- @examcom[illegal since X class wide]
@end[Example]

Again it can be fixed by a suitable conversion to a specific type.
(@AILink{AI=[AI95-00306-01],Text=[AI-306]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-4-3-2.html],
Text=[4.3.2((13.b)]})

@leading@;8 @em If a generic library unit and an instance
of it both have child units with the same name then they now hide
each other. Thus
@begin[Example]
@tabset[P42]
@key[generic package] G @key[is] ... ;@\-- @examcom[a generic G]

@key[generic package] G.C @key[is] ... ;@\-- @examcom[a child C]

@key[with] G;
@key[package] I @key[is new] G;@\-- @examcom[the instance]

@key[package] I.C @key[is] ... ;@\-- @examcom[child of instance]

@key[with] G.C;  @key[with] I.C;@\-- @examcom[illegal, both hidden]
@key[package] P ...
@end[Example]

Originally it seems that this was allowed but it was not specified
which package @exam[C] would refer to. This was fairly foolish and
confusing. (@AILink{AI=[AI95-00220-01],Text=[AI-377]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-8-3.html],
Text=[8.3(29.z)]})

9 @em A subprogram body acting as a declaration
(that is without a distinct specification) cannot with a private child.
This was allowed by mistake originally and permitted the export of
types declared in private child packages.
(@AILink{AI=[AI95-00220-01],Text=[AI-220]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-10-1-2.html],
Text=[10.1.2(31.f)]})

10 @em For the purposes of deciding whether a unit
can be preelaborable a generic formal object is nonstatic.
(@AILink{AI=[AI95-00403-01],Text=[AI-403]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-10-2-1.html],
Text=[10.2.1(28.f)]})

11 @em Storage pools (and the attribute @exam[Storage_Size])
are not permitted for access to subprogram types. Originally it looked
as if they were allowed provided they were never used (or the size
was zero). (@AILink{AI=[AI95-00435-01],Text=[AI-435]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-13-11.html],
Text=[13.11(43.d)]})

12 @em The rules for the two pragmas @exam[Interrupt_Handler] and
@exam[Attach_Handler] are the same with respect to where
they are permitted. Originally it appeared that @exam[Interrupt_Handler]
could be declared in a place remote from the subprogram it was referring
to. (@AILink{AI=[AI95-00253-01],Text=[AI-253]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-C-3-1.html],
Text=[C.3.1(25.a)]})

13 @em There are some changes regarding attributes
in remote type and RCI units. These changes primarily concern streams
for limited types.
(@AILink{AI=[AI95-00240-01],Text=[AI-240]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-E-2-2.html],
Text=[E.2.2(18.a)]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-E-2-3.html],
Text=[E.2.3(20.b)]})


@LabeledSubClause{Inconsistencies with original Ada 95}


There are a small number of inconsistencies between the original Ada
95 and that resulting from various corrections.

1 @em The function @exam[Exception_Identity] applied to the value
@exam[Null_Occurrence] now returns @exam[Null_Id] whereas it originally raised
@exam[Constraint_Error] in Ada 95. See @RefSecNum{Exceptions} .
(@AILink{AI=[AI95-00241-01],Text=[AI-241]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-11.4.1.html],
Text=[11.4.1(19.y)]})

2 @em The procedure @exam[Raise_Exception] applied
to the value @exam[Null_Id] now raises @exam[Constraint_Error] whereas
it originally did nothing (and thus returned). See
@RefSecNum{Pragmas and restrictions}.
(@AILink{AI=[AI95-00446-01],Text=[AI-446]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-11.4.1.html],
Text=[11.4.1(19.aa)]})

3 @em Rounding of static real expressions is now
implementation-defined whereas it was originally defined as away from
zero. The reason for the change is to match the behaviour of the hardware;
this also means that static and non-static expressions are more likely
to get the same answer which is comforting.
(@AILink{AI=[AI95-00268-01],Text=[AI-268]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-4-9.html],
Text=[4.9(44.s)]})

4 @em The @LocalTarget{Target=[Item2-4],Text={lower}} bounds of strings
returned by functions @exam[Exception_Name], @exam[Exception_Message], and
@exam[Exception_ Information] (and wide versions) are now defined to be
@exam[1]. (@AILink{AI=[AI95-00378-01],Text=[AI-378]},
@AILink{AI=[AI95-00417-01],Text=[AI-417]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-11-4-1.html],
Text=[11.4.1(19.z)]})

Similarly the bounds of the various functions @exam[Slice] are now
defined. (@AILink{AI=[AI95-00238-01],Text=[AI-238]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-A-4-4.html],
Text=[A.4.4(106.e)]})

5 @em There are some changes regarding stream attributes.
(@AILink{AI=[AI95-00108-01],Text=[AI-108]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-13-13-2.html],
Text=[13.13.2(60.g)]} and @AILink{AI=[AI95-00195-01],Text=[AI-195]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-13-13-2.html],
Text=[13.13.2(60.h)]})

6 @em There are changes regarding truncation of
stream files. (@AILink{AI=[AI95-00283-01],Text=[AI-283]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-A-12-1.html],
Text=[A.12.1(36.a)]})

7 @em There is a potential inconsistency regarding
the use of @exam[Internal_Tag] outside of streaming. However, there
was an implementation permission to do as is now required and so programs
were not portable anyway. (@AILink{AI=[AI95-00279-01],Text=[AI-279]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-3-9.html],
Text=[3.9(33.b)]})

8 @em The procedure @exam[Update] in @exam[Interfaces.C.Strings]
no longer adds a nul character. (@AILink{AI=[AI95-00242-01],Text=[AI-242]},
@URLLink{URL=[http://www.adaic.org/standards/05aarm/html/AA-B-3-1.html],
Text=[B.3.1(60.a)]})


@LabeledClause{Unfinished topics}


A number of topics which seemed to be good ideas initially were abandoned
for various reasons. Usually the reason was simply that a good solution
could not be produced in the time available and the trouble with a
bad solution is that it is hard to put it right later. In other cases
it is now felt that the topic deserved further consideration in the
light of better understanding; sometimes there was fairly general
agreement that the current situation was not ideal and ought to be
improved, nevertheless there was no agreement on what should be done.
And in some cases the good idea seemed a bad idea after further discussion.

So it might be that when Ada is next revised these further features
might be reconsidered and so perhaps this section might be called
forthcoming attractions. But on the other hand maybe other matters
will need to be dealt with in the light of user experience with Ada
2005.

The following subsections briefly outline the main topics @em for
a fuller discussion, consult the text of the Ada Issue concerned.


@LabeledSubClause{Aggregates for private types}


@leading@;The @exam[<>] notation was introduced for aggregates to mean the
default value if any. See @RefSecNum{Aggregates}. A curiosity is that we can
write
@begin[Example]
@key[type] Secret@key[ is private];

@key[type] Visible @key[is]
   @key[record]
      A: Integer;
      S: Secret;
   @key[end record];

X: Visible := (A => 77; S => <>);
@end[Example]

@leading@;but we cannot write
@begin[Example]
@tabset[P35]
S: Secret := <>;@\-- @examcom[illegal]
@end[Example]

The argument is that this would be of little use since the components
take their default values anyway.

@leading@;For uniformity @AILink{AI=[AI95-00389-01],Text=[AI-389]} proposed
allowing
@begin[Example]
S: Secret := (@key[others] => <>);
@end[Example]

@leading@;for private types and also for task and protected types. One advantage
would be that we could then write
@begin[Example]
S: @key[constant] Secret := (@key[others] => <>);
@end[Example]

whereas at the moment it is not possible to declare a constant of
a private type because we are unable to give an initial value.

However, discussion of this issue lead into a quagmire concerning
the related @AILink{AI=[AI95-00413-01],Text=[AI-413]} and in the end both
were abandoned.


@LabeledSubClause{Partial generic instantiation}


@leading@;Certain attempts to use signature packages lead to circularities.
The AI (@AILink{AI=[AI95-00359-04],Text=[AI-359]})
outlines the following example
@begin[Example]
@key[generic]
   @key[type] Element@key[ is private];
   @key[type] Set @key[is private];
   @key[with function] Union(L, R: Set) @key[return] Set @key[is] <>;
   @key[with function] Intersection(L, R: Set) @key[return] Set @key[is] <>;
   ... -- @examcom[and so on]
@key[package] Set_Signature @key[is end];
@end[Example]

Remember that a signature is a generic package consisting only of
a specification. When we instantiate it, the effect is to assert that
the actual parameters are consistent and the instantiation provides
a name to refer to them as a group.

@leading@;If we now attempt to write
@begin[Example]
@key[generic]
   @key[type] Elem @key[is private];
   @key[with function ]Hash(E: Elem) @key[return] Integer;
@key[package] Hashed_Sets @key[is]
   @key[type] Set @key[is private];
   @key[function] Union(L, R: Set) @key[return] Set;
   @key[function] Intersection(L, R: Set) @key[return] Set;
   ...
   @key[package] Signature @key[is new] Set_Signature(Elem, Set);
@key[private]
   @key[type] Set @key[is]
      @key[record]
         ...
      @key[end record];
@key[end] Hashed_Sets;
@end[Example]

then we are in trouble. The problem is that the instantiation of
@exam[ Set_Signature] tries to freeze the type @exam[Set] prematurely.

Other similar examples concern the use of access types with private
types. The essence of the problem is that we want to instantiate a
package with a private type before the full declaration of that type.

@leading@;The solution proposed was to split an instantiation into two parts,
a partial instantiation and a full (that is, normal) instantiation.
The partial instantiation might take the form
@begin[Example]
@key[package] P @key[is new] G(Private_Type) @key[with private];
@end[Example]

and this can be done with the partial view of the type. The full instantiation
can then be given after the full declaration of the type.

This fell by the wayside at the last minute largely because of fears
that awkward situations might be introduced inadvertently.


@LabeledSubClause{Support for IEEE 559: 1989}


The proposal (@AILink{AI=[AI95-00315-01],Text=[AI-315]})
was to provide full support for all aspects of IEEE
559 arithmetic such as Nans (a Nan is Not A Number). This would have
necessitated adding attributes such as @exam[S'Infinity], @exam[S'Is_Nan],
@exam[S'Finite] and so on plus a package@exam[ Ada.Numerics.IEC_559].

The proposal was abandoned because it would have had a big impact
on implementers and it was not clear that there was sufficient demand.


@LabeledSubClause{Defaults for generic parameters}


Generic subprogram parameters and object parameters of mode in can
have defaults. But other parameters such as packages and types cannot.
This was considered irksome and untidy and efforts (@AILink{AI=[AI95-00299-01],Text=[AI-299]})
were made to define
a suitable notation for all possible generic parameters.

However, it was abandoned partly because an appropriate syntax seemed
hard to find and more importantly, it was not felt to be that important.


@LabeledSubClause{Pre/post-conditions for subprograms}


@leading@;This proposal (@AILink{AI=[AI95-00288-01],Text=[AI-288]}) was to add pragmas
such as @exam[Pre_Assert] and @exam[Post_Assert].
Thus in the case of a subprogram @exam[Push] on a type @exam[Stack]
we might write
@begin[Example]
@key[procedure] Push(S: @key[in out ]Stack; X: @key[in] Item);
@key[pragma] Pre_Assert(Push, @key[not] Is_Full(S));
@key[pragma] Post_Assert(Push, @key[not] Is_Empty(S));
@end[Example]

These pragmas would be controlled by the pragma @exam[Assertion_Policy]
which controls the pragma @exam[Assert] (which was of course incorporated
into Ada 2005). Optional message parameters were allowed as well.

The general idea was that when the procedure @exam[Push] was called,
the expression @exam[Is_Full(S)] would be evaluated and if this were
false then action would be taken as for an @exam[Assert] pragma. Note
that the key difference from assert is that the pragmas go on the
subprogram specification whereas to use @exam[Assert] it would have
to be placed in the body.

There were other pragmas for dispatching subprograms and so this was
not quite so simple as at first appeared.

The proposal was abandoned for a number of reasons. There were more
important matters to deal with and we were running out of time. Moreover,
it seemed just the sort of topic where user experience on a trial
implementation would be helpful in deciding what was required. And
there was some feeling that since this was all dynamic it was not
helpful to the high integrity community where the emphasis was on
static analysis and proof.


@LabeledSubClause{Type and package invariants}


This (@AILink{AI=[AI95-00375-01],Text=[AI-375]}) defined further pragmas similar to
those in the previous proposal (@AILink{AI=[AI95-00288-01],Text=[AI-288]}) but
concerned with packages and types. Thus the pragma @exam[Package_Invariant]
identified a function returning a Boolean result. This function would be
implicitly called after the call of each subprogram in the package and if the
result were false the behaviour would be as for an @exam[Assert] pragma that
failed.

This proposal was abandoned for the same reasons as
@AILink{AI=[AI95-00288-01],Text=[AI-288]}.


@LabeledSubClause{Exceptions as types}


This AI originally arose out of a workshop organized by Ada-Europe.
The proposal was quite complex and considered far too radical a change
and probably expensive to implement. As a consequence it was slimmed
down considerably (see @AILink{AI=[AI95-00264-01],Text=[AI-264]}).
But having been slimmed down it seemed pointless
and was then abandoned. The only part to survive was the idea of raise
with message which became a separate AI and was incorporated into
Ada 2005.


@LabeledSubClause{Sockets operations}


This seemed a very good idea at the time but no detailed proposal
was forthcoming
(@AILink{AI=[AI95-00292-01],Text=[AI-292]} is empty)
and so it died.


@LabeledSubClause{In out parameters for functions}


This is a really interesting topic. Ada functions are curious. On
the one hand they look as if they are going to be well behaved since
they only allow in parameters and thus it appears as if they cannot
have side effects. But of course they can have any side effects they
like by using global variables! And parameters can be access types
and nothing prevents the accessed values from being changed. Indeed
access parameters are a sort of sly way of getting in out parameters
anyway.

The proposal (@AILink{AI=[AI95-00323-01],Text=[AI-323]}) was to allow
functions to have parameters of all modes.
The rationale for the proposal is well summarized in the problem part
of the AI thus "Ada functions can have arbitrary side effects, but
are not allowed to announce that in their specifications".

Clearly, Ada functions are indeed curious. But strangely, this AI
was abandoned quite early in the revision process on the grounds that
it was "too late". (Perhaps too late in this context meant 25 years
too late.) In any event there was no agreement on a way forward since
there are strong arguments both ways. But there was agreement that
time would be better spent discussing and agreeing other matters.

One suggestion is that two kinds of functions should be supported.
Absolutely pure side-effect free functions that merely deliver the
value of some state. Functions in SPARK
@LocalLink{Target=[R9],Sec=[References],Text={[9]}} are like this.
And the other sort of function could be one that is just like a procedure
and can do anything and have all modes of parameters but for convenience
returns a result which can then be used in an expression.

It is interesting to note that Preliminary Ada
@LocalLink{Target=[R11],Sec=[References],Text={[11]}} had value returning
procedures as well as functions. The functions were pure but value returning
procedures were much as current functions and could have side effects. But
value returning procedures could not have out and in out parameters. The
difference between the two was thus not enough and so pure functions were
dropped and value returning procedures became functions.

This topic may deserve to be revisited at some time.


@LabeledSubClause{Application defined scheduling}


The International Real-Time Ada Workshops have been a source of suggestions
for improvements to Ada. The Workshop at Oporto suggested a number
of further scheduling algorithms @LocalLink{Target=[R12],Sec=[References],Text={[12]}}. Most of these such as Round
Robin and EDF have been included in Ada 2005. But that for application
defined scheduling (@AILink{AI=[AI95-00358-01],Text=[AI-358]}) was not.

The reason is perhaps that it was felt desirable to see how those
that had been included worked out before adding yet more burden for
implementers.


@LabeledClause{Acknowledgements}


This is the last of the papers in this series and so this seems a
good moment to once more thank all those who have helped by reviewing
various drafts and pointing out where I had gone astray. I am especially
grateful to Randy Brukardt, Pascal Leroy and Tucker Taft for their
diligence and patience.

I must also thank Ada-Europe and the Ada Resource Association and
also the British Standards Institute for financial support for attending
various meetings.

Writing this rationale has been a learning experience for me and I
trust that readers will also have found the material useful in learning
about Ada 2005. An integrated description of Ada 2005 as a whole including
some further examples will be found in a forthcoming version of the
textbook @LocalLink{Target=[R13],Sec=[References],Text={[13]}}.


