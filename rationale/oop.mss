@Part(oop, Root="rat.msm")

@comment($Source: e:\\cvsroot/ARM/Rationale/oop.mss,v $)
@comment($Revision: 1.9 $ $Date: 2008/01/31 05:06:19 $)

@LabeledSection{Object oriented model}

@i{This chapter describes various important improvements to the object oriented
model for Ada 2005.}

@i{First an alternative more traditional prefixed notation for calling
operations has been introduced. A major improvement is that Java-like
interfaces are introduced thereby permitting simple multiple inheritance; null
procedures have also been introduced as a category of operation. Greater
general flexibility is provided by allowing type extension at a more nested
level than that of the parent.}

@i{There are also explicit features for overcoming nasty bugs which arise from
confusion between overloading and overriding.}

@LabeledClause{Ada Issues: Object-oriented model}

The WG9 guidance document @LocalLink{Target=[R1],Sec=[References],Text={[1]}} identifies
very large complex systems
as a major application area for Ada. It says

"The main purpose of the Amendment is to address identified problems
in Ada that are interfering with Ada's usage or adoption, especially
in its major application areas (such as high-reliability, long-lived
real-time and/or embedded applications and very large complex systems).
The resulting changes may range from relatively minor, to more substantial."

Object oriented techniques are of course important in very large systems
in providing flexibility and extensibility. The document later asks
the ARG to pay particular attention to

@begin[DescribeCode]
Improvements that will remedy shortcomings in Ada. It cites in
particular improvements in OO features, specifically, adding a Java-like
interface feature and improved interfacing to other OO languages.
@end[DescribeCode]

@leading@;Ada 2005 does indeed make many improvements in the object oriented
area. The following Ada Issues cover the relevant changes and are
described in detail in this chapter:

@begin[Description]
@begin[Description]@Comment{Second one to indent this}
@AILink{AI=[AI95-00218-03],Text=[218]}@\Accidental overloading when overriding

@AILink{AI=[AI95-00251-01],Text=[251]}@\Abstract interfaces to provide multiple
inheritance

@AILink{AI=[AI95-00252-01],Text=[252]}@\Object.Operator notation

@AILink{AI=[AI95-00260-02],Text=[260]}@\Abstract formal subprograms & dispatching
constructors

@AILink{AI=[AI95-00284-01],Text=[284]}@\New reserved words

@AILink{AI=[AI95-00310-01],Text=[310]}@\Ignore abstract nondispatching ops during overloading

@AILink{AI=[AI95-00344-01],Text=[344]}@\Allow nested type extensions

@AILink{AI=[AI95-00348-01],Text=[348]}@\Null procedures

@AILink{AI=[AI95-00391-01],Text=[391]}@\Functions with controlling results on null extension

@AILink{AI=[AI95-00396-01],Text=[396]}@\The "no hidden interfaces" rule (this is discussed in Section @RefSecNum{Synchronized interfaces})

@AILink{AI=[AI95-00401-01],Text=[401]}@\Terminology for interfaces

@AILink{AI=[AI95-00405-01],Text=[405]}@\Progenitors and @exam[Ada.Tags]

@AILink{AI=[AI95-00407-01],Text=[407]}@\Terminology and semantics for prefix names

@AILink{AI=[AI95-00411-01],Text=[411]}@\Equality for types derived from interfaces

@AILink{AI=[AI95-00417-01],Text=[417]}@\Lower bound of functions in @exam[Ada.Tags] etc

@AILink{AI=[AI95-00419-01],Text=[419]}@\Limitedness of derived types

@AILink{AI=[AI95-00430-01],Text=[430]}@\Convention of inherited subprograms

@end[Description]
@end[Description]

These changes can be grouped as follows.

First we discuss the fact that Ada 2005 has three new reserved words,
@key[interface], @key[overriding], and @key[synchronized]. It so happens
that these are all used in different aspects of the OO model and so
we discuss them in this chapter (@AILink{AI=[AI95-00284-01],Text=[284]}).

Then there is the introduction of the @exam[Obj.Op] or prefixed notation
used by many other languages (@AILink{AI=[AI95-00252-01],Text=[252]},
@AILink{AI=[AI95-00407-01],Text=[407]}). This should make Ada easier
to use, improve its image, and improve interfacing to other languages.

A huge improvement is the addition of Java-like interfaces which allow
proper multiple inheritance (@AILink{AI=[AI95-00251-01],Text=[251]},
@AILink{AI=[AI95-00396-01],Text=[396]},
@AILink{AI=[AI95-00401-01],Text=[401]},
@AILink{AI=[AI95-00411-01],Text=[411]},
@AILink{AI=[AI95-00419-01],Text=[419]},
@AILink{AI=[AI95-00430-01],Text=[430]}). A related
change is the introduction of null procedures as a category of operation
somewhat like abstract operations (@AILink{AI=[AI95-00348-01],Text=[348]}).

Type extension is now permitted at a more nested level than that of
the parent type (@AILink{AI=[AI95-00344-01],Text=[344]}). An important
consequence is that controlled types no longer need to be declared at library
level.

An interesting development is the introduction of generic functions
for the dynamic creation of objects of any type of a class
(@AILink{AI=[AI95-00260-02],Text=[260]},
@AILink{AI=[AI95-00400-01],Text=[400]},
@AILink{AI=[AI95-00405-01],Text=[405]},
@AILink{AI=[AI95-00417-01],Text=[417]}). These are sometimes called object
factory functions or just object factories.

Additional syntax permits the user to say whether an operation is
expected to be overriding or not (@AILink{AI=[AI95-00218-03],Text=[218]}).
This detects certain unfortunate
errors during compilation which otherwise can be difficult to find
at execution time. A small change to the overriding rules is that
a function with a controlling result does not "go abstract" if an
extension is in fact null (@AILink{AI=[AI95-00391-01],Text=[391]}).
Finally, we discuss a minor but useful
change to the overloading rules; in a sense this is not about OO at
all since it concerns the rules for nondispatching operations but
it is convenient to discuss it here (@AILink{AI=[AI95-00310-01],Text=[310]}).

There are many other OO related improvements in Ada 2005 concerning
matters such as access types, visibility, and generics. They will
be described in Chapters @RefSecNum{Access types} and
@RefSecNum{Exceptions, generics etc}.


@LabeledClause{Reserved words}

Ada 2005 has three further reserved words namely @key[interface],
@key[overriding], and @key[synchronized]. Readers may recall that
Ada 95 had six more reserved words than Ada 83 and the fact that this
meant that some programs were incompatible and thus had to be rewritten
loomed large in the minds of many commentators.@Defn{reserved words}

When new syntax for the introduction of interfaces was being discussed
it was strongly felt that incompatibilities should be avoided and
that any new syntax words should be unreserved. It was also noted
that @exam[Interface] was a popular identifier and that making it
a reserved word would cause many programs to have to be rewritten.

@leading@;However, it was soon realised that treating @exam[Interface]
as unreserved would have permitted sequences such as
@begin[Example]
@key[type] T @key[is interface];
@key[subtype] Interface @key[is] T;
@end[Example]

in which @exam[Interface] is a subtype of the interface @exam[T].
This would have been total madness. Some reviewers also had memories
of PL/I in which words such as IF were not reserved so that one could
write IF IF ... where the first IF is a syntax word and the second
is a user identifier.

Accordingly it was decided that the new words would have to be reserved.
No sensible alternative to @key[interface] could be thought of although
it would be irritating for users who had packages called @exam[Interface]
@en actually a brief survey revealed that most such packages had longer
names such as @exam[Radar_Interface] so that the problem was more
apparent than real. The other new reserved words @key[overriding]
and @key[synchronized] clearly present less of a problem since they
are less likely to have been used as identifiers.


@LabeledClause{The prefixed notation}

@leading@;As mentioned in the Introduction
(see Section @RefSecNum{Overview: The object oriented model}), the Ada 95
object oriented model
has been criticized for not being really OO since the notation for
applying a subprogram (method) to an object emphasizes the subprogram
and not the object. Thus given

@begin[Example]
@key[package] P @key[is]
   @key[type] T @key[is tagged] ... ;

   @key[procedure] Op(X: T; ... );
   ...
@key[end] P;
@end[Example]

@leading@keepnext@;then we usually have to write

@begin[Example]
@tabset(P28)
P.Op(Y, ... );@\-- @examcom[subprogram first]
@end[Example]

@leading@;in order to apply the operation to an object @exam[Y] of type
@exam[T] whereas an OO person would expect to write
@begin[Example]
@tabset(P28)
Y.Op( ... );@\-- @examcom[object first]
@end[Example]

Some hard line OO languages such as Smalltalk take the view that everything
is an object and that all activities are operations upon some object.
Thus adding 2 and 3 can be seen as sending a message to 2 instructing
3 to be added to it. This is clearly an extreme view.

@leading@;Older languages take the view that subprograms are dominant and that
they act upon parameters which might be raw numbers such as 2 or denote
objects such as a circle. Ada 95 primarily takes this view which reflects
its Pascal foundation over 20 years ago. Thus if @exam[Area] is a
function which returns the area of a circle then we write

@begin[Example]
A := Area(A_Circle);
@end[Example]

@leading@;However, when we come to tasks and protected objects Ada takes the
OO view in which the identity of the object comes first. Thus given
a task @exam[Actor] with an entry @exam[Start] we call the entry by
writing

@begin[Example]
Actor.Start( ... );
@end[Example]

So Ada 95 already uses the object notation although it only applies
to concurrent objects such as tasks. Other objects and, in particular,
objects of tagged types have to use the subprogram notation.

@leading@;A major irritation of the subprogram notation is that it is usually
necessary to name the package containing the declaration of the subprogram
thus

@begin[Example]
@tabset(P28)
P.Op(Y, ... );@\-- @examcom[package P mentioned]
@end[Example]

There are two situations when @exam[P] need not be mentioned @en one
is where the procedure call is actually inside the package @exam[P],
the other is where we have a use clause for @exam[P] (and even that
sometimes does not give the required visibility). But these are special
cases.

@leading@;In Ada 2005 we can replace @exam[P.Op(Y, ... );] by the so-called
prefixed notation@Defn{prefixed notation}

@begin[Example]
@tabset(P28)
Y.Op( ... );@\-- @examcom[package P never mentioned]
@end[Example]

@leading@keepnext@;provided that

@begin[Itemize]
@exam[T] is a tagged type,

@exam[Op] is a primitive (dispatching) or class wide operation
of @exam[T],

@exam[Y] is the first parameter of @exam[Op].
@end[Itemize]

The reason there is never any need to mention the package is that,
by starting from the object, we can identify its type and thus the
primitive operations of the type. Note that a class wide operation
can be called in this way only if it is declared at the same place
as the primitive operations of @exam[T] (or one of its ancestors).
The parameter @exam[Y] need not be simply the name of an object.
It can be anything allowed as a parameter such as a dereference or
a function call. But the type @exam[T] must be tagged.

There are many advantages of the prefixed notation as we shall see
but perhaps the most important is ease of maintenance from not having
to mention the package containing the declaration of the operation.
Having to name the package is often tricky because in complicated
situations involving several levels of inheritance it may not be obvious
where the operation is declared. This happens especially when operations
are declared implicitly and when class-wide operations are involved.
Moreover if we change the structure for some reason then operations
might move.

As a simple example consider a hierarchy of plane geometrical object
types. All objects have a position given by the two coordinates @i[x]
and @i[y] (this is the position of the centre of gravity of the object).
There will be other specific properties according to the type such
as the radius of a circle. In addition there might be general properties
such as the area of the object, its distance from the origin and moment
of inertia about its centre.

There are a number of ways in which such a hierarchy might be structured.
We might have a package declaring a root abstract type and then another
package with several derived types.

@begin[Example]
@key[package] Root @key[is]
   @key[type] Object @key[is abstract tagged]
      @key[record]
         X_Coord: Float;
         Y_Coord: Float;
      @key[end record];

   @key[function] Area(O: Object) @key[return] Float @key[is abstract];
   @key[function] MI(O: Object) @key[return] Float@key[ is abstract];
   @key[function] Distance(O: Object) @key[return] Float;
@key[end] Root;

@key[package] @key[body] Root @key[is]
   @key[function] Distance(O: Object) @key[return] Float @key[is]
   @key[begin]
      @key[return] Sqrt(O.X_Coord**2 + O.Y_Coord**2);
   @key[end] Distance;
@key[end] Root;
@end[Example]

@leading@;This package declares the root type and two abstract operations
@exam[Area] and @exam[MI] (moment of inertia) and a concrete operation
@exam[Distance]. We might then have

@begin[Example]
@tabset(P28)
@key[with] Root;
@key[package] Shapes @key[is]
   @key[type] Circle @key[is new ]Root.Object @key[with]
      @key[record]
         Radius: Float;
      @key[end record];

   @key[function] Area(C: Circle) @key[return] Float;
   @key[function] MI(C: Circle) @key[return] Float;

   @key[type] Triangle @key[is new] Root.Object @key[with]
      @key[record]
         A, B, C: Float;@\-- @examcom[lengths of sides]
      @key[end record];

   @key[function] Area(T: Triangle) @key[return] Float;
   @key[function] MI(T: Triangle) @key[return] Float;

-- @examcom[and so on for other types such as Square]

@key[end] Shapes;
@end[Example]

(In the following discussion we will assume that use clauses are not
being used. This is quite realistic because many projects forbid use
clauses.)

@leading@;Having declared some objects such as @exam[A_Circle] and
@exam[A_Triangle] we can then apply the operations @exam[Area],
@exam[Distance], and @exam[MI]. In Ada 95 we write

@begin[Example]
A := Shapes.Area(A_Circle);
D := Shapes.Distance(A_Triangle);
M := Shapes.MI(A_Square);
@end[Example]

@leading@;Observe that the operation @exam[Distance] is inherited and so is
implicitly declared in the package @exam[Shapes] for all types even
though there is no mention of it in the text of the package @exam[Shapes].
However, if we were using Ada 2005 and the prefixed notation then
we could simply write
@begin[Example]
A := A_Circle.Area;
D := A_Triangle.Distance;
M := A_Square.MI;
@end[Example]

and there is no mention of the package @exam[Shapes] at all.

@leading@;A clever friend then points out that by its nature @exam[Distance]
is the same for all types so it would be safer to avoid the risk of
it getting changed by making it class wide. So we change the declaration
of @exam[Distance] in the package @exam[Root] thus
@begin[Example]
   @key[function] Distance(O: Object'Class) @key[return] Float;
@end[Example]

@leading@;and recompile our program. But the Ada 95 version won't recompile.
Why? Because class wide operations are not inherited. So there is
only one function @exam[Distance] and it is declared in the package
@exam[Root]. So all our calls of @exam[Distance] have to be changed to
@begin[Example]
D := Root.Distance(A_Triangle);
@end[Example]

However, if we had been using the prefixed notation then there would
have been nothing to change.

@leading@;Our manager might then read about the virtues of child packages and
tell us to restructure the whole thing as follows
@begin[Example]
@key[package] Geometry @key[is]
   @key[type] Object @key[is abstract] ...

   ... -- @examcom[functions Area, MI, Distance]
@key[end] Geometry;

@key[package] Geometry.Circles @key[is]
   @key[type] Circle@key[ is new] Object @key[with]
      @key[record]
         Radius: Float;
      @key[end record];

   ... -- @examcom[functions Area, MI]
@key[end] Geometry.Circles;

@key[package] Geometry.Triangles @key[is]
   @key[type] Triangle@key[ is new] Object @key[with]
      @key[record]
         A, B, C: Float;
      @key[end record];

   ... -- @examcom[functions Area, MI]
@key[end] Geometry.Triangles;

-- @examcom[and so on]
@end[Example]

@leading@;This is of course a much more beautiful structure and avoids having
to write @exam[Root.Object] when doing the extensions. But, horrors,
our assignments in Ada 95 now have to be changed to

@begin[Example]
A := Geometry.Circles.Area(A_Circle);
D := Geometry.Distance(A_Triangle);
M := Geometry.Squares.MI(A_Square);
@end[Example]

@leading@;But the lucky programmer using Ada 2005 can still write
@begin[Example]
A := A_Circle.Area;
D := A_Triangle.Distance;
M := A_Square.MI;
@end[Example]

and have a refreshing coffee (or a relaxing martini) while we are
toiling with the editor.

@leading@;Some time later the program might be extended to accommodate
triangles that are specialized to be equilateral. This might be done by
@begin[Example]
@key[package] Geometry.Triangles.Equilateral @key[is]
   @key[type]  Equilateral_Triangle @key[new] Triangle @key[with private];
   ...
@key[private]
   ...
@key[end];
@end[Example]

@leading@;This type of course inherits all the operations of the type
@exam[Triangle]. We might now realize that the object @exam[A_Triangle] of type
@exam[Triangle] was equilateral anyway and so it would be better to change it
to be of type @exam[Equilateral_Triangle]. The lucky Ada 2005 programmer will
only have to change the declaration of the object but the poor Ada 95
programmer will have to change the calls on all its primitive operations such
as
@begin[Example]
A := Geometry.Triangles.Area(A_Triangle);
@end[Example]

@leading@keepnext@;to the corresponding
@begin[Example]
A := Geometry.Triangles.Equilateral.Area(A_Triangle);
@end[Example]

@leading@;Other advantages of the @exam[prefixed] notation were mentioned in
the Introduction. One is that it unifies the notation for calling
a function with a single parameter and directly reading a component
of the object. Thus we can write uniformly
@begin[Example]
X := A_Circle.X_Coord;
A := A_Circle.Area;
@end[Example]

Of course if we were foolish and had a @i[visible] component
@exam[Area] as well as a function @exam[Area] then we could not call the
function in this way.

@leading@;But now suppose we decide to make the root type private so that the
coordinates cannot be changed inadvertently. Moreover we decide to
provide functions to read them. So we have
@begin[Example]
@key[package] Geometry @key[is]
   @key[type] Object @key[is abstract tagged private;]
   @key[function] Area(O: Object) @key[return] Float @key[is abstract];
   @key[function] MI(O: Object) @key[return] Float@key[ is abstract];
   @key[function] Distance(O: Object'Class) @key[return] Float;

   @key[function] X_Coord(O: Object'Class) @key[return] Float;
   @key[function] Y_Coord(O: Object'Class) @key[return] Float;

@key[private]
   @key[type] Object @key[is tagged]
      @key[record]
         X_Coord: Float;
         Y_Coord: Float;
      @key[end record];

@key[end] Geometry;
@end[Example]

@leading@keepnext@;Using Ada 95 we would now have to change statements such as
@begin[Example]
X := A_Triangle.X_Coord;
Y := A_Triangle.Y_Coord;
@end[Example]

@leading@keepnext@;into
@begin[Example]
X := Geometry.X_Coord(A_Triangle);
Y := Geometry.Y_Coord(A_Triangle);
@end[Example]

@leading@;or (if we had not been wise enough to make the functions class wide)
perhaps even
@begin[Example]
X := Geometry.Triangles.Equilateral.X_Coord(A_Triangle);
Y := Geometry.Triangles.Equilateral.Y_Coord(A_Triangle);
@end[Example]

whereas in Ada 2005 we do not have to make any changes at all.

@leading@;Another advantage mentioned in the Introduction is that when using
access types explicit dereferencing is not necessary. Suppose we have
@begin[Example]
@key[type] Pointer @key[is access all] Geometry.Object'Class;
...
This_One: Pointer := A_Circle'Access;
@end[Example]

@leading@;In Ada 95 (assuming that @exam[X_Coord] is a visible component) we
have to write
@begin[Example]
Put(This_One.X_Coord); ...
Put(This_One.Y_Coord); ...
Put(Geometry.Area(This_One.@key[all]));
@end[Example]

@leading@keepnext@;whereas in Ada 2005 we can uniformly write
@begin[Example]
Put(This_One.X_Coord); ...
Put(This_One.Y_Coord); ...
Put(This_One.Area);
@end[Example]

and of course this remains unchanged if we make the coordinates into
functions whereas the Ada 95 statements will need to be changed.

There are other structural changes that can occur during program development
which are much easier to cope with using the prefix notation. For
example, a class wide operation might be moved. And in the case of
multiple interfaces to be described in the next section an operation
might be moved from one interface to another.

It is clear that the prefixed notation has significant benefits both
in terms of program clarity and for program maintenance.

Other variations on the rules for the use of the notation were considered.
One was that the mechanism should apply to untagged types as well
but this was rejected on the grounds that it might add to rather than
reduce confusion in some cases. In any event, untagged types do not
have class wide types so they are intrinsically simpler.
It would have been particularly confusing to permit the notation to apply to
access types especially an access type @exam[A] referring to a tagged
type @exam[T]. If the access type and the tagged type both had the same or
similar operations @exam[Op] then ambiguities or errors could easily arise.

@leading@;It is of course important to note that the first parameter of an
operation plays a special role since in order to take advantage of the prefixed
notation we have to ensure that the first parameter is a controlling
parameter. Treating the first parameter specially can appear odd in
some circumstances such as when there is symmetry among the parameters.
Thus suppose we have a set package for creating and manipulating sets
of integers
@begin[Example]
@key[package] Sets @key[is]
   @key[type] Set @key[is tagged private];
   @key[function] Empty @key[return] Set;
   @key[function] Unit(N: Integer) @key[return] Set;
   @key[function] Union(S, T: Set) @key[return] Set;
   @key[function] Intersection(S, T: Set) @key[return] Set;
   @key[function] Size(S: Set) @key[return] Integer;
   ...
@key[end] Sets;
@end[Example]

@leading@;then we can apply the function @exam[Union] in the traditional way
@begin[Example]
A, B, C: Set;
...
C := Sets.Union(A, B);
@end[Example]

@leading@keepnext@;The object oriented addict can also write
@begin[Example]
C := A.Union(B);
@end[Example]

but this destroys the obvious symmetry and is rather like sending
3 to be added to 2 mentioned at the beginning of this discussion.

@leading@;Hopefully the mature programmer will use the OO notation wisely.
Maybe its existence will encourage a more uniform style in which the first
parameter is always a controlling operand wherever possible. Of course it
cannot be used for functions which are tag indeterminate such as
@begin[Example]
   @key[function] Empty @key[return] Set;
   @key[function] Unit(N: Integer) @key[return] Set;
@end[Example]

since there are no controlling parameters. If a subprogram has just
one parameter (which is controlling) such as @exam[Size] then the
call just becomes @exam[X.Size] and no parentheses are necessary.

@leading@;Remember that the prefix does not have to be simply the name
of an object such as @exam[A_Circle] or an implicit dereference such as
@exam[This_One], it could be a function call so we might write
@begin[Example]
@tabset{P35}
N := Sets.Empty.Size;@\-- @examcom[N = 0]
M := Sets.Unit(99).Size;@\-- @examcom[M = 1]
@end[Example]

with the obvious results as indicated.


@LabeledClause{Interfaces}

In Ada 95, a derived type can really only have one immediate ancestor.
This means that true multiple inheritance is not possible although
curious techniques involving discriminants and generics can be used
in some circumstances.@Defn{multiple inheritance}

@leading@;General multiple inheritance has problems. Suppose that we have a
type @exam[T] with some components and operations. Perhaps
@begin[Example]
@key[type] T @key[is tagged]
   @key[record]
      A: Integer;
      B: Boolean;
   @key[end record];

@key[procedure] Op1(X: T);
@key[procedure] Op2(X: T);
@end[Example]

@leading@keepnext@;Now suppose we derive two new types from @exam[T] thus
@begin[Example]
@key[type] T1 @key[is new] T @key[with]
   @key[record]
      C: Character;
   @key[end record];

@key[procedure] Op3(X: T1);

-- @examcom[Op1 and Op2 inherited, Op3 added]

@key[type] T2 @key[is new] T @key[with]
   @key[record]
      C: Colour;
   @key[end record];

@key[procedure] Op1(X: T2);
@key[procedure] Op4(X: T2);

-- @examcom[Op1 overridden, Op2 inherited, Op4 added]
@end[Example]

@leading@;Now suppose that we were able to derive a further type from both
@exam[T1] and @exam[T2] by perhaps writing
@begin[Example]
@tabset{P49}
@key[type] TT @key[is new] T1 @key[and] T2 @key[with null record];@\-- @examcom[illegal]
@end[Example]

This is about the simplest example one could imagine. We have added
no further components or operations. But what would @exam[TT] have
inherited from its two parents?

There is a general rule that a record cannot have two components with
the same identifier so presumably it has just one component @exam[A]
and one component @exam[B]. But what about @exam[C]? Does it inherit
the character or the colour? Or is it illegal because of the clash?
Suppose @exam[T2] had a component @exam[D] instead of @exam[C]. Would
that be OK? Would @exam[TT] then have four components?

And then consider the operations. Presumably it has both @exam[Op1]
and @exam[Op2]. But which implementation of @exam[Op1]? Is it the
original @exam[Op1] inherited from @exam[T] via @exam[T1] or the overridden
version inherited from @exam[T2]? Clearly it cannot have both. But
there is no reason why it cannot have both @exam[Op3] and @exam[Op4],
one inherited from each parent.

The problems arise when inheriting components from more than one parent
and inheriting different@i[ implementations ]of the same operation
from more than one parent. There is no problem with inheriting the
same specification of an operation from two parents.

These observations provide the essence of the solution. At most one
parent can have components and at most one parent can have concrete
operations @en for simplicity we make them the same parent. But abstract
operations can be inherited from several parents. This can be phrased
as saying that this kind of multiple inheritance is about merging
contracts to be satisfied rather than merging algorithms or state.

So Ada 2005 introduces the concept of an interface which is a tagged
type with no components and no concrete operations. The idea of a
null procedure as an operation of a tagged type is also introduced;
this has no body but behaves as if it has a null body. Interfaces
are only permitted to have abstract subprograms and null procedures
as operations.@Defn{interface}

We will outline the ways in which interfaces can be declared and composed
in a symbolic way and then conclude with a more practical example.

@leading@;We might declare a package @exam[Pi1] containing an interface
@exam[Int1] thus
@begin[Example]
@key[package] Pi1 @key[is]
   @key[type] Int1 @key[is interface];
   @key[procedure] Op1(X: Int1) @key[is abstract];
   @key[procedure] N1(X: Int1) @key[is null];
@key[end] Pi1;
@end[Example]

Note the syntax. It uses the new reserved word @key[interface]. It
does not say @key[tagged] although all interface types are tagged.
The abstract procedure @exam[Op1] has to be explicitly stated to be
abstract as usual. The null procedure @exam[N1] uses new syntax as
well. Remember that a null procedure behaves as if its body comprises
a single null statement; but it doesn't actually have a concrete body.

@leading@;The main type derivation rule then becomes that a tagged type can
be derived from zero or one conventional tagged types plus zero or
more interface types. Thus
@begin[Example]
@key[type] NT @key[is new] T @key[and] Int1 @key[and] Int2 @key[with]
... ;
@end[Example]

where @exam[Int1] and @exam[Int2] are interface types. The normal
tagged type if any has to be given first in the declaration. The first
type is known as the parent so the parent could be a normal tagged
type or an interface. The other types are known as progenitors. Additional
components and operations are allowed in the usual way.@Defn{parent}@Defn{progenitor}

The term progenitors may seem strange but the term ancestors in this
context was confusing and so a new term was necessary. Progenitors
comes from the Latin progignere, to beget, and so is very appropriate.

It might have been thought that it would be quite feasible to avoid
the formal introduction of the concept of an interface by simply saying
that multiple parents are allowed provided only the first has components
and concrete operations. However, there would have been implementation
complexities with the risk of violating privacy and distributed overheads.
Moreover, it would have caused maintenance problems since simply adding
a component to a type or making one of its abstract operations concrete
would cause errors elsewhere in the system if it was being used as
a secondary parent. It is thus much better to treat interfaces as
a fundamentally new concept. Another advantage is that this provides
a new class of generic parameter rather neatly without complex rules
for instantiations.

@leading@;If the normal tagged type @exam[T] is in a package @exam[Pt] with
operations @exam[Opt1], @exam[Opt2] and so on we could now write
@begin[Example]
@tabset{P35}
@key[with] Pi1, Pt;
@key[package] PNT @key[is]
   @key[type] NT @key[is new] Pt.T @key[and] Pi1.Int1 @key[with] ... ;
   @key[procedure] Op1(X: NT);@\-- @examcom[concrete procedure]
   -- @examcom[ possibly other ops of NT]
@key[end] PNT;
@end[Example]

We must of course provide a concrete procedure for @exam[Op1] inherited
from the interface @exam[Int1] since we have declared @exam[NT] as
a concrete type. We could also provide an overriding for @exam[N1]
but if we do not then we simply inherit the null procedure of @exam[Int1].
We could also override the inherited operations @exam[Opt1] and @exam[Opt2]
from @exam[T] in the usual way.

@leading@keepnext@;Interfaces can be composed from other interfaces thus
@begin[Example]
@key[type] Int2@key[ is interface];
...
@key[type] Int3 @key[is interface and] Int1;
...
@key[type] Int4 @key[is interface and] Int1 @key[and] Int2;
...
@end[Example]

Note the syntax. A tagged type declaration always has just one of
@key[interface], @key[tagged] and @key[with] (it doesn't have any
if it is not a tagged type). When we derive interfaces in this way
we can add new operations so that the new interface such as @exam[Int4]
will have all the operations of both @exam[Int1] and @exam[Int2] plus
possibly some others declared specifically as operations of @exam[Int4].
All these operations must be abstract or null and there are fairly
obvious rules regarding what happens if two or more of the ancestor
interfaces have the same operation. Thus a null procedure overrides
an abstract one but otherwise repeated operations must have profiles
that are type conformant and have the same convention.

We refer to all the interfaces in an interface list as progenitors.
So @exam[Int1] and @exam[Int2] are progenitors of @exam[Int4]. The
first one is not a parent @en that term is only used when deriving
a type as opposed to composing an interface.

Note that the term ancestor covers all generations whereas parent
and progenitors are first generation only.

@leading@;Similar rules apply when a tagged type is derived from another type
plus one or more interfaces as in the case of the type @exam[NT] which
was
@begin[Example]
@key[type] NT @key[is new] T @key[and] Int1 @key[and] Int2 @key[with] ... ;
@end[Example]

In this case it might be that @exam[T] already has some of the operations
of @exam[Int1] and/or @exam[Int2]. If so then the operations of @exam[T]
must match those of @exam[Int1] or @exam[Int2] (be type conformant
etc).

We informally speak of a specific tagged type as implementing an interface
from which it is derived (directly or indirectly). The phrase "implementing
an interface" is not used formally in the definition of Ada 2005 but
it is useful for purposes of discussion.@Defn{implementing an interface}

Thus in the above example the tagged type @exam[NT] must implement
all the operations of the interfaces @exam[Int1] and @exam[Int2].
If the type @exam[T] already implements some of the operations then
the type @exam[NT] will automatically implement them because it will
inherit the implementations from @exam[T]. It could of course override
such inherited operations in the usual way.

@leading@;The normal "going abstract" rules apply in the case of functions.
Thus if one operation is a function @exam[F] thus
@begin[Example]
@key[package] Pi2 @key[is]
   @key[type] Int2 @key[is interface];
   @key[function] F(Y: Int2) @key[return] Int2@key[ is abstract];
@key[end] Pi2;
@end[Example]

@leading@keepnext@;and @exam[T] already has such a conforming operation

@begin[Example]
@key[package] PT @key[is]
   @key[type] T @key[is tagged record] ...
   @key[function] F(X: T) @key[return] T;
@key[end] PT;
@end[Example]

then in this case the type @exam[NT] must provide a concrete function
@exam[F]. See however the discussion in Section
@RefSecNum{Overriding and overloading}) for
the case when the type @exam[NT] has a null extension.

@leading@;Class wide types also apply to interface types. The class wide type
@exam[Int1'Class] covers all the types derived from the interface
@exam[Int1] (both other interfaces as well as normal tagged types).
We can then dispatch using an object of a concrete tagged type in
that class in the usual way since we know that any abstract operation
of @exam[Int1] will have been overridden. So we might have

@begin[Example]
@key[type] Int1_Ref @key[is access all] Int1'Class;
NT_Var: @key[aliased] NT;
Ref: Int1_Ref := NT_Var'Access;
@end[Example]

Observe that conversion is permitted between the access to class wide
type @exam[Int1_Ref] and any access type that designates a type derived
from the interface type @exam[Int1].

Interfaces can also be used in private extensions and as generic parameters.

@leading@keepnext@;Thus
@begin[Example]
   @key[type] PT @key[is new ]T @key[and] Int2 @key[and] Int3 @key[with private];
   ...
@key[private]
   @key[type] PT @key[is new] T @key[and] Int2 @key[and] Int3 @key[with null record];
@end[Example]

An important rule regarding private extensions is that the full view
and the partial view must agree with respect to the set of interfaces
they implement. Thus although the parent in the full view need not
be @exam[T] but can be any type derived from @exam[T], the same is
not true of the interfaces which must be such that they both implement
the same set exactly. This rule is important in order to prevent a
client type from overriding private operations of the parent if the
client implements an interface added in the private part.

@leading@keepnext@;Generic parameters take the form
@begin[Example]
@key[generic]
   @key[type] FI @key[is interface and] Int1 @key[and] Int2;
@key[package] ...
@end[Example]

and then the actual parameter must be an interface which implements
all the ancestors @exam[Int1], @exam[Int2] etc. The formal could also
just be @key[type]@exam[ FI ]@key[is interface]@exam[;] in which case
the actual parameter can be any interface. There might be subprograms
passed as further parameters which would require that the actual has
certain operations. The interfaces @exam[Int1] and @exam[Int2] might
themselves be formal parameters occurring earlier in the parameter
list.

@leading@keepnext@;Interfaces (and formal interfaces) can also be limited thus
@begin[Example]
@key[type] LI@key[ is limited interface];
@end[Example]

We can compose mixtures of limited and nonlimited interfaces but if
any one of them is nonlimited then the resulting interface must not
be specified as limited. This is because it must implement the equality
and assignment operations implied by the nonlimited interface. Similar
rules apply to types which implement one or more interfaces. We will
come back to this topic in a moment.

There are other forms of interfaces, namely synchronized interfaces,
task interfaces, and protected interfaces. These bring support for
polymorphic, class wide object oriented programming to the real time
programming arena. They are described in Section
@RefSecNum{Synchronized interfaces}.

Having described the general ideas in somewhat symbolic terms, we
will now discuss a more concrete example.

Before doing so it is important to emphasize that interfaces cannot
have components and therefore if we are to perform multiple inheritance
then we should think in terms of abstract operations to read and write
components rather than the components themselves. This is standard
OO thinking anyway because it preserves abstraction by hiding implementation
details.

Thus rather than having a component such as @exam[Comp] it is better
to have a pair of operations. The function to read the component can
simply be called @exam[Comp]. A procedure to update the component
might be @exam[Set_Comp]. We will generally use this convention although
it is not always appropriate to treat the components as unrelated
entities.

@leading@;Suppose now that we want to print images of the geometrical objects.
We will assume that the root type is declared as
@begin[Example]
@key[package] Geometry @key[is]
   @key[type] Object @key[is abstract tagged private];
   @key[procedure] Move(O: @key[in out] Object'Class; X, Y: @key[in] Float);
   ...
@key[private]
   @key[type] Object @key[is abstract tagged]
      @key[record]
         X_Coord: Float := 0.0;
         Y_Coord: Float := 0.0;
      @key[end record];
   ...
@key[end];
@end[Example]

The type @exam[Object] is private and by default both coordinates
have the value of zero. The procedure @exam[Move], which is class
wide, enables any object to be moved to the location specified by
the parameters.

@leading@;Suppose also that we have a line drawing package with the following
specification
@begin[Example]
@key[package] Line_Draw @key[is]
   @key[type] Printable @key[is interface];
   @key[type] Colour @key[is] ... ;
   @key[type] Points @key[is] ... ;
   @key[procedure] Set_Hue(P: @key[in out] Printable; C: @key[in] Colour) @key[is abstract];
   @key[function] Hue(P: Printable) @key[return] Colour @key[is abstract];
   @key[procedure] Set_Width(P: @key[in out] Printable; W: @key[in] Points) @key[is abstract];
   @key[function] Width(P: Printable) @key[return] Points @key[is abstract];

   @key[type] Line @key[is] ... ;
   @key[type] Line_Set @key[is] ... ;

   @key[function] To_Lines(P: Printable) @key[return] Line_Set @key[is abstract];

   @key[procedure] Print(P: @key[in] Printable'Class);

@key[private]
   @key[procedure] Draw_It(L: Line; C: Colour; W: Points);

@key[end] Line_Draw;
@end[Example]

The idea of this package is that it enables the drawing of an image
as a set of lines. The attributes of the image are the hue and the
width of the lines and there are pairs of subprograms to set and read
these properties of any object of the interface @exam[Printable] and
its descendants. These operations are of course abstract.

In order to prepare an object in a form that can be printed it has
to be converted to a set of lines. The function @exam[To_Lines] converts
an object of the type @exam[Printable] into a set of lines; again
it is abstract. The details of various types such as @exam[Line] and
@exam[Line_Set] are not shown.

Finally the package @exam[Line_Draw] declares a concrete procedure
@exam[Print] which takes an object of type @exam[Printable'Class]
and does the actual drawing using the slave procedure @exam[Draw_It]
declared in the private part. Note that @exam[Print] is class wide
and is concrete. This is an important point. Although all primitive
operations of an interface must be abstract this does not apply to
class wide operations since these are not primitive.

@leading@;The body of the procedure @exam[Print] could take the form
@begin[Example]
@key[procedure] Print(P: @key[in] Printable'Class) @key[is]
   L: Line_Set := To_Lines(P);
   A_Line: Line;
@key[begin]
   @key[loop]
      -- @examcom[iterate over the Line_Set and extract each line]
      A_Line := ...
      Draw_It(A_Line, Hue(P), Width(P));
   @key[end loop];
@key[end] Print;
@end[Example]

but this is all hidden from the user. Note that the procedure @exam[Draw_It]
is declared in the private part since it need not be visible to the
user.

One reason why the user has to provide @exam[To_Lines] is that only
the user knows about the details of how best to represent the object.
For example the poor circle will have to be represented crudely as
a polygon of many sides, perhaps a hectogon of 100 sides.

@leading@;We can now take at least two different approaches. We can for example
write
@begin[Example]
@key[with] Geometry, Line_Draw;@key[
package] Printable_Geometry @key[is]
   @key[type] Printable_Object @key[is]
                         @key[abstract new] Geometry.Object @key[and] Line_Draw.Printable @key[with private];
   @key[procedure] Set_Hue(P: @key[in out] Printable_Object; C: @key[in] Colour);
   @key[function] Hue(P: Printable_Object) @key[return] Colour;
   @key[procedure] Set_Width(P: @key[in out] Printable_Object; W: @key[in] Points);
   @key[function] Width(P: Printable_Object) @key[return] Points;
   @key[function] To_Lines(P: Printable_Object) @key[return] Line_Set @key[is abstract];

@key[private]
   ...
@key[end] Printable_Geometry;
@end[Example]

@leading@;The type @exam[Printable_Object] is a descendant of both
@exam[Object] and @exam[Printable] and all concrete types descended from
@exam[Printable_Object] will therefore have all the operations of both
@exam[Object] and @exam[Printable]. Note carefully that we have to put
@exam[Object] first in the declaration of @exam[Printable_Object] and that the
following would be illegal
@begin[Example]
@key[type] Printable_Object @key[is]
        @key[abstract new] Line_Draw.Printable @key[and] Geometry.Object @key[with private];    -- @examcom[illegal]
@end[Example]

This is because of the rule that only the first type in the list can
be a normal tagged type; any others must be interfaces. Remember that
the first type is always known as the parent type and so the parent
type in this case is @exam[Object].

@leading@;The type @exam[Printable_Object] is declared as abstract because we
do not want to implement @exam[To_Lines] at this stage. Nevertheless
we can provide concrete subprograms for all the other operations of
the interface @exam[Printable]. We have given the type a private extension
and so in the private part of its containing package we might have
@begin[Example]
@key[private]
    @key[type] Printable_Object @key[is abstract new] Geometry.Object @key[and] Line_Draw.Printable @key[with]
      @key[record]
         Hue: Colour := Black;
         Width: Points := 1;
      @key[end record];
@key[end] Printable_Geometry;
@end[Example]

@leading@;Just for way of illustration, the components have been given default
values. In the package body the operations such as the function @exam[Hue]
are simply
@begin[Example]
   @key[function] Hue(P: Printable_Object) @key[return] Colour @key[is]
   @key[begin]
      @key[return] P.Hue;
   @key[end];
@end[Example]

Luckily the visibility rules are such that this does not do an infinite
recursion!

Note that the information containing the style components is in the
record structure following the geometrical properties. This is a simple
linear structure since interfaces cannot add components. However,
since the type @exam[Printable_Object] has all the operations of both
an @exam[Object] and a @exam[Printable], this adds a small amount
of complexity to the arrangement of dispatch tables. But this detail
is hidden from the user.

@leading@;The key point is that we can now pass any object of the type
@exam[Printable_Object] or its descendants to the procedure
@begin[Example]
@key[procedure] Print(P: @key[in] Printable'Class);
@end[Example]

and then (as outlined above) within @exam[Print] we can find the colour
to be used by calling the function @exam[Hue] and the line width to
use by calling the function @exam[Width ]and we can convert the object
into a set of lines by calling the function @exam[To_Lines].

And now we can declare the various types @exam[Circle], @exam[Triangle],
@exam[Square] and so on by making them descendants of the type
@exam[Printable_Object] and in each case we have to implement the function
@exam[To_Lines].

The unfortunate aspect of this approach is that we have to move the
geometry hierarchy. For example the triangle package might now be
@begin[Example]
@key[package] Printable_Geometry.Triangles @key[is]
   @key[type] Printable_Triangle @key[is new] Printable_Object @key[with]
      @key[record]
         A, B, C: Float;
      @key[end record];
   ... -- @examcom[functions Area, To_Lines etc]
@key[end];
@end[Example]

@leading@keepnext@;We can now declare a @exam[Printable_Triangle] thus
@begin[Example]
A_Triangle: Printable_Triangle := (Printable_Object @key[with] A => 4.0, B => 4.0, C => 4.0);
@end[Example]

This declares an equilateral triangle with sides of length 4.0. Its
private @exam[Hue] and @exam[Width] components are set by default.
Its coordinates which are also private are by default set to zero
so that it is located at the origin. (The reader can improve the example
by making the components @exam[A],@exam[ B] and @exam[C] private as
well.)

@leading@;We can conveniently move it to wherever we want by using the
procedure @exam[Move] which being class wide applies to all types derived from
@exam[Object]. So we can write
@begin[Example]
A_Triangle.Move(1.0, 2.0);
@end[Example]

@leading@keepnext@;And now we can make a red sign
@begin[Example]
Sign: Printable_Triangle := A_Triangle;
@end[Example]

@leading@;Having declared the object @exam[Sign], we can give it width and hue
and print it
@begin[Example]
@tabset(P35)
Sign.Set_Hue(Red);
Sign.Set_Width(3);
Sign.Print;@\-- @examcom[print thick red triangle]
@end[Example]

As we observed earlier this approach has the disadvantage that we
had to move the geometry hierarchy. A different approach which avoids
this is to declare printable objects of just the kinds we want as
and when we want them.

@leading@;So assume now that we have the package @exam[Line_Draw] as before
and the original package @exam[Geometry] and its child packages. Suppose
we want to make printable triangles and circles. We could write
@begin[Example]
@key[with] Geometry, Line_Draw;  @key[use ]Geometry;@key[
package] Printable_Objects @key[is]
   @key[type] Printable_Triangle @key[is new] Triangles.Triangle @key[and] Line_Draw.Printable @key[with private];
   @key[type] Printable_Circle@key[ is new] Circles.Circle @key[and] Line_Draw.Printable @key[with private];

   @key[procedure] Set_Hue(P: @key[in out] Printable_Triangle; C: @key[in] Colour);
   @key[function] Hue(P: Printable_Triangle @key[return] Colour;
   @key[procedure] Set_Width(P: @key[in out] Printable_Triangle; W: @key[in] Points);
   @key[function] Width(P: Printable_Triangle) @key[return] Points;
   @key[function] To_Lines(T: Printable_Triangle) @key[return] Line_Set;

   @key[procedure] Set_Hue(P: @key[in out] Printable_Circle; C: @key[in] Colour);
   @key[function] Hue(P: Printable_Circle) @key[return] Colour;
   @key[procedure] Set_Width(P: @key[in out] Printable_Circle; W: @key[in] Points);
   @key[function] Width(P: Printable_Circle) @key[return] Points;
   @key[function] To_Lines(C: Printable_Circle) @key[return] Line_Set;
@key[private]

   @key[type] Printable_Triangle @key[is new] Triangles.Triangle @key[and] Line_Draw.Printable @key[with]
      @key[record]
         Hue: Colour := Black;
         Width: Points := 1;
      @key[end record];

   @key[type] Printable_Circle@key[ is new] Circles.Circle @key[and] Line_Draw.Printable @key[with]
      @key[record]
         Hue: Colour := Black;
         Width: Points := 1;
      @key[end record];
@key[end] Printable_Objects;
@end[Example]

and the body of the package will provide the various subprogram bodies.

@leading@;Now suppose we already have a normal triangle thus
@begin[Example]
A_Triangle: Geometry.Triangles.Triangle := ... ;
@end[Example]

@leading@;In order to print @exam[A_Triangle] we first have to declare a
printable triangle thus
@begin[Example]
Sign: Printable_Triangle;
@end[Example]

@leading@;and now we can set the triangle components of it using a view
conversion thus
@begin[Example]
Triangle(Sign) := A_Triangle;
@end[Example]

@leading@keepnext@;And then as before we write
@begin[Example]
@tabset(P35)
Sign.Set_Hue(Red);
Sign.Set_Width(3);
Sign.Print_It;@\-- @examcom[print thick red triangle]
@end[Example]

@leading@;This second approach is probably better since it does not require
changing the geometry hierarchy. The downside is that we have to declare
the boring hue and width subprograms repeatedly. We can make this
much easier by declaring a generic package thus
@begin[Example]
@key[with] Line_Draw;  @key[use] Line_Draw;
@key[generic]
   @key[type] T @key[is abstract tagged private];
@key[package] Make_Printable @key[is]
   @key[type] Printable_T @key[is] @key[abstract new] T @key[and] Printable @key[with private];

   @key[procedure] Set_Hue(P: @key[in out] Printable_T; C: @key[in] Colour);
   @key[function] Hue(P: Printable_T) @key[return] Colour;
   @key[procedure] Set_Width(P: @key[in out] Printable_T; W: @key[in] Points);
   @key[function] Width(P: Printable_T) @key[return] Points;

@key[private]
   @key[type] Printable_T @key[is abstract new ]T @key[and] Printable @key[with]
      @key[record]
         Hue: Colour := Black;
         Width: Points := 1;
      @key[end record];
@key[end];
@end[Example]

This generic can be used to make any type printable. We simply write
@begin[Example]
@key[package] P_Triangle @key[is new] Make_Printable(Triangle);
@key[type] Printable_Triangle@key[ is new ]P_Triangle.Printable_T @key[with null record];
@key[function] To_Lines(T: Printable_Triangle) @key[return] Line_Set;
@end[Example]

The instantiation of the package creates a type @exam[Printable_T]
which has all the hue and width operations and the required additional
components. However, it simply inherits the abstract function @exam[To_Lines]
and so itself has to be an abstract type. Note that the function @exam[To_Lines]
has to be especially coded for each type anyway unlike the hue and
width operations which can be the same.

We now do a further derivation largely in order to give the type @exam[Printable_T]
the required name @exam[Printable_Triangle] and at this stage we provide
the concrete function @exam[To_Lines].

We can then proceed as before. Thus the generic makes the whole process
very easy @en any type can be made printable by just writing three
lines plus the body of the function @exam[To_Lines].

Hopefully this example has illustrated a number of important points
about the use of interfaces. The key thing perhaps is that we can
use the procedure @exam[Print] to print anything that implements the
interface @exam[Printable].

@leading@;Earlier we stated that it was a common convention to provide pairs
of operations to read and update properties such as @exam[Hue] and
@exam[Set_Hue] and @exam[Width] and @exam[Set_Width]. This is not
always appropriate. Thus if we have related components such as @exam[X_Coord]
and @exam[Y_Coord ]then although individual functions to read them
might be sensible, it is undoubtedly  better to update the two values
together with a single procedure such as the procedure @exam[Move]
declared earlier. Thus if we wish to move an object from the origin
(0.0, 0.0) to say (3.0, 4.0) and do it by two calls
@begin[Example]
@tabset(P42)
Obj.Set_X_Coord(3.0);@\-- @examcom[first change X]
Obj.Set_Y_Coord(4.0);@\-- @examcom[then change Y]
@end[Example]

then it seems as if it was transitorily at the point (3.0, 0.0). There
are various other risks as well. We might forget to set one component
or accidentally set the same component twice.

Finally, as discussed earlier, null procedures are a new kind of subprogram
and the user-defined operations of an interface must be null procedures
or abstract subprograms @en there is of course no such thing as a
null function.

(Nonlimited interfaces do have one concrete operation and that is
predefined equality; it could even be overridden with an abstract
one.)

Null procedures will be found useful for interfaces but are in fact
applicable to any types. As an example the package @exam[Ada.Finalization]
now uses null procedures for @exam[Initialize], @exam[Adjust], and
@exam[Finalize] as described in the Introduction.

@leading@;We conclude this section with a few further remarks on limitedness.
We noted earlier that an interface can be explicitly stated to be
limited so we might have@Defn{limited interface}@Defn2{Term=[interface],Sec=[limited]}
@begin[Example]
@tabset(P42)
@key[type] LI @key[is limited interface];@\-- @examcom[limited]
@key[type] NLI @key[is interface];@\-- @examcom[nonlimited]
@end[Example]

@leading@;An interface is limited only if it says limited (or synchronized etc).
As mentioned earlier, a descendant of a nonlimited interface must
be nonlimited since it must implement assignment and equality. So
if an interface is composed from a mixture of limited and nonlimited
interfaces it must be nonlimited
@begin[Example]
@tabset(P42)
@key[type] I @key[is interface and] LI @key[and] NLI;@\-- @examcom[legal]
@key[type] I @key[is limited interface and] LI @key[and] NLI;@\-- @examcom[illegal]
@end[Example]

@leading@;In other words, limitedness is never inherited from an interface but
has to be stated explicitly. This applies to both the composition
of interfaces and type derivation. On the other hand, in the case
of type derivation, limitedness is inherited from the parent provided
it is not an interface. This is necessary for compatibility with Ada
95. So given
@begin[Example]
@key[type] LT @key[is limited tagged] ...
@key[type] NLT@key[ is tagged ]...
@end[Example]

@leading@keepnext@;then

@begin[Example]
@tabset(P42)
@key[type] T@key[ is new] NLT @key[and] LI @key[with] ...@\--@examcom[legal, T not limited]
@key[type] T @key[is new] NLT @key[and] NLI @key[with] ...@\--@examcom[legal, T not limited]
@key[type] T@key[ is new] LT @key[and] LI @key[with] ...@\-- @examcom[legal, T limited]
@key[type] T @key[is new] LT @key[and] NLI @key[with] ...@\--@examcom[illegal]
@end[Example]

The last is illegal because @exam[T] is expected to be limited because
it is derived from the limited parent type @exam[LT] and yet it is
also a descendant of the nonlimited interface @exam[NLI].

In order to avoid certain curious difficulties, Ada 2005 permits @key[limited]
to be stated explicitly on type derivation. (It would have been nice
to insist on this always for clarity but such a change would have
been too much of an incompatibility.) If we do state @key[limited]
explicitly then the parent must be limited (whether it is a type or
an interface).

@leading@keepnext@;Using @key[limited] is necessary if we wish to derive a
limited type from a limited interface thus
@begin[Example]
@key[type] T @key[is limited new ]LI @key[with] ...
@end[Example]

These rules really all come down to the same thing. If a parent or
progenitor (indeed any ancestor) is nonlimited then the descendant
must be nonlimited. We can state that in reverse, if a type (including
an interface) is limited then all its ancestors must be limited.

@leading@;An earlier version of Ada 2005 ran into difficulties in this area
because in the case of a type derived just from interfaces, the behaviour
could depend upon the order of their appearance in the list (because
the rules for parent and progenitors are a bit different). But in
the final version of the language the order does not matter. So
@begin[Example]
@tabset(P42)
@key[type] T @key[is new] NLI @key[and] LI @key[with] ...@\--@examcom[legal, not limited]
@key[type] T @key[is new] LI @key[and] NLI @key[with] ...@\--@examcom[legal, not limited]
@end[Example]

@leading@keepnext@;But the following are of course illegal
@begin[Example]
@tabset(P42)
@key[type] T @key[is limited new] NLI @key[and] LI @key[with] ...@\-- @examcom[illegal]
@key[type] T @key[is limited new] LI @key[and] NLI @key[with] ...@\-- @examcom[illegal]
@end[Example]

There are also similar changes to generic formals and type extension
@en Ada 2005 permits @key[limited] to be given explicitly in both
cases.


@LabeledClause{Nested type extension}

In Ada 95 type extension of tagged types has to be at the same level
as the parent type. This can be quite a problem. In particular it
means that all controlled types must be declared at library level
because the root types @exam[Controlled] and @exam[Limited_Controlled]
are declared in the library level package @exam[Ada.Finalization].
The same applies to storage pools and streams because again the root
types @exam[Root_Storage_Pool ]and @exam[Root_Stream_Type] are declared
in library packages.@Defn{nested type extension}

This has a cumulative effect since if we write a generic unit using
any of these types then that package can itself only be instantiated
at library level. This enforces a very flat level of programming and
hinders abstraction.

The problems can actually be illustrated without having to use controlled
types or generics. As a simple example consider the following which
is adapted from a text book @LocalLink{Target=[R6],Sec=[References],Text={[6]}}. It manipulates lists of colours and
we assume that the type @exam[Colour] is declared somewhere.

@begin[Example]
@key[package] Lists @key[is]
   @key[type] List@key[ is limited private];
   @key[type] Iterator @key[is abstract tagged null record];
   @key[procedure] Iterate(IC: @key[in] Iterator'Class; L: @key[in] List);
   @key[procedure] Action(It: @key[in out] Iterator; C: @key[in out] Colour) @key[is abstract];
@key[private]
   ...
@key[end];
@end[Example]

The idea is that a call of @exam[Iterate] calls @exam[Action] (by
dispatching) on each object of the list and thereby gives access to
the colour of that object. The user has to declare an extension of
@exam[Iterator] and a specific procedure @exam[Action] to do whatever
is required on each object.

@leading@;Some readers may find this sort of topic confusing. It might be easier
to understand if we look at the private part and body of the package
@exam[Lists] which might be
@begin[Example]
@tabset(P42)
@key[private]
   @key[type] Cell @key[is]
      @key[record]
         Next: @key[access] Cell;@\-- @examcom[anonymous type]
         C: Colour;
      @key[end record];

   @key[type] List @key[is access] Cell;
@key[end];

@key[package body] Lists @key[is]
   @key[procedure] Iterate(IC: @key[in] Iterator'Class; L: @key[in] List) @key[is]
      This: @key[access] Cell := L;
   @key[begin]
      @key[while] This /= @key[null loop]
         Action(IC, This.C);@\-- @examcom[dispatching call]
@\-- @examcom[or IC.Action(This.C);]
         This := This.Next;
      @key[end loop];
   @key[end] Iterate;
@key[end] Lists;
@end[Example]

Note the use of the anonymous access types which avoid the need to
have an incomplete declaration of @exam[Cell] in the private part.

@leading@;Now suppose we wish to change the colour of every green object to
red. We write (in some library level package)
@begin[Example]
@tabset(P42)
@key[type] GTR_It @key[is new] Iterator@key[ with null record];

@key[procedure] Action(It:@key[ in out] GTR_It; C: @key[in out] Colour)@key[is]
@key[begin]
   @key[if] C = Green @key[then] C := Red; @key[end if];
@key[end] Action;

@key[procedure] Green_To_Red(L: @key[in] List) @key[is]
   It: GTR_It;
@key[begin]
   Iterate(It, L);@\-- @examcom[or It.Iterate(L);]
@key[end] Green_To_Red;
@end[Example]

This works but is not ideal. The type @exam[GTR_It] and the procedure
@exam[Action] should not be declared outside the procedure @exam[Green_To_Red]
since they are really only part of its internal workings. But we cannot
declare the type @exam[GTR_It] inside the procedure in Ada 95 because
that would be an extension at an inner level.

The extra facilities of the predefined library in Ada 2005 and especially
the introduction of containers which are naturally implemented as
generic units forced a reconsideration of the reasons for restricting
type extension in Ada 95. The danger of nested extension of course
is that values of objects could violate the accessibility rules and
outlive their type declaration. It was concluded that type extension
could be permitted at nested levels with the addition of just a few
checks to ensure that the accessibility rules were not violated.

@leading@keepnext@;So in Ada 2005 the procedure @exam[Green_To_Red] can be
written as
@begin[Example]
@tabset(P42)
@key[procedure] Green_To_Red(L: @key[in] List) @key[is]
   @key[type] GTR_It @key[is new] Iterator@key[ with null record];

   @key[procedure] Action(It: @key[in out] GTR_It; C: @key[in out] Colour) @key[is]
   @key[begin]
      @key[if] C = Green @key[then] C := Red; @key[end if];
   @key[end] Action;

   It: GTR_It;
@key[begin]
   Iterate(It, L);@\-- @examcom[or It.Iterate(L);]
@key[end] Green_To_Red;
@end[Example]

and all the workings are now wrapped up within the procedure as they
should be.

Note incidentally that we can use the notation @exam[It.Iterate(L);]
even though the type @exam[GTR_It] is not declared in a package in
this case. Remember that although we cannot add new dispatching operations
to a type unless it is declared in a package specification, nevertheless
we can always override existing ones such as @exam[Action].

This example is all quite harmless and nothing can go wrong despite
the fact that we have performed the extension at an inner level. This
is because the value @exam[It] does not outlive the execution of the
procedure@exam[ Action].

@leading@;But suppose we have a class wide object @exam[Global_It] as in the
following
@begin[Example]
@tabset(P42)
@key[with] Lists; @key[use] Lists;
@key[package body] P @key[is]

   @key[function] Dodgy @key[return] Iterator'Class @key[is]
      @key[type] Bad_It @key[is new] Iterator@key[ with null record];

      @key[procedure] Action(It: @key[in out] GTR_It; C: @key[in out] Colour) @key[is]
      @key[begin]
         ...
      @key[end] Action;

      It: Bad_It;
   @key[begin]
      @key[return] It;
   @key[end] Dodgy;

   Global_It: Iterator'Class := Dodgy;
@key[begin]
   Global_It.Action(Red_For_Danger);@\-- @examcom[dispatches]
@key[end] P;
@end[Example]

Now we are in deep trouble. We have returned a value of the local
type @exam[Bad_It], assigned it as the initial value to @exam[Global_It]
and then dispatched on it to the procedure @exam[Action]. But the
procedure @exam[Action] that will be called is the one inside @exam[Dodgy]
and this does not exist anymore since we have left the function @exam[Dodgy].
So this must not be allowed to happen.

So various accessibility checks are required. There is a check on
the return from a function with a class wide result that the value
being returned does not have the tag of a type at a deeper level than
that of the function itself. So in this example there is a check on
the return from the function @exam[Dodgy]; this fails and raises
@exam[Program_Error] so all is well.@Defn{accessibility checks}

There are similar checks on class wide allocators and when using
@exam[T'Class'Input] or @exam[T'Class'Output]. Some of these can be carried out
at compile time but others have to be checked at run time and they also raise
@exam[Program_Error] if they fail.

Moreover, in order to implement the checks associated with @exam[T'Class'Input]
and @exam[T'Class'Output] two additional functions are declared in
the package @exam[Ada.Tags]; these are
@begin[Example]
@key[function] Descendant_Tag(External: String; Ancestor: Tag) @key[return] Tag;

@key[function] Is_Descendant_At_Same_Level (Descendant, Ancestor: Tag) @key[return] Boolean;
@end[Example]

The use of these will be outlined in the next section.


@LabeledClause{Object factory functions}

The Ada 95 Rationale
(@URLLink{URL=[http://www.adaic.com/standards/95rat/RAThtml/rat95-p2-4.html#4],Text=[Section 4.4.1]})
@LocalLink{Target=[R7],Sec=[References],Text={[7]}} says "We also note
that object oriented programming requires thought especially if variant
programming is to be avoided. There is a general difficulty in finding out what
is coming which is particularly obvious with input@en@;output; it is easy to
write dispatching output operations but generally impossible for input." In
this context, variant programming means messing about with case statements and
so on.

@leading@;The point about input@en@;output is that it is easy to write a
heterogeneous file but not so easy to read it. In the simple case of a
text file we can just do a series of calls of @exam[Put] thus
@begin[Example]
Put ("John is ");  Put(21, 0);  Put(" years old.");
@end[Example]

But text input is not so easy unless we know the order of the items
in the file. If we don't know the order then we really have to read
the wretched thing a line at a time and then analyse the lines.

Ada 95 includes a mechanism for doing this relatively easily in the
case of tagged types and stream input@en@;output. Suppose we have a
class of tagged types rooted at @exam[Root] with various derived specific
types @exam[T1], @exam[T2] and so on. We can then output a sequence
of values @exam[X1], @exam[X2], @exam[X3] of a variety of these types
to a file identified by the stream access value @exam[S] by writing
@begin[Example]
Root'Class'Output(S, X1);
Root'Class'Output(S, X2);
Root'Class'Output(S, X3);
...
@end[Example]

The various calls first write the tag of the specific type and then
the value of the type. The tag corresponding to the type@exam[ T1]
is the string @exam[External_Tag(T1'Tag)]. Remember that @exam[External_Tag]
is a function in the predefined package @exam[Ada.Tags].

@leading@keepnext@;On input we can reverse the process by writing something
like
@begin[Example]
@tabset(P42)
@key[declare]
   X: Root'Class := Root'Class'Input(S);
@key[begin]
   Process(X);@\-- @examcom[now process the object in X]
@end[Example]

The call of @exam[Root'Class'Input] first reads the external tag and
then dispatches to the appropriate function @exam[Tn'Input] according
to the value of the tag. The function reads the value and this is
now assigned as the initial value to the class wide variable @exam[X].
We can then do whatever we want with @exam[X] by perhaps dispatching
to a procedure @exam[Process] which deals with it according to its
specific type.

This works in Ada 95 but it is all magic and done by smoke and mirrors
inside the implementation. The underlying techniques are unfortunately
not available to the user.

@leading@;This means that if we want to devise our own stream protocol or
maybe just process some values in circumstances where we cannot directly
use dispatching then we have to do it all ourselves with if statements
or case statements. Thus we might be given a tag value and separately
some information from which we can create the values of the particular
type. In Ada 95 we typically have to do something like
@begin[Example]
@tabset(P42)
The_Tag: Ada.Tags.Tag;
A_T1: T1;@\-- @examcom[series of objects of each]
A_T2: T2;@\-- @examcom[specific type]
A_T3: T3;
...
The_Tag := Get_Tag( ... );@\-- @examcom[get the tag value]
@key[if] The_Tag = T1'Tag @key[then]
   A_T1 := Get_T( ... );@\-- @examcom[get value of specific type]
   Process(A_T1);@\-- @examcom[process the object]
@key[elsif] The_Tag = T2'Tag @key[then]
   A_T2 := Get_T( ... );@\-- @examcom[get value of specific type]
   Process(A_T2);@\-- @examcom[process the object]
@key[elsif]
   ...
@key[end if];
@end[Example]

We assume that @exam[Get_T] is a primitive function of the class rooted
at @exam[Root]. There is therefore a function for each specific type
and the selection in the if statements is made at compile time by
the normal overload rules. Similarly @exam[Process] is also a primitive
subprogram of the class of types.

This is all very tedious and needs careful maintenance if we add further
types to the class.

@leading@;Ada 2005 overcomes this problem by providing a generic constructor
function. The objective of this is to create an object given the value of its
tag. Such functions are often called object factory functions for obvious
reasons (the word factory is derived from the Latin facere, to make). The
specification of the
function is@Defn{object constructor}@Defn{object factory}@Defn{factory}@Defn{constructor function}
@begin[Example]
@key[generic]
   @key[type] T (<>) @key[is abstract tagged limited private];
   @key[type] Parameters (<>) @key[is limited private];
   @key[with function] Constructor(Params: @key[not null access] Parameters)
@key[return] T @key[is abstract];
@key[function] Ada.Tags.Generic_Dispatching_Constructor
         (The_Tag: Tag; Params: @key[not null access] Parameters) @key[return] T'Class;
@key[pragma] Preelaborate(Generic_Dispatching_Constructor);
@key[pragma] Convention(Intrinsic, Generic_Dispatching_Constructor);
@end[Example]

This generic function works for both limited and nonlimited types.
Remember that a nonlimited type is allowed as an actual generic parameter
corresponding to a limited formal generic type. The generic function
@exam[Generic_Dispatching_Constructor] is preelaborable and has convention
@exam[Intrinsic].

Note carefully the formal function @exam[Constructor]. This is an
example of a new kind of formal generic parameter introduced in Ada
2005. The distinctive feature is the use of @key[is abstract] in its
specification. The interpretation is that the actual function must
be a dispatching operation of a tagged type uniquely identified by
the profile of the formal function. The actual operation can be concrete
or abstract. Remember that the overriding rules ensure that the specific
operation for any concrete type will always have a concrete body.
Note also that since the operation is abstract it can only be called
through dispatching.

In this example, it therefore has to be a dispatching operation of
the type @exam[T] since that is the only tagged type involved in the
profile of @exam[Constructor]. We say that @exam[T] is the controlling
type. In the general case, the controlling type does not itself have
to be a formal parameter of the generic unit but usually will be as
here. Moreover, note that although the operation has to be a dispatching
operation, it is not primitive and so if we derive from the type @exam[T],
it will not be inherited.

@leading@;Formal abstract subprograms can of course be procedures as well as
functions. It is important that there is exactly one controlling type
in the profile. Thus given that @exam[TT1] and @exam[TT2] are tagged
types then the following would both be illegal
@begin[Example]
@tabset{P56}
@key[with procedure] Do_This(X1: TT1; X2: TT2) @key[is abstract];@\-- @examcom[illegal]
@key[with function] Fn(X: Float) @key[return] Float @key[is abstract];@\-- @examcom[illegal]
@end[Example]

The procedure @exam[Do_This] is illegal because it has two controlling
types @exam[TT1] and @exam[TT2]. Remember that we can declare a subprogram
with parameters of more than one tagged type but it can only be a
dispatching operation of one tagged type. The function @exam[Fn] is
illegal because it doesn't have any controlling types at all (and
so could never be called in a dispatching call anyway).

The formal function @exam[Constructor] is legal because only @exam[T]
is tagged; the type @exam[Parameters] which also occurs in its profile
is not tagged.

And now to return to the dispatching constructor. The idea is that
we instantiate the generic function with a (root) tagged type @exam[T],
some type @exam[Parameters] and the dispatching function @exam[Constructor].
The type @exam[Parameters] provides a means whereby auxiliary information
can be passed to the function @exam[Constructor].

The generic function @exam[Generic_Dispatching_Constructor] takes
two parameters, one is the tag of the type of the object to be created
and the other is the auxiliary information to be passed to the dispatching
function @exam[Constructor].@Defn2{Term=[function],Sec=[Ada.Tags.Generic_Dispatching_Constructor]}@Defn{Generic_Dispatching_Constructor function}@Defn{Ada.Tags.Generic_Dispatching_Constructor function}@Defn{Generic_Dispatching_Constructor function}

@leading@;Note that the type @exam[Parameters] is used as an access parameter
in both the generic function and the formal function @exam[Constructor].
This is so that it can be matched by the profile of the attribute
@exam[Input] whose specification is
@begin[Example]
@key[function] T'Input(Stream: @key[access] Root_Stream_Type'Class) @key[return] T;
@end[Example]

Suppose we instantiate @exam[Generic_Dispatching_Constructor] to give
a function @exam[Make_T]. A call of @exam[Make_T] takes a tag value,
dispatches to the appropriate @exam[Constructor] which creates a value
of the specific tagged type corresponding to the tag and this is finally
returned as the value of the class wide type @exam[T'Class] as the
result of @exam[Make_T]. It's still magic but anyone can use the magic
and not just the magician implementing stream input@en@;output.

@leading@keepnext@;We can now do our abstract problem as follows
@begin[Example]
@tabset(P42)
@key[function] Make_T @key[is] @key[new] Generic_Dispatching_Constructor(Root, Params, Get_T);
...
@key[declare]
   Aux: @key[aliased] Params := ... ;
   A_T: Root'Class:= Make_T(Get_Tag( ... ), Aux'Access);
@key[begin]
   Process(A_T);@\-- @examcom[dispatch to process the object]
@key[end];
@end[Example]

We no longer have the tedious sequence of if statements and the calls
of @exam[Get_T] and @exam[Process] are dispatching calls.

@leading@;The previously magic function @exam[T'Class'Input] can now be
implemented in a very natural way by something like
@begin[Example]
@tabset(P56)
@key[function] Dispatching_Input @key[is]
   @key[new] Generic_Dispatching_Constructor(T, Root_Stream_Type'Class, T'Input);

@key[function] T_Class_Input(S: @key[access] Root_Stream_Type'Class) @key[return] T'Class @key[is]
   The_String: String := String'Input(S);@\-- @examcom[read tag as string from stream]
   The_Tag: Tag := Descendant_Tag(The_String, T'Tag);@\-- @examcom[convert to a tag]
@key[begin]
   -- @examcom[now dispatch to the appropriate function Input]
   @key[return] Dispatching_Input(The_Tag, S);
@key[end] T_Class_Input;

@key[for] T'Class'Input @key[use] T_Class_Input;
@end[Example]

@leading@keepnext@;The body could of course be written as one giant statement
@begin[Example]
@key[return] Dispatching_Input(Descendant_Tag(String'Input(S), T'Tag), S);
@end[Example]

but breaking it down hopefully clarifies what is happening.

Note the use of @exam[Descendant_Tag] rather than @exam[Internal_Tag].
@exam[Descendant_Tag] is one of a few new functions introduced into
the package @exam[Ada.Tags] in Ada 2005. Streams did not work very
well for nested tagged types in Ada 95 because of the possibility
of multiple elaboration of declarations (as a result of tasking and
recursion); this meant that two descendant types could have the same
external tag value and @exam[Internal_Tag] could not distinguish them.
This is not an important problem in Ada 95 as nested tagged types
are rarely used. In Ada 2005 the situation is potentially made worse
because of the possibility of nested type extension.@Defn{Descendant_Tag function}

The goal in Ada 2005 is simply to ensure that streams do work with
types declared at the same level and to prevent erroneous behaviour
otherwise. The goal is not to permit streams to work with the nested
extensions introduced in Ada 2005. Any attempt to do so will result
in @exam[Tag_Error] being raised.

Note that we cannot actually declare an attribute function such as
@exam[T'Class'Input] by directly using the attribute name. We have
to use some other identifier such as @exam[T_Class_Input] and then
use an attribute definition clause as shown above.

@leading@keepnext@;Observe that @exam[T'Class'Output] can be implemented as
@begin[Example]
@key[procedure] T_Class_Output(S: @key[access] Root_Stream_Type'Class; X: @key[in] T'Class) @key[is]
@key[begin]
   @key[if not] Is_Descendant_At_Same_Level (X'Tag, T'Tag) @key[then]
      @key[raise] Tag_Error;
   @key[end if];
   String'Output(S, External_Tag(X'Tag));
   T'Output(S, X);
@key[end] T_Class_Output;

@key[for] T'Class'Output @key[use] T_Class_Output;
@end[Example]

Remember that streams are designed to work only with types declared
at the same accessibility level as the parent type @exam[T]. The call
of @exam[Is_Descendant_At_Same_Level], which is another new function
in Ada 2005, ensures this.

We can use the generic constructor to create our own stream protocol. We could
in fact replace @exam[T'Class'Input] and @exam[T'Class'Output] or just create
our own distinct subsystem. One reason why we might want to use a different
protocol is when the external protocol is already given such as in the case of
XML.

@leading@;Note that it will sometimes be the case that there is no need to
pass any auxiliary parameters to the constructor function in which
case we can declare
@begin[Example]
@key[type] Params @key[is null record];
Aux: @key[aliased] Params := (@key[null record]);
@end[Example]

@leading@;Another example can be based on part
of the program Magic Moments in @LocalLink{Target=[R6],Sec=[References],Text={[6]}}. This reads in the values necessary
to create various geometrical objects such as a @exam[Circle], @exam[Triangle], or
@exam[Square] which are derived from an abstract type @exam[Object].
The values are preceded by a letter C, T or S as appropriate. The
essence of the code is
@begin[Example]
Get(Code_Letter);
@key[case] Code_Letter is
   @key[when] 'C' => Object_Ptr := Get_Circle;
   @key[when] 'T' => Object_Ptr := Get_Triangle;
   @key[when] 'S' => Object_Ptr := Get_Square;
   ...
@key[end case];
@end[Example]

The types @exam[Circle], @exam[Triangle], and @exam[Square]
are derived from the root type @exam[Object] and @exam[Object_Ptr] is of the
type @key[access] @exam[Object'Class]. The function @exam[Get_Circle]
reads the value of the radius from the keyboard, the function
@exam[Get_Triangle] reads the values of the lengths of the three sides from
the keyboard and so on.

The first thing to do is to change the various constructor functions
such as @exam[Get_Circle] into various
specific overridings of a primitive operation @exam[Get_Object]
so that we can dispatch on it.

@leading@;Rather than just read the code letter
we could make the user type the external tag string and then we might
have
@begin[Example]
@key[function] Make_Object @key[is]
   @key[new] Generic_Dispatching_Constructor(Object, Params, Get_Object);

...
S: String := Get_String;
...
Object_Ptr := @key[new] Object'(Make_Object(Internal_Tag(S), Aux'Access));
@end[Example]

@leading@;but this is very tedious because
the user now has to type the external tag which will be an implementation
defined mess of characters. Observe that the string produced by a
call of @exam[Expanded_Name] such as "@exam[OBJECTS.CIRCLE]"
cannot be used because it will not
in general be unique and so there is no reverse function. (It is not
generally unique because of tasking and recursion.) But @exam[Expanded_Name]
is useful for debugging purposes.

In these circumstances the best way to proceed is to invent some sort
of registration system to make a map to convert the simple code letters
into the tag. We might have a package
@begin[Example]
@key[with] Ada.Tags; @key[use] Ada.Tags;
@key[package] Tag_Registration @key[is]
   @key[procedure] Register(The_Tag: Tag; Code: Character);
   @key[function] Decode(Code: Character) @key[return] Tag;
@key[end];
@end[Example]

@leading@keepnext@;and then we can write
@begin[Example]
Register(Circle'Tag, 'C');
Register(Triangle'Tag, 'T');
Register(Square'Tag, 'S');
@end[Example]

@leading@;And now the program to read the code
and then make the object becomes simply
@begin[Example]
Get(Code_Letter);
Object_Ptr := @key[new] Object'(Make_Object(Decode(Code_Letter), Aux'Access));
@end[Example]

and there are no case statements to maintain.

@leading@;The really important point about this example is that if we decide at
a later date to add more types such as @exam['P'] for @exam[Pentagon] and
@exam['H'] for @exam[Hexagon] then all we have to do is register the new code
letters thus
@begin[Example]
Register(Pentagon'Tag, 'P');
Register(Hexagon'Tag, 'H');
@end[Example]

and nothing else needs changing.
This registration can conveniently be done when the types are declared.

@leading@;The package @exam[Tag_Registration]
could be implemented trivially as follows by
@begin[Example]
@key[package body] Tag_Registration @key[is]
   Table: @key[array] (Character @key[range] 'A' .. 'Z') @key[of] Tag := (@key[others] => No_Tag);
   @key[procedure] Register(The_Tag: Tag; Code: Character) @key[is]
   @key[begin]
      Table(Code) := The_Tag;
   @key[end] Register;

   @key[function] Decode(Code: Character) @key[return] Tag @key[is]
   @key[begin]
      @key[return] Table(Code);
   @key[end] Decode;
@key[end] Tag_Registration;
@end[Example]

The constant @exam[No_Tag] is a value of the type @exam[Tag] which does not
represent an actual tag. If we forget to register a type then @exam[No_Tag]
will be returned by @exam[Decode] and this will cause @exam[Make_Object] to
raise @exam[Tag_Error].

A more elegant registration system could be easily implemented using
the container library which is described in Chapter @RefSecNum{Containers}.

Note that any instance of @exam[Generic_Dispatching_Constructor]
checks that the tag passed as parameter is indeed that of a type descended
from the root type @exam[T] and raises @exam[Tag_Error] if it is not.

In simple cases we could in fact perform that check for ourselves
by writing something like
@begin[Example]
   Trial_Tag: Tag := The_Tag;
@key[loop]
   @key[if] Trial_Tag = T'Tag @key[then exit]; @key[end if];
   Trial_Tag := Parent_Tag(Trial_Tag);
   @key[if] Trial_Tag = No_Tag @key[then raise] Tag_Error; @key[end if];
@key[end loop];
@end[Example]

@leading@;The function @exam[Parent_Tag] and the constant @exam[No_Tag] are
further items in the package @exam[Ada.Tags] whose specification in
Ada 2005 is
@begin[Example]
@tabset{P28}@key[package] Ada.Tags @key[is]@Defn{Tags package}@Defn{Ada.Tags package}@Defn2{Term=[package],Sec=[Ada.Tags]}
   @key[pragma] Preelaborate(Tags);

   @key[type] Tag @key[is private];
   @key[pragma] Preelaborable_Initialization(Tag);
   No_Tag: @key[constant] Tag;

   @key[function] Expanded_Name(T: Tag) @key[return] String;
   ...@\-- @examcom[also Wide and Wide_Wide versions]
   @key[function] External_Tag(T: Tag) @key[return] String;
   @key[function] Internal_Tag(External: String) @key[return] Tag;
   @key[function] Descendant_Tag(External: String; Ancestor: Tag)
@key[return] Tag;
   @key[function] Is_Descendant_At_Same_Level(Descendant, Ancestor: Tag) @key[return] Boolean;
   @key[function] Parent_Tag(T: Tag) @key[return] Tag;

   @key[type] Tag_Array @key[is] (Positive @key[range] <>) @key[of] Tag;
   @key[function] Interface_Ancestor_Tags(T: Tag) @key[return] Tag_Array;

   Tag_Error: @key[exception];
@key[private]
   ...
@key[end] Ada.Tags;
@end[Example]

The function @exam[Parent_Tag] returns @exam[No_Tag] if the parameter
@exam[T] of type @exam[Tag] has no parent which will be the case if
it is the ultimate root type of the class. As mentioned earlier, two
other new functions @exam[Descendant_Tag ]and @exam[Is_Descendant_At_Same_Level]
are necessary to prevent the misuse of streams with types not all
declared at the same level.

There is also a function @exam[Interface_Ancestor_Tags] which returns
the tags of all those interfaces which are ancestors of @exam[T] as
an array. This includes the parent if it is an interface, any progenitors
and all their ancestors which are interfaces as well @en but it excludes
the type @exam[T] itself.

Finally note that the introduction of 16- and 32-bit characters in
identifiers means that functions also have to be provided to return
the images of identifiers as a @exam[Wide_String] or @exam[Wide_Wide_String].
So we have functions @exam[Wide_Expanded_Name] and @exam[Wide_Wide_Expanded_Name]
as well as@exam[ Expanded_Name]. The lower bound of the strings returned
by these functions and by@exam[ External_Tag] is @exam[1] @en Ada
95 forgot to state this for @exam[External_Tag] and @exam[Expanded_Name]!


@LabeledClause{Overriding and overloading}

One of the key goals in the design of Ada was to encourage the writing
of correct programs. It was intended that the structure, strong typing,
and so on should ensure that many errors which are not detected by
most languages until run time should be caught at compile time in
Ada. Unfortunately the introduction of type extension and overriding
in Ada 95 produced a situation where careless errors in subprogram
profiles lead to errors which are awkward to detect.

@leading@;The Introduction described two typical examples. The first concerns
the procedure @exam[Finalize]. Consider
@begin[Example]
@key[with] Ada.Finalization;  @key[use] Ada.Finalization;
@key[package] Root @key[is]
   @key[type] T @key[is new] Controlled @key[with] ... ;
   @key[procedure] Op(Obj: @key[in out] T; Data: @key[in] Integer);
   @key[procedure] Finalise(Obj: @key[in out] T);
@key[end] Root;
@end[Example]

We have inadvertently written @exam[Finalise] rather than @exam[Finalize].
This means that @exam[Finalize] does not get overridden as expected
and so the expected behaviour does not occur on finalization of objects
of type @exam[T].

@leading@;In Ada 2005 we can prefix the declaration with @key[overriding]@Defn{overriding indicator}
@begin[Example]
   @key[overriding]
   @key[procedure] Finalize(Obj: @key[in out] T);
@end[Example]

And now if we inadvertently write @exam[Finalise] then this will be
detected during compilation.

@leading@keepnext@;Similar errors can occur in a profile. If we write
@begin[Example]
@tabset{P42}
@key[package] Root.Leaf @key[is]
   @key[type] NT @key[is new] T @key[with null record];
   @key[overriding]@\-- @examcom[overriding indicator]
   @key[procedure] Op(Obj: @key[in out] NT; Data: @key[in] String);
@key[end] Root.Leaf;
@end[Example]

then the compiler will detect that the new procedure @exam[Op] has
a parameter of type @exam[String] rather than @exam[Integer].

@leading@keepnext@;However if we do want a new operation then we can write
@begin[Example]
   @key[not overriding]
   @key[procedure] Op(Obj: @key[in out] NT; Data: @key[in] String);
@end[Example]

@leading@;The overriding indicators can also be used with abstract subprograms,
null procedures, renamings, instantiations, stubs, bodies and entries
(we will deal with entries in the chapter on tasking @en
@RefSecNum{Synchronized interfaces}). So we can have
@begin[Example]
@key[overriding]
@key[procedure] Pap(X: TT) @key[is abstract];

@key[overriding]
@key[procedure] Pep(X: TT) @key[is null];

@key[overriding]
@key[procedure] Pip(Y: TT) @key[renames] Pop;

@key[not overriding]
@key[procedure] Poop @key[is new] Peep( ... );

@key[overriding]
@key[procedure] Pup(Z: TT)@key[ is separate];

@key[overriding]
@key[procedure] Pup(X: TT) @key[is]
@key[begin] ... @key[end] Pup;
@end[Example]

@leading@;We do not need to apply an overriding indicator to both a procedure
specification and body but if we do then they naturally must not conflict.
It is expected that overriding indicators will typically only be given
on specifications but they would be appropriate in the case of a body
standing alone as in the example of @exam[Action] in the previous
section. So we might have
@begin[Example]
@key[procedure] Green_To_Red(L: @key[in] List) @key[is]
   @key[type] GTR_It @key[is new] Iterator@key[ with null record];

   @key[overriding]
   @key[procedure] Action(It: @key[in out] GTR_It; C: @key[in out] Colour) @key[is]
   @key[begin]
      @key[if] C = Green @key[then] C := Red; @key[end] @key[if];
   @key[end] Action;
...
@end[Example]

The overriding indicators are optional for two reasons. One is simply
for compatibility with Ada 95. The other concerns awkward problems
with private types and generics.

@leading@keepnext@;Consider
@begin[Example]
@key[package] P @key[is]
   @key[type] NT@key[ is new] T @key[with private];
   @key[procedure] Op(X: NT);
@key[private]
@end[Example]

@leading@;Now suppose the type @exam[T] does not have an operation @exam[Op].
Then clearly it would be wrong to write
@begin[Example]
@tabset{P42}
@key[package] P @key[is]
   @key[type] NT@key[ is new] T @key[with private];  @\-- @examcom[T has no Op]
   @key[overriding@\--]@examcom[ illegal]
   @key[procedure] Op(X: NT);
@key[private]
@end[Example]

because that would violate the information known in the partial view.

@leading@;But suppose that in fact it turns out that in the private part the
type @exam[NT] is actually derived from @exam[TT] (itself derived
from @exam[T]) and that @exam[TT] does have an operation @exam[Op].
@begin[Example]
@tabset{P42}
@key[private]
   @key[type] NT @key[is new] TT @key[with] ...@\-- @examcom[TT has Op]
@key[end] P;
@end[Example]

In such a case it turns out in the end that @exam[Op] is in fact overriding
after all. We can then put an overriding indicator on the body of
@exam[Op] since at that point we do know that it is overriding.

Equally of course we should not specify @key[not overriding] for @exam[Op]
in the visible part because that might not be true either (since it
might be that @exam[TT] does have @exam[Op]). However if we did put
@key[not overriding] on the partial view then that would not in itself
be an error but would simply constrain the full view not to be overriding
and thus ensure that @exam[TT] does not have @exam[Op].

Of course if @exam[T] itself has @exam[Op] then we could and indeed
should put an overriding indicator in the visible part since we know
that to be the truth at that point.

The general rule is not to lie. But the rules are slightly different
for @key[overriding] and @key[not overriding]. For @key[overriding]
it must not lie at the point concerned. For @key[not overriding] it
must not lie anywhere.

This asymmetry is a bit like presuming the prisoner is innocent until
proved guilty. We sometimes start with a view in which an operation
appears not to be overriding and then later on we find that it is
overriding after all. But the reverse never happens @en we never start
with a view in which it is overriding and then later discover that
it was not. So the asymmetry is real and justified.

There are other similar but more complex problems with private types
concerning implicit declarations where the implicit declaration turns
up much later and is overriding but has no physical presence on which
to hang the indicator. It was concluded that by far the best approach
to these problems was just to say that the overriding indicator is
always optional. We cannot expect to find all the bugs in a program
through syntax and static semantics; the key goal here is to provide
a simple way of finding most of them.

@leading@;Similar problems arise with generics. As is usual with generics the
rules are checked in the generic itself and then rechecked upon instantiation
(in this case for uses within both the visible part and private part
of the specification). Consider
@begin[Example]
@tabset{P42}
@key[generic]
   @key[type] GT @key[is tagged private];
@key[package] GP @key[is]
   @key[type] NT @key[is new] GT @key[with private];
   @key[overriding]@\-- @examcom[illegal, GT has no Op]
   @key[procedure] Op(X: NT);
@key[private]
@end[Example]

This has to be illegal because @exam[GT] has no operation @exam[Op].
Of course the actual type at instantiation might have @exam[Op] but
the check has to pass both in the generic and in the instantiation.

@leading@;On the other hand saying @key[not overriding] is allowed
@begin[Example]
@tabset{P42}
@key[generic]
   @key[type] GT @key[is tagged private];
@key[package] GP @key[is]
   @key[type] NT @key[is new] GT @key[with private];
   @key[not] @key[overriding]@\-- @examcom[legal, GT has no Op]
   @key[procedure] Op(X: NT);
@key[private]
@end[Example]

However, in this case we cannot instantiate @exam[GP] with a type
that does have an operation @exam[Op] because it would fail when checked
on the instantiation. So in a sense this imposes a further contract
on the generic. If we do not want to impose this restriction then
we must not give an overriding indicator on the procedure @exam[Op]
for @exam[NT].

@leading@;Another situation arises when the generic formal is derived
@begin[Example]
@tabset{P42}
@key[generic]
   @key[type] GT @key[is new] T @key[with private];
@key[package] GP @key[is]
   @key[type] NT @key[is new] GT @key[with private];
   @key[overriding]@\-- @examcom[legal if T has Op]
   @key[procedure] Op(X: NT);
@key[private]
@end[Example]

In this case it might be that the type @exam[T] does have an operation
@exam[Op] in which case we can give the overriding indicator.

@leading@keepnext@;We might also try
@begin[Example]
@tabset{P42}
@key[generic]
   @key[type] GT @key[is tagged private];
   @key[with procedure] Op(X: GT);
@key[package] GP @key[is]
   @key[type] NT @key[is new] GT @key[with private];
   @key[overriding]@\-- @examcom[illegal, Op not primitive]
   @key[procedure] Op(X: NT);
@key[private]
@end[Example]

But this is incorrect because although @exam[GT] has to have an operation
corresponding to @exam[Op] as specified in the formal parameter list,
nevertheless it does not have to be a primitive operation nor does
it have to be called @exam[Op] and thus it isn't inherited.

@leading@;It should also be observed that overriding indicators can be used
with untagged types although they have been introduced primarily to
avoid problems with dispatching operations. Consider
@begin[Example]
@tabset{P42}
@key[package] P is
   @key[type] T @key[is private];
   @key[function] "+" (Left, Right: T) @key[return] T;
@key[private]
   @key[type] T @key[is range] 0 .. 100; @\-- @examcom["+" overrides]
@key[end] P;
@end[Example]

@leading@keepnext@;as opposed to
@begin[Example]
@tabset{P42}
@key[package] P @key[is]
   @key[type] T@key[ is private];
   @key[function] "+" (Left, Right: T) @key[return] T;
@key[private]
   @key[type] T @key[is] (Red, White, Blue);@\-- @examcom["+" does not override]
@key[end] P;
@end[Example]

The point is that the partial view does not reveal whether overriding
occurs or not @en nor should it since either implementation ought
to be acceptable. We should therefore remain silent regarding overriding
in the partial view. This is similar to the private extension and
generic cases discussed earlier. Inserting @key[overriding] would
be illegal on both examples, while @key[not overriding] would be allowed
only on the second one (which would constrain the implementation as
in the previous examples). Again, it is permissible to put an overriding
indicator on the body of @exam["+"] to indicate whether or not it
does override.

It is also possible for a subprogram to be primitive for more than
one type (this cannot happen for more than one tagged type but it
can happen for untagged types or one tagged type and some untagged
types). It could then be overriding for some types and not overriding
for others. In such a case it is considered to be overriding as a
whole and any indicator should reflect this.

The possibility of having a pragma which would enforce the use of
overriding indicators (so that they too could not be inadvertently
omitted) was eventually abandoned largely because of the private type
and generic problem which made the topic very complicated.

Note the recommended layout, an overriding indicator should be placed
on the line before the subprogram specification and aligned with it.
This avoids disturbing the layout of the specification.

It is hoped that programmers will use overriding indicators freely.
As mentioned in the Introduction, they are very valuable for preventing
nasty errors during maintenance. Thus if we add a further parameter
to an operation such as Op for a root type and all type extensions
have overriding indicators then the compiler will report an error
if we do not modify the operators of all the derived types correctly.

We now turn to a minor change in the overriding rules for functions
with controlling results.

The reader may recall the general rule in Ada 95 that a function that
is a primitive operation of a tagged type and returns a value of the
type, always requires overriding when the type is extended. This is
because the function for the extended type must create values for
the additional components. (This rule is sometimes incorrectly phrased
as saying that the function "goes abstract" if the extended type is concrete;
this is incorrect as the rules for abstract functions and functions that
"require overriding" are quite different.) The irritating thing about the rule
in Ada 95 is that it applies even if there are no additional
components.@Defn{goes abstract}@Defn{requires overriding}

@leading@keepnext@;Thus consider a generic version of the set package of
Section @RefSecNum{The prefixed notation}
@begin[Example]
@key[generic]
   @key[type] Element @key[is private];
@key[package] Sets @key[is]
   @key[type] Set @key[is tagged private];
   @key[function] Empty @key[return] Set;
   @key[function] Unit(E: Element) @key[return] Set;
   @key[function] Union(S, T: Set) @key[return] Set;
   @key[function] Intersection(S, T: Set) @key[return] Set;
   ...
@key[end] Sets;
@end[Example]

@leading@keepnext@;Now suppose we declare an instantiation thus
@begin[Example]
@key[package] My_Sets @key[is new] Sets(My_Type);
@end[Example]

@leading@;This results in the type @exam[Set] and all its operations being
declared inside the package @exam[My_Sets]. However, for various reasons we
might wish to have the type and its operations at the current scope.
One reason could just be for simplicity of naming so that we do not
have to write @exam[My_Sets.Set] and @exam[My_Sets.Union] and so on.
(We might be in a regime where use clauses are forbidden.) An obvious
approach is to derive our own type locally so that we have
@begin[Example]
@key[package] My_Sets @key[is new] Sets(My_Type);
@key[type] My_Set @key[is new] My_Sets.Set @key[with null record];
@end[Example]

@leading@;Another situation where we might need to do this is where we wish
to use the type @exam[Set] as the full type for a private type thus
@begin[Example]
   @key[type] My_Set @key[is private];
@key[private]
   @key[package] My_Sets @key[is new] Sets(My_Type);
   @key[type] My_Set @key[is new] My_Sets.Set @key[with null record];
@end[Example]

@leading@;But this doesn't work nicely in Ada 95 since all the functions have
controlling results and so "go abstract" and therefore have to be
overridden with wrappers thus
@begin[Example]
@key[function] Union(S, T: My_Set) @key[return] My_Set @key[is]
@key[begin]
   @key[return] My_Set(My_Sets.Union(My_Sets.Set(S), My_Sets.Set(T)));
@key[end] Union;
@end[Example]

This is clearly a dreadful nuisance. Ada 2005 sensibly allows the
functions to be inherited provided that the extension is visibly null
(and that there is no new discriminant part) and so no overriding
is required. This new facility will be much appreciated by users of
the new container library in Ada 2005 which has just this style of
generic packages which export tagged types.

@leading@;The final topic to be discussed concerns a problem with overloading
and untagged types. Remember that the concept of abstract subprograms
was introduced into Ada 95 largely for the purpose of tagged types.
However it can also be used with untagged types on derivation if we
do not want an operation to be inherited. This often happens with
types representing physical measurements. Consider
@begin[Example]
@key[type] Length@key[ is new] Float;
@key[type] Area @key[is new] Float;
@end[Example]

@leading@;These types inherit various undesirable operations such as
multiplying a length by a length to give a length when of course we want an
area. We can overcome this by overriding them with abstract operations. Thus
@begin[Example]
@key[function] "*" (L, R: Length) @key[return] Length @key[is abstract];
@key[function] "*" (L, R: Area) @key[return] Area @key[is abstract];
@key[function] "*" (L, R: Length) @key[return] Area;
@end[Example]

We have also declared a function to multiply two lengths to give an
area. So now we have two functions multiplying two lengths, one returns
a length but is abstract and so can never be called and the other
correctly returns an area.

@leading@;Now suppose we want to print out some values of these types. We
might declare a couple of functions delivering a string image thus
@begin[Example]
@key[function] Image(L: Length) @key[return] String;
@key[function] Image(L: Area) @key[return] String;
@end[Example]

@leading@keepnext@;And then we decide to write
@begin[Example]
@tabset(P42)
X: Length := 2.5;
...
Put_Line(Image(X * X));@\-- @examcom[ambiguous in Ada 95]
@end[Example]

This fails to compile in Ada 95 since it is ambiguous because both
@exam[Image] and @exam["*"] are overloaded. The problem is that although
the function @exam["*"] returning a length is abstract it nevertheless
is still there and is considered for overload resolution. So we don't
know whether we are calling @exam[Image] on a length or on an area
because we don't know which @exam["*"] is involved.@Defn2{Term=[overload resolution],Sec=[abstract subprograms ignored]}

So declaring the operation as abstract does not really get rid of
the operation at all, it just prevents it from being called but its
ghost lives on and is a nuisance.

In Ada 2005 this is overcome by a new rule that says "abstract nondispatching
subprograms are ignored during overload resolution". So the abstract
@exam["*"] is ignored and there is no ambiguity in Ada 2005.

Note that this rule does not apply to dispatching operations of tagged
types since we might want to dispatch to a concrete operation of a
descendant type. But it does apply to operations of a class-wide type.

