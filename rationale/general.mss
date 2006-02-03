@Part(xxx, Root="rat.msm")

@comment($Source: e:\\cvsroot/ARM/Rationale/general.mss,v $)
@comment($Revision: 1.3 $ $Date: 2006/02/03 07:39:46 $)

@LabeledSection{Exceptions, generics etc}

@Subheading{Abstract}

@i{This paper
describes various improvements in a number of general areas in Ada
2005.}

@i{There are some minor almost cosmetic improvements in the exceptions
area which add to convenience rather than functionality. There are
some important changes in the numerics area: one concerns mixing signed
and unsigned integers and another concerns fixed point multiplication
and division.}

@i{There are also a number of additional pragmas and Restrictions identifiers
mostly of a safety-related nature.}

@i{Finally there are a number of improvements in the generics area such
as better control of partial parameters of formal packages. }

@i{This is one of a number of papers concerning Ada 2005 which are being
published in the Ada User Journal. An earlier version of this paper
appeared in the Ada User Journal, Vol. 26, Number 3, September 2005.
Other papers in this series will be found in later issues of the Journal
or elsewhere on this website.}

@LabeledClause{Ada Issues: Exceptions, generics, etc}

The areas mentioned in this paper are not specifically mentioned in
the WG9 guidance document @LocalLink{Target=[R1],Sec=[References],Text={[1]}}
other than under the request to remedy shortcomings and improve interfacing.

@leading@;The following Ada Issues cover the relevant changes and are described
in detail in this paper.

@begin[Description]
@AILink{AI=[AI95-00161-01],Text=[161]}@\Preelaborable initialization

@AILink{AI=[AI95-00216-01],Text=[216]}@\Unchecked unions @en variants without
discriminant

@AILink{AI=[AI95-00224-01],Text=[224]}@\pragma Unsuppress

@AILink{AI=[AI95-00241-01],Text=[241]}@\Testing for null occurrence

@AILink{AI=[AI95-00251-01],Text=[251]}@\Abstract interfaces to provide multiple
inheritance

@AILink{AI=[AI95-00257-01],Text=[257]}@\Restrictions for implementation defined
entities

@AILink{AI=[AI95-00260-02],Text=[260]}@\Abstract formal subprograms &
dispatching constructors

@AILink{AI=[AI95-00267-01],Text=[267]}@\Fast float to integer conversion

@AILink{AI=[AI95-00286-01],Text=[286]}@\Assert pragma

@AILink{AI=[AI95-00317-01],Text=[317]}@\Partial parameter lists for formal
packages

@AILink{AI=[AI95-00329-01],Text=[329]}@\pragma No_Return @en procedures that
never return

@AILink{AI=[AI95-00340-01],Text=[340]}@\Mod attribute

@AILink{AI=[AI95-00361-01],Text=[361]}@\Raise with message

@AILink{AI=[AI95-00364-01],Text=[364]}@\Fixed point multiply and divide

@AILink{AI=[AI95-00368-01],Text=[368]}@\Restrictions for obsolescent features

@AILink{AI=[AI95-00381-01],Text=[381]}@\New Restrictions identifier @en
No_Dependence

@AILink{AI=[AI95-00394-01],Text=[394]}@\Redundant Restrictions identifiers and
Ravenscar

@AILink{AI=[AI95-00398-01],Text=[398]}@\Parameters of formal packages given at
most once

@AILink{AI=[AI95-00400-01],Text=[400]}@\Wide and wide-wide images

@AILink{AI=[AI95-00414-01],Text=[414]}@\pragma No_Return for overriding
procedures

@AILink{AI=[AI95-00417-01],Text=[417]}@\Lower bound of functions in
@exam[Ada.Exceptions] etc

@AILink{AI=[AI95-00419-01],Text=[419]}@\Limitedness of derived types

@AILink{AI=[AI95-00420-01],Text=[420]}@\Resolution of universal operations in
@exam[Standard]

@AILink{AI=[AI95-00423-01],Text=[423]}@\Renaming, null exclusion and formal
objects

@end[Description]

These changes can be grouped as follows.

First there are some minor changes to exception handling. There are
neater means for testing for null occurrence and raising an exception with a
message (@AILink{AI=[AI95-00241-01],Text=[241]},
@AILink{AI=[AI95-00361-01],Text=[361]}) and also wide and wide-wide
versions of some procedures (@AILink{AI=[AI95-00400-01],Text=[400]},
@AILink{AI=[AI95-00417-01],Text=[417]}).

The numerics area has a number of small but important changes. They are the
introduction of an attribute @exam[Mod] to aid conversion between signed and
unsigned integers (@AILink{AI=[AI95-00340-01],Text=[340]}); changes to the
rules for fixed point multiplication and division which permit user-defined
operations (@AILink{AI=[AI95-00364-01],Text=[364]},
@AILink{AI=[AI95-00420-01],Text=[420]}); and an attribute
@exam[Machine_Rounding] which can be used to aid fast conversions from floating
to integer types (@AILink{AI=[AI95-00267-01],Text=[267]}).

A number of new pragmas and @exam[Restrictions] identifiers have been added.
These generally make for more reliable programming. The pragmas are:
@exam[Assert], @exam[No_Return], @exam[Preelaborable_Initialization],
@exam[Unchecked_Union], and @exam[Unsuppress]
(@AILink{AI=[AI95-00161-01],Text=[161]},
@AILink{AI=[AI95-00216-01],Text=[216]},
@AILink{AI=[AI95-00224-01],Text=[224]},
@AILink{AI=[AI95-00286-01],Text=[286]},
@AILink{AI=[AI95-00329-01],Text=[329]},
@AILink{AI=[AI95-00414-01],Text=[414]}). The restrictions identifiers are
@exam[No_Dependence], @exam[No_Implementation_Pragmas],
@exam[No_Implementation_Restrictions], and @exam[No_Obsolescent_Features]
(@AILink{AI=[AI95-00257-01],Text=[257]},
@AILink{AI=[AI95-00368-01],Text=[368]},
@AILink{AI=[AI95-00381-01],Text=[381]}). Note that there are also other new
pragmas and new restrictions identifiers concerned with tasking as described in
the previous paper (see @RefSecNum{The Ravenscar profile}). However, the
introduction of @exam[No_Dependence] means that the identifiers
@exam[No_Asynchronous_Control], @exam[No_Unchecked_Conversion] and
@exam[No_Unchecked_Deallocation] are now obsolescent
(@AILink{AI=[AI95-00394-01],Text=[394]}).

Finally there are changes in generic units. There are changes in generic
parameters which are consequences of changes in other areas such as the
introduction of interfaces and dispatching constructors as described in the
paper on the object oriented model (parts of
@AILink{AI=[AI95-00251-01],Text=[251]} and
@AILink{AI=[AI95-00260-02],Text=[260]}); there are also changes to formal
access and derived types (@AILink{AI=[AI95-00419-01],Text=[419]},
@AILink{AI=[AI95-00423-01],Text=[423]}). Also, it is now possible to give
just some parameters of a formal package in the generic formal part
(@AILink{AI=[AI95-00317-01],Text=[317]},
@AILink{AI=[AI95-00398-01],Text=[398]}).


@LabeledClause{Exceptions}


There are two minor improvements in this area.

One concerns the detection of a null exception occurrence which might
be useful in a routine for analysing a log of exceptions. This is
tricky because although a constant @exam[Null_Occurrence] is declared
in the package @exam[Ada.Exceptions], the type @exam[Exception_Occurrence]
is limited and no equality is provided. So the obvious test cannot
be performed.

@leading@;We can however apply the function @exam[Exception_Identity] to a
value of the type @exam[Exception_Occurrence] and this returns the
corresponding @exam[Exception_Id]. Thus we could check to see whether a
particular occurrence @exam[X] was caused by @exam[Program_Error] by writing
@begin[Example]
@key[if] Exception_Identity(X) = Program_Error'Identity @key[then]
@end[Example]

However, in Ada 95, applying @exam[Exception_Identity] to the value
@exam[Null_Occurrence] raises @exam[Constraint_Error] so we have to
resort to a revolting trick such as declaring a function as follows

@begin[Example]
@key[function] Is_Null_Occurrence(X: Exception_Occurrence) @key[return] Boolean @key[is]
   Id: Exception_Id;
@key[begin]
   Id := Exception_Identity(X);
   @key[return] False;
@key[exception]
   @key[when] Constraint_Error => @key[return] True;
@key[end] Is_Null_Occurrence;
@end[Example]

@leading@;We can now write some general analysis routine as
@begin[Example]
@tabset[P42]
@key[procedure] Process_Ex(X: @key[in] Exception_Occurrence) @key[is]
@key[begin]
   @key[if] Is_Null_Occurrence(X) @key[then]@\-- @examcom[OK in Ada 95]
      -- @examcom[process the case of a null occurrence]
   @key[else]
      -- @examcom[process proper occurrences]
   @key[end if];
@key[end] Process_Ex;
@end[Example]

@leading@;But the detection of @exam[Constraint_Error] in
@exam[Is_Null_Occurrence]
is clearly bad practice since it would be all too easy to mask some
other error by mistake. Accordingly, in Ada 2005, the behaviour of
@exam[Exception_Identity] is changed to return @exam[Null_Id] when
applied to @exam[Null_Occurrence]. So we can now dispense with the
dodgy function @exam[Is_Null_Occurrence] and just write@Defn{Exception_Identity}
@begin[Example]
@tabset[P42]
@key[procedure] Process_Ex(X: @key[in] Exception_Occurrence) @key[is]
@key[begin]
   @key[if] Exception_Identity(X) = Null_Id @key[then]@\-- @examcom[OK in Ada 2005]
      -- @examcom[process the case of a null occurrence]
   @key[else]
      -- @examcom[process proper occurrences]
   @key[end if];
@key[end] Process_Ex;
@end[Example]

Beware that, technically, we now have an incompatibility between Ada
95 and Ada 2005 since the nasty function @exam[Is_Null_Occurrence]
will always return @exam[False] in Ada 2005.

Observe that @exam[Constraint_Error] is also raised if any of the
three functions @exam[Exception_Name], @exam[Exception_Message], or
@exam[Exception_Information] are applied to the value @exam[Null_Occurrence]
so the similar behaviour with @exam[Exception_Identity] in Ada 95
is perhaps understandable at first sight. However, it is believed
that it was not the intention of the language designers but got in
by mistake. Actually the change described here was originally classified
as a correction to Ada 95 but later reclassified as an amendment in
order to draw more attention to it because of the potential incompatibility.

@leading@;The other change in the exception area concerns the raise statement.
It is now possible (optionally of course) to supply a message thus@Defn{raise with message}
@begin[Example]
@key[raise] An_Error @key[with] "A message";
@end[Example]

@leading@;This is purely for convenience and is identical to writing
@begin[Example]
Raise_Exception(An_Error'Identity, "A message");
@end[Example]

There is no change to the form of raise statement without an exception
which simply reraises an existing occurrence.

@leading@keepnext@;Note the difference between
@begin[Example]
@tabset[P35]
@key[raise] An_Error;@\-- @examcom[message is implementation defined]
@end[Example]

@leading@keepnext@;and
@begin[Example]
@tabset[P35]
@key[raise] An_Error @key[with] "";@\-- @examcom[message is null]
@end[Example]

In the first case a subsequent call of @exam[Exception_Message] returns
implementation defined information about the error whereas in the
second case it simply returns the given message which in this example
is a null string.

Some minor changes to the procedure @exam[Raise_Exception] are mentioned
in Section @RefSecNum{Pragmas and Restrictions} below.

There are also additional functions in the package @exam[Ada.Exceptions] to
return the name of an exception as a @exam[Wide_String] or
@exam[Wide_Wide_String]. They have identifiers @exam[Wide_Exception_Name] and
@exam[Wide_Wide_Exception_Name ]and are overloaded to take a parameter of type
@exam[Exception_Id] or @exam[Exception_Occurrence]. The lower bound of the
strings returned by these functions and by the existing functions
@exam[Exception_Name], @exam[Exception_Message] and
@exam[Exception_Information] is @exam[1] (Ada 95 forgot to state this for the
existing functions). The reader will recall that similar additional functions
(and forgetfulness) in the package @exam[Ada.Tags] were mentioned in
@RefSecNum{Object factory functions}.


@LabeledClause{Numerics}

Although Ada 95 introduced unsigned integer types in the form of modular
types, nevertheless, the strong typing rules of Ada have not made
it easy to get unsigned and signed integers to work together. The
following discussion using Ada 95 is based on that in
@AILink{AI=[AI95-00340-01],Text=[AI-340]}.

@leading@;Suppose we wish to implement a simulation of a typical computer which
has addresses and offsets. We make it a generic
@begin[Example]
@key[generic]
   @key[type] Address_Type @key[is mod] <>;
   @key[type] Offset_Type @key[is range] <>;
   ...
@key[package] Simulator @key[is]
   @key[function] Calc_Address(
                Base_Add: Address_Type;
                Offset: Offset_Type) @key[return] Address_Type;
   ...
@key[end] Simulator;
@end[Example]

@leading@;Addresses are represented as unsigned integers (a modular type),
whereas offsets are signed integers. The function @exam[Calc_Address] aims
to add an offset to a base address and return an address. The offset
could be negative.

Na@latin1(239)vely we might hope to write
@begin[Example]
@tabset[P35]
@key[function] Calc_Address(
             Base_Add: Address_Type;
             Offset: Offset_Type) @key[return] Address_Type @key[is]
@key[begin]
   @key[return] Base_Add + Offset;@\-- @examcom[illegal]
@key[end] Calc_Address;
@end[Example]

but this is plainly illegal because @exam[Base_Add] and @exam[Offset]
are of different types.

@leading@keepnext@;We can try a type conversion thus
@begin[Example]
@key[return] Base_Add + Address_Type(Offset);
@end[Example]

@leading@;or perhaps, since @exam[Address_Type] might have a constraint,
@begin[Example]
@key[return] Base_Add + Address_Type'Base(Offset);
@end[Example]

but in any case the conversion is doomed to raise @exam[Constraint_Error]
if @exam[Offset] is negative.

@leading@keepnext@;We then try to be clever and write
@begin[Example]
@key[return] Base_Add +
    Address_Type'Base(Offset @key[mod] Offset_Type'Base(Address_Type'Modulus));
@end[Example]

@leading@;but this raises @exam[Constraint_Error] if @exam[Address_Type'Modulus
> Offset_Type'Base'Last] which it often will be. To see this consider
for example a 32-bit machine with
@begin[Example]
@key[type] Offset_Type @key[is range] @en@;(2**31) .. 2**31@en@;1;
@key[type] Address_Type @key[is mod] 2**32;
@end[Example]

in which case @exam[Address_Type'Modulus] is @exam[2**32] which is
greater than @exam[Offset_Type'Base'Last] which is @exam[2**31@en@;1].

@leading@keepnext@;So we try an explicit test for a negative offset
@begin[Example]
@key[if] Offset >= 0 @key[then]
   @key[return] Base_Add + Address_Type'Base(Offset);
@key[else]
   @key[return] Base_Add - Address_Type'Base(@en@;Offset);
@key[end if];
@end[Example]

But if @exam[Address_Type'Base'Last < Offset_Type'Last] then this
will raise @exam[Constraint_Error] for some values of @exam[Offset].
Unlikely perhaps but this is a generic and so ought to work for all
possible pairs of types.

If we attempt to overcome this then we run into problems in trying
to compare these two values since they are of different types and
converting one to the other can raise the @exam[Constraint_Error]
problem once more. One solution is to use a bigger type to do the
test but this may not exist in some implementations. We could of course
handle the @exam[Constraint_Error] and then patch up the answer. The
ruthless programmer might even think of @exam[Unchecked_Conversion]
but this has its own problems. And so on @en 'tis a wearisome tale.

@leading@;The problem is neatly overcome in Ada 2005 by the introduction of
a new functional attribute@Defn{Mod attribute}@Defn2{Term=[attribute],Sec=[Mod]}
@begin[Example]
@key[function] S'Mod(Arg: @examcom[universal_integer]) @key[return] S'Base;
@end[Example]

@leading@;@exam[S'Mod] applies to any modular subtype @exam[S] and returns
@begin[Example]
Arg @key[mod] S'Modulus
@end[Example]

@leading@;In other words it converts a @examcom{universal_integer} value
to the modular type using the corresponding mathematical mod operation.
We can then happily write
@begin[Example]
@key[function] Calc_Address(
             Base_Add: Address_Type;
             Offset: Offset_Type) @key[return] Address_Type @key[is]
@key[begin]
   @key[return] Base_Add + Address_Type'Mod(Offset);
@key[end] Calc_Address;
@end[Example]

and this always works.

The next topic in the numerics area concerns rounding. One of the
problems in the design of any programming language is getting the
correct balance between performance and portability. This is particularly
evident with numeric types where the computer has to implement only
a crude approximation to the mathematician's integers and reals. The
best performance is achieved by using types and operations that correspond
exactly to the hardware. On the other hand, perfect portability requires
using types with precisely identical characteristics on all implementations.

An interesting example of this problem arises with conversions from
a floating point type to an integer type when the floating type value
is midway between two integer values.

@leading@;In Ada 83 the rounding in the midway case was not specified. This
upset some people and so Ada 95 went the other way and decreed that
such rounding was always away from zero. As well as this rule for
conversion to integer types, Ada 95 also introduced a functional attribute
to round a floating value. Thus for a subtype @exam[S] of a floating
point type @exam[T] we have
@begin[Example]
@key[function] S'Rounding(X: T) @key[return] T;
@end[Example]

This returns the nearest integral value and for midway values rounds
away from zero.

@leading@;Ada 95 also gives a bit more control for the benefit of the
statistically minded by introducing
@begin[Example]
@key[function] S'Unbiased_Rounding(X: T) @key[return] T;
@end[Example]

This returns the nearest integral value and for midway values rounds
to the even value.

However, there are many applications where we don't care which value
we get but would prefer the code to be fast. Implementers have reported
problems with the elementary functions where table look-up is used
to select a particular polynomial expansion. Either polynomial will
do just as well when at the midpoint of some range. However on some
popular hardware such as the Pentium, doing the exact rounding required
by Ada 95 just wastes time and the resulting function is perhaps 20%
slower. This is serious in any comparison with C.

@leading@;This problem is overcome in Ada 2005 by the introduction of a further
attribute@Defn{Machine_Rounding attribute}@Defn2{Term=[attribute],Sec=[Machine_Rounding]}
@begin[Example]
@key[function] S'Machine_Rounding(X: T) @key[return] T;
@end[Example]

This does not specify which of the adjacent integral values is returned
if @exam[X] lies midway. Note that it is not implementation defined
but deliberately unspecified. This should discourage users from depending
upon the behaviour on a particular implementation and thus writing
non-portable code.

Zerophiles will be pleased to note that if @exam[S'Signed_Zeros] is
true and the answer is zero then it has the same sign as @exam[X].

@leading@;It should be noted that @exam[Machine_Rounding], like the other
rounding functions, returns a value of the floating point type and not perhaps
@examcom{universal_integer} as might be expected. So it will
typically be used in a context such as
@begin[Example]
@tabset[P35]
X: Some_Float;
Index: Integer;
...
Index := Integer(Some_Float'Machine_Rounding(X));
...@\-- @examcom[now use Index for table look-up]
@end[Example]

Implementations are urged to detect this case in order to generate
fast code.

The third improvement to the core language in the numerics area concerns
fixed point arithmetic. This is a topic that concerns few people but
those who do use it probably feel passionately about it.

The trouble with floating point is that it is rather machine dependent
and of course integers are just integers. Many application areas have
used some form of scaled integers for many decades and the Ada fixed
point facility is important in certain applications where rigorous
error analysis is desirable.

@leading@;The model of fixed point was changed somewhat from Ada 83 to Ada 95.
One change was that the concepts of model and safe numbers were replaced
by a much simpler model just based on the multiples of the number
@i[small]. Thus consider the type
@begin[Example]
Del: @key[constant] := 2.0**(@en@;15);
@key[type] Frac @key[is delta] Del @key[range] @en@;1.0 .. 1.0;
@end[Example]

In Ada 83 small was defined to be the largest power of 2 not greater
than @exam[Del], and in this case is indeed @exam[2.0**(@en@;15)]. But
in Ada 95, small can be chosen by the implementation to be any power
of 2 not greater than @exam[Del] provided of course that the full
range of values is covered. In both languages an aspect clause can
be used to specify small and it need not be a power of 2. (Remember
that representation clauses are now known as aspect clauses.)

A more far reaching change introduced in Ada 95 concerns the introduction
of operations on the type @examcom{universal_fixed} and type
conversion.

@leading@;A minor problem in Ada 83 was that explicit type conversion was
required in places where it might have been considered quite unnecessary. Thus
supposing we have variables @exam[F], @exam[G], @exam[H] of the above
type @exam[Frac], then in Ada 83 we could not write
@begin[Example]
@tabset[P35]
H := F * G;@\-- @examcom[illegal in Ada 83]
@end[Example]

@leading@keepnext@;but had to use an explicit conversion

@begin[Example]
@tabset[P35]
H := Frac(F * G);@\-- @examcom[legal in Ada 83]
@end[Example]

In Ada 83, multiplication was defined between any two fixed point
types and produced a result of the type @examcom{universal_fixed}
and an explicit conversion was then required to convert this to the
type @exam[Frac].

@leading@;This explicit conversion was considered to be a nuisance so the rule
was changed in Ada 95 to say that multiplication was only defined
between @examcom{universal_fixed} operands and delivered a
@examcom{universal_fixed} result. Implicit conversions were
then allowed for both operands and result provided the type resolution
rules identified no ambiguity. So since the expected type was @exam[Frac]
and no other interpretation was possible, the implicit conversion
was allowed and so in Ada 95 we can simply write
@begin[Example]
@tabset[P35]
H := F * G;@\-- @examcom[legal in Ada 95]
@end[Example]

Similar rules apply to division in both Ada 83 and Ada 95.

@leading@keepnext@;Note however that
@begin[Example]
@tabset[P35]
F := F * G * H;@\-- @examcom[illegal]
@end[Example]

is illegal in Ada 95 because of the existence of the pervasive type
@exam[Duration] defined in @exam[Standard]. The intermediate result
could be either @exam[Frac] or @exam[Duration]. So we have to add
an explicit conversion somewhere.

@leading@;One of the great things about Ada is the ability to define your own
operations. And in Ada 83 many programmers wrote their own arithmetic
operations for fixed point. These might be saturation operations in
which the result is not allowed to overflow but just takes the extreme
implemented value. Such operations often match the behaviour of some
external device. So we might declare
@begin[Example]
@key[function] "*"(Left, Right: Frac) @key[return] Frac @key[is]
@key[begin]
   @key[return] Standard."*"(Left, Right);
@key[exception]
   @key[when] Constraint_Error =>
      @key[if] (Left>0.0 @key[and] Right>0.0) @key[or] (Left<0.0 @key[and] Right<0.0) @key[then]
         @key[return] Frac'Last;
      @key[else]
         @key[return] Frac'First;
      @key[end if];
@key[end] "*";
@end[Example]

and similar functions for addition, subtraction, and division (taking
due care over division by zero and so on). This works fine in Ada
83 and all calculations can now use the new operations rather than
the predefined ones in a natural manner.

@leading@keepnext@;Note however that
@begin[Example]
H := Frac(F * G);
@end[Example]

@leading@;is now ambiguous in Ada 83 since both our own new @exam["*"] and the
predefined @exam["*"] are possible interpretations. However, if we
simply write the more natural
@begin[Example]
H := F * G;
@end[Example]

then there is no ambiguity. So we can program in Ada 83 without the
explicit conversion.

@leading@;However, in Ada 95 we run into a problem when we introduce our own
operations since
@begin[Example]
H := F * G;
@end[Example]

is ambiguous because both the predefined operation and our own operation
are possible interpretations of @exam["*"] in this context. There
is no cure for this in Ada 95 except for changing our own multiplying
operations to be functions with identifiers such as @exam[mul] and
@exam[div]. This is a very tedious chore and prone to errors.

It has been reported that because of this difficulty many projects
using fixed point have not moved from Ada 83 to Ada 95.

This problem is solved in Ada 2005 by changing the name resolution
rules to forbid the use of the predefined multiplication (division)
operation if there is a user-defined primitive multiplication (division)
operation for either operand type unless there is an explicit conversion
on the result or we write @exam[Standard."*"] (or @exam[Standard."/"]).

@leading@keepnext@;This means that when there is no conversion as in
@begin[Example]
H := F * G;
@end[Example]

then the predefined operation cannot apply if there is a primitive
user-defined @exam["*"] for one of the operand types. So the ambiguity
is resolved. Note that if there is a conversion then it is still ambiguous
as in Ada 83.

@leading@;If we absolutely need to have a conversion then we can always use
a qualification as well or just instead. Thus we can write
@begin[Example]
F := Frac'(F * G) * H;
@end[Example]

and this will unambiguously use our own operation.

@leading@;On the other hand if we truly want to use the predefined operation
then we can always write
@begin[Example]
H := Standard."*"(F, G);
@end[Example]

@leading@;Another example might be instructive. Suppose we declare three types
@exam[TL], @exam[TA], @exam[TV] representing lengths, areas, and volumes.
We use centimetres as the basic unit with an accuracy of 0.1 cm together
with corresponding consistent units and accuracies for areas and volumes.
We might declare
@begin[Example]
@key[type] TL @key[is delta] 0.1 @key[range] @en@;100.0 .. 100.0;
@key[type] TA @key[is delta] 0.01 @key[range] @en@;10_000.0 .. 10_000.0;
@key[type] TV @key[is delta] 0.001 @key[range] @en@;1000_000.0 .. 1000_000.0;
@key[for] TL'Small @key[use] TL'Delta;
@key[for] TA'Small @key[use] TA'Delta;
@key[for] TV'Small @key[use] TV'Delta;

@key[function] "*"(Left: TL; Right: TL) @key[return] TA;
@key[function] "*"(Left: TL; Right: TA) @key[return] TV;
@key[function] "*"(Left: TA Right: TL) @key[return] TV;
@key[function] "/"(Left: TV; Right: TL) @key[return] TA;
@key[function] "/"(Left: TV; Right: TA) @key[return] TL;
@key[function] "/"(Left: TA; Right: TL) @key[return] TL;

XL, YL: TL;
XA, YA: TA;
XV, YV: TV;
@end[Example]

These types have an explicit small equal to their delta and are such
that no scaling is required to implement the appropriate multiplication
and division operations. This absence of scaling is not really relevant
to the discussion below but simply illustrates why we might have several
fixed point types and operations between them.

Note that all three types have primitive user-defined multiplication
and division operations even though in the case of multiplication,
@exam[TV] only appears as a result type. Thus the predefined multiplication
or division with any of these types as operands can only be considered
if the result has a type conversion.

@leading@keepnext@;As a consequence the following are legal
@begin[Example]
@tabset[P35]
XV := XL * XA;@\-- @examcom[OK, volume = length @latin1(215) area]
XL := XV / XA;@\-- @examcom[OK, length = volume @latin1(247) area]
@end[Example]

@leading@;but the following are not because they do not match the user-defined
operations
@begin[Example]
@tabset[P35]
XV := XL * XL;@\-- @examcom[no, volume @unicode(8800) length @latin1(215) length]
XV := XL / XA;@\-- @examcom[no, volume @unicode(8800) length @latin1(247) area]
XL := XL * XL;@\-- @examcom[no, length @unicode(8800) length @latin1(215) length]
@end[Example]

@leading@;But if we insist on multiplying two lengths together then we can use
an explicit conversion thus
@begin[Example]
@tabset[P35]
XL := TL(XL * XL);@\-- @examcom[legal, predefined operation]
@end[Example]

and this uses the predefined operation.

@leading@;If we need to multiply three lengths to get a volume without storing
an intermediate area then we can write
@begin[Example]
XV := XL * XL * XL;
@end[Example]

and this is unambiguous since there are no explicit conversions and
so the only relevant operations are those we have declared.

It is interesting to compare this with the corresponding solution
using floating point where we would need to make the unwanted predefined
operations abstract as discussed in an earlier paper
(see @RefSecNum{Overriding and overloading}).

It is hoped that the reader has not found this discussion to be too
protracted. Although fixed point is a somewhat specialized area, it
is important to those who find it useful and it is good to know that
the problems with Ada 95 have been resolved.

There are a number of other improvements in the numerics area but
these concern the Numerics annex and so will be discussed in a later
paper (see @RefSecNum{Numerics annex}).


@LabeledClause{Pragmas and Restrictions}


Ada 2005 introduces a number of new pragmas and @exam[Restrictions]
identifiers. Many of these were described in the previous paper when
discussing tasking and the Real-Time and High Integrity annexes (see
@RefSecNum{The Ravenscar profile}). For
convenience here is a complete list giving the annex if appropriate.

@leading@keepnext@;The new pragmas are
@begin[Example]
@tabset[P35]
Assert
Assertion_Policy
Detect_Blocking@\@examcom[High-Integrity]
No_Return
Preelaborable_Initialization
Profile@\@examcom[Real-Time]
Relative_Deadline@\@examcom[Real-Time]
Unchecked_Union@\@examcom[Interface]
Unsuppress
@end[Example]

@leading@keepnext@;The new @exam[Restrictions] identifiers are
@begin[Example]
@tabset[P35]
Max_Entry_Queue_Length@\@examcom[Real-Time]
No_Dependence
No_Dynamic_Attachment@\@examcom[Real-Time]
No_Implementation_Attributes
No_Implementation_Pragmas
No_Local_Protected_Objects@\@examcom[Real-Time]
No_Obsolescent_Features
No_Protected_Type_Allocators@\@examcom[Real-Time]
No_Relative_Delay@\@examcom[Real-Time]
No_Requeue_Statements@\@examcom[Real-Time]
No_Select_Statements@\@examcom[Real-Time]
No_Synchronous_Control@\@examcom[Real-Time]
No_Task_Termination@\@examcom[Real-Time]
Simple_Barriers@\@examcom[Real-Time]
@end[Example]

We will now discuss in detail the pragmas and @exam[Restrictions]
identifiers in the core language and so not discussed in the previous
paper.

@leading@Defn{Assert pragma}@Defn2{Term=[pragma],Sec=[Assert]}@Defn{Assertion_Policy pragma}@Defn2{Term=[pragma],Sec=[Assertion_Policy]}@;First
there is the pragma @exam[Assert] and the associated pragma
@exam[Assertion_Policy]. Their syntax is as follows
@begin[Example]
@key[pragma] Assert([Check =>] @examcom[boolean]_expression [, [Message =>] @examcom[string]_expression]);

@key[pragma] Assertion_Policy(@examcom[policy]_identifier);
@end[Example]

The first parameter of @exam[Assert] is thus a boolean expression
and the second (and optional) parameter is a string. Remember that
when we write Boolean we mean of the predefined type whereas boolean
includes any type derived from @exam[Boolean] as well.

The parameter of @exam[Assertion_Policy ]is an identifier which controls
the behaviour of the pragma @exam[Assert]. Two policies are defined
by the language, namely, @exam[Check] and @exam[Ignore]. Further policies
may be defined by the implementation.

@leading@;There is also a package @exam[Ada.Assertions] thus
@begin[Example]
@key[package] Ada.Assertions @key[is]
   @key[pragma] Pure(Assertions);

   Assertion_Error: @key[exception];

   @key[procedure] Assert(Check: @key[in] Boolean);
   @key[procedure] Assert(Check: @key[in] Boolean; Message: @key[in] String);
@key[end] Ada.Assertions;
@end[Example]

@leading@;The pragma @exam[Assert] can be used wherever a declaration or statement
is allowed. Thus it might occur in a list of declarations such as
@begin[Example]
N: @key[constant] Integer := ... ;
@key[pragma] Assert(N > 1);
A: Real_Matrix(1 .. N, 1 .. N);
EV: Real_Vector(1 .. N);
@end[Example]

@leading@;and in a sequence of statements such as
@begin[Example]
@key[pragma] Assert(Transpose(A) = A, "A not symmetric");
EV := Eigenvalues(A);
@end[Example]

@leading@;If the policy set by @exam[Assertion_Policy] is @exam[Check] then
the above pragmas are equivalent to
@begin[Example]
@key[if not] N > 1 @key[then]
   @key[raise] Assertion_Error;
@key[end if];
@end[Example]

@leading@;and
@begin[Example]
@key[if not] Transpose(A) = A @key[then]
   @key[raise] Assertion_Error @key[with] "A not symmetric";
@key[end if];
@end[Example]

Remember from Section 2 that a raise statement without any explicit
message is not the same as one with an explicit null message. In the
former case a subsequent call of @exam[Exception_Message ]returns
implementation defined information whereas in the latter case it returns
a null string. This same behaviour thus occurs with the @exam[Assert]
pragma as well @en providing no message is not the same as providing
a null message.

If the policy set by @exam[Assertion_Policy] is @exam[Ignore] then
the @exam[Assert] pragma is ignored at execution time @en but of course
the syntax of the parameters is checked during compilation.

@leading@;The two procedures @exam[Assert] in the package @exam[Ada.Assertions]
have an identical effect to the corresponding @exam[Assert] pragmas
except that their behaviour does not depend upon the assertion policy.
Thus the call
@begin[Example]
Assert(Some_Test);
@end[Example]

@leading@;is always equivalent to
@begin[Example]
@key[if not] Some_Test @key[then]
   @key[raise] Assertion_Error;
@key[end if];
@end[Example]

@leading@;In other words we could define the behaviour of
@begin[Example]
@key[pragma] Assert(Some_Test);
@end[Example]

@leading@keepnext@;as equivalent to
@begin[Example]
@tabset[P35]
@key[if] @examcom[policy_identifier] = Check @key[then]
   Assert(Some_Test);@\       -- @examcom[call of procedure Assert]
@key[end if];
@end[Example]

Note again that there are two procedures @exam[Assert], one with and
one without the message parameter. These correspond to raise statements
with and without an explicit message.

The pragma @exam[Assertion_Policy] is a configuration pragma and controls
the behaviour of @exam[Assert] throughout the units to which it applies.
It is thus possible for different policies to be in effect in different
parts of a partition.

An implementation could define other policies such as @exam[Assume]
which might mean that the compiler is free to do optimizations based
on the assumption that the boolean expressions are true although there
would be no code to check that they were true. Careless use of such
a policy could lead to erroneous behaviour.

@leading@;There was some concern that pragmas such as @exam[Assert] might be
misunderstood to imply that static analysis was being carried out.
Thus in the SPARK language
@LocalLink{Target=[R9],Sec=[References],Text={[9]}}, the annotation
@begin[Example]
--# @key[assert] N /= 0
@end[Example]

is indeed a static assertion and the appropriate tools can be used
to verify this.

However, other languages such as Eiffel have used @key[assert] in
a dynamic manner as now introduced into Ada 2005 and, moreover, many
implementations of Ada have already provided a pragma @exam[Assert]
so it is expected that there will be no confusion with its incorporation
into the standard.

@leading@Defn{No_Return pragma}@Defn2{Term=[pragma],Sec=[No_Return]}@;Another
pragma with a related flavour is @exam[No_Return]. This can
be applied to a procedure (not to a function) and asserts that the
procedure never returns in the normal sense. Control can leave the
procedure only by the propagation of an exception or it might loop
forever (which is common among certain real-time programs). The syntax is
@begin[Example]
@key[pragma] No_Return(@examcom[procedure]_local_name {, @examcom[procedure]_local_name});
@end[Example]

@leading@;Thus we might have a procedure @exam[Fatal_Error] which outputs some
message and then propagates an exception which can be handled in the
main subprogram. For example
@begin[Example]
@tabset[P35]
@key[procedure] Fatal_Error(Msg: @key[in] String) is
   @key[pragma] No_Return(Fatal_Error);
@key[begin]
   Put_Line(Msg);
   ...@\-- @examcom[other last wishes]
   @key[raise] Death;
@key[end] Fatal_Error;
...

@key[procedure] Main @key[is]
   ...
   ...
   Put_Line("Program terminated successfully");
@key[exception]
   @key[when] Death =>
      Put_Line("Program terminated: known error");
   @key[when others] =>
      Put_Line("Program terminated: unknown error");
@key[end] Main;
@end[Example]

There are two consequences of supplying a pragma @exam[No_Return].

@begin[Itemize]
The implementation checks at compile time that the procedure
concerned has no explicit return statements. There is also a check
at run time that it does not attempt to run into the final end @en
@exam[Program_Error] is raised if it does as in the case of running
into the end of a function.

The implementation is able to assume that calls of the
procedure do not return and so various optimizations can be made.
@end[Itemize]

@leading@;We might then have a call of @exam[Fatal_Error] as in

@begin[Example]
@tabset[P35]
@key[function] Pop @key[return] Symbol @key[is]
@key[begin]
   @key[if] Top = 0 @key[then]
      Fatal_Error("Stack empty");@\-- @examcom[never returns]
   @key[elsif]
      Top := Top @en 1;
      @key[return] S(Top+1);
   @key[end if];
@key[end] Pop;
@end[Example]

If @exam[No_Return] applies to @exam[Fatal_Error] then the compiler
should not compile a jump after the call of @exam[Fatal_Error] and
should not produce a warning that control might run into the final
end of @exam[Pop].

@leading@;The pragma @exam[No_Return] now applies to the predefined procedure
@exam[Raise_Exception]. To enable this to be possible its behaviour
with @exam[Null_Id] has had to be changed. In Ada 95 writing
@begin[Example]
Raise_Exception(Null_Id, "Nothing");
@end[Example]

does nothing at all (and so does return in that case) whereas in Ada
2005 it is defined to raise @exam[Constraint_Error] and so now never
returns.

@leading@;We could restructure the procedure @exam[Fatal_Error] to use
@exam[Raise_Exception] thus
@begin[Example]
@tabset[P35]
@key[procedure] Fatal_Error(Msg: @key[in] String) is
   @key[pragma] No_Return(Fatal_Error);
@key[begin]
   ...@\-- @examcom[other last wishes]
   Raise_Exception(Death'Identity, Msg);
@key[end] Fatal_Error;
@end[Example]

Since pragma @exam[No_Return] applies to @exam[Fatal_Error] it is
important that we also know that @exam[Raise_Exception] cannot return.

The exception handler for @exam[Death] in the main subprogram can
now use @exam[Exception_Message] to print out the message.

@leading@;Remember also from Section @RefSecNum{Exceptions} that we can
now also write
@begin[Example]
@key[raise] Death @key[with] Msg;
@end[Example]

rather than call @exam[Raise_Exception].

The pragma @exam[No_Return] is a representation pragma. If a subprogram
has no distinct specification then the pragma @exam[No_Return] is
placed inside the body (as shown above). If a subprogram has a distinct
specification then the pragma must follow the specification in the
same compilation or declarative region. Thus one pragma @exam[No_Return]
could apply to several subprograms declared in the same package specification.

@leading@;It is important that dispatching works correctly with procedures that
do not return. A non-returning dispatching procedure can only be overridden
by a non-returning procedure and so the overriding procedure must
also have pragma @exam[No_Return] thus
@begin[Example]
@key[type] T @key[is tagged] ...
@key[procedure] P(X: T; ... );
@key[pragma] No_Return(P);
...
@key[type] TT @key[is new] T @key[with] ...
@key[overriding]
@key[procedure] P(X: TT; ... );
@key[pragma] No_Return(P);
@end[Example]

The reverse is not true of course. A procedure that does return can
be overridden by one that does not.

It is possible to give a pragma @exam[No_Return] for an abstract procedure,
but obviously not for a null procedure. A pragma @exam[No_Return]
can also be given for a generic procedure. It then applies to all
instances.

@leading@Defn{Preelaborable_Initialization pragma}@Defn2{Term=[pragma],Sec=[Preelaborable_Initialization]}The
next new pragma is @exam[Preelaborable_Initialization]. The syntax
is
@begin[Example]
@key[pragma] Preelaborable_Initialization(direct_name);
@end[Example]

This pragma concerns the categorization of library units and is related
to pragmas such as @exam[Pure] and @exam[Preelaborate]. It is used
with a private type and promises that the full type given by the parameter
will indeed have preelaborable initialization. The details of its
use will be explained in the next paper (see @RefSecNum{Categorization of library units}).

@leading@Defn{Unchecked_Union pragma}@Defn2{Term=[pragma],Sec=[Unchecked_Union]}Another
new pragma is @exam[Unchecked_Union]. The syntax is
@begin[Example]
@key[pragma] Unchecked_Union(@examcom[first_subtype]_local_name);
@end[Example]

The parameter has to denote an unconstrained discriminated record
subtype with a variant part. The purpose of the pragma is to permit
interfacing to unions in C. The following example was given in the
Introduction@Defn2{Term=[union],Sec=[of C]}
@begin[Example]
@key[type] Number(Kind: Precision) @key[is]
   @key[record]
      @key[case] Kind @key[is]
         @key[when] Single_Precision =>
            SP_Value: Long_Float;
         @key[when] Multiple_Precision =>
            MP_Value_Length: Integer;
            MP_Value_First: @key[access] Long_Float;
      @key[end case];
   @key[end record];

@key[pragma] Unchecked_Union(Number);
@end[Example]

@leading@;Specifying the pragma @exam[Unchecked_Union] ensures the following
@begin[Itemize]
The representation of the type does not allow space for
any discriminants.

There is an implicit suppression of @exam[Discriminant_Check].

There is an implicit @key[pragma] @exam[Convention(C)].
@end[Itemize]

@leading@;The above Ada text provides a mapping of the following C union
@begin[Example]
union {
   double spvalue;
   struct {
      int length;
      double* first;
      } mpvalue;
} number;
@end[Example]

The general idea is that the C programmer has created a type which
can be used to represent a floating point number in one of two ways
according to the precision required. One way is just as a double length
value (a single item) and the other way is as a number of items considered
juxtaposed to create a multiple precision value. This latter is represented
as a structure consisting of an integer giving the number of items
followed by a pointer to the first of them. These two different forms
are the two alternatives of the union.

@leading@;In the Ada mapping the choice of precision is governed by the
discriminant @exam[Kind] which is of an enumeration type as follows
@begin[Example]
@key[type] Precision @key[is] (Single_Precision, Multiple_Precision);
@end[Example]

In the single precision case the component @exam[SP_Value] of type
@exam[Long_Float] maps onto the C component @exam[spvalue ]of type
@exam[double].

The multiple precision case is somewhat troublesome. The Ada component
@exam[MP_Value_Length] maps onto the C component @exam[length] and
the Ada component @exam[MP_Value_First] of type @key[access]@exam[
Long_Float] maps onto the C component @exam[first] of type @exam[double*].

@leading@;In our Ada program we can declare a variable thus
@begin[Example]
X: Number(Multiple_Precision);
@end[Example]

@leading@;and we then obtain a value in @exam[X] by calling some C subprogram.
We can then declare an array and map it onto the C sequence of double
length values thus
@begin[Example]
A: @key[array] (1 .. X.MP_Value_Length) @key[of] Long_Float;
@key[for] A'Address @key[use] X.MP_Value_First.@key[all]'Address;
@key[pragma] Import(C, A);
@end[Example]

The elements of @exam[A] are now the required values. Note that we
don't use an Ada array in the declaration of @exam[Number] because
there might be problems with dope information.

The Ada type can also have a non-variant part preceding the variant
part and variant parts can be nested. It may have several discriminants.

@leading@;When an object of an unchecked union type is created, values must
be supplied for all its discriminants even though they are not stored.
This ensures that appropriate default values can be supplied and that
an aggregate contains the correct components. However, since the discriminants
are not stored, they cannot be read. So we can write
@begin[Example]
X: Number := (Single_Precision, 45.6);
Y: Number(Single_Precision);
...
Y.SP_Value := 55.7;
@end[Example]

@leading@;The variable @exam[Y] is said to have an inferable discriminant whereas
@exam[X] does not. Although it is clear that playing with unchecked
unions is potentially dangerous, nevertheless Ada 2005 imposes certain
rules that avoid some dangers. One rule is that predefined equality
can only be used on operands with inferable discriminants; @exam[Program_Error]
is raised otherwise. So
@begin[Example]
@tabset[P35]
@key[if] Y = 55.8 @key[then]@\-- @examcom[OK]

@key[if] X = 45.5 @key[then]@\-- @examcom[raises Program_Error]

@key[if] X = Y @key[then]@\-- @examcom[raises Program_Error]
@end[Example]

@leading@;It is important to be aware that unchecked union types are introduced
in Ada 2005 for the sole purpose of interfacing to C programs and
not for living dangerously. Thus consider
@begin[Example]
@key[type] T(Flag: Boolean := False) @key[is]
   @key[record]
      @key[case] Flag @key[is]
         @key[when] False =>
            F1: Float := 0.0;
         @key[when] True =>
            F2: Integer := 0;
      @key[end case];
   @key[end record];
@key[pragma] Unchecked_Union(T);
@end[Example]

@leading@;The type @exam[T] can masquerade as either type @exam[Integer] or
@exam[Float]. But we should not use unchecked union types as an alternative
to unchecked conversion. Thus consider
@begin[Example]
@tabset[P35]
X: T;@\-- @examcom[Float by default]
Y: Integer := X.F2;@\-- @examcom[erroneous]
@end[Example]

The object @exam[X] has discriminant @exam[False] by default and thus
has the value zero of type @exam[Integer]. In the absence of the pragma
@exam[Unchecked_Union], the attempt to read @exam[X.F2] would raise
@exam[Constraint_Error] because of the discriminant check. The use
of @exam[Unchecked_Union] suppresses the discriminant check and so
the assignment will occur. But note that the ARM clearly says (11.5(26))
that if a check is suppressed and the corresponding error situation
arises then the program is erroneous.

However, assigning a @exam[Float] value to an @exam[Integer] object
using @exam[Unchecked_Conversion] is not erroneous providing certain
conditions hold such as that @exam[Float'Size = Integer'Size].

@leading@Defn{Unsuppress pragma}@Defn2{Term=[pragma],Sec=[Unsuppress]}@;The
final pragma to be considered is @exam[Unsuppress]. Its syntax is
@begin[Example]
@key[pragma] Unsuppress(identifier);
@end[Example]

The identifier is that of a check or perhaps @exam[All_Checks]. The
pragma @exam[Unsuppress] is essentially the opposite of the existing
pragma @exam[Suppress] and can be used in the same places with similar
scoping rules.

Remember that pragma @exam[Suppress] gives an implementation the permission
to omit the checks but it does not require that the checks be omitted
(they might be done by hardware). The pragma @exam[Unsuppress] simply
revokes this permission. One pragma can override the other in a nested
manner. If both are given in the same region then they apply from
the point where they are given and the later one thus overrides.

A likely scenario would be that @exam[Suppress] applies to a large
region of the program (perhaps all of it) and @exam[Unsuppress] applies
to a smaller region within. The reverse would also be possible but
perhaps less likely.

Note that @exam[Unsuppress] does not override the implicit @exam[Suppress]
of @exam[Discriminant_Check] provided by the pragma @exam[Unchecked_Union]
just discussed.

@leading@;A sensible application of @exam[Unsuppress] would be in the fixed
point operations mentioned in Section @RefSecNum{Numerics} thus
@begin[Example]
@key[function] "*"(Left, Right: Frac) @key[return] Frac @key[is]
   @key[pragma] Unsuppress(Overflow_Check);
@key[begin]
   @key[return] Standard."*"(Left, Right);
@key[exception]
   @key[when] Constraint_Error =>
      @key[if] (Left>0.0 @key[and] Right>0.0) @key[or] (Left<0.0 @key[and] Right<0.0) @key[then]
         @key[return] Frac'Last;
      @key[else]
         @key[return] Frac'First;
      @key[end if];
@key[end] "*";
@end[Example]

The use of @exam[Unsuppress] ensures that the overflow check is not
suppressed even if there is a global @exam[Suppress] for the whole
program (or the user has switched checks off through the compiler
command line). So @exam[Constraint_Error] will be raised as necessary
and the code will work correctly.

@leading@;In Ada 95 the pragma @exam[Suppress] has the syntax
@begin[Example]
@tabset[P49]
@key[pragma] Suppress(identifier [ , [On =>] name]);@\--@examcom[Ada 95]
@end[Example]

@leading@;The second and optional parameter gives the name of the entity to
which the permission applies. There was never any clear agreement
on what this meant and implementations varied. Accordingly, in Ada
2005 the second parameter is banished to
@URLLink{URL=[http://www.adaic.org/standards/05rm/html/RM-J-10.html],Text=[Annex J]}
so that the syntax
in the core language is similar to @exam[Unsuppress] thus
@begin[Example]
@tabset[P49]
@key[pragma] Suppress(identifier);@\-- @examcom[Ada 2005]
@end[Example]

For symmetry, Annex J actually allows an obsolete @exam[On] parameter
for @exam[Unsuppress]. It might seem curious that a feature should
be born obsolescent.

@leading@Defn2{Term=[restrictions identifier],Sec=[No_Dependence]}A
number of new @exam[Restrictions] identifiers are added in Ada 2005.
The first is @exam[No_Dependence] whose syntax is
@begin[Example]
@key[pragma] Restrictions(No_Dependence => name);
@end[Example]

This indicates that there is no dependence on a library unit with
the given name.

@leading@;The name might be that of a predefined unit but it could in fact be
any unit. For example, it might be helpful to know that there is no
dependence on a particular implementation-defined unit such as a package
@exam[Superstring] thus
@begin[Example]
@key[pragma] Restrictions(No_Dependence => Superstring);
@end[Example]

Care needs to be taken to spell the name correctly; if we write
@exam[Supperstring] by mistake then the compiler will not be able to help us.

@leading@;The introduction of @exam[No_Dependence] means that the existing
@exam[Restrictions] identifier @exam[No_Asynchronous_Control] is moved to
@URLLink{URL=[http://www.adaic.org/standards/05rm/html/RM-J-13.html],Text=[Annex J]}
since we can now write
@begin[Example]
@key[pragma] Restrictions(No_Dependence => Ada.Asynchronous_Task_Control);
@end[Example]

Similarly, the identifiers @exam[No_Unchecked_Conversion] and @exam[No_Unchecked_Deallocation]
are also moved to @URLLink{URL=[http://www.adaic.org/standards/05rm/html/RM-J-13.html],Text=[Annex J]}.

Note that the identifier @exam[No_Dynamic_Attachment] which refers
to the use of the subprograms in the package @exam[Ada.Interrupts]
cannot be treated in this way because of the child package
@exam[Ada.Interrupts.Names]. No dependence on @exam[Ada.Interrupts] would
exclude the use of the child package @exam[Names] as well.

The restrictions identifier @exam[No_Dynamic_Priorities] cannot be
treated this way either for a rather different reason. In Ada 2005
this identifier is extended so that it also excludes the use of the
attribute @exam[Priority] and this would not be excluded by just saying
no dependence on @exam[Ada.Dynamic_Priorities].

@leading@Defn2{Term=[restrictions identifier],Sec=[No_Implementation_Pragmas]}@Defn2{Term=[restrictions identifier],Sec=[No_Implementation_Attributes]}Two
further @exam[Restrictions] identifiers are introduced to encourage
portability. We can write
@begin[Example]
@key[pragma] Restrictions(No_Implementation_Pragmas, No_Implementation_Attributes);
@end[Example]

These do not apply to the whole partition but only to the compilation
or environment concerned. This helps us to ensure that implementation
dependent areas of a program are identified.

@leading@Defn2{Term=[restrictions identifier],Sec=[No_Obsolescent_Features]}The
final new restrictions identifier similarly prevents us from inadvertently
using features in
@URLLink{URL=[http://www.adaic.org/standards/05rm/html/RM-J.html],Text=[Annex J]}
thus
@begin[Example]
@key[pragma] Restrictions(No_Obsolescent_Features);
@end[Example]

Again this does not apply to the whole partition but only to the compilation
or environment concerned. (It is of course not itself defined in Annex
J.)

@leading@;The reader will recall that in Ada 83 the predefined packages had
names such as @exam[Text_IO] whereas in Ada 95 they are @exam[Ada.Text_IO]
and so on. In order to ease transition from Ada 83, a number of renamings
were declared in
@URLLink{URL=[http://www.adaic.org/standards/05rm/html/RM-J-1.html],Text=[Annex J]}
such as
@begin[Example]
@key[with] Ada.Text_IO;
@key[package] Text_IO @key[renames] Ada.Text_IO;
@end[Example]

A mild problem is that the user could write these renamings anyway
and we do not want the @exam[No_Obsolescent_Features] restriction
to prevent this. Moreover, implementations might actually implement
the renamings in Annex J by just compiling them and we don't want
to force implementations to use some trickery to permit the user to
do it but not the implementation. Accordingly, whether the
@exam[No_Obsolescent_Features] restriction applies to these renamings or not is
implementation defined.


@LabeledClause{Generic units}


There are a number of improvements in the area of generics many of
which have already been outlined in earlier papers.

@leading@;A first point concerns access types. The introduction of types that
exclude null means that a formal access type parameter can take the
form
@begin[Example]
@key[generic]
   ...
   @key[type] A @key[is not null access] T;
   ...
@end[Example]

The actual type corresponding to @exam[A] must then itself be an access
type that excludes null. A similar rule applies in reverse @en if
the formal parameter includes null then the actual parameter must
also include null. If the two did not match in this respect then all
sorts of difficulties could arise.

@leading@;Similarly if the formal parameter is derived from an access type
@begin[Example]
@tabset[P35]
@key[generic]
   ...
   @key[type] FA @key[is new] A;@\-- @examcom[A is an access type]
   ...
@end[Example]

@leading@;then the actual type corresponding to @exam[FA] must exclude null
if @exam[A] excludes null and vice versa. Half of this rule is automatically
enforced since a type derived from a type that excludes null will
automatically exclude null. But the reverse is not true as mentioned
in an earlier paper (see @RefSecNum{Null exclusion and constant}) when
discussing access types. If @exam[A] has the declaration
@begin[Example]
@tabset[P35]
@key[type] A @key[is access all] Integer;@\-- @examcom[does not exclude null]
@end[Example]

@leading@keepnext@;then we can declare
@begin[Example]
@tabset[P35]
@key[type] NA @key[is new] A;@\-- @examcom[does not exclude null]
@key[type] NNA @key[is new not null] A;@\-- @examcom[does exclude null]
@end[Example]

and then @exam[NA] matches the formal parameter @exam[FA] in the above
generic but @exam[NNA] does not.

@leading@;There is also a change to formal derived types concerning limitedness.
In line with the changes described in the paper on the object oriented
model (see @RefSecNum{Interfaces}), the syntax now permits @key[limited]
to be stated explicitly thus
@begin[Example]
@tabset[P49]
@key[generic]
   @key[type] T @key[is limited new] LT;@\-- @examcom[untagged]
   @key[type] TT @key[is limited new] TLT @key[with private];@\-- @examcom[tagged]
@end[Example]

However, this can be seen simply as a documentation aid since the
actual types corresponding to @exam[T] and @exam[TT] must be derived
from @exam[LT] and @exam[TLT] and so will be limited if @exam[LT]
and @exam[TLT] are limited anyway.

@leading@;Objects of anonymous access types are now also allowed as generic
formal parameters so we can have
@begin[Example]
@key[generic]
   A: @key[access] T := @key[null];
   AN: @key[in out] @key[not null access] T;
   F: @key[access function ](X: Float) @key[return] Float;
   FN: @key[not null access function] (X: Float) @key[return] Float;
@end[Example]

If the subtype of the formal object excludes null (as in @exam[AN]
and @exam[FN]) then the actual must also exclude null but not vice
versa. This contrasts with the rule for formal access types discussed
above in which case both the formal type and actual type have to exclude
null or not. Note moreover that object parameters of anonymous access
types can have mode @key[in out].

@leading@;If the subprogram profile itself has access parameters that exclude
null as in
@begin[Example]
@key[generic]
   PN: @key[access procedure] (AN: @key[not null access] T);
@end[Example]

@leading@;then the actual subprogram must also have access parameters that
exclude null and so on. The same rule applies to named formal subprogram
parameters. If we have
@begin[Example]
@key[generic]
   @key[with procedure] P(AN: @key[not null access] T);
   @key[with procedure] Q(AN: @key[access] T);
@end[Example]

@leading@;then the actual corresponding to @exam[P] must have a parameter that
excludes null but the actual corresponding to @exam[Q] might or might
not. The rule is similar to renaming @en "not null must never lie".
Remember that the matching of object and subprogram generic parameters
is defined in terms of renaming. Here is an example to illustrate
why the asymmetry is important. Suppose we have
@begin[Example]
@key[generic]
   @key[type] T @key[is private];
   @key[with procedure] P(Z: @key[in] T);
@key[package] G @key[is]
@end[Example]

@leading@keepnext@;This can be matched by
@begin[Example]
@key[type] A @key[is access] ...;
@key[procedure] Q(Y: @key[in] @key[not null] A);
...
@key[package] NG @key[is new] G(T => A; P => Q);
@end[Example]

Note that since the formal type @exam[T] is not known to be an access
type in the generic declaration, there is no mechanism for applying
a null exclusion to it. Nevertheless there is no reason why the instantiation
should not be permitted.

@leading@;There are some other changes to existing named formal subprogram parameters.
The reader will recall from the discussion on interfaces in an earlier
paper (see @RefSecNum{Interfaces}) that the concept of null procedures
has been added in Ada 2005.@Defn2{Term=[null procedure],Sec=[formal parameter default]}
A null procedure has no body but behaves as if it has a body comprising
a null statement. It is now possible to use a null procedure as a
possible form of default for a subprogram parameter. Thus there are
now three possible forms of default as follows
@begin[Example]
@tabset[P42]
@key[with procedure] P( ... ) @key[is] <>;@\-- @examcom[OK in Ada 95]
@key[with procedure] Q( ... ) @key[is] Some_Proc;@\-- @examcom[OK in Ada 95]
@key[with procedure] R( ... ) @key[is null];@\-- @examcom[only in Ada 2005]
@end[Example]

@leading@keepnext@;So if we have
@begin[Example]
@key[generic]
   @key[type] T @key[is] (<>);
   @key[with procedure] R(X: @key[in] Integer; Y: @key[in out] T) @key[is null];
@key[package] PP ...
@end[Example]

@leading@;then an instantiation omitting the parameter for @exam[R] such as
@begin[Example]
@key[package] NPP @key[is new] PP(T => Colour);
@end[Example]

@leading@;is equivalent to providing an actual procedure @exam[AR] thus
@begin[Example]
@key[procedure] AR(X: @key[in] Integer; Y: @key[in out] Colour) @key[is]
@key[begin]
   @key[null];
@key[end] AR;
@end[Example]

Note that the profile of the actual procedure is conjured up to match
the formal procedure.

Of course, there is no such thing as a null function and so null is
not permitted as the default for a formal function.

@leading@;A new kind of subprogram parameter was introduced in some detail
when discussing object factory functions in the paper on the object oriented
model (see @RefSecNum{Object factory functions}). This is the abstract
formal subprogram.@Defn{abstract formal subprogram} The example given was
the predefined generic function @exam[Generic_Dispatching_Constructor]
thus
@begin[Example]
@key[generic]
   @key[type] T (<>) @key[is abstract tagged limited private];
   @key[type] Parameters (<>) @key[is limited private];
   @key[with function] Constructor(Params: @key[not null] @key[access] Parameters) @key[return] T @key[is abstract];
@key[function] Ada.Tags.Generic_Dispatching_Constructor
   (The_Tag: Tag; Params: @key[not null] @key[access] Parameters) @key[return] T'Class;
@end[Example]

The formal function @exam[Constructor] is an example of an abstract
formal subprogram. Remember that the interpretation is that the actual
function must be a dispatching operation of a tagged type uniquely
identified by the profile of the formal function. The actual operation
can be concrete or abstract. Formal abstract subprograms can of course
be procedures as well as functions. It is important that there is
exactly one controlling type in the profile.

@leading@;Formal abstract subprograms can have defaults in much the same way
that formal concrete subprograms can have defaults. We write
@begin[Example]
@key[with] @key[procedure] P(X: @key[in out] T) @key[is abstract] <>;
@key[with function] F @key[return] T @key[is abstract] Unit;
@end[Example]

The first means of course that the default has to have identifier
@exam[P] and the second means that the default is some function @exam[Unit].
It is not possible to give null as the default for an abstract parameter
for various reasons. Defaults will probably be rarely used for abstract
parameters.

@leading@;The introduction of interfaces in Ada 2005 means that a new class
of generic parameters is possible. Thus we might have@Defn2{Term=[interface],Sec=[formal type]}
@begin[Example]
@key[generic]
   @key[type] F @key[is interface];
@end[Example]

The actual type could then be any interface. This is perhaps unlikely.

@leading@;If we wanted to ensure that a formal interface had certain operations
then we might first declare an interface @exam[A] with the required
operations
@begin[Example]
@key[type] A @key[is interface];
@key[procedure] Op1(X: A; ... ) @key[is abstract];
@key[procedure] N1(X: A; ... ) @key[is null];
@end[Example]

@leading@keepnext@;and then
@begin[Example]
@key[generic]
   @key[type] F @key[is interface and ]A;
@end[Example]

and then the actual interface must be descended from @exam[A] and
so have operations which match @exam[Op1] and @exam[N1].

@leading@;A formal interface might specify several ancestors
@begin[Example]
@key[generic]
   @key[type] FAB @key[is interface and] A @key[and] B;
@end[Example]

@leading@;where @exam[A] and @exam[B] are themselves interfaces. And @exam[A]
and @exam[B] or just some of them might themselves be further formal
parameters as in
@begin[Example]
@key[generic]
   @key[type] A @key[is interface];
   @key[type] FAB @key[is interface and] A @key[and] B;
@end[Example]

These means that @exam[FAB] must have both @exam[A] and @exam[B] as
ancestors; it could of course have other ancestors as well.

@leading@;The syntax for formal tagged types is also changed to take into
account the possibility of interfaces. Thus we might have
@begin[Example]
@key[generic]
   @key[type] NT @key[is new] T @key[and] A @key[and] B @key[with private];
@end[Example]

in which case the actual type must be descended both from the tagged
type @exam[T] and the interfaces @exam[A] and @exam[B]. The parent
type @exam[T] itself might be an interface or a normal tagged type.
Again some or all of @exam[T], @exam[A], and @exam[B] might be earlier
formal parameters. Also we can explicitly state @key[limited] in which
case all of the ancestor types must also be limited.

@leading@;An example of this sort of structure occurred when discussing
printable geometric objects in the paper on the object oriented model
(see @RefSecNum{Interfaces}). We had
@begin[Example]
@key[generic]
   @key[type] T @key[is abstract tagged private];
@key[package] Make_Printable @key[is]
   @key[type] Printable_T @key[is] @key[abstract new] T @key[and] Printable @key[with private];
   ...
@key[end];
@end[Example]

@leading@;It might be that we have various interfaces all derived from
@exam[Printable]
which serve different purposes (perhaps for different output devices,
laser printer, card punch and so on). We would then want the generic
package to take any of these interfaces thus
@begin[Example]
@key[generic]
   @key[type] T @key[is abstract tagged private];
   @key[type] Any_Printable @key[is interface and] Printable;
@key[package] Make_Printable @key[is]
   @key[type] Printable_T @key[is] @key[abstract new] T @key[and] Any_Printable @key[with private];
   ...
@key[end];
@end[Example]

A formal interface can also be marked as limited in which case the
actual interface must also be limited and vice versa.

@leading@;As discussed in the previous paper (see
@RefSecNum{Synchronized interfaces}),
interfaces can also be synchronized, task, or protected. Thus we might have
@begin[Example]
@key[generic]
   @key[type] T @key[is task interface];
@end[Example]

and then the actual interface must itself be a task interface. The
correspondence must be exact. A formal synchronized interface can
only be matched by an actual synchronized interface and so on. Remember
from the discussion in the previous paper (see @RefSecNum{Synchronized interfaces})
that a task interface can
be composed from a synchronized interface. This flexibility does not
extend to matching actual and formal generic parameters.

@leading@;Another small change concerns object parameters of limited types.
In Ada 95 the following is illegal
@begin[Example]
@tabset[P42]
@key[type] LT @key[is limited]
   @key[record]
      A: Integer;
      B: Float;
   @key[end record];@\-- @examcom[a limited type]

@key[generic]
   X: @key[in] LT;@\-- @examcom[illegal in Ada 95]
   ...
@key[procedure] P ...
@end[Example]

It is illegal in Ada 95 because it is not possible to provide an actual
parameter. This is because the parameter mechanism is one of initialization
of the formal object parameter by the actual and this is treated as
assignment and so is not permitted for limited types.@Defn[limited generic formal object]

@leading@;However, in Ada 2005, initialization of a limited object by an
aggregate is allowed since the value is created @i[in situ] as discussed in an
earlier paper (see @RefSecNum{Limited types and return statements}). So an
instantiation is possible thus
@begin[Example]
@key[procedure] Q @key[is new] P(X => (A => 1, B => 2.0), ... );
@end[Example]

Remember that an initial value can also be provided by a function
call and so the actual parameter could also be a function call returning
a limited type.

The final improvement to the generic parameter mechanism concerns
package parameters.

@leading@;In Ada 95 package parameters take two forms. Given a generic package
@exam[Q] with formal parameters @exam[F1], @exam[F2], @exam[F3], then
we can have
@begin[Example]
@key[generic]
   @key[with package] P @key[is new] Q(<>);
@end[Example]

@leading@;and then the actual package corresponding to the formal @exam[P] can
be any instantiation of @exam[Q]. Alternatively
@begin[Example]
@key[generic]
   @key[with package] R @key[is new] Q(P1, P2, P3);
@end[Example]

and then the actual package corresponding to @exam[R] must be an instantiation
of @exam[Q] with the specified actual parameters @exam[P1], @exam[P2],
@exam[P3].

@leading@;As mentioned in the Introduction, a simple example of the use of
these two forms occurs with the package @exam[Generic_Complex_Arrays] which
takes instantiations of @exam[Generic_Real_Arrays] and
@exam[Generic_Complex_Types] which in turn both have the underlying floating
type as their single parameter. It is vital that both packages use the same
floating point type and this is assured by writing
@begin[Example]
@key[generic]
   @key[with package] Real_Arrays @key[is] @key[new] Generic_Real_Arrays(<>);
   @key[with package] Complex_Types @key[is new] Generic_Complex_Types(Real_Arrays.Real);
@key[package] Generic_Complex_Arrays @key[is] ...
@end[Example]

However, the mechanism does not work very well when several parameters
are involved as will now be illustrated with some examples.

@leading@;The first example concerns using the new container library which will
be discussed in some detail in a later paper@Comment{ (see @RefSecNum{*** TBD - containers ***})}.
There are generic packages such as
@begin[Example]
@key[generic]
   @key[type] Index_Type @key[is range] <>;
   @key[type] Element_Type @key[is private]:
   @key[with function] "=" (Left, Right: Element_Type ) @key[return] Boolean @key[is] <>;
@key[package] Ada.Containers.Vectors @key[is] ...
@end[Example]

@leading@;and
@begin[Example]
@key[generic]
   @key[type] Key_Type @key[is private];
   @key[type] Element_Type @key[is private]:
   @key[with function] Hash(Key: Key_Type) @key[return] Hash_Type;
   @key[with function] Equivalent_Keys(Left, Right: Key_Type) @key[return] Boolean;
   @key[with function] "=" (Left, Right: Element_Type ) @key[return] Boolean @key[is] <>;
@key[package] Ada.Containers.Hashed_Maps @key[is] ...
@end[Example]

We might wish to pass instantiations of both of these to some other
package with the proviso that both were instantiated with the same
@exam[Element_Type]. Otherwise the parameters can be unrelated.

@leading@;It would be natural to make the vector package the first parameter
and give it the @exam[(<>)] form. But we then find that in Ada 95
we have to repeat all the parameters other than @exam[Element_Type]
for the maps package. So we have
@begin[Example]
@key[with] ... ; @key[use] Ada.Containers;
@key[generic]
   @key[with package] V @key[is new] Vectors(<>);
   @key[type] Key_Type @key[is private];
   @key[with function] Hash(Key: Key_Type) @key[return] Hash_Type;
   @key[with function] Equivalent_Keys(Left, Right: Key_Type) @key[return] Boolean;
   @key[with function] "=" (Left, Right: Element_Type ) @key[return] Boolean @key[is] <>;
   @key[with package] HM @key[is new] Hashed_Maps(
             Key_Type => Key_Type,
             Element_Type => V.Element_Type,
             Hash => Hash,
             Equivalent_Keys => Equivalent_Keys,
             "=" => "=");
@key[package] HMV @key[is] ...
@end[Example]

@leading@;This is a nuisance since when we instantiate @exam[HMV] we have to
provide all the parameters required by @exam[Hashed_Maps] even though
we must already have instantiated it elsewhere in the program. Suppose
that instantiation was
@begin[Example]
@key[package] My_Hashed_Map @key[is new] Hashed_Maps(My_Key, Integer, Hash_It, Equiv, "=");
@end[Example]

@leading@keepnext@;and suppose also that we have instantiated @exam[Vectors]
@begin[Example]
@key[package] My_Vectors @key[is new] Vectors(Index, Integer, "=");
@end[Example]

@leading@keepnext@;Now when we come to instantiate @exam[HMV] we have to write
@begin[Example]
@key[package] My_HMV @key[is]
             @key[new] HMV(My_Vectors, My_Key, Hash_It, Equiv, "=", My_Hashed_Maps);
@end[Example]

This is very annoying. Not only do we have to repeat all the auxiliary
parameters of @exam[Hashed_Maps] but the situation regarding @exam[Vectors]
and @exam[Hashed_Maps] is artificially made asymmetric. (Life would
have been a bit easier if we had made @exam[Hashed_Maps] the first
package parameter but that just illustrates the asymmetry.) Of course
we could more or less overcome the asymmetry by passing all the parameters
of @exam[Vectors] as well but then @exam[HMV ]would have even more
parameters. This rather defeats the point of package parameters which
were introduced into Ada 95 in order to avoid the huge parameter lists
that had occurred in Ada 83.

@leading@;Ada 2005 overcomes this problem by permitting just some of the actual
parameters to be specified. Any omitted parameters are indicated using
the @exam[<>] notation thus@Defn2{Term=[actual parameter],Sec=[for formal package]}
@begin[Example]
@key[generic]
   @key[with package] S @key[is new] Q(P1, F2 => <>, F3 => <>);
@end[Example]

@leading@;In this case the actual package corresponding to @exam[S] can be any
package which is an instantiation of @exam[Q] where the first actual
parameter is @exam[P1] but the other two parameters are left unspecified.
We can also abbreviate this to
@begin[Example]
@key[generic]
   @key[with package] S @key[is new] Q(P1, @key[others] => <>);
@end[Example]

Note that the @exam[<>] notation can only be used with named parameters
and also that @exam[(<>)] is now considered to be a shorthand for
@exam[(]@key[others] @exam[=> <>)].

@leading@keepnext@;As another example
@begin[Example]
@key[generic]
   @key[with package] S @key[is new] Q(F1 => <>, F2 => P2, F3 => <>);
@end[Example]

@leading@;means that the actual package corresponding to @exam[S] can be any
package which is an instantiation of @exam[Q] where the second actual
parameter is @exam[P2] but the other two parameters are left unspecified.
This can be abbreviated to
@begin[Example]
@key[generic]
   @key[with package] S @key[is new] Q(F2 => P2, @key[others] => <>);
@end[Example]

@leading@;Using this new notation, the package @exam[HMV] can now simply be
written as
@begin[Example]
@key[with] ... ; @key[use] Ada.Containers;
@key[generic]
   @key[with package] V @key[is new] Vectors(<>);
   @key[with package] HM @key[is new] Hashed_Maps
             (Element_Type => V.Element_Type, @key[others] => <>);
@key[package] HMV @key[is] ...
@end[Example]

@leading@;and our instantiation of @exam[HMV] becomes simply
@begin[Example]
@key[package] My_HMV @key[is] @key[new] HMV(My_Vectors, My_Hashed_Maps);
@end[Example]

@leading@;Some variations on this example are obviously possible. For example
it is likely that the instantiation of @exam[Hashed_Maps] must use
the same definition of equality for the type @exam[Element_Type] as
@exam[Vectors]. We can ensure this by writing
@begin[Example]
@key[with] ... ; @key[use] Ada.Containers;
@key[generic]
   @key[with package] V @key[is new] Vectors(<>);
   @key[with package] HM @key[is new] Hashed_Maps
             (Element_Type => V.Element_Type, "=" => V."=", @key[others] => <>);
@key[package] HMV @key[is] ...
@end[Example]

@leading@;If this seems rather too hypothetical, a more concrete example might
be a generic function which converts a vector into a list provided
they have the same element type and equality. Note first that the
specification of the container package for lists is
@begin[Example]
@key[generic]
   @key[type] Element_Type @key[is private];
   @key[with function] "=" (Left, Right: Element_Type) @key[return] Boolean @key[is] <>;
@key[package] Ada.Containers.Doubly_Linked_Lists @key[is] ...
@end[Example]

@leading@;The specification of a generic function @exam[Convert] might be
@begin[Example]
@key[generic]
   @key[with package] DLL @key[is new] Doubly_Linked_Lists(<>);
   @key[with package] V @key[is new] Vectors
             (Index_Type => <>, Element_Type => DLL.Element_Type, "=" => DLL."=");
@key[function] Convert(The_Vector: V.Vector) @key[return] DLL.List;
@end[Example]

@leading@;On the other hand if we only care about the element types matching
and not about equality then we could write
@begin[Example]
@key[generic]
   @key[with package] DLL @key[is new] Doubly_Linked_Lists(<>);
   @key[with package] V @key[is new] Vectors(Element_Type => DLL.Element_Type, @key[others] => <>);
@key[function] Convert(The_Vector: V.Vector) @key[return] DLL.List;
@end[Example]

Note that if we had reversed the roles of the formal packages then
we would not need the new @exam[<>] notation if both equality and
element type had to match but it would be necessary for the case where
only the element type had to match.

@leading@;Other examples might arise in the numerics area. Suppose we have two
independently written generic packages @exam[Do_This] and @exam[Do_That]
which both have a floating point type parameter and several other
parameters as well. For example
@begin[Example]
@key[generic]
   @key[type] Real @key[is digits] <>;
   Accuracy: @key[in] Real;
   @key[type] Index @key[is range] <>;
   Max_Trials: @key[in] Index;
@key[package] Do_This @key[is] ...

@key[generic]
   @key[type] Floating @key[is digits] <>;
   Bounds: @key[in] Floating;
   Iterations: @key[in] Integer;
   Repeat: @key[in] Boolean;
@key[package] Do_That @key[is] ...
@end[Example]

(This is typical of much numerical stuff. Authors are cautious and
unable to make firm decisions about many aspects of their algorithms
and therefore pass the buck back to the user in the form of a turgid
list of auxiliary parameters.)

@leading@;We now wish to write a package @exam[Super_Solver] which takes
instantiations
of both @exam[Do_This] and @exam[Do_That] with the requirement that
the floating type used for the instantiation is the same in each case
but otherwise the parameters are unrelated. In Ada 95 we are again
forced to repeat one set of parameters thus@Defn{super solver}
@begin[Example]
@key[generic]
   @key[with package] This @key[is new] Do_This(<>);
   S_Bounds: @key[in] This.Real;
   S_Iterations: @key[in] Integer;
   S_Repeat: @key[in] Boolean;
   @key[with package] That @key[is new] Do_That(This.Real, S_Bounds, S_Iterations, S_Repeat);
@key[package] Super_Solver @key[is] ...
@end[Example]

@leading@;And when we come to instantiate @exam[Super_Solver] we have to
provide all the auxiliary parameters required by @exam[Do_That] even though we
must already have instantiated it elsewhere in the program. Suppose the
instantiation was
@begin[Example]
@key[package] That_One @key[is new] Do_That(Float, 0.01, 7, False);
@end[Example]

@leading@;and suppose also that we have instantiated @exam[Do_This]
@begin[Example]
@key[package] This_One @key[is] @key[new] Do_This( ... );
@end[Example]

@leading@;Now when we instantiate @exam[Super_Solver] we have to write
@begin[Example]
@key[package] SS @key[is] @key[new] Super_Solver(This_One, 0.01, 7, False, That_One);
@end[Example]

Just as with @exam[HMV] we have all these duplicated parameters and
an artificial asymmetry between @exam[This] and @exam[That].

@leading@;In Ada 2005 the package @exam[Super_Solver] can be written as
@begin[Example]
@key[generic]
   @key[with package] This @key[is new] Do_This(<>);
   @key[with package] That @key[is new] Do_That(This.Real, @key[others] => <>);
@key[package] Super_Solver @key[is] ...
@end[Example]

@leading@;and the instantiation of @exam[Super_Solver] becomes simply

@begin[Example]
@key[package] SS @key[is new] Super_Solver(This_One, That_One);
@end[Example]

@leading@;Other examples occur with signature packages. Remember that a
signature package is one without a specification. It can be used to ensure that
a group of entities are related in the correct way and an instantiation can
then be used to identify the group as a whole. A trivial example might
be@Defn{signature package}
@begin[Example]
@key[generic]
   @key[type] Index @key[is] (<>);
   @key[type] item @key[is private];
   @key[type] Vec @key[is array] (Index @key[range] <>) @key[of] Item;
@key[package] General_Vector @key[is end];
@end[Example]

@leading@;An instantiation of @exam[General_Vector] just asserts that the three
types concerned have the appropriate relationship. Thus we might have
@begin[Example]
@key[type] My_Array @key[is array] (Integer @key[range] <>) @key[of] Float;
@end[Example]

@leading@keepnext@;and then
@begin[Example]
@key[package] Vector @key[is] @key[new] General_Vector(Integer, Float, My_Array);
@end[Example]

The package @exam[General_Vector] could then be used as a parameter
of other packages thereby reducing the number of parameters.

@leading@;Another example might be the signature of a package for manipulating
sets. Thus
@begin[Example]
@key[generic]
   @key[type] Element i@key[s private];
   @key[type] Set @key[is private];
   @key[with function] Empty @key[return] Set;
   @key[with function] Unit(E: Element) @key[return] Set;
   @key[with function] Union(S, T: Set) @key[return] Set;
   @key[with function] Intersection(S, T: Set) @key[return] Set;
   ...
@key[package] Set_Signature @key[is end];
@end[Example]

We might then have some other generic package which takes an instantiation
of this set signature. However, it is likely that we would need to
specify the type of the elements but possibly not the set type and
certainly not all the operations. So typically we would have
@begin[Example]
@key[generic]
   @key[type] My_Element @key[is private];
   @key[with package] Sets @key[is new] Set_Signature(Element => My_Element, @key[others] => <>);
@end[Example]

An example of this technique occurred when considering the possibility
of including a system of units facility within Ada 2005. Although
it was considered not appropriate to include it, the use of signature
packages was almost essential to make the mechanism usable. The interested
reader should consult @AILink{AI=[AI95-00324-01],Text=[AI-324]}.

@leading@;We conclude by noting a small change to the syntax of a subprogram
instantiation in that an overriding indicator can be supplied as mentioned
in @RefSecNum{Overriding and overloading}. Thus (in appropriate
circumstances) we can write@Defn{overriding indicator}
@begin[Example]
@key[overriding]
@key[procedure] This @key[is new] That( ... );
@end[Example]

This means that the instantiation must be an overriding operation
for some type.
