@Part(13, Root="ada.mss")

@Comment{$Date: 2000/05/17 00:17:44 $}
@LabeledSection{Representation Issues}

@Comment{$Source: e:\\cvsroot/ARM/Source/13a.mss,v $}
@Comment{$Revision: 1.11 $}

@begin{Intro}
@redundant[
This section describes features for
querying and controlling aspects of representation
and for interfacing to hardware.

]
@end{Intro}

@begin{DiffWord83}
The clauses of this section have been reorganized.
This was necessary to preserve a logical order,
given the new Ada 9X semantics given in this section.
@end{DiffWord83}

@LabeledClause{Representation Items}

@begin{Intro}
@Defn{representation item}
@RootDefn{representation pragma}
@RootDefn{pragma, representation}
There are three kinds of @i{representation items}:
@nt{representation_clause}s,
@nt<component_clause>s, and @i{representation pragmas}.
@Redundant[Representation items specify how the types and other entities of
the language are to be mapped onto the underlying machine.
They can be provided to give more efficient representation or to
interface with features that are outside the domain of the language
(for example, peripheral hardware).
Representation items also specify other specifiable
properties of entities.
A representation item applies to an entity identified by
a @nt<local_name>, which denotes an entity declared local to the
current declarative region, or a library unit declared immediately
preceding a representation pragma in a @nt<compilation>.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<representation_clause>,rhs="@Syn2{attribute_definition_clause}
      | @Syn2{enumeration_representation_clause}
      | @Syn2{record_representation_clause}
      | @Syn2{at_clause}"}

@Syn{lhs=<local_name>,rhs="@Syn2{direct_name}
      | @Syn2{direct_name}@SingleQuote@Syn2{attribute_designator}
      | @SynI{library_unit_}@Syn2{name}"}

@begin{SyntaxText}
A representation pragma is allowed only at places where a
@nt{representation_clause}
or @nt{compilation_unit} is allowed.
@end{SyntaxText}
@end{Syntax}

@begin{Resolution}
In a representation item,
if the @nt<local_name> is a @nt<direct_name>, then it shall
resolve to denote a declaration
(or, in the case of a @nt{pragma}, one or more declarations)
that occurs immediately within the same
@nt{declarative_region} as the representation item.
If the @nt<local_name> has an @nt<attribute_designator>, then it shall
resolve to denote an implementation-defined
component (see @RefSecNum{Record Representation Clauses})
or a class-wide
type implicitly declared immediately within the same
@nt<declarative_region> as the representation item.
A @nt<local_name> that is a @i{library_unit_}@nt<name> (only
permitted in a representation pragma) shall resolve
to denote the @nt<library_item> that immediately precedes
(except for other pragmas) the representation pragma.
@begin{Reason}
This is a @ResolutionName,
because we don't want a representation item for X to be ambiguous
just because there's another X declared in an outer
declarative region.
It doesn't make much difference, since most
representation items are for types or subtypes, and type and
subtype names can't be overloaded.
@end{Reason}
@begin{Ramification}
The visibility rules imply that the declaration has to occur
before the representation item.

For objects, this implies that representation items can be
applied only to stand-alone objects.
@end{Ramification}
@end{Resolution}

@begin{Legality}
The @nt{local_name} of a @nt<representation_clause> or representation
pragma shall
statically denote an entity
(or, in the case of a @nt{pragma}, one or more entities)
declared immediately preceding it in a @nt<compilation>,
or within the same
@nt{declarative_part}, @nt{package_specification}, @nt{task_definition},
@nt{protected_definition}, or @nt{record_definition}
as the representation item.
If a @nt<local_name> denotes a @Redundant[local] callable entity,
it may do so through a @Redundant[local]
@nt<subprogram_renaming_declaration>
@Redundant[(as a way to resolve ambiguity in the presence of overloading)];
otherwise, the @nt<local_name> shall not denote a @nt<renaming_declaration>.
@begin{Ramification}
The ``statically denote'' part
implies that it is impossible to specify the representation of
an object that is not a stand-alone object,
except in the case of a representation
item like pragma Atomic
that is allowed inside a @nt{component_list}
(in which case the representation item specifies
the representation of components of all objects of the type).
It also prevents the problem of
renamings of things like ``P.@key[all]''
(where P is an access-to-subprogram value)
or ``E(I)'' (where E is an entry family).

The part about where the denoted entity has to have been
declared appears twice @em once as a @ResolutionName,
and once as a @LegalityName.
Suppose P renames Q,
and we have a representation item in a @nt{declarative_part}
whose @nt<local_name> is P.
The fact that the representation item has to appear in the same
@nt{declarative_part} as P is a @ResolutionName,
whereas the fact that the representation item has to appear in the
same @nt{declarative_part} as Q is a @LegalityName.
This is subtle, but it seems like the least confusing set of rules.
@end{Ramification}
@begin{Discussion}
  A separate @LegalityName applies for @nt<component_clause>s.
  See @RefSec{Record Representation Clauses}.
@end{Discussion}

@Defn{representation of an object}
@Defn2{Term=size, Sec=(of an object)}The @i{representation} of an
object consists of a certain
number of bits (the @i(size) of
the object).
These are the bits that are normally read
or updated by the machine code
when loading, storing, or operating-on the value of the object.
This includes some padding bits, when the size of the object
is greater than the size of its subtype.
@Defn{gaps}
@Defn{padding bits}
Such padding bits are considered to be part of the representation of the
object, rather than being gaps between objects,
if these bits are normally read and updated.
@begin{Honest}
@PDefn{contiguous representation}
@PDefn{discontiguous representation}
Discontiguous representations are allowed,
but the ones we're interested in here are generally contiguous
sequences of bits.
@end{Honest}
@begin{Ramification}
Two objects with the same value do not necessarily have the same
representation.
For example, an implementation might represent False as zero and True
as any odd value.
Similarly, two objects (of the same type)
with the same sequence of bits do not necessarily have the
same value.
For example, an implementation might use a biased representation in
some cases but not others:
@begin{Example}
@key[subtype] S @key[is] Integer @key[range] 1..256;
@key[type] A @key[is] @key[array](Natural @key[range] 1..4) @key[of] S;
@key[pragma] Pack(A);
X : S := 3;
Y : A := (1, 2, 3, 4);
@end{Example}

The implementation might use a biased-by-1 representation for the
array elements, but not for X.
X and Y(3) have the same value, but different representation:
the representation of X is a sequence of (say) 32 bits: 0...011,
whereas the representation of Y(3) is a sequence of 8 bits:
00000010 (assuming a two's complement representation).

Such tricks are not required, but are allowed.
@end{Ramification}
@begin{Discussion}
  The value of any padding bits is not specified by the language,
  though for a numeric type, it will be much harder to properly implement
  the predefined operations if the padding bits are not either all zero,
  or a sign extension.
@end{Discussion}
@begin{Ramification}
For example, suppose S'Size = 2, and an object X is of subtype S.
If the machine code typically uses a 32-bit load instruction to load the
value of X, then X'Size should be 32, even though 30 bits of the value
are just zeros or sign-extension bits.
On the other hand, if the machine code typically masks out those 30
bits, then X'Size should be 2.
Usually, such masking only happens for components of a composite type
for which packing, Component_Size, or record layout is specified.

Note, however, that the formal parameter of an instance of
Unchecked_Conversion is a special case.
Its Size is required to be the same as that of its subtype.

Note that we don't generally talk about the representation of a
value.
A value is considered to be an amorphous blob
without any particular representation.
An object is considered to be more concrete.
@end{Ramification}

@RootDefn{aspect of representation}
@Defn{representation aspect}
@Defn2{Term=[directly specified],
  Sec=(of an aspect of representation of an entity)}
A representation item @i{directly specifies}
an @i{aspect of representation} of the entity denoted
by the @nt{local_name},
except in the case of a type-related representation item,
whose @nt{local_name} shall denote a first subtype,
and which directly specifies an aspect
of the subtype's type.
@RootDefn2{Term=[type-related], Sec=(representation item)}
@RootDefn2{term=[subtype-specific], Sec=(of a representation item)}
@RootDefn2{Term=[type-related], Sec=(aspect)}
@RootDefn2{term=[subtype-specific], Sec=(of an aspect)}
A representation item that names a subtype is either
@i(subtype-specific) (Size and Alignment clauses)
or @i{type-related} (all others).
@Redundant[Subtype-specific aspects may differ for
different subtypes of the same type.]
@begin{Honest}
@i{Type-related} and @i{subtype-specific} are defined likewise for
the corresponding aspects of representation.
@end{Honest}
@begin{Honest}
Some representation items directly specify more than one aspect.
@end{Honest}
@begin{Discussion}
For example, a @nt{pragma} Export specifies the convention
of an entity,
and also specifies that it is exported.
@end{Discussion}
@begin{Ramification}
Each specifiable attribute constitutes a separate aspect.
An @nt{enumeration_representation_clause} specifies the coding aspect.
A @nt{record_representation_clause} (without the @nt{mod_clause})
specifies the record layout aspect.
Each representation pragma specifies a separate aspect.
@end{Ramification}
@begin{Reason}
We don't need to say that an @nt{at_clause} or a
@nt{mod_clause} specify separate aspects,
because these are equivalent to @nt{attribute_definition_clause}s.
See @RefSec{At Clauses}, and @RefSec{Mod Clauses}.
@end{Reason}
@begin{Ramification}
The following representation items are type-related:
@begin{Itemize}
@nt{enumeration_representation_clause}

@nt{record_representation_clause}

Component_Size clause

External_Tag clause

Small clause

Bit_Order clause

Storage_Pool clause

Storage_Size clause

Read clause

Write clause

Input clause

Output clause

Machine_Radix clause

pragma Pack

pragmas Import, Export, and Convention (when applied to a type)

pragmas Atomic and Volatile (when applied to a type)

pragmas Atomic_Components and Volatile_Components (when applied to an array type)

pragma Discard_Names (when applied to an enumeration or tagged type)
@end{Itemize}

The following representation items are subtype-specific:
@begin{Itemize}
Alignment clause (when applied to a first subtype)

Size clause (when applied to a first subtype)
@end{Itemize}

The following representation items do not apply to subtypes,
so they are neither type-related nor subtype-specific:
@begin{Itemize}
Address clause (applies to objects and program units)

Alignment clause (when applied to an object)

Size clause (when applied to an object)

pragmas Import, Export, and Convention (when applied to anything other
than a type)

pragmas Atomic and Volatile (when applied to an object or a component)

pragmas Atomic_Components and Volatile_Components (when applied to an
array object)

pragma Discard_Names (when applied to an exception)

pragma Asynchronous (applies to procedures)
@end{Itemize}
@end{Ramification}

A representation item that directly specifies an aspect of a subtype or
type shall appear after the type is completely
defined
(see @RefSecNum{Completions of Declarations}),
and before the subtype or type is frozen
(see @RefSecNum{Freezing Rules}).
If a representation item is given that directly specifies an aspect of an
entity, then it is illegal to give another representation item that
directly specifies the same aspect of the entity.
@begin{Ramification}
The fact that
a representation item that directly specifies an aspect of an entity
is required to appear before the entity is frozen
prevents changing the representation of an entity
after using the entity in ways that require the
representation to be known.
@end{Ramification}


For an untagged derived type,
no type-related representation items
are allowed if the parent type is a by-reference type,
or has any user-defined primitive subprograms.
@begin{Ramification}
  On the other hand,
  subtype-specific representation items may be given for the first
  subtype of such a type.
@end{Ramification}
@begin{Reason}
  The reason for forbidding type-related representation items on
  untagged by-reference types is because a change of representation
  is impossible when passing by reference (to an inherited subprogram).
  The reason for forbidding type-related representation items on
  untagged types with user-defined primitive subprograms
  was to prevent implicit change of representation for type-related
  aspects of representation upon calling inherited subprograms,
  because such changes of representation are likely to be
  expensive at run time.
  Changes of subtype-specific representation attributes, however, are
  likely to be cheap.
  This rule is not needed for tagged types,
  because other rules prevent a type-related representation item
  from changing the representation of the parent part;
  we want to allow a type-related representation item on a type extension
  to specify aspects of the extension part.
  For example, a @nt{pragma} Pack will cause packing of the extension
  part, but not of the parent part.
@end{Reason}

Representation aspects of a generic formal parameter are the same as
those of the actual.
A type-related representation item is not allowed for a
descendant of a generic formal untagged type.
@begin{Ramification}
Representation items are allowed for types whose subcomponent types
or index subtypes are generic formal types.
@end{Ramification}
@begin{Reason}
Since it is not known whether a formal type has
user-defined primitive subprograms, specifying
type-related representation items for them
is not allowed, unless they are tagged (in which case only
the extension part is affected in any case).
@end{Reason}

A representation item that specifies the Size for a given subtype,
or the size or storage place for an object (including a component)
of a given subtype, shall allow for enough storage space to
accommodate any value of the subtype.

A representation item that is not supported
by the implementation is illegal, or raises an exception
at run time.
@end{Legality}

@begin{StaticSem}
If two subtypes statically match,
then their subtype-specific aspects (Size and Alignment)
are the same.
@PDefn2{Term=[statically matching],Sec=(effect on subtype-specific aspects)}
@begin{Reason}
This is necessary because we allow (for example)
conversion between access types whose designated subtypes
statically match.
Note that it is illegal to specify an aspect
(including a subtype-specific one)
for a nonfirst subtype.

Consider, for example:
@begin{Example}
@key[package] P1 @key[is]
    @key[subtype] S1 @key[is] Integer @key[range] 0..2**16-1;
    @key[for] S1'Size @key[use] 16; --@i{ Illegal!}
        --@i{ S1'Size would be 16 by default.}
    @key[type] A1 @key[is] @key[access] S1;
    X1: A1;
@key[end] P1;

@key[package] P2 @key[is]
    @key[subtype] S2 @key[is] Integer @key[range] 0..2**16-1;
    @key[for] S2'Size @key[use] 32; --@i{ Illegal!}
    @key[type] A2 @key[is] @key[access] S2;
    X2: A2;
@key[end] P2;

@key[procedure] Q @key[is]
    @key[use] P1, P2;
    @key[type] Array1 @key[is] @key[array](Integer @key[range] <>) @key[of] @key[aliased] S1;
    @key[pragma] Pack(Array1);
    Obj1: Array1(1..100);
    @key[type] Array2 @key[is] @key[array](Integer @key[range] <>) @key[of] @key[aliased] S2;
    @key[pragma] Pack(Array2);
    Obj2: Array2(1..100);
@key[begin]
    X1 := Obj2(17)'Access;
    X2 := Obj1(17)'Access;
@key[end] Q;
@end{Example}

Loads and stores through X1 would read and write 16 bits,
but X1 points to a 32-bit location.
Depending on the endianness of the machine,
loads might load the wrong 16 bits.
Stores would fail to zero the other half in any case.

Loads and stores through X2 would read and write 32 bits,
but X2 points to a 16-bit location.
Thus, adjacent memory locations would be trashed.

Hence, the above is illegal.
Furthermore, the compiler is forbidden from choosing different
Sizes by default, for the same reason.

The same issues apply to Alignment.

@end{Reason}

A derived type inherits each type-related aspect of its parent type that
was directly specified before the declaration of the derived type,
or (in the case where the parent is derived)
that was inherited by the parent type from the grandparent type.
A derived subtype inherits each subtype-specific aspect of its parent subtype that
was directly specified before the declaration of the derived type,
or (in the case where the parent is derived)
that was inherited by the parent subtype from the grandparent subtype,
but only if the parent subtype statically matches the first subtype of
the parent type.
An inherited aspect of representation is overridden by a subsequent
representation item that specifies the same aspect of the type or
subtype.
@begin{Honest}
A @nt{record_representation_clause} for a record extension
does not override the layout of the parent part;
if the layout was specified for the parent type,
it is inherited by the record extension.
@end{Honest}
@begin{Ramification}
If a representation item for the parent appears after the
@nt{derived_type_declaration},
then inheritance does not happen for that representation item.
@end{Ramification}


Each aspect of representation of an entity is as follows:
@begin{Itemize}
@Defn2{Term=[specified],
  Sec=(of an aspect of representation of an entity)}
If the aspect is @i{specified} for the entity,
meaning that it is
either directly specified or inherited,
then that aspect of the entity is as specified,
except in the case of Storage_Size,
which specifies a minimum.
@begin{Ramification}
This rule implies that queries of the aspect return the
specified value.  For example, if the user writes ``@key{for}
X'Size @key{use} 32;'',
then a query of X'Size will return 32.
@end{Ramification}

@PDefn{unspecified}
If an aspect of representation of an entity is not specified,
it is chosen by default in an unspecified manner.
@end{Itemize}
@begin{Ramification}
Note that @nt{representation_clause}s can affect the
semantics of the entity.

The rules forbid things like
``@key[for] S'Base'Alignment @key[use] ...''
and
``@key[for] S'Base @key[use] record ...''.
@end{Ramification}
@begin{Discussion}
The intent is that implementations will represent the
components of a composite value in the same way for all subtypes of a
given composite type.
Hence, Component_Size and record layout are type-related aspects.
@end{Discussion}
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(representation_clause)}
For the elaboration of a @nt{representation_clause},
any evaluable constructs within it are evaluated.
@begin{Ramification}
Elaboration of representation pragmas is covered by the
general rules for pragmas in Section 2.
@end{Ramification}
@end{RunTime}

@begin{ImplPerm}
An implementation may interpret aspects of representation in an
implementation-defined manner.
An implementation may place implementation-defined restrictions on
representation items.
@RootDefn{recommended level of support}
A @i{recommended level of support} is specified for representation items
and related features in each subclause.
These recommendations are changed to requirements
for implementations that support the Systems Programming Annex
(see @RefSec{Required Representation Support}).
@ImplDef{The interpretation of each aspect of representation.}
@ImplDef{Any restrictions placed upon representation items.}
@begin{Ramification}
Implementation-defined restrictions may be enforced either at compile
time or at run time.
There is no requirement that an implementation justify any such
restrictions.
They can be based on avoiding implementation complexity,
or on avoiding excessive inefficiency, for example.
@end{Ramification}
@end{ImplPerm}

@begin{ImplAdvice}
@PDefn2{Term=[recommended level of support], Sec=(with respect to
nonstatic expressions)}
The recommended level of support for all representation items
is qualified as follows:
@begin{Itemize}
An implementation need not support representation items containing
nonstatic expressions,
except that an implementation should support a representation item
for a given entity
if each nonstatic expression in the representation item
is a name that statically denotes a constant declared
before the entity.
@begin{Reason}
This is to avoid the following sort of thing:
@begin{Example}
X : Integer := F(...);
Y : Address := G(...);
@key[for] X'Address @key[use] Y;
@end{Example}

In the above, we have to evaluate the initialization expression for X
before we know where to put the result.
This seems like an unreasonable implementation burden.

The above code should instead be written like this:
@begin{Example}
Y : @key[constant] Address := G(...);
X : Integer := F(...);
@key[for] X'Address @key[use] Y;
@end{Example}

This allows the expression ``Y'' to be safely evaluated before X is
created.

The constant could be a formal parameter of mode @key[in].

An implementation can support other nonstatic expressions if it wants
to.  Expressions of type Address are hardly ever static,
but their value might be known at compile time anyway
in many cases.
@end{Reason}

An implementation need not support a specification for the
Size for a given composite subtype, nor the size or storage place for an object
(including a component) of a given composite subtype, unless the constraints
on the subtype and its composite subcomponents (if any)
are all static constraints.

An aliased component, or a component whose type is by-reference,
should always be allocated at an addressable location.
@begin{Reason}
The intent is that access types, type System.Address,
and the pointer used for a by-reference parameter should
be implementable as a single machine address @em bit-field pointers
should not be required.
(There is no requirement that this implementation be used @em we just
want to make sure its feasible.)
@end{Reason}
@begin{ImplNote}
Note that the above rule does not apply to types that merely allow
by-reference parameter passing;
for such types, a copy typically needs to be made at the call site
when a bit-aligned component is passed as a parameter.
@end{ImplNote}
@begin{Ramification}
A pragma Pack will typically not pack so tightly as to disobey the
above rule.
A Component_Size clause or @nt{record_representation_clause} will
typically by illegal if it disobeys the above rule.
Atomic components have similar restrictions
(see @RefSec{Shared Variable Control}).
@end{Ramification}
@end{Itemize}
@end{ImplAdvice}

@begin{Incompatible83}
It is now illegal for a representation item to cause a derived
by-reference type to have a different record layout from its
parent.
This is necessary for by-reference parameter passing to be feasible.
This only affects programs that specify the representation of types
derived from types containing tasks;
most by-reference types are new to Ada 9X.
For example, if A1 is an array of tasks, and A2 is derived from A1,
it is illegal to apply a @nt{pragma} Pack to A2.
@end{Incompatible83}

@begin{Extend83}
Ada 9X allows additional @nt{representation_clause}s for
objects.

@end{Extend83}

@begin{DiffWord83}
The syntax rule for @nt{type_representation_clause} is removed;
the right-hand side of that rule is moved up to where it was used,
in @nt{representation_clause}.
There are two references to ``type representation clause'' in RM83,
both in Section 13;
these have been reworded.

We have defined a new term ``representation item,''
which includes both @nt{representation_clause}s
and representation pragmas, as well as @nt<component_clause>s.
This is convenient because the rules are almost identical for all
three.

All of the forcing occurrence stuff has been moved into its own
subclause (see @RefSecNum{Freezing Rules}),
and rewritten to use the term ``freezing''.

RM83-13.1(10) requires implementation-defined restrictions on
representation items to be enforced at compile time.
However, that is impossible in some cases.
If the user specifies a junk (nonstatic) address in an address
clause, and the implementation chooses to detect the error (for example,
using hardware memory management with protected pages), then it's
clearly going to be a run-time error.
It seems silly to call that ``semantics'' rather than ``a restriction.''

RM83-13.1(10) tries to pretend that @nt{representation_clause}s don't affect
the semantics of the program.
One counter-example is the Small clause.
Ada 9X has more counter-examples.
We have noted the opposite above.

Some of the more stringent requirements are moved to
@RefSec{Required Representation Support}.
@end{DiffWord83}

@LabeledClause{Pragma Pack}

@begin{Intro}
@redundant[
A @nt{pragma} Pack specifies that storage
minimization should be the main criterion when
selecting the representation of a composite type.
]
@end{Intro}

@begin{Syntax}
@begin{SyntaxText}
The form of a @nt{pragma} Pack is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(Pack)(@SynI{first_subtype_}@Syn2{local_name});'
@end{Syntax}

@begin{Legality}
The @SynI{first_subtype_}@nt{local_name} of a @nt{pragma} Pack
shall denote a composite subtype.
@end{Legality}

@begin{StaticSem}
@PDefn2{Term=[representation pragma], Sec=(Pack)}
@PDefn2{Term=[pragma, representation], Sec=(Pack)}
@PDefn2{Term=[aspect of representation], Sec=(packing)}
@Defn2{Term=[packing], Sec=(aspect of representation)}
@Defn{packed}
A @nt{pragma} Pack specifies the @i{packing} aspect of representation;
the type (or the extension part) is said to be @i{packed}.
For a type extension, the parent part is packed as for the parent
type, and a @nt{pragma} Pack causes packing only of the extension part.
@begin{Ramification}
The only high level semantic effect of a @nt{pragma} Pack
is independent addressability (see @RefSec{Shared Variables}).
@end{Ramification}
@end{StaticSem}

@begin{ImplAdvice}
If a type is packed, then the implementation should try to minimize
storage allocated to objects of the type,
possibly at the expense of speed of accessing components,
subject to reasonable complexity in addressing calculations.
@begin{Ramification}
A @nt{pragma} Pack is for gaining space efficiency,
possibly at the expense of time.
If more explicit control over representation is desired,
then a @nt{record_representation_clause},
a Component_Size clause,
or a Size clause should be used instead of,
or in addition to,
a @nt{pragma} Pack.
@end{Ramification}

@PDefn2{Term=[recommended level of support], Sec=(pragma Pack)}
The recommended level of support for pragma Pack is:
@begin{Itemize}
For a packed record type,
the components should be packed as tightly as possible
subject to the Sizes of the component subtypes,
and subject to any @nt{record_representation_clause} that applies to
the type; the implementation may, but need not, reorder components
or cross aligned word boundaries to improve the packing.
A component whose Size is greater than the word size
may be allocated an integral number of words.
@begin{Ramification}
The implementation can always allocate an integral number of
words for a component that will not fit in a word.
The rule also allows small component sizes to be rounded up if such
rounding does not waste space.
For example, if Storage_Unit = 8, then a component of size 8 is
probably more efficient than a component of size 7 plus a 1-bit gap
(assuming the gap is needed anyway).
@end{Ramification}

For a packed array type, if the component subtype's Size is less
than or equal to the word size, and Component_Size is not
specified for the type, Component_Size should be less than or
equal to the Size of the component subtype, rounded up to the nearest
factor of the word size.
@begin{Ramification}
If a component subtype is aliased,
its Size will generally be a multiple of Storage_Unit,
so it probably won't get packed very tightly.
@end{Ramification}
@end{Itemize}
@end{ImplAdvice}

@LabeledClause{Representation Attributes}

@begin{Intro}
@redundant[
@Defn{representation attribute}
@Defn2{Term=[attribute], Sec=(representation)}
The values of certain implementation-dependent characteristics can be
obtained by interrogating appropriate representation attributes.
@RootDefn2{Term=[attribute], Sec=(specifying)}
Some of these attributes are specifiable via an
@nt{attribute_definition_clause}.
]
@end{Intro}

@begin{MetaRules}
In general, the meaning of a given attribute should not depend on
whether the attribute was specified via an
@nt{attribute_definition_clause},
or chosen by default by the implementation.
@end{MetaRules}

@begin{Syntax}
@Syn{lhs=<attribute_definition_clause>,rhs="
      @key{for} @Syn2{local_name}@SingleQuote@Syn2{attribute_designator} @key{use} @Syn2{expression};
    | @key{for} @Syn2{local_name}@SingleQuote@Syn2{attribute_designator} @key{use} @Syn2{name};"}
@end{Syntax}

@begin{Resolution}
For an @nt{attribute_definition_clause} that specifies
an attribute that denotes a value,
the form with an @nt{expression} shall be used.
Otherwise, the form with a @nt{name} shall be used.

@PDefn2{Term=[expected type],
  Sec=(attribute_definition_clause expression or name)}
For an @nt{attribute_definition_clause} that specifies
an attribute that denotes a value or an object,
the expected type for the expression or @nt{name}
is that of the attribute.
@begin{Ramification}
For example, the Size attribute is of type @i{universal_integer}.
Therefore, the expected type for Y in ``@key[for] X'Size @key[use] Y;'' is
@i{universal_integer},
which means that Y can be of any integer type.
@end{Ramification}
@PDefn2{Term=[expected profile],
  Sec=(attribute_definition_clause name)}
For an @nt{attribute_definition_clause} that specifies
an attribute that denotes a subprogram,
the expected profile for the @nt{name}
is the profile required for the attribute.
@begin{Discussion}
The required profile is indicated separately for the individual
attributes.
@end{Discussion}
For an @nt{attribute_definition_clause} that specifies
an attribute that denotes some other kind of entity,
the @nt{name} shall resolve to denote an entity of the appropriate
kind.
@begin{Ramification}
For an @nt{attribute_definition_clause} with a @nt{name},
the @nt{name} need not statically denote the entity it denotes.
For example, the following kinds of things are allowed:
@begin{Example}
@key[for] Some_Access_Type'Storage_Pool @key[use] Storage_Pool_Array(I);
@key[for] Some_Type'Read @key[use] Subprogram_Pointer.@key[all];
@end{Example}
@end{Ramification}
@end{Resolution}

@begin{Legality}
@RootDefn{specifiable (of an attribute and for an entity)}
@RootDefn2{Term=[attribute], Sec=(specifiable)}
An @nt{attribute_designator} is allowed in an
@nt{attribute_definition_clause} only if this International Standard
explicitly allows it,
or for an implementation-defined attribute
if the implementation allows it.
@PDefn2{Term=[aspect of representation], Sec=(specifiable attributes)}
Each specifiable attribute constitutes an aspect of representation.
@begin{Discussion}
For each specifiable attribute,
we generally say something like,
``The ... attribute may be specified for ... via
an @nt{attribute_definition_clause}.''

The above wording allows for
T'Class'Alignment, T'Class'Size, T'Class'Input, and T'Class'Output
to be specifiable.

A specifiable attribute is not necessarily
specifiable for all entities for which it is defined.
For example, one is allowed to ask T'Component_Size for an array
subtype T, but ``@key[for] T'Component_Size @key[use] ...''
is only allowed if T is a first subtype,
because Component_Size is a type-related aspect.
@end{Discussion}

For an @nt{attribute_definition_clause} that specifies
an attribute that denotes a subprogram,
the profile shall be mode conformant with the one
required for the attribute,
and the convention shall be Ada.
Additional requirements are defined for particular attributes.
@Defn2{Term=[subtype conformance],Sec=(required)}
@begin{Ramification}
This implies, for example, that if one writes:
@begin{Example}
@key[for] T'Read @key[use] R;
@end{Example}

R has to be a procedure with two parameters with the appropriate
subtypes and modes as shown in
@RefSecNum{Stream-Oriented Attributes}.
@end{Ramification}
@end{Legality}

@begin{StaticSem}
@Defn{Address clause}
@Defn{Alignment clause}
@Defn{Size clause}
@Defn{Component_Size clause}
@Defn{External_Tag clause}
@Defn{Small clause}
@Defn{Bit_Order clause}
@Defn{Storage_Pool clause}
@Defn{Storage_Size clause}
@Defn{Read clause}
@Defn{Write clause}
@Defn{Input clause}
@Defn{Output clause}
@Defn{Machine_Radix clause}
A @i{Size clause} is an @nt{attribute_definition_clause} whose
@nt{attribute_designator} is Size.
Similar definitions apply to the other specifiable attributes.
@begin{Honest}
@PDefn2{Term=[type-related], Sec=(attribute_definition_clause)}
@PDefn2{Term=[subtype-specific], Sec=(attribute_definition_clause)}
An @nt{attribute_definition_clause}
is type-related or subtype-specific if the @nt{attribute_designator}
denotes a type-related or subtype-specific attribute, respectively.
@end{Honest}

@Defn{storage element}
@IndexSee{Term=[byte],See=(storage element)}
A @i{storage element} is
an addressable element of storage in the machine.
@Defn{word}
A @i{word} is the largest amount of storage that can be conveniently and
efficiently manipulated by the hardware,
given the implementation's run-time model.
A word consists of an integral number of storage elements.
@begin{Discussion}
A storage element is not intended to be a single bit,
unless the machine can efficiently address individual bits.
@end{Discussion}
@begin{Ramification}
For example, on a machine with 8-bit storage elements,
if there exist 32-bit integer registers,
with a full set of arithmetic and logical instructions to manipulate those
registers, a word ought to be 4 storage elements @em that is, 32 bits.
@end{Ramification}
@begin{Discussion}
The ``given the implementation's run-time model'' part is
intended to imply that, for example, on an 80386 running MS-DOS,
the word might be 16 bits, even though the hardware can support 32
bits.

A word is what ACID refers to as a ``natural hardware boundary''.

Storage elements may, but need not be, independently addressable
(see @RefSec{Shared Variables}).
Words are expected to be independently addressable.
@end{Discussion}

The following attributes are defined:

For @PrefixType{a prefix X that denotes an object, program unit,
or label}:
@begin{Description}
@Attribute{Prefix=<X>, AttrName=<Address>,
  Text=<Denotes the address of the first of the storage elements
allocated to X.  For a program unit or
label, this value refers to the machine code associated with
the corresponding body or @nt{statement}.
The value of this attribute is of type System.Address.>}
@begin{Ramification}

  Here, the ``first of the storage elements'' is intended to mean
  the one with the lowest address;
  the endianness of the machine doesn't matter.

@end{Ramification}

@PDefn2{Term=[specifiable], Sec=(of Address for stand-alone
objects and for program units)}
@Defn{Address clause}
@NoPrefix@;Address may be specified for @Redundant[stand-alone] objects
and for program units
via an @nt{attribute_definition_clause}.
  @begin{Ramification}
  Address is not allowed for enumeration literals,
  predefined operators, derived task types,
  or derived protected types,
  since they are not program units.

  The validity of a given address depends on the run-time model;
  thus, in order to use Address clauses correctly,
  one needs intimate knowledge of the run-time model.

  If the Address of an object is specified,
  any explicit or implicit initialization takes place as usual,
  unless a @nt{pragma} Import is also specified for the object
  (in which case any necessary initialization is presumably
  done in the foreign language).

  Any compilation unit containing an @nt<attribute_reference> of
  a given type depends semantically on the declaration of the package
  in which the type is declared, even if not mentioned
  in an applicable @nt<with_clause>
  @em see @RefSecNum{Compilation Units - Library Units}.
  In this case, it means that if a compilation unit contains
  X'Address, then it depends on the declaration of System.
  Otherwise, the fact that the value of Address is of
  a type in System wouldn't make sense;
  it would violate the ``legality determinable via semantic
  dependences'' @MetaRulesName.

  AI-00305 @em If X is a task type,
  then within the body of X,
  X denotes the current task object;
  thus, X'Address denotes the object's address.

  Interrupt entries and their addresses are
  described in @RefSec{Interrupt Entries}.

  If X is not allocated on a storage element boundary,
  X'Address points at the first of the storage elements
  that contains any part of X.
  This is important for the definition of the Position
  attribute to be sensible.
  @end{Ramification}
@end{Description}
@EndPrefixType{}
@end{StaticSem}

@begin{Erron}
If an Address is specified,
it is the programmer's responsibility to ensure that
the address is valid;
otherwise, program execution is erroneous.
@end{Erron}

@begin{ImplAdvice}
For an array X, X'Address should point at the first component of the
array, and not at the array bounds.
@begin{Ramification}
On the other hand, we have no advice to offer about
discriminants and tag fields;
whether or not the address points at them is
not specified by the language.
If discriminants are stored separately,
then the Position of a discriminant might be negative,
or might raise an exception.
@end{Ramification}

@PDefn2{Term=[recommended level of support], Sec=(Address attribute)}
The recommended level of support for the Address attribute is:
@begin{Itemize}
X'Address should produce a useful result if X is an
object that is aliased or of a by-reference
type, or is an entity whose Address has been specified.
@begin{Reason}
  Aliased objects are the ones for which the
  Unchecked_Access attribute is allowed;
  hence, these have to be allocated on an addressable
  boundary anyway.  Similar considerations apply to objects
  of a by-reference type.

  An implementation need not go to any trouble
  to make Address work in other cases.
  For example, if an object X is not aliased and not of a by-reference type,
  and the implementation chooses to store it in a register,
  X'Address might return System.Null_Address
  (assuming registers are not addressable).
  For a subprogram whose calling convention is Intrinsic,
  or for a package,
  the implementation need not generate an out-of-line
  piece of code for it.
@end{Reason}

An implementation should support Address clauses for
imported subprograms.

Objects (including subcomponents) that are aliased or
of a by-reference type
should be allocated on storage element boundaries.
@begin{Reason}
This is necessary for the Address attribute to be useful
(since First_Bit and Last_Bit apply only to components).
Implementations generally need to do this anyway,
for tasking to work properly.
@end{Reason}

If the Address of an object is specified,
or it is imported or exported,
then the implementation should not perform optimizations based on
assumptions of no aliases.
@end{Itemize}
@end{ImplAdvice}

@begin{Notes}
The specification of a link name in a @nt{pragma} Export
(see @RefSecNum{Interfacing Pragmas})
for a subprogram or object is an alternative to explicit
specification of its link-time address, allowing a link-time directive
to place the subprogram or object within memory.

The rules for the Size attribute imply,
for an aliased object X, that if X'Size = Storage_Unit,
then X'Address points at a storage element containing all
of the bits of X, and only the bits of X.
@end{Notes}

@begin{DiffWord83}
The intended meaning of the various attributes,
and their @nt{attribute_definition_clause}s,
is more explicit.

The @nt{address_clause} has been renamed to @nt{at_clause} and moved
to @RefSec{Obsolescent Features}.
One can use an Address clause
(``for T'Address @key[use] ...;'')
instead.

The attributes defined in RM83-13.7.3 are moved to
@RefSecNum{Numerics},
@RefSecNum{Attributes of Floating Point Types}, and
@RefSecNum{Attributes of Fixed Point Types}.
@end{DiffWord83}

@begin{MetaRules}
By default, the Alignment of a subtype should
reflect the ``natural'' alignment for objects of the
subtype on the machine.
The Alignment, whether specified or default,
should be known at compile time, even though
Addresses are generally not known at compile
time.
(The generated code should never need to check
at run time the number of zero bits at the end
of an address to determine an alignment).

There are two symmetric purposes of Alignment clauses, depending on
whether or not the implementation has control over object
allocation.
If the implementation allocates an object,
the implementation should ensure that
the Address and Alignment are consistent with
each other.
If something outside the implementation allocates an
object, the implementation should be allowed to
assume that the Address and Alignment are
consistent, but should not assume stricter alignments
than that.
@end{MetaRules}

@begin{StaticSem}
For @PrefixType{a prefix X that denotes a subtype or
object}:
@begin{Description}
@Attribute{Prefix=<X>, AttrName=<Alignment>,
  Text=<The Address of an object that is allocated under
    control of the implementation is an integral
    multiple of the Alignment of the object
    (that is, the Address modulo the Alignment is zero).
    The offset of a record component is a multiple of the
    Alignment of the component.
    For an object that is not allocated under control of
    the implementation
    (that is, one that is imported,
    that is allocated by a user-defined allocator,
    whose Address has been specified,
    or is designated by an access value returned by an
    instance of Unchecked_Conversion),
    the implementation may assume that the Address is
    an integral multiple of its Alignment.
    The implementation shall not assume a stricter alignment.

    @NoPrefix@;The value of this attribute is of type @i{universal_integer},
    and nonnegative;
    zero means that the object is not necessarily
    aligned on a storage element boundary.>}
@EndPrefixType{}
@begin{Ramification}
The Alignment is passed by an @nt{allocator} to the
Allocate operation;
the implementation has to choose a value such that if the address
returned by Allocate is aligned as requested,
the generated code can correctly access the object.


The above mention of ``modulo'' is referring to the "@key[mod]"
operator declared in System.Storage_Elements;
if X @key[mod] N = 0, then X is by definition aligned on an
N-storage-element boundary.

@end{Ramification}

@PDefn2{Term=[specifiable], Sec=(of Alignment for first subtypes and objects)}
@Defn{Alignment clause}
@NoPrefix@;Alignment may be specified for first subtypes and
@Redundant[stand-alone] objects
via an @nt{attribute_definition_clause};
the expression of such a clause shall be static,
and its value nonnegative.
If the Alignment of a subtype is specified,
then the Alignment of an object of the subtype is at least as strict,
unless the object's Alignment is also specified.
The Alignment of an object created by an allocator is that of the
designated subtype.

@NoPrefix@;If an Alignment is specified for a composite subtype or object, this
Alignment shall be equal to the least common multiple of any
specified Alignments of the subcomponent subtypes, or an integer
multiple thereof.
@end{Description}
@end{StaticSem}

@begin{Erron}
Program execution is erroneous if an Address clause is given that
conflicts with the Alignment.
@begin{Ramification}
The user has to either give an Alignment clause also,
or else know what Alignment the implementation will choose by default.
@end{Ramification}

If the Alignment is specified for an object that is not allocated
under control of the implementation,
execution is erroneous if the object is not aligned according to the
Alignment.
@end{Erron}

@begin{ImplAdvice}
@PDefn2{Term=[recommended level of support], Sec=(Alignment attribute
for subtypes)}
The recommended level of support for the Alignment attribute for
subtypes is:
@begin{Itemize}
An implementation should support specified
Alignments that are factors and multiples of
the number of storage elements per word,
subject to the following:

An implementation need not support specified
Alignments for combinations of Sizes and
Alignments that cannot be easily loaded and
stored by available machine instructions.

An implementation need not support specified
Alignments that are greater than the maximum
Alignment the implementation ever returns
by default.
@end{Itemize}

@PDefn2{Term=[recommended level of support], Sec=(Alignment attribute
for objects)}
The recommended level of support for the Alignment attribute for
objects is:
@begin{Itemize}
Same as above, for subtypes, but in addition:

For stand-alone library-level objects of statically constrained
subtypes, the implementation should support all Alignments
supported by the target linker.  For example, page alignment
is likely to be supported for such objects, but not for subtypes.
@end{Itemize}
@end{ImplAdvice}

@begin{Notes}
Alignment is a subtype-specific attribute.

The Alignment of a composite object is always equal to the least
common multiple of the Alignments of its components, or a multiple
thereof.
@begin{Discussion}
For default Alignments, this follows from the semantics of Alignment.
For specified Alignments, it follows from a @LegalityName stated above.
@end{Discussion}

A @nt{component_clause}, Component_Size clause, or a @nt{pragma} Pack
can override a specified Alignment.
@begin{Discussion}
Most objects are allocated by the implementation; for these, the
implementation obeys the Alignment.  The implementation is of course
allowed to make an object @i{more} aligned than its Alignment requires
@em an object whose Alignment is 4 might just happen to land at an
address that's a multiple of 4096.
For formal parameters, the implementation might
want to force an Alignment stricter than the parameter's subtype.
For example, on some systems, it is customary to always align
parameters to 4 storage elements.

Hence, one might initially assume that the implementation could
evilly make all Alignments 1 by default, even though integers, say,
are normally aligned on a 4-storage-element boundary.  However, the
implementation cannot get away with that @em if the Alignment is 1,
the generated code cannot assume an Alignment of 4, at least not for
objects allocated outside the control of the implementation.

Of course implementations can assume anything they can prove, but
typically an implementation will be unable to prove much about the
alignment of, say, an imported object.  Furthermore, the information
about where an address ``came from'' can be lost to the compiler due to
separate compilation.

The Alignment of an object that is a component of a packed
composite object will usually be 0, to indicate that the component is
not necessarily aligned on a storage element boundary.
For a subtype, an Alignment of 0 means that objects of the subtype are
not normally aligned on a storage element boundary at all.
For example, an implementation might choose to make Component_Size be 0
for an array of Booleans, even when @nt{pragma} Pack has not been
specified for the array.
In this case, Boolean'Alignment would be 0.
(In the presence of tasking, this would in general be feasible only on a
machine that had atomic test-bit and set-bit instructions.)

If the machine has no particular natural alignments, then all subtype
Alignments will probably be 1 by default.

Specifying an Alignment of 0 in an @nt{attribute_definition_clause} does
not require the implementation to do anything (except return 0 when the
Alignment is queried).
However, it might be taken as advice on some implementations.

It is an error for an Address clause to disobey the object's Alignment.
The error cannot be detected at compile time, in general, because the
Address is not necessarily known at compile time (and is almost
certainly not static).  We do not require a run-time check, since
efficiency seems paramount here, and Address clauses are treading on
thin ice anyway.  Hence, this misuse of Address clauses is just like any
other misuse of Address clauses @em it's erroneous.

A type extension can have a stricter Alignment than its parent.
This can happen, for example, if the Alignment of the parent is 4,
but the extension contains a component with Alignment 8.
The Alignment of a class-wide type or object will have to be the
maximum possible Alignment of any extension.

The recommended level of support for the Alignment attribute is
intended to reflect a minimum useful set of capabilities.  An
implementation can assume that all Alignments are multiples of each
other @em 1, 2, 4, and 8 might be the only supported Alignments for
subtypes.  An Alignment of 3 or 6 is unlikely to be useful.
For objects that can be allocated statically, we recommend that
the implementation support larger alignments, such as 4096.  We do
not recommend such large alignments for subtypes, because the maximum
subtype alignment will also have to be used as the alignment of stack
frames, heap objects, and class-wide objects.  Similarly, we do not
recommend such large alignments for stack-allocated objects.

If the maximum default Alignment is 8
(say, Long_Float'Alignment = 8),
then the implementation can refuse to accept stricter alignments
for subtypes.  This simplifies the generated code, since the compiler
can align the stack and class-wide types to this maximum without a
substantial waste of space (or time).

Note that the recommended level of support takes into account
interactions between Size and Alignment.  For example, on a 32-bit
machine with 8-bit storage elements, where load and store
instructions have to be aligned according to the size of the thing
being loaded or stored, the implementation might accept an Alignment
of 1 if the Size is 8, but might reject an Alignment of 1 if the Size
is 32.  On a machine where unaligned loads and stores are merely
inefficient (as opposed to causing hardware traps),
we would expect an Alignment of 1 to be supported for any Size.
@end{Discussion}
@end{Notes}

@begin{DiffWord83}
The nonnegative part is missing from RM83
(for @nt{mod_clause}s, nee @nt{alignment_clause}s,
which are an obsolete version of Alignment clauses).
@end{DiffWord83}

@begin{StaticSem}
For @PrefixType{a prefix X that denotes an
object}:
@begin{Description}
@Attribute{Prefix=<X>, AttrName=<Size>,
  Text=<Denotes the size in bits of
the representation of the object.
The value of this attribute is of the type
@i{universal_integer}.>}
@EndPrefixType{}
@begin{Ramification}
Note that Size is in bits even if Machine_Radix is 10.
Each decimal digit (and the sign) is presumably represented
as some number of bits.
@end{Ramification}

@PDefn2{Term=[specifiable], Sec=(of Size for stand-alone objects)}
@Defn{Size clause}
@NoPrefix@;Size may be specified for @Redundant[stand-alone] objects
via an @nt{attribute_definition_clause};
the expression of such a clause shall be static
and its value nonnegative.
@end{Description}
@end{StaticSem}

@begin{ImplAdvice}
@PDefn2{Term=[recommended level of support], Sec=(Size attribute)}
The recommended level of support for the Size attribute
of objects is:
@begin{Itemize}
A Size clause should be supported for an object if the specified Size
is at least as large as its subtype's Size, and corresponds to a size in
storage elements that is a multiple of the object's Alignment (if the
Alignment is nonzero).
@end{Itemize}
@end{ImplAdvice}

@begin{StaticSem}
For @PrefixType{every subtype S}:
@begin{Description}
@Attribute{Prefix=<S>, AttrName=<Size>,
  Text=<If S is definite,
denotes the size @Redundant{(in bits)}
that the implementation would choose for
the following objects of subtype S:
@begin{Itemize}
A record component of subtype S
when the record type is packed.

The formal parameter of an instance of Unchecked_Conversion
that converts from subtype S to some other subtype.
@end{Itemize}

@NoPrefix@;If S is indefinite,
the meaning is implementation defined.
The value of this attribute is of the type
@i{universal_integer}.>}
@PDefn2{Term=[specifiable], Sec=(of Size for first subtypes)}
@Defn{Size clause}

The Size of an object is at least as large as that of its subtype,
unless the object's Size is determined by a Size clause,
a component_clause, or a Component_Size clause.

Size may be specified for first subtypes
via an @nt{attribute_definition_clause};
the expression of such a clause shall be static
and its value nonnegative.
@ImplDef{The meaning of Size for indefinite subtypes.}
  @begin{Reason}
  The effects of specifying the Size of a subtype are:
  @begin{Itemize}
    Unchecked_Conversion works in a predictable manner.

    A composite type cannot be packed so tightly as to override
    the specified Size of a component's subtype.

    Assuming the @ImplAdviceName is obeyed,
    if the specified Size allows independent addressability,
    then the Size of certain objects of the subtype
    should be equal to the subtype's Size.
    This applies to stand-alone objects and to components
    (unless a @nt{component_clause} or a Component_Size clause applies).
  @end{Itemize}

  A @nt{component_clause} or a Component_Size clause can cause an object
  to be smaller than its subtype's specified size.
  A @nt{pragma} Pack cannot; if a component subtype's size is specified,
  this limits how tightly the composite object can be packed.

  The Size of a class-wide (tagged) subtype is unspecified,
  because it's not clear what it should mean;
  it should certainly not depend on all of the descendants that happen
  to exist in a given program.
  Note that this cannot be detected at compile time,
  because in a generic unit, it is not necessarily known
  whether a given subtype is class-wide.
  It might raise an exception on some implementations.
  @end{Reason}
  @begin{Ramification}
  A Size clause for a numeric subtype need not
  affect the underlying numeric type.
  For example, if I say:
  @begin{Example}
@key[type] S @key[is] @key[range] 1..2;
@key[for] S'Size @key[use] 64;
  @end{Example}

  I am not guaranteed that S'Base'Last >= 2**63@en@;1,
  nor that intermediate results will be represented in 64 bits.
  @end{Ramification}
  @begin{Reason}
  There is no need to complicate implementations for this sort of
  thing, because the right way to affect the base range of a type
  is to use the normal way of declaring the base range:
  @begin{Example}
@key[type] Big @key[is] @key[range] -2**63 .. 2**63 - 1;
@key[subtype] Small @key[is] Big @key[range] 1..1000;
  @end{Example}
  @end{Reason}
  @begin{Ramification}
  The Size of a large unconstrained subtype (e.g. String'Size)
  is likely to raise Constraint_Error,
  since it is a nonstatic expression of type @i{universal_integer}
  that might overflow the largest signed integer type.
  There is no requirement that the largest integer type be able to
  represent the size in bits of the largest possible object.
  @end{Ramification}
@end{Description}
@EndPrefixType{}
@end{StaticSem}

@begin{ImplReq}
In an implementation, Boolean'Size shall be 1.
@end{ImplReq}

@begin{ImplAdvice}
If the Size of a subtype is specified,
and allows for efficient independent addressability
(see @RefSecNum{Shared Variables}) on the target architecture,
then the Size of the following objects of the subtype should equal the
Size of the subtype:
@begin{Itemize}
Aliased objects (including components).

Unaliased components, unless the Size of the
component is determined by a @nt{component_clause} or Component_Size
clause.
@end{Itemize}
@begin{Ramification}
Thus, on a typical 32-bit machine,
``@key[for] S'Size @key[use] 32;''
will guarantee that aliased objects of subtype S,
and components whose subtype is S, will have Size
= 32 (assuming the implementation chooses to obey this @ImplAdviceTitle).
On the other hand, if one writes,
``@key[for] S2'Size @key[use] 5;''
then stand-alone objects of subtype S2 will typically have their Size
rounded up to ensure independent addressability.

Note that ``@key[for] S'Size @key[use] 32;''
does not cause things like formal parameters to have Size = 32 @em
the implementation is allowed to make all parameters be at least 64
bits, for example.

Note that
``@key[for] S2'Size @key[use] 5;''
requires record components whose subtype is S2 to be exactly 5 bits
if the record type is packed.
The same is not true of array components;
their Size may be rounded up to the nearest factor of the word size.
@end{Ramification}
@begin{ImplNote}
@Defn{gaps}
On most machines, arrays don't contain gaps between components;
if the Component_Size is greater than the Size of the component subtype,
the extra bits are generally considered part of each component,
rather than gaps between components.
On the other hand,
a record might contain gaps between components,
depending on what sorts of loads, stores, and masking operations
are generally done by the generated code.

For an array,
any extra bits stored for each component will generally be part
of the component @em the whole point of storing extra bits is to
make loads and stores more efficient by avoiding the need to mask out
extra bits.
The PDP-10 is one counter-example;
since the hardware supports byte strings with a gap at the end of
each word,
one would want to pack in that manner.
@end{ImplNote}

A Size clause on a composite subtype should not affect
the internal layout of components.
@begin{Reason}
That's what Pack @nt{pragma}s, @nt{record_representation_clause}s,
and Component_Size clauses are for.
@end{Reason}

@PDefn2{Term=[recommended level of support], Sec=(Size attribute)}
The recommended level of support for the Size attribute
of subtypes is:
@begin{Itemize}

The Size (if not specified) of a static discrete or fixed point subtype
should be the number of bits needed to represent each value belonging to
the subtype using an unbiased representation,
leaving space for a sign bit only if the subtype contains negative
values.
If such a subtype is a first subtype,
then an implementation should support a specified Size for it that
reflects this representation.

@begin{ImplNote}
  This applies to static enumeration subtypes,
  using the internal codes used to represent the values.

  For a two's-complement machine, this implies that
  for a static signed integer subtype S,
  if all values of S are in the range 0 .. 2@+{@i{n}}@en@;1,
  or all values of S are in the range @en@;2@+{@i{n@en@;1}} .. 2@+{@i{n@en@;1}}@en@;1,
  for some @i{n} less than or equal to the word size,
  then S'Size should be <= the smallest such @i{n}.
  For a one's-complement machine,
  it is the same except that in the second range,
  the lower bound ``@en@;2@+{@i{n@en@;1}}'' is replaced by ``@en@;2@+{@i{n@en@;1}}+1''.


  If an integer subtype (whether signed or unsigned)
  contains no negative values, the Size should not include space
  for a sign bit.


  Typically, the implementation will choose to make the Size of a
  subtype be exactly the smallest such @i{n}.
  However, it might, for example, choose a biased representation,
  in which case it could choose a smaller value.

  On most machines, it is in general not a good idea to pack (parts of)
  multiple stand-alone objects into the same storage element,
  because (1) it usually doesn't save much space,
  and (2) it requires locking to prevent tasks from interfering with each
  other, since separate stand-alone objects are independently
  addressable.
  Therefore, if S'Size = 2
  on a machine with 8-bit storage elements,
  the size of a stand-alone object of subtype S will probably not be 2.
  It might, for example, be 8, 16 or 32, depending on the availability
  and efficiency of various machine instructions.
  The same applies to components of composite types,
  unless packing, Component_Size, or record layout is specified.

  For an unconstrained discriminated object,
  if the implementation allocates the maximum
  possible size,
  then the Size attribute should return that maximum
  possible size.
@end{ImplNote}
  @begin{Ramification}
  The Size of an object X is not usually the same as that of
  its subtype S.
  If X is a stand-alone object or a parameter, for example,
  most implementations will round X'Size up to a storage
  element boundary, or more, so X'Size might be greater than S'Size.
  On the other hand,
  X'Size cannot be less than S'Size, even
  if the implementation can prove, for example,
  that the range of values actually taken on by X during execution
  is smaller than the range of S.

  For example, if S is a first integer subtype whose range
  is 0..3, S'Size will be probably be 2 bits, and components of
  packed composite types of this subtype will be 2 bits
  (assuming Storage_Unit is a multiple of 2),
  but stand-alone objects and parameters will probably
  not have a size of 2 bits; they might be rounded up to
  32 bits, for example.
  On the other hand, Unchecked_Conversion will use the 2-bit size,
  even when converting a stand-alone object,
  as one would expect.

  Another reason for making the Size of an object bigger than
  its subtype's Size is to support the run-time detection of
  uninitialized variables.
@PDefn{uninitialized variables}
  The implementation might add an extra value to a discrete subtype
  that represents the uninitialized state,
  and check for this value on use.
  In some cases, the extra value will require an extra bit in the
  representation of the object.
  Such detection is not required by the language.
  If it is provided, the implementation has to be able to turn it off.
  For example, if the programmer gives a
  @nt{record_representation_clause} or Component_Size clause that makes
  a component too small to allow the extra bit,
  then the implementation will not be able to perform the checking
  (not using this method, anyway).

  The fact that the size of an object is not necessarily the same as its
  subtype can be confusing:
  @begin{Example}
@key[type] Device_Register @key[is] @key[range] 0..2**8 - 1;
@key[for] Device_Register'Size @key[use] 8; --@i{ Confusing!}
My_Device : Device_Register;
@key[for] My_Device'Address @key[use] To_Address(16#FF00#);
  @end{Example}

  The programmer might think that My_Device'Size is 8,
  and that My_Device'Address points at an 8-bit location.
  However, this is not true.
  In Ada 83 (and in Ada 9X), My_Device'Size might well be 32,
  and My_Device'Address might well point at the high-order 8 bits of
  the 32-bit object, which are always all zero bits.
  If My_Device'Address is passed to an assembly language subprogram,
  based on the programmer's assumption,
  the program will not work properly.
  @end{Ramification}
  @begin{Reason}
  It is not reasonable to require that an implementation allocate
  exactly 8 bits to all objects of subtype Device_Register.
  For example, in many run-time models, stand-alone objects
  and parameters are always aligned to a word boundary.
  Such run-time models are generally based on hardware considerations
  that are beyond the control of the implementer.
  (It is reasonable to require that an implementation allocate exactly
  8 bits to all components of subtype Device_Register, if packed.)
  @end{Reason}
  @begin{Ramification}
  The correct way to write the above code is like this:
  @begin{Example}
@key[type] Device_Register @key[is] @key[range] 0..2**8 - 1;
My_Device : Device_Register;
@key[for] My_Device'Size @key[use] 8;
@key[for] My_Device'Address @key[use] To_Address(16#FF00#);
  @end{Example}

  If the implementation cannot accept 8-bit stand-alone objects,
  then this will be illegal.
  However, on a machine where an 8-bit device register exists,
  the implementation will probably be able to accept 8-bit stand-alone
  objects.  Therefore, My_Device'Size will be 8,
  and My_Device'Address will point at those 8 bits,
  as desired.

  If an object of subtype Device_Register is passed to a foreign
  language subprogram, it will be passed according to that subprogram's
  conventions.  Most foreign language implementations have similar
  run-time model restrictions.
  For example, when passing to a C function,
  where the argument is of
  the C type char* (that is, pointer to char),
  the C compiler will generally expect a full word value,
  either on the stack, or in a register.
  It will @i{not} expect a single byte.
  Thus, Size clauses for subtypes really have nothing to do with
  passing parameters to foreign language subprograms.
  @end{Ramification}

For a subtype implemented with levels of indirection,
the Size should include the size of the pointers,
but not the size of what they point at.
@begin{Ramification}
For example, if a task object is represented as a pointer to some
information (including a task stack), then the size of the object
should be the size of the pointer.
The Storage_Size, on the other hand,
should include the size of the stack.
@end{Ramification}
@end{Itemize}
@end{ImplAdvice}

@begin{Notes}
Size is a subtype-specific attribute.

A @nt{component_clause} or Component_Size clause
can override a specified Size.
A @nt{pragma} Pack cannot.
@end{Notes}

@begin{DiffWord83}
The requirement for a nonnegative value in a Size clause
was not in RM83, but it's hard to see how it would make sense.
For uniformity, we forbid negative sizes,
rather than letting implementations define their meaning.
@end{DiffWord83}

@begin{StaticSem}
For @PrefixType{a prefix T that denotes a task
object @Redundant[(after any implicit dereference)]}:
@begin{Description}
@Attribute{Prefix=<T>, AttrName=<Storage_Size>,
  Text=<Denotes the number of storage elements reserved for
  the task.
The value of this attribute is of the type
@i{universal_integer}.
The Storage_Size includes the size of the task's stack,
if any.  The language does not specify whether or
not it includes other storage associated with the task
(such as the ``task control block'' used by some
implementations.)>}
If a @nt{pragma} Storage_Size is given,
the value of the Storage_Size attribute is at least
the value specified in the @nt{pragma}.
@EndPrefixType{}
  @begin{Ramification}
  The value of this attribute is never negative,
  since it is impossible to ``reserve'' a negative number
  of storage elements.

  If the implementation chooses to allocate an initial amount of
  storage, and then increase this as needed,
  the Storage_Size cannot include the additional amounts
  (assuming the allocation of the additional amounts can raise
  Storage_Error); this is inherent in the meaning of ``reserved.''

  The implementation is allowed to allocate different amounts of
  storage for different tasks of the same subtype.

  Storage_Size is also defined for access subtypes
  @em see @RefSecNum{Storage Management}.
  @end{Ramification}
@end{Description}
@end{StaticSem}

@begin{Intro}
@redundant[
@IndexSeeAlso{Term=[Storage_Size clause],See=[pragma Storage_Size]}
A @nt{pragma} Storage_Size specifies the amount of storage to be
reserved for the execution of a task.
]
@end{Intro}

@begin{Syntax}
@begin{SyntaxText}
The form of a @nt{pragma} Storage_Size is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(Storage_Size)(@Syn2{expression});'

@begin{SyntaxText}
A @nt{pragma} Storage_Size is allowed only immediately within a
@nt{task_definition}.
@end{SyntaxText}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected type],
  Sec=(Storage_Size pragma argument)}
The @nt{expression} of a @nt<pragma> Storage_Size
is expected to be of any integer type.
@end{Resolution}

@begin{RunTime}
A @nt<pragma> Storage_Size is elaborated when an object
of the type defined by the immediately enclosing @nt<task_definition>
is created.
@PDefn2{Term=[elaboration],Sec=(Storage_Size pragma)}
For the elaboration of a @nt<pragma> Storage_Size, the @nt<expression>
is evaluated; the Storage_Size attribute of the newly created task object
is at least the value of the @nt<expression>.
@begin{Ramification}
  The implementation is allowed to round up a specified Storage_Size
  amount.
  For example, if the implementation always allocates in
  chunks of 4096 bytes, the number 200 might be rounded
  up to 4096.  Also, if the user specifies a negative
  number, the implementation has to normalize this to 0,
  or perhaps to a positive number.
@end{Ramification}

@IndexCheck{Storage_Check}
@Defn2{Term=[Storage_Error],Sec=(raised by failure of run-time check)}
At the point of task object creation, or upon task activation,
Storage_Error is raised if there is insufficient free storage to
accommodate the requested Storage_Size.
@end{RunTime}

@begin{StaticSem}
For @PrefixType{a prefix X that denotes an array subtype or array
object @Redundant[(after any implicit dereference)]}:
@begin{Description}
@Attribute{Prefix=<X>, AttrName=<Component_Size>,
  Text=<Denotes the size in bits of
components of the type of X.
The value of this attribute is of type @i{universal_integer}.>}
@EndPrefixType{}

@PDefn2{Term=[specifiable], Sec=(of Component_Size for
array types)}
@Defn{Component_Size clause}
@NoPrefix@;Component_Size may be specified for array types
via an @nt{attribute_definition_clause};
the expression of such a clause shall be static,
and its value nonnegative.
@begin{ImplNote}
The intent is that the value of X'Component_Size is always nonnegative.
If the array is stored ``backwards'' in memory
(which might be caused by an implementation-defined pragma),
X'Component_Size is still positive.
@end{ImplNote}
@begin{Ramification}
For an array object A, A'Component_Size = A(I)'Size for any index I.
@end{Ramification}
@end{Description}
@end{StaticSem}

@begin{ImplAdvice}
@PDefn2{Term=[recommended level of support], Sec=(Component_Size attribute)}
The recommended level of support for the Component_Size attribute is:
@begin{Itemize}
An implementation need not support specified Component_Sizes that
are less than the Size of the component subtype.

An implementation should support specified Component_Sizes that
are factors and multiples of the word size.
For such Component_Sizes, the array should contain no gaps between
components.
For other Component_Sizes (if supported), the array should
contain no gaps between components when packing is also specified;
the implementation should forbid this combination in cases where it
cannot support a no-gaps representation.
@begin{Ramification}
For example, if Storage_Unit = 8, and Word_Size = 32,
then the user is allowed to specify a Component_Size of
1, 2, 4, 8, 16, and 32, with no gaps.
In addition, @i{n}*32 is allowed for positive integers @i{n},
again with no gaps.
If the implementation accepts Component_Size = 3,
then it might allocate 10 components per word,
with a 2-bit gap at the end of each word
(unless packing is also specified),
or it might not have any internal gaps at all.
(There can be gaps at either end of the array.)
@end{Ramification}
@end{Itemize}
@end{ImplAdvice}

@begin{StaticSem}

For @PrefixType{every subtype S of a tagged type @i(T)
(specific or class-wide)},
the following attribute is defined:
@begin{Description}
@Attribute{Prefix=<S>, AttrName=<External_Tag>,
  Text=[@Defn{External_Tag clause}
  @PDefn2{Term=(specifiable), Sec=(of External_Tag for a tagged type)}
  S'External_Tag denotes an external string representation
  for S'Tag; it is of the predefined type String.
  External_Tag may be specified
  for a specific tagged type
  via an @nt{attribute_definition_clause};
  the expression of such a clause shall be static.
  The default external
  tag representation is implementation defined.
  See @RefSecNum{Dispatching Operations of Tagged Types} and
  @RefSecNum{Stream-Oriented Attributes}.]}
  @ImplDef{The default external representation for a type tag.}
  @end{Description}
@EndPrefixType()
@end{StaticSem}

@begin{ImplReq}
In an implementation,
the default external tag for each specific tagged type declared
in a partition
shall be distinct, so long as the type is declared outside an
instance of a generic body.
If the compilation unit in which
a given tagged type is declared, and all compilation units on which it
semantically depends, are the same in two different partitions,
then the external tag for the type shall be the same in the
two partitions.
What it means for a compilation unit to be the same in
two different partitions is implementation defined.
At a minimum, if the compilation unit is not recompiled
between building the two different partitions that include it, the compilation
unit is considered the same in the two partitions.
@ImplDef{What determines whether a compilation unit is the
same in two different partitions.}
@begin{Reason}
  These requirements are important because external tags are used
  for input/output of class-wide types.  These requirements ensure
  that what is written by one program can be read back by some other
  program so long as they share the same declaration for the
  type (and everything it depends on).

  The user may specify the external tag if (s)he wishes its value
  to be stable even across changes to the compilation unit
  in which the type is declared (or changes in some unit on which it
  depends).

  We use a String rather than a Storage_Array to represent an
  external tag for portability.
@end{Reason}
@begin{Ramification}
  Note that the characters
  of an external tag need not all be graphic characters.
  In other words, the external tag can be a sequence of arbitrary
  8-bit bytes.
@end{Ramification}
@end{ImplReq}


@begin{Notes}
The following language-defined attributes are specifiable,
at least for some of the kinds of entities to which they apply:
Address, Size, Component_Size, Alignment, External_Tag,
Small, Bit_Order, Storage_Pool, Storage_Size,
Write, Output, Read, Input,
and Machine_Radix.

It follows from the general rules in @RefSecNum{Representation Items}
that if one writes ``@key[for] X'Size @key[use] Y;'' then the X'Size
@nt{attribute_reference} will return Y
(assuming the implementation allows the Size clause).
The same is true for all of the specifiable attributes
except Storage_Size.
@begin{Ramification}
An implementation may specify that an
implementation-defined attribute is specifiable
for certain entities.
This follows from the fact that the semantics of
implementation-defined attributes is implementation defined.
An implementation is not allowed to make
a language-defined attribute specifiable if it isn't.
@end{Ramification}
@end{Notes}

@begin{Examples}
@i{Examples of attribute definition clauses:}
@begin{Example}
Byte : @key[constant] := 8;
Page : @key[constant] := 2**12;

@key[type] Medium @key[is] @key[range] 0 .. 65_000;
@key[for] Medium'Size @key[use] 2*Byte;
@key[for] Medium'Alignment @key[use] 2;
Device_Register : Medium;
@key[for] Device_Register'Size @key[use] Medium'Size;
@key[for] Device_Register'Address @key[use] System.Storage_Elements.To_Address(16#FFFF_0020#);

@key[type] Short @key[is] @key[delta] 0.01 @key[range] -100.0 .. 100.0;
@key[for] Short'Size @key[use] 15;

@key[for] Car_Name'Storage_Size @key[use] --@i{ specify access type's storage pool size}
        2000*((Car'Size/System.Storage_Unit) +1); --@i{ approximately 2000 cars}

@key[function] My_Read(Stream : @key[access] Ada.Streams.Root_Stream_Type'Class)
  @key[return] T;
@key(for) T'Read @key(use) My_Read; --@i{ see @RefSecNum{Stream-Oriented Attributes}}
@end{Example}
@end{Examples}

@begin{Notes}
@i{Notes on the examples:}
In the Size clause for Short,
fifteen bits is the minimum necessary,
since the type definition requires Short'Small <= 2**(@en@;7).
@end{Notes}

@begin{Extend83}
The syntax rule for @nt{length_clause} is replaced with the new syntax rule
for @nt{attribute_definition_clause}, and it is modified to allow a
@nt{name} (as well as an expression).
@end{Extend83}

@begin{DiffWord83}
The syntax rule for @nt{attribute_definition_clause} now requires that the
prefix of the @nt{attribute} be a @nt{local_name};
in Ada 83 this rule was stated in the text.

In Ada 83, the relationship between a @nt{representation_clause}
specifying a certain aspect and an attribute that queried that
aspect was unclear.
In Ada 9X, they are the same,
except for certain explicit exceptions.
@end{DiffWord83}

@LabeledClause{Enumeration Representation Clauses}

@begin{Intro}
@redundant[
An @nt{enumeration_representation_clause} specifies the internal
codes for enumeration literals.
]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<enumeration_representation_clause>,rhs="
    @key{for} @SynI{first_subtype_}@Syn2{local_name} @key{use} @Syn2{enumeration_aggregate};"}
@Syn{lhs=<enumeration_aggregate>,rhs="@Syn2{array_aggregate}"}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected type],
  Sec=(enumeration_representation_clause expressions)}
The @nt<enumeration_aggregate> shall be written as a one-dimensional
@nt<array_aggregate>, for which the index subtype is the unconstrained
subtype of the enumeration type, and each component expression
is expected to be of any integer type.
@begin{Ramification}
The ``full coverage rules'' for @nt<aggregate>s applies.
An @key{others} is not allowed @em there is no applicable index
constraint in this context.
@end{Ramification}
@end{Resolution}

@begin{Legality}
The @SynI{first_subtype_}@nt{local_name} of an
@nt{enumeration_representation_clause} shall denote an
enumeration subtype.
@begin{Ramification}
As for all type-related representation items,
the @nt{local_name} is required to denote a first subtype.
@end{Ramification}

The expressions given in the @nt{array_aggregate} shall be static,
and shall specify distinct integer codes for each value
of the enumeration type; the associated integer codes shall
satisfy the predefined ordering relation of the type.
@begin{Reason}
  Each value of the enumeration type has to be given an internal code,
  even if the first subtype of the enumeration type is constrained
  to only a subrange (this is only possible if the enumeration type
  is a derived type).  This ``full coverage'' requirement is important
  because one may refer to Enum'Base'First and Enum'Base'Last, which
  need to have defined representations.
@end{Reason}
@end{Legality}

@begin{StaticSem}
@PDefn2{Term=[aspect of representation], Sec=(coding)}
@Defn2{Term=[coding], Sec=(aspect of representation)}
An @nt{enumeration_representation_clause} specifies the
@i{coding} aspect of representation.
@Defn{internal code}
The coding consists of the @i{internal code} for each enumeration
literal, that is, the integral value used internally to
represent each literal.

@end{StaticSem}

@begin{ImplReq}
For nonboolean enumeration types,
if the coding is not specified for the type, then
for each value of the type, the internal code shall be equal to
its position number.
@begin{Reason}
  This default representation is already used by all known
  Ada compilers for nonboolean enumeration types.  Therefore,
  we make it a requirement so users can depend on it, rather
  than feeling obliged to supply for every enumeration type
  an enumeration representation clause that is equivalent
  to this default rule.
@end{Reason}
@begin{Discussion}
  For boolean types, it is relatively common to use all ones for
  True, and all zeros for False, since some hardware supports
  that directly.  Of course, for a one-bit Boolean object
  (like in a packed array), False is presumably zero and True
  is presumably one (choosing the reverse would be extremely
  unfriendly!).
@end{Discussion}
@end{ImplReq}

@begin{ImplAdvice}
@PDefn2{Term=[recommended level of support], Sec=(@nt{enumeration_representation_clause})}
The recommended level of support for @nt{enumeration_representation_clause}s is:
@begin{Itemize}
An implementation should support at least the internal codes in the
range System.Min_Int..System.Max_Int.
An implementation need not support
@nt{enumeration_representation_clause}s for boolean types.
@begin{Ramification}
The implementation may support numbers outside the above
range, such as numbers greater than System.Max_Int.
See AI-00564.
@end{Ramification}
@begin{Reason}
The benefits of specifying the internal coding of a boolean type do
not outweigh the implementation costs.
Consider, for example, the implementation of the logical operators on
a packed array of booleans with strange internal codes.
It's implementable, but not worth it.
@end{Reason}
@end{Itemize}
@end{ImplAdvice}

@begin{Notes}
Unchecked_Conversion may be used to query the internal codes used
for an enumeration type.
The attributes of the type, such as Succ, Pred, and Pos,
are unaffected by the @nt{representation_clause}.
For example, Pos always returns the position number, @i{not} the
internal integer code that might have been specified in a
@nt{representation_clause}.
@begin{Discussion}
Suppose the enumeration type in question is derived:
@begin{Example}
@key[type] T1 @key[is] (Red, Green, Blue);
@key[subtype] S1 @key[is] T1 @key[range] Red .. Green;
@key[type] S2 @key[is] @key[new] S1;
@key[for] S2 @key[use] (Red => 10, Green => 20, Blue => 30);
@end{Example}

The @nt{representation_clause} has to specify values for all
enumerals, even ones that are not in S2 (such as Blue).
The Base attribute can be used to get at these values.
For example:
@begin{Example}
@key[for] I @key[in] S2'Base @key[loop]
    ... --@i{ When I equals Blue, the internal code is 30.}
@key[end] @key[loop];
@end{Example}

We considered allowing or requiring
``@key[for] S2'Base @key[use] ...'' in cases like this,
but it didn't seem worth the trouble.
@end{Discussion}
@end{Notes}

@begin{Examples}
@i{Example of an enumeration representation clause:}
@begin{Example}
@key[type] Mix_Code @key[is] (ADD, SUB, MUL, LDA, STA, STZ);

@key[for] Mix_Code @key[use]
   (ADD => 1, SUB => 2, MUL => 3, LDA => 8, STA => 24, STZ =>33);
@end{Example}
@end{Examples}

@begin{Extend83}
As in other similar contexts, Ada 9X allows expressions of any integer type,
not just expressions of type @i{universal_integer}, for the component
expressions in the @nt<enumeration_aggregate>.  The preference rules
for the predefined operators of @i{root_integer} eliminate
any ambiguity.

For portability, we now require that the default coding for an enumeration
type be the ``obvious'' coding using position numbers.
This is satisfied by all known implementations.
@end{Extend83}

@LabeledClause{Record Layout}

@begin{Intro}
@PDefn2{Term=[aspect of representation], Sec=(layout)}
@Defn2{Term=[layout], Sec=(aspect of representation)}
@PDefn2{Term=[aspect of representation], Sec=(record layout)}
@Defn2{Term=[record layout], Sec=(aspect of representation)}
@PDefn2{Term=[aspect of representation], Sec=(storage place)}
@Defn2{Term=[storage place], Sec=(of a component)}
The @i{(record) layout} aspect of representation
consists of the @i{storage places} for some or all components,
that is, storage place attributes of the components.
The layout can be specified with a @nt{record_representation_clause}.
@end{Intro}

@LabeledSubClause{Record Representation Clauses}

@begin{Intro}
@redundant[
A @nt{record_representation_clause} specifies the storage representation
of records and record extensions, that is, the order, position, and size
of components (including discriminants, if any).
@IndexSee{Term=[bit field],See=(record_representation_clause)}
]
@end{Intro}

@begin{MetaRules}
It should be feasible for an implementation to use negative offsets
in the representation of composite types.
However, no implementation should be forced to support negative
offsets.
Therefore, negative offsets should be disallowed in
@nt{record_representation_clause}s.
@end{MetaRules}

@begin{Syntax}
@Syn{lhs=<record_representation_clause>,rhs="
    @key{for} @SynI{first_subtype_}@Syn2{local_name} @key{use}
      @key{record} [@Syn2{mod_clause}]
        {@Syn2{component_clause}}
      @key{end} @key{record};"}
@Hinge{}

@Syn{lhs=<component_clause>,rhs="
    @SynI{component_}@Syn2{local_name} @key{at} @Syn2{position} @key{range} @Syn2{first_bit} .. @Syn2{last_bit};"}

@Syn{lhs=<position>,rhs="@SynI{static_}@Syn2{expression}"}
@Syn{lhs=<first_bit>,rhs="@SynI{static_}@Syn2{simple_expression}"}
@Syn{lhs=<last_bit>,rhs="@SynI{static_}@Syn2{simple_expression}"}
@begin{Reason}
@nt{First_bit} and @nt{last_bit} need to be @nt{simple_expression}
instead of @nt{expression} for the same reason as in @nt{range}
(see @RefSec{Scalar Types}).
@end{Reason}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected type],
  Sec=(component_clause expressions)}
@PDefn2{Term=[expected type],
  Sec=(position)}
@PDefn2{Term=[expected type],
  Sec=(first_bit)}
@PDefn2{Term=[expected type],
  Sec=(last_bit)}
Each @nt{position}, @nt{first_bit}, and @nt{last_bit} is expected to
be of any integer type.
@begin{Ramification}
These need not have the same integer type.
@end{Ramification}
@end{Resolution}

@begin{Legality}
The @SynI{first_subtype_}@nt{local_name} of a
@nt{record_representation_clause} shall denote a specific
nonlimited record or record extension subtype.
@begin{Ramification}
As for all type-related representation items,
the @nt{local_name} is required to denote a first subtype.
@end{Ramification}

If the @i{component_}@nt<local_name> is a @nt<direct_name>,
the @nt{local_name} shall denote a component of the type.
For a record extension, the component shall not be inherited,
and shall not be a discriminant that corresponds to a discriminant of
the parent type.
If the @i{component_}@nt<local_name> has an @nt{attribute_designator},
the @nt{direct_name} of the @nt<local_name> shall denote either
the declaration of the type or a component of the type,
and the @nt{attribute_designator} shall denote an
implementation-defined implicit component of the type.

The @nt{position}, @nt{first_bit}, and @nt{last_bit} shall be
static expressions.
The value of @nt{position} and @nt{first_bit} shall be nonnegative.
The value of @nt{last_bit} shall be no less than
@nt{first_bit} @en 1.
@begin{Ramification}
A @nt{component_clause} such as
``X @key{at} 4 @key{range} 0..@en@;1;''
is allowed if X can fit in zero bits.
@end{Ramification}

At most one @nt{component_clause} is allowed for each component of
the type, including for each discriminant
(@nt{component_clauses} may be given for some, all, or none of the
components).
Storage places within a @nt{component_list} shall not overlap,
unless they are for components in distinct @nt{variant}s of the same
@nt{variant_part}.

A name that denotes a component of a type is not allowed within
a @nt{record_representation_clause} for the type,
except as the @SynI{component_}@nt<local_name>
of a @nt{component_clause}.
@begin{Reason}
    It might seem strange to make the
    @nt{record_representation_clause} part of the declarative region,
    and then disallow mentions of the components within almost all of
    the @nt{record_representation_clause}.
    The alternative would be to treat the
    @SynI{component_}@nt<local_name> like a formal parameter name in
    a subprogram call (in terms of visibility).
    However, this rule would imply slightly different semantics,
    because (given the actual rule)
    the components can hide other declarations.
    This was the rule in Ada 83, and we see no reason to change it.
    The following, for example, was and is illegal:
    @begin{Example}
@key[type] T @key[is]
    @key[record]
        X : Integer;
    @key[end] @key[record];
X : @key[constant] := 31; --@i{ Same defining name as the component.}
@key[for] T @key[use]
    @key[record]
        X @key[at] 0 @key[range] 0..X; --@i{ Illegal!}
    @key[end] @key[record];
    @end{Example}

    The component X hides the named number X throughout
    the @nt{record_representation_clause}.
@end{Reason}
@end{Legality}

@begin{StaticSem}
A @nt{record_representation_clause}
(without the @nt{mod_clause})
specifies the layout.
The storage place
attributes (see @RefSecNum{Storage Place Attributes})
are taken from the
values of the @nt{position}, @nt{first_bit},
and @nt{last_bit} expressions
after normalizing those values so that
@nt{first_bit} is less than Storage_Unit.
@begin{Ramification}
For example,
if Storage_Unit is 8, then
``C @key[at] 0 @key[range] 24..31;''
defines C'Position = 3, C'First_Bit = 0, and C'Last_Bit = 7.
This is true of machines with either bit ordering.

A @nt{component_clause} also determines the value of the Size
attribute of the component,
since this attribute is related to First_Bit and Last_Bit.
@end{Ramification}

@Redundant[A @nt{record_representation_clause} for a record extension
does not override the layout of the parent part;]
if the layout was specified for the parent type,
it is inherited by the record extension.
@end{StaticSem}

@begin{ImplPerm}
An implementation may generate implementation-defined components (for
example, one containing the offset of another component).
An implementation may generate names that denote
such implementation-defined components;
such names shall be implementation-defined @nt{attribute_reference}s.
An implementation may allow such implementation-defined names to be
used in @nt{record_representation_clauses}.
An implementation can restrict such @nt{component_clause}s in any
manner it sees fit.
@ImplDef{Implementation-defined components.}
@begin{Ramification}
Of course, since the semantics of implementation-defined
attributes is implementation defined, the implementation need not
support these names in all situations.
They might be purely for the purpose of @nt{component_clause}s,
for example.
The visibility rules for such names are up to the implementation.

We do not allow such component names to be normal identifiers
@em that would constitute blanket permission to do all kinds of evil
things.
@end{Ramification}
@begin{Discussion}
@Defn{dope}
Such implementation-defined components are known in the
vernacular as ``dope.''
Their main purpose is for storing offsets of components that depend
on discriminants.
@end{Discussion}

If a @nt<record_representation_clause> is given for an untagged derived type,
the storage place attributes for all of the components of the derived
type may differ
from those of the corresponding components of the parent type,
even for components whose storage
place is not specified explicitly in the @nt<record_representation_clause>.
@begin{Reason}
  This is clearly necessary, since the whole record may need to be
  laid out differently.
@end{Reason}
@end{ImplPerm}

@begin{ImplAdvice}
@PDefn2{Term=[recommended level of support], Sec=(@nt{record_representation_clause})}
The recommended level of support for @nt{record_representation_clause}s is:
@begin{Itemize}
An implementation should support storage places that can be extracted
with a load, mask, shift sequence of machine code,
and set with a load, shift, mask, store sequence,
given the available machine instructions and run-time model.

A storage place should be supported if its size is equal to the Size
of the component subtype,
and it starts and ends on a boundary that obeys the Alignment of the
component subtype.

If the default bit ordering applies to the declaration of
a given type, then for a component
whose subtype's Size is less than the word size,
any storage place that does not cross an aligned word boundary should
be supported.
@begin{Reason}
The above recommendations are sufficient to
define interfaces to most interesting hardware.
This causes less implementation burden than the definition in ACID,
which requires arbitrary bit alignments of arbitrarily large
components.
Since the ACID definition is neither enforced by the ACVC,
nor supported by all implementations,
it seems OK for us to weaken it.
@end{Reason}

An implementation may reserve a storage place for the tag
field of a tagged type, and disallow other components from overlapping
that place.
@begin{Ramification}
Similar permission for other dope is not granted.
@end{Ramification}

An implementation need not support a
@nt{component_clause} for a component of an extension part
if the storage place is not after the storage places of all components
of the parent type, whether or not those storage places had been specified.
@begin{Reason}
These restrictions are probably necessary if block equality
operations are to be feasible for class-wide types.
For block comparison to work, the implementation typically has to fill
in any gaps with zero (or one) bits.
If a ``gap'' in the parent type is filled in with a component in a
type extension, then this won't work when a class-wide object is
passed by reference, as is required.
@end{Reason}
@end{Itemize}
@end{ImplAdvice}

@begin{Notes}
If no @nt{component_clause} is given for a component, then the
choice of the storage place for the component is left to the
implementation.  If @nt{component_clause}s are given for all components,
the @nt{record_representation_clause} completely specifies the
representation of the type and will be obeyed exactly by the
implementation.
@begin{Ramification}
The
visibility rules prevent the name of a component of the type from
appearing in a @nt{record_representation_clause} at any place
@i{except} for the @SynI{component_}@nt<local_name> of a
@nt{component_clause}.
However, since the @nt{record_representation_clause} is part of the
declarative region of the type declaration,
the component names hide outer homographs throughout.

A @nt{record_representation_clause} cannot be given for a protected type,
even though protected types, like record types, have components.
The primary reason for this rule is that there is likely to be too
much dope in a protected type @em entry queues,
bit maps for barrier values, etc.
In order to control the representation of the user-defined components, simply
declare a record type, give it a @nt{representation_clause}, and give the
protected type one component whose type is the record type.
Alternatively, if the protected object is protecting something like a
device register, it makes more sense to keep the thing being
protected outside the protected object (possibly with a pointer to it
in the protected object), in order to keep implementation-defined
components out of the way.
@end{Ramification}
@end{Notes}

@begin{Examples}
@i{Example of specifying the layout of a record type:}
@begin{Example}
Word : @key[constant] := 4;  --@i{  storage element is byte, 4 bytes per word}

@key[type] State         @key[is] (A,M,W,P);
@key[type] Mode          @key[is] (Fix, Dec, Exp, Signif);

@key[type] Byte_Mask     @key[is] @key[array] (0..7)  @key[of] Boolean;
@key[type] State_Mask    @key[is] @key[array] (State) @key[of] Boolean;
@key[type] Mode_Mask     @key[is] @key[array] (Mode)  @key[of] Boolean;

@key[type] Program_Status_Word @key[is]
  @key[record]
      System_Mask        : Byte_Mask;
      Protection_Key     : Integer @key[range] 0 .. 3;
      Machine_State      : State_Mask;
      Interrupt_Cause    : Interruption_Code;
      Ilc                : Integer @key[range] 0 .. 3;
      Cc                 : Integer @key[range] 0 .. 3;
      Program_Mask       : Mode_Mask;
      Inst_Address       : Address;
@key[end] @key[record];

@key[for] Program_Status_Word @key[use]
  @key[record]
      System_Mask      @key[at] 0*Word @key[range] 0  .. 7;
      Protection_Key   @key[at] 0*Word @key[range] 10 .. 11; --@i{ bits 8,9 unused}
      Machine_State    @key[at] 0*Word @key[range] 12 .. 15;
      Interrupt_Cause  @key[at] 0*Word @key[range] 16 .. 31;
      Ilc              @key[at] 1*Word @key[range] 0  .. 1;  --@i{ second word}
      Cc               @key[at] 1*Word @key[range] 2  .. 3;
      Program_Mask     @key[at] 1*Word @key[range] 4  .. 7;
      Inst_Address     @key[at] 1*Word @key[range] 8  .. 31;
  @key[end] @key[record];

@key[for] Program_Status_Word'Size @key[use] 8*System.Storage_Unit;
@key[for] Program_Status_Word'Alignment @key[use] 8;
@end{Example}
@end{Examples}

@begin{Notes}
@i{Note on the example:}
The @nt{record_representation_clause} defines the record layout.  The
Size clause guarantees that (at least) eight storage elements are used
for objects of the type.  The Alignment clause guarantees that
aliased, imported, or exported objects of the type will have
addresses divisible by eight.
@end{Notes}


@begin{DiffWord83}
The @nt{alignment_clause} has been renamed to @nt{mod_clause} and moved
to @RefSec{Obsolescent Features}.

We have clarified that implementation-defined component names have to be
in the form of an @nt{attribute_reference} of a component or of the
first subtype itself;
surely Ada 83 did not intend to allow arbitrary identifiers.

The RM83-13.4(7) wording incorrectly allows components in
non-variant records to overlap.
We have corrected that oversight.
@end{DiffWord83}

@LabeledSubClause{Storage Place Attributes}

@begin{StaticSem}
@Defn2{Term=[storage place attributes], Sec=(of a component)}
For @PrefixType{a component C of a composite, non-array
object R},
the @i{storage place attributes} are defined:
@begin{Ramification}
The storage place attributes are not (individually) specifiable,
but the user may control their values by giving a
@nt{record_representation_clause}.
@end{Ramification}
@begin{Description}
@Attribute{Prefix=<R.C>, AttrName=<Position>,
  Text=<Denotes the same value as R.C'Address @en@; R'Address.
The value of this attribute is of the type
@i{universal_integer}.>}
@begin{Ramification}
Thus, R.C'Position is the offset of C in storage
elements from the beginning of the object,
where the first storage element of an object is numbered zero.
R'Address + R.C'Position = R.C'Address.
For record extensions, the offset is not measured from
the beginning of the extension part, but from the
beginning of the whole object, as usual.

In ``R.C'Address @en@; R'Address'',
the "@en@;" operator is the one in System.Storage_Elements
that takes two Addresses and returns a Storage_Offset.
@end{Ramification}

@Attribute{Prefix=<R.C>, AttrName=<First_Bit>,
  Text=<Denotes the offset, from the start of the first of the
storage elements occupied by C, of the first bit occupied by C.
This offset is measured in bits.
The first bit of a storage element is numbered zero.
The value of this attribute is of the type
@i{universal_integer}.>}

@Attribute{Prefix=<R.C>, AttrName=<Last_Bit>,
  Text=<Denotes the offset, from the start of the first of the
storage elements occupied by C, of the last bit occupied by C.
This offset is measured in bits. The value of this attribute
is of the type @i{universal_integer}.>}
@begin{Ramification}
The ordering of bits in a storage element is
is defined in @RefSec{Bit Ordering}.

R.C'Size = R.C'Last_Bit @en@; R.C'First_Bit + 1.
(Unless the implementation chooses an indirection
representation.)

If a @nt{component_clause} applies to a component,
then that component will be at the same relative storage place
in all objects of the type.
Otherwise, there is no such requirement.
@end{Ramification}
@end{Description}
@EndPrefixType{}
@end{StaticSem}

@begin{ImplAdvice}
@PDefn{contiguous representation}
@PDefn{discontiguous representation}
If a component is represented using some form of pointer (such as an
offset) to the actual data of the component, and this data is contiguous with
the rest of the object, then the storage place attributes should reflect
the place of the actual data, not the pointer.
If a component is allocated discontiguously from the rest of the object,
then a warning should be generated upon reference to one
of its storage place attributes.
@begin{Reason}
For discontiguous components, these attributes make no sense.
For example, an implementation might allocate dynamic-sized components
on the heap.
For another example, an implementation might allocate the discriminants
separately from the other components, so that multiple objects of the
same subtype can share discriminants.
Such representations cannot happen if there is a @nt{component_clause}
for that component.
@end{Reason}
@end{ImplAdvice}

@LabeledSubClause{Bit Ordering}

@begin{Intro}
@redundant[
The Bit_Order attribute specifies the interpretation of the storage
place attributes.
]
@begin{Reason}
The intention is to provide uniformity in the
interpretation of storage places across implementations on a
particular machine by allowing the user to specify the Bit_Order.
It is not intended to fully support data interoperability across
different machines,
although it can be used for that purpose in some situations.

We can't require all implementations on a given machine to use the same
bit ordering by default;
if the user cares, a @nt{pragma} Bit_Order can be used to force all
implementations to use the same bit ordering.
@end{Reason}
@end{Intro}

@begin{StaticSem}
@Defn{bit ordering}
A bit ordering is a method of interpreting the meaning of the
storage place attributes.
@Defn{High_Order_First}
@Defn{big endian}
@Defn2{Term=[endian], Sec=(big)}
High_Order_First @Redundant[(known in the vernacular as ``big endian'')]
means that the first bit of a storage element
(bit 0) is the most significant bit (interpreting the sequence of
bits that represent a component as an unsigned integer value).
@Defn{Low_Order_First}
@Defn{little endian}
@Defn2{Term=[endian], Sec=(little)}
Low_Order_First @Redundant[(known in the vernacular as ``little endian'')]
means the opposite: the first bit is the least significant.

For @PrefixType{every specific record subtype S},
the following attribute is defined:
@begin{Description}
@Attribute{Prefix=<S>, AttrName=<Bit_Order>,
  Text=<Denotes the bit ordering for the type of S.
The value of this attribute is of type System.Bit_Order.>}
@EndPrefixType{}
@PDefn2{Term=[specifiable], Sec=(of Bit_Order for record types and
record extensions)}
@Defn{Bit_Order clause}
Bit_Order may be specified for specific record types
via an @nt{attribute_definition_clause};
the expression of such a clause shall be static.
@end{Description}

If Word_Size = Storage_Unit,
the default bit ordering is implementation defined.
If Word_Size > Storage_Unit,
the default bit ordering is the same as the ordering of storage
elements in a word, when interpreted as an
integer.
@IndexSee{Term=[byte sex],See=(ordering of storage elements in a word)}
@ImplDef{If Word_Size = Storage_Unit,
the default bit ordering.}
@begin{Ramification}
Consider machines whose Word_Size = 32,
and whose Storage_Unit = 8.
Assume the default bit ordering applies.
On a machine with big-endian addresses,
the most significant storage element of an integer is at the address of
the integer.
Therefore, bit zero of a storage element is the most significant bit.
On a machine with little-endian addresses,
the least significant storage element of an integer is at the address of
the integer.
Therefore, bit zero of a storage element is the least significant
bit.
@end{Ramification}

The storage place attributes of a
component of a type are
interpreted according to the bit ordering
of the type.
@begin{Ramification}
This implies that the interpretation of the @nt{position},
@nt{first_bit}, and @nt{last_bit} of a @nt{component_clause} of a
@nt{record_representation_clause} obey the bit ordering given in a
representation item.
@end{Ramification}
@end{StaticSem}

@begin{ImplAdvice}
@PDefn2{Term=[recommended level of support], Sec=(bit ordering)}
The recommended level of support for the nondefault bit ordering is:
@begin{Itemize}
  If Word_Size = Storage_Unit, then
  the implementation should support the nondefault bit ordering
  in addition to the default bit ordering.
@end{Itemize}
@begin{Ramification}
If Word_Size = Storage_Unit,
the implementation should support both bit orderings.
We don't push for support of the nondefault bit ordering when
Word_Size > Storage_Unit (except
of course for upward compatibility with a preexisting implementation
whose Ada 83 bit order did not correspond to the required Ada 9X
default bit order), because
implementations are required to support storage positions that cross
storage element boundaries when Word_Size > Storage_Unit.
Such storage positions will be split into two or three pieces if the
nondefault bit ordering is used, which could be onerous to support.
However, if Word_Size = Storage_Unit,
there might not be a natural bit ordering,
but the splitting problem need not occur.
@end{Ramification}
@end{ImplAdvice}

@begin{Extend83}
The Bit_Order attribute is new to Ada 9X.
@end{Extend83}

@LabeledClause{Change of Representation}

@begin{Intro}
@redundant[
@Defn{change of representation}
@Defn2{Term=[representation], Sec=(change of)}
A @nt{type_conversion} (see @RefSecNum{Type Conversions})
can be used to convert between two different
representations of the same array or record.
To convert an array from one representation to another,
two array types need to be declared with
matching component subtypes, and convertible index types.
If one type has packing specified and the other does not,
then explicit conversion can be used to pack or unpack an array.

To convert a record from one representation to another,
two record types with a common ancestor type need to be declared,
with no inherited subprograms.  Distinct representations can then
be specified for the record types, and explicit conversion between
the types can be used to effect a change in representation.
]
@begin{Ramification}
This technique does not work if the first type is an
untagged type with user-defined primitive subprograms.
It does not work at all for tagged types.
@end{Ramification}
@end{Intro}

@begin{Examples}
@i{Example of change of representation:}
@begin{Example}
--@i{ Packed_Descriptor and Descriptor are two different types}
--@i{ with identical characteristics, apart from their}
--@i{ representation}

@key[type] Descriptor @key[is]
    @key[record]
      --@i{ components of a descriptor}
    @key[end] @key[record];

@key[type] Packed_Descriptor @key[is] @key[new] Descriptor;

@key[for] Packed_Descriptor @key[use]
    @key[record]
      --@i{ component clauses for some or for all components}
    @key[end] @key[record];

@i{-- Change of representation can now be accomplished by explicit type conversions:}

D : Descriptor;
P : Packed_Descriptor;

P := Packed_Descriptor(D);  --@i{ pack D}
D := Descriptor(P);         --@i{ unpack P}
@end{Example}
@end{Examples}

@LabeledClause{The Package System}

@begin{Intro}
@redundant[
For each implementation there is a library package called System
which includes the definitions of certain configuration-dependent
characteristics.
]
@end{Intro}

@begin{StaticSem}
The following language-defined library package exists:
@ImplDef{The contents of the visible part of package System
and its language-defined children.}
@begin{Example}
@RootLibUnit{System}
@key[package] System @key[is]
   @key{pragma} Preelaborate(System);

@LangDefType{Package=[System],Type=[Name]}
   @key[type] Name @key[is] @i{implementation-defined-enumeration-type};
   System_Name : @key[constant] Name := @i{implementation-defined};
@Hinge{}


   --@i{ System-Dependent Named Numbers:}

   Min_Int               : @key[constant] := @i{root_integer}'First;
   Max_Int               : @key[constant] := @i{root_integer}'Last;
@Defn2{Term=[Min_Int], Sec=(named number in package System)}
@Defn2{Term=[Max_Int], Sec=(named number in package System)}

   Max_Binary_Modulus    : @key[constant] := @i{implementation-defined};
   Max_Nonbinary_Modulus : @key[constant] := @i{implementation-defined};
@Defn2{Term=[Max_Binary_Modulus], Sec=(named number in package System)}
@Defn2{Term=[Max_Nonbinary_Modulus], Sec=(named number in package System)}

   Max_Base_Digits       : @key[constant] := @i{root_real}'Digits;
   Max_Digits            : @key[constant] := @i{implementation-defined};
@Defn2{Term=[Max_Base_Digits], Sec=(named number in package System)}
@Defn2{Term=[Max_Digits], Sec=(named number in package System)}

   Max_Mantissa          : @key[constant] := @i{implementation-defined};
   Fine_Delta            : @key[constant] := @i{implementation-defined};
@Defn2{Term=[Max_Mantissa], Sec=(named number in package System)}
@Defn2{Term=[Fine_Delta], Sec=(named number in package System)}

   Tick                  : @key[constant] := @i{implementation-defined};
@Defn2{Term=[Tick], Sec=(named number in package System)}

   --@i{ Storage-related Declarations:}

   @key[type] Address @key[is] @i{implementation-defined};
   Null_Address : @key[constant] Address;
@LangDefType{Package=[System],Type=[Address]}
@Defn2{Term=[address], Sec=(null)}
@Defn2{Term=[Null_Address], Sec=(constant in System)}

   Storage_Unit : @key[constant] := @i{implementation-defined};
   Word_Size    : @key[constant] := @i{implementation-defined} * Storage_Unit;
   Memory_Size  : @key[constant] := @i{implementation-defined};
@Defn2{Term=[Storage_Unit], Sec=(named number in package System)}
@Defn2{Term=[Word_Size], Sec=(named number in package System)}
@Hinge{}

   --@i{ @Defn2{Term=[address], Sec=(comparison)}Address Comparison:}
   @key(function) "<" (Left, Right : Address) @key(return) Boolean;
   @key(function) "<="(Left, Right : Address) @key(return) Boolean;
   @key(function) ">" (Left, Right : Address) @key(return) Boolean;
   @key(function) ">="(Left, Right : Address) @key(return) Boolean;
   @key(function) "=" (Left, Right : Address) @key(return) Boolean;
-- @key(function) "/=" (Left, Right : Address) @key(return) Boolean;
   --@i{ "/=" is implicitly defined}
   @key[pragma] Convention(Intrinsic, "<");
   ... --@i{ and so on for all language-defined subprograms in this package}
@Hinge{}


   --@i{ Other System-Dependent Declarations:}
@LangDefType{Package=[System],Type=[Bit_Order]}
   @key[type] Bit_Order @key[is] (High_Order_First, Low_Order_First);
   Default_Bit_Order : @key[constant] Bit_Order;


   --@i{ Priority-related declarations (see @RefSecNum{Task Priorities}):}
   @key{subtype} Any_Priority @key{is} Integer @key{range} @i{implementation-defined};
   @key{subtype} Priority @key{is} Any_Priority @key{range} Any_Priority'First .. @i{implementation-defined};
   @key{subtype} Interrupt_Priority @key{is} Any_Priority @key{range} Priority'Last+1 .. Any_Priority'Last;

   Default_Priority : @key{constant} Priority := (Priority'First + Priority'Last)/2;

@key[private]
   ... -- @i{not specified by the language}
@key[end] System;
@end{Example}


Name is an enumeration subtype.
Values of type Name are the names of alternative machine
configurations handled by the implementation.
System_Name represents the current machine configuration.

The named numbers Fine_Delta and Tick are of the type
@i{universal_real}; the others are of the type @i{universal_integer}.

The meanings of the named numbers are:
@begin{Description}
@Redundant[
Min_Int @\The smallest (most negative) value
allowed for the expressions of a
@nt{signed_integer_type_definition}.

Max_Int @\The largest (most positive) value
allowed for the expressions of a
@nt{signed_integer_type_definition}.

Max_Binary_Modulus @\A power of two such that it,
and all lesser positive powers of two, are allowed
as the modulus of a @nt<modular_type_definition>.

Max_Nonbinary_Modulus @\A value such that it,
and all lesser positive integers, are allowed
as the modulus of a @nt<modular_type_definition>.
@begin{Ramification}
There is no requirement that Max_Nonbinary_Modulus
be less than or equal to Max_Binary_Modulus,
although that's what makes most sense.
On a typical 32-bit machine, for example,
Max_Binary_Modulus will be 2**32
and Max_Nonbinary_Modulus will be 2**31,
because supporting nonbinary moduli in above 2**31
causes implementation difficulties.
@end{Ramification}

Max_Base_Digits @\The largest value allowed for the requested decimal
precision in a @nt{floating_point_definition}.

Max_Digits @\The largest value allowed for the requested decimal
precision in a @nt{floating_point_definition}
that has no @nt{real_range_specification}.
Max_Digits is less than or equal to Max_Base_Digits.

Max_Mantissa @\The largest possible number of binary digits in the mantissa
of machine numbers of a user-defined ordinary fixed point type.
(The mantissa is defined in @RefSecNum{Numerics}.)

Fine_Delta @\The smallest delta allowed in an @nt{ordinary_fixed_point_definition} that
has the @nt{real_range_specification} @key{range} @en@;1.0 .. 1.0.
]

Tick @\A period in seconds approximating the real time interval during
which the value of Calendar.Clock remains constant.
@begin{Ramification}
There is no required relationship between System.Tick and
Duration'Small,
other than the one described here.

The inaccuracy of the @nt{delay_statement} has no relation to Tick.
In particular, it is possible that the clock used for the
@nt{delay_statement} is less accurate than Calendar.Clock.

We considered making Tick a run-time-determined quantity,
to allow for easier configurability.
However, this would not be upward compatible,
and the desired configurability can be achieved using
functionality defined in @RefSec{Real-Time Systems}.
@end{Ramification}

Storage_Unit @\The number of bits per storage element.

Word_Size @\The number of bits per word.

Memory_Size @\An implementation-defined value
@Redundant[that is intended to reflect the memory size of the
configuration in storage elements.]
@begin{Discussion}
It is unspecified whether this refers to the size of the
address space, the amount of physical memory on the machine,
or perhaps some other interpretation of ``memory size.''
In any case, the value has to be given by a static expression,
even though the amount of memory on many modern machines is
a dynamic quantity in several ways.
Thus, Memory_Size is not very useful.
@end{Discussion}
@end{Description}

Address is of a definite, nonlimited type.
Address represents machine addresses capable of addressing individual
storage elements.
Null_Address is an address that is distinct from the
address of any object or program unit.
@IndexSee{Term=[pointer],See=(type System.Address)}
@begin{Ramification}
The implementation has to ensure that there is at least one
address that nothing will be allocated to;
Null_Address will be one such address.
@end{Ramification}
@begin{Ramification}
Address is the type of the result of the attribute Address.
@end{Ramification}
@begin{Reason}
Address is required to be nonlimited and definite because
it is important to be able to assign addresses,
and to declare uninitialized address variables.
@end{Reason}

See @RefSecNum{Bit Ordering} for an explanation of Bit_Order and
Default_Bit_Order.
@end{StaticSem}

@begin{ImplPerm}
An implementation may add additional implementation-defined
declarations to package System and its children.
@Redundant[However, it is usually better
for the implementation to provide
additional functionality via implementation-defined
children of System.]
Package System may be declared pure.
@begin{Ramification}
The declarations in package System and its children can be implicit.
For example, since Address is not limited,
the predefined "=" and "/=" operations are probably sufficient.
However, the implementation is not @i{required} to use the predefined
"=".
@end{Ramification}
@end{ImplPerm}

@begin{ImplAdvice}
Address should be of a private type.
@begin{Reason}
This promotes uniformity by avoiding having
implementation-defined predefined operations for the type.
We don't require it, because implementations may want to stick with
what they have.
@end{Reason}
@begin{ImplNote}
It is not necessary for Address to be able to point at
individual bits within a storage element.
Nor is it necessary for it to be able to point at machine registers.
It is intended as a memory address that matches the hardware's notion
of an address.

The representation of the @key{null} value of a general access type should
be the same as that of Null_Address;
instantiations of Unchecked_Conversion should work accordingly.
If the implementation supports interfaces to other languages,
the representation of the @key{null} value of a general access type
should be the same as in those other languages,
if appropriate.

Note that the children of the Interfaces package will generally provide
foreign-language-specific null values where appropriate.
See UI-0065 regarding Null_Address.
@end{ImplNote}
@end{ImplAdvice}

@begin{Notes}
There are also some language-defined child packages of System
defined elsewhere.
@end{Notes}

@begin{DiffWord83}
Much of the content of System is standardized,
to provide more uniformity across implementations.
Implementations can still add their own declarations to System,
but are encouraged to do so via children of System.

Some of the named numbers are defined more explicitly in terms of the
standard numeric types.

The pragmas System_Name, Storage_Unit, and Memory_Size are no
longer defined by the language.
However, the corresponding declarations in package
System still exist.
Existing implementations may continue to support the three
pragmas as implementation-defined pragmas,
if they so desire.

Priority semantics, including subtype Priority,
have been moved to the Real Time Annex.
@end{DiffWord83}

@LabeledSubClause{The Package System.Storage_Elements}

@begin{StaticSem}
The following language-defined library package exists:
@begin{Example}
@ChildUnit{Parent=[System],Child=[Storage_Elements],Expanded=[System.Storage_Elements]}
@key[package] System.Storage_Elements @key[is]
   @key{pragma} Preelaborate(System.Storage_Elements);

@LangDefType{Package=[System.Storage_Elements],Type=[Storage_Offset]}
   @key[type] Storage_Offset @key[is] @key[range] @i(implementation-defined);
@Defn2{Term=[Storage_Count], Sec=(subtype in package System.Storage_Elements)}

   @key[subtype] Storage_Count @key[is] Storage_Offset @key[range] 0..Storage_Offset'Last;
@Hinge{}

@LangDefType{Package=[System.Storage_Elements],Type=[Storage_Element]}
   @key[type] Storage_Element @key[is] @key[mod] @i{implementation-defined};
   @key[for] Storage_Element'Size @key[use] Storage_Unit;
@LangDefType{Package=[System.Storage_Elements],Type=[Storage_Array]}
   @key[type] Storage_Array @key[is] @key[array]
     (Storage_Offset @key[range] <>) @key[of] @key[aliased] Storage_Element;
   @key[for] Storage_Array'Component_Size @key[use] Storage_Unit;
@Hinge{}

   --@i{ @Defn2{Term=[address], Sec=(arithmetic)}Address Arithmetic:}

   @key(function) "+"(Left : Address; Right : Storage_Offset)
     @key(return) Address;
   @key(function) "+"(Left : Storage_Offset; Right : Address)
     @key(return) Address;
   @key(function) "-"(Left : Address; Right : Storage_Offset)
     @key(return) Address;
   @key(function) "-"(Left, Right : Address)
     @key(return) Storage_Offset;
@Hinge{}

   @key(function) "@key(mod)"(Left : Address; Right : Storage_Offset)
     @key(return) Storage_Offset;
@Hinge{}

   --@i{ Conversion to/from integers:}

@LangDefType{Package=[System.Storage_Elements],Type=[Integer_Address]}
   @key[type] Integer_Address @key[is] @i{implementation-defined};
   @key[function] To_Address(Value : Integer_Address) @key[return] Address;
   @key[function] To_Integer(Value : Address) @key[return] Integer_Address;
@Hinge{}

   @key[pragma] Convention(Intrinsic, "+");
      @i(-- ...and so on for all language-defined subprograms declared in this package.)
@key[end] System.Storage_Elements;
@end{Example}
@begin{Reason}
The Convention @nt{pragma}s imply that
the attribute Access is not allowed for those operations.

The @key(mod) function is needed so that the
definition of Alignment makes sense.
@end{Reason}

Storage_Element represents a storage element.
Storage_Offset represents an offset in storage elements.
Storage_Count represents a number of storage elements.
@PDefn{contiguous representation}
@PDefn{discontiguous representation}
Storage_Array represents a contiguous sequence of storage elements.
@begin{Reason}
The index subtype of Storage_Array is Storage_Offset
because we wish to allow maximum flexibility.
Most Storage_Arrays will probably have a lower bound of 0 or 1,
but other lower bounds, including negative ones,
make sense in some situations.

Note that there are some language-defined subprograms that fill part of
a Storage_Array, and return the index of the last element filled as a
Storage_Offset.
The Read procedures in
Streams (see @RefSecNum{The Package Streams}),
Streams.Stream_IO (see @RefSecNum{The Package Streams.Stream_IO}),
and System.RPC (see @RefSecNum{Partition Communication Subsystem})
behave in this manner.
These will raise Constraint_Error if the resulting Last value is not in
Storage_Offset.
This implies that the Storage_Array passed to these subprograms should
not have a lower bound of Storage_Offset'First,
because then a read of 0 elements would always raise Constraint_Error.
A better choice of lower bound is 1.
@end{Reason}


Integer_Address is a @Redundant[(signed or modular)] integer subtype.

To_Address and To_Integer convert back and forth between
this type and Address.
@end{StaticSem}

@begin{ImplReq}
Storage_Offset'Last shall be greater than or equal to Integer'Last or
the largest possible storage offset,
whichever is smaller.
Storage_Offset'First shall be <= (@en@;Storage_Offset'Last).
@end{ImplReq}

@begin{ImplPerm}
Package System.Storage_Elements may be declared pure.
@end{ImplPerm}

@begin{ImplAdvice}
Operations in System and its children should reflect the
target environment semantics as closely as is reasonable.
For example, on most machines, it makes sense for address arithmetic
to ``wrap around.''
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
Operations that do not make sense should raise
Program_Error.
@begin{Discussion}
For example, on a segmented architecture,
X < Y might raise Program_Error if X and Y do not point at the same
segment (assuming segments are unordered).
Similarly, on a segmented architecture,
the conversions between Integer_Address and Address
might not make sense for some values,
and so might raise Program_Error.
@end{Discussion}
@begin{Reason}
We considered making Storage_Element a private type.
However, it is better to declare it as a modular type in the visible part,
since code that uses it is already low level,
and might as well have access to the underlying representation.
We also considered allowing Storage_Element to be any
integer type, signed integer or modular,
but it is better to have uniformity across implementations
in this regard,
and viewing storage elements as unsigned seemed to make the most
sense.
@end{Reason}
@begin{ImplNote}
To_Address is intended for use in Address clauses.
Implementations should overload To_Address if appropriate.
For example, on a segmented architecture,
it might make sense to have a record type representing a
segment/offset pair, and have a To_Address conversion that
converts from that record type to type Address.
@end{ImplNote}
@end{ImplAdvice}

@LabeledSubClause{The Package System.Address_To_Access_Conversions}

@begin{StaticSem}

The following language-defined generic library package exists:
@begin{Example}
@ChildUnit{Parent=[System],Child=[Address_To_Access_Conversions],Expanded=[System.Address_To_Access_Conversions]}
@key[generic]
    @key[type] Object(<>) @key[is] @key[limited] @key[private];
@key[package] System.Address_To_Access_Conversions @key[is]
   @key[pragma] Preelaborate(Address_To_Access_Conversions);

   @key[type] Object_Pointer @key[is] @key[access] @key[all] Object;
   @key[function] To_Pointer(Value : Address) @key[return] Object_Pointer;
   @key[function] To_Address(Value : Object_Pointer) @key[return] Address;

   @key[pragma] Convention(Intrinsic, To_Pointer);
   @key[pragma] Convention(Intrinsic, To_Address);
@key[end] System.Address_To_Access_Conversions;
@end{Example}



The To_Pointer and To_Address subprograms
convert back and forth between
values of types Object_Pointer and Address.
To_Pointer(X'Address) is equal to X'Unchecked_Access
for any X that allows Unchecked_Access.
To_Pointer(Null_Address) returns @key[null].
@PDefn{unspecified}
For other addresses,
the behavior is unspecified.
To_Address(@key[null]) returns Null_Address (for @key[null] of the
appropriate type).
To_Address(Y), where Y /= @key[null], returns Y.@key[all]'Address.
@begin{Discussion}
The programmer should ensure that
the address passed to To_Pointer is either Null_Address,
or the address of an object of type Object.
Otherwise, the behavior of the program is unspecified;
it might raise an exception or crash, for example.
@end{Discussion}
@begin{Reason}
Unspecified is almost the same thing as erroneous;
they both allow arbitrarily bad behavior.
We don't say erroneous here, because the implementation
might allow the address passed to To_Pointer to point
at some memory that just happens to ``look like'' an
object of type Object.
That's not necessarily an error; it's just not portable.
However, if the actual type passed to Object is (for example)
an array type, the programmer
would need to be aware of any dope that the implementation expects to
exist, when passing an address that did not come from the Address
attribute of an object of type Object.

One might wonder why To_Pointer and To_Address are any better than
unchecked conversions.  The answer is that Address does not necessarily
have the same representation as an access type.  For example, an access
value might point at the bounds of an array when an address would point
at the first element.  Or an access value might be an offset in words
from someplace, whereas an address might be an offset in bytes from the
beginning of memory.
@end{Reason}
@end{StaticSem}

@begin{ImplPerm}
An implementation may place restrictions on instantiations of
Address_To_Access_Conversions.
@begin{Ramification}
For example, if the hardware requires aligned loads and stores,
then dereferencing an access value that is not properly aligned might
raise an exception.

For another example,
if the implementation has chosen to use negative component offsets
(from an access value),
it might not be possible to preserve the semantics,
since negative offsets from the Address are not allowed.
(The Address attribute always points at
``the first of the storage elements....'')
Note that while the implementation knows how to convert an access
value into an address, it might not be able to do the reverse.
To avoid generic contract model violations,
the restriction might have to be detected at run time in some cases.
@end{Ramification}
@end{ImplPerm}

@LabeledClause{Machine Code Insertions}

@begin{Intro}
@redundant[
@Defn{machine code insertion}
A machine code insertion can be achieved by a call to a subprogram whose
@nt{sequence_of_statements} contains @nt{code_statement}s.
]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<code_statement>,rhs="@Syn2{qualified_expression};"}

@begin{SyntaxText}
A @nt{code_statement} is only allowed in the
@nt{handled_sequence_of_statements} of a @nt{subprogram_body}.
If a @nt{subprogram_body} contains any @nt{code_statement}s, then within
this @nt{subprogram_body} the only allowed form of @nt{statement} is a
@nt{code_statement} (labeled or not),
the only allowed @nt{declarative_item}s are @nt{use_clause}s,
and no @nt{exception_handler} is allowed (@nt{comment}s and
@nt{pragma}s are allowed as usual).
@end{SyntaxText}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected type],
  Sec=(code_statement)}
The @nt{qualified_expression} is expected to be of any type.
@end{Resolution}

@begin{Legality}
The @nt{qualified_expression} shall be of a
type declared in package System.Machine_Code.
@begin{Ramification}
This includes types declared in children of System.Machine_Code.
@end{Ramification}

A @nt<code_statement> shall appear only within the scope of
a @nt<with_clause> that mentions package System.Machine_Code.
@begin{Ramification}
Note that this is not a note;
without this rule, it would be possible to write machine code
in compilation units which depend on System.Machine_Code only indirectly.
@end{Ramification}
@end{Legality}

@begin{StaticSem}
@Defn{System.Machine_Code}
@ChildUnit{Parent=[System],Child=[Machine_Code],Expanded=[System.Machine_Code]}
The contents of the library package System.Machine_Code
(if provided) are implementation defined.
The meaning of @nt{code_statement}s is implementation defined.
@Redundant{Typically, each @nt{qualified_expression}
represents a machine instruction or assembly directive.}
@begin{Discussion}
For example, an instruction might be a record with an Op_Code
component and other components for the operands.
@end{Discussion}
@ImplDef{The contents of the visible part of package System.Machine_Code,
and the meaning of @nt{code_statement}s.}
@end{StaticSem}

@begin{ImplPerm}
An implementation may place restrictions on @nt{code_statement}s.
An implementation is not required to provide package System.Machine_Code.
@end{ImplPerm}

@begin{Notes}
An implementation may provide implementation-defined pragmas
specifying register conventions and calling conventions.

Machine code functions are exempt from the rule that a
@nt{return_statement} is required.
In fact, @nt{return_statement}s are forbidden,
since only @nt{code_statement}s are allowed.
@begin{Discussion}
The idea is that the author of a machine code subprogram knows
the calling conventions, and refers to parameters and results
accordingly.
The implementation should document where to put the result of a
machine code function, for example,
``Scalar results are returned in register 0.''
@end{Discussion}

Intrinsic subprograms (see @RefSec{Conformance Rules})
can also be used to achieve machine code insertions.
Interface to assembly language can be achieved
using the features in @RefSec{Interface to Other Languages}.
@end{Notes}

@begin{Examples}
@i{Example of a code statement:}
@begin{Example}
M : Mask;
@key[procedure] Set_Mask; @key[pragma] Inline(Set_Mask);

@key[procedure] Set_Mask @key[is]
  @key[use] System.Machine_Code; --@i{ assume ``@key[with] System.Machine_Code;'' appears somewhere above}
@key[begin]
  SI_Format'(Code => SSM, B => M'Base_Reg, D => M'Disp);
  --@i{  Base_Reg and Disp are implementation-defined attributes}
@key[end] Set_Mask;
@end{Example}
@end{Examples}

@begin{Extend83}
Machine code functions are allowed in Ada 9X;
in Ada 83, only procedures were allowed.
@end{Extend83}

@begin{DiffWord83}
The syntax for @nt{code_statement} is changed to say
``@nt{qualified_expression}'' instead of
``@Syn2{subtype_mark}'@Syn2{record_aggregate}''.
Requiring the type of each instruction to be a record type is
overspecification.
@end{DiffWord83}

@LabeledClause{Unchecked Type Conversions}

@begin{Intro}
@redundant[
@Defn{unchecked type conversion}
@Defn2{Term=[type conversion], Sec=(unchecked)}
@Defn2{Term=[conversion], Sec=(unchecked)}
@IndexSeeAlso{Term=[type_conversion],See=(unchecked type conversion)}
@IndexSee{Term=[cast],See=(unchecked type conversion)}
An unchecked type conversion can be achieved by a call to an instance
of the generic function Unchecked_Conversion.
]
@end{Intro}

@begin{StaticSem}
The following language-defined generic library function exists:
@begin{Example}
@key[generic]
   @key[type] Source(<>) @key[is] @key[limited] @key[private];
   @key[type] Target(<>) @key[is] @key[limited] @key[private];
@ChildUnit{Parent=[Ada],Child=[Unchecked_Conversion],Expanded=[Ada.Unchecked_Conversion]}
@key[function] Ada.Unchecked_Conversion(S : Source) @key[return] Target;
@key[pragma] Convention(Intrinsic, Ada.Unchecked_Conversion);
@key[pragma] Pure(Ada.Unchecked_Conversion);
@end{Example}
@begin{Reason}
The @nt{pragma} Convention implies that
the attribute Access is not allowed
for instances of Unchecked_Conversion.
@end{Reason}
@end{StaticSem}

@begin{RunTime}
The size of the formal parameter S in an instance of
Unchecked_Conversion is that of its subtype.
@Redundant[This is the actual subtype passed to Source,
except when the actual is an unconstrained composite subtype,
in which case the subtype is constrained by the bounds or
discriminants of the value of the actual expression passed to S.]

If all of the following are true,
the effect of an unchecked conversion is to return the
value of an object of the target subtype whose representation is the
same as that of the source object S:
@begin{Itemize}
S'Size = Target'Size.
@begin{Ramification}

Note that there is no requirement that the Sizes be known at compile
time.

@end{Ramification}

S'Alignment = Target'Alignment.

The target subtype is not an unconstrained composite subtype.

@PDefn{contiguous representation}
@PDefn{discontiguous representation}
S and the target subtype both have a contiguous
representation.

The representation of S is a representation of
an object of the target subtype.
@end{Itemize}

Otherwise, the effect is implementation defined;
in particular, the result can be abnormal
(see @RefSecNum{Data Validity}).
@ImplDef{The effect of unchecked conversion.}
@begin{Ramification}
Whenever unchecked conversions are used, it is the programmer's
responsibility to ensure that these conversions maintain the properties
that are guaranteed by the language for objects of the target type.
This requires the user to understand the underlying run-time model of
the implementation.
The execution of a program that violates these properties by means of
unchecked conversions is erroneous.

An instance of Unchecked_Conversion can be applied to an object of a
private type,
assuming the implementation allows it.
@end{Ramification}
@end{RunTime}

@begin{ImplPerm}
An implementation may return the result of
an unchecked conversion
by reference, if the Source type is not a by-copy type.
@Redundant[In this case, the result of the unchecked conversion represents
simply a different (read-only) view of the operand of the conversion.]
@begin{Ramification}
  In other words, the result object of a call on an instance
  of Unchecked_Conversion can occupy the same storage as the
  formal parameter S.
@end{Ramification}

An implementation may place restrictions on Unchecked_Conversion.
@begin{Ramification}
For example, an instantiation of Unchecked_Conversion for types for
which unchecked conversion doesn't make sense may be disallowed.
@end{Ramification}
@end{ImplPerm}

@begin{ImplAdvice}
The Size of an array object should not include its bounds;
hence, the bounds should not be part of the converted data.
@begin{Ramification}
On the other hand, we have no advice to offer about
discriminants and tag fields.
@end{Ramification}

The implementation should not generate unnecessary run-time checks
to ensure that the representation of S is a representation of the target
type.
It should take advantage of the permission to return by reference
when possible.
Restrictions on unchecked conversions should be avoided
unless required by the target environment.
@begin{ImplNote}

As an example of an unnecessary run-time check,
consider a record type with gaps between components.
The compiler might assume that such gaps are always zero bits.
If a value is produced that does not obey that assumption,
then the program might misbehave.
The implementation should not generate extra code to check for zero bits
(except, perhaps, in a special error-checking mode).

@end{ImplNote}

@PDefn2{Term=[recommended level of support], Sec=(unchecked conversion)}
The recommended level of support for unchecked conversions is:
@begin{Itemize}
Unchecked conversions should be supported and should be reversible in
the cases where this clause defines the result.
@PDefn{contiguous representation}
@PDefn{discontiguous representation}
To enable meaningful use of unchecked conversion,
a contiguous representation should be used for elementary subtypes,
for statically constrained array subtypes whose component subtype is
one of the subtypes described in this paragraph,
and for record subtypes without discriminants
whose component subtypes are described in this paragraph.
@end{Itemize}
@end{ImplAdvice}

@LabeledSubClause{Data Validity}

@begin{Intro}
Certain actions that can potentially lead to erroneous execution are not
directly erroneous,
but instead can cause objects to become @i{abnormal}.
Subsequent uses of abnormal objects can be erroneous.

A scalar object can have an @i{invalid representation},
which means that the object's representation does not represent any
value of the object's subtype.
@RootDefn{uninitialized variables}
The primary cause of invalid representations is uninitialized variables.

Abnormal objects and invalid representations are explained in this
subclause.
@end{Intro}

@begin{RunTime}
@RootDefn{normal state of an object}
@RootDefn{abnormal state of an object}
When an object is first created, and any explicit or default
initializations have been performed, the object
and all of its parts are in the @i{normal} state.
Subsequent operations generally leave them normal.
However, an object or part of an object can become @i{abnormal}
in the following ways:
@begin{Itemize}
@Defn{disruption of an assignment}
An assignment to the object is disrupted due to an abort
(see @RefSecNum{Abort of a Task - Abort of a Sequence of Statements})
or due to the failure of a language-defined check
(see @RefSecNum{Exceptions and Optimization}).

The object is not scalar, and is passed to an @key[in out]
or @key[out] parameter
of an imported procedure or language-defined input procedure,
if after return from the procedure the representation of the parameter
does not represent a value of the parameter's subtype.
@end{Itemize}

@PDefn{unspecified}
Whether or not an object actually becomes abnormal in these cases is
not specified.
An abnormal object becomes normal again upon successful completion of
an assignment to the object as a whole.
@end{RunTime}

@begin{Erron}
It is erroneous to evaluate a @nt<primary> that is a @nt<name>
denoting an abnormal object,
or to evaluate a @nt{prefix} that denotes an abnormal object.
@begin{Ramification}
Although a composite object with no subcomponents of an access type,
and with static constraints all the way down cannot become abnormal,
a scalar subcomponent of such an object can become abnormal.

The @key[in out] or @key[out] parameter case does not apply to scalars;
bad scalars are merely invalid representations,
rather than abnormal, in this case.
@end{Ramification}
@begin{Reason}
The reason we allow access objects, and objects containing subcomponents of an
access type, to become abnormal is because the correctness of an access
value cannot necessarily be determined merely by looking at the bits of
the object.
The reason we allow scalar objects to become abnormal is that we wish to
allow the compiler to optimize assuming that the value of a scalar
object belongs to the object's subtype,
if the compiler can prove that the object is initialized with a value
that belongs to the subtype.
The reason we allow composite objects to become abnormal if some
constraints are nonstatic is that such object might be represented with
implicit levels of indirection;
if those are corrupted, then even assigning into a component of the
object, or simply asking for its Address, might have an unpredictable
effect.
The same is true if the discriminants have been destroyed.
@end{Reason}
@end{Erron}

@begin{Bounded}
@Defn{invalid representation}
If the representation of a scalar object does not represent a value of
the object's subtype
(perhaps because the object was not initialized),
the object is said to have an @i{invalid representation}.
It is a bounded error to evaluate the value of such
an object.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
If the error is detected, either Constraint_Error or Program_Error is
raised.
Otherwise, execution continues using the invalid representation.
The rules of the language outside this subclause assume that all objects
have valid representations.
The semantics of operations on invalid representations are as follows:

@begin{Discussion}

The AARM is more explicit about what happens when
the value of the case expression is an invalid representation.

@end{Discussion}
@begin{Itemize}
If the representation of the object represents a value of the object's
type, the value of the type is used.

If the representation of the object does not represent a value of the
object's type,
the semantics of operations on such representations is
implementation-defined, but does not by itself lead to
erroneous or unpredictable
execution, or to other objects becoming abnormal.
@end{Itemize}
@end{Bounded}

@begin{Erron}
A call to an imported function or an instance of Unchecked_Conversion is
erroneous if the result is scalar, and the result object has an invalid
representation.
@begin{Ramification}
In a typical implementation, every bit pattern that fits in an object of
an integer subtype will represent a value of the type,
if not of the subtype.
However, for an enumeration or floating point type,
there are typically bit patterns that do not represent any
value of the type.
In such cases, the implementation ought to define the semantics of
operations on the invalid representations in the obvious manner
(assuming the bounded error is not detected):
a given representation should be equal to itself,
a representation that is in between the internal codes of
two enumeration literals should behave accordingly when passed to
comparison operators and membership tests,
etc.
We considered @i{requiring} such sensible behavior,
but it resulted in too much arcane verbiage,
and since implementations have little incentive to behave
irrationally, such verbiage is not important to have.

If a stand-alone scalar object is initialized to a an in-range
value, then the implementation can take advantage of the fact
that any out-of-range value has to be abnormal.
Such an out-of-range value can be produced only by things like
unchecked conversion, input, and
disruption of an assignment due to abort or to failure of a
language-defined check.
This depends on out-of-range values being checked before
assignment (that is, checks are not optimized away unless they
are proven redundant).

Consider the following example:
@begin{Example}
@key[type] My_Int @key[is] @key[range] 0..99;
@key[function] Safe_Convert @key[is] @key[new] Unchecked_Conversion(My_Int, Integer);
@key[function] Unsafe_Convert @key[is] @key[new] Unchecked_Conversion(My_Int, Positive);
X : Positive := Safe_Convert(0); --@i{ Raises Constraint_Error.}
Y : Positive := Unsafe_Convert(0); --@i{ Erroneous.}
@end{Example}

The call to Unsafe_Convert causes erroneous execution.
The call to Safe_Convert is not erroneous.
The result object is an object of subtype Integer containing the
value 0.
The assignment to X is required to do a constraint check;
the fact that the conversion is unchecked does not obviate the need for
subsequent checks required by the language rules.
@end{Ramification}
@begin{ImplNote}
  If an implementation wants to have a ``friendly'' mode, it
  might always assign an uninitialized scalar a default initial value
  that is outside the object's subtype (if there is one), and check
  for this value on some or all reads of the object, so as to help
  detect references to uninitialized scalars.
  Alternatively, an implementation might want to provide an
  ``unsafe'' mode where it presumed even uninitialized scalars
  were always within their subtype.
@end{ImplNote}
@begin{Ramification}
  The above rules imply that it is a bounded error
  to apply a predefined operator to
  an object with a scalar subcomponent having an invalid representation,
  since this implies reading the value of each subcomponent.
  Either Program_Error or Constraint_Error is raised,
  or some result is produced, which if composite, might have a corresponding
  scalar subcomponent still with an invalid representation.

  Note that it is not an error to assign, convert, or pass as a parameter
  a composite object with an uninitialized scalar subcomponent.
  In the other hand, it is a (bounded) error to apply
  a predefined operator such as =, <, and @key(xor)
  to a composite operand with an invalid scalar subcomponent.
@end{Ramification}

The dereference of an access value is erroneous if
it does not designate an object of an appropriate type or a subprogram
with an appropriate profile,
if it designates a nonexistent object,
or if it is an access-to-variable value that designates a
constant object.
@Redundant[Such an access value can exist, for example, because of
Unchecked_Deallocation, Unchecked_Access, or Unchecked_Conversion.]
@begin{Ramification}
The above mentioned Unchecked_... features are not the only causes
of such access values.
For example, interfacing to other languages can also cause the problem.

One obscure example is if the Adjust subprogram of a controlled type
uses Unchecked_Access to create an access-to-variable value designating
a subcomponent of its controlled parameter, and saves this access value
in a global object.
When Adjust is called during the initialization of a constant object of
the type,
the end result will be an access-to-variable value that designates a
constant object.
@end{Ramification}
@end{Erron}

@begin{Notes}
Objects can become abnormal due to other kinds of actions that directly
update the object's representation;
such actions are generally considered directly erroneous, however.
@end{Notes}

@begin{DiffWord83}
In order to reduce the amount of erroneousness,
we separate the concept of an undefined value into
objects with invalid representation (scalars only)
and abnormal objects.

Reading an object with an invalid representation is a bounded error
rather than erroneous;
reading an abnormal object is still erroneous.
In fact, the only safe thing to do to an abnormal object is
to assign to the object as a whole.
@end{DiffWord83}

@LabeledSubClause{The Valid Attribute}

@begin{Intro}
The Valid attribute can be used to check the validity of data produced
by unchecked conversion, input, interface to foreign languages,
and the like.
@end{Intro}

@begin{StaticSem}
For @PrefixType{a
@nt<prefix> X that denotes a scalar object
@Redundant[(after any implicit dereference)]},
the following attribute is defined:
@begin(description)
@Attribute{Prefix=<X>, AttrName=<Valid>,
  Text=<Yields True if and only if
the object denoted by X is normal and has a valid
representation.
The value of this attribute is of the predefined type Boolean.>}
@begin{Ramification}
  Having checked that X'Valid is True, it is safe to read the
  value of X without fear of erroneous execution
  caused by abnormality,
  or a bounded error caused by an invalid representation.
  Such a read will produce a value in the subtype of X.
@end{Ramification}
@end{description}
@EndPrefixType{}
@end{StaticSem}

@begin{Notes}
Invalid data can be created in the following cases
(not counting erroneous or unpredictable execution):
@begin{Itemize}
an uninitialized scalar object,

the result of an unchecked conversion,

input,

interface to another language (including machine code),

aborting an assignment,

disrupting an assignment due to the failure
of a language-defined check
(see @RefSecNum{Exceptions and Optimization}), and

use of an object whose Address has been specified.
@end{Itemize}

X'Valid is not considered to be a read of X;
hence, it is not an error to check the validity
of invalid data.
@begin{Ramification}
If X is of an enumeration type with a representation clause, then
X'Valid checks that the value of X when viewed as an integer is one of
the specified internal codes.
@end{Ramification}
@begin{Reason}
Valid is defined only for scalar objects because the implementation
and description burden would be too high for other types.
For example, given a typical run-time model, it is impossible to check
the validity of an access value.
The same applies to composite types implemented with internal pointers.
One can check the validity of a composite object by checking the
validity of each of its scalar subcomponents.
The user should ensure that any composite types that need to be checked
for validity are represented in a way that does not involve
implementation-defined components, or gaps between components.
Furthermore, such types should not contain access subcomponents.

Note that one can safely check the validity of a composite object with
an abnormal value only if the constraints on the object and all of its
subcomponents are static.
Otherwise, evaluation of the @nt{prefix} of the @nt{attribute_reference}
causes erroneous execution (see @RefSecNum{Names}).
@end{Reason}
@end{Notes}

@begin{Extend83}
X'Valid is new in Ada 9X.
@end{Extend83}

@LabeledClause{Unchecked Access Value Creation}

@begin{Intro}
@redundant[
The attribute Unchecked_Access is used to create access values
in an unsafe manner @em the programmer is responsible for preventing
``dangling references.''
]
@end{Intro}

@begin{StaticSem}
The following attribute is defined for @PrefixType{a @nt{prefix} X that
denotes an aliased view of an object}:
@begin{Description}
@Attribute{Prefix=<X>, AttrName=<Unchecked_Access>,
  Text=<All rules and semantics that apply to
X'Access (see @RefSecNum{Operations of Access Types})
apply also to X'Unchecked_Access,
except that,
for the purposes of accessibility rules and checks,
it is as if X were declared immediately within a library package.>}
@IndexSeeAlso{Term=[Access attribute],See=(Unchecked_Access attribute)}
@end{Description}
@EndPrefixType{}
@end{StaticSem}

@begin{Notes}
This attribute is provided to support the situation where a local
object is to be inserted into a global linked data structure, when the
programmer knows that it will always be removed from the data structure prior
to exiting the object's scope.  The Access attribute would
be illegal in this case
(see @RefSec{Operations of Access Types}).
@begin{Ramification}
@PDefn2{Term=[expected type],
  Sec=(Unchecked_Access attribute)}
The expected type for X'Unchecked_Access is as for X'Access.

If an @nt<attribute_reference> with Unchecked_Access is used
as the actual parameter for an access parameter,

an Accessibility_Check can never fail on that access
parameter.

@end{Ramification}

There is no Unchecked_Access attribute for subprograms.
@begin{Reason}
  Such an attribute would allow ``downward closures,''
  where an access value designating a more nested subprogram is passed
  to a less nested subprogram.
  This requires some means of reconstructing the global environment for
  the more nested subprogram,
  so that it can do up-level references to objects.
  The two methods of implementing up-level references are displays and
  static links.
  If downward closures were supported,
  each access-to-subprogram value would have to carry the static link
  or display with it.
  In the case of displays, this was judged to be infeasible,
  and we don't want to disrupt implementations by forcing them
  to use static links if they already use displays.

  If desired, an instance of Unchecked_Conversion can be used to create
  an access value of a global access-to-subprogram type that
  designates a local subprogram.  The semantics of using
  such a value are not specified by the language.
  In particular, it is not specified what happens if such
  subprograms make up-level references; even if the frame
  being referenced still exists, the up-level reference might
  go awry if the representation of a value of a global access-to-subprogram
  type doesn't include a static link.
@end{Reason}

@end{Notes}

@LabeledClause{Storage Management}

@begin{Intro}
@Redundant[
@Defn{user-defined storage management}
@Defn2{Term=[storage management], Sec=(user-defined)}
@Defn{user-defined heap management}
@Defn2{Term=[heap management], Sec=(user-defined)}
Each access-to-object type has an associated storage
pool.
The storage allocated by an @nt{allocator} comes from the pool;
instances of Unchecked_Deallocation return storage to the pool.
Several access types can share the same pool.]

@Redundant[A storage pool is a variable of a type in the
class rooted
at Root_Storage_Pool, which is an abstract limited controlled type.
By default, the implementation chooses a @i{standard storage pool}
for each access type.
The user may define new pool types,
and may override the choice of pool for an access
type by specifying Storage_Pool for the type.]
@begin{Ramification}
By default, the implementation might choose to have a single global
storage pool,
which is used (by default) by all access types,
which might mean that storage is reclaimed automatically only
upon partition completion.
Alternatively, it might choose to create a new pool
at each accessibility level,
which might mean that storage is reclaimed for an access type
when leaving the appropriate scope.
Other schemes are possible.
@end{Ramification}
@end{Intro}

@begin{Legality}
If Storage_Pool is specified for a given access type,
Storage_Size shall not be specified for it.
@begin{Reason}
The Storage_Pool determines the Storage_Size;
hence it would not make sense to specify both.
Note that this rule is simplified by the fact that the aspects in
question cannot be specified for derived types,
nor for non-first subtypes,
so we don't have to worry about whether, say, Storage_Pool on a
derived type overrides Storage_Size on the parent type.
For the same reason, ``specified'' means the same thing as
``directly specified'' here.
@end{Reason}
@end{Legality}

@begin{StaticSem}
The following language-defined library package exists:
@begin{Example}
@key[with] Ada.Finalization;
@key[with] System.Storage_Elements;
@ChildUnit{Parent=[System],Child=[Storage_Pools],Expanded=[System.Storage_Pools]}
@key[package] System.Storage_Pools @key[is]
    @key{pragma} Preelaborate(System.Storage_Pools);

@LangDefType{Package=[System.Storage_Pools],Type=[Root_Storage_Pool]}
    @key[type] Root_Storage_Pool @key[is]
        @key[abstract] @key[new] Ada.Finalization.Limited_Controlled @key[with] @key[private];

    @key[procedure] Allocate(
      Pool : @key[in] @key[out] Root_Storage_Pool;
      Storage_Address : @key[out] Address;
      Size_In_Storage_Elements : @key[in] Storage_Elements.Storage_Count;
      Alignment : @key[in] Storage_Elements.Storage_Count) @key[is] @key[abstract];

    @key[procedure] Deallocate(
      Pool : @key[in] @key[out] Root_Storage_Pool;
      Storage_Address : @key[in] Address;
      Size_In_Storage_Elements : @key[in] Storage_Elements.Storage_Count;
      Alignment : @key[in] Storage_Elements.Storage_Count) @key[is] @key[abstract];

    @key[function] Storage_Size(Pool : Root_Storage_Pool)
        @key[return] Storage_Elements.Storage_Count @key[is] @key[abstract];

@key[private]
   ... -- @i{not specified by the language}
@key[end] System.Storage_Pools;
@end{Example}
@begin{Reason}
The Alignment parameter is provided to Deallocate because some
allocation strategies require it.
If it is not needed, it can be ignored.
@end{Reason}

@Defn{storage pool type}
@Defn{pool type}
A @i{storage pool type} (or @i{pool type}) is a descendant of
Root_Storage_Pool.
@Defn{storage pool element}
@Defn{pool element}
@Defn2{Term=[element], Sec=(of a storage pool)}
The @i{elements} of a storage pool are the objects allocated in the
pool by @nt{allocator}s.
@begin{Discussion}
In most cases, an element corresponds to a single memory block
allocated by Allocate.
However, in some cases the implementation may choose to associate
more than one memory block with a given pool element.
@end{Discussion}

For @PrefixType{every access subtype S},
the following attributes are defined:
@begin{Description}
@Attribute{Prefix=<S>, AttrName=<Storage_Pool>,
  Text=<Denotes the storage pool of the type of S.
The type of this attribute is Root_Storage_Pool'Class.>}

@Attribute{Prefix=<S>, AttrName=<Storage_Size>,
  Text=<Yields the result of calling
Storage_Size(S'Storage_Pool)@Redundant{,
which is intended to be a measure of the number of storage elements
reserved for the pool.}
The type of this attribute is @i{universal_integer}.>}
@EndPrefixType{}
@begin{Ramification}
Storage_Size is also defined for task subtypes and objects
@em see @RefSecNum{Representation Attributes}.

Storage_Size is not a measure of how much un-allocated space is
left in the pool.
That is, it includes both allocated and unallocated space.
Implementations and users may provide a Storage_Available function
for their pools, if so desired.
@end{Ramification}
@end{Description}

@PDefn2{Term=[specifiable], Sec=(of Storage_Size for
a non-derived access-to-object type)}
@PDefn2{Term=[specifiable], Sec=(of Storage_Pool for
a non-derived access-to-object type)}
@Defn{Storage_Pool clause}
@Defn{Storage_Size clause}
Storage_Size or Storage_Pool may be specified for
a non-derived access-to-object type
via an @nt{attribute_definition_clause};
the @nt{name} in a Storage_Pool clause shall denote a variable.

An @nt{allocator} of type T allocates storage from T's storage pool.
If the storage pool is a user-defined object, then
the storage is allocated by calling Allocate,
passing T'Storage_Pool as the Pool parameter.
The Size_In_Storage_Elements parameter indicates the number of storage elements
to be allocated,
and is no more than D'Max_Size_In_Storage_Elements,
where D is the designated subtype.
The Alignment parameter is D'Alignment.
@PDefn{contiguous representation}
@PDefn{discontiguous representation}
The result returned in the Storage_Address parameter is used by the
@nt{allocator} as the address of the allocated storage,
which is a contiguous block of memory of Size_In_Storage_Elements
storage elements.
@Redundant[Any exception propagated by Allocate is propagated by the
@nt{allocator}.]
@begin{Ramification}
If the implementation chooses to represent the designated
subtype in multiple pieces,
one @nt{allocator} evaluation might result in more than one call upon
Allocate.
In any case, @nt{allocator}s for the access type obtain all the required
storage for an object of the designated type by calling the
specified Allocate procedure.

Note that the implementation does not turn other exceptions into
Storage_Error.
@end{Ramification}

@Defn{standard storage pool}
If Storage_Pool is not specified for a type defined by an
@nt{access_to_object_definition},
then the implementation chooses a standard storage pool for it
in an implementation-defined manner.
@IndexCheck{Storage_Check}
@Defn2{Term=[Storage_Error],Sec=(raised by failure of run-time check)}
In this case,
the exception Storage_Error is raised by an @nt{allocator}
if there is not enough storage.
It is implementation defined whether or not the implementation
provides user-accessible names for the standard pool type(s).
@ImplDef{The manner of choosing a storage pool for an access type when
Storage_Pool is not specified for the type.}
@ImplDef{Whether or not the implementation
provides user-accessible names for the standard pool type(s).}
@begin{Ramification}
An anonymous access type has no pool.
An access-to-object type defined by a @nt{derived_type_definition}
inherits its pool from its parent type, so
all access-to-object types in the same derivation class share the
same pool.
Hence the ``defined by an @nt{access_to_object_definition}'' wording
above.

@PDefn{contiguous representation}
@PDefn{discontiguous representation}
There is no requirement that all storage pools be implemented using a
contiguous block of memory (although each allocation returns
a pointer to a contiguous block of memory).
@end{Ramification}

If Storage_Size is specified for an access type,
then the Storage_Size of this pool is at least that requested,
and the storage for the pool is reclaimed when the master containing
the declaration of the access type is left.
@Defn2{Term=[Storage_Error],Sec=(raised by failure of run-time check)}
If the implementation cannot satisfy the request,
Storage_Error is raised at the point of the
@nt{attribute_definition_clause}.
If neither Storage_Pool nor Storage_Size are specified,
then the meaning of Storage_Size is implementation defined.
@ImplDef{The meaning of Storage_Size.}
@begin{Ramification}
The Storage_Size function and attribute will return the actual
size, rather than the requested size.
Comments about rounding up, zero, and negative
on task Storage_Size apply here, as well.
See also AI-00557, AI-00558, and AI-00608.

The expression in a Storage_Size clause
need not be static.

The reclamation happens after the master is finalized.
@end{Ramification}
@begin{ImplNote}
For a pool allocated on the stack, normal stack cut-back can
accomplish the reclamation.
For a library-level pool, normal partition termination actions can
accomplish the reclamation.
@end{ImplNote}

If Storage_Pool is specified for an access type,
then the specified pool is used.

@PDefn{unspecified}
The effect of calling Allocate and Deallocate for a standard storage
pool directly
(rather than implicitly via an @nt{allocator} or an instance
of Unchecked_Deallocation) is unspecified.
@begin{Ramification}
For example, an @nt{allocator} might put the pool element on a
finalization list.
If the user directly Deallocates it, instead of calling an instance
of Unchecked_Deallocation, then the implementation would probably try
to finalize the object upon master completion,
which would be bad news.
Therefore, the implementation should define such situations as
erroneous.
@end{Ramification}
@end{StaticSem}

@begin{Erron}
If Storage_Pool is specified for an access type,
then if Allocate can satisfy the request,
it should allocate a contiguous block of memory,
and return the address of the first storage element in Storage_Address.
The block should contain Size_In_Storage_Elements storage elements,
and should be aligned according to Alignment.
The allocated storage should not be used for any
other purpose while the pool element remains in existence.
If the request cannot be satisfied,
then Allocate should propagate an exception
@Redundant[(such as Storage_Error)].
If Allocate behaves in any other manner,
then the program execution is erroneous.
@end{Erron}

@begin{DocReq}
An implementation shall document
the set of values that a user-defined Allocate procedure needs
to accept for the Alignment parameter.
An implementation shall document
how the standard storage pool is chosen,
and how storage is allocated by standard storage pools.
@ImplDef{Implementation-defined aspects of storage pools.}
@end{DocReq}

@begin{ImplAdvice}
An implementation should document any cases in which it dynamically
allocates heap storage for a purpose other than the evaluation of an
@nt{allocator}.
@begin{Reason}
This is ``@ImplAdviceTitle'' because the term ``heap storage''
is not formally definable;
therefore, it is not testable whether the implementation obeys this advice.
@end{Reason}

A default (implementation-provided) storage pool for an
access-to-constant type should
not have overhead to support deallocation of individual objects.
@begin{Ramification}
Unchecked_Deallocation is not defined for
such types.  If the access-to-constant type is library-level,
then no deallocation (other than at partition completion) will
ever be necessary, so if the size needed by an @nt{allocator}
of the type is known at link-time, then the allocation
should be performed statically.
If, in addition, the initial value of the designated object is known
at compile time,
the object can be allocated to read-only memory.
@end{Ramification}
@begin{ImplNote}
If the Storage_Size for an access type is specified,
the storage pool should consist of a contiguous block of memory,
possibly allocated on the stack.
The pool should contain approximately this number of
storage elements.
These storage elements should be reserved at the place of the
Storage_Size clause,
so that @nt{allocator}s cannot raise Storage_Error due to running out
of pool space until the appropriate number of storage elements has
been used up.
This approximate (possibly rounded-up) value should be used as a
maximum;
the implementation should not increase the size of the pool on the
fly.
If the Storage_Size for an access type is specified as zero,
then the pool should not take up any storage space,
and any @nt{allocator} for the type should raise Storage_Error.
@end{ImplNote}
@begin{Ramification}
Note that most of this is approximate,
and so cannot be (portably) tested.
That's why we make it an Implementation Note.
There is no particular number of allocations that is guaranteed to
succeed, and there is no particular number of allocations that is
guaranteed to fail.
@end{Ramification}

A storage pool for an anonymous access type should be created
at the point of an allocator for the type, and be reclaimed when
the designated object becomes inaccessible.
@begin{ImplNote}
  Normally the "storage pool" for an anonymous access type
  would not exist as a separate entity.
  Instead, the designated object of the allocator
  would be allocated, in the case of an access parameter,
  as a local aliased variable at the call site, and in the
  case of an access discriminant, contiguous with the object
  containing the discriminant.
  This is similar to the way storage for @nt{aggregate}s is typically
  managed.
@end{ImplNote}
@end{ImplAdvice}

@begin{Notes}
A user-defined storage pool type can be obtained by extending the
Root_Storage_Pool type,
and overriding the primitive subprograms Allocate, Deallocate, and
Storage_Size.
A user-defined storage pool can then be obtained by declaring
an object of the type extension.
The user can override Initialize and Finalize if there is any need
for non-trivial initialization and finalization for a user-defined
pool type.
For example, Finalize might reclaim blocks of storage that are allocated
separately from the pool object itself.

The writer of the user-defined allocation and deallocation
procedures, and users of @nt{allocator}s for the associated access
type, are responsible for dealing with any interactions with
tasking.  In particular:
@begin{itemize}
  If the @nt{allocator}s are used in different tasks, they require
mutual exclusion.

 If they are used inside protected objects, they cannot block.

 If they are used by interrupt handlers
(see @RefSec{Interrupt Support}),
the mutual exclusion mechanism has to work properly in that context.
 @end{itemize}

The primitives Allocate, Deallocate, and Storage_Size are declared as
abstract (see @RefSecNum{Abstract Types and Subprograms}),
and therefore they have to be overridden when
a new (non-abstract) storage pool type is declared.
@begin{Ramification}
Note that the Storage_Pool attribute denotes an object,
rather than a value,
which is somewhat unusual for attributes.

The calls to Allocate, Deallocate, and Storage_Size are dispatching
calls @em this follows from the fact that the actual parameter for
Pool is T'Storage_Pool, which is of type Root_Storage_Pool'Class.  In
many cases (including all cases in which Storage_Pool is not
specified), the compiler can determine the tag statically.
However, it is possible to construct cases where it cannot.

All access types in the same derivation class share the same pool,
whether implementation defined or user defined.
This is necessary because we allow type conversions among them
(even if they are pool-specific),
and we want pool-specific access values to always designate
an element of the right pool.
@end{Ramification}
@begin{ImplNote}
If an access type has a standard storage pool,
then the implementation doesn't actually have to follow the pool
interface described here,
since this would be semantically invisible.
For example, the allocator could conceivably be implemented with inline
code.
@end{ImplNote}
@end{Notes}

@begin{Examples}
To associate an access type with a storage pool object, the user
first declares a pool object of some type derived from
Root_Storage_Pool.  Then, the user defines its Storage_Pool
attribute, as follows:

@begin{Example}
Pool_Object : Some_Storage_Pool_Type;

@key[type] T @key[is] @key[access] Designated;
@key[for] T'Storage_Pool @key[use] Pool_Object;
@end{Example}

Another access type may be added to an existing storage pool, via:
@begin{Example}
@key[for] T2'Storage_Pool @key[use] T'Storage_Pool;
@end{Example}

The semantics of this is implementation defined
for a standard storage pool.
@begin{Reason}
For example, the implementation is allowed to choose a storage pool
for T that takes advantage of the fact that T is of a certain size.
If T2 is not of that size, then the above will probably not work.
@end{Reason}

As usual, a derivative of Root_Storage_Pool may define additional
operations.  For example, presuming that Mark_Release_Pool_Type has
two additional operations, Mark and Release,
the following is a possible use:

@begin{Example}
@key[type] Mark_Release_Pool_Type
   (Pool_Size : Storage_Elements.Storage_Count;
    Block_Size : Storage_Elements.Storage_Count)
        @key[is] @key[new] Root_Storage_Pool @key[with limited private];

...

@tabclear{}
MR_Pool : Mark_Release_Pool_Type (@^Pool_Size => 2000,
 @\Block_Size => 100);
@tabclear{}

@key[type] Acc @key[is] @key[access] ...;
@key[for] Acc'Storage_Pool @key[use] MR_Pool;
...

Mark(MR_Pool);
... --@i{ Allocate objects using ``@key[new] Designated(...)''.}
Release(MR_Pool); --@i{ Reclaim the storage.}
@end{Example}
@end{Examples}

@begin{Extend83}
User-defined storage pools are new to Ada 9X.
@end{Extend83}

@begin{DiffWord83}
Ada 83 had a concept called a ``collection,''
which is similar to what we call a storage pool.
All access types in the same derivation class
shared the same collection.
In Ada 9X, all access types in the same derivation class
share the same storage pool,
but other (unrelated) access types can also share the same storage pool,
either by default, or as specified by the user.
A collection was an amorphous collection of objects;
a storage pool is a more concrete concept @em hence
the different name.

RM83 states the erroneousness of reading or updating deallocated
objects incorrectly by missing various cases.
@end{DiffWord83}

@LabeledSubClause{The Max_Size_In_Storage_Elements Attribute}

@begin{Intro}
@redundant[
The Max_Size_In_Storage_Elements attribute is useful in writing user-defined pool
types.
]
@end{Intro}

@begin{StaticSem}
For @PrefixType{every subtype S},
the following attribute is defined:
@begin{Description}
@Attribute{Prefix=<S>, AttrName=<Max_Size_In_Storage_Elements>,
  Text=<Denotes the maximum value for Size_In_Storage_Elements
that will be requested via Allocate for an access type whose
designated subtype is S.
The value of this attribute is of type @i{universal_integer}.>}
@EndPrefixType{}
@begin{Ramification}
If S is an unconstrained array subtype,
or an unconstrained subtype with discriminants,
S'Max_Size_In_Storage_Elements might be very large.
@end{Ramification}
@end{Description}
@end{StaticSem}

@LabeledSubClause{Unchecked Storage Deallocation}

@begin{Intro}
@redundant[
@Defn{unchecked storage deallocation}
@Defn2{Term=[storage deallocation], Sec=(unchecked)}
@Defn{deallocation of storage}
@Defn{reclamation of storage}
@Defn{freeing storage}
Unchecked storage deallocation of an object designated by a value of an
access type is achieved by a call to an instance of
the generic procedure Unchecked_Deallocation.
]
@end{Intro}

@begin{StaticSem}
The following language-defined generic library procedure exists:
@begin{Example}
@key[generic]
   @key[type] Object(<>) @key[is] @key[limited] @key[private];
   @key[type] Name   @key[is] @key[access]  Object;
@ChildUnit{Parent=[Ada],Child=[Unchecked_Deallocation],Expanded=[Ada.Unchecked_Deallocation]}
@key[procedure] Ada.Unchecked_Deallocation(X : @key[in] @key[out] Name);
@key[pragma] Convention(Intrinsic, Ada.Unchecked_Deallocation);
@key[pragma] Preelaborate(Ada.Unchecked_Deallocation);
@end{Example}
@begin{Reason}
The @nt{pragma} Convention implies that
the attribute Access is not allowed
for instances of Unchecked_Deallocation.
@end{Reason}
@end{StaticSem}

@begin{RunTime}
Given an instance of Unchecked_Deallocation
declared as follows:
@begin{Example}
@key[procedure] Free @key[is]
    @key[new] Ada.Unchecked_Deallocation(
        @i[object_subtype_name], @i[access_to_variable_subtype_name]);
@end{Example}

Procedure Free has the following effect:
@begin{Enumerate}
After executing Free(X), the value of X is @key{null}.

Free(X), when X is already equal to @key{null}, has no effect.

Free(X), when X is not equal to @key{null} first
performs finalization, as described in
@RefSecNum{User-Defined Assignment and Finalization}.
It then deallocates the storage occupied by the object designated by X.
If the storage pool is a user-defined object, then
the storage is deallocated by calling Deallocate,
passing @i[access_to_variable_subtype_name]'Storage_Pool as the Pool parameter.
Storage_Address is the value returned in the Storage_Address parameter of the
corresponding Allocate call.
Size_In_Storage_Elements and Alignment are the same values passed to the
corresponding Allocate call.
There is one exception: if the object being freed contains tasks,
the object might not be deallocated.
@begin{Ramification}
Free calls only the specified Deallocate procedure
to do deallocation.
For any given object deallocation,
the number of calls to Free (usually one)
will be equal to the number of Allocate calls it took
to allocate the object.
We do not define the relative order of multiple calls used to deallocate
the same object @em that is, if the @nt{allocator} allocated two pieces @i{x}
and @i{y}, then Free might deallocate @i{x} and then @i{y},
or it might deallocate @i{y} and then @i{x}.
@end{Ramification}
@end{Enumerate}

@IndexSee{Term=[freed],See=(nonexistent)}
@Defn{nonexistent}
After Free(X), the object designated by X, and any
subcomponents thereof, no longer exist;
their storage can be reused for other purposes.
@end{RunTime}

@begin{Bounded}
It is a bounded error to free a discriminated, unterminated
task object.
The possible consequences are:
@begin{Reason}
  This is an error because the task might refer to its discriminants,
  and the discriminants might be deallocated by freeing the task object.
@end{Reason}
@begin{Itemize}
No exception is raised.

@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
@Defn2{Term=[Tasking_Error],Sec=(raised by failure of run-time check)}
Program_Error or Tasking_Error is raised at the point of the
deallocation.

@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
@Defn2{Term=[Tasking_Error],Sec=(raised by failure of run-time check)}
Program_Error or Tasking_Error is raised in the task the next time it
references any of the discriminants.
@begin{ImplNote}
  This last case presumes an implementation where the task references
  its discriminants indirectly,
  and the pointer is nulled out when the task object is deallocated.
@end{ImplNote}
@end{Itemize}

In the first two cases,
the storage for the discriminants
(and for any enclosing object if it is designated by an access
discriminant of the task)
is not reclaimed prior to task termination.
@begin{Ramification}
  The storage might never be reclaimed.
@end{Ramification}
@end{Bounded}

@begin{Erron}
@Defn{nonexistent}

Evaluating a name that denotes a nonexistent object is erroneous.

The execution of a call to an instance of Unchecked_Deallocation is
erroneous if
the object was created other than by an @nt<allocator> for
an access type whose pool is Name'Storage_Pool.
@end{Erron}

@begin{ImplAdvice}
For a standard storage pool,
Free should actually reclaim the storage.
@begin{Ramification}
This is not a testable property,
since we do not how much storage is used by a given pool element,
nor whether fragmentation can occur.
@end{Ramification}
@end{ImplAdvice}

@begin{Notes}
The rules here that refer to Free apply to any instance
of Unchecked_Deallocation.

Unchecked_Deallocation cannot be instantiated for an
access-to-constant type.
This is implied by the rules of @RefSecNum{Formal Access Types}.
@end{Notes}

@LabeledSubClause{Pragma Controlled}

@begin{Intro}
@Redundant[Pragma Controlled is used to prevent any automatic
reclamation of storage (garbage collection) for the objects
created by @nt<allocator>s of a given access type.]
@end{Intro}

@begin{Syntax}
@begin{SyntaxText}
The form of a @nt{pragma} Controlled is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(Controlled)(@SynI{first_subtype_}@Syn2{local_name});'
@begin{Discussion}
Not to be confused with type Finalization.Controlled.
@end{Discussion}
@end{Syntax}

@begin{Legality}
The @SynI{first_subtype_}@nt<local_name> of a @nt{pragma} Controlled
shall denote a non-derived access subtype.
@end{Legality}

@begin{StaticSem}
@PDefn2{Term=[representation pragma], Sec=(Controlled)}
@PDefn2{Term=[pragma, representation], Sec=(Controlled)}
A @nt{pragma} Controlled is a representation pragma
@PDefn2{Term=[aspect of representation], Sec=(controlled)}
@Defn2{Term=[controlled], Sec=(aspect of representation)}
that specifies the @i{controlled} aspect of representation.

@Defn{garbage collection}
@i{Garbage collection} is a process that automatically reclaims storage,
or moves objects to a different address,
while the objects still exist.
@begin{Ramification}
Storage reclamation upon leaving a master is not considered garbage
collection.

Note that garbage collection includes compaction of a pool
(``moved to a different Address''), even if storage reclamation is not
done.
@end{Ramification}
@begin{Reason}
Programs that will be damaged by automatic storage reclamation
are just as likely to be damaged by having objects moved to different
locations in memory.
A @nt{pragma} Controlled should turn off both flavors of garbage collection.
@end{Reason}
@begin{ImplNote}
If garbage collection reclaims the storage of a controlled object,
it should first finalize it.
Finalization is not done when moving an object;
any self-relative pointers will have to be updated by the garbage
collector.
If an implementation provides garbage collection
for a storage pool containing controlled objects
(see @RefSecNum{User-Defined Assignment and Finalization}),
then it should provide a means for deferring garbage collection of
those controlled objects.
@end{ImplNote}
@begin{Reason}
  This allows the manager of a resource released
  by a Finalize operation to defer garbage collection
  during its critical regions;
  it is up to the author of the Finalize operation to do so.
  Garbage collection, at least in some systems,
  can happen asynchronously with respect to normal user code.
  Note that it is not enough to defer garbage collection
  during Initialize, Adjust, and Finalize,
  because the resource in question might be used
  in other situations as well.
  For example:
  @begin{Example}
@key[with] Ada.Finalization;
@key[package] P @key[is]

    @key[type] My_Controlled @key[is]
        @key[new] Ada.Finalization.Limited_Controlled @key[with] @key[private];
    @key[procedure] Finalize(Object : @key[in] @key[out] My_Controlled);
    @key[type] My_Controlled_Access @key[is] @key[access] My_Controlled;

    @key[procedure] Non_Reentrant;

@key[private]
    ...
@key[end] P;

@key[package] @key[body] P @key[is]
    X : Integer := 0;
    A : @key[array](Integer @key[range] 1..10) @key[of] Integer;

    @key[procedure] Non_Reentrant @key[is]
    @key[begin]
        X := X + 1;
        --@i{ If the system decides to do a garbage collection here,}
        --@i{ then we're in trouble, because it will call Finalize on}
        --@i{ the collected objects; we essentially have two threads}
        --@i{ of control erroneously accessing shared variables.}
        --@i{ The garbage collector behaves like a separate thread}
        --@i{ of control, even though the user hasn't declared}
        --@i{ any tasks.}
        A(X) := ...;
    @key[end] Non_Reentrant;

    @key[procedure] Finalize(Object : @key[in] @key[out] My_Controlled) @key[is]
    @key[begin]
        Non_Reentrant;
    @key[end] Finalize;
@key[end] P;

@key[with] P; @key[use] P;
@key[procedure] Main @key[is]
@key[begin]
    ... @key[new] My_Controlled ... --@i{ allocate some objects}
    ... @i{ forget the pointers to some of them, so they become garbage}
    Non_Reentrant;
@key[end] Main;
  @end{Example}

  It is the user's responsibility to protect against this sort of
  thing, and the implementation's responsibility to provide the
  necessary operations.

  We do not give these operations names,
  nor explain their exact semantics,
  because different implementations of garbage collection might have
  different needs, and because garbage collection is not supported by
  most Ada implementations, so portability is not important here.
  Another reason not to
  turn off garbage collection during each entire
  Finalize operation is that it would create a serial bottleneck;
  it might be only part of the Finalize operation that conflicts with
  some other resource.
  It is the intention that the mechanisms provided be finer-grained
  than pragma Controlled.
@end{Reason}

If a @nt{pragma} Controlled is specified for an access type
with a standard storage pool,
then garbage collection is not performed for objects in that pool.
@begin{Ramification}
If Controlled is not specified,
the implementation may, but need not, perform garbage
collection.
If Storage_Pool is specified,
then a @nt{pragma} Controlled for that type is ignored.
@end{Ramification}
@begin{Reason}
Controlled means that implementation-provided garbage collection is
turned off;
if the Storage_Pool is specified, the pool controls
whether garbage collection is done.
@end{Reason}
@end{StaticSem}

@begin{ImplPerm}
An implementation need not support garbage collection, in which case,
a pragma Controlled has no effect.
@end{ImplPerm}

@begin{DiffWord83}
Ada 83 used the term ``automatic storage reclamation'' to refer to what
is known traditionally as ``garbage collection''.
Because of the existence of storage pools
(see @RefSecNum{Storage Management}),
we need to distinguish this from the storage reclamation that might
happen upon leaving a master.
Therefore, we now use the term ``garbage collection''
in its normal computer-science sense.
This has the additional advantage of making our terminology more
accessible to people outside the Ada world.
@end{DiffWord83}

@LabeledClause{Pragma Restrictions}

@begin{Intro}
@redundant[
A @nt{pragma} Restrictions expresses the user's intent to abide by
certain restrictions.
This may facilitate the construction of
simpler run-time environments.
]
@end{Intro}

@begin{Syntax}
@begin{SyntaxText}
The form of a @nt{pragma} Restrictions is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(Restrictions)(@Syn2{restriction}{, @Syn2{restriction}});'

@Syn{lhs=(restriction), rhs="@SynI{restriction_}@Syn2{identifier}
    | @SynI{restriction_parameter_}@Syn2{identifier} => @Syn2{expression}"}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected type],
  Sec=(restriction parameter expression)}
Unless otherwise specified for a particular restriction,
the @nt{expression} is expected to be of any integer type.
@end{Resolution}

@begin{Legality}
Unless otherwise specified for a particular restriction,
the @nt{expression} shall be static,
and its value shall be nonnegative.
@end{Legality}

@begin{StaticSem}
The set of @nt{restrictions} is implementation defined.
@ImplDef{The set of @nt{restrictions} allowed in a @nt{pragma}
Restrictions.}
@end{StaticSem}

@begin{LinkTime}
@PDefn2{Term=[configuration pragma], Sec=(Restrictions)}
@PDefn2{Term=[pragma, configuration], Sec=(Restrictions)}
A @nt{pragma} Restrictions is a configuration pragma;
unless otherwise specified for a particular restriction,
a partition shall obey the restriction
if a @nt{pragma} Restrictions applies to any compilation unit
included in the partition.
@end{LinkTime}

@begin{ImplPerm}
An implementation may place limitations on the values of the
@nt{expression} that are supported,
and limitations on the supported combinations of restrictions.
The consequences of violating such limitations are
implementation defined.
@ImplDef{The consequences of violating limitations on
Restrictions @nt{pragma}s.}
@begin{Ramification}
Such limitations may be enforced at compile time or at run time.
Alternatively, the implementation is allowed to declare violations of
the restrictions to be erroneous, and not enforce them at all.
@end{Ramification}
@end{ImplPerm}

@begin{Notes}
Restrictions intended to facilitate the construction of
efficient tasking run-time systems are defined
in @RefSecNum{Tasking Restrictions}.
Safety- and security-related
restrictions are defined in
@RefSecNum{Safety and Security Restrictions}.

An implementation has to enforce the restrictions in cases where
enforcement is required,
even if it chooses not to take advantage of the restrictions in terms of
efficiency.
@begin{Discussion}
It is not the intent that an implementation will support a different
run-time system for every possible combination of restrictions.
An implementation might support only two run-time systems,
and document a set of restrictions that is sufficient to allow
use of the more efficient and safe one.
@end{Discussion}
@end{Notes}

@begin{Extend83}
Pragma Restrictions is new to Ada 9X.
@end{Extend83}

@LabeledClause{Streams}

@begin{Intro}
@Defn{stream}
@Defn{stream type}
A @i{stream} is a sequence of elements comprising values from
possibly different types and allowing sequential access to these values.
A @i{stream type} is a type in the class whose root type is
Streams.Root_Stream_Type.
A stream type may be implemented in various ways,
such as an external sequential file, an internal buffer,
or a network channel.
@begin{Discussion}
A stream element will often be the same size as a storage element,
but that is not required.
@end{Discussion}
@end{Intro}

@begin{Extend83}
Streams are new in Ada 9X.
@end{Extend83}

@LabeledSubClause{The Package Streams}

@begin{StaticSem}
The abstract type Root_Stream_Type is the root type of the class of
stream types.  The types in this class represent different kinds of
streams.  A new stream type is defined by extending the root type
(or some other stream type), overriding the Read and Write
operations, and optionally defining additional primitive
subprograms,
according to the requirements of the particular kind of stream.
The predefined stream-oriented attributes like T'Read and T'Write
make dispatching
calls on the Read and Write procedures of the Root_Stream_Type.
(User-defined T'Read and T'Write attributes can also make such calls,
or can call the Read and Write attributes of other types.)
@begin{example}
@ChildUnit{Parent=[Ada],Child=[Streams],Expanded=[Ada.Streams]}
@key[package] Ada.Streams @key[is]
    @key[pragma] Pure(Streams)@Defn{unpolluted};

@LangDefType{Package=[Ada.Streams],Type=[Root_Stream_Type]}
    @key[type] Root_Stream_Type @key[is] @key[abstract tagged limited private];

    @key[type] Stream_Element @key[is] @key[mod] @i{implementation-defined};
    @key[type] Stream_Element_Offset @key[is] @key[range] @i{implementation-defined};
    @key[subtype] Stream_Element_Count @key[is]
        Stream_Element_Offset @key[range] 0..Stream_Element_Offset'Last;
    @key[type] Stream_Element_Array @key[is]
        @key[array](Stream_Element_Offset @key[range] <>) @key[of] Stream_Element;

    @key[procedure] Read(
      Stream : @key[in] @key[out] Root_Stream_Type;
      Item   : @key[out] Stream_Element_Array;
      Last   : @key[out] Stream_Element_Offset) @key[is abstract];

    @key[procedure] Write(
      Stream : @key[in] @key[out] Root_Stream_Type;
      Item   : @key[in] Stream_Element_Array) @key[is abstract];

@key[private]
   ... -- @i{not specified by the language}
@key[end] Ada.Streams;
@end{example}

The Read operation transfers Item'Length stream elements from
the specified stream to fill the array Item.  The index of the last stream
element transferred is returned in Last.  Last is less than Item'Last
only if the end of the stream is reached.

The Write operation appends Item to the specified stream.

@end{StaticSem}

@begin{Notes}
See @RefSec{The Package Streams.Stream_IO} for an example of extending
type Root_Stream_Type.
@end{Notes}

@LabeledSubClause{Stream-Oriented Attributes}

@begin{Intro}
The Write, Read, Output, and Input attributes convert values to a
stream of elements and reconstruct values from a stream.
@end{Intro}

@begin{StaticSem}
For @PrefixType{every subtype S of a specific type @i(T)},
the following attributes are defined.
@begin{Description}
@Attribute{Prefix=<S>, AttrName=<Write>,
  Text=<S'Write denotes a procedure with the following specification:
@begin{Example}
@key(procedure) S'Write(
   @i{Stream} : @key{access} Ada.Streams.Root_Stream_Type'Class;
   @i{Item} : @key{in} @i(T))
@end{Example}

S'Write writes the value of @i{Item} to @i{Stream}.>}

@Attribute{Prefix=<S>, AttrName=<Read>,
  Text=<S'Read denotes a procedure with the following specification:
@begin{Example}
@key(procedure) S'Read(
   @i{Stream} : @key{access} Ada.Streams.Root_Stream_Type'Class;
   @i{Item} : @key{out} @i(T))
@end{Example}

S'Read reads the value of @i{Item} from @i{Stream}.>}
@end{Description}
@EndPrefixType{}


For elementary types,
the representation in terms of stream elements
is implementation defined.
For composite types,
the Write or Read attribute for each component is called in a
canonical order.
The canonical order of components is last dimension varying fastest for
an array,
and positional aggregate order for a record.
Bounds are not included in the
stream if @i(T) is an array type.
If @i(T) is a discriminated type,
discriminants are included only if they have defaults.
If @i(T) is a tagged type,
the tag is not included.

@ImplDef{The representation used by the Read and Write attributes of
elementary types in terms of stream elements.}
@begin{Reason}
  A discriminant with a default value is treated simply as
  a component of the object.  On the other hand,
  an array bound or a discriminant without a default value,
  is treated as ``descriptor'' or ``dope'' that must be provided in order
  to create the object and thus is logically separate from the regular
  components.  Such ``descriptor'' data are written by 'Output and
  produced as part of the delivered result by the 'Input function,
  but they are not written by 'Write nor read by 'Read.
  A tag is like a discriminant without a default.
@end{Reason}
@begin{Ramification}
  For a composite object,
  the subprogram denoted by the Write or Read attribute of each
  component is called,
  whether it is the default or is user-specified.
@end{Ramification}

For @PrefixType{every subtype S'Class of a class-wide type
@i(T)'Class}:
@begin{Description}
@Attribute{Prefix=<S'Class>, AttrName=<Write>,
  Text=<S'Class'Write denotes a procedure with the following
specification:
@begin{Example}
@key(procedure) S'Class'Write(
   @i{Stream} : @key{access} Ada.Streams.Root_Stream_Type'Class;
   @i{Item}   : @key{in} @i(T)'Class)
@end{Example}

Dispatches to the subprogram denoted by the Write attribute of
the specific type identified by the tag of Item.>}

@Attribute{Prefix=<S'Class>, AttrName=<Read>,
  Text=<S'Class'Read denotes a procedure with the following specification:
@begin{Example}
@key(procedure) S'Class'Read(
   @i{Stream} : @key{access} Ada.Streams.Root_Stream_Type'Class;
   @i{Item} : @key{out} @i(T)'Class)
@end{Example}

Dispatches to the subprogram denoted by the Read attribute of
the specific type identified by the tag of Item.>}
@begin{Reason}
It is necessary to have class-wide versions of Read and Write
in order to avoid generic contract model violations;
in a generic, we don't necessarily know at compile time whether a given
type is specific or class-wide.
@end{Reason}
@end{Description}
@EndPrefixType{}
@end{StaticSem}

@begin{ImplAdvice}

If a stream element is the same size as a storage element,
then the normal in-memory representation should be used by Read and
Write for scalar objects.
Otherwise, Read and Write should use the smallest
number of stream elements needed to represent all values in the base
range of the scalar type.

@end{ImplAdvice}

@begin{StaticSem}
For @PrefixType{every subtype S of a specific type @i(T)},
the following attributes are defined.
@begin{Description}
@Attribute{Prefix=<S>, AttrName=<Output>,
  Text=<S'Output denotes a procedure with the following specification:
@begin{Example}
@key(procedure) S'Output(
   @i{Stream} : @key{access} Ada.Streams.Root_Stream_Type'Class;
   @i{Item} : @key{in} @i(T))
@end{Example}

S'Output writes the value of @i{Item} to @i{Stream}, including
any bounds or discriminants.>}
@begin{Ramification}
Note that the bounds are included even for an array type whose
first subtype is constrained.
@end{Ramification}

@Attribute{Prefix=<S>, AttrName=<Input>,
  Text=<S'Input denotes a function with the following specification:
@begin{Example}
@key(function) S'Input(
   @i{Stream} : @key{access} Ada.Streams.Root_Stream_Type'Class)
   @key(return) @i(T)
@end{Example}

S'Input reads and returns one value from
@i{Stream}, using any bounds or discriminants written by a corresponding
S'Output to determine how much to read.>}
@end{Description}
@EndPrefixType{}

Unless overridden by an @nt<attribute_definition_clause>, these
subprograms execute as follows:
@begin(Itemize)
If @i(T) is an array type, S'Output first writes the bounds,
and S'Input first reads the bounds.
If @i(T) has discriminants without defaults, S'Output first writes
the discriminants (using S'Write for each), and S'Input first
reads the discriminants (using S'Read for each).

S'Output then calls S'Write to write the value of @i{Item} to the stream.
S'Input then creates an object (with the bounds or discriminants, if any,
taken from the stream), initializes it with S'Read, and returns
the value of the object.
@end(Itemize)

For @PrefixType{every subtype S'Class of a class-wide type
@i(T)'Class}:
@begin{Description}
@Attribute{Prefix=<S'Class>, AttrName=<Output>,
  Text=<S'Class'Output denotes a procedure with the following
specification:
@begin{Example}
@key(procedure) S'Class'Output(
   @i{Stream} : @key{access} Ada.Streams.Root_Stream_Type'Class;
   @i{Item}   : @key{in} @i(T)'Class)
@end{Example}

First writes the external tag of @i{Item} to @i{Stream}
(by calling String'Output(Tags.External_Tag(@i{Item}'Tag) @em
see @RefSecNum{Tagged Types and Type Extensions})
and then dispatches to the subprogram denoted by the Output attribute of
the specific type identified by the tag.>}

@Attribute{Prefix=<S'Class>, AttrName=<Input>,
  Text=<S'Class'Input denotes a function with the following specification:
@begin{Example}
@key(function) S'Class'Input(
   @i{Stream} : @key{access} Ada.Streams.Root_Stream_Type'Class)
   @key{return} @i(T)'Class
@end{Example}

First reads the external tag from @i{Stream} and determines
the corresponding internal tag
(by calling Tags.Internal_Tag(String'Input(@i{Stream})) @em
see @RefSecNum{Tagged Types and Type Extensions})
and then dispatches to the subprogram denoted by the Input attribute of
the specific type identified by the internal tag;
returns that result.>}
@end{Description}
@EndPrefixType{}


@IndexCheck{Range_Check}
In the default implementation of Read and Input for a composite type,
for each scalar component that is a discriminant or whose
@nt{component_declaration} includes a @nt{default_expression},
a check is made that the value returned by Read for the component
belongs to its subtype.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
Constraint_Error is raised if this check fails.
For other scalar components, no check is made.
For each component that is of an access type, if the implementation can
detect that the value returned by Read for the component is not
a value of its subtype, Constraint_Error is raised.  If the value
is not a value of its subtype and this error is not detected,
the component has an abnormal value, and erroneous execution
can result
(see @RefSecNum{Data Validity}).


@PDefn2{Term=[specifiable], Sec=(of Read for a type)}
@PDefn2{Term=[specifiable], Sec=(of Write for a type)}
@PDefn2{Term=[specifiable], Sec=(of Input for a type)}
@PDefn2{Term=[specifiable], Sec=(of Output for a type)}
@Defn{Read clause}
@Defn{Write clause}
@Defn{Input clause}
@Defn{Output clause}
The stream-oriented attributes may be specified
for any type via an @nt{attribute_definition_clause}.
All nonlimited types have default implementations
for these operations.

An @nt{attribute_reference} for one of these attributes is
illegal if the type is limited,
unless the attribute has been specified by an
@nt{attribute_definition_clause}.
For an @nt{attribute_definition_clause} specifying one of these
attributes, the subtype of the Item parameter shall be the base subtype
if scalar, and the first subtype otherwise.
The same rule applies to the result of the Input function.

@begin{Reason}
  This is to simplify implementation.
@end{Reason}
@end{StaticSem}

@begin{Notes}
For a definite subtype S of a type @i(T), only @i(T)'Write and @i(T)'Read
are needed to pass
an arbitrary value of the subtype through a stream.
For an indefinite subtype S of a type @i(T), @i(T)'Output and @i(T)'Input
will normally be needed, since @i(T)'Write and @i(T)'Read do not
pass bounds, discriminants, or tags.

User-specified attributes of S'Class are not inherited by other
class-wide types descended from S.
@end{Notes}

@begin{Examples}
@i{Example of user-defined Write attribute:}
@begin{Example}
@key[procedure] My_Write(
  Stream : @key[access] Ada.Streams.Root_Stream_Type'Class; Item : My_Integer'Base);
@key(for) My_Integer'Write @key(use) My_Write;
@end{Example}
@begin{Discussion}
@i{Example of network input/output using input output attributes:}
@begin{Example}
@key(with) Ada.Streams; @key(use) Ada.Streams;
@key(generic)
    @key(type) Msg_Type(<>) @key(is private);
@key(package) Network_IO @key(is)
    @i[-- Connect/Disconnect are used to establish the stream]
    @key(procedure) Connect(...);
    @key(procedure) Disconnect(...);

    @i[-- Send/Receive transfer messages across the network]
    @key(procedure) Send(X : @key[in] Msg_Type);
    @key(function) Receive @key(return) Msg_Type;
@key(private)
    @key(type) Network_Stream @key(is new) Root_Stream_Type @key(with) ...
    @key(procedure) Read(...);  @i[-- define Read/Write for Network_Stream]
    @key(procedure) Write(...);
@key(end) Network_IO;

@key(with) Ada.Streams; @key(use) Ada.Streams;
@key(package body) Network_IO @key(is)
    Current_Stream : @key(aliased) Network_Stream;
    . . .
    @key(procedure) Connect(...) @key(is) ...;
    @key(procedure) Disconnect(...) @key(is) ...;

    @key(procedure) Send(X : @key[in] Msg_Type) @key(is)
    @key(begin)
        Msg_Type'Output(Current_Stream'Access, X);
    @key(end) Send;

    @key(function) Receive @key(return) Msg_Type @key(is)
    @key(begin)
        @key(return) Msg_Type'Input(Current_Stream'Access);
    @key(end) Receive;
@key(end) Network_IO;
@end{Example}
@end{Discussion}
@end{Examples}

@LabeledClause{Freezing Rules}

@begin{Intro}
@redundant[
This clause defines
a place in the program text where each declared entity becomes
``frozen.''
A use of an entity, such as a reference to it by name,
or (for a type) an expression of the type,
causes freezing of the entity in some contexts,
as described below.
The @LegalityTitle forbid certain kinds of uses of an entity
in the region of text where it is frozen.
]
@begin{Reason}
This concept has two purposes: a compile-time one and a run-time one.

The compile-time purpose of the freezing rules comes from the fact that
the evaluation of static expressions depends on overload resolution,
and overload resolution sometimes depends on the value of a
static expression.
(The dependence of static evaluation upon overload resolution is
obvious.  The dependence in the other direction is more subtle.
There are three rules that require static expressions in contexts that
can appear in declarative places:  The expression in an
@nt{attribute_designator} shall be static.
In a record aggregate, variant-controlling discriminants shall be static.
In an array aggregate with more than one named association,
the choices shall be static.
The compiler needs to know the value of these expressions in order to
perform overload resolution and legality checking.)
We wish to allow a compiler to evaluate static expressions when it sees
them in a single pass over the @nt{compilation_unit}.
The freezing rules ensure that.

The run-time purpose of the freezing rules is called the ``linear
elaboration model.''
This means that declarations are elaborated in the order in which they
appear in the program text,
and later elaborations can depend on the results of earlier ones.
The elaboration of the declarations of certain entities requires run-time
information about the implementation details of other entities.
The freezing rules ensure that this information has been calculated by
the time it is used.
For example, suppose the initial value of a constant is the result of
a function call that takes a parameter of type @i(T).
In order to pass that parameter, the size of type @i(T) has to be known.
If @i(T) is composite, that size might be known only at run time.

(Note that in these discussions, words like ``before'' and ``after''
generally refer to places in the program text,
as opposed to times at run time.)
@end{Reason}
@begin{Discussion}
The ``implementation details'' we're talking about above are:
@begin{Itemize}
For a tagged type,
the implementations of all the primitive subprograms of the type
@em that is (in the canonical implementation model),
the contents of the type descriptor, which contains pointers to the code for
each primitive subprogram.

For a type,
the full type declaration of any parts
(including the type itself)
that are private.

For a deferred constant,
the full constant declaration, which gives the constant's value.
(Since this information necessarily comes after the constant's type
and subtype are fully
known, there's no need to worry about its type or subtype.)

For any entity,
representation information specified by the user
via representation items.
Most representation items are for types
or subtypes; however, various other kinds of entities,
such as objects and subprograms, are possible.
@end{Itemize}

Similar issues arise for incomplete types.
However, we do not use freezing there;
incomplete types have different, more severe, restrictions.
Similar issues also arise for subprograms, protected operations,
tasks and generic units.
However, we do not use freezing there either;
@RefSecNum{Declarative Parts} prevents problems with run-time
Elaboration_Checks.
@end{Discussion}
@end{Intro}

@begin{MetaRules}
An evaluable construct should freeze anything that's needed to evaluate
it.

However, if the construct is not evaluated where it appears,
let it cause freezing later, when it is evaluated.
This is the case for @nt{default_expression}s and @nt{default_name}s.
(Formal parameters, generic formal parameters, and components can have
@nt{default_expression}s or @nt{default_name}s.)

The compiler should be allowed to evaluate static expressions without
knowledge of their context.  (I.e. there should not be any special
rules for static expressions that happen to occur in a context that
requires a static expression.)

Compilers should be allowed to evaluate static expressions (and record
the results) using the run-time representation of the type.
For example, suppose Color'Pos(Red) = 1, but the internal code for Red
is 37.
If the value of a static expression is Red,
some compilers might store 1 in their symbol table,
and other compilers might store 37.
Either compiler design should be feasible.

Compilers should never be required to detect erroneousness or exceptions at
compile time (although it's very nice if they do).
This implies that we should not require
code-generation for a nonstatic expression of type @i(T) too early,
even if we can prove that that expression will be erroneous,
or will raise an exception.

Here's an example (modified from AI-00039, Example 3):
@begin{example}
@key[type] T @key[is]
    @key[record]
        ...
    @key[end] @key[record];
@key[function] F @key[return] T;
@key[function] G(X : T) @key[return] Boolean;
Y : Boolean := G(F); --@i{ doesn't force T in Ada 83}
@key[for] T @key[use]
    @key[record]
        ...
    @key[end] @key[record];
@end{example}

AI-00039 says this is legal.
Of course, it raises Program_Error because the function bodies aren't
elaborated yet.  A one-pass compiler has to generate code for an expression of
type T before it knows the representation of T.
Here's a similar example, which AI-00039 also says is legal:
@begin{example}
@key[package] P @key[is]
    @key[type] T @key[is] @key[private];
    @key[function] F @key[return] T;
    @key[function] G(X : T) @key[return] Boolean;
    Y : Boolean := G(F); --@i{ doesn't force T in Ada 83}
@key[private]
    @key[type] T @key[is]
        @key[record]
            ...
        @key[end] @key[record];
@key[end] P;
@end{example}

If T's size were dynamic, that size would be stored in some
compiler-generated dope; this dope would be initialized at the place
of the full type declaration.
However, the generated code for the function calls
would most likely allocate a temp of the size specified by the dope
@i{before} checking for Program_Error.
That dope would contain uninitialized junk,
resulting in disaster.
To avoid doing that,
the compiler would have to determine,
at compile time, that the expression will raise Program_Error.

This is silly.  If we're going to require compilers to detect the exception at
compile time, we might as well formulate the rule as a legality rule.

Compilers should not be required to generate code to load the value
of a variable before the address of the variable has been determined.



After an entity has been frozen, no further requirements may be
placed on its representation (such as by a representation item
or a @nt{full_type_declaration}).
@end{MetaRules}

@begin{Intro}
@RootDefn2{Term=[freezing], Sec=(entity)}
@Defn2{Term=[freezing points], Sec=(entity)}
The @i(freezing) of an entity occurs at one or more places
(@i{freezing points}) in the program
text where the representation for the entity has to be
fully determined.  Each entity is frozen from its first
freezing point to the end of the program text
(given the ordering of compilation units defined in
@RefSecNum{The Compilation Process}).
@begin{Ramification}
  The ``representation'' for a subprogram includes its calling convention
  and means for referencing the subprogram body, either a ``link-name'' or
  specified address.  It does
  not include the code for the subprogram body itself, nor its
  address if a link-name is used to reference the body.
@end{Ramification}

@Defn2{Term=[freezing],
  Sec=(entity caused by the end of an enclosing construct)}
The end of a @nt{declarative_part},
@nt{protected_body},
or a declaration of a library package or generic library package,
causes @i(freezing) of each entity declared within it,
except for incomplete types.
@Defn2{Term=freezing, Sec=(entity caused by a body)}
A noninstance body causes freezing of each entity declared
before it within the same @nt{declarative_part}.
@begin{Discussion}
  This is worded carefully to handle nested packages
  and private types.
  Entities declared in a nested @nt{package_specification}
  will be frozen by some containing construct.

  An incomplete type declared in the private part of
  a library @nt{package_specification}
  can be completed in the body.
@end{Discussion}
@begin{Ramification}
  The part about bodies does not say @i{immediately}
  within.
  A renaming-as-body does not have this property.
  Nor does a @nt{pragma} Import.
@end{Ramification}
@begin{Reason}
  The reason bodies cause freezing is because we want
  @nt{proper_bodies} and @nt{body_stub}s to be interchangeable @em one
  should be able to move a @nt{proper_body} to a @nt{subunit}, and
  vice-versa, without changing the semantics.
  Clearly, anything that should cause freezing should do so even if
  it's inside a @nt{proper_body}.
  However, if we make it a @nt{body_stub}, then the compiler can't
  see that thing that should cause freezing.
  So we make @nt{body_stub}s cause freezing, just in case they
  contain something that should cause freezing.
  But that means we need to do the same for @nt{proper_bodies}.

  Another reason for bodies to cause freezing,
  there could be an added implementation burden if an entity
  declared in an enclosing @nt<declarative_part> is frozen
  within a nested body,
  since some compilers look at bodies after looking
  at the containing @nt{declarative_part}.
@end{Reason}

@RootDefn2{Term=[freezing], Sec=(entity caused by a construct)}
A construct that (explicitly or implicitly) references an
entity can cause the @i(freezing) of the entity, as defined by
subsequent paragraphs.
@PDefn2{Term=[freezing], Sec=(by a constituent of a construct)}
At the place where a construct causes freezing,
each @nt<name>, expression@Redundant[, or @nt{range}]
within the construct causes freezing:
@begin{Ramification}
Note that in the sense of this paragraph,
a @nt{subtype_mark} ``references'' the denoted subtype,
but not the type.
@end{Ramification}

@begin{Itemize}
@PDefn2{Term=[freezing], Sec=(generic_instantiation)}
The occurrence of a @nt{generic_instantiation} causes freezing;
also, if a parameter of the instantiation is defaulted,
the @nt{default_expression} or @nt{default_name} for that parameter
causes freezing.

@PDefn2{Term=[freezing], Sec=(object_declaration)}
The occurrence of an @nt<object_declaration> that has no corresponding
completion causes freezing.
@begin{Ramification}
  Note that this does not include a @nt{formal_object_declaration}.
@end{Ramification}

@PDefn2{Term=[freezing], Sec=(subtype caused by a record extension)}
The declaration of a record extension causes freezing of the parent
subtype.
@begin{Ramification}
This combined with another rule specifying
that primitive subprogram declarations shall precede freezing
ensures that all descendants of a tagged type implement all of
its dispatching operations.

The declaration of a private
  extension does not cause freezing.  The freezing is deferred
  until the full type declaration, which will necessarily be
  for a record extension.
@end{Ramification}
@end{Itemize}


@PDefn2{Term=[freezing], Sec=(by an expression)}
A static expression causes freezing where it occurs.
A nonstatic expression
causes freezing where it occurs, unless
the expression is part of a
@nt<default_expression>, a @nt<default_name>, or a per-object expression
of a component's @nt<constraint>, in which case,
the freezing occurs later as part of another construct.


The following rules define which entities are frozen at the place where
a construct causes freezing:
@begin{Itemize}
@PDefn2{Term=[freezing], Sec=(type caused by an expression)}
At the place where an expression causes freezing,
the type of the expression is frozen,
unless the expression is an enumeration literal used as a
@nt{discrete_choice} of the @nt{array_aggregate} of an
@nt{enumeration_representation_clause}.
@begin{Reason}
We considered making enumeration literals never cause freezing,
which would be more upward compatible,
but examples like the variant record aggregate (Discrim => Red, ...)
caused us to change our mind.
Furthermore, an enumeration literal is a static expression,
so the implementation should be allowed to represent it using its
representation.
@end{Reason}
@begin{Ramification}
The following pathological example was legal in Ada 83,
but is illegal in Ada 9X:
@begin{Example}
@key[package] P1 @key[is]
    @key[type] T @key[is] @key[private];
    @key[package] P2 @key[is]
        @key[type] Composite(D : Boolean) @key[is]
            @key[record]
                @key[case] D @key[is]
                    @key[when] False => Cf : Integer;
                    @key[when] True  => Ct : T;
                @key[end] @key[case];
            @key[end] @key[record];
    @key[end] P2;
    X : Boolean := P2."="( (False,1), (False,1) );
@key[private]
    @key[type] T @key[is] @key[array](1..Func_Call) @key[of] Integer;
@key[end];
@end{Example}

In Ada 9X, the declaration of X freezes Composite
(because it contains an expression of that type),
which in turn freezes T (even though Ct does not exist in this
particular case).
But type T is not completely defined at that point,
violating the rule that a type shall be completely defined before it
is frozen.
In Ada 83, on the other hand, there is no occurrence of the name T,
hence no forcing occurrence of T.
@end{Ramification}

@PDefn2{Term=freezing, Sec=(entity caused by a name)}
At the place where a @nt<name> causes freezing,
the entity denoted by the @nt<name> is frozen, unless
the @nt<name> is a @nt<prefix> of an expanded name;
@PDefn2{Term=[freezing], Sec=(nominal subtype caused by a name)}
at the place where an object @nt{name} causes freezing, the
nominal subtype associated with the @nt<name> is frozen.
@begin{Ramification}
This only matters in the presence of deferred constants or
access types; an @nt{object_declaration} other than a
@nt{deferred_constant_declaration} causes freezing of the nominal
subtype, plus all component junk.

@nt{Implicit_dereference}s are covered by @nt{expression}.
@end{Ramification}

@Redundant[@PDefn2{Term=freezing, Sec=(type caused by a range)}
At the place where a @nt{range} causes freezing, the type of the
@nt<range> is frozen.]
@begin{TheProof}
This is consequence of the facts that
expressions freeze their type,
and the Range attribute is defined to be equivalent
to a pair of expressions separated by ``..''.}
@end{TheProof}

@PDefn2{Term=[freezing],
  Sec=(designated subtype caused by an allocator)}
At the place where an @nt<allocator> causes freezing,
the designated subtype of its type is frozen.
If the type of the @nt<allocator> is a derived type,
then all ancestor types are also frozen.
@begin{Ramification}
  @nt{Allocator}s also freeze the named subtype,
  as a consequence of other rules.

  The ancestor types are frozen to prevent things like this:
@begin{Example}
@key[type] Pool_Ptr @key[is] @key[access] System.Storage_Pools.Root_Storage_Pool'Class;
@key[function] F @key[return] Pool_Ptr;

@key[package] P @key[is]
    @key[type] A1 @key[is] @key[access] Boolean;
    @key[type] A2 @key[is] @key[new] A1;
    @key[type] A3 @key[is] @key[new] A2;
    X : A3 := @key[new] Boolean; --@i{ Don't know what pool yet!}
    @key[for] A1'Storage_Pool @key[use] F.all;
@key[end] P;
@end{Example}

  This is necessary
  because derived access types share their parent's pool.
@end{Ramification}

@PDefn2{Term=[freezing], Sec=(subtypes of the profile of a callable entity)}
At the place where a callable entity is frozen,
each subtype of its profile is frozen.
If the callable entity is a member of an entry family, the
index subtype of the family is frozen.
@PDefn2{Term=[freezing], Sec=(function call)}
At the place where a function call
causes freezing, if a parameter of the call is defaulted,
the @nt{default_expression} for that parameter causes freezing.
@begin{Discussion}
  We don't worry about freezing for procedure calls or entry calls, since
  a body freezes everything that precedes it, and
  the end of a declarative part freezes everything in the declarative
  part.
@end{Discussion}

@PDefn2{Term=[freezing],
  Sec=[type caused by the freezing of a subtype]}
At the place where a subtype is frozen,
its type is frozen.
@PDefn2{Term=[freezing], Sec=(constituents of a full type definition)}
@PDefn2{Term=[freezing],
  Sec=(first subtype caused by the freezing of the type)}
At the place where a type is frozen, any expressions or @nt<name>s within
the full type definition cause freezing;
the first subtype, and
any component subtypes,
index subtypes, and parent subtype
of the type are frozen as well.
@PDefn2{Term=[freezing],
  Sec=(class-wide type caused by the freezing of the specific type)}
@PDefn2{Term=[freezing],
  Sec=(specific type caused by the freezing of the class-wide type)}
For a specific tagged type,
the corresponding class-wide type is frozen as well.
For a class-wide type,
the corresponding specific type is frozen as well.
@begin{Ramification}
  Freezing a type needs to freeze its first subtype in order to
  preserve the property that the subtype-specific aspects of statically
  matching subtypes are the same.

  Freezing an access type does not freeze its designated subtype.
@end{Ramification}
@end{Itemize}

@end{Intro}

@begin{Legality}

@Redundant[The explicit declaration of a primitive subprogram of a
tagged type shall occur before the type is frozen
(see @RefSecNum{Dispatching Operations of Tagged Types}).]
@begin{Reason}
This rule is needed
because (1) we don't want people dispatching to things that haven't
been declared yet, and (2) we want to allow tagged type descriptors
to be static (allocated statically, and initialized to link-time-known
symbols).  Suppose T2 inherits primitive P from T1, and then
overrides P.  Suppose P is called @i{before} the declaration of the
overriding P.  What should it dispatch to?  If the answer is the new
P, we've violated the first principle above.  If the answer is the
old P, we've violated the second principle.  (A call
to the new one necessarily raises Program_Error, but that's
beside the point.)

Note that a call upon a dispatching operation of type @i(T) will freeze @i(T).

We considered applying this rule to all derived types,
for uniformity.
However, that would be upward incompatible,
so we rejected the idea.
As in Ada 83, for an untagged type, the above call upon P will call the
old P (which is arguably confusing).
@end{Reason}

@Redundant[A type shall be completely defined before it is frozen
(see @RefSecNum{Completions of Declarations} and
@RefSecNum{Private Types and Private Extensions}).]

@Redundant[The completion of a deferred constant declaration shall occur
before the constant is frozen
(see @RefSecNum{Deferred Constants}).]

@Redundant[A representation item that directly specifies an aspect of an
entity shall appear before the entity is frozen
(see @RefSecNum{Representation Items}).]


@begin{Discussion}
From RM83-13.1(7).  The wording here forbids freezing
within the @nt{representation_clause} itself, which was not true of the Ada
83 wording.
The wording of this rule is carefully written to
work properly for type-related representation items.
For example, an @nt{enumeration_representation_clause} is illegal
after the type is frozen, even though the @nt{_clause} refers to the
first subtype.
@end{Discussion}
@begin{TheProof}
The above @LegalityTitle are stated ``officially''
in the referenced clauses.
@end{TheProof}
@begin{Discussion}
Here's an example that illustrates when freezing occurs in the
presence of defaults:
@begin{Example}
@key[type] T @key[is] ...;
@key[function] F @key[return] T;
@key[type] R @key[is]
    @key[record]
        C : T := F;
        D : Boolean := F = F;
    @key[end] @key[record];
X : R;
@end{Example}

Since the elaboration of R's declaration does not allocate component C,
there is no need to freeze C's subtype at that place.
Similarly, since the elaboration of R does not evaluate the
@nt{default_expression} ``F = F'', there is no need to freeze the types
involved at that point.
However, the declaration of X @i{does} need to freeze these things.
Note that even if component C did not exist, the elaboration of the
declaration of X would still need information about T @em even though
D is not of type T,
its @nt{default_expression} requires that information.
@end{Discussion}
@begin{Ramification}
Although we define freezing in terms of the program text as a whole
(i.e. after applying the rules of Section 10),
the freezing rules actually have no effect beyond compilation unit
boundaries.
@end{Ramification}
@begin{Reason}
That is important, because Section 10 allows some
implementation definedness in the order of things,
and we don't want the freezing rules to be implementation defined.
@end{Reason}
@begin{Ramification}
These rules also have no effect in @nt{statement}s @em they only apply within
a single @nt{declarative_part}, @nt{package_specification},
@nt{task_definition},
@nt{protected_definition},
or @nt{protected_body}.
@end{Ramification}
@begin{ImplNote}
An implementation may choose to generate code for
@nt{default_expression}s and @nt{default_name}s in
line at the place of use.
@Defn{thunk}
Alternatively, an implementation may choose to generate thunks
(subprograms implicitly generated by the compiler)
for evaluation of defaults.
Thunk generation cannot, in general, be done at the place of the
declaration that includes the default.
Instead, they can be generated at the first freezing point of the
type(s) involved.
(It is impossible to write a purely one-pass Ada compiler,
for various reasons.
This is one of them @em the compiler needs to store a representation of
defaults in its symbol table, and then walk that representation later,
no earlier than the first freezing point.)

In implementation terms, the linear elaboration model can be thought of
as preventing uninitialized dope.
For example, the implementation might generate dope to contain the size
of a private type.
This dope is initialized at the place where the type becomes
completely defined.
It cannot be initialized earlier,
because of the order-of-elaboration rules.
The freezing rules prevent elaboration of earlier declarations from
accessing the size dope for a private type before it is initialized.

@RefSecNum{Pragmas} overrides the freezing rules in the case of
unrecognized @nt{pragma}s.

A @nt{representation_clause} for an entity should most certainly
@i{not} be a freezing point for the entity.
@end{ImplNote}
@end{Legality}

@begin{Incompatible83}
RM83 defines a forcing occurrence of a type as follows:
``A forcing occurrence is any occurrence [of the name of the type,
subtypes of the type, or types or subtypes with subcomponents of the type]
other than in a type or subtype
declaration, a subprogram specification, an entry declaration, a deferred
constant declaration, a @nt{pragma}, or a @nt{representation_clause} for the type
itself. In any case, an occurrence within an expression is always forcing.''

It seems like the wording allows things like this:
@begin{Example}
@key[type] A @key[is] @key[array](Integer @key[range] 1..10) @key[of] Boolean;
@key[subtype] S @key[is] Integer @key[range] A'Range;
    --@i{ not forcing for A}
@end{Example}

Occurrences within @nt{pragma}s can cause freezing in Ada 9X.
(Since such @nt{pragma}s are ignored in Ada 83,
this will probably fix more bugs than it causes.)
@end{Incompatible83}

@begin{Extend83}
In Ada 9X, @nt{generic_formal_parameter_declaration}s
do not normally freeze the entities from which they are defined.
For example:
@begin{example}
@key[package] Outer @key[is]
    @key[type] T @key[is] @key[tagged] @key[limited] @key[private];
    @key[generic]
        @key[type] T2 @key[is]
            @key[new] T @key[with] @key[private]; --@i{ Does not freeze T}
                                --@i{ in Ada 9X.}
    @key[package] Inner @key[is]
        ...
    @key[end] Inner;
@key[private]
    @key[type] T @key[is] ...;
@key[end] Outer;
@end{example}

This is important for the usability of generics.
The above example uses the Ada 9X feature of formal derived types.
Examples using the kinds of formal parameters already allowed in
Ada 83 are well known.
See, for example, comments 83-00627 and 83-00688.
The extensive use expected for formal derived types
makes this issue even more compelling
than described by those comments.
Unfortunately, we are unable to solve the problem that
@nt{explicit_generic_actual_parameter}s cause freezing, even though a package
equivalent to the instance would not cause freezing.
This is primarily because such an equivalent package would have its
body in the body of the containing program unit, whereas an instance
has its body right there.
@end{Extend83}

@begin{DiffWord83}
The concept of freezing is based on Ada 83's concept of ``forcing
occurrences.''
The first freezing point of an entity
corresponds roughly to the place of the first forcing occurrence, in Ada
83 terms.
The reason for changing the terminology is that the new rules do not
refer to any particular ``occurrence'' of a name of an entity.
Instead, we refer to ``uses'' of an entity,
which are sometimes implicit.

In Ada 83, forcing occurrences were used only in rules about
@nt{representation_clause}s.
We have expanded the concept to cover private types,
because the rules stated in RM83-7.4.1(4) are almost identical to the
forcing occurrence rules.

The Ada 83 rules are changed in Ada 9X for the following reasons:
@begin{Itemize}
The Ada 83 rules do not work right for subtype-specific aspects.
In an earlier version of Ada 9X, we considered allowing representation
items to apply to subtypes other than the first subtype.
This was part of the reason for changing the Ada 83 rules.
However, now that we have dropped that functionality,
we still need the rules to be different from the Ada 83 rules.

The Ada 83 rules do not achieve the intended effect.
In Ada 83, either with or without the AIs,
it is possible to force the compiler to generate code that references
uninitialized dope, or force it to detect erroneousness and exception
raising at compile time.

It was a goal of Ada 83 to avoid uninitialized access values.
However, in the case of deferred constants, this goal was not achieved.

The Ada 83 rules are not only too weak @em they are also too strong.
They allow loopholes (as described above),
but they also prevent certain kinds of @nt{default_expression}s that are
harmless,
and certain kinds of @nt{generic_declaration}s that are both harmless
and very useful.

Ada 83 had a case where a @nt{representation_clause} had a strong
effect on the semantics of the program @em 'Small.
This caused certain semantic anomalies.
There are more cases in Ada 9X,
because the @nt{attribute_representation_clause} has been generalized.
@end{Itemize}
@end{DiffWord83}
