@Part(13, Root="ada.mss")

@Comment{$Date: 2015/04/03 04:12:42 $}
@LabeledSection{Representation Issues}

@Comment{$Source: e:\\cvsroot/ARM/Source/13a.mss,v $}
@Comment{$Revision: 1.113 $}

@begin{Intro}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@redundant[This @Chg{Version=[3],New=[clause],Old=[section]} describes features for
querying and controlling @Chg{New=[certain aspects of entities],
Old=[aspects of representation]} and for interfacing to hardware.]
@end{Intro}

@begin{DiffWord83}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
The @Chg{Version=[3],New=[subclauses],Old=[clauses]} of this
@Chg{Version=[3],New=[clause],Old=[section]} have been reorganized.
This was necessary to preserve a logical order,
given the new Ada 95 semantics given in this section.
@end{DiffWord83}

@LabeledRevisedClause{Version=[3],New=[Operational and Representation Aspects],Old=[Representation Items]}
@Comment{@LabeledRevisedClause{Version=[1],New=[Operational and Representation Items],Old=[Representation Items]} Don't have a way to include the middle title}

@begin{Intro}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0295-1]}
@ChgAdded{Version=[1],Text=[@Redundant[@Chg{Version=[3],New=[@Defn{aspect}],
Old=[Representation and operational items can be used to specify aspects of
entities. ]}Two kinds of aspects of entities can be specified:
@Chg{Version=[3],New=[],Old=[aspects of ]}representation@Chg{Version=[3],
New=[ aspects],Old=[]} and operational aspects. Representation
@Chg{Version=[3],New=[aspects affect],Old=[items specify]}
how the types and other entities of the language are to be mapped onto
the underlying machine. Operational @Chg{Version=[3],New=[aspects determine],
Old=[items specify]} other properties of entities.]]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0183-1],ARef=[AI05-0295-1]}
@ChgAdded{Version=[1],Text=[@Redundant[Either kind of aspect of an entity may be
specified by means of an @nt{aspect_specification} (see @RefSecNum{Aspect Specifications}),
which is an optional element
of most kinds of declarations and applies to the entity or entities being
declared. Aspects may also be specified by certain other constructs occurring
subsequent to the declaration of the affected entity: a representation aspect
value may be specified by means of a representation item
and an operational aspect value may be specified by means of an
operational item.]]}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@Defn{representation item}
@RootDefn{representation pragma}
@RootDefn{pragma, representation}
There are @Chg{New=[six],Old=[three]} kinds of @i{representation items}:
@Chg{New=[@nt{attribute_@!definition_@!clause}s for representation attributes,
@nt{enumeration_@!representation_@!clause}s,
@nt{record_@!representation_@!clause}s, @nt{at_clause}s, ],
Old=[@nt{representation_@!clause}s, ]}@nt<component_clause>s, and
@i{representation pragmas}.
@Redundant[@Chg{New=[],Old=[Representation items specify how the types and other entities of
the language are to be mapped onto the underlying machine.]}
They can be provided to give more efficient representation or to
interface with features that are outside the domain of the language
(for example, peripheral hardware).
@Chg{New=[],Old=[Representation items also specify other specifiable
properties of entities.
A representation item applies to an entity identified by
a @nt<local_name>, which denotes an entity declared local to the
current declarative region, or a library unit declared immediately
preceding a representation pragma in a @nt<compilation>.]}]

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgAdded{Version=[1],Text=[An @Defn{operational item}@i<operational item> is an
@nt<attribute_definition_clause> for an operational attribute.]}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgAdded{Version=[1],Text=[@Redundant[An operational item or a representation
item applies to an entity identified by a @nt<local_name>, which denotes an
entity declared local to the current declarative region, or a library unit
declared immediately preceding a representation pragma in a
@nt<compilation>.]]}
@end{Intro}

@begin{Metarules}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
@ChgAdded{Version=[1],Text=[@Chg{Version=[3],New=[Representation
aspects],Old=[Aspects of representation]} are intended to refer to
properties that need to be known before the compiler can generate code to
create or access an entity. For instance, the size of an object needs to be
known before the object can be created. Conversely, operational aspects are
those that only need to be known before they can be used. For instance, how an
object is read from a stream only needs to be known when a stream read is
executed. Thus, @Chg{Version=[3],New=[],Old=[aspects of
]}representation@Chg{Version=[3], New=[ aspects],Old=[]} have stricter rules as
to when they can be specified.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00291-02]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0295-1]}
@ChgAdded{Version=[2],Text=[Confirming the value of an aspect
@Chg{Version=[3],New=[],Old=[with an operational or representation item ]}should
never change the semantics of
the aspect. Thus Size = 8 (for example) means the same thing whether it
was specified with a representation item or whether the compiler chose this
value by default.]}
@end{Metarules}

@ChgToGlossary{Version=[3],Kind=[Added],Term=<Aspect>,
Text=<@ChgAdded{Version=[3],Text=[An aspect is
a specifiable property of an entity. An aspect may be specified by an
@nt{aspect_specification} on the declaration of the entity. Some aspects may be
queried via attributes.]}>}

@begin{Syntax}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@Syn{lhs=<@Chg{New=[aspect_clause],Old=[representation_clause]}>,rhs="@Syn2{attribute_definition_clause}
      | @Syn2{enumeration_representation_clause}
      | @Syn2{record_representation_clause}
      | @Syn2{at_clause}"}

@Syn{lhs=<local_name>,rhs="@Syn2{direct_name}
      | @Syn2{direct_name}@SingleQuote@Syn2{attribute_designator}
      | @SynI{library_unit_}@Syn2{name}"}

@begin{SyntaxText}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
A representation pragma is allowed only at places where
@Chg{New=[an @nt{aspect_clause}],Old=[a @nt{representation_clause}]}
or @nt{compilation_unit} is allowed.
@Chg{New=[@IndexSee(Term=(representation_clause),See=(aspect_clause))],Old=[]}
@end{SyntaxText}
@end{Syntax}

@begin{Resolution}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
In @Chg{New=[an operational item or],Old=[a]} representation item,
if the @nt<local_name> is a @nt<direct_name>, then it shall
resolve to denote a declaration
(or, in the case of a @nt{pragma}, one or more declarations)
that occurs immediately within the same
declarative region as the @Chg{New=[],Old=[representation ]}item.
If the @nt<local_name> has an @nt<attribute_designator>, then it shall
resolve to denote an implementation-defined
component (see @RefSecNum{Record Representation Clauses})
or a class-wide
type implicitly declared immediately within the same
declarative region as the @Chg{New=[],Old=[representation ]}item.
A @nt<local_name> that is a @i{library_unit_}@nt<name> (only
permitted in a representation pragma) shall resolve
to denote the @nt<library_item> that immediately precedes
(except for other pragmas) the representation pragma.
@begin{Reason}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
This is a @ResolutionName,
because we don't want @Chg{New=[an operational or],Old=[a]} representation
item for X to be ambiguous just because there's another X declared in an outer
declarative region.
It doesn't make much difference, since most
@Chg{New=[operational or ],Old=[]}representation items are for types or
subtypes, and type and subtype names can't be overloaded.
@end{Reason}
@begin{Ramification}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
The visibility rules imply that the declaration has to occur
before the @Chg{New=[operational or ],Old=[]}representation item.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
For objects, this implies that @Chg{New=[operational or ],Old=[]}representation items can be
applied only to stand-alone objects.
@end{Ramification}
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
The @nt{local_name} of @Chg{New=[an @nt<aspect_clause>],
Old=[a @nt<representation_clause>]} or representation pragma shall
statically denote an entity (or, in the case of a @nt{pragma},
one or more entities) declared immediately preceding it in a @nt<compilation>,
or within the same @nt{declarative_@!part}, @nt{package_@!specification},
@nt{task_@!definition}, @nt{protected_@!definition}, or
@nt{record_@!definition} as the representation @Chg{New=[or operational ],Old=[]}item.
If a @nt<local_name> denotes a @Redundant[local] callable entity,
it may do so through a @Redundant[local]
@nt<subprogram_@!renaming_@!declaration>
@Redundant[(as a way to resolve ambiguity in the presence of overloading)];
otherwise, the @nt<local_name> shall not denote a @nt<renaming_@!declaration>.
@begin{Ramification}
The @lquotes@;statically denote@rquotes@; part
implies that it is impossible to specify the representation of
an object that is not a stand-alone object,
except in the case of a representation
item like pragma Atomic
that is allowed inside a @nt{component_list}
(in which case the representation item specifies
the representation of components of all objects of the type).
It also prevents the problem of
renamings of things like @lquotes@;P.@key[all]@rquotes@;
(where P is an access-to-subprogram value)
or @lquotes@;E(I)@rquotes@; (where E is an entry family).

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

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00291-02]}
@Defn{representation of an object}
@Defn2{Term=[size], Sec=(of an object)}The @i{representation} of an
object consists of a certain
number of bits (the @i(size) of
the object).
@Chg{Version=[2],New=[For an object of an elementary type, these],Old=[These]}
are the bits that are normally read
or updated by the machine code
when loading, storing, or operating-on the value of the object.
@Chg{Version=[2],New=[For an object of a composite type, these
are the bits reserved for this object, and include bits occupied by
subcomponents of the object. If],Old=[This includes some padding bits, when]}
the size of @Chg{Version=[2],New=[an],Old=[the]} object
is greater than @Chg{Version=[2],New=[that],Old=[the size]} of its subtype@Chg{Version=[2],
New=[, the additional bits are padding bits.],Old=[.
@Defn{gaps}]}
@Defn{padding bits}
@Chg{Version=[2],New=[For an elementary object, these],Old=[Such]} padding bits
@Chg{Version=[2],New=[],Old=[are considered to be part of the representation of the
object, rather than being gaps between objects,
if these bits ]}are normally read and updated@Chg{Version=[2],New=[ along
with the others. For a
composite object, padding bits might not be read or updated in any given
composite operation, depending on the implementation],Old=[]}.

@begin{Honest}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00291-02]}
@PDefn{contiguous representation}
@PDefn{discontiguous representation}
Discontiguous representations are allowed,
but the ones we're interested in here are generally contiguous
sequences of bits.@Chg{Version=[2],New=[ For a discontiguous representation,
the size doesn't necessarily describe the @lquotes@;footprint@rquotes of
the object in memory (that is, the amount of space taken in the address space
for the object).],Old=[]}
@end{Honest}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00291-02]}
@ChgAdded{Version=[2],Text=[In the case of composite objects, we want the
implementation to have the flexibility to either do operations
component-by-component, or with a block operation covering all of the bits. We
carefully avoid giving a preference in the wording. There is no requirement
for the choice to be documented, either, as the implementation can make that
choice based on many factors, and could make a different choice for different
operations on the same object.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00291-02]}
@ChgAdded{Version=[2],Text=[In the case of a properly aligned, contiguous
object whose size is a multiple of the storage unit size, no other bits should
be read or updated as part of operating on the object. We don't say this
normatively because it would be difficult to normatively define
@lquotes@;properly aligned@rquotes or @lquotes@;contiguous@rquotes.]}
@end{Discussion}
@begin{Ramification}
@Leading@;Two objects with the same value do not necessarily have the same
representation.
For example, an implementation might represent False as zero and True
as any odd value.
Similarly, two objects (of the same type)
with the same sequence of bits do not necessarily have the
same value.
For example, an implementation might use a biased representation in
some cases but not others:
@begin{Example}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@key[subtype] S @key[is] Integer @key[range] 1..256;
@key[type] A @key[is] @key[array](Natural @key[range] 1..4) @key[of] S@Chg{Version=[3],New=[
   @key[with] Pack],Old=[;
@key[pragma] Pack(A)]};
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
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
For example, suppose S'Size = 2, and an object X is of subtype S.
If the machine code typically uses a 32-bit load instruction to load the
value of X, then X'Size should be 32, even though 30 bits of the value
are just zeros or sign-extension bits.
On the other hand, if the machine code typically masks out those 30
bits, then X'Size should be 2.
Usually, such masking only happens for components of a composite type
for which @Chg{Version=[3],New=[Pack],Old=[packing]}, Component_Size, or
record layout is specified.

Note, however, that the formal parameter of an instance of
Unchecked_Conversion is a special case.
Its Size is required to be the same as that of its subtype.

Note that we don't generally talk about the representation of a
value.
A value is considered to be an amorphous blob
without any particular representation.
An object is considered to be more concrete.
@end{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0112-1],ARef=[AI05-0295-1]}
@RootDefn{aspect of representation}
@Defn{representation aspect}
@Chg{Version=[3],New=[@Defn2{Term=[directly specified],
  Sec=(of a representation aspect of an entity)}],
Old=[@Defn2{Term=[directly specified],
  Sec=(of an aspect of representation of an entity)}]}
A representation item @i{directly specifies}
@Chg{Version=[3],New=[a @i{representation aspect}],
Old=[an @i{aspect of representation}]} of the entity denoted
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
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
For example, a @nt{pragma} Export
@Chg{Version=[3],New=[(see @RefSecNum{Interfacing Pragmas}) ],Old=[]}specifies the convention
of an entity,
and also specifies that it is exported.@Chg{Version=[3],New=[ Such items are
obsolescent; directly specifying
the associated aspects is preferred.],Old=[]}
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

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0112-1]}
@ChgAdded{Version=[3],Text=[We give a default naming for representation
aspects of representation pragmas so we don't have to do that for every
pragma. Operational and representation attributes are given a default naming
in @RefSecNum{Operational and Representation Attributes}.
We don't want any anonymous aspects; that would make other rules
more difficult to write and understand.]}
@end{Reason}
@begin{Ramification}
@Leading@;The following representation items are type-related:
@begin{Itemize}
@nt{enumeration_representation_clause}

@nt{record_representation_clause}

Component_Size clause

@ChgRef{Version=[1],Kind=[Deleted],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgDeleted{Version=[1],Text=[External_Tag clause]}

Small clause

Bit_Order clause

Storage_Pool clause

Storage_Size clause

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00270-01]}
@ChgAdded{Version=[2],Text=[Stream_Size clause]}

@ChgRef{Version=[1],Kind=[Deleted],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgDeleted{Version=[1],Text=[Read clause]}

@ChgRef{Version=[1],Kind=[Deleted],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgDeleted{Version=[1],Text=[Write clause]}

@ChgRef{Version=[1],Kind=[Deleted],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgDeleted{Version=[1],Text=[Input clause]}

@ChgRef{Version=[1],Kind=[Deleted],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgDeleted{Version=[1],Text=[Output clause]}

Machine_Radix clause

pragma Pack

pragmas Import, Export, and Convention (when applied to a type)

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0009-1]}
pragmas Atomic@Chg{Version=[3],New=[, Independent,],Old=[]} and Volatile
(when applied to a type)

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0009-1]}
pragmas Atomic_Components@Chg{Version=[3],New=[, Independent_Components,],Old=[]}
and Volatile_Components (when applied to @Chg{Version=[3],New=[a],Old=[an
array]} type)

pragma Discard_Names (when applied to an enumeration or tagged type)
@end{Itemize}

@Leading@;The following representation items are subtype-specific:
@begin{Itemize}
Alignment clause (when applied to a first subtype)

Size clause (when applied to a first subtype)
@end{Itemize}

@Leading@;The following representation items do not apply to subtypes,
so they are neither type-related nor subtype-specific:
@begin{Itemize}
Address clause (applies to objects and program units)

Alignment clause (when applied to an object)

Size clause (when applied to an object)

pragmas Import, Export, and Convention (when applied to anything other
than a type)

pragmas Atomic and Volatile (when applied to an object or a component)

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0009-1]}
pragmas Atomic_Components@Chg{Version=[3],New=[, Independent_Components,],Old=[]}
and Volatile_Components (when applied to an array object)

pragma Discard_Names (when applied to an exception)

pragma Asynchronous (applies to procedures)

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00414-01]}
@ChgAdded{Version=[2],Text=[pragma No_Return (applies to subprograms)]}
@end{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[While an @nt{aspect_specification} is not
a representation item, a similar categorization applies to the aspect
that corresponds to each of these representation items (along with
aspects that do not have associated representation items).]}
@end{Ramification}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0183-1]}
@ChgAdded{Version=[1],Text=[An operational item @i<directly specifies> an @i<operational aspect>
of the @Chg{Version=[3],New=[entity],Old=[type of the subtype]} denoted by
the @nt{local_name}@Chg{Version=[3],New=[, except in
the case of a type-related operational item, whose @nt{local_name} shall
denote a first subtype, and which directly specifies an aspect of the
type of the subtype],Old=[. The @nt{local_name}
of an operational item shall denote a first subtype. An operational item that
names a subtype is type-related]}.
@RootDefn{operational aspect}
@Defn2{Term=[directly specified],
  Sec=(of an operational aspect of an entity)}
@RootDefn2{Term=[type-related], Sec=(operational item)}
@Defn2{Term=[type-related], Sec=(aspect)}]}

@begin{Ramification}
@ChgRef{Version=[1],Kind=[AddedNormal],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgAdded{Version=[1],Type=[Leading],Text=[The following operational items are
type-related:]}
@begin{Itemize}

@ChgRef{Version=[1],Kind=[AddedNormal]}
@ChgAdded{Version=[1],Text=[External_Tag clause]}

@ChgRef{Version=[1],Kind=[AddedNormal]}
@ChgAdded{Version=[1],Text=[Read clause]}

@ChgRef{Version=[1],Kind=[AddedNormal]}
@ChgAdded{Version=[1],Text=[Write clause]}

@ChgRef{Version=[1],Kind=[AddedNormal]}
@ChgAdded{Version=[1],Text=[Input clause]}

@ChgRef{Version=[1],Kind=[AddedNormal]}
@ChgAdded{Version=[1],Text=[Output clause]}

@end{Itemize}
@end{Ramification}


@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0116-1]}
A representation item that directly specifies an aspect of a subtype or
type shall appear after the type is completely defined
(see @RefSecNum{Completions of Declarations}),
and before the subtype or type is frozen (see
@RefSecNum{Freezing Rules}).@Chg{Version=[4],New=[],Old=[ If a
representation item @Chg{Version=[3],New=[or @nt{aspect_specification} ],Old=[]}is
given that directly specifies an aspect of an
entity, then it is illegal to give another representation item
@Chg{Version=[3],New=[or @nt{aspect_specification} ],Old=[]}that
directly specifies the same aspect of the entity.]}
@begin{Ramification}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
The fact that a representation item @Chg{New=[(or operational item,
see next paragraph) ],Old=[]}that directly specifies
an aspect of an entity is required to appear before the entity is frozen
prevents changing the representation of an entity
after using the entity in ways that require the representation to be known.
@end{Ramification}
@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
  @ChgRef{Version=[4],Kind=[Deleted],ARef=[AI12-0116-1]}
  @ChgAdded{Version=[3],Text=[@Chg{Version=[4],New=[],Old=[The rule preventing
  multiple specification
  is also intended to cover other ways to specify representation aspects,
  such as obsolescent @nt{pragma} Priority. Priority is not a representation
  pragma, and as such is neither a representation item nor an
  @nt{aspect_specification}. Regardless, giving both a @nt{pragma} Priority
  and an @nt{aspect_specification} for Priority is illegal. We didn't want
  to complicate the wording solely to support obsolescent features.]}]}
@end{Honest}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0183-1]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0116-1]}
@ChgAdded{Version=[1],Text=[An operational item that directly specifies an
aspect of @Chg{Version=[3],New=[an entity],Old=[a type]} shall appear before the
@Chg{Version=[3],New=[entity],Old=[type]} is frozen (see
@RefSecNum{Freezing Rules}).@Chg{Version=[4],New=[],Old=[ If an operational
item @Chg{Version=[3],New=[or
@nt{aspect_specification} ],Old=[]}is given that directly specifies an aspect of
@Chg{Version=[3],New=[an entity],Old=[a type]}, then it is illegal to give
another operational item @Chg{Version=[3],New=[or @nt{aspect_specification}
],Old=[]}that directly specifies the same aspect of the
@Chg{Version=[3],New=[entity],Old=[type]}.]}]}
@begin{Ramification}
  @ChgRef{Version=[1],Kind=[AddedNormal]}
  @ChgAdded{Version=[1],Text=[Unlike representation items, operational
  items can be specified on
  partial views. Since they don't affect the representation, the full
  declaration need not be known to determine their legality.]}
@end{Ramification}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0116-1]}
@ChgAdded{Version=[4],Text=[If a representation item, operational item, or
@nt{aspect_specification} is given that directly specifies an aspect of an
entity, then it is illegal to give another representation item, operational
item, or @nt{aspect_specification} that directly specifies the same aspect of
the entity.]}

@begin{Ramification}
  @ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0116-1]}
  @ChgAdded{Version=[4],Text=[This rule applies to all aspects, not just
  those that are operational aspects or representation aspects. For instance,
  it applies to subtype predicates and type invariants.]}
@end{Ramification}
@begin{Honest}
  @ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0116-1]}
  @ChgAdded{Version=[4],Text=[This rule is also intended to cover other ways
  to specify representation aspects,
  such as obsolescent @nt{pragma} Priority. Priority is not a representation
  pragma, and as such is neither a representation item nor an
  @nt{aspect_specification}. Regardless, giving both a @nt{pragma} Priority
  and an @nt{aspect_specification} for Priority is illegal. We didn't want
  to complicate the wording solely to support obsolescent features.]}
@end{Honest}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0106-1],ARef=[AI05-0295-1]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0116-1]}@Comment{Just a paragraph number change}
@ChgAdded{Version=[3],Text=[Unless otherwise specified, it is illegal to
specify an operational or representation aspect of a generic formal
parameter.]}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Specifying an aspect on a generic formal
  parameter implies an added contract for a generic unit. That contract
  needs to be defined via generic parameter matching rules, and, as
  aspects vary widely, that has to be done for each such aspect. Since
  most aspects do not need this complexity (including all language-defined
  aspects as of this writing), we avoid the complexity by saying that
  such contract-forming aspect specifications are banned unless the
  rules defining them explicitly exist. Note that the method of specification
  does not matter: @nt{aspect_specification}s, representation items,
  and operational items are all covered by this (and similar) rules.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0109-1]}
For an untagged derived type, @Chg{Version=[3],New=[it is illegal to
specify a],Old=[no]} type-related representation
@Chg{Version=[3],New=[aspect],Old=[items are allowed]} if the parent type is a
by-reference type, or has any user-defined primitive
subprograms.@Chg{Version=[4],New=[ Similarly, it is illegal to specify a
nonconfirming type-related representation aspect for
an untagged by-reference type
after one or more types have been derived from it.],Old=[]}
@begin{Ramification}
  @ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
  On the other hand, subtype-specific representation
  @Chg{Version=[3],New=[aspects],Old=[items]} may
  be @Chg{Version=[3],New=[specified],Old=[given]}
  for the first subtype of such a type@Chg{New=[, as can operational
  @Chg{Version=[3],New=[aspects],Old=[items]}],Old=[]}.
@end{Ramification}
@begin{Reason}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1],ARef=[AI05-0295-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0109-1]}
  The reason for forbidding
  @Chg{Version=[3],New=[specification of ],Old=[]}type-related
  representation @Chg{Version=[3],New=[aspects],Old=[items]} on
  untagged by-reference types is because a change of representation
  is impossible when passing by reference (to an inherited
  subprogram).@Chg{Version=[4],New=[ (A by-reference object cannot be copied
  to change its representation.)],Old=[]}
  The reason for forbidding
  @Chg{Version=[3],New=[specification of ],Old=[]}type-related
  representation @Chg{Version=[3],New=[aspects],Old=[items]} on
  untagged types with user-defined primitive subprograms
  was to prevent implicit change of representation for type-related
  aspects of representation upon calling inherited subprograms,
  because such changes of representation are likely to be
  expensive at run time.
  Changes of subtype-specific representation attributes, however, are
  likely to be cheap.
  This rule is not needed for tagged types,
  because other rules prevent a type-related representation
  @Chg{Version=[3],New=[aspect],Old=[item]}
  from changing the representation of the parent part;
  we want to allow
  @Chg{Version=[3],New=[specifying ],Old=[]}a type-related
  representation @Chg{Version=[3],New=[aspect],Old=[item]}
  on a type extension to specify aspects of the extension part.
  For example, @Chg{Version=[3],New=[specifying aspect],Old=[a @nt{pragma}]}
  Pack will cause packing of the extension part, but not of the parent part.
@end{Reason}
@begin{Discussion}
@ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0109-1]}
   @ChgAdded{Version=[4],Text=[@Ldquote@;By-reference type@rdquote usually
   cannot be used in @LegalityTitle, as it is privacy breaking. Our use here
   is privacy breaking, but we're stuck with it for compatibility reasons.
   Since representation aspects cannot be specified on partial views, privacy
   violations only can happen when a type includes a component of a private type.
   In that case, whether these rules are triggered depends on the full type of
   the private type @em which is clearly privacy breaking.]}
@end{Discussion}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01],Ref=[8652/0011],ARef=[AI95-00117-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00326-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
@Chg{New=[Operational and representation],Old=[Representation]} aspects of
a generic formal parameter are the same as those of the actual.
@Chg{New=[Operational and representation aspects
@Chg{Version=[2],New=[],Old=[of a partial view ]}are the
same @Chg{Version=[2],New=[for all views of a type],Old=[as those of the full view]}.],Old=[]}
@Chg{Version=[3],New=[Specification of a],Old=[A]}
type-related representation @Chg{Version=[3],New=[aspect],Old=[item]}
is not allowed for a descendant of a generic formal untagged type.
@begin{Ramification}
  @ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
  @Chg{Version=[3],New=[Specifying representation aspects is],Old=[Representation
  items are]} allowed for
  types whose subcomponent types
  or index subtypes are generic formal types.
  @Chg{New=[@Chg{Version=[3],New=[Specifying operational
  aspects],Old=[Operational items]} and subtype-related representation
  @Chg{Version=[3],New=[aspects is],Old=[items are]}
  allowed on descendants of generic formal types.],Old=[]}
@end{Ramification}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
  Since it is not known whether a formal type has
  user-defined primitive subprograms, specifying
  type-related representation @Chg{Version=[3],New=[aspects],Old=[items]}
  for them is not allowed, unless they are tagged (in which case only
  the extension part is affected in any case).
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00326-01]}
  @ChgAdded{Version=[2],Text=[All views of a type, including the incomplete
  and partial views, have the same operational and representation aspects.
  That's important so that the properties don't change when changing views.
  While most aspects are not available for an incomplete view, we don't want
  to leave any holes by not saying that they are the same.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0083-1]}
  @ChgAdded{Version=[3],Text=[However, this does not apply to objects.
  Different views of an object can have
  different representation aspects. For instance, an actual object passed
  by reference and the associated formal parameter may have different values for
  Alignment even though the formal parameter is merely a view of the
  actual object. This is necessary to maintain the language design principle
  that Alignments are always known at compile time.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
@Chg{Version=[3],New=[The specification of],Old=[A
representation item that specifies]} the Size@Chg{Version=[3],New=[ aspect],Old=[]}
for a given subtype, or the size or storage place for an object (including a component)
of a given subtype, shall allow for enough storage space to
accommodate any value of the subtype.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
@Chg{Version=[3],New=[If a specification of a],Old=[A]}
representation @Chg{New=[or operational ],Old=[]}@Chg{Version=[3],New=[aspect],
Old=[item that]} is
not supported by the implementation@Chg{Version=[3],New=[, it ],Old=[]}is
illegal@Chg{Version=[3],New=[],Old=[,]} or raises an exception at run time.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0295-1]}
@ChgAdded{Version=[2],Text=[A @nt{type_declaration} is illegal if it has one or
more progenitors, and a
@Chg{Version=[3],New=[nonconfirming value was specified
for a ],Old=[]}representation
@Chg{Version=[3],New=[aspect of],Old=[item applies to]}
an ancestor, and this@Chg{Version=[3],New=[],Old=[ representation item]}
conflicts with the representation of
some other ancestor. The cases that cause conflicts are
implementation defined.]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The cases that cause conflicts between the representation of
the ancestors of a @nt{type_declaration}.]}]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[This rule is needed because it may be the case
  that only the combination of types in a type declaration causes a conflict.
  Thus it is not possible, in general, to reject the original representation
  item@Chg{Version=[3],New=[ or @nt{aspect_specification}],Old=[]}.
  For instance:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package} Pkg1 @key{is}
   @key{type} Ifc @key{is interface};
   @key{type} T @key{is tagged record}
      Fld : Integer;
   @key{end record};
   @key{for} T @key{use record}
      Fld @key{at} 0 @key{range} 0 .. Integer'Size - 1;
   @key{end record};
@key{end} Pkg1;]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[Assume the implementation uses a
  single tag with a default offset of zero, and that it allows the use of
  nondefault locations for the tag (and thus accepts representation items
  like the one above). The representation item will force a nondefault
  location for the tag (by putting a component other than the tag into the
  default location). Clearly, this package will be accepted by the
  implementation. However, other declarations could cause trouble. For
  instance, the implementation could reject:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{with} Pkg1;
@key{package} Pkg2 @key{is}
   @key{type} NewT @key{is new} Pkg1.T @key{and} Pkg1.Ifc @key{with null record};
@key{end} Pkg2;]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
  @ChgAdded{Version=[2],Text=[because the declarations of T and Ifc have a
  conflict in their representation items. This is clearly necessary (it's hard
  to imagine how Ifc'Class could work with the tag at a location other than the
  one it is expecting@Chg{Version=[3],New=[ without introducing distributed
  overhead],Old=[]}).]}@Comment{Make Steve happy.}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
  @ChgAdded{Version=[2],Text=[Conflicts will usually involve
  implementation-defined attributes (for specifying the location of the tag,
  for instance), although the example above shows that doesn't have to be the
  case. For this reason, we didn't try to specify exactly what causes a
  conflict; it will depend on the implementation's implementation model and
  what representation @Chg{Version=[3],New=[aspects],Old=[items]} it
  allows@Chg{Version=[3],New=[ to be changed],Old=[]}.]}
@end{Reason}
@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
  @ChgAdded{Version=[2],Text=[An implementation can only use this rule to
  reject @nt{type_declaration}s where one of its ancestors
  @Chg{Version=[3],New=[had a nonconfirming],Old=[has a]} representation
  @Chg{Version=[3],New=[value specified],Old=[item]}.
  An implementation must ensure that
  the default representations of ancestors cannot conflict.]}
@end{ImplNote}
@end{Legality}

@begin{StaticSem}
If two subtypes statically match,
then their subtype-specific aspects (Size and Alignment)
are the same.
@PDefn2{Term=[statically matching],Sec=(effect on subtype-specific aspects)}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
  This is necessary because we allow (for example)
  conversion between access types whose designated subtypes
  statically match.
  Note that @Chg{Version=[3],New=[most aspects (including the subtype-specific aspects
  Size and Alignment) may not be specified for a nonfirst subtype. The only
  language-defined exceptions to this rule are the Static_Predicate and
  Dynamic_Predicate aspects.],Old=[it is illegal to specify an aspect
  (including a subtype-specific one) for a nonfirst subtype.]}

@Leading@Keepnext@;Consider, for example:
@begin{Example}
@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-00114}
@key[package] P1 @key[is]
   @key[subtype] S1 @key[is] Integer @key[range] 0..2**16-1;
   @key[for] S1'Size @key[use] 16; --@RI{ Illegal!}
      --@RI{ S1'Size would be 16 by default.}
   @key[type] A1 @key[is] @key[access] @Chg{New=[@Key[all] ],Old=[]}S1;
   X1: A1;
@key[end] P1;

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-00114}
@key[package] P2 @key[is]
   @key[subtype] S2 @key[is] Integer @key[range] 0..2**16-1;
   @key[for] S2'Size @key[use] 32; --@RI{ Illegal!}
   @key[type] A2 @key[is] @key[access] @Chg{New=[@Key[all] ],Old=[]}S2;
   X2: A2;
@key[end] P2;

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-00114}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@key[procedure] Q @key[is]
   @key[use] P1, P2;
   @key[type] Array1 @key[is] @key[array](Integer @key[range] <>) @key[of] @key[aliased] S1@Chg{Version=[3],New=[
      @key[with] Pack],Old=[;
   @key[pragma] Pack(Array1)]};
   Obj1: Array1(1..100);
   @key[type] Array2 @key[is] @key[array](Integer @key[range] <>) @key[of] @key[aliased] S2@Chg{Version=[3],New=[
      @key[with] Pack],Old=[;
   @key[pragma] Pack(Array2)]};
   Obj2: Array2(1..100);
@key[begin]
   X1 := Obj2(17)'@Chg{New=[Unchecked_],Old=[]}Access;
   X2 := Obj1(17)'@Chg{New=[Unchecked_],Old=[]}Access;
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

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0040],ARef=[AI95-00108-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0009-1],ARef=[AI05-0295-1]}
A derived type inherits each type-related
@Chg{Version=[3],New=[representation ],Old=[]}aspect
@Chg{Version=[3],New=[],Old=[@Chg{New=[of representation ],Old=[]}]}of
its parent type that
was directly specified before the declaration of the derived type,
or (in the case where the parent is derived)
that was inherited by the parent type from the grandparent type.
A derived subtype inherits each subtype-specific
@Chg{Version=[3],New=[representation ],Old=[]}aspect
@Chg{Version=[3],New=[],Old=[@Chg{New=[of representation ],Old=[]}]}of
its parent subtype that
was directly specified before the declaration of the derived type,
or (in the case where the parent is derived)
that was inherited by the parent subtype from the grandparent subtype,
but only if the parent subtype statically matches the first subtype of
the parent type.
An inherited @Chg{Version=[3],New=[representation ],Old=[]}aspect
@Chg{Version=[3],New=[],Old=[of representation ]}is
overridden by a subsequent
@Chg{Version=[3],New=[@nt{aspect_specification} or ],Old=[]}representation
item that specifies @Chg{Version=[3],New=[a different value for ],Old=[]}the
same aspect of the type or subtype.
@begin{Honest}
A @nt{record_representation_clause} for a record extension
does not override the layout of the parent part;
if the layout was specified for the parent type,
it is inherited by the record extension.
@end{Honest}
@begin{Ramification}
  If a representation item for the parent appears after the
  @nt{derived_@!type_@!definition},
  then inheritance does not happen for that representation item.

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0009-1],ARef=[AI05-0295-1]}
  @ChgAdded{Version=[3],Text=[If an inherited aspect is confirmed by
  an @nt{aspect_specification} or a later representation item for a derived type,
  the confirming specification does
  not override the inherited one. Thus the derived type has both a specified
  confirming value and an inherited nonconfirming representation value @em this
  means that rules that apply only to nonconfirming representation values still
  apply to this type.]}

  @ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0109-1]}
  @ChgAdded{Version=[4],Text=[If an aspect was specified by an
  @nt{aspect_specification} and the parent type has not yet been frozen, then
  the inherited aspect might not yet have been resolved and evaluated. The
  implementation will need to have a mechanism to handle such an aspect.]}
@end{Ramification}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0040],ARef=[AI95-00108-01]}
@ChgRef{Version=[2],Kind=[RevisedAdded],ARef=[AI95-00444-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0183-1],ARef=[AI05-0295-1]}
@ChgAdded{Version=[1],Text=[In contrast, whether operational aspects are
inherited by @Chg{Version=[3],New=[a],Old=[@Chg{Version=[2],New=[an untagged],Old=[a]}]}
derived type depends on each specific aspect@Chg{Version=[3],New=[; unless specified,
an operational aspect is not inherited],Old=[]}. @Chg{Version=[3],New=[],
Old=[@Chg{Version=[2],New=[@Redundant[Operational aspects are never inherited
for a tagged type.] ],Old=[]}]}When operational aspects are
inherited by @Chg{Version=[3],New=[a],Old=[@Chg{Version=[2],New=[an untagged],Old=[a]}]} derived type,
aspects that were directly specified @Chg{Version=[2],New=[by
@Chg{Version=[3],New=[@nt{aspect_specification}s or ],Old=[]}operational
items that are visible at the point],Old=[before the declaration]} of
the derived type@Chg{Version=[2],New=[ declaration],Old=[]}, or
(in the case where the parent is derived)
that were inherited by the parent type from the grandparent type are inherited.
An inherited operational aspect is overridden by a subsequent
@Chg{Version=[3],New=[@nt{aspect_specification} or ],Old=[]}operational
item that specifies the same aspect of the type.]}
@begin{Ramification}
@ChgRef{Version=[1],Kind=[Added]}
@ChgAdded{Version=[1],Text=[As with representation items, if an operational
item for the parent appears after the @nt{derived_@!type_@!definition}, then
inheritance does not happen for that operational item.]}
@end{Ramification}
@begin{Discussion}
@ChgRef{Version=[1],Kind=[Added]}
@ChgRef{Version=[2],Kind=[RevisedAdded],ARef=[AI95-00444-01]}
@ChgRef{Version=[3],Kind=[DeletedAddedNoDelMsg],ARef=[AI05-0183-1]}
@ChgDeleted{Version=[3],Text=[@Chg{Version=[1],New=[@Chg{Version=[2],New=[Only],Old=[Currently, only]}
untagged types inherit operational aspects.
@Chg{Version=[2],New=[Inheritance from tagged types causes problems, as the
different views can have different visibility on operational items @em
potentially leading to operational items that depend on the view. We want
aspects to be the same for all views. Untagged types don't have this problem
as plain private types don't have ancestors, and thus can't inherit anything.
In addition, it seems unlikely that we'll need inheritance for tagged types,
as usually we'll want to incorporate the parent's operation into a new one that
also handles any extension components.],Old=[We considered writing this rule
that way, but rejected it as that could be too
specific for future operational aspects. (After all, that is precisely the
problem that caused us to introduce @lquotes@;operational aspects@rquotes in
the first place.)]}],Old=[]}]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00444-01]}
@ChgAdded{Version=[2],Text=[When an aspect that is a subprogram is inherited,
the derived type inherits the aspect in the same way that a derived type
inherits a user-defined primitive subprogram from its parent (see
@RefSecNum{Derived Types and Classes}).]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This defines the parameter names and types,
and the needed implicit conversions.]}
@end{Reason}

@Leading@;Each aspect of representation of an entity is as follows:
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
specified value. For example, if the user writes @lquotes@;@key{for}
X'Size @key{use} 32;@rquotes@;,
then a query of X'Size will return 32.
@end{Ramification}

@PDefn{unspecified}
If an aspect of representation of an entity is not specified,
it is chosen by default in an unspecified manner.
@end{Itemize}
@begin{Ramification}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
Note that @Chg{New=[@Chg{Version=[3],New=[specifying a ],Old=[]}representation
@Chg{Version=[3],New=[aspect],Old=[items]}],Old=[@nt{representation_clause}s]}
can affect the semantics of the entity.

The rules forbid things like
@lquotes@;@key[for] S'Base'Alignment @key[use] ...@rquotes@;
and
@lquotes@;@key[for] S'Base @key[use] record ...@rquotes@;.
@end{Ramification}
@begin{Discussion}
The intent is that implementations will represent the
components of a composite value in the same way for all subtypes of a
given composite type.
Hence, Component_Size and record layout are type-related aspects.
@end{Discussion}
@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0083-1]}
@ChgAdded{Version=[3],Text=[As noted previously, in the case of an
object, the entity mentioned in this text is a specific view of an object. That
means that only references to the same view of an object that has a specified
value for a representation aspect @i<R> necessarily have that value for the
aspect @i<R>. The value of the aspect @i<R> for a different view of that object
is unspecified. In particular, this means that the representation values for
by-reference parameters is unspecified; they do not have to be the same as those
of the underlying object.]}
@end{Ramification}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0040],ARef=[AI95-00108-01]}
@ChgAdded{Version=[1],Text=[@Defn2{Term=[specified], Sec=(of an operational aspect of an entity)}
If an operational aspect is @i<specified> for an entity (meaning
that it is either directly specified or inherited), then that aspect of the
entity is as specified. Otherwise, the aspect of the entity has the default
value for that aspect.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00291-02]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0295-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[An @nt{aspect_specification}
or ],Old=[A]}representation
item that specifies @Chg{Version=[3],New=[a],Old=[an aspect of]}
representation@Chg{Version=[3],New=[ aspect],Old=[]}
that would have been chosen in the absence of the
@Chg{Version=[3],New=[@nt{aspect_specification} or ],Old=[]}
representation item is said to be
@i{confirming}.@Defn2{Term=[confirming], Sec=(representation item)}@Chg{Version=[3],New=[
The aspect value specified in this case is said to be a
@i<confirming> representation aspect value. Other values of the aspect
are said to be @i<nonconfirming>, as are the
@nt{aspect_specification}s and representation items that specified
them.@Defn2{Term=[confirming], Sec=(representation value)}
@Defn2{Term=[confirming], Sec=(aspect specification)}
@Defn2{Term=[nonconfirming], Sec=(representation value)}
@Defn2{Term=[nonconfirming], Sec=(representation item)}
@Defn2{Term=[nonconfirming], Sec=(aspect specification)}],Old=[]}]}
@end{StaticSem}

@begin{RunTime}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@Chg{New=[@PDefn2{Term=[elaboration], Sec=(aspect_clause)}],
Old=[@PDefn2{Term=[elaboration], Sec=(representation_clause)}]}
For the elaboration of @Chg{New=[an @nt{aspect_clause}],
Old=[a @nt{representation_clause}]},
any evaluable constructs within it are evaluated.
@begin{Ramification}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
Elaboration of representation pragmas is covered by the
general rules for pragmas in @Chg{Version=[3],New=[@RefSecNum{Pragmas}],Old=[Section 2]}.
@end{Ramification}
@end{RunTime}

@begin{ImplPerm}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
An implementation may interpret
@Chg{Version=[3],New=[],Old=[aspects of
]}representation@Chg{Version=[3],New=[ aspects],Old=[]}
in an implementation-defined manner.
An implementation may place implementation-defined restrictions on
@Chg{Version=[3],New=[the specification of ],Old=[]}representation
@Chg{Version=[3],New=[aspects],Old=[items]}.
@RootDefn{recommended level of support}
A @i{recommended level of support} is
@Chg{Version=[3],New=[defined],Old=[specified]} for
@Chg{Version=[3],New=[the specification of ],Old=[]}representation
@Chg{Version=[3],New=[aspects],Old=[items]}
and related features in each subclause.
These recommendations are changed to requirements
for implementations that support the Systems Programming Annex
(see @RefSec{Required Representation Support}).

@ChgImplDef{Version=[3],Kind=[Revised],InitialVersion=[0],Text=[The
interpretation of each @Chg{Version=[3],New=[],Old=[aspect of
]}representation@Chg{Version=[3],New=[ aspect],Old=[]}.]}

@ChgImplDef{Version=[3],Kind=[Revised],InitialVersion=[0],Text=[Any restrictions
placed upon @Chg{Version=[3],New=[the specification of ],Old=[]}representation
@Chg{Version=[3],New=[aspects],Old=[items]}.]}

@begin{Ramification}
Implementation-defined restrictions may be enforced either at compile
time or at run time.
There is no requirement that an implementation justify any such
restrictions.
They can be based on avoiding implementation complexity,
or on avoiding excessive inefficiency, for example.

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgAdded{Version=[1],Text=[There is no such permission for operational aspects.]}
@end{Ramification}
@end{ImplPerm}

@begin{ImplAdvice}
@Leading@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
@PDefn2{Term=[recommended level of support], Sec=(with respect to
nonstatic expressions)}
The recommended level of support for
@Chg{Version=[3],New=[the specification of ],Old=[]}all representation
@Chg{Version=[3],New=[aspects],Old=[items]}
is qualified as follows:
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00291-02]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0295-1]}
@ChgAdded{Version=[2],Text=[A confirming
@Chg{Version=[3],New=[specification for a ],Old=[]}representation
@Chg{Version=[3],New=[aspect],Old=[item]}
should be supported.]}
@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
  @ChgAdded{Version=[2],Text=[A confirming representation
  @Chg{Version=[3],New=[aspect value],Old=[item]} might not be possible for some
  entities. For instance, consider an unconstrained array. The size of such a
  type is implementation-defined, and might not actually be a representable
  value, or might not be static.]}
@end{Honest}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
An implementation need not support @Chg{Version=[3],New=[the specification for
a ],Old=[]}representation
@Chg{Version=[3],New=[aspect that contains],Old=[items containing]}
nonstatic expressions,
@Chg{Version=[3],New=[unless],Old=[except that an implementation should
support a representation item for a given entity if]} each nonstatic
expression@Chg{Version=[3],New=[],Old=[ in the representation item]}
is a @Chg{Version=[3],New=[@nt{name}],Old=[name]} that statically denotes
a constant declared before the entity.
@begin{Reason}
@Leading@;This is to avoid the following sort of thing:
@begin{Example}
X : Integer := F(...);
Y : Address := G(...);
@key[for] X'Address @key[use] Y;
@end{Example}

In the above, we have to evaluate the initialization expression for X
before we know where to put the result.
This seems like an unreasonable implementation burden.

@Leading@;The above code should instead be written like this:
@begin{Example}
Y : @key[constant] Address := G(...);
X : Integer := F(...);
@key[for] X'Address @key[use] Y;
@end{Example}

This allows the expression @lquotes@;Y@rquotes@; to be safely evaluated before X is
created.

The constant could be a formal parameter of mode @key[in].

An implementation can support other nonstatic expressions if it wants
to. Expressions of type Address are hardly ever static,
but their value might be known at compile time anyway
in many cases.
@end{Reason}

An implementation need not support a specification for the
Size for a given composite subtype, nor the size or storage place for an object
(including a component) of a given composite subtype, unless the constraints
on the subtype and its composite subcomponents (if any)
are all static constraints.


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00291-02]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
@Chg{Version=[2],New=[An implementation need not support
@Chg{Version=[3],New=[specifying ],Old=[]}a nonconfirming
representation @Chg{Version=[3],New=[aspect value],Old=[item]}
if it could cause an aliased object or an object of a
by-reference type to be allocated at a nonaddressable location or, when the
alignment attribute of the subtype of such an object is nonzero, at an address
that is not an integral multiple of that alignment.],Old=[An aliased component,
or a component whose type is by-reference, should always be allocated at an
addressable location.]}
@begin{Reason}
@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-0079}
The intent is that access types, type System.Address,
and the pointer used for a by-reference parameter should
be implementable as a single machine address @em bit-field pointers
should not be required.
(There is no requirement that this implementation be used @em we just
want to make sure @Chg{New=[it's],Old=[its]} feasible.)
@end{Reason}
@begin{ImplNote}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00291-02]}
@Chg{Version=[2],New=[We want subprograms to be able to assume the properties
of the types of their parameters inside of subprograms. While many objects
can be copied to allow this
(and thus do not need limitations), aliased or by-reference objects cannot
be copied (their memory location is part of their identity). Thus,],
Old=[Note that]}
the above rule does not apply to types that merely allow
by-reference parameter passing;
for such types, a copy typically needs to be made at the call site
when a bit-aligned component is passed as a parameter.
@end{ImplNote}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00291-02]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
@ChgAdded{Version=[2],Text=[An implementation need not support
@Chg{Version=[3],New=[specifying ],Old=[]}a nonconfirming
representation @Chg{Version=[3],New=[aspect value],Old=[item]}
if it could cause an aliased object of an elementary type
to have a size other than that which would have been chosen by default.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Since all bits of elementary objects participate
  in operations, aliased objects must not have a different size than that
  assumed by users of the access type.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00291-02]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
@ChgAdded{Version=[2],Text=[An implementation need not support
@Chg{Version=[3],New=[specifying ],Old=[]}a nonconfirming
representation @Chg{Version=[3],New=[aspect value],Old=[item]}
if it could cause an aliased object of a composite type, or
an object whose type is by-reference, to have a size smaller than that which
would have been chosen by default.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Unlike elementary objects, there is no
  requirement that all bits of a composite object participate in operations.
  Thus, as long as the object is the same or larger in size than that expected
  by the access type, all is well.]}
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This rule presumes that the implementation
  allocates an object of a size specified to be larger than the default size in
  such a way that access of the default size suffices to correctly read and
  write the value of the object.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00291-02]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
@ChgAdded{Version=[2],Text=[An implementation need not support
@Chg{Version=[3],New=[specifying ],Old=[]}a nonconfirming
subtype-specific representation @Chg{Version=[3],New=[aspect value
for],Old=[item specifying an aspect of representation of]}
an indefinite or abstract subtype.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[Representation aspects],
  Old=[Aspects of representations]} are often not
  well-defined for such types.]}
@end{Reason}

@begin{Ramification}
@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-00075}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00291-02]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Chg{Version=[3],New=[A type with the Pack aspect specified],Old=[A pragma Pack]}
will typically not @Chg{Version=[3],New=[be packed],Old=[pack]} so tightly as to
disobey the above
@Chg{Version=[2],New=[rules],Old=[rule]}. A Component_Size clause or
@nt{record_representation_clause} will typically @Chg{New=[be],Old=[by]}
illegal if it disobeys the above @Chg{Version=[2],New=[rules],Old=[rule]}.
Atomic components have similar restrictions
(see @RefSec{Shared Variable Control}).
@end{Ramification}
@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00291-02]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0295-1]}
@ChgAdded{Version=[2],Text=[For purposes of these rules, the determination of
whether @Chg{Version=[3],New=[specifying ],Old=[]}a
representation @Chg{Version=[3],New=[aspect value for],Old=[item applied to]}
a type @i{could cause} an object to have
some property is based solely on the properties of the type itself, not on any
available information about how the type is used. In particular, it presumes
that minimally aligned objects of this type might be declared at some point.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The recommended level of support for all representation items should be
followed.]}]}
@end{ImplAdvice}

@begin{Notes}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[Aspects that can be specified are defined
    throughout this International Standard, and are summarized in
    @RefSecNum{Language-Defined Aspects}.]}
@end{Notes}


@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
It is now illegal for a representation item to cause a derived
by-reference type to have a different record layout from its
parent.
This is necessary for by-reference parameter passing to be feasible.
This only affects programs that specify the representation of types
derived from types containing tasks;
most by-reference types are new to Ada 95.
For example, if A1 is an array of tasks, and A2 is derived from A1,
it is illegal to apply a @nt{pragma} Pack to A2.
@end{Incompatible83}

@begin{Extend83}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@Defn{extensions to Ada 83}
Ada 95 allows additional @Chg{New=[@nt{aspect_clause}s],
Old=[@nt{representation_clause}s]} for objects.
@end{Extend83}

@begin{DiffWord83}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
The syntax rule for @ntf{type_representation_clause} is removed;
the right-hand side of that rule is moved up to where it was used,
in @Chg{New=[@nt{aspect_clause}],Old=[@nt{representation_clause}]}.
There are two references to @lquotes@;type representation clause@rquotes@; in RM83,
both in Section 13; these have been reworded.
@Chg{New=[Also, the @ntf{representation_clause} has been renamed the
@nt{aspect_clause} to reflect that it can be used to control more than just
representation aspects.],Old=[]}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
We have defined a new term @lquotes@;representation item,@rquotes@;
which includes @Chg{New=[all representation clauses],
Old=[@nt{representation_clause}s]} and representation pragmas, as well as
@nt<component_clause>s.
This is convenient because the rules are almost identical for all @Chg{New=[of them],
Old=[three]}. @Chg{New=[We have also defined the new terms
@lquotes@;operational item@rquotes@; and @lquotes@;operational aspects@rquotes@;
in order to conveniently handle new types of @Chg{Version=[2],New=[specifiable],Old=[specifable]} entities.],Old=[]}

All of the forcing occurrence stuff has been moved into its own
subclause (see @RefSecNum{Freezing Rules}),
and rewritten to use the term @lquotes@;freezing@rquotes@;.

RM83-13.1(10) requires implementation-defined restrictions on
representation items to be enforced at compile time.
However, that is impossible in some cases.
If the user specifies a junk (nonstatic) address in an address
clause, and the implementation chooses to detect the error (for example,
using hardware memory management with protected pages), then it's
clearly going to be a run-time error.
It seems silly to call that @lquotes@;semantics@rquotes@; rather than
@lquotes@;a restriction.@rquotes@;

RM83-13.1(10) tries to pretend that @ntf{representation_clause}s don't affect
the semantics of the program.
One counter-example is the Small clause.
Ada 95 has more counter-examples.
We have noted the opposite above.

Some of the more stringent requirements are moved to
@RefSec{Required Representation Support}.
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00291-02]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @b[Amendment Correction:] Confirming representation items are
  defined, and the recommended level of support is now that they always
  be supported.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0009],ARef=[AI95-00137-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Added operational items
  in order to eliminate unnecessary restrictions and permissions on
  stream attributes. As part of this, @ntf{representation_clause} was
  renamed to @nt{aspect_clause}.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0009],ARef=[AI95-00137-01],ARef=[AI95-00326-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Added wording to say that the
  partial and full views have the same operational and representation aspects.
  Ada 2005 extends this to cover all views, including the incomplete view.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0040],ARef=[AI95-00108-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Changed operational items
  to have inheritance specified for each such aspect.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
  @ChgAdded{Version=[2],Text=[Added wording to allow the rejection of
  types with progenitors that have conflicting representation items.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00291-02]}
  @ChgAdded{Version=[2],Text=[The description of the representation of an
  object was clarified (with great difficulty reaching agreement). Added
  wording to say that representation items on aliased and by-reference objects
  never need be supported if they would not be implementable without
  distributed overhead even if other recommended level of support says
  otherwise. This wording matches the rules with reality.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00444-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[Added wording so that inheritance depends on
  whether operational items are visible rather than whether they occur before
  the declaration (we don't want to look into private parts).
  @Chg{Version=[3],New=[Also limited],Old=[Limited]}
  operational inheritance to untagged types to avoid
  @Chg{Version=[3],New=[anomalies],Old=[anomolies]} with private extensions
  (this is not incompatible, no existing operational attribute used this
  capability). Also added wording to clearly define that subprogram inheritance
  works like derivation of subprograms.]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0106-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b<Correction:>
  Specifying a language-defined aspect for a generic formal parameter is no
  longer allowed. Most aspects could not be specified on these anyway; moreover,
  this was not allowed in Ada 83, so it is unlikely that compilers are
  supporting this as a capability (and it is not likely that they have a
  consistent definition of what it means if it is allowed). Thus, we expect
  this to occur rarely in existing programs.]}
@end{Incompatible2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0009-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Defined that overriding of
  an representation aspect only happens for a nonconfirming representation
  item. This prevents a derived type from being considered to have
  only a confirming representation item when the value would be nonconfirming
  if given on a type that does not inherit any aspects of representation.
  This change just eliminates a wording confusion and ought not change any
  behavior.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0112-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Defined a default naming for
  representation aspects that are representation pragmas.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
  @ChgAdded{Version=[3],Text=[Added text ensuring that the rules for
  representational and operational items also apply appropriately to
  @nt{aspect_specification}s; generalized operational aspects so that they
  can be defined for entities other than types. Any extensions are documented
  elsewhere.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0295-1]}
  @ChgAdded{Version=[3],Text=[Rewrote many rules to be in terms of
  "specifying a representation aspect" rather than use of
  a "representation item". This better separates @i<how> an aspect is
  specified from @i<what> rules apply to the value of the aspect.]}
@end{DiffWord2005}

@begin{Incompatible2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0109-1]}
  @ChgAdded{Version=[4],Text=[@Defn{incompatibilities with Ada 2012}@b<Corrigendum:>
  Added a rule that makes it illegal to specify a representation value after
  a type is derived from an untagged by-reference type. This restriction is
  incompatible, but since the implementation would have had to copy an object
  that does not allow copying in order to change the representation for any
  implicit or explicit conversion between the original and the derived type,
  it is unlikely that any program could exist without running into internal
  compiler errors or bogus results.]}
@end{Incompatible2012}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0116-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Clarified that an aspect
  (any aspect) can be specified only once for an entity, no matter what
  means of specifying it are used. We did not document this as an
  incompatibility as only aspects that are neither operational nor
  representation could change behavior and there is no known implementation
  of these new aspects that allows multiple definitions.]}
@end{DiffWord2012}


@LabeledAddedSubClause{Version=[3],Name=[Aspect Specifications]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@ChgAdded{Version=[3],Text=[@Redundant[Certain representation or operational
aspects of an entity may be specified as part of its declaration using an
@nt{aspect_specification}, rather than using a separate representation or
operational item.] The declaration with the @nt{aspect_specification} is termed
the @i{associated declaration}.@Defn2{Term=[associated declaration],Sec=[of an
aspect specification]}]}
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<aspect_specification>,Old=<>}>,
rhs=`@Chg{Version=[3],New="
   @key[with] @Syn2{aspect_mark} [=> @Syn2{aspect_definition}] {,
           @Syn2{aspect_mark} [=> @Syn2{aspect_definition}] }",Old=<>}'}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<aspect_mark>,Old=<>}>,
rhs="@Chg{Version=[3],New=<@SynI<aspect_>@Syn2<identifier>['Class]>,Old=<>}"}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<aspect_definition>,Old=<>}>,
rhs="@Chg{Version=[3],New=<@Syn2<name> | @Syn2<expression> | @Syn2<identifier>>,Old=<>}"}

@end{Syntax}

@begin{Metarules}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1],ARef=[AI05-0267-1]}
  @ChgAdded{Version=[3],Text=[The @nt{aspect_specification} is an
   optional element in most kinds of declarations. Here is a list
   of all kinds of declarations and an indication
   of whether or not they allow aspect clauses, and in some cases
   a short discussion of why (* = allowed, NO = not allowed). Kinds
   of declarations with no indication are followed by their subdivisions
   (which have indications).]}

@begin{Display}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0005-1]}
@ChgAdded{Version=[3],Text=[@nt{basic_declaration}
  @nt{type_declaration}
    @nt{full_type_declaration}
      @i<type declaration syntax>*
      @nt{task_type_declaration}*
      @nt{protected_type_declaration}*
    @nt{incomplete_type_declaration}  --  NO
      -- @Examcom{Incomplete type aspects cannot be read by an attribute or specified by @nt{attribute_definition_clause}s }
      -- @Examcom{(the attribute name is illegal), so it would not make sense to allow this in another way.}
    @nt{private_type_declaration}*
    @nt{private_extension_declaration}*
  @nt{subtype_declaration}*
  @nt{object_declaration}
    @i<object declaration syntax>*
    @nt{single_task_declaration}*
    @nt{single_protected_declaration}*
  @nt{number_declaration}  --  NO
  @nt{subprogram_declaration}*
  @nt{abstract_subprogram_declaration}*
  @nt{null_procedure_declaration}*@Chg{Version=[4],New=[
  @nt{expression_function_declaration}*],Old=[]}
  @nt{package_declaration}*  -- @Examcom{via} @nt{package_specification}
  @nt{renaming_declaration}*
    -- @Examcom{There are no language-defined aspects that may be specified}
    -- @Examcom{on renames, but implementations might support some.}
  @nt{exception_declaration}*
  @nt{generic_declaration}
    @nt{generic_subprogram_declaration}*
    @nt{generic_package_declaration}* -- @Examcom{via} @nt{package_specification}
  @nt{generic_instantiation}*
@nt{enumeration_literal_specification}  --  NO
@nt{discriminant_specification}  --  NO
@nt{component_declaration}*
@nt{loop_parameter_specification}  --  NO
@nt{iterator_specification}  --  NO
@nt{parameter_specification}  --  NO
@nt{subprogram_body}*  --  @Examcom{ - but language-defined aspects only if there is no explicit specification}
@nt{entry_declaration}*
@nt{entry_index_specification}  --  NO
@nt{subprogram_body_stub}*  --  @Examcom{ - but language-defined aspects only if there is no explicit specification}
@nt{choice_parameter_specification}  --  NO
@nt{generic_formal_parameter_declaration}
    -- @Examcom{There are no language-defined aspects that may be specified}
    -- @Examcom{on generic formals, but implementations might support some.}
  @nt{formal_object_declaration}*
  @nt{formal_type_declaration}*
  @nt{formal_subprogram_declaration}
    @nt{formal_concrete_subprogram_declaration}*
    @nt{formal_abstract_subprogram_declaration}*
  @nt{formal_package_declaration}*
@nt{extended_return_statement}  --  NO]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[-- @Examcom{We also allow @nt{aspect_specification}s on all kinds of bodies, but are no language-defined aspects}
-- @Examcom{that may be specified on a body. These are allowed for implementation-defined aspects.}
-- @Examcom{See above for subprogram bodies and stubs (as these can be declarations).}
@nt{package_body}*
@nt{task_body}*
@nt{protected_body}*
@nt{package_body_stub}*
@nt{task_body_stub}*
@nt{protected_body_stub}*]}
@end{Display}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0267-1]}
  @ChgAdded{Version=[3],Text=[Syntactically, @nt{aspect_specification}s
  generally are located at the end of declarations. When a declaration is all in
  one piece such as a @nt{null_procedure_declaration}, @nt{object_declaration},
  or @nt{generic_instantiation} the @nt{aspect_specification} goes at the end of
  the declaration; it is then more visible and less likely to interfere with the
  layout of the rest of the structure. However, we make an exception for program
  units (other than subprogram specifications) and bodies, in which the
  @nt{aspect_specification} goes before the @key[is]. In these cases, the entity
  could be large and could contain other declarations that also have
  @nt{aspect_specification}s, so it is better to put the
  @nt{aspect_specification} toward the top of the declaration. (Some aspects @en
  such as Pure @en also affect the legality of the contents of a unit, so it
  would be annoying to only see those after reading the entire unit.)]}

@end{Metarules}

@begin{Resolution}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[An @nt{aspect_mark} identifies an
aspect of the entity defined by the associated declaration (the @i<associated
entity>@Defn2{Term=[associated entity],Sec=[of an aspect specification]}); the
aspect denotes an object, a value, an expression, a subprogram, or some other
kind of entity. If the @nt{aspect_mark} identifies:]}
@begin{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[an aspect that denotes an
object, the @nt{aspect_definition} shall be a @nt{name}. The expected type for
the @nt{name} is the type of the identified aspect of the associated
entity;@PDefn2{Term=[expected type], Sec=[object in an @nt{aspect_specification}]}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[an aspect that is
a value or an expression, the @nt{aspect_definition} shall be an @nt{expression}.
The expected type for the @nt{expression} is the type of the identified aspect
of the associated
entity;@PDefn2{Term=[expected type],
Sec=[@nt{name} in an @nt{aspect_specification}]}@PDefn2{Term=[expected type],
Sec=[@nt{expression} in an @nt{aspect_specification}]}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[an aspect that denotes a subprogram, the
@nt{aspect_definition} shall be a @nt{name}; the expected profile for the @nt{name}
is the profile required for the aspect of the associated
entity;@PDefn2{Term=[expected profile], Sec=[@nt{name} in an @nt{aspect_specification}]}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[an aspect that
denotes some other kind of entity, the @nt{aspect_definition} shall be a
@nt{name}, and the name shall resolve to denote an entity of the appropriate
kind;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[an aspect that is given by an identifier specific to
the aspect, the @nt{aspect_definition} shall be an @nt{identifier}, and the
@nt{identifier} shall be one of the identifiers specific to the identified
aspect.]}
@end{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@ChgAdded{Version=[3],Text=[The usage names in an @nt{aspect_definition}
@Redundant[ are not resolved at the point of the associated declaration, but
rather] are resolved at the end of the immediately enclosing declaration list.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@ChgAdded{Version=[3],Text=[If the associated declaration is for a subprogram or
entry, the names of the formal parameters are directly visible within the
@nt{aspect_definition}, as are certain attributes, as specified elsewhere in
this International Standard for the identified aspect. If the associated
declaration is a @nt{type_declaration}, within the @nt{aspect_definition} the
names of any components are directly visible, and the name of the first subtype
denotes the current instance of the type (see
@RefSecNum{The Context of Overload Resolution}). If the associated declaration
is a @nt{subtype_declaration}, within the @nt{aspect_definition} the name of
the new subtype denotes the current instance of the subtype.]}
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@ChgAdded{Version=[3],Text=[If the first freezing point of the associated entity
comes before the end of the immediately enclosing declaration list, then each
usage name in the @nt{aspect_definition} shall resolve to the same entity at the
first freezing point as it does at the end of the immediately enclosing
declaration list.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@ChgAdded{Version=[3],Text=[At most one occurrence of each @nt{aspect_mark} is
allowed within a single @nt{aspect_specification}. The aspect identified by the
@nt{aspect_mark} shall be an aspect that can be specified for the associated
entity (or view of the entity defined by the associated declaration).]}

@begin{Ramification}
@ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0116-1]}
@ChgAdded{Version=[4],Text=[This rule prevents multiple specifications in the same
  @nt{aspect_specification}. Rules in @RefSecNum{Operational and Representation Aspects} prevent multiple specifications in
  different @nt{aspect_specification}s (on different views of the same type,
  for instance) or between operational or representation items and
  an @nt{aspect_specification}, even for aspects that are neither operational
  nor representation aspects.]}

@end{Ramification}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@ChgAdded{Version=[3],Text=[The @nt{aspect_definition} associated with a given
@nt{aspect_mark} may be omitted only when the @nt{aspect_mark} identifies an
aspect of a boolean type, in which case it is equivalent to the
@nt{aspect_definition} being specified as True.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@ChgAdded{Version=[3],Text=[If the @nt{aspect_mark} includes
'Class, then the associated entity shall be a tagged type or a primitive
subprogram of a tagged type.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1],ARef=[AI05-0267-1]}
@ChgAdded{Version=[3],Text=[There are no language-defined aspects that
may be specified on a @nt{renaming_declaration},
a @nt{generic_formal_parameter_declaration}, a @nt{subunit}, a @nt{package_body},
a @nt{task_body}, a @nt{protected_body}, or a @nt{body_stub} other than a
@nt{subprogram_body_stub}.]}
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Implementation-defined aspects can be allowed on
  these, of course; the implementation will need to define the semantics. In
  particular, the implementation will need to define actual type matching
  rules for any aspects allowed on formal types; there are no default matching
  rules defined by the language.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1],ARef=[AI05-0267-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0105-1]}
@ChgAdded{Version=[3],Text=[A language-defined aspect shall not be specified in
an @nt{aspect_specification} given on a
@Chg{Version=[4],New=[],Old=[@nt{subprogram_body} or
@nt{subprogram_body_stub} that is a ]}completion of @Chg{Version=[4],New=[a
subprogram or generic subprogram],Old=[another declaration]}.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Most language-defined aspects (for example,
    preconditions) are intended to be available to callers, and specifying them
    on a body that has a separate declaration hides them from callers. Specific
    language-defined aspects may allow this, but they have to do so explicitly
    (by defining an alternative @LegalityName), and provide any needed rules
    about visibility. Note that this rule does not apply to
    implementation-defined aspects, so implementers need to carefully define
    whether such aspects can be applied to bodies and stubs, and what happens
    if they are specified on both the declaration and body of a unit.]}
@end{Reason}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI05-0183-1],ARef=[AI12-0138-1]}
@ChgAdded{Version=[4],Text=[If an aspect of a derived type is inherited from an
ancestor type and has the boolean value True, the inherited value shall not be
overridden to have the value False for the derived type, unless otherwise
specified in this International Standard.]}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0138-1]}
@ChgAdded{Version=[4],Text=[Certain type-related aspects are defined to be
@i<nonoverridable>; all such aspects are specified using
an @nt{aspect_definition} that is a @nt{name}.@Defn2{Term=[nonoverridable],Sec=[aspect]}]}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0138-1]}
@ChgAdded{Version=[4],Text=[If a nonoverridable aspect is directly specified
for a type @i<T>, then any explicit specification of that aspect for any
other descendant of @i<T> shall be @i<confirming>;@PDefn2{Term=[confirming],Sec=[nonoverridable aspect]}
that is, the specified @nt{name} shall @i<match>@Defn2{Term=[match],Sec=[value of nonoverridable aspect]}
the inherited aspect, meaning that the specified @nt{name} shall denote the
same declarations as would the inherited @nt{name}.]}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0138-1]}
@ChgAdded{Version=[4],Text=[If a full type has a partial view, and a given
nonoverridable aspect is allowed for both the full view and the partial view,
then the given aspect for the partial view and the full view shall be the same:
the aspect shall be directly specified only on the partial view; if the full
type inherits the aspect, then a matching definition shall be specified
(directly or by inheritance) for the partial view.]}

@begin{Ramification}
  @ChgAdded{Version=[4],Text=[In order to enforce these rules without breaking
  privacy, we cannot allow a private type that could have a particular
  overridable aspect to have a hidden definition of that aspect. There is no
  problem if the private type does not allow the aspect (as the
  aspect could not be specified on descendants in that case).]}
@end{Ramification}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0138-1]}
@ChgAdded{Version=[4],Text=[@PDefn{generic contract issue}
In addition to the places where
@LegalityTitle normally apply (see @RefSecNum{Generic Instantiation}),
these rules about nonoverridable aspects also apply in the private part
of an instance of a generic unit.]}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0138-1]}
@ChgAdded{Version=[4],Text=[@Redundant[The Default_Iterator, Iterator_Element,
Implicit_Dereference, Constant_Indexing, and Variable_Indexing aspects
are nonoverridable.]]}

@begin{Discussion}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[We don't need an assume-the-worst rule for most
  nonoverridable aspects as they only work on tagged types and deriving from
  formal tagged types is not allowed in generic bodies. In the case of
  Implicit_Dereference, a derivation in a generic body does not cause problems
  (the ancestor necessarily cannot have the aspect, else specifying the aspect
  would be illegal), as there could be no place with visibility on both aspects.]}
@end{Discussion}

@end{Legality}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[Depending on which aspect is
identified by the @nt{aspect_mark}, an @nt{aspect_definition} specifies:]}
@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[a @nt{name} that denotes a subprogram, object, or
  other kind of entity;]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[an @nt{expression}, which is either evaluated to
  produce a single value, or which (as in a precondition) is to be evaluated at
  particular points during later execution; or]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[an @nt{identifier} specific to the aspect.]}
@end{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The identified aspect of the
associated entity, or in some cases, the view of the entity defined by the
declaration, is as specified by the @nt{aspect_definition} (or by the default of
True when boolean). Whether an @nt{aspect_specification} @i<applies> to an entity or
only to the particular view of the entity defined by the declaration is
determined by the @nt{aspect_mark} and the kind of entity. The following aspects
are view specific:@PDefn2{Term=[applies],Sec=[aspect]}]}

@begin{Itemize}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[An aspect specified on an @nt{object_declaration};]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[An aspect specified on a @nt{subprogram_declaration};]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[An aspect specified on a @nt{renaming_declaration}.]}

@end{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@ChgAdded{Version=[3],Text=[All other @nt{aspect_specification}s are associated
with the entity, and @i<apply> to all views of the entity, unless otherwise
specified in this International Standard.@PDefn2{Term=[applies],Sec=[aspect]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0106-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[If the @nt{aspect_mark} includes
'Class@Chg{Version=[4],New=[ (a @i<class-wide
aspect>)@Defn{class-wide aspect}@Defn2{Term=[aspect],Sec=[class-wide]}],Old=[]},
then@Chg{Version=[4],New=[, unless specified otherwise for a particular
class-wide aspect],Old=[]}:]}
@begin{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[if the associated entity is a tagged type, the
  specification @i<applies> to all descendants of the type;@PDefn2{Term=[applies],Sec=[aspect]}]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[if the associated entity is a primitive subprogram
  of a tagged type @i<T>, the specification @i<applies> to the corresponding primitive
  subprogram of all descendants of @i<T>.@PDefn2{Term=[applies],Sec=[aspect]}]}
@end{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[All specifiable operational and representation
attributes may be specified with an @nt{aspect_specification} instead of an
@nt{attribute_definition_clause} (see
@RefSecNum{Operational and Representation Attributes}).]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The name of the aspect is the same as that of the attribute
  (see @RefSecNum{Operational and Representation Attributes}), so the @nt{aspect_mark}
  is the @nt{attribute_designator} of the attribute.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0154-1]}
@ChgAdded{Version=[3],Text=[Any aspect specified by a representation pragma
or library unit pragma that has a @nt{local_name} as its single argument
may be specified by an @nt{aspect_specification}, with the entity being the
@nt{local_name}. The @nt{aspect_definition} is expected to be of type Boolean.
The expression shall be static.@Chg{Version=[4],New=[ Notwithstanding what
this International Standard says elsewhere, the expression of an aspect that
can be specified by a library unit pragma is resolved and evaluated at the
point where it occurs in the @nt{aspect_specification}@Redundant[, rather
than the first freezing point of the associated
package].@Defn{Notwithstanding}],Old=[]}]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The name of the aspect is the same as that of the
  pragma (see @RefSecNum{Operational and Representation Aspects}), so the
  @nt{aspect_mark} is the name of the pragma.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[In addition,
other operational and representation aspects not associated with specifiable
attributes or representation pragmas may be specified, as specified elsewhere
in this International Standard.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@ChgRef{Version=[4],Kind=[Deleted],ARef=[AI12-0138-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[4],New=[],Old=[If an aspect of a
derived type is inherited from an ancestor type and has the boolean value True,
the inherited value shall not be overridden to have the value False for the
derived type, unless otherwise specified in this International Standard.]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@ChgAdded{Version=[3],Text=[If a @LegalityName or @StaticSemTitle rule only
applies when a particular aspect has been specified, the aspect is considered to
have been specified only when the @nt{aspect_specification} or
@nt{attribute_definition_clause} is visible (see @RefSecNum{Visibility}) at the point
of the application of the rule.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Some rules only apply when an aspect has been
  specified (for instance, an indexable type is one that has aspect
  Variable_Indexing specified). In order to prevent privacy breaking, this can
  only be true when the specification of the aspect is visible. In particular,
  if the Variable_Indexing aspect is specified on the full view of a private
  type, the private type is not considered an indexable type.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@ChgAdded{Version=[3],Text=[Alternative legality and semantics rules may apply
for particular aspects, as specified elsewhere in this International Standard.]}

@end{StaticSem}

@begin{Runtime}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@ChgAdded{Version=[3],Text=[At the freezing point of the associated entity, the
@nt{aspect_specification} is elaborated. The elaboration of the
@nt{aspect_specification} includes the evaluation of the @nt{name} or
@nt{expression}, if any, unless the aspect itself is an expression. If the
corresponding aspect represents an expression (as in a precondition), the
elaboration has no effect; the expression is evaluated later at points
within the execution as specified elsewhere in this International Standard for
the particular aspect.]}
@end{Runtime}

@begin{ImplPerm}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
@ChgAdded{Version=[3],Text=[Implementations may support implementation-defined
aspects. The @nt{aspect_specification} for an implementation-defined aspect may
use an implementation-defined syntax for the @nt{aspect_definition}, and may
follow implementation-defined legality and semantics rules.]}
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The intent is to allow implementations to support
  aspects that are defined, for example, by a @nt{subtype_indication} rather
  than an @nt{expression} or a @nt{name}. We chose not to try to enumerate all
  possible @nt{aspect_definition} syntaxes, but to give implementations maximum
  freedom. Unrecognized aspects are illegal whether or not they use custom
  syntax, so this freedom does not reduce portability.]}
@end{Discussion}
@ChgImplDef{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[Implementation-defined aspects, inluding the syntax for specifying
such aspects and the legality rules for such aspects.]}]}
@end{ImplPerm}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1],ARef=[AI05-0229-1],ARef=[AI05-0267-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Aspect specifications are new.]}
@end{Extend2005}

@begin{Incompatible2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0154-1]}
  @ChgAdded{Version=[4],Text=[@Defn{incompatibilities with Ada 2012}@b<Corrigendum:>
  Added a clarification that aspects that correspond to library unit pragmas
  are resolved and evaluated immediately. This is incompatible, as a
  reference to an entity defined after the aspect will now be illegal.
  However, this would have
  require retroactive enforcement of such aspects, which is a new
  capability not available from the associated pragma, and moreover
  no known Ada 2012 implementation has ever allowed late evaluation of
  such aspects. As such, there should be no practical incompatibility.]}
@end{Incompatible2012}

@begin{Diffword2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI125-0105-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Clarified the wording so that the restriction
  against language-defined aspects on subprogram completions includes
  completions that are expressions functions and null procedures.]}

  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI125-0106-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Defined class-wide aspect
  for use in rules in @RefSecNum{Stream-oriented attributes}.]}

  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI125-0138-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Added a definition of
  nonoverridable aspects. This is necessary to prevent generic contract
  problems with formal derived types.]}
@end{Diffword2012}


@LabeledRevisedClause{Version=[3],New=[Packed Types],Old=[Pragma Pack]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@redundant[@Chg{Version=[3],New=[The Pack aspect having the value True],Old=[A
@nt{pragma} Pack]} specifies that storage minimization should be the main
criterion when selecting the representation of a composite type.]
@end{Intro}

@begin{NotIso}
@ChgAdded{Version=[3],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 2
through 4 were moved to @RefSec{Obsolescent Features}.>}]}@Comment{This message
should be deleted if the paragraphs are ever renumbered.}
@end{NotIso}

@begin{MetaRules}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0001-1]}
  @ChgAdded{Version=[4],Text=[If the default representation already uses
  minimal storage for a particular type, aspect Pack might not cause any
  representation change. It follows that aspect Pack should always be allowed,
  even when it has no effect on representation.]}

  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[As a consequence, the chosen representation for a
  packed type may change during program maintenance even if the type is
  unchanged (in particular, if other representation aspects change on a part of
  the type). This is different than the behavior of most other representation
  aspects, whose properties remain guaranteed no matter what changes are made to
  other aspects.]}

  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[Therefore, aspect Pack should not be used to
  achieve a representation required by external criteria. For instance, setting
  Component_Size to 1 should be preferred over using aspect Pack to ensure an
  array of bits. If future maintenance would make the array components aliased,
  independent, or atomic, the program would become illegal if Component_Size is
  used (immediately identifying a problem) while the aspect Pack version would
  simply change representations (probably causing a hard-to-find bug).]}
@end{MetaRules}

@begin{Syntax}
@begin{SyntaxText}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Type=[Leading],KeepNext=[T],Text=[The form of a
@nt{pragma} Pack is as follows:]}
@end{SyntaxText}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@DeletedPragmaSyn<Version=[3],InitialVersion=[0],@ChgDeleted{Version=[3],
Text=[@key{pragma} @prag(Pack)(@SynI{first_subtype_}@Syn2{local_name});]}>
@end{Syntax}

@begin{Legality}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Text=[The @SynI{first_subtype_}@nt{local_name} of a
@nt{pragma} Pack shall denote a composite subtype.]}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[]}@Comment{A fake to get a conditional Leading}
@Chg{Version=[3],New=[For a full type declaration of a composite type, the
following language-defined representation aspect may be specified:],
Old=[@PDefn2{Term=[representation pragma], Sec=(Pack)}
@PDefn2{Term=[pragma, representation], Sec=(Pack)}
@Chg{Version=[3],New=[@PDefn2{Term=[representation aspect], Sec=(packing)}],
Old=[@PDefn2{Term=[aspect of representation], Sec=(packing)}
@Defn2{Term=[packing], Sec=(aspect of representation)}]}
@Defn{packed}
A @nt{pragma} Pack specifies the @i{packing} aspect of representation;
the type (or the extension part) is said to be @i{packed}.
For a type extension, the parent part is packed as for the parent
type, and a @nt{pragma} Pack causes packing only of the extension part.]}

@begin{Description}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Pack@\The type of aspect Pack is Boolean. When
  aspect Pack is True for a type, the type (or the extension part) is said to be
  @i{packed}. For a type extension, the parent part is packed as for the parent
  type, and specifying Pack causes packing only of the extension part.
@Defn{packed}@AspectDefn{Pack}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Pack],
    Text=[@ChgAdded{Version=[3],Text=[Minimize storage when laying out records
      and arrays.]}]}

@ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],NoPrefix=[T],Text=[If directly specified, the
  @nt{aspect_definition} shall be a static expression. If not specified
  (including by inheritance), the aspect is False.]}
@end{Description}

@begin{Ramification}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
The only high level semantic effect of @Chg{Version=[3],New=[specifying the],Old=[a @nt{pragma}]
Pack@Chg{Version=[3],New=[ aspect],Old=[]}
is @Chg{Version=[3],New=[potential loss of ],Old=[]}independent
addressability (see @RefSec{Shared Variables}).]}
@end{Ramification}
@end{StaticSem}

@begin{ImplAdvice}
If a type is packed, then the implementation should try to minimize
storage allocated to objects of the type,
possibly at the expense of speed of accessing components,
subject to reasonable complexity in addressing calculations.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Storage allocated to objects of a packed type should be minimized.]}]}
@begin{Ramification}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Chg{Version=[3],New=[Specifying the],Old=[A @nt{pragma}]}
Pack@Chg{Version=[3],New=[ aspect],Old=[]} is for gaining space efficiency,
possibly at the expense of time.
If more explicit control over representation is desired,
then a @nt{record_representation_clause},
a Component_Size clause,
or a Size clause should be used instead of,
or in addition to,
@Chg{Version=[3],New=[the],Old=[a @nt{pragma}]}
Pack@Chg{Version=[3],New=[ aspect],Old=[]}.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00291-02]}
@ChgRef{Version=[4],Kind=[DeletedAdded],ARef=[AI12-0001-1]}
@ChgDeleted{Version=[4],Text=[@Chg{Version=[2],New=[If a packed type has a
component that is not of a by-reference type and has no aliased part, then such
a component need not be aligned according to the Alignment of its subtype; in
particular it need not be allocated on a storage element
boundary.@Comment{No "should" here; thus no ImplAdvice entry. This really
qualifies the item above}],Old=[]}]}

@Leading@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Chg{Version=[3],New=[@PDefn2{Term=[recommended level of support], Sec=(aspect Pack)}],
Old=[@PDefn2{Term=[recommended level of support], Sec=(pragma Pack)}]}
The recommended level of support for
@Chg{Version=[3],New=[the],Old=[@nt{pragma}]}
Pack@Chg{Version=[3],New=[ aspect],Old=[]} is:
@begin{Itemize}
  @ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0001-1]}
  @ChgAdded{Version=[4],Text=[Any component of a packed type that is of a
  by-reference type, that is specified as independently addressable, or that
  contains an aliased part, shall be aligned according to the alignment of its
  subtype.]}

  @begin{Ramification}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[This also applies to atomic components. "Atomic"
  implies "specified as independently addressable", so we don't need to mention
  atomic here.]}

  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[Other components do not have to respect the
  alignment of the subtype when packed; in many cases, the Recommended Level of
  Support will require the alignment to be ignored.]}
  @end{Ramification}

  @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0001-1]}
  For a packed record type, the components should be packed as tightly as
  possible subject to @Chg{Version=[4],New=[the above alignment
  requirements, ],Old=[]}the Sizes of the component subtypes, and
  @Chg{Version=[4],New=[],Old=[subject to ]}any
  @nt{record_representation_clause} that applies to the type; the implementation
  may, but need not, reorder components or cross aligned word boundaries to
  improve the packing. A component whose Size is greater than the word size may
  be allocated an integral number of words.

  @begin{Ramification}
    The implementation can always allocate an integral number of words for a
    component that will not fit in a word. The rule also allows small component
    sizes to be rounded up if such rounding does not waste space. For example,
    if Storage_Unit = 8, then a component of size 8 is probably more efficient
    than a component of size 7 plus a 1-bit gap (assuming the gap is needed
    anyway).
  @end{Ramification}

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0009-1]}
  @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0001-1]}
  For a packed array type, if the @Chg{Version=[3],New=[], Old=[component
  subtype's ]}Size@Chg{Version=[3],New=[ of the component subtype],Old=[]} is
  less than or equal to the word size@Chg{Version=[3],New=[],Old=[, and
  Component_Size is not specified for the type]}, Component_Size should be less
  than or equal to the Size of the component subtype, rounded up to the nearest
  factor of the word size@Chg{Version=[4],New=[, unless this would violate
  the above alignment requirements],Old=[]}.

  @begin{Ramification}
    @ChgRef{Version=[4],Kind=[Deleted],ARef=[AI12-0001-1]}
    @ChgDeleted{Version=[4],Text=[If a component subtype is aliased,
    its Size will generally be a multiple of Storage_Unit,
    so it probably won't get packed very tightly.]}
  @end{Ramification}
@end{Itemize}
@ChgImplAdvice{Version=[3],Kind=[Revised],InitialVersion=[2],
Text=[@ChgAdded{Version=[2],
Text=[The recommended level of support for @Chg{Version=[3],New=[the],Old=[pragma]}
Pack@Chg{Version=[3],New=[ aspect],Old=[]} should be
followed.]}]}
@end{ImplAdvice}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00291-02]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[2],Text=[Added clarification that
  @Chg{Version=[3],New=[the],Old=[pragma]} Pack@Chg{Version=[3],New=[ aspect],Old=[]} can
  ignore alignment requirements on types that don't have by-reference or
  aliased parts. This was always intended, but there was no wording to that
  effect.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Aspect Pack is new; @nt{pragma} Pack is now obsolescent.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0009-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Fixed so that the
  presence or absence of a confirming Component_Size representation
  clause does not change the meaning of the Pack aspect.]}
@end{DiffWord2005}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0001-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Fixed so that the
  Recommended Level of Support does not require packing of
  components for which such packing would violate other representation
  items or aspects. This is not incompatible, as either such Pack
  aspects were treated as illegal or the Recommended Level of Support
  was ignored as impractical, neither of which would change the
  behavior of any working programs. (Other behavior cannot be justifed
  from the Standard.)]}
@end{DiffWord2012}



@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledRevisedClause{Version=[1],New=[Operational and Representation Attributes], Old=[Representation Attributes]}

@begin{Intro}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@redundant[@Defn{representation attribute}
@Defn2{Term=[attribute], Sec=(representation)}
The values of certain implementation-dependent characteristics can be
obtained by interrogating appropriate
@Chg{New=[operational or ],Old=[]}representation attributes.
@RootDefn2{Term=[attribute], Sec=(specifying)}
Some of these attributes are specifiable via an
@nt{attribute_definition_clause}.]
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
@PDefn2{Term=[expected profile],
  Sec=(attribute_definition_clause name)}
For an @nt{attribute_definition_clause} that specifies
an attribute that denotes a subprogram,
the expected profile for the @nt{name}
is the profile required for the attribute.
For an @nt{attribute_definition_clause} that specifies
an attribute that denotes some other kind of entity,
the @nt{name} shall resolve to denote an entity of the appropriate
kind.
@begin{Ramification}
For example, the Size attribute is of type @i{universal_integer}.
Therefore, the expected type for Y in @lquotes@;@key[for] X'Size @key[use] Y;@rquotes@; is
@i{universal_integer},
which means that Y can be of any integer type.
@end{Ramification}
@begin{Discussion}
For attributes that denote subprograms, the required profile is indicated
separately for the individual attributes.
@end{Discussion}
@begin{Ramification}
@Leading@;For an @nt{attribute_definition_clause} with a @nt{name},
the @nt{name} need not statically denote the entity it denotes.
For example, the following kinds of things are allowed:
@begin{Example}
@key[for] Some_Access_Type'Storage_Pool @key[use] Storage_Pool_Array(I);
@key[for] Some_Type'Read @key[use] Subprogram_Pointer.@key[all];
@end{Example}
@end{Ramification}
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@RootDefn{specifiable (of an attribute and for an entity)}
@RootDefn2{Term=[attribute], Sec=(specifiable)}
An @nt{attribute_designator} is allowed in an
@nt{attribute_definition_clause} only if this International Standard
explicitly allows it,
or for an implementation-defined attribute
if the implementation allows it.
@Chg{Version=[3],New=[@PDefn2{Term=[representation aspect], Sec=(specifiable attributes)}],
Old=[@PDefn2{Term=[aspect of representation], Sec=(specifiable attributes)}]}
Each specifiable attribute constitutes an
@Chg{New=[@PDefn2{Term=[operational aspect], Sec=(specifiable attributes)}
operational aspect or ],Old=[]}aspect of representation@Chg{Version=[3],New=[;
the name of the aspect is that of the attribute],Old=[]}.
@begin{Discussion}
For each specifiable attribute,
we generally say something like,
@lquotes@;The ... attribute may be specified for ... via
an @nt{attribute_definition_clause}.@rquotes@;

The above wording allows for
T'Class'Alignment, T'Class'Size, T'Class'Input, and T'Class'Output
to be specifiable.

A specifiable attribute is not necessarily
specifiable for all entities for which it is defined.
For example, one is allowed to ask T'Component_Size for an array
subtype T, but @lquotes@;@key[for] T'Component_Size @key[use] ...@rquotes@;
is only allowed if T is a first subtype,
because Component_Size is a type-related aspect.
@end{Discussion}

For an @nt{attribute_definition_clause} that specifies
an attribute that denotes a subprogram,
the profile shall be mode conformant with the one
required for the attribute,
and the convention shall be Ada.
Additional requirements are defined for particular attributes.
@Defn2{Term=[mode conformance],Sec=(required)}
@begin{Ramification}
@Leading@;This implies, for example, that if one writes:
@begin{Example}
@key[for] T'Read @key[use] R;
@end{Example}

R has to be a procedure with two parameters with the appropriate
subtypes and modes as shown in
@RefSecNum{Stream-Oriented Attributes}.
@end{Ramification}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00270-01]}
@Defn{Address clause}
@Defn{Alignment clause}
@Defn{Size clause}
@Defn{Component_Size clause}
@Defn{External_Tag clause}
@Defn{Small clause}
@Defn{Bit_Order clause}
@Defn{Storage_Pool clause}
@Defn{Storage_Size clause}@Chg{Version=[2],New=[
@Defn{Stream_Size clause}],Old=[]}
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
The @lquotes@;given the implementation's run-time model@rquotes@; part is
intended to imply that, for example, on an 80386 running MS-DOS,
the word might be 16 bits, even though the hardware can support 32
bits.

A word is what ACID refers to as a @lquotes@;natural hardware
boundary@rquotes@;.

Storage elements may, but need not be, independently addressable
(see @RefSec{Shared Variables}).
Words are expected to be independently addressable.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00133-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0092-1]}
@ChgAdded{Version=[2],Text=[@Defn{machine scalar}
A @i{machine scalar} is an amount of storage that can be conveniently and
efficiently loaded, stored, or operated upon by the hardware. Machine scalars
consist of an integral number of storage elements. The set of machine scalars
is implementation defined, but @Chg{Version=[3],New=[includes],Old=[must include]}
at least the storage element and
the word. Machine scalars are used to interpret @nt{component_clause}s when the
nondefault bit ordering applies.]}
@ChgImplDef{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[The set of machine scalars.]}]}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0092-1]}
  @ChgAdded{Version=[3],Text=[A single storage element is a machine scalar
  in all Ada implementations. Similarly, a word is a machine scalar in
  all implementations (although it might be the same as a storage element).
  An implementation may define other machine scalars that make sense on the
  target (a half-word, for instance).]}
@end{Ramification}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0191-1]}
@Chg{New=[The following representation attributes are defined: Address,
Alignment, Size, Storage_Size,
@Chg{Version=[3],New=[],Old=[and ]}Component_Size@Chg{Version=[3],New=[,
Has_Same_Storage, and Overlaps_Storage],Old=[]}.],
Old=[@Leading@;The following attributes are defined:]}

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{To be consistent with 8652/0006}
@Leading@;For @ChgPrefixType{Version=[1],Kind=[Revised],Text=[a
@Chg{New=[@nt{prefix}],Old=[prefix]} X that
denotes an object, program unit, or label]}:
@begin{Description}
@Attribute{Prefix=<X>, AttrName=<Address>,
  Text=<Denotes the address of the first of the storage elements
allocated to X. For a program unit or
label, this value refers to the machine code associated with
the corresponding body or @nt{statement}.
The value of this attribute is of type System.Address.>}

@begin{Ramification}
  Here, the @lquotes@;first of the storage elements@rquotes@; is intended
  to mean the one with the lowest address;
  the endianness of the machine doesn't matter.
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0095-1]}
@ChgAdded{Version=[3],NoPrefix=[T],Text=[The prefix of X'Address shall not statically denote
a subprogram that has convention Intrinsic. X'Address raises Program_Error if X
denotes a subprogram that has convention Intrinsic.]}

@NoPrefix@PDefn2{Term=[specifiable], Sec=(of Address for stand-alone
objects and for program units)}
@Defn{Address clause}
@ChgNote{Removed Redundant here, as per AI-00114. Did not mark change, as it is
AARM-only, not to the text of the item.}Address may be specified for
stand-alone objects and for program units via an
@nt{attribute_definition_clause}.@Chg{Version=[3],New=[@AspectDefn{Address}],Old=[]}
  @begin{Ramification}
  Address is not allowed for enumeration literals,
  predefined operators, derived task types,
  or derived protected types, since they are not program units.

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Address is not allowed for intrinsic subprograms,
  either. That can be checked statically unless the prefix is a generic formal
  subprogram and the attribute reference is in the body of a generic unit.
  We define that case to raise Program_Error, in order that the compiler
  does not have to build a wrapper for intrinsic subprograms.]}

  The validity of a given address depends on the run-time model;
  thus, in order to use Address clauses correctly,
  one needs intimate knowledge of the run-time model.

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
  If the Address of an object is specified,
  any explicit or implicit initialization takes place as usual,
  unless @Chg{Version=[3],New=[the],Old=[a @nt{pragma}]} Import
  @Chg{Version=[3],New=[aspect ],Old=[]}is also specified for the object
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
  it would violate the @lquotes@;legality determinable via semantic
  dependences@rquotes@; @MetaRulesName.

  AI83-00305 @em If X is a task type,
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

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Address],
    Text=[@ChgAdded{Version=[3],Text=[Machine address of an entity.]}]}

@end{Description}
@EndPrefixType{}
@end{StaticSem}

@begin{Erron}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0009-1]}
@PDefn2{Term=(erroneous execution),Sec=(cause)}If an Address is specified,
it is the programmer's responsibility to ensure that
the address is valid@Chg{Version=[3],New=[ and appropriate for the entity and
its use],Old=[]}; otherwise, program execution is erroneous.

@begin{Discussion}
@ChgAdded{Version=[3],Text=[@ldquote@;Appropriate for the entity and its
use@rdquote covers cases such as
misaligned addresses, read-only code addresses for variable data objects (and
nonexecutable data addresses for code units), and addresses which would
force objects that are supposed to be independently addressable to not be.
Such addresses may be @ldquote@;valid@rdquote as they designate locations that
are accessible to the program, but the program execution is still erroneous
(meaning that implementations do not have to worry about these cases).]}
@end{Discussion}
@end{Erron}

@begin{ImplAdvice}
For an array X, X'Address should point at the first component of the
array, and not at the array bounds.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[For an array X, X'Address should point at the first component of the
array rather than the array bounds.]}]}
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
@Leading@;The recommended level of support for the Address attribute is:
@begin{Itemize}
X'Address should produce a useful result if X is an
object that is aliased or of a by-reference
type, or is an entity whose Address has been specified.
@begin{Reason}
  Aliased objects are the ones for which the
  Unchecked_Access attribute is allowed;
  hence, these have to be allocated on an addressable
  boundary anyway. Similar considerations apply to objects
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

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00291-02]}
@ChgDeleted{Version=[2],Text=[Objects (including subcomponents) that are
aliased or of a by-reference type
should be allocated on storage element boundaries.]}
@Comment{There is a now a blanket permission to this effect}
@begin{Reason}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[This is necessary for the Address attribute to be
useful (since First_Bit and Last_Bit apply only to components). Implementations
generally need to do this anyway, for tasking to work properly.]}
@end{Reason}

If the Address of an object is specified,
or it is imported or exported,
then the implementation should not perform optimizations based on
assumptions of no aliases.
@end{Itemize}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The recommended level of support for the Address attribute should be
followed.]}]}
@end{ImplAdvice}

@begin{Notes}
The specification of a link name @Chg{Version=[3],New=[with the Link_Name
aspect],Old=[in a @nt{pragma} Export]}
(see @RefSecNum{Interfacing Aspects})
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

The @ntf{address_clause} has been renamed to @nt{at_clause} and moved
to @RefSec{Obsolescent Features}.
One can use an Address clause
(@lquotes@;for T'Address @key[use] ...;@rquotes@;)
instead.

The attributes defined in RM83-13.7.3 are moved to
@RefSecNum{Numerics},
@RefSecNum{Attributes of Floating Point Types}, and
@RefSecNum{Attributes of Fixed Point Types}.
@end{DiffWord83}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0183-1]}
  @ChgAdded{Version=[3],Text=[Defined that the names of aspects are the same as
  the name of the attribute; that gives a name to use in @nt{aspect_specification}s
  (see @RefSecNum{Aspect Specifications}).]}
@end{DiffWord2005}

@begin{MetaRules}
By default, the Alignment of a subtype should
reflect the @lquotes@;natural@rquotes@; alignment for objects of the
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
@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{To be consistent with 8652/0006}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00291-02]}
@Leading@;For @ChgPrefixType{Version=[1],Kind=[Revised],Text=[a
@Chg{New=[@nt{prefix}],Old=[prefix]} X that denotes @Chg{Version=[2],New=[an],
Old=[a subtype or]} object]}:
@begin{Description}
@ChgAttribute{Version=[2], Kind=[Revised], ChginAnnex=[T], Leading=[F],
  Prefix=<X>, AttrName=<Alignment>, ARef=[AI95-00291-02], InitialVersion=[0],
  Text=[@Chg{Version=[2],New=[The value of this attribute is of type
    @i{universal_integer}, and nonnegative; zero means that the object is not
    necessarily aligned on a storage element boundary. If X'Alignment is not
    zero, then X is aligned on a storage unit boundary and X'Address],
    Old=[The Address of an object that is allocated under
    control of the implementation]} is an integral multiple of
    @Chg{Version=[2],New=[X'Alignment],Old=[the Alignment of the object]}
    (that is, the Address modulo the Alignment is zero).@Chg{Version=[2],
    New=[],Old=[The offset of a record component is a multiple of the
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
    The implementation shall not assume a stricter alignment.]}

@Comment{Deleted below causes trouble in the generated text for attributes,
but no fix appears to be possible. And the trouble is much worse in the
RTF version than the HTML version, so for now we're making a hand fix.}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00291-02]}
@ChgDeleted{Version=[2],NoPrefix=[T],Text=[The value of this attribute
    is of type @i{universal_integer}, and nonnegative;
    zero means that the object is not necessarily
    aligned on a storage element boundary.]}]}@Comment{End X'Alignment}
@EndPrefixType{}
@begin{Ramification}
The Alignment is passed by an @nt{allocator} to the Allocate operation;
the implementation has to choose a value such that if the address
returned by Allocate is aligned as requested,
the generated code can correctly access the object.

The above mention of @lquotes@;modulo@rquotes@; is referring to the "@key[mod]"
operator declared in System.Storage_Elements;
if X @key[mod] N = 0, then X is by definition aligned on an
N-storage-element boundary.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00291-02]}
@NoPrefix@Chg{Version=[2],New=[@PDefn2{Term=[specifiable], Sec=(of Alignment for objects)}],
Old=[@PDefn2{Term=[specifiable], Sec=(of Alignment for first subtypes and objects)}]}
@Defn{Alignment clause}
Alignment may be specified for@Chg{Version=[2],New=[],Old=[ first subtypes and]}
@Redundant[stand-alone] objects via an @nt{attribute_@!definition_@!clause};
the expression of such a clause shall be static, and its value
nonnegative.@Chg{Version=[2],New=[],Old=[If the Alignment of a subtype is
specified, then the Alignment of an object of the subtype is at least as
strict, unless the object's Alignment is also specified.
The Alignment of an object created by an allocator is that of the
designated subtype.@Chg{Version=[3],New=[@AspectDefn{Alignment (object)}],Old=[]}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Alignment (object)],
    Text=[@ChgAdded{Version=[3],Text=[Alignment of an object.]}]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00247-01]}
@ChgDeleted{Version=[2],NoPrefix=[T],Text=[If an Alignment is specified
for a composite subtype or object, this
Alignment shall be equal to the least common multiple of any
specified Alignments of the subcomponent subtypes, or an integer
multiple thereof.]}
@end{Description}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00291-02]}
@ChgAdded{Version=[2],Type=[Leading],KeepNext=[T],
Text=[For @PrefixType{every subtype S}:]}
@begin{Description}
@ChgAttribute{Version=[2], Kind=[Added], ChginAnnex=[T], Leading=[F],
  Prefix=<S>, AttrName=<Alignment>, ARef=[AI95-00291-02], InitialVersion=[2],
  Text=[@Chg{Version=[2],New=[The value of this attribute is of type
  @i{universal_integer}, and nonnegative.],Old=[]}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00051-02],ARef=[AI95-00291-02]}
  @ChgAdded{Version=[2],NoPrefix=[T], Text=[For an object X of subtype S,
  if S'Alignment is not zero, then X'Alignment is a nonzero integral multiple
  of S'Alignment unless specified otherwise by a representation
  item.]}]}@Comment{End S'Alignment}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00291-02]}
  @ChgAdded{Version=[2],NoPrefix=[T], Text=[@PDefn2{Term=[specifiable], Sec=(of Alignment for first subtypes)}
  @Defn{Alignment clause}
  Alignment may be specified for first subtypes via an
  @nt{attribute_@!definition_@!clause};
  the expression of such a clause shall be static, and its value
  nonnegative.@Chg{Version=[3],New=[@AspectDefn{Alignment (subtype)}],Old=[]}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Alignment (subtype)],
    Text=[@ChgAdded{Version=[3],Text=[Alignment of a subtype.]}]}

@end{Description}

@end{StaticSem}

@begin{Erron}
@PDefn2{Term=(erroneous execution),Sec=(cause)}
Program execution is erroneous if an Address clause is given that
conflicts with the Alignment.
@begin{Ramification}
The user has to either give an Alignment clause also,
or else know what Alignment the implementation will choose by default.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00051-02],ARef=[AI95-00291-02]}
@PDefn2{Term=(erroneous execution),Sec=(cause)}
@Chg{Version=[2],New=[For],Old=[If the Alignment is specified for]} an
object that is not allocated under control of the implementation,
execution is erroneous if the object is not aligned according to
@Chg{Version=[2],New=[its],Old=[the]} Alignment.

@end{Erron}

@begin{ImplAdvice}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0116-1]}
@ChgAdded{Version=[3],Text=[For any tagged specific subtype @i<S>,
@i<S>'Class'Alignment should equal @i<S>'Alignment.]}

@begin{Reason}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[A tagged object should never be less aligned than
the alignment of the type of its view, so for a class-wide type T'Class, the
alignment should be no greater than that of any type covered by T'Class. If the
implementation only supports alignments that are required by the recommended
level of support (and this is most likely), then the alignment of any covered
type has to be the same or greater than that of T @em which leaves the only
reasonable value of T'Class'Alignment being T'Alignment. Thus we recommend this,
but don't require it so that in the unlikely case that the implementation does
support smaller alignments for covered types, it can select a smaller value
for T'Class'Alignment.]}
@end{Reason}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[For any tagged specific subtype @i<S>,
@i<S>'Class'Alignment should equal @i<S>'Alignment.]}]}

@PDefn2{Term=[recommended level of support], Sec=(Alignment attribute
for subtypes)}
@Leading@;The recommended level of support for the Alignment attribute for
subtypes is:
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00051-02]}
An implementation should support @Chg{Version=[2],New=[an Alignment clause
for a discrete type, fixed point type, record type, or
array type, specifying an Alignment value that is zero or
a power of two],Old=[specified Alignments that are factors and multiples of
the number of storage elements per word]}, subject to the following:

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00051-02]}
An implementation need not support
@Chg{Version=[2], New=[an Alignment clause for a signed
integer type specifying an Alignment greater than the largest
Alignment value that is ever chosen by default by the implementation
for any signed integer type. A corresponding limitation may be
imposed for modular integer types, fixed point types, enumeration types,
record types, and array types],Old=[specified Alignments for combinations
of Sizes and Alignments that cannot be easily loaded and
stored by available machine instructions]}.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00051-02]}
An implementation need not support
@Chg{Version=[2],New=[a nonconfirming Alignment clause which could enable the
creation of an object of an elementary type which cannot be easily loaded and
stored by available machine instructions.],
Old=[specified Alignments that are greater than the maximum
Alignment the implementation ever returns
by default.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00291-02]}
@ChgAdded{Version=[2],Text=[An implementation need not support an
Alignment specified for a derived tagged type which is not a multiple of the
Alignment of the parent type. An implementation need not support a
nonconfirming Alignment specified for a derived untagged by-reference type.]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00291-02]}
  @ChgAdded{Version=[2],Text=[There is no recommendation to support any
  nonconfirming Alignment clauses for types not mentioned above.
  Remember that
  @RefSecNum{Operational and Representation Aspects} requires support for
  confirming Alignment clauses for all types.]}
@end{Ramification}
@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0116-1]}
  @ChgAdded{Version=[3],Text=[An implementation that tries to support other
  alignments for derived tagged types will need to allow inherited subprograms
  to be passed objects that are less aligned than expected by the parent
  subprogram and type. This is unlikely to work if alignment has any effect on
  code selection. Similar issues arise for untagged derived types whose
  parameters are passed by reference.]}
@end{ImplNote}
@end{Itemize}

@Leading@PDefn2{Term=[recommended level of support], Sec=(Alignment attribute
for objects)}
The recommended level of support for the Alignment attribute for
objects is:
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00291-02]}
@ChgDeleted{Version=[2],Text=[Same as above, for subtypes, but in addition:]}

For stand-alone library-level objects of statically constrained
subtypes, the implementation should support all Alignments
supported by the target linker. For example, page alignment
is likely to be supported for such objects, but not for subtypes.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00291-02]}
@ChgAdded{Version=[2],Text=[For other objects, an implementation should at
least support the alignments supported for their
subtype, subject to the following:]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00291-02]}
@ChgAdded{Version=[2],Text=[An implementation need not support Alignments
specified for objects of a by-reference type or for objects of types containing
aliased subcomponents if the specified Alignment is not a multiple of the
Alignment of the subtype of the object.]}
@end{Itemize}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The recommended level of support for the Alignment attribute should be
followed.]}]}
@end{ImplAdvice}

@begin{Notes}
Alignment is a subtype-specific attribute.

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00247-01]}
@ChgDeleted{Version=[2],Text=[The Alignment of a composite object is always
equal to the least common multiple of the Alignments of its components, or a
multiple thereof.]}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[For default Alignments, this follows from the
semantics of Alignment. For specified Alignments, it follows from a
@LegalityName stated above.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1],ARef=[AI05-0269-1]}
A @nt{component_clause}, Component_Size clause, or
@Chg{Version=[3],New=[specifying the],Old=[a @nt{pragma}]} Pack
@Chg{Version=[3],New=[aspect as True ],Old=[]}can override a specified
Alignment.
@begin{Discussion}
Most objects are allocated by the implementation; for these, the
implementation obeys the Alignment. The implementation is of course
allowed to make an object @i{more} aligned than its Alignment requires
@em an object whose Alignment is 4 might just happen to land at an
address that's a multiple of 4096.
For formal parameters, the implementation might
want to force an Alignment stricter than the parameter's subtype.
For example, on some systems, it is customary to always align
parameters to 4 storage elements.

Hence, one might initially assume that the implementation could
evilly make all Alignments 1 by default, even though integers, say,
are normally aligned on a 4-storage-element boundary. However, the
implementation cannot get away with that @em if the Alignment is 1,
the generated code cannot assume an Alignment of 4, at least not for
objects allocated outside the control of the implementation.

Of course implementations can assume anything they can prove, but
typically an implementation will be unable to prove much about the
alignment of, say, an imported object. Furthermore, the information
about where an address @lquotes@;came from@rquotes@; can be lost to the compiler due to
separate compilation.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
The Alignment of an object that is a component of a packed
composite object will usually be 0, to indicate that the component is
not necessarily aligned on a storage element boundary.
For a subtype, an Alignment of 0 means that objects of the subtype are
not normally aligned on a storage element boundary at all.
For example, an implementation might choose to make Component_Size be
@Chg{Version=[2],New=[1],Old=[0]}
for an array of Booleans, even
when @Chg{Version=[3],New=[the],Old=[@nt{pragma}]} Pack
@Chg{Version=[3],New=[aspect ],Old=[]}has not been
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
certainly not static). We do not require a run-time check, since
efficiency seems paramount here, and Address clauses are treading on
thin ice anyway. Hence, this misuse of Address clauses is just like any
other misuse of Address clauses @em it's erroneous.

A type extension can have a stricter Alignment than its parent.
This can happen, for example, if the Alignment of the parent is 4,
but the extension contains a component with Alignment 8.
The Alignment of a class-wide type or object will have to be the
maximum possible Alignment of any extension.

The recommended level of support for the Alignment attribute is
intended to reflect a minimum useful set of capabilities. An
implementation can assume that all Alignments are multiples of each
other @em 1, 2, 4, and 8 might be the only supported Alignments for
subtypes. An Alignment of 3 or 6 is unlikely to be useful.
For objects that can be allocated statically, we recommend that
the implementation support larger alignments, such as 4096. We do
not recommend such large alignments for subtypes, because the maximum
subtype alignment will also have to be used as the alignment of stack
frames, heap objects, and class-wide objects. Similarly, we do not
recommend such large alignments for stack-allocated objects.

If the maximum default Alignment is 8
(say, Long_Float'Alignment = 8),
then the implementation can refuse to accept stricter alignments
for subtypes. This simplifies the generated code, since the compiler
can align the stack and class-wide types to this maximum without a
substantial waste of space (or time).

Note that the recommended level of support takes into account
interactions between Size and Alignment. For example, on a 32-bit
machine with 8-bit storage elements, where load and store
instructions have to be aligned according to the size of the thing
being loaded or stored, the implementation might accept an Alignment
of 1 if the Size is 8, but might reject an Alignment of 1 if the Size
is 32. On a machine where unaligned loads and stores are merely
inefficient (as opposed to causing hardware traps),
we would expect an Alignment of 1 to be supported for any Size.
@end{Discussion}
@end{Notes}

@begin{DiffWord83}
The nonnegative part is missing from RM83
(for @nt{mod_clause}s, nee @ntf{alignment_clause}s,
which are an obsolete version of Alignment clauses).
@end{DiffWord83}

@begin{StaticSem}
@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{To be consistent with 8652/0006}
@Leading@;For @ChgPrefixType{Version=[1],Kind=[Revised],Text=[a
@Chg{New=[@nt{prefix}],Old=[prefix]} X that denotes an object]}:
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

@NoPrefix@PDefn2{Term=[specifiable], Sec=(of Size for stand-alone objects)}
@Defn{Size clause}
Size may be specified for @Redundant[stand-alone] objects
via an @nt{attribute_definition_clause};
the expression of such a clause shall be static and its
value nonnegative.@Chg{Version=[3],New=[@AspectDefn{Size (object)}],Old=[]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Size (object)],
    Text=[@ChgAdded{Version=[3],Text=[Size in bits of an object.]}]}

@end{Description}
@end{StaticSem}

@begin{ImplAdvice}
@ChgNote{Moved from 13.9}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00051-02]}
@ChgAdded{Version=[2],Text=[The size of an array object should not include
its bounds.]}
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[The Size of an array object should not include its bounds.]}]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00051-02],ARef=[AI95-00291-02]}
@ChgDeleted{Version=[2],Type=[Leading],Text=[]}@Comment{A fake to get a conditional Leading}
@PDefn2{Term=[recommended level of support], Sec=(Size attribute)}
The recommended level of support for the Size attribute
of objects is@Chg{Version=[2],New=[ the same as for subtypes (see below),
except that only a confirming Size clause need be supported for an aliased
elementary object.],Old=[:]}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00051-02]}
@ChgDeleted{Version=[2],Text=[A Size clause should be supported for an object
if the specified Size is at least as large as its subtype's Size, and
corresponds to a size in storage elements that is a multiple of the object's
Alignment (if the Alignment is nonzero).]}
@Comment{No ImplDef summary here; there is no reason to separately mention it}
@end{Itemize}
@end{ImplAdvice}

@begin{StaticSem}
@Leading@Keepnext@;For @PrefixType{every subtype S}:
@begin{Description}
@AttributeLeading{Prefix=<S>, AttrName=<Size>,
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
via an @nt{attribute_@!definition_@!clause};
the expression of such a clause shall be static
and its value nonnegative.@Chg{Version=[3],New=[@AspectDefn{Size (subtype)}],Old=[]}
@ImplDef{The meaning of Size for indefinite subtypes.}
  @begin{Reason}
  @Leading@;The effects of specifying the Size of a subtype are:
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

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
  A @nt{component_clause} or a Component_Size clause can cause an object
  to be smaller than its subtype's specified size.
  @Chg{Version=[3],New=[The aspect],Old=[A @nt{pragma}]}
  Pack cannot; if a component subtype's size is specified,
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
  @Leading@;A Size clause for a numeric subtype need not
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

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Size (subtype)],
    Text=[@ChgAdded{Version=[3],Text=[Size in bits of a subtype.]}]}

@end{Description}
@EndPrefixType{}
@end{StaticSem}

@begin{ImplReq}
In an implementation, Boolean'Size shall be 1.
@end{ImplReq}

@begin{ImplAdvice}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00051-02]}
@Leading@;If the Size of a subtype @Chg{Version=[2],New=[],Old=[is specified,
and ]}allows for efficient independent addressability
(see @RefSecNum{Shared Variables}) on the target architecture,
then the Size of the following objects of the subtype should equal the
Size of the subtype:
@begin{Itemize}
Aliased objects (including components).

Unaliased components, unless the Size of the
component is determined by a @nt{component_clause} or Component_Size
clause.
@end{Itemize}
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[If the Size of a subtype allows for efficient independent
addressability, then the Size of most objects of the subtype should
equal the Size of the subtype.]}]}
@begin{Ramification}
Thus, on a typical 32-bit machine,
@lquotes@;@key[for] S'Size @key[use] 32;@rquotes@;
will guarantee that aliased objects of subtype S,
and components whose subtype is S, will have Size
= 32 (assuming the implementation chooses to obey this @ImplAdviceTitle).
On the other hand, if one writes,
@lquotes@;@key[for] S2'Size @key[use] 5;@rquotes@;
then stand-alone objects of subtype S2 will typically have their Size
rounded up to ensure independent addressability.

Note that @lquotes@;@key[for] S'Size @key[use] 32;@rquotes@;
does not cause things like formal parameters to have Size = 32 @em
the implementation is allowed to make all parameters be at least 64
bits, for example.

Note that
@lquotes@;@key[for] S2'Size @key[use] 5;@rquotes@;
requires record components whose subtype is S2 to be exactly 5 bits
if the record type is packed.
The same is not true of array components;
their Size may be rounded up to the nearest factor of the word size.
@end{Ramification}
@begin{ImplNote}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00291-02]}
@Defn{gaps}
On most machines, arrays don't contain gaps between @Chg{Version=[2],
New=[elementary ],Old=[]}components;
if the Component_Size is greater than the Size of the component subtype,
the extra bits are generally considered part of each component,
rather than gaps between components.
On the other hand,
a record might contain gaps between @Chg{Version=[2],
New=[elementary ],Old=[]}components,
depending on what sorts of loads, stores, and masking operations
are generally done by the generated code.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00291-02]}
For an array,
any extra bits stored for each @Chg{Version=[2],
New=[elementary ],Old=[]}component will generally be part
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
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[A Size clause on a composite subtype should not affect
the internal layout of components.]}]}
@begin{Reason}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
That's what Pack @Chg{Version=[3],New=[aspects],Old=[@nt{pragma}s]},
@nt{record_representation_clause}s,
and Component_Size clauses are for.
@end{Reason}

@Leading@PDefn2{Term=[recommended level of support], Sec=(Size attribute)}
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
  the lower bound @lquotes@;@en@;2@+{@i{n@en@;1}}@rquotes@; is replaced by @lquotes@;@en@;2@+{@i{n@en@;1}}+1@rquotes@;.


  If an integer subtype (whether signed or unsigned)
  contains no negative values, the Size should not include space
  for a sign bit.


  Typically, the implementation will choose to make the Size of a
  subtype be exactly the smallest such @i{n}.
  However, it might, for example, choose a biased representation,
  in which case it could choose a smaller value.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
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
  unless @Chg{Version=[3],New=[Pack],Old=[packing]},
  Component_Size, or record layout is specified.

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

  @Leading@;The fact that the size of an object is not necessarily the same
  as its subtype can be confusing:
  @begin{Example}
@key[type] Device_Register @key[is] @key[range] 0..2**8 - 1;
@key[for] Device_Register'Size @key[use] 8; --@RI{ Confusing!}
My_Device : Device_Register;
@key[for] My_Device'Address @key[use] To_Address(16#FF00#);
  @end{Example}

  The programmer might think that My_Device'Size is 8,
  and that My_Device'Address points at an 8-bit location.
  However, this is not true.
  In Ada 83 (and in Ada 95), My_Device'Size might well be 32,
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
  @Leading@;The correct way to write the above code is like this:
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
  objects. Therefore, My_Device'Size will be 8,
  and My_Device'Address will point at those 8 bits,
  as desired.

  If an object of subtype Device_Register is passed to a foreign
  language subprogram, it will be passed according to that subprogram's
  conventions. Most foreign language implementations have similar
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

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00051-02]}
@ChgAdded{Version=[2],Type=[Leading],Text=[An implementation should support a
Size clause for a discrete type, fixed point type, record type, or array type,
subject to the following:]}
@begin{InnerItemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00051-02]}
@ChgAdded{Version=[2],Text=[An implementation need not support a Size clause
for a signed integer type specifying a Size greater than that of the largest
signed integer type supported by the implementation in the absence of a size
clause (that is, when the size is chosen by default). A corresponding
limitation may be imposed for modular integer types, fixed point types,
enumeration types, record types, and array types.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00051-02]}
  @ChgAdded{Version=[2],Text=[Note that the @lquotes@;corresponding
  limitation@rquotes for a record or array type implies that an implementation
  may impose some reasonable maximum size for records and arrays (e.g. 2**32
  bits), which is an upper bound (@lquotes@;capacity@rquotes limit) on the
  size, whether chosen by default or by being specified by the user. The
  largest size supported for records need not be the same as the largest
  size supported for arrays.]}
@end{Discussion}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0155-1]}
  @ChgAdded{Version=[3],Text=[Only Size clauses with a size greater than or
  equal to the Size that would be chosen by default may be safely presumed to be
  supported on nonstatic elementary subtypes. Implementations may choose to
  support smaller sizes, but only if the Size allows any value of the subtype to
  be represented, for any possible value of the bounds.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00291-02]}
@ChgAdded{Version=[2],Text=[A nonconfirming size clause for the first subtype
of a derived untagged by-reference type need not be supported.]}
@end{InnerItemize}
@end{Itemize}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The recommended level of support for the Size attribute should be
followed.]}]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00291-02]}
  @ChgAdded{Version=[2],Text=[There is no recommendation to support any
  nonconfirming Size clauses for types not mentioned above.
  Remember that
  @RefSecNum{Operational and Representation Aspects} requires support for
  confirming Size clauses for all types.]}
@end{Ramification}
@end{ImplAdvice}

@begin{Notes}
Size is a subtype-specific attribute.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
A @nt{component_clause} or Component_Size clause
can override a specified Size.
@Chg{Version=[3],New=[Aspect],Old=[A @nt{pragma}]} Pack cannot.
@end{Notes}

@begin{Inconsistent83}
  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00114-01]}
  @ChgAdded{Version=[2],Text=[@Defn{inconsistencies with Ada 83}We specify
  the meaning of Size in much more
  detail than Ada 83. This is not technically an inconsistency, but it is in
  practice, as most Ada 83 compilers use a different definition for Size than
  is required here. This should have been documented more explicitly during
  the Ada 9X process.]}
@end{Inconsistent83}

@begin{DiffWord83}
The requirement for a nonnegative value in a Size clause
was not in RM83, but it's hard to see how it would make sense.
For uniformity, we forbid negative sizes,
rather than letting implementations define their meaning.
@end{DiffWord83}

@begin{StaticSem}
@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{To be consistent with 8652/0006}
@Leading@;For @ChgPrefixType{Version=[1],Kind=[Revised],Text=[a
@Chg{New=[@nt{prefix}],Old=[prefix]} T that denotes a task
object @Redundant[(after any implicit dereference)]]}:
@begin{Description}
@ChgAttribute{Version=[3], Kind=[Revised], ChginAnnex=[F], Leading=[F],
  Prefix=<T>, AttrName=<Storage_Size>, ARef=[AI05-0229-1],
  Text=<Denotes the number of storage elements reserved for
  the task.
The value of this attribute is of the type
@i{universal_integer}.
The Storage_Size includes the size of the task's stack,
if any. The language does not specify whether or
not it includes other storage associated with the task
(such as the @lquotes@;task control block@rquotes@; used by some
implementations.)>}
If @Chg{Version=[3],New=[the aspect],Old=[a @nt{pragma}]} Storage_Size is
@Chg{Version=[3],New=[specified for the type of the object],Old=[given]},
the value of the Storage_Size attribute is at least
the value @Chg{Version=[3],New=[determined by the aspect],Old=[specified
in the @nt{pragma}]}.
@EndPrefixType{}
  @begin{Ramification}
  The value of this attribute is never negative,
  since it is impossible to @lquotes@;reserve@rquotes@; a negative number
  of storage elements.

  If the implementation chooses to allocate an initial amount of
  storage, and then increase this as needed,
  the Storage_Size cannot include the additional amounts
  (assuming the allocation of the additional amounts can raise
  Storage_Error); this is inherent in the meaning of @lquotes@;reserved.@rquotes@;

  The implementation is allowed to allocate different amounts of
  storage for different tasks of the same subtype.

  Storage_Size is also defined for access subtypes
  @em see @RefSecNum{Storage Management}.
  @end{Ramification}
@end{Description}
@end{StaticSem}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI95-0229-1]}
@redundant[@Chg{Version=[3],New=[Aspect],Old=[@IndexSeeAlso{Term=[Storage_Size clause],See=[pragma Storage_Size]}
A @nt{pragma}]} Storage_Size specifies the amount of storage to be
reserved for the execution of a task.]
@end{Intro}

@begin{NotIso}
@ChgAdded{Version=[3],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 62
through 65 were moved to @RefSec{Obsolescent Features}.>}]}@Comment{This message
should be deleted if the paragraphs are ever renumbered.}
@end{NotIso}

@begin{Syntax}
@begin{SyntaxText}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Type=[Leading],KeepNext=[T],Text=[The form of a
@nt{pragma} Storage_Size is as follows:]}
@end{SyntaxText}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@DeletedPragmaSyn<Version=[3],InitialVersion=[0],@ChgDeleted{Version=[3],
Text=[@key{pragma} @prag(Storage_Size)(@Syn2{expression});]}>

@begin{SyntaxText}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Text=[A @nt{pragma} Storage_Size is allowed only
immediately within a @nt{task_definition}.]}
@end{SyntaxText}
@end{Syntax}

@begin{Resolution}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Text=[@PDefn2{Term=[expected type],
  Sec=(Storage_Size pragma argument)}
The @nt{expression} of a @nt<pragma> Storage_Size
is expected to be of any integer type.]}
@end{Resolution}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0229-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For a task type (including the
anonymous type of a @nt{single_task_declaration}),
the following language-defined representation aspect may be specified:]}

@begin{Description}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Storage_Size@\The Storage_Size aspect is
an @nt{expression}, which shall be of any integer
type.@AspectDefn{Storage_Size (task)}]}

@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This definition somewhat conflicts with the
  "automatic" one for the obsolescent attribute Storage_Size (which can be
  specified). The only difference is where the given expression is evaluated.
  We intend for the above definition to supersede that "automatic"
  definition for this attribute.]}
@end{Honest}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Note that the value of the Storage_Size aspect
  @i<is> an @nt{expression}; it is not the @i<value> of an @nt{expression}.
  The @nt{expression} is evaluated for each object of the type (see below).]}
@end{Ramification}

@ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Storage_Size (task)],
  Text=[@ChgAdded{Version=[3],Text=[Size in storage elements reserved for a task
    type or single task object.]}]}

@end{Description}
@end{StaticSem}

@begin{Legality}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[The Storage_Size aspect shall not be specified for a
task interface type.]}
@end{Legality}

@begin{RunTime}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Chg{Version=[3],New=[When a task object is created, the @nt{expression}
(if any) associated with the Storage_Size aspect of its
type],Old=[A @nt<pragma> Storage_Size is elaborated when an object
of the type defined by the immediately enclosing @nt<task_definition>
is created.
@PDefn2{Term=[elaboration],Sec=(Storage_Size pragma)}
For the elaboration of a @nt<pragma> Storage_Size, the @nt<expression>]}
is evaluated; the Storage_Size attribute of the newly created task object
is at least the value of the @nt<expression>.

@begin{Ramification}
  The implementation is allowed to round up a specified Storage_Size
  amount.
  For example, if the implementation always allocates in
  chunks of 4096 bytes, the number 200 might be rounded
  up to 4096. Also, if the user specifies a negative
  number, the implementation has to normalize this to 0,
  or perhaps to a positive number.

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[If the Storage_Size aspect is not specified for
  the type of the task object, the value of the Storage_Size attribute is
  unspecified.]}
@end{Ramification}

@IndexCheck{Storage_Check}
@Defn2{Term=[Storage_Error],Sec=(raised by failure of run-time check)}
At the point of task object creation, or upon task activation,
Storage_Error is raised if there is insufficient free storage to
accommodate the requested Storage_Size.
@end{RunTime}

@begin{StaticSem}
@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{To be consistent with 8652/0006}
@Leading@;For @ChgPrefixType{Version=[1],Kind=[Revised],Text=[a
@Chg{New=[@nt{prefix}],Old=[prefix]} X that denotes an array subtype or
array object @Redundant[(after any implicit dereference)]]}:
@begin{Description}
@Attribute{Prefix=<X>, AttrName=<Component_Size>,
  Text=<Denotes the size in bits of
components of the type of X.
The value of this attribute is of type @i{universal_integer}.>}
@EndPrefixType{}

@NoPrefix@PDefn2{Term=[specifiable], Sec=(of Component_Size for
array types)}@Defn{Component_Size clause}
Component_Size may be specified for array types
via an @nt{attribute_@!definition_@!clause};
the expression of such a clause shall be static,
and its value nonnegative.@Chg{Version=[3],New=[@AspectDefn{Component_Size}],Old=[]}

@begin{ImplNote}
The intent is that the value of X'Component_Size is always nonnegative.
If the array is stored @lquotes@;backwards@rquotes@; in memory
(which might be caused by an implementation-defined pragma),
X'Component_Size is still positive.
@end{ImplNote}
@begin{Ramification}
For an array object A, A'Component_Size = A(I)'Size for any index I.
@end{Ramification}

@ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Component_Size],
  Text=[@ChgAdded{Version=[3],Text=[Size in bits of a component of an array type.]}]}

@end{Description}
@end{StaticSem}

@begin{ImplAdvice}
@Leading@PDefn2{Term=[recommended level of support], Sec=(Component_Size attribute)}
The recommended level of support for the Component_Size attribute is:
@begin{Itemize}
An implementation need not support specified Component_Sizes that
are less than the Size of the component subtype.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
An implementation should support specified Component_Sizes that
are factors and multiples of the word size.
For such Component_Sizes, the array should contain no gaps between
components.
For other Component_Sizes (if supported), the array should
contain no gaps between components when
@Chg{Version=[3],New=[Pack],Old=[packing]} is also specified;
the implementation should forbid this combination in cases where it
cannot support a no-gaps representation.
@begin{Ramification}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
For example, if Storage_Unit = 8, and Word_Size = 32,
then the user is allowed to specify a Component_Size of
1, 2, 4, 8, 16, and 32, with no gaps.
In addition, @i{n}*32 is allowed for positive integers @i{n},
again with no gaps.
If the implementation accepts Component_Size = 3,
then it might allocate 10 components per word,
with a 2-bit gap at the end of each word
(unless @Chg{Version=[3],New=[Pack],Old=[packing]} is also specified),
or it might not have any internal gaps at all.
(There can be gaps at either end of the array.)
@end{Ramification}
@end{Itemize}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The recommended level of support for the Component_Size attribute should
be followed.]}]}
@end{ImplAdvice}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0191-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For
@ChgPrefixType{Version=[3],Kind=[Added],Text=[a @nt{prefix} X that denotes
an object]}:]}
@begin(description)
@ChgNote{ Original Version=[3],Kind=[Added]}
@ChgAttribute{Version=[4],Kind=[RevisedAdded],ChginAnnex=[T],
  Leading=<F>, Prefix=<X>, AttrName=<Has_Same_Storage>, ARef=[AI05-0191-1],
  InitialVersion=[3], Text=[@Chg{Version=[3],New=[X'Has_Same_Storage denotes
  a function with the following specification:],Old=[]}

@begin(Descexample)
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@key(function) X'Has_Same_Storage (@RI{Arg} : @RI{any_type})
  @key(return) Boolean]}
@end(Descexample)

   @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0191-1],ARef=[AI05-0264-1]}
   @ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0077-1]}
   @ChgAdded{Version=[3],NoPrefix=[T],Text=[The actual parameter shall be a name
   that denotes an object. The object denoted by the actual parameter can be of
   any type. This function evaluates the names of the objects
   involved@Chg{Version=[4],New=[. It],Old=[ and]}
   returns True if the representation of the object denoted by the actual
   parameter occupies exactly the same bits as the representation of the object
   denoted by X@Chg{Version=[4],New=[ and the objects occupy at least
   one bit],Old=[]}; otherwise, it returns False.]}]}@Comment{End of Annex text here.}

@begin{Discussion}
@ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Has_Same_Storage means that, if the representation
  is contiguous, the objects sit at the same address and occupy the same length
  of memory.]}
@end{Discussion}
@end(description)

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0191-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For @PrefixType{a @nt{prefix} X that
denotes an object}:]}
@begin(description)
@ChgAttribute{Version=[3],Kind=[Added],ChginAnnex=[T],
  Leading=<F>, Prefix=<X>, AttrName=<Overlaps_Storage>, ARef=[AI05-0191-1],
  InitialVersion=[3], Text=[@Chg{Version=[3],New=[X'Overlaps_Storage denotes
  a function with the following specification:],Old=[]}

@begin(Descexample)
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@key(function) X'Overlaps_Storage (@RI{Arg} : @RI{any_type})
  @key(return) Boolean]}
@end(Descexample)

   @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0191-1],ARef=[AI05-0264-1]}
   @ChgAdded{Version=[3],NoPrefix=[T],Text=[The actual parameter shall be a name
   that denotes an object. The object denoted by the actual parameter can be of
   any type. This function evaluates the names of the objects involved and
   returns True if the representation of the object denoted by the actual
   parameter shares at least one bit with the representation of the object
   denoted by X; otherwise, it returns False.]}]}@Comment{End of Annex text here.}
@end(description)
@end{StaticSem}

@begin{Notes}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0191-1]}
@ChgAdded{Version=[3],Text=[X'Has_Same_Storage(Y) implies X'Overlaps_Storage(Y).]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0191-1]}
@ChgAdded{Version=[3],Text=[X'Has_Same_Storage(Y) and X'Overlaps_Storage(Y) are
not considered to be reads of X and Y.]}
@end{Notes}

@begin{StaticSem}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0183-1]}
@ChgAdded{Version=[1],Text=[The following
@Chg{Version=[3],New=[type-related ],Old=[]}operational attribute is
defined: External_Tag.]}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@Leading@;For @PrefixType{every subtype S of a tagged type @i(T)
(specific or class-wide)}@Chg{New=[],Old=[, the following
attribute is defined]}:

@begin{Description}
@Comment{Original. @ChgAttribute{Version=[1], Kind=[Revised], ChginAnnex=[F], Leading=[F],
  Prefix=<S>, AttrName=<External_Tag>, Ref=[8652/0040], ARef=[AI95-00108-01], ...}
  We don't have a way to change multiple versions for attributes.}
@ChgAttribute{Version=[3], Kind=[Revised], ChginAnnex=[F], Leading=[F],
  Prefix=<S>, AttrName=<External_Tag>, Ref=[8652/0040], ARef=[AI95-00108-01], ARef=[AI05-0092-1],
  InitialVersion=[0], Text=[@Defn{External_Tag clause}
  @PDefn2{Term=(specifiable), Sec=(of External_Tag for a tagged type)}
  S'External_Tag denotes an external string representation
  for S'Tag; it is of the predefined type String.
  External_Tag may be specified for a specific tagged type
  via an @nt{attribute_definition_clause};
  the expression of such a clause shall be static.@Chg{Version=[3],New=[@AspectDefn{External_Tag}],Old=[]}
  The default external tag representation is implementation defined.
  See @Chg{Version=[3],New=[],Old=[@RefSecNum{Dispatching Operations of Tagged Types}
  and ]}@RefSecNum{Stream-Oriented Attributes}.]}
  @Chg{New=[The value of External_Tag is never inherited@Redundant[; the
  default value is always used unless a new value is directly specified
  for a type].],Old=[]}
  @ImplDef{The default external representation for a type tag.}
  @end{Description}
@EndPrefixType()

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[External_Tag],
    Text=[@ChgAdded{Version=[3],Text=[Unique identifier for a tagged type in
      streams.]}]}

@end{StaticSem}

@begin{RunTime}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0113-1]}
@ChgAdded{Version=[3],Text=[If a user-specified external tag S'External_Tag is
the same as T'External_Tag for some other tagged type declared by a different
declaration in the partition, Program_Error is raised by the elaboration of the
@nt{attribute_definition_clause}.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This rule does not depend on the visibility of the
  other tagged type, but it does depend on the existence of the other tagged
  type. The other tagged type could have the default external tag or a
  user-specified external tag.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This rule allows the same declaration to be
  elaborated multiple times. In that case, different types could have the same
  external tag. If that happens, Internal_Tag would return some unspecified tag,
  and Descendant_Tag probably would return the intended tag (using the given
  ancestor to determine which type is intended). However, in some cases (such as
  multiple instantiations of a derived tagged type declared in a generic body),
  Tag_Error might be raised by Descendant_Tag if multiple types are identified.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Note that while there is a race condition inherent
  in this definition (which attribute_definition_clause raises Program_Error
  depends on the order of elaboration), it doesn't matter as a program with two
  such clauses is simply wrong. Two types that both come from the same
  declaration are allowed, as noted previously.]}
@end{Ramification}
@end{RunTime}

@begin{ImplReq}
In an implementation, the default external tag for each specific tagged type
declared in a partition
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
  for input/output of class-wide types. These requirements ensure
  that what is written by one program can be read back by some other
  program so long as they share the same declaration for the
  type (and everything it depends on).

  The user may specify the external tag if (s)he wishes its value
  to be stable even across changes to the compilation unit
  in which the type is declared (or changes in some unit on which it
  depends).

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
  We use a String rather than a @Chg{Version=[2],
  New=[Stream_Element_Array],Old=[Storage_Array]} to represent an
  external tag for portability.
@end{Reason}
@begin{Ramification}
  Note that the characters
  of an external tag need not all be graphic characters.
  In other words, the external tag can be a sequence of arbitrary
  8-bit bytes.
@end{Ramification}
@end{ImplReq}

@begin{ImplPerm}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0113-1]}
@ChgAdded{Version=[3],Text=[If a user-specified external tag S'External_Tag is
the same as T'External_Tag for some other tagged type declared by a different
declaration in the partition, the partition may be rejected.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is, in general, a post-compilation check.
  This permission is intended for implementations that do link-time construction
  of the external tag lookup table; implementations that dynamically construct
  the table will likely prefer to raise Program_Error upon elaboration of the
  problem construct. We don't want this check to require any implementation
  complexity, as it will be very rare that there would be a problem.]}
@end{Ramification}
@end{ImplPerm}

@begin{Notes}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00270-01]}
The following language-defined attributes are specifiable,
at least for some of the kinds of entities to which they apply:
Address,
@Chg{Version=[2],New=[],Old=[Size, Component_Size, ]}Alignment,
@Chg{Version=[2],New=[Bit_Order, Component_Size, ],Old=[]}
External_Tag,
@Chg{Version=[2],New=[Input, Machine_Radix, Output, Read, Size, ],Old=[]}
Small, @Chg{Version=[2],New=[],Old=[Bit_Order, ]}
Storage_Pool, Storage_Size,
@Chg{Version=[2],New=[Stream_Size, and ],Old=[]}
Write@Chg{Version=[2],New=[],Old=[, Output, Read,
Input, and Machine_Radix]}.

It follows from the general rules in @RefSecNum{Operational and Representation Aspects}
that if one writes @lquotes@;@key[for] X'Size @key[use] Y;@rquotes@; then
the X'Size @nt{attribute_reference} will return Y
(assuming the implementation allows the Size clause).
The same is true for all of the specifiable attributes except Storage_Size.
@begin{Ramification}
An implementation may specify that an implementation-defined attribute is
specifiable for certain entities.
This follows from the fact that the semantics of
implementation-defined attributes is implementation defined.
An implementation is not allowed to make a language-defined attribute
specifiable if it isn't.
@end{Ramification}
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Examples of attribute definition clauses:}
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

@key[for] Car_Name'Storage_Size @key[use] --@RI{ specify access type's storage pool size}
        2000*((Car'Size/System.Storage_Unit) +1); --@RI{ approximately 2000 cars}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00441-01]}
@key[function] @Chg{Version=[2],New=[My_Input],Old=[My_Read]}(Stream : @key[@Chg{Version=[2],New=[not null ],Old=[]}access] Ada.Streams.Root_Stream_Type'Class)
  @key[return] T;
@key(for) T'@Chg{Version=[2],New=[Input],Old=[Read]} @key(use) @Chg{Version=[2],New=[My_Input],Old=[My_Read]}; --@RI{ see @RefSecNum{Stream-Oriented Attributes}}
@end{Example}
@end{Examples}

@begin{Notes}
@i{Notes on the examples:}
In the Size clause for Short,
fifteen bits is the minimum necessary,
since the type definition requires Short'Small <= 2**(@en@;7).
@end{Notes}

@begin{Extend83}
@Defn{extensions to Ada 83}
The syntax rule for @ntf{length_clause} is replaced with the new syntax rule
for @nt{attribute_definition_clause}, and it is modified to allow a
@nt{name} (as well as an expression).
@end{Extend83}

@begin{DiffWord83}
The syntax rule for @nt{attribute_definition_clause} now requires that the
prefix of the attribute be a @nt{local_name};
in Ada 83 this rule was stated in the text.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
In Ada 83, the relationship between a @Chg{Version=[2],New=[@nt{aspect_clause}],Old=[@nt{representation_clause}]}
specifying a certain aspect and an attribute that queried that
aspect was unclear.
In Ada 95, they are the same,
except for certain explicit exceptions.
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0009],ARef=[AI95-00137-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Added wording to specify for
  each attribute whether it is an operational or representation attribute.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0040],ARef=[AI95-00108-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Added wording to specify that
  External_Tag is never inherited.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00051-01],ARef=[AI95-00291-01]}
  @ChgAdded{Version=[2],Text=[Adjusted the Recommended Level of Support for
  Alignment to eliminate nonsense requirements and to ensure that useful
  capabilities are required.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00051-01],ARef=[AI95-00291-01]}
  @ChgAdded{Version=[2],Text=[Adjusted the Recommended Level of Support for
  Size to eliminate nonsense requirements and to ensure that useful
  capabilities are required. Also eliminated any dependence on whether an
  aspect was specified (a confirming representation item should not affect the
  semantics).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00133-01]}
  @ChgAdded{Version=[2],Text=[Added the definition of machine scalar.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00247-01]}
  @ChgAdded{Version=[2],Text=[Removed the requirement that specified
  alignments for a composite type cannot override those for their components,
  because it was never intended to apply to components whose location was
  specified with a representation item. Moreover, it causes a difference in
  legality when a confirming alignment is specified for one of the composite
  types.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00291-02]}
  @ChgAdded{Version=[2],Text=[Removed recommended level of support rules about
  types with by-reference and aliased parts, because there are now blanket
  rules covering all recommended level of support rules.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00291-02]}
  @ChgAdded{Version=[2],Text=[Split the definition of Alignment for subtypes
  and for objects. This simplified the wording and eliminated confusion about
  which rules applied to objects, which applied to subtypes, and which applied
  to both.]}
@end{DiffWord95}

@begin{Inconsistent2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI95-0095-1]}
  @ChgAdded{Version=[3],Text=[@Defn{inconsistencies with Ada 2005}
  @b<Correction:> An address attribute with a prefix of a generic formal
  subprogram whose actual parameter has convention Intrinsic now raises
  Program_Error. Since it is unlikely that such an attribute would have done
  anything useful (a subprogram with convention Intrinsic is not expected to
  have a normal subprogram body), it is highly unlikely that any existing
  programs would notice the difference, and any that do probably are buggy.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI95-0113-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> User-specified
  external tags that conflict with other external tags raise Program_Error
  (or are optionally illegal). This was legal and did not raise an exception
  in the past, although the effects were not defined. So while a program
  might depend on such behavior, the results were not portable (even to
  different versions of the same implementation). Such programs should be
  rare.]}
@end{Inconsistent2005}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0095-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b[Correction:]
  An address attribute with a prefix of a subprogram with convention Intrinsic is
  now illegal. Such attributes are very unlikely to have provided a useful
  answer (the intended meaning
  of convention Intrinsic is that there is no actual subprogram body for
  the operation), so this is highly unlikely to affect any existing programs
  unless they have a hidden bug.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0191-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Attributes Has_Same_Storage and Overlaps_Storage are new.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[Aspect Storage_Size is new;
  @nt{pragma} Storage_Size is now obsolescent, joining attribute
  Storage_Size for task types.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0009-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Improved the description
  of erroneous execution for address clauses to make it clear that
  specifying an address inappropriate for the entity will lead to
  erroneous execution.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0116-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added @ImplAdviceTitle for
  the alignment of class-wide types.]}
@end{DiffWord2005}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0070-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Clarified the behavior of
  Has_Same_Storage when 'Size = 0.]}
@end{DiffWord2012}



@LabeledClause{Enumeration Representation Clauses}

@begin{Intro}
@redundant[An @nt{enumeration_representation_clause} specifies the internal
codes for enumeration literals.]
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
The @lquotes@;full coverage rules@rquotes@; for @nt<aggregate>s applies.
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

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
@Chg{Version=[2],New=[Each component of the @nt{array_aggregate} shall be
given by an @nt{expression} rather than a <>. ],Old=[]}The
@Chg{Version=[2],New=[@nt{expression}s],Old=[expressions]} given in the
@nt{array_aggregate} shall be static,
and shall specify distinct integer codes for each value
of the enumeration type; the associated integer codes shall
satisfy the predefined ordering relation of the type.
@begin{Reason}
  Each value of the enumeration type has to be given an internal code,
  even if the first subtype of the enumeration type is constrained
  to only a subrange (this is only possible if the enumeration type
  is a derived type). This @lquotes@;full coverage@rquotes@; requirement
  is important because one may refer to Enum'Base'First and Enum'Base'Last,
  which need to have defined representations.
@end{Reason}
@end{Legality}

@begin{StaticSem}
@Chg{Version=[3],New=[@PDefn2{Term=[representation aspect], Sec=(coding)}],
Old=[@PDefn2{Term=[aspect of representation], Sec=(coding)}
@Defn2{Term=[coding], Sec=(aspect of representation)}]}
An @nt{enumeration_representation_clause} specifies the
@i{coding} aspect of representation.
@Defn{internal code}
The coding consists of the @i{internal code} for each enumeration
literal, that is, the integral value used internally to
represent each literal.@Chg{Version=[3],New=[@AspectDefn{Coding}],Old=[]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Coding],
    Text=[@ChgAdded{Version=[3],Text=[Internal representation of enumeration
      literals. Specified by an @nt{enumeration_representation_clause}, not
      by an @nt{aspect_specification}.]}]}

@end{StaticSem}

@begin{ImplReq}
For nonboolean enumeration types,
if the coding is not specified for the type, then
for each value of the type, the internal code shall be equal to
its position number.
@begin{Reason}
  This default representation is already used by all known
  Ada compilers for nonboolean enumeration types. Therefore,
  we make it a requirement so users can depend on it, rather
  than feeling obliged to supply for every enumeration type
  an enumeration representation clause that is equivalent
  to this default rule.
@end{Reason}
@begin{Discussion}
  For boolean types, it is relatively common to use all ones for
  True, and all zeros for False, since some hardware supports
  that directly. Of course, for a one-bit Boolean object
  (like in a packed array), False is presumably zero and True
  is presumably one (choosing the reverse would be extremely
  unfriendly!).
@end{Discussion}
@end{ImplReq}

@begin{ImplAdvice}
@Leading@PDefn2{Term=[recommended level of support], Sec=(@nt{enumeration_representation_clause})}
The recommended level of support for @nt{enumeration_representation_clause}s is:
@begin{Itemize}
An implementation should support at least the internal codes in the
range System.Min_Int..System.Max_Int. An implementation need not support
@nt{enumeration_@!representation_@!clause}s for boolean types.
@begin{Ramification}
The implementation may support numbers outside the above
range, such as numbers greater than System.Max_Int.
See AI83-00564.
@end{Ramification}
@begin{Reason}
The benefits of specifying the internal coding of a boolean type do
not outweigh the implementation costs.
Consider, for example, the implementation of the logical operators on
a packed array of booleans with strange internal codes.
It's implementable, but not worth it.
@end{Reason}
@end{Itemize}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The recommended level of support for
@nt{enumeration_representation_clause}s should be followed.]}]}
@end{ImplAdvice}

@begin{Notes}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
Unchecked_Conversion may be used to query the internal codes used
for an enumeration type.
The attributes of the type, such as Succ, Pred, and Pos,
are unaffected by the @Chg{New=[@nt{enumeration_representation_clause}],Old=[@nt{representation_clause}]}.
For example, Pos always returns the position number, @i{not} the
internal integer code that might have been specified in
@Chg{New=[an @nt{enumeration_representation_clause}],
Old=[a @nt{representation_clause}]}@Chg<Version=[3],New=[],Old=[}]>.
@begin{Discussion}
@Leading@;Suppose the enumeration type in question is derived:
@begin{Example}
@key[type] T1 @key[is] (Red, Green, Blue);
@key[subtype] S1 @key[is] T1 @key[range] Red .. Green;
@key[type] S2 @key[is] @key[new] S1;
@key[for] S2 @key[use] (Red => 10, Green => 20, Blue => 30);
@end{Example}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@Leading@;The @Chg{New=[@nt{enumeration_representation_clause}],Old=[@nt{representation_clause}]}
has to specify values for all enumerals, even ones that are not in S2 (such as
Blue). The Base attribute can be used to get at these values.
For example:
@begin{Example}
@key[for] I @key[in] S2'Base @key[loop]
    ... --@RI{ When I equals Blue, the internal code is 30.}
@key[end] @key[loop];
@end{Example}

We considered allowing or requiring
@lquotes@;@key[for] S2'Base @key[use] ...@rquotes@; in cases like this,
but it didn't seem worth the trouble.
@end{Discussion}
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Example of an enumeration representation clause:}
@begin{Example}
@key[type] Mix_Code @key[is] (ADD, SUB, MUL, LDA, STA, STZ);

@key[for] Mix_Code @key[use]
   (ADD => 1, SUB => 2, MUL => 3, LDA => 8, STA => 24, STZ =>33);
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
As in other similar contexts, Ada 95 allows expressions of any integer type,
not just expressions of type @i{universal_integer}, for the component
expressions in the @nt<enumeration_aggregate>. The preference rules
for the predefined operators of @i{root_integer} eliminate
any ambiguity.

For portability, we now require that the default coding for an enumeration
type be the @lquotes@;obvious@rquotes@; coding using position numbers.
This is satisfied by all known implementations.
@end{Extend83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0009],ARef=[AI95-00137-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Updated to reflect that we
  no longer have something called @ntf{representation_clause}.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
  @ChgAdded{Version=[2],Text=[Added wording to prevent the use of <> in a
  @nt{enumeration_representation_clause}. (<> is newly added to
  @nt{array_aggregate}s.)]}
@end{DiffWord95}



@LabeledClause{Record Layout}

@begin{Intro}
@Chg{Version=[3],New=[@AspectDefn{Layout}
@PDefn2{Term=[representation aspect], Sec=(layout)}
@PDefn2{Term=[representation aspect], Sec=(record layout)}
@AspectDefn{Record layout}
@PDefn2{Term=[representation aspect], Sec=(storage place)}
@Defn2{Term=[storage place], Sec=(representation aspect)}
@Defn2{Term=[storage place], Sec=(of a component)}],
Old=[@PDefn2{Term=[aspect of representation], Sec=(layout)}
@Defn2{Term=[layout], Sec=(aspect of representation)}
@PDefn2{Term=[aspect of representation], Sec=(record layout)}
@Defn2{Term=[record layout], Sec=(aspect of representation)}
@PDefn2{Term=[aspect of representation], Sec=(storage place)}
@Defn2{Term=[storage place], Sec=(of a component)}]}
The @i{(record) layout} aspect of representation
consists of the @i{storage places} for some or all components,
that is, storage place attributes of the components.
The layout can be specified with a @nt{record_@!representation_@!clause}.
@end{Intro}


@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledSubClause{Record Representation Clauses}

@begin{Intro}
@redundant[A @nt{record_representation_clause} specifies the storage representation
of records and record extensions, that is, the order, position, and size
of components (including discriminants, if any).
@IndexSee{Term=[bit field],See=(record_representation_clause)}]
@end{Intro}

@begin{MetaRules}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
It should be feasible for an implementation to use negative offsets
in the representation of composite types.
However, no implementation should be forced to support negative
offsets.
Therefore@Chg{Version=[2],New=[, in the interest of uniformity],Old=[]},
negative offsets should be disallowed in
@nt{record_representation_clause}s.
@end{MetaRules}

@begin{Syntax}
@Syn{lhs=<record_representation_clause>,rhs="
    @key{for} @SynI{first_subtype_}@Syn2{local_name} @key{use}
      @key{record} [@Syn2{mod_clause}]
        {@Syn2{component_clause}}
      @key{end} @key{record};"}


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
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00436-01]}
The @SynI{first_subtype_}@nt{local_name} of a
@nt{record_representation_clause} shall denote a specific
@Chg{Version=[2],New=[],Old=[nonlimited ]}record or record extension subtype.
@begin{Ramification}
As for all type-related representation items,
the @nt{local_name} is required to denote a first subtype.
@end{Ramification}

If the @i{component_}@nt<local_name> is a @nt<direct_name>,
the @nt{local_name} shall denote a component of the type.
For a record extension, the component shall not be inherited,
and shall not be a discriminant that corresponds to a discriminant of
the parent type.
If the @i{component_}@!@nt<local_@!name> has an @nt{attribute_@!designator},
the @nt{direct_@!name} of the @nt<local_@!name> shall denote either
the declaration of the type or a component of the type,
and the @nt{attribute_@!designator} shall denote an
implementation-defined implicit component of the type.

The @nt{position}, @nt{first_bit}, and @nt{last_bit} shall be
static expressions.
The value of @nt{position} and @nt{first_bit} shall be nonnegative.
The value of @nt{last_bit} shall be no less than
@nt{first_bit} @en 1.
@begin{Ramification}
A @nt{component_clause} such as
@lquotes@;X @key{at} 4 @key{range} 0..@en@;1;@rquotes@;
is allowed if X can fit in zero bits.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00133-01]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[If the
nondefault bit ordering applies to the type, then either:]}
@begin{Itemize}
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[the value of @nt{last_bit} shall be
  less than the size of the largest machine scalar; or]}

  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[the value of @nt{first_bit} shall be zero and the
  value of @nt{last_bit} + 1 shall be a multiple of System.Storage_Unit.]}
@end{Itemize}

At most one @nt{component_clause} is allowed for each component of
the type, including for each discriminant
(@nt{component_clause}s may be given for some, all, or none of the
components).
Storage places within a @nt{component_list} shall not overlap,
unless they are for components in distinct @nt{variant}s of the same
@nt{variant_part}.

A name that denotes a component of a type is not allowed within
a @nt{record_representation_clause} for the type,
except as the @SynI{component_}@nt<local_name>
of a @nt{component_clause}.
@begin{Reason}
    @Leading@;It might seem strange to make the
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
X : @key[constant] := 31; --@RI{ Same defining name as the component.}
@key[for] T @key[use]
    @key[record]
        X @key[at] 0 @key[range] 0..X; --@RI{ Illegal!}
    @key[end] @key[record];
    @end{Example}

    The component X hides the named number X throughout
    the @nt{record_representation_clause}.
@end{Reason}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00133-01]}
A @nt{record_representation_clause}
(without the @nt{mod_clause})
specifies the layout.@Chg{Version=[2],New=[],Old=[
The storage place
attributes (see @RefSecNum{Storage Place Attributes})
are taken from the
values of the @nt{position}, @nt{first_bit},
and @nt{last_bit} expressions
after normalizing those values so that
@nt{first_bit} is less than
Storage_Unit.@Chg{Version=[3],New=[@AspectDefn{Layout (record)}],Old=[]}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Layout (record)],
    Text=[@ChgAdded{Version=[3],Text=[Layout of record components. Specified by
      a @nt{record_representation_clause}, not by an @nt{aspect_specification}.]}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Record layout],
    Text=[@ChgAdded{Version=[3],Text=[See Layout.]}]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00133-01]}
@ChgAdded{Version=[2],Text=[If the default bit ordering applies to the type,
the @nt{position}, @nt{first_bit}, and @nt{last_bit} of each
@nt{component_clause} directly specify
the position and size of the corresponding component.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00133-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[If the nondefault bit ordering
applies to the type@Chg{Version=[3],New=[,],Old=[]} then
the layout is determined as follows:]}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[the @nt{component_clause}s for which the value of
@nt{last_bit} is greater than or equal to the size of the largest machine scalar
directly specify the position and size of the corresponding component;]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[for other @nt{component_clause}s, all of the
components having the same value of @nt{position} are considered to be part of
a single machine scalar, located at that @nt{position}; this machine scalar
has a size which is the smallest machine scalar size larger than the largest
@nt{last_bit} for all @nt{component_clause}s at that @nt{position}; the
@nt{first_bit} and @nt{last_bit} of each @nt{component_clause} are then
interpreted as bit offsets in this machine scalar.]}
@end{Itemize}

@begin{Ramification}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00133-01]}
@ChgNote{We want the header title to remain, so we use @Chg instead if @ChgDeleted.}
@Chg{Version=[2],New=[],Old=[For example,
if Storage_Unit is 8, then
@lquotes@;C @key[at] 0 @key[range] 24..31;@rquotes@;
defines C'Position = 3, C'First_Bit = 0, and C'Last_Bit = 7.
This is true of machines with either bit ordering.]}

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
An implemen@!tation may allow such implementation-defined names to be
used in @nt{record_@!representation_@!clause}s.
An implementation can restrict such @nt{component_@!clause}s in any
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
vernacular as @lquotes@;dope.@rquotes@;
Their main purpose is for storing offsets of components that depend
on discriminants.
@end{Discussion}

If a @nt<record_representation_clause> is given for an untagged derived type,
the storage place attributes for all of the components of the derived
type may differ
from those of the corresponding components of the parent type,
even for components whose storage
place is not specified explicitly in the @nt<record_@!representation_@!clause>.
@begin{Reason}
  This is clearly necessary, since the whole record may need to be
  laid out differently.
@end{Reason}
@end{ImplPerm}

@begin{ImplAdvice}
@Leading@PDefn2{Term=[recommended level of support], Sec=(@nt{record_representation_clause})}
The recommended level of support for @nt{record_representation_clause}s is:
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00133-01]}
@ChgAdded{Version=[2],Text=[An implementation should support machine scalars
that correspond to all of the integer, floating point, and address formats
supported by the machine.]}

An implementation should support storage places that can be extracted
with a load, mask, shift sequence of machine code,
and set with a load, shift, mask, store sequence,
given the available machine instructions and run-time model.

A storage place should be supported if its size is equal to the Size
of the component subtype,
and it starts and ends on a boundary that obeys the Alignment of the
component subtype.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00133-01]}
@Chg{Version=[2],New=[For],Old=[If the default bit ordering applies to
the declaration of a given type, then for]} a component
@Chg{Version=[2],New=[with a subtype ],Old=[]}whose
@Chg{Version=[2],New=[],Old=[subtype's ]}Size is less than the word size, any
storage place that does not cross an aligned word boundary should be supported.

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
If a @lquotes@;gap@rquotes@; in the parent type is filled in with a component in a
type extension, then this won't work when a class-wide object is
passed by reference, as is required.
@end{Reason}
@end{Itemize}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The recommended level of support for @nt{record_representation_clause}s
should be followed.]}]}
@end{ImplAdvice}

@begin{Notes}
If no @nt{component_clause} is given for a component, then the
choice of the storage place for the component is left to the
implementation. If @nt{component_clause}s are given for all components,
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

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
A @nt{record_representation_clause} cannot be given for a protected type,
even though protected types, like record types, have components.
The primary reason for this rule is that there is likely to be too
much dope in a protected type @em entry queues,
bit maps for barrier values, etc.
In order to control the representation of the user-defined components, simply
declare a record type, give it a
@Chg{New=[@nt{record_@!representation_@!clause}],Old=[@nt{representation_clause}]},
and give the protected type one component whose type is the record type.
Alternatively, if the protected object is protecting something like a
device register, it makes more sense to keep the thing being
protected outside the protected object (possibly with a pointer to it
in the protected object), in order to keep implementation-defined
components out of the way.
@end{Ramification}
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Example of specifying the layout of a record type:}
@begin{Example}
Word : @key[constant] := 4;  --@RI{  storage element is byte, 4 bytes per word}

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
      Protection_Key   @key[at] 0*Word @key[range] 10 .. 11; --@RI{ bits 8,9 unused}
      Machine_State    @key[at] 0*Word @key[range] 12 .. 15;
      Interrupt_Cause  @key[at] 0*Word @key[range] 16 .. 31;
      Ilc              @key[at] 1*Word @key[range] 0  .. 1;  --@RI{ second word}
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
The @nt{record_representation_clause} defines the record layout. The
Size clause guarantees that (at least) eight storage elements are used
for objects of the type. The Alignment clause guarantees that
aliased, imported, or exported objects of the type will have
addresses divisible by eight.
@end{Notes}


@begin{DiffWord83}
The @ntf{alignment_clause} has been renamed to @nt{mod_clause} and moved
to @RefSec{Obsolescent Features}.

We have clarified that implementation-defined component names have to be
in the form of an @nt{attribute_reference} of a component or of the
first subtype itself;
surely Ada 83 did not intend to allow arbitrary identifiers.

The RM83-13.4(7) wording incorrectly allows components in
nonvariant records to overlap.
We have corrected that oversight.
@end{DiffWord83}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00133-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  @b[Amendment Correction:] The meaning of a @nt{record_representation_clause}
  for the nondefault
  bit order is now clearly defined. Thus, such clauses can be portably
  written. In order to do that though, the equivalence of bit 1 in word 1 to
  bit 9 in word 0 (for a machine with Storage_Unit = 8) had to be dropped for
  the nondefault bit order. Any @nt{record_representation_clause}s which
  depends on that equivalence will break (although such code would imply a
  noncontiguous representation for a component, and it seems unlikely that
  compilers were supporting that anyway).]}
@end{Incompatible95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00436-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @b[Amendment Correction:] The undocumented (and
  likely unintentional) incompatibility with Ada 83 caused by not allowing
  @nt{record_representation_clause}s on limited record types is removed.]}
@end{Extend95}

@LabeledSubClause{Storage Place Attributes}

@begin{StaticSem}
@Leading@Defn2{Term=[storage place attributes], Sec=(of a component)}
For @PrefixType{a component C of a composite, non-array
object R},
the @i{storage place attributes} are defined:
@begin{Ramification}
The storage place attributes are not (individually) specifiable,
but the user may control their values by giving a
@nt{record_representation_clause}.
@end{Ramification}
@begin{Description}
@ChgAttribute{Version=[2],Kind=[Revised],ChginAnnex=[T],
  Leading=<F>, Prefix=<R.C>, AttrName=<Position>,
  ARef=[AI95-00133-01], InitialVersion=[0],
  Text=<@Chg{Version=[2],New=[If the nondefault bit ordering applies to the
  composite type, and if a @nt{component_clause} specifies the placement of C,
  denotes the value given for the @nt{position} of the @nt{component_clause};
  otherwise, denotes],Old=[Denotes]} the same value as R.C'Address @en@;
  R'Address. The value of this attribute is of the type
  @i{universal_integer}.>}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00133-01]}
Thus, @Chg{Version=[2],New=[for the default bit order, ],Old=[]}R.C'Position
is the offset of C in storage
elements from the beginning of the object,
where the first storage element of an object is numbered zero.
R'Address + R.C'Position = R.C'Address.
For record extensions, the offset is not measured from
the beginning of the extension part, but from the
beginning of the whole object, as usual.

In @lquotes@;R.C'Address @en@; R'Address@rquotes@;,
the "@en@;" operator is the one in System.Storage_Elements
that takes two Addresses and returns a Storage_Offset.
@end{Ramification}

@ChgAttribute{Version=[2],Kind=[Revised],ChginAnnex=[T],
  Leading=<F>, Prefix=<R.C>, AttrName=<First_Bit>,
  ARef=[AI95-00133-01], InitialVersion=[0],
  Text=<@Chg{Version=[2],New=[If the nondefault bit ordering applies to the
  composite type, and if a @nt{component_clause} specifies the placement of C,
  denotes the value given for the @nt{first_bit} of the @nt{component_clause};
  otherwise, denotes],Old=[Denotes]} the offset, from the start of the first
  of the storage elements occupied by C, of the first bit occupied by C.
  This offset is measured in bits.
  The first bit of a storage element is numbered zero.
  The value of this attribute is of the type
  @i{universal_integer}.>}

@ChgAttribute{Version=[2],Kind=[Revised],ChginAnnex=[T],
  Leading=<F>, Prefix=<R.C>, AttrName=<Last_Bit>,
  ARef=[AI95-00133-01], InitialVersion=[0],
  Text=<@Chg{Version=[2],New=[If the nondefault bit ordering applies to the
  composite type, and if a @nt{component_clause} specifies the placement of C,
  denotes the value given for the @nt{last_bit} of the @nt{component_clause};
  otherwise, denotes],Old=[Denotes]} the offset, from the start of the first
  of the storage elements occupied by C, of the last bit occupied by C.
  This offset is measured in bits. The value of this attribute
  is of the type @i{universal_integer}.>}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
The ordering of bits in a storage element is
@Chg{Version=[2],New=[],Old=[is ]}defined in @RefSec{Bit Ordering}.

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
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[If a component is represented using a pointer to the actual data of
the component which is contiguous with
the rest of the object, then the storage place attributes should reflect
the place of the actual data.
If a component is allocated discontiguously from the rest of the object,
then a warning should be generated upon reference to one
of its storage place attributes.]}]}
@end{ImplAdvice}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00133-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  @b[Amendment Correction:] The meaning of the storage place attributes for
  the nondefault
  bit order is now clearly defined, and can be different than that given by
  strictly following the Ada 95 wording. Any code which depends on the
  original Ada 95 values for a type using the nondefault bit order where
  they are different will break.]}
@end{Incompatible95}



@LabeledSubClause{Bit Ordering}

@begin{Intro}
@redundant[The Bit_Order attribute specifies the interpretation of the storage
place attributes.]
@begin{Reason}
The intention is to provide uniformity in the
interpretation of storage places across implementations on a
particular machine by allowing the user to specify the Bit_Order.
It is not intended to fully support data interoperability across
different machines,
although it can be used for that purpose in some situations.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
We can't require all implementations on a given machine to use the same
bit ordering by default;
if the user cares, a @Chg{Version=[2],New=[],Old=[@nt{pragma} ]}Bit_Order
@Chg{Version=[2],New=[@nt{attribute_definition_clause} ],Old=[]}can be used
to force all implementations to use the same bit ordering.
@end{Reason}
@end{Intro}

@begin{StaticSem}
@Defn{bit ordering}
A bit ordering is a method of interpreting the meaning of the
storage place attributes.
@Defn{High_Order_First}
@Defn{big endian}
@Defn2{Term=[endian], Sec=(big)}
High_Order_First @Redundant[(known in the vernacular as @lquotes@;big endian@rquotes@;)]
means that the first bit of a storage element
(bit 0) is the most significant bit (interpreting the sequence of
bits that represent a component as an unsigned integer value).
@Defn{Low_Order_First}
@Defn{little endian}
@Defn2{Term=[endian], Sec=(little)}
Low_Order_First @Redundant[(known in the vernacular as @lquotes@;little endian@rquotes@;)]
means the opposite: the first bit is the least significant.

@Leading@;For @PrefixType{every specific record subtype S},
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
the expression of such a clause shall be
static.@Chg{Version=[3],New=[@AspectDefn{Bit_Order}],Old=[]}

@ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Bit_Order],
  Text=[@ChgAdded{Version=[3],Text=[Order of bit numbering in a
    @nt{record_representation_clause}.]}]}

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
@Leading@PDefn2{Term=[recommended level of support], Sec=(bit ordering)}
The recommended level of support for the nondefault bit ordering is:
@begin{Itemize}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00133-01]}
  @Chg{Version=[2],New=[The],Old=[If Word_Size = Storage_Unit, then
  the]} implementation should support the nondefault bit ordering
  in addition to the default bit ordering.
@end{Itemize}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00133-01]}
@Chg{Version=[2],New=[The],Old=[If Word_Size = Storage_Unit,
the]} implementation should support both bit orderings.
@Chg{Version=[2],New=[Implementations],Old=[We don't push for support of
the nondefault bit ordering when Word_Size > Storage_Unit (except
of course for upward compatibility with a preexisting implementation
whose Ada 83 bit order did not correspond to the required Ada 95
default bit order), because
implementations]} are required to support storage positions that cross
storage element boundaries when Word_Size > Storage_Unit@Chg{Version=[2],
New=[ but the definition of the storage place attributes for the nondefault
bit order ensures that such],Old=[. Such]} storage positions will @Chg{Version=[2],
New=[not ],Old=[]}be split into two or three pieces@Chg{Version=[2],New=[. Thus,
there is no significant implementation burden to supporting the nondefault
bit order, given that the set of machine scalars is implementation-defined],
Old=[ if the nondefault bit ordering is used, which could be onerous to
support. However, if Word_Size = Storage_Unit,
there might not be a natural bit ordering,
but the splitting problem need not occur]}.
@end{Ramification}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The recommended level of support for the nondefault bit ordering
should be followed.]}]}
@end{ImplAdvice}

@begin{Notes}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00133-01]}
  @ChgAdded{Version=[2],Text=[Bit_Order clauses make it possible to write
  @nt{record_representation_clause}s that can be ported between machines having
  different bit ordering. They do not guarantee transparent exchange of data
  between such machines.]}
@end{Notes}

@begin{Extend83}
@Defn{extensions to Ada 83}
The Bit_Order attribute is new to Ada 95.
@end{Extend83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00133-01]}
  @ChgAdded{Version=[2],Text=[We now suggest that all implementations
  support the nondefault bit order.]}
@end{Diffword95}



@RMNewPageVer{Version=[2]}@Comment{For printed version of Ada 2005 RM}
@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledClause{Change of Representation}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@redundant[@Defn{change of representation}
@Defn2{Term=[representation], Sec=(change of)}
A @nt{type_conversion} (see @RefSecNum{Type Conversions})
can be used to convert between two different
representations of the same array or record.
To convert an array from one representation to another,
two array types need to be declared with
matching component subtypes, and convertible index types.
If one type has @Chg{Version=[3],New=[Pack],Old=[packing]}
specified and the other does not,
then explicit conversion can be used to pack or unpack an array.

To convert a record from one representation to another,
two record types with a common ancestor type need to be declared,
with no inherited subprograms. Distinct representations can then
be specified for the record types, and explicit conversion between
the types can be used to effect a change in representation.]
@begin{Ramification}
This technique does not work if the first type is an
untagged type with user-defined primitive subprograms.
It does not work at all for tagged types.
@end{Ramification}
@end{Intro}

@begin{Examples}
@leading@keepnext@i{Example of change of representation:}
@begin{Example}
--@RI{ Packed_Descriptor and Descriptor are two different types}
--@RI{ with identical characteristics, apart from their}
--@RI{ representation}

@key[type] Descriptor @key[is]
    @key[record]
      --@RI{ components of a descriptor}
    @key[end] @key[record];

@key[type] Packed_Descriptor @key[is] @key[new] Descriptor;

@key[for] Packed_Descriptor @key[use]
    @key[record]
      --@RI{ component clauses for some or for all components}
    @key[end] @key[record];

@RI{-- Change of representation can now be accomplished by explicit type conversions:}

D : Descriptor;
P : Packed_Descriptor;

P := Packed_Descriptor(D);  --@RI{ pack D}
D := Descriptor(P);         --@RI{ unpack P}
@end{Example}
@end{Examples}

